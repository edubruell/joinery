if (!requireNamespace("duckdb", quietly = TRUE) ||
    !requireNamespace("DBI", quietly = TRUE) ||
    !requireNamespace("dplyr", quietly = TRUE)) {
  return(invisible(NULL))
}

Duck_tbl <- new_S3_class("tbl_duckdb_connection")


#' Internal helper: SQL predicate for the pre-join rarity / document-frequency cut
#'
#' Returns one SQL boolean expression combining the strategy's `min_rarity`
#' (rarity-metric floor) and `max_token_df` (raw document-frequency cap), or
#' `NULL` when both are off. Applied to the token table *before* the
#' `(src_column, token, block)` equi-join - never after scoring. The DuckDB
#' mirror of `.rarity_prefilter_dt()`; the two predicates must stay identical
#' so both backends thin the same tokens.
#' @noRd
.rarity_prefilter_sql <- function(strategy) {
  conds <- character()
  if (strategy@min_rarity > 0) {
    conds <- c(conds, paste0("rarity >= ", strategy@min_rarity))
  }
  if (is.finite(strategy@max_token_df)) {
    conds <- c(conds, paste0("df <= ", strategy@max_token_df))
  }
  if (length(conds) == 0L) return(NULL)
  paste(conds, collapse = " AND ")
}


#' Internal helper: global token-blocking explosion on a DuckDB token table.
#'
#' The DuckDB mirror of `.explode_token_blocks_dt()`. Because the per-batch
#' tokenizer sees only a slice of the ids, the blocking-token df cap must be
#' computed once over the whole corpus. This pulls the raw `(id, blocking-cols)`
#' from the input table into R (distinct by id - one row per record, cheap),
#' reuses the data.table survivor kernel (`.btok_surviving_dt`) to get the
#' corpus-wide surviving `(id, ._btok)` pairs, writes them to DuckDB, and
#' rebuilds the token table as an INNER JOIN that fans each surviving record's
#' tokens across its block keys and drops records with no surviving key.
#'
#' No-op when the strategy has no `block_on_tokens()` spec.
#' @noRd
.explode_token_blocks_duck <- function(con, token_tbl, input_table, id,
                                       strategy, out_name) {
  specs <- .token_block_specs(strategy)
  if (length(specs) == 0L) return(token_tbl)

  id_q <- sprintf('"%s"', id)
  block_cols <- unique(vapply(specs, function(s) s@column, character(1)))
  sel_cols <- paste(c(id_q, sprintf('"%s"', block_cols)), collapse = ", ")

  # One row per record (distinct by id) carrying the raw blocking columns.
  raw <- DBI::dbGetQuery(con, paste0(
    "SELECT ", sel_cols, " FROM ",
    "(SELECT *, ROW_NUMBER() OVER (PARTITION BY ", id_q, ") AS _rn FROM ",
    input_table, ") WHERE _rn = 1"))
  raw <- data.table::as.data.table(raw)
  raw[[id]] <- as.character(raw[[id]])

  surv_list <- lapply(specs, function(s) .btok_surviving_dt(raw, id, s, strategy))
  surv <- unique(data.table::rbindlist(surv_list, use.names = TRUE))
  data.table::setnames(surv, "id", id)
  surv[[id]] <- as.character(surv[[id]])

  surv_tbl <- paste0("_joinery_btok_", sample.int(1e9, 1))
  DBI::dbWriteTable(con, surv_tbl, as.data.frame(surv), overwrite = TRUE)

  # Rebuild the token table with `._btok` attached (inner join: records with no
  # surviving block key drop out, exactly as the data.table backend does).
  tok_name <- token_tbl$lazy_query$x
  rebuilt  <- paste0(out_name, "_btok")
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", rebuilt, " AS\n",
    "SELECT t.*, s.\"._btok\" AS \"._btok\"\n",
    "FROM ", tok_name, " t\n",
    "JOIN ", surv_tbl, " s ON t.", id_q, " = s.", id_q, ";"))

  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", surv_tbl))
  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tok_name))
  DBI::dbExecute(con, paste0("ALTER TABLE ", rebuilt, " RENAME TO ", out_name))

  dplyr::tbl(con, out_name)
}


# Internal: Score token pairs with optional Phase 3 features
#------------------------------------------------------------------------------
# Handles:
#  - rIP computation with optional smoothing (log, softmax, offset)
#  - Token pair scoring with optional feedback adjustment
#  - Threshold filtering and optional containment (top-N per record)
#
# Returns: Name of temp table containing final scored pairs
#
# The function creates 3-4 temp tables:
#  1. _tokens_enriched: tokens with weights and smoothed rIP
#  2. _total_rip (if feedback enabled): total rIP per record
#  3. _pairs_scored: raw scored pairs
#  4. _pairs_final: after threshold and containment filtering
.score_pairs_sql <- function(con, 
                             tokens_tbl,
                             id1_col,
                             id2_col,
                             strategy,
                             join_type = c("self", "cross"),
                             block_join = "",
                             block_cols = character()) {

  tmp <- function(prefix) paste0(prefix, "_", sample.int(1e9, 1))
  join_type <- match.arg(join_type)

  # Token-blocking (`._btok`): a record is exploded across several blocks, each
  # carrying its full token set. rIP must normalise PER BLOCK, and the pair
  # score is the MAX over the blocks the pair co-occurs in (see the collapse
  # after STEP 2 and .score_token_pairs() on the data.table side). With plain
  # blocking each record sits in one block, so this is a no-op.
  block_part <- if (length(block_cols))
    paste0(", ", paste(sprintf('"%s"', block_cols), collapse = ", ")) else ""
  
  # ============================================================
  # STEP 1: Add weights and compute smoothed rIP
  # ============================================================
  enriched_tbl <- tmp("_tokens_enriched")
  
  # Build weight CASE expression
  if (length(strategy@weights)) {
    weights <- strategy@weights
  } else {
    cols <- names(strategy@preparers)
    weights <- rep_len(1 / length(cols), length(cols))
    names(weights) <- cols
  }
  
  weight_expr <- paste0(
    "CASE src_column ",
    paste(sprintf("WHEN '%s' THEN %g", names(weights), weights), collapse = " "),
    " ELSE 1.0 END"
  )
  
  # Determine partition for rIP calculation (per record, per block, per column).
  partition_by <- if (join_type == "self") {
    paste0(id1_col, ", src_column", block_part)
  } else {
    paste0(id1_col, ", source, src_column", block_part)
  }
  
  # Build rIP expression with optional smoothing
  sm <- strategy@smoothing
  if (sm@method == "none") {
    rip_expr <- paste0(
      "rarity / SUM(rarity) OVER (PARTITION BY ", partition_by, ")"
    )
  } else if (sm@method == "log") {
    rip_expr <- paste0(
      "LN(1.0 + rarity) / ",
      "SUM(LN(1.0 + rarity)) OVER (PARTITION BY ", partition_by, ")"
    )
  } else if (sm@method == "softmax") {
    temp <- sm@temperature
    rip_expr <- paste0(
      "EXP(rarity / ", temp, ") / ",
      "SUM(EXP(rarity / ", temp, ")) OVER (PARTITION BY ", partition_by, ")"
    )
  } else if (sm@method == "offset") {
    alpha <- sm@alpha
    rip_expr <- paste0(
      "(rarity + ", alpha, ") / ",
      "SUM(rarity + ", alpha, ") OVER (PARTITION BY ", partition_by, ")"
    )
  } else {
    cli::cli_abort("Unknown smoothing method: {.val {sm@method}}")
  }
  
  # Set semantics: collapse within-record token multiplicity before computing
  # rIP and the overlap join. The token table has no per-occurrence column
  # (row_id is the record id, shared by a token's repeated occurrences), and
  # freq/df/N/rarity are constant per (block, src_column, token), so SELECT
  # DISTINCT * collapses a repeated token to one row per record. This is a
  # no-op for records whose tokens are already distinct. rarity (and hence
  # inverse_freq = 1/freq, a corpus term-frequency) is left untouched.
  sql_enriched <- paste0(
    "CREATE TEMP TABLE ", enriched_tbl, " AS\n",
    "SELECT *,\n",
    "  ", weight_expr, " AS weight,\n",
    "  ", rip_expr, " AS rip\n",
    "FROM (SELECT DISTINCT * FROM ", tokens_tbl, ") AS _token_set;\n"
  )
  
  DBI::dbExecute(con, sql_enriched)
  
  
  # ============================================================
  # STEP 2: Score pairs (with optional feedback)
  # ============================================================
  scored_tbl <- tmp("_pairs_scored")
  
  # Build join conditions based on join type
  if (join_type == "self") {
    # Self-join for duplicate detection
    join_cond <- paste0(
      "  ON t1.src_column = t2.src_column\n",
      " AND t1.token = t2.token\n",
      if (block_join != "") paste0(" ", block_join, "\n") else ""
    )
    where_clause <- paste0("WHERE t1.", id1_col, " <> t2.", id1_col, "\n")
    id1_ref <- paste0("t1.", id1_col)
    id2_ref <- paste0("t2.", id1_col)
  } else {
    # Cross join for candidate search
    join_cond <- paste0(
      "  ON t1.src_column = t2.src_column\n",
      " AND t1.token = t2.token\n",
      " AND t1.source = 'base'\n",
      " AND t2.source = 'target'\n",
      if (block_join != "") paste0(" ", block_join, "\n") else ""
    )
    where_clause <- ""
    id1_ref <- paste0("t1.", id1_col)
    id2_ref <- paste0("t2.", id2_col)
  }
  
  # Per (pair, block): block columns are equal on both sides (block_join), so
  # group on t1's copy. The per-block scores are collapsed to one best row per
  # pair right after STEP 2.
  block_grp <- if (length(block_cols))
    paste0(", ", paste(sprintf('t1."%s"', block_cols), collapse = ", ")) else ""
  group_by <- paste0(id1_ref, ", ", id2_ref, block_grp)

  if (strategy@feedback_strength > 0) {
    # WITH FEEDBACK: need total_rip and matched_rip
    total_rip_tbl <- tmp("_total_rip")
    
    # First: compute total rip per record
    sql_total <- paste0(
      "CREATE TEMP TABLE ", total_rip_tbl, " AS\n",
      "SELECT ", id1_col, ",\n",
      "       SUM(rip) AS total_rip\n",
      "FROM ", enriched_tbl, "\n",
      if (join_type == "cross") "WHERE source = 'base'\n" else "",
      "GROUP BY ", id1_col, ";\n"
    )
    
    DBI::dbExecute(con, sql_total)
    
    # Then: score with feedback adjustment
    s <- strategy@feedback_strength
    sql_scored <- paste0(
      "CREATE TEMP TABLE ", scored_tbl, " AS\n",
      "SELECT\n",
      "  ", id1_ref, " AS id1,\n",
      "  ", id2_ref, " AS id2,\n",
      "  SUM(t1.rip * t1.weight) AS raw_score,\n",
      "  SUM(t1.rip) AS matched_rip,\n",
      "  tr.total_rip,\n",
      "  SUM(t1.rip * t1.weight) * (\n",
      "    1.0 - ", s, " * (1.0 - SUM(t1.rip) / tr.total_rip)\n",
      "  ) AS score\n",
      "FROM ", enriched_tbl, " t1\n",
      "JOIN ", enriched_tbl, " t2\n",
      join_cond,
      "JOIN ", total_rip_tbl, " tr ON ", id1_ref, " = tr.", id1_col, "\n",
      where_clause,
      "GROUP BY ", group_by, ", tr.total_rip;\n"
    )
    
    DBI::dbExecute(con, sql_scored)
    
  } else {
    # WITHOUT FEEDBACK: simple scoring
    sql_scored <- paste0(
      "CREATE TEMP TABLE ", scored_tbl, " AS\n",
      "SELECT\n",
      "  ", id1_ref, " AS id1,\n",
      "  ", id2_ref, " AS id2,\n",
      "  SUM(t1.rip * t1.weight) AS score\n",
      "FROM ", enriched_tbl, " t1\n",
      "JOIN ", enriched_tbl, " t2\n",
      join_cond,
      where_clause,
      "GROUP BY ", group_by, ";\n"
    )
    
    DBI::dbExecute(con, sql_scored)
  }

  # ============================================================
  # STEP 2.25: collapse per-block scores to one best row per pair.
  # Only needed under token-blocking (a pair can co-block under several `._btok`
  # keys). Downstream (renormalise, threshold, containment, the long-form
  # expansion) consumes only id1/id2/score, so MAX(score) per pair suffices.
  # ============================================================
  if (length(block_cols)) {
    collapsed_tbl <- tmp("_pairs_collapsed")
    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE ", collapsed_tbl, " AS\n",
      "SELECT id1, id2, MAX(score) AS score\n",
      "FROM ", scored_tbl, "\n",
      "GROUP BY id1, id2;\n"
    ))
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", scored_tbl, ";"))
    scored_tbl <- collapsed_tbl
  }


  # ============================================================
  # STEP 2.5: on_missing = "renormalise" — rescale score by the present-column
  # denominator so a column empty on BOTH records stops capping the score at
  # 1 - weight(col). z_pair = WA + WB - Wboth = total weight of columns present
  # on either record. Mirrors the data.table .present_col_denominator(). B1/§25.
  # ============================================================
  if (.strategy_on_missing(strategy) == "renormalise") {
    pres_a  <- tmp("_pres_a")
    pres_b  <- tmp("_pres_b")
    denom_t <- tmp("_pair_denom")
    resc_t  <- tmp("_pairs_rescaled")

    if (join_type == "self") {
      DBI::dbExecute(con, paste0(
        "CREATE TEMP TABLE ", pres_a, " AS\n",
        "SELECT DISTINCT ", id1_col, " AS rid, src_column, weight FROM ", enriched_tbl, ";"
      ))
      pres_b_ref <- pres_a
    } else {
      DBI::dbExecute(con, paste0(
        "CREATE TEMP TABLE ", pres_a, " AS\n",
        "SELECT DISTINCT ", id1_col, " AS rid, src_column, weight FROM ", enriched_tbl,
        " WHERE source = 'base';"
      ))
      DBI::dbExecute(con, paste0(
        "CREATE TEMP TABLE ", pres_b, " AS\n",
        "SELECT DISTINCT ", id2_col, " AS rid, src_column, weight FROM ", enriched_tbl,
        " WHERE source = 'target';"
      ))
      pres_b_ref <- pres_b
    }

    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE ", denom_t, " AS\n",
      "WITH s AS (SELECT DISTINCT id1, id2 FROM ", scored_tbl, "),\n",
      "     wa AS (SELECT rid, SUM(weight) AS wa FROM ", pres_a, " GROUP BY rid),\n",
      "     wb AS (SELECT rid, SUM(weight) AS wb FROM ", pres_b_ref, " GROUP BY rid),\n",
      "     wboth AS (\n",
      "       SELECT s.id1, s.id2, SUM(a.weight) AS wboth\n",
      "       FROM s\n",
      "       JOIN ", pres_a, " a ON a.rid = s.id1\n",
      "       JOIN ", pres_b_ref, " b ON b.rid = s.id2 AND b.src_column = a.src_column\n",
      "       GROUP BY s.id1, s.id2\n",
      "     )\n",
      "SELECT s.id1, s.id2,\n",
      "       wa.wa + wb.wb - COALESCE(wboth.wboth, 0) AS z_pair\n",
      "FROM s\n",
      "JOIN wa ON wa.rid = s.id1\n",
      "JOIN wb ON wb.rid = s.id2\n",
      "LEFT JOIN wboth ON wboth.id1 = s.id1 AND wboth.id2 = s.id2;"
    ))

    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE ", resc_t, " AS\n",
      "SELECT sc.* EXCLUDE (score),\n",
      "       sc.score / (CASE WHEN d.z_pair > 0 THEN d.z_pair ELSE 1 END) AS score\n",
      "FROM ", scored_tbl, " sc\n",
      "JOIN ", denom_t, " d ON d.id1 = sc.id1 AND d.id2 = sc.id2;"
    ))

    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", scored_tbl, ";"))
    walk(c(pres_a, if (join_type == "cross") pres_b, denom_t),
         \(t) DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", t, ";")))
    scored_tbl <- resc_t
  }


  # ============================================================
  # STEP 3: Apply threshold and containment
  # ============================================================
  final_tbl <- tmp("_pairs_final")
  thr <- strategy@threshold
  
  if (is.finite(strategy@max_candidates)) {
    # WITH CONTAINMENT: use ROW_NUMBER
    sql_final <- paste0(
      "CREATE TEMP TABLE ", final_tbl, " AS\n",
      "SELECT * EXCLUDE (rn)\n",
      "FROM (\n",
      "  SELECT *,\n",
      "    ROW_NUMBER() OVER (PARTITION BY id1 ORDER BY score DESC) AS rn\n",
      "  FROM ", scored_tbl, "\n",
      "  WHERE score >= ", thr, "\n",
      ")\n",
      "WHERE rn <= ", strategy@max_candidates, ";\n"
    )
  } else {
    # WITHOUT CONTAINMENT: simple filter
    sql_final <- paste0(
      "CREATE TEMP TABLE ", final_tbl, " AS\n",
      "SELECT *\n",
      "FROM ", scored_tbl, "\n",
      "WHERE score >= ", thr, ";\n"
    )
  }
  
  DBI::dbExecute(con, sql_final)
  
  # Return the final scored pairs table name
  return(final_tbl)
}


# Method: prepare_search_data for duckdb tbl, character ID, and Search_Strategy
#------------------------------------------------------------------------------
method(
  prepare_search_data,
  list(Duck_tbl, class_character, Search_Strategy)
) <- function(data, id, strategy,
              output_table = NULL,
              control = duckdb_control()) {

  lazy  <- data
  con   <- lazy$src$con
  lazy  <- .materialise_duck_input(lazy, con)
  data  <- lazy
  table <- lazy$lazy_query$x

  .check_reserved_names(dplyr::tbl_vars(data), id)

  # D2: one global id-uniqueness check (the per-batch tokenizer suppresses its
  # own warning). Counts duplicate id *values* across the whole input via SQL,
  # so an id duplicated across batch boundaries is still caught.
  n_dup_ids <- DBI::dbGetQuery(con, paste0(
    "SELECT COUNT(*) - COUNT(DISTINCT ", sprintf('"%s"', id), ") AS n FROM ", table
  ))$n
  .warn_nonunique_id(n_dup_ids, id)

  # Batch planning blocks by PLAIN columns only: a block_on_tokens() spec is not
  # a literal column to slice batches by, and its `._btok` explosion runs once
  # globally after batch_map (so the df cap is corpus-wide).
  block_by <- .plain_block_cols(strategy)
  if (!length(block_by)) block_by <- NULL

  out_name <- output_table %||%
    paste0("_joinery_tokens_", sample.int(1e9, 1))

  cli::cli_inform(
    "Preparing search token table in batches",
    .auto_close = TRUE
  )

  # Preprocess batching is per-row (atomic_blocks = FALSE): token generation is
  # row-independent, so a block may be sub-split safely. (Scoring chunking, by
  # contrast, is block-atomic — see search_candidates / duckdb_control.)
  plan <- duckdb_batch_plan(
    db_tbl            = lazy,
    id                = id,
    target_batch_size = control@target_batch_size,
    min_batch_size    = control@min_batch_size,
    chunk_strategy    = control@chunk_strategy,
    block_by          = block_by
  )
  
  fn <- function(df) {
    dt <- data.table::as.data.table(df)
    # A batch is a partial slice of the ids; the global uniqueness check runs
    # once above (D2), so suppress the per-batch warning here. Token-blocking is
    # exploded globally below (a batch's global-df cut would be batch-local), so
    # the per-batch tokenizer skips it.
    prepare_search_data(dt, id, strategy, warn_nonunique_id = FALSE,
                        explode_token_blocks = FALSE)
  }

  token_tbl <- batch_map(
    plan         = plan,
    con          = con,
    input_table  = table,
    fn           = fn,
    persist      = TRUE,
    output_table = out_name
  )

  # Token-blocking (Feature A): explode the assembled token table by each
  # record's surviving rare blocking-tokens into a `._btok` column. Run once,
  # globally, so the df cap on blocking-token eligibility is corpus-wide (a
  # per-batch cut would be batch-local). See .explode_token_blocks_duck().
  token_tbl <- .explode_token_blocks_duck(con, token_tbl, table, id, strategy,
                                          out_name)

  token_tbl
}


# Method: compute_rarity for DuckDB token table and Search_Strategy
#------------------------------------------------------------------------------
method(
  compute_rarity,
  list(Duck_tbl, Search_Strategy)
) <- function(tokens, strategy) {
    lazy <- tokens
    con  <- lazy$src$con
    table <- lazy$lazy_query$x
    
    rarity_method <- strategy@rarity
    # Effective block columns of the token table (plain + derived `._btok`); the
    # df / cost windows partition by these. See .block_cols().
    block_by      <- .block_cols(strategy)
    if (!length(block_by)) block_by <- NULL
    rarity_scope  <- strategy@rarity_scope

    # Quote block identifiers: the derived `._btok` (token-blocking) is not a
    # bare SQL identifier, and quoting is harmless for plain column names.
    block_cols_sql <- if (length(block_by))
      paste(sprintf('"%s"', block_by), collapse = ", ") else ""
    block_cols_prefixed <- if (length(block_by)) paste0(", ", block_cols_sql) else ""

    # Under global scope the rarity formula reads the corpus-wide trio
    # (freq_global/df_global/N_global, computed with the block columns dropped
    # from the PARTITION BY). The block-local freq/df/N stay on the output as
    # the cost axis (df cut, fan-out guard) - only informativeness follows
    # rarity_scope. See notes/region_free_linking.md section 5.2.
    is_global <- identical(rarity_scope, "global")
    fcol <- if (is_global) "freq_global" else "freq"
    dcol <- if (is_global) "df_global"   else "df"
    ncol <- if (is_global) "N_global"    else "N"

    rarity_expr <- switch(
      rarity_method,
      inverse_freq          = paste0("1.0 / ", fcol),
      smoothed_inverse_freq = paste0("1.0 / (", fcol, " + 1)"),
      tfidf = paste0(
        "(", fcol, " / SUM(", fcol, ") OVER (PARTITION BY src_column",
        if (is_global) "" else if (length(block_by)) paste0(", ", block_cols_sql) else "",
        ")) * LOG(1.0 + ", ncol, " / ", dcol, ")"
      ),
      bm25 = paste0("LOG((", ncol, " - ", dcol, " + 0.5) / (", dcol, " + 0.5))"),
      cli::cli_abort("Unknown rarity method: {.val {rarity_method}}")
    )

    global_window <- if (is_global) paste0(
      "    COUNT(*) OVER (PARTITION BY src_column, token) AS freq_global,\n",
      "    COUNT(DISTINCT row_id) OVER (PARTITION BY src_column, token) AS df_global,\n",
      "    COUNT(DISTINCT row_id) OVER (PARTITION BY src_column) AS N_global,\n"
    ) else ""

    # Two-level SQL: inner query computes freq/df/N as window functions; outer
    # query applies the rarity formula. This avoids DuckDB's prohibition on
    # nested window function calls (e.g. SUM(freq) OVER (...) where freq is
    # itself a window function result, as in tfidf).
    sql <- paste0(
      "SELECT *, ", rarity_expr, " AS rarity\n",
      "FROM (\n",
      "  SELECT *,\n",
      global_window,
      "    COUNT(*) OVER (PARTITION BY src_column, token", block_cols_prefixed, ") AS freq,\n",
      "    COUNT(DISTINCT row_id) OVER (PARTITION BY src_column, token", block_cols_prefixed, ") AS df,\n",
      "    COUNT(DISTINCT row_id) OVER (PARTITION BY src_column", block_cols_prefixed, ") AS N\n",
      "  FROM ", table, "\n",
      ") AS _inner"
    )

    temp_table <- paste0(table, "_rewrite_", sample.int(1e9, 1))
    
    DBI::dbExecute(con, paste0("CREATE TABLE ", temp_table, " AS ", sql))
    
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS temp.", table))
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS main.", table))
    
    DBI::dbExecute(con, paste0("ALTER TABLE ", temp_table, " RENAME TO ", table))
    
    dplyr::tbl(con, dbplyr::ident(table))
  }
