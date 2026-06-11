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
#' `(src_column, token, block)` equi-join — never after scoring. The DuckDB
#' mirror of [.rarity_prefilter_dt()]; the two predicates must stay identical
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
                             block_join = "") {
  
  tmp <- function(prefix) paste0(prefix, "_", sample.int(1e9, 1))
  join_type <- match.arg(join_type)
  
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
  
  # Determine partition for rIP calculation
  partition_by <- if (join_type == "self") {
    paste0(id1_col, ", src_column")
  } else {
    paste0(id1_col, ", source, src_column")
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
  
  group_by <- paste0(id1_ref, ", ", id2_ref)
  
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
              target_batch_size = NULL,
              min_batch_size = NULL,
              chunk_strategy = "block_consolidated") {

  lazy  <- data
  con   <- lazy$src$con
  lazy  <- .materialise_duck_input(lazy, con)
  data  <- lazy
  table <- lazy$lazy_query$x

  .check_reserved_names(dplyr::tbl_vars(data), id)

  block_by <- strategy@block_by %||% NULL
  
  out_name <- output_table %||%
    paste0("_joinery_tokens_", sample.int(1e9, 1))
  
  cli::cli_inform(
    "Preparing search token table in batches",
    .auto_close = TRUE
  )
  
  plan <- duckdb_batch_plan(
    db_tbl            = lazy,
    id                = id,
    target_batch_size = target_batch_size,
    min_batch_size    = min_batch_size,
    chunk_strategy    = chunk_strategy,
    block_by          = block_by
  )
  
  fn <- function(df) {
    dt <- data.table::as.data.table(df)
    prepare_search_data(dt, id, strategy)   
  }
  
  token_tbl <- batch_map(
    plan         = plan,
    con          = con,
    input_table  = table,
    fn           = fn,
    persist      = TRUE,
    output_table = out_name
  )
  
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
    block_by      <- strategy@block_by %||% NULL
    
    block_cols_sql <- if (length(block_by)) paste(block_by, collapse = ", ") else ""
    block_cols_prefixed <- if (length(block_by)) paste0(", ", block_cols_sql) else ""
    
    rarity_expr <- switch(
      rarity_method,
      inverse_freq          = "1.0 / freq",
      smoothed_inverse_freq = "1.0 / (freq + 1)",
      tfidf = paste0(
        "(freq / SUM(freq) OVER (PARTITION BY src_column",
        if (length(block_by)) paste0(", ", block_cols_sql) else "",
        ")) * LOG(1.0 + N / df)"
      ),
      bm25 = "LOG((N - df + 0.5) / (df + 0.5))",
      cli::cli_abort("Unknown rarity method: {.val {rarity_method}}")
    )

    # Two-level SQL: inner query computes freq/df/N as window functions; outer
    # query applies the rarity formula. This avoids DuckDB's prohibition on
    # nested window function calls (e.g. SUM(freq) OVER (...) where freq is
    # itself a window function result, as in tfidf).
    sql <- paste0(
      "SELECT *, ", rarity_expr, " AS rarity\n",
      "FROM (\n",
      "  SELECT *,\n",
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
