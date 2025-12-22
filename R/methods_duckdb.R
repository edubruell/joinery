if (requireNamespace("duckdb", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("dplyr", quietly = TRUE)) {
    return(invisible(NULL))
}

Duck_tbl <- new_S3_class("tbl_duckdb_connection")



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
    stop("Unknown smoothing method: ", sm@method, call. = FALSE)
  }
  
  sql_enriched <- paste0(
    "CREATE TEMP TABLE ", enriched_tbl, " AS\n",
    "SELECT *,\n",
    "  ", weight_expr, " AS weight,\n",
    "  ", rip_expr, " AS rip\n",
    "FROM ", tokens_tbl, ";\n"
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
  table <- lazy$lazy_query$x
  
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
      stop("Unknown rarity method: ", rarity_method)
    )
    
    sql <- paste(
      "SELECT",
      "  *,",
      paste0("  COUNT(*) OVER (PARTITION BY src_column, token", block_cols_prefixed, ") AS freq,"),
      paste0("  COUNT(DISTINCT row_id) OVER (PARTITION BY src_column, token", block_cols_prefixed, ") AS df,"),
      paste0("  COUNT(DISTINCT row_id) OVER (PARTITION BY src_column", block_cols_prefixed, ") AS N,"),
      paste0("  ", rarity_expr, " AS rarity"),
      "FROM", table
    )
    
    
    temp_table <- paste0(table, "_rewrite_", sample.int(1e9, 1))
    
    DBI::dbExecute(con, paste0("CREATE TABLE ", temp_table, " AS ", sql))
    
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS temp.", table))
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS main.", table))
    
    DBI::dbExecute(con, paste0("ALTER TABLE ", temp_table, " RENAME TO ", table))
    
    dplyr::tbl(con, dbplyr::ident(table))
  }
  
  
  
# Method: detect_duplicates for DuckDB table an id and a Search_Strategy
#------------------------------------------------------------------------------
# Detects duplicate records within a single table using SQL joins and
# recursive CTEs for connected component detection.
#
# Algorithm:
# 1. Score all record pairs via self-join on (column, token, block_by)
# 2. Filter pairs by similarity threshold
# 3. Build undirected edge set from scored pairs
# 4. Compute connected components using recursive CTE (transitive closure)
# 5. Assign group labels and ranks based on best score per record
# 6. Merge back original data
#
# The recursive CTE approach scales to millions of duplicate pairs without
# transferring edge lists to R, keeping all operations columnar in DuckDB.
method(
  detect_duplicates,
  list(Duck_tbl, class_character, Search_Strategy)
) <- function(base_table, id, strategy, weights = NULL, base_tokens = NULL,
              debug = FALSE) {
  
  con <- base_table$src$con
  id_q <- sprintf('"%s"', id)
  
  tmp <- function(prefix) paste0(prefix, "_", sample.int(1e9, 1))
  
  # ----------------------------------------------------------
  # 1. Prepare tokens and compute rarity
  # ----------------------------------------------------------
  tokens <- if (is.null(base_tokens)) {
    base_table |>
      prepare_search_data(id, strategy)
  } else {
    base_tokens
  }
  
  tokens <- compute_rarity(tokens, strategy)
  tbl_tokens <- tokens$lazy_query$x
  
  if (strategy@min_rarity > 0) {
    threshold <- strategy@min_rarity
    
    sql <- paste0(
      "CREATE OR REPLACE TABLE ", tbl_tokens, " AS\n",
      "SELECT * FROM ", tbl_tokens, "\n",
      "WHERE rarity >= ", threshold, ";"
    )
    
    DBI::dbExecute(con, sql)
  }
  
  # ----------------------------------------------------------
  # 2. Score pairs using helper 
  # ----------------------------------------------------------
  block_by <- strategy@block_by %||% character()
  block_join <- if (length(block_by)) {
    paste(sprintf("AND t1.\"%s\" = t2.\"%s\"", block_by, block_by), collapse = "\n")
  } else ""
  
  # Override weights if provided as argument
  if (!is.null(weights)) {
    strategy@weights <- weights
  }
  
  # Use helper function for scoring (includes enrichment, smoothing, feedback, containment)
  pairs_tbl <- .score_pairs_sql(
    con        = con,
    tokens_tbl = tbl_tokens,
    id1_col    = id_q,
    id2_col    = id_q,
    strategy   = strategy,
    join_type  = "self",
    block_join = block_join
  )
  
  # Quick empty check
  n_pairs <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", pairs_tbl))$n
  if (n_pairs == 0L) {
    out_name <- tmp("_joinery_tmp_dups")
    DBI::dbExecute(
      con,
      paste0(
        "CREATE TABLE ", out_name, " AS\n",
        "SELECT CAST(NULL AS VARCHAR) AS id,\n",
        "       CAST(NULL AS INTEGER) AS duplicate_group,\n",
        "       CAST(NULL AS DOUBLE) AS score,\n",
        "       CAST(NULL AS INTEGER) AS rank\n",
        "LIMIT 0;"
      )
    )
    return(dplyr::tbl(con, out_name))
  }
  
  # ----------------------------------------------------------
  # 5. TEMP: edges (both directions)
  # ----------------------------------------------------------
  edges_tbl <- tmp("_joinery_tmp_edges")
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TEMP TABLE ", edges_tbl, " AS\n",
      "SELECT id1 AS a, id2 AS b FROM ", pairs_tbl, "\n",
      "UNION ALL\n",
      "SELECT id2 AS a, id1 AS b FROM ", pairs_tbl, ";\n"
    )
  )
  
  # ----------------------------------------------------------
  # 6. TEMP: connected components in DuckDB
  # ----------------------------------------------------------
  comp_tbl <- tmp("_joinery_tmp_components")
  
  sql_cc <- paste0(
    "CREATE TEMP TABLE ", comp_tbl, " AS\n",
    "WITH RECURSIVE cc AS (\n",
    "  SELECT a AS node, a AS label FROM ", edges_tbl, "\n",
    "  UNION ALL\n",
    "  SELECT e.b AS node, MIN(cc.label) AS label\n",
    "  FROM ", edges_tbl, " e\n",
    "  JOIN cc ON e.a = cc.node\n",
    "  WHERE cc.label < e.b\n",
    "  GROUP BY e.b\n",
    ")\n",
    "SELECT node AS id, MIN(label) AS root\n",
    "FROM cc\n",
    "GROUP BY node;\n"
  )
  DBI::dbExecute(con, sql_cc)
  
  # ----------------------------------------------------------
  # 7. TEMP: best score per id
  # ----------------------------------------------------------
  best_tbl <- tmp("_joinery_best_scores")
  
  sql_best <- paste0(
    "CREATE TEMP TABLE ", best_tbl, " AS\n",
    "SELECT id,\n",
    "       MAX(score) AS score\n",
    "FROM (\n",
    "  SELECT id1 AS id, score FROM ", pairs_tbl, "\n",
    "  UNION ALL\n",
    "  SELECT id2 AS id, score FROM ", pairs_tbl, "\n",
    ")\n",
    "GROUP BY id;\n"
  )
  DBI::dbExecute(con, sql_best)
  
  # ----------------------------------------------------------
  # 8. FINAL: join components + best score + base table + ranks
  # ----------------------------------------------------------
  out_name <- tmp("_joinery_tmp_dups")
  base_tbl <- base_table$lazy_query$x
  
  final_sql <- paste0(
    "CREATE TABLE ", out_name, " AS\n",
    "SELECT c.id AS id,\n",
    "       DENSE_RANK() OVER (ORDER BY c.root) AS duplicate_group,\n",
    "       b.score,\n",
    "       ROW_NUMBER() OVER (\n",
    "         PARTITION BY c.root\n",
    "         ORDER BY b.score DESC NULLS LAST\n",
    "       ) AS rank,\n",
    "       bt.* EXCLUDE (", id_q, ")\n",
    "FROM ", comp_tbl, " c\n",
    "LEFT JOIN ", best_tbl, " b ON c.id = b.id\n",
    "LEFT JOIN ", base_tbl, " bt ON c.id = bt.", id_q, "\n",
    "ORDER BY duplicate_group, rank;\n"
  )
  
  
  DBI::dbExecute(con, final_sql)
  
  # Drop intermediate tables unless debugging
  if (!debug) {
    # Note: pairs_tbl and its intermediate tables (from .score_pairs_sql helper)
    # are already temp tables and will be cleaned up automatically
    walk(c(pairs_tbl, edges_tbl, comp_tbl, best_tbl), function(tbl) {
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tbl))
    })
  }
  
  dplyr::tbl(con, out_name)
}

# Method: deduplicate_table
#------------------------------------------------------------------------------
#
# Removes duplicate records from a table based on duplicate detection results.
# Uses SQL anti-join to exclude records whose IDs appear in duplicates with rank != 1.
#
# The logic:
# 1. Extract IDs from duplicates table where rank != 1 (these are the losers)
# 2. Anti-join: keep all rows from base_table whose id is NOT in the losers set
# 3. Return filtered table as DuckDB tbl_lazy object
# Method: deduplicate_table
#------------------------------------------------------------------------------
method(
  deduplicate_table,
  list(Duck_tbl, Duck_tbl, class_character)
) <- function(base_table, duplicates, id) {
  
  con <- base_table$src$con
  
  base_tbl_name <- base_table$lazy_query$x
  dups_tbl_name <- duplicates$lazy_query$x
  
  out_name <- paste0("_joinery_dedup_", sample.int(1e9, 1))
  
  id_q <- sprintf('"%s"', id)
  
  sql <- paste0(
    "CREATE TABLE ", out_name, " AS\n",
    "SELECT b.*\n",
    "FROM ", base_tbl_name, " AS b\n",
    "WHERE NOT EXISTS (\n",
    "  SELECT 1\n",
    "  FROM ", dups_tbl_name, " AS d\n",
    "  WHERE d.rank != 1\n",
    "    AND d.id = b.", id_q, "\n",
    ");"
  )
  
  DBI::dbExecute(con, sql)
  dplyr::tbl(con, out_name)
}



# Method: search_candidates for DuckDB tables
#------------------------------------------------------------------------------
# Summary:
#   Given a base table and a target table, compute candidate record matches
#   by tokenizing selected columns, weighting tokens by rarity and column
#   importance, and scoring overlaps. Produces a long-form table with one
#   row per record per match.
#
# Pipeline:
#   1. Tokenize all selected columns for base and target (batching if needed)
#   2. Combine tokens with source markers (base or target)
#   3. Compute token rarity across the combined token universe
#   4. Score pairs using .score_pairs_sql() helper (smoothing, feedback, containment)
#   5. Assign match IDs and expand to long form
#   6. Merge the corresponding base and target data back in
#   7. Rank records within each match group
#
# Returned value:
#   A DuckDB table containing match_id, score, source, id, rank, and
#   all original columns.
method(
  search_candidates,
  list(Duck_tbl, Duck_tbl, class_character, class_character, Search_Strategy)
) <- function(base_table,
              target_table,
              base_id,
              target_id,
              strategy,
              weights = NULL,
              base_tokens = NULL,
              target_tokens = NULL,
              debug = FALSE) {
  

  con <- base_table$src$con
  base_id_q   <- sprintf('"%s"', base_id)
  target_id_q <- sprintf('"%s"', target_id)
  
  block_by <- strategy@block_by %||% NULL
  tmp <- function(prefix) paste0(prefix, "_", sample.int(1e9, 1))
  
  
  #----------------------------------------------------------
  # 1. Prepare tokens for base and target
  #----------------------------------------------------------
  base_tokens <- if (is.null(base_tokens)) {
    base_table |> prepare_search_data(base_id, strategy)
  } else base_tokens
  
  target_tokens <- if (is.null(target_tokens)) {
    target_table |> prepare_search_data(target_id, strategy)
  } else target_tokens
  
  
  # Raw lazy tables from compute step
  tbl_base_tokens_raw   <- base_tokens$lazy_query$x
  tbl_target_tokens_raw <- target_tokens$lazy_query$x
  
  
  #----------------------------------------------------------
  # 2. Rename id columns using SQL 
  #----------------------------------------------------------
  tbl_base_tokens   <- tmp("_joinery_tmp_base_tokens")
  tbl_target_tokens <- tmp("_joinery_tmp_target_tokens")
  
  sql_rename_base <- paste0(
    "CREATE TEMP TABLE ", tbl_base_tokens, " AS\n",
    "SELECT *, ", base_id_q, " AS doc_id\n",
    "FROM ", tbl_base_tokens_raw, ";\n"
  )
  
  sql_rename_target <- paste0(
    "CREATE TEMP TABLE ", tbl_target_tokens, " AS\n",
    "SELECT *, ", target_id_q, " AS doc_id\n",
    "FROM ", tbl_target_tokens_raw, ";\n"
  )
  
  DBI::dbExecute(con, sql_rename_base)
  DBI::dbExecute(con, sql_rename_target)
  
  
  #----------------------------------------------------------
  # 3. Union tokens with source marker
  #----------------------------------------------------------
  union_tbl <- tmp("_joinery_tmp_union")
  
  sql_union <- paste0(
    "CREATE TEMP TABLE ", union_tbl, " AS\n",
    "SELECT *, 'base' AS source\n",
    "FROM ", tbl_base_tokens, "\n",
    "UNION ALL\n",
    "SELECT *, 'target' AS source\n",
    "FROM ", tbl_target_tokens, ";\n"
  )
  
  DBI::dbExecute(con, sql_union)
  
  
  #----------------------------------------------------------
  # 4. Compute rarity
  #----------------------------------------------------------
  union_lazy <- dplyr::tbl(con, union_tbl)
  all_tokens <- compute_rarity(union_lazy, strategy)
  enriched_tbl <- all_tokens$lazy_query$x
  
  if (strategy@min_rarity > 0) {
    sql_filter <- paste0(
      "CREATE OR REPLACE TABLE ", enriched_tbl, " AS\n",
      "SELECT * FROM ", enriched_tbl, " WHERE rarity >= ", strategy@min_rarity, ";\n"
    )
    DBI::dbExecute(con, sql_filter)
  }
  
  
  #----------------------------------------------------------
  # 5. Score pairs using helper
  #----------------------------------------------------------
  if (length(block_by)) {
    block_join <- paste(sprintf("AND t1.\"%s\" = t2.\"%s\"", block_by, block_by), collapse = "\n")
  } else {
    block_join <- ""
  }
  
  # Override weights if provided as argument
  if (!is.null(weights)) {
    strategy@weights <- weights
  }
  
  # Use helper function for scoring (includes enrichment, smoothing, feedback, containment)
  scored_pairs_tbl <- .score_pairs_sql(
    con        = con,
    tokens_tbl = enriched_tbl,
    id1_col    = "doc_id",
    id2_col    = "doc_id",
    strategy   = strategy,
    join_type  = "cross",
    block_join = block_join
  )
  
  
  # Empty early exit
  n_pairs <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", scored_pairs_tbl))$n
  if (n_pairs == 0L) {
    out_name <- tmp("_joinery_tmp_candidates")
    DBI::dbExecute(
      con,
      paste0(
        "CREATE TABLE ", out_name, " AS\n",
        "SELECT CAST(NULL AS INTEGER) AS match_id,\n",
        "       CAST(NULL AS DOUBLE) AS score,\n",
        "       CAST(NULL AS VARCHAR) AS source,\n",
        "       CAST(NULL AS VARCHAR) AS id,\n",
        "       CAST(NULL AS INTEGER) AS rank\n",
        "LIMIT 0;"
      )
    )
    return(dplyr::tbl(con, out_name))
  }
  
  
  #----------------------------------------------------------
  # 6. Assign match IDs (scored_pairs_tbl uses id1/id2 from helper)
  #----------------------------------------------------------
  matched_tbl <- tmp("_joinery_tmp_matched")
  
  sql_matched <- paste0(
    "CREATE TEMP TABLE ", matched_tbl, " AS\n",
    "SELECT\n",
    "  ROW_NUMBER() OVER (ORDER BY id1, id2) AS match_id,\n",
    "  id1 AS base_id,\n",
    "  id2 AS target_id,\n",
    "  score\n",
    "FROM ", scored_pairs_tbl, ";\n"
  )
  
  DBI::dbExecute(con, sql_matched)
  

  #----------------------------------------------------------
  # 7. Expand to long form
  #----------------------------------------------------------
  long_tbl <- tmp("_joinery_tmp_long")
  
  sql_long <- paste0(
    "CREATE TEMP TABLE ", long_tbl, " AS\n",
    "SELECT match_id, score, 'base' AS source, base_id AS id\n",
    "FROM ", matched_tbl, "\n",
    "UNION ALL\n",
    "SELECT match_id, score, 'target' AS source, target_id AS id\n",
    "FROM ", matched_tbl, ";\n"
  )
  
  DBI::dbExecute(con, sql_long)
  
  
  #----------------------------------------------------------
  # 8. Merge original base rows
  #----------------------------------------------------------
  base_with_cols_tbl <- tmp("_joinery_tmp_base_merge")
  base_tbl <- base_table$lazy_query$x
  
  sql_base_merge <- paste0(
    "CREATE TEMP TABLE ", base_with_cols_tbl, " AS\n",
    "SELECT\n",
    "  l.match_id,\n",
    "  l.score,\n",
    "  l.source,\n",
    "  l.id,\n",
    "  b.* EXCLUDE (", base_id_q, ")\n",
    "FROM ", long_tbl, " l\n",
    "LEFT JOIN ", base_tbl, " b\n",
    "  ON l.id = b.", base_id_q, "\n",
    "WHERE l.source = 'base';\n"
  )
  
  DBI::dbExecute(con, sql_base_merge)
  
  
  #----------------------------------------------------------
  # 9. Merge original target rows
  #----------------------------------------------------------
  target_with_cols_tbl <- tmp("_joinery_tmp_target_merge")
  target_tbl <- target_table$lazy_query$x
  
  sql_target_merge <- paste0(
    "CREATE TEMP TABLE ", target_with_cols_tbl, " AS\n",
    "SELECT\n",
    "  l.match_id,\n",
    "  l.score,\n",
    "  l.source,\n",
    "  l.id,\n",
    "  t.* EXCLUDE (", target_id_q, ")\n",
    "FROM ", long_tbl, " l\n",
    "LEFT JOIN ", target_tbl, " t\n",
    "  ON l.id = t.", target_id_q, "\n",
    "WHERE l.source = 'target';\n"
  )
  
  DBI::dbExecute(con, sql_target_merge)
  
  
  # ---------------------------------------------------------------
  # 10. Overwrite base_with_cols_tbl and target_with_cols_tbl so that
  # both have identical column sets and column order
  # ---------------------------------------------------------------
  
  # Fetch column names
  cols_base   <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(", base_with_cols_tbl, ");"))$name
  cols_target <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(", target_with_cols_tbl, ");"))$name
  
  # Union of all column names
  common_cols <- union(cols_base, cols_target)
  
  # Build aligned SELECT lists
  sel_base <- paste(
    vapply(common_cols, function(col) {
      if (col %in% cols_base) col else paste0("NULL AS ", col)
    }, character(1L)),
    collapse = ", "
  )
  
  sel_target <- paste(
    vapply(common_cols, function(col) {
      if (col %in% cols_target) col else paste0("NULL AS ", col)
    }, character(1L)),
    collapse = ", "
  )
  
  # Overwrite tables with aligned versions
  sql_overwrite_base <- paste0(
    "CREATE OR REPLACE TEMP TABLE ", base_with_cols_tbl, " AS\n",
    "SELECT ", sel_base, "\n",
    "FROM ", base_with_cols_tbl, ";\n"
  )
  
  sql_overwrite_target <- paste0(
    "CREATE OR REPLACE TEMP TABLE ", target_with_cols_tbl, " AS\n",
    "SELECT ", sel_target, "\n",
    "FROM ", target_with_cols_tbl, ";\n"
  )
  
  DBI::dbExecute(con, sql_overwrite_base)
  DBI::dbExecute(con, sql_overwrite_target)
  
  
  #----------------------------------------------------------
  # 11. Final union and ranking
  #----------------------------------------------------------
  out_name <- tmp("_joinery_tmp_candidates")
  
  sql_final <- paste0(
    "CREATE TABLE ", out_name, " AS\n",
    "SELECT\n",
    "  match_id,\n",
    "  score,\n",
    "  source,\n",
    "  id,\n",
    "  ROW_NUMBER() OVER (\n",
    "    PARTITION BY match_id\n",
    "    ORDER BY score DESC NULLS LAST\n",
    "  ) AS rank,\n",
    "  * EXCLUDE (match_id, score, source, id)\n",
    "FROM (\n",
    "  SELECT * FROM ", base_with_cols_tbl, "\n",
    "  UNION ALL\n",
    "  SELECT * FROM ", target_with_cols_tbl, "\n",
    ")\n",
    "ORDER BY match_id, source, rank;\n"
  )
  
  DBI::dbExecute(con, sql_final)
  
  
  #----------------------------------------------------------
  # Cleanup
  #----------------------------------------------------------
  if (!debug) {
    # Note: scored_pairs_tbl and its intermediate tables (from .score_pairs_sql helper)
    # are already temp tables and will be cleaned up automatically
    temp_tables <- c(
      union_tbl, enriched_tbl,
      scored_pairs_tbl, matched_tbl, long_tbl,
      base_with_cols_tbl, target_with_cols_tbl,
      tbl_base_tokens, tbl_target_tokens
    )
    walk(temp_tables, \(tbl) {
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tbl))
    })
  }
  
  dplyr::tbl(con, out_name)
}

  
# Method: extract_unmatched for DuckDB
#------------------------------------------------------------------------------
method(
  extract_unmatched,
  list(Duck_tbl, class_character, Duck_tbl)
) <- function(data, id, matches) {
  
  con <- data$src$con
  
  data_tbl    <- data$lazy_query$x
  matches_tbl <- matches$lazy_query$x
  
  id_q <- sprintf('"%s"', id)
  
  # Validate: id column exists in data table
  data_cols <- DBI::dbGetQuery(
    con,
    paste0("PRAGMA table_info(", data_tbl, ");")
  )$name
  
  if (!id %in% data_cols) {
    stop(sprintf("ID column '%s' not found in data", id), call. = FALSE)
  }
  
  # Validate: matches table has an 'id' column
  matches_cols <- DBI::dbGetQuery(
    con,
    paste0("PRAGMA table_info(", matches_tbl, ");")
  )$name
  
  if (!"id" %in% matches_cols) {
    stop("`matches` must contain a column named 'id'", call. = FALSE)
  }
  
  # Generate output table name
  out_name <- paste0("_joinery_tmp_unmatched_", sample.int(1e9, 1))
  
  # SQL anti-join: keep rows where id NOT IN matched set
  sql <- paste0(
    "CREATE TABLE ", out_name, " AS\n",
    "SELECT d.*\n",
    "FROM ", data_tbl, " AS d\n",
    "WHERE d.", id_q, " NOT IN (\n",
    "  SELECT DISTINCT id\n",
    "  FROM ", matches_tbl, "\n",
    ");"
  )
  
  DBI::dbExecute(con, sql)
  dplyr::tbl(con, out_name)
}
  
  
# Method: multi_stage_match for DuckDB
#------------------------------------------------------------------------------
# Runs multiple search strategies in sequence, extracting unmatched records
# between stages.
method(
  multi_stage_match,
  list(Duck_tbl, Duck_tbl, class_character, class_character, class_list)
) <- function(base_table,
              target_table,
              base_id,
              target_id,
              strategies,
              ...) {
  
  con <- base_table$src$con
  
  # ---- VALIDATION ----------------------------------------------------------
  c("strategies must be a list"    =  is.list(strategies),
    "strategies must not be empty" =  length(strategies) > 0) |> 
    validate_inputs()
  
  # If names missing: assign "strategy_1", "strategy_2", …
  if (is.null(names(strategies)) || any(names(strategies) == "")) {
    names(strategies) <- paste0("strategy_", seq_along(strategies))
  }
  
  # Ensure all elements are Search_Strategy
  c("strategies must be a list of Search_Strategy objects" = 
      is.list(strategies) && all(sapply(strategies, S7_inherits, Search_Strategy))
  ) |> validate_inputs()
  
  # ---- PREP ----------------------------------------------------------------
  base_res   <- base_table
  target_res <- target_table
  
  all_matches   <- list()
  match_counter <- 0L
  
  tmp <- function(prefix) paste0(prefix, "_", sample.int(1e9, 1))
  
  # ---- MAIN LOOP -----------------------------------------------------------
  for (stage_name in names(strategies)) {
    strategy <- strategies[[stage_name]]
    
    # Run stage matching
    stage_matches <- search_candidates(
      base_res,
      target_res,
      base_id,
      target_id,
      strategy = strategy
    )
    
    stage_tbl <- stage_matches$lazy_query$x
    
    # Check if any matches found
    n_matches <- DBI::dbGetQuery(
      con,
      paste0("SELECT COUNT(*) AS n FROM ", stage_tbl)
    )$n
    
    if (n_matches > 0) {
      # Create new table with stage label and adjusted match_id
      staged_tbl <- tmp("_joinery_tmp_staged")
      
      sql_stage <- paste0(
        "CREATE TEMP TABLE ", staged_tbl, " AS\n",
        "SELECT\n",
        "  match_id + ", match_counter, " AS match_id,\n",
        "  score,\n",
        "  '", stage_name, "' AS stage,\n",
        "  source,\n",
        "  id,\n",
        "  rank,\n",
        "  * EXCLUDE (match_id, score, source, id, rank)\n",
        "FROM ", stage_tbl, ";"
      )
      
      DBI::dbExecute(con, sql_stage)
      
      # Update match counter
      match_counter <- DBI::dbGetQuery(
        con,
        paste0("SELECT MAX(match_id) AS max_id FROM ", staged_tbl)
      )$max_id
      
      all_matches[[stage_name]] <- staged_tbl
      
      # Remove matched rows (per side)
      base_res <- extract_unmatched(
        base_res,
        base_id,
        dplyr::tbl(con, staged_tbl) |> dplyr::filter(source == "base")
      )
      
      target_res <- extract_unmatched(
        target_res,
        target_id,
        dplyr::tbl(con, staged_tbl) |> dplyr::filter(source == "target")
      )
      
      # Check if either side is empty
      base_tbl <- base_res$lazy_query$x
      target_tbl <- target_res$lazy_query$x
      
      n_base <- DBI::dbGetQuery(
        con,
        paste0("SELECT COUNT(*) AS n FROM ", base_tbl)
      )$n
      
      n_target <- DBI::dbGetQuery(
        con,
        paste0("SELECT COUNT(*) AS n FROM ", target_tbl)
      )$n
      
      # Stop if one side is empty
      if (n_base == 0L || n_target == 0L) break
    }
  }
  
  # ---- RETURN --------------------------------------------------------------
  if (length(all_matches) == 0L) {
    # Empty-structure return (schema only)
    out_name <- tmp("_joinery_tmp_multistage")
    DBI::dbExecute(
      con,
      paste0(
        "CREATE TABLE ", out_name, " AS\n",
        "SELECT\n",
        "  CAST(NULL AS INTEGER) AS match_id,\n",
        "  CAST(NULL AS DOUBLE) AS score,\n",
        "  CAST(NULL AS VARCHAR) AS stage,\n",
        "  CAST(NULL AS VARCHAR) AS source,\n",
        "  CAST(NULL AS VARCHAR) AS id,\n",
        "  CAST(NULL AS INTEGER) AS rank\n",
        "LIMIT 0;"
      )
    )
    return(dplyr::tbl(con, out_name))
  }
  
  # Union all stage results
  out_name <- tmp("_joinery_tmp_multistage")
  
  union_parts <- paste(
    paste0("SELECT * FROM ", all_matches),
    collapse = "\nUNION ALL\n"
  )
  
  sql_final <- paste0(
    "CREATE TABLE ", out_name, " AS\n",
    union_parts, "\n",
    "ORDER BY match_id, stage, source, rank;"
  )
  
  DBI::dbExecute(con, sql_final)
  dplyr::tbl(con, out_name)
}


# Method: .inspect_tokens for DuckDB
#------------------------------------------------------------------------------
method(
  .inspect_tokens,
  list(Duck_tbl, class_character, Search_Strategy, class_character)
) <- function(data, id, strategy, column) {
  
  con <- data$src$con
  data_tbl <- data$lazy_query$x
  
  id_q <- sprintf('"%s"', id)
  column_q <- sprintf('"%s"', column)
  
  # --- Validate inputs -----------------------------------------------------
  data_cols <- DBI::dbGetQuery(
    con,
    paste0("PRAGMA table_info(", data_tbl, ");")
  )$name
  
  if (!id %in% data_cols) {
    stop(sprintf("ID column '%s' not found in data", id), call. = FALSE)
  }
  if (!column %in% data_cols) {
    stop(sprintf("Column '%s' not found in data", column), call. = FALSE)
  }
  if (!column %in% names(strategy@preparers)) {
    stop(sprintf("Column '%s' not found in strategy preparers", column), call. = FALSE)
  }
  
  # --- 1. Create single-column strategy for efficiency ---------------------
  single_col_strategy <- copy(strategy)
  single_col_strategy@preparers <- list(strategy@preparers[[column]])
  names(single_col_strategy@preparers) <- column
  
  # --- 2. Prepare tokens via joinery's interpreter -------------------------
  tokens <- prepare_search_data(
    data     = data,
    id       = id,
    strategy = single_col_strategy
  )
  
  tokens_tbl <- tokens$lazy_query$x
  
  # --- 3. Join back to original data and count occurrences -----------------
  out_name <- paste0("_joinery_tmp_inspect_", sample.int(1e9, 1))
  
  sql <- paste0(
    "CREATE TABLE ", out_name, " AS\n",
    "SELECT\n",
    "  t.token,\n",
    "  d.", column_q, " AS ", column_q, ",\n",
    "  COUNT(*) AS n\n",
    "FROM ", tokens_tbl, " t\n",
    "LEFT JOIN ", data_tbl, " d\n",
    "  ON t.", id_q, " = d.", id_q, "\n",
    "GROUP BY t.token, d.", column_q, "\n",
    "ORDER BY n DESC, t.token;"
  )
  
  DBI::dbExecute(con, sql)
  dplyr::tbl(con, out_name)
}
  

#' Drop all temporary DuckDB tables created by joinery
#'
#' Removes ephemeral tables generated during batch preprocessing steps
#' (for example token tables created by `prepare_search_data()`).
#' These tables follow a reserved prefix convention (such as
#' `"_joinery_tokens_"` or `"_joinery_tmp_"`) and are safe to delete.
#'
#' This function does not touch user tables. Only tables whose names
#' begin with one of the specified prefixes are removed. Additional
#' prefixes can be supplied to support future temporary table types.
#'
#' @param con A DuckDB connection.
#' @param prefixes Character vector of table name prefixes that identify
#'   joinery temporary tables. Defaults cover all current ephemeral
#'   table types.
#'
#' @return A character vector of removed table names, invisibly.
#'
#' @examples
#' \dontrun{
#'   # List all tables
#'   dbListTables(con)
#'
#'   # Remove all temporary joinery tables
#'   drop_joinery_temp_tables(con)
#' }
#'
drop_joinery_temp_tables <- function(
    con,
    prefixes = c("_joinery_tokens_", "_joinery_tmp_")
) {
  existing <- DBI::dbListTables(con)
  
  to_drop <- unlist(lapply(prefixes, function(pfx) {
    grep(paste0("^", pfx), existing, value = TRUE)
  }))
  
  if (length(to_drop) == 0) {
    return(invisible(character()))
  }
  
  for (tbl in to_drop) {
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tbl))
  }
  
  invisible(to_drop)
}
