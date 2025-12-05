if (requireNamespace("duckdb", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("dplyr", quietly = TRUE)) {
    return(invisible(NULL))
}

Duck_tbl <- new_S3_class("tbl_duckdb_connection")


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
  # 2. Build weight CASE expression
  # ----------------------------------------------------------
  if (is.null(weights)) {
    if (length(strategy@weights)) {
      weights <- strategy@weights
    } else {
      cols <- names(strategy@preparers)
      weights <- rep_len(1 / length(cols), length(cols))
      names(weights) <- cols
    }
  }
  
  weight_expr <- paste0(
    "CASE src_column ",
    paste(sprintf("WHEN '%s' THEN %g", names(weights), weights), collapse = " "),
    " ELSE 1.0 END"
  )
  
  block_by <- strategy@block_by %||% character()
  thr      <- strategy@threshold
  if (is.null(thr)) stop("Strategy must define a threshold.", call. = FALSE)
  
  block_join <- if (length(block_by)) {
    paste(sprintf("AND t1.\"%s\" = t2.\"%s\"", block_by, block_by), collapse = "\n")
  } else ""
  
  # ----------------------------------------------------------
  # 3. TEMP: enriched tokens (weight + rIP)
  # ----------------------------------------------------------
  enriched_tbl <- tmp("_joinery_tmp_tok_enriched")
  sql_enriched <- paste0(
    "CREATE TEMP TABLE ", enriched_tbl, " AS\n",
    "SELECT *,\n",
    "  ", weight_expr, " AS weight,\n",
    "  rarity / SUM(rarity) OVER (PARTITION BY ", id_q, ", src_column) AS rip\n",
    "FROM ", tbl_tokens, ";\n"
  )
  DBI::dbExecute(con, sql_enriched)
  
  # ----------------------------------------------------------
  # 4. TEMP: scored_pairs
  # ----------------------------------------------------------
  pairs_tbl <- tmp("_joinery_tmp_pairs")
  sql_pairs <- paste0(
    "CREATE TEMP TABLE ", pairs_tbl, " AS\n",
    "SELECT t1.", id_q, " AS id1,\n",
    "       t2.", id_q, " AS id2,\n",
    "       SUM(t1.rip * t1.weight) AS score\n",
    "FROM ", enriched_tbl, " t1\n",
    "JOIN ", enriched_tbl, " t2\n",
    "  ON t1.src_column = t2.src_column\n",
    " AND t1.token = t2.token\n",
    if (block_join != "") paste0(" ", block_join, "\n") else "",
    "WHERE t1.", id_q, " <> t2.", id_q, "\n",
    "GROUP BY id1, id2\n",
    "HAVING SUM(t1.rip * t1.weight) >= ", thr, ";\n"
  )
  DBI::dbExecute(con, sql_pairs)
  
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
    walk(c(enriched_tbl, pairs_tbl, edges_tbl, comp_tbl, best_tbl), function(tbl) {
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
#   4. Compute rIP (relative identification potential) and apply column weights
#   5. Generate candidate pairs by joining on identical tokens
#   6. Score each pair by summing rIP × weight over matching tokens
#   7. Filter pairs by similarity threshold from strategy
#   8. Assign match IDs and expand to long form
#   9. Merge the corresponding base and target data back in
#  10. Rank records within each match group
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
  # 5. Build weight CASE expression
  #----------------------------------------------------------
  if (is.null(weights)) {
    if (length(strategy@weights)) {
      weights <- strategy@weights
    } else {
      cols <- names(strategy@preparers)
      weights <- rep_len(1 / length(cols), length(cols))
      names(weights) <- cols
    }
  }
  
  weight_expr <- paste0(
    "CASE src_column ",
    paste(sprintf("WHEN '%s' THEN %g", names(weights), weights), collapse = " "),
    " ELSE 1.0 END"
  )
  
  thr <- strategy@threshold
  if (is.null(thr)) stop("Strategy must define a threshold.")
  
  
  if (length(block_by)) {
    block_join <- paste(sprintf("AND t1.\"%s\" = t2.\"%s\"", block_by, block_by), collapse = "\n")
  } else {
    block_join <- ""
  }
  
  
  #----------------------------------------------------------
  # 6. Enrich tokens (weight + rIP)
  #----------------------------------------------------------
  tokens_enriched_tbl <- tmp("_joinery_tmp_tokens_enriched")
  
  sql_enrich <- paste0(
    "CREATE TEMP TABLE ", tokens_enriched_tbl, " AS\n",
    "SELECT\n",
    "  *,\n",
    "  ", weight_expr, " AS weight,\n",
    "  rarity / SUM(rarity) OVER (\n",
    "      PARTITION BY doc_id, source, src_column\n",
    "  ) AS rip\n",
    "FROM ", enriched_tbl, ";\n"
  )
  
  DBI::dbExecute(con, sql_enrich)
  
  
  #----------------------------------------------------------
  # 7. Cross join on matching tokens and score pairs
  #----------------------------------------------------------
  scored_pairs_tbl <- tmp("_joinery_tmp_scored_pairs")
  
  sql_scored <- paste0(
    "CREATE TEMP TABLE ", scored_pairs_tbl, " AS\n",
    "SELECT\n",
    "  t1.doc_id AS base_id,\n",
    "  t2.doc_id AS target_id,\n",
    "  SUM(t1.rip * t1.weight) AS score\n",
    "FROM ", tokens_enriched_tbl, " t1\n",
    "JOIN ", tokens_enriched_tbl, " t2\n",
    "  ON t1.src_column = t2.src_column\n",
    " AND t1.token = t2.token\n",
    " AND t1.source = 'base'\n",
    " AND t2.source = 'target'\n",
    if (block_join != "") paste0(" ", block_join, "\n") else "",
    "GROUP BY base_id, target_id\n",
    "HAVING SUM(t1.rip * t1.weight) >= ", thr, ";\n"
  )
  
  DBI::dbExecute(con, sql_scored)
  
  
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
  # 8. Assign match IDs
  #----------------------------------------------------------
  matched_tbl <- tmp("_joinery_tmp_matched")
  
  sql_matched <- paste0(
    "CREATE TEMP TABLE ", matched_tbl, " AS\n",
    "SELECT\n",
    "  ROW_NUMBER() OVER (ORDER BY base_id, target_id) AS match_id,\n",
    "  base_id,\n",
    "  target_id,\n",
    "  score\n",
    "FROM ", scored_pairs_tbl, ";\n"
  )
  
  DBI::dbExecute(con, sql_matched)
  

  #----------------------------------------------------------
  # 9. Expand to long form
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
  # 10. Merge original base rows
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
  # 11. Merge original target rows
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
  # 12. Overwrite base_with_cols_tbl and target_with_cols_tbl so that
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
  # 13. Final union and ranking
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
    temp_tables <- c(
      union_tbl, enriched_tbl, tokens_enriched_tbl,
      scored_pairs_tbl, matched_tbl, long_tbl,
      base_with_cols_tbl, target_with_cols_tbl,
      tbl_base_tokens, tbl_target_tokens
    )
    purrr::walk(temp_tables, \(tbl) {
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tbl))
    })
  }
  
  dplyr::tbl(con, out_name)
}

  
  # ============================================================================
  # Method: extract_unmatched
  # ============================================================================
  #
  # Extracts records that did not match from a table.
  #
  # TODO: Implement this method.
  
  # method(
  #   extract_unmatched,
  #   list(Duck_tbl, class_character, Duck_tbl)
  # ) <- function(data, id, matches) {
  #   stop("DuckDB backend not yet implemented", call. = FALSE)
  # }
  
  
  # ============================================================================
  # Method: multi_stage_match
  # ============================================================================
  #
  # Runs multiple search strategies in sequence, extracting unmatched records
  # between stages.
  #
  # TODO: Implement this method. Can likely reuse generic implementation
  # if the other methods are properly implemented.
  
  # method(
  #   multi_stage_match,
  #   list(Duck_tbl, Duck_tbl, class_character, class_character, class_list)
  # ) <- function(base_table, target_table, base_id, target_id, strategies) {
  #   stop("DuckDB backend not yet implemented", call. = FALSE)
  # }
  

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
