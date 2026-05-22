if (!requireNamespace("duckdb", quietly = TRUE) ||
    !requireNamespace("DBI", quietly = TRUE) ||
    !requireNamespace("dplyr", quietly = TRUE)) {
  return(invisible(NULL))
}

Duck_tbl <- new_S3_class("tbl_duckdb_connection")



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

  # Allow callers to supply pre-computed data.frame/data.table token tables
  # instead of tbl_duckdb_connection objects. Used in tests to bypass batch
  # preprocessing and exercise the SQL scoring paths directly.
  if (!is.null(base_tokens) && !inherits(base_tokens, "tbl_duckdb_connection")) {
    tmp_tok <- paste0("_joinery_tokens_", sample.int(1e9, 1))
    DBI::dbWriteTable(con, tmp_tok, as.data.frame(base_tokens))
    base_tokens <- dplyr::tbl(con, tmp_tok)
  }
  if (!is.null(target_tokens) && !inherits(target_tokens, "tbl_duckdb_connection")) {
    tmp_tok <- paste0("_joinery_tokens_", sample.int(1e9, 1))
    DBI::dbWriteTable(con, tmp_tok, as.data.frame(target_tokens))
    target_tokens <- dplyr::tbl(con, tmp_tok)
  }

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
