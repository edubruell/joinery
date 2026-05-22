if (!requireNamespace("duckdb", quietly = TRUE) ||
    !requireNamespace("DBI", quietly = TRUE) ||
    !requireNamespace("dplyr", quietly = TRUE)) {
  return(invisible(NULL))
}

Duck_tbl <- new_S3_class("tbl_duckdb_connection")

  
  
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

  # Allow callers to supply a pre-computed data.frame/data.table token table
  # (e.g. from the data.table backend) instead of a DuckDB tbl. Used in
  # tests to bypass batch preprocessing and exercise the SQL scoring paths.
  if (!is.null(base_tokens) && !inherits(base_tokens, "tbl_duckdb_connection")) {
    tmp_tok <- paste0("_joinery_tokens_", sample.int(1e9, 1))
    DBI::dbWriteTable(con, tmp_tok, as.data.frame(base_tokens))
    base_tokens <- dplyr::tbl(con, tmp_tok)
  }

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
    "  UNION\n",
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
