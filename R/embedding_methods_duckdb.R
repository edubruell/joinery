if (!requireNamespace("duckdb",   quietly = TRUE) ||
    !requireNamespace("DBI",      quietly = TRUE) ||
    !requireNamespace("dplyr",    quietly = TRUE) ||
    !requireNamespace("tidyllm",  quietly = TRUE)) {
  return(invisible(NULL))
}

# Internal: Ensure the DuckDB VSS extension is loaded
#
# Called once at the top of every DuckDB embedding method. Emits a user-facing
# error with install instructions if the extension is not available.
.ensure_vss <- function(con) {
  tryCatch(
    DBI::dbExecute(con, "LOAD vss;"),
    error = function(e) {
      stop(
        "The DuckDB 'vss' extension is required for embedding-based matching.\n",
        "Install it by running:\n",
        "  DBI::dbExecute(con, \"INSTALL vss;\")\n",
        "Then reconnect or reload with:\n",
        "  DBI::dbExecute(con, \"LOAD vss;\")",
        call. = FALSE
      )
    }
  )
}

# Internal: Add FLOAT[dim] embeddings column to a DuckDB table if absent
.ensure_embeddings_column <- function(con, table_name, dim) {
  cols <- DBI::dbGetQuery(
    con,
    paste0("PRAGMA table_info(", table_name, ");")
  )$name
  if (!"embeddings" %in% cols) {
    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE ", table_name,
        " ADD COLUMN embeddings FLOAT[", dim, "];"
      )
    )
  }
}


# Method: compute_embeddings for DuckDB tables
#------------------------------------------------------------------------------
# Populates an `embeddings FLOAT[dim]` column on the backing DuckDB table,
# then returns a lazy reference to (id, embeddings). Only records with a NULL
# embeddings value are re-embedded, so this is safe to call multiple times.
#
# Side-effect: alters the backing DuckDB table in place.
method(
  compute_embeddings,
  list(Duck_tbl, class_character, Embedding_Strategy)
) <- function(data, id, strategy) {

  con        <- data$src$con
  .ensure_vss(con)
  id_q       <- sprintf('"%s"', id)
  table_name <- data$lazy_query$x
  tmp        <- function(prefix) paste0(prefix, "_", sample.int(1e9, 1))

  # Validate blocking columns if specified
  block_by <- strategy@block_by
  if (!is.null(block_by)) {
    all_cols     <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(", table_name, ");"))$name
    missing_cols <- setdiff(block_by, all_cols)
    if (length(missing_cols) > 0) {
      stop(
        "Blocking columns not found in table: ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # Fetch all rows from DuckDB for text assembly
  full_data <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", table_name, ";"))

  assembled <- assemble_record_text(
    data    = full_data,
    id      = id,
    columns = strategy@columns,
    sep     = strategy@collapse_sep
  )

  # Skip records already embedded (e.g. second call in multi-stage)
  table_cols <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(", table_name, ");"))$name
  if ("embeddings" %in% table_cols) {
    already_embedded <- DBI::dbGetQuery(
      con,
      paste0("SELECT ", id_q, " FROM ", table_name, " WHERE embeddings IS NOT NULL;")
    )[[1]]
    assembled <- assembled[!assembled[[id]] %in% already_embedded, , drop = FALSE]
  }

  if (nrow(assembled) == 0L) {
    return(
      dplyr::tbl(con, table_name) |>
        dplyr::select(dplyr::all_of(c(id, "embeddings")))
    )
  }

  batch_size <- as.integer(strategy@batch_size)
  n_records  <- nrow(assembled)

  batch_starts  <- seq.int(1L, n_records, by = batch_size)
  batch_ends    <- pmin(batch_starts + batch_size - 1L, n_records)
  total_batches <- length(batch_starts)
  pnum          <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)

  cli::cli_alert_info(
    "Computing DuckDB Embeddings for {pnum(n_records)} records in {pnum(total_batches)} batches:"
  )

  dim_known <- FALSE
  dim       <- NULL

  for (i in seq_along(batch_starts)) {
    start <- batch_starts[[i]]
    end   <- batch_ends[[i]]

    batch_ids  <- assembled[[id]][start:end]
    batch_text <- assembled$text[start:end]

    emb_tbl  <- tidyllm::embed(batch_text, strategy@embedding_model)
    emb_vecs <- emb_tbl$embeddings

    if (strategy@normalize) {
      emb_vecs <- lapply(emb_vecs, function(vec) {
        norm <- sqrt(sum(vec^2))
        if (norm > 0) vec / norm else vec
      })
    }

    # First batch: detect dimension and ensure embeddings column exists
    if (!dim_known) {
      dim <- length(emb_vecs[[1L]])
      .ensure_embeddings_column(con, table_name, dim)
      dim_known <- TRUE
    }

    cli::cli_alert_info("Embedding Batch {pnum(i)}/{pnum(total_batches)}")

    # Serialize vectors to JSON strings; DuckDB casts VARCHAR → FLOAT[dim]
    emb_json <- vapply(emb_vecs, function(v) {
      paste0("[", paste(v, collapse = ","), "]")
    }, character(1L))

    batch_df <- data.frame(
      id       = batch_ids,
      emb_json = emb_json,
      stringsAsFactors = FALSE
    )
    names(batch_df)[[1L]] <- id

    temp_tbl <- tmp("_joinery_emb")
    DBI::dbWriteTable(con, temp_tbl, batch_df, temporary = TRUE, overwrite = TRUE)

    DBI::dbExecute(
      con,
      paste0(
        "UPDATE ", table_name, "\n",
        "SET embeddings = t.emb_json::FLOAT[", dim, "]\n",
        "FROM ", temp_tbl, " t\n",
        "WHERE ", table_name, ".", id_q, " = t.", id_q, ";\n"
      )
    )

    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", temp_tbl, ";"))
  }

  dplyr::tbl(con, table_name) |>
    dplyr::select(dplyr::all_of(c(id, "embeddings")))
}


# Method: search_candidates for DuckDB + Embedding_Strategy
#------------------------------------------------------------------------------
# Populates embeddings on both tables, runs VSS similarity search via
# array_cosine_distance, then expands to the standard long-form match schema.
method(
  search_candidates,
  list(Duck_tbl, Duck_tbl, class_character, class_character, Embedding_Strategy)
) <- function(base_table,
              target_table,
              base_id,
              target_id,
              strategy,
              threshold = NULL,
              weights   = NULL) {

  if (!is.null(weights)) {
    stop("Embedding strategies do not support weights", call. = FALSE)
  }

  con <- base_table$src$con
  .ensure_vss(con)

  thr         <- threshold %||% strategy@threshold
  base_id_q   <- sprintf('"%s"', base_id)
  target_id_q <- sprintf('"%s"', target_id)
  tmp         <- function(prefix) paste0(prefix, "_", sample.int(1e9, 1))

  # Populate embeddings in both backing tables (side-effecting)
  compute_embeddings(base_table,   base_id,   strategy)
  compute_embeddings(target_table, target_id, strategy)

  base_tbl   <- base_table$lazy_query$x
  target_tbl <- target_table$lazy_query$x

  block_by <- strategy@block_by
  block_conditions <- if (!is.null(block_by) && length(block_by) > 0) {
    sprintf("AND b.\"%s\" = t.\"%s\"", block_by, block_by)
  } else character(0L)

  #----------------------------------------------------------
  # 1. Score pairs via VSS
  #----------------------------------------------------------
  scored_tbl <- tmp("_joinery_emb_scored")

  where_parts <- c(
    paste0("(1.0 - array_cosine_distance(b.embeddings, t.embeddings)) >= ", thr),
    "b.embeddings IS NOT NULL",
    "t.embeddings IS NOT NULL",
    block_conditions
  )
  where_sql <- paste("WHERE", paste(where_parts, collapse = "\n  AND "))

  sql_scored <- paste0(
    "CREATE TEMP TABLE ", scored_tbl, " AS\n",
    "SELECT\n",
    "  b.", base_id_q, " AS base_id,\n",
    "  t.", target_id_q, " AS target_id,\n",
    "  1.0 - array_cosine_distance(b.embeddings, t.embeddings) AS score\n",
    "FROM ", base_tbl, " b\n",
    "CROSS JOIN ", target_tbl, " t\n",
    where_sql, ";\n"
  )

  DBI::dbExecute(con, sql_scored)

  # Empty early exit
  n_pairs <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", scored_tbl))$n
  if (n_pairs == 0L) {
    out_name <- tmp("_joinery_tmp_candidates")
    DBI::dbExecute(
      con,
      paste0(
        "CREATE TABLE ", out_name, " AS\n",
        "SELECT CAST(NULL AS INTEGER) AS match_id,\n",
        "       CAST(NULL AS DOUBLE)  AS score,\n",
        "       CAST(NULL AS VARCHAR) AS source,\n",
        "       CAST(NULL AS VARCHAR) AS id,\n",
        "       CAST(NULL AS INTEGER) AS rank\n",
        "LIMIT 0;"
      )
    )
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", scored_tbl))
    return(dplyr::tbl(con, out_name))
  }

  #----------------------------------------------------------
  # 2. Assign match IDs
  #----------------------------------------------------------
  matched_tbl <- tmp("_joinery_emb_matched")
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TEMP TABLE ", matched_tbl, " AS\n",
      "SELECT\n",
      "  ROW_NUMBER() OVER (ORDER BY base_id, target_id) AS match_id,\n",
      "  base_id,\n",
      "  target_id,\n",
      "  score\n",
      "FROM ", scored_tbl, ";\n"
    )
  )

  #----------------------------------------------------------
  # 3. Expand to long form
  #----------------------------------------------------------
  long_tbl <- tmp("_joinery_emb_long")
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TEMP TABLE ", long_tbl, " AS\n",
      "SELECT match_id, score, 'base'   AS source, base_id   AS id FROM ", matched_tbl, "\n",
      "UNION ALL\n",
      "SELECT match_id, score, 'target' AS source, target_id AS id FROM ", matched_tbl, ";\n"
    )
  )

  #----------------------------------------------------------
  # 4. Merge original columns (excluding id and embeddings)
  #----------------------------------------------------------
  base_merge_tbl   <- tmp("_joinery_emb_base_merge")
  target_merge_tbl <- tmp("_joinery_emb_target_merge")

  DBI::dbExecute(
    con,
    paste0(
      "CREATE TEMP TABLE ", base_merge_tbl, " AS\n",
      "SELECT l.match_id, l.score, l.source, l.id,\n",
      "       b.* EXCLUDE (", base_id_q, ", embeddings)\n",
      "FROM ", long_tbl, " l\n",
      "LEFT JOIN ", base_tbl, " b ON l.id = b.", base_id_q, "\n",
      "WHERE l.source = 'base';\n"
    )
  )

  DBI::dbExecute(
    con,
    paste0(
      "CREATE TEMP TABLE ", target_merge_tbl, " AS\n",
      "SELECT l.match_id, l.score, l.source, l.id,\n",
      "       t.* EXCLUDE (", target_id_q, ", embeddings)\n",
      "FROM ", long_tbl, " l\n",
      "LEFT JOIN ", target_tbl, " t ON l.id = t.", target_id_q, "\n",
      "WHERE l.source = 'target';\n"
    )
  )

  # Align column sets between base and target (handles schema mismatches)
  cols_base   <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(", base_merge_tbl,   ");"))$name
  cols_target <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(", target_merge_tbl, ");"))$name
  common_cols <- union(cols_base, cols_target)

  build_select <- function(cols_available) {
    paste(
      vapply(common_cols, function(col) {
        if (col %in% cols_available) col else paste0("NULL AS ", col)
      }, character(1L)),
      collapse = ", "
    )
  }

  DBI::dbExecute(con, paste0(
    "CREATE OR REPLACE TEMP TABLE ", base_merge_tbl, " AS\n",
    "SELECT ", build_select(cols_base), " FROM ", base_merge_tbl, ";\n"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE OR REPLACE TEMP TABLE ", target_merge_tbl, " AS\n",
    "SELECT ", build_select(cols_target), " FROM ", target_merge_tbl, ";\n"
  ))

  #----------------------------------------------------------
  # 5. Final union + ranking
  #----------------------------------------------------------
  out_name <- tmp("_joinery_tmp_candidates")
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE ", out_name, " AS\n",
      "SELECT\n",
      "  match_id, score, source, id,\n",
      "  ROW_NUMBER() OVER (\n",
      "    PARTITION BY match_id ORDER BY score DESC NULLS LAST\n",
      "  ) AS rank,\n",
      "  * EXCLUDE (match_id, score, source, id)\n",
      "FROM (\n",
      "  SELECT * FROM ", base_merge_tbl,   "\n",
      "  UNION ALL\n",
      "  SELECT * FROM ", target_merge_tbl, "\n",
      ")\n",
      "ORDER BY match_id, source, rank;\n"
    )
  )

  walk(c(scored_tbl, matched_tbl, long_tbl, base_merge_tbl, target_merge_tbl), function(tbl) {
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tbl))
  })

  dplyr::tbl(con, out_name)
}


# Method: detect_duplicates for DuckDB + Embedding_Strategy
#------------------------------------------------------------------------------
# Populates embeddings, runs a self-join via array_cosine_distance, then
# builds connected components using the same recursive CTE as the token backend.
method(
  detect_duplicates,
  list(Duck_tbl, class_character, Embedding_Strategy)
) <- function(base_table, id, strategy, threshold = NULL) {

  con <- base_table$src$con
  .ensure_vss(con)

  thr      <- threshold %||% strategy@threshold
  id_q     <- sprintf('"%s"', id)
  tmp      <- function(prefix) paste0(prefix, "_", sample.int(1e9, 1))

  # Populate embeddings (side-effecting)
  compute_embeddings(base_table, id, strategy)
  base_tbl <- base_table$lazy_query$x

  block_by <- strategy@block_by
  block_conditions <- if (!is.null(block_by) && length(block_by) > 0) {
    sprintf("AND a.\"%s\" = b.\"%s\"", block_by, block_by)
  } else character(0L)

  #----------------------------------------------------------
  # 1. Score self-pairs via VSS (each pair once: a.id < b.id)
  #----------------------------------------------------------
  pairs_tbl <- tmp("_joinery_emb_pairs")

  where_parts <- c(
    paste0("a.", id_q, " < b.", id_q),
    paste0("(1.0 - array_cosine_distance(a.embeddings, b.embeddings)) >= ", thr),
    "a.embeddings IS NOT NULL",
    "b.embeddings IS NOT NULL",
    block_conditions
  )
  where_sql <- paste("WHERE", paste(where_parts, collapse = "\n  AND "))

  sql_pairs <- paste0(
    "CREATE TEMP TABLE ", pairs_tbl, " AS\n",
    "SELECT\n",
    "  a.", id_q, " AS id1,\n",
    "  b.", id_q, " AS id2,\n",
    "  1.0 - array_cosine_distance(a.embeddings, b.embeddings) AS score\n",
    "FROM ", base_tbl, " a\n",
    "CROSS JOIN ", base_tbl, " b\n",
    where_sql, ";\n"
  )

  DBI::dbExecute(con, sql_pairs)

  # Empty early exit
  n_pairs <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", pairs_tbl))$n
  if (n_pairs == 0L) {
    out_name <- tmp("_joinery_tmp_dups")
    DBI::dbExecute(
      con,
      paste0(
        "CREATE TABLE ", out_name, " AS\n",
        "SELECT CAST(NULL AS VARCHAR)  AS id,\n",
        "       CAST(NULL AS INTEGER)  AS duplicate_group,\n",
        "       CAST(NULL AS DOUBLE)   AS score,\n",
        "       CAST(NULL AS INTEGER)  AS rank\n",
        "LIMIT 0;"
      )
    )
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", pairs_tbl))
    return(dplyr::tbl(con, out_name))
  }

  #----------------------------------------------------------
  # 2. Build bidirectional edge list
  #----------------------------------------------------------
  edges_tbl <- tmp("_joinery_emb_edges")
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TEMP TABLE ", edges_tbl, " AS\n",
      "SELECT id1 AS a, id2 AS b FROM ", pairs_tbl, "\n",
      "UNION ALL\n",
      "SELECT id2 AS a, id1 AS b FROM ", pairs_tbl, ";\n"
    )
  )

  #----------------------------------------------------------
  # 3. Connected components via recursive CTE
  #----------------------------------------------------------
  comp_tbl <- tmp("_joinery_emb_components")
  DBI::dbExecute(
    con,
    paste0(
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
  )

  #----------------------------------------------------------
  # 4. Best score per id
  #----------------------------------------------------------
  best_tbl <- tmp("_joinery_emb_best")
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TEMP TABLE ", best_tbl, " AS\n",
      "SELECT id, MAX(score) AS score\n",
      "FROM (\n",
      "  SELECT id1 AS id, score FROM ", pairs_tbl, "\n",
      "  UNION ALL\n",
      "  SELECT id2 AS id, score FROM ", pairs_tbl, "\n",
      ")\n",
      "GROUP BY id;\n"
    )
  )

  #----------------------------------------------------------
  # 5. Final result: components + scores + original columns + ranks
  #----------------------------------------------------------
  out_name <- tmp("_joinery_tmp_dups")
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE ", out_name, " AS\n",
      "SELECT c.id AS id,\n",
      "       DENSE_RANK() OVER (ORDER BY c.root) AS duplicate_group,\n",
      "       b.score,\n",
      "       ROW_NUMBER() OVER (\n",
      "         PARTITION BY c.root ORDER BY b.score DESC NULLS LAST\n",
      "       ) AS rank,\n",
      "       bt.* EXCLUDE (", id_q, ", embeddings)\n",
      "FROM ", comp_tbl, " c\n",
      "LEFT JOIN ", best_tbl,  " b  ON c.id = b.id\n",
      "LEFT JOIN ", base_tbl,  " bt ON c.id = bt.", id_q, "\n",
      "ORDER BY duplicate_group, rank;\n"
    )
  )

  walk(c(pairs_tbl, edges_tbl, comp_tbl, best_tbl), function(tbl) {
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tbl))
  })

  dplyr::tbl(con, out_name)
}
