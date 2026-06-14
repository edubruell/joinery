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
              debug = FALSE,
              control = duckdb_control()) {


  con <- base_table$src$con
  base_id_q   <- sprintf('"%s"', base_id)
  target_id_q <- sprintf('"%s"', target_id)
  # Materialise filtered lazy inputs so the final-join SQL that reads
  # `<table>$lazy_query$x` as a table name doesn't break.
  base_table   <- .materialise_duck_input(base_table, con)
  target_table <- .materialise_duck_input(target_table, con)

  block_by <- strategy@block_by %||% NULL
  tmp <- function(prefix) paste0(prefix, "_", sample.int(1e9, 1))

  # ==========================================================================
  # run_core: the scoring core (tokenise -> rarity -> score -> match_id ->
  # long -> merge -> rank). Run once for the monolithic path, or once per
  # block-atomic chunk. `match_id_offset` makes match_id globally unique across
  # chunks; returns the chunk's output table + its max match_id + pair count.
  # ==========================================================================
  run_core <- function(base_table, target_table, base_tokens, target_tokens,
                       match_id_offset = 0L) {

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
  
  # Pre-join cut: rarity floor + document-frequency cap, in one predicate,
  # before .score_pairs_sql() does the (src_column, token, block) cross-join.
  prefilter <- .rarity_prefilter_sql(strategy)
  if (!is.null(prefilter)) {
    sql_filter <- paste0(
      "CREATE OR REPLACE TABLE ", enriched_tbl, " AS\n",
      "SELECT * FROM ", enriched_tbl, " WHERE ", prefilter, ";\n"
    )
    DBI::dbExecute(con, sql_filter)
  }

  # Bound the cross-join: auto-cap (or abort on) hyper-common tokens that would
  # fan a dense block into a quadratic overlap join. Same df-ceiling cut as the
  # data.table backend, decided from a pairs-free df histogram.
  fanout_cut <- .fanout_guard_sql(con, enriched_tbl, strategy, face = "cross")
  if (is.finite(fanout_cut)) {
    DBI::dbExecute(con, paste0(
      "CREATE OR REPLACE TABLE ", enriched_tbl, " AS\n",
      "SELECT * FROM ", enriched_tbl, " WHERE df <= ", fanout_cut, ";\n"))
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
    return(list(out = out_name, max_id = match_id_offset, n_pairs = 0L))
  }
  
  
  #----------------------------------------------------------
  # 6. Assign match IDs (scored_pairs_tbl uses id1/id2 from helper)
  #----------------------------------------------------------
  matched_tbl <- tmp("_joinery_tmp_matched")
  
  sql_matched <- paste0(
    "CREATE TEMP TABLE ", matched_tbl, " AS\n",
    "SELECT\n",
    "  ROW_NUMBER() OVER (ORDER BY id1, id2) + ", match_id_offset, " AS match_id,\n",
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

  # max match_id for the global re-key; n_pairs already computed above.
  max_id <- DBI::dbGetQuery(
    con,
    paste0("SELECT COALESCE(MAX(match_id), ", match_id_offset, ") AS m FROM ", out_name)
  )$m
  list(out = out_name, max_id = max_id, n_pairs = n_pairs)
  }
  # ======================= end run_core =====================================

  # --------------------------------------------------------------------------
  # Decide monolithic vs block-atomic chunked execution.
  # Chunking re-tokenises per chunk, so it is only available when tokens are not
  # pre-supplied (the base_tokens/target_tokens test bypass forces monolithic).
  # --------------------------------------------------------------------------
  can_chunk <- is.null(base_tokens) && is.null(target_tokens)

  chunk_unit <- if (can_chunk) {
    n_base <- DBI::dbGetQuery(
      con, paste0("SELECT COUNT(*) AS n FROM ", base_table$lazy_query$x))$n
    .resolve_chunk_unit(control@chunk_by, block_by, n_base)
  } else {
    if (is.character(control@chunk_by)) {
      cli::cli_warn(
        "{.arg chunk_by} is ignored when pre-computed tokens are supplied; running monolithic."
      )
    }
    NULL
  }

  # ---- Monolithic path (unchanged behaviour) -------------------------------
  if (is.null(chunk_unit)) {
    res <- run_core(base_table, target_table, base_tokens, target_tokens, 0L)
    return(dplyr::tbl(con, res$out))
  }

  # ---- Block-atomic chunked path -------------------------------------------
  plan <- duckdb_batch_plan(
    db_tbl            = base_table,
    id                = base_id,
    target_batch_size = control@target_batch_size,
    min_batch_size    = control@min_batch_size,
    chunk_strategy    = "block_consolidated",
    block_by          = chunk_unit,
    atomic_blocks     = TRUE
  )

  n_chunks <- nrow(plan)
  master   <- tmp("_joinery_tmp_candidates")
  offset   <- 0
  first    <- TRUE
  fails    <- .new_chunk_failure_log()

  for (i in seq_len(n_chunks)) {
    tuples  <- plan$blocks[[i]]
    where   <- .block_tuples_where(con, tuples)
    key_lbl <- .block_tuples_label(tuples)

    if (!isFALSE(control@progress)) {
      cli::cli_inform(
        "chunk {i}/{n_chunks} (key={key_lbl}, rows={plan$row_count[i]})",
        .auto_close = TRUE
      )
    }

    # Bracket the chunk's table creations so a mid-chunk failure can be cleaned
    # up completely: run_core (and the prepare_search_data it calls) creates
    # intermediate temps that are only dropped on its success path. On error we
    # drop everything that appeared since this snapshot, except the accumulator
    # `master` ([[feedback_drop_joinery_temp_tables]] — true isolation).
    before_tbls <- .list_duck_tables(con)
    run_chunk <- function() {
      base_slice   <- .slice_duck_tbl(con, base_table,   where, tmp("_joinery_chunk_base"))
      target_slice <- .slice_duck_tbl(con, target_table, where, tmp("_joinery_chunk_target"))
      run_core(dplyr::tbl(con, base_slice), dplyr::tbl(con, target_slice),
               NULL, NULL, offset)
    }
    drop_chunk_temps <- function() .drop_tables_since(con, before_tbls, keep = master)

    res <- tryCatch(
      run_chunk(),
      error = function(e) {
        drop_chunk_temps()
        if (control@on_error == "retry") {
          # one conservative retry before giving up
          res2 <- tryCatch(run_chunk(), error = function(e2) {
            drop_chunk_temps()
            fails <<- .log_chunk_failure(fails, i, key_lbl, "failed", conditionMessage(e2))
            NULL
          })
          return(res2)
        }
        if (control@on_error == "stop") {
          DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", master))
          cli::cli_abort(c(
            "Scoring chunk {i}/{n_chunks} (key={key_lbl}) failed under \\
             {.code on_error = \"stop\"}.",
            x = conditionMessage(e)
          ))
        }
        # default skip
        fails <<- .log_chunk_failure(fails, i, key_lbl, "skipped", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(res) && res$n_pairs > 0) {
      if (first) {
        DBI::dbExecute(con, paste0("CREATE TABLE ", master, " AS SELECT * FROM ", res$out))
        first <- FALSE
      } else {
        DBI::dbExecute(con, paste0("INSERT INTO ", master, " SELECT * FROM ", res$out))
      }
      offset <- res$max_id
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", res$out))
    } else if (!is.null(res)) {
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", res$out))
    }
    drop_chunk_temps()
  }

  # No chunk produced rows (all empty / skipped): emit the empty schema.
  if (first) {
    DBI::dbExecute(
      con,
      paste0(
        "CREATE TABLE ", master, " AS\n",
        "SELECT CAST(NULL AS INTEGER) AS match_id,\n",
        "       CAST(NULL AS DOUBLE) AS score,\n",
        "       CAST(NULL AS VARCHAR) AS source,\n",
        "       CAST(NULL AS VARCHAR) AS id,\n",
        "       CAST(NULL AS INTEGER) AS rank\n",
        "LIMIT 0;"
      )
    )
  }

  .summarise_chunk_failures(fails, n_chunks)

  result <- dplyr::tbl(con, master)
  attr(result, "failed_chunks") <- fails
  result
}
