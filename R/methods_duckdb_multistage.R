if (!requireNamespace("duckdb", quietly = TRUE) ||
    !requireNamespace("DBI", quietly = TRUE) ||
    !requireNamespace("dplyr", quietly = TRUE)) {
  return(invisible(NULL))
}

Duck_tbl <- new_S3_class("tbl_duckdb_connection")

  
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
    cli::cli_abort("ID column {.field {id}} not found in data")
  }

  # Validate: matches table has an 'id' column
  matches_cols <- DBI::dbGetQuery(
    con,
    paste0("PRAGMA table_info(", matches_tbl, ");")
  )$name

  if (!"id" %in% matches_cols) {
    cli::cli_abort("{.arg matches} must contain a column named {.field id}")
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
  if (!is.list(strategies) || length(strategies) == 0L) {
    cli::cli_abort("{.arg strategies} must be a non-empty list")
  }

  # If names missing: assign "strategy_1", "strategy_2", …
  if (is.null(names(strategies)) || any(names(strategies) == "")) {
    names(strategies) <- paste0("strategy_", seq_along(strategies))
  }

  # Ensure all elements are Search_Strategy or Embedding_Strategy
  valid_strategy <- function(s) S7_inherits(s, Search_Strategy) || S7_inherits(s, Embedding_Strategy)
  if (!all(map_lgl(strategies, valid_strategy))) {
    cli::cli_abort("{.arg strategies} must be a list of {.cls Search_Strategy} or {.cls Embedding_Strategy} objects")
  }
  
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
      
      # Materialize per-side filters into temp tables so extract_unmatched()
      # receives a real backing table (it inspects the lazy_query$x name via
      # PRAGMA table_info, which fails on composed lazy queries).
      base_match_tbl   <- tmp("_joinery_tmp_stage_base")
      target_match_tbl <- tmp("_joinery_tmp_stage_target")
      DBI::dbExecute(con, paste0(
        "CREATE TEMP TABLE ", base_match_tbl,
        " AS SELECT * FROM ", staged_tbl, " WHERE source = 'base';"
      ))
      DBI::dbExecute(con, paste0(
        "CREATE TEMP TABLE ", target_match_tbl,
        " AS SELECT * FROM ", staged_tbl, " WHERE source = 'target';"
      ))

      # Remove matched rows (per side)
      base_res <- extract_unmatched(
        base_res, base_id, dplyr::tbl(con, base_match_tbl)
      )
      target_res <- extract_unmatched(
        target_res, target_id, dplyr::tbl(con, target_match_tbl)
      )

      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", base_match_tbl, ";"))
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", target_match_tbl, ";"))
      
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
