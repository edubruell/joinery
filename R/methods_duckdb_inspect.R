if (!requireNamespace("duckdb", quietly = TRUE) ||
    !requireNamespace("DBI", quietly = TRUE) ||
    !requireNamespace("dplyr", quietly = TRUE)) {
  return(invisible(NULL))
}

Duck_tbl <- new_S3_class("tbl_duckdb_connection")


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
    cli::cli_abort("ID column {.field {id}} not found in data")
  }
  if (!column %in% data_cols) {
    cli::cli_abort("Column {.field {column}} not found in data")
  }
  if (!column %in% names(strategy@preparers)) {
    cli::cli_abort("Column {.field {column}} not found in strategy preparers")
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
    prefixes = c("_joinery_tokens_", "_joinery_tmp_", "_joinery_emb_")
) {
  existing <- DBI::dbListTables(con)
  
  to_drop <- unlist(map(prefixes, function(pfx) {
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
