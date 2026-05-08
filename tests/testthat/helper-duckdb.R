# DuckDB Test Helpers
#
# Helper functions for testing DuckDB preparers and backend methods.
# These fixtures create temporary DuckDB connections and tables for testing.
#
# Only loaded when duckdb is available.

if (requireNamespace("duckdb", quietly = TRUE) && 
    requireNamespace("dplyr", quietly = TRUE)) {
  
  #' Create a temporary DuckDB connection
  #'
  #' Creates an in-memory DuckDB connection that automatically closes
  #' when the test finishes.
  #'
  #' @param env Environment for deferred cleanup (default: parent.frame())
  #' @return DuckDB connection object
  local_duckdb_con <- function(env = parent.frame()) {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
    con
  }
  
  
  #' Create a DuckDB table from data
  #'
  #' Uploads a data.frame/tibble to a temporary DuckDB table.
  #' The connection is automatically created and cleaned up.
  #'
  #' @param data Data frame or tibble to upload
  #' @param table_name Name for the DuckDB table (default: "test_table")
  #' @param env Environment for deferred cleanup (default: parent.frame())
  #' @return dplyr tbl_duckdb_connection object
  local_duckdb_table <- function(data, 
                                  table_name = "test_table",
                                  env = parent.frame()) {
    con <- local_duckdb_con(env = env)
    dplyr::copy_to(con, data, name = table_name, temporary = TRUE)
  }
  
  
  #' Prepare token tables via both backends for comparison
  #'
  #' Runs prepare_search_data() on small data via the data.table backend and
  #' sets up a matching DuckDB connection+table. The pre-computed token table
  #' can then be injected into DuckDB methods via base_tokens= to exercise the
  #' SQL scoring paths without going through batch preprocessing.
  #'
  #' @param data Data frame or data.table to process
  #' @param id Character. ID column name
  #' @param strategy Search_Strategy object
  #' @param env Environment for deferred cleanup (default: parent.frame())
  #' @return List with dt_tokens (data.table), duck_tbl (tbl_duckdb_connection),
  #'         and con (DuckDB connection)
  compare_backends <- function(data, id, strategy, env = parent.frame()) {
    dt_tokens <- prepare_search_data(data.table::as.data.table(data), id, strategy)

    con <- local_duckdb_con(env = env)
    tbl_name <- paste0("src_", sample.int(1e9, 1))
    DBI::dbWriteTable(con, tbl_name, as.data.frame(data))
    duck_tbl <- dplyr::tbl(con, tbl_name)

    list(dt_tokens = dt_tokens, duck_tbl = duck_tbl, con = con)
  }
  
  
  #' Execute SQL and return results as data.frame
  #'
  #' Convenience wrapper for testing SQL expressions.
  #'
  #' @param con DuckDB connection
  #' @param sql SQL query string
  #' @return data.frame with query results
  query_duckdb <- function(con, sql) {
    DBI::dbGetQuery(con, sql)
  }
  
}
