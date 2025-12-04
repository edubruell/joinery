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
  
  
  #' Compare data.table and DuckDB preparer results
  #'
  #' Helper to test that a preparer produces equivalent results
  #' across data.table and DuckDB backends.
  #'
  #' @param data Input data frame
  #' @param preparer_fn Preparer function (e.g., normalize_text)
  #' @param col Column name to apply preparer to
  #' @param ... Additional arguments to preparer_fn
  #' @return List with dt_result and duckdb_result for comparison
  compare_backends <- function(data, preparer_fn, col, ...) {
    # data.table result
    dt_result <- preparer_fn(data[[col]], ...)
    
    # DuckDB result (to be implemented as preparers are built)
    # For now, returns NULL to signal not yet implemented
    duckdb_result <- NULL
    
    list(
      dt = dt_result,
      duckdb = duckdb_result
    )
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
