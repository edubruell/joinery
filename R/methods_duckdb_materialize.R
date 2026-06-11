if (!requireNamespace("duckdb", quietly = TRUE) ||
    !requireNamespace("DBI", quietly = TRUE) ||
    !requireNamespace("dplyr", quietly = TRUE)) {
  return(invisible(NULL))
}

Duck_tbl <- new_S3_class("tbl_duckdb_connection")


# Build the semi-join CREATE TABLE statement. Factored out as a pure
# string-builder so the §20 contract — a temp-table JOIN, never an
# `id IN (<literal list>)` — is directly assertable in a unit test rather
# than only inferable from wall-clock timing.
.materialize_join_sql <- function(out_name, data_tbl, ids_tbl, id_q) {
  paste0(
    "CREATE TABLE ", out_name, " AS\n",
    "SELECT d.*\n",
    "FROM ", data_tbl, " AS d\n",
    "JOIN ", ids_tbl, " AS r\n",
    "  ON CAST(d.", id_q, " AS VARCHAR) = r.id;"
  )
}


# Method: materialize_records for DuckDB
#------------------------------------------------------------------------------
# Positive (semi-join) complement of extract_unmatched(). The ids are ALWAYS
# registered as a temp table and JOINed — never inlined as `id IN (<literal
# list>)`, which binds in ~O(n^2) and pins cores for minutes on a large
# residual set (the §20 footgun).
method(
  materialize_records,
  list(Duck_tbl, class_character)
) <- function(data, id, ids, ...) {

  con  <- data$src$con
  data <- .materialise_duck_input(data, con)

  data_tbl <- data$lazy_query$x
  id_q     <- sprintf('"%s"', id)

  # Validate: id column exists in data table
  data_cols <- DBI::dbGetQuery(
    con,
    paste0("PRAGMA table_info(", data_tbl, ");")
  )$name

  if (!id %in% data_cols) {
    cli::cli_abort("ID column {.field {id}} not found in data")
  }

  # Resolve `ids` into a temp table holding a single character column `id`.
  ids_tbl <- paste0("_joinery_ids_", sample.int(1e9, 1))

  if (inherits(ids, "tbl_duckdb_connection")) {
    # Already in the database — materialise to a bare table, find the id
    # column (lookup order: "id" first, then the `id`-named column), and
    # project a single CAST-to-VARCHAR `id` column. No collect-to-R.
    ids_in    <- .materialise_duck_input(ids, con)
    ids_in_tbl <- ids_in$lazy_query$x
    ids_cols  <- DBI::dbGetQuery(
      con, paste0("PRAGMA table_info(", ids_in_tbl, ");")
    )$name
    src_col <- if ("id" %in% ids_cols) {
      "id"
    } else if (id %in% ids_cols) {
      id
    } else {
      cli::cli_abort(
        "{.arg ids} table must contain a column named {.field id} or {.field {id}}"
      )
    }
    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE ", ids_tbl, " AS\n",
      "SELECT DISTINCT CAST(\"", src_col, "\" AS VARCHAR) AS id\n",
      "FROM ", ids_in_tbl, ";"
    ))
  } else {
    # An in-memory vector or table — extract the id values and write them.
    id_vals <- .materialize_id_values(ids, id)
    DBI::dbWriteTable(
      con, ids_tbl,
      data.frame(id = unique(as.character(id_vals)), stringsAsFactors = FALSE),
      temporary = TRUE
    )
  }

  # Semi-join via temp-table JOIN. CAST the corpus id to VARCHAR so a
  # BIGINT-corpus / character-id request still matches.
  out_name <- paste0("_joinery_tmp_materialize_", sample.int(1e9, 1))
  DBI::dbExecute(con, .materialize_join_sql(out_name, data_tbl, ids_tbl, id_q))

  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", ids_tbl, ";"))
  dplyr::tbl(con, out_name)
}
