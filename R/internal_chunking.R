# ============================================================
# Shared scoring-chunk orchestration helpers (v0.8 Stage 05)
# ============================================================
#
# Cross-cutting bits the DuckDB scoring-chunk loop (search_candidates) and the
# Stage 07 multi-stage search reuse: resolve the chunk *unit* from a
# Duckdb_Control + the strategy's block_by (enforcing the block-atomic subset
# rule), and accumulate / summarise per-chunk failures. The per-chunk SQL
# execution itself stays inlined in the DuckDB method (consistent with how the
# §14 dedup loop is inlined) — only the orchestration / accounting is shared.
#
# Block-atomic rule: a pair only forms WITHIN a block, so a scoring chunk must
# consist of *whole* blocks. The "chunk unit" is the set of columns whose
# distinct tuples are the indivisible atoms the planner packs under a budget.
# It must be a subset of block_by, or cross-chunk pairs are silently dropped.
# ============================================================

# Below this row count an auto (chunk_by = NULL) input runs monolithic, so
# small inputs / the test suite are unaffected. Above it, auto chunks on the
# full block_by as the atomic unit.
.CHUNK_AUTO_THRESHOLD <- 2e6


#' Resolve the scoring chunk unit
#'
#' Turns a `Duckdb_Control`'s `chunk_by` plus the strategy's `block_by` into the
#' set of columns the scoring chunker treats as indivisible atoms (fed to
#' `duckdb_batch_plan(atomic_blocks = TRUE)`), or `NULL` to run monolithic.
#'
#' Enforces the block-atomic subset rule and the no-`block_by` degrade policy
#' **before any work runs**.
#'
#' @param chunk_by `NULL` (auto), `FALSE` (force monolithic), or a character
#'   vector (explicit unit).
#' @param block_by The strategy's blocking columns (`NULL`/empty allowed).
#' @param total_rows Total rows of the larger input, for the auto threshold.
#'
#' @return `NULL` (run monolithic) or a character vector chunk unit.
#'
#' @noRd
.resolve_chunk_unit <- function(chunk_by, block_by, total_rows) {
  block_by <- block_by %||% character()

  # Explicit FALSE -> always monolithic.
  if (isFALSE(chunk_by)) return(NULL)

  # No block_by: chunking is impossible without dropping cross-chunk pairs.
  if (length(block_by) == 0L) {
    if (is.null(chunk_by)) {
      if (isTRUE(total_rows >= .CHUNK_AUTO_THRESHOLD)) {
        cli::cli_warn(c(
          "Input has {prettyNum(total_rows, big.mark = ',')} rows but the strategy \\
           has no {.arg block_by}; running scoring monolithic.",
          i = "Chunking off a non-block key would silently drop cross-chunk pairs. \\
               Add a {.arg block_by} to bound the scoring intermediate."
        ))
      }
      return(NULL)
    }
    cli::cli_abort(c(
      "{.arg chunk_by} was supplied but the strategy has no {.arg block_by}.",
      x = "Chunking off a non-block key would silently drop cross-chunk pairs.",
      i = "Add a {.arg block_by} to the strategy, or set {.code chunk_by = FALSE}."
    ))
  }

  # Auto: monolithic below the threshold, else the full block_by is the atom.
  if (is.null(chunk_by)) {
    if (isTRUE(total_rows < .CHUNK_AUTO_THRESHOLD)) return(NULL)
    return(block_by)
  }

  # Explicit chunk_by must be a subset of block_by.
  if (!all(chunk_by %in% block_by)) {
    bad <- setdiff(chunk_by, block_by)
    cli::cli_abort(c(
      "{.arg chunk_by} must be a subset of the strategy's {.arg block_by}.",
      x = "{.val {bad}} {?is/are} not in {.arg block_by} ({.val {block_by}}).",
      i = "Chunking off a non-block key would silently drop cross-chunk pairs."
    ))
  }
  chunk_by
}


#' Empty per-chunk failure log
#'
#' One row per chunk that did **not** complete (skipped or failed). Attached to
#' a chunked result as `attr(result, "failed_chunks")`. A compact record - not
#' the full per-block audit table - kept because failure isolation is
#' meaningless if the caller can't tell what was skipped.
#'
#' @noRd
.new_chunk_failure_log <- function() {
  data.table::data.table(
    chunk_id  = integer(),
    chunk_key = character(),
    status    = character(),   # "skipped" | "failed"
    message   = character()
  )
}


#' Append a failure record
#' @noRd
.log_chunk_failure <- function(log, chunk_id, chunk_key, status, message) {
  data.table::rbindlist(list(
    log,
    data.table::data.table(
      chunk_id  = as.integer(chunk_id),
      chunk_key = as.character(chunk_key),
      status    = status,
      message   = message
    )
  ))
}


#' Build a WHERE predicate selecting a chunk's block tuples
#'
#' `tuples` is a list of block tuples (each a named list `col = value`), as
#' carried in a `duckdb_batch_plan(atomic_blocks = TRUE)` plan row's `blocks`
#' cell. Produces `(c1 = v1 AND c2 = v2) OR (…)`, with `IS NULL` for `NA`.
#'
#' @noRd
.block_tuples_where <- function(con, tuples) {
  ors <- vapply(tuples, function(tp) {
    conds <- vapply(names(tp), function(col) {
      v <- tp[[col]]
      if (length(v) == 0L || is.na(v)) {
        paste0('"', col, '" IS NULL')
      } else {
        paste0('"', col, '" = ', DBI::dbQuoteLiteral(con, v))
      }
    }, character(1))
    paste0("(", paste(conds, collapse = " AND "), ")")
  }, character(1))
  paste(ors, collapse = " OR ")
}


#' Short human label for a chunk's block tuples
#' @noRd
.block_tuples_label <- function(tuples) {
  tp  <- tuples[[1]]
  lbl <- paste(names(tp), vapply(tp, function(v) {
    if (length(v) == 0L || is.na(v)) "NA" else as.character(v)
  }, character(1)), sep = "=", collapse = ",")
  if (length(tuples) > 1L) lbl <- paste0(lbl, " +", length(tuples) - 1L, " more")
  lbl
}


#' Materialise a chunk slice of a DuckDB table into a temp table
#'
#' @return The temp table name (caller wraps `dplyr::tbl()` and drops it).
#' @noRd
.slice_duck_tbl <- function(con, src_tbl, where, name) {
  src <- src_tbl$lazy_query$x
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE ", name, " AS SELECT * FROM ", src, " WHERE ", where, ";"
  ))
  name
}


#' Snapshot the connection's table names
#'
#' Used to bracket a chunk's execution so any tables it leaves behind on the
#' error path can be dropped (true isolation: a failed chunk must not leak its
#' intermediate temps). `duckdb_tables()`
#' lists both `TEMP` and regular tables.
#'
#' @noRd
.list_duck_tables <- function(con) {
  DBI::dbGetQuery(con, "SELECT table_name FROM duckdb_tables()")$table_name
}


#' Drop tables created since a snapshot, except a keep-set
#'
#' The cleanup complement of [.list_duck_tables()]: drop every table that
#' appeared after `before` was taken, minus `keep` (the accumulator the chunk
#' loop must preserve across iterations). Run on a chunk's error path so the
#' partial `_joinery_*` / prepare temps a failed `run_core` left behind don't
#' survive the run.
#'
#' @noRd
.drop_tables_since <- function(con, before, keep = character()) {
  orphans <- setdiff(.list_duck_tables(con), c(before, keep))
  for (tbl in orphans) {
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tbl))
  }
  invisible(orphans)
}


#' Final loud summary of skipped / failed chunks
#'
#' A default `on_error = "skip"` must still be loud, so a silent mass-skip can't
#' pass for success.
#'
#' @noRd
.summarise_chunk_failures <- function(log, n_chunks) {
  if (nrow(log) == 0L) return(invisible())
  n_skip <- sum(log$status == "skipped")
  n_fail <- sum(log$status == "failed")
  cli::cli_warn(c(
    "!" = "{nrow(log)} of {n_chunks} chunk{?s} did not complete \\
           ({n_skip} skipped, {n_fail} failed).",
    i = "See {.code attr(result, \"failed_chunks\")} for keys and messages."
  ))
  invisible()
}
