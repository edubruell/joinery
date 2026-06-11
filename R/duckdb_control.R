# ============================================================
# Duckdb_Control class + duckdb_control() constructor
# ============================================================
#
# The single execution-control surface for the DuckDB backend. It carries
# every batching / chunking / failure knob and is threaded uniformly through
# the DuckDB methods (prepare_search_data, detect_duplicates,
# search_candidates, and the staged verbs). It subsumes the loose
# target_batch_size / min_batch_size / chunk_strategy arguments that the
# DuckDB methods used to take individually.
#
# DESIGN BOUNDARY (v0.8 Stage 05). Chunking is an *execution* concern —
# memory, backend, corpus scale — NOT a matching-semantics concern. The
# Search_Strategy is the data-independent semantic IR; the same strategy needs
# heavy chunking at 50M rows and none at 1k. So execution tuning lives here,
# never on the strategy. Two stages, two atomicity rules:
#   - preprocess batching (tokenization) is PER-ROW  -> any split is safe
#   - scoring chunking (overlap join) is BLOCK-ATOMIC -> a block is indivisible
# `chunk_by` drives the block-atomic scoring chunks; the batch-size knobs drive
# the per-row preprocess batches. DuckDB-only: the in-memory data.table backend
# never needed chunking.
# ============================================================


# ---------------------------------------------------------------------------
# Duckdb_Control class
# ---------------------------------------------------------------------------

#' DuckDB Execution-Control Class
#'
#' @description
#' An S7 class bundling the DuckDB backend's execution knobs (batch sizes,
#' scoring chunk key, per-chunk failure policy, progress). Construct it with
#' [duckdb_control()] and pass it as the `control =` argument to the DuckDB
#' methods. It carries no matching semantics — those live on the
#' [Search_Strategy].
#'
#' @slot target_batch_size NULL (auto-tune) or a positive number. Preprocess
#'   batch budget *and* the scoring chunk budget (rows of whole blocks packed
#'   per scoring chunk).
#' @slot min_batch_size NULL (auto-tune) or a positive number. Minimum table
#'   size before preprocess batching engages.
#' @slot chunk_strategy One of `"even"`, `"block_first"`, `"block_consolidated"`.
#'   Preprocess (per-row) chunking strategy only.
#' @slot chunk_by Scoring chunk key. `NULL` = auto-derive when large; `FALSE` =
#'   force monolithic; a character vector = explicit key (must be a subset of
#'   the strategy's `block_by`).
#' @slot on_error One of `"skip"`, `"retry"`, `"stop"`. Per-scoring-chunk
#'   failure policy.
#' @slot progress `NULL` (auto) / `TRUE` / `FALSE`. Force or suppress progress
#'   output.
#'
#' @seealso [duckdb_control()]
#'
#' @noRd
Duckdb_Control <- new_class(
  "Duckdb_Control",
  properties = list(
    target_batch_size = class_any,
    min_batch_size    = class_any,
    chunk_strategy    = class_character,
    chunk_by          = class_any,
    on_error          = class_character,
    progress          = class_any
  ),
  validator = function(self) {
    if (length(self@chunk_strategy) != 1 ||
        !self@chunk_strategy %in% c("even", "block_first", "block_consolidated")) {
      return("chunk_strategy must be one of 'even', 'block_first', 'block_consolidated'")
    }
    if (length(self@on_error) != 1 ||
        !self@on_error %in% c("skip", "retry", "stop")) {
      return("on_error must be one of 'skip', 'retry', 'stop'")
    }
    NULL
  }
)


#' @noRd
print.Duckdb_Control <- new_external_generic("base", "print", "x")


#' @noRd
method(print.Duckdb_Control, Duckdb_Control) <- function(x, ...) {
  cli::cli_text("{.strong <joinery::Duckdb_Control>}")

  fmt <- function(v) {
    if (is.null(v)) "auto"
    else if (isFALSE(v)) "FALSE (monolithic)"
    else if (is.character(v)) paste(v, collapse = ", ")
    else format(v)
  }

  cli::cli_text()
  cli::cli_bullets(c(
    sprintf("target_batch_size: %s", fmt(x@target_batch_size)),
    sprintf("min_batch_size: %s",    fmt(x@min_batch_size)),
    sprintf("chunk_strategy: %s",    x@chunk_strategy),
    sprintf("chunk_by: %s",          fmt(x@chunk_by)),
    sprintf("on_error: %s",          x@on_error),
    sprintf("progress: %s",          fmt(x@progress))
  ))
  invisible(x)
}


# ---------------------------------------------------------------------------
# Constructor
# ---------------------------------------------------------------------------

#' DuckDB Execution Control
#'
#' @description
#' Build a [Duckdb_Control] bundling the DuckDB backend's execution knobs, and
#' pass it as `control =` to [prepare_search_data()], [detect_duplicates()], or
#' [search_candidates()] on DuckDB tables. It controls **how** a match runs
#' (memory, batching, chunking, failure isolation), never **what** matches —
#' matching semantics stay on the [search_strategy()].
#'
#' Two execution stages, two atomicity rules:
#' - **Preprocess batching** (tokenization) is per-row, governed by
#'   `target_batch_size` / `min_batch_size` / `chunk_strategy`. Any row split is
#'   safe.
#' - **Scoring chunking** (the overlap join) is *block-atomic* — a pair only
#'   forms within a block, so a block can never be split. `chunk_by` packs
#'   *whole* blocks under `target_batch_size`; `on_error` isolates a
#'   pathological block from the rest of the run.
#'
#' Chunking is a DuckDB (out-of-core) concern; the in-memory data.table backend
#' ignores it.
#'
#' @param target_batch_size `NULL` (auto-tune from RAM / row size) or a positive
#'   number of rows per batch / scoring chunk.
#' @param min_batch_size `NULL` (auto-tune) or a positive number — the minimum
#'   table size before preprocess batching engages.
#' @param chunk_strategy Preprocess chunking strategy: `"block_consolidated"`
#'   (default), `"block_first"`, or `"even"`.
#' @param chunk_by Scoring chunk key. `NULL` (default) auto-derives a coarse key
#'   when the input is large and leaves small inputs monolithic; `FALSE` forces
#'   the monolithic path; a character vector names an explicit key, which must be
#'   a subset of the strategy's `block_by` (else cross-chunk pairs would be
#'   silently dropped).
#' @param on_error Per-scoring-chunk failure policy: `"skip"` (default — record
#'   and continue), `"retry"` (re-run once with conservative pragmas, then skip),
#'   or `"stop"` (re-raise).
#' @param progress `NULL` (auto), `TRUE`, or `FALSE` to force or suppress
#'   progress output.
#'
#' @return A [Duckdb_Control] object.
#'
#' @seealso [prepare_search_data()], [detect_duplicates()], [search_candidates()].
#'
#' @export
duckdb_control <- function(target_batch_size = NULL,
                           min_batch_size    = NULL,
                           chunk_strategy    = c("block_consolidated",
                                                 "block_first", "even"),
                           chunk_by          = NULL,
                           on_error          = c("skip", "retry", "stop"),
                           progress          = NULL) {

  # Take the first element for the default (vector) case; bad explicit values
  # are caught by the S7 validator with locale-independent English messages
  # (match.arg would emit a localized error without the argument name).
  chunk_strategy <- chunk_strategy[1]
  on_error       <- on_error[1]

  check_number_decimal(target_batch_size, min = 1, allow_null = TRUE)
  check_number_decimal(min_batch_size,    min = 1, allow_null = TRUE)

  # chunk_by: NULL | FALSE | character
  if (!is.null(chunk_by) && !isFALSE(chunk_by) && !is.character(chunk_by)) {
    cli::cli_abort(c(
      "{.arg chunk_by} must be {.code NULL} (auto), {.code FALSE} (monolithic), \\
       or a character vector of block columns.",
      x = "You supplied {.obj_type_friendly {chunk_by}}."
    ))
  }

  if (!is.null(progress) && !is.logical(progress)) {
    cli::cli_abort("{.arg progress} must be {.code NULL}, {.code TRUE}, or {.code FALSE}.")
  }

  Duckdb_Control(
    target_batch_size = target_batch_size,
    min_batch_size    = min_batch_size,
    chunk_strategy    = chunk_strategy,
    chunk_by          = chunk_by,
    on_error          = on_error,
    progress          = progress
  )
}
