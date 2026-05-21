# ============================================================
# Labelling round-trip — Phase 0.7 M4
# ============================================================
#
# export_for_labelling(): write a sample to a CSV with an `equal`
#   column pre-filled on header rows (block defaults). Users edit only
#   exceptions in any spreadsheet.
# import_labels()       : read the round-trip CSV, propagate block-default
#   labels onto unmarked candidate rows, validate, return a data.table
#   ready for fit_filter() / calibrate_matches().
#
# Format-agnostic. No UI. Round-trip integrity (no label drift, no row
# drift, schema preserved) is the testable contract.
#
# Block definition:
#   candidates : block = base record. Header rows = rows where
#                source == "base". `equal` on a header row is the block
#                default. Pair rows = source == "target" (and any other
#                non-base rows) inherit the default unless explicitly set.
#   duplicates : block = duplicate_group. Header rows = rank == 1L.
#                Other-rank rows inherit the default unless explicitly
#                set.
# ============================================================


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' @noRd
.labelling_extract_rows <- function(sample) {
  if (inherits(sample, "Match_Sample") || S7::S7_inherits(sample, Match_Sample)) {
    rows <- sample@rows
  } else {
    rows <- sample
  }
  data.table::as.data.table(rows)
}

#' @noRd
.labelling_block_id_col <- function(match_type) {
  switch(match_type,
    candidates = "match_id",
    duplicates = "duplicate_group"
  )
}

#' @noRd
.labelling_is_header <- function(dt, match_type) {
  if (match_type == "candidates") {
    if (!"source" %in% names(dt)) {
      stop(
        "Candidate matches CSV must have a `source` column ",
        "(values 'base' / 'target').",
        call. = FALSE
      )
    }
    dt[["source"]] == "base"
  } else {
    if (!"rank" %in% names(dt)) {
      stop(
        "Duplicate matches CSV must have a `rank` column.",
        call. = FALSE
      )
    }
    dt[["rank"]] == 1L
  }
}


# ---------------------------------------------------------------------------
# Exported verbs
# ---------------------------------------------------------------------------

#' Export a match sample to CSV for manual labelling
#'
#' @description
#' Write a sampled set of matches to a CSV pre-filled with an `equal`
#' column on block-header rows. Users edit the CSV in any spreadsheet,
#' marking only exceptions (e.g. false positives) and leaving the rest
#' as defaults.
#'
#' Block definition follows the matches schema: for candidate matches
#' (from [search_candidates()]), the header is the base-side row and
#' candidate rows inherit its default. For duplicate matches (from
#' [detect_duplicates()]), the header is the rank-1 row and the
#' remaining records in the duplicate group inherit its default.
#'
#' @param sample A [`Match_Sample`] object or a `data.table` / `data.frame`
#'   with the matches schema.
#' @param file Path to the CSV file to write.
#' @param default_label Integer scalar (default `1L`) used as the
#'   block-default `equal` value on header rows. `0L` for the inverse
#'   workflow.
#'
#' @return Invisibly returns `file`.
#'
#' @seealso [import_labels()]
#'
#' @export
export_for_labelling <- function(sample, file, default_label = 1L) {
  if (missing(file) || !is.character(file) || length(file) != 1L ||
      is.na(file) || !nzchar(file)) {
    stop("`file` must be a non-empty character scalar path.", call. = FALSE)
  }
  if (!is.numeric(default_label) || length(default_label) != 1L ||
      is.na(default_label) || !default_label %in% c(0L, 1L)) {
    stop("`default_label` must be 0L or 1L.", call. = FALSE)
  }
  default_label <- as.integer(default_label)

  rows <- .labelling_extract_rows(sample)
  if (nrow(rows) == 0L) {
    stop(
      "Cannot export an empty sample for labelling - at least one match row required.",
      call. = FALSE
    )
  }
  match_type <- .detect_match_type(rows)

  out <- data.table::copy(rows)
  is_header <- .labelling_is_header(out, match_type)
  out[, equal := NA_integer_]
  out[is_header, equal := default_label]

  # Place `equal` at the front so spreadsheet users see it immediately.
  col_order <- c("equal", setdiff(names(out), "equal"))
  data.table::setcolorder(out, col_order)

  data.table::fwrite(out, file)
  invisible(file)
}


#' Import a labelled CSV back into a feature/label table
#'
#' @description
#' Read a CSV written by [export_for_labelling()] (optionally edited by a
#' user), propagate the block-default `equal` value from each header row
#' onto unmarked rows in that block, validate the schema, and return a
#' `data.table` ready for `fit_filter()` / `calibrate_matches()`.
#'
#' @param file Path to the CSV file to read.
#'
#' @return A `data.table` with the same rows as the original sample plus
#'   a fully populated `equal` column (`0L` / `1L`).
#'
#' @seealso [export_for_labelling()]
#'
#' @export
import_labels <- function(file) {
  if (missing(file) || !is.character(file) || length(file) != 1L ||
      is.na(file) || !nzchar(file)) {
    stop("`file` must be a non-empty character scalar path.", call. = FALSE)
  }
  if (!file.exists(file)) {
    stop(sprintf("File does not exist: %s", file), call. = FALSE)
  }

  dt <- data.table::fread(file, na.strings = c("", "NA"))
  if (nrow(dt) == 0L) {
    stop("Imported labels file is empty.", call. = FALSE)
  }
  if (!"equal" %in% names(dt)) {
    stop(
      "Imported labels file must contain an `equal` column ",
      "(produced by export_for_labelling()).",
      call. = FALSE
    )
  }

  match_type <- .detect_match_type(dt)
  block_col  <- .labelling_block_id_col(match_type)
  if (!block_col %in% names(dt)) {
    stop(
      sprintf(
        "Imported labels file missing block key column `%s` required for %s matches.",
        block_col, match_type
      ),
      call. = FALSE
    )
  }
  is_header <- .labelling_is_header(dt, match_type)

  # Coerce equal to integer; allow user-entered "1"/"0" or numeric.
  raw_equal <- dt[["equal"]]
  equal_int <- suppressWarnings(as.integer(raw_equal))
  if (any(!is.na(raw_equal) & is.na(equal_int))) {
    stop(
      "Column `equal` must be coercible to integer 0L / 1L (or empty for ",
      "block-default).",
      call. = FALSE
    )
  }
  if (any(!is.na(equal_int) & !(equal_int %in% c(0L, 1L)))) {
    stop("Column `equal` may only contain 0, 1, or empty values.", call. = FALSE)
  }
  dt[, equal := equal_int]

  # For each block, pick a default = the header row's equal value (if any).
  header_dt <- dt[is_header, c(block_col, "equal"), with = FALSE]
  data.table::setnames(header_dt, "equal", ".block_default")
  # If multiple header rows per block disagree, take the first non-NA.
  defaults <- header_dt[, .(.block_default = {
    v <- .SD[[1L]]
    v_nonna <- v[!is.na(v)]
    if (length(v_nonna) == 0L) NA_integer_ else as.integer(v_nonna[1L])
  }), by = block_col, .SDcols = ".block_default"]

  dt <- merge(dt, defaults, by = block_col, all.x = TRUE, sort = FALSE)
  dt[, equal := data.table::fifelse(is.na(equal), .block_default, equal)]

  unresolved <- is.na(dt$equal)
  if (any(unresolved)) {
    n_unresolved <- sum(unresolved)
    stop(
      sprintf(
        "%d row(s) have no `equal` value and no block default to inherit. ",
        n_unresolved
      ),
      "Either fill `equal` on those rows or on their block headers.",
      call. = FALSE
    )
  }

  dt[, .block_default := NULL]
  dt[, equal := as.integer(equal)]
  dt[]
}
