# ============================================================
# Labelling round-trip
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
      cli::cli_abort("Candidate matches CSV must have a {.field source} column (values {.val base}/{.val target})")
    }
    dt[["source"]] == "base"
  } else {
    if (!"rank" %in% names(dt)) {
      cli::cli_abort("Duplicate matches CSV must have a {.field rank} column")
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
  check_name(file)
  check_number_whole(default_label, min = 0, max = 1)
  default_label <- as.integer(default_label)

  rows <- .labelling_extract_rows(sample)
  if (nrow(rows) == 0L) {
    cli::cli_abort("Cannot export an empty sample for labelling, at least one match row required")
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
  check_name(file)
  if (!file.exists(file)) {
    cli::cli_abort("File does not exist: {.file {file}}")
  }

  dt <- data.table::fread(file, na.strings = c("", "NA"))
  if (nrow(dt) == 0L) {
    cli::cli_abort("Imported labels file is empty")
  }
  if (!"equal" %in% names(dt)) {
    cli::cli_abort(c(
      "Imported labels file must contain an {.field equal} column",
      "i" = "It should be produced by {.fn export_for_labelling}"
    ))
  }

  match_type <- .detect_match_type(dt)
  block_col  <- .labelling_block_id_col(match_type)
  if (!block_col %in% names(dt)) {
    cli::cli_abort("Imported labels file missing block key column {.field {block_col}} required for {match_type} matches")
  }
  is_header <- .labelling_is_header(dt, match_type)

  # Coerce equal to integer; allow user-entered "1"/"0" or numeric.
  raw_equal <- dt[["equal"]]
  equal_int <- suppressWarnings(as.integer(raw_equal))
  if (any(!is.na(raw_equal) & is.na(equal_int))) {
    cli::cli_abort(c(
      "Column {.field equal} must be coercible to integer 0L/1L",
      "i" = "Empty cells are allowed for block-default inheritance"
    ))
  }
  if (any(!is.na(equal_int) & !(equal_int %in% c(0L, 1L)))) {
    cli::cli_abort("Column {.field equal} may only contain {.val 0}, {.val 1}, or empty values")
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
    cli::cli_abort(c(
      "{n_unresolved} row(s) have no {.field equal} value and no block default to inherit",
      "i" = "Fill {.field equal} on those rows or on their block headers"
    ))
  }

  dt[, .block_default := NULL]
  dt[, equal := as.integer(equal)]
  dt[]
}
