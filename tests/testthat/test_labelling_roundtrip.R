# tests/testthat/test_labelling_roundtrip.R
# export_for_labelling() <-> import_labels() round-trip

library(data.table)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

make_lab_cand_matches <- function() {
  data.table(
    match_id = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
    id       = c("a", "t1", "a", "t2", "b", "t3", "c", "t4"),
    source   = c("base", "target", "base", "target",
                 "base", "target", "base", "target"),
    score    = c(0.95, 0.95, 0.90, 0.90, 0.80, 0.80, 0.60, 0.60),
    rank     = c(1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L)
  )
}

make_lab_dup_matches <- function() {
  data.table(
    duplicate_group = c(1L, 1L, 1L, 2L, 2L, 3L, 3L),
    id              = c("a", "b", "c", "d", "e", "f", "g"),
    score           = c(0.95, 0.90, 0.85, 0.80, 0.75, 0.60, 0.55),
    rank            = c(1L, 2L, 3L, 1L, 2L, 1L, 2L)
  )
}


# ---------------------------------------------------------------------------
# 1. export validation
# ---------------------------------------------------------------------------

test_that("export_for_labelling validates inputs", {
  dt <- make_lab_cand_matches()
  tmp <- tempfile(fileext = ".csv")

  expect_error(export_for_labelling(dt), "file")
  expect_error(export_for_labelling(dt, tmp, default_label = 5L), "0L or 1L")
  expect_error(export_for_labelling(dt[0L], tmp), "empty sample")
})

test_that("export writes a CSV with `equal` column first", {
  dt  <- make_lab_cand_matches()
  tmp <- tempfile(fileext = ".csv")
  out <- export_for_labelling(dt, tmp, default_label = 1L)
  expect_equal(out, tmp)
  expect_true(file.exists(tmp))

  raw <- fread(tmp, na.strings = c("", "NA"))
  expect_equal(names(raw)[1L], "equal")
  expect_true(all(names(dt) %in% names(raw)))
})


# ---------------------------------------------------------------------------
# 2. round-trip integrity — candidates
# ---------------------------------------------------------------------------

test_that("candidates round-trip preserves rows, schema, and propagates defaults", {
  dt  <- make_lab_cand_matches()
  tmp <- tempfile(fileext = ".csv")
  export_for_labelling(dt, tmp, default_label = 1L)

  imp <- import_labels(tmp)
  expect_s3_class(imp, "data.table")
  expect_equal(nrow(imp), nrow(dt))
  expect_true(all(names(dt) %in% names(imp)))
  expect_true("equal" %in% names(imp))

  # Default propagation: all rows should inherit `1`.
  expect_true(all(imp$equal == 1L))
})

test_that("candidates round-trip honours user edits on target rows", {
  dt  <- make_lab_cand_matches()
  tmp <- tempfile(fileext = ".csv")
  export_for_labelling(dt, tmp, default_label = 1L)

  edited <- fread(tmp, na.strings = c("", "NA"))
  # mark match_id == 2's target as FP (equal = 0)
  edited[match_id == 2L & source == "target", equal := 0L]
  fwrite(edited, tmp)

  imp <- import_labels(tmp)
  expect_equal(imp[match_id == 1L & source == "target", equal], 1L)
  expect_equal(imp[match_id == 2L & source == "target", equal], 0L)
  # base rows keep default
  expect_true(all(imp[source == "base", equal] == 1L))
})

test_that("default_label = 0L round-trip works", {
  dt  <- make_lab_cand_matches()
  tmp <- tempfile(fileext = ".csv")
  export_for_labelling(dt, tmp, default_label = 0L)

  imp <- import_labels(tmp)
  expect_true(all(imp$equal == 0L))
})


# ---------------------------------------------------------------------------
# 3. round-trip integrity — duplicates
# ---------------------------------------------------------------------------

test_that("duplicates round-trip preserves rows and propagates defaults", {
  dt  <- make_lab_dup_matches()
  tmp <- tempfile(fileext = ".csv")
  export_for_labelling(dt, tmp, default_label = 1L)

  imp <- import_labels(tmp)
  expect_equal(nrow(imp), nrow(dt))
  expect_true(all(imp$equal == 1L))
})

test_that("duplicates round-trip honours edits on non-rank-1 rows", {
  dt  <- make_lab_dup_matches()
  tmp <- tempfile(fileext = ".csv")
  export_for_labelling(dt, tmp, default_label = 1L)

  edited <- fread(tmp, na.strings = c("", "NA"))
  # group 1 record "c" gets explicitly marked as 0
  edited[duplicate_group == 1L & id == "c", equal := 0L]
  fwrite(edited, tmp)

  imp <- import_labels(tmp)
  expect_equal(imp[duplicate_group == 1L & id == "c", equal], 0L)
  expect_equal(imp[duplicate_group == 1L & id == "b", equal], 1L)
  expect_equal(imp[duplicate_group == 2L & id == "e", equal], 1L)
})


# ---------------------------------------------------------------------------
# 4. import validation / errors
# ---------------------------------------------------------------------------

test_that("import_labels errors on missing file", {
  expect_error(import_labels(tempfile()), "does not exist")
})

test_that("import_labels errors on missing `equal` column", {
  dt  <- make_lab_cand_matches()
  tmp <- tempfile(fileext = ".csv")
  fwrite(dt, tmp)
  expect_error(import_labels(tmp), "equal")
})

test_that("import_labels errors on invalid equal values", {
  dt  <- make_lab_cand_matches()
  tmp <- tempfile(fileext = ".csv")
  export_for_labelling(dt, tmp, default_label = 1L)
  edited <- fread(tmp, na.strings = c("", "NA"))
  edited[1L, equal := 7L]
  fwrite(edited, tmp)
  expect_error(import_labels(tmp), "0.*1.*empty")
})

test_that("import_labels errors when a block has no resolvable default", {
  dt  <- make_lab_cand_matches()
  tmp <- tempfile(fileext = ".csv")
  export_for_labelling(dt, tmp, default_label = 1L)
  edited <- fread(tmp, na.strings = c("", "NA"))
  # blank out the header row defaults for match_id == 1, plus its target
  edited[match_id == 1L, equal := NA_integer_]
  fwrite(edited, tmp)
  expect_error(import_labels(tmp), "no equal value")
})


# ---------------------------------------------------------------------------
# 5. Match_Sample input path
# ---------------------------------------------------------------------------

test_that("export_for_labelling accepts a Match_Sample directly", {
  dt  <- make_lab_cand_matches()
  ms  <- sample_matches(dt, mode = "high", n = 4L)
  tmp <- tempfile(fileext = ".csv")
  export_for_labelling(ms, tmp, default_label = 1L)
  imp <- import_labels(tmp)
  expect_equal(nrow(imp), nrow(ms@rows))
  expect_true(all(imp$equal == 1L))
})

test_that("Match_Sample input on duplicates also round-trips", {
  dt  <- make_lab_dup_matches()
  ms  <- sample_matches(dt, mode = "high", n = 5L)
  tmp <- tempfile(fileext = ".csv")
  export_for_labelling(ms, tmp, default_label = 1L)
  imp <- import_labels(tmp)
  expect_equal(nrow(imp), nrow(ms@rows))
  expect_true(all(imp$equal == 1L))
})


# ---------------------------------------------------------------------------
# 6. Round-trip property: nrow, full schema, column order, fully resolved
# ---------------------------------------------------------------------------

test_that("candidates round-trip is a complete contract: rows, schema, equal fully resolved", {
  dt  <- make_lab_cand_matches()
  tmp <- tempfile(fileext = ".csv")
  export_for_labelling(dt, tmp, default_label = 1L)
  imp <- import_labels(tmp)

  expect_equal(nrow(imp), nrow(dt))
  expect_setequal(names(imp), c(names(dt), "equal"))
  # Remaining column order (drop `equal`) matches original
  expect_equal(setdiff(names(imp), "equal"), names(dt))
  expect_false(anyNA(imp$equal))
})

test_that("duplicates round-trip is a complete contract: rows, schema, equal fully resolved", {
  dt  <- make_lab_dup_matches()
  tmp <- tempfile(fileext = ".csv")
  export_for_labelling(dt, tmp, default_label = 1L)
  imp <- import_labels(tmp)

  expect_equal(nrow(imp), nrow(dt))
  expect_setequal(names(imp), c(names(dt), "equal"))
  expect_equal(setdiff(names(imp), "equal"), names(dt))
  expect_false(anyNA(imp$equal))
})


# ---------------------------------------------------------------------------
# 7. Realistic spreadsheet inputs
# ---------------------------------------------------------------------------

test_that("import accepts character '0' / '1' / empty values from a spreadsheet", {
  dt  <- make_lab_cand_matches()
  tmp <- tempfile(fileext = ".csv")
  export_for_labelling(dt, tmp, default_label = 1L)

  raw <- readLines(tmp)
  # Mimic spreadsheet: quote some `equal` values as strings, blank some out
  # Just rewrite the file with character labels.
  edited <- fread(tmp, na.strings = c("", "NA"))
  edited[, equal := as.character(equal)]
  edited[match_id == 2L & source == "target", equal := "0"]
  edited[match_id == 1L & source == "target", equal := ""]
  fwrite(edited, tmp)

  imp <- import_labels(tmp)
  expect_false(anyNA(imp$equal))
  # match_id == 1 target inherits header default (1)
  expect_equal(imp[match_id == 1L & source == "target", equal], 1L)
  # match_id == 2 target explicit "0"
  expect_equal(imp[match_id == 2L & source == "target", equal], 0L)
})

test_that("import errors when the block-key column is missing", {
  dt  <- make_lab_cand_matches()
  tmp <- tempfile(fileext = ".csv")
  export_for_labelling(dt, tmp, default_label = 1L)
  edited <- fread(tmp, na.strings = c("", "NA"))
  edited[, match_id := NULL]
  fwrite(edited, tmp)
  expect_error(import_labels(tmp), "match table|joinery", perl = TRUE)
})

test_that("import errors on empty CSV", {
  tmp <- tempfile(fileext = ".csv")
  # Header-only file
  writeLines("equal,match_id,id,source,score,rank", tmp)
  expect_error(import_labels(tmp), "empty")
})
