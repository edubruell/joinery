# ============================================================
# Tests: calibration-related recommendations catalog entries
# ============================================================

library(data.table)


# ---------- dispatcher-level ----------------------------------------

test_that("consider_calibration_borderline fires when share > 0.10", {
  out <- joinery:::.dispatch_recommendations(list(
    pct_pairs_borderline = 0.20
  ))
  expect_true("consider_calibration_borderline" %in% out$ids)
  expect_match(out$messages, "calibrate_matches")
})

test_that("consider_calibration_borderline does NOT fire at 0.10 (strict >)", {
  out <- joinery:::.dispatch_recommendations(list(
    pct_pairs_borderline = 0.10
  ))
  expect_false("consider_calibration_borderline" %in% out$ids)
})

test_that("consider_calibration_ambiguity fires only above 0.20", {
  out_hi <- joinery:::.dispatch_recommendations(list(
    pct_records_with_ge3_matches = 0.30
  ))
  expect_true("consider_calibration_ambiguity" %in% out_hi$ids)

  out_lo <- joinery:::.dispatch_recommendations(list(
    pct_records_with_ge3_matches = 0.15
  ))
  expect_false("consider_calibration_ambiguity" %in% out_lo$ids)
})

test_that("calibration_low_n_warning fires below 500", {
  out <- joinery:::.dispatch_recommendations(list(training_n = 50))
  expect_true("calibration_low_n_warning" %in% out$ids)
  expect_match(out$messages, "50 labelled pairs")
})

test_that("calibration_low_n_warning does NOT fire at 500 (strict <)", {
  out <- joinery:::.dispatch_recommendations(list(training_n = 500))
  expect_false("calibration_low_n_warning" %in% out$ids)
})

test_that("calibration_drift_warning fires above TV=0.15", {
  out <- joinery:::.dispatch_recommendations(list(
    stage_dist_tv_distance = 0.40
  ))
  expect_true("calibration_drift_warning" %in% out$ids)
  expect_match(out$messages, "drifted")
})

test_that("calibration_drift_warning does NOT fire at TV=0.15 (strict >)", {
  out <- joinery:::.dispatch_recommendations(list(
    stage_dist_tv_distance = 0.15
  ))
  expect_false("calibration_drift_warning" %in% out$ids)
})

test_that("consider_calibration_ambiguity boundary: exactly 0.20 does not fire", {
  out <- joinery:::.dispatch_recommendations(list(
    pct_records_with_ge3_matches = 0.20
  ))
  expect_false("consider_calibration_ambiguity" %in% out$ids)
})

test_that("summarise_matches without threshold does not set pct_pairs_borderline", {
  matches <- data.table(
    match_id = rep(1:5, each = 2L),
    source   = rep(c("base", "target"), 5L),
    id       = paste0("r", 1:10),
    score    = rep(c(0.80, 0.85, 0.50, 0.90, 0.60), each = 2L)
  )
  mo <- summarise_matches(matches)
  expect_false(any(grepl("decision threshold", mo@recommendations)))
})


# ---------- end-to-end: surfaced by summarise_matches ----------------

test_that("summarise_matches surfaces consider_calibration_borderline", {
  set.seed(123)
  matches <- data.table(
    match_id = rep(seq_len(20), each = 2L),
    source   = rep(c("base", "target"), 20L),
    id       = paste0("r", seq_len(40)),
    score    = rep(runif(20, 0.79, 0.81), each = 2L)  # all near 0.80
  )
  mo <- summarise_matches(matches, threshold = 0.80,
                          borderline_epsilon = 0.05)
  expect_true(any(grepl("decision threshold", mo@recommendations)))
})


# ---------- end-to-end: drift fires from apply_filter ----------------

test_that("apply_filter surfaces calibration_drift_warning under stage drift", {
  base <- data.table(
    id   = c("a", "b", "c", "d", "e", "f"),
    name = c("john smith", "jon smith", "jane doe",
             "john doe",   "alice cooper", "alyce cooper")
  )
  s <- search_strategy(name ~ word_tokens(), threshold = 0.2)
  dups <- detect_duplicates(base, "id", s)
  mf <- match_features(dups, s, base = base, id = "id")

  # Inject a stage column into both the features and the labels so the
  # Filter_Model carries a training stage distribution.
  mf@features[, stage := "train"]

  labels <- copy(dups)
  labels[, stage := "train"]
  labels[, equal := NA_integer_]
  labels[rank == 1L, equal := 1L]
  labels[id == "c", equal := 0L]
  labels[is.na(equal), equal := 1L]

  fm <- fit_filter(mf, labels)
  expect_true(!is.null(fm@training_stage_dist))

  # Now build inference features with an unseen stage value -> TV = 1.
  mf2 <- mf
  mf2@features <- copy(mf@features)
  mf2@features[, stage := "other"]
  cm <- apply_filter(mf2, fm)
  expect_true(any(grepl("drift", cm@recommendations)))
})
