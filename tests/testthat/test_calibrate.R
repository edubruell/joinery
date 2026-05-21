# ============================================================
# Tests: Phase 0.7 M6 — calibrate() + Filter_Calibration
# ============================================================

library(data.table)


# ---------- core metric helpers --------------------------------------

test_that(".brier_score is mean squared error", {
  expect_equal(joinery:::.brier_score(c(0.9, 0.1), c(1L, 0L)),
               mean(c(0.01, 0.01)))
})

test_that(".log_loss matches the closed form on a simple case", {
  prob <- c(0.9, 0.2)
  y    <- c(1L,  0L)
  expected <- -mean(c(log(0.9), log(0.8)))
  expect_equal(joinery:::.log_loss(prob, y), expected, tolerance = 1e-8)
})

test_that(".log_loss clips probabilities away from 0/1", {
  out <- joinery:::.log_loss(c(0, 1), c(1L, 0L))
  expect_true(is.finite(out))
})

test_that(".reliability_table aggregates by bin", {
  set.seed(7)
  p <- c(0.05, 0.06, 0.55, 0.95)
  y <- c(0L,   1L,   1L,   1L)
  rel <- joinery:::.reliability_table(p, y, bins = 10L)
  expect_true(all(c("bin","bin_lower","bin_upper","n","mean_pred","obs_pos")
                  %in% names(rel)))
  expect_equal(sum(rel$n), length(p))
})

test_that(".confusion_per_class returns rows for both classes", {
  cm <- joinery:::.confusion_per_class(
    prob      = c(0.9, 0.1, 0.6, 0.4),
    y         = c(1L,  0L,  1L,  0L),
    threshold = 0.5
  )
  expect_equal(cm$equal, c(0L, 1L))
  expect_equal(sum(cm$n_pred0 + cm$n_pred1), 4L)
})

test_that(".threshold_curve has monotone tpr at fixed thresholds", {
  prob <- c(0.1, 0.4, 0.6, 0.9)
  y    <- c(0L,  0L,  1L,  1L)
  tc <- joinery:::.threshold_curve(prob, y,
                                   grid = c(0, 0.5, 1))
  expect_equal(tc$tpr, c(1, 1, 0))
})


# ---------- end-to-end via Calibrated_Matches ------------------------

make_dedup_calibrated <- function() {
  base <- data.table(
    id   = c("a", "b", "c", "d", "e", "f"),
    name = c("john smith", "jon smith", "jane doe",
             "john doe",   "alice cooper", "alyce cooper")
  )
  s <- search_strategy(name ~ word_tokens(), threshold = 0.2)
  dups <- detect_duplicates(base, "id", s)
  mf <- match_features(dups, s, base = base, id = "id")

  labels <- copy(dups)
  labels[, equal := NA_integer_]
  labels[rank == 1L, equal := 1L]
  labels[id == "c", equal := 0L]
  labels[is.na(equal), equal := 1L]

  fm <- fit_filter(mf, labels)
  cm <- apply_filter(mf, fm, matches = dups)
  list(cm = cm, fm = fm, labels = labels)
}

test_that("calibrate(cm) uses training labels by default", {
  bits <- make_dedup_calibrated()
  cal  <- calibrate(bits$cm)

  expect_true(S7::S7_inherits(cal, joinery:::Filter_Calibration))
  expect_equal(cal@n_eval, bits$fm@training_n)
  expect_true(is.finite(cal@brier))
  expect_true(is.finite(cal@log_loss))
  expect_true(nrow(cal@confusion_per_class) == 2L)
  expect_true(nrow(cal@threshold_curve) > 0L)
})

test_that("calibrate() fires calibration_low_n_warning on small samples", {
  bits <- make_dedup_calibrated()
  cal  <- calibrate(bits$cm)
  expect_true(any(grepl("labelled pairs", cal@recommendations)))
  out  <- joinery:::.dispatch_recommendations(
    list(training_n = bits$fm@training_n)
  )
  expect_true("calibration_low_n_warning" %in% out$ids)
})

test_that("calibrate(cm, labels) evaluates on supplied labels", {
  bits <- make_dedup_calibrated()
  cal  <- calibrate(bits$cm, labels = bits$labels)

  expect_true(S7::S7_inherits(cal, joinery:::Filter_Calibration))
  expect_true(cal@n_eval > 0L)
})

test_that("Filter_Calibration coerces to a single-row data.table", {
  bits <- make_dedup_calibrated()
  cal  <- calibrate(bits$cm)
  dt   <- as.data.table(cal)
  expect_s3_class(dt, "data.table")
  expect_equal(nrow(dt), 1L)
  expect_true(all(c("n_eval", "class_balance", "threshold",
                    "brier", "log_loss", "n_recommendations")
                  %in% names(dt)))
})

test_that("recommendations() accessor returns Filter_Calibration messages", {
  bits <- make_dedup_calibrated()
  cal  <- calibrate(bits$cm)
  expect_identical(recommendations(cal), cal@recommendations)
})


# ---------- empty / single-class corner cases ------------------------

test_that("metric helpers handle empty inputs", {
  expect_true(is.na(joinery:::.brier_score(numeric(), integer())))
  expect_true(is.na(joinery:::.log_loss(numeric(), integer())))
  rel <- joinery:::.reliability_table(numeric(), integer())
  expect_equal(nrow(rel), 0L)
  tc <- joinery:::.threshold_curve(numeric(), integer())
  expect_equal(nrow(tc), 0L)
})

test_that(".threshold_curve handles single-class input", {
  tc <- joinery:::.threshold_curve(c(0.1, 0.4), c(1L, 1L),
                                   grid = c(0.2, 0.5))
  expect_equal(nrow(tc), 2L)
  # N = 0 -> fpr is NA, precision uses tp/(tp+fp) with fp = 0 -> 1 or NA
  expect_true(all(is.na(tc$fpr)))
})


# ---------- candidate-side calibrate with labels ---------------------

test_that("calibrate(cm, labels) dispatches the candidates match-type branch", {
  base <- data.table(
    id   = c("a", "b", "c"),
    name = c("john smith", "jane doe", "alice cooper")
  )
  target <- data.table(
    id   = c("x", "y", "z"),
    name = c("jon smith", "j doe", "alyce cooper")
  )
  s <- search_strategy(name ~ word_tokens(), threshold = 0.1)
  cands <- search_candidates(base, target, "id", "id", s)
  mf <- match_features(cands, s, base = base, id = "id",
                       target = target, target_id = "id")

  labels <- copy(cands)
  labels[, equal := NA_integer_]
  # Mark first match_id rows as negatives, rest as positives so we have
  # both classes.
  mids <- sort(unique(labels$match_id))
  labels[, equal := ifelse(match_id == mids[1L], 0L, 1L)]

  fm <- fit_filter(mf, labels)
  cm <- apply_filter(mf, fm, matches = cands)
  cal <- calibrate(cm, labels = labels)
  expect_true(S7::S7_inherits(cal, joinery:::Filter_Calibration))
  expect_true(cal@n_eval > 0L)
})
