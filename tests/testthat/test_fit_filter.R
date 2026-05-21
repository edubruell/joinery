# ============================================================
# Tests: fit_filter() / apply_filter()
# ============================================================
#
# Reference: notes/calibration_design.md.
# ============================================================

library(data.table)


# ---------- fixtures --------------------------------------------------

make_dedup_fixture <- function() {
  data.table(
    id   = c("a", "b", "c", "d", "e", "f"),
    name = c(
      "john smith", "jon smith",   "jane doe",
      "john doe",   "alice cooper", "alyce cooper"
    )
  )
}

simple_strategy <- function() {
  search_strategy(name ~ word_tokens(), threshold = 0.2)
}

make_dedup_features_and_labels <- function() {
  base <- make_dedup_fixture()
  s    <- simple_strategy()
  dups <- detect_duplicates(base, "id", s)
  mf   <- match_features(dups, s, base = base, id = "id")

  # Build labels: mark a-d (john/jon/doe/etc.) as positives, e-f
  # (alice/alyce) sub-pairs as positives, cross-group pairs as
  # negatives. Use the round-trip CSV path to validate the typical
  # workflow.
  labels <- data.table::copy(dups)
  labels[, equal := NA_integer_]
  # rank == 1 -> block default 1
  labels[rank == 1L, equal := 1L]
  # mark every rank>=2 pair as positive if same surname group, else 0
  # crude grouping: a/b/d share "john"/"jon"/"smith"/"doe"; c is "jane doe";
  # e/f share "cooper". We mark same-block rank-k rows individually:
  labels[id == "c", equal := 0L]   # jane doe is FP w.r.t. the johns
  # leave everything else inherited from block-header default of 1
  labels[is.na(equal), equal := 1L]

  list(dups = dups, mf = mf, labels = labels, base = base, strategy = s)
}


# ---------- youden_j helper ------------------------------------------

test_that(".youden_j_threshold picks the perfect-separation cut", {
  thr <- joinery:::.youden_j_threshold(
    prob  = c(0.1, 0.2, 0.6, 0.7, 0.8, 0.9),
    equal = c(0L,  0L,  1L,  1L,  1L,  1L)
  )
  expect_true(thr >= 0.5 && thr <= 0.7)
})

test_that(".youden_j_threshold returns 0.5 with a single class", {
  expect_equal(
    joinery:::.youden_j_threshold(c(0.1, 0.4, 0.9), c(1L, 1L, 1L)),
    0.5
  )
})


# ---------- predictor selection --------------------------------------

test_that(".feature_predictors drops id columns and equal", {
  dt <- data.table(
    searched = "a", found = "b", match_id = 1L, stage = NA_character_,
    score = 0.5, scnt = 1L, equal = 1L
  )
  preds <- joinery:::.feature_predictors(dt)
  expect_setequal(preds, c("score", "scnt"))
  expect_false("equal" %in% preds)
  expect_false("searched" %in% preds)
})


# ---------- end-to-end on dedup --------------------------------------

test_that("fit_filter() returns a Filter_Model fit by glm", {
  bits <- make_dedup_features_and_labels()
  fm <- fit_filter(bits$mf, bits$labels)

  expect_true(S7::S7_inherits(fm, joinery:::Filter_Model))
  expect_equal(fm@backend, "glm")
  expect_equal(fm@model_class, "glm")
  expect_true(fm@training_n > 0L)
  expect_true(length(fm@predictors) >= 1L)
  expect_false(fm@class_weighted)
})

test_that("apply_filter() adds tp_prob and predicted_tp", {
  bits <- make_dedup_features_and_labels()
  fm  <- fit_filter(bits$mf, bits$labels)
  cal <- apply_filter(bits$mf, fm)

  expect_true(S7::S7_inherits(cal, joinery:::Calibrated_Matches))
  out <- cal@matches
  expect_true(all(c("tp_prob", "predicted_tp") %in% names(out)))
  expect_true(all(out$tp_prob >= 0 & out$tp_prob <= 1, na.rm = TRUE))
  expect_true(all(out$predicted_tp %in% c(0L, 1L), na.rm = TRUE))
  expect_equal(cal@threshold_method, "youden_j")
})

test_that("apply_filter(matches=) broadcasts predictions back to dedup rows", {
  bits <- make_dedup_features_and_labels()
  fm  <- fit_filter(bits$mf, bits$labels)
  cal <- apply_filter(bits$mf, fm, matches = bits$dups)

  out <- cal@matches
  expect_true(all(c("tp_prob", "predicted_tp") %in% names(out)))
  expect_equal(nrow(out), nrow(bits$dups))
  # rank-1 rows have no pair-level prediction
  expect_true(all(is.na(out[rank == 1L]$tp_prob)) || nrow(out[rank == 1L]) == 0L)
  # rank-k rows do have predictions
  expect_true(all(!is.na(out[rank >= 2L]$tp_prob)))
})

test_that("user-supplied threshold overrides Youden's J", {
  bits <- make_dedup_features_and_labels()
  fm  <- fit_filter(bits$mf, bits$labels)
  cal <- apply_filter(bits$mf, fm, threshold = 0.95)

  expect_equal(cal@threshold, 0.95)
  expect_equal(cal@threshold_method, "user")
  out <- cal@matches
  expect_true(all(out$predicted_tp == as.integer(out$tp_prob >= 0.95)))
})

test_that("fit_filter() is deterministic across runs", {
  bits <- make_dedup_features_and_labels()
  fm1 <- fit_filter(bits$mf, bits$labels)
  fm2 <- fit_filter(bits$mf, bits$labels)
  expect_equal(coef(fm1@fit), coef(fm2@fit))
})


# ---------- class-weighted variant -----------------------------------

test_that("class_weighted = TRUE fits and differs from unweighted on imbalance", {
  bits <- make_dedup_features_and_labels()
  fm_un <- fit_filter(bits$mf, bits$labels, class_weighted = FALSE)
  fm_w  <- fit_filter(bits$mf, bits$labels, class_weighted = TRUE)

  expect_true(fm_w@class_weighted)
  # If labels are imbalanced, weighted intercept differs.
  expect_false(identical(coef(fm_un@fit), coef(fm_w@fit)))
})


# ---------- candidates path ------------------------------------------

make_candidates_fixture <- function() {
  base <- data.table(
    id   = c("b1", "b2", "b3"),
    name = c("john smith", "alice jones", "carlos rare")
  )
  target <- data.table(
    id   = c("t1", "t2", "t3"),
    name = c("john smith", "alice jonas", "carlos rare")
  )
  s     <- search_strategy(name ~ word_tokens(), threshold = 0.2)
  cands <- search_candidates(base, target, "id", "id", s)
  mf    <- match_features(cands, s, base = base, id = "id",
                          target = target, target_id = "id")

  labels <- data.table::copy(cands)
  labels[, equal := NA_integer_]
  labels[source == "base", equal := 1L]   # block default
  # mark all target candidates as positive (TPs)
  labels[is.na(equal), equal := 1L]
  # except force the carlos/alice cross-pair to negative if any
  cross_ids <- labels[source == "target" & id == "t2" &
                      match_id %in% labels[source == "base" & id == "b1",
                                           match_id], match_id]
  if (length(cross_ids) > 0L) {
    labels[match_id %in% cross_ids & source == "target",
           equal := 0L]
  } else {
    # if no cross-pair, fabricate one negative by picking the lowest-scored
    drop_mid <- labels[, .(s = score[1]), by = match_id][which.min(s), match_id]
    labels[match_id == drop_mid & source == "target", equal := 0L]
  }

  list(cands = cands, mf = mf, labels = labels,
       base = base, target = target, strategy = s)
}

test_that("fit_filter and apply_filter work on candidates", {
  bits <- make_candidates_fixture()
  fm  <- fit_filter(bits$mf, bits$labels)
  cal <- apply_filter(bits$mf, fm, matches = bits$cands)

  expect_true(S7::S7_inherits(cal, joinery:::Calibrated_Matches))
  out <- cal@matches
  expect_true(all(c("tp_prob", "predicted_tp") %in% names(out)))
  # both base + target rows of a match_id receive the same prediction
  base_rows   <- out[source == "base"]
  target_rows <- out[source == "target"]
  if (nrow(base_rows) > 0L && nrow(target_rows) > 0L) {
    chk <- merge(base_rows[, .(match_id, tp_b = tp_prob)],
                 target_rows[, .(match_id, tp_t = tp_prob)],
                 by = "match_id")
    expect_equal(chk$tp_b, chk$tp_t)
  }
})


# ---------- validation errors ----------------------------------------

test_that("fit_filter errors on non-Match_Features input", {
  expect_error(
    fit_filter(data.table(a = 1), data.table(equal = 1L)),
    "Match_Features"
  )
})

test_that("fit_filter errors when labels are single-class", {
  bits <- make_dedup_features_and_labels()
  bad <- data.table::copy(bits$labels)
  bad[, equal := 1L]
  expect_error(fit_filter(bits$mf, bad), "one class")
})

test_that("fit_filter errors when there is no overlap with labels", {
  bits <- make_dedup_features_and_labels()
  bad <- data.table::copy(bits$labels)
  bad[, duplicate_group := duplicate_group + 999999L]
  expect_error(fit_filter(bits$mf, bad), "No features rows matched")
})

test_that("apply_filter rejects invalid threshold", {
  bits <- make_dedup_features_and_labels()
  fm <- fit_filter(bits$mf, bits$labels)
  expect_error(apply_filter(bits$mf, fm, threshold = 1.5),
               "probability in")
})

test_that("fit_filter rejects unsupported model strings", {
  bits <- make_dedup_features_and_labels()
  expect_error(fit_filter(bits$mf, bits$labels, model = "xgboost"),
               "parsnip")
})


# ---------- Filter_Model slots ---------------------------------------

test_that("Filter_Model carries training_prob / training_equal aligned with training_n", {
  bits <- make_dedup_features_and_labels()
  fm <- fit_filter(bits$mf, bits$labels)

  expect_equal(length(fm@training_prob), fm@training_n)
  expect_equal(length(fm@training_equal), fm@training_n)
  expect_true(all(fm@training_equal %in% c(0L, 1L)))
  expect_true(all(fm@training_prob >= 0 & fm@training_prob <= 1))
  expect_equal(fm@training_class_balance, mean(fm@training_equal == 1L))
  expect_equal(fm@na_fill, 0)
})


# ---------- print/format methods -------------------------------------

test_that("print() / format() on Filter_Model and Calibrated_Matches do not error", {
  bits <- make_dedup_features_and_labels()
  fm   <- fit_filter(bits$mf, bits$labels)
  cal  <- apply_filter(bits$mf, fm)

  lines_fm  <- format(fm)
  lines_cal <- format(cal)
  expect_type(lines_fm,  "character")
  expect_type(lines_cal, "character")
  expect_true(any(grepl("Filter_Model",       lines_fm)))
  expect_true(any(grepl("Calibrated_Matches", lines_cal)))
  expect_invisible(print(fm))
  expect_invisible(print(cal))
})


# ---------- recommendations accessor ---------------------------------

test_that("recommendations() returns character() by default on Calibrated_Matches", {
  bits <- make_dedup_features_and_labels()
  fm   <- fit_filter(bits$mf, bits$labels)
  cal  <- apply_filter(bits$mf, fm)
  expect_identical(recommendations(cal), character())
})


# ---------- import_labels round-trip to fit_filter -------------------

test_that("CSV labelling round-trip feeds fit_filter without drift", {
  bits <- make_dedup_features_and_labels()
  samp <- sample_matches(bits$dups, mode = "random",
                         n = nrow(bits$dups), seed = 1L)
  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf), add = TRUE)
  export_for_labelling(samp, tf, default_label = 1L)
  imported <- import_labels(tf)
  # force at least one negative so the fit can run
  imported[id == "c", equal := 0L]

  fm  <- fit_filter(bits$mf, imported)
  cal <- apply_filter(bits$mf, fm)
  expect_true(S7::S7_inherits(cal, joinery:::Calibrated_Matches))
})


# ---------- na_fill round-trip ----------------------------------------

test_that("fit_filter na_fill is honoured at predict time", {
  bits <- make_dedup_features_and_labels()
  fm <- fit_filter(bits$mf, bits$labels, na_fill = -1)
  expect_equal(fm@na_fill, -1)
  cal <- apply_filter(bits$mf, fm)
  expect_true(all(cal@matches$tp_prob >= 0 & cal@matches$tp_prob <= 1))
})


# ---------- constant-predictor branch --------------------------------

test_that("fit_filter drops constant predictors and still fits", {
  bits <- make_dedup_features_and_labels()
  mf2 <- bits$mf
  ft  <- data.table::copy(mf2@features)
  ft[, score := 0.5]   # make `score` constant
  S7::prop(mf2, "features") <- ft

  fm <- fit_filter(mf2, bits$labels)
  expect_true(S7::S7_inherits(fm, joinery:::Filter_Model))
  # score is in @predictors (kept for the predict path) but absent from
  # the fitted coefficients.
  expect_true("score" %in% fm@predictors)
  expect_false("score" %in% names(coef(fm@fit)))
})


# ---------- apply_filter threshold validation ------------------------

test_that("apply_filter rejects NaN / negative / >1 thresholds", {
  bits <- make_dedup_features_and_labels()
  fm   <- fit_filter(bits$mf, bits$labels)
  expect_error(apply_filter(bits$mf, fm, threshold = NaN),       "probability in")
  expect_error(apply_filter(bits$mf, fm, threshold = -0.01),     "probability in")
  expect_error(apply_filter(bits$mf, fm, threshold =  1.01),     "probability in")
})


# ---------- apply_filter back-fills missing predictors ---------------

test_that("apply_filter back-fills predictors that are missing on the features table", {
  bits <- make_dedup_features_and_labels()
  fm   <- fit_filter(bits$mf, bits$labels)

  mf2 <- bits$mf
  ft  <- data.table::copy(mf2@features)
  # drop a known feature column entirely
  drop_col <- intersect(fm@predictors, names(ft))[1L]
  ft[, (drop_col) := NULL]
  S7::prop(mf2, "features") <- ft

  cal <- suppressWarnings(apply_filter(mf2, fm))
  expect_true("tp_prob" %in% names(cal@matches))
  expect_true(all(!is.na(cal@matches$tp_prob)))
})


# ---------- empty / single-row inputs --------------------------------

test_that("apply_filter on an empty Match_Features returns an empty result", {
  empty_mf <- joinery:::Match_Features(
    features       = data.table::data.table(
      searched = character(), found = character(),
      match_id = integer(),  stage = character(),
      score = numeric(),     cnt = integer(),
      icnt = integer(),      ipos = numeric(),
      scnt = integer(),      rcnt = integer()
    ),
    schema         = "token",
    strategy_class = "Search_Strategy",
    top_n          = c(default = 5L),
    columns        = "name",
    aip_summary    = NULL
  )
  bits <- make_dedup_features_and_labels()
  fm <- fit_filter(bits$mf, bits$labels)

  cal <- apply_filter(empty_mf, fm, threshold = 0.5)
  expect_equal(nrow(cal@matches), 0L)
  expect_true(all(c("tp_prob", "predicted_tp") %in% names(cal@matches)))
})
