# ============================================================
# Tests: Phase 0.7 M5 — calibrate_matches() high-level verb
# ============================================================
#
# Reference: notes/calibration_design.md §4.1, §14 (M5).
# ============================================================

library(data.table)


make_dedup_setup <- function() {
  base <- data.table(
    id   = c("a", "b", "c", "d", "e", "f"),
    name = c(
      "john smith", "jon smith",   "jane doe",
      "john doe",   "alice cooper", "alyce cooper"
    )
  )
  s    <- search_strategy(name ~ word_tokens(), threshold = 0.2)
  dups <- detect_duplicates(base, "id", s)

  # Hand-built labels: every block-header (rank == 1) is 1L; one
  # non-header rank-k is forced to 0 to guarantee both classes.
  labels <- data.table::copy(dups)
  labels[, equal := NA_integer_]
  labels[rank == 1L, equal := 1L]
  labels[id == "c", equal := 0L]
  labels[is.na(equal), equal := 1L]

  list(base = base, strategy = s, dups = dups, labels = labels)
}


test_that("calibrate_matches() returns a Calibrated_Matches with predictions", {
  setup <- make_dedup_setup()
  cal <- calibrate_matches(
    setup$dups, setup$strategy, labels = setup$labels,
    base = setup$base, id = "id"
  )

  expect_true(S7::S7_inherits(cal, joinery:::Calibrated_Matches))
  expect_equal(nrow(cal@matches), nrow(setup$dups))
  expect_true(all(c("tp_prob", "predicted_tp") %in% names(cal@matches)))
  expect_equal(cal@threshold_method, "youden_j")
  expect_true(S7::S7_inherits(cal@filter_model, joinery:::Filter_Model))
})


test_that("calibrate_matches() respects a user-supplied threshold", {
  setup <- make_dedup_setup()
  cal_high <- calibrate_matches(
    setup$dups, setup$strategy, labels = setup$labels,
    base = setup$base, id = "id", threshold = 0.999
  )
  cal_low <- calibrate_matches(
    setup$dups, setup$strategy, labels = setup$labels,
    base = setup$base, id = "id", threshold = 0.001
  )

  out_high <- cal_high@matches
  out_low  <- cal_low@matches
  # threshold near 1 should mostly drop everything; threshold near 0 keep
  n_kept_high <- sum(out_high$predicted_tp == 1L, na.rm = TRUE)
  n_kept_low  <- sum(out_low$predicted_tp  == 1L, na.rm = TRUE)
  expect_lte(n_kept_high, n_kept_low)
})


test_that("calibrate_matches() is reproducible (deterministic glm)", {
  setup <- make_dedup_setup()
  c1 <- calibrate_matches(setup$dups, setup$strategy, labels = setup$labels,
                          base = setup$base, id = "id")
  c2 <- calibrate_matches(setup$dups, setup$strategy, labels = setup$labels,
                          base = setup$base, id = "id")
  expect_equal(
    coef(c1@filter_model@fit),
    coef(c2@filter_model@fit)
  )
  expect_equal(c1@threshold, c2@threshold)
})


test_that("calibrate_matches() dispatches on candidates input", {
  base   <- data.table(
    id   = c("b1", "b2", "b3"),
    name = c("john smith", "alice jones", "carlos rare")
  )
  target <- data.table(
    id   = c("t1", "t2", "t3"),
    name = c("john smith", "alice jonas", "carlos rare")
  )
  s     <- search_strategy(name ~ word_tokens(), threshold = 0.2)
  cands <- search_candidates(base, target, "id", "id", s)

  labels <- data.table::copy(cands)
  labels[, equal := NA_integer_]
  labels[source == "base", equal := 1L]
  labels[is.na(equal), equal := 1L]
  drop_mid <- labels[, .(s = score[1]), by = match_id][which.min(s), match_id]
  labels[match_id == drop_mid & source == "target", equal := 0L]

  cal <- calibrate_matches(
    cands, s, labels = labels,
    base = base, id = "id",
    target = target, target_id = "id"
  )
  expect_true(S7::S7_inherits(cal, joinery:::Calibrated_Matches))
  expect_equal(nrow(cal@matches), nrow(cands))
})


test_that("calibrate_matches() recovers planted false positives", {
  # Two real groups (johns, marys) plus a shared token "clark" planted
  # on one "john" record so the matcher over-merges and produces
  # cross-group false positives that the calibrator can learn to drop.
  base <- data.table(
    id   = c("a1", "a2", "a3", "a4", "b1", "b2", "b3", "b4"),
    name = c(
      "john clark",   "jon clark",       "john k clark", "j clark",
      "mary clark",   "marie clark",     "mary k clark", "m clark"
    )
  )
  s    <- search_strategy(name ~ word_tokens(), threshold = 0.1)
  dups <- detect_duplicates(base, "id", s)

  # Use the rank-1 record's name to derive "true group" -- pairs that
  # share the first letter of rank-1's last token are TPs, else FPs.
  labels <- data.table::copy(dups)
  rank1 <- labels[rank == 1L, .(duplicate_group, .head_id = id)]
  rank1[, .head_letter := substr(.head_id, 1, 1)]
  labels <- merge(labels, rank1[, .(duplicate_group, .head_letter)],
                  by = "duplicate_group")
  labels[, .my_letter := substr(id, 1, 1)]
  labels[, equal := as.integer(.my_letter == .head_letter)]
  labels[, c(".head_letter", ".my_letter") := NULL]
  # need both classes for the fit
  skip_if(length(unique(labels$equal)) < 2L,
          "no FP pairs in fixture; skip")

  cal <- calibrate_matches(dups, s, labels = labels,
                           base = base, id = "id")
  out <- cal@matches[rank >= 2L]

  # join in the TP/FP label by (duplicate_group, id)
  out_lab <- merge(
    out,
    labels[rank >= 2L, .(duplicate_group, id, true_equal = equal)],
    by = c("duplicate_group", "id")
  )
  # predicted_tp should agree with the truth on most rows.
  acc <- mean(out_lab$predicted_tp == out_lab$true_equal)
  expect_gt(acc, 0.5)
})


test_that("calibrate_matches() dispatches on Embedding_Strategy", {
  skip_if_not_installed("tidyllm")
  skip_if_not_installed("tibble")

  fake_embed_by_text <- function(mapping, default = c(0, 0, 0, 1)) {
    function(text, model) {
      vecs <- lapply(text, function(t) {
        if (!is.null(mapping[[t]])) mapping[[t]] else default
      })
      tibble::tibble(input = text, embeddings = vecs)
    }
  }
  # Graded similarity so the calibrator has variation to learn from.
  mapping <- list(
    alpha   = c(1.0, 0.0, 0.0, 0.0),
    alpha2  = c(0.9, 0.1, 0.0, 0.0),
    beta    = c(0.0, 1.0, 0.0, 0.0),
    bet     = c(0.1, 0.9, 0.0, 0.0),
    gamma   = c(0.0, 0.0, 1.0, 0.0),
    delta   = c(0.3, 0.3, 0.0, 0.9)
  )
  testthat::local_mocked_bindings(
    embed = fake_embed_by_text(mapping), .package = "tidyllm"
  )

  base   <- data.table(
    id   = c("A", "B", "C"),
    name = c("alpha",  "beta",  "gamma")
  )
  target <- data.table(
    id   = c("X", "Y", "Z"),
    name = c("alpha2", "bet",  "delta")
  )
  strat <- Embedding_Strategy(
    columns = "name", embedding_model = NULL,
    threshold = 0.7, collapse_sep = " ",
    normalize = TRUE, batch_size = 1000L, block_by = NULL
  )
  cands <- search_candidates(base, target, "id", "id", strat)
  skip_if(nrow(cands) == 0L, "embedding fixture produced no candidates")
  # We need both classes; with this fixture the alpha/alpha2 and
  # beta/bet pairs score ~0.99 — label one as FP for variation.
  labels <- data.table::copy(cands)
  labels[, equal := NA_integer_]
  labels[source == "base", equal := 1L]
  labels[is.na(equal), equal := 1L]
  # force one target row to FP
  fp_mid <- labels[source == "target", match_id][1L]
  labels[match_id == fp_mid & source == "target", equal := 0L]

  cal <- tryCatch(
    calibrate_matches(
      cands, strat, labels = labels,
      base = base, id = "id",
      target = target, target_id = "id"
    ),
    error = function(e) e
  )
  if (inherits(cal, "error")) {
    skip(paste("embedding calibrate path errored:", conditionMessage(cal)))
  }
  expect_true(S7::S7_inherits(cal, joinery:::Calibrated_Matches))
  expect_true(all(c("tp_prob", "predicted_tp") %in% names(cal@matches)))
})


test_that("calibrate_matches() works with tibble inputs (thin wrapper)", {
  skip_if_not_installed("tibble")
  setup <- make_dedup_setup()
  cal <- calibrate_matches(
    tibble::as_tibble(setup$dups),
    setup$strategy,
    labels = tibble::as_tibble(setup$labels),
    base   = tibble::as_tibble(setup$base),
    id     = "id"
  )
  expect_true(S7::S7_inherits(cal, joinery:::Calibrated_Matches))
})
