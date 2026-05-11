# Tests for the recommendations catalog and dispatcher (Phase 0.6 M1).

test_that("dispatcher returns empty when no signals trigger", {
  out <- joinery:::.dispatch_recommendations(list(
    pct_records_with_ge3_matches = 0.01,
    score_top_gap_median         = 0.5,
    max_cluster_size             = 2,
    base_coverage_candidates     = 0.9
  ))
  expect_identical(out$ids, character(0))
  expect_identical(out$messages, character(0))
})

test_that("dispatcher fires each catalog entry when its trigger is met", {
  out <- joinery:::.dispatch_recommendations(list(
    pct_records_with_ge3_matches = 0.20,
    score_top_gap_median         = 0.01,
    max_cluster_size             = 100,
    base_coverage_candidates     = 0.01
  ))
  expect_setequal(
    out$ids,
    c(
      "candidates_high_ambiguity",
      "candidates_weak_decisiveness",
      "duplicates_mega_cluster",
      "low_coverage_candidates"
    )
  )
  expect_equal(length(out$messages), 4L)
})

test_that("messages embed the computed value", {
  out <- joinery:::.dispatch_recommendations(list(
    pct_records_with_ge3_matches = 0.125
  ))
  expect_match(out$messages, "12\\.5%")
})

test_that("NA and missing signals are ignored", {
  out <- joinery:::.dispatch_recommendations(list(
    max_cluster_size = NA_real_
  ))
  expect_identical(out$ids, character(0))
})

test_that("dispatcher respects each operator", {
  # max_cluster_size threshold is `>= 50`; exactly 50 should fire
  out <- joinery:::.dispatch_recommendations(list(max_cluster_size = 50))
  expect_true("duplicates_mega_cluster" %in% out$ids)
  out2 <- joinery:::.dispatch_recommendations(list(max_cluster_size = 49))
  expect_false("duplicates_mega_cluster" %in% out2$ids)
})
