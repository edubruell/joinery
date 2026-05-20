# Tests for the recommendations catalog and dispatcher.

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


# ---------------------------------------------------------------------------
# block_imbalanced and high_low_rarity_pressure
# ---------------------------------------------------------------------------

test_that("block_imbalanced fires when block_top_share > 0.70", {
  out <- joinery:::.dispatch_recommendations(list(block_top_share = 0.80))
  expect_true("block_imbalanced" %in% out$ids)
  expect_match(out$messages, "80")
})

test_that("block_imbalanced does NOT fire at exactly 0.70 (strict >)", {
  out <- joinery:::.dispatch_recommendations(list(block_top_share = 0.70))
  expect_false("block_imbalanced" %in% out$ids)
})

test_that("high_low_rarity_pressure fires and message contains column name and percentage", {
  out <- joinery:::.dispatch_recommendations(list(
    max_pct_low_rarity_tokens = 0.60,
    worst_rarity_column       = "firma"
  ))
  expect_true("high_low_rarity_pressure" %in% out$ids)
  expect_match(out$messages, "firma")
  expect_match(out$messages, "60\\.0%")
})

test_that("high_low_rarity_pressure does NOT fire at exactly 0.50 (strict >)", {
  out <- joinery:::.dispatch_recommendations(list(
    max_pct_low_rarity_tokens = 0.50,
    worst_rarity_column       = "firma"
  ))
  expect_false("high_low_rarity_pressure" %in% out$ids)
})

test_that("context_fn extension: existing 4 entries unaffected", {
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
