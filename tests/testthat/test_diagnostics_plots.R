# ============================================================
# Tests for diagnostic plot functions
# ============================================================
#
# Policy (CLAUDE.md):
#   - NO raster snapshot output, NO visual regression testing
#   - pdf(NULL) / grDevices::dev.off() to suppress rendering
#   - Assert: runs without error, returns invisible data.table,
#     ... override accepted, error on wrong match_type / NULL slots

library(data.table)

# ---------------------------------------------------------------------------
# Device-suppression helper
# ---------------------------------------------------------------------------

with_null_device <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
}


# ---------------------------------------------------------------------------
# Fixture builders
# ---------------------------------------------------------------------------

make_audit_obj <- function(with_block = FALSE, with_target = FALSE) {
  strat_args <- list(
    first_name ~ normalize_text() + word_tokens(),
    last_name  ~ normalize_text() + word_tokens(),
    threshold  = 0.9
  )
  if (with_block) strat_args$block_by <- "region"
  strat <- do.call(search_strategy, strat_args)

  base <- data.table::data.table(
    id         = paste0("r", 1:10),
    first_name = c("alice", "bob",   "carol",  "david", "eve",
                   "frank", "grace", "harry",  "iris",  "jack"),
    last_name  = c("smith", "jones", "brown",  "davis", "evans",
                   "ford",  "green", "hill",   "irwin", "james")
  )
  if (with_block)
    base[, region := rep(c("north", "south"), each = 5L)]

  if (with_target) {
    target <- data.table::data.table(
      id         = paste0("t", 1:5),
      first_name = c("alice", "bob", "X", "Y", "Z"),
      last_name  = c("smith", "jones", "A", "B", "C")
    )
    if (with_block) target[, region := rep(c("north", "south", "north"), len = 5L)]
    audit_strategy(base, "id", strat, target = target)
  } else {
    audit_strategy(base, "id", strat)
  }
}

make_dup_overview <- function() {
  dt <- data.table::data.table(
    duplicate_group = c(1L, 1L, 1L, 2L, 2L),
    id              = c("a", "b", "c", "d", "e"),
    score           = c(0.95, 0.90, 0.88, 0.80, 0.78),
    rank            = c(1L, 2L, 3L, 1L, 2L)
  )
  base <- data.table::data.table(id = letters[1:8])
  summarise_matches(dt, base = base)
}

make_cand_overview <- function() {
  dt <- data.table::data.table(
    match_id = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
    score    = c(0.95, 0.95, 0.92, 0.92, 0.30, 0.30, 0.85, 0.85),
    source   = c("base", "target", "base", "target",
                 "base", "target", "base", "target"),
    id       = c("a", "x", "a", "y", "a", "z", "b", "x"),
    rank     = 1L
  )
  base   <- data.table::data.table(id = letters[1:5])
  target <- data.table::data.table(id = c("x", "y", "z", "w"))
  summarise_matches(dt, base = base, target = target)
}

make_expl_obj <- function() {
  data_dt <- data.table::data.table(
    id         = c("r1", "r2", "r3"),
    first_name = c("alice", "alice", "bob"),
    last_name  = c("smith", "smyth", "jones")
  )
  strat   <- search_strategy(
    first_name ~ normalize_text() + word_tokens(),
    last_name  ~ normalize_text() + word_tokens(),
    threshold  = 0.3
  )
  matches <- detect_duplicates(data_dt, "id", strat)
  # match_id = 1L is stable: detect_duplicates assigns group IDs in order of
  # first appearance; r1/r2 share "alice" and will always form group 1.
  explain_match(matches, strat, base = data_dt, id = "id", match_id = 1L)
}

make_stage_obj <- function() {
  token_rows <- data.table::data.table(
    match_id = rep(1L:4L, each = 2L),
    score    = rep(c(0.95, 0.90, 0.88, 0.85), each = 2L),
    stage    = "token",
    source   = rep(c("base", "target"), times = 4L),
    id       = c("b1", "t1", "b2", "t2", "b3", "t3", "b4", "t4"),
    rank     = 1L
  )
  emb_rows <- data.table::data.table(
    match_id = rep(5L:6L, each = 2L),
    score    = rep(c(0.80, 0.75), each = 2L),
    stage    = "embedding",
    source   = rep(c("base", "target"), times = 2L),
    id       = c("b5", "t5", "b6", "t6"),
    rank     = 1L
  )
  matches <- rbind(token_rows, emb_rows)
  base    <- data.table::data.table(id = paste0("b", 1:10))
  target  <- data.table::data.table(id = paste0("t", 1:10))
  compare_stages(matches, base = base, target = target)
}

make_sample_obj <- function() {
  dt <- data.table::data.table(
    duplicate_group = c(1L, 1L, 2L, 2L, 3L, 3L),
    id              = c("a", "b", "c", "d", "e", "f"),
    score           = c(0.95, 0.90, 0.80, 0.75, 0.60, 0.55),
    rank            = c(1L, 2L, 1L, 2L, 1L, 2L)
  )
  sample_matches(dt, mode = "high", n = 3L)
}


# ---------------------------------------------------------------------------
# rarity_histogram()
# ---------------------------------------------------------------------------

test_that("rarity_histogram runs without error", {
  sa <- make_audit_obj()
  expect_no_error(with_null_device(rarity_histogram(sa)))
})

test_that("rarity_histogram returns invisible data.table", {
  sa  <- make_audit_obj()
  out <- with_null_device(withVisible(rarity_histogram(sa)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
})

test_that("rarity_histogram ... override accepted", {
  sa <- make_audit_obj()
  expect_no_error(with_null_device(rarity_histogram(sa, main = "custom")))
})


# ---------------------------------------------------------------------------
# token_frequency_plot()
# ---------------------------------------------------------------------------

test_that("token_frequency_plot runs without error", {
  sa <- make_audit_obj()
  expect_no_error(with_null_device(token_frequency_plot(sa)))
})

test_that("token_frequency_plot returns invisible data.table", {
  sa  <- make_audit_obj()
  out <- with_null_device(withVisible(token_frequency_plot(sa)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
})

test_that("token_frequency_plot ... override accepted", {
  sa <- make_audit_obj()
  expect_no_error(with_null_device(token_frequency_plot(sa, main = "custom")))
})


# ---------------------------------------------------------------------------
# block_size_plot()
# ---------------------------------------------------------------------------

test_that("block_size_plot runs without error when block_by is set", {
  sa <- make_audit_obj(with_block = TRUE)
  expect_no_error(with_null_device(block_size_plot(sa)))
})

test_that("block_size_plot returns invisible data.table", {
  sa  <- make_audit_obj(with_block = TRUE)
  out <- with_null_device(withVisible(block_size_plot(sa)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
})

test_that("block_size_plot errors when block_summary is NULL", {
  sa <- make_audit_obj(with_block = FALSE)
  expect_error(block_size_plot(sa), "block_by")
})

test_that("block_size_plot ... override accepted", {
  sa <- make_audit_obj(with_block = TRUE)
  expect_no_error(with_null_device(block_size_plot(sa, main = "custom")))
})


# ---------------------------------------------------------------------------
# vocab_overlap_plot()
# ---------------------------------------------------------------------------

test_that("vocab_overlap_plot runs without error when target supplied", {
  sa <- make_audit_obj(with_target = TRUE)
  expect_no_error(with_null_device(vocab_overlap_plot(sa)))
})

test_that("vocab_overlap_plot returns invisible data.table", {
  sa  <- make_audit_obj(with_target = TRUE)
  out <- with_null_device(withVisible(vocab_overlap_plot(sa)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
})

test_that("vocab_overlap_plot errors when no target was supplied", {
  sa <- make_audit_obj(with_target = FALSE)
  expect_error(vocab_overlap_plot(sa), "target")
})

test_that("vocab_overlap_plot ... override accepted", {
  sa <- make_audit_obj(with_target = TRUE)
  expect_no_error(with_null_device(vocab_overlap_plot(sa, main = "custom")))
})


# ---------------------------------------------------------------------------
# score_histogram()
# ---------------------------------------------------------------------------

test_that("score_histogram runs without error on duplicates overview", {
  ov <- make_dup_overview()
  expect_no_error(with_null_device(score_histogram(ov)))
})

test_that("score_histogram returns invisible data.table", {
  ov  <- make_dup_overview()
  out <- with_null_device(withVisible(score_histogram(ov)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
})

test_that("score_histogram returned data.table has bin_mid column", {
  ov  <- make_dup_overview()
  out <- with_null_device(score_histogram(ov))
  expect_true("bin_mid" %in% names(out))
})

test_that("score_histogram threshold arg runs without error", {
  ov <- make_dup_overview()
  expect_no_error(with_null_device(score_histogram(ov, threshold = 0.85)))
})

test_that("score_histogram ... override accepted", {
  ov <- make_dup_overview()
  expect_no_error(with_null_device(score_histogram(ov, main = "custom")))
})

test_that("score_histogram works on candidates overview too", {
  ov <- make_cand_overview()
  expect_no_error(with_null_device(score_histogram(ov)))
})


# ---------------------------------------------------------------------------
# score_density()
# ---------------------------------------------------------------------------

test_that("score_density runs without error", {
  ov <- make_dup_overview()
  expect_no_error(with_null_device(score_density(ov)))
})

test_that("score_density returns invisible data.table", {
  ov  <- make_dup_overview()
  out <- with_null_device(withVisible(score_density(ov)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
})

test_that("score_density ... override accepted", {
  ov <- make_dup_overview()
  expect_no_error(with_null_device(score_density(ov, main = "custom")))
})


# ---------------------------------------------------------------------------
# coverage_plot()
# ---------------------------------------------------------------------------

test_that("coverage_plot runs without error when coverage available", {
  ov <- make_dup_overview()
  expect_no_error(with_null_device(coverage_plot(ov)))
})

test_that("coverage_plot returns invisible data.table", {
  ov  <- make_dup_overview()
  out <- with_null_device(withVisible(coverage_plot(ov)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
})

test_that("coverage_plot works with both base and target coverage", {
  ov <- make_cand_overview()
  expect_no_error(with_null_device(coverage_plot(ov)))
  out <- with_null_device(coverage_plot(ov))
  expect_true("target" %in% out$side)
})

test_that("coverage_plot errors when all coverage values are NA", {
  ov <- summarise_matches(data.table::data.table(
    duplicate_group = c(1L, 1L),
    id = c("a", "b"),
    score = c(0.9, 0.9),
    rank = c(1L, 2L)
  ))
  expect_error(coverage_plot(ov), "NA")
})

test_that("coverage_plot ... override accepted", {
  ov <- make_dup_overview()
  expect_no_error(with_null_device(coverage_plot(ov, main = "custom")))
})


# ---------------------------------------------------------------------------
# cluster_size_plot()
# ---------------------------------------------------------------------------

test_that("cluster_size_plot runs without error on duplicates overview", {
  ov <- make_dup_overview()
  expect_no_error(with_null_device(cluster_size_plot(ov)))
})

test_that("cluster_size_plot returns invisible data.table", {
  ov  <- make_dup_overview()
  out <- with_null_device(withVisible(cluster_size_plot(ov)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
})

test_that("cluster_size_plot errors on candidates overview", {
  ov <- make_cand_overview()
  expect_error(cluster_size_plot(ov), "candidates")
})

test_that("cluster_size_plot ... override accepted", {
  ov <- make_dup_overview()
  expect_no_error(with_null_device(cluster_size_plot(ov, main = "custom")))
})


# ---------------------------------------------------------------------------
# ambiguity_plot()
# ---------------------------------------------------------------------------

test_that("ambiguity_plot runs without error on candidates overview", {
  ov <- make_cand_overview()
  expect_no_error(with_null_device(ambiguity_plot(ov)))
})

test_that("ambiguity_plot returns invisible data.table", {
  ov  <- make_cand_overview()
  out <- with_null_device(withVisible(ambiguity_plot(ov)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
})

test_that("ambiguity_plot errors on duplicates overview", {
  ov <- make_dup_overview()
  expect_error(ambiguity_plot(ov), "duplicates")
})

test_that("ambiguity_plot ... override accepted", {
  ov <- make_cand_overview()
  expect_no_error(with_null_device(ambiguity_plot(ov, main = "custom")))
})


# ---------------------------------------------------------------------------
# top_gap_density()
# ---------------------------------------------------------------------------

test_that("top_gap_density runs without error on candidates with gaps", {
  ov <- make_cand_overview()
  expect_no_error(with_null_device(top_gap_density(ov)))
})

test_that("top_gap_density returns invisible data.table", {
  ov  <- make_cand_overview()
  out <- with_null_device(withVisible(top_gap_density(ov)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
})

test_that("top_gap_density returned data.table has bin_mid column", {
  ov  <- make_cand_overview()
  out <- with_null_device(top_gap_density(ov))
  expect_true("bin_mid" %in% names(out))
})

test_that("top_gap_density errors on duplicates overview", {
  ov <- make_dup_overview()
  expect_error(top_gap_density(ov), "candidates")
})

test_that("top_gap_density errors when top_gap_dist is NULL", {
  # Every base record has exactly one candidate -> no gaps
  cand <- data.table::data.table(
    match_id = rep(1:3, each = 2L),
    score    = rep(c(0.85, 0.80, 0.75), each = 2L),
    source   = rep(c("base", "target"), times = 3L),
    id       = c("a", "x", "b", "y", "c", "z"),
    rank     = 1L
  )
  ov <- summarise_matches(cand)
  expect_error(top_gap_density(ov), "NULL")
})

test_that("top_gap_density ... override accepted", {
  ov <- make_cand_overview()
  expect_no_error(with_null_device(top_gap_density(ov, main = "custom")))
})


# ---------------------------------------------------------------------------
# contribution_plot()
# ---------------------------------------------------------------------------

test_that("contribution_plot runs without error", {
  ex <- make_expl_obj()
  expect_no_error(with_null_device(contribution_plot(ex)))
})

test_that("contribution_plot returns invisible data.table", {
  ex  <- make_expl_obj()
  out <- with_null_device(withVisible(contribution_plot(ex)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
})

test_that("contribution_plot ... override accepted", {
  ex <- make_expl_obj()
  expect_no_error(with_null_device(contribution_plot(ex, main = "custom")))
})


# ---------------------------------------------------------------------------
# token_contribution_plot()
# ---------------------------------------------------------------------------

test_that("token_contribution_plot runs without error", {
  ex <- make_expl_obj()
  expect_no_error(with_null_device(token_contribution_plot(ex)))
})

test_that("token_contribution_plot returns invisible data.table", {
  ex  <- make_expl_obj()
  out <- with_null_device(withVisible(token_contribution_plot(ex)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
})

test_that("token_contribution_plot returned data.table has token_label column", {
  ex  <- make_expl_obj()
  out <- with_null_device(token_contribution_plot(ex))
  expect_true("token_label" %in% names(out))
})

test_that("token_contribution_plot ... override accepted", {
  ex <- make_expl_obj()
  expect_no_error(with_null_device(token_contribution_plot(ex, main = "custom")))
})

test_that("contribution_plot errors when per_column_contrib is NULL", {
  ex <- make_expl_obj()
  ex@per_column_contrib <- NULL
  expect_error(contribution_plot(ex), "NULL")
})

test_that("token_contribution_plot errors when shared_tokens is NULL", {
  ex <- make_expl_obj()
  ex@shared_tokens <- NULL
  expect_error(token_contribution_plot(ex), "NULL")
})


# ---------------------------------------------------------------------------
# stage_coverage_plot()
# ---------------------------------------------------------------------------

test_that("stage_coverage_plot runs without error (pct branch)", {
  sc <- make_stage_obj()
  expect_no_error(with_null_device(stage_coverage_plot(sc)))
})

test_that("stage_coverage_plot returns invisible data.table", {
  sc  <- make_stage_obj()
  out <- with_null_device(withVisible(stage_coverage_plot(sc)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
})

test_that("stage_coverage_plot returned data.table has stage_idx column", {
  sc  <- make_stage_obj()
  out <- with_null_device(stage_coverage_plot(sc))
  expect_true("stage_idx" %in% names(out))
})

test_that("stage_coverage_plot runs on count branch (no base supplied)", {
  token_rows <- data.table::data.table(
    match_id = rep(1L:3L, each = 2L),
    score    = rep(c(0.95, 0.90, 0.85), each = 2L),
    stage    = "s1",
    source   = rep(c("base", "target"), times = 3L),
    id       = c("b1", "t1", "b2", "t2", "b3", "t3"),
    rank     = 1L
  )
  emb_rows <- data.table::data.table(
    match_id = 4L,
    score    = 0.80,
    stage    = "s2",
    source   = c("base", "target"),
    id       = c("b4", "t4"),
    rank     = 1L
  )
  sc_no_base <- compare_stages(rbind(token_rows, emb_rows))
  expect_no_error(with_null_device(stage_coverage_plot(sc_no_base)))
})

test_that("stage_coverage_plot ... override accepted", {
  sc <- make_stage_obj()
  expect_no_error(with_null_device(stage_coverage_plot(sc, main = "custom")))
})


# ---------------------------------------------------------------------------
# stage_score_plot()
# ---------------------------------------------------------------------------

test_that("stage_score_plot runs without error", {
  sc <- make_stage_obj()
  expect_no_error(with_null_device(stage_score_plot(sc)))
})

test_that("stage_score_plot returns invisible data.table", {
  sc  <- make_stage_obj()
  out <- with_null_device(withVisible(stage_score_plot(sc)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
})

test_that("stage_score_plot returned data.table has bin_mid column", {
  sc  <- make_stage_obj()
  out <- with_null_device(stage_score_plot(sc))
  expect_true("bin_mid" %in% names(out))
})

test_that("stage_score_plot ... override accepted", {
  sc <- make_stage_obj()
  expect_no_error(with_null_device(stage_score_plot(sc, main = "custom")))
})

test_that("stage_score_plot works with single-stage input", {
  single_stage <- data.table::data.table(
    match_id = rep(1L:3L, each = 2L),
    score    = rep(c(0.95, 0.90, 0.85), each = 2L),
    stage    = "token",
    source   = rep(c("base", "target"), times = 3L),
    id       = c("b1", "t1", "b2", "t2", "b3", "t3"),
    rank     = 1L
  )
  sc_single <- compare_stages(single_stage)
  expect_no_error(with_null_device(stage_score_plot(sc_single)))
})


# ---------------------------------------------------------------------------
# Default plot() methods
# ---------------------------------------------------------------------------

test_that("plot.Match_Overview dispatches to score_histogram (has bin_mid)", {
  ov  <- make_dup_overview()
  out <- with_null_device(withVisible(plot(ov)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
  # score_histogram uniquely returns a table with bin_mid
  expect_true("bin_mid" %in% names(out$value))
})

test_that("plot.Strategy_Audit dispatches to rarity_histogram (has rarity_p50)", {
  sa  <- make_audit_obj()
  out <- with_null_device(withVisible(plot(sa)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
  # rarity_histogram uniquely returns column_rarity_stats with rarity_p50
  expect_true("rarity_p50" %in% names(out$value))
})

test_that("plot.Match_Explanation dispatches to contribution_plot (has src_column)", {
  ex  <- make_expl_obj()
  out <- with_null_device(withVisible(plot(ex)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
  # contribution_plot returns per_column_contrib with src_column
  expect_true("src_column" %in% names(out$value))
})

test_that("plot.Match_Sample returns invisible data.table with score column", {
  smp <- make_sample_obj()
  out <- with_null_device(withVisible(plot(smp)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
  expect_true("score" %in% names(out$value))
})

test_that("plot.Stage_Comparison dispatches to stage_coverage_plot (has stage_idx)", {
  sc  <- make_stage_obj()
  out <- with_null_device(withVisible(plot(sc)))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.table")
  # stage_coverage_plot uniquely adds stage_idx column
  expect_true("stage_idx" %in% names(out$value))
})
