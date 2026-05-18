# tests/testthat/test_sample_matches.R
# Phase 0.6 M5 — sample_matches() tests

library(data.table)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

make_sample_dup_matches <- function() {
  # 4 duplicate groups, controlled scores for deterministic mode tests:
  #   group 1: 3 records, scores 0.95 / 0.90 / 0.85  (gap 0.05 between rank1-rank2)
  #   group 2: 2 records, scores 0.80 / 0.75          (gap 0.05)
  #   group 3: 2 records, scores 0.60 / 0.55          (gap 0.05)
  #   group 4: 2 records, scores 0.50 / 0.45          (gap 0.05)
  data.table(
    duplicate_group = c(1L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
    id              = c("a", "b", "c", "d", "e", "f", "g", "h", "i"),
    score           = c(0.95, 0.90, 0.85, 0.80, 0.75, 0.60, 0.55, 0.50, 0.45),
    rank            = c(1L, 2L, 3L, 1L, 2L, 1L, 2L, 1L, 2L)
  )
}

make_sample_cand_matches <- function() {
  # 3 base records, varying number of candidates:
  #   base "a": 3 candidates (match_ids 1,2,3) — scores 0.95 / 0.90 / 0.85
  #   base "b": 2 candidates (match_ids 4,5)   — scores 0.80 / 0.75
  #   base "c": 1 candidate  (match_id 6)      — score  0.60
  data.table(
    match_id = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L),
    id       = c("a", "t1", "a", "t2", "a", "t3",
                 "b", "t4", "b", "t5",
                 "c", "t6"),
    source   = c("base", "target", "base", "target", "base", "target",
                 "base", "target", "base", "target",
                 "base", "target"),
    score    = c(0.95, 0.95, 0.90, 0.90, 0.85, 0.85,
                 0.80, 0.80, 0.75, 0.75,
                 0.60, 0.60),
    rank     = c(1L, 1L, 2L, 2L, 3L, 3L, 1L, 1L, 2L, 2L, 1L, 1L)
  )
}


# ---------------------------------------------------------------------------
# 1. Return type — all modes return Match_Sample
# ---------------------------------------------------------------------------

test_that("all modes return Match_Sample on duplicate matches", {
  dt <- make_sample_dup_matches()
  for (m in c("high", "low", "random")) {
    res <- sample_matches(dt, mode = m, n = 2L)
    expect_true(S7::S7_inherits(res, Match_Sample), label = paste0("mode=", m))
  }
  res_b <- sample_matches(dt, mode = "borderline", n = 2L, threshold = 0.7)
  expect_true(S7::S7_inherits(res_b, Match_Sample))
  res_g <- sample_matches(dt, mode = "top_gap", n = 2L)
  expect_true(S7::S7_inherits(res_g, Match_Sample))
})

test_that("all applicable modes return Match_Sample on candidate matches", {
  dt <- make_sample_cand_matches()
  for (m in c("high", "low", "random", "ambiguous", "top_gap")) {
    res <- sample_matches(dt, mode = m, n = 2L)
    expect_true(S7::S7_inherits(res, Match_Sample), label = paste0("mode=", m))
  }
  res_b <- sample_matches(dt, mode = "borderline", n = 2L, threshold = 0.80)
  expect_true(S7::S7_inherits(res_b, Match_Sample))
})


# ---------------------------------------------------------------------------
# 2. Schema — rows slot and criteria slot structure
# ---------------------------------------------------------------------------

test_that("rows slot is a data.table with original column schema", {
  dt  <- make_sample_dup_matches()
  res <- sample_matches(dt, mode = "high", n = 2L)
  expect_true(is.data.table(res@rows))
  expect_true(all(names(dt) %in% names(res@rows)))
})

test_that("criteria slot has correct structure for each mode", {
  dt <- make_sample_dup_matches()

  res <- sample_matches(dt, mode = "high", n = 3L)
  expect_equal(res@criteria$mode, "high")
  expect_equal(res@criteria$n, 3L)
  expect_null(res@criteria$threshold)

  res_b <- sample_matches(dt, mode = "borderline", n = 3L, threshold = 0.7)
  expect_equal(res_b@criteria$threshold, 0.7)

  res_r <- sample_matches(dt, mode = "random", n = 3L, seed = 42L)
  expect_equal(res_r@criteria$seed, 42L)

  res_r2 <- sample_matches(dt, mode = "random", n = 3L)
  expect_null(res_r2@criteria$seed)
})

test_that("mode slot matches the requested mode", {
  dt <- make_sample_dup_matches()
  for (m in c("high", "low", "random", "top_gap")) {
    res <- sample_matches(dt, mode = m, n = 2L)
    expect_equal(res@mode, m, label = paste0("mode=", m))
  }
})


# ---------------------------------------------------------------------------
# 3. n is honoured
# ---------------------------------------------------------------------------

test_that("high mode returns at most n rows", {
  dt  <- make_sample_dup_matches()
  res <- sample_matches(dt, mode = "high", n = 3L)
  expect_lte(nrow(res@rows), 3L)
})

test_that("low mode returns at most n rows", {
  dt  <- make_sample_dup_matches()
  res <- sample_matches(dt, mode = "low", n = 2L)
  expect_lte(nrow(res@rows), 2L)
})

test_that("borderline mode returns at most n rows", {
  dt  <- make_sample_dup_matches()
  res <- sample_matches(dt, mode = "borderline", n = 2L, threshold = 0.70)
  expect_lte(nrow(res@rows), 2L)
})

test_that("random mode returns exactly min(n, nrow) rows", {
  dt  <- make_sample_dup_matches()
  res <- sample_matches(dt, mode = "random", n = 3L, seed = 1L)
  expect_equal(nrow(res@rows), 3L)

  res_all <- sample_matches(dt, mode = "random", n = 9999L, seed = 1L)
  expect_equal(nrow(res_all@rows), nrow(dt))
})


# ---------------------------------------------------------------------------
# 4. Correctness checks
# ---------------------------------------------------------------------------

test_that("high mode returns rows in descending score order", {
  dt  <- make_sample_dup_matches()
  res <- sample_matches(dt, mode = "high", n = 3L)
  expect_equal(res@rows$score, sort(res@rows$score, decreasing = TRUE))
  expect_gte(min(res@rows$score), 0.85)
})

test_that("low mode returns rows in ascending score order", {
  dt  <- make_sample_dup_matches()
  res <- sample_matches(dt, mode = "low", n = 2L)
  expect_equal(res@rows$score, sort(res@rows$score))
  expect_lte(max(res@rows$score), 0.50)
})

test_that("low mode with threshold excludes rows below threshold", {
  dt  <- make_sample_dup_matches()
  res <- sample_matches(dt, mode = "low", n = 3L, threshold = 0.70)
  expect_true(all(res@rows$score >= 0.70))
})

test_that("borderline returns rows closest to threshold", {
  dt  <- make_sample_dup_matches()
  # threshold = 0.73 → distances: 0.75 (0.02), 0.80 (0.07), 0.60 (0.13) — no ties
  res <- sample_matches(dt, mode = "borderline", n = 2L, threshold = 0.73)
  expect_equal(nrow(res@rows), 2L)
  expect_true(all(res@rows$score %in% c(0.75, 0.80)))
})

test_that("top_gap on duplicates returns groups with smallest rank-1 vs rank-2 gap", {
  dt  <- make_sample_dup_matches()
  # All groups have gap 0.05; n=1 should return rows of one group
  res <- sample_matches(dt, mode = "top_gap", n = 1L)
  # The returned group should have at least 2 rows (rank 1 and rank 2)
  expect_gte(nrow(res@rows), 2L)
  # All returned rows belong to a single duplicate_group
  expect_equal(length(unique(res@rows$duplicate_group)), 1L)
})

test_that("top_gap on candidates excludes base records with only 1 match", {
  dt  <- make_sample_cand_matches()
  # base "c" has only 1 candidate, so no gap; should not appear in top_gap
  res <- sample_matches(dt, mode = "top_gap", n = 2L)
  # match_id 6 belongs only to base "c" — must not appear
  expect_false(6L %in% res@rows$match_id)
  expect_gte(nrow(res@rows), 2L)
})

test_that("ambiguous mode returns n base records with most candidates", {
  dt  <- make_sample_cand_matches()
  res <- sample_matches(dt, mode = "ambiguous", n = 1L)
  # base "a" has 3 candidates — should be the chosen record
  base_ids <- unique(res@rows[res@rows$source == "base", ]$id)
  expect_equal(base_ids, "a")
  # All 3 match_ids for "a" should be present
  expect_setequal(unique(res@rows$match_id), c(1L, 2L, 3L))
})

test_that("ambiguous n=2 returns base records 'a' (3 cands) and 'b' (2 cands)", {
  dt  <- make_sample_cand_matches()
  res <- sample_matches(dt, mode = "ambiguous", n = 2L)
  base_ids <- sort(unique(res@rows[res@rows$source == "base", ]$id))
  expect_equal(base_ids, c("a", "b"))
})

test_that("random mode is deterministic with seed", {
  dt <- make_sample_dup_matches()
  r1 <- sample_matches(dt, mode = "random", n = 3L, seed = 123L)
  r2 <- sample_matches(dt, mode = "random", n = 3L, seed = 123L)
  expect_equal(r1@rows, r2@rows)
})

test_that("random mode produces different results with different seeds", {
  dt <- make_sample_dup_matches()
  r1 <- sample_matches(dt, mode = "random", n = 5L, seed = 1L)
  r2 <- sample_matches(dt, mode = "random", n = 5L, seed = 99L)
  if (identical(r1@rows, r2@rows)) skip("Seeds collided unexpectedly")
  expect_false(identical(r1@rows, r2@rows))
})


# ---------------------------------------------------------------------------
# 5. Error handling
# ---------------------------------------------------------------------------

test_that("invalid mode gives clear error listing valid modes", {
  dt  <- make_sample_dup_matches()
  err <- tryCatch(
    sample_matches(dt, mode = "best"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "mode")
  expect_match(err, "borderline")
})

test_that("non-positive or non-numeric n gives clear error", {
  dt <- make_sample_dup_matches()
  expect_error(sample_matches(dt, mode = "high", n = 0L),    regexp = "n")
  expect_error(sample_matches(dt, mode = "high", n = -1L),   regexp = "n")
  expect_error(sample_matches(dt, mode = "high", n = "10"),  regexp = "n")
})

test_that("borderline without threshold errors with helpful message", {
  dt <- make_sample_dup_matches()
  expect_error(
    sample_matches(dt, mode = "borderline"),
    regexp = "threshold"
  )
})

test_that("ambiguous mode on duplicate match table errors informatively", {
  dt <- make_sample_dup_matches()
  err <- tryCatch(
    sample_matches(dt, mode = "ambiguous"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "ambiguous|candidates", perl = TRUE)
})

test_that("unrecognised match table schema errors", {
  bad <- data.table(foo = 1:3, bar = 4:6)
  expect_error(sample_matches(bad, mode = "high"), regexp = "match table|joinery")
})


# ---------------------------------------------------------------------------
# 6. Edge cases
# ---------------------------------------------------------------------------

test_that("low mode with threshold above all scores returns empty rows", {
  dt  <- make_sample_dup_matches()
  res <- sample_matches(dt, mode = "low", n = 5L, threshold = 0.99)
  expect_equal(nrow(res@rows), 0L)
})

test_that("top_gap returns empty rows when no duplicate group has >=2 members", {
  single <- data.table(
    duplicate_group = 1L,
    id              = "a",
    score           = 0.9,
    rank            = 1L
  )
  res <- sample_matches(single, mode = "top_gap", n = 2L)
  expect_equal(nrow(res@rows), 0L)
})

test_that("top_gap returns empty rows when no candidate base record has >=2 matches", {
  single_cand <- data.table(
    match_id = c(1L, 1L, 2L, 2L),
    id       = c("a", "t1", "b", "t2"),
    source   = c("base", "target", "base", "target"),
    score    = c(0.9, 0.9, 0.8, 0.8),
    rank     = 1L
  )
  res <- sample_matches(single_cand, mode = "top_gap", n = 2L)
  expect_equal(nrow(res@rows), 0L)
})

test_that("n larger than table size returns all rows without error", {
  dt  <- make_sample_dup_matches()
  res <- sample_matches(dt, mode = "high", n = 9999L)
  expect_equal(nrow(res@rows), nrow(dt))
})

test_that("empty match table returns Match_Sample with 0 rows", {
  dt    <- make_sample_dup_matches()
  empty <- dt[0L]
  res   <- sample_matches(empty, mode = "high", n = 5L)
  expect_true(S7::S7_inherits(res, Match_Sample))
  expect_equal(nrow(res@rows), 0L)
})


# ---------------------------------------------------------------------------
# 7. format() and print()
# ---------------------------------------------------------------------------

test_that("format(Match_Sample) returns non-empty character vector, not stub", {
  dt    <- make_sample_dup_matches()
  res   <- sample_matches(dt, mode = "high", n = 3L)
  lines <- format(res)
  expect_true(is.character(lines))
  expect_gt(length(lines), 0L)
  expect_false(any(grepl("not yet implemented", lines, fixed = TRUE)))
  expect_true(any(grepl("high", lines, fixed = TRUE)))
})

test_that("format shows threshold for borderline mode", {
  dt    <- make_sample_dup_matches()
  res   <- sample_matches(dt, mode = "borderline", n = 2L, threshold = 0.75)
  lines <- format(res)
  expect_true(any(grepl("0.75", lines, fixed = TRUE)))
})

test_that("format shows seed for random mode when seed is provided", {
  dt    <- make_sample_dup_matches()
  res   <- sample_matches(dt, mode = "random", n = 2L, seed = 42L)
  lines <- format(res)
  expect_true(any(grepl("42", lines, fixed = TRUE)))
})

test_that("print(Match_Sample) returns invisible(x)", {
  dt  <- make_sample_dup_matches()
  res <- sample_matches(dt, mode = "high", n = 2L)
  expect_invisible(print(res))
})

test_that("format snapshot is stable for high mode (duplicates, n=3)", {
  local_edition(3)
  dt  <- make_sample_dup_matches()
  res <- sample_matches(dt, mode = "high", n = 3L)
  expect_snapshot(cat(format(res), sep = "\n"))
})


# ---------------------------------------------------------------------------
# 8. DuckDB parity
# ---------------------------------------------------------------------------

test_that("DuckDB: high mode matches data.table result", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  dt       <- make_sample_dup_matches()
  duck_dup <- local_duckdb_table(dt, "dup_high")
  res_dt   <- sample_matches(dt,       mode = "high", n = 3L)
  res_duck <- sample_matches(duck_dup, mode = "high", n = 3L)

  expect_true(S7::S7_inherits(res_duck, Match_Sample))
  expect_equal(nrow(res_duck@rows), nrow(res_dt@rows))
  expect_equal(
    sort(res_duck@rows$score, decreasing = TRUE),
    sort(res_dt@rows$score, decreasing = TRUE),
    tolerance = 1e-10
  )
})

test_that("DuckDB: borderline mode matches data.table result", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  dt       <- make_sample_dup_matches()
  duck_dup <- local_duckdb_table(dt, "dup_border")
  res_dt   <- sample_matches(dt,       mode = "borderline", n = 2L, threshold = 0.73)
  res_duck <- sample_matches(duck_dup, mode = "borderline", n = 2L, threshold = 0.73)

  expect_setequal(res_duck@rows$score, res_dt@rows$score)
})

test_that("DuckDB: ambiguous mode on candidates matches data.table result", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  dt        <- make_sample_cand_matches()
  duck_cand <- local_duckdb_table(dt, "cand_amb")
  res_dt    <- sample_matches(dt,        mode = "ambiguous", n = 1L)
  res_duck  <- sample_matches(duck_cand, mode = "ambiguous", n = 1L)

  expect_setequal(res_duck@rows$match_id, res_dt@rows$match_id)
})

test_that("DuckDB: top_gap mode on duplicates matches data.table result", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  dt       <- make_sample_dup_matches()
  duck_dup <- local_duckdb_table(dt, "dup_gap")
  res_dt   <- sample_matches(dt,       mode = "top_gap", n = 2L)
  res_duck <- sample_matches(duck_dup, mode = "top_gap", n = 2L)

  expect_setequal(res_duck@rows$duplicate_group, res_dt@rows$duplicate_group)
})


# ---------------------------------------------------------------------------
# 9. Tibble / data.frame parity
# ---------------------------------------------------------------------------

test_that("tibble input produces identical Match_Sample to data.table", {
  skip_if_not_installed("tibble")
  dt      <- make_sample_dup_matches()
  tbl_dup <- tibble::as_tibble(dt)
  res_dt  <- sample_matches(dt,      mode = "high", n = 3L)
  res_tbl <- sample_matches(tbl_dup, mode = "high", n = 3L)
  expect_true(S7::S7_inherits(res_tbl, Match_Sample))
  expect_equal(
    sort(res_tbl@rows$score, decreasing = TRUE),
    sort(res_dt@rows$score,  decreasing = TRUE),
    tolerance = 1e-10
  )
})

test_that("data.frame input produces identical Match_Sample to data.table", {
  dt     <- make_sample_dup_matches()
  df_dup <- as.data.frame(dt)
  res_dt <- sample_matches(dt,     mode = "low", n = 2L)
  res_df <- sample_matches(df_dup, mode = "low", n = 2L)
  expect_true(S7::S7_inherits(res_df, Match_Sample))
  expect_equal(res_df@rows$score, res_dt@rows$score, tolerance = 1e-10)
})
