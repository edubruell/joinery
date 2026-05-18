# ============================================================
# Tests for compare_stages() — Phase 0.6 M6
# ============================================================
#
# Conventions (matching M1–M5 pattern):
#   - All fixtures are small, deterministic, hand-crafted tables
#   - DuckDB parity tests use local_duckdb_table() from helper-duckdb.R
#   - Tibble/data.frame parity tests use tibble::as_tibble() / as.data.frame()
#   - Snapshot covers format() for clean two-stage fixture
# ============================================================

library(data.table)

# ---------------------------------------------------------------------------
# Fixture builders
# ---------------------------------------------------------------------------

# 10 base records, 12 target records
make_ms_base <- function() {
  data.table(id = paste0("b", 1:10), name = paste0("base_", 1:10))
}

make_ms_target <- function() {
  data.table(id = paste0("t", 1:12), name = paste0("target_", 1:12))
}

# Two-stage candidate matches — disjoint ids (mirrors multi_stage_match output)
# Stage "token": 4 match pairs (base: b1-b4, target: t1-t4)
# Stage "embedding": 2 match pairs (base: b5-b6, target: t5-t6)
# → 4 base added by token, 2 base added by embedding; no overlap
make_two_stage_cand <- function() {
  token_rows <- data.table(
    match_id = rep(1L:4L, each = 2L),
    score    = rep(c(0.95, 0.90, 0.88, 0.85), each = 2L),
    stage    = "token",
    source   = rep(c("base", "target"), times = 4L),
    id       = c("b1", "t1", "b2", "t2", "b3", "t3", "b4", "t4"),
    rank     = 1L
  )
  emb_rows <- data.table(
    match_id = rep(5L:6L, each = 2L),
    score    = rep(c(0.80, 0.75), each = 2L),
    stage    = "embedding",
    source   = rep(c("base", "target"), times = 2L),
    id       = c("b5", "t5", "b6", "t6"),
    rank     = 1L
  )
  rbind(token_rows, emb_rows)
}

# Three-stage matches for arithmetic tests
# Stage "s1": base b1-b3, target t1-t3
# Stage "s2": base b4-b5, target t4-t5
# Stage "s3": base b6,    target t6
make_three_stage_cand <- function() {
  rbind(
    data.table(match_id = 1L:3L, score = c(0.95, 0.92, 0.90),
               stage = "s1", source = "base",   id = c("b1", "b2", "b3"), rank = 1L),
    data.table(match_id = 1L:3L, score = c(0.95, 0.92, 0.90),
               stage = "s1", source = "target", id = c("t1", "t2", "t3"), rank = 1L),
    data.table(match_id = 4L:5L, score = c(0.85, 0.82),
               stage = "s2", source = "base",   id = c("b4", "b5"), rank = 1L),
    data.table(match_id = 4L:5L, score = c(0.85, 0.82),
               stage = "s2", source = "target", id = c("t4", "t5"), rank = 1L),
    data.table(match_id = 6L, score = 0.78,
               stage = "s3", source = "base",   id = "b6", rank = 1L),
    data.table(match_id = 6L, score = 0.78,
               stage = "s3", source = "target", id = "t6", rank = 1L)
  )
}

# Low-yield fixture: stage "emb" adds only 1 of 10 base records (< 1% of 100)
# We use a big base table (100 records) so 1 / 100 < 0.01 threshold
make_low_yield_base <- function() {
  # 200 records so that 1 matched record = 0.5% < 1% threshold
  data.table(id = paste0("b", 1:200), name = paste0("base_", 1:200))
}

make_low_yield_matches <- function() {
  rbind(
    # "token" stage adds b1-b50 — healthy (50/200 = 25%)
    do.call(rbind, lapply(1:50, function(i) {
      data.table(
        match_id = as.integer(i),
        score    = 0.9,
        stage    = "token",
        source   = c("base", "target"),
        id       = c(paste0("b", i), paste0("t", i)),
        rank     = 1L
      )
    })),
    # "emb" stage adds only b51 — 1/200 = 0.5% < 1% threshold → fires low_yield_stage
    data.table(
      match_id = 51L,
      score    = 0.70,
      stage    = "emb",
      source   = c("base", "target"),
      id       = c("b51", "t51"),
      rank     = 1L
    )
  )
}

# Duplicates-style multi-stage table (for schema detection test)
make_dup_stage_matches <- function() {
  data.table(
    duplicate_group = c(1L, 1L, 2L, 2L, 3L, 3L),
    id              = c("a", "b", "c", "d", "e", "f"),
    score           = c(0.9, 0.9, 0.8, 0.8, 0.7, 0.7),
    stage           = c("s1", "s1", "s1", "s1", "s2", "s2"),
    rank            = c(1L, 2L, 1L, 2L, 1L, 2L)
  )
}


# ---------------------------------------------------------------------------
# 1. Validation
# ---------------------------------------------------------------------------

test_that("compare_stages errors without stage column", {
  no_stage <- data.table(
    match_id = 1L, score = 0.9, source = "base", id = "a", rank = 1L
  )
  expect_error(compare_stages(no_stage), "stage")
})

test_that("compare_stages succeeds with valid two-stage table", {
  expect_no_error(compare_stages(make_two_stage_cand()))
})


# ---------------------------------------------------------------------------
# 2. Return class
# ---------------------------------------------------------------------------

test_that("compare_stages returns Stage_Comparison", {
  res <- compare_stages(make_two_stage_cand())
  expect_true(S7::S7_inherits(res, Stage_Comparison))
})


# ---------------------------------------------------------------------------
# 3. per_stage_overview
# ---------------------------------------------------------------------------

test_that("per_stage_overview is a named list", {
  res <- compare_stages(make_two_stage_cand())
  expect_true(is.list(res@per_stage_overview))
  expect_true(!is.null(names(res@per_stage_overview)))
})

test_that("per_stage_overview names equal unique stages (insertion order)", {
  res <- compare_stages(make_two_stage_cand())
  expect_identical(names(res@per_stage_overview), c("token", "embedding"))
})

test_that("per_stage_overview length equals number of unique stages", {
  res <- compare_stages(make_two_stage_cand())
  expect_identical(length(res@per_stage_overview), 2L)
})

test_that("each per_stage_overview element is a Match_Overview", {
  res <- compare_stages(make_two_stage_cand())
  for (nm in names(res@per_stage_overview)) {
    expect_true(
      S7::S7_inherits(res@per_stage_overview[[nm]], Match_Overview),
      label = nm
    )
  }
})

test_that("per_stage_overview match_type is candidates for candidates fixture", {
  res <- compare_stages(make_two_stage_cand())
  for (nm in names(res@per_stage_overview)) {
    expect_identical(res@per_stage_overview[[nm]]@match_type, "candidates")
  }
})

test_that("per_stage_overview n_pairs match stage sizes", {
  res <- compare_stages(make_two_stage_cand())
  expect_identical(res@per_stage_overview[["token"]]@n_records$n_pairs_or_groups, 4L)
  expect_identical(res@per_stage_overview[["embedding"]]@n_records$n_pairs_or_groups, 2L)
})


# ---------------------------------------------------------------------------
# 4. Marginal coverage arithmetic (candidates, two-stage)
# ---------------------------------------------------------------------------

test_that("marginal_coverage is a data.table with correct columns", {
  res <- compare_stages(make_two_stage_cand())
  mc  <- res@marginal_coverage
  expect_s3_class(mc, "data.table")
  expect_true(all(c("stage", "base_added", "target_added",
                    "base_cumulative", "target_cumulative") %in% names(mc)))
})

test_that("marginal_coverage has one row per stage (insertion order)", {
  res <- compare_stages(make_two_stage_cand())
  expect_identical(nrow(res@marginal_coverage), 2L)
  expect_identical(res@marginal_coverage$stage, c("token", "embedding"))
})

test_that("base_added sums to final base_cumulative", {
  res <- compare_stages(make_two_stage_cand())
  mc  <- res@marginal_coverage
  expect_identical(sum(mc$base_added), mc$base_cumulative[nrow(mc)])
})

test_that("first stage base_added equals base_cumulative[1]", {
  res <- compare_stages(make_two_stage_cand())
  mc  <- res@marginal_coverage
  expect_identical(mc$base_added[1L], mc$base_cumulative[1L])
})

test_that("base_cumulative is monotone non-decreasing", {
  res <- compare_stages(make_three_stage_cand())
  mc  <- res@marginal_coverage
  expect_true(all(diff(mc$base_cumulative) >= 0L))
})

test_that("base_pct_cumulative is correct when base supplied", {
  base <- make_ms_base()
  res  <- compare_stages(make_two_stage_cand(), base = base)
  mc   <- res@marginal_coverage
  last <- mc[nrow(mc)]
  expect_equal(
    last$base_pct_cumulative,
    last$base_cumulative / nrow(base),
    tolerance = 1e-10
  )
})

test_that("base_pct_added is all NA when base not supplied", {
  res <- compare_stages(make_two_stage_cand())
  expect_true(all(is.na(res@marginal_coverage$base_pct_added)))
})

test_that("target_pct_cumulative is correct when target supplied", {
  target <- make_ms_target()
  res    <- compare_stages(make_two_stage_cand(), target = target)
  mc     <- res@marginal_coverage
  last   <- mc[nrow(mc)]
  expect_equal(
    last$target_pct_cumulative,
    last$target_cumulative / nrow(target),
    tolerance = 1e-10
  )
})


# ---------------------------------------------------------------------------
# 5. Marginal coverage — three-stage arithmetic
# ---------------------------------------------------------------------------

test_that("three-stage: each stage's base_added is correct", {
  res <- compare_stages(make_three_stage_cand())
  mc  <- res@marginal_coverage
  expect_identical(mc$base_added, c(3L, 2L, 1L))
})

test_that("three-stage: cumulative is monotone and correct", {
  res <- compare_stages(make_three_stage_cand())
  mc  <- res@marginal_coverage
  expect_identical(mc$base_cumulative, c(3L, 5L, 6L))
})

test_that("three-stage: sum of base_added == total matched", {
  res <- compare_stages(make_three_stage_cand())
  mc  <- res@marginal_coverage
  expect_identical(sum(mc$base_added), mc$base_cumulative[nrow(mc)])
})


# ---------------------------------------------------------------------------
# 6. Duplicates schema with stage column
# ---------------------------------------------------------------------------

test_that("compare_stages works with duplicates schema + stage column", {
  res <- compare_stages(make_dup_stage_matches())
  expect_true(S7::S7_inherits(res, Stage_Comparison))
  for (nm in names(res@per_stage_overview)) {
    expect_identical(res@per_stage_overview[[nm]]@match_type, "duplicates")
  }
})

test_that("duplicates: target_added is NA (no source column)", {
  res <- compare_stages(make_dup_stage_matches())
  expect_true(all(is.na(res@marginal_coverage$target_added)))
})


# ---------------------------------------------------------------------------
# 7. score_dist_by_stage
# ---------------------------------------------------------------------------

test_that("score_dist_by_stage has columns stage, bin_lower, bin_upper, count", {
  res <- compare_stages(make_two_stage_cand())
  sds <- res@score_dist_by_stage
  expect_true(all(c("stage", "bin_lower", "bin_upper", "count") %in% names(sds)))
})

test_that("score_dist_by_stage contains all stages", {
  res    <- compare_stages(make_two_stage_cand())
  stages <- unique(res@score_dist_by_stage$stage)
  expect_setequal(stages, c("token", "embedding"))
})

test_that("score_dist_by_stage count > 0 for non-empty stages", {
  res <- compare_stages(make_two_stage_cand())
  expect_true(all(res@score_dist_by_stage$count >= 0L))
  expect_true(sum(res@score_dist_by_stage$count) > 0L)
})


# ---------------------------------------------------------------------------
# 8. Recommendations
# ---------------------------------------------------------------------------

test_that("clean fixture: no recommendations", {
  res <- compare_stages(make_two_stage_cand(), base = make_ms_base())
  expect_identical(res@recommendations, character(0))
  expect_identical(attr(res, "recommendation_ids"), character(0))
})

test_that("low_yield_stage fires when stage adds < 1% of base", {
  big_base <- make_low_yield_base()
  res      <- compare_stages(make_low_yield_matches(), base = big_base)
  ids      <- attr(res, "recommendation_ids")
  expect_true("low_yield_stage" %in% ids)
  expect_true(length(res@recommendations) >= 1L)
  expect_match(res@recommendations[ids == "low_yield_stage"], "emb")
})

test_that("low_yield_stage does NOT fire without base (signal is NA)", {
  res <- compare_stages(make_low_yield_matches())
  expect_false("low_yield_stage" %in% attr(res, "recommendation_ids"))
})


# ---------------------------------------------------------------------------
# 9. DuckDB parity
# ---------------------------------------------------------------------------

test_that("DuckDB: per_stage_overview match_types match data.table", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  dt   <- make_two_stage_cand()
  duck <- local_duckdb_table(dt, "ms_cand")

  dt_res   <- compare_stages(dt)
  duck_res <- compare_stages(duck)

  for (nm in names(dt_res@per_stage_overview)) {
    expect_identical(
      duck_res@per_stage_overview[[nm]]@match_type,
      dt_res@per_stage_overview[[nm]]@match_type
    )
  }
})

test_that("DuckDB: marginal_coverage matches data.table", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  dt   <- make_two_stage_cand()
  duck <- local_duckdb_table(dt, "ms_cand2")

  dt_res   <- compare_stages(dt, base = make_ms_base(), target = make_ms_target())
  duck_res <- compare_stages(duck, base = make_ms_base(), target = make_ms_target())

  expect_identical(duck_res@marginal_coverage$base_added,
                   dt_res@marginal_coverage$base_added)
  expect_equal(duck_res@marginal_coverage$base_pct_cumulative,
               dt_res@marginal_coverage$base_pct_cumulative,
               tolerance = 1e-10)
})

test_that("DuckDB: score_dist_by_stage stage names match data.table", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  dt   <- make_two_stage_cand()
  duck <- local_duckdb_table(dt, "ms_score")

  dt_res   <- compare_stages(dt)
  duck_res <- compare_stages(duck)

  expect_setequal(unique(duck_res@score_dist_by_stage$stage),
                  unique(dt_res@score_dist_by_stage$stage))
})

test_that("DuckDB: recommendation_ids match data.table", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  big_base <- make_low_yield_base()
  dt       <- make_low_yield_matches()
  duck     <- local_duckdb_table(dt, "ms_low")

  dt_res   <- compare_stages(dt,   base = big_base)
  duck_res <- compare_stages(duck, base = big_base)

  expect_identical(attr(duck_res, "recommendation_ids"),
                   attr(dt_res,   "recommendation_ids"))
})


# ---------------------------------------------------------------------------
# 10. tibble / data.frame parity
# ---------------------------------------------------------------------------

test_that("tibble input gives same result as data.table", {
  skip_if_not_installed("tibble")

  dt  <- make_two_stage_cand()
  tbl <- tibble::as_tibble(dt)

  dt_res  <- compare_stages(dt)
  tbl_res <- compare_stages(tbl)

  expect_identical(names(tbl_res@per_stage_overview), names(dt_res@per_stage_overview))
  expect_identical(tbl_res@marginal_coverage$base_added,
                   dt_res@marginal_coverage$base_added)
})

test_that("data.frame input gives same result as data.table", {
  dt  <- make_two_stage_cand()
  df  <- as.data.frame(dt)

  dt_res <- compare_stages(dt)
  df_res <- compare_stages(df)

  expect_identical(names(df_res@per_stage_overview), names(dt_res@per_stage_overview))
  expect_identical(df_res@marginal_coverage$base_added,
                   dt_res@marginal_coverage$base_added)
})


# ---------------------------------------------------------------------------
# 11. format / print
# ---------------------------------------------------------------------------

test_that("format.Stage_Comparison returns character vector", {
  res <- compare_stages(make_two_stage_cand())
  out <- format(res)
  expect_type(out, "character")
  expect_true(length(out) > 0L)
})

test_that("format output contains stage names", {
  res  <- compare_stages(make_two_stage_cand())
  out  <- paste(format(res), collapse = "\n")
  expect_match(out, "token")
  expect_match(out, "embedding")
})

test_that("print.Stage_Comparison runs without error and returns invisible(x)", {
  res <- compare_stages(make_two_stage_cand())
  out <- withVisible(print(res))
  expect_false(out$visible)
  expect_true(S7::S7_inherits(out$value, Stage_Comparison))
})

test_that("format output — clean two-stage fixture contains key elements", {
  res <- compare_stages(make_two_stage_cand(), base = make_ms_base(), target = make_ms_target())
  out <- paste(format(res), collapse = "\n")
  expect_match(out, "Stage_Comparison")
  expect_match(out, "token.*embedding|embedding.*token")
  expect_match(out, "marginal coverage")
  expect_match(out, "base_added")
})


# ---------------------------------------------------------------------------
# 12. as.data.table / as.data.frame
# ---------------------------------------------------------------------------

test_that("as.data.table.Stage_Comparison returns data.table", {
  res <- compare_stages(make_two_stage_cand())
  dt  <- as.data.table(res)
  expect_s3_class(dt, "data.table")
})

test_that("as.data.table has one row per stage with key columns", {
  res <- compare_stages(make_two_stage_cand())
  dt  <- as.data.table(res)
  expect_identical(nrow(dt), 2L)
  expect_true(all(c("stage", "base_added", "base_cumulative") %in% names(dt)))
})

test_that("as.data.frame.Stage_Comparison returns data.frame", {
  res <- compare_stages(make_two_stage_cand())
  df  <- as.data.frame(res)
  expect_s3_class(df, "data.frame")
  expect_identical(nrow(df), 2L)
})


# ---------------------------------------------------------------------------
# 13. Edge cases
# ---------------------------------------------------------------------------

test_that("single-stage table works (no comparison across stages)", {
  single <- data.table(
    match_id = 1L:2L, score = c(0.9, 0.8), stage = "only",
    source = c("base", "target"), id = c("a", "x"), rank = 1L
  )
  res <- compare_stages(single)
  expect_true(S7::S7_inherits(res, Stage_Comparison))
  expect_identical(length(res@per_stage_overview), 1L)
  expect_identical(nrow(res@marginal_coverage), 1L)
})

test_that("empty matches table (0 rows) returns Stage_Comparison without error", {
  empty <- data.table(
    match_id = integer(), score = numeric(), stage = character(),
    source = character(), id = character(), rank = integer()
  )
  expect_no_error(compare_stages(empty))
  res <- compare_stages(empty)
  expect_true(S7::S7_inherits(res, Stage_Comparison))
  expect_identical(length(res@per_stage_overview), 0L)
})

test_that("recommendations slot contains character(0) when no recommendations fire", {
  res <- compare_stages(make_two_stage_cand())
  expect_type(res@recommendations, "character")
})
