# Tests for summarise_matches() — Phase 0.6 M1 (data.table backend).
#
# Uses hand-built match tables (duplicate and candidate schemas) so the
# tests do not depend on the matching engine working correctly. Engine
# parity tests live alongside the engine in test_methods_datatable.R.

library(data.table)

# ---------------------------------------------------------------------------
# Fixture builders
# ---------------------------------------------------------------------------

make_dup_matches <- function(...) {
  # 2 clusters: cluster 1 has 3 records, cluster 2 has 2 records.
  dt <- data.table::data.table(
    duplicate_group = c(1L, 1L, 1L, 2L, 2L),
    id              = c("a", "b", "c", "d", "e"),
    score           = c(0.95, 0.90, 0.88, 0.80, 0.78),
    rank            = c(1L, 2L, 3L, 1L, 2L)
  )
  list(...) # consume forwarded args (none used yet)
  dt
}

make_cand_matches <- function() {
  # 4 base->target candidate pairs
  #   match_id 1: base=a, target=x  (score 0.95)
  #   match_id 2: base=a, target=y  (score 0.92)
  #   match_id 3: base=a, target=z  (score 0.30)   -> 3 candidates for `a`
  #   match_id 4: base=b, target=x  (score 0.85)   -> 1 candidate for `b`
  data.table::data.table(
    match_id = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
    score    = c(0.95, 0.95, 0.92, 0.92, 0.30, 0.30, 0.85, 0.85),
    source   = c("base", "target", "base", "target",
                 "base", "target", "base", "target"),
    id       = c("a", "x", "a", "y", "a", "z", "b", "x"),
    rank     = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
  )
}

make_base_table <- function() {
  data.table::data.table(id = letters[1:10], name = paste0("rec_", letters[1:10]))
}

make_target_table <- function() {
  data.table::data.table(id = c("x", "y", "z", "w"), name = c("X", "Y", "Z", "W"))
}


# ---------------------------------------------------------------------------
# Duplicates path
# ---------------------------------------------------------------------------

test_that("summarise_matches detects duplicates schema", {
  res <- summarise_matches(make_dup_matches())
  expect_s7_class <- function(o, cls) expect_true(S7::S7_inherits(o, cls))

  expect_s7_class(res, Match_Overview)
  expect_identical(res@match_type, "duplicates")
  expect_identical(res@n_records$n_pairs_or_groups, 2L)
  expect_identical(res@n_records$n_records_involved, 5L)

  # cluster size distribution: one cluster of size 3, one of size 2
  cd <- res@cluster_dist
  expect_equal(sort(cd$cluster_size), c(2L, 3L))
  expect_equal(sum(cd$n_clusters), 2L)

  expect_identical(res@cluster_summary$max_cluster_size, 3L)
  expect_true(is.na(res@cluster_summary$pct_records_in_cluster))

  # no base supplied -> coverage NA
  expect_true(is.na(res@coverage$base_coverage))
  expect_true(is.na(res@coverage$target_coverage))
})

test_that("summarise_matches duplicates: base coverage populated when base supplied", {
  res <- summarise_matches(make_dup_matches(), base = make_base_table())
  expect_equal(res@coverage$base_coverage, 5 / 10)
  expect_equal(res@cluster_summary$pct_records_in_cluster, 5 / 10)
})

test_that("summarise_matches duplicates: score summary fields populated", {
  res <- summarise_matches(make_dup_matches())
  s <- res@score_dist$summary
  expect_named(s, c("min", "q1", "median", "mean", "q3", "max"))
  expect_equal(s[["min"]], 0.78)
  expect_equal(s[["max"]], 0.95)
})


# ---------------------------------------------------------------------------
# Candidates path
# ---------------------------------------------------------------------------

test_that("summarise_matches detects candidates schema", {
  res <- summarise_matches(make_cand_matches())
  expect_identical(res@match_type, "candidates")
  expect_identical(res@n_records$n_pairs_or_groups, 4L)
  # ids involved: a, b (base) + x, y, z (target) = 5
  expect_identical(res@n_records$n_records_involved, 5L)
})

test_that("summarise_matches candidates: ambiguity_dist correct", {
  res <- summarise_matches(make_cand_matches())
  ad <- res@ambiguity_dist
  # base record `a` has 3 candidates, base record `b` has 1
  expect_equal(sort(ad$candidates_per_record), c(1L, 3L))
  expect_equal(ad[candidates_per_record == 3L]$n_records, 1L)
  expect_equal(ad[candidates_per_record == 1L]$n_records, 1L)
})

test_that("summarise_matches candidates: top_gap_dist captures the actual gap", {
  # Only base record `a` has >= 2 candidates: scores 0.95 and 0.92 -> gap 0.03.
  res <- summarise_matches(make_cand_matches())
  td <- res@top_gap_dist
  expect_true(!is.null(td))
  expect_named(td, c("bin_lower", "bin_upper", "count"))
  expect_equal(sum(td$count), 1L)

  # The constant-gap branch produces exactly one row.
  expect_equal(nrow(td), 1L)
  expect_equal(td$bin_lower[1L], 0.03)
  expect_equal(td$bin_upper[1L], 0.03)
})

test_that("summarise_matches candidates: top_gap_dist is NULL when no record has >=2 candidates", {
  # Every base record has exactly one candidate -> no gaps computable.
  cand <- data.table::data.table(
    match_id = rep(1:4, each = 2L),
    score    = rep(c(0.85, 0.80, 0.75, 0.70), each = 2L),
    source   = rep(c("base", "target"), times = 4L),
    id       = c("a", "x", "b", "y", "c", "z", "d", "w"),
    rank     = 1L
  )
  res <- summarise_matches(cand)
  expect_null(res@top_gap_dist)
  # candidates_weak_decisiveness depends on score_top_gap_median; it must
  # not fire when no gaps exist.
  expect_false(
    "candidates_weak_decisiveness" %in% attr(res, "recommendation_ids")
  )
})

test_that("summarise_matches candidates: coverage when base & target supplied", {
  res <- summarise_matches(
    make_cand_matches(),
    base   = make_base_table(),
    target = make_target_table()
  )
  # 2 base ids (a, b) of 10
  expect_equal(res@coverage$base_coverage, 2 / 10)
  # 3 target ids (x, y, z) of 4
  expect_equal(res@coverage$target_coverage, 3 / 4)
})


# ---------------------------------------------------------------------------
# Recommendations
# ---------------------------------------------------------------------------

test_that("candidates_high_ambiguity fires when >10% of base records have 3+ matches", {
  # Construct a candidate table where 2 of 4 base records have 3 candidates
  # (50% > 10%, triggers).
  build <- function(base_id, k) {
    do.call(rbind, lapply(seq_len(k), function(i) {
      mid <- as.integer(paste0(base_id, i))
      data.table::data.table(
        match_id = c(mid, mid),
        score    = c(0.9 - 0.01 * i, 0.9 - 0.01 * i),
        source   = c("base", "target"),
        id       = c(base_id, paste0("t_", base_id, i)),
        rank     = c(1L, 1L)
      )
    }))
  }
  # Encode base id as digit prefix for unique match_ids
  cand <- rbind(
    build("1", 3),  # base 1 has 3 candidates
    build("2", 3),  # base 2 has 3 candidates
    build("3", 1),  # base 3 has 1 candidate
    build("4", 1)   # base 4 has 1 candidate
  )
  res <- summarise_matches(cand)
  expect_true("candidates_high_ambiguity" %in% attr(res, "recommendation_ids"))
  # Match on a rule-specific phrase so the assertion doesn't pass on
  # unrelated candidate-flavoured recommendations.
  expect_match(
    paste(recommendations(res), collapse = "|"),
    "max_candidates"
  )
})

test_that("duplicates_mega_cluster fires at >= 50 records", {
  big_cluster <- data.table::data.table(
    duplicate_group = rep(1L, 60L),
    id              = paste0("r", seq_len(60L)),
    score           = runif(60, 0.85, 0.99),
    rank            = seq_len(60L)
  )
  res <- summarise_matches(big_cluster)
  expect_true("duplicates_mega_cluster" %in% attr(res, "recommendation_ids"))
})

test_that("low_coverage_candidates fires when base coverage < 5%", {
  cand <- make_cand_matches()
  big_base <- data.table::data.table(
    id = paste0("b", seq_len(100L))
  )
  # base ids in cand (`a`, `b`) are not in big_base -> coverage 0
  res <- summarise_matches(cand, base = big_base, target = make_target_table())
  expect_true("low_coverage_candidates" %in% attr(res, "recommendation_ids"))
})

test_that("clean fixtures produce no recommendations", {
  clean <- data.table::data.table(
    duplicate_group = c(1L, 1L, 2L, 2L),
    id              = c("a", "b", "c", "d"),
    score           = c(0.95, 0.93, 0.91, 0.90),
    rank            = c(1L, 2L, 1L, 2L)
  )
  res <- summarise_matches(clean)
  expect_identical(recommendations(res), character(0))
})


# ---------------------------------------------------------------------------
# Coercion
# ---------------------------------------------------------------------------

test_that("as.data.table flattens to a single-row summary with the full column set", {
  res <- summarise_matches(make_dup_matches(), base = make_base_table())
  flat <- as.data.table(res)
  expect_s3_class(flat, "data.table")
  expect_equal(nrow(flat), 1L)
  expect_setequal(names(flat), c(
    "match_type", "n_pairs_or_groups", "n_records_involved",
    "base_coverage", "target_coverage",
    "score_min", "score_q1", "score_median", "score_mean",
    "score_q3", "score_max",
    "max_cluster_size", "pct_records_in_cluster",
    "n_recommendations"
  ))
  expect_identical(flat$match_type, "duplicates")
  expect_equal(flat$n_pairs_or_groups, 2L)
  expect_equal(flat$max_cluster_size, 3L)
  expect_true(is.na(flat$target_coverage))
})

test_that("as.data.table for candidates has NA cluster columns and matches recommendations() length", {
  res <- summarise_matches(make_cand_matches())
  flat <- as.data.table(res)
  expect_true(is.na(flat$max_cluster_size))
  expect_true(is.na(flat$pct_records_in_cluster))
  expect_equal(flat$n_recommendations, length(recommendations(res)))
})

test_that("as.data.frame returns a single-row data.frame", {
  res <- summarise_matches(make_cand_matches())
  flat <- as.data.frame(res)
  expect_s3_class(flat, "data.frame")
  expect_equal(nrow(flat), 1L)
  expect_identical(flat$match_type, "candidates")
})


# ---------------------------------------------------------------------------
# Validation errors
# ---------------------------------------------------------------------------

test_that("summarise_matches errors on unrecognised schema", {
  bad <- data.table::data.table(foo = 1:3, bar = 4:6)
  expect_error(summarise_matches(bad), "match table")
})

test_that("summarise_matches errors on non-numeric score", {
  bad <- data.table::data.table(
    duplicate_group = 1L,
    id              = "a",
    score           = "high",
    rank            = 1L
  )
  expect_error(summarise_matches(bad), "numeric")
})


# ---------------------------------------------------------------------------
# Print / format stability
# ---------------------------------------------------------------------------

test_that("format(Match_Overview) snapshot is stable (duplicates)", {
  testthat::local_edition(3)
  res <- summarise_matches(make_dup_matches(), base = make_base_table())
  expect_snapshot(cat(format(res), sep = "\n"))
})

test_that("format(Match_Overview) snapshot is stable (candidates with recommendations)", {
  # make_cand_matches() fires candidates_weak_decisiveness (gap median 0.03 < 0.05);
  # snapshot locks the `recommendations:` block in addition to the body.
  testthat::local_edition(3)
  res <- summarise_matches(make_cand_matches())
  expect_snapshot(cat(format(res), sep = "\n"))
})

test_that("print(Match_Overview) returns invisible(x) without error", {
  res <- summarise_matches(make_cand_matches())
  expect_invisible(print(res))
})


# ---------------------------------------------------------------------------
# Score distribution sub-slots (quantiles, histogram, threshold, bins arg)
# ---------------------------------------------------------------------------

make_known_score_dup <- function() {
  # 11 evenly-spaced scores 0.50..1.00 across one cluster, hand-computable.
  scores <- seq(0.50, 1.00, by = 0.05)
  data.table::data.table(
    duplicate_group = 1L,
    id              = paste0("r", seq_along(scores)),
    score           = scores,
    rank            = seq_along(scores)
  )
}

test_that("score_dist$summary, quantiles, threshold are populated correctly", {
  res <- summarise_matches(make_known_score_dup())
  scores <- seq(0.50, 1.00, by = 0.05)

  s <- res@score_dist$summary
  expect_named(s, c("min", "q1", "median", "mean", "q3", "max"))
  expect_equal(s[["min"]],    0.50)
  expect_equal(s[["q1"]],     0.625)
  expect_equal(s[["median"]], 0.75)
  expect_equal(s[["mean"]],   0.75)
  expect_equal(s[["q3"]],     0.875)
  expect_equal(s[["max"]],    1.00)

  q <- res@score_dist$quantiles
  expect_named(q, c("p05", "p10", "p25", "p50", "p75", "p90", "p95"))
  expect_equal(
    unname(q),
    unname(stats::quantile(
      scores,
      probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
      names = FALSE
    ))
  )

  # threshold is intentionally not propagated yet (M2 follow-up). Pin it
  # so any future change is explicit.
  expect_identical(res@score_dist$threshold, NA_real_)
})

test_that("score_dist$histogram has correct schema, total count, and spans the range", {
  res <- summarise_matches(make_known_score_dup())
  hist <- res@score_dist$histogram

  expect_s3_class(hist, "data.table")
  expect_named(hist, c("bin_lower", "bin_upper", "count"))
  expect_type(hist$count, "integer")
  expect_equal(sum(hist$count), 11L)
  expect_equal(min(hist$bin_lower), 0.50)
  expect_equal(max(hist$bin_upper), 1.00)
  expect_equal(nrow(hist), 50L)  # default bins
})

test_that("bins argument controls histogram size", {
  res <- summarise_matches(make_known_score_dup(), bins = 10L)
  expect_equal(nrow(res@score_dist$histogram), 10L)
  expect_equal(sum(res@score_dist$histogram$count), 11L)
})


# ---------------------------------------------------------------------------
# .score_distribution edge branches: empty input, constant scores
# ---------------------------------------------------------------------------

test_that("summarise_matches handles an empty duplicates table", {
  empty <- data.table::data.table(
    duplicate_group = integer(),
    id              = character(),
    score           = numeric(),
    rank            = integer()
  )
  res <- summarise_matches(empty)
  expect_identical(res@match_type, "duplicates")
  expect_equal(res@n_records$n_pairs_or_groups, 0L)
  expect_equal(res@n_records$n_records_involved, 0L)

  s <- res@score_dist$summary
  expect_true(all(is.na(s)))
  hist <- res@score_dist$histogram
  expect_equal(nrow(hist), 0L)
  expect_named(hist, c("bin_lower", "bin_upper", "count"))
})

test_that("summarise_matches handles constant scores (single-bin histogram)", {
  const <- data.table::data.table(
    duplicate_group = c(1L, 1L, 1L, 1L, 1L),
    id              = paste0("r", 1:5),
    score           = rep(0.9, 5L),
    rank            = 1:5
  )
  res <- summarise_matches(const)
  hist <- res@score_dist$histogram
  expect_equal(nrow(hist), 1L)
  expect_equal(hist$bin_lower, 0.9)
  expect_equal(hist$bin_upper, 0.9)
  expect_equal(hist$count, 5L)
})


# ---------------------------------------------------------------------------
# Recommendations: end-to-end firing on the canonical fixture
# ---------------------------------------------------------------------------

test_that("candidates_weak_decisiveness fires on make_cand_matches (gap 0.03 < 0.05)", {
  res <- summarise_matches(make_cand_matches())
  expect_true(
    "candidates_weak_decisiveness" %in% attr(res, "recommendation_ids")
  )
})

test_that("candidates_weak_decisiveness does not fire when gaps are wide", {
  # Two base records, both with two candidates and gap = 0.20 (median 0.20).
  cand <- data.table::data.table(
    match_id = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
    score    = c(0.95, 0.95, 0.75, 0.75, 0.95, 0.95, 0.75, 0.75),
    source   = c("base", "target", "base", "target",
                 "base", "target", "base", "target"),
    id       = c("a", "x", "a", "y", "b", "x", "b", "y"),
    rank     = 1L
  )
  res <- summarise_matches(cand)
  expect_false(
    "candidates_weak_decisiveness" %in% attr(res, "recommendation_ids")
  )
})

test_that("multiple recommendations fire together through the verb", {
  # Construct a candidate table that simultaneously triggers:
  #   - candidates_high_ambiguity     (>10% base with >=3 matches)
  #   - candidates_weak_decisiveness  (median gap < 0.05)
  #   - low_coverage_candidates       (base coverage < 5%)
  build <- function(base_id, ts) {
    do.call(rbind, lapply(seq_along(ts), function(i) {
      mid <- as.integer(paste0(base_id, i))
      data.table::data.table(
        match_id = c(mid, mid),
        score    = c(ts[i], ts[i]),
        source   = c("base", "target"),
        id       = c(base_id, paste0("t_", base_id, i)),
        rank     = 1L
      )
    }))
  }
  cand <- rbind(
    build("1", c(0.90, 0.89, 0.88)),  # base 1: 3 candidates, gap 0.01
    build("2", c(0.85, 0.84, 0.83))   # base 2: 3 candidates, gap 0.01
  )
  big_base <- data.table::data.table(id = paste0("b", seq_len(100L)))
  res <- summarise_matches(cand, base = big_base,
                           target = make_target_table())
  expect_setequal(
    attr(res, "recommendation_ids"),
    c("candidates_high_ambiguity",
      "candidates_weak_decisiveness",
      "low_coverage_candidates")
  )
})

test_that("candidates_high_ambiguity boundary: exactly 10% does not fire (strict >)", {
  # 10 base records, exactly 1 has 3 candidates -> pct_records_with_ge3_matches = 0.10
  build <- function(base_id, k) {
    do.call(rbind, lapply(seq_len(k), function(i) {
      mid <- as.integer(sprintf("%d%d", as.integer(sub("b", "", base_id)), i))
      data.table::data.table(
        match_id = c(mid, mid),
        score    = c(0.9, 0.9),
        source   = c("base", "target"),
        id       = c(base_id, paste0("t_", base_id, "_", i)),
        rank     = 1L
      )
    }))
  }
  parts <- c(
    list(build("b1", 3L)),
    lapply(paste0("b", 2:10), build, k = 1L)
  )
  cand <- do.call(rbind, parts)
  res <- summarise_matches(cand)
  expect_false(
    "candidates_high_ambiguity" %in% attr(res, "recommendation_ids")
  )
})

test_that("low_coverage_candidates boundary: exactly 5% does not fire (strict <)", {
  cand <- make_cand_matches()
  # 2 distinct base ids in cand ({a, b}); 40 base rows -> coverage = 0.05.
  big_base <- data.table::data.table(
    id = c("a", "b", paste0("filler_", seq_len(38L)))
  )
  res <- summarise_matches(cand, base = big_base,
                           target = make_target_table())
  expect_equal(res@coverage$base_coverage, 0.05)
  expect_false(
    "low_coverage_candidates" %in% attr(res, "recommendation_ids")
  )
})


# ---------------------------------------------------------------------------
# Ordering invariants & accessor
# ---------------------------------------------------------------------------

test_that("cluster_dist is returned ordered by cluster_size", {
  # Build clusters of sizes 4, 2, 3 in scrambled group order.
  dt <- data.table::rbindlist(list(
    data.table::data.table(duplicate_group = 1L,
                           id = paste0("a", 1:4),
                           score = 0.9, rank = 1:4),
    data.table::data.table(duplicate_group = 2L,
                           id = paste0("b", 1:2),
                           score = 0.9, rank = 1:2),
    data.table::data.table(duplicate_group = 3L,
                           id = paste0("c", 1:3),
                           score = 0.9, rank = 1:3)
  ))
  res <- summarise_matches(dt)
  expect_equal(res@cluster_dist$cluster_size,
               sort(res@cluster_dist$cluster_size))
})

test_that("ambiguity_dist is returned ordered by candidates_per_record", {
  # Mix base records with 1 and 3 candidates in scrambled id order.
  build <- function(base_id, k, start_mid) {
    do.call(rbind, lapply(seq_len(k), function(i) {
      mid <- start_mid + i
      data.table::data.table(
        match_id = c(mid, mid),
        score    = c(0.9, 0.9),
        source   = c("base", "target"),
        id       = c(base_id, paste0("t_", base_id, i)),
        rank     = 1L
      )
    }))
  }
  cand <- rbind(
    build("z", 3L, 100L),
    build("a", 1L, 200L),
    build("m", 3L, 300L),
    build("b", 1L, 400L)
  )
  res <- summarise_matches(cand)
  expect_equal(res@ambiguity_dist$candidates_per_record,
               sort(res@ambiguity_dist$candidates_per_record))
})

test_that("recommendations() accessor returns the slot value", {
  ov <- Match_Overview(
    match_type      = "duplicates",
    n_records       = list(n_pairs_or_groups = 1L, n_records_involved = 2L),
    coverage        = list(base_coverage = NA_real_, target_coverage = NA_real_),
    score_dist      = list(
      summary   = c(min = NA_real_, q1 = NA_real_, median = NA_real_,
                    mean = NA_real_, q3 = NA_real_, max = NA_real_),
      quantiles = stats::setNames(rep(NA_real_, 7L),
                                  c("p05","p10","p25","p50","p75","p90","p95")),
      histogram = data.table::data.table(
        bin_lower = numeric(), bin_upper = numeric(), count = integer()
      ),
      threshold = NA_real_
    ),
    cluster_dist    = NULL,
    cluster_summary = NULL,
    ambiguity_dist  = NULL,
    top_gap_dist    = NULL,
    recommendations = c("alpha", "beta")
  )
  expect_identical(recommendations(ov), c("alpha", "beta"))
})


# ---------------------------------------------------------------------------
# Validation: candidate-shaped table missing `score`
# ---------------------------------------------------------------------------

test_that("summarise_matches errors on a candidate-shaped table missing score", {
  bad <- data.table::data.table(
    match_id = 1L,
    source   = "base",
    id       = "a"
  )
  expect_error(summarise_matches(bad), "match table")
})
