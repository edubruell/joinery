# Tests for audit_strategy() (Phase 0.6 M3).
#
# Fixture convention (M2+): one clean fixture (zero recommendations) + one
# trigger fixture per recommendation rule (fires only that rule).

library(data.table)

# ---------------------------------------------------------------------------
# Fixture builders
# ---------------------------------------------------------------------------

make_clean_strategy <- function(block_by = NULL) {
  search_strategy(
    first_name ~ normalize_text() + word_tokens(),
    last_name  ~ normalize_text() + word_tokens(),
    block_by   = block_by,
    threshold  = 0.9
  )
}

# 20 records, all unique single-word names → rarity=1.0 for every token,
# pct_low_rarity=0 for both columns. Fires zero recommendations.
make_clean_data <- function() {
  data.table::data.table(
    id         = paste0("r", 1:20),
    first_name = c(
      "alice", "bob", "carol", "david", "eve",
      "frank", "grace", "harry", "iris", "jack",
      "kate", "leo", "mia", "ned", "olivia",
      "peter", "quinn", "rose", "sam", "tara"
    ),
    last_name  = c(
      "anderson", "brown", "clark", "davis", "evans",
      "foster", "green", "harris", "irwin", "jones",
      "king", "lewis", "moore", "norton", "owen",
      "parker", "quinn", "reed", "smith", "taylor"
    )
  )
}

# Same data with a balanced block column (10 records per region).
make_blocked_data <- function() {
  dt <- make_clean_data()
  dt[, region := rep(c("north", "south"), each = 10L)]
  dt
}

# Stopword fixture: 11 stopwords × 200 records + 10 unique-name records.
# Total unique tokens = 21. Each stopword has rarity = 1/200 = 0.005 < 0.01.
# pct_low_rarity = 11/21 ≈ 0.524 > 0.50 → fires high_low_rarity_pressure.
make_stopword_data <- function() {
  sw <- c("gmbh", "ag", "co", "ltd", "inc", "corp", "llc", "plc", "srl", "bv", "nv")
  base_r <- do.call(rbind, lapply(sw, function(w) {
    data.table::data.table(
      id      = paste0(w, "_", seq_len(200L)),
      company = rep(w, 200L)
    )
  }))
  unique_r <- data.table::data.table(
    id      = paste0("u", seq_len(10L)),
    company = paste0("company_", letters[seq_len(10L)])
  )
  rbind(base_r, unique_r)
}

make_stopword_strategy <- function() {
  search_strategy(
    company ~ normalize_text() + word_tokens(),
    threshold = 0.9
  )
}

# Imbalanced blocking fixture: 16 records in "majority", 4 in "minority".
# top1_share = 16/20 = 0.80 > 0.70 → fires block_imbalanced.
make_imbalanced_block_data <- function() {
  dt <- make_clean_data()
  dt[, region := c(rep("majority", 16L), rep("minority", 4L))]
  dt
}

# Target table for vocab overlap tests: 10 records, partially overlapping.
# first_name overlaps on: alice, bob, carol, david, eve (5/20 base tokens).
# last_name overlaps on: anderson, brown, foster, green, harris, irwin, jones (7/20).
make_target_data <- function() {
  data.table::data.table(
    id         = paste0("t", seq_len(10L)),
    first_name = c(
      "alice", "bob", "carol", "david", "eve",
      "zara", "xavier", "winston", "vivian", "ulric"
    ),
    last_name  = c(
      "anderson", "brown", "zeller", "yates", "xu",
      "foster", "green", "harris", "irwin", "jones"
    )
  )
}


# ---------------------------------------------------------------------------
# 1. Return type and n_records
# ---------------------------------------------------------------------------

test_that("audit_strategy returns Strategy_Audit", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  expect_true(S7::S7_inherits(res, Strategy_Audit))
})

test_that("n_records matches input size", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  expect_identical(res@n_records, 20L)
})


# ---------------------------------------------------------------------------
# 2. column_token_stats schema and arithmetic
# ---------------------------------------------------------------------------

test_that("column_token_stats has correct column names", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  expect_named(
    res@column_token_stats,
    c("column", "n_tokens", "n_unique_tokens", "pct_unique",
      "na_rate", "avg_tokens_per_record")
  )
})

test_that("column_token_stats has one row per strategy column", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  expect_equal(nrow(res@column_token_stats), 2L)
  expect_setequal(res@column_token_stats$column, c("first_name", "last_name"))
})

test_that("column_token_stats: na_rate is 0 when no NAs present", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  expect_equal(res@column_token_stats$na_rate, c(0, 0), tolerance = 1e-10)
})

test_that("column_token_stats: na_rate is correct when NAs present", {
  s   <- make_clean_strategy()
  dat <- make_clean_data()
  dat[1:4, first_name := NA_character_]  # 4/20 = 20%
  res <- audit_strategy(dat, "id", s)
  fn  <- res@column_token_stats[column == "first_name"]
  expect_equal(fn$na_rate, 0.20, tolerance = 1e-10)
})

test_that("column_token_stats: pct_unique = n_unique_tokens / n_tokens", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  cts <- res@column_token_stats
  expect_equal(
    cts$pct_unique,
    cts$n_unique_tokens / cts$n_tokens,
    tolerance = 1e-10
  )
})

test_that("column_token_stats: avg_tokens_per_record = n_tokens / n_records", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  cts <- res@column_token_stats
  expect_equal(
    cts$avg_tokens_per_record,
    cts$n_tokens / 20,
    tolerance = 1e-10
  )
})


# ---------------------------------------------------------------------------
# 3. column_rarity_stats schema and ordering
# ---------------------------------------------------------------------------

test_that("column_rarity_stats has correct column names", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  expect_named(
    res@column_rarity_stats,
    c("column", "rarity_p05", "rarity_p25", "rarity_p50",
      "rarity_p75", "rarity_p95", "pct_low_rarity")
  )
})

test_that("column_rarity_stats: quantiles are in non-decreasing order", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  crs <- res@column_rarity_stats
  for (i in seq_len(nrow(crs))) {
    q <- unlist(crs[i, .(rarity_p05, rarity_p25, rarity_p50, rarity_p75, rarity_p95)])
    q <- q[!is.na(q)]
    expect_true(
      all(diff(q) >= 0),
      info = paste("quantiles not non-decreasing for column", crs$column[i])
    )
  }
})

test_that("column_rarity_stats: pct_low_rarity is in [0, 1]", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  crs <- res@column_rarity_stats
  plr <- crs$pct_low_rarity
  expect_true(all(plr >= 0 & plr <= 1 | is.na(plr)))
})


# ---------------------------------------------------------------------------
# 4. est_comparisons
# ---------------------------------------------------------------------------

test_that("est_comparisons = n*(n-1)/2 without blocking", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  expect_equal(res@est_comparisons, 20 * 19 / 2, tolerance = 1e-10)
})

test_that("est_comparisons uses block sizes with blocking", {
  s   <- make_clean_strategy(block_by = "region")
  res <- audit_strategy(make_blocked_data(), "id", s)
  # Two balanced blocks of 10: each contributes 10*9/2=45, total 90
  expect_equal(res@est_comparisons, 90, tolerance = 1e-10)
  expect_lt(res@est_comparisons, 20 * 19 / 2)
})


# ---------------------------------------------------------------------------
# 5. block_summary
# ---------------------------------------------------------------------------

test_that("block_summary is NULL when strategy has no block_by", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  expect_null(res@block_summary)
})

test_that("block_summary is populated when block_by is set", {
  s   <- make_clean_strategy(block_by = "region")
  res <- audit_strategy(make_blocked_data(), "id", s)
  expect_false(is.null(res@block_summary))
})

test_that("block_summary$distribution has correct schema", {
  s    <- make_clean_strategy(block_by = "region")
  res  <- audit_strategy(make_blocked_data(), "id", s)
  dist <- res@block_summary$distribution
  expect_named(dist, c("block_key", "n_records", "pct_records"))
})

test_that("block_summary$summary has correct n_blocks and top1_share for balanced blocks", {
  s   <- make_clean_strategy(block_by = "region")
  res <- audit_strategy(make_blocked_data(), "id", s)
  sm  <- res@block_summary$summary
  expect_equal(sm$n_blocks, 2L)
  expect_equal(sm$top1_share, 0.50, tolerance = 1e-10)
  expect_equal(sm$min_size, 10L)
  expect_equal(sm$max_size, 10L)
})

test_that("block_summary$distribution pct_records sums to 1", {
  s   <- make_clean_strategy(block_by = "region")
  res <- audit_strategy(make_blocked_data(), "id", s)
  expect_equal(sum(res@block_summary$distribution$pct_records), 1.0, tolerance = 1e-10)
})


# ---------------------------------------------------------------------------
# 6. Vocab overlap
# ---------------------------------------------------------------------------

test_that("vocab_overlap attribute is NULL when target not supplied", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  expect_null(attr(res, "vocab_overlap"))
})

test_that("vocab_overlap values are in [0, 1] when target supplied", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s, target = make_target_data())
  vo  <- attr(res, "vocab_overlap")
  expect_false(is.null(vo))
  expect_true(all(vo >= 0 & vo <= 1 | is.na(vo)))
})

test_that("vocab_overlap names match strategy columns", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s, target = make_target_data())
  vo  <- attr(res, "vocab_overlap")
  expect_setequal(names(vo), c("first_name", "last_name"))
})

test_that("vocab_overlap = 1 when base and target are the same table", {
  s   <- make_clean_strategy()
  dat <- make_clean_data()
  res <- audit_strategy(dat, "id", s, target = dat)
  vo  <- attr(res, "vocab_overlap")
  expect_equal(unname(vo), c(1.0, 1.0), tolerance = 1e-10)
})

test_that("vocab_overlap is strictly between 0 and 1 with partial target overlap", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s, target = make_target_data())
  vo  <- attr(res, "vocab_overlap")
  expect_true(all(vo > 0 & vo < 1))
})


# ---------------------------------------------------------------------------
# 7. Recommendations
# ---------------------------------------------------------------------------

test_that("high_low_rarity_pressure fires on stopword fixture", {
  s   <- make_stopword_strategy()
  res <- audit_strategy(make_stopword_data(), "id", s)
  expect_true("high_low_rarity_pressure" %in% attr(res, "recommendation_ids"))
})

test_that("high_low_rarity_pressure does NOT fire on clean fixture", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  expect_false("high_low_rarity_pressure" %in% attr(res, "recommendation_ids"))
})

test_that("block_imbalanced fires on imbalanced blocking fixture", {
  s   <- make_clean_strategy(block_by = "region")
  res <- audit_strategy(make_imbalanced_block_data(), "id", s)
  expect_true("block_imbalanced" %in% attr(res, "recommendation_ids"))
})

test_that("block_imbalanced does NOT fire on balanced blocking fixture", {
  s   <- make_clean_strategy(block_by = "region")
  res <- audit_strategy(make_blocked_data(), "id", s)
  expect_false("block_imbalanced" %in% attr(res, "recommendation_ids"))
})

test_that("recommendations() accessor returns character(0) on clean fixture", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  expect_identical(recommendations(res), character(0))
})

test_that("block_imbalanced recommendation message contains the block share percentage", {
  s   <- make_clean_strategy(block_by = "region")
  res <- audit_strategy(make_imbalanced_block_data(), "id", s)
  msg <- recommendations(res)
  expect_match(paste(msg, collapse = "|"), "80")
})

test_that("high_low_rarity_pressure recommendation message contains the column name", {
  s   <- make_stopword_strategy()
  res <- audit_strategy(make_stopword_data(), "id", s)
  msg <- recommendations(res)
  expect_match(paste(msg, collapse = "|"), "company")
})


# ---------------------------------------------------------------------------
# 8. sample_n
# ---------------------------------------------------------------------------

test_that("sample_n returns Strategy_Audit with n_records <= sample_n", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s, sample_n = 10L)
  expect_true(S7::S7_inherits(res, Strategy_Audit))
  expect_lte(res@n_records, 10L)
})

test_that("sample_n >= nrow(data) returns all records", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s, sample_n = 100L)
  expect_equal(res@n_records, 20L)
})


# ---------------------------------------------------------------------------
# 9. format() and print()
# ---------------------------------------------------------------------------

test_that("format() returns a character vector with multiple lines", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  out <- format(res)
  expect_type(out, "character")
  expect_gt(length(out), 1L)
})

test_that("format() snapshot is stable (clean fixture)", {
  testthat::local_edition(3)
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  expect_snapshot(cat(format(res), sep = "\n"))
})

test_that("print() returns invisible(x)", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  expect_invisible(print(res))
})


# ---------------------------------------------------------------------------
# 10. as.data.table() / as.data.frame()
# ---------------------------------------------------------------------------

test_that("as.data.table() returns a single-row data.table with key columns", {
  s    <- make_clean_strategy()
  res  <- audit_strategy(make_clean_data(), "id", s)
  flat <- as.data.table(res)
  expect_s3_class(flat, "data.table")
  expect_equal(nrow(flat), 1L)
  expect_true("n_records" %in% names(flat))
  expect_true("est_comparisons" %in% names(flat))
  expect_true("n_recommendations" %in% names(flat))
})

test_that("as.data.table(): n_records column matches slot value", {
  s   <- make_clean_strategy()
  res <- audit_strategy(make_clean_data(), "id", s)
  expect_equal(as.data.table(res)$n_records, 20L)
})

test_that("as.data.frame() returns a single-row data.frame", {
  s    <- make_clean_strategy()
  res  <- audit_strategy(make_clean_data(), "id", s)
  flat <- as.data.frame(res)
  expect_s3_class(flat, "data.frame")
  expect_equal(nrow(flat), 1L)
})

test_that("as.data.table(): vocab_overlap columns added when target supplied", {
  s    <- make_clean_strategy()
  res  <- audit_strategy(make_clean_data(), "id", s, target = make_target_data())
  flat <- as.data.table(res)
  expect_true("vocab_overlap_first_name" %in% names(flat))
  expect_true("vocab_overlap_last_name" %in% names(flat))
})


# ---------------------------------------------------------------------------
# 11. Backend parity — tibble and data.frame
# ---------------------------------------------------------------------------

test_that("tibble input gives identical result to data.table", {
  skip_if_not_installed("tibble")
  s       <- make_clean_strategy()
  dat     <- make_clean_data()
  tbl     <- tibble::as_tibble(dat)
  res_dt  <- audit_strategy(dat, "id", s)
  res_tbl <- audit_strategy(tbl, "id", s)
  expect_identical(res_dt@n_records, res_tbl@n_records)
  expect_equal(res_dt@column_token_stats,  res_tbl@column_token_stats)
  expect_equal(res_dt@column_rarity_stats, res_tbl@column_rarity_stats)
  expect_equal(res_dt@est_comparisons,     res_tbl@est_comparisons)
})

test_that("data.frame input gives identical result to data.table", {
  s      <- make_clean_strategy()
  dat    <- make_clean_data()
  df     <- as.data.frame(dat)
  res_dt <- audit_strategy(dat, "id", s)
  res_df <- audit_strategy(df,  "id", s)
  expect_identical(res_dt@n_records, res_df@n_records)
  expect_equal(res_dt@column_token_stats, res_df@column_token_stats)
  expect_equal(res_dt@est_comparisons,    res_df@est_comparisons)
})


# ---------------------------------------------------------------------------
# 12. Backend parity — DuckDB
# ---------------------------------------------------------------------------

test_that("DuckDB backend: returns Strategy_Audit", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  s    <- make_clean_strategy()
  duck <- local_duckdb_table(make_clean_data(), "audit_clean")
  res  <- audit_strategy(duck, "id", s)
  expect_true(S7::S7_inherits(res, Strategy_Audit))
})

test_that("DuckDB backend: n_records matches data.table (full pull, no sample)", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  s      <- make_clean_strategy()
  dat    <- make_clean_data()
  duck   <- local_duckdb_table(dat, "audit_duck_n")
  res_dt <- audit_strategy(dat,  "id", s)
  res_dk <- audit_strategy(duck, "id", s)
  expect_identical(res_dk@n_records, res_dt@n_records)
})

test_that("DuckDB backend: column_token_stats matches data.table", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  s      <- make_clean_strategy()
  dat    <- make_clean_data()
  duck   <- local_duckdb_table(dat, "audit_duck_cts")
  res_dt <- audit_strategy(dat,  "id", s)
  res_dk <- audit_strategy(duck, "id", s)
  expect_equal(res_dt@column_token_stats, res_dk@column_token_stats)
})

test_that("DuckDB backend: recommendations match data.table", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  s      <- make_stopword_strategy()
  dat    <- make_stopword_data()
  duck   <- local_duckdb_table(dat, "audit_duck_rec")
  res_dt <- audit_strategy(dat,  "id", s)
  res_dk <- audit_strategy(duck, "id", s)
  expect_identical(recommendations(res_dk), recommendations(res_dt))
})

test_that("DuckDB backend: sample_n returns Strategy_Audit with n_records <= sample_n", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  s    <- make_clean_strategy()
  duck <- local_duckdb_table(make_clean_data(), "audit_duck_samp")
  res  <- audit_strategy(duck, "id", s, sample_n = 5L)
  expect_true(S7::S7_inherits(res, Strategy_Audit))
  expect_lte(res@n_records, 5L)
})
