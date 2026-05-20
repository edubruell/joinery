# Tests for explain_match().
#
# Fixture convention: small deterministic tables with known exact scores.
# Round-trip property: sum(per_column_contrib$contribution) == score (no feedback);
# sum × feedback_factor ≈ score (with feedback).

library(data.table)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# 4 records, 2 columns.
# r1 and r2 share "alice" in first_name (dedup match).
# r3 and r4 share nothing with r1/r2.
make_explain_data <- function() {
  data.table::data.table(
    id         = c("r1", "r2", "r3", "r4"),
    first_name = c("alice", "alice", "bob",   "carol"),
    last_name  = c("smith", "smyth", "jones", "baker")
  )
}

make_explain_strategy <- function() {
  search_strategy(
    first_name ~ normalize_text() + word_tokens(),
    last_name  ~ normalize_text() + word_tokens(),
    threshold  = 0.3
  )
}

make_dup_matches <- function() {
  data  <- make_explain_data()
  strat <- make_explain_strategy()
  detect_duplicates(data, "id", strat)
}

# Candidate fixtures: base has 3 records, target has 3 records.
# b1 <-> t1 match on first_name + last_name; b2 <-> t2 match on first_name only.
# Use "id" for both base and target (matches codebase convention; avoids
# data.table column/variable name collision in search_candidates).
make_base_data <- function() {
  data.table::data.table(
    id         = c("b1", "b2", "b3"),
    first_name = c("alice", "bob",   "charlie"),
    last_name  = c("smith", "jones", "brown")
  )
}

make_target_data <- function() {
  data.table::data.table(
    id         = c("t1", "t2", "t3"),
    first_name = c("alice", "bob",   "diana"),
    last_name  = c("smith", "young", "purple")
  )
}

make_cand_matches <- function() {
  base   <- make_base_data()
  target <- make_target_data()
  strat  <- make_explain_strategy()
  search_candidates(base, target, "id", "id", strat)
}

# Fixture with feedback strength > 0
make_feedback_strategy <- function() {
  search_strategy(
    first_name ~ normalize_text() + word_tokens(),
    last_name  ~ normalize_text() + word_tokens(),
    threshold         = 0.1,
    feedback_strength = 0.5
  )
}

make_feedback_matches <- function() {
  detect_duplicates(make_explain_data(), "id", make_feedback_strategy())
}


# ---------------------------------------------------------------------------
# 1. Return type
# ---------------------------------------------------------------------------

test_that("explain_match returns a Match_Explanation object", {
  dups <- make_dup_matches()
  s    <- make_explain_strategy()
  dat  <- make_explain_data()

  ex <- explain_match(dups, s, base = dat, id = "id", match_id = 1L)
  expect_s3_class(ex, "joinery::Match_Explanation")
})


# ---------------------------------------------------------------------------
# 2. pair slot schema
# ---------------------------------------------------------------------------

test_that("pair slot contains 2 rows and original columns", {
  dups <- make_dup_matches()
  s    <- make_explain_strategy()
  dat  <- make_explain_data()

  ex <- explain_match(dups, s, base = dat, id = "id", match_id = 1L)

  expect_true(data.table::is.data.table(ex@pair))
  expect_equal(nrow(ex@pair), 2L)
  expect_true("id"         %in% names(ex@pair))
  expect_true("first_name" %in% names(ex@pair))
  expect_true("last_name"  %in% names(ex@pair))
})


# ---------------------------------------------------------------------------
# 3. per_column_contrib schema
# ---------------------------------------------------------------------------

test_that("per_column_contrib has correct schema", {
  dups <- make_dup_matches()
  s    <- make_explain_strategy()
  dat  <- make_explain_data()

  ex   <- explain_match(dups, s, base = dat, id = "id", match_id = 1L)
  pcc  <- ex@per_column_contrib

  expect_true(data.table::is.data.table(pcc))
  expect_true(all(c("src_column", "contribution", "n_shared_tokens") %in% names(pcc)))
  expect_true(is.numeric(pcc$contribution))
  expect_true(is.integer(pcc$n_shared_tokens))
  expect_true(nrow(pcc) >= 1L)
})


# ---------------------------------------------------------------------------
# 4. shared_tokens schema
# ---------------------------------------------------------------------------

test_that("shared_tokens has correct schema", {
  dups <- make_dup_matches()
  s    <- make_explain_strategy()
  dat  <- make_explain_data()

  ex <- explain_match(dups, s, base = dat, id = "id", match_id = 1L)
  st <- ex@shared_tokens

  expect_true(data.table::is.data.table(st))
  expect_true(all(c("src_column", "token", "rarity", "rIP", "weight", "contribution") %in% names(st)))
  expect_true(is.numeric(st$rarity))
  expect_true(is.numeric(st$rIP))
  expect_true(is.numeric(st$weight))
  expect_true(is.numeric(st$contribution))
})


# ---------------------------------------------------------------------------
# 5. Round-trip property (no feedback)
# ---------------------------------------------------------------------------

test_that("sum(per_column_contrib$contribution) == score (no feedback)", {
  dups <- make_dup_matches()
  s    <- make_explain_strategy()
  dat  <- make_explain_data()

  ex <- explain_match(dups, s, base = dat, id = "id", match_id = 1L)

  expect_equal(
    sum(ex@per_column_contrib$contribution),
    ex@score,
    tolerance = 1e-10
  )
})

test_that("shared_tokens contributions sum to per_column_contrib (no feedback)", {
  dups <- make_dup_matches()
  s    <- make_explain_strategy()
  dat  <- make_explain_data()

  ex  <- explain_match(dups, s, base = dat, id = "id", match_id = 1L)
  st  <- ex@shared_tokens
  pcc <- ex@per_column_contrib

  tok_by_col <- st[, .(tok_sum = sum(contribution)), by = "src_column"]
  merged <- merge(pcc, tok_by_col, by = "src_column")
  expect_equal(merged$contribution, merged$tok_sum, tolerance = 1e-10)
})


# ---------------------------------------------------------------------------
# 6. Round-trip with feedback
# ---------------------------------------------------------------------------

test_that("sum(contrib) x feedback_factor ≈ score when feedback_strength > 0", {
  s   <- make_feedback_strategy()
  dups <- make_feedback_matches()
  dat  <- make_explain_data()

  if (nrow(dups) == 0L) skip("No matches in feedback fixture")

  grp1 <- dups[dups$duplicate_group == dups$duplicate_group[1L], ]
  if (nrow(grp1) < 2L) skip("No pair in feedback group")

  ex <- explain_match(dups, s, base = dat, id = "id", match_id = dups$duplicate_group[1L])

  raw_score      <- sum(ex@per_column_contrib$contribution)
  feedback_factor <- ex@score_breakdown$feedback_factor
  reconstructed  <- raw_score * feedback_factor

  expect_equal(reconstructed, ex@score, tolerance = 1e-10)
})


# ---------------------------------------------------------------------------
# 7. Both calling forms produce identical Match_Explanation
# ---------------------------------------------------------------------------

test_that("ergonomic and power-user forms produce identical results", {
  dat  <- make_explain_data()
  s    <- make_explain_strategy()
  dups <- make_dup_matches()

  # Pre-compute tokens+rarity for power-user form
  tokens_full   <- prepare_search_data(dat, "id", s)
  tokens_rarity <- compute_rarity(tokens_full, s)

  mid <- dups$duplicate_group[1L]

  ex_erg   <- explain_match(dups, s,             base = dat, id = "id", match_id = mid)
  ex_power <- explain_match(dups, tokens_rarity, id = "id", strategy = s, match_id = mid)

  expect_equal(ex_erg@score, ex_power@score, tolerance = 1e-10)

  # Sort both to the same order before comparing
  pcc_erg   <- ex_erg@per_column_contrib[order(src_column)]
  pcc_power <- ex_power@per_column_contrib[order(src_column)]
  expect_equal(pcc_erg$contribution,    pcc_power$contribution,    tolerance = 1e-10)
  expect_equal(pcc_erg$n_shared_tokens, pcc_power$n_shared_tokens)

  st_erg   <- ex_erg@shared_tokens[order(src_column, token)]
  st_power <- ex_power@shared_tokens[order(src_column, token)]
  expect_equal(st_erg$contribution, st_power$contribution, tolerance = 1e-10)
})


# ---------------------------------------------------------------------------
# 8. Candidate (cross-table) form
# ---------------------------------------------------------------------------

test_that("explain_match works for candidate matches", {
  base   <- make_base_data()
  target <- make_target_data()
  s      <- make_explain_strategy()
  cands  <- make_cand_matches()

  if (nrow(cands) == 0L) skip("No candidate matches in fixture")

  first_mid <- cands$match_id[1L]
  ex <- explain_match(
    cands, s,
    base      = base,
    id        = "id",
    target    = target,
    target_id = "id",
    match_id  = first_mid
  )

  expect_s3_class(ex, "joinery::Match_Explanation")
  expect_equal(nrow(ex@pair), 2L)
  expect_true("source" %in% names(ex@pair))
  expect_gt(ex@score, 0)
  expect_equal(sum(ex@per_column_contrib$contribution), ex@score, tolerance = 1e-10)
})


# ---------------------------------------------------------------------------
# 9. score_breakdown structure
# ---------------------------------------------------------------------------

test_that("score_breakdown contains expected fields", {
  dups <- make_dup_matches()
  s    <- make_explain_strategy()
  dat  <- make_explain_data()

  ex <- explain_match(dups, s, base = dat, id = "id", match_id = 1L)
  sb <- ex@score_breakdown

  expect_true(is.list(sb))
  expect_true("smoothing_method"  %in% names(sb))
  expect_true("feedback_strength" %in% names(sb))
  expect_true("feedback_factor"   %in% names(sb))
  expect_equal(sb$feedback_factor, 1.0)  # no feedback in this fixture
})


# ---------------------------------------------------------------------------
# 10. print and format
# ---------------------------------------------------------------------------

test_that("print() and format() work without error", {
  dups <- make_dup_matches()
  s    <- make_explain_strategy()
  dat  <- make_explain_data()

  ex <- explain_match(dups, s, base = dat, id = "id", match_id = 1L)

  expect_no_error(print(ex))
  lines <- format(ex)
  expect_true(is.character(lines))
  expect_true(length(lines) > 0L)
  expect_true(any(nchar(lines) > 0L))
})


# ---------------------------------------------------------------------------
# 11. Invalid match_id gives a clear error
# ---------------------------------------------------------------------------

test_that("invalid match_id gives clear error", {
  dups <- make_dup_matches()
  s    <- make_explain_strategy()
  dat  <- make_explain_data()

  expect_error(
    explain_match(dups, s, base = dat, id = "id", match_id = 9999L),
    regexp = "not found"
  )
})


# ---------------------------------------------------------------------------
# 12. Missing base or id gives clear error
# ---------------------------------------------------------------------------

test_that("missing base gives clear error", {
  dups <- make_dup_matches()
  s    <- make_explain_strategy()

  expect_error(
    explain_match(dups, s, id = "id", match_id = 1L),
    regexp = "base"
  )
})

test_that("missing id gives clear error", {
  dups <- make_dup_matches()
  s    <- make_explain_strategy()
  dat  <- make_explain_data()

  expect_error(
    explain_match(dups, s, base = dat, match_id = 1L),
    regexp = "id"
  )
})


# ---------------------------------------------------------------------------
# 13. DuckDB parity
# ---------------------------------------------------------------------------

test_that("DuckDB form produces same score as data.table form", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  dat  <- make_explain_data()
  s    <- make_explain_strategy()
  dups <- make_dup_matches()
  mid  <- dups$duplicate_group[1L]

  # DT result (reference)
  ex_dt <- explain_match(dups, s, base = dat, id = "id", match_id = mid)

  # DuckDB result
  con       <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "matches", as.data.frame(dups))
  DBI::dbWriteTable(con, "dat",     as.data.frame(dat))

  duck_matches <- dplyr::tbl(con, "matches")
  duck_dat     <- dplyr::tbl(con, "dat")

  ex_duck <- explain_match(duck_matches, s, base = duck_dat, id = "id", match_id = mid)

  expect_equal(ex_duck@score, ex_dt@score, tolerance = 1e-6)

  pcc_dt   <- ex_dt@per_column_contrib[order(src_column)]
  pcc_duck <- ex_duck@per_column_contrib[order(src_column)]
  expect_equal(pcc_dt$contribution, pcc_duck$contribution, tolerance = 1e-6)
})


# ---------------------------------------------------------------------------
# 14. Power-user form: wrong id column name gives clear error
# ---------------------------------------------------------------------------

test_that("power-user form with wrong id column errors clearly", {
  dat   <- make_explain_data()
  s     <- make_explain_strategy()
  dups  <- make_dup_matches()

  tokens <- compute_rarity(prepare_search_data(dat, "id", s), s)

  expect_error(
    explain_match(dups, tokens, id = "not_a_column", strategy = s, match_id = 1L),
    regexp = "not_a_column"
  )
})


# ---------------------------------------------------------------------------
# 15. Single-column strategy works without error
# ---------------------------------------------------------------------------

test_that("single-column strategy works", {
  dat <- data.table::data.table(
    id   = c("a", "b", "c"),
    name = c("alice smith", "alice jones", "bob baker")
  )
  s <- search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold = 0.3
  )
  dups <- detect_duplicates(dat, "id", s)
  if (nrow(dups) == 0L) skip("No matches in single-column fixture")

  ex <- explain_match(dups, s, base = dat, id = "id", match_id = dups$duplicate_group[1L])
  expect_s3_class(ex, "joinery::Match_Explanation")
  expect_equal(sum(ex@per_column_contrib$contribution), ex@score, tolerance = 1e-10)
})


# ---------------------------------------------------------------------------
# 16. Weight validation: mismatched weights error clearly
# ---------------------------------------------------------------------------

test_that(".pair_attribution_dt errors on missing weights", {
  dat  <- make_explain_data()
  s    <- make_explain_strategy()
  dups <- make_dup_matches()
  mid  <- dups$duplicate_group[1L]

  # Pre-compute tokens+rarity, then pass wrong weights via a patched strategy
  tokens_full <- compute_rarity(prepare_search_data(dat, "id", s), s)

  # Build a strategy whose column set doesn't match the tokens
  s_bad <- search_strategy(
    first_name ~ normalize_text() + word_tokens(),
    threshold = 0.3
  )
  # The tokens table has both first_name and last_name; s_bad only covers first_name
  # so weights["last_name"] is NA, which should error
  expect_error(
    explain_match(dups, tokens_full, id = "id", strategy = s_bad, match_id = mid),
    regexp = "Weights missing"
  )
})


# ---------------------------------------------------------------------------
# 17. Blocking: round-trip holds with block_by
# ---------------------------------------------------------------------------

test_that("round-trip holds when strategy uses block_by", {
  dat <- data.table::data.table(
    id         = c("r1", "r2", "r3", "r4"),
    first_name = c("alice", "alice", "bob",   "carol"),
    last_name  = c("smith", "smyth", "jones", "baker"),
    region     = c("north", "north", "south", "south")
  )
  s <- search_strategy(
    first_name ~ normalize_text() + word_tokens(),
    last_name  ~ normalize_text() + word_tokens(),
    block_by   = "region",
    threshold  = 0.3
  )
  dups <- detect_duplicates(dat, "id", s)
  if (nrow(dups) == 0L) skip("No matches in blocked fixture")

  mid <- dups$duplicate_group[1L]
  ex  <- explain_match(dups, s, base = dat, id = "id", match_id = mid)

  expect_s3_class(ex, "joinery::Match_Explanation")
  expect_equal(sum(ex@per_column_contrib$contribution), ex@score, tolerance = 1e-10)
})
