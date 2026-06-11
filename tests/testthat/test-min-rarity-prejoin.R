# min_rarity pre-join verification (v0.8 Stage 04, Part 1).
#
# The single biggest cheap lever for a slow/dense linkage is to thin the token
# table BEFORE the (src_column, token, block) overlap join — never after
# scoring, where it would save nothing. These tests lock that the cut is
# pre-join and prevent a future "tidy-up" from reordering the filter past the
# join. The strong assertion is on the INTERMEDIATE pair count
# (.score_token_pairs output), not just the final result.
#
# Fixture: a single block where ZETA is a rare token shared only by the true
# duplicate pair (r1/r2), while COMMON is a hyper-common token shared by four
# other records (r3..r6), fanning the intermediate. Raising min_rarity past
# COMMON's rarity must collapse the COMMON fan-out while leaving the ZETA pair
# and its score untouched.

library(data.table)

.prejoin_fixture <- function() {
  data.table(
    id   = c("r1", "r2", "r3", "r4", "r5", "r6"),
    name = c("Zeta", "Zeta", "Common", "Common", "Common", "Common")
  )
}

# inverse_freq rarity = 1/corpus-freq. ZETA freq 2 -> rarity 0.5;
# COMMON freq 4 -> rarity 0.25. A cut at 0.3 drops COMMON, keeps ZETA.
.s0 <- function() search_strategy(name ~ normalize_text() + word_tokens(),
                                  threshold = 0.5, min_rarity = 0)
.s1 <- function() search_strategy(name ~ normalize_text() + word_tokens(),
                                  threshold = 0.5, min_rarity = 0.3)

# ---------------------------------------------------------------------------
# 1. White-box: the cut shrinks the INTERMEDIATE pair count, not just the final
# ---------------------------------------------------------------------------

test_that("min_rarity thins the .score_token_pairs intermediate, pre-join", {
  d <- .prejoin_fixture()
  w <- c(name = 1)

  intermediate <- function(strat) {
    tk <- compute_rarity(prepare_search_data(d, "id", strat), strat)
    tk <- .rarity_prefilter_dt(tk, strat)              # the pre-join cut
    pr <- .score_token_pairs(tk, tk, "id", "id", strat, w)
    pr[lhs_id != rhs_id]
  }

  p0 <- intermediate(.s0())
  p1 <- intermediate(.s1())

  # min_rarity=0: ZETA pair (2 ordered) + COMMON fan-out among r3..r6 (4*3=12).
  expect_equal(nrow(p0), 14L)
  # cut drops every COMMON pair, leaving only the ZETA pair (both directions).
  expect_equal(nrow(p1), 2L)

  # The high-rarity (ZETA) pair survives with an IDENTICAL score — rIP is
  # normalised within (id, column) and ZETA is r1/r2's only token, so dropping
  # COMMON elsewhere cannot perturb it.
  s_before <- p0[lhs_id == "r1" & rhs_id == "r2", score]
  s_after  <- p1[lhs_id == "r1" & rhs_id == "r2", score]
  expect_equal(s_after, s_before)
  expect_equal(s_after, 1.0)
})

# ---------------------------------------------------------------------------
# 2. End-to-end proxy: final duplicate rows collapse to the ZETA pair
# ---------------------------------------------------------------------------

test_that("min_rarity drops COMMON-driven duplicates end-to-end (data.table)", {
  d <- .prejoin_fixture()

  dup0 <- detect_duplicates(d, "id", .s0())
  dup1 <- detect_duplicates(d, "id", .s1())

  expect_setequal(dup0$id, c("r1", "r2", "r3", "r4", "r5", "r6"))
  expect_setequal(dup1$id, c("r1", "r2"))
  expect_true(all(dup1$score == 1.0))
})

# ---------------------------------------------------------------------------
# 3. Two-backend same-predicate: identical surviving pairs on DT and DuckDB
# ---------------------------------------------------------------------------

test_that("min_rarity cut is identical on data.table and DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  d <- .prejoin_fixture()
  s <- .s1()

  dt_dup <- detect_duplicates(d, "id", s)

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "d", as.data.frame(d))
  dk_dup <- detect_duplicates(dplyr::tbl(con, "d"), "id", s) |>
    dplyr::collect() |> data.table::as.data.table()

  expect_setequal(dt_dup$id, dk_dup$id)
  expect_setequal(dk_dup$id, c("r1", "r2"))
})
