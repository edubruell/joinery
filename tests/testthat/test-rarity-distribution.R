# rarity_distribution() — read-side lever lookup (v0.8 Stage 04, Part 3).
#
# A scoring-free pre-match helper: it tokenizes + computes rarity and reports
# the per-(column[, block]) df/rarity distribution plus the top-df offender
# list, so a user SETS min_rarity / max_token_df from the real distribution
# instead of guessing. These tests verify the offender ranking, the suggested
# floor, that the verb does NOT perform the overlap join / scoring, and that
# the DuckDB path agrees with data.table.

library(data.table)

.rd_fixture <- function() {
  # STRASSE is the hyper-common fan-out driver (df 5); the rest are rarer.
  data.table(
    id   = c("a", "b", "c", "d", "e"),
    name = c("Anna Strasse", "Bert Strasse", "Cara Strasse",
             "Dora Strasse", "Emil Strasse")
  )
}

.rd_strat <- function() search_strategy(name ~ normalize_text() + word_tokens(),
                                        threshold = 0.5)

# ---------------------------------------------------------------------------
# 1. Offender list ranks the hyper-common token first; suggested floor matches
# ---------------------------------------------------------------------------

test_that("offenders rank the highest-df token first and suggest its rarity", {
  rd <- rarity_distribution(.rd_fixture(), "id", .rd_strat())

  expect_s3_class(rd, "joinery::Rarity_Distribution")

  off <- rd@offenders
  expect_equal(off$token[1L], "STRASSE")          # df 5, the worst driver
  expect_equal(off$df[1L], 5L)
  # df is sorted descending.
  expect_false(is.unsorted(rev(off$df)))

  # The suggested floor for the column is the top-df token's rarity, so setting
  # min_rarity just above it drops STRASSE.
  d <- rd@distribution
  expect_equal(d$top_token[d$src_column == "name"], "STRASSE")
  expect_equal(d$suggested_min_rarity[d$src_column == "name"],
               off$rarity[off$token == "STRASSE"][1L])
})

# ---------------------------------------------------------------------------
# 2. n_offenders bound + unblocked schema
# ---------------------------------------------------------------------------

test_that("n_offenders caps the list and unblocked output omits block", {
  rd <- rarity_distribution(.rd_fixture(), "id", .rd_strat(), n_offenders = 2L)
  expect_equal(nrow(rd@offenders), 2L)
  expect_false("block" %in% names(rd@offenders))     # no block_by -> no block col
  expect_false("block" %in% names(rd@distribution))
  expect_false(rd@blocked)
})

# ---------------------------------------------------------------------------
# 3. Blocked: per-block distribution, block key present
# ---------------------------------------------------------------------------

test_that("block_by yields a per-block distribution", {
  d <- data.table(
    id   = c("a", "b", "c", "d"),
    plz  = c("10", "10", "20", "20"),
    name = c("Anna Weg", "Bert Weg", "Cara Weg", "Dora Weg")
  )
  s  <- search_strategy(name ~ normalize_text() + word_tokens(),
                        block_by = "plz", threshold = 0.5)
  rd <- rarity_distribution(d, "id", s)

  expect_true(rd@blocked)
  expect_true("block" %in% names(rd@distribution))
  expect_setequal(rd@distribution$block, c("10", "20"))
  # WEG fans out within each block (df 2 per block), the offender in both.
  expect_true(all(rd@offenders[token == "WEG", df] == 2L))
})

# ---------------------------------------------------------------------------
# 4. Scoring-free: no blow-up on a dense block a real join would choke on
# ---------------------------------------------------------------------------

test_that("rarity_distribution is scoring-free (no O(n^2) overlap join)", {
  # 4000 records sharing one common token. A scoring path would form ~n^2/2 = 8M
  # pairs on this single block; the distribution lookup must stay ~linear.
  n  <- 4000L
  d  <- data.table(
    id   = as.character(seq_len(n)),
    name = paste0("Tok", seq_len(n), " Common")
  )
  s  <- .rd_strat()

  t  <- system.time(rd <- rarity_distribution(d, "id", s))[["elapsed"]]
  expect_lt(t, 5)                                  # would be many seconds if it scored
  expect_equal(rd@offenders$token[1L], "COMMON")
  expect_equal(rd@offenders$df[1L], n)
})

# ---------------------------------------------------------------------------
# 5. DuckDB path agrees with data.table (collect-and-delegate)
# ---------------------------------------------------------------------------

test_that("DuckDB rarity_distribution matches data.table", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  d <- .rd_fixture()
  s <- .rd_strat()

  dt_rd <- rarity_distribution(d, "id", s)

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "d", as.data.frame(d))
  dk_rd <- rarity_distribution(dplyr::tbl(con, "d"), "id", s)

  expect_equal(dk_rd@offenders$token[1L], dt_rd@offenders$token[1L])
  expect_equal(dk_rd@distribution$df_max, dt_rd@distribution$df_max)
  expect_equal(dk_rd@distribution$suggested_min_rarity,
               dt_rd@distribution$suggested_min_rarity)
})
