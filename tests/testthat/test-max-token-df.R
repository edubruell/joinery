# max_token_df guard (v0.8 Stage 04, Part 2).
#
# max_token_df caps raw document frequency within (block, column) — the blunt
# "drop any token appearing in > N records" knob, the companion to the
# rarity-metric min_rarity. It applies in the SAME pre-join predicate. These
# tests cover: default Inf changes nothing (back-compat); setting it below the
# common token's df drops exactly that token's pairs while high-rarity pairs
# and scores are unchanged; both backends agree; and it composes with
# min_rarity rather than conflicting.

library(data.table)

.df_fixture <- function() {
  data.table(
    id   = c("r1", "r2", "r3", "r4", "r5", "r6"),
    name = c("Zeta", "Zeta", "Common", "Common", "Common", "Common")
  )
}

# ZETA df 2, COMMON df 4. max_token_df = 3 drops COMMON, keeps ZETA.
.base   <- function() search_strategy(name ~ normalize_text() + word_tokens(),
                                      threshold = 0.5)
.capped <- function() search_strategy(name ~ normalize_text() + word_tokens(),
                                      threshold = 0.5, max_token_df = 3)

# ---------------------------------------------------------------------------
# 1. Default Inf = no change (back-compat is mandatory)
# ---------------------------------------------------------------------------

test_that("max_token_df defaults to Inf and changes nothing", {
  s <- .base()
  expect_identical(s@max_token_df, Inf)

  d <- .df_fixture()
  # All six records are mutual duplicates with the default (ZETA + COMMON both kept).
  dups <- detect_duplicates(d, "id", s)
  expect_setequal(dups$id, c("r1", "r2", "r3", "r4", "r5", "r6"))
})

# ---------------------------------------------------------------------------
# 2. Capping below the common df drops exactly its pairs (white-box)
# ---------------------------------------------------------------------------

test_that("max_token_df drops the high-df token pre-join, keeps the rare pair", {
  d <- .df_fixture()
  w <- c(name = 1)

  intermediate <- function(strat) {
    tk <- compute_rarity(prepare_search_data(d, "id", strat), strat)
    tk <- .rarity_prefilter_dt(tk, strat)
    pr <- .score_token_pairs(tk, tk, "id", "id", strat, w)
    pr[lhs_id != rhs_id]
  }

  p_full <- intermediate(.base())
  p_cap  <- intermediate(.capped())

  expect_equal(nrow(p_full), 14L)   # ZETA pair + COMMON fan-out
  expect_equal(nrow(p_cap),  2L)    # only the ZETA pair survives

  # ZETA pair score unchanged.
  expect_equal(
    p_cap[lhs_id == "r1" & rhs_id == "r2", score],
    p_full[lhs_id == "r1" & rhs_id == "r2", score]
  )
})

# ---------------------------------------------------------------------------
# 3. Both backends apply the identical df cap
# ---------------------------------------------------------------------------

test_that("max_token_df is identical on data.table and DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  d <- .df_fixture()
  s <- .capped()

  dt_dup <- detect_duplicates(d, "id", s)

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "d", as.data.frame(d))
  dk_dup <- detect_duplicates(dplyr::tbl(con, "d"), "id", s) |>
    dplyr::collect() |> data.table::as.data.table()

  expect_setequal(dt_dup$id, dk_dup$id)
  expect_setequal(dk_dup$id, c("r1", "r2"))
})

# ---------------------------------------------------------------------------
# 4. Composition with min_rarity — union of cuts on different axes, no conflict
# ---------------------------------------------------------------------------

test_that("max_token_df and min_rarity compose (union of cuts)", {
  # Both predicates active in one WHERE (an AND), so the result is the union of
  # what each drops. The two cut on different axes — raw df vs the rarity metric:
  #   RARE2 : df 2, rarity 0.5  -> dropped by the min_rarity = 0.6 floor
  #                                (df 2 <= 3, so the df cap alone would keep it)
  #   COMMON: df 4, rarity 0.25 -> dropped by the max_token_df = 3 cap
  #   UA/UB : df 1, rarity 1    -> kept by both
  # RARE2 is the orthogonality witness: only the floor removes it, proving the
  # floor is live alongside the df cap rather than redundant with it.
  d <- data.table(
    id   = c("a", "b", "c", "d", "e", "f"),
    name = c("Rare2 Ua", "Rare2 Ub", "Common", "Common", "Common", "Common")
  )
  s_both <- search_strategy(name ~ normalize_text() + word_tokens(),
                            threshold = 0.5, min_rarity = 0.6, max_token_df = 3)
  w <- c(name = 1)

  tk <- compute_rarity(prepare_search_data(d, "id", s_both), s_both)
  tk <- .rarity_prefilter_dt(tk, s_both)

  surviving <- sort(unique(tk$token))
  # RARE2 gone (rarity floor); COMMON gone (df cap); only the unique Ua/Ub remain.
  expect_false("RARE2"  %in% surviving)
  expect_false("COMMON" %in% surviving)
  expect_setequal(surviving, c("UA", "UB"))
})
