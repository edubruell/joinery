# rarity_scope = "block" | "global" (Feature B, notes/region_free_linking.md §5).
#
# The single load-bearing invariant: rarity scope and the fan-out cost axis
# DECOUPLE. The block-local `df` column (consumed by max_token_df, the fan-out
# guard, and the prefilter) MUST stay block-local under both scopes; only the
# `rarity` metric switches to corpus-wide counts under scope = "global".
# See §5.2.

library(data.table)


# ---------------------------------------------------------------------------
# Fixture: token "shared" appears in two different blocks (one row each); the
# block-local df of "shared" is 1 in each block, but its corpus-wide df is 2.
# "common" repeats inside block A only.
# ---------------------------------------------------------------------------

.scope_fixture <- function() {
  data.table(
    id    = c("a1", "a2", "b1"),
    blk   = c("A",  "A",  "B"),
    name  = c("shared common", "common", "shared")
  )
}

.strat_block <- function() search_strategy(
  name ~ normalize_text() + word_tokens(),
  block_by = "blk", threshold = 0.5
)
.strat_global <- function() search_strategy(
  name ~ normalize_text() + word_tokens(),
  block_by = "blk", rarity_scope = "global", threshold = 0.5
)


# ---------------------------------------------------------------------------
# 1. global df is the corpus-wide distinct-row count; block-local df is not
# ---------------------------------------------------------------------------

test_that("df_global is corpus-wide while block-local df stays block-local", {
  d  <- .scope_fixture()
  s  <- .strat_global()
  tk <- compute_rarity(prepare_search_data(d, "id", s), s)

  sh_g <- unique(tk[token == "SHARED", df_global])
  sh_b <- tk[token == "SHARED", df]

  # "shared" appears in two blocks, one distinct row in each.
  expect_equal(sh_g, 2L)                 # corpus-wide
  expect_true(all(sh_b == 1L))           # block-local, per block

  # N_global is the corpus-wide distinct-row count for the column (3 rows).
  expect_equal(unique(tk$N_global), 3L)
})


# ---------------------------------------------------------------------------
# 2. Cost axis unaffected: block-local df identical under block vs global scope
# ---------------------------------------------------------------------------

test_that("block-local df column is identical whether scope is block or global", {
  d  <- .scope_fixture()

  tk_b <- compute_rarity(prepare_search_data(d, "id", .strat_block()),  .strat_block())
  tk_g <- compute_rarity(prepare_search_data(d, "id", .strat_global()), .strat_global())

  key <- c("id", "src_column", "token", "blk")
  setkeyv(tk_b, key); setkeyv(tk_g, key)

  expect_identical(tk_b[, ..key], tk_g[, ..key])
  expect_identical(tk_b$df, tk_g$df)     # the fan-out / max_token_df axis
  expect_identical(tk_b$freq, tk_g$freq)
  expect_identical(tk_b$N, tk_g$N)

  # block scope must NOT carry the global trio at all.
  expect_false("df_global" %in% names(tk_b))
})


# ---------------------------------------------------------------------------
# 3. Rarity-inflation regression (§35): a token repeated across many fine
#    blocks keeps HIGH rarity under global scope; a corpus-common token gets
#    LOW global rarity.
# ---------------------------------------------------------------------------

test_that("global scope keeps a fine-block-repeated rare token high-rarity", {
  # "persist" belongs to one firm that recurs once in each of 4 fine blocks
  # (block-local freq 1 everywhere, but it would look 'frequent' to any
  # corpus-naive eye). "common" is genuinely corpus-common (8 rows).
  d <- data.table(
    id   = paste0("r", 1:8),
    blk  = c("1", "2", "3", "4", "1", "2", "3", "4"),
    name = c("persist common", "persist common", "persist common", "persist common",
             "common",         "common",         "common",         "common")
  )
  s  <- search_strategy(name ~ normalize_text() + word_tokens(),
                        block_by = "blk", rarity_scope = "global", threshold = 0.5)
  tk <- compute_rarity(prepare_search_data(d, "id", s), s)

  r_persist <- unique(tk[token == "PERSIST", rarity])
  r_common  <- unique(tk[token == "COMMON",  rarity])

  # inverse_freq global: persist df_global 4 vs common df_global 8.
  expect_true(r_persist > r_common)
})


# ---------------------------------------------------------------------------
# 4. explain_match round-trip under scope = "global"
#    sum(per_column_contrib$contribution) == score, exact (no feedback).
# ---------------------------------------------------------------------------

test_that("explain_match round-trip holds under rarity_scope = global", {
  dat <- data.table(
    id         = c("r1", "r2", "r3", "r4"),
    first_name = c("alice", "alice", "bob",   "carol"),
    last_name  = c("smith", "smyth", "jones", "baker")
  )
  s <- search_strategy(
    first_name ~ normalize_text() + word_tokens(),
    last_name  ~ normalize_text() + word_tokens(),
    rarity_scope = "global",
    threshold    = 0.3
  )
  dups <- detect_duplicates(dat, "id", s)
  ex   <- explain_match(dups, s, base = dat, id = "id", match_id = 1L)

  expect_equal(sum(ex@per_column_contrib$contribution), ex@score, tolerance = 1e-10)
})


# ---------------------------------------------------------------------------
# 5. Backend parity (data.table vs DuckDB) under global scope
# ---------------------------------------------------------------------------

test_that("global-rarity dedup matches on data.table and DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  d <- .scope_fixture()
  s <- .strat_global()

  dt_dup <- detect_duplicates(d, "id", s)

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "d", as.data.frame(d))
  dk_dup <- detect_duplicates(dplyr::tbl(con, "d"), "id", s) |>
    dplyr::collect() |> data.table::as.data.table()

  expect_setequal(dt_dup$id, dk_dup$id)

  # Compare per-pair scores within tolerance, keyed on the duplicate id set.
  setkey(dt_dup, id); setkey(dk_dup, id)
  expect_equal(dt_dup[order(id)]$score, dk_dup[order(id)]$score, tolerance = 1e-8)
})


# ---------------------------------------------------------------------------
# 6. print() surfaces global scope on the rarity line
# ---------------------------------------------------------------------------

test_that("print surfaces global rarity scope and leaves block scope unchanged", {
  out_g <- cli::cli_fmt(print(.strat_global()))
  expect_true(any(grepl("rarity:.*global", out_g)))

  out_b <- cli::cli_fmt(print(.strat_block()))
  expect_false(any(grepl("global", out_b)))
})
