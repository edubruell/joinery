# Item 1 (v0.8 implementation plan) — empty-pairs branch of the
# DuckDB detect_duplicates() must emit the same schema as the
# non-empty branch, so that callers UNION-ing results across blocks
# don't fail with a column-count mismatch.

skip_if_not_installed("duckdb")
skip_if_not_installed("DBI")
skip_if_not_installed("dplyr")

test_that("detect_duplicates() returns full schema even when no pairs match", {
  base <- data.frame(
    id     = c("a", "b", "c"),
    block  = c("X", "X", "Y"),
    name   = c("alpha", "beta", "gamma"),
    street = c("aaa", "bbb", "ccc"),
    stringsAsFactors = FALSE
  )

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "src", base)
  src <- dplyr::tbl(con, "src")

  strat <- search_strategy(
    name   ~ normalize_text + word_tokens(min_nchar = 3),
    street ~ normalize_text + word_tokens(min_nchar = 3),
    weights   = c(name = 0.5, street = 0.5),
    block_by  = "block",
    threshold = 0.99   # impossibly high — no pair will match
  )

  dups <- detect_duplicates(src, id = "id", strategy = strat)
  out  <- as.data.frame(dups)

  # Schema must contain all 4 dedup cols + all base cols except id_col
  # (the base id is renamed to "id").
  expect_setequal(
    names(out),
    c("id", "duplicate_group", "score", "rank",
      "block", "name", "street")
  )
  expect_equal(nrow(out), 0L)
})

test_that("empty-pairs result can be UNIONed with a non-empty result", {
  # Two-block table; block X has a guaranteed dup pair, block Y does not.
  base <- data.frame(
    id     = c("a", "b", "c", "d"),
    block  = c("X", "X", "Y", "Y"),
    name   = c("identical name", "identical name", "alpha", "beta"),
    street = c("same street",    "same street",    "aaa",   "bbb"),
    stringsAsFactors = FALSE
  )

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "src_x", subset(base, block == "X"))
  DBI::dbWriteTable(con, "src_y", subset(base, block == "Y"))

  strat <- search_strategy(
    name   ~ normalize_text + word_tokens(min_nchar = 3),
    street ~ normalize_text + word_tokens(min_nchar = 3),
    weights   = c(name = 0.5, street = 0.5),
    block_by  = "block",
    threshold = 0.5
  )

  dx <- as.data.frame(
    detect_duplicates(dplyr::tbl(con, "src_x"), id = "id", strategy = strat)
  )
  dy <- as.data.frame(
    detect_duplicates(dplyr::tbl(con, "src_y"), id = "id", strategy = strat)
  )

  # Both branches must share the same schema, so rbind() works.
  expect_setequal(names(dx), names(dy))
  combined <- rbind(dx, dy)
  expect_setequal(
    names(combined),
    c("id", "duplicate_group", "score", "rank", "block", "name", "street")
  )
  # Non-empty side actually produced pairs:
  expect_gt(nrow(dx), 0L)
  expect_equal(nrow(dy), 0L)
})
