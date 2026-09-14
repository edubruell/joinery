# The data.table detect_duplicates() empty branch must emit the same schema as
# its own non-empty branch, and the same schema the DuckDB backend emits, so
# that callers rbind()-ing results across blocks don't hit a column mismatch.
# Companion to test-duckdb-dedup-empty-schema.R.

test_that("detect_duplicates() returns full schema even when no pairs match", {
  base <- data.table::data.table(
    id     = c("a", "b", "c"),
    block  = c("X", "X", "Y"),
    name   = c("alpha", "beta", "gamma"),
    street = c("aaa", "bbb", "ccc")
  )

  strat <- search_strategy(
    name   ~ normalize_text + word_tokens(min_nchar = 3),
    street ~ normalize_text + word_tokens(min_nchar = 3),
    weights   = c(name = 0.5, street = 0.5),
    block_by  = "block",
    threshold = 0.99   # impossibly high, no pair will match
  )

  out <- detect_duplicates(base, id = "id", strategy = strat)

  expect_setequal(
    names(out),
    c("id", "duplicate_group", "score", "rank", "block", "name", "street")
  )
  expect_equal(nrow(out), 0L)
})

test_that("empty and non-empty results share one schema and rbind cleanly", {
  base <- data.table::data.table(
    id     = c("a", "b", "c", "d"),
    block  = c("X", "X", "Y", "Y"),
    name   = c("identical name", "identical name", "alpha", "beta"),
    street = c("same street",    "same street",    "aaa",   "bbb")
  )

  strat <- search_strategy(
    name   ~ normalize_text + word_tokens(min_nchar = 3),
    street ~ normalize_text + word_tokens(min_nchar = 3),
    weights   = c(name = 0.5, street = 0.5),
    block_by  = "block",
    threshold = 0.5
  )

  dx <- detect_duplicates(base[block == "X"], id = "id", strategy = strat)
  dy <- detect_duplicates(base[block == "Y"], id = "id", strategy = strat)

  expect_setequal(names(dx), names(dy))
  expect_equal(names(dx), names(dy))

  combined <- rbind(dx, dy)
  expect_setequal(
    names(combined),
    c("id", "duplicate_group", "score", "rank", "block", "name", "street")
  )
  expect_gt(nrow(dx), 0L)
  expect_equal(nrow(dy), 0L)
})

test_that("empty data.table and DuckDB results agree on schema", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  base <- data.frame(
    id     = c("a", "b", "c"),
    block  = c("X", "X", "Y"),
    name   = c("alpha", "beta", "gamma"),
    street = c("aaa", "bbb", "ccc"),
    stringsAsFactors = FALSE
  )

  strat <- search_strategy(
    name   ~ normalize_text + word_tokens(min_nchar = 3),
    street ~ normalize_text + word_tokens(min_nchar = 3),
    weights   = c(name = 0.5, street = 0.5),
    block_by  = "block",
    threshold = 0.99
  )

  dt_out <- detect_duplicates(
    data.table::as.data.table(base), id = "id", strategy = strat
  )

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "src", base)
  duck_out <- as.data.frame(
    detect_duplicates(dplyr::tbl(con, "src"), id = "id", strategy = strat)
  )

  expect_setequal(names(dt_out), names(duck_out))
  expect_equal(nrow(dt_out), 0L)
  expect_equal(nrow(duck_out), 0L)
})
