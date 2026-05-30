# Item 2 (v0.8 implementation plan) — DuckDB backend must accept
# filtered lazy queries (`tbl(con, "x") |> filter(...)`) as input.
# Previously `data$lazy_query$x` was assumed to be a length-1 string;
# filtered lazies break that.

skip_if_not_installed("duckdb")
skip_if_not_installed("DBI")
skip_if_not_installed("dplyr")

test_that("detect_duplicates() accepts a filtered lazy input", {
  base <- data.frame(
    id     = c("a", "b", "c", "d"),
    block  = c("X", "X", "Y", "Y"),
    name   = c("alpha matched", "alpha matched",
               "beta unrelated", "gamma other"),
    street = c("same address", "same address",
               "second street",  "third avenue"),
    stringsAsFactors = FALSE
  )

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "src", base)

  filtered <- dplyr::tbl(con, "src") |>
    dplyr::filter(block == "X")

  strat <- search_strategy(
    name   ~ normalize_text + word_tokens(min_nchar = 3),
    street ~ normalize_text + word_tokens(min_nchar = 3),
    weights   = c(name = 0.5, street = 0.5),
    block_by  = "block",
    threshold = 0.5
  )

  expect_no_error({
    dups <- detect_duplicates(filtered, id = "id", strategy = strat)
    out  <- as.data.frame(dups)
  })

  # The materialised slice must only contain block X rows.
  expect_true(all(out$block == "X"))
  expect_gt(nrow(out), 0L)
})

test_that("filtered lazy result matches the CREATE TABLE AS slice", {
  base <- data.frame(
    id     = as.character(1:6),
    block  = c("A", "A", "A", "B", "B", "B"),
    name   = c("foo", "foo", "bar", "baz", "qux", "qux"),
    street = c("aaa", "aaa", "bbb", "ccc", "ddd", "ddd"),
    stringsAsFactors = FALSE
  )

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "src", base)

  # Path 1: filtered lazy (Item 2 path).
  via_filter <- as.data.frame(
    detect_duplicates(
      dplyr::tbl(con, "src") |> dplyr::filter(block == "A"),
      id = "id",
      strategy = search_strategy(
        name   ~ normalize_text + word_tokens(min_nchar = 3),
        street ~ normalize_text + word_tokens(min_nchar = 3),
        weights   = c(name = 0.5, street = 0.5),
        block_by  = "block",
        threshold = 0.5
      )
    )
  )

  # Path 2: pre-materialised slice.
  DBI::dbExecute(con, "CREATE TEMP TABLE src_a AS SELECT * FROM src WHERE block = 'A'")
  via_mat <- as.data.frame(
    detect_duplicates(
      dplyr::tbl(con, "src_a"),
      id = "id",
      strategy = search_strategy(
        name   ~ normalize_text + word_tokens(min_nchar = 3),
        street ~ normalize_text + word_tokens(min_nchar = 3),
        weights   = c(name = 0.5, street = 0.5),
        block_by  = "block",
        threshold = 0.5
      )
    )
  )

  # Same id set, same scores, same group assignment.
  expect_setequal(via_filter$id, via_mat$id)
  expect_equal(
    nrow(via_filter), nrow(via_mat)
  )
})
