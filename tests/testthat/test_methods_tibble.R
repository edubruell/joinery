test_that("detect_duplicates() works with tibble input and returns a tibble", {
  skip_if_not_installed("tibble")

  tbl <- tibble::tibble(
    id   = c("A", "B"),
    name = c("alpha", "alpha")
  )

  strat <- search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold = 0.5
  )

  result <- detect_duplicates(tbl, "id", strat)

  expect_true(is.data.frame(result))
  expect_true(tibble::is_tibble(result))
  expect_true(all(c("duplicate_group", "id", "score", "rank") %in% names(result)))
})

test_that("detect_duplicates() works with data.frame input and returns a data.frame", {
  df <- data.frame(
    id   = c("A", "B"),
    name = c("alpha", "alpha"),
    stringsAsFactors = FALSE
  )

  strat <- search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold = 0.5
  )

  result <- detect_duplicates(df, "id", strat)

  expect_true(is.data.frame(result))
  expect_false(data.table::is.data.table(result))
  expect_true(all(c("duplicate_group", "id", "score", "rank") %in% names(result)))
})
