library(testthat)
library(data.table)
library(joinery)

# ============================================================
# Reserved column name validation
# ============================================================

test_that(".check_reserved_names errors on data column matching reserved name", {
  dt <- data.table(id = 1:3, name = c("alice", "bob", "carol"), score = c(0.9, 0.8, 0.7))
  s  <- search_strategy(name ~ word_tokens())
  expect_error(
    detect_duplicates(dt, id = "id", strategy = s),
    "conflict"
  )
})

test_that(".check_reserved_names errors when id column is a reserved name", {
  dt <- data.table(rank = 1:3, name = c("alice", "bob", "carol"))
  s  <- search_strategy(name ~ word_tokens())
  expect_error(
    detect_duplicates(dt, id = "rank", strategy = s),
    "conflict"
  )
})

test_that(".check_reserved_names errors on token reserved name", {
  dt <- data.table(id = 1:3, token = c("a", "b", "c"), name = c("alice", "bob", "carol"))
  s  <- search_strategy(name ~ word_tokens())
  expect_error(
    detect_duplicates(dt, id = "id", strategy = s),
    "conflict"
  )
})

test_that(".check_reserved_names passes for clean column names", {
  dt <- data.table(id = 1:3, first_name = c("alice", "alice b", "bob"))
  s  <- search_strategy(first_name ~ word_tokens())
  expect_no_error(detect_duplicates(dt, id = "id", strategy = s))
})

test_that(".check_reserved_names errors on src_column in search_candidates", {
  base   <- data.table(id = 1:3, name = c("alice", "bob", "carol"), src_column = "x")
  target <- data.table(id = 11:13, name = c("alice", "bob", "dave"))
  s      <- search_strategy(name ~ word_tokens())
  expect_error(
    search_candidates(base, target, "id", "id", s),
    "conflict"
  )
})

test_that(".check_reserved_names fires on match_id column", {
  dt <- data.table(id = 1:3, name = c("alice", "bob", "carol"), match_id = 1:3)
  s  <- search_strategy(name ~ word_tokens())
  expect_error(
    detect_duplicates(dt, id = "id", strategy = s),
    "conflict"
  )
})

# ============================================================
# Weight name validation in search_strategy constructor
# ============================================================

test_that("search_strategy errors when weight names don't match any preparer column", {
  expect_error(
    search_strategy(
      name ~ word_tokens(),
      weights = c(nonexistent = 2.0)
    ),
    "not found in any preparer"
  )
})

test_that("search_strategy accepts weights matching preparer columns", {
  expect_no_error(
    search_strategy(
      name ~ word_tokens(),
      city ~ word_tokens(),
      weights = c(name = 2.0, city = 1.0)
    )
  )
})

test_that("search_strategy accepts partial weights (subset of preparer columns)", {
  expect_no_error(
    search_strategy(
      name ~ word_tokens(),
      city ~ word_tokens(),
      weights = c(name = 2.0)
    )
  )
})
