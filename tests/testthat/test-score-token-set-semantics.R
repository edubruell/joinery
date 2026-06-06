# Regression: scoring must treat each record's tokens as a SET, not a bag.
# A token repeated within one record's column must contribute once, so a pair
# score can never exceed sum(weights). Before the fix, the token-overlap
# self-join multiplied a shared token's rIP by the product of per-record
# multiplicities, producing scores well above 1.0 (e.g. "Fritzel Fritzel
# Fritzel" -> 2.8 on real Yellow-Pages data).

make_strategy <- function() {
  search_strategy(
    name ~ normalize_text + word_tokens(min_nchar = 3),
    weights   = c(name = 1.0),
    threshold = 0.10
  )
}

# Record b repeats the (shared, rare) token "fritzel" 3x; a and c have it once.
# All three are token-identical as SETS, so every pair must score exactly 1.0.
toy <- function() {
  data.table::data.table(
    eid  = c("a", "b", "c"),
    name = c("fritzel", "fritzel fritzel fritzel", "fritzel")
  )
}

test_that("data.table dedup: repeated tokens do not inflate score past sum(weights)", {
  res <- detect_duplicates(data.table::copy(toy()), id = "eid",
                           strategy = make_strategy())
  dt  <- data.table::as.data.table(res)
  expect_true(all(dt$score <= 1.0 + 1e-9))
  expect_equal(unique(dt$score), 1.0, tolerance = 1e-9)
})

test_that("DuckDB dedup: repeated tokens do not inflate score past sum(weights)", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::dbWriteTable(con, "toy", toy())

  res <- detect_duplicates(dplyr::tbl(con, "toy"), id = "eid",
                           strategy = make_strategy())
  dt  <- data.table::as.data.table(dplyr::collect(res))
  expect_true(all(dt$score <= 1.0 + 1e-9))
  expect_equal(unique(dt$score), 1.0, tolerance = 1e-9)
})

test_that("backend parity: set-semantics scores agree on data.table and DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  s <- make_strategy()
  dt_res <- data.table::as.data.table(
    detect_duplicates(data.table::copy(toy()), id = "eid", strategy = s)
  )

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::dbWriteTable(con, "toy", toy())
  dk_res <- data.table::as.data.table(
    dplyr::collect(detect_duplicates(dplyr::tbl(con, "toy"), id = "eid", strategy = s))
  )

  expect_equal(sort(dt_res$score), sort(dk_res$score), tolerance = 1e-9)
})

test_that("explain_match round-trip holds on a multiplicity pair", {
  # The attribution path must apply the identical set collapse, so
  # sum(per_column_contrib$contribution) still equals the reported score.
  base <- toy()
  s    <- make_strategy()
  dups <- detect_duplicates(data.table::copy(base), id = "eid", strategy = s)
  dt   <- data.table::as.data.table(dups)

  mid  <- dt$duplicate_group[1]
  ex   <- explain_match(dups, s, base = base, id = "eid", match_id = mid)
  expect_equal(sum(ex@per_column_contrib$contribution), ex@score,
               tolerance = 1e-10)
  expect_true(ex@score <= 1.0 + 1e-9)
})
