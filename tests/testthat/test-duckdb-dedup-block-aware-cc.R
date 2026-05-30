# Item 3 (v0.8 implementation plan) — the DuckDB CC step iterates
# blocks instead of running one global recursive CTE. Verifies:
#   (a) each block's CC is independent of other blocks' sizes,
#   (b) duplicate_group IDs are globally unique across blocks,
#   (c) an attr("wall_seconds") is attached to the result.

skip_if_not_installed("duckdb")
skip_if_not_installed("DBI")
skip_if_not_installed("dplyr")

test_that("CC iterates per block; group IDs are globally unique", {
  # Block X: a large fully-connected cluster of 6 rows on identical name+street.
  # Block Y: 2 independent matched pairs (a separate component each).
  base <- data.frame(
    id     = c(paste0("x", 1:6), paste0("y", 1:4)),
    block  = c(rep("X", 6), rep("Y", 4)),
    name   = c(rep("alpha clinic", 6),
               "beta",  "beta",  "gamma", "gamma"),
    street = c(rep("main street",  6),
               "one road", "one road", "two avenue", "two avenue"),
    stringsAsFactors = FALSE
  )

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "src", base)

  strat <- search_strategy(
    name   ~ normalize_text + word_tokens(min_nchar = 3),
    street ~ normalize_text + word_tokens(min_nchar = 3),
    weights   = c(name = 0.5, street = 0.5),
    block_by  = "block",
    threshold = 0.5
  )

  dups <- detect_duplicates(dplyr::tbl(con, "src"),
                            id = "id", strategy = strat)
  out  <- as.data.frame(dups)

  # All 10 ids appear (every row matches at least one other within block).
  expect_setequal(out$id, base$id)

  # Group IDs partition the ids: each id is in exactly one group.
  expect_equal(length(unique(out$id)), nrow(out))

  # Block X must form ONE group; block Y must form TWO groups
  # (the two beta rows and the two gamma rows).
  block_of_id <- setNames(base$block, base$id)
  groups_x <- unique(out$duplicate_group[block_of_id[out$id] == "X"])
  groups_y <- unique(out$duplicate_group[block_of_id[out$id] == "Y"])

  expect_length(groups_x, 1L)
  expect_length(groups_y, 2L)

  # No group ID is shared across blocks (globally unique).
  expect_length(intersect(groups_x, groups_y), 0L)
})

test_that("wall_seconds attribute is attached to dedup results", {
  base <- data.frame(
    id     = c("a", "b", "c"),
    block  = c("X", "X", "Y"),
    name   = c("foo bar", "foo bar", "qux"),
    street = c("road", "road", "lane"),
    stringsAsFactors = FALSE
  )

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "src", base)

  strat <- search_strategy(
    name   ~ normalize_text + word_tokens(min_nchar = 3),
    street ~ normalize_text + word_tokens(min_nchar = 3),
    weights   = c(name = 0.5, street = 0.5),
    block_by  = "block",
    threshold = 0.5
  )

  dups <- detect_duplicates(dplyr::tbl(con, "src"),
                            id = "id", strategy = strat)
  ws <- attr(dups, "wall_seconds")

  expect_true(is.numeric(ws))
  expect_gte(ws, 0)
})
