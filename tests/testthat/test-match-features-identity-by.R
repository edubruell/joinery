# `icnt` counts distinct found records. When the candidate side has already
# been resolved into entities, several records stand for one entity and the
# whitepaper's identity count is a different number. `identity_by` names the
# entity column so `ident_cnt` can carry it. Contention C6, resolved 2026-09-14.

make_strategy <- function() {
  search_strategy(
    name  ~ normalize_text + word_tokens(min_nchar = 3),
    town  ~ normalize_text + word_tokens(min_nchar = 3),
    weights   = c(name = 0.5, town = 0.5),
    threshold = 0.1
  )
}

# Three target records, two of which are the same entity.
target_tbl <- function() {
  data.table::data.table(
    tid    = c("t1", "t2", "t3"),
    entity = c("E1", "E1", "E2"),
    name   = c("oakfield joinery", "oakfield joinery ltd", "brightwood works"),
    town   = c("hexham", "hexham", "hexham")
  )
}

base_tbl <- function() {
  data.table::data.table(
    bid  = "b1",
    name = "oakfield joinery",
    town = "hexham"
  )
}

test_that("ident_cnt is absent unless identity_by is supplied", {
  strat <- make_strategy()
  base  <- base_tbl()
  targ  <- target_tbl()

  cand <- search_candidates(base, targ, "bid", "tid", strat)
  feats <- match_features(cand, strat, base = base, id = "bid",
                          target = targ, target_id = "tid")

  df <- as.data.frame(feats)
  expect_true(all(c("cnt", "icnt", "ipos") %in% names(df)))
  expect_false("ident_cnt" %in% names(df))
})

test_that("ident_cnt counts entities where icnt counts records", {
  strat <- make_strategy()
  base  <- base_tbl()
  targ  <- target_tbl()

  cand <- search_candidates(base, targ, "bid", "tid", strat)
  feats <- match_features(cand, strat, base = base, id = "bid",
                          target = targ, target_id = "tid",
                          identity_by = "entity")

  df <- as.data.frame(feats)
  expect_true("ident_cnt" %in% names(df))

  # Every row belongs to the one searched record, so both counts are constant
  # across the block. The block spans fewer entities than records.
  expect_equal(length(unique(df$ident_cnt)), 1L)
  expect_lt(df$ident_cnt[1], df$icnt[1])

  n_entities <- length(unique(
    targ$entity[targ$tid %in% df$found]
  ))
  expect_equal(df$ident_cnt[1], n_entities)
  expect_equal(df$icnt[1], length(unique(df$found)))
})

test_that("ident_cnt equals icnt when one record is one entity", {
  strat <- make_strategy()
  base  <- base_tbl()
  targ  <- target_tbl()
  targ$entity <- targ$tid   # every record its own entity

  cand <- search_candidates(base, targ, "bid", "tid", strat)
  feats <- match_features(cand, strat, base = base, id = "bid",
                          target = targ, target_id = "tid",
                          identity_by = "entity")

  df <- as.data.frame(feats)
  expect_equal(df$ident_cnt, df$icnt)
})

test_that("identity_by is validated", {
  strat <- make_strategy()
  base  <- base_tbl()
  targ  <- target_tbl()
  cand  <- search_candidates(base, targ, "bid", "tid", strat)

  expect_error(
    match_features(cand, strat, base = base, id = "bid",
                   target = targ, target_id = "tid",
                   identity_by = "no_such_column"),
    "not found on the found side"
  )

  expect_error(
    match_features(cand, strat, base = base, id = "bid",
                   target = targ, target_id = "tid",
                   identity_by = c("entity", "name")),
    "must be a single column name"
  )

  expect_error(
    match_features(cand, strat, base = base, id = "bid",
                   target = targ, target_id = "tid",
                   identity_by = "entity",
                   include_block_stats = FALSE),
    "needs"
  )
})

test_that("identity_by works on the dedup face against the base table", {
  strat <- search_strategy(
    name ~ normalize_text + word_tokens(min_nchar = 3),
    weights   = c(name = 1),
    threshold = 0.1
  )
  base <- data.table::data.table(
    bid    = c("b1", "b2", "b3"),
    entity = c("E1", "E1", "E1"),
    name   = c("oakfield joinery", "oakfield joinery", "oakfield joinery")
  )

  dups <- detect_duplicates(base, id = "bid", strategy = strat)
  skip_if(nrow(dups) == 0L, "fixture produced no duplicate pairs")

  feats <- match_features(dups, strat, base = base, id = "bid",
                          identity_by = "entity")
  df <- as.data.frame(feats)

  expect_true("ident_cnt" %in% names(df))
  expect_true(all(df$ident_cnt == 1L))
})

test_that("DuckDB match_features accepts identity_by and agrees with data.table", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  strat <- make_strategy()
  base  <- base_tbl()
  targ  <- target_tbl()
  cand  <- search_candidates(base, targ, "bid", "tid", strat)

  dt_feats <- as.data.frame(
    match_features(cand, strat, base = base, id = "bid",
                   target = targ, target_id = "tid",
                   identity_by = "entity")
  )

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "cand", as.data.frame(cand))
  duck_feats <- as.data.frame(
    match_features(dplyr::tbl(con, "cand"), strat,
                   base = base, id = "bid",
                   target = targ, target_id = "tid",
                   identity_by = "entity")
  )

  expect_true("ident_cnt" %in% names(duck_feats))
  expect_equal(
    duck_feats$ident_cnt[order(duck_feats$found)],
    dt_feats$ident_cnt[order(dt_feats$found)]
  )
})
