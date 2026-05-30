# Item 4 (v0.8 implementation plan) — when `entity_cols` is supplied
# to summarise_matches() and many duplicate groups have identical
# values across all entity columns, the cluster_identical_name_street
# recommendation must fire and suppress duplicates_mega_cluster.

test_that("identical-entity recommendation fires and suppresses mega_cluster", {
  # 10 groups of 60 rows each, all with identical name+street within
  # group (an upstream cardinality artefact). This guarantees both
  # max_cluster_size >= 50 (mega_cluster fires) AND >= 5 identical
  # groups (cardinality recommendation fires + suppresses mega_cluster).
  dups <- data.table::rbindlist(lapply(seq_len(10), function(g) {
    data.table::data.table(
      id              = paste0("g", g, "_r", seq_len(60)),
      duplicate_group = g,
      score           = 1.0,
      rank            = seq_len(60),
      name            = paste0("clinic ", g),
      street          = paste0("road ", g)
    )
  }))

  ov <- summarise_matches(dups,
                          entity_cols = c("name", "street"))
  recs_ids <- attr(ov, "recommendation_ids")

  expect_true("cluster_identical_name_street" %in% recs_ids)
  expect_false("duplicates_mega_cluster" %in% recs_ids)

  # n_identical_entity_groups must be reported on Match_Overview.
  expect_equal(ov@cluster_summary$n_identical_entity_groups, 10L)

  # The message references both the count and the entity column names.
  msg <- paste(ov@recommendations, collapse = " | ")
  expect_true(grepl("name, street", msg))
  expect_true(grepl("10 duplicate groups", msg))
})

test_that("entity_cols absent: mega_cluster fires as before", {
  dups <- data.table::rbindlist(lapply(seq_len(3), function(g) {
    data.table::data.table(
      id              = paste0("g", g, "_r", seq_len(60)),
      duplicate_group = g,
      score           = 1.0,
      rank            = seq_len(60),
      name            = paste0("clinic ", g),
      street          = paste0("road ", g)
    )
  }))

  ov <- summarise_matches(dups)
  recs_ids <- attr(ov, "recommendation_ids")
  expect_true("duplicates_mega_cluster" %in% recs_ids)
  expect_false("cluster_identical_name_street" %in% recs_ids)
})

test_that("identical-entity recommendation works on DuckDB matches", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  dups <- data.table::rbindlist(lapply(seq_len(8), function(g) {
    data.table::data.table(
      id              = paste0("g", g, "_r", seq_len(55)),
      duplicate_group = g,
      score           = 1.0,
      rank            = seq_len(55),
      name            = paste0("clinic ", g),
      street          = paste0("road ", g)
    )
  }))

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "mocked_dups", as.data.frame(dups))
  matches_tbl <- dplyr::tbl(con, "mocked_dups")

  ov <- summarise_matches(matches_tbl,
                          entity_cols = c("name", "street"))
  recs_ids <- attr(ov, "recommendation_ids")
  expect_true("cluster_identical_name_street" %in% recs_ids)
  expect_false("duplicates_mega_cluster" %in% recs_ids)
  expect_equal(ov@cluster_summary$n_identical_entity_groups, 8L)
})
