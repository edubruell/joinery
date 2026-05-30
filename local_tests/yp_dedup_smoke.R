# =============================================================================
# Smoke test — YP DuckDB dedup, 2020 / plz2='10' slice
# -----------------------------------------------------------------------------
# Test bed for v0.8 implementation pass (Items 1–4 in
# notes/v08_implementation_plan.md). Each item adds an assertion below.
#
# Inputs: pre-built ~/yp_duckdb/yellowpages.duckdb with the yp_raw table.
# Skips cleanly if the DB isn't present (local-only by design —
# local_tests/ is .Rbuildignored).
#
# Run with:
#   Rscript -e 'testthat::test_file("local_tests/yp_dedup_smoke.R")'
# =============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(duckdb); library(DBI); library(dplyr)
})

devtools::load_all(
  "/Users/ebr/Seafile/MeineBibliothek/git_projects/joinery",
  quiet = TRUE
)

test_that("YP 2020/plz2='10' slice dedup smoke", {

  db_path <- "~/yp_duckdb/yellowpages.duckdb"
  if (!file.exists(path.expand(db_path))) {
    skip("YP DuckDB not present at ~/yp_duckdb/yellowpages.duckdb")
  }

  # RW because joinery's DuckDB backend creates temp tables. Smoke
  # test only reads yp_raw; it leaves random _joinery_* temps behind
  # that get DROPped at the end of the run.
  con <- dbConnect(duckdb::duckdb(), dbdir = path.expand(db_path),
                   read_only = FALSE)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dbExecute(con, "PRAGMA threads=6")
  dbExecute(con, "PRAGMA memory_limit='16GB'")

  # A single small-but-multi-block slice: 2020, one urban plz2.
  # ~47k rows, ~220 wz08_3 blocks.
  slice <- tbl(con, "yp_raw") |>
    filter(year == 2020L, plz2 == "10")

  strat <- search_strategy(
    name   ~ normalize_text + word_tokens(min_nchar = 3),
    street ~ normalize_text + word_tokens(min_nchar = 3),
    weights   = c(name = 0.6, street = 0.4),
    block_by  = c("year", "plz2", "wz08_3"),
    threshold = 0.90
  )

  # Item 2: filtered lazy must not error out.
  dups <- detect_duplicates(slice, id = "entry_id_yp", strategy = strat)
  dups_df <- as.data.frame(dups)

  # Item 1: schema must be the full shape (id + dedup cols + all base
  # columns except the renamed id), even if some blocks produced zero
  # pairs. yp_raw has 13 columns; entry_id_yp becomes id, so we expect
  # 4 dedup cols + 12 carried cols = 16 columns.
  expected_cols <- c(
    "id", "duplicate_group", "score", "rank",
    "year", "plz2", "wz08_3", "name", "street", "ort_n",
    "plz5", "vorwahl", "rufnummer", "email", "webadresse", "atom"
  )
  expect_setequal(names(dups_df), expected_cols)

  # Item 3: CC must scale — assertion is on wall-clock, not correctness.
  # < 60s for a 47k-row, ~220-block slice. Skipped until Item 3 attaches
  # the attribute (the plan introduces this contract).
  ws <- attr(dups, "wall_seconds")
  if (is.null(ws)) {
    skip("wall_seconds attribute not yet wired (Item 3 pending)")
  } else {
    expect_lt(ws, 60)
  }

  # Item 4: cardinality recommendation must fire on a slice we know
  # contains 1-name / 1-street giant groups (Phase 2 explode artefact).
  ov <- summarise_matches(dups, threshold = 0.90,
                          entity_cols = c("name", "street"))
  recs <- paste(recommendations(ov), collapse = " | ")
  expect_true(
    grepl("identical values across all entity columns", recs,
          ignore.case = TRUE)
  )
})
