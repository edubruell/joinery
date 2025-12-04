test_that("compute_rarity() works for DuckDB backend", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  
  data("base_example")
  
  yp_strategy <- search_strategy(
    Nachname ~ normalize_text + word_tokens(min_nchar = 3),
    Vorname ~ normalize_text + word_tokens(min_nchar = 3),
    Strasse ~ normalize_text + word_tokens,
    Hausnummer ~ normalize_text + word_tokens,
    Ort ~ normalize_text + word_tokens,
    block_by = "Kreis"
  )
  
  # Setup DuckDB connection and table
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  DBI::dbWriteTable(con, "base_data", base_example, overwrite = TRUE)
  db_tbl <- dplyr::tbl(con, "base_data")
  
  # Prepare search data
  tokens_db <- prepare_search_data(
    data     = db_tbl,
    id       = "id_base",
    strategy = yp_strategy
  )
  
  # Compute rarity on DuckDB
  rar_db <- compute_rarity(tokens_db, yp_strategy)
  
  # Materialize to data.table for inspection
  rar_dt <- data.table::as.data.table(rar_db)
  
  # Basic structure ----------------------------------------------------------
  expect_true(data.table::is.data.table(rar_dt))
  expect_true("rarity" %in% names(rar_dt))
  expect_type(rar_dt$rarity, "double")
  
  # Rarity must be non-negative
  expect_true(all(rar_dt$rarity >= 0))
  
  # Expected grouping variables
  block_cols <- yp_strategy@block_by %||% character()
  expected_cols <- c("id_base", "src_column", "token", "row_id", block_cols, "freq", "df", "N", "rarity")
  expect_true(all(expected_cols %in% names(rar_dt)))
  
  # Inverse frequency expectation -------------------------------------------
  # For inverse_freq, rarity = 1 / freq
  subset <- rar_dt[1:20]
  expect_equal(
    subset$rarity,
    1 / subset$freq,
    tolerance = 1e-10
  )
  
  # Column/block grouping should yield same freq for duplicate rows ----------
  rar_check <- rar_dt[, .(
    n_rows        = .N,
    freq_unique   = uniqueN(freq),
    rarity_unique = uniqueN(rarity)
  ), by = c(block_cols, "src_column", "token")]
  
  # freq and rarity should be constant within each group
  expect_true(all(rar_check$freq_unique == 1))
  expect_true(all(rar_check$rarity_unique == 1))
})


test_that("DuckDB backend matches data.table backend for duplicate detection", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  
  data("base_example")
  
  strategy <- search_strategy(
    Nachname ~ normalize_text() + word_tokens(min_nchar = 3),
    Vorname  ~ normalize_text() + word_tokens(min_nchar = 3),
    Strasse  ~ normalize_street(lang = "de") + word_tokens(min_nchar = 3),
    Hausnummer ~ numeric_tokens,
    Ort ~ normalize_text(),
    block_by  = "Kreis",
    threshold = 0.8
  )
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  DBI::dbWriteTable(con, "base_example", base_example, overwrite = TRUE)
  duck_tbl <- dplyr::tbl(con, "base_example")
  
  duck_res <- duck_tbl |>
    detect_duplicates("id_base", strategy) |>
    dplyr::collect()
  
  # data.table path
  dt_res <- base_example |>
    as.data.table() |>
    detect_duplicates("id_base", strategy)
  
  # Both sides should detect the same ids
  expect_true(all(duck_res$id %in% dt_res$id))
  
  # Same number of rows
  expect_equal(nrow(duck_res), nrow(dt_res))
})

test_that("deduplicate_table() works and matches data.table backend", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  
  data("base_example")
  
  strategy <- search_strategy(
    Nachname ~ normalize_text() + word_tokens(min_nchar = 3),
    Vorname  ~ normalize_text() + word_tokens(min_nchar = 3),
    Strasse  ~ normalize_street(lang = "de") + word_tokens(min_nchar = 3),
    Hausnummer ~ numeric_tokens,
    Ort ~ normalize_text(),
    block_by  = "Kreis",
    threshold = 0.8
  )
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  DBI::dbWriteTable(con, "base_example", base_example, overwrite = TRUE)
  duck_tbl <- dplyr::tbl(con, "base_example")
  
  # DuckDB path
  duck_dups <- detect_duplicates(duck_tbl, "id_base", strategy)
  duck_dedup <- deduplicate_table(duck_tbl, duck_dups, "id_base") |>
    dplyr::collect()
  
  # data.table path
  dt_dups <- detect_duplicates(
    as.data.table(base_example),
    "id_base",
    strategy
  )
  dt_dedup <- deduplicate_table(
    as.data.table(base_example),
    dt_dups,
    "id_base"
  )
  
  # Counts match
  expect_equal(nrow(duck_dedup), nrow(dt_dedup))
  
  # No remaining duplicates (rank == 1 rule)
  duck_dups_dt <- as.data.table(duck_dups)
  removed_duck <- duck_dups_dt[rank != 1, id]
  expect_true(all(!duck_dedup$id_base %in% removed_duck))
})

