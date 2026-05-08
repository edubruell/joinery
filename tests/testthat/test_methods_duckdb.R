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

# ---------------------------------------------------------------------------
# SQL scoring path tests — token injection bypasses batch preprocessing
# ---------------------------------------------------------------------------

test_that("compute_rarity() supports tfidf, smoothed_inverse_freq, bm25 in DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  small <- data.table(
    id   = c("A", "B", "C", "D"),
    name = c("alpha beta", "alpha gamma", "delta epsilon", "delta zeta")
  )

  for (m in c("tfidf", "smoothed_inverse_freq", "bm25")) {
    strat <- search_strategy(
      name ~ normalize_text() + word_tokens(),
      rarity    = m,
      threshold = 0.1
    )
    # Compute tokens via data.table backend, inject into DuckDB for rarity step
    dt_tokens <- prepare_search_data(small, "id", strat)
    con <- local_duckdb_con()
    tok_name <- paste0("_tok_", sample.int(1e9, 1))
    DBI::dbWriteTable(con, tok_name, as.data.frame(dt_tokens))
    duck_rar <- compute_rarity(dplyr::tbl(con, tok_name), strat)
    rar_dt   <- data.table::as.data.table(duck_rar)

    expect_true(is.numeric(rar_dt$rarity), info = paste("numeric rarity for", m))
    expect_false(any(is.na(rar_dt$rarity)), info = paste("no NA for", m))
  }

  # Unknown rarity method errors inside DuckDB
  strat_bad <- search_strategy(
    name ~ normalize_text() + word_tokens(),
    rarity    = "unknown_method",
    threshold = 0.1
  )
  dt_tokens_bad <- prepare_search_data(small, "id", strat_bad)
  con2 <- local_duckdb_con()
  tok_name2 <- paste0("_tok_", sample.int(1e9, 1))
  DBI::dbWriteTable(con2, tok_name2, as.data.frame(dt_tokens_bad))
  expect_error(compute_rarity(dplyr::tbl(con2, tok_name2), strat_bad))
})

# Shared tiny fixture used across the remaining SQL-path tests
.duckdb_small_dup <- data.table(
  id       = 1:3,
  Nachname = c("Meyer", "Meier", "Mair"),
  grp      = "A"
)

.duckdb_strat_ngram <- search_strategy(
  Nachname ~ normalize_text() + generate_ngrams(2),
  threshold = 0.1
)

test_that("detect_duplicates() log smoothing works in DuckDB SQL path", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  strat <- search_strategy(
    Nachname ~ normalize_text() + generate_ngrams(2),
    threshold = 0.1,
    smoothing = smooth_rip_log()
  )
  be  <- compare_backends(.duckdb_small_dup, "id", strat)
  res <- detect_duplicates(be$duck_tbl, "id", strat,
                           base_tokens = be$dt_tokens) |>
    dplyr::collect()

  expect_true(is.numeric(res$score))
  expect_false(any(is.na(res$score)))
})

test_that("detect_duplicates() offset smoothing works in DuckDB SQL path", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  strat <- search_strategy(
    Nachname ~ normalize_text() + generate_ngrams(2),
    threshold = 0.1,
    smoothing = smooth_rip_offset(0.1)
  )
  be  <- compare_backends(.duckdb_small_dup, "id", strat)
  res <- detect_duplicates(be$duck_tbl, "id", strat,
                           base_tokens = be$dt_tokens) |>
    dplyr::collect()

  expect_true(is.numeric(res$score))
  expect_false(any(is.na(res$score)))
})

test_that("detect_duplicates() softmax smoothing works in DuckDB SQL path", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  strat <- search_strategy(
    Nachname ~ normalize_text() + generate_ngrams(2),
    threshold = 0.1,
    smoothing = smooth_rip_softmax(2.0)
  )
  be  <- compare_backends(.duckdb_small_dup, "id", strat)
  res <- detect_duplicates(be$duck_tbl, "id", strat,
                           base_tokens = be$dt_tokens) |>
    dplyr::collect()

  expect_true(is.numeric(res$score))
  expect_false(any(is.na(res$score)))
})

test_that("detect_duplicates() feedback_strength penalizes partial matches in DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  feedback_data <- data.table(
    id   = c("R1", "R2", "R3"),
    name = c("Smith Jones Brown", "Smith Jones Brown", "Smith Anderson Lee")
  )

  strat_no_fb <- search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold = 0.1
  )
  strat_fb <- search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold = 0.1,
    feedback_strength = 0.5
  )

  be_nofb <- compare_backends(feedback_data, "id", strat_no_fb)
  be_fb   <- compare_backends(feedback_data, "id", strat_fb)

  res_no_fb <- detect_duplicates(be_nofb$duck_tbl, "id", strat_no_fb,
                                 base_tokens = be_nofb$dt_tokens) |>
    dplyr::collect() |> data.table::as.data.table()

  res_fb <- detect_duplicates(be_fb$duck_tbl, "id", strat_fb,
                              base_tokens = be_fb$dt_tokens) |>
    dplyr::collect() |> data.table::as.data.table()

  # Both find results
  expect_gt(nrow(res_no_fb), 0)
  expect_gt(nrow(res_fb), 0)

  # If R3 appears in both, its score must be lower with feedback
  if ("R3" %in% res_no_fb$id && "R3" %in% res_fb$id) {
    expect_lt(res_fb[id == "R3", score][1], res_no_fb[id == "R3", score][1])
  }
})

test_that("detect_duplicates() min_rarity filters tokens in DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  # "foo" appears in all 4 records (freq=4, rarity=0.25 with inverse_freq)
  common_data <- data.table(
    id   = c("A", "B", "C", "D"),
    name = c("foo baz", "foo qux", "foo abc", "foo def")
  )

  strat_no_filter <- search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold  = 0.1,
    min_rarity = 0
  )
  strat_filtered <- search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold  = 0.1,
    min_rarity = 0.5   # "foo" rarity = 0.25 < 0.5 → filtered
  )

  be_nof <- compare_backends(common_data, "id", strat_no_filter)
  be_f   <- compare_backends(common_data, "id", strat_filtered)

  res_nof <- detect_duplicates(be_nof$duck_tbl, "id", strat_no_filter,
                               base_tokens = be_nof$dt_tokens) |>
    dplyr::collect()
  res_f   <- detect_duplicates(be_f$duck_tbl, "id", strat_filtered,
                               base_tokens = be_f$dt_tokens) |>
    dplyr::collect()

  expect_gt(nrow(res_nof), 0)
  expect_equal(nrow(res_f), 0)
})

test_that("detect_duplicates() returns correct empty schema in DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  no_overlap <- data.table(id = c("A", "B"), name = c("alpha", "omega"))
  strat <- search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold = 0.99
  )
  be  <- compare_backends(no_overlap, "id", strat)
  res <- detect_duplicates(be$duck_tbl, "id", strat,
                           base_tokens = be$dt_tokens) |>
    dplyr::collect()

  expect_equal(nrow(res), 0)
  expect_true(all(c("id", "duplicate_group", "score", "rank") %in% names(res)))
})

test_that("search_candidates() returns correct empty schema in DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  base_data   <- data.table(id = "A", name = "alpha")
  target_data <- data.table(id = "B", name = "omega")
  strat <- search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold = 0.99
  )

  # Single connection for both sides; inject pre-computed tokens
  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "src_base",   as.data.frame(base_data))
  DBI::dbWriteTable(con, "src_target", as.data.frame(target_data))
  base_tokens   <- prepare_search_data(base_data,   "id", strat)
  target_tokens <- prepare_search_data(target_data, "id", strat)

  res <- search_candidates(
    dplyr::tbl(con, "src_base"),
    dplyr::tbl(con, "src_target"),
    "id", "id", strat,
    base_tokens   = base_tokens,
    target_tokens = target_tokens
  ) |> dplyr::collect()

  expect_equal(nrow(res), 0)
  expect_true(all(c("match_id", "score", "source", "id", "rank") %in% names(res)))
})

test_that("search_candidates() max_candidates works in DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  base_data   <- data.table(id = "B1", name = "Smith")
  target_data <- data.table(
    id   = paste0("T", 1:5),
    name = c("Smith", "Smith", "Smithson", "Smithe", "Smith")
  )
  strat_limited <- search_strategy(
    name ~ normalize_text() + generate_ngrams(2),
    threshold      = 0.1,
    max_candidates = 2
  )

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "base_sc",   as.data.frame(base_data))
  DBI::dbWriteTable(con, "target_sc", as.data.frame(target_data))
  base_tokens   <- prepare_search_data(base_data,   "id", strat_limited)
  target_tokens <- prepare_search_data(target_data, "id", strat_limited)

  res <- search_candidates(
    dplyr::tbl(con, "base_sc"),
    dplyr::tbl(con, "target_sc"),
    "id", "id", strat_limited,
    base_tokens   = base_tokens,
    target_tokens = target_tokens
  ) |> dplyr::collect() |> data.table::as.data.table()

  expect_lte(nrow(res[source == "base"]), 2)
})

test_that("extract_unmatched() errors on bad inputs in DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "dt",     data.frame(id = c("A", "B"), val = 1:2))
  DBI::dbWriteTable(con, "mt",     data.frame(id = "A"))
  DBI::dbWriteTable(con, "mt_bad", data.frame(wrong_col = "A"))

  expect_error(extract_unmatched(dplyr::tbl(con, "dt"), "missing_col", dplyr::tbl(con, "mt")))
  expect_error(extract_unmatched(dplyr::tbl(con, "dt"), "id", dplyr::tbl(con, "mt_bad")))
})



# ── Regression: small-table prepare_search_data via batch_map ────────────────
# See notes/batch_duckdb_brittleness.md. Prior to the small-fast-path fix,
# these flows aborted with "Batch plan row contains neither windows nor blocks"
# because the planner emitted NA windows that batch_map() could not slice on.

test_that("prepare_search_data() works on a 3-row DuckDB table", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  tiny <- data.frame(
    id   = c("A", "B", "C"),
    name = c("alpha beta", "gamma delta", "alpha beta"),
    stringsAsFactors = FALSE
  )
  duck_tbl <- local_duckdb_table(tiny, table_name = "tiny_prep")
  strat <- search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold = 0.5
  )

  tokens <- prepare_search_data(duck_tbl, "id", strat)
  out <- dplyr::collect(tokens)

  expect_true(all(c("id", "src_column", "token") %in% names(out)))
  expect_gt(nrow(out), 0)
  expect_setequal(unique(out$id), c("A", "B", "C"))
})

test_that("search_candidates() works on small DuckDB tables (token strategy)", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  base <- data.frame(
    id = c("A", "B", "C"),
    name = c("alpha beta", "gamma delta", "alpha beta"),
    stringsAsFactors = FALSE
  )
  target <- data.frame(
    id = c("X", "Y"),
    name = c("alpha beta", "epsilon zeta"),
    stringsAsFactors = FALSE
  )

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "base_tbl",   base)
  DBI::dbWriteTable(con, "target_tbl", target)
  base_duck   <- dplyr::tbl(con, "base_tbl")
  target_duck <- dplyr::tbl(con, "target_tbl")

  strat <- search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold = 0.5
  )

  out <- search_candidates(base_duck, target_duck, "id", "id", strat)
  out_df <- dplyr::collect(out)

  expect_true(all(c("match_id", "score", "source", "id", "rank") %in% names(out_df)))
  # "alpha beta" appears in both, so we expect at least one match pair
  expect_gt(nrow(out_df), 0)
})
