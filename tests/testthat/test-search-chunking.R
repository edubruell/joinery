# Block-atomic scoring chunking for search_candidates (v0.8 Stage 05).
#
# search_candidates() on DuckDB can stream the scoring overlap-join one
# block-atomic chunk at a time, isolating a pathological block from the rest of
# the run, instead of one monolithic join over all blocks. Chunking is driven by
# a duckdb_control() object. These tests cover: the control constructor; the
# atomic planner never splitting a block; chunked == monolithic parity (the
# cross-chunk-pair guard); globally-unique match_id across chunks; the
# chunk_by subset rule + no-block degrade; and per-chunk failure isolation.

library(data.table)

# ---------------------------------------------------------------------------
# 1. duckdb_control() constructor / validation / print
# ---------------------------------------------------------------------------

test_that("duckdb_control validates and echoes its knobs", {
  ctl <- duckdb_control()
  expect_s3_class(ctl, "joinery::Duckdb_Control")
  expect_null(ctl@target_batch_size)
  expect_equal(ctl@chunk_strategy, "block_consolidated")
  expect_equal(ctl@on_error, "skip")

  expect_equal(duckdb_control(chunk_by = "plz2")@chunk_by, "plz2")
  expect_false(duckdb_control(chunk_by = FALSE)@chunk_by)

  expect_error(duckdb_control(on_error = "explode"), "on_error")
  expect_error(duckdb_control(chunk_strategy = "nope"), "chunk_strategy")
  expect_error(duckdb_control(chunk_by = 3L), "chunk_by")
  expect_error(duckdb_control(target_batch_size = -5), "target_batch_size")
  expect_error(duckdb_control(progress = "yes"), "progress")

  txt <- cli::cli_fmt(print(duckdb_control(chunk_by = "xkey", target_batch_size = 1000)))
  txt <- paste(txt, collapse = "\n")
  expect_match(txt, "Duckdb_Control")
  expect_match(txt, "xkey")                          # echoes resolved knobs
})

# ---------------------------------------------------------------------------
# 2. atomic_blocks: never split a block; reject even; require block_by
# ---------------------------------------------------------------------------

test_that("duckdb_batch_plan(atomic_blocks=TRUE) keeps blocks whole", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")
  con <- local_duckdb_con()

  d <- data.frame(id = as.character(1:300),
                  blk = rep(c("A", "B"), c(250L, 50L)))
  DBI::dbWriteTable(con, "d", d)
  tref <- dplyr::tbl(con, "d")

  p <- duckdb_batch_plan(tref, "id", target_batch_size = 100,
                         block_by = "blk", atomic_blocks = TRUE)
  # A (250 rows) exceeds the 100 budget but must NOT be split: one chunk, oversized.
  expect_true("oversized" %in% names(p))
  expect_equal(sum(p$row_count), 300L)
  big <- p[oversized == TRUE]
  expect_equal(nrow(big), 1L)
  expect_equal(big$row_count, 250L)

  # even + atomic is a contradiction (windows split blocks).
  expect_error(
    duckdb_batch_plan(tref, "id", chunk_strategy = "even",
                      block_by = "blk", atomic_blocks = TRUE),
    "even"
  )
  # atomic needs blocks.
  expect_error(
    duckdb_batch_plan(tref, "id", atomic_blocks = TRUE),
    "block_by"
  )
})

test_that("atomic_blocks does not change the non-atomic plan schema", {
  skip_if_not_installed("duckdb")
  con <- local_duckdb_con()
  d <- data.frame(id = as.character(1:120), blk = rep(c("A", "B"), each = 60L))
  DBI::dbWriteTable(con, "d", d)
  p <- duckdb_batch_plan(dplyr::tbl(con, "d"), "id", target_batch_size = 1000,
                         block_by = "blk")          # default atomic_blocks = FALSE
  expect_false("oversized" %in% names(p))           # schema unchanged
})

# ---------------------------------------------------------------------------
# Shared fixture for the search-level tests
# ---------------------------------------------------------------------------

.chunk_fixture <- function(con) {
  base <- data.frame(
    id  = paste0("b", 1:6),
    blk = c("X", "X", "Y", "Y", "Z", "Z"),
    name = c("anna meier", "bert klee", "cara low", "dora fund", "emil rast", "fritz gut")
  )
  tgt <- data.frame(
    id  = paste0("t", 1:6),
    blk = c("X", "X", "Y", "Y", "Z", "Z"),
    name = c("anna meier", "bert klee", "cara low", "dora fund", "emil rast", "xx none")
  )
  DBI::dbWriteTable(con, "base", base)
  DBI::dbWriteTable(con, "tgt", tgt)
  search_strategy(name ~ normalize_text() + word_tokens(),
                  block_by = "blk", threshold = 0.5)
}

# ---------------------------------------------------------------------------
# 3. Chunked == monolithic (the cross-chunk-pair guard)
# ---------------------------------------------------------------------------

test_that("chunked search_candidates is identical to monolithic", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")
  con <- local_duckdb_con()
  s <- .chunk_fixture(con)

  mono <- search_candidates(dplyr::tbl(con, "base"), dplyr::tbl(con, "tgt"),
                            "id", "id", s,
                            control = duckdb_control(chunk_by = FALSE)) |>
    dplyr::collect() |> as.data.table()
  chk <- search_candidates(dplyr::tbl(con, "base"), dplyr::tbl(con, "tgt"),
                           "id", "id", s,
                           control = duckdb_control(chunk_by = "blk",
                                                    target_batch_size = 2)) |>
    dplyr::collect() |> as.data.table()

  m <- mono[, .(source, id, sc = round(score, 8))][order(source, id)]
  c <- chk[,  .(source, id, sc = round(score, 8))][order(source, id)]
  expect_equal(c, m)                                  # no cross-chunk pair dropped
  expect_equal(nrow(chk), nrow(mono))
})

# ---------------------------------------------------------------------------
# 4. match_id is globally unique + stable across chunk boundaries
# ---------------------------------------------------------------------------

test_that("chunked match_id is globally unique and stable across runs", {
  skip_if_not_installed("duckdb")
  con <- local_duckdb_con()
  s <- .chunk_fixture(con)

  run <- function() {
    search_candidates(dplyr::tbl(con, "base"), dplyr::tbl(con, "tgt"),
                      "id", "id", s,
                      control = duckdb_control(chunk_by = "blk",
                                               target_batch_size = 2)) |>
      dplyr::collect() |> as.data.table()
  }
  a <- run(); b <- run()

  # every match_id groups exactly its 2 rows (base + target); no collisions.
  per_id <- a[, .N, by = match_id]
  expect_true(all(per_id$N == 2L))
  expect_equal(uniqueN(a$match_id), nrow(a) / 2L)

  # stable across runs (downstream calibrate_matches keys on match_id).
  setkey(a, source, id); setkey(b, source, id)
  expect_equal(a$match_id, b$match_id)
})

# ---------------------------------------------------------------------------
# 5. chunk_by must be a subset of block_by
# ---------------------------------------------------------------------------

test_that("chunk_by not in block_by aborts before any work runs", {
  skip_if_not_installed("duckdb")
  con <- local_duckdb_con()
  s <- .chunk_fixture(con)
  expect_error(
    search_candidates(dplyr::tbl(con, "base"), dplyr::tbl(con, "tgt"),
                      "id", "id", s,
                      control = duckdb_control(chunk_by = "not_a_block")),
    "subset of the strategy's"
  )
})

# ---------------------------------------------------------------------------
# 6. No block_by: NULL degrades to monolithic, explicit chunk_by aborts
# ---------------------------------------------------------------------------

test_that("no block_by: auto degrades to monolithic, explicit chunk_by aborts", {
  skip_if_not_installed("duckdb")
  con <- local_duckdb_con()
  base <- data.frame(id = paste0("b", 1:4), name = c("ann lee","bo rae","cy fox","dee orr"))
  tgt  <- data.frame(id = paste0("t", 1:4), name = c("ann lee","bo rae","cy fox","zz top"))
  DBI::dbWriteTable(con, "nb_base", base); DBI::dbWriteTable(con, "nb_tgt", tgt)
  s <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.5)

  # auto (NULL) + no block_by -> monolithic, no error.
  out <- search_candidates(dplyr::tbl(con, "nb_base"), dplyr::tbl(con, "nb_tgt"),
                           "id", "id", s, control = duckdb_control()) |>
    dplyr::collect()
  expect_gt(nrow(out), 0L)

  # explicit chunk_by + no block_by -> abort.
  expect_error(
    search_candidates(dplyr::tbl(con, "nb_base"), dplyr::tbl(con, "nb_tgt"),
                      "id", "id", s,
                      control = duckdb_control(chunk_by = "name")),
    "block_by"
  )
})

# ---------------------------------------------------------------------------
# 7. Per-chunk failure isolation
# ---------------------------------------------------------------------------

test_that("a failing chunk is isolated; good blocks survive; recorded in failed_chunks", {
  skip_if_not_installed("duckdb")
  con <- local_duckdb_con()
  s <- .chunk_fixture(con)

  # Force block Y to error inside the slice; the real impl runs for X / Z.
  boom_slice <- function(con, src_tbl, where, name) {
    if (grepl("'Y'", where)) stop("boom on Y")
    src <- src_tbl$lazy_query$x
    DBI::dbExecute(con, paste0("CREATE TEMP TABLE ", name,
                               " AS SELECT * FROM ", src, " WHERE ", where, ";"))
    name
  }

  # The default skip policy must be loud (final summary warning).
  expect_warning(
    res <- testthat::with_mocked_bindings(
      search_candidates(dplyr::tbl(con, "base"), dplyr::tbl(con, "tgt"),
                        "id", "id", s,
                        control = duckdb_control(chunk_by = "blk",
                                                 target_batch_size = 2)),
      .slice_duck_tbl = boom_slice
    ),
    "did not complete"
  )
  out <- res |> dplyr::collect() |> as.data.table()

  # `blk` is an original column carried into the candidate output (both sides
  # share it within a block). Run completed; X and Z pairs intact; no Y pairs.
  expect_true(all(c("X", "Z") %in% out$blk))
  expect_false("Y" %in% out$blk)

  fc <- attr(res, "failed_chunks")
  expect_equal(nrow(fc), 1L)
  expect_true(grepl("blk=Y", fc$chunk_key))
  expect_equal(fc$status, "skipped")
  expect_true(grepl("boom on Y", fc$message))

  # True isolation: the failed chunk leaks no intermediate / slice temps.
  # Only the final result table (master) should remain; no chunk slices,
  # no run_core / prepare intermediates.
  leftover <- DBI::dbGetQuery(con,
    "SELECT table_name FROM duckdb_tables()
       WHERE starts_with(table_name, '_joinery_chunk')
          OR starts_with(table_name, '_joinery_tmp_union')
          OR starts_with(table_name, '_joinery_tmp_matched')
          OR starts_with(table_name, '_joinery_tmp_long')
          OR starts_with(table_name, '_joinery_tmp_base')
          OR starts_with(table_name, '_joinery_tmp_target')
          OR starts_with(table_name, '_joinery_tokens')")$table_name
  expect_equal(length(leftover), 0L)

  # on_error = "stop" re-raises.
  expect_error(
    testthat::with_mocked_bindings(
      search_candidates(dplyr::tbl(con, "base"), dplyr::tbl(con, "tgt"),
                        "id", "id", s,
                        control = duckdb_control(chunk_by = "blk",
                                                 target_batch_size = 2,
                                                 on_error = "stop")),
      .slice_duck_tbl = boom_slice
    ),
    "failed under"
  )
})
