test_that("duckdb_batch_plan creates unblocked plan with even chunking", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  
  # Use base_example directly (~3300 rows)
  data("base_example")
  DBI::dbWriteTable(con, "test_tbl", base_example, overwrite = TRUE)
  tbl_ref <- dplyr::tbl(con, "test_tbl")
  
  # Plan with target_batch_size = 1000, min_batch_size = 500
  plan <- duckdb_batch_plan(
    tbl_ref,
    id = "id_base",
    target_batch_size = 1000,
    min_batch_size = 500,
    chunk_strategy = "even"
  )
  
  # Assertions
  expect_s3_class(plan, "data.table")
  expect_equal(nrow(plan), 4)  # 3300 / 1000 = 3.3 → 4 batches (ceil)
  expect_named(plan, c("batch_id", "row_count", "row_start", "row_end"))
  expect_equal(plan$batch_id, 1:4)
  expect_equal(sum(plan$row_count), 3300)  # total preserved
  expect_equal(plan$row_start, c(1, 1001, 2001, 3001))
  expect_equal(plan$row_end, c(1000, 2000, 3000, 3300))
})

test_that("duckdb_batch_plan handles small tables below min_batch_size", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  
  data("base_example")
  DBI::dbWriteTable(con, "test_tbl", base_example, overwrite = TRUE)
  tbl_ref <- dplyr::tbl(con, "test_tbl")
  
  # Plan with min_batch_size > total rows (3300)
  plan <- duckdb_batch_plan(
    tbl_ref,
    id = "id",
    target_batch_size = 1000,
    min_batch_size = 5000,  # larger than total
    chunk_strategy = "even"
  )
  
  # Should return single batch
  expect_equal(nrow(plan), 1)
  expect_equal(plan$batch_id, 1)
  expect_equal(plan$row_count, 3300)
  expect_true(is.na(plan$row_start))
  expect_true(is.na(plan$row_end))
})

test_that(".compute_block_stats computes block cardinalities from DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  
  data("base_example")
  DBI::dbWriteTable(con, "test_tbl", base_example, overwrite = TRUE)
  tbl_ref <- dplyr::tbl(con, "test_tbl")
  
  # Compute block stats for single block variable (Kreis)
  stats <- joinery:::.compute_block_stats(tbl_ref, block_by = "Kreis")
  
  expect_s3_class(stats, "data.table")
  expect_named(stats, c("Kreis", "n"))
  expect_equal(nrow(stats), 36)  # 36 unique Kreis values
  expect_equal(sum(stats$n), 3300)  # total rows preserved
  
  # Spot-check a few blocks
  expect_true("Städteregion Aachen" %in% stats$Kreis)
  expect_true(any(stats$n == 539))  # Städteregion Aachen has 539 rows
})
  
test_that("duckdb_batch_plan with block_first strategy creates one batch per block", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  
  data("base_example")
  DBI::dbWriteTable(con, "test_tbl", base_example, overwrite = TRUE)
  tbl_ref <- dplyr::tbl(con, "test_tbl")
  
  # Plan with block_by = "Kreis", block_first strategy
  plan <- duckdb_batch_plan(
    tbl_ref,
    id = "id_base",
    target_batch_size = 1000,
    min_batch_size = 100,
    chunk_strategy = "block_first",
    block_by = "Kreis"
  )
  
  # Assertions
  expect_s3_class(plan, "data.table")
  expect_named(plan, c("batch_id", "row_count", "row_start", "row_end", "Kreis","block_size"))
  
  # Should have 40
  expect_equal(nrow(plan), 36)
  expect_equal(sum(plan$row_count), 3300)
  
  # All row_start and row_end should be non-NA and define valid windows
  expect_true(all(!is.na(plan$row_start)))
  expect_true(all(!is.na(plan$row_end)))
  expect_true(all(plan$row_end >= plan$row_start))
})

test_that("duckdb_batch_plan with block_first strategy sub-chunks large blocks", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  
  data("base_example")
  DBI::dbWriteTable(con, "test_tbl", base_example, overwrite = TRUE)
  tbl_ref <- dplyr::tbl(con, "test_tbl")
  
  # Plan with small target_batch_size to force sub-chunking
  plan <- duckdb_batch_plan(
    tbl_ref,
    id = "id_base",
    target_batch_size = 200,  # Small to force sub-chunking
    min_batch_size = 100,
    chunk_strategy = "block_first",
    block_by = "Kreis"
  )
  
  # Assertions
  expect_s3_class(plan, "data.table")
  expect_named(plan, c("batch_id", "row_count", "row_start", "row_end", "Kreis","block_size"))
  
  # Should have more than 36 batches because large blocks are sub-chunked
  expect_gt(nrow(plan), 36)
  expect_equal(sum(plan$row_count), 3300)
  
  # Find Städteregion Aachen (539 rows) - should be sub-chunked into 3 batches
  aachen_batches <- plan[Kreis == "Städteregion Aachen"]
  expect_equal(nrow(aachen_batches), 3)  # ceil(539 / 200) = 3
  expect_equal(sum(aachen_batches$row_count), 539)
})

test_that("duckdb_batch_plan with multiple block columns", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  
  # Create test data with multiple blocking columns (all combinations)
  test_data <- expand.grid(
    region = c("North", "South"),
    year = c(2020, 2021, 2022, 2023),
    rep = 1:25
  ) |>
    dplyr::mutate(
      id = dplyr::row_number(),
      value = rnorm(dplyr::n())
    ) |>
    dplyr::select(id, region, year, value)
  
  DBI::dbWriteTable(con, "test_tbl", test_data, overwrite = TRUE)
  tbl_ref <- dplyr::tbl(con, "test_tbl")
  
  # Plan with multiple block columns
  plan <- duckdb_batch_plan(
    tbl_ref,
    id = "id",
    target_batch_size = 50,
    min_batch_size = 10,
    chunk_strategy = "block_first",
    block_by = c("region", "year")
  )
  
  # Assertions
  expect_s3_class(plan, "data.table")
  expect_named(plan, c("batch_id", "row_count", "row_start", "row_end", "region", "year","block_size"))
  expect_equal(sum(plan$row_count), 200)
  
  # Should have 8 batches (2 regions × 4 years) since each is 25 rows
  expect_equal(nrow(plan), 8)
})

test_that("duckdb_batch_plan with block_consolidated consolidates small blocks", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  
  # Create synthetic data with known block structure
  test_data <- expand.grid(
    block_id = 1:10,
    rep = 1:30
  ) |>
    dplyr::mutate(
      id = dplyr::row_number(),
      value = rnorm(dplyr::n())
    ) |>
    dplyr::select(id, block_id, value)
  
  DBI::dbWriteTable(con, "test_tbl", test_data, overwrite = TRUE)
  tbl_ref <- dplyr::tbl(con, "test_tbl")
  
  # Each block has 30 rows; consolidate with target = 100 → fits ~3 blocks per batch
  plan <- duckdb_batch_plan(
    tbl_ref,
    id = "id",
    target_batch_size = 100,
    min_batch_size = 50,
    chunk_strategy = "block_consolidated",
    block_by = "block_id"
  )
  
  # Assertions
  expect_s3_class(plan, "data.table")
  expect_equal(sum(plan$row_count), 300)
  
  # Should have ~3-4 batches (10 blocks × 30 rows / 100 per batch ≈ 3-4 batches)
  expect_lte(nrow(plan), 5)
  expect_gte(nrow(plan), 3)
  
  # No batch should exceed target_batch_size by much
  expect_true(all(plan$row_count <= 120))
})

test_that("duckdb_batch_plan block_consolidated vs block_first comparison", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  
  data("base_example")
  DBI::dbWriteTable(con, "test_tbl", base_example, overwrite = TRUE)
  tbl_ref <- dplyr::tbl(con, "test_tbl")
  
  # Both strategies with same parameters
  plan_consolidated <- duckdb_batch_plan(
    tbl_ref,
    id = "id_base",
    target_batch_size = 500,
    min_batch_size = 100,
    chunk_strategy = "block_consolidated",
    block_by = "Kreis"
  )
  
  plan_first <- duckdb_batch_plan(
    tbl_ref,
    id = "id_base",
    target_batch_size = 500,
    min_batch_size = 100,
    chunk_strategy = "block_first",
    block_by = "Kreis"
  )
  
  # Assertions
  expect_equal(sum(plan_consolidated$row_count), 3300)
  expect_equal(sum(plan_first$row_count), 3300)
  
  # block_consolidated should have fewer batches (consolidates small blocks)
  expect_lt(nrow(plan_consolidated), nrow(plan_first))
})

test_that("duckdb_batch_plan validates input arguments", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  
  data("base_example")
  DBI::dbWriteTable(con, "test_tbl", base_example, overwrite = TRUE)
  tbl_ref <- dplyr::tbl(con, "test_tbl")
  
  # Invalid db_tbl (not a dplyr table)
  expect_error(
    duckdb_batch_plan(base_example, id = "id_base"),
    "db_tbl must be a dplyr lazy table"
  )
  
  # Invalid id (not character)
  expect_error(
    duckdb_batch_plan(tbl_ref, id = 123),
    "id must be a character vector"
  )
  
  # Invalid target_batch_size (not positive)
  expect_error(
    duckdb_batch_plan(tbl_ref, id = "id_base", target_batch_size = -1000),
    "target_batch_size must be NULL or positive"
  )
  
  # Invalid min_batch_size (not positive)
  expect_error(
    duckdb_batch_plan(tbl_ref, id = "id_base", min_batch_size = 0),
    "min_batch_size must be NULL or positive"
  )
  
  # Invalid chunk_strategy (not in allowed values)
  expect_error(
    duckdb_batch_plan(tbl_ref, id = "id_base", chunk_strategy = "invalid"),
    "chunk_strategy must be one of 'even', 'block_first', or 'block_consolidated'"
  )
  
  # Invalid block_by (not character or NULL)
  expect_error(
    duckdb_batch_plan(tbl_ref, id = "id_base", block_by = 123),
    "block_by must be NULL or a character vector"
  )
})

test_that("duckdb_batch_plan produces correct row windows for block batches", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  
  # Simple data: 3 blocks with 30 rows each (90 total)
  test_data <- expand.grid(
    block_id = 1:3,
    rep = 1:30
  ) |>
    dplyr::mutate(
      id = dplyr::row_number(),
      value = rnorm(dplyr::n())
    ) |>
    dplyr::select(id, block_id, value)
  
  DBI::dbWriteTable(con, "test_tbl", test_data, overwrite = TRUE)
  tbl_ref <- dplyr::tbl(con, "test_tbl")
  
  # Create plan with block_first
  plan <- duckdb_batch_plan(
    tbl_ref,
    id = "id",
    target_batch_size = 50,  # Each block has 30, fits in one batch
    min_batch_size = 10,
    chunk_strategy = "block_first",
    block_by = "block_id"
  )
  
  # Should have 3 batches (one per block)
  expect_equal(nrow(plan), 3)
  
  # All row_start and row_end should be non-NA
  expect_true(all(!is.na(plan$row_start)))
  expect_true(all(!is.na(plan$row_end)))
  
  # Row windows should be contiguous and non-overlapping
  expect_equal(plan$row_start[1], 1)
  expect_equal(plan$row_end[1], 30)
  expect_equal(plan$row_start[2], 31)
  expect_equal(plan$row_end[2], 60)
  expect_equal(plan$row_start[3], 61)
  expect_equal(plan$row_end[3], 90)
  
  # Each batch row count should match window size
  expect_equal(plan$row_count[1], plan$row_end[1] - plan$row_start[1] + 1)
  expect_equal(plan$row_count[2], plan$row_end[2] - plan$row_start[2] + 1)
  expect_equal(plan$row_count[3], plan$row_end[3] - plan$row_start[3] + 1)
})

