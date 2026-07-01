# Create a Batch Plan for DuckDB Table Processing

Analyses a DuckDB table and generates a batch plan (data.table) that
defines how to split the table into atomic processing units. Each row of
the plan represents one batch with row counts, optional row-number
windows, and block identifiers (if blocking is used).

## Usage

``` r
duckdb_batch_plan(
  db_tbl,
  id,
  target_batch_size = NULL,
  min_batch_size = NULL,
  chunk_strategy = "block_consolidated",
  block_by = NULL,
  atomic_blocks = FALSE
)
```

## Arguments

- db_tbl:

  A DuckDB table reference (result of `dplyr::tbl(con, "table_name")`)

- id:

  Character. Column name(s) to use as record identifier(s). Not used for
  batching but validated to exist in the table.

- target_batch_size:

  Positive integer. Target number of rows per batch. Default: 1e6 (1
  million rows).

- min_batch_size:

  Positive integer. Minimum table size to trigger batching. If total
  rows \< min_batch_size, returns single batch. Default: 1e5 (100k
  rows).

- chunk_strategy:

  Character. One of `"even"`, `"block_first"`, or
  `"block_consolidated"`. Default: `"block_consolidated"`.

- block_by:

  Optional character vector. Column name(s) to use for semantic
  blocking. If specified, batches respect block boundaries. Supports
  multiple columns (e.g., c("region", "year")).

- atomic_blocks:

  Logical. When `FALSE` (default) the planner may sub-split a block
  larger than `target_batch_size` into row-number windows - correct for
  *preprocess* batching, where token generation is per-row independent.
  When `TRUE` (the *scoring* path) a block is treated as
  **indivisible**: small blocks are consolidated under the budget but a
  large block is kept whole as a single chunk (flagged
  `oversized = TRUE`), never sub-split - because a match pair only forms
  *within* a block, so splitting one would silently drop cross-pairs.
  Requires `block_by`; rejects `chunk_strategy = "even"`.

## Value

A `data.table` with columns:

- `batch_id`: integer, sequential batch identifier (1, 2, 3, ...)

- `row_count`: integer, number of rows in this batch

- `row_start`: integer (or NA), window start for row-number-based
  batches; NA for block-based

- `row_end`: integer (or NA), window end for row-number-based batches;
  NA for block-based

- Additional columns (if `block_by` specified): one per blocking
  variable, containing block values

## Details

The function supports three chunking strategies:

- `"even"`: Simple row-number chunking, ignores blocks

- `"block_first"`: Each batch = one block (or sub-chunks if block \>
  target_batch_size)

- `"block_consolidated"`: Consolidates small blocks to minimize batch
  count (default)

**Small tables**: If total rows \< `min_batch_size`, returns a single
batch regardless of strategy. With blocking, still respects blocks.

**Row-number windows**: For unblocked or large-block sub-chunking,
`row_start` and `row_end` define window boundaries (1-based, inclusive).
For block-based batches (small blocks), these are NA.

**Consolidation**: `"block_consolidated"` (default) combines multiple
small blocks into single batches up to `target_batch_size` to reduce
overhead. Each batch may contain zero, one, or multiple blocks
(depending on sizes and consolidation).

**Row ordering**: To ensure `row_start` and `row_end` windows are
consecutive and can be reliably sliced from the DB, the function sorts
by the `id` column before computing row numbers. This ensures
reproducible, deterministic batch boundaries.

## Examples

``` r
# \donttest{
if (requireNamespace("duckdb", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("dplyr", quietly = TRUE)) {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbWriteTable(
    con, "data",
    data.frame(id = 1:1000, region = rep(LETTERS[1:5], length.out = 1000))
  )
  tbl_ref <- dplyr::tbl(con, "data")

  # Unblocked, even row-number chunking
  plan1 <- duckdb_batch_plan(
    tbl_ref, id = "id",
    target_batch_size = 200, chunk_strategy = "even"
  )

  # Blocked, consolidated strategy (default, respects regions)
  plan2 <- duckdb_batch_plan(
    tbl_ref, id = "id",
    target_batch_size = 200, block_by = "region"
  )
  DBI::dbDisconnect(con, shutdown = TRUE)
}
#> ℹ Auto-tuned batch sizes: target 200, min 500,000
#> 
#> ℹ Auto-tuned batch sizes: target 200, min 500,000
#> 
# }
```
