# DuckDB Execution Control

Build a Duckdb_Control bundling the DuckDB backend's execution knobs,
and pass it as `control =` to
[`prepare_search_data()`](https://edubruell.github.io/joinery/reference/prepare_search_data.md),
[`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md),
or
[`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md)
on DuckDB tables. It controls **how** a match runs (memory, batching,
chunking, failure isolation), never **what** matches - matching
semantics stay on the
[`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md).

Two execution stages, two atomicity rules:

- **Preprocess batching** (tokenization) is per-row, governed by
  `target_batch_size` / `min_batch_size` / `chunk_strategy`. Any row
  split is safe.

- **Scoring chunking** (the overlap join) is *block-atomic* - a pair
  only forms within a block, so a block can never be split. `chunk_by`
  packs *whole* blocks under `target_batch_size`; `on_error` isolates a
  pathological block from the rest of the run.

Chunking is a DuckDB (out-of-core) concern; the in-memory data.table
backend ignores it.

## Usage

``` r
duckdb_control(
  target_batch_size = NULL,
  min_batch_size = NULL,
  chunk_strategy = c("block_consolidated", "block_first", "even"),
  chunk_by = NULL,
  on_error = c("skip", "retry", "stop"),
  progress = NULL
)
```

## Arguments

- target_batch_size:

  `NULL` (auto-tune from RAM / row size) or a positive number of rows
  per batch / scoring chunk.

- min_batch_size:

  `NULL` (auto-tune) or a positive number - the minimum table size
  before preprocess batching engages.

- chunk_strategy:

  Preprocess chunking strategy: `"block_consolidated"` (default),
  `"block_first"`, or `"even"`.

- chunk_by:

  Scoring chunk key. `NULL` (default) auto-derives a coarse key when the
  input is large and leaves small inputs monolithic; `FALSE` forces the
  monolithic path; a character vector names an explicit key, which must
  be a subset of the strategy's `block_by` (else cross-chunk pairs would
  be silently dropped).

- on_error:

  Per-scoring-chunk failure policy: `"skip"` (default - record and
  continue), `"retry"` (re-run once with conservative pragmas, then
  skip), or `"stop"` (re-raise).

- progress:

  `NULL` (auto), `TRUE`, or `FALSE` to force or suppress progress
  output.

## Value

A Duckdb_Control object.

## See also

[`prepare_search_data()`](https://edubruell.github.io/joinery/reference/prepare_search_data.md),
[`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md),
[`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md).

## Examples

``` r
# The control object just bundles execution knobs; it carries no data.
ctrl <- duckdb_control(target_batch_size = 5e5, on_error = "skip")
ctrl
#> <joinery::Duckdb_Control>
#> 
#> target_batch_size: 5e+05
#> min_batch_size: auto
#> chunk_strategy: block_consolidated
#> chunk_by: auto
#> on_error: skip
#> progress: auto

# \donttest{
# Pass it to a verb running on a DuckDB table.
if (requireNamespace("duckdb", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("dplyr", quietly = TRUE)) {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbWriteTable(con, "reg", as.data.frame(workshop_register))
  strat <- search_strategy(
    workshop ~ normalize_text() + word_tokens(min_nchar = 3),
    block_by  = c("postcode_area", "trade"),
    threshold = 0.7
  )
  detect_duplicates(dplyr::tbl(con, "reg"), "reg_no", strat, control = ctrl)
  DBI::dbDisconnect(con, shutdown = TRUE)
}
#> Preparing search token table in batches
#> ℹ Auto-tuned batch sizes: target 500,000, min 500,000
#> 
#> Processing batch 1 of 1 (1052 rows)
#> 
# }
```
