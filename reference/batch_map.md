# Apply a function to DuckDB table batches

Streams a DuckDB table through a batch plan and applies a user-defined
function to each batch. The function must accept a data.frame and return
a data.frame. Results can be collected in memory or written back to
DuckDB incrementally.

## Usage

``` r
batch_map(plan, con, input_table, fn, persist = TRUE, output_table = NULL)
```

## Arguments

- plan:

  A batch plan produced by
  [`duckdb_batch_plan()`](https://edubruell.github.io/joinery/reference/duckdb_batch_plan.md).
  Must include columns `batch_id` and `row_count`, plus either
  row-number windows (`row_start`, `row_end`), block identifier columns,
  or a `blocks` list-column for consolidated batches.

- con:

  A DuckDB connection.

- input_table:

  Character. Name of the source table in DuckDB.

- fn:

  A function applied to each batch. Receives a data.frame and must
  return a data.frame.

- persist:

  Logical. If `TRUE`, results of each batch are appended to
  `output_table` inside DuckDB and a lazy table reference is returned.
  If `FALSE`, returns a list of batch results as data.frames.

- output_table:

  Optional DuckDB table name where results are stored when
  `persist = TRUE`. If omitted, a random temporary table name is
  generated. Ignored when `persist = FALSE`.

## Value

- If `persist = TRUE`: A `tbl_duckdb_connection` pointing to the output
  table.

- If `persist = FALSE`: A list of data.frames, one per batch.

## Details

Database work is performed batch-by-batch, allowing preprocessing of
tables that exceed available RAM. For each batch, a SQL slice or block
filter is executed, the function is applied, and (optionally) results
are appended to a DuckDB table.
