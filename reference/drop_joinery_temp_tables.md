# Drop all temporary DuckDB tables created by joinery

The DuckDB backend writes ephemeral tables during batch preprocessing
(for example the token tables built by
[`prepare_search_data()`](https://edubruell.github.io/joinery/reference/prepare_search_data.md)).
A clean run drops them when it finishes, but a run that is killed
partway, or a machine that loses power mid-job, can leave them behind on
disk. This sweeps them up.

## Usage

``` r
drop_joinery_temp_tables(
  con,
  prefixes = c("_joinery_tokens_", "_joinery_tmp_", "_joinery_emb_")
)
```

## Arguments

- con:

  A DuckDB connection.

- prefixes:

  Character vector of table name prefixes that identify joinery
  temporary tables. Defaults cover all current ephemeral table types.

## Value

A character vector of removed table names, invisibly.

## Details

Each temporary table carries a reserved name prefix such as
`"_joinery_tokens_"` or `"_joinery_tmp_"`. Only tables whose names begin
with one of those prefixes are removed, so your own tables are never
touched. Pass extra `prefixes` to cover temporary table types added in
future.

## Examples

``` r
# \donttest{
if (requireNamespace("duckdb", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE)) {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  # A stray joinery temp table left behind by an interrupted run:
  DBI::dbWriteTable(con, "_joinery_tmp_demo", data.frame(x = 1))

  drop_joinery_temp_tables(con)  # removes it, returns its name invisibly
  DBI::dbDisconnect(con, shutdown = TRUE)
}
#> duckdb: caching downloaded extensions in the package library:
#> ℹ /home/runner/work/_temp/Library/duckdb/extensions
#> ℹ This is removed when the package is re-installed; see `?duckdb_storage` to choose a different location.
# }
```
