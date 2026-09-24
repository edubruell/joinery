# Drop all temporary DuckDB tables created by joinery

The DuckDB backend writes ephemeral tables during batch preprocessing
(for example the token tables built by
[`prepare_search_data()`](https://edubruell.github.io/joinery/dev/reference/prepare_search_data.md)).
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
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/Rtmp7Dq2po/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
# }
```
