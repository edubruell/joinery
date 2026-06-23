# Clear the embedding reuse cache

Empties joinery's in-session embedding cache, and optionally the on-disk
cache. The cache stores raw embedding vectors so that the data.table and
tibble backends reuse them instead of re-embedding on every call. You
rarely need to call this by hand; it is mainly useful to force a clean
re-embed or to reclaim memory in a long-running session.

## Usage

``` r
clear_embedding_cache(disk = FALSE)
```

## Arguments

- disk:

  Logical. If `TRUE`, also delete the on-disk cache files in the
  directory set by `options(joinery.embedding_cache_dir = ...)`.
  Defaults to `FALSE` (clear the in-session cache only).

## Value

Invisibly `NULL`.

## See also

[`compute_embeddings()`](https://edubruell.github.io/joinery/reference/compute_embeddings.md)
for how the cache is filled, and `joinery` (package options) for
`joinery.embedding_reuse` and `joinery.embedding_cache_dir`.
