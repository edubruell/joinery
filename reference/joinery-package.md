# joinery: Heuristic Index-Based Record Linkage in R

Index-based heuristic record linkage for R.

## Package options

joinery reads a small number of global options. Set them with
[`options()`](https://rdrr.io/r/base/options.html); all have working
defaults, so you only touch them to change behaviour.

Embedding strategies embed each record once and reuse the vector on
later calls, so a multi-stage run does not pay the (expensive) embedding
cost again. Two options control that reuse:

- `joinery.embedding_reuse`:

  Logical, default `TRUE`. When `TRUE`, the data.table and tibble
  backends keep a per-session cache of embedding vectors keyed by model
  and record content, and reuse a vector whenever the same text is
  embedded again. Set to `FALSE` to embed fresh every time, for example
  when benchmarking or using a non-deterministic model. (The DuckDB
  backend reuses through its own persisted `embeddings` column and
  ignores this option.) The session cache grows as you embed more
  distinct records and is only released at the end of the session or by
  [`clear_embedding_cache()`](https://edubruell.github.io/joinery/reference/clear_embedding_cache.md);
  call that to reclaim memory in a long-running session.

- `joinery.embedding_cache_dir`:

  Character path, default unset (`NULL`). When set, the embedding cache
  also writes each vector to this directory, so reuse survives across R
  sessions. When unset, the cache lives only in the current session. The
  cache is keyed by record content, so a changed record re-embeds on its
  own; clear stale files with
  [`clear_embedding_cache()`](https://edubruell.github.io/joinery/reference/clear_embedding_cache.md)
  (`disk = TRUE`).

## See also

[`clear_embedding_cache()`](https://edubruell.github.io/joinery/reference/clear_embedding_cache.md),
[`embedding_strategy()`](https://edubruell.github.io/joinery/reference/embedding_strategy.md)

## Author

**Maintainer**: Eduard Brüll <eduard.bruell@zew.de>
