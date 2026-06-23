# Compute Embeddings for Records

Compute embedding vectors for records using an `Embedding_Strategy`.
This is a backend-specific generic that handles data retrieval, text
assembly, and embedding computation via tidyllm.

Embedding is the expensive part of a vector match, so each record is
embedded once and the vector is reused on later calls. The data.table
and tibble backends keep a per-session cache keyed by model and record
content; the DuckDB backend reuses through its persisted `embeddings`
column. Reuse is controlled by the `joinery.embedding_reuse` and
`joinery.embedding_cache_dir` options (see `joinery` package options)
and can be cleared with
[`clear_embedding_cache()`](https://edubruell.github.io/joinery/reference/clear_embedding_cache.md).

## Usage

``` r
compute_embeddings(data, id, strategy, ...)
```

## Arguments

- data:

  A data.frame / tibble / data.table (or db table in other backends).

- id:

  Character scalar naming the ID column in `data`.

- strategy:

  An `Embedding_Strategy` object specifying columns, embedding model,
  and normalization settings.

- ...:

  Additional arguments passed to backend-specific methods.

## Value

A backend-specific table with columns: `id` and `embedding` (where
`embedding` contains numeric vectors).

## See also

[`clear_embedding_cache()`](https://edubruell.github.io/joinery/reference/clear_embedding_cache.md)
