# Discover candidate stopwords from a prepared token table

Scores every `(src_column, token)` by its document frequency - the share
of records in that column whose value contains the token - and returns
the tokens common enough to be poor discriminators. These are stopword
candidates: feed them to
[`filter_stopwords()`](https://edubruell.github.io/joinery/reference/filter_stopwords.md)
in the preparer chain and re-run
[`prepare_search_data()`](https://edubruell.github.io/joinery/reference/prepare_search_data.md).

## Usage

``` r
find_stopwords(
  tokens,
  max_prop = 0.3,
  top_n = NULL,
  by_block = FALSE,
  block_by = NULL
)
```

## Arguments

- tokens:

  A token table produced by
  [`prepare_search_data()`](https://edubruell.github.io/joinery/reference/prepare_search_data.md)
  (data.table or DuckDB backend). Must contain `src_column`, `token`,
  and `row_id`.

- max_prop:

  Numeric in `(0, 1]`. Return tokens whose document-frequency share is
  at least this value. Default `0.3` (token appears in 30% or more of a
  column's records).

- top_n:

  Optional integer. If supplied, instead of (or in addition to) the
  `max_prop` cut, keep at most the `top_n` most frequent tokens per
  column. When both are given, the union is returned.

- by_block:

  Logical. Compute the share within each block rather than corpus-wide.
  Requires `block_by` to name the block columns (the token table also
  carries the id column, so they cannot be inferred safely). Default
  `FALSE`.

- block_by:

  Character vector of block columns. Required when `by_block = TRUE`;
  pass the strategy's `block_by`. Ignored otherwise.

## Value

A `data.table` with one row per flagged `(src_column, token)`:
`src_column`, `token`, `df` (distinct records containing the token),
`n_records` (records in the column / block), and
`prop = df / n_records`. Sorted by `src_column` then descending `prop`.
Empty (zero-row) when nothing crosses the threshold.

## Details

Document frequency is computed corpus-wide by default
(`by_block = FALSE`), i.e. across all blocks. This matches the intuition
of a stopword as a globally common term. With `by_block = TRUE` the
share is computed within each block and a token is returned if it
crosses `max_prop` in *any* block, reported at its maximum block-level
share - useful when a token is rare overall but saturates a single dense
block.

## See also

[`filter_stopwords()`](https://edubruell.github.io/joinery/reference/filter_stopwords.md)
to apply the result in a preparer chain.
