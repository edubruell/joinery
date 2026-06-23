# Prepare Data for Record Linkage Search

Turn a table into the long-format token table the matching verbs work
on: it applies each column's preparation steps, splits the text into
tokens, and attaches the id and any blocking columns. The other verbs
([`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md),
[`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md))
call this for you, so you rarely need it directly; reach for it when you
want to see or post-process the tokens yourself.

## Usage

``` r
prepare_search_data(data, id, strategy, ...)
```

## Arguments

- data:

  A data.frame / tibble / data.table (or db table in other backends).

- id:

  Character scalar naming the ID column in `data`.

- strategy:

  A `Search_Strategy` object.

- ...:

  Additional arguments passed to backend-specific methods.

## Value

A long-format token table with one row per token, carrying the id, the
source `column`, the `token`, a `row_id`, and any blocking columns.

## See also

[`inspect_tokens()`](https://edubruell.github.io/joinery/reference/inspect_tokens.md)
for a quick per-column look at the tokens.
