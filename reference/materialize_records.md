# Materialize Records by ID

Rehydrate a set of record IDs back into their **full records**. The
positive (semi-join) complement of
[`extract_unmatched()`](https://edubruell.github.io/joinery/reference/extract_unmatched.md):
where
[`extract_unmatched()`](https://edubruell.github.io/joinery/reference/extract_unmatched.md)
*produces* a residual set of IDs, `materialize_records()` pulls those
IDs back into complete, scorable rows for the next stage.

## Usage

``` r
materialize_records(data, id, ids, ...)
```

## Arguments

- data:

  A data.frame / tibble / data.table (or db table in other backends) -
  the corpus to pull records from.

- id:

  Character scalar naming the ID column in `data`.

- ids:

  Either an atomic vector of ID values, or a table carrying them (read
  from an `id` column, else a column named `id`'s value).

- ...:

  Additional arguments passed to backend-specific methods.

## Value

The rows of `data` whose ID is in `ids`, all columns intact, one row per
matching record, in no guaranteed order.

## Details

`ids` is **polymorphic**. It may be either

- an atomic vector of ID values, or

- a table (data.frame / data.table / backend tbl) carrying the IDs. The
  lookup order for the ID column is: a column literally named `id` first
  (the
  [`extract_unmatched()`](https://edubruell.github.io/joinery/reference/extract_unmatched.md)
  /
  [`resolve_entities()`](https://edubruell.github.io/joinery/reference/resolve_entities.md)
  output convention), otherwise a column named the same as `id`.

The return is a **semi-join**: IDs absent from `data` are silently
dropped (there is nothing to rehydrate), never NULL-filled. IDs are
coerced to a common type on both sides, so a BIGINT-corpus /
character-id request still matches. Row order is not guaranteed; the
caller sorts if needed.

On the DuckDB backend the IDs are **always** registered as a temp table
and joined - never inlined as an `id IN (<literal list>)`, which binds
in roughly O(n^2) and pins cores for minutes on large residual sets.

## See also

[`extract_unmatched()`](https://edubruell.github.io/joinery/reference/extract_unmatched.md),
the negative complement that produces the residual IDs this verb
rehydrates.
