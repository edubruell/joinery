# Detect Duplicate Records

Find likely duplicate records inside a single table and group them.
Records are compared by how much of their rare, informative token
content they share (not by character-level edit distance), every pair is
scored, and any pair scoring at or above the threshold is linked.
Records that link directly or transitively form one duplicate group.

Pass a
[`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md)
for fuzzy, scored matching, or an
[`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md)
to group only records whose token sets are identical.

## Usage

``` r
detect_duplicates(base_table, id, strategy, ...)
```

## Arguments

- base_table:

  A data.frame, tibble, data.table, or backend table to deduplicate.

- id:

  Character scalar naming the ID column in `base_table`.

- strategy:

  A `Search_Strategy` (or `Exact_Strategy`) describing how to tokenize
  each column, how to block, and the matching threshold.

- ...:

  Additional arguments passed to backend-specific methods. The most
  useful are `threshold` (override the strategy's threshold) and
  `weights` (a named numeric vector overriding the strategy's column
  weights).

## Value

A table with one row per record that belongs to a duplicate group:

- duplicate_group:

  Group label shared by all records that are duplicates of one another.

- id:

  The record ID.

- score:

  The record's match score within its group.

- rank:

  Rank within the group; rank 1 is the representative kept by
  [`deduplicate_table()`](https://edubruell.github.io/joinery/reference/deduplicate_table.md).

- `<original columns>`:

  Every other column from `base_table`.

## See also

[`deduplicate_table()`](https://edubruell.github.io/joinery/reference/deduplicate_table.md)
to collapse the groups,
[`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md)
for the cross-table version,
[`multi_stage_dedup()`](https://edubruell.github.io/joinery/reference/multi_stage_dedup.md)
for staged passes.

## Examples

``` r
data(base_example)

strat <- search_strategy(
  Nachname ~ normalize_text() + word_tokens(min_nchar = 3),
  Vorname  ~ normalize_text() + word_tokens(min_nchar = 3),
  Ort      ~ normalize_text(),
  block_by = "Kreis",
  threshold = 0.8
)

dups <- detect_duplicates(base_example, id = "id_base", strategy = strat)
head(dups)
#> # A tibble: 6 × 10
#>   id    duplicate_group score  rank Vorname Nachname Strasse    Hausnummer Ort  
#>   <chr>           <int> <dbl> <int> <chr>   <chr>    <chr>      <chr>      <chr>
#> 1 B0008               8     1     1 Lukas   Lehmann  Sandweg    141        Stol…
#> 2 B1926               8     1     2 Lukas   Lehmann  Schmiedes… 18         Stol…
#> 3 B0018              18     1     1 Uwe     Klein    Willy-Bra… 137        Baes…
#> 4 B3149              18     1     2 Uwe     Klein    Willy-Bra… 137A       Baes…
#> 5 B0022              22     1     1 Helmut  Meyer    Hauptstra… 111        Frei…
#> 6 B3187              22     1     2 Helmut  Meyer    Hauptstra… 111B       Frei…
#> # ℹ 1 more variable: Kreis <chr>
```
