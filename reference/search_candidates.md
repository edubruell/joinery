# Search for Candidate Matches Between Tables

Find candidate matches between two tables: for each record on one side,
the records on the other side that share enough rare, informative token
content to score at or above the threshold. This is the cross-table
counterpart of
[`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md).

Pass a
[`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md)
for fuzzy, scored matching, or an
[`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md)
to keep only pairs whose token sets are identical.

## Usage

``` r
search_candidates(base_table, target_table, base_id, target_id, strategy, ...)
```

## Arguments

- base_table:

  A data.frame, tibble, data.table, or backend table.

- target_table:

  The table to search against.

- base_id:

  Character scalar naming the ID column in `base_table`.

- target_id:

  Character scalar naming the ID column in `target_table`.

- strategy:

  A `Search_Strategy` (or `Exact_Strategy`) describing how to tokenize
  each column, how to block, and the matching threshold.

- ...:

  Additional arguments passed to backend-specific methods, such as
  `threshold` and `weights`.

## Value

A table with two rows per matched pair (one for the base record, one for
the target record), sharing a `match_id`:

- match_id:

  Identifier shared by the two rows of a matched pair.

- score:

  The pair's match score.

- source:

  `"base"` or `"target"`.

- id:

  The record ID.

- `<original columns>`:

  Every other column from the source table.

- rank:

  Rank of this candidate among a record's matches.

## See also

[`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md)
for the within-table version,
[`extract_unmatched()`](https://edubruell.github.io/joinery/reference/extract_unmatched.md)
for the residual,
[`multi_stage_search()`](https://edubruell.github.io/joinery/reference/multi_stage_search.md)
for staged passes.

## Examples

``` r
data(base_example)
data(target_example)

strat <- search_strategy(
  Nachname ~ normalize_text() + word_tokens(min_nchar = 3),
  Vorname  ~ normalize_text() + word_tokens(min_nchar = 3),
  Ort      ~ normalize_text(),
  block_by = "Kreis",
  threshold = 0.8
)

matches <- search_candidates(
  base_example, target_example,
  base_id = "id_base", target_id = "id_target",
  strategy = strat
)
head(matches)
#> # A tibble: 6 × 14
#>   id    match_id score source id_base Vorname Nachname Strasse  Hausnummer Ort  
#>   <chr>    <int> <dbl> <chr>  <chr>   <chr>   <chr>    <chr>    <chr>      <chr>
#> 1 B0003        1     1 base   B0003   Peter   Becker   Turmstr… 147        Sind…
#> 2 T0003        1     1 target NA      Peter   Becker   Turmstr… 147        Sind…
#> 3 B0005        2     1 base   B0005   Sarah   Schmidt  Willy-B… 13         Köln 
#> 4 T0005        2     1 target NA      Sarah   Schmidt  Willy-B… 13         Köln 
#> 5 B0010        3     1 base   B0010   Michael Wagner   Dorfstr… 20         Bad …
#> 6 T0010        3     1 target NA      Michael Wagner   Dorfstr… 20         Bad …
#> # ℹ 4 more variables: Kreis <chr>, actual_link <chr>, id_target <chr>,
#> #   rank <int>
```
