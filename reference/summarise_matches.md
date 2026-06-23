# Summarise a Match Result

Post-match overview (Q2). Auto-detects whether the input is a duplicate
table (presence of `duplicate_group` column) or a candidate table
(presence of `match_id` and `source` columns), and reports score
distribution, coverage (when `base` / `target` are supplied),
cluster-size or candidates-per-record distribution, and top-1-vs-top-2
score-gap distribution for candidates. Recommendations link symptoms to
strategy levers.

## Usage

``` r
summarise_matches(matches, ...)
```

## Arguments

- matches:

  Match output table from
  [`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md)
  or
  [`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md).

- ...:

  Method-specific arguments. The data.table method accepts: `base`
  (optional base input table for coverage), `target` (optional target
  input table for candidate coverage), and `bins` (integer number of
  histogram bins for the score distribution; default `50`).

## Value

A `Match_Overview` object.

## Examples

``` r
if (FALSE) { # \dontrun{
s <- search_strategy(
  name ~ normalize_text() + word_tokens(),
  threshold = 0.9
)
dups <- detect_duplicates(base_example, "id", s)
summarise_matches(dups, base = base_example)
} # }
```
