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
s <- search_strategy(
  Nachname ~ normalize_text() + word_tokens(min_nchar = 3),
  block_by = "Kreis",
  threshold = 0.8
)
dups <- detect_duplicates(base_example, "id_base", s)
summarise_matches(dups, base = base_example)
#> 
#> ── Match_Overview (duplicates) ─────────────────────────────────────────────────
#> n_pairs_or_groups: "642" n_records_involved: "2935"
#> coverage: base=88.9% target=NA
#> score summary
#> min: 1.000
#> q1: 1.000
#> median: 1.000
#> mean: 1.000
#> q3: 1.000
#> max: 1.000
#> cluster size distribution (top 5)
#> size 2: 208 cluster(s)
#> size 3: 147 cluster(s)
#> size 4: 87 cluster(s)
#> size 5: 50 cluster(s)
#> size 6: 32 cluster(s)
```
