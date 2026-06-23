# Bar chart of top-1 vs top-2 score gap distribution (candidates only)

Bar chart of top-1 vs top-2 score gap distribution (candidates only)

## Usage

``` r
top_gap_density(x, ...)
```

## Arguments

- x:

  A `Match_Overview` object from
  [`summarise_matches()`](https://edubruell.github.io/joinery/reference/summarise_matches.md)
  with `match_type == "candidates"`.

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

## Value

Invisibly, the plotted `data.table` (top_gap_dist with bin_mid).
