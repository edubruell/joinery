# Bar chart of candidates-per-record distribution (candidates only)

Bar chart of candidates-per-record distribution (candidates only)

## Usage

``` r
ambiguity_plot(x, ...)
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

Invisibly, the plotted `data.table` (ambiguity_dist).
