# Bar chart of the pre-binned score distribution

Bar chart of the pre-binned score distribution

## Usage

``` r
score_histogram(x, threshold = x@score_dist$threshold %||% NA_real_, ...)
```

## Arguments

- x:

  A `Match_Overview` object from
  [`summarise_matches()`](https://edubruell.github.io/joinery/reference/summarise_matches.md).

- threshold:

  Numeric. Draws a dashed vertical line. Defaults to the threshold
  stored in `x@score_dist$threshold` when available.

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

## Value

Invisibly, the plotted `data.table` (histogram with bin_mid column).
