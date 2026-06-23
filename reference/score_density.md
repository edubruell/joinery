# Kernel density of the score distribution

Expands the pre-binned histogram to approximate raw scores before
passing to the density estimator.

## Usage

``` r
score_density(x, threshold = x@score_dist$threshold %||% NA_real_, ...)
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

Invisibly, the `data.table` of expanded scores.
