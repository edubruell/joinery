# Horizontal bar chart of per-column score contributions

Horizontal bar chart of per-column score contributions

## Usage

``` r
contribution_plot(x, ...)
```

## Arguments

- x:

  A `Match_Explanation` object from
  [`explain_match()`](https://edubruell.github.io/joinery/reference/explain_match.md).

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

## Value

Invisibly, the plotted `data.table` (per_column_contrib).
