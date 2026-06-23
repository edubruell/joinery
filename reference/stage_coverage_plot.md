# Line plot of cumulative base coverage by stage

Uses percentage coverage when base was supplied to
[`compare_stages()`](https://edubruell.github.io/joinery/reference/compare_stages.md),
raw record counts otherwise.

## Usage

``` r
stage_coverage_plot(x, ...)
```

## Arguments

- x:

  A `Stage_Comparison` object from
  [`compare_stages()`](https://edubruell.github.io/joinery/reference/compare_stages.md).

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

## Value

Invisibly, the plotted `data.table` (marginal_coverage with stage_idx).
