# Grouped bar chart of score distributions by stage

Grouped bar chart of score distributions by stage

## Usage

``` r
stage_score_plot(x, ...)
```

## Arguments

- x:

  A `Stage_Comparison` object from
  [`compare_stages()`](https://edubruell.github.io/joinery/reference/compare_stages.md).

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

## Value

Invisibly, the plotted `data.table` (score_dist_by_stage with bin_mid).
