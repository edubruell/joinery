# Cost/recall frontier scatter for a strategy plan

Plots each candidate block key at (candidate, exact_twin_survival) - the
recall axis - with the brute-pair cost in the point labels. The knee is
the cheapest candidate whose twin survival stays high.

## Usage

``` r
frontier_plot(x, ...)
```

## Arguments

- x:

  A `Strategy_Plan` object from
  [`plan_strategy()`](https://edubruell.github.io/joinery/reference/plan_strategy.md).

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

## Value

Invisibly, the plotted `data.table` (frontier).
