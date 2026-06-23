# Bar chart of vocabulary overlap between base and target per column

Bar chart of vocabulary overlap between base and target per column

## Usage

``` r
vocab_overlap_plot(x, ...)
```

## Arguments

- x:

  A `Strategy_Audit` object from
  [`audit_strategy()`](https://edubruell.github.io/joinery/reference/audit_strategy.md)
  called with `target` supplied.

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

## Value

Invisibly, the plotted `data.table`.
