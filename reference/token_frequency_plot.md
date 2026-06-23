# Bar chart of average tokens per record per column

Bar chart of average tokens per record per column

## Usage

``` r
token_frequency_plot(x, ...)
```

## Arguments

- x:

  A `Strategy_Audit` object from
  [`audit_strategy()`](https://edubruell.github.io/joinery/reference/audit_strategy.md).

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

## Value

Invisibly, the plotted `data.table` (column_token_stats).
