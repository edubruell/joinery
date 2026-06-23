# Bar chart of block sizes (requires block_by on strategy)

Bar chart of block sizes (requires block_by on strategy)

## Usage

``` r
block_size_plot(x, ...)
```

## Arguments

- x:

  A `Strategy_Audit` object from
  [`audit_strategy()`](https://edubruell.github.io/joinery/reference/audit_strategy.md).

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

## Value

Invisibly, the plotted `data.table` (block_summary\$distribution).
