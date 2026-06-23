# Bar chart of median token rarity per column

Bar chart of median token rarity per column

## Usage

``` r
rarity_histogram(x, ...)
```

## Arguments

- x:

  A `Strategy_Audit` object from
  [`audit_strategy()`](https://edubruell.github.io/joinery/reference/audit_strategy.md).

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

## Value

Invisibly, the plotted `data.table` (column_rarity_stats).
