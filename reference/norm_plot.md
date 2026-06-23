# Bar chart of embedding norm quantiles

Plots p05/p25/p50/p75/p95 of the embedding vector norms. A norm of 1 is
annotated; for an L2-normalised strategy all bars should sit on it.

## Usage

``` r
norm_plot(x, ...)
```

## Arguments

- x:

  An `Embedding_Audit` object from
  [`audit_strategy()`](https://edubruell.github.io/joinery/reference/audit_strategy.md).

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

## Value

Invisibly, the plotted `data.table` (quantile, norm).
