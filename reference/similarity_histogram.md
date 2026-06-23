# Histogram of sampled pairwise cosine similarities

Histogram of sampled pairwise cosine similarities

## Usage

``` r
similarity_histogram(x, threshold = attr(x, "threshold"), bins = 30L, ...)
```

## Arguments

- x:

  An `Embedding_Audit` object from
  [`audit_strategy()`](https://edubruell.github.io/joinery/reference/audit_strategy.md).

- threshold:

  Numeric. Draws a dashed vertical line at the strategy threshold
  (default: `attr(x, "threshold")`).

- bins:

  Integer. Number of histogram bins.

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

## Value

Invisibly, the histogram `data.table` with columns `bin_lower`,
`bin_upper`, `bin_mid`, `count`.
