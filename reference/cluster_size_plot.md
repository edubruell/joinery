# Bar chart of cluster-size distribution (duplicates only)

Bar chart of cluster-size distribution (duplicates only)

## Usage

``` r
cluster_size_plot(x, ...)
```

## Arguments

- x:

  A `Match_Overview` object from
  [`summarise_matches()`](https://edubruell.github.io/joinery/reference/summarise_matches.md)
  with `match_type == "duplicates"`.

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

## Value

Invisibly, the plotted `data.table` (cluster_dist).
