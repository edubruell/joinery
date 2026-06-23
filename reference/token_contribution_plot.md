# Horizontal bar chart of per-token score contributions, coloured by column

Horizontal bar chart of per-token score contributions, coloured by
column

## Usage

``` r
token_contribution_plot(x, ...)
```

## Arguments

- x:

  A `Match_Explanation` object from
  [`explain_match()`](https://edubruell.github.io/joinery/reference/explain_match.md).

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

## Value

Invisibly, the plotted `data.table` (shared_tokens with token_label).
