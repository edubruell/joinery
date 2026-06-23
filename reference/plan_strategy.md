# Plan a Search Strategy from Raw Inputs

Helps you choose a blocking before you run anything. Where
[`audit_strategy()`](https://edubruell.github.io/joinery/reference/audit_strategy.md)
grades a strategy you have already settled on, and
[`rarity_distribution()`](https://edubruell.github.io/joinery/reference/rarity_distribution.md)
reads one column's token distribution, `plan_strategy()` compares
several candidate blockings side by side and shows the trade-off between
how many comparisons each one costs and how many true matches it would
keep together.

It never builds the pair set, so it is safe to run on a full corpus. For
each candidate blocking it reports: how many blocks it makes and how big
they are, an estimate of how many record comparisons it implies, and the
share of identical-token records that stay in the same block (the recall
it would cost you). It also reports how much an
[`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md)
front stage would absorb, the shape of the leftover records, and how
discriminative each column is, including a warning when a column that is
often empty puts a ceiling on achievable scores.

The strategy you pass supplies only the column preparation steps; its
own `block_by` is ignored, since the blocking is exactly what you are
choosing here.

## Usage

``` r
plan_strategy(
  base,
  strategy,
  target = NULL,
  block_candidates = list(),
  base_id = NULL,
  target_id = NULL,
  n_offenders = 20L,
  min_rarity_grid = NULL,
  containment = FALSE,
  ...
)
```

## Arguments

- base:

  A data.frame / tibble / data.table (or backend table).

- strategy:

  A `Search_Strategy` supplying the tokenization to plan against.

- target:

  Optional second table. `NULL` (default) plans a dedup; non-`NULL`
  plans a cross-table search.

- block_candidates:

  Named list of candidate `block_by` specs to compare (e.g.
  `list(plz2 = "plz2", plz5_wz = c("plz5", "wz08_3"))`).

- base_id:

  Character scalar naming the id column in `base` (required).

- target_id:

  Character scalar naming the id column in `target` (defaults to
  `base_id`).

- n_offenders:

  Number of top-`df` "offender" tokens (the fan-out drivers) to report
  per column. Defaults to `20`.

- min_rarity_grid:

  Optional numeric vector of `min_rarity` cut points for the cost curve.
  `NULL` (default) picks a grid from the rarity distribution.

- containment:

  Logical. When `TRUE`, adds the per-column containment share, the one
  read that performs a bounded structural join. Defaults to `FALSE`,
  which keeps `plan_strategy()` scoring-free.

- ...:

  Backend-specific arguments, such as `sample_n` (DuckDB).

## Value

A `Strategy_Plan` object.

## See also

[`audit_strategy()`](https://edubruell.github.io/joinery/reference/audit_strategy.md)
to grade a chosen strategy,
[`rarity_distribution()`](https://edubruell.github.io/joinery/reference/rarity_distribution.md)
for one column's distribution,
[`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md)
for the front stage it sizes.

## Examples

``` r
strat <- search_strategy(
  workshop ~ normalize_text() + word_tokens(min_nchar = 3)
)
# Compare two candidate blockings side by side before committing to one.
plan_strategy(
  workshop_register, strat,
  block_candidates = list(area       = "postcode_area",
                          area_trade = c("postcode_area", "trade")),
  base_id = "reg_no"
)
#> 
#> ── Strategy_Plan (dedup) ───────────────────────────────────────────────────────
#> blocking frontier (by brute_pairs)
#> area_trade: 255 blocks, brute_pairs=2290, twin_survival=26.2%
#> area: 33 blocks, brute_pairs=17293, twin_survival=26.2%
#> persister rate (overall): "23.2%"
#> residual matchable: "100.0%"
#> ! block key 'area_trade' keeps 26.2% of exact twins while cutting brute pairs by 87%; prefer this coarser block.
```
