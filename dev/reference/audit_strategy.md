# Audit a Search Strategy Against Data

Pre-match diagnostic (Q1). Runs preparation and rarity computation,
reports per-column token / rarity statistics and (when `block_by` is
set) block-size distribution and estimated comparison count. Surfaces
recommendations linking pre-match symptoms to strategy levers.

## Usage

``` r
audit_strategy(data, id, strategy, ...)
```

## Arguments

- data:

  A data.frame / tibble / data.table (or backend-specific table).

- id:

  Character scalar naming the ID column in `data`.

- strategy:

  A `Search_Strategy` object.

- ...:

  Additional backend-specific arguments. Notably: `target` (optional
  second table for cross-table vocabulary overlap) and `sample_n`
  (optional integer; if set, audit a random sample of rows).

## Value

A `Strategy_Audit` object.

## Examples

``` r
strat <- search_strategy(
  workshop ~ normalize_text() + word_tokens(min_nchar = 3),
  block_by  = c("postcode_area", "trade"),
  threshold = 0.7
)
audit_strategy(workshop_register, "reg_no", strat)
#> 
#> ── Strategy_Audit ──────────────────────────────────────────────────────────────
#> n_records: 1052
#> column token stats
#> workshop: 3365 tokens, 913 unique (27.1%), na_rate=0.0%
#> column rarity quantiles
#> workshop: p50=1.0000, pct_low_rarity=0.0%
#> blocks: 255 blocks, top1_share="1.0%"
#> est_comparisons: "2290"
```
