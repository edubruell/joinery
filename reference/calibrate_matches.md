# Calibrate matches end-to-end (features -\> filter -\> apply)

High-level Q5 verb. Builds features via
[`match_features()`](https://edubruell.github.io/joinery/reference/match_features.md),
fits a `Filter_Model` via
[`fit_filter()`](https://edubruell.github.io/joinery/reference/fit_filter.md),
and applies it via
[`apply_filter()`](https://edubruell.github.io/joinery/reference/apply_filter.md)
to return a `Calibrated_Matches` object enriched with `tp_prob` /
`predicted_tp`. Dispatches on the strategy class.

## Usage

``` r
calibrate_matches(matches, strategy, ...)
```

## Arguments

- matches:

  Match output table (data.table / tibble / data.frame / DuckDB lazy
  `tbl`).

- strategy:

  The
  [`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md)
  or
  [`embedding_strategy()`](https://edubruell.github.io/joinery/reference/embedding_strategy.md)
  used to produce `matches`.

- ...:

  Method-specific arguments. Required: `labels` (manually labelled rows
  produced by
  [`import_labels()`](https://edubruell.github.io/joinery/reference/import_labels.md)),
  `base`, and `id`. Optional: `target`, `target_id` (forwarded to
  [`match_features()`](https://edubruell.github.io/joinery/reference/match_features.md)),
  `model`, `class_weighted`, `na_fill`, `threshold`, plus all
  [`match_features()`](https://edubruell.github.io/joinery/reference/match_features.md)
  tuning knobs (`top_n`, `include_string_sim`, `include_block_stats`,
  `method`).

## Value

A `Calibrated_Matches` object.

## Examples

``` r
strat <- search_strategy(
  workshop   ~ normalize_text() + word_tokens(min_nchar = 3),
  proprietor ~ normalize_text() + word_tokens(min_nchar = 2),
  block_by  = c("postcode_area", "trade"),
  threshold = 0.30
)
matches <- search_candidates(
  workshop_listings, workshop_register,
  base_id = "listing_id", target_id = "reg_no", strategy = strat
)
# One call: build features, fit the filter, apply it. Uses the shipped
# labelled pairs, which line up with this exact search.
calibrate_matches(matches, strat, labels = match_labels_example,
                  base = workshop_listings, id = "listing_id",
                  target = workshop_register, target_id = "reg_no")
#> 
#> ── Calibrated_Matches ──────────────────────────────────────────────────────────
#> <joinery::Calibrated_Matches>
#> threshold : 0.7237 (method: youden_j)
#> n_rows : 1930
#> predicted_tp == 1: 1232
#> predicted_tp == 0: 698
#> tp_prob quantiles: 0.000 / 0.016 / 0.972 / 0.998 / 1.000
```
