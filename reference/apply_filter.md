# Apply a fitted filter to match features

Score a `Match_Features` table with a fitted `Filter_Model` and return a
`Calibrated_Matches` object. When `matches` is supplied, the original
match table is enriched with `tp_prob` and `predicted_tp` columns and
stored in the result's `@matches` slot; when `matches` is `NULL`, the
features table itself is enriched and stored.

## Usage

``` r
apply_filter(
  features,
  filter_model,
  threshold = NULL,
  threshold_rule = c("youden", "target_recall", "cost_weighted"),
  target_recall = 0.95,
  cost_ratio = 1,
  matches = NULL,
  ...
)
```

## Arguments

- features:

  A `Match_Features` object.

- filter_model:

  A `Filter_Model` produced by
  [`fit_filter()`](https://edubruell.github.io/joinery/reference/fit_filter.md).

- threshold:

  Numeric scalar in (0, 1) or `NULL`. When non-`NULL` it is used
  verbatim and overrides `threshold_rule`. When `NULL`, the threshold is
  chosen on the training labels per `threshold_rule`. Decision 13.7
  default.

- threshold_rule:

  The operating-point rule used when `threshold` is `NULL`: `"youden"`
  (default, maximise Youden's J, symmetric error costs),
  `"target_recall"` (the highest threshold still achieving
  `target_recall`), or `"cost_weighted"` (minimise
  `cost_ratio * FN + FP`). For a firm panel the recall-favouring rules
  are usually the right operating point, splitting one business across
  years is worse than admitting a few co-located firms a later collapse
  can still catch.

- target_recall:

  Target recall in (0, 1\] for `threshold_rule = "target_recall"`.
  Default `0.95`.

- cost_ratio:

  `cost(FN) / cost(FP)` for `threshold_rule = "cost_weighted"`; `> 1`
  favours recall. Default `1` (symmetric).

- matches:

  Optional raw matches table to enrich. When supplied, `tp_prob` /
  `predicted_tp` are broadcast onto every row of the pair (candidates:
  both `source == "base"` and `source == "target"` rows of a `match_id`;
  duplicates: every row of a `duplicate_group`).

- ...:

  Reserved for future expansion.

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
feats <- match_features(matches, strat,
                        base = workshop_listings, id = "listing_id",
                        target = workshop_register, target_id = "reg_no")
model <- fit_filter(feats, match_labels_example)
# Broadcast the true-positive probability back onto the match rows.
apply_filter(feats, model, matches = matches)
#> 
#> ── Calibrated_Matches ──────────────────────────────────────────────────────────
#> <joinery::Calibrated_Matches>
#> threshold : 0.7237 (method: youden_j)
#> n_rows : 1930
#> predicted_tp == 1: 1232
#> predicted_tp == 0: 698
#> tp_prob quantiles: 0.000 / 0.016 / 0.972 / 0.998 / 1.000
```
