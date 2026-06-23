# Fit a false-positive filter on labelled match pairs

Fit a baseline classifier to predict whether each scored pair is a true
match (`equal == 1L`) or a false positive (`equal == 0L`). The baseline
path uses [`stats::glm`](https://rdrr.io/r/stats/glm.html) with the
logit link and no external dependencies. The features object is the
input from
[`match_features()`](https://edubruell.github.io/joinery/reference/match_features.md);
labels carry the `equal` column produced by
[`import_labels()`](https://edubruell.github.io/joinery/reference/import_labels.md).

## Usage

``` r
fit_filter(
  features,
  labels,
  model = "logistic",
  class_weighted = FALSE,
  na_fill = 0,
  ...
)
```

## Arguments

- features:

  A
  [`Match_Features`](https://edubruell.github.io/joinery/reference/match_features.md)
  object.

- labels:

  A `data.table` / `data.frame` with the matches schema plus an integer
  `equal` column (`0L` / `1L`). Typically produced by
  [`import_labels()`](https://edubruell.github.io/joinery/reference/import_labels.md).

- model:

  Character scalar (default `"logistic"`) selecting the baseline
  [`glm()`](https://rdrr.io/r/stats/glm.html) path. Future M6 work will
  accept a fitted parsnip / workflow object here.

- class_weighted:

  Logical scalar. When `TRUE`, fit `glm` with inverse-class-frequency
  `weights =`, useful for imbalanced training sets. Default `FALSE`.

- na_fill:

  Numeric scalar used to impute predictor NAs. Default `0` (sensible for
  aIP slot columns where NA means "no token").

- ...:

  Reserved for future expansion.

## Value

A `Filter_Model` object.

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
# match_labels_example carries the same pairs with a hand-checked `equal` flag.
model <- fit_filter(feats, match_labels_example)
model
#> <joinery::Filter_Model>
#> backend : glm
#> model_class : glm
#> predictors (42) : score, cnt, icnt, ipos, scnt, rcnt, r1, r2
#> ... +34 more
#> training_n : 442
#> class_balance : 0.756 (share of equal == 1L)
#> class_weighted : FALSE
```
