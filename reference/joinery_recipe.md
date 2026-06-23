# Build a tidymodels recipe for calibration features

Construct a pre-configured
[`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
suitable for fitting a false-positive filter on the output of
[`match_features()`](https://edubruell.github.io/joinery/reference/match_features.md).
Tags ID columns (`searched`, `found`, `match_id`) with role `"id"`, sets
`equal` as the outcome, and keeps every other numeric column as a
predictor. Requires the suggested `recipes` package.

## Usage

``` r
joinery_recipe(features, labels, ...)
```

## Arguments

- features:

  A
  [`Match_Features`](https://edubruell.github.io/joinery/reference/match_features.md)
  object.

- labels:

  A labels `data.table` with `equal` (as for
  [`fit_filter()`](https://edubruell.github.io/joinery/reference/fit_filter.md)).

- ...:

  Reserved for future expansion.

## Value

A
[`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
object.
