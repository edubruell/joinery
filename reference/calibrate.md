# Evaluate a fitted filter on labelled pairs

Compute calibration diagnostics for a fitted false-positive filter on a
labelled evaluation set. Returns a `Filter_Calibration` carrying the
reliability table, Brier score, log-loss, per-class confusion matrix,
and a threshold sweep curve.

Two call shapes:

- `calibrate(calibrated_matches, labels)` - evaluate on labels held out
  from the training fit.

- `calibrate(calibrated_matches)` - evaluate on the training labels
  stored on the `Filter_Model` (sanity-check view; do not use for model
  selection).

## Usage

``` r
calibrate(x, labels = NULL, bins = 10L, ...)
```

## Arguments

- x:

  A `Calibrated_Matches` object from
  [`apply_filter()`](https://edubruell.github.io/joinery/reference/apply_filter.md)
  /
  [`calibrate_matches()`](https://edubruell.github.io/joinery/reference/calibrate_matches.md).

- labels:

  Optional labels `data.table` (typically from
  [`import_labels()`](https://edubruell.github.io/joinery/reference/import_labels.md))
  for held-out evaluation.

- bins:

  Integer. Number of equal-width probability bins for the reliability
  table. Default `10`.

- ...:

  Reserved for future expansion.

## Value

A `Filter_Calibration` object.
