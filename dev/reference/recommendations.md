# Recommendations from a Diagnostic Object

Accessor returning the recommendations strings stored on a diagnostic
result object. Returns `character(0)` when no recommendations fired. The
same strings are surfaced inline by the object's
[`print()`](https://rdrr.io/r/base/print.html) method.

Methods for individual classes live alongside those classes - diagnostic
classes (`Match_Overview`, `Strategy_Audit`) in `diagnostic_classes.R`;
calibration classes (`Calibrated_Matches`, `Filter_Calibration`) in
`calibration_classes.R`.

## Usage

``` r
recommendations(x, ...)
```

## Arguments

- x:

  A diagnostic result object (`Strategy_Audit`, `Match_Overview`,
  `Calibrated_Matches`, `Filter_Calibration`).

- ...:

  Reserved for future methods.

## Value

A character vector.
