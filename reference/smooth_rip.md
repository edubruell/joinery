# Configure rIP smoothing for a search strategy

Helper functions that construct S7 `Smoothing` objects used by
[`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md)
to control how relative identification potential (rIP) is smoothed
before scoring.

All helpers are pure configuration; they do not perform any computation
by themselves. Backend methods for
[`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md)
and
[`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md)
interpret the resulting `Smoothing` object.

## Usage

``` r
smooth_rip_identity()

smooth_rip_log()

smooth_rip_offset(alpha = 0.5)

smooth_rip_softmax(temperature = 1)
```

## Arguments

- alpha:

  Numeric scalar; offset that is added to rIP values prior to
  normalization. Must be non negative.

- temperature:

  Numeric scalar; softmax temperature parameter. Must be strictly
  positive.

## Value

An object inheriting from `Smoothing` that can be passed to the
`smoothing` argument of
[`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md).

## Details

rIP Smoothing Helpers

## Functions

- `smooth_rip_identity()`: Identity rIP smoothing (no transformation
  beyond standard per record normalization). This is the default.

- `smooth_rip_log()`: Logarithmic rIP smoothing. Backends typically
  apply `log1p(rIP)` and then renormalize within each record and column.

- `smooth_rip_offset()`: Offset based rIP smoothing with a constant
  offset `alpha` that is added to all rIP values before renormalization.

- `smooth_rip_softmax()`: Softmax style rIP smoothing with a temperature
  parameter that controls how sharp or flat the transformed distribution
  is.

## See also

[`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md)
