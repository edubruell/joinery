# Import a labelled CSV back into a feature/label table

Read a CSV written by
[`export_for_labelling()`](https://edubruell.github.io/joinery/reference/export_for_labelling.md)
(optionally edited by a user), propagate the block-default `equal` value
from each header row onto unmarked rows in that block, validate the
schema, and return a `data.table` ready for
[`fit_filter()`](https://edubruell.github.io/joinery/reference/fit_filter.md)
/
[`calibrate_matches()`](https://edubruell.github.io/joinery/reference/calibrate_matches.md).

## Usage

``` r
import_labels(file)
```

## Arguments

- file:

  Path to the CSV file to read.

## Value

A `data.table` with the same rows as the original sample plus a fully
populated `equal` column (`0L` / `1L`).

## See also

[`export_for_labelling()`](https://edubruell.github.io/joinery/reference/export_for_labelling.md)
