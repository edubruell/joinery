# Export a match sample to CSV for manual labelling

Write a sampled set of matches to a CSV pre-filled with an `equal`
column on block-header rows. Users edit the CSV in any spreadsheet,
marking only exceptions (e.g. false positives) and leaving the rest as
defaults.

Block definition follows the matches schema: for candidate matches (from
[`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md)),
the header is the base-side row and candidate rows inherit its default.
For duplicate matches (from
[`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md)),
the header is the rank-1 row and the remaining records in the duplicate
group inherit its default.

## Usage

``` r
export_for_labelling(sample, file, default_label = 1L)
```

## Arguments

- sample:

  A `Match_Sample` object or a `data.table` / `data.frame` with the
  matches schema.

- file:

  Path to the CSV file to write.

- default_label:

  Integer scalar (default `1L`) used as the block-default `equal` value
  on header rows. `0L` for the inverse workflow.

## Value

Invisibly returns `file`.

## See also

[`import_labels()`](https://edubruell.github.io/joinery/reference/import_labels.md)
