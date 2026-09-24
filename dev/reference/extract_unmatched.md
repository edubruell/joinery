# Extract Unmatched Records

Identify and extract records from a table that were not matched in a
record linkage operation.

## Usage

``` r
extract_unmatched(data, id, matches, ...)
```

## Arguments

- data:

  A data.frame / tibble / data.table (or db table in other backends)
  containing the original records.

- id:

  Character scalar naming the ID column in `data`.

- matches:

  A table of matched record pairs, containing the ID column.

- ...:

  Additional arguments passed to backend-specific methods.

## Value

A subset of `data` containing only records whose IDs do not appear in
`matches`.

## Examples

``` r
strat <- search_strategy(
  workshop ~ normalize_text() + word_tokens(min_nchar = 3),
  block_by  = c("postcode_area", "trade"),
  threshold = 0.7
)
matches <- search_candidates(
  workshop_listings, workshop_register,
  base_id = "listing_id", target_id = "reg_no", strategy = strat
)
# The listings that found no register match, ready for a looser next pass.
leftover <- extract_unmatched(workshop_listings, "listing_id", matches)
nrow(leftover)
#> [1] 549
```
