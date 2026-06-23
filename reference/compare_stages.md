# Compare Stages of a Multi-Stage Match

Multi-stage diagnostic. Produces per-stage `Match_Overview` objects,
marginal coverage per stage, and overlaid per-stage score distributions.
Note that
[`summarise_matches()`](https://edubruell.github.io/joinery/reference/summarise_matches.md)
does **not** auto-detect a `stage` column - users explicitly call this
verb when they want per-stage analysis (see
`notes/diagnostics_design.md`).

## Usage

``` r
compare_stages(matches, ...)
```

## Arguments

- matches:

  Multi-stage match table with a `stage` column.

- ...:

  Method-specific arguments. The data.table method will accept `base`
  and `target` for coverage.

## Value

A `Stage_Comparison` object.

## Examples

``` r
exact <- exact_strategy(
  workshop ~ normalize_text() + word_tokens(min_nchar = 3),
  block_by = c("postcode_area", "trade")
)
fuzzy <- search_strategy(
  workshop ~ normalize_text() + word_tokens(min_nchar = 3),
  block_by  = c("postcode_area", "trade"),
  threshold = 0.55
)
g <- multi_stage_search(
  workshop_panel, workshop_panel,
  base_id = "record_id", target_id = "record_id",
  list(exact = exact, fuzzy = fuzzy),
  self = TRUE, source_by = "year", collapse = "rep"
)
# See how much each pass added that earlier passes had not reached.
compare_stages(g, base = workshop_panel, target = workshop_panel)
#> 
#> ── Stage_Comparison (candidates, 2 stages) ─────────────────────────────────────
#> exact -> fuzzy
#> [exact] 894 pairs base=56.4% target=56.4% score median=1.000
#> [fuzzy] 195 pairs base=16.9% target=20.9% score median=0.667
#> marginal coverage
#> exact: +478 base (56.4%)
#> fuzzy: +94 base (11.1%)
```
