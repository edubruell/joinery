# Staged Search Across Tables or Sources

Link the same real-world entity across two tables, or across several
datasets or vintages of one dataset, by running an ordered list of
strategies as successive search passes. Each pass adds the links it
finds to a running record of every match (the `ledger`), and at the end
all the links are grouped into entities, one row per record showing
which entity it belongs to.

A typical run starts with a cheap
[`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md)
pass to catch the clean matches, then applies one or more looser
[`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md)
passes to the records still unmatched. Use this when the two sides are
not interchangeable: for example one record may carry only part of
another's information, so it matters which side is searched against
which. For finding duplicates within a single table, use
[`multi_stage_dedup()`](https://edubruell.github.io/joinery/reference/multi_stage_dedup.md)
instead.

## Usage

``` r
multi_stage_search(
  base_table,
  target_table,
  base_id,
  target_id,
  strategies,
  ...
)
```

## Arguments

- base_table:

  The left table in the linkage.

- target_table:

  The right table. Pass `base_table` again with `self = TRUE` to search
  a single pooled table against itself.

- base_id:

  Character scalar naming the ID column in `base_table`.

- target_id:

  Character scalar naming the ID column in `target_table`.

- strategies:

  Named, ordered list of strategies to apply in turn. Each element is an
  [`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md),
  [`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md),
  or
  [`embedding_strategy()`](https://edubruell.github.io/joinery/reference/embedding_strategy.md).

- ...:

  Further arguments controlling the staged run:

  - `self`: logical; `TRUE` searches `base_table` against itself (for
    example, pooling several years into one table and linking across
    them).

  - `source_by`: optional character vector naming the column(s) that
    record where each row came from (for example `"year"` or
    `"register"`). When set, every link is tagged as within-source or
    cross-source, and the result reports each entity's `source` and
    `covered_sources`.

  - `collapse`: what happens between stages. `"none"` only carries the
    still-unmatched records forward, while `"rep"` also collapses each
    group found so far to a single representative, shrinking the search
    space for the looser passes that follow. (A third mode that merges
    the token sets of a whole group is reserved for a future release and
    not yet available.)

  - `rep_rule`: rule for choosing each group's representative. Currently
    `"canonical"` is the only rule wired; to set the representative
    yourself, pass a priority column with `rep_by`. Other rules are
    reserved for a future release.

  - `rebind`: how the next stage's two sides are formed from the
    representatives and the residual: `"explicit"`, `"self"`, or
    `"accumulate"` (the path for incremental panel updates).

  - `direction`: which way each pass searches: `"forward"`,
    `"backward"`, or `"bidirectional"`.

  - `edge_filter`: optional callback `function(edges, stage_name)`
    applied to each pass's links before they are accumulated (for
    example a domain rule that drops implausible matches).

  - `rep_by`: optional priority column for choosing representatives
    (passed to
    [`resolve_entities()`](https://edubruell.github.io/joinery/reference/resolve_entities.md)).

  Backend methods may accept additional arguments.

## Value

One row per pooled record describing its entity:
`entity | id | rep | rank | score | source | covered_sources | n_in_entity | stage`.
The full list of links found, with the stage and direction of each, is
attached as the `ledger` attribute and read with
`attr(result, "ledger")`.

## See also

[`multi_stage_dedup()`](https://edubruell.github.io/joinery/reference/multi_stage_dedup.md)
for the within-one-table version,
[`resolve_entities()`](https://edubruell.github.io/joinery/reference/resolve_entities.md)
for the grouping step,
[`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md)
for the usual front stage.

## Examples

``` r
# Follow each workshop across years: pool the panel, search it against itself,
# exact first then fuzzy, collapsing each group found so later passes see less.
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
head(g)
#> # A tibble: 6 × 9
#>   entity id       rep       rank score source covered_sources n_in_entity stage
#>    <int> <chr>    <chr>    <int> <dbl> <chr>            <int>       <int> <chr>
#> 1      1 YR-00764 YR-00764     1 1     2019                 5           7 fuzzy
#> 2      1 YR-00765 YR-00764     2 1     2020                 5           7 fuzzy
#> 3      1 YR-00766 YR-00764     3 1     2021                 5           7 exact
#> 4      1 YR-00768 YR-00764     4 1     2023                 5           7 exact
#> 5      1 YR-00767 YR-00764     5 0.643 2022                 5           7 fuzzy
#> 6      1 YR-00001 YR-00764     6 0.625 2023                 5           7 fuzzy
```
