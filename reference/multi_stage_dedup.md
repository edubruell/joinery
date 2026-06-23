# Staged Duplicate Detection (within one table)

Deduplicate a single table in increasingly tolerant passes. A typical
run starts with a cheap
[`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md)
pass that catches the clean duplicates, then applies looser
[`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md)
passes (often with wider blocking) to the records still unmatched. All
the links found across the passes are grouped into duplicate groups at
the end, so a record linked to `B` in an early pass and `B` linked to
`C` in a later one all land in the same group.

For linking across two tables or several sources, use
[`multi_stage_search()`](https://edubruell.github.io/joinery/reference/multi_stage_search.md).

## Usage

``` r
multi_stage_dedup(table, id, strategies, ...)
```

## Arguments

- table:

  A data.frame, tibble, data.table, or backend table to deduplicate.

- id:

  Character scalar naming the ID column in `table`.

- strategies:

  Named, ordered list of strategies to apply in turn. Each element is an
  [`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md),
  [`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md),
  or
  [`embedding_strategy()`](https://edubruell.github.io/joinery/reference/embedding_strategy.md).

- ...:

  Further arguments to the staged run:

  - `rep_by`: optional character scalar naming a priority column on
    `table` used to choose each group's representative (passed to
    [`resolve_entities()`](https://edubruell.github.io/joinery/reference/resolve_entities.md):
    smallest `rep_by` wins, ties broken by smallest id).

  - `edge_filter`: optional callback `function(edges, stage_name)`
    applied to each pass's links before they are accumulated (for
    example a domain rule that drops implausible matches). The links
    carry `from`, `to`, `score`, and `stage`.

  Backend methods may accept additional arguments.

## Value

The standard dedup result: `duplicate_group | id | score | rank` plus
the original columns of `table`, and a `stage` column recording which
pass first linked each record.

## See also

[`multi_stage_search()`](https://edubruell.github.io/joinery/reference/multi_stage_search.md)
for the cross-table version,
[`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md)
for a single pass,
[`resolve_entities()`](https://edubruell.github.io/joinery/reference/resolve_entities.md)
for the grouping step.

## Examples

``` r
# Two passes over one table: exact token-set first, then a looser fuzzy pass
# on whatever the exact pass left unmatched.
exact <- exact_strategy(
  workshop ~ normalize_text() + word_tokens(min_nchar = 3),
  block_by = c("postcode_area", "trade")
)
fuzzy <- search_strategy(
  workshop ~ normalize_text() + word_tokens(min_nchar = 3),
  block_by  = c("postcode_area", "trade"),
  threshold = 0.6
)
dups <- multi_stage_dedup(workshop_register, "reg_no",
                          list(exact = exact, fuzzy = fuzzy))
head(dups)
#> # A tibble: 6 × 19
#>   id      duplicate_group score  rank stage workshop proprietor trade legal_form
#>   <chr>             <int> <dbl> <int> <chr> <chr>    <chr>      <chr> <chr>     
#> 1 GMC-00…              21   0.6     1 fuzzy Lowther… Victor Lo… Wood… Ltd       
#> 2 GMC-00…              21   0.6     2 fuzzy Logan W… Craig Log… Wood… Ltd       
#> 3 GMC-00…              34   1       1 exact Davenpo… Arthur Da… Wood… Sole Trad…
#> 4 GMC-D0…              34   1       2 exact Davenpo… Arthur Da… Wood… Sole Trad…
#> 5 GMC-00…              42   1       1 fuzzy Fallow … Harold Fa… Join… Partnersh…
#> 6 GMC-D0…              42   1       2 fuzzy Fallow … Fallow     Join… Partnersh…
#> # ℹ 10 more variables: postcode_area <chr>, town <chr>, address <chr>,
#> #   established <int>, employees <dbl>, apprentices <dbl>, guild_member <lgl>,
#> #   sic <chr>, true_entity <chr>, gen_tier <chr>
```
