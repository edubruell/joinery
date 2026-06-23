# Matching across years and sources

A business register is usually a yearly snapshot: a list of the
workshops trading in some area this year, then another list next year,
and so on. Stack several years of those snapshots together and you can
do something a single year cannot. You can follow each workshop through
time and read off when it opened, when it closed, and how long it
lasted.

The obstacle is that a workshop is not recorded the same way every year.
It is written one way in 2019 and a little differently in 2021; one
relocates to a new part of town; one trades under a name spelled by ear.
Pool all the years into one table and the job is no longer “find the
duplicates in a list”. It is “decide which rows, across all the years,
are the same workshop”, so that each workshop becomes one trajectory
rather than a scatter of unconnected yearly entries.

This article does that on a small multi-year panel of woodworking
workshops. It pools the years, links across them in ordered passes, and
turns the result into one stable identity per workshop. The payoff it
builds toward: once you have those trajectories you can count how many
workshops open and close each year, and the matching choices you make
upstream decide whether those counts come out right.

``` r

library(joinery)
library(dplyr)
```

## The data

`workshop_panel` is the same universe of woodworking workshops as the
rest of the examples, observed across five years. Each row is one
workshop in one year. The `true_entity` column is the answer key: every
year-row of one workshop shares it, and our job is to recover that
grouping from the names alone. The `change_tier` column records what
makes each trajectory hard.

``` r

library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following objects are masked from 'package:dplyr':
#> 
#>     between, first, last
#> The following object is masked from 'package:base':
#> 
#>     %notin%
panel <- as.data.table(workshop_panel)

c(rows = nrow(panel),
  workshops = uniqueN(panel$true_entity),
  years = uniqueN(panel$year))
#>      rows workshops     years 
#>       847       320         5

panel[, .N, by = change_tier]
#>    change_tier     N
#>         <char> <int>
#> 1:      stable   496
#> 2:  name_drift   162
#> 3:       mover   116
#> 4:    phonetic    73
```

Most workshops just carry light year-to-year noise (`stable`). The hard
minority is what the passes below are built for: `name_drift` workshops
change their name part-way through, `mover` workshops relocate to a
different postcode area, and `phonetic` ones have a spelled-by-ear twin.

### One way to measure

Every run below returns the same thing: an entity table, one row per
record, with an `entity` id that groups the records the matcher believes
are one workshop. A trajectory is **fully recovered** when all of a true
workshop’s year-rows land in a single entity. That one rule scores every
approach, so we write it once.

``` r

trajectories_intact <- function(grouping) {
  grouping |>
    select(record_id = id, entity) |>
    left_join(select(workshop_panel, record_id, true_entity),
              by = "record_id") |>
    group_by(true_entity) |>
    summarise(n_entities = n_distinct(entity), .groups = "drop") |>
    summarise(intact = sum(n_entities == 1), total = n())
}
```

There are 320 true workshops, so 320 is a perfect score.

## Linking the pooled table to itself

When you link two different sources you search one against the other.
Here there is one pooled table, so you search it against itself.
[`multi_stage_search()`](https://edubruell.github.io/joinery/reference/multi_stage_search.md)
does this with `self = TRUE`: it pools the rows, runs each strategy as a
search pass, and groups the accumulated links into entities at the end.
Tell it which column records where a row came from with `source_by`,
here the `year`, so the result can report how many years each workshop
spans.

Start with a single forgiving pass and see how far one strategy gets.

``` r

fuzzy <- search_strategy(
  workshop   ~ normalize_text() + word_tokens(min_nchar = 3),
  proprietor ~ normalize_text() + word_tokens(min_nchar = 2),
  block_by  = c("postcode_area", "trade"),
  threshold = 0.55
)

g1 <- multi_stage_search(
  panel, panel,
  base_id = "record_id", target_id = "record_id",
  list(fuzzy = fuzzy),
  self = TRUE, source_by = "year"
)

trajectories_intact(g1)
#> # A tibble: 1 × 2
#>   intact total
#>    <int> <int>
#> 1    283   320
```

One pass already recovers most of the panel. What it misses are the
trajectories where the name moved too far in one step for a single
threshold to bridge.

## Staging: a strict pass, then looser ones

The fix is to run several strategies in order, strictest first. An exact
pass matches only workshops whose name tokens are identical, which is
cheap and almost never wrong, and it clears the easy bulk out of the
way. Then a fuzzy pass works on what is left.

The important option is `collapse`. Between stages, `collapse = "rep"`
takes every group found so far and collapses it to a single
representative row that stays searchable. A drifting name is then linked
one short step at a time: 2019 links to 2020, that pair collapses to one
representative, and the representative is what the next pass compares
against 2021. Without the collapse, the looser pass has to bridge the
whole drift in one jump.

``` r

exact <- exact_strategy(
  workshop ~ normalize_text() + word_tokens(min_nchar = 3),
  block_by = c("postcode_area", "trade")
)

g2 <- multi_stage_search(
  panel, panel,
  base_id = "record_id", target_id = "record_id",
  list(exact = exact, fuzzy = fuzzy),
  self = TRUE, source_by = "year", collapse = "rep"
)

trajectories_intact(g2)
#> # A tibble: 1 × 2
#>   intact total
#>    <int> <int>
#> 1    284   320
```

To see what the collapse buys, run the same two stages with
`collapse = "none"`, which carries only the still-unmatched records
forward and never builds a bridge:

``` r

g_none <- multi_stage_search(
  panel, panel,
  base_id = "record_id", target_id = "record_id",
  list(exact = exact, fuzzy = fuzzy),
  self = TRUE, source_by = "year", collapse = "none"
)

trajectories_intact(g_none)
#> # A tibble: 1 × 2
#>   intact total
#>    <int> <int>
#> 1    207   320
```

Many more trajectories shatter without the carried representative. The
drift chains break into pieces because no single pass ever sees the two
ends close enough together. Collapse-and-continue is what holds a slowly
drifting name in one piece.

## A pass for the movers

The exact and fuzzy passes both block on `(postcode_area, trade)`: they
only ever compare workshops in the same area. That is fast and right for
most of the panel, but it can never link a workshop to its own later
self once it has relocated, because the two rows are in different areas.
Those movers need a pass that does not block on area at all.

Block instead on a rare word from the workshop name. Rarity here just
means how few records carry a token: a distinctive surname is rare, the
word “joinery” is common. A rare name token is shared by a workshop and
its relocated self but by almost nothing else, so it co-blocks the two
rows wherever they sit. Measure that rarity across the whole pool rather
than within a block, so a distinctive token reads as strong evidence no
matter which area it appears in.

``` r

mover <- search_strategy(
  workshop ~ normalize_text() + word_tokens(min_nchar = 3),
  block_by     = list(block_on_tokens("workshop", max_df = 50, min_nchar = 4),
                      "trade"),
  rarity_scope = "global",
  threshold    = 0.6
)

g3 <- multi_stage_search(
  panel, panel,
  base_id = "record_id", target_id = "record_id",
  list(exact = exact, fuzzy = fuzzy, mover = mover),
  self = TRUE, source_by = "year", collapse = "rep"
)

trajectories_intact(g3)
#> # A tibble: 1 × 2
#>   intact total
#>    <int> <int>
#> 1    314   320
```

That recovers nearly all of the 320 trajectories. The three passes share
the work in a readable way: the exact pass links the clean rows, the
fuzzy pass picks up the drifters, and the mover pass reaches across the
relocations the area block could not.

``` r

as.data.table(g3)[, .N, by = stage]
#>     stage     N
#>    <char> <int>
#> 1:   <NA>    75
#> 2:  exact   478
#> 3:  fuzzy   235
#> 4:  mover    59
```

This table counts *records*, one per workshop-year, not trajectories.
The `NA` row is the workshops that never linked to anything: a workshop
seen in only one year, or one no pass could attach. Each is its own
single-record entity.

## Reading the result

The entity table has one row per record and a handful of columns worth
knowing:

``` r

head(g3)
#>    entity       id      rep  rank score source covered_sources n_in_entity
#>     <int>   <char>   <char> <int> <num> <char>           <int>       <int>
#> 1:      1 YR-00001 YR-00001     1    NA   2023               1           1
#> 2:      2 YR-00002 YR-00002     1    NA   2019               1           1
#> 3:      3 YR-00003 YR-00003     1     1   2020               4           4
#> 4:      3 YR-00004 YR-00003     2     1   2021               4           4
#> 5:      3 YR-00005 YR-00003     3     1   2022               4           4
#> 6:      3 YR-00006 YR-00003     4     1   2023               4           4
#>     stage
#>    <char>
#> 1:   <NA>
#> 2:   <NA>
#> 3:  exact
#> 4:  exact
#> 5:  exact
#> 6:  fuzzy
```

- `entity` is the recovered identity. Records that share it are one
  workshop.
- `rep` is the representative record chosen for the entity.
- `source` and `covered_sources` come from `source_by`. Here `source` is
  the row’s year and `covered_sources` is how many distinct years the
  whole entity spans, the trajectory length.
- `n_in_entity` is how many records the entity holds; `stage` is the
  pass that attached this record.

Every link the passes found, with the stage and direction of each, is
kept as a ledger you can pull off the result. It is the audit trail
behind the grouping:

``` r

ledger <- attr(g3, "ledger")
head(ledger[, c("from", "to", "stage", "source_from", "source_to",
                "within_source")])
#>        from       to  stage source_from source_to within_source
#>      <char>   <char> <char>      <char>    <char>        <lgcl>
#> 1: YR-00640 YR-00641  exact        2022      2023         FALSE
#> 2: YR-00641 YR-00640  exact        2023      2022         FALSE
#> 3: YR-00800 YR-00801  exact        2019      2020         FALSE
#> 4: YR-00800 YR-00802  exact        2019      2021         FALSE
#> 5: YR-00800 YR-00803  exact        2019      2022         FALSE
#> 6: YR-00801 YR-00800  exact        2020      2019         FALSE
```

`within_source` is `FALSE` for a cross-year link and `TRUE` for a
within-year one, so you can tell at a glance which links actually do the
work of joining a trajectory across time.

To see how the passes divide the labour,
[`compare_stages()`](https://edubruell.github.io/joinery/reference/compare_stages.md)
reports what each one added:

``` r

compare_stages(g3, base = panel, target = panel)
#> 
#> ── Stage_Comparison (candidates, 3 stages) ─────────────────────────────────────
#> exact -> fuzzy -> mover
#> [exact] 894 pairs base=56.4% target=56.4% score median=1.000
#> [fuzzy] 359 pairs base=30.8% target=31.1% score median=0.727
#> [mover] 108 pairs base=8.4% target=8.9% score median=1.000
#> marginal coverage
#> exact: +478 base (56.4%)
#> fuzzy: +193 base (22.8%)
#> mover: +54 base (6.4%)
```

Read the “marginal coverage” block: it shows how many records each pass
linked that no earlier pass had reached, so you can see the exact front
doing the bulk of the work and the later passes earning their keep on
the harder remainder.

## The payoff: entry and exit

Now turn the trajectories into the numbers you actually wanted. For each
recovered workshop, the first and last year it appears is its entry and
exit; before 2019 or after 2023 we cannot see, so a workshop first seen
in 2019 may be older and one still present in 2023 has not exited yet.

``` r

span <- as.data.table(g3)[, .(record_id = id, entity)] |>
  merge(panel[, .(record_id, year)], by = "record_id")
span <- span[, .(first = min(year), last = max(year)), by = entity]

c(births_after_2019 = span[first > 2019, .N],
  exits_before_2023 = span[last < 2023, .N])
#> births_after_2019 exits_before_2023 
#>               206               165
```

Compare those to the truth, computed the same way on `true_entity`:

``` r

truth <- panel[, .(first = min(year), last = max(year)), by = true_entity]

c(births_after_2019 = truth[first > 2019, .N],
  exits_before_2023 = truth[last < 2023, .N])
#> births_after_2019 exits_before_2023 
#>               200               159
```

The counts are close but not equal, and they err in a specific
direction. The few trajectories the matcher still split show up as a
workshop that “exits” when its name drifts and a brand new one that
“enters” the next year. Every missed link manufactures one phantom exit
and one phantom entry, so under-linking pushes both counts up and makes
the sector look more churny than it is.

That is the whole reason the matching threshold is not just a tuning
detail when you study entry and exit. Link too loosely and you fuse two
different workshops, erasing a real birth and a real death; link too
strictly and you split one workshop, inventing a birth and a death. The
two mistakes bias churn in opposite directions, and neither is safe by
default. Choosing that operating point deliberately, and checking what
it costs, is its own job; the [calibration
article](https://edubruell.github.io/joinery/articles/calibration.md) is
about exactly that decision.

## Where to look next

- The strategies used here (exact matching, fuzzy scoring, token
  blocking, and global rarity) get their full treatment in the [features
  article](https://edubruell.github.io/joinery/articles/features.md).
- Deciding how strict to be, and what a wrong or missed link costs your
  analysis, is the [calibration
  article](https://edubruell.github.io/joinery/articles/calibration.md).
- The same staged search runs on a database backend for panels too large
  for memory, with the verbs unchanged. That is the subject of the
  DuckDB article.
- For finding duplicates inside a single table rather than linking
  across years, reach for
  [`multi_stage_dedup()`](https://edubruell.github.io/joinery/reference/multi_stage_dedup.md)
  instead of
  [`multi_stage_search()`](https://edubruell.github.io/joinery/reference/multi_stage_search.md).
