# Define an Exact Matching Strategy

Creates an Exact_Strategy for exact, score-1.0 token-set matching. Hand
it to the same verbs you would hand a
[`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md):
[`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md)
to group identical records within a table,
[`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md)
to match them across tables. Both return the usual result with
`score == 1.0`.

Two records link only when **every column's token set is equal** within
the same block. This is the same as a fuzzy score of exactly 1.0,
reached without any scoring or threshold, and it is **robust to empty
columns**: two records with identical names and both streets blank will
link, where a weighted threshold would silently reject them. (A blank
column drags a weighted score below its threshold, since its weight
stays in the denominator; exact matching has no such ceiling.)

Use it as the cheap first stage of a staged workflow (exact first, then
fuzzy on whatever is left): the leftover records come from
[`extract_unmatched()`](https://edubruell.github.io/joinery/reference/extract_unmatched.md),
and
[`multi_stage_dedup()`](https://edubruell.github.io/joinery/reference/multi_stage_dedup.md)
/
[`multi_stage_search()`](https://edubruell.github.io/joinery/reference/multi_stage_search.md)
thread them through for you when you pass
`list(exact_strategy(...), search_strategy(...))`.

## Usage

``` r
exact_strategy(
  ...,
  block_by = NULL,
  rarity = "inverse_freq",
  containment = c("off", "forward", "bidirectional"),
  min_base_rarity = 0,
  min_containment_tokens = 1
)
```

## Arguments

- ...:

  Two-sided formulas `column ~ step1 + step2`, identical in form to
  [`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md).

- block_by:

  Optional character vector of blocking columns.

- rarity:

  Character scalar rarity metric, used only by the `min_base_rarity`
  containment guard to measure how much identifying weight a base
  record's tokens carry. One of `"inverse_freq"` (default),
  `"smoothed_inverse_freq"`, `"tfidf"`, or `"bm25"`; see
  [`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md)
  for what each formula does. Plain equality and forward containment
  without a `min_base_rarity` floor never consult it, so the default is
  almost always fine.

- containment:

  One of `"off"` (set-equality, default), `"forward"` (link when the
  base record's tokens are a subset of the target's), or
  `"bidirectional"` (either direction). Whether it helps depends on the
  data; it over-links on noisy corpora, so it is never the default.

- min_base_rarity:

  Numeric containment guard: drop links whose base record carries summed
  rarity mass below this floor. Default `0`.

- min_containment_tokens:

  Numeric containment guard, default `1` (no restriction). A *proper*
  containment link (one record's tokens a strict subset of the other's)
  requires the contained record to hold at least this many tokens;
  set-equality always links. Raise to `2` to stop a single generic
  token - a bare category or hub name (e.g. a shopping-centre name) -
  being a subset of every richer "Store + Centre" name and transitively
  chaining unrelated records into one entity. Ignored when
  `containment = "off"`.

## Value

An Exact_Strategy object.

## See also

[`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md),
[`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md),
[`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md),
[`extract_unmatched()`](https://edubruell.github.io/joinery/reference/extract_unmatched.md).

## Examples

``` r
# Link only workshops whose name tokens are identical within the same area
# and trade. No threshold to tune, and blank columns do not sink a match.
ex <- exact_strategy(
  workshop ~ normalize_text() + word_tokens(min_nchar = 3),
  block_by = c("postcode_area", "trade")
)
dups <- detect_duplicates(workshop_register, id = "reg_no", strategy = ex)
head(dups)
#> # A tibble: 6 × 18
#>   id        duplicate_group score  rank workshop     proprietor trade legal_form
#>   <chr>               <int> <dbl> <int> <chr>        <chr>      <chr> <chr>     
#> 1 GMC-00034              34     1     1 Davenport W… Arthur Da… Wood… Sole Trad…
#> 2 GMC-D0014              34     1     2 Davenport W… Arthur Da… Wood… Sole Trad…
#> 3 GMC-00044              44     1     1 Wetherell W… Graham We… Wood… Ltd       
#> 4 GMC-D0022              44     1     2 Wetherell W… Graham We… Wood… Ltd       
#> 5 GMC-00045              45     1     1 Stark Cabin… Miles Sta… Cabi… LLP       
#> 6 GMC-D0026              45     1     2 Stark Cabin… Miles Sta… Cabi… LLP       
#> # ℹ 10 more variables: postcode_area <chr>, town <chr>, address <chr>,
#> #   established <int>, employees <dbl>, apprentices <dbl>, guild_member <lgl>,
#> #   sic <chr>, true_entity <chr>, gen_tier <chr>
```
