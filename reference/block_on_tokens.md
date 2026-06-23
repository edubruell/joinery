# Block on a Column's Rare Tokens (region-free blocking)

Build a token-blocking key for use inside a strategy's `block_by`. Where
a plain column name blocks two records only when they share a literal
value, `block_on_tokens()` blocks them when they share **any** of a
designated column's (rare) tokens. This is **region-free**: a record
that drifts across a region boundary - a firm that moves to a new
postcode, say - still co-blocks with its earlier self through a
distinctive name token, and so becomes a candidate where a literal block
would never compare them.

Hand it to `block_by` in place of (or mixed with) a column name:

    # fully region-free - share a rare name token, regardless of place
    search_strategy(name ~ normalize_text + word_tokens(min_nchar = 3),
                    block_by = block_on_tokens("name", max_df = 50))

    # region-bounded - share a rare name token AND sit in the same plz2
    search_strategy(name ~ normalize_text + word_tokens(min_nchar = 3),
                    block_by = list(block_on_tokens("name", max_df = 50), "plz2"))

`max_df` and `min_rarity` select which tokens are eligible block keys,
using the **global** (corpus-wide) document frequency: a token appearing
in more than `max_df` records, or whose global rarity falls below
`min_rarity`, is dropped as a key. This is where "block on the
distinctive words, not the common ones" lives - a franchise name
("ALDI") is globally common, fails the cap, and never becomes a block
key, while a distinctive brand survives. A record with no surviving
block key is **unreachable via token-blocking in this stage** (it
contributes no token-block rows).

Token-blocking is the densest operation in the package: every pair
sharing a surviving key is materialised. It is safe **only** behind a
real `max_df` (or `min_rarity`) plus the always-on fan-out guard.
Passing neither cap is a loud warning, not an error, but you almost
always want one.

## Usage

``` r
block_on_tokens(
  column,
  max_df = Inf,
  min_rarity = 0,
  preparer = NULL,
  min_nchar = 3L
)
```

## Arguments

- column:

  The column whose tokens become block keys (for example `"name"`).

- max_df:

  Numeric scalar. Global document-frequency cap: tokens appearing in
  more than `max_df` records corpus-wide are dropped as block keys.
  Default `Inf` (no cap - see the density warning above).

- min_rarity:

  Numeric scalar. Global rarity floor: tokens whose corpus-wide rarity
  falls below this are dropped as block keys. Default `0`.

- preparer:

  Optional preprocessing pipeline for the blocking column, given as a
  one-sided or two-sided formula like the `column ~ steps` you pass to
  [`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md)
  (for example `~ normalize_text + word_tokens(min_nchar = 4)`). Default
  `NULL` reuses the column's own scored preparer when `column` is also a
  scored column, else falls back to
  `normalize_text + word_tokens(min_nchar = min_nchar)`.

- min_nchar:

  Integer scalar. Minimum token length for the default preparer. Default
  `3L`.

## Value

A `Block_On_Tokens` spec, to be placed in `block_by`.

## See also

[`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md)

## Examples

``` r
# Block on a rare word from the workshop name instead of a region, so a
# workshop still co-blocks with its relocated self. The max_df cap keeps
# common words ("joinery") from becoming block keys.
strat <- search_strategy(
  workshop ~ normalize_text() + word_tokens(min_nchar = 3),
  block_by     = list(block_on_tokens("workshop", max_df = 50, min_nchar = 4),
                      "trade"),
  rarity_scope = "global",
  threshold    = 0.6
)
strat
#> <joinery::Search_Strategy>
#> 
#> columns
#> workshop: normalize_text() -> word_tokens(min_nchar = 3)
#> 
#> blocking: block_on_tokens(workshop, max_df=50), trade
#> weights: none
#> rarity: inverse_freq (global, min=0)
#> fan-out guard: cap at 50,000,000
#> smoothing: none
#> threshold: 0.6
#> max_candidates: none
#> feedback_strength: none
```
