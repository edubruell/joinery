# Sample Matches for Review

Sampling diagnostic (Q4). Modes: `"high"`, `"low"`, `"borderline"`,
`"ambiguous"`, `"top_gap"`, `"random"`.

## Usage

``` r
sample_matches(matches, ...)
```

## Arguments

- matches:

  Match output table.

- ...:

  Method-specific arguments. Standard arguments: `mode` (one of the
  sampling modes above), `n` (number of rows to sample), and
  mode-specific extras (e.g. `threshold` for `"borderline"`).

## Value

A `Match_Sample` object.

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
# Pull the borderline pairs near the threshold, the ones worth eyeballing.
sample_matches(matches, mode = "borderline", n = 5, threshold = 0.7)
#> <joinery::Match_Sample>
#> mode : borderline
#> n : 5
#> threshold : 0.7000
#> 
#> rows: 5 row(s)
#> 
#> id match_id score source listing_id
#> <char> <int> <num> <char> <char>
#> 1: L00021 541 0.7000000 base L00021
#> 2: GMC-00106 541 0.7000000 target <NA>
#> 3: L00021 542 0.7000000 base L00021
#> 4: GMC-D0054 542 0.7000000 target <NA>
#> 5: L00881 540 0.7017544 base L00881
#> workshop proprietor trade
#> <char> <char> <char>
#> 1: Crawford, D. Shopfitting Derek Crawford Shopfitter
#> 2: Crawford Shopfitter Derek Crawford Shopfitter
#> 3: Crawford, D. Shopfitting Derek Crawford Shopfitter
#> 4: Crawford Shopfitter Crawford Shopfitter
#> 5: Crakehall, G. - Staircase Specialist Graham Crakehall Staircase Specialist
#> postcode_area town actual_link gen_tier reg_no legal_form
#> <char> <char> <char> <char> <char> <char>
#> 1: DH Durham GMC-00106 variant <NA> <NA>
#> 2: DH Durham <NA> core GMC-00106 Sole Trader
#> 3: DH Durham GMC-00106 variant <NA> <NA>
#> 4: DH Durham <NA> register_dup GMC-D0054 Sole Trader
#> 5: BS Bristol GMC-00373 variant <NA> <NA>
#> address established employees apprentices guild_member sic
#> <char> <int> <num> <num> <lgcl> <char>
#> 1: <NA> NA NA NA NA <NA>
#> 2: 80 Albion Works 1977 4 1 FALSE 43320
#> 3: <NA> NA NA NA NA <NA>
#> 4: 37 Victoria Road 1977 4 1 FALSE 43320
#> 5: <NA> NA NA NA NA <NA>
#> true_entity rank
#> <char> <int>
#> 1: <NA> 1
#> 2: GMC-00106 2
#> 3: <NA> 1
#> 4: GMC-00106 2
#> 5: <NA> 1
```
