# Explain a Single Match

Attribution diagnostic (Q3). Reconstructs per-column and per-token
contributions to a single match score. Dispatches on the second
positional argument: a
[`Search_Strategy`](https://edubruell.github.io/joinery/reference/search_strategy.md)
triggers reconstruction from raw inputs; a tokens-shaped table is used
directly.

## Usage

``` r
explain_match(matches, x, ...)
```

## Arguments

- matches:

  Match output table.

- x:

  Either a
  [`Search_Strategy`](https://edubruell.github.io/joinery/reference/search_strategy.md)
  (ergonomic form) or a tokens table with `rarity` (power-user form).

- ...:

  Backend-specific arguments. For the ergonomic form: `base`, `target`,
  `match_id`.

## Value

A `Match_Explanation` object.

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
# Break one pair's score down into its per-token contributions.
first_id <- matches$match_id[matches$source == "target"][1]
explain_match(matches, strat,
              base = workshop_listings, id = "listing_id",
              target = workshop_register, target_id = "reg_no",
              match_id = first_id)
#> <joinery::Match_Explanation> match 1
#> 
#> Records:
#> lhs id=L00415 source=base listing_id=L00415 workshop=The Joiner proprietor=NA
#> trade=Joiner postcode_area=CM town=Chelmsford actual_link=NA
#> gen_tier=category_trap reg_no=NA legal_form=NA address=NA established=NA
#> employees=NA apprentices=NA guild_member=NA sic=NA true_entity=NA
#> rhs id=GMC-00175 source=target listing_id=NA workshop=Oakes the Joiner
#> proprietor=Terence Oakes trade=Joiner postcode_area=CM town=Chelmsford
#> actual_link=NA gen_tier=core reg_no=GMC-00175 legal_form=Sole Trader address=89
#> Station Road established=1986 employees=1 apprentices=0 guild_member=TRUE
#> sic=43320 true_entity=GMC-00175
#> 
#> Score: 1.0000
#> 
#> Per-column contributions:
#> workshop 1.0000 (2 shared tokens)
#> 
#> Shared tokens (showing 2 of 2):
#> workshop / THE rarity=0.5000 rIP=0.7778 weight=1.0000 contrib=0.7778
#> workshop / JOINER rarity=0.1429 rIP=0.2222 weight=1.0000 contrib=0.2222
```
