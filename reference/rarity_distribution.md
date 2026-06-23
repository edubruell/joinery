# Read the Token Rarity Distribution

A pre-match read of how token rarity is distributed in your data. For
each column (and block, when the strategy blocks) it reports the spread
of token document frequency and rarity, plus an offender list: the most
common tokens, the ones that drive a match to balloon. Use it to set
`min_rarity` and `max_token_df` from what is actually in the data
instead of guessing.

It never builds the pair set: it only tokenizes and measures rarity, so
it is cheap enough to run on a full corpus before committing to a
strategy.

## Usage

``` r
rarity_distribution(data, id, strategy, ...)
```

## Arguments

- data:

  A data.frame / tibble / data.table (or backend-specific table).

- id:

  Character scalar naming the ID column in `data`.

- strategy:

  A `Search_Strategy` object.

- ...:

  Additional backend-specific arguments. Notably `n_offenders` (integer;
  how many top-df tokens to list, default 20) and `sample_n` (DuckDB:
  rows to pull before delegating; default all).

## Value

A `Rarity_Distribution` object.

## See also

[`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md)
for the `min_rarity` / `max_token_df` levers this verb informs;
[`audit_strategy()`](https://edubruell.github.io/joinery/reference/audit_strategy.md)
for the broader pre-match audit.

## Examples

``` r
strat <- search_strategy(
  workshop ~ normalize_text() + word_tokens(min_nchar = 3),
  block_by = c("postcode_area", "trade")
)
# Read the token distribution and the most common tokens before matching.
rarity_distribution(workshop_register, "reg_no", strat)
#> 
#> ── Rarity_Distribution ─────────────────────────────────────────────────────────
#> rarity method: "inverse_freq" (per block)
#> per-column distribution
#> workshop [block NR, Cabinet Maker]: 18 tokens, df_max=11 (CABINET), rarity
#> p50=1, suggested min_rarity >~ 0.09091
#> workshop [block LN, Boat Builder]: 12 tokens, df_max=9 (BOAT), rarity p50=0.75,
#> suggested min_rarity >~ 0.1111
#> workshop [block DH, French Polisher]: 11 tokens, df_max=8 (FRENCH), rarity
#> p50=1, suggested min_rarity >~ 0.125
#> workshop [block PL, Boat Builder]: 12 tokens, df_max=8 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.125
#> workshop [block BD, Wood Turner]: 10 tokens, df_max=8 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.125
#> workshop [block BS, Staircase Specialist]: 15 tokens, df_max=8 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.125
#> workshop [block WR, Staircase Specialist]: 9 tokens, df_max=8 (STAIRCASE),
#> rarity p50=0.5, suggested min_rarity >~ 0.125
#> workshop [block AB, Boat Builder]: 10 tokens, df_max=8 (BOAT), rarity p50=0.5,
#> suggested min_rarity >~ 0.125
#> workshop [block LL, Boat Builder]: 13 tokens, df_max=7 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.1429
#> workshop [block BS, Shopfitter]: 14 tokens, df_max=7 (SHOPFITTING), rarity
#> p50=0.5, suggested min_rarity >~ 0.1429
#> workshop [block NR, Shopfitter]: 12 tokens, df_max=7 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.1429
#> workshop [block DE, Joiner]: 11 tokens, df_max=7 (JOINERY), rarity p50=1,
#> suggested min_rarity >~ 0.1429
#> workshop [block AB, Joiner]: 14 tokens, df_max=7 (SONS), rarity p50=1,
#> suggested min_rarity >~ 0.1429
#> workshop [block TD, Cabinet Maker]: 13 tokens, df_max=7 (CABINET), rarity
#> p50=0.5, suggested min_rarity >~ 0.1429
#> workshop [block NR, Joiner]: 14 tokens, df_max=7 (JOINERY), rarity p50=1,
#> suggested min_rarity >~ 0.1429
#> workshop [block AB, Cabinet Maker]: 8 tokens, df_max=7 (CABINET), rarity
#> p50=0.6667, suggested min_rarity >~ 0.1429
#> workshop [block LS, Boat Builder]: 11 tokens, df_max=6 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.1667
#> workshop [block LN, Joiner]: 18 tokens, df_max=6 (JOINERY), rarity p50=1,
#> suggested min_rarity >~ 0.1667
#> workshop [block DT, Boat Builder]: 11 tokens, df_max=6 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.1667
#> workshop [block HR, French Polisher]: 8 tokens, df_max=6 (FRENCH), rarity
#> p50=1, suggested min_rarity >~ 0.1667
#> workshop [block GU, Staircase Specialist]: 13 tokens, df_max=6 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.1667
#> workshop [block CF, Wood Turner]: 10 tokens, df_max=6 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.1667
#> workshop [block WR, Joiner]: 14 tokens, df_max=6 (JOINERY), rarity p50=1,
#> suggested min_rarity >~ 0.1667
#> workshop [block KA, Wood Turner]: 8 tokens, df_max=6 (WOOD), rarity p50=0.75,
#> suggested min_rarity >~ 0.1667
#> workshop [block BT, Cabinet Maker]: 12 tokens, df_max=6 (CABINET), rarity
#> p50=1, suggested min_rarity >~ 0.1667
#> workshop [block TD, Shopfitter]: 8 tokens, df_max=6 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.1667
#> workshop [block TR, Shopfitter]: 7 tokens, df_max=6 (SHOPFITTING), rarity
#> p50=0.5, suggested min_rarity >~ 0.1667
#> workshop [block LL, French Polisher]: 12 tokens, df_max=6 (FRENCH), rarity
#> p50=1, suggested min_rarity >~ 0.1667
#> workshop [block KY, Shopfitter]: 11 tokens, df_max=6 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.1667
#> workshop [block DT, Joiner]: 13 tokens, df_max=6 (JOINERY), rarity p50=0.5,
#> suggested min_rarity >~ 0.1667
#> workshop [block DH, Staircase Specialist]: 11 tokens, df_max=6 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.1667
#> workshop [block SA, French Polisher]: 10 tokens, df_max=6 (FRENCH), rarity
#> p50=0.5, suggested min_rarity >~ 0.1667
#> workshop [block NE, Cabinet Maker]: 12 tokens, df_max=6 (CABINET), rarity
#> p50=1, suggested min_rarity >~ 0.1667
#> workshop [block SO, Cabinet Maker]: 9 tokens, df_max=6 (CABINET), rarity
#> p50=0.5, suggested min_rarity >~ 0.1667
#> workshop [block NR, Boat Builder]: 11 tokens, df_max=5 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.2
#> workshop [block SA, Joiner]: 13 tokens, df_max=5 (JOINERY), rarity p50=1,
#> suggested min_rarity >~ 0.2
#> workshop [block TR, Wood Turner]: 11 tokens, df_max=5 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.2
#> workshop [block BS, Boat Builder]: 12 tokens, df_max=5 (BOAT), rarity p50=0.75,
#> suggested min_rarity >~ 0.2
#> workshop [block NR, Staircase Specialist]: 10 tokens, df_max=5 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.2
#> workshop [block IV, Wood Turner]: 9 tokens, df_max=5 (WOOD), rarity p50=0.5,
#> suggested min_rarity >~ 0.2
#> workshop [block SA, Cabinet Maker]: 8 tokens, df_max=5 (CABINET), rarity
#> p50=0.5, suggested min_rarity >~ 0.2
#> workshop [block PR, Joiner]: 10 tokens, df_max=5 (JOINERY), rarity p50=1,
#> suggested min_rarity >~ 0.2
#> workshop [block LL, Staircase Specialist]: 10 tokens, df_max=5 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.2
#> workshop [block LL, Wood Turner]: 12 tokens, df_max=5 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.2
#> workshop [block TA, Staircase Specialist]: 10 tokens, df_max=5 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.2
#> workshop [block PR, Boat Builder]: 16 tokens, df_max=5 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.2
#> workshop [block GL, Shopfitter]: 10 tokens, df_max=5 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.2
#> workshop [block EH, Cabinet Maker]: 10 tokens, df_max=5 (CABINET), rarity
#> p50=1, suggested min_rarity >~ 0.2
#> workshop [block SO, Wood Turner]: 6 tokens, df_max=5 (WOOD), rarity p50=0.6667,
#> suggested min_rarity >~ 0.2
#> workshop [block GU, Joiner]: 13 tokens, df_max=5 (JOINERY), rarity p50=1,
#> suggested min_rarity >~ 0.2
#> workshop [block HR, Shopfitter]: 15 tokens, df_max=5 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.2
#> workshop [block KY, Joiner]: 7 tokens, df_max=5 (SONS), rarity p50=1, suggested
#> min_rarity >~ 0.2
#> workshop [block SY, Cabinet Maker]: 10 tokens, df_max=5 (CABINET), rarity
#> p50=1, suggested min_rarity >~ 0.2
#> workshop [block KY, Boat Builder]: 9 tokens, df_max=5 (BOAT), rarity p50=0.5,
#> suggested min_rarity >~ 0.2
#> workshop [block SA, Carpenter]: 9 tokens, df_max=5 (CARPENTRY), rarity p50=0.5,
#> suggested min_rarity >~ 0.2
#> workshop [block SA, Staircase Specialist]: 9 tokens, df_max=5 (STAIRCASE),
#> rarity p50=0.5, suggested min_rarity >~ 0.2
#> workshop [block TA, Carpenter]: 15 tokens, df_max=5 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.2
#> workshop [block WR, Wood Turner]: 14 tokens, df_max=5 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.2
#> workshop [block DH, Carpenter]: 13 tokens, df_max=5 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.2
#> workshop [block AB, Carpenter]: 10 tokens, df_max=5 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.2
#> workshop [block TD, Joiner]: 10 tokens, df_max=5 (JOINERY), rarity p50=1,
#> suggested min_rarity >~ 0.2
#> workshop [block WR, Boat Builder]: 10 tokens, df_max=5 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.2
#> workshop [block EX, Shopfitter]: 4 tokens, df_max=5 (SHOPFITTER), rarity
#> p50=0.6667, suggested min_rarity >~ 0.2
#> workshop [block GL, Joiner]: 7 tokens, df_max=5 (JOINERY), rarity p50=1,
#> suggested min_rarity >~ 0.2
#> workshop [block TR, French Polisher]: 7 tokens, df_max=5 (FRENCH), rarity
#> p50=0.5, suggested min_rarity >~ 0.2
#> workshop [block LN, Cabinet Maker]: 14 tokens, df_max=4 (SONS), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block DT, Shopfitter]: 7 tokens, df_max=4 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.25
#> workshop [block HR, Boat Builder]: 9 tokens, df_max=4 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block CA, Staircase Specialist]: 8 tokens, df_max=4 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.25
#> workshop [block WR, Cabinet Maker]: 14 tokens, df_max=4 (CABINET), rarity
#> p50=1, suggested min_rarity >~ 0.25
#> workshop [block CM, Boat Builder]: 11 tokens, df_max=4 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block EH, French Polisher]: 9 tokens, df_max=4 (FRENCH), rarity
#> p50=1, suggested min_rarity >~ 0.25
#> workshop [block PL, Carpenter]: 9 tokens, df_max=4 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block PL, French Polisher]: 9 tokens, df_max=4 (FRENCH), rarity
#> p50=1, suggested min_rarity >~ 0.25
#> workshop [block CF, Joiner]: 8 tokens, df_max=4 (SONS), rarity p50=0.75,
#> suggested min_rarity >~ 0.25
#> workshop [block CF, Carpenter]: 10 tokens, df_max=4 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block HR, Carpenter]: 11 tokens, df_max=4 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block SO, Joiner]: 7 tokens, df_max=4 (BROOKS), rarity p50=0.3333,
#> suggested min_rarity >~ 0.25
#> workshop [block AB, Wood Turner]: 10 tokens, df_max=4 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block TA, Wood Turner]: 9 tokens, df_max=4 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block PR, French Polisher]: 9 tokens, df_max=4 (FRENCH), rarity
#> p50=1, suggested min_rarity >~ 0.25
#> workshop [block KY, Wood Turner]: 10 tokens, df_max=4 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block KY, Cabinet Maker]: 13 tokens, df_max=4 (CABINET), rarity
#> p50=1, suggested min_rarity >~ 0.25
#> workshop [block IV, Boat Builder]: 9 tokens, df_max=4 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block NR, Carpenter]: 8 tokens, df_max=4 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block LL, Shopfitter]: 14 tokens, df_max=4 (LTD), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block CM, Joiner]: 13 tokens, df_max=4 (JOINER), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block IP, Cabinet Maker]: 8 tokens, df_max=4 (CABINET), rarity
#> p50=0.75, suggested min_rarity >~ 0.25
#> workshop [block DH, Wood Turner]: 7 tokens, df_max=4 (WOOD), rarity p50=0.5,
#> suggested min_rarity >~ 0.25
#> workshop [block TA, Shopfitter]: 6 tokens, df_max=4 (SHOPFITTER), rarity
#> p50=0.75, suggested min_rarity >~ 0.25
#> workshop [block NR, Wood Turner]: 13 tokens, df_max=4 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block SY, French Polisher]: 9 tokens, df_max=4 (FRENCH), rarity
#> p50=1, suggested min_rarity >~ 0.25
#> workshop [block SA, Shopfitter]: 11 tokens, df_max=4 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.25
#> workshop [block SY, Boat Builder]: 9 tokens, df_max=4 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block SO, Shopfitter]: 8 tokens, df_max=4 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.25
#> workshop [block LN, Staircase Specialist]: 7 tokens, df_max=4 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.25
#> workshop [block CF, Boat Builder]: 9 tokens, df_max=4 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block NE, Staircase Specialist]: 8 tokens, df_max=4 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.25
#> workshop [block LS, Joiner]: 12 tokens, df_max=4 (JOINERY), rarity p50=0.5,
#> suggested min_rarity >~ 0.25
#> workshop [block GU, Cabinet Maker]: 9 tokens, df_max=4 (CABINET), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block HR, Staircase Specialist]: 9 tokens, df_max=4 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.25
#> workshop [block EH, Wood Turner]: 9 tokens, df_max=4 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block EH, Shopfitter]: 5 tokens, df_max=4 (SHOPFITTING), rarity
#> p50=0.5, suggested min_rarity >~ 0.25
#> workshop [block IV, Staircase Specialist]: 9 tokens, df_max=4 (STAIRCASE),
#> rarity p50=0.5, suggested min_rarity >~ 0.25
#> workshop [block EH, Carpenter]: 10 tokens, df_max=4 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.25
#> workshop [block DH, Joiner]: 11 tokens, df_max=4 (LTD), rarity p50=1, suggested
#> min_rarity >~ 0.25
#> workshop [block TD, French Polisher]: 8 tokens, df_max=4 (FRENCH), rarity
#> p50=1, suggested min_rarity >~ 0.25
#> workshop [block AB, Staircase Specialist]: 6 tokens, df_max=4 (STAIRCASE),
#> rarity p50=0.5, suggested min_rarity >~ 0.25
#> workshop [block NR, French Polisher]: 5 tokens, df_max=4 (FRENCH), rarity
#> p50=0.3333, suggested min_rarity >~ 0.25
#> workshop [block LN, French Polisher]: 8 tokens, df_max=4 (FRENCH), rarity
#> p50=0.75, suggested min_rarity >~ 0.25
#> workshop [block SA, Boat Builder]: 8 tokens, df_max=3 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block DE, Cabinet Maker]: 10 tokens, df_max=3 (CABINET), rarity
#> p50=1, suggested min_rarity >~ 0.3333
#> workshop [block BS, Wood Turner]: 10 tokens, df_max=3 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block EX, Carpenter]: 8 tokens, df_max=3 (SONS), rarity p50=0.6667,
#> suggested min_rarity >~ 0.3333
#> workshop [block EX, Boat Builder]: 7 tokens, df_max=3 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block BD, Joiner]: 7 tokens, df_max=3 (JOINERY), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block WR, Shopfitter]: 10 tokens, df_max=3 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.3333
#> workshop [block BD, Shopfitter]: 9 tokens, df_max=3 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.3333
#> workshop [block DT, French Polisher]: 6 tokens, df_max=3 (FRENCH), rarity
#> p50=0.5, suggested min_rarity >~ 0.3333
#> workshop [block KA, Joiner]: 12 tokens, df_max=3 (JOINERS), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block SY, Staircase Specialist]: 8 tokens, df_max=3 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.3333
#> workshop [block PR, Shopfitter]: 9 tokens, df_max=3 (SHOPFITTING), rarity
#> p50=0.5, suggested min_rarity >~ 0.3333
#> workshop [block DE, Wood Turner]: 9 tokens, df_max=3 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block GL, Wood Turner]: 7 tokens, df_max=3 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block LS, Shopfitter]: 7 tokens, df_max=3 (LTD), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block SY, Shopfitter]: 12 tokens, df_max=3 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.3333
#> workshop [block DH, Shopfitter]: 10 tokens, df_max=3 (SHOPFITTER), rarity
#> p50=1, suggested min_rarity >~ 0.3333
#> workshop [block PL, Cabinet Maker]: 8 tokens, df_max=3 (CABINET), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block LL, Joiner]: 10 tokens, df_max=3 (JOINERY), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block DE, Staircase Specialist]: 6 tokens, df_max=3 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.3333
#> workshop [block GL, Carpenter]: 10 tokens, df_max=3 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block NE, Joiner]: 8 tokens, df_max=3 (JOINERS), rarity p50=0.5,
#> suggested min_rarity >~ 0.3333
#> workshop [block GL, Staircase Specialist]: 8 tokens, df_max=3 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.3333
#> workshop [block CF, Shopfitter]: 9 tokens, df_max=3 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.3333
#> workshop [block GL, Cabinet Maker]: 7 tokens, df_max=3 (CABINET), rarity
#> p50=0.5, suggested min_rarity >~ 0.3333
#> workshop [block LS, Carpenter]: 8 tokens, df_max=3 (SONS), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block GL, French Polisher]: 7 tokens, df_max=3 (FRENCH), rarity
#> p50=1, suggested min_rarity >~ 0.3333
#> workshop [block EH, Staircase Specialist]: 8 tokens, df_max=3 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.3333
#> workshop [block SO, French Polisher]: 7 tokens, df_max=3 (FRENCH), rarity
#> p50=1, suggested min_rarity >~ 0.3333
#> workshop [block CF, Cabinet Maker]: 8 tokens, df_max=3 (CABINET), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block IP, Carpenter]: 5 tokens, df_max=3 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block LN, Wood Turner]: 9 tokens, df_max=3 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block NE, French Polisher]: 7 tokens, df_max=3 (FRENCH), rarity
#> p50=1, suggested min_rarity >~ 0.3333
#> workshop [block CA, Joiner]: 10 tokens, df_max=3 (JOINERY), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block LL, Carpenter]: 4 tokens, df_max=3 (CARPENTRY), rarity
#> p50=0.4167, suggested min_rarity >~ 0.3333
#> workshop [block GU, Boat Builder]: 8 tokens, df_max=3 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block TD, Wood Turner]: 10 tokens, df_max=3 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block TA, French Polisher]: 5 tokens, df_max=3 (FRENCH), rarity
#> p50=0.5, suggested min_rarity >~ 0.3333
#> workshop [block TA, Joiner]: 10 tokens, df_max=3 (JOINERY), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block BT, Carpenter]: 6 tokens, df_max=3 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block BS, Cabinet Maker]: 10 tokens, df_max=3 (CABINET), rarity
#> p50=1, suggested min_rarity >~ 0.3333
#> workshop [block DT, Carpenter]: 9 tokens, df_max=3 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block CM, Cabinet Maker]: 9 tokens, df_max=3 (CABINET), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block LS, Cabinet Maker]: 7 tokens, df_max=3 (CABINET), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block EX, French Polisher]: 6 tokens, df_max=3 (FRENCH), rarity
#> p50=0.75, suggested min_rarity >~ 0.3333
#> workshop [block TR, Carpenter]: 7 tokens, df_max=3 (HUGHES), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block IV, Cabinet Maker]: 8 tokens, df_max=3 (CABINET), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block IP, Boat Builder]: 5 tokens, df_max=3 (BOAT), rarity p50=0.5,
#> suggested min_rarity >~ 0.3333
#> workshop [block EH, Joiner]: 12 tokens, df_max=3 (JOINERY), rarity p50=0.5,
#> suggested min_rarity >~ 0.3333
#> workshop [block TA, Boat Builder]: 9 tokens, df_max=3 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block WR, Carpenter]: 8 tokens, df_max=3 (CARPENTRY), rarity
#> p50=0.75, suggested min_rarity >~ 0.3333
#> workshop [block HR, Joiner]: 7 tokens, df_max=3 (LTD), rarity p50=0.5,
#> suggested min_rarity >~ 0.3333
#> workshop [block TR, Staircase Specialist]: 7 tokens, df_max=3 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.3333
#> workshop [block AB, Shopfitter]: 8 tokens, df_max=3 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.3333
#> workshop [block SA, Wood Turner]: 8 tokens, df_max=3 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.3333
#> workshop [block IP, Shopfitter]: 10 tokens, df_max=3 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.3333
#> workshop [block EX, Cabinet Maker]: 6 tokens, df_max=3 (CABINET), rarity
#> p50=0.5, suggested min_rarity >~ 0.3333
#> workshop [block GU, Wood Turner]: 5 tokens, df_max=3 (WOOD), rarity p50=0.5,
#> suggested min_rarity >~ 0.3333
#> workshop [block KA, French Polisher]: 3 tokens, df_max=3 (WRIGHT), rarity
#> p50=0.3333, suggested min_rarity >~ 0.3333
#> workshop [block IP, Wood Turner]: 5 tokens, df_max=2 (WOOD), rarity p50=0.5,
#> suggested min_rarity >~ 0.5
#> workshop [block LL, Cabinet Maker]: 7 tokens, df_max=2 (LTD), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block DE, Shopfitter]: 5 tokens, df_max=2 (LTD), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block TA, Cabinet Maker]: 5 tokens, df_max=2 (CABINET), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block BD, Carpenter]: 8 tokens, df_max=2 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block KA, Boat Builder]: 6 tokens, df_max=2 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block KA, Cabinet Maker]: 8 tokens, df_max=2 (CABINET), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block CM, Shopfitter]: 10 tokens, df_max=2 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.5
#> workshop [block IV, Shopfitter]: 5 tokens, df_max=2 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.5
#> workshop [block DE, Boat Builder]: 6 tokens, df_max=2 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block BD, Cabinet Maker]: 7 tokens, df_max=2 (CABINET), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block LN, Carpenter]: 5 tokens, df_max=2 (HUGHES), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block BT, Boat Builder]: 7 tokens, df_max=2 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block SY, Wood Turner]: 9 tokens, df_max=2 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block TD, Boat Builder]: 7 tokens, df_max=2 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block LS, Staircase Specialist]: 6 tokens, df_max=2 (LYLE), rarity
#> p50=0.75, suggested min_rarity >~ 0.5
#> workshop [block TD, Carpenter]: 7 tokens, df_max=2 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block KA, Staircase Specialist]: 5 tokens, df_max=2 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.5
#> workshop [block NE, Shopfitter]: 6 tokens, df_max=2 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.5
#> workshop [block CA, Carpenter]: 7 tokens, df_max=2 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block BT, Shopfitter]: 7 tokens, df_max=2 (SHOPFITTING), rarity
#> p50=1, suggested min_rarity >~ 0.5
#> workshop [block CF, French Polisher]: 6 tokens, df_max=2 (FRENCH), rarity
#> p50=1, suggested min_rarity >~ 0.5
#> workshop [block IP, French Polisher]: 4 tokens, df_max=2 (BUCHANAN), rarity
#> p50=0.5, suggested min_rarity >~ 0.5
#> workshop [block GU, Shopfitter]: 6 tokens, df_max=2 (LLP), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block GL, Boat Builder]: 7 tokens, df_max=2 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block EX, Staircase Specialist]: 6 tokens, df_max=2 (LTD), rarity
#> p50=1, suggested min_rarity >~ 0.5
#> workshop [block CM, Carpenter]: 5 tokens, df_max=2 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block TR, Joiner]: 9 tokens, df_max=2 (JOINERY), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block SY, Carpenter]: 3 tokens, df_max=2 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block CM, French Polisher]: 6 tokens, df_max=2 (FRENCH), rarity
#> p50=1, suggested min_rarity >~ 0.5
#> workshop [block DE, Carpenter]: 7 tokens, df_max=2 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block CA, French Polisher]: 5 tokens, df_max=2 (POLISHING), rarity
#> p50=1, suggested min_rarity >~ 0.5
#> workshop [block KA, Carpenter]: 5 tokens, df_max=2 (LTD), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block DT, Cabinet Maker]: 6 tokens, df_max=2 (CABINET), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block GU, French Polisher]: 6 tokens, df_max=2 (FRENCH), rarity
#> p50=1, suggested min_rarity >~ 0.5
#> workshop [block NE, Boat Builder]: 4 tokens, df_max=2 (BOAT), rarity p50=0.75,
#> suggested min_rarity >~ 0.5
#> workshop [block BT, French Polisher]: 5 tokens, df_max=2 (MENZIES), rarity
#> p50=0.5, suggested min_rarity >~ 0.5
#> workshop [block SO, Boat Builder]: 4 tokens, df_max=2 (TELFORD), rarity
#> p50=0.5, suggested min_rarity >~ 0.5
#> workshop [block KA, Shopfitter]: 4 tokens, df_max=2 (SHOPFITTING), rarity
#> p50=0.75, suggested min_rarity >~ 0.5
#> workshop [block HR, Cabinet Maker]: 5 tokens, df_max=2 (CABINET), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block CM, Wood Turner]: 8 tokens, df_max=2 (WOOD), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block CA, Boat Builder]: 5 tokens, df_max=2 (MANSON), rarity p50=0.5,
#> suggested min_rarity >~ 0.5
#> workshop [block WR, French Polisher]: 5 tokens, df_max=2 (FRENCH), rarity
#> p50=1, suggested min_rarity >~ 0.5
#> workshop [block CA, Cabinet Maker]: 6 tokens, df_max=2 (CABINET), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block BD, Boat Builder]: 6 tokens, df_max=2 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block PR, Carpenter]: 5 tokens, df_max=2 (CARPENTRY), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block GU, Carpenter]: 4 tokens, df_max=2 (SONS), rarity p50=0.75,
#> suggested min_rarity >~ 0.5
#> workshop [block CF, Staircase Specialist]: 6 tokens, df_max=2 (STAIRCASE),
#> rarity p50=1, suggested min_rarity >~ 0.5
#> workshop [block IP, Joiner]: 6 tokens, df_max=2 (SONS), rarity p50=1, suggested
#> min_rarity >~ 0.5
#> workshop [block DH, Boat Builder]: 6 tokens, df_max=2 (BOAT), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block IV, Joiner]: 5 tokens, df_max=2 (JOINERY), rarity p50=1,
#> suggested min_rarity >~ 0.5
#> workshop [block KY, Carpenter]: 4 tokens, df_max=1 (SOWERBY), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block TD, Staircase Specialist]: 3 tokens, df_max=1 (BELMONT), rarity
#> p50=1, suggested min_rarity >~ 1
#> workshop [block TR, Boat Builder]: 4 tokens, df_max=1 (OSBORNE), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block PL, Joiner]: 5 tokens, df_max=1 (CALTHORPE), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block BT, Staircase Specialist]: 6 tokens, df_max=1 (TEMPLETON),
#> rarity p50=1, suggested min_rarity >~ 1
#> workshop [block PL, Wood Turner]: 4 tokens, df_max=1 (BROUGHTON), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block BT, Wood Turner]: 4 tokens, df_max=1 (CARTWRIGHT), rarity
#> p50=1, suggested min_rarity >~ 1
#> workshop [block IP, Staircase Specialist]: 7 tokens, df_max=1 (BRAIDWOOD),
#> rarity p50=1, suggested min_rarity >~ 1
#> workshop [block PR, Wood Turner]: 6 tokens, df_max=1 (WORMLEY), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block PL, Shopfitter]: 3 tokens, df_max=1 (WELLINGTON), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block TR, Cabinet Maker]: 3 tokens, df_max=1 (KELSON), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block DT, Wood Turner]: 10 tokens, df_max=1 (SAWYER), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block CA, Shopfitter]: 7 tokens, df_max=1 (FORBES), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block BS, French Polisher]: 4 tokens, df_max=1 (KEATS), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block EX, Joiner]: 3 tokens, df_max=1 (POUND), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block DE, French Polisher]: 4 tokens, df_max=1 (HASTINGS), rarity
#> p50=1, suggested min_rarity >~ 1
#> workshop [block SO, Staircase Specialist]: 7 tokens, df_max=1 (LAWSON), rarity
#> p50=1, suggested min_rarity >~ 1
#> workshop [block BD, Staircase Specialist]: 3 tokens, df_max=1 (UTTLEY), rarity
#> p50=1, suggested min_rarity >~ 1
#> workshop [block NE, Carpenter]: 5 tokens, df_max=1 (GREIG), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block LS, Wood Turner]: 4 tokens, df_max=1 (NANCARROW), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block BT, Joiner]: 3 tokens, df_max=1 (RYDER), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block EH, Boat Builder]: 3 tokens, df_max=1 (LUNN), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block HR, Wood Turner]: 4 tokens, df_max=1 (WYNNE), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block BS, Joiner]: 4 tokens, df_max=1 (LLEWELLYN), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block KY, Staircase Specialist]: 4 tokens, df_max=1 (HAWTHORN),
#> rarity p50=1, suggested min_rarity >~ 1
#> workshop [block DT, Staircase Specialist]: 5 tokens, df_max=1 (GLEESON), rarity
#> p50=1, suggested min_rarity >~ 1
#> workshop [block PL, Staircase Specialist]: 4 tokens, df_max=1 (MCINTYRE),
#> rarity p50=1, suggested min_rarity >~ 1
#> workshop [block LN, Shopfitter]: 2 tokens, df_max=1 (KELVEY), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block CM, Staircase Specialist]: 2 tokens, df_max=1 (CROSBIE), rarity
#> p50=1, suggested min_rarity >~ 1
#> workshop [block NE, Wood Turner]: 3 tokens, df_max=1 (FALKINER), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block SY, Joiner]: 3 tokens, df_max=1 (GOLIGHTLY), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block AB, French Polisher]: 4 tokens, df_max=1 (HALSALL), rarity
#> p50=1, suggested min_rarity >~ 1
#> workshop [block PR, Cabinet Maker]: 4 tokens, df_max=1 (CHALLENOR), rarity
#> p50=1, suggested min_rarity >~ 1
#> workshop [block DH, Cabinet Maker]: 3 tokens, df_max=1 (CLARKE), rarity p50=1,
#> suggested min_rarity >~ 1
#> workshop [block LS, French Polisher]: 3 tokens, df_max=1 (WRIGHT), rarity
#> p50=1, suggested min_rarity >~ 1
#> top-df offenders (fan-out drivers)
#> workshop [NR, Cabinet Maker]: 'CABINET' df=11, rarity=0.09091
#> workshop [LN, Boat Builder]: 'BOAT' df=9, rarity=0.1111
#> workshop [DH, French Polisher]: 'FRENCH' df=8, rarity=0.125
#> workshop [PL, Boat Builder]: 'BOAT' df=8, rarity=0.125
#> workshop [BD, Wood Turner]: 'WOOD' df=8, rarity=0.125
#> workshop [BD, Wood Turner]: 'TURNER' df=8, rarity=0.125
#> workshop [BS, Staircase Specialist]: 'STAIRCASE' df=8, rarity=0.125
#> workshop [WR, Staircase Specialist]: 'STAIRCASE' df=8, rarity=0.125
#> workshop [AB, Boat Builder]: 'BOAT' df=8, rarity=0.125
#> workshop [LL, Boat Builder]: 'BOAT' df=7, rarity=0.1429
```
