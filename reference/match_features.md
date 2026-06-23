# Build a per-pair feature table for calibration

Computes a wide, one-row-per-pair feature `data.table` from a joinery
match result, suitable for downstream calibration / false-positive
filtering. The schema is documented in `notes/calibration_design.md` and
treated as the public API. Additions are allowed; reorders or renames
are not.

Dispatches on `(matches, strategy)`. A
[`Search_Strategy`](https://edubruell.github.io/joinery/reference/search_strategy.md)
returns the full token schema (core + token-side columns + string
similarity). An
[`Embedding_Strategy`](https://edubruell.github.io/joinery/reference/embedding_strategy.md)
returns the reduced "embedding" schema (core columns + string
similarity + `cosine_sim` + embedding norms).

## Usage

``` r
match_features(matches, strategy, ...)
```

## Arguments

- matches:

  A match result table (data.table / tibble / data.frame / DuckDB lazy
  `tbl`) from
  [`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md)
  or
  [`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md).

- strategy:

  The
  [`Search_Strategy`](https://edubruell.github.io/joinery/reference/search_strategy.md)
  or
  [`Embedding_Strategy`](https://edubruell.github.io/joinery/reference/embedding_strategy.md)
  used to produce `matches`.

- ...:

  Method-specific arguments. Both strategy methods accept: `base` (the
  base table used as input to matching), `id` (character scalar naming
  the ID column in `base`), `target` (optional target table for
  cross-table candidate matches), `target_id` (ID column in `target`,
  defaults to `id`), `include_string_sim` (logical; when `TRUE`
  (default) emits `sim_sf_<col>` / `sim_fs_<col>` per column via
  [`stringdist::stringsim()`](https://rdrr.io/pkg/stringdist/man/stringsim.html) -
  requires the `stringdist` suggested package), `method` (stringdist
  method applied to every column, default `"jw"`. Only a scalar is
  honoured today; the argument shape also reserves a named character
  vector for per-column methods, the additive path to the per-column
  comparators a future probabilistic strategy will use), and
  `include_block_stats` (logical; whether to compute `cnt` / `icnt` /
  `ipos`). The
  [`Search_Strategy`](https://edubruell.github.io/joinery/reference/search_strategy.md)
  method additionally accepts `top_n` (named integer / list controlling
  per-column top-N counts for the `m_/f_/s_` columns; use a `default`
  entry as fallback; set a column to 0 to suppress its set). The
  [`Embedding_Strategy`](https://edubruell.github.io/joinery/reference/embedding_strategy.md)
  method emits `cosine_sim` (pass-through of `score`) and
  `embedding_norm_s` / `embedding_norm_f` (L2 norms of the
  **pre-normalization** embeddings, recomputed only over the matched
  record subset).

## Value

A `Match_Features` object wrapping a wide feature `data.table`.

## Examples

``` r
strat <- search_strategy(
  workshop   ~ normalize_text() + word_tokens(min_nchar = 3),
  proprietor ~ normalize_text() + word_tokens(min_nchar = 2),
  block_by  = c("postcode_area", "trade"),
  threshold = 0.30
)
matches <- search_candidates(
  workshop_listings, workshop_register,
  base_id = "listing_id", target_id = "reg_no", strategy = strat
)
# One row per pair, with the features a filter can learn from.
feats <- match_features(matches, strat,
                        base = workshop_listings, id = "listing_id",
                        target = workshop_register, target_id = "reg_no")
feats
#> 
#> ── Match_Features (token) ──────────────────────────────────────────────────────
#> strategy_class: "Search_Strategy" n_pairs: "965" n_features: "46"
#> strategy columns: workshop and proprietor
#> preview
#>    searched     found match_id  stage score   cnt  icnt  ipos  scnt  rcnt
#>      <char>    <char>    <int> <char> <num> <int> <int> <num> <int> <int>
#> 1:   L00018 GMC-H0521        1   <NA>     1     2     2   0.5     4     1
#> 2:   L00018 GMC-H0522        2   <NA>     1     2     2   0.5     4     1
#> 3:   L00734 GMC-00004        3   <NA>     1     1     1   1.0     3     1
#> 4:   L00384 GMC-00005        4   <NA>     1     1     1   1.0     4     1
#> 5:   L00671 GMC-00013        5   <NA>     1     1     1   1.0     5     1
#>           r1        r2 m_workshop_1 m_workshop_2 m_workshop_3 m_workshop_4
#>        <num>     <num>        <num>        <num>        <num>        <num>
#> 1: 0.5908422 0.3470364    0.5908422    0.3733835    0.1406170           NA
#> 2: 0.5908422 0.3470364    0.5908422    0.3733835    0.1406170           NA
#> 3: 1.0000000 1.0000000    1.0000000    0.1781656    0.1142127           NA
#> 4: 1.0000000 1.0000000    1.0000000    0.3562388    0.1406170           NA
#> 5: 1.0000000 1.0000000    1.0000000    0.7877549    0.7877549    0.2004064
#>    m_workshop_5 m_proprietor_1 m_proprietor_2 m_proprietor_3 m_proprietor_4
#>           <num>          <num>          <num>          <num>          <num>
#> 1:           NA      0.3470364      0.3169893             NA             NA
#> 2:           NA      0.3470364      0.3169893             NA             NA
#> 3:           NA      1.0000000             NA             NA             NA
#> 4:           NA      1.0000000      0.4713661             NA             NA
#> 5:           NA      1.0000000      0.2123903             NA             NA
#>    m_proprietor_5 f_workshop_1 f_workshop_2 f_workshop_3 f_workshop_4
#>             <num>        <num>        <num>        <num>        <num>
#> 1:             NA           NA           NA           NA           NA
#> 2:             NA           NA           NA           NA           NA
#> 3:             NA           NA           NA           NA           NA
#> 4:             NA   0.07723766           NA           NA           NA
#> 5:             NA           NA           NA           NA           NA
#>    f_workshop_5 f_proprietor_1 f_proprietor_2 f_proprietor_3 f_proprietor_4
#>           <num>          <num>          <num>          <num>          <num>
#> 1:           NA             NA             NA             NA             NA
#> 2:           NA             NA             NA             NA             NA
#> 3:           NA      0.4713661             NA             NA             NA
#> 4:           NA             NA             NA             NA             NA
#> 5:           NA             NA             NA             NA             NA
#>    f_proprietor_5 s_workshop_1 s_workshop_2 s_workshop_3 s_workshop_4
#>             <num>        <num>        <num>        <num>        <num>
#> 1:             NA           NA           NA           NA           NA
#> 2:             NA           NA           NA           NA           NA
#> 3:             NA           NA           NA           NA           NA
#> 4:             NA           NA           NA           NA           NA
#> 5:             NA           NA           NA           NA           NA
#>    s_workshop_5 s_proprietor_1 s_proprietor_2 s_proprietor_3 s_proprietor_4
#>           <num>          <num>          <num>          <num>          <num>
#> 1:           NA             NA             NA             NA             NA
#> 2:           NA             NA             NA             NA             NA
#> 3:           NA             NA             NA             NA             NA
#> 4:           NA             NA             NA             NA             NA
#> 5:           NA             NA             NA             NA             NA
#>    s_proprietor_5 sim_sf_workshop sim_sf_proprietor sim_fs_workshop
#>             <num>           <num>             <num>           <num>
#> 1:             NA       0.5294118         1.0000000       0.5294118
#> 2:             NA       0.5294118         1.0000000       0.5294118
#> 3:             NA       0.9615385         0.5357143       0.9615385
#> 4:             NA       0.8621693         1.0000000       0.8621693
#> 5:             NA       0.3990148         1.0000000       0.3990148
#>    sim_fs_proprietor
#>                <num>
#> 1:         1.0000000
#> 2:         1.0000000
#> 3:         0.5357143
#> 4:         1.0000000
#> 5:         1.0000000
```
