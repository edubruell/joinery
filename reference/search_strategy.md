# Define a Search Strategy for Record Linkage

Creates a Search_Strategy object that specifies how columns should be
preprocessed for token index based record linkage, along with optional
weights, blocking variables, rarity computation method, rIP smoothing,
and similarity threshold.

## Usage

``` r
search_strategy(
  ...,
  block_by = NULL,
  weights = numeric(),
  rarity = "inverse_freq",
  rarity_scope = c("block", "global"),
  min_rarity = 0,
  max_token_df = Inf,
  threshold = 0.9,
  smoothing = smooth_rip_identity(),
  max_candidates = Inf,
  max_fanout = 5e+07,
  on_fanout = c("cap", "abort", "off"),
  feedback_strength = 0,
  on_missing = c("penalise", "renormalise")
)
```

## Arguments

- ...:

  Two sided formulas of the form `column ~ preprocessing_steps`. The
  left hand side names the column; the right hand side contains one or
  more function calls to apply in sequence (for example
  `name ~ normalize_text() + word_tokens(min_nchar = 3)`).

- block_by:

  Optional character vector of column names to use for blocking.
  Candidate searches will be restricted to records sharing the same
  blocking key values. Default is `NULL` (no blocking).

- weights:

  Optional named numeric vector of weights for similarity scoring. Names
  should correspond to columns. Default is
  [`numeric()`](https://rdrr.io/r/base/numeric.html) (uniform weights).

- rarity:

  Character scalar choosing how a token's rarity (its informativeness,
  the weight it carries in scoring) is computed from token counts. A
  shared rare token is strong evidence two records match; a shared
  common one is weak. The four methods differ in how hard they push
  common tokens down. Let `f` be the token's frequency, `df` its
  document frequency (how many records contain it), and `N` the record
  count, all measured over the scope set by `rarity_scope`. One of:

  `"inverse_freq"`

  :   (default) `rarity = 1 / f`. Simple and robust: a token seen once
      is maximally rare, one seen often counts for little. A good first
      choice and what every other article uses.

  `"smoothed_inverse_freq"`

  :   `rarity = 1 / (f + 1)`. The same shape, damped so the very rarest
      tokens do not dominate quite as sharply. Reach for it when
      single-occurrence tokens (often typos) are swinging scores too
      much.

  `"tfidf"`

  :   `rarity = tf * idf` with `tf = f / sum(f)` and
      `idf = log(1 + N / df)`. Weighs a token by how few records carry
      it, not just its raw count. Use when the same token recurs at very
      different rates across columns or blocks and you want document
      spread to matter.

  `"bm25"`

  :   `rarity = log((N - df + 0.5) / (df + 0.5))`, the Okapi BM25
      inverse-document-frequency term. The most aggressive: it drives
      common tokens toward zero and turns *negative* for a token in more
      than half the records, which actively penalises boilerplate. Use
      on corpora thick with shared legal-form or trade words ("Ltd",
      "Joinery") that you want suppressed hard.

  Default is `"inverse_freq"`; most linkages never need to change it.
  Inspect the token distribution with
  [`rarity_distribution()`](https://edubruell.github.io/joinery/reference/rarity_distribution.md)
  before switching, and note `rarity_scope` decides whether these counts
  are block-local or corpus-wide.

- rarity_scope:

  Character scalar, `"block"` (default) or `"global"`, selecting the
  scope over which a token's rarity (informativeness) is measured.
  `"block"` measures rarity within each block (the historical and only
  previous behaviour). `"global"` measures it across the whole corpus,
  so a token's informativeness no longer depends on which block it lands
  in. This is the chain defence for region-free linking: a globally
  common name (think a franchise) gets low global rarity and is dropped
  by `min_rarity`, while a distinctive brand reads as a strong link
  signal regardless of where it appears. Only the rarity metric and the
  `min_rarity` gate follow this argument; the cost axis (block-local
  `df`, `max_token_df`, the fan-out guard) stays block-local under both
  scopes.

- min_rarity:

  Numeric scalar specifying the minimum rarity value required for a
  token to be included in similarity scoring. Tokens with rarity below
  this threshold are filtered out. Default is `0`.

- max_token_df:

  Numeric scalar specifying the maximum raw document frequency a token
  may have within its `(block, column)` to be kept. Tokens appearing in
  more than `max_token_df` records are dropped *before* the
  token-overlap join, so a single hyper-common token (a house number,
  `STRASSE`) can't fan out a block even at `min_rarity = 0`. The blunt
  document-frequency companion to the rarity-metric `min_rarity`; the
  two cut on different axes and compose. Default is `Inf` (off). See
  [`rarity_distribution()`](https://edubruell.github.io/joinery/reference/rarity_distribution.md)
  to choose a value from the token distribution.

- threshold:

  Numeric scalar specifying the minimum relative identification
  potential required for two records to be considered matches. Default
  is `0.9`.

- smoothing:

  A `Smoothing` object created by one of the
  [smooth_rip](https://edubruell.github.io/joinery/reference/smooth_rip.md)
  helpers that controls how rIP values are smoothed before scoring.
  Default is
  [`smooth_rip_identity()`](https://edubruell.github.io/joinery/reference/smooth_rip.md).

- max_candidates:

  Numeric scalar specifying the maximum number of candidate matches to
  retain per record. Default is `Inf` (no limit). When finite, only the
  top `max_candidates` highest scoring matches are kept per record.

- max_fanout:

  Numeric scalar. The always-on guard against a single hot or
  boilerplate token (think a directory publisher's name, or a stopword
  that slipped through) fanning one block into a huge number of pairwise
  comparisons. This is the same failure `min_rarity` / `max_token_df`
  address, but on by default. It caps the estimated number of record
  pairs the token-overlap join will form, predicted cheaply from token
  frequencies before the join runs (no pairs are built to measure it).
  Default `5e7`; set `Inf` (or `on_fanout = "off"`) to disable. Use
  [`rarity_distribution()`](https://edubruell.github.io/joinery/reference/rarity_distribution.md)
  or
  [`plan_strategy()`](https://edubruell.github.io/joinery/reference/plan_strategy.md)
  to pick a value for your data.

- on_fanout:

  What to do when the estimated fan-out exceeds `max_fanout`: `"cap"`
  (default) auto-drops the smallest set of hyper-common tokens needed to
  get under budget (they carry near-zero rarity, so scores barely move)
  and emits a loud warning naming what was dropped; `"abort"` stops with
  an actionable error instead; `"off"` disables the guard entirely.

- feedback_strength:

  Numeric scalar controlling feedback weighted scoring. Default is `0`
  (disabled). Positive values adjust scores based on the proportion of
  matched tokens.

- on_missing:

  How to score a pair when a weighted column is **empty on both
  records**. With `"penalise"` (default) the column still counts against
  the score. For example, if `Strasse` has weight 0.3 and a record's
  street is blank, that record's score can never rise above 0.7, so a
  threshold of 0.8 will never match it even on a perfect name.
  `"renormalise"` removes that ceiling: it spreads the weight of any
  column blank on *both* sides across the columns that are present (a
  column present on only one side still counts as a genuine mismatch).
  This is powerful but aggressive, since it turns a record with no
  street into a name-only matcher, so it is opt-in and never the
  default. If you mainly want to handle empty columns, the safer route
  is to run an
  [`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md)
  stage first, whose matches do not depend on weights or thresholds.

## Value

A Search_Strategy object.

## Examples

``` r
# Tokenize two name columns, block on region, keep pairs scoring at least 0.8.
strat <- search_strategy(
  Nachname ~ normalize_text() + word_tokens(min_nchar = 3),
  Vorname  ~ normalize_text() + word_tokens(min_nchar = 3),
  block_by  = "Kreis",
  threshold = 0.8
)
strat
#> <joinery::Search_Strategy>
#> 
#> columns
#> Nachname: normalize_text() -> word_tokens(min_nchar = 3)
#> Vorname: normalize_text() -> word_tokens(min_nchar = 3)
#> 
#> blocking: Kreis
#> weights: none
#> rarity: inverse_freq (min=0)
#> fan-out guard: cap at 50,000,000
#> smoothing: none
#> threshold: 0.8
#> max_candidates: none
#> feedback_strength: none
```
