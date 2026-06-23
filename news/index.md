# Changelog

## joinery 1.0.0

First stable release. The token core, the DuckDB backend, embedding
matching, diagnostics, and calibration are feature-complete and the
public API is stable. This release adds the documentation that makes the
package usable end to end.

#### Documentation

- **Reference site** built with pkgdown: a grouped function index, a
  getting-started vignette, a concept glossary, and five how-to articles
  (fuzzy and exact strategies, matching across years and sources,
  calibration, embeddings, and working at scale with DuckDB).
- **Runnable examples** on every entry-point verb.

#### Example data

- **`workshop_register`**, **`workshop_listings`**,
  **`workshop_panel`**, **`match_labels_example`**: synthetic
  woodworking-workshop data with planted difficulty tiers and
  ground-truth links, used throughout the articles. Each tier
  (containment, movers, phonetic twins, hub tokens) has a minority that
  measurably benefits from the feature it exercises.

------------------------------------------------------------------------

## joinery 0.9.0

### Phase 0.9: Staged linkage and region-free matching

Staged entity resolution, region-free linking across blocks, and an
always-on cost guard, plus embedding reuse and faster preparers.

#### Staged entity resolution

Run strategies in order, carry residuals forward, resolve entities once
at the end.

- **[`multi_stage_dedup()`](https://edubruell.github.io/joinery/reference/multi_stage_dedup.md)**
  and
  **[`multi_stage_search()`](https://edubruell.github.io/joinery/reference/multi_stage_search.md)**:
  run an ordered list of strategies as successive passes.
  [`multi_stage_dedup()`](https://edubruell.github.io/joinery/reference/multi_stage_dedup.md)
  finds duplicates within one table;
  [`multi_stage_search()`](https://edubruell.github.io/joinery/reference/multi_stage_search.md)
  links records across tables, or across years of one pooled table with
  `self = TRUE`. Both take a mix of exact, fuzzy, and embedding
  strategies;
  [`multi_stage_search()`](https://edubruell.github.io/joinery/reference/multi_stage_search.md)
  supports collapse-and-continue so a slowly drifting name links one
  step at a time. Renames `multi_stage_match()`.
- **[`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md)**:
  identical-token-set matching as a strategy, for a cheap first pass.
  Runs through
  [`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md)
  and
  [`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md)
  like any strategy. Optional containment matches a subset rather than
  an exact set, with a per-column `min_containment_tokens` floor.
- **[`resolve_entities()`](https://edubruell.github.io/joinery/reference/resolve_entities.md)**:
  group an edge list into entities (connected components) and pick a
  representative per group.
- **[`materialize_records()`](https://edubruell.github.io/joinery/reference/materialize_records.md)**:
  fetch the original rows for a set of ids, the complement of
  [`extract_unmatched()`](https://edubruell.github.io/joinery/reference/extract_unmatched.md).
- **[`plan_strategy()`](https://edubruell.github.io/joinery/reference/plan_strategy.md)**:
  compare blocking keys before matching. Reports each candidate’s block
  sizes, comparison cost, and how many true twins stay co-blocked,
  without computing any scores.
- **[`rarity_distribution()`](https://edubruell.github.io/joinery/reference/rarity_distribution.md)**:
  report a column’s token frequency and rarity before matching, with a
  suggested `min_rarity`.
- **[`find_stopwords()`](https://edubruell.github.io/joinery/reference/find_stopwords.md)**:
  list a column’s high-frequency, low-information tokens for
  [`filter_stopwords()`](https://edubruell.github.io/joinery/reference/filter_stopwords.md).
- **[`duckdb_control()`](https://edubruell.github.io/joinery/reference/duckdb_control.md)**:
  one object for DuckDB execution tuning (batch size, scoring chunk key,
  per-chunk failure policy, progress), passed as `control =`. Replaces
  the loose batch arguments.

#### Region-free linking

Follow an entity across geographic blocks (movers, name drift, year to
year) without giving up block-based cost control.

- **[`block_on_tokens()`](https://edubruell.github.io/joinery/reference/block_on_tokens.md)**:
  block on a record’s own rare name tokens instead of a fixed key, so
  two records sharing any rare token are compared wherever they sit. Mix
  it with plain column names in `block_by`.
- **`rarity_scope = "global"`**: measure rarity across the whole corpus,
  so a distinctive name reads as strong evidence in any block and a
  common one stays weak.

#### Fan-out guard

- **`max_fanout` / `on_fanout`**: an automatic ceiling on comparison
  cost. When a hot or boilerplate token would fan a dense block into a
  near-quadratic join, joinery drops the offending tokens with a warning
  (`"cap"`, the default) or stops (`"abort"`). On by default. Replaces
  `max_comparisons`.

#### Embedding reuse

- **Embed once**: the data.table and tibble backends cache embedding
  vectors per session, so a multi-stage run no longer re-embeds a record
  on every pass. Keyed by model and text. Set
  `joinery.embedding_cache_dir` to persist across sessions, or
  `joinery.embedding_reuse = FALSE` to opt out.
- **[`clear_embedding_cache()`](https://edubruell.github.io/joinery/reference/clear_embedding_cache.md)**:
  empty the cache, optionally on disk too.
- **Faster scorer**:
  [`score_embeddings()`](https://edubruell.github.io/joinery/reference/score_embeddings.md)
  scores all pairs in a block as one matrix product, dropping a few
  hundred thousand pairs from seconds to a fraction of a second.

#### Preparers

- **[`drop_short_tokens()`](https://edubruell.github.io/joinery/reference/drop_short_tokens.md)**:
  drop tokens below a length, useful after phonetic encoding.
- **Phonetic encoders on tokens**:
  [`as_cologne()`](https://edubruell.github.io/joinery/reference/as_cologne.md),
  [`as_soundex()`](https://edubruell.github.io/joinery/reference/as_soundex.md),
  [`as_metaphone()`](https://edubruell.github.io/joinery/reference/as_metaphone.md),
  and `as_nysiis()` now encode token lists as well as raw strings, so
  you can encode after tokenizing.
- **[`normalize_street()`](https://edubruell.github.io/joinery/reference/normalize_street.md)**
  gains `drop_house_numbers` and `drop_stopwords` to strip address
  noise.
- **Faster preparers**:
  [`word_tokens()`](https://edubruell.github.io/joinery/reference/word_tokens.md),
  [`filter_stopwords()`](https://edubruell.github.io/joinery/reference/filter_stopwords.md),
  [`drop_numeric_tokens()`](https://edubruell.github.io/joinery/reference/drop_numeric_tokens.md),
  [`token_shapes()`](https://edubruell.github.io/joinery/reference/token_shapes.md),
  and
  [`extract_initials()`](https://edubruell.github.io/joinery/reference/extract_initials.md)
  now run group-wise over token tables.

#### Scoring and validation

- **Missing-column reweighting**: when a column is empty for a record,
  its weight is shared among the present columns rather than dropped, so
  scores stay in range.
- **Earlier errors**:
  [`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md)
  rejects overlapping id spaces and
  [`prepare_search_data()`](https://edubruell.github.io/joinery/reference/prepare_search_data.md)
  rejects duplicate ids, both of which corrupt results silently
  otherwise.

#### Bug fixes

- [`resolve_entities()`](https://edubruell.github.io/joinery/reference/resolve_entities.md)
  no longer drops singletons when ids mix integer and double forms.
- [`summarise_matches()`](https://edubruell.github.io/joinery/reference/summarise_matches.md)
  (DuckDB) no longer produces an out-of-range histogram bin for scores
  just above 1.0.
- [`drop_joinery_temp_tables()`](https://edubruell.github.io/joinery/reference/drop_joinery_temp_tables.md)
  is now exported.

------------------------------------------------------------------------

## joinery 0.8.0

### Phase 0.8: Stability and quality

Internal consolidation after the calibration work, plus fixes surfaced
by a full-scale Yellow-Pages panel build. Output schemas unchanged.

- **Token-set scoring**: a token repeated within one record no longer
  inflates a pair’s score; scores stay within `[0, sum(weights)]`.
- **DuckDB at scale**: connected components run per block instead of one
  global recursion that exhausted memory at corpus scale; empty dedup
  results carry the full schema; filtered lazy inputs
  (`tbl |> filter(...)`) are accepted everywhere.
- **`summarise_matches(entity_cols =)`**: count duplicate groups whose
  listed columns are single-valued, separating real stopword clusters
  from cardinality artefacts.
- **Consistent errors**: unified on
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  with `rlang` argument checks across exported verbs.
- **File layout**: `R/` reorganised under an eight-prefix naming scheme
  (see `CLAUDE.md`).

------------------------------------------------------------------------

## joinery 0.7.0

### Phase 0.7: Error calibration

An optional post-match filter that learns to drop false positives from a
small labelled sample. The same verb works on token and embedding
strategies.

- **[`match_features()`](https://edubruell.github.io/joinery/reference/match_features.md)**:
  build a one-row-per-pair feature table from a match result, with
  token-overlap counts, auxiliary-side informativeness (`aIP`, after
  Doherr 2023), and string similarities.
- **[`fit_filter()`](https://edubruell.github.io/joinery/reference/fit_filter.md)
  /
  [`apply_filter()`](https://edubruell.github.io/joinery/reference/apply_filter.md)**:
  fit a logistic false-positive filter and apply it, choosing a
  threshold by Youden’s J unless you set one.
- **[`calibrate_matches()`](https://edubruell.github.io/joinery/reference/calibrate_matches.md)**:
  one verb composing features, fit, and apply.
- **[`calibrate()`](https://edubruell.github.io/joinery/reference/calibrate.md)**:
  evaluate a fitted filter on a labelled set; returns reliability, Brier
  score, log-loss, confusion matrix, and a threshold sweep.
- **Labelling helpers**:
  [`sample_matches()`](https://edubruell.github.io/joinery/reference/sample_matches.md)
  stratification, plus
  [`export_for_labelling()`](https://edubruell.github.io/joinery/reference/export_for_labelling.md)
  /
  [`import_labels()`](https://edubruell.github.io/joinery/reference/import_labels.md)
  for a CSV round-trip.
- **Tidymodels support**: pass a parsnip spec or workflow to
  [`fit_filter()`](https://edubruell.github.io/joinery/reference/fit_filter.md)
  via
  [`joinery_recipe()`](https://edubruell.github.io/joinery/reference/joinery_recipe.md).
  All tidymodels packages are optional; the glm path needs none.

------------------------------------------------------------------------

## joinery 0.6.0

### Phase 0.6: Diagnostics

Verbs to answer four questions about a strategy and its results: will it
work, did it work, why this pair, and where to look.

- **[`audit_strategy()`](https://edubruell.github.io/joinery/reference/audit_strategy.md)**:
  grade a strategy before matching.
- **[`summarise_matches()`](https://edubruell.github.io/joinery/reference/summarise_matches.md)**:
  overview of a dedup or candidate result, unified across backends.
- **[`explain_match()`](https://edubruell.github.io/joinery/reference/explain_match.md)**:
  per-token attribution of a single pair’s score.
- **[`sample_matches()`](https://edubruell.github.io/joinery/reference/sample_matches.md)**:
  draw pairs by mode (high, low, borderline, ambiguous, top-gap,
  random).
- **[`compare_stages()`](https://edubruell.github.io/joinery/reference/compare_stages.md)**:
  per-stage coverage for multi-stage workflows.
- **Diagnostic plots**: a family of pipe-composable `tinyplot`
  functions, one per view.
- **Recommendations**: strategies and results surface inline advice from
  a signal-driven catalog, also available via
  [`recommendations()`](https://edubruell.github.io/joinery/reference/recommendations.md).

------------------------------------------------------------------------

## joinery 0.5.0

### Phase 0.5: Embedding-Based Matching

Optional semantic matching that complements rather than replaces the
token core. Use embeddings for fields where word-overlap fails
(paraphrases, multilingual variants, fuzzy free-text descriptions) and
combine them with token strategies via `multi_stage_match()`.

#### New Features

- **[`embedding_strategy()`](https://edubruell.github.io/joinery/reference/embedding_strategy.md)**:
  declarative strategy for embedding-based linkage, mirroring the
  ergonomics of
  [`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md).
  Specify one or more embedding columns, an optional `block_by`, an
  optional `threshold`, and an optional `weights` vector across
  embedding columns.
- **Cosine-similarity scoring** between record-level embedding vectors,
  with optional pre-normalization so cosine reduces to a fast inner
  product at scoring time. Strategies expose a `normalize` flag for
  users who want to keep raw magnitudes.
- **Drop-in compatibility with the existing verbs**:
  [`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md),
  [`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md),
  and
  [`extract_unmatched()`](https://edubruell.github.io/joinery/reference/extract_unmatched.md)
  all accept an `Embedding_Strategy` and return the standard joinery
  output schemas (`duplicate_group` / `match_id`, `score`, `rank`,
  original columns).
- **Multi-stage token + embedding workflows**: `multi_stage_match()`
  accepts a sequence of mixed `Search_Strategy` and `Embedding_Strategy`
  objects, threading residuals between stages and stopping early when
  either side is exhausted. Useful pattern: cheap token stage first,
  then embedding stage on the residual.
- **`block_by` support for embeddings** so cosine search runs within
  blocks (e.g. country, year bucket) instead of across the whole table.
- **Backend parity**: full implementation on data.table, DuckDB, and
  tibble / data.frame, with the same call signatures across backends.
  DuckDB scales embedding search to large tables via the existing batch
  infrastructure.
- **Embedding generation via `tidyllm`** (optional `Suggests`
  dependency): provider-agnostic helpers for Ollama, OpenAI, and other
  tidyllm-supported backends, so users can move from raw text to a
  matchable embedding column without leaving R.
- **Embedding-aware diagnostics groundwork**: strategy-class dispatch in
  place so Phase 0.6 diagnostics can specialise to embedding strategies
  without API churn.

#### Bug fixes

- DuckDB `block_by` SQL bug fixed.
- DuckDB lazy-query bug in multi-stage match fixed.

------------------------------------------------------------------------

## joinery 0.4.0

### Phase 0.4: Stability & Test-Quality Hardening

A maintenance release with no new user-facing features. The goal was to
harden the test suite and close coverage gaps before resuming feature
work on embeddings and diagnostics.

- `methods_duckdb.R` coverage raised from 34% to 90%; full behavioural
  parity with the data.table backend now exercised by tests.
- `embedding_methods_*` coverage raised to 95%+ on both data.table and
  DuckDB backends.
- Small-table `batch_duckdb` brittleness diagnosed and fixed (see
  `notes/batch_duckdb_brittleness.md`). User-facing impact: small inputs
  no longer hit pathological batching behaviour.
- Total package coverage: 87.25%. Remaining low-coverage files are
  intentional: S7 dispatch boilerplate, interactive-only progress paths,
  and live-embedding paths reserved for `local_tests/`.

------------------------------------------------------------------------

## joinery 0.3.1

- Fix `batch_duckdb` small-table brittleness.

------------------------------------------------------------------------

## joinery 0.3.0

### Phase 3: SearchEngine Heuristics

This release implements advanced matching heuristics that significantly
improve accuracy and robustness.

#### New Features

- **rIP Smoothing**: Four smoothing methods for token weights:
  - `smoothing(method = "log")`: Log transformation
  - `smoothing(method = "softmax", temperature = 1.0)`: Softmax with
    temperature
  - `smoothing(method = "offset", alpha = 0.1)`: Additive smoothing
  - `smoothing(method = "none")`: No smoothing (default)
- **Containment**: Control maximum matches per record:
  - `max_candidates` parameter limits top-N matches
  - Prevents one-token overmatching
  - Works with threshold filtering
- **Feedback Weighting**: Penalize low token overlap:
  - `feedback_strength` parameter (0-1) controls intensity
  - Reduces noise in partial matches
  - Rewards comprehensive token overlap

#### DuckDB Backend

- Unified `.score_pairs_sql()` helper consolidates scoring logic
- All Phase 3 features supported in DuckDB backend
- Used by both
  [`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md)
  and
  [`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md)

#### Backend Improvements

- Both data.table and DuckDB backends support all Phase 3 features
- Full test coverage for all smoothing, containment, and feedback
  methods
- 454 tests passing

------------------------------------------------------------------------

## joinery 0.2.0

### Phase 2: DuckDB Backend

- Full DuckDB backend implementation
- Scalable processing of datasets up to 50M rows
- Batch-based processing with R preprocessing pipeline
- Feature parity between data.table and DuckDB backends
- All core generics working on both backends

------------------------------------------------------------------------

## joinery 0.1.0

- Initial release
- data.table backend
- Token-based record linkage
- Basic preprocessing pipeline
- S7 class system
