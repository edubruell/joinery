# joinery 0.9.0

## Phase 0.9: Staged Linkage, Region-Free Matching, and Documentation

Delivers the Part-A staged-entity-resolution spine built during the v0.8 development cycle, two new production features (region-free linking and the fan-out guard), a batch of preparer improvements, and the first public-facing documentation (getting-started vignette, pkgdown site, example data).

### Staged entity resolution (the Part-A spine)

The central feature of this cycle: a composable, multi-pass linkage kernel that threads residuals between stages and resolves connected components once at the end.

* **`resolve_entities()`**: connected-components kernel over an edge table. Accepts any `(id_a, id_b[, score])` data.table; returns a group table with a `rep` column identifying each component's representative. Optional `vertices =` adds singletons not present in any edge. Optional `rep_by =` chooses representatives by a named column (e.g. `"score"`) rather than by smallest id.
* **`find_stopwords()`**: corpus-frequency helper that identifies token-level stopwords (high-df terms that carry no discriminating power) for a given column, suitable for supplying to `filter_stopwords()` or for inspecting the token distribution.
* **`exact_strategy()` / `Exact_Strategy`**: score-1.0 token-set matching as a first-class strategy class (not a verb). Dispatches through the standard apply verbs (`detect_duplicates()`, `search_candidates()`), returns the standard output schema with `score == 1.0`, and is composable in `multi_stage_dedup()` / `multi_stage_search()` as an exact front. The fingerprint rides `prepare_search_data()`; residual extraction uses the existing `extract_unmatched()`. Both data.table and DuckDB backends.
* **`materialize_records()`**: rehydrate-by-id semi-join generic. Given a data frame and a vector of ids, returns the original rows for those ids; the complement of `extract_unmatched()` for selective retrieval. All three backends.
* **`rarity_distribution()`**: pre-match, scoring-free diagnostic. Runs `prepare_search_data()` + `compute_rarity()` without the overlap join, and reports the per-column df/rarity distribution plus the top-df offender list (fan-out drivers). Returns a `Rarity_Distribution` with a `suggested_min_rarity` per column. Use it to calibrate `min_rarity` / `max_token_df` before running search.
* **`duckdb_control()`**: unified DuckDB execution-tuning object. Replaces the old loose `target_batch_size` / `min_batch_size` / `chunk_strategy` arguments (clean break). Consolidates batch sizes, scoring chunk key, per-chunk failure policy (`on_error = "skip" | "retry" | "stop"`), and progress into one `Duckdb_Control` object passed as `control =`. Chunking is execution, not semantics: it is never a `Search_Strategy` slot. Block-atomic chunking in `search_candidates()` ensures that a block is never split across chunks (splitting one would drop within-block cross-pairs). Failed chunks are recorded in `attr(result, "failed_chunks")` for post-hoc inspection.
* **`multi_stage_dedup()` + `multi_stage_search()`**: two-face staged linkage over a shared engine (`R/internal_staging.R`). `multi_stage_dedup()` is the within-table face: successive passes accumulate edges, the residual carry-forward keeps each found group's representative as a bridge, and one final connected-components call resolves everything. `multi_stage_search()` is the cross-source face (directed search; hard rename of the former `multi_stage_match()`): matches entities across `base_table` / `target_table`, accumulates a directed ledger, and supports `collapse = "rep"` for collapse-and-continue bridging between stages. Both accept an ordered list of `Search_Strategy`, `Exact_Strategy`, or `Embedding_Strategy` objects. The `multi_stage_match()` name is removed; update call sites.
* **`plan_strategy()`**: pre-match, pre-strategy blocking planner. Upstream of `audit_strategy()`: helps you *choose* a blocking key rather than grade one. Given a base table, a strategy (preparer pipeline only; `block_by` is ignored), and a list of `block_candidates`, it measures each candidate on four axes without ever computing token overlap: blocking-resolution frontier (block count, size distribution, brute-pair cost estimate, exact-twin co-blocking recall), exact-set persister rate (A2 yield), residual structure, and per-column discriminativeness (rarity distribution + cost curve + empty-column score ceiling). Returns a `Strategy_Plan`; default `plot()` is `frontier_plot()`. DuckDB samples with `SELECT *` and delegates to data.table; no pairs touch the connection.

### Region-free linking

Recovers the same entity across different geographic blocks (movers, name drift, year-over-year) without abandoning block-based cost control.

* **`block_on_tokens(column, max_df, min_rarity, preparer, min_nchar)`**: token-blocking spec. Place it in the `block_by =` list of a `search_strategy()` alongside plain column names. In `prepare_search_data()`, each record is exploded against its own rare tokens of `column` into a derived `._btok` block column, so two records sharing any rare token co-block. The rarity/fan-out guard and the scoring join see `._btok` (via `.block_cols()`); entity resolution and DuckDB batch/chunk slicing use plain columns (via `.plain_block_cols()`) so a record matched under several block-tokens still resolves into one entity.
* **`rarity_scope = "global"`** on `search_strategy()`: global rarity. Under `"global"`, token rarity is measured corpus-wide rather than within each block; a distinctive brand reads as a strong signal anywhere, and a globally common name gets low global rarity (dropped by `min_rarity`). The cost axis (df, max_token_df, fan-out guard) stays block-local regardless; only the rarity metric and the `min_rarity` distinctiveness floor follow `rarity_scope`. Both backends; the `explain_match` round-trip contract holds under global scope.

### Always-on fan-out guard

* **`max_fanout` / `on_fanout`** on `search_strategy()`: automatic cost ceiling. Estimates the overlap join's intermediate-row count from the pairs-free df histogram (`Σ df·(df−1)` for self / `Σ df_b·df_t` for cross) and, when it busts `max_fanout` (default `5e7`), auto-derives a `df ≤ cut` ceiling dropping the smallest set of near-zero-rarity hot tokens (`on_fanout = "cap"`, loud `cli_warn`) or aborts (`"abort"`); `"off"` disables. The cut is on the same df axis as `max_token_df` and identical on both backends. Supersedes the old dedup-only `max_comparisons` argument (removed; clean break). Unlike `min_rarity` / `max_token_df`, the guard is always on by default: it is the protection against a hot token fanning a dense block into a quadratic join.

### Exact strategy enhancements

* **Per-column `min_containment_tokens`** on `exact_strategy()`: a per-column minimum number of tokens the base record must contribute to a match for it to count as a containment hit. Prevents a single shared hub token from triggering a false-positive containment link when the base record has very few tokens.
* **Feedback framing improvements**: the feedback term in the exact-strategy scoring is now framed consistently with the token-scoring kernel so contributions are comparable between exact and fuzzy stages in a multi-stage workflow.

### Embedding reuse

* **Embed once, reuse on later calls**: the data.table and tibble backends now keep a per-session cache of embedding vectors, so a multi-stage run no longer re-embeds the same record on every stage (embedding is roughly 2000x more expensive than the retrieval it feeds). The cache is keyed by model and record content, so a record whose text changed re-embeds on its own. This brings the in-memory backends in line with the DuckDB backend, which already reused through its persisted `embeddings` column.
* **Two new package options**: `joinery.embedding_reuse` (default `TRUE`; set `FALSE` to embed fresh every time) and `joinery.embedding_cache_dir` (unset by default; set a path to persist the cache across R sessions). See `?joinery` for the full description.
* **`clear_embedding_cache(disk = FALSE)`**: new exported helper to empty the in-session cache, and optionally the on-disk cache.

### New and improved preparers

* **`drop_short_tokens(min_nchar = 2)`**: new preparer. Drops tokens shorter than `min_nchar` characters from a token list-column. Useful after phonetic encoding (encoders can produce short codes that act as hub tokens) or to strip uninformative single-character tokens.
* **Phonetic encoders accept token list-columns**: `as_cologne()`, `as_soundex()`, `as_metaphone()`, and `as_nysiis()` now work on both raw character columns (string → code) and token list-columns produced by `word_tokens()` (token → code per element). This lets you apply phonetic encoding after tokenization rather than before, which is usually the right order.
* **Vectorized `word_tokens()` + `filter_stopwords()`**: rewritten to operate group-wise on data.table token tables instead of row-by-row; throughput improvement roughly proportional to group size.
* **Vectorized `drop_numeric_tokens()`, `token_shapes()`, `extract_initials()`**: same treatment; batch-safe for large token tables.
* **`normalize_street()`** gains `drop_house_numbers` and `drop_stopwords` arguments: strips house numbers and common address stopwords (e.g. `"Str."`, `"Straße"`) before normalization, reducing token noise on address fields.
* **DuckDB chunking vectorized**: `.chunk_block_consolidated()` is now per-block rather than per-row (`[.data.table` scoped), cutting overhead on wide block tables.

### Calibration and validation improvements

* **`on_missing` renormalization**: when a column's tokens are entirely absent for a record in a pair, the column's weight is redistributed among the present columns rather than silently dropped, so scores remain in `[0, 1]` regardless of missingness patterns.
* **Comparison ID pre-flight** (`D1`): `search_candidates()` now checks that `base_id` and `target_id` are non-overlapping before the overlap join; overlapping id spaces produce spurious self-links that are hard to diagnose downstream.
* **Non-unique-id pre-flight** (`D2`): `prepare_search_data()` errors early when the id column contains duplicates; downstream CC resolution assumes unique ids and silently gives wrong results otherwise.
* **Calibration robustness**: `fit_filter()` and `apply_filter()` handle edge cases in small labelled sets (zero-variance columns, all-positive or all-negative splits) without crashing.

### Example data

* **`workshop_register`** + **`workshop_listings`**: synthetic woodworking-workshop data (chamber register vs. marketplace listings) with planted difficulty tiers (`gen_tier`) and `actual_link` ground truth per pair. Each feature tier (exact front + containment blocker, region-free mover, phonetic twin, fan-out hub) has a planted minority that measurably wins when the corresponding feature is switched on. Tibble dispatch for `exact_strategy` and `materialize_records` was added while building these datasets.

### Documentation

* **Getting-started vignette** (`vignette("getting-started", package = "joinery")`): end-to-end walkthrough on `base_example` / `target_example`: preprocessing, dedup, cross-table search, multi-stage exact+fuzzy, `explain_match`, precision/recall trade-off sweep. Tibble-first; `eval = TRUE`; fast enough for `R CMD check`.
* **pkgdown site**: `_pkgdown.yml` with Litera bootswatch, grouped reference index (61 exports + 6 datasets in pipeline order), and favicons. `check_pkgdown()` clean. Build the site locally with `pkgdown::build_site()`; CI/GitHub Pages wiring deferred until the repo goes public.
* **Roxygen markdown enabled**: `DESCRIPTION` carries `Roxygen: list(markdown = TRUE)`; all docstrings can now use standard Markdown formatting.
* **Preparer docstring overhaul**: Tier-A verb descriptions rewritten; mechanical jargon swept; `@examples` added for `detect_duplicates()`, `search_candidates()`, and `resolve_entities()`.

### Bug fixes

* **`resolve_entities()`**: round-number numeric ids (e.g. `1e5`) are now coerced to plain decimal strings before the graph join, preventing a class-mismatch that dropped singletons when ids mixed integer and double representations.
* **`summarise_matches()`** (DuckDB): score-histogram bin boundaries are clamped so floating-point values just above `1.0` land in the top bin rather than producing an underflow bin outside `[0, 1]`.
* **`drop_joinery_temp_tables()`**: was missing `@export` despite having full roxygen documentation; it is now callable as documented.
* **Exact set-equality kernel** (DuckDB): replaced the `O(N²)` self-join with a `GROUP BY` fingerprint aggregation; no change in results, substantial speedup on dense blocks.

### Tests

* `R CMD check` clean; full test suite passes.
* DuckDB `resolve_entities` paths for empty-without-vertices and `vertices` singleton-fold now covered.

---

# joinery 0.8.0

## Phase 0.8: Stability & Quality Assurance

Consolidates the internals after the 0.7 calibration work, with a focus on consistent error reporting, validation, and code organisation. Public output schemas are unchanged; one new optional argument on `summarise_matches()`. Includes a batch of bug fixes surfaced by the YP-panel practicality test.

### Internal Improvements

* **Unified error reporting**: migrated `stop()` / `rlang::abort()` call sites to `cli::cli_abort()` for consistent, formatted diagnostics across the package.
* **Validation shims**: adopted `rlang` `check_*()` helpers at exported entry points for uniform argument checking.
* **`purrr` shim**: replaced ad-hoc functional patterns with the vendored standalone `purrr` shim in user-facing glue code.
* **File layout**: reorganised `R/` under an eight-prefix naming schema (`strategy_`, `preparer_`, `generics_`, `methods_<backend>_<stage>`, `embedding_methods_`, `diagnostic_`, `calibration_`, `internal_`); see `CLAUDE.md` for the legend.
* **Readability pass**: extracted repeated constants and tightened internal helpers.

### Bug fixes surfaced by the YP practicality test

* **DuckDB dedup empty-result schema**: `detect_duplicates()` on a DuckDB tbl now returns the full base schema with zero rows when no pairs cross threshold, so per-block results can be `UNION`-ed without a column-count mismatch.
* **Filtered lazy DuckDB inputs**: every user-facing DuckDB method (`detect_duplicates`, `search_candidates`, `prepare_search_data`, `extract_unmatched`, `audit_strategy`, `summarise_matches`, embedding equivalents) now accepts `tbl(con, "x") |> filter(...)` inputs. Filtered lazies are silently materialised to a TEMP TABLE.
* **Block-aware connected components in DuckDB dedup**: the recursive CTE now iterates per block instead of running one global recursion, which previously OOMed on disk at corpus scale. Result objects carry an `attr("wall_seconds")` for downstream wall-clock budgeting.
* **Scoring now uses token SETS, not bags**: a token repeated within one record's column (e.g. `"Fritzel … Fritzel … Fritzel"`) previously inflated a pair's score through the token-overlap self-join, producing scores above the `sum(weights)` ceiling (up to 2.8 on real Yellow-Pages data) and breaking the `[0, 1]`-style threshold semantics. The scoring path on both backends now collapses within-record token multiplicity before computing rIP and the overlap join, so `score ∈ [0, sum(weights)]`. The fix is confined to scoring (`.score_pairs_sql`, `.score_token_pairs`, and `explain_match`'s attribution, which stay in lock-step for the round-trip contract); rarity is untouched, so `inverse_freq` keeps its corpus term-frequency definition. The change is a no-op for records whose tokens are already distinct.

### New optional API

* `summarise_matches(..., entity_cols = c(...))`: when supplied, counts duplicate groups whose listed columns are single-valued and fires a new `cluster_identical_name_street` recommendation that shadows the generic `duplicates_mega_cluster` advice. Useful for distinguishing real "stopword" clusters from upstream cardinality artefacts.
* Recommendations catalog: new `suppresses` field lets a firing rule shadow another rule's message.

### Tests

* `R CMD check` clean; full test suite passes (1741 PASS / 0 FAIL).
* New `local_tests/yp_dedup_smoke.R` exercises the four fixes end-to-end on a real YP slice.
* New `tests/testthat/test-score-token-set-semantics.R` pins the set-semantics scoring bound, backend parity, and the explain round-trip on a multiplicity pair.

---

# joinery 0.7.0

## Phase 0.7: Ex Post ML & Error Calibration

Optional, post-match false-positive filter learned from a small labelled sample. The same verb dispatches on `Search_Strategy` and `Embedding_Strategy` (reduced feature set for the latter).

### New Features

* **`match_features()`**: builds a wide one-row-per-pair feature `data.table` from a joinery match result. Token strategies expose the full schema (`scnt`, `rcnt`, `r1..rn`, `m_/f_/s_` top-N aIP blocks, string similarities); embedding strategies expose a reduced schema with `cosine_sim` and pre-normalization L2 norms.
* **`aIP` primitive**: Doherr (2023) eq. (9) auxiliary-side token informativeness, computed via the internal `prepare_auxiliary_registry()` generic (data.table + DuckDB + tibble parity) and the `compute_aip()` helper.
* **String similarity columns** `sim_sf_<col>` / `sim_fs_<col>` via `stringdist::stringsim()`; single global `method =` argument (default `"jw"`). Opt out with `include_string_sim = FALSE`.
* **`sample_matches()` stratification**: new `stratify_by` and `expand_to_block` modes for constructing balanced labelling sets.
* **`export_for_labelling()` / `import_labels()`**: CSV round-trip for manual labelling. Block-header rows pre-filled via `default_label`; format-agnostic, no UI shipped.
* **`fit_filter()` / `apply_filter()`**: logistic-regression baseline filter. `apply_filter()` picks the threshold via Youden's J on training data by default; results either enrich features or broadcast `tp_prob` / `predicted_tp` onto the raw matches table.
* **`calibrate_matches()`**: high-level verb composing `match_features()` then `fit_filter()` then `apply_filter()`. Dispatches on `(matches, strategy)` for data.table, DuckDB (collect-and-delegate), and tibble / data.frame inputs.
* **`calibrate()` + `Filter_Calibration`**: evaluates a fitted filter on a labelled set; returns reliability table, Brier score, log-loss, per-class confusion matrix, and threshold sweep curve.
* **Tidymodels shim**: `joinery_recipe()` + `fit_filter(model = <parsnip spec | fitted parsnip | (un)fitted workflow>)`. Fitted workflows detected via `workflows::is_trained_workflow()` so pre-fit workflows are not silently re-trained. All tidymodels packages live in `Suggests`; the baseline glm path is dependency-free.
* **Four new recommendations**: `consider_calibration_borderline`, `consider_calibration_ambiguity`, `calibration_low_n_warning`, `calibration_drift_warning`. `summarise_matches()` gains `threshold` / `borderline_epsilon` arguments on both backends.

### Tests

* 1706 PASS / 0 FAIL / 1 SKIP. `R CMD check` clean.

---

# joinery 0.6.0

## Phase 0.6: Diagnostics & Match Quality

Diagnostics organised around four user questions: *will it work?* (`audit_strategy`), *did it work?* (`summarise_matches`), *why this pair?* (`explain_match`), *where to look?* (`sample_matches`); plus multi-stage diagnostics (`compare_stages`).

### New Features

* **`summarise_matches()`**: `Match_Overview` unified across dedup / candidates via the `match_type` slot; data.table, DuckDB, and tibble / data.frame backends.
* **`audit_strategy()`**: pre-match strategy audit. Dispatches on strategy class: `Strategy_Audit` (token) or `Embedding_Audit` (embedding).
* **`explain_match()`**: per-token contribution attribution for `Search_Strategy`; pair + score only for `Embedding_Strategy`. The round-trip contract is enforced as a property test on both backends.
* **`sample_matches()`**: six modes (`high`, `low`, `borderline`, `ambiguous`, `top_gap`, `random`).
* **`compare_stages()`**: per-stage overviews, marginal coverage, `low_yield_stage` recommendation for multi-stage workflows.
* **Diagnostic plots**: 14 first-class `tinyplot` functions. Each plot is a separately named pipe-composable function; default `plot()` methods per diagnostic class call the most-useful single view. `tinyplot` is a hard `Imports` dependency.
* **Recommendations catalog** in `R/diagnostics_recommendations.R` links signals to thresholds to messages; surfaced via inline `cli` warnings in `print()` and the `recommendations(x)` accessor.

### Backend Improvements

* Embedding strategy diagnostics (M8) reach parity with token diagnostics where conceptually meaningful.
* Two DuckDB bugs fixed during `summarise_matches` hardening.

---

# joinery 0.5.0

## Phase 0.5: Embedding-Based Matching

Optional semantic matching that complements rather than replaces the token core. Use embeddings for fields where word-overlap fails (paraphrases, multilingual variants, fuzzy free-text descriptions) and combine them with token strategies via `multi_stage_match()`.

### New Features

* **`embedding_strategy()`**: declarative strategy for embedding-based linkage, mirroring the ergonomics of `search_strategy()`. Specify one or more embedding columns, an optional `block_by`, an optional `threshold`, and an optional `weights` vector across embedding columns.
* **Cosine-similarity scoring** between record-level embedding vectors, with optional pre-normalization so cosine reduces to a fast inner product at scoring time. Strategies expose a `normalize` flag for users who want to keep raw magnitudes.
* **Drop-in compatibility with the existing verbs**: `detect_duplicates()`, `search_candidates()`, and `extract_unmatched()` all accept an `Embedding_Strategy` and return the standard joinery output schemas (`duplicate_group` / `match_id`, `score`, `rank`, original columns).
* **Multi-stage token + embedding workflows**: `multi_stage_match()` accepts a sequence of mixed `Search_Strategy` and `Embedding_Strategy` objects, threading residuals between stages and stopping early when either side is exhausted. Useful pattern: cheap token stage first, then embedding stage on the residual.
* **`block_by` support for embeddings** so cosine search runs within blocks (e.g. country, year bucket) instead of across the whole table.
* **Backend parity**: full implementation on data.table, DuckDB, and tibble / data.frame, with the same call signatures across backends. DuckDB scales embedding search to large tables via the existing batch infrastructure.
* **Embedding generation via `tidyllm`** (optional `Suggests` dependency): provider-agnostic helpers for Ollama, OpenAI, and other tidyllm-supported backends, so users can move from raw text to a matchable embedding column without leaving R.
* **Embedding-aware diagnostics groundwork**: strategy-class dispatch in place so Phase 0.6 diagnostics can specialise to embedding strategies without API churn.

### Bug fixes

* DuckDB `block_by` SQL bug fixed.
* DuckDB lazy-query bug in multi-stage match fixed.

---

# joinery 0.4.0

## Phase 0.4: Stability & Test-Quality Hardening

A maintenance release with no new user-facing features. The goal was to harden the test suite and close coverage gaps before resuming feature work on embeddings and diagnostics.

* `methods_duckdb.R` coverage raised from 34% to 90%; full behavioural parity with the data.table backend now exercised by tests.
* `embedding_methods_*` coverage raised to 95%+ on both data.table and DuckDB backends.
* Small-table `batch_duckdb` brittleness diagnosed and fixed (see `notes/batch_duckdb_brittleness.md`). User-facing impact: small inputs no longer hit pathological batching behaviour.
* Total package coverage: 87.25%. Remaining low-coverage files are intentional: S7 dispatch boilerplate, interactive-only progress paths, and live-embedding paths reserved for `local_tests/`.

---

# joinery 0.3.1

* Fix `batch_duckdb` small-table brittleness.

---

# joinery 0.3.0

## Phase 3: SearchEngine Heuristics

This release implements advanced matching heuristics that significantly improve accuracy and robustness.

### New Features

* **rIP Smoothing**: Four smoothing methods for token weights:
  - `smoothing(method = "log")`: Log transformation
  - `smoothing(method = "softmax", temperature = 1.0)`: Softmax with temperature
  - `smoothing(method = "offset", alpha = 0.1)`: Additive smoothing
  - `smoothing(method = "none")`: No smoothing (default)
  
* **Containment**: Control maximum matches per record:
  - `max_candidates` parameter limits top-N matches
  - Prevents one-token overmatching
  - Works with threshold filtering
  
* **Feedback Weighting**: Penalize low token overlap:
  - `feedback_strength` parameter (0-1) controls intensity
  - Reduces noise in partial matches
  - Rewards comprehensive token overlap

### DuckDB Backend

* Unified `.score_pairs_sql()` helper consolidates scoring logic
* All Phase 3 features supported in DuckDB backend
* Used by both `detect_duplicates()` and `search_candidates()`

### Backend Improvements

* Both data.table and DuckDB backends support all Phase 3 features
* Full test coverage for all smoothing, containment, and feedback methods
* 454 tests passing

---

# joinery 0.2.0

## Phase 2: DuckDB Backend

* Full DuckDB backend implementation
* Scalable processing of datasets up to 50M rows
* Batch-based processing with R preprocessing pipeline
* Feature parity between data.table and DuckDB backends
* All core generics working on both backends

---

# joinery 0.1.0

* Initial release
* data.table backend
* Token-based record linkage
* Basic preprocessing pipeline
* S7 class system
