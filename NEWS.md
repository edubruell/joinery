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

* **DuckDB dedup empty-result schema** — `detect_duplicates()` on a DuckDB tbl now returns the full base schema with zero rows when no pairs cross threshold, so per-block results can be `UNION`-ed without a column-count mismatch.
* **Filtered lazy DuckDB inputs** — every user-facing DuckDB method (`detect_duplicates`, `search_candidates`, `prepare_search_data`, `extract_unmatched`, `audit_strategy`, `summarise_matches`, embedding equivalents) now accepts `tbl(con, "x") |> filter(...)` inputs. Filtered lazies are silently materialised to a TEMP TABLE.
* **Block-aware connected components in DuckDB dedup** — the recursive CTE now iterates per block instead of running one global recursion, which previously OOMed on disk at corpus scale. Result objects carry an `attr("wall_seconds")` for downstream wall-clock budgeting.
* **Scoring now uses token SETS, not bags** — a token repeated within one record's column (e.g. `"Fritzel … Fritzel … Fritzel"`) previously inflated a pair's score through the token-overlap self-join, producing scores above the `sum(weights)` ceiling (up to 2.8 on real Yellow-Pages data) and breaking the `[0, 1]`-style threshold semantics. The scoring path on both backends now collapses within-record token multiplicity before computing rIP and the overlap join, so `score ∈ [0, sum(weights)]`. The fix is confined to scoring (`.score_pairs_sql`, `.score_token_pairs`, and `explain_match`'s attribution, which stay in lock-step for the round-trip contract); rarity is untouched, so `inverse_freq` keeps its corpus term-frequency definition. The change is a no-op for records whose tokens are already distinct.

### New optional API

* `summarise_matches(..., entity_cols = c(...))` — when supplied, counts duplicate groups whose listed columns are single-valued and fires a new `cluster_identical_name_street` recommendation that shadows the generic `duplicates_mega_cluster` advice. Useful for distinguishing real "stopword" clusters from upstream cardinality artefacts.
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

* **rIP Smoothing** — Four smoothing methods for token weights:
  - `smoothing(method = "log")` — Log transformation
  - `smoothing(method = "softmax", temperature = 1.0)` — Softmax with temperature
  - `smoothing(method = "offset", alpha = 0.1)` — Additive smoothing
  - `smoothing(method = "none")` — No smoothing (default)
  
* **Containment** — Control maximum matches per record:
  - `max_candidates` parameter limits top-N matches
  - Prevents one-token overmatching
  - Works with threshold filtering
  
* **Feedback Weighting** — Penalize low token overlap:
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
