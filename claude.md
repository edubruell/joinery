# joinery — Developer / Coding-Agent Guide

## Overview

**joinery** is a heuristic, token-based record linkage system for R.
It integrates cleanly with tidyverse workflows and supports:

- **data.table** (main in-memory backend)
- **tibbles** (defers to data.table backend)
- base **data.frames** (defers to data.table backend)
- **DuckDB tables** (batch-based, R-preprocessing pipeline)

The package is built on the **S7 class system**, separating linkage into:

1. **A declarative search strategy** — defines *how* text fields should be normalized, tokenized, encoded, weighted, scored, and blocked.
2. **Backend-specific execution** — defines *how* data is matched using the IR.

## File layout

Every file under `R/` carries one of eight prefixes that maps to a role.
Use the directory listing as a navigation tool — the prefix tells you what
kind of code is inside before you open the file.

| Prefix | Role |
|---|---|
| `strategy_*.R` | S7 strategy classes & their constructors (`Step`, `Search_Preparer`, smoothing family, `Search_Strategy`, `Embedding_Strategy`). |
| `preparer_*.R` | Step preparer functions (text → tokens), grouped by signature shape (`preparer_word.R` is text-in/text-out; `preparer_tokens.R` produces or operates on tokens). |
| `generics_*.R` | S7 generic declarations, grouped by lifecycle era (`generics_core.R`, `generics_calibration.R`, `generics_embedding.R`, `generics_diagnostic.R`). |
| `methods_<backend>_<stage>.R` | Token-backend dispatch, one file per (backend, workflow stage) — e.g. `methods_datatable_prepare.R`, `methods_duckdb_search.R`. Stages: `prepare`, `dedup`, `search`, `multistage`, `inspect`; DuckDB also has `methods_duckdb_batch.R` for the batching machinery. |
| `embedding_methods_<backend>.R` | Embedding-backend dispatch (`embedding_methods_datatable.R`, `_duckdb.R`, `_tibble.R`). |
| `diagnostic_*.R` | Diagnostic verbs (`diagnostic_audit.R`, `_summarise.R`, `_explain.R`, `_sample.R`, `_compare.R`), their result S7 classes (`diagnostic_classes.R`), recommendations catalog (`diagnostic_recommendations.R`), and plot family (`diagnostic_plots.R`). |
| `calibration_*.R` | Calibration verbs, helpers, and result S7 classes — `calibration_features.R` + `_features_embedding.R` (`match_features()`), `calibration_filter.R` + `_tidymodels.R` (`fit_filter()` / `apply_filter()`), `calibration_calibrate.R` + `_recipe.R` (`calibrate()` / `joinery_recipe()`), `calibration_dispatch.R` (`calibrate_matches()`), `calibration_labelling.R` (CSV round-trip), `calibration_aip.R` (`aIP` primitive), `calibration_classes.R` (`Match_Features`, `Filter_Model`, `Calibrated_Matches`, `Filter_Calibration`). |
| `internal_*.R` | Cross-cutting utilities — `internal_validation.R` (validation helpers + cli_abort exemplar), `internal_progress.R` (progress bars). |

Conventional names that stay outside the schema: `joinery-package.R`,
`data.R`, and the vendored rlang shims (`import-standalone-*.R`).

`DESCRIPTION`'s `Collate:` field is hand-maintained. Any file rename,
split, or new file requires a manual `Collate:` update; S7 class
definitions and external generics must precede the files that declare
methods on them.

### Where to look for X

- **Add a new preparer function** → `preparer_word.R` if the function maps strings to strings, `preparer_tokens.R` if it produces or operates on tokens. Document via roxygen; the generic catalog lives in `notes/preparers_reference.md`.
- **Add a new diagnostic verb** → new generic in `generics_diagnostic.R`, new result class in `diagnostic_classes.R`, new verb file `diagnostic_<verb>.R`, plot helpers in `diagnostic_plots.R`, threshold rules in `diagnostic_recommendations.R`.
- **Add a new backend** → five `methods_<backend>_<stage>.R` files (`prepare`, `dedup`, `search`, `multistage`, `inspect`) plus optionally `embedding_methods_<backend>.R`. Add `Collate:` entries after the existing methods blocks.
- **Extend calibration** → verb code in `calibration_<verb>.R`; result classes in `calibration_classes.R`; generic declarations in `generics_calibration.R`. Tidymodels-specific code is isolated in `calibration_tidymodels.R` / `calibration_recipe.R` so the `Suggests` dependency boundary is visible.
- **Change error message style** → `internal_validation.R` carries the `cli::cli_abort()` exemplar; new code should follow that style.

## Core S7 Classes

### `Step`

Represents **one preprocessing step** (e.g., `normalize_text()`, `word_tokens(min_nchar=3)`, `as_metaphone()`, `filter_stopwords()`).

Stores:
- `name` – function name as string
- `args` – list of unevaluated expressions

### `Search_Preparer`

Represents the preprocessing pipeline for **one column**.

Holds:
- `column` – column name
- `steps` – ordered list of `Step` objects (pipeline order matters)

### `Search_Strategy`

Top-level IR object defining how matching works.

Contains:
- `preparers` – named list of `Search_Preparer`
- `weights` – named numeric vector (optional)
- `block_by` – character vector (optional)
- `rarity` – `"inverse_freq"` (default) or `"tfidf"`
- `threshold` – default match threshold

Constructed via:
```r
search_strategy(
  column ~ step1 + step2 + step3,
  ...,
  weights = c(),
  block_by = NULL,
  rarity = "inverse_freq",
  threshold = 0.9
)
```

## Backend Generics

All generics dispatch on backend class:

```r
prepare_search_data(data, id, strategy)
compute_rarity(tokens, strategy)
detect_duplicates(base_table, id, strategy, threshold = NULL)
search_candidates(base_table, target_table, base_id, target_id, strategy,
                  threshold = NULL, weights = NULL)
deduplicate_table(base_table, duplicates, id)
extract_unmatched(data, id, matches)
multi_stage_match(base_table, target_table, base_id, target_id, strategies)
```

`prepare_search_data()` is the **central interpreter** for the IR and everything else builds on its output.

## Search Workflow (7 Core Steps)

1. **Preprocessing** — `prepare_search_data()` interprets all `Step` objects, produces long-form tokens, attaches block variables.
2. **Rarity Computation** — measures token informativeness per column/block using inverse frequency, TF-IDF, or other metrics.
3. **Token Overlap Join** — self-join (duplicates) or cross-join (candidates) on `(column, token, block_by)` to find record pairs.
4. **Scoring (rIP)** — relative identification potential: `rIP = rarity / sum(rarity)` per record, then `score = sum(rIP * weight)`.
5. **Thresholding** — keep pairs where `score >= threshold` (argument or strategy default).
6. **Residual Generation** — extract unmatched records via `extract_unmatched()` for multi-pass workflows.
7. **Multi-Stage Matching** — sequential strategies with residual extraction; stop early if either residual becomes empty.

## Diagnostics

Five diagnostic verbs, organised around four user questions (Q1 will-it-work, Q2 did-it-work, Q3 why-this-pair, Q4 where-to-look) plus multi-stage:

- `audit_strategy()` → `Strategy_Audit` (token strategies) or `Embedding_Audit` (embedding strategies) — dispatches on strategy class
- `summarise_matches()` → `Match_Overview` (unified across dedup/candidates via `match_type` slot)
- `explain_match()` → `Match_Explanation` (per-token attribution for `Search_Strategy`; pair+score only for `Embedding_Strategy`)
- `sample_matches()` → `Match_Sample` (modes: `high`, `low`, `borderline`, `ambiguous`, `top_gap`, `random`; also `stratify_by` and `expand_to_block` for stratified labelling-set construction)
- `compare_stages()` → `Stage_Comparison` (multi-stage diagnostics)

Recommendations live in `R/diagnostics_recommendations.R` (signal → threshold → message), surfaced via inline `cli` warnings in `print()` and the `recommendations(x)` accessor.

Plotting is first-class via `tinyplot` (hard `Imports` dependency). Diagnostic verbs return data only. Each plot is a separately named function (no `plot(x, type=...)`); pipe-composable: `summarise_matches(m) |> score_histogram()`. Default `plot()` methods per class call the most-useful single view.

The `explain_match` round-trip contract (`sum(per_column_contrib$contribution) × feedback_factor == score`, exact to 1e-10 with no feedback) is mandatory on both backends — it is the property test that prevents scoring drift.

## Calibration Primitives

- **`prepare_auxiliary_registry()`** — internal generic. Builds a per-column token-occurrence registry on the auxiliary (search / target) side. Block-agnostic and cross-table by construction (distinct from the per-block retrieval-time `compute_rarity()`).
- **`compute_aip()`** — internal helper. Implements Doherr (2023) eq. (9) on a pair of registries, producing `aip` per `(src_column, token)`. Consumed by `match_features()`. Lives in `R/aip.R`; design in `notes/calibration_design.md`.
- **`export_for_labelling()` / `import_labels()`** — exported verbs. CSV round-trip for manual labelling of a `Match_Sample`. `export_for_labelling()` pre-fills `equal` on block-header rows (base-side rows for candidates, rank-1 rows for dedup) using `default_label = 1L` (use `0L` for the inverse workflow); writes a flat CSV with `equal` placed first. `import_labels()` reads back, propagates the block-default `equal` onto unmarked rows, validates schema, returns a `data.table` ready for `fit_filter()` / `calibrate_matches()`. Format-agnostic; no UI shipped. Lives in `R/labelling.R`; design in `notes/calibration_design.md`.
- **`match_features()`** — exported verb. Builds a wide one-row-per-pair feature `data.table` from a joinery match result. Dispatches on strategy class. `Search_Strategy` returns the full token schema (`searched`, `found`, `match_id`, `stage`, `score`, `cnt`, `icnt`, `ipos`, `stage_*`, `scnt`, `rcnt`, `r1..rn`, `m_<col>_*`, `f_<col>_*`, `s_<col>_*`, `sim_sf_<col>`, `sim_fs_<col>`). `Embedding_Strategy` returns the reduced schema (core + `stage_*` + `sim_sf_*` / `sim_fs_*` + `cosine_sim` + `embedding_norm_s` + `embedding_norm_f`). String similarity uses `stringdist::stringsim()` with a single global `method =` argument (default `"jw"`); pass `include_string_sim = FALSE` to opt out on minimal installs. Embedding norms are L2 norms of the **pre-normalization** embeddings, recomputed only over the matched subset under a temporary strategy with `normalize = FALSE`. Column order is the public API — additions only, never reorder or rename. Lives in `R/match_features.R`; schema in `notes/calibration_design.md`.
- **`fit_filter()` / `apply_filter()` / `calibrate_matches()`** — exported verbs. Post-retrieval false-positive filter. `fit_filter(features, labels, model = "logistic", class_weighted = FALSE, na_fill = 0)` joins a `Match_Features` to a labels `data.table` on `(match_id, found)`, fits a logistic `glm`, and returns a `Filter_Model` carrying the fitted model plus training probabilities / labels / per-stage distribution. `apply_filter(features, filter_model, threshold = NULL, matches = NULL)` scores features, picks the threshold via Youden's J on training data when `threshold` is `NULL`, and returns a `Calibrated_Matches` whose `@matches` slot either holds the enriched features table or — when `matches =` is supplied — the original raw matches table with `tp_prob` / `predicted_tp` broadcast onto every row of the pair (candidates: by `match_id`; duplicates: by `(duplicate_group, id == found)` on rank-k rows only). `calibrate_matches(matches, strategy, labels, base, id, target, target_id, ...)` is the high-level verb that composes `match_features()` → `fit_filter()` → `apply_filter()`; it dispatches on `(matches, strategy)` for data.table, DuckDB (collect-and-delegate), and tibble / data.frame inputs. Threshold selection defaults to Youden's J; user supplies `threshold =` to override. Lives in `R/fit_filter.R` and `R/calibrate_matches.R`; design in `notes/calibration_design.md`.
- **`calibrate()` / `Filter_Calibration`** — exported verb. Evaluates a fitted `Filter_Model` (carried on a `Calibrated_Matches`) on a labelled set and returns a `Filter_Calibration` with reliability table, Brier score, log-loss, per-class confusion matrix, and threshold sweep curve. `calibrate(cm)` uses the training labels stored on the `Filter_Model`; `calibrate(cm, labels)` evaluates on an independent labelled set (dispatches on candidates vs duplicates by inspecting `@matches`). Surfaces `calibration_low_n_warning` from the recommendations catalog. Lives in `R/calibrate.R`.
- **`joinery_recipe()` + tidymodels `fit_filter()` path** — exported. `joinery_recipe(features, labels)` returns a `recipes::recipe` with `searched` / `found` / `match_id` tagged as role `"id"` and `equal` as the outcome. `fit_filter(model = <parsnip spec | fitted parsnip | (un)fitted workflow>)` accepts tidymodels objects and wraps them in a `Filter_Model` (backend = `"parsnip"` / `"workflow"`); fitted workflows are detected via `workflows::is_trained_workflow()` and not re-fit. `apply_filter()` scores via `predict(type = "prob")` and reads the `.pred_1` column (training fixes `equal` to `factor(c(0L, 1L))`). All tidymodels packages are in `Suggests`; `requireNamespace()` guards keep the baseline glm path dependency-free.
- **Calibration-related recommendations** — four entries in `R/diagnostics_recommendations.R`: `consider_calibration_borderline` (fires from `summarise_matches(matches, threshold = ...)` when `pct_pairs_borderline > 0.10`), `consider_calibration_ambiguity` (fires from `Match_Overview` when `pct_records_with_ge3_matches > 0.20`), `calibration_low_n_warning` (fires from `Filter_Calibration` when training_n < 500), and `calibration_drift_warning` (fires from `apply_filter()` when stage-distribution TV distance vs training > 0.15). `summarise_matches(threshold = NULL, borderline_epsilon = 0.05)` is the entry point on both data.table and DuckDB backends.

## Expected Output Schemas

### Duplicate Detection
```
duplicate_group  
score
id
<original columns of base_table>
rank
```

### Cross-Table Candidate Matches
```
match_id
score
source     # "base" or "target"
id
<original columns>
rank
```

## Key Principles for Coding in joinery

- **Do not assume a specific backend.** Use generics; implement new methods as needed.
- **Token tables are the universal interface:**
  ```
  id | column | token | row_id | <block_by>
  ```
- **Scoring uses rIP internally** — `sum(rarity * weight)`.
- **Thresholding is applied after scoring.**
- **Output must follow the schemas above.**
- **Multi-stage matching is sequential** — matches → extract residuals → next strategy.

## Testing Policy

- Run normal package tests with `Rscript -e "devtools::test()"`.
- Run coverage with `Rscript -e "covr::package_coverage()"` when `covr` is installed.
- Add ordinary `testthat` tests for small deterministic cases, validation errors, backend parity, scoring branches, and output schemas.
- Do not put large DuckDB jobs, stress tests, provider-dependent embedding tests, or expensive benchmarks in `tests/testthat/`.
- Put those larger checks in `local_tests/`; they are intentionally local and excluded from package builds/checks via `.Rbuildignore`.
- Keep `examples/`, `localwip/`, `notes/`, and `joinery.Rproj` local unless explicitly requested otherwise.

## Reference Documentation

For detailed guidance on specific topics, consult:

**Core Architecture:**
- **`notes/architecture.md`** — Data.table backend internals, token table schema, rarity & scoring details.
- **`notes/preparers_reference.md`** — Complete catalog of text normalization, phonetic encoding, token generation, and token transformation functions.

**DuckDB Backend:**
- **`notes/duckdb_status.md`** — Implementation status, completed features, test coverage, known limitations.
- **`notes/duckdb_coding_guide.md`** — Practical guide for using and extending the DuckDB backend.
- **`notes/duckdb_backend.md`** — Design philosophy, batch execution architecture, no-SQL-translation approach.
- **`notes/duckdb_performance.md`** — Performance tuning guide, batch size recommendations, optimization strategies.

**Current Work (v0.8 practicality test):**
- **`notes/yp_panel_joinery_plan.md`** — Active plan: rebuild the full German Yellow-Pages panel (all years, all branches) with joinery's DuckDB backend, blocked by `(plz2, wz08_3)`. First real-life stress test of v0.8. **Read this before doing any YP-related work.**
- **`notes/v08_lessons.md`** — Running log of design-relevant insights surfaced *while* running the YP build through joinery. Captures rough edges in `block_by`, audit verbs, progress reporting, and docs framing that we want to revisit during the v0.8 application pass. Items promoted to actionable package work appear as stubs pointing at `v08_implementation_plan.md`.
- **`notes/v08_implementation_plan.md`** — Actionable v0.8 implementation pass. Ordered list of bugs surfaced by the YP run (empty-result schema, filtered-lazy materialise, block-aware CC iteration, cardinality recommendation) plus the `local_tests/yp_dedup_smoke.R` test bed. Each item carries file paths, fix shape, test plan, and a fresh-context agent audit prompt. Read this before touching `R/methods_duckdb_*` or `R/diagnostic_recommendations.R`.
- **`notes/yp_panel_legacy_workflow.md`** — Retrospective of the legacy MatchMakeR pipeline that built the existing Hausärzte panel; raw-data layout, blocking decisions, output schemas, and which legacy decisions to carry over or revisit. Source data lives at `/Users/ebr/Dropbox/medlaborsupply/gelbe_seiten_raw/` (raw text + cached parquet at `.../pqt/`); legacy scripts at `/Users/ebr/Dropbox/medlaborsupply/gelbe_seiten_raw/read_yellow_pages.R` and `/Users/ebr/Dropbox/medlaborsupply/entry_regulations_R/{deduplicate_yp_data,create_yp_panel}.R`.
- **Working folder for YP-test scripts and intermediates:** `localwip/yp_panel/` (untracked; `localwip/` is in both `.gitignore` and `.Rbuildignore`). Already contains `wz08_3_labels.csv` (274 WZ08-3 group codes + English labels, extracted from `localwip/Labels_SIAB_7523_v2_btr_basis_en.log`). Branche → WZ08-3 classifier (Phase 1) will follow the tidyllm pattern from `/Users/ebr/Dropbox/Keeping the Doctor Away/gop_matching/gop_tidyllm_code.R`.

**Project Planning:**
- **`notes/code_quality_pass.md`** — Completed code-quality refactor (May 2026): unified naming schema and error/validation styles now in place across the package. Reference for the conventions new code should follow.
- **`notes/roadmap.md`** — Strategic roadmap, feature priorities, current phase.
- **`notes/embedding_design.md`** — Implementation design for embedding-based matching.
- **`notes/diagnostics_design.md`** — Design notes for the diagnostics verbs.
- **`notes/calibration_design.md`** — Calibration design: post-match false-positive filter, `aIP` primitive, `match_features()` schema, `calibrate_matches()` verb, tidymodels boundary.
- **`notes/searchengine_lessons_for_v07.md`** — Distilled lessons from Doherr's SearchEngine whitepaper that informed the calibration design.
- **`notes/HolisticMatching.md`** — OCR'd full text of the SearchEngine whitepaper (Doherr 2023) for reference.
- **`notes/batch_duckdb_brittleness.md`** — Investigation and fix notes for the small-table batch_map brittleness.
