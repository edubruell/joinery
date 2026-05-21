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
- `sample_matches()` → `Match_Sample` (modes: `high`, `low`, `borderline`, `ambiguous`, `top_gap`, `random`; Phase 0.7 M4 adds `stratify_by` and `expand_to_block` for stratified labelling-set construction)
- `compare_stages()` → `Stage_Comparison` (multi-stage diagnostics)

Recommendations live in `R/diagnostics_recommendations.R` (signal → threshold → message), surfaced via inline `cli` warnings in `print()` and the `recommendations(x)` accessor.

Plotting is first-class via `tinyplot` (hard `Imports` dependency). Diagnostic verbs return data only. Each plot is a separately named function (no `plot(x, type=...)`); pipe-composable: `summarise_matches(m) |> score_histogram()`. Default `plot()` methods per class call the most-useful single view.

The `explain_match` round-trip contract (`sum(per_column_contrib$contribution) × feedback_factor == score`, exact to 1e-10 with no feedback) is mandatory on both backends — it is the property test that prevents scoring drift.

## Calibration Primitives (Phase 0.7, in progress)

- **`prepare_auxiliary_registry()`** — internal generic. Builds a per-column token-occurrence registry on the auxiliary (search / target) side. Block-agnostic and cross-table by construction (distinct from the per-block retrieval-time `compute_rarity()`).
- **`compute_aip()`** — internal helper. Implements Doherr (2023) eq. (9) on a pair of registries, producing `aip` per `(src_column, token)`. Consumed by `match_features()`. Lives in `R/aip.R`; design in `notes/calibration_design.md` §5.
- **`export_for_labelling()` / `import_labels()`** — exported verbs (M4). CSV round-trip for manual labelling of a `Match_Sample`. `export_for_labelling()` pre-fills `equal` on block-header rows (base-side rows for candidates, rank-1 rows for dedup) using `default_label = 1L` (use `0L` for the inverse workflow); writes a flat CSV with `equal` placed first. `import_labels()` reads back, propagates the block-default `equal` onto unmarked rows, validates schema, returns a `data.table` ready for `fit_filter()` / `calibrate_matches()`. Format-agnostic; no UI shipped. Lives in `R/labelling.R`; design in `notes/calibration_design.md` §9.
- **`match_features()`** — exported verb (M2 + M3). Builds a wide one-row-per-pair feature `data.table` from a joinery match result. Dispatches on strategy class. `Search_Strategy` returns the full token schema (`searched`, `found`, `match_id`, `stage`, `score`, `cnt`, `icnt`, `ipos`, `stage_*`, `scnt`, `rcnt`, `r1..rn`, `m_<col>_*`, `f_<col>_*`, `s_<col>_*`, `sim_sf_<col>`, `sim_fs_<col>`). `Embedding_Strategy` returns the reduced schema (core + `stage_*` + `sim_sf_*` / `sim_fs_*` + `cosine_sim` + `embedding_norm_s` + `embedding_norm_f`). String similarity uses `stringdist::stringsim()` with a single global `method =` argument (default `"jw"`); pass `include_string_sim = FALSE` to opt out on minimal installs. Embedding norms are L2 norms of the **pre-normalization** embeddings, recomputed only over the matched subset under a temporary strategy with `normalize = FALSE`. Column order is the public API of v0.7 — additions only, never reorder or rename. Lives in `R/match_features.R`; schema in `notes/calibration_design.md` §6.

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

**Project Planning:**
- **`notes/roadmap.md`** — Strategic roadmap, feature priorities, current phase.
- **`notes/embedding_design.md`** — Implementation design for embedding-based matching.
- **`notes/diagnostics_design.md`** — Design notes for the diagnostics verbs.
- **`notes/calibration_design.md`** — Phase 0.7 design: post-match false-positive filter, `aIP` primitive, `match_features()` schema, `calibrate_matches()` verb, tidymodels boundary.
- **`notes/searchengine_lessons_for_v07.md`** — Distilled lessons from Doherr's SearchEngine whitepaper informing the Phase 0.7 design.
- **`notes/HolisticMatching.md`** — OCR'd full text of the SearchEngine whitepaper (Doherr 2023) for reference.
- **`notes/batch_duckdb_brittleness.md`** — Investigation and fix notes for the small-table batch_map brittleness.
