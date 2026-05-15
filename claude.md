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

## Current Development Stage

**Phase 0.3 (SearchEngine Heuristics) is complete.**
**Phase 0.4 (Test Coverage Hardening) is complete.**
**Phase 0.5 (Embedding-Based Matching) is feature-complete** — implementation done, mocked tests pass. Tier B live-provider validation against ollama (cross-model dimensions, live `block_by`, live multi-stage, throughput) is deferred until the user runs it on real yellow-pages data.

**Phase 0.6 (Diagnostics) is in progress.** See `notes/diagnostics_design.md` — all §13 user decisions are resolved. M1 and M2 are complete; next step is M3 (`audit_strategy`).

### Locked design summary

- **Five verbs** organised around four user questions (Q1 will-it-work, Q2 did-it-work, Q3 why-this-pair, Q4 where-to-look) plus multi-stage:
  - `audit_strategy()` → `Strategy_Audit` (Q1, pre-match)
  - `summarise_matches()` → `Match_Overview` (Q2, post-match overview, unified across dedup/candidates via `match_type` slot)
  - `explain_match()` → `Match_Explanation` (Q3, attribution; dispatch on 2nd arg: `Search_Strategy` reconstructs, tokens table uses directly)
  - `sample_matches()` → `Match_Sample` (Q4, modes incl. `top_gap`, `ambiguous`)
  - `compare_stages()` → `Stage_Comparison` (multi-stage; `summarise_matches` does NOT auto-detect `stage`)
- **Recommendations catalog** in dedicated `R/diagnostics_recommendations.R`; surfaced both via inline `cli` warnings in `print()` and via `recommendations(x)` accessor.
- **Plotting** is first-class with `tinyplot` as a hard `Imports` dependency. Diagnostic verbs return data only. Each plot is a separately named function (no `plot(x, type=...)`); pipe-composable: `summarise_matches(m) |> score_histogram()`. Default `plot()` method per class calls the most-useful single view.

### Next implementation steps (M1 → M7)

Implement in order. Do not skip ahead — later milestones depend on conventions established earlier.

**M1 — Skeleton + `summarise_matches` (data.table only). COMPLETE (2026-05-11)**
- `R/diagnostics_classes.R`: S7 classes `Strategy_Audit`, `Match_Overview`, `Match_Explanation`, `Match_Sample`, `Stage_Comparison` with slots, `format()`/`print()`/`as.data.table()`/`as.data.frame()` methods.
- `R/diagnostics_generics.R`: five verb generics + `recommendations()` accessor.
- `R/diagnostics_recommendations.R`: catalog (`signal → threshold → message`) + `.dispatch_recommendations()` helper.
- `R/summarise_matches.R`: `summarise_matches.data.table()` end-to-end (schema detection, score distribution, cluster/ambiguity/top_gap, coverage, recommendations).
- 47 tests; print snapshots stable.

**M2 — Backend parity for `summarise_matches`. COMPLETE (2026-05-16)**
- DuckDB: SQL-native aggregations (`APPROX_QUANTILE`, `FLOOR`-arithmetic histogram); pulls only summary scalars and small distribution tables to R.
- Tibble/data.frame: thin wrappers delegating to DT method via `as_DT`.
- `.detect_match_type()` refactored to delegate to `.detect_match_type_cols(cols)` (backend-agnostic).
- 12 backend parity tests; all 884 tests pass.

**M3 — `audit_strategy` (both backends).**
- data.table: token counts, unique tokens, rarity quantiles, NA-rate, block size distribution + imbalance metric (gini or top-1 share), comparison-count estimate, optional vocab-overlap when `target` supplied. Honour `sample_n` for large inputs.
- DuckDB equivalent.
- Extend recommendations catalog with pre-match triggers (low-rarity stopword pressure, block imbalance, low vocab overlap).
- Tests: small fixtures with known token vocabularies; verify recommendations fire on planted symptoms; verify `sample_n` returns audit slots within sampling tolerance.

**M4 — `explain_match` (both backends, both calling forms).**
- Add internal helper to `methods_datatable.R` and `methods_duckdb.R` that returns the per-column contribution table for a single pair, **reusing the engine's scoring path** (no re-implementation — drift would silently break smoothing/feedback). Refactor scoring code if necessary to expose this cleanly.
- `explain_match()` dispatches on 2nd argument: `Search_Strategy` → reconstruct tokens/rarity for the pair only (DuckDB: `WHERE id IN (...)`); tokens-shaped table → use directly.
- Tests: round-trip property (sum of per-column contributions == score modulo documented smoothing/feedback adjustments) on both backends; both calling forms produce identical `Match_Explanation`; ergonomic form does not pull full token table on DuckDB.

**M5 — `sample_matches`.**
- All modes: `high`, `low`, `borderline`, `ambiguous`, `top_gap`, `random`. Both backends.
- Tests: each mode returns the expected rows on small deterministic fixtures; `top_gap` correctly identifies near-coin-flip pairs; `n` is honoured; `mode` validation errors are clear.

**M6 — `compare_stages` and multi-stage extensions.**
- Consume a multi-stage matches table (with `stage` column), produce per-stage `Match_Overview` plus marginal coverage and overlaid score distributions.
- Stage-specific recommendations (e.g. "stage N added <1% coverage").
- Tests: synthetic two-stage and three-stage matches tables; verify marginal coverage arithmetic; verify recommendations fire for low-yield stages.

**M7 — Plot functions and default `plot()` methods.**
- Add `tinyplot` to `DESCRIPTION` `Imports`.
- Create `R/diagnostics_plots.R` with the catalog from `notes/diagnostics_design.md` §7.1: `rarity_histogram`, `token_frequency_plot`, `block_size_plot`, `vocab_overlap_plot`, `score_histogram`, `score_density`, `coverage_plot`, `cluster_size_plot`, `ambiguity_plot`, `top_gap_density`, `contribution_plot`, `token_contribution_plot`, `stage_coverage_plot`, `stage_score_plot`.
- All plot functions take the diagnostic object as first arg, accept `...` passthrough to `tinyplot`, invisibly return the plotted `data.table`.
- Implement small `joinery` theme overlay (palette, dashed threshold-line style, margins). Plots must look publishable with no arguments.
- Default `plot()` methods per class (e.g. `plot.Match_Overview` → `score_histogram`).
- Tests: each plot function runs without error on a representative diagnostic object, returns the expected invisible data.table, and respects `...` overrides. Visual correctness is verified manually; do not snapshot raster output in `tests/testthat/`.
- pkgdown: one screenshot per plot in `pkgdown/figures/`.

### Testing policy specific to Phase 0.6

- `tests/testthat/` contains only small deterministic fixtures, parity checks, recommendations catalog tests, and round-trip property tests. No large-data benchmarks, no live embedding providers, no plot-image snapshots.
- Backend parity tests should use the existing `as_DT` / DuckDB harness pattern from earlier phases.
- The `explain_match` round-trip property test is mandatory on both backends — it is the contract that prevents scoring drift.
- Larger end-to-end checks (e.g. `audit_strategy` on millions of rows, multi-stage on real yellow-pages data) live in `local_tests/`.

Implemented Phase 0.3 features:
- **rIP Smoothing** — log, softmax, and offset smoothing methods prevent over-dominance of rare tokens
- **Containment** — `max_candidates` limits matches per record, preventing one-token overmatching
- **Feedback Weighting** — penalizes low token overlap, reduces noise in partial matches

Phase 0.4 outcome (2026-05-09): total coverage 87.25%. Per-file:
- `preparers.R` 99%, `embedding_methods_duckdb.R` 97%, `embedding_methods_datatable.R` 95%, `methods_duckdb.R` 90%, `batch_duckdb.R` 87%, `methods_datatable.R` 87%, `embedding_strategy.R` 75%, `utilities.R` 64%, `methods_tibble.R` 59%, `embedding_methods_tibble.R` 58%, `search_strategy.R` 42%.
- `methods_duckdb.R` 34→90% closed the primary gap; the small-table `batch_duckdb` brittleness was diagnosed and fixed (see `notes/batch_duckdb_brittleness.md`).
- Remaining low-coverage files are intentional: `generics.R` is S7 dispatch boilerplate, `search_strategy.R` 42% is mostly S7 class definition, `utilities.R` gaps are interactive progress-spinner branches, `methods_tibble.R` and `embedding_methods_tibble.R` rely on tidyllm/live embedding paths that belong in `local_tests/`.

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
- **`notes/roadmap.md`** — Strategic roadmap (phases 0.4–1.0, feature priorities, current phase).
- **`notes/test_coverage_plan.md`** — Coverage-hardening plan and local-test policy (Phase 0.4, complete).
- **`notes/embedding_design.md`** — Implementation design for embedding-based matching (Phase 0.5).
- **`notes/diagnostics_design.md`** — Locked design for Phase 0.6 (diagnostics). All §13 user decisions resolved; M1–M7 implementation roadmap is the source of truth for next steps.
- **`notes/batch_duckdb_brittleness.md`** — Investigation and fix notes for the small-table batch_map brittleness.
