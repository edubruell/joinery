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

The current work is **embedding implementation** (Phase 0.5).

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
- **`notes/test_coverage_plan.md`** — Current coverage-hardening plan and local-test policy.
- **`notes/embedding_design.md`** — Detailed implementation design for embedding-based matching (Phase 0.4).
