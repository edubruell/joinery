# 📦 joinery — Developer / Coding-Agent Guide

## Current Development Stage

**Phase 0.2 (DuckDB Backend) — COMPLETE ✅**

All core backend functionality is implemented and tested. The package now supports:
- Scalable processing of datasets up to 50M rows
- Full feature parity between data.table and DuckDB backends
- All core generics working on both backends
- Comprehensive test coverage

**Next Phase:** 0.3 (SearchEngine Heuristics) — Focus shifts to accuracy improvements (rIP smoothing, containment rules, feedback weighting).

See `notes/duckdb_status.md` for detailed status and `notes/roadmap.md` for upcoming work.

---

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

## Reference Documentation

For detailed guidance on specific topics, consult:

**Core Architecture:**
- **`notes/architecture.md`** — Data.table backend internals, token table schema, rarity & scoring details.
- **`notes/preparers_reference.md`** — Complete catalog of text normalization, phonetic encoding, token generation, and token transformation functions.

**DuckDB Backend (Phase 0.2 — COMPLETE):**
- **`notes/duckdb_status.md`** — **Implementation status, completed features, test coverage, known limitations.**
- **`notes/duckdb_coding_guide.md`** — **Practical guide for using and extending the DuckDB backend.**
- **`notes/duckdb_backend.md`** — Design philosophy, batch execution architecture, no-SQL-translation approach.
- **`notes/duckdb_performance.md`** — Performance tuning guide, batch size recommendations, optimization strategies.

**Project Planning:**
- **`notes/roadmap.md`** — Strategic roadmap (phases 0.2–1.0, feature priorities, current phase).
