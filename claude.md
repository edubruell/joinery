
# 📦 joinery — Developer / Coding-Agent Guide

## Overview

**joinery** is a heuristic, token-based record linkage system for R.
It is designed to integrate cleanly with tidyverse workflows while also supporting:

* **tibbles** (dplyr)
* base **data.frames**
* **data.table**
* **DuckDB tables** (future SQL backend)

The package is built on the **new S7 class system** to keep the linkage workflow cleanly separated into:

### 1. A **declarative search strategy**

Defines *how* text fields should be normalized, tokenized, encoded, weighted, scored, and blocked.

### 2. Backend-specific execution

Defines *how* data is prepared and matched depending on whether the user works with:

* in-memory **data.table** (fully implemented)
* **tibble** (planned)
* **DuckDB relations** (future)

The S7 classes give joinery a **backend-agnostic intermediate representation (IR)** for preprocessing steps.

# Core S7 Classes

## `Step`

Represents **one preprocessing step**, e.g.:

* `normalize_text()`
* `word_tokens(min_nchar = 3)`
* `generate_ngrams(n = 3)`
* phonetic encoders (`as_metaphone()`, etc.)
* filters (`filter_stopwords()`)
* etc.

A `Step` stores:

* `name` – function name as string
* `args` – list of unevaluated expressions

## `Search_Preparer`

Represents the preprocessing pipeline for **one column** in the dataset.

Holds:

* `column` – column name
* `steps` – ordered list of `Step` objects
  (pipeline order matters)

## `Search_Strategy`

Top-level IR object defining how matching works.

Contains:

* `preparers` – named list of `Search_Preparer`
* `weights` – named numeric vector (optional)
* `block_by` – character vector (optional)
* `rarity` – `"inverse_freq"` (default) or `"tfidf"` (plus planned methods)
* `threshold` – default match threshold (if the function does not override)

Constructed via:

```r
search_strategy(
  column ~ step1 + step2 + step3,
  ...,
  weights = c(),
  block_by = NULL,
  rarity   = "inverse_freq",
  threshold = 0.9
)
```

# Backend Generics

All generics dispatch on the backend class:

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

`prepare_search_data()` is the **central interpreter** for the IR
and everything else builds on its output.


# How the **data.table** backend works (current implementation)

## `prepare_search_data()`

* Applies each `Search_Preparer` pipeline to its column
* Produces a **long token table** with columns:

```
id               # actual id column name (e.g. "id_base")
column           # source column name
token            # token emitted by the pipeline
row_id           # row index in the original table
<block_by vars>  # optional blocking fields
```

Example:

```
   id_base   column     token     row_id    Kreis
   B0001     Nachname   MUELLER        1    Region Hannover
   B0001     Vorname    MICHAEL        1    Region Hannover
```

This token table is the internal structure for all matching operations.


# Search Workflow (aligned with actual implementation)

## 1. Preprocessing

`prepare_search_data()`:

* interprets all `Step` objects in order
* produces long-form tokens
* attaches block variables

This is backend-specific but IR-driven.


## 2. Rarity Computation

Rarity measures how informative a token is.

joinery computes rarity:

* **per column**
* **per block**
* on the **token universe used in the run**

  * duplicates → only base
  * candidates → base + target combined

Supported metrics (actual + planned):

* **inverse_freq** *(default)*
* **tfidf**
* **smoothed_inverse_freq**
* **bm25**

All columns/blocks share the same *metric*,
but rarity values are computed **within each column and block**.


## 3. Token Overlap Join

* Duplicate detection → **self-join** on (`column`, `token`, block_by)
* Candidate search → join **base_tokens ↔ target_tokens**

This produces token-level record pairs.


## 4. Scoring (actual rIP implementation)

Tokens are scored using **relative identification potential (rIP)**:

```
rIP = rarity / sum(rarity), by = (id, column)
```

Record-pair similarity:

```
score = sum_over_shared_tokens( rIP * weight(column) )
```

Weights come from:

* `weights` argument to `search_candidates()`, or
* `strategy@weights` if omitted

Weights and rIP are already implemented and match this specification.


## 5. Thresholding

Threshold is:

* argument `threshold` (if supplied), or
* `strategy@threshold` (default)

Pairs with:

```
score >= threshold
```

are kept.

---

## 6. Residual Generation

After matches are produced:

```
residual_base   = base_table   \ matched_base_ids
residual_target = target_table \ matched_target_ids
```

API is:

```r
extract_unmatched(data, id, matches)
```

Residual generation is used by:

* multi-pass matching
* multi-stage pipelines
* strict → phonetic → fuzzy workflows


## 7. Multi-Stage Matching (generic interface implemented)

The generic `multi_stage_match()` is implemented, though backends may refine it.

Workflow:

1. For each strategy in order:

   * run `search_candidates()`
   * accumulate matches (add stage info)
   * `extract_unmatched()` on base and target
2. Stop early if either residual becomes empty

Sketch:

```r
strategies <- list(
  strict   = strat_strict,
  phonetic = strat_phonetic,
  ngrams   = strat_ngrams
)

results <- multi_stage_match(
  base_table, target_table,
  base_id   = "id_base",
  target_id = "id_target",
  strategies = strategies
)
```


# Expected Output Schemas

## Duplicate Detection

```
duplicate_group  
score
id
<original columns of base_table>
rank
```

These fields are implemented and fully tested.


## Cross-Table Candidate Matches

```
match_id
score
source     # "base" or "target"
id
<original columns>
rank
```

This is implemented; tests still being expanded.



# Summary for the Agent

When writing code inside joinery:

* **Always use the S7 IR.**
  Never hard-code preprocessing.

* **Do not assume a specific backend.**
  Use generics; implement new methods as needed.

* **Token tables are the universal interface:**

  ```
  id | column | token | row_id | <block_by>
  ```

* **Scoring uses rIP internally** but conceptually remains
  `sum(rarity * weight)`.

* **Thresholding is applied after scoring.**

* **Output must follow the schemas above.**

* **Multi-stage matching is sequential:**
  matches → extract residuals → next strategy.


