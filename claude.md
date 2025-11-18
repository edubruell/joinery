# 📦 joinery — Developer / Coding-Agent Guide

## Overview

**joinery** is a heuristic, token-based record linkage system for R.
It is designed to integrate cleanly with tidyverse workflows while also supporting:

- **tibbles** (dplyr)
- base **data.frames**
- **data.table**
- **DuckDB tables** (future work: SQL backend)

The package is built on the **new S7 class system** to keep the linkage workflow cleanly separated into:

### 1. A **declarative search strategy**

Defines *how* text fields should be normalized, tokenized, encoded, weighted, and blocked.

### 2. Backend-specific execution

Defines *how* data is prepared and matched depending on whether the user works with:

- in-memory data.table,
- tibble,
- or DuckDB relations (future).

The S7 classes give joinery a **backend-agnostic intermediate representation (IR)** for preprocessing steps.


# Core S7 Classes

## `Step`

Represents **one preprocessing step**, e.g.:

- `normalize_text()`
- `word_tokens(min_nchar = 3)`
- `generate_ngrams(n = 3)`
- etc.

A `Step` stores:

- `name` – function name as string
- `args` – list of expressions (unevaluated)

## `Search_Preparer`

Represents preprocessing for **one column** in the dataset.

Holds:

- `column` – the column name
- `steps` – an *ordered list* of `Step` objects

This mirrors how pipelines work in dplyr, except it’s deferred until a backend executes it.

## `Search_Strategy`

Top-level IR object.

Contains:

- `preparers` – named list of `Search_Preparer`
- `weights` – named numeric vector (optional)
- `block_by` – character vector of blocking vars (optional)
- `rarity` – method name: `"inverse_freq"` (default) or `"tfidf"`

Created via:

```r
search_strategy(
  column ~ step1 + step2 + step3,
  ...,
  weights = c(),
  block_by = NULL,
  rarity = "inverse_freq"
)
```


# Backend generics

All generics dispatch on backend class (data.table, tibble, duckdb):

```r
prepare_search_data(data, id, strategy)
detect_duplicates(base_table, id, strategy, threshold)
search_candidates(base_table, target_table, base_id, target_id, strategy, threshold, weights = NULL)
deduplicate_table(base_table, duplicates, id)
```

`prepare_search_data()` is the **core**, and every other function depends on its output.


# How the data.table backend works (actual implementation)

### `prepare_search_data()` (data.table version):

- Applies each `Search_Preparer` pipeline to its column.
- Produces a **long token table** with these columns:

```
id               # actual id column name (e.g., "id_base")
column           # which column tokens came from (e.g. "Nachname")
token            # token value
row_id           # original row index
<block_by vars>  # e.g. "Kreis"
```

Example output:

```
   id_base   column     token   row_id    Kreis
   B0001     Nachname   MUELLER     1      Region Hannover
   B0001     Vorname    MICHAEL     1      Region Hannover
   ...
```

This  table is then used for both duplication detection and cross-table candidate search.

# Search workflow description (aligned with actual code)

The pipeline operates as:

###  1. Preprocessing (backend-specific)

`prepare_search_data()` interprets the IR and builds a token table and is the internal core function for each backend.
It is exported but rarely needed for the end-user, but is instead called by the `detect_duplciates()` and `search_candidates()`
functions. 

### 2. Rarity computation (block/column-aware)

Rarity measures how informative a token is when scoring matches.  
In **joinery**, rarity is always computed:

- using **one global rarity metric per run**, and  
- **within each block** (`block_by`),  
- **per column**, so that each field has its own frequency profile.

Formally:

```
rarity(token, column, block) → numeric
```

**Supported metrics (planned):**
- **inverse_freq** *(default)*  
```
freq = count(token within block AND column)
rarity = 1 / freq

```
- **tfidf** *(block-local IDF)*  
```
df = number of rows in block/column containing token
N  = number of rows in block/column
rarity = log(N / df)
```
- **smoothed_inverse_freq**  
```
rarity = 1 / (freq + α)
```
- **bm25**  
```
rarity = log((N - df + 0.5) / (df + 0.5))
```
All columns and all blocks share the same **metric** during a run  
(but the computation is always **column-specific and block-specific**)


###  3. Token overlap join

- Duplicates → self-join on (`column`, `token`, `block_by`)
- Cross-year matches → join `base_tokens` with `target_tokens`

###  4. Scoring

Similarity score between two records is:

```
sum_over_shared_tokens( rarity(token) * column_weight(column) )
```

Column weights come from:

- `weights` arg
- or `strategy@weights` if none supplied

These are normalized to relative identification potential `rIP`, i.e.:
```
tokens[, rIP := rarity / sum(rarity), by = .(get(id), column)]
```
Example code for rIP-based tresholding:
```
scored <- joined[
    , .(score = sum(rIP * weight, na.rm = TRUE)),
    by = c(id, id2)
  ]
  
  # Apply threshold
  scored <- scored[score >= threshold]
```

### 5. Threshold filtering

Only record pairs where:

```
rIP  >= threshold
```

are returned.


### 6. Residual generation (planned)

After producing matches, joinery creates **residual tables**:

```
residual_base = base_table \ matched_base_ids
residual_target = target_table \ matched_target_ids
```

- Run a **strict** strategy first,
- Then run a **phonetic** or **n-gram** strategy on residuals,
- Continue until no rows remain or no further matches are found.

This enables flexible, multi-pass matching without embedding recursion or
fallback logic inside a single strategy definition.

A minimal API for this functionality is:

```r
extract_unmatched(data, id, matches)
```
For cross-table matching, it would be called separately for base and target:
```r
residual_base   <- extract_unmatched(base_table,   base_id,   matches)
residual_target <- extract_unmatched(target_table, target_id, matches)
```
### 7. Multi-stage matching pipeline (planned)

Joinery will support *multi-stage* linkage by running several strategies in
sequence. Each stage produces matches and then removes the matched records
before the next stage runs.

The workflow is:

1. Run `search_candidates()` with Strategy A  
2. `extract_unmatched()` removes all matched rows  
3. Run `search_candidates()` on the remaining rows with Strategy B  
4. Repeat for Strategy C, D, … until no unmatched rows remain

Example sketch:

```r
# Stage 1: strict normalization + words
matches1 <- search_candidates(base, target, ..., strategy = strat_strict)
base1   <- extract_unmatched(base,   "id_base",   matches1)
target1 <- extract_unmatched(target, "id_target", matches1)

# Stage 2: phonetic fallback
matches2 <- search_candidates(base1, target1, ..., strategy = strat_phonetic)
```

### 8. Proposed interface for multi-stage strategies (planned)

To simplify multi-stage linkage, joinery may support passing a **list of
strategies** that are executed in order. Each stage runs normally, and
`extract_unmatched()` is applied automatically between stages.

Proposed interface:

```r
multi_stage_match(
  base_table,
  target_table,
  base_id,
  target_id,
  strategies,   # named list of search_strategy() objects
  ...
)
```
Example:

```r
strategies <- list(
  strict   = strat_normalized_words,
  phonetic = strat_metaphone,
  ngrams   = strat_ngrams
)

results <- multi_stage_match(
  base, target,
  base_id   = "id_base",
  target_id = "id_target",
  strategies = strategies
)
```

The function would:

1. run each strategy in sequence,
2. collect matches per stage,
3. remove matched rows automatically using `extract_unmatched()`,
4. stop early if no unmatched rows remain.

This provides a concise wrapper for strict → phonetic → fuzzy matching workflows

# Desired User Interface (aligned with actual code) for the next-to-implement part

```r
library(joinery)

yellow_pages <- readstata13::read.dta13("yellow_pages_hausaerzte.dta")

base_table <- yellow_pages |>
  filter(year == 2016, entry_line == 1) |>
  rename(id_base = entry)

target_table <- yellow_pages |>
  filter(year == 2017, entry_line == 1) |>
  rename(id_target = entry)

yp_strategy <- search_strategy(
  Nachname   ~ normalize_text + word_tokens(min_nchar = 3),
  Vorname    ~ normalize_text + word_tokens(min_nchar = 3),
  Strasse    ~ normalize_text + word_tokens(),
  Hausnummer ~ normalize_text + word_tokens(),
  Ort        ~ normalize_text + word_tokens(),
  block_by   = "kreis"
)

likely_duplicates <- detect_duplicates(
  base_table = base_table,
  id         = "id_base",
  strategy   = yp_strategy,
  threshold  = 0.8
)

deduped <- deduplicate_table(base_table, likely_duplicates, "id_base")

candidates <- search_candidates(
  base_table  = base_table,
  target_table = target_table,
  base_id     = "id_base",
  target_id   = "id_target",
  strategy    = yp_strategy,
  threshold   = 0.6,
  weights     = c(Hausnummer = 0.1, Nachname = 0.5,
                  Vorname    = 0.2, Strasse = 0.1, Ort = 0.1)
)
```


# Expected Output Schemas

### Duplicate detection:

```
duplicate_group  
score
id
<original columns of base_table>
rank
```
(Implemented and working and tests ready)

### Cross-table candidate matches:

```
match_id
score
direction   # "base" or "target"
id
<original columns>
rank
```

### tibble-Output for these steps:

```r
likely_duplicates
# A tibble: 12 × 13
   duplicate_group score   id        year entry_line kreis Nachname     Vorname      Strasse         Hausnummer Ort           PLZ    rank
             <int> <dbl>   <chr>    <dbl>      <dbl> <chr> <chr>        <chr>        <chr>           <chr>      <chr>         <chr> <int>
 1               1 0.97    2016-021  2016          1 A     Schneider    Peter        Lindenweg        3          Neustadt      80100     1
 2               1 0.97    2016-389  2016          1 A     Schnyder     Peter        Lindenweg        3          Neustadt      80100     2
 3               1 0.95    2016-402  2016          1 A     Schneider    P.           Lindenweg        3          Neustadt      80100     3
 4               2 0.92    2016-054  2016          1 B     Hoffmann     Julia        Gartenstr.       18         Kleinstadt    80250     1
 5               2 0.92    2016-288  2016          1 B     Hofmann      Julia        Gartenstraße     18         Kleinstadt    80250     2
 6               3 0.90    2016-077  2016          1 B     Braun        Markus       Kirchweg         7A         Oberdorf      80300     1
 7               3 0.90    2016-501  2016          1 B     Braun        M.           Kirchweg         7A         Oberdorf      80300     2
 8               3 0.89    2016-640  2016          1 B     Braune       Markus       Kirchweg         7A         Oberdorf      80300     3
 9               4 0.84    2016-112  2016          1 C     Lehmann      Sophie       Schulstr.        2          Talhausen     80420     1
10               4 0.84    2016-290  2016          1 C     Lehman       Sophie       Schulstraße      2          Talhausen     80420     2


candidates
# A tibble: 12 × 15
   match_id score source   id        year entry_line kreis Nachname   Vorname      Strasse         Hausnummer Ort          PLZ    rank
     <int> <dbl> <chr>     <chr>    <dbl>      <dbl> <chr> <chr>      <chr>        <chr>           <chr>      <chr>        <chr> <int>
 1       1  0.98 base      2016-001  2016          1 A     Müller     Hans         Hauptstr.       12         Musterstadt  80000     1
 2       1  0.98 target    2017-010  2017          1 A     Mueller    Hans         Hauptstrasse    12         Musterstadt  80000     1
 3       2  0.96 base      2016-003  2016          1 A     Schmidt    Anna Marie   Bergstr.        7          Musterstadt  80000     1
 4       2  0.96 target    2017-011  2017          1 A     Schmidt    Anne Marie   Bergstrasse     7          Musterstadt  80000     1
 5       3  0.81 base      2016-005  2016          1 B     Weber      Fritz        Marktplatz      3          Dorfhausen   81000     1
 6       3  0.81 target    2017-012  2017          1 B     Weber      Fritz        Neue Str.       99         Grossstadt   83000     1
 7       4  0.94 base      2016-006  2016          1 B     von Schön  Lukas        Dorfstr.        5A         Dorfhausen   81000     1
 8       4  0.94 target    2017-013  2017          1 B     von Schoen Lukas        Dorfstrasse     5A         Dorfhausen   81000     1
 9       5  0.91 base      2016-007  2016          1 B     Yilmaz     Mehmet       Ringstr.        22         Dorfhausen   81000     1
10       5  0.91 target    2017-014  2017          1 B     Yilmaz     Mehmet       Ringstrasse     22         Dorfhausen   81000     1
11       6  0.83 base      2016-008  2016          1 C     Kowalski   Joanna       Bahnhofstr.     1          Kleinstadt   82000     1
12       6  0.83 target    2017-015  2017          1 C     Kowalski   J.           Bahnhofstrasse  1          Kleinstadt   82000     1
```


# DuckDB backend (future extension)

The coding agent should note:

- The IR is backend-agnostic.
- Only the interpreter (`prepare_search_data()`, joins, scoring) differ.
- For DuckDB, output should be **database tables**, not in-memory data.tables.

# Summary for the agent

When writing new joinery code:

- **Do not rewrite preprocessing logic directly.**
  Always use the S7 IR (`Step`, `Search_Preparer`, `Search_Strategy`).

- **Never assume a specific backend.**
  Always write generic methods that dispatch on `data` or implement new backends as needed.

- **Work from token tables.**
  All linkage logic uses the long form:

  ```
  id | column | token | row_id | <block_by>
  ```

-  **Similarity scoring = sum(rarity * weight).**

- **Thresholding happens after scoring.**

- **Output must match the schemas above.**

