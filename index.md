# joinery

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

> **Crafting joins that fit.** Heuristic record linkage and fuzzy joins
> for R.

## Overview

**joinery** provides a tidy, declarative interface for **heuristic
index-based record linkage**, **fuzzy joins**, and **duplicate
detection**. It uses token-based indexing, flexible text normalization,
phonetic encoders, and optional blocking to match imperfect or
inconsistent records. The package is inspired by the ideas behind
Thorsten Doherr’s **searchengine** project (see:
<https://github.com/ThorstenDoherr/searchengine/>).

Like good woodworking, the goal is to make the joins clean, without
reaching for the mallet.

## How joinery differs from classical record linkage

Most record linkage tools rely on edit distances or probabilistic
pairwise comparison after blocking. This works well for clean data and
short strings, but often breaks down for messy administrative data where
rare tokens matter more than character-level similarity.

**joinery** follows a different paradigm inspired by Doherr’s
*SearchEngine*:

- Matching is framed as **search and candidate retrieval**, not
  exhaustive pairwise comparison.
- Records are decomposed into tokens weighted by their **informativeness
  (rarity)**.
- Similarity comes from **overlap of informative tokens**, not string
  distance.
- Candidate generation and scoring are the same operation.
- Matching is **directional**; one table defines token frequencies, the
  other provides search queries.

This approach works especially well when:

- Names are long, noisy, or inconsistently formatted.
- Rare identifiers should dominate common boilerplate terms.
- No labeled training data is available.
- Matching proceeds in multiple, increasingly tolerant stages.

It also runs where the data does not fit in memory. The same strategy
you prototype on an in-memory table runs unchanged against a **DuckDB**
database, which does the large joins out-of-core, on disk, a piece at a
time. Little else in R links and resolves entities this way: in practice
it has turned a 51-million-row business directory, a job that would
classically call for a cluster, into an overnight run on a laptop.

joinery is not a drop-in replacement for probabilistic linkage engines.
It is a complementary tool for **transparent, strategy-driven matching**
where robustness and explainability matter more than calibrated match
probabilities.

To place it among the alternatives: reach for **fuzzyjoin** or
**zoomerjoin** when you need string- or edit-distance joins on short,
mostly clean fields; reach for **fastLink** or **reclin2** when you need
a Fellegi-Sunter model with calibrated m/u match probabilities. Reach
for joinery when records are long and messy, rare tokens carry the
signal, and you want to see exactly why a pair matched. A probabilistic
scoring strategy is on the roadmap, so that boundary is expected to
narrow over time.

## Installation

``` r

install.packages("devtools")   # if needed
devtools::install_github("edubruell/joinery")
```

## Minimal example

This example uses the package’s built-in sample data. The [Getting
started](https://edubruell.github.io/joinery/articles/joinery.html)
vignette walks the same path step by step and scores the result against
a known answer key (`target_example$actual_link` holds the true link for
each copied record).

``` r

library(joinery)

# Load example data (shipped as tibbles; joinery also takes data.frames / data.table / DuckDB)
data("base_example")
data("target_example")

# Define a simple first-pass strategy
strat <- search_strategy(
  Nachname   ~ normalize_text() + word_tokens(min_nchar = 3),
  Vorname    ~ normalize_text() + word_tokens(min_nchar = 3),
  Strasse    ~ normalize_street(lang = "de") + word_tokens(min_nchar = 3),
  Hausnummer ~ numeric_tokens,
  Ort        ~ normalize_text(),
  block_by   = "Kreis",
  threshold  = 0.8
)

# Inspect the tokenization
inspect_tokens(base_example, "id_base", strat, Vorname)

# Detect and collapse duplicates within the base data
duplicates    <- detect_duplicates(base_example, id = "id_base", strategy = strat)
base_dedupped <- deduplicate_table(base_example, duplicates, id = "id_base")

# Cross-table candidate matches
matches <- search_candidates(
  base_dedupped,
  target_example,
  base_id   = "id_base",
  target_id = "id_target",
  strategy  = strat
)

# Check the result, then read the residuals
summarise_matches(matches, threshold = 0.8)
extract_unmatched(base_dedupped, "id_base", matches)
extract_unmatched(target_example, "id_target", matches)
```

### Stage exact, then fuzzy

A cheap, strict exact pass first, then a tolerant fuzzy pass only on
what is left:

``` r

staged <- multi_stage_search(
  base_dedupped,
  target_example,
  base_id   = "id_base",
  target_id = "id_target",
  strategies = list(
    exact = exact_strategy(
      Nachname ~ normalize_text() + word_tokens(min_nchar = 3),
      Vorname  ~ normalize_text() + word_tokens(min_nchar = 3),
      Ort      ~ normalize_text(),
      block_by = "Kreis"
    ),
    fuzzy = strat
  )
)
```

## Documentation

The [package website](https://edubruell.github.io/joinery/) carries the
full guide. Start with [Getting
started](https://edubruell.github.io/joinery/articles/joinery.html),
which walks the whole path on a pair of built-in tables and scores the
result against a known answer key. From there, five articles each take
on one problem:

- [Beyond the basics: fuzzy and exact
  strategies](https://edubruell.github.io/joinery/articles/features.html).
  Containment, region-free movers, phonetic matching, and staging them
  together.
- [Matching across years and
  sources](https://edubruell.github.io/joinery/articles/staged.html).
  Pool a multi-year panel and follow each record through time.
- [Calibrating a false-positive
  filter](https://edubruell.github.io/joinery/articles/calibration.html).
  Train a model on labelled pairs for when one threshold is not enough.
- [Embedding-based
  matching](https://edubruell.github.io/joinery/articles/embeddings.html).
  Match on meaning when two records share no tokens at all.
- [Working at scale with
  DuckDB](https://edubruell.github.io/joinery/articles/duckdb.html). Run
  the same strategies on a database backend when the table will not fit
  in memory.

The tutorial also ships as a vignette:

``` r

vignette("joinery", package = "joinery")
```

## License

MIT
