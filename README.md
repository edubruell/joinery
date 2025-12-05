# joinery <img src="man/figures/logo.png" align="right" height="139" alt="" />

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

> **Crafting joins that fit.**
> Heuristic record linkage and fuzzy joins for R.

## Overview

**joinery** provides a tidy, declarative interface for **heuristic index-based record linkage**, **fuzzy joins**, and **duplicate detection**.
It uses token-based indexing, flexible text normalization, phonetic encoders, and optional blocking to match imperfect or inconsistent records.
The package is  inspired by the ideas behind Thorsten Doherr’s **searchengine** project (see: [https://github.com/ThorstenDoherr/searchengine/](https://github.com/ThorstenDoherr/searchengine/)).

Like good woodworking, the goal is to make the joins clean, without reaching for the mallet.

## Installation

```r
install.packages("devtools")   # if needed
devtools::install_github("edubruell/joinery")
```

## Minimal Example

This example uses the package’s built-in sample data and will be expanded in the **Getting Started** vignette.

```r
library(joinery)
library(data.table)

# Load example data
data("base_example")
data("target_example")

# Define a simple first-pass strategy
hard_strategy <- search_strategy(
  Nachname   ~ normalize_text() + word_tokens(min_nchar = 3),
  Vorname    ~ normalize_text() + word_tokens(min_nchar = 3),
  Strasse    ~ normalize_street(lang = "de") + word_tokens(min_nchar = 3),
  Hausnummer ~ numeric_tokens,
  Ort        ~ normalize_text(),
  block_by   = "Kreis",
  threshold  = 0.8
)

# Inspect the tokenization
inspect_tokens(base_example, "id_base", hard_strategy, Vorname)

# Detect duplicates within the base data
duplicates <- detect_duplicates(
  base_example, 
  id = "id_base",
  strategy = hard_strategy
)

base_dedupped <- deduplicate_table(
  base_example, 
  duplicates, 
  id = "id_base"
)

# Cross-table candidate matches
matches_hard <- search_candidates(
  base_dedupped,
  as.data.table(target_example),
  base_id   = "id_base",
  target_id = "id_target",
  strategy  = hard_strategy
)

# Unmatched residuals 
extract_unmatched(base_dedupped, "id_base", matches_hard)
extract_unmatched(target_example, "id_target", matches_hard)
```

## Documentation

Full documentation and a step-by-step tutorial are available in the package vignettes.


## License

MIT



