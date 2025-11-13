# Joinery <img src="man/figures/logo.png" align="right" height="134" alt="" />

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Crafting joins that fit.**  
_Heuristic record linkage and fuzzy joins, designed with care._


## Overview

**Joinery** is a modern R package for **heuristic record linkage** and **fuzzy joining**.  
It’s inspired by the precision of traditional wood joinery. Each match crafted to fit, not forced.


Joinery provides a declarative, tidy interface for linking messy records across datasets using token-based heuristics, phonetic encoding, and (optionally) semantic similarity.  This package reimplements the main ideas from [Thorsten Doherr's search engine](https://github.com/ThorstenDoherr/searchengine/) to work within modern R-Workflows. 
It also expands it with the ability to combine heuristic index-based linkage with blocking strategies. It’s designed to scale from small data frames to large databases via DuckDB backends, with an S7 object model that makes it modular and extensible.

## Features

-  **Crafted Matching:** Token- and heuristic-based candidate retrieval that replaces rigid blocking.  
-  **Text Normalization:** Case folding, transliteration, and flexible tokenization (word, n-gram, phonetic).  
-  **Backend Flexibility:** Works with baseR backend or DuckDB for large datasets.  
-  **Transparent Evaluation:** Inspect similarity overlap, weights, and matching thresholds.


## Installation

```r
# Install the development version from GitHub
if (!requireNamespace("devtools", quietly = TRUE))
  install.packages("devtools")

devtools::install_github("edubruell/joinery")
```
## Basic Usage Example

Below is an example of how to use the MatchMakeR package for detecting duplicates and searching for matching candidates.
```r
library(joinery)

# Define a record-preparation recipe
prep <- prep_join(
  name     ~ normalize + word_tokens(min_length = 3),
  address  ~ normalize + ngram_tokens(n = 3),
  postcode ~ normalize
)

# Prepare base and target search indices
base_index   <- prepare_data(base_table,   prep, key = "id")
target_index <- prepare_data(target_table, prep, key = "id")

# Match candidate records using heuristic similarity
matches <- match_candidates(
  base_index, target_index,
  threshold = 0.75,
  weights = c(name = 0.5, address = 0.3, postcode = 0.2)
)

# Review and refine matches
report(matches)
refined <- refine(matches, fields = c("name", "address"))
```

## Functions Overview

- **`normalize_text()`**: Normalize a text string by converting to uppercase, transliterating special characters, retaining only alphanumeric characters and spaces, and removing extra spaces.
- **`as_metaphone()`**: Convert a text string to its Metaphone encoding.
- **`as_soundex()`**: Convert a text string to its Soundex encoding.
- **`word_tokens()`**: Return a list of word tokens for the input text.
- **`use_dictionary()`**: Group similar tokens together using a pre-defined dictionary.
- **`generate_ngrams()`**: Generate n-gram tokens from a text string.
- **`search_preparers()`**: Create a list of preparer functions based on a formula syntax.
- **`preapare_search_data()`**: Apply the preparer functions to the specified columns in the input data frame to create a search table
- **`search_candidates()`**: Search for matching candidates between a base table and a target table using token-based heuristic linkage.
- **`detect_duplicates()`**: Detect duplicate records within a base table using token-based heuristic linkage.
- **`deduplicate_table()`**: Use the results of `detect_duplicates` to remove duplicate records from a data table.
- **`build_similarity_dict()`**: Build a dicionary of similar tokens for a base and a target table.

