# Package index

## Strategies

Declare how matching works. A strategy holds preparation pipelines,
blocking, weights, and a threshold; it runs nothing itself.

- [`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md)
  : Define a Search Strategy for Record Linkage
- [`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md)
  : Define an Exact Matching Strategy
- [`embedding_strategy()`](https://edubruell.github.io/joinery/reference/embedding_strategy.md)
  : Create an Embedding Strategy
- [`block_on_tokens()`](https://edubruell.github.io/joinery/reference/block_on_tokens.md)
  : Block on a Column's Rare Tokens (region-free blocking)
- [`smooth_rip_identity()`](https://edubruell.github.io/joinery/reference/smooth_rip.md)
  [`smooth_rip_log()`](https://edubruell.github.io/joinery/reference/smooth_rip.md)
  [`smooth_rip_offset()`](https://edubruell.github.io/joinery/reference/smooth_rip.md)
  [`smooth_rip_softmax()`](https://edubruell.github.io/joinery/reference/smooth_rip.md)
  : Configure rIP smoothing for a search strategy

## Text preparation

Turn a column’s text into tokens. Used inside a strategy formula (column
~ step1 + step2).

- [`normalize_text()`](https://edubruell.github.io/joinery/reference/normalize_text.md)
  : Normalize text for matching
- [`normalize_street()`](https://edubruell.github.io/joinery/reference/normalize_street.md)
  : Normalize street names across languages
- [`normalize_date()`](https://edubruell.github.io/joinery/reference/normalize_date.md)
  : Normalize dates to ISO 8601 format (YYYY-MM-DD)
- [`approximate_date()`](https://edubruell.github.io/joinery/reference/approximate_date.md)
  : Approximate dates by rounding to coarser time units
- [`word_tokens()`](https://edubruell.github.io/joinery/reference/word_tokens.md)
  : Split text into word tokens
- [`numeric_tokens()`](https://edubruell.github.io/joinery/reference/numeric_tokens.md)
  : Tokenize numeric fields, expanding ranges into individual numbers
- [`date_tokens()`](https://edubruell.github.io/joinery/reference/date_tokens.md)
  : Extract date components as tokens
- [`fuzzy_tokens()`](https://edubruell.github.io/joinery/reference/fuzzy_tokens.md)
  : Collapse near-duplicate tokens to a canonical form
- [`generate_ngrams()`](https://edubruell.github.io/joinery/reference/generate_ngrams.md)
  : Generate character n-grams from text
- [`token_shapes()`](https://edubruell.github.io/joinery/reference/token_shapes.md)
  : Convert tokens to shape signatures
- [`extract_initials()`](https://edubruell.github.io/joinery/reference/extract_initials.md)
  : Extract initials from tokens
- [`as_metaphone()`](https://edubruell.github.io/joinery/reference/as_metaphone.md)
  : Encode text phonetically with Metaphone
- [`as_soundex()`](https://edubruell.github.io/joinery/reference/as_soundex.md)
  : Encode text phonetically with Soundex
- [`as_cologne()`](https://edubruell.github.io/joinery/reference/as_cologne.md)
  : Encode text phonetically with the Cologne procedure
- [`strip_vowels()`](https://edubruell.github.io/joinery/reference/strip_vowels.md)
  : Strip vowels from text (consonant skeleton)
- [`filter_stopwords()`](https://edubruell.github.io/joinery/reference/filter_stopwords.md)
  : Filter out stopwords from token lists
- [`find_stopwords()`](https://edubruell.github.io/joinery/reference/find_stopwords.md)
  : Discover candidate stopwords from a prepared token table
- [`drop_numeric_tokens()`](https://edubruell.github.io/joinery/reference/drop_numeric_tokens.md)
  : Drop numeric (house-number) tokens from token lists
- [`drop_short_tokens()`](https://edubruell.github.io/joinery/reference/drop_short_tokens.md)
  : Drop short tokens from token lists
- [`use_dictionary()`](https://edubruell.github.io/joinery/reference/use_dictionary.md)
  : Map tokens to canonical groups with a lookup table

## Matching

Find duplicates within a table, candidates across tables, resolve pairs
into entities, and stage passes together.

- [`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md)
  : Detect Duplicate Records
- [`deduplicate_table()`](https://edubruell.github.io/joinery/reference/deduplicate_table.md)
  : Deduplicate a Table
- [`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md)
  : Search for Candidate Matches Between Tables
- [`extract_unmatched()`](https://edubruell.github.io/joinery/reference/extract_unmatched.md)
  : Extract Unmatched Records
- [`materialize_records()`](https://edubruell.github.io/joinery/reference/materialize_records.md)
  : Materialize Records by ID
- [`resolve_entities()`](https://edubruell.github.io/joinery/reference/resolve_entities.md)
  : Group Matched Pairs into Entities
- [`multi_stage_dedup()`](https://edubruell.github.io/joinery/reference/multi_stage_dedup.md)
  : Staged Duplicate Detection (within one table)
- [`multi_stage_search()`](https://edubruell.github.io/joinery/reference/multi_stage_search.md)
  : Staged Search Across Tables or Sources
- [`prepare_search_data()`](https://edubruell.github.io/joinery/reference/prepare_search_data.md)
  : Prepare Data for Record Linkage Search
- [`compute_rarity()`](https://edubruell.github.io/joinery/reference/compute_rarity.md)
  : Compute Token Rarity for Record Linkage

## Diagnostics

Check a strategy before you run it, and inspect what it found.

- [`inspect_tokens()`](https://edubruell.github.io/joinery/reference/inspect_tokens.md)
  : Inspect Tokens for a Specific Column
- [`plan_strategy()`](https://edubruell.github.io/joinery/reference/plan_strategy.md)
  : Plan a Search Strategy from Raw Inputs
- [`audit_strategy()`](https://edubruell.github.io/joinery/reference/audit_strategy.md)
  : Audit a Search Strategy Against Data
- [`rarity_distribution()`](https://edubruell.github.io/joinery/reference/rarity_distribution.md)
  : Read the Token Rarity Distribution
- [`summarise_matches()`](https://edubruell.github.io/joinery/reference/summarise_matches.md)
  : Summarise a Match Result
- [`explain_match()`](https://edubruell.github.io/joinery/reference/explain_match.md)
  : Explain a Single Match
- [`sample_matches()`](https://edubruell.github.io/joinery/reference/sample_matches.md)
  : Sample Matches for Review
- [`compare_stages()`](https://edubruell.github.io/joinery/reference/compare_stages.md)
  : Compare Stages of a Multi-Stage Match
- [`recommendations()`](https://edubruell.github.io/joinery/reference/recommendations.md)
  : Recommendations from a Diagnostic Object

## Plots

Visualise a diagnostic result. Each verb returns data; these draw it.
The default plot() method of each diagnostic class calls the most useful
one of these.

- [`score_histogram()`](https://edubruell.github.io/joinery/reference/score_histogram.md)
  : Bar chart of the pre-binned score distribution
- [`score_density()`](https://edubruell.github.io/joinery/reference/score_density.md)
  : Kernel density of the score distribution
- [`coverage_plot()`](https://edubruell.github.io/joinery/reference/coverage_plot.md)
  : Bar chart of match coverage (base and/or target)
- [`cluster_size_plot()`](https://edubruell.github.io/joinery/reference/cluster_size_plot.md)
  : Bar chart of cluster-size distribution (duplicates only)
- [`ambiguity_plot()`](https://edubruell.github.io/joinery/reference/ambiguity_plot.md)
  : Bar chart of candidates-per-record distribution (candidates only)
- [`top_gap_density()`](https://edubruell.github.io/joinery/reference/top_gap_density.md)
  : Bar chart of top-1 vs top-2 score gap distribution (candidates only)
- [`rarity_histogram()`](https://edubruell.github.io/joinery/reference/rarity_histogram.md)
  : Bar chart of median token rarity per column
- [`token_frequency_plot()`](https://edubruell.github.io/joinery/reference/token_frequency_plot.md)
  : Bar chart of average tokens per record per column
- [`block_size_plot()`](https://edubruell.github.io/joinery/reference/block_size_plot.md)
  : Bar chart of block sizes (requires block_by on strategy)
- [`vocab_overlap_plot()`](https://edubruell.github.io/joinery/reference/vocab_overlap_plot.md)
  : Bar chart of vocabulary overlap between base and target per column
- [`similarity_histogram()`](https://edubruell.github.io/joinery/reference/similarity_histogram.md)
  : Histogram of sampled pairwise cosine similarities
- [`norm_plot()`](https://edubruell.github.io/joinery/reference/norm_plot.md)
  : Bar chart of embedding norm quantiles
- [`contribution_plot()`](https://edubruell.github.io/joinery/reference/contribution_plot.md)
  : Horizontal bar chart of per-column score contributions
- [`token_contribution_plot()`](https://edubruell.github.io/joinery/reference/token_contribution_plot.md)
  : Horizontal bar chart of per-token score contributions, coloured by
  column
- [`stage_coverage_plot()`](https://edubruell.github.io/joinery/reference/stage_coverage_plot.md)
  : Line plot of cumulative base coverage by stage
- [`stage_score_plot()`](https://edubruell.github.io/joinery/reference/stage_score_plot.md)
  : Grouped bar chart of score distributions by stage
- [`frontier_plot()`](https://edubruell.github.io/joinery/reference/frontier_plot.md)
  : Cost/recall frontier scatter for a strategy plan

## Calibration

Train and evaluate a false-positive filter on labelled pairs.

- [`export_for_labelling()`](https://edubruell.github.io/joinery/reference/export_for_labelling.md)
  : Export a match sample to CSV for manual labelling
- [`import_labels()`](https://edubruell.github.io/joinery/reference/import_labels.md)
  : Import a labelled CSV back into a feature/label table
- [`match_features()`](https://edubruell.github.io/joinery/reference/match_features.md)
  : Build a per-pair feature table for calibration
- [`fit_filter()`](https://edubruell.github.io/joinery/reference/fit_filter.md)
  : Fit a false-positive filter on labelled match pairs
- [`apply_filter()`](https://edubruell.github.io/joinery/reference/apply_filter.md)
  : Apply a fitted filter to match features
- [`calibrate_matches()`](https://edubruell.github.io/joinery/reference/calibrate_matches.md)
  : Calibrate matches end-to-end (features -\> filter -\> apply)
- [`calibrate()`](https://edubruell.github.io/joinery/reference/calibrate.md)
  : Evaluate a fitted filter on labelled pairs
- [`joinery_recipe()`](https://edubruell.github.io/joinery/reference/joinery_recipe.md)
  : Build a tidymodels recipe for calibration features

## Embeddings

Vector-based matching for the embedding strategy.

- [`compute_embeddings()`](https://edubruell.github.io/joinery/reference/compute_embeddings.md)
  : Compute Embeddings for Records
- [`score_embeddings()`](https://edubruell.github.io/joinery/reference/score_embeddings.md)
  : Score Embedding Pairs Using Cosine Similarity
- [`clear_embedding_cache()`](https://edubruell.github.io/joinery/reference/clear_embedding_cache.md)
  : Clear the embedding reuse cache

## DuckDB backend control

Tuning for the out-of-core DuckDB backend.

- [`duckdb_control()`](https://edubruell.github.io/joinery/reference/duckdb_control.md)
  : DuckDB Execution Control
- [`duckdb_batch_plan()`](https://edubruell.github.io/joinery/reference/duckdb_batch_plan.md)
  : Create a Batch Plan for DuckDB Table Processing
- [`batch_map()`](https://edubruell.github.io/joinery/reference/batch_map.md)
  : Apply a function to DuckDB table batches
- [`drop_joinery_temp_tables()`](https://edubruell.github.io/joinery/reference/drop_joinery_temp_tables.md)
  : Drop all temporary DuckDB tables created by joinery

## Example data

Synthetic datasets used throughout the documentation.

- [`base_example`](https://edubruell.github.io/joinery/reference/base_example.md)
  : Base dataset for record linkage example
- [`target_example`](https://edubruell.github.io/joinery/reference/target_example.md)
  : Target dataset for record linkage example
- [`workshop_register`](https://edubruell.github.io/joinery/reference/workshop_register.md)
  : Workshop guild register (base) for record linkage examples
- [`workshop_listings`](https://edubruell.github.io/joinery/reference/workshop_listings.md)
  : Workshop external directory (target) for record linkage examples
- [`workshop_panel`](https://edubruell.github.io/joinery/reference/workshop_panel.md)
  : Multi-year workshop panel for cross-year linkage examples
- [`match_labels_example`](https://edubruell.github.io/joinery/reference/match_labels_example.md)
  : Labelled candidate pairs for calibration examples
- [`street_types`](https://edubruell.github.io/joinery/reference/street_types.md)
  : Multilingual Street-Type Normalization Dictionary
- [`street_stopwords`](https://edubruell.github.io/joinery/reference/street_stopwords.md)
  : Multilingual Street-Name Stopwords
