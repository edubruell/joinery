# ============================================================
# Match Statistics S7 Generics
# ============================================================
#
# Defines S7 generics for match quality assessment, strategy
# diagnostics, and match review tools.
#
# Backend implementations:
#   - data.table: R/matchstat_datatable.R
#   - DuckDB:     R/matchstat_duckdb.R
#   - tibble/df:  R/matchstat_tibble.R (delegates to data.table)
#
# Implementation is phased:
#   Phase 1: Core match summaries (this file)
#   Phase 2: Strategy diagnostics
#   Phase 3: Advanced tools
#
# See notes/matchstat_design.md for full design specification.
# ============================================================


# ============================================================
# S7 Classes for Summary Objects
# ============================================================

#' Match Summary Object
#'
#' @description
#' S7 class representing summary statistics for match results.
#' Created by `summarize_matches()`.
#'
#' @slot match_type Character: "duplicates" or "candidates"
#' @slot n_matches Integer: total match groups or pairs
#' @slot n_records Integer: total records involved in matches
#' @slot coverage_rate Numeric: proportion of input records matched
#' @slot score_summary Named numeric: min, mean, median, max, sd of scores
#' @slot score_quantiles Named numeric: 0%, 25%, 50%, 75%, 100% quantiles
#' @slot duplicate_stats List: (if duplicates) cluster size distribution, 
#'   avg cluster size, max cluster size
#' @slot candidate_stats List: (if candidates) n_base_matched, n_target_matched,
#'   base_coverage, target_coverage, ambiguous_matches
#'
#' @export
Match_Summary <- new_class(
  "Match_Summary",
  properties = list(
    match_type       = class_character,
    n_matches        = class_integer,
    n_records        = class_integer,
    coverage_rate    = class_numeric,
    score_summary    = class_numeric,
    score_quantiles  = class_numeric,
    duplicate_stats  = class_list,
    candidate_stats  = class_list
  )
)


# ============================================================
# Phase 1: Core Match Summaries
# ============================================================

#' Summarize Match Results
#'
#' @description
#' Compute summary statistics for match outputs from `detect_duplicates()`
#' or `search_candidates()`. Returns a `Match_Summary` object with coverage
#' rates, score distributions, and match-type-specific metrics.
#'
#' @param matches Match output table from `detect_duplicates()` or 
#'   `search_candidates()`. Must contain at minimum: score column and 
#'   appropriate ID columns.
#' @param type Character: "duplicates" or "candidates". Determines which
#'   schema to expect and which metrics to compute. If NULL (default),
#'   auto-detects based on presence of duplicate_group vs match_id column.
#' @param base_id Character: (candidates only) ID column name for base table.
#'   Required when type = "candidates" to compute base coverage.
#' @param target_id Character: (candidates only) ID column name for target table.
#'   Required when type = "candidates" to compute target coverage.
#'
#' @return A `Match_Summary` S7 object with slots:
#'   - `n_matches`: total match groups or pairs
#'   - `n_records`: total records involved
#'   - `coverage_rate`: % of input records matched
#'   - `score_summary`: min/mean/median/max/sd
#'   - `score_quantiles`: 0%, 25%, 50%, 75%, 100%
#'   - `duplicate_stats`: (duplicates only) cluster size metrics
#'   - `candidate_stats`: (candidates only) base/target coverage, ambiguous matches
#'
#' @examples
#' \dontrun{
#' # Duplicate detection summary
#' duplicates <- detect_duplicates(data, "id", strategy)
#' summarize_matches(duplicates, type = "duplicates")
#'
#' # Candidate matching summary
#' matches <- search_candidates(base, target, "id_base", "id_target", strategy)
#' summarize_matches(matches, type = "candidates", 
#'                   base_id = "id_base", target_id = "id_target")
#' }
#'
#' @export
summarize_matches <- new_generic(
  "summarize_matches",
  c("matches")
)


#' Score Distribution
#'
#' @description
#' Compute or visualize the distribution of match scores. Useful for
#' threshold tuning, identifying natural score gaps, and detecting
#' bimodal distributions (high-confidence vs borderline matches).
#'
#' @param matches Match output table from `detect_duplicates()` or
#'   `search_candidates()`. Must contain a score column.
#' @param breaks Integer number of bins, or numeric vector of custom
#'   breakpoints. Default is 10 bins.
#' @param plot Logical: if TRUE, returns a simple histogram plot.
#'   If FALSE (default), returns a data.frame with bins and counts.
#'
#' @return 
#'   - If `plot = FALSE`: data.frame with columns (score_bin, count, pct)
#'   - If `plot = TRUE`: a plot object showing score distribution
#'
#' @examples
#' \dontrun{
#' matches <- search_candidates(base, target, "id_base", "id_target", strategy)
#' 
#' # Get distribution table
#' score_distribution(matches, breaks = 20)
#' 
#' # Plot distribution
#' score_distribution(matches, breaks = 10, plot = TRUE)
#' }
#'
#' @export
score_distribution <- new_generic(
  "score_distribution",
  c("matches")
)


#' Sample Matches for Review
#'
#' @description
#' Extract a sample of matches for manual validation. Supports multiple
#' sampling strategies: highest scores, lowest scores, borderline cases
#' near threshold, or random sample.
#'
#' @param matches Match output table from `detect_duplicates()` or
#'   `search_candidates()`.
#' @param n Integer: number of matches to sample. Default is 10.
#' @param type Character: sampling strategy. One of:
#'   - `"high"`: top n matches by score
#'   - `"low"`: bottom n matches by score (but above threshold)
#'   - `"borderline"`: n matches closest to threshold
#'   - `"random"`: random sample of n matches
#' @param threshold Numeric: (required for "borderline") the threshold value
#'   to find matches near. If NULL, uses the minimum score in matches.
#'
#' @return Subset of `matches` containing the sampled rows, maintaining
#'   the original schema and column order.
#'
#' @examples
#' \dontrun{
#' matches <- search_candidates(base, target, "id_base", "id_target", strategy)
#' 
#' # Review highest-scoring matches
#' sample_matches(matches, n = 5, type = "high")
#' 
#' # Review borderline cases near threshold
#' sample_matches(matches, n = 10, type = "borderline", threshold = 0.8)
#' 
#' # Random sample for spot-checking
#' sample_matches(matches, n = 20, type = "random")
#' }
#'
#' @export
sample_matches <- new_generic(
  "sample_matches",
  c("matches")
)


# ============================================================
# Phase 2: Strategy Diagnostics (Stubs for Future)
# ============================================================

# Uncomment and implement in Phase 2:
#
# #' @export
# summarize_strategy <- new_generic("summarize_strategy", c("data", "id", "strategy"))
#
# #' @export
# token_stats <- new_generic("token_stats", c("tokens"))
#
# #' @export
# rarity_distribution <- new_generic("rarity_distribution", c("tokens", "strategy"))
#
# #' @export
# block_stats <- new_generic("block_stats", c("data", "block_by"))


# ============================================================
# Phase 3: Advanced Tools (Stubs for Future)
# ============================================================

# Uncomment and implement in Phase 3:
#
# #' @export
# explain_match <- new_generic("explain_match", c("matches", "match_id", "tokens"))
#
# #' @export
# ambiguous_matches <- new_generic("ambiguous_matches", c("matches"))
