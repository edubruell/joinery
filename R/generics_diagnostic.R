# ============================================================
# Diagnostic verb generics
# ============================================================
#
# Five verbs organised around the four user questions defined in
# notes/diagnostics_design.md:
#   Q1 (pre-match)        : audit_strategy()        -> Strategy_Audit
#   Q2 (post-match)       : summarise_matches()     -> Match_Overview
#   Q3 (attribution)      : explain_match()         -> Match_Explanation
#   Q4 (sample for review): sample_matches()        -> Match_Sample
#   multi-stage           : compare_stages()        -> Stage_Comparison
#
# Plus the cross-cutting `recommendations()` accessor. Methods on the
# individual diagnostic and calibration classes live alongside those
# class definitions (diagnostic_classes.R / calibration_classes.R).
# ============================================================


#' Audit a Search Strategy Against Data
#'
#' @description
#' Pre-match diagnostic (Q1). Runs preparation and rarity
#' computation, reports per-column token / rarity statistics and
#' (when `block_by` is set) block-size distribution and estimated
#' comparison count. Surfaces recommendations linking pre-match
#' symptoms to strategy levers.
#'
#' @param data A data.frame / tibble / data.table (or backend-specific table).
#' @param id Character scalar naming the ID column in `data`.
#' @param strategy A `Search_Strategy` object.
#' @param ... Additional backend-specific arguments. Notably:
#'   `target` (optional second table for cross-table vocabulary overlap)
#'   and `sample_n` (optional integer; if set, audit a random sample of rows).
#'
#' @return A `Strategy_Audit` object.
#'
#' @export
audit_strategy <- new_generic(
  "audit_strategy",
  c("data", "id", "strategy")
)


#' Summarise a Match Result
#'
#' @description
#' Post-match overview (Q2). Auto-detects whether the input
#' is a duplicate table (presence of `duplicate_group` column) or a
#' candidate table (presence of `match_id` and `source` columns), and
#' reports score distribution, coverage (when `base` / `target` are
#' supplied), cluster-size or candidates-per-record distribution, and
#' top-1-vs-top-2 score-gap distribution for candidates. Recommendations
#' link symptoms to strategy levers.
#'
#' @param matches Match output table from [detect_duplicates()] or
#'   [search_candidates()].
#' @param ... Method-specific arguments. The data.table method accepts:
#'   `base` (optional base input table for coverage), `target` (optional
#'   target input table for candidate coverage), and `bins` (integer
#'   number of histogram bins for the score distribution; default `50`).
#'
#' @return A `Match_Overview` object.
#'
#' @examples
#' \dontrun{
#' s <- search_strategy(
#'   name ~ normalize_text() + word_tokens(),
#'   threshold = 0.9
#' )
#' dups <- detect_duplicates(base_example, "id", s)
#' summarise_matches(dups, base = base_example)
#' }
#'
#' @export
summarise_matches <- new_generic(
  "summarise_matches",
  c("matches")
)


#' Explain a Single Match
#'
#' @description
#' Attribution diagnostic (Q3). Reconstructs per-column and
#' per-token contributions to a single match score. Dispatches on the
#' second positional argument: a [`Search_Strategy`] triggers
#' reconstruction from raw inputs; a tokens-shaped table is used
#' directly.
#'
#' @param matches Match output table.
#' @param x Either a [`Search_Strategy`] (ergonomic form) or a tokens
#'   table with `rarity` (power-user form).
#' @param ... Backend-specific arguments. For the ergonomic form:
#'   `base`, `target`, `match_id`.
#'
#' @return A `Match_Explanation` object.
#'
#' @export
explain_match <- new_generic(
  "explain_match",
  c("matches", "x")
)


#' Sample Matches for Review
#'
#' @description
#' Sampling diagnostic (Q4). Modes: `"high"`, `"low"`,
#' `"borderline"`, `"ambiguous"`, `"top_gap"`, `"random"`.
#'
#' @param matches Match output table.
#' @param ... Method-specific arguments. Standard arguments: `mode`
#'   (one of the sampling modes above), `n` (number of rows to sample),
#'   and mode-specific extras (e.g. `threshold` for `"borderline"`).
#'
#' @return A `Match_Sample` object.
#'
#' @export
sample_matches <- new_generic(
  "sample_matches",
  c("matches")
)


#' Compare Stages of a Multi-Stage Match
#'
#' @description
#' Multi-stage diagnostic. Produces per-stage
#' [`Match_Overview`]s, marginal coverage per stage, and overlaid
#' per-stage score distributions. Note that [summarise_matches()] does
#' **not** auto-detect a `stage` column — users explicitly call this
#' verb when they want per-stage analysis (see
#' `notes/diagnostics_design.md`).
#'
#' @param matches Multi-stage match table with a `stage` column.
#' @param ... Method-specific arguments. The data.table method will
#'   accept `base` and `target` for coverage.
#'
#' @return A `Stage_Comparison` object.
#'
#' @export
compare_stages <- new_generic(
  "compare_stages",
  c("matches")
)


#' Read the Token Rarity Distribution
#'
#' @description
#' A pre-match read of how token rarity is distributed in your data. For each
#' column (and block, when the strategy blocks) it reports the spread of token
#' document frequency and rarity, plus an offender list: the most common tokens,
#' the ones that drive a match to balloon. Use it to set `min_rarity` and
#' `max_token_df` from what is actually in the data instead of guessing.
#'
#' It never builds the pair set: it only tokenizes and measures rarity, so it
#' is cheap enough to run on a full corpus before committing to a strategy.
#'
#' @param data A data.frame / tibble / data.table (or backend-specific table).
#' @param id Character scalar naming the ID column in `data`.
#' @param strategy A `Search_Strategy` object.
#' @param ... Additional backend-specific arguments. Notably `n_offenders`
#'   (integer; how many top-df tokens to list, default 20) and `sample_n`
#'   (DuckDB: rows to pull before delegating; default all).
#'
#' @return A `Rarity_Distribution` object.
#'
#' @seealso [search_strategy()] for the `min_rarity` / `max_token_df` levers
#'   this verb informs; [audit_strategy()] for the broader pre-match audit.
#'
#' @export
rarity_distribution <- new_generic(
  "rarity_distribution",
  c("data", "id", "strategy")
)


#' Plan a Search Strategy from Raw Inputs
#'
#' @description
#' Helps you choose a blocking before you run anything. Where
#' [audit_strategy()] grades a strategy you have already settled on, and
#' [rarity_distribution()] reads one column's token distribution,
#' `plan_strategy()` compares several candidate blockings side by side and
#' shows the trade-off between how many comparisons each one costs and how many
#' true matches it would keep together.
#'
#' It never builds the pair set, so it is safe to run on a full corpus. For
#' each candidate blocking it reports: how many blocks it makes and how big
#' they are, an estimate of how many record comparisons it implies, and the
#' share of identical-token records that stay in the same block (the recall it
#' would cost you). It also reports how much an [exact_strategy()] front stage
#' would absorb, the shape of the leftover records, and how discriminative each
#' column is, including a warning when a column that is often empty puts a
#' ceiling on achievable scores.
#'
#' The strategy you pass supplies only the column preparation steps; its own
#' `block_by` is ignored, since the blocking is exactly what you are choosing
#' here.
#'
#' @param base A data.frame / tibble / data.table (or backend table).
#' @param strategy A `Search_Strategy` supplying the tokenization to plan
#'   against.
#' @param target Optional second table. `NULL` (default) plans a dedup;
#'   non-`NULL` plans a cross-table search.
#' @param block_candidates Named list of candidate `block_by` specs to compare
#'   (e.g. `list(plz2 = "plz2", plz5_wz = c("plz5", "wz08_3"))`).
#' @param base_id Character scalar naming the id column in `base` (required).
#' @param target_id Character scalar naming the id column in `target`
#'   (defaults to `base_id`).
#' @param n_offenders Number of top-`df` "offender" tokens (the fan-out drivers)
#'   to report per column. Defaults to `20`.
#' @param min_rarity_grid Optional numeric vector of `min_rarity` cut points for
#'   the cost curve. `NULL` (default) picks a grid from the rarity distribution.
#' @param containment Logical. When `TRUE`, adds the per-column containment
#'   share, the one read that performs a bounded structural join. Defaults to
#'   `FALSE`, which keeps `plan_strategy()` scoring-free.
#' @param ... Backend-specific arguments, such as `sample_n` (DuckDB).
#'
#' @return A `Strategy_Plan` object.
#'
#' @seealso [audit_strategy()] to grade a chosen strategy,
#'   [rarity_distribution()] for one column's distribution,
#'   [exact_strategy()] for the front stage it sizes.
#'
#' @export
plan_strategy <- new_generic(
  "plan_strategy",
  c("base", "strategy"),
  function(base, strategy, target = NULL, block_candidates = list(),
           base_id = NULL, target_id = NULL, n_offenders = 20L,
           min_rarity_grid = NULL, containment = FALSE, ...) {
    S7_dispatch()
  }
)


#' Recommendations from a Diagnostic Object
#'
#' @description
#' Accessor returning the recommendations strings stored on a
#' diagnostic result object. Returns `character(0)` when no
#' recommendations fired. The same strings are surfaced inline by the
#' object's `print()` method.
#'
#' Methods for individual classes live alongside those classes —
#' diagnostic classes (`Match_Overview`, `Strategy_Audit`) in
#' `diagnostic_classes.R`; calibration classes (`Calibrated_Matches`,
#' `Filter_Calibration`) in `calibration_classes.R`.
#'
#' @param x A diagnostic result object (`Strategy_Audit`,
#'   `Match_Overview`, `Calibrated_Matches`, `Filter_Calibration`).
#' @param ... Reserved for future methods.
#'
#' @return A character vector.
#'
#' @export
recommendations <- new_generic("recommendations", "x", function(x, ...) S7_dispatch())
