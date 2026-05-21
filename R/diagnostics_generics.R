# ============================================================
# Diagnostic verb generics
# ============================================================
#
# Five verbs organised around the four user questions defined in
# notes/diagnostics_design.md §3:
#   Q1 (pre-match)        : audit_strategy()        -> Strategy_Audit
#   Q2 (post-match)       : summarise_matches()     -> Match_Overview
#   Q3 (attribution)      : explain_match()         -> Match_Explanation
#   Q4 (sample for review): sample_matches()        -> Match_Sample
#   multi-stage           : compare_stages()        -> Stage_Comparison
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
#' `notes/diagnostics_design.md` §10).
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


#' Build a per-pair feature table for calibration
#'
#' @description
#' Computes a wide, one-row-per-pair feature `data.table` from a joinery
#' match result, suitable for downstream calibration / false-positive
#' filtering. The schema is documented in
#' `notes/calibration_design.md` §6 and treated as the public API of
#' v0.7. Additions are allowed; reorders or renames are not.
#'
#' Dispatches on `(matches, strategy)`. A [`Search_Strategy`] returns
#' the full token schema (core + token-side columns + string similarity).
#' An [`Embedding_Strategy`] returns the reduced "embedding" schema
#' (core columns + string similarity + `cosine_sim` + embedding norms).
#'
#' @param matches A match result table (data.table / tibble / data.frame
#'   / DuckDB lazy `tbl`) from [detect_duplicates()] or
#'   [search_candidates()].
#' @param strategy The [`Search_Strategy`] or [`Embedding_Strategy`]
#'   used to produce `matches`.
#' @param ... Method-specific arguments. Both strategy methods accept:
#'   `base` (the base table used as input to matching), `id` (character
#'   scalar naming the ID column in `base`), `target` (optional target
#'   table for cross-table candidate matches), `target_id` (ID column
#'   in `target`, defaults to `id`), `include_string_sim` (logical;
#'   when `TRUE` (default) emits `sim_sf_<col>` / `sim_fs_<col>`
#'   per column via `stringdist::stringsim()` — requires the
#'   `stringdist` suggested package), `method` (stringdist method,
#'   default `"jw"`), and `include_block_stats` (logical; whether to
#'   compute `cnt` / `icnt` / `ipos`). The [`Search_Strategy`] method
#'   additionally accepts `top_n` (named integer / list controlling
#'   per-column top-N counts for the `m_/f_/s_` columns; use a
#'   `default` entry as fallback; set a column to 0 to suppress its
#'   set). The [`Embedding_Strategy`] method emits `cosine_sim`
#'   (pass-through of `score`) and `embedding_norm_s` /
#'   `embedding_norm_f` (L2 norms of the **pre-normalization**
#'   embeddings, recomputed only over the matched record subset).
#'
#' @return A [`Match_Features`] object wrapping a wide feature
#'   `data.table`.
#'
#' @export
match_features <- new_generic(
  "match_features",
  c("matches", "strategy")
)


#' Recommendations from a Diagnostic Object
#'
#' @description
#' Accessor returning the recommendations strings stored on a
#' diagnostic result object. Returns `character(0)` when no
#' recommendations fired. The same strings are surfaced inline by the
#' object's `print()` method.
#'
#' @param x A diagnostic result object (`Strategy_Audit`,
#'   `Match_Overview`).
#' @param ... Reserved for future methods.
#'
#' @return A character vector.
#'
#' @export
recommendations <- new_generic("recommendations", "x", function(x, ...) S7_dispatch())

method(recommendations, Match_Overview)     <- function(x) x@recommendations
method(recommendations, Strategy_Audit)     <- function(x) x@recommendations
method(recommendations, Calibrated_Matches) <- function(x) x@recommendations
method(recommendations, Filter_Calibration) <- function(x) x@recommendations


#' Fit a false-positive filter on labelled match pairs
#'
#' @description
#' Fit a baseline classifier to predict whether each scored pair is a
#' true match (`equal == 1L`) or a false positive (`equal == 0L`).
#' The baseline path uses `stats::glm` with the logit link and no
#' external dependencies. The features object is the input from
#' [match_features()]; labels carry the `equal` column produced by
#' [import_labels()].
#'
#' @param features A [`Match_Features`] object.
#' @param labels A `data.table` / `data.frame` with the matches schema
#'   plus an integer `equal` column (`0L` / `1L`). Typically produced by
#'   [import_labels()].
#' @param model Character scalar (default `"logistic"`) selecting the
#'   baseline `glm()` path. Future M6 work will accept a fitted parsnip
#'   / workflow object here.
#' @param class_weighted Logical scalar. When `TRUE`, fit `glm` with
#'   inverse-class-frequency `weights =`, useful for imbalanced
#'   training sets. Default `FALSE`.
#' @param na_fill Numeric scalar used to impute predictor NAs. Default
#'   `0` (sensible for aIP slot columns where NA means "no token").
#' @param ... Reserved for future expansion.
#'
#' @return A [`Filter_Model`] object.
#'
#' @export
fit_filter <- function(features, labels,
                       model = "logistic",
                       class_weighted = FALSE,
                       na_fill = 0,
                       ...) {
  .fit_filter_impl(
    features       = features,
    labels         = labels,
    model          = model,
    class_weighted = class_weighted,
    na_fill        = na_fill,
    ...
  )
}


#' Apply a fitted filter to match features
#'
#' @description
#' Score a [`Match_Features`] table with a fitted [`Filter_Model`] and
#' return a [`Calibrated_Matches`] object. When `matches` is supplied,
#' the original match table is enriched with `tp_prob` and
#' `predicted_tp` columns and stored in the result's `@matches` slot;
#' when `matches` is `NULL`, the features table itself is enriched and
#' stored.
#'
#' @param features A [`Match_Features`] object.
#' @param filter_model A [`Filter_Model`] produced by [fit_filter()].
#' @param threshold Numeric scalar in [0, 1] or `NULL`. When `NULL`,
#'   the threshold is chosen by Youden's J on the training labels
#'   stored on the [`Filter_Model`]. Decision 13.7 default.
#' @param matches Optional raw matches table to enrich. When supplied,
#'   `tp_prob` / `predicted_tp` are broadcast onto every row of the
#'   pair (candidates: both `source == "base"` and `source == "target"`
#'   rows of a `match_id`; duplicates: every row of a `duplicate_group`).
#' @param ... Reserved for future expansion.
#'
#' @return A [`Calibrated_Matches`] object.
#'
#' @export
apply_filter <- function(features, filter_model,
                         threshold = NULL,
                         matches   = NULL,
                         ...) {
  .apply_filter_impl(
    features      = features,
    filter_model  = filter_model,
    threshold     = threshold,
    matches       = matches,
    ...
  )
}


#' Calibrate matches end-to-end (features -> filter -> apply)
#'
#' @description
#' High-level Q5 verb. Builds features via [match_features()], fits a
#' [`Filter_Model`] via [fit_filter()], and applies it via
#' [apply_filter()] to return a [`Calibrated_Matches`] object enriched
#' with `tp_prob` / `predicted_tp`. Dispatches on the strategy class.
#'
#' @param matches Match output table (data.table / tibble / data.frame
#'   / DuckDB lazy `tbl`).
#' @param strategy The [`Search_Strategy`] or [`Embedding_Strategy`]
#'   used to produce `matches`.
#' @param ... Method-specific arguments. Required: `labels` (manually
#'   labelled rows produced by [import_labels()]), `base`, and `id`.
#'   Optional: `target`, `target_id` (forwarded to [match_features()]),
#'   `model`, `class_weighted`, `na_fill`, `threshold`, plus all
#'   [match_features()] tuning knobs (`top_n`, `include_string_sim`,
#'   `include_block_stats`, `method`).
#'
#' @return A [`Calibrated_Matches`] object.
#'
#' @export
calibrate_matches <- new_generic(
  "calibrate_matches",
  c("matches", "strategy")
)


#' Evaluate a fitted filter on labelled pairs
#'
#' @description
#' Compute calibration diagnostics for a fitted false-positive filter on
#' a labelled evaluation set. Returns a [`Filter_Calibration`] carrying
#' the reliability table, Brier score, log-loss, per-class confusion
#' matrix, and a threshold sweep curve.
#'
#' Two call shapes:
#'   * `calibrate(calibrated_matches, labels)` — evaluate on labels held
#'      out from the training fit.
#'   * `calibrate(calibrated_matches)` — evaluate on the training labels
#'      stored on the [`Filter_Model`] (sanity-check view; do not use
#'      for model selection).
#'
#' @param x A [`Calibrated_Matches`] object from [apply_filter()] /
#'   [calibrate_matches()].
#' @param labels Optional labels `data.table` (typically from
#'   [import_labels()]) for held-out evaluation.
#' @param bins Integer. Number of equal-width probability bins for the
#'   reliability table. Default `10`.
#' @param ... Reserved for future expansion.
#'
#' @return A [`Filter_Calibration`] object.
#'
#' @export
calibrate <- new_generic("calibrate", "x")


#' Build a tidymodels recipe for calibration features
#'
#' @description
#' Construct a pre-configured [recipes::recipe()] suitable for fitting a
#' false-positive filter on the output of [match_features()]. Tags ID
#' columns (`searched`, `found`, `match_id`) with role `"id"`, sets
#' `equal` as the outcome, and keeps every other numeric column as a
#' predictor. Requires the suggested `recipes` package.
#'
#' @param features A [`Match_Features`] object.
#' @param labels A labels `data.table` with `equal` (as for
#'   [fit_filter()]).
#' @param ... Reserved for future expansion.
#'
#' @return A [recipes::recipe()] object.
#'
#' @export
joinery_recipe <- function(features, labels, ...) {
  .joinery_recipe_impl(features, labels, ...)
}
