# ============================================================
# S7 generics — calibration primitives
# ============================================================
#
# Generics that support the post-retrieval calibration workflow:
# auxiliary registry construction, aIP, match_features(), and the
# fit_filter() / apply_filter() / calibrate_matches() chain.
#
# ============================================================

#' Prepare Auxiliary Search-Side Registry
#'
#' @description
#' Internal primitive. Builds a per-column token-occurrence
#' registry over the *auxiliary* (search / target) side of a linkage
#' task. Together with the base-side registry produced by
#' [compute_rarity()], it feeds the absolute identification potential
#' (`aIP`) used by [compute_aip()] and the forthcoming
#' `match_features()` verb.
#'
#' Unlike [compute_rarity()], the auxiliary registry is **block-agnostic
#' and cross-table**: occurrences are aggregated globally per
#' `(src_column, token)` regardless of any `block_by` setting on the
#' strategy. This matches the SearchEngine whitepaper's definition
#' (Doherr 2023, eq. 9) and the design note
#' `notes/calibration_design.md`.
#'
#' @param data A data.frame / tibble / data.table (or backend-specific
#'   table) representing the auxiliary (search / target) side.
#' @param id  Character scalar naming the ID column in `data`.
#' @param strategy A `Search_Strategy` object. Only the preparers are
#'   used; `block_by` and `weights` are deliberately ignored.
#' @param ... Additional backend-specific arguments.
#'
#' @return A backend-specific table with columns
#'   `src_column`, `token`, `occ` (number of distinct records in
#'   which the token appears), and `maxocc` (per `src_column` maximum
#'   of `occ`).
#'
#' @noRd
prepare_auxiliary_registry <- new_generic(
  "prepare_auxiliary_registry",
  c("data", "id", "strategy")
)


#' Build a per-pair feature table for calibration
#'
#' @description
#' Computes a wide, one-row-per-pair feature `data.table` from a joinery
#' match result, suitable for downstream calibration / false-positive
#' filtering. The schema is documented in
#' `notes/calibration_design.md` and treated as the public API.
#' Additions are allowed; reorders or renames are not.
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
#'   `stringdist` suggested package), `method` (stringdist method:
#'   a scalar applied to every column (default `"jw"`), or a named
#'   character vector selecting a per-column method — a scalar is the
#'   degenerate single-element case), and `include_block_stats` (logical; whether to
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
calibrate <- new_generic(
  "calibrate", "x",
  function(x, labels = NULL, bins = 10L, ...) {
    S7_dispatch()
  }
)
