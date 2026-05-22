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
