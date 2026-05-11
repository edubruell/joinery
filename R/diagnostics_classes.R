# ============================================================
# Diagnostic result classes (Phase 0.6)
# ============================================================
#
# Five small S7 result classes — one per question the diagnostics
# module is organised around. See notes/diagnostics_design.md §4.
#
# Each class carries summary structures only (small data.tables and
# named lists) plus a `recommendations` character vector drawn from
# the catalog in R/diagnostics_recommendations.R.
#
# Conventions (mirrored on every class):
#   - format(x, ...) returns the printable lines as a character vector
#     (used by print() internally and by tests for snapshotting; this
#     avoids cli-capture brittleness — see commit 72e6722).
#   - print(x, ...) emits formatted output via cli and returns invisible(x).
#   - as.data.table(x) / as.data.frame(x) flatten to a single-row
#     summary table; distributions remain accessible via slot accessors.
# ============================================================


# ---------------------------------------------------------------------------
# Class definitions
# ---------------------------------------------------------------------------

#' Strategy Audit Result
#'
#' @description
#' Result of [audit_strategy()] (Phase 0.6 Q1, pre-match).
#'
#' @slot n_records Integer. Number of records audited.
#' @slot block_summary `data.table` or `NULL`. Distribution of block sizes
#'   when `block_by` is set on the strategy.
#' @slot column_token_stats `data.table`. Per-column token counts, unique
#'   tokens, NA rate.
#' @slot column_rarity_stats `data.table`. Per-column rarity quantiles.
#' @slot est_comparisons Numeric scalar or `NULL`. Estimated number of
#'   pairwise comparisons given blocking.
#' @slot recommendations Character. Strings from the recommendations catalog.
#'
#' @noRd
Strategy_Audit <- new_class(
  "Strategy_Audit",
  properties = list(
    n_records           = class_integer,
    block_summary       = class_any,
    column_token_stats  = class_any,
    column_rarity_stats = class_any,
    est_comparisons     = class_any,
    recommendations     = class_character
  )
)


#' Match Overview Result
#'
#' @description
#' Result of [summarise_matches()] (Phase 0.6 Q2, post-match overview).
#' Unified across duplicate and candidate match types via the
#' `match_type` slot.
#'
#' @slot match_type Character. Either `"duplicates"` or `"candidates"`.
#' @slot n_records Named list with `n_pairs_or_groups` and
#'   `n_records_involved`.
#' @slot coverage Named list with `base_coverage` and `target_coverage`
#'   (both `NA` when input tables were not supplied).
#' @slot score_dist Named list with `summary` (named numeric:
#'   min/q1/median/mean/q3/max), `quantiles` (named numeric at fixed
#'   probabilities), `histogram` (`data.table` with `bin_lower`,
#'   `bin_upper`, `count`), and `threshold` (numeric or `NA`).
#' @slot cluster_dist `data.table` or `NULL`. Cluster-size distribution
#'   for `match_type == "duplicates"`.
#' @slot cluster_summary Named list or `NULL`. `max_cluster_size`,
#'   `pct_records_in_cluster` for duplicates.
#' @slot ambiguity_dist `data.table` or `NULL`. Candidates-per-record
#'   distribution for `match_type == "candidates"`.
#' @slot top_gap_dist `data.table` or `NULL`. Top-1 vs top-2 score gap
#'   distribution for candidates.
#' @slot recommendations Character. Strings from the recommendations catalog.
#'
#' @noRd
Match_Overview <- new_class(
  "Match_Overview",
  properties = list(
    match_type      = class_character,
    n_records       = class_list,
    coverage        = class_list,
    score_dist      = class_list,
    cluster_dist    = class_any,
    cluster_summary = class_any,
    ambiguity_dist  = class_any,
    top_gap_dist    = class_any,
    recommendations = class_character
  )
)


#' Match Explanation Result
#'
#' @description
#' Result of [explain_match()] (Phase 0.6 Q3, attribution). Slots are
#' defined now to lock the surface; method dispatch is implemented in M4.
#'
#' @slot match_id Integer scalar. Identifier of the explained pair / group.
#' @slot pair `data.table`. Original column values for the records in the pair.
#' @slot per_column_contrib `data.table`. Per-column weighted contribution.
#' @slot shared_tokens `data.table`. Per-token rarity and rIP contribution.
#' @slot score Numeric scalar. Total score for the pair.
#' @slot score_breakdown Named list. Smoothing / feedback adjustments
#'   recorded so contributions round-trip to score.
#'
#' @noRd
Match_Explanation <- new_class(
  "Match_Explanation",
  properties = list(
    match_id           = class_integer,
    pair               = class_any,
    per_column_contrib = class_any,
    shared_tokens      = class_any,
    score              = class_numeric,
    score_breakdown    = class_list
  )
)


#' Match Sample Result
#'
#' @description
#' Result of [sample_matches()] (Phase 0.6 Q4, sampling for review).
#' Slots are defined now to lock the surface; implementation lands in M5.
#'
#' @slot mode Character scalar. One of `"high"`, `"low"`, `"borderline"`,
#'   `"ambiguous"`, `"top_gap"`, `"random"`.
#' @slot criteria Named list. Mode-specific parameters (e.g. `threshold`
#'   for `"borderline"`).
#' @slot rows `data.table`. Sampled rows from the matches table.
#'
#' @noRd
Match_Sample <- new_class(
  "Match_Sample",
  properties = list(
    mode     = class_character,
    criteria = class_list,
    rows     = class_any
  )
)


#' Stage Comparison Result
#'
#' @description
#' Result of [compare_stages()] (Phase 0.6 multi-stage diagnostics).
#' Slots are defined now to lock the surface; implementation lands in M6.
#'
#' @slot per_stage_overview Named list of `Match_Overview` objects.
#' @slot marginal_coverage `data.table`. Records added by each stage.
#' @slot score_dist_by_stage `data.table`. Long-form score distributions
#'   per stage for overlay plotting.
#'
#' @noRd
Stage_Comparison <- new_class(
  "Stage_Comparison",
  properties = list(
    per_stage_overview  = class_list,
    marginal_coverage   = class_any,
    score_dist_by_stage = class_any
  )
)


# ---------------------------------------------------------------------------
# External generics for print, format, as.data.table, as.data.frame
# ---------------------------------------------------------------------------

#' @noRd
print.Match_Overview <- new_external_generic("base", "print", "x")
#' @noRd
print.Strategy_Audit <- new_external_generic("base", "print", "x")
#' @noRd
print.Match_Explanation <- new_external_generic("base", "print", "x")
#' @noRd
print.Match_Sample <- new_external_generic("base", "print", "x")
#' @noRd
print.Stage_Comparison <- new_external_generic("base", "print", "x")

#' @noRd
format.Match_Overview <- new_external_generic("base", "format", "x")
#' @noRd
format.Strategy_Audit <- new_external_generic("base", "format", "x")
#' @noRd
format.Match_Explanation <- new_external_generic("base", "format", "x")
#' @noRd
format.Match_Sample <- new_external_generic("base", "format", "x")
#' @noRd
format.Stage_Comparison <- new_external_generic("base", "format", "x")

#' @noRd
as.data.table.Match_Overview <- new_external_generic(
  "data.table", "as.data.table", "x"
)
#' @noRd
as.data.frame.Match_Overview <- new_external_generic(
  "base", "as.data.frame", "x"
)


# ---------------------------------------------------------------------------
# format() / print() — Match_Overview
# ---------------------------------------------------------------------------

#' @noRd
.format_match_overview <- function(x) {
  lines <- character()
  push <- function(...) lines <<- c(lines, paste0(...))

  push("<joinery::Match_Overview> (", x@match_type, ")")
  push("")
  push(sprintf(
    "n_pairs_or_groups: %d   n_records_involved: %d",
    as.integer(x@n_records$n_pairs_or_groups %||% NA_integer_),
    as.integer(x@n_records$n_records_involved %||% NA_integer_)
  ))

  cov <- x@coverage
  bc <- cov$base_coverage
  tc <- cov$target_coverage
  push(sprintf(
    "coverage: base=%s   target=%s",
    if (is.null(bc) || is.na(bc)) "NA" else sprintf("%.1f%%", 100 * bc),
    if (is.null(tc) || is.na(tc)) "NA" else sprintf("%.1f%%", 100 * tc)
  ))

  s <- x@score_dist$summary
  if (!is.null(s)) {
    push("score summary:")
    push(sprintf(
      "  min=%.3f  q1=%.3f  median=%.3f  mean=%.3f  q3=%.3f  max=%.3f",
      s[["min"]], s[["q1"]], s[["median"]],
      s[["mean"]], s[["q3"]], s[["max"]]
    ))
  }

  if (x@match_type == "duplicates" && !is.null(x@cluster_dist)) {
    push("")
    push("cluster size distribution (top 5):")
    cd <- utils::head(x@cluster_dist, 5)
    for (i in seq_len(nrow(cd))) {
      push(sprintf(
        "  size %d: %d cluster(s)",
        cd$cluster_size[i], cd$n_clusters[i]
      ))
    }
    cs <- x@cluster_summary
    if (!is.null(cs)) {
      push(sprintf(
        "  max_cluster_size=%s   pct_records_in_cluster=%s",
        format(cs$max_cluster_size),
        if (is.na(cs$pct_records_in_cluster)) "NA"
        else sprintf("%.1f%%", 100 * cs$pct_records_in_cluster)
      ))
    }
  }

  if (x@match_type == "candidates" && !is.null(x@ambiguity_dist)) {
    push("")
    push("candidates-per-record distribution (top 5):")
    ad <- utils::head(x@ambiguity_dist, 5)
    for (i in seq_len(nrow(ad))) {
      push(sprintf(
        "  %d candidate(s): %d record(s)",
        ad$candidates_per_record[i], ad$n_records[i]
      ))
    }
  }

  if (length(x@recommendations) > 0) {
    push("")
    push("recommendations:")
    for (r in x@recommendations) push("  ! ", r)
  }

  lines
}

method(format.Match_Overview, Match_Overview) <- function(x, ...) {
  .format_match_overview(x)
}

method(print.Match_Overview, Match_Overview) <- function(x, ...) {
  cli::cli_h1(sprintf("Match_Overview ({.field %s})", x@match_type))
  cli::cli_text(sprintf(
    "n_pairs_or_groups: {.val %d}   n_records_involved: {.val %d}",
    as.integer(x@n_records$n_pairs_or_groups %||% NA_integer_),
    as.integer(x@n_records$n_records_involved %||% NA_integer_)
  ))

  cov <- x@coverage
  bc <- cov$base_coverage
  tc <- cov$target_coverage
  cli::cli_text(sprintf(
    "coverage: base=%s   target=%s",
    if (is.null(bc) || is.na(bc)) "NA" else sprintf("%.1f%%", 100 * bc),
    if (is.null(tc) || is.na(tc)) "NA" else sprintf("%.1f%%", 100 * tc)
  ))

  s <- x@score_dist$summary
  if (!is.null(s)) {
    cli::cli_text("{.strong score summary}")
    cli::cli_bullets(c(
      sprintf("min: %.3f", s[["min"]]),
      sprintf("q1: %.3f", s[["q1"]]),
      sprintf("median: %.3f", s[["median"]]),
      sprintf("mean: %.3f", s[["mean"]]),
      sprintf("q3: %.3f", s[["q3"]]),
      sprintf("max: %.3f", s[["max"]])
    ))
  }

  if (x@match_type == "duplicates" && !is.null(x@cluster_dist) &&
      nrow(x@cluster_dist) > 0) {
    cli::cli_text("{.strong cluster size distribution} (top 5)")
    cd <- utils::head(x@cluster_dist, 5)
    bullets <- sprintf(
      "size %d: %d cluster(s)",
      cd$cluster_size, cd$n_clusters
    )
    cli::cli_bullets(bullets)
  }

  if (x@match_type == "candidates" && !is.null(x@ambiguity_dist) &&
      nrow(x@ambiguity_dist) > 0) {
    cli::cli_text("{.strong candidates-per-record} (top 5)")
    ad <- utils::head(x@ambiguity_dist, 5)
    bullets <- sprintf(
      "%d candidate(s): %d record(s)",
      ad$candidates_per_record, ad$n_records
    )
    cli::cli_bullets(bullets)
  }

  for (r in x@recommendations) {
    cli::cli_alert_warning(r)
  }

  invisible(x)
}


# ---------------------------------------------------------------------------
# Coercion — Match_Overview
# ---------------------------------------------------------------------------

#' @noRd
.match_overview_to_dt <- function(x) {
  s <- x@score_dist$summary %||% rep(NA_real_, 6L)
  data.table::data.table(
    match_type            = x@match_type,
    n_pairs_or_groups     = as.integer(x@n_records$n_pairs_or_groups %||% NA_integer_),
    n_records_involved    = as.integer(x@n_records$n_records_involved %||% NA_integer_),
    base_coverage         = as.numeric(x@coverage$base_coverage %||% NA_real_),
    target_coverage       = as.numeric(x@coverage$target_coverage %||% NA_real_),
    score_min             = unname(s["min"]),
    score_q1              = unname(s["q1"]),
    score_median          = unname(s["median"]),
    score_mean            = unname(s["mean"]),
    score_q3              = unname(s["q3"]),
    score_max             = unname(s["max"]),
    max_cluster_size      = if (is.null(x@cluster_summary)) NA_integer_
                            else as.integer(x@cluster_summary$max_cluster_size),
    pct_records_in_cluster = if (is.null(x@cluster_summary)) NA_real_
                             else as.numeric(x@cluster_summary$pct_records_in_cluster),
    n_recommendations     = length(x@recommendations)
  )
}

method(as.data.table.Match_Overview, Match_Overview) <- function(x, ...) {
  .match_overview_to_dt(x)
}

method(as.data.frame.Match_Overview, Match_Overview) <- function(x, ...) {
  as.data.frame(.match_overview_to_dt(x))
}


# ---------------------------------------------------------------------------
# Stub format/print for the not-yet-implemented classes (M3–M6)
# ---------------------------------------------------------------------------

method(format.Strategy_Audit, Strategy_Audit) <- function(x, ...) {
  "<joinery::Strategy_Audit> (not yet implemented in M1)"
}
method(print.Strategy_Audit, Strategy_Audit) <- function(x, ...) {
  cli::cli_text(format(x))
  invisible(x)
}

method(format.Match_Explanation, Match_Explanation) <- function(x, ...) {
  "<joinery::Match_Explanation> (not yet implemented in M1)"
}
method(print.Match_Explanation, Match_Explanation) <- function(x, ...) {
  cli::cli_text(format(x))
  invisible(x)
}

method(format.Match_Sample, Match_Sample) <- function(x, ...) {
  "<joinery::Match_Sample> (not yet implemented in M1)"
}
method(print.Match_Sample, Match_Sample) <- function(x, ...) {
  cli::cli_text(format(x))
  invisible(x)
}

method(format.Stage_Comparison, Stage_Comparison) <- function(x, ...) {
  "<joinery::Stage_Comparison> (not yet implemented in M1)"
}
method(print.Stage_Comparison, Stage_Comparison) <- function(x, ...) {
  cli::cli_text(format(x))
  invisible(x)
}


# `%||%` is imported from rlang via the package-level `@import` directive
# in R/joinery-package.R.
