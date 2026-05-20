# ============================================================
# Diagnostic result classes
# ============================================================
#
# Five small S7 result classes -- one per question the diagnostics
# module is organised around. See notes/diagnostics_design.md Section4.
#
# Each class carries summary structures only (small data.tables and
# named lists) plus a `recommendations` character vector drawn from
# the catalog in R/diagnostics_recommendations.R.
#
# Conventions (mirrored on every class):
#   - format(x, ...) returns the printable lines as a character vector
#     (used by print() internally and by tests for snapshotting; this
#     avoids cli-capture brittleness -- see commit 72e6722).
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
#' Result of [audit_strategy()] (Q1, pre-match).
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


#' Embedding Audit Result
#'
#' @description
#' Result of [audit_strategy()] (Q1, pre-match) when called on
#' an [`Embedding_Strategy`]. A separate class from [`Strategy_Audit`]
#' because the diagnostic surface is different: no per-column token /
#' rarity statistics, but a coverage rate, embedding-norm distribution,
#' and a sampled pairwise cosine similarity distribution.
#'
#' @slot n_records Integer. Number of records audited.
#' @slot n_embedded Integer. Records with non-empty assembled text (i.e.
#'   records that produce a usable embedding).
#' @slot coverage_rate Numeric. `n_embedded / n_records`.
#' @slot norm_summary Named list with `quantiles` (named numeric at
#'   `c(.05, .25, .5, .75, .95)`), `median`, and `iqr`. Norms close to 1
#'   indicate L2-normalised embeddings.
#' @slot similarity_sample `data.table` (`base_id`, `target_id`,
#'   `similarity`) or `NULL`. Pairwise cosine similarities from a random
#'   subsample, computed eagerly.
#' @slot block_summary `list` or `NULL`. Same structure as on
#'   [`Strategy_Audit`] when `block_by` is set on the strategy.
#' @slot est_comparisons Numeric scalar. Estimated number of pairwise
#'   comparisons given blocking.
#' @slot recommendations Character. Strings from the recommendations catalog.
#'
#' @noRd
Embedding_Audit <- new_class(
  "Embedding_Audit",
  properties = list(
    n_records         = class_integer,
    n_embedded        = class_integer,
    coverage_rate     = class_numeric,
    norm_summary      = class_list,
    similarity_sample = class_any,
    block_summary     = class_any,
    est_comparisons   = class_any,
    recommendations   = class_character
  )
)


#' Match Overview Result
#'
#' @description
#' Result of [summarise_matches()] (Q2, post-match overview).
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
#' Result of [explain_match()] (Q3, attribution).
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
#' Result of [sample_matches()] (Q4, sampling for review).
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
#' Result of [compare_stages()] (multi-stage diagnostics).
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
    score_dist_by_stage = class_any,
    recommendations     = class_character
  )
)


#' Match Features Result (Phase 0.7 M2)
#'
#' @description
#' Result of [match_features()]. A wide, one-row-per-pair feature table
#' suitable for downstream calibration / filtering (Phase 0.7 M5+).
#' Schema is documented in `notes/calibration_design.md` §6 and is treated
#' as the public API of v0.7 — additions only, never reorder or rename.
#'
#' @slot features `data.table`. The wide feature matrix.
#' @slot schema Character. One of `"token"` (full schema) or
#'   `"embedding"` (reduced schema — no token columns).
#' @slot strategy_class Character. Class name of the strategy used.
#' @slot top_n Named integer. Effective per-column `top_n` (after defaulting).
#' @slot columns Character. Strategy column names in their canonical order.
#' @slot aip_summary Named list or `NULL`. Diagnostic statistics over the
#'   per-token aIP values consumed (token strategies only).
#'
#' @noRd
Match_Features <- new_class(
  "Match_Features",
  properties = list(
    features       = class_any,
    schema         = class_character,
    strategy_class = class_character,
    top_n          = class_any,
    columns        = class_character,
    aip_summary    = class_any
  )
)

#' @noRd
print.Match_Features <- new_external_generic("base", "print", "x")
#' @noRd
format.Match_Features <- new_external_generic("base", "format", "x")
#' @noRd
as.data.table.Match_Features <- new_external_generic(
  "data.table", "as.data.table", "x"
)
#' @noRd
as.data.frame.Match_Features <- new_external_generic(
  "base", "as.data.frame", "x"
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
print.Embedding_Audit <- new_external_generic("base", "print", "x")

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
format.Embedding_Audit <- new_external_generic("base", "format", "x")

#' @noRd
as.data.table.Match_Overview <- new_external_generic(
  "data.table", "as.data.table", "x"
)
#' @noRd
as.data.frame.Match_Overview <- new_external_generic(
  "base", "as.data.frame", "x"
)

#' @noRd
as.data.table.Strategy_Audit <- new_external_generic(
  "data.table", "as.data.table", "x"
)
#' @noRd
as.data.frame.Strategy_Audit <- new_external_generic(
  "base", "as.data.frame", "x"
)

#' @noRd
as.data.table.Stage_Comparison <- new_external_generic(
  "data.table", "as.data.table", "x"
)
#' @noRd
as.data.frame.Stage_Comparison <- new_external_generic(
  "base", "as.data.frame", "x"
)

#' @noRd
as.data.table.Embedding_Audit <- new_external_generic(
  "data.table", "as.data.table", "x"
)
#' @noRd
as.data.frame.Embedding_Audit <- new_external_generic(
  "base", "as.data.frame", "x"
)


# ---------------------------------------------------------------------------
# format() / print() -- Match_Overview
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
# Coercion -- Match_Overview
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
# Stub format/print for classes whose full methods live in their own files
# ---------------------------------------------------------------------------

#' @noRd
.format_strategy_audit <- function(x) {
  lines <- character()
  push <- function(...) lines <<- c(lines, paste0(...))

  push("<joinery::Strategy_Audit>")
  push("")
  push(sprintf("n_records: %d", x@n_records))

  cts <- x@column_token_stats
  if (!is.null(cts) && nrow(cts) > 0L) {
    push("")
    push("column token stats:")
    for (i in seq_len(nrow(cts))) {
      push(sprintf(
        "  %s: %d tokens (%d unique, %.1f%% unique, na_rate=%.1f%%, avg_per_record=%.2f)",
        cts$column[i], cts$n_tokens[i], cts$n_unique_tokens[i],
        100 * cts$pct_unique[i], 100 * cts$na_rate[i],
        cts$avg_tokens_per_record[i]
      ))
    }
  }

  crs <- x@column_rarity_stats
  if (!is.null(crs) && nrow(crs) > 0L) {
    push("")
    push("column rarity stats (p05/p25/p50/p75/p95):")
    for (i in seq_len(nrow(crs))) {
      push(sprintf(
        "  %s: %.4f / %.4f / %.4f / %.4f / %.4f  (low_rarity=%.1f%%)",
        crs$column[i],
        crs$rarity_p05[i], crs$rarity_p25[i], crs$rarity_p50[i],
        crs$rarity_p75[i], crs$rarity_p95[i],
        100 * crs$pct_low_rarity[i]
      ))
    }
  }

  bs <- x@block_summary
  if (!is.null(bs)) {
    sm <- bs$summary
    push("")
    push(sprintf(
      "block summary: %d blocks, top1_share=%.1f%%, min=%d, median=%.1f, max=%d",
      sm$n_blocks, 100 * sm$top1_share,
      sm$min_size, sm$median_size, sm$max_size
    ))
    dist <- utils::head(bs$distribution, 5L)
    push("  top blocks (up to 5):")
    for (i in seq_len(nrow(dist))) {
      push(sprintf(
        "    %s: %d records (%.1f%%)",
        dist$block_key[i], dist$n_records[i], 100 * dist$pct_records[i]
      ))
    }
  }

  ec <- x@est_comparisons
  if (!is.null(ec) && !is.na(ec)) {
    push("")
    push(sprintf("est_comparisons: %.0f", ec))
  }

  vo <- attr(x, "vocab_overlap")
  if (!is.null(vo) && length(vo) > 0L) {
    push("")
    push("vocab overlap (base vs target):")
    for (col in names(vo)) {
      push(sprintf("  %s: %.1f%%", col, 100 * vo[[col]]))
    }
  }

  if (length(x@recommendations) > 0L) {
    push("")
    push("recommendations:")
    for (r in x@recommendations) push("  ! ", r)
  }

  lines
}

method(format.Strategy_Audit, Strategy_Audit) <- function(x, ...) {
  .format_strategy_audit(x)
}

method(print.Strategy_Audit, Strategy_Audit) <- function(x, ...) {
  cli::cli_h1("Strategy_Audit")
  cli::cli_text("n_records: {.val {x@n_records}}")

  cts <- x@column_token_stats
  if (!is.null(cts) && nrow(cts) > 0L) {
    cli::cli_text("{.strong column token stats}")
    for (i in seq_len(nrow(cts))) {
      cli::cli_bullets(sprintf(
        "{.field %s}: %d tokens, %d unique (%.1f%%), na_rate=%.1f%%",
        cts$column[i], cts$n_tokens[i], cts$n_unique_tokens[i],
        100 * cts$pct_unique[i], 100 * cts$na_rate[i]
      ))
    }
  }

  crs <- x@column_rarity_stats
  if (!is.null(crs) && nrow(crs) > 0L) {
    cli::cli_text("{.strong column rarity quantiles}")
    for (i in seq_len(nrow(crs))) {
      cli::cli_bullets(sprintf(
        "{.field %s}: p50=%.4f, pct_low_rarity=%.1f%%",
        crs$column[i], crs$rarity_p50[i], 100 * crs$pct_low_rarity[i]
      ))
    }
  }

  bs <- x@block_summary
  if (!is.null(bs)) {
    sm <- bs$summary
    cli::cli_text(
      "{.strong blocks}: {sm$n_blocks} blocks, top1_share={.val {sprintf('%.1f%%', 100*sm$top1_share)}}"
    )
  }

  ec <- x@est_comparisons
  if (!is.null(ec) && !is.na(ec)) {
    cli::cli_text("est_comparisons: {.val {sprintf('%.0f', ec)}}")
  }

  vo <- attr(x, "vocab_overlap")
  if (!is.null(vo) && length(vo) > 0L) {
    cli::cli_text("{.strong vocab overlap}")
    for (col in names(vo)) {
      cli::cli_bullets(sprintf("{.field %s}: %.1f%%", col, 100 * vo[[col]]))
    }
  }

  for (r in x@recommendations) cli::cli_alert_warning(r)

  invisible(x)
}


# ---------------------------------------------------------------------------
# Coercion -- Strategy_Audit
# ---------------------------------------------------------------------------

#' @noRd
.strategy_audit_to_dt <- function(x) {
  cts <- x@column_token_stats
  crs <- x@column_rarity_stats
  bs  <- x@block_summary

  row <- data.table::data.table(
    n_records           = x@n_records,
    n_columns           = if (!is.null(cts)) nrow(cts) else NA_integer_,
    total_n_tokens      = if (!is.null(cts)) sum(cts$n_tokens) else NA_integer_,
    mean_na_rate        = if (!is.null(cts)) mean(cts$na_rate) else NA_real_,
    min_pct_unique      = if (!is.null(cts)) min(cts$pct_unique) else NA_real_,
    max_pct_low_rarity  = if (!is.null(crs)) max(crs$pct_low_rarity) else NA_real_,
    est_comparisons     = if (!is.null(x@est_comparisons)) as.numeric(x@est_comparisons) else NA_real_,
    n_blocks            = if (!is.null(bs)) as.integer(bs$summary$n_blocks) else NA_integer_,
    block_top1_share    = if (!is.null(bs)) as.numeric(bs$summary$top1_share) else NA_real_,
    n_recommendations   = length(x@recommendations)
  )

  vo <- attr(x, "vocab_overlap")
  if (!is.null(vo)) {
    for (col in names(vo)) {
      row[, (paste0("vocab_overlap_", col)) := as.numeric(vo[[col]])]
    }
  }

  row
}

method(as.data.table.Strategy_Audit, Strategy_Audit) <- function(x, ...) {
  .strategy_audit_to_dt(x)
}

method(as.data.frame.Strategy_Audit, Strategy_Audit) <- function(x, ...) {
  as.data.frame(.strategy_audit_to_dt(x))
}

method(format.Match_Explanation, Match_Explanation) <- function(x, ...) {
  lines <- character()
  push  <- function(...) lines <<- c(lines, paste0(...))

  push("<joinery::Match_Explanation>  match ", x@match_id)
  push("")

  # --- Records ---------------------------------------------------------------
  push("Records:")
  if (!is.null(x@pair) && nrow(x@pair) >= 1L) {
    for (i in seq_len(min(nrow(x@pair), 2L))) {
      label <- if (i == 1L) "  lhs" else "  rhs"
      vals  <- paste(
        vapply(names(x@pair), function(col) {
          sprintf("%s=%s", col, as.character(x@pair[[col]][i]))
        }, character(1L)),
        collapse = "   "
      )
      push(label, "  ", vals)
    }
  }
  push("")

  # --- Score -----------------------------------------------------------------
  sb <- x@score_breakdown
  ff <- sb$feedback_factor %||% 1.0
  os <- sb$overlap_share

  score_line <- sprintf("Score: %.4f", x@score)
  if (!is.null(ff) && !is.na(ff) && abs(ff - 1.0) > 1e-9) {
    score_line <- paste0(
      score_line,
      sprintf(
        "  [raw=%.4f  x  feedback_factor=%.4f (overlap=%.2f)]",
        x@score / ff, ff, os
      )
    )
  }
  push(score_line)

  # --- Embedding-strategy note ----------------------------------------------
  sb_method <- x@score_breakdown$method
  if (is.null(x@per_column_contrib) && is.null(x@shared_tokens) &&
      !is.null(sb_method) && identical(sb_method, "cosine_similarity")) {
    push("")
    push("Per-token attribution is not available for embedding matches.")
  }

  # --- Per-column contributions ----------------------------------------------
  if (!is.null(x@per_column_contrib) && nrow(x@per_column_contrib) > 0L) {
    push("")
    push("Per-column contributions:")
    for (i in seq_len(nrow(x@per_column_contrib))) {
      r <- x@per_column_contrib[i, ]
      push(sprintf(
        "  %-20s %.4f  (%d shared token%s)",
        r$src_column, r$contribution, r$n_shared_tokens,
        if (r$n_shared_tokens == 1L) "" else "s"
      ))
    }
  }

  # --- Shared tokens (top 10) ------------------------------------------------
  if (!is.null(x@shared_tokens) && nrow(x@shared_tokens) > 0L) {
    push("")
    n_show <- min(nrow(x@shared_tokens), 10L)
    push(sprintf("Shared tokens (showing %d of %d):", n_show, nrow(x@shared_tokens)))
    for (i in seq_len(n_show)) {
      r <- x@shared_tokens[i, ]
      push(sprintf(
        "  %-12s / %-15s  rarity=%.4f  rIP=%.4f  weight=%.4f  contrib=%.4f",
        r$src_column, r$token, r$rarity, r$rIP, r$weight, r$contribution
      ))
    }
  }

  lines
}

method(print.Match_Explanation, Match_Explanation) <- function(x, ...) {
  lines <- format(x)
  for (ln in lines) cli::cli_text(ln)
  invisible(x)
}

.format_match_sample <- function(x) {
  lines <- character()
  push  <- function(...) lines <<- c(lines, paste0(...))

  push("<joinery::Match_Sample>")
  push(sprintf("  mode : %s", x@mode))
  push(sprintf("  n    : %d", x@criteria$n %||% NA_integer_))

  if (x@mode == "borderline" && !is.null(x@criteria$threshold))
    push(sprintf("  threshold : %.4f", x@criteria$threshold))
  if (x@mode == "random" && !is.null(x@criteria$seed))
    push(sprintf("  seed : %d", as.integer(x@criteria$seed)))

  push("")
  n_rows <- if (!is.null(x@rows)) nrow(x@rows) else 0L
  push(sprintf("rows: %d row(s)", n_rows))

  if (!is.null(x@rows) && n_rows > 0L) {
    push("")
    preview <- utils::head(x@rows, 10L)
    for (r in utils::capture.output(print(preview))) push("  ", r)
    if (n_rows > 10L)
      push(sprintf("  ... and %d more row(s)", n_rows - 10L))
  }

  lines
}

method(format.Match_Sample, Match_Sample) <- function(x, ...) {
  .format_match_sample(x)
}
method(print.Match_Sample, Match_Sample) <- function(x, ...) {
  lines <- format(x)
  for (ln in lines) cli::cli_text(ln)
  invisible(x)
}

#' @noRd
.format_stage_comparison <- function(x) {
  lines <- character()
  push  <- function(...) lines <<- c(lines, paste0(...))

  stages     <- names(x@per_stage_overview)
  n_stages   <- length(stages)
  match_type <- if (n_stages > 0L) x@per_stage_overview[[1L]]@match_type else "unknown"

  push("<joinery::Stage_Comparison>")
  push("")
  push(sprintf(
    "stages: %s    match_type: %s",
    paste(stages, collapse = " -> "),
    match_type
  ))

  if (n_stages > 0L) {
    push("")
    push("per-stage summary:")
    for (s in stages) {
      ov <- x@per_stage_overview[[s]]
      sc <- ov@score_dist$summary
      n  <- ov@n_records$n_pairs_or_groups %||% NA_integer_
      bc <- ov@coverage$base_coverage
      tc <- ov@coverage$target_coverage
      push(sprintf(
        "  [%s]  %d pairs   base=%s  target=%s",
        s, as.integer(n),
        if (is.null(bc) || is.na(bc)) "NA" else sprintf("%.1f%%", 100 * bc),
        if (is.null(tc) || is.na(tc)) "NA" else sprintf("%.1f%%", 100 * tc)
      ))
      if (!is.null(sc)) {
        push(sprintf(
          "         score: min=%.3f  median=%.3f  max=%.3f",
          sc[["min"]], sc[["median"]], sc[["max"]]
        ))
      }
    }
  }

  mc <- x@marginal_coverage
  if (!is.null(mc) && nrow(mc) > 0L) {
    push("")
    push("marginal coverage:")
    has_pct <- "base_pct_added" %in% names(mc) && !all(is.na(mc$base_pct_added))
    for (i in seq_len(nrow(mc))) {
      r <- mc[i]
      pct_part <- if (has_pct && !is.na(r$base_pct_added)) {
        sprintf("  (%.1f%% of base)", 100 * r$base_pct_added)
      } else ""
      push(sprintf(
        "  %-12s  base_added=%d  target_added=%s  base_cum=%d%s",
        r$stage,
        r$base_added,
        if (is.na(r$target_added)) "NA" else as.character(r$target_added),
        r$base_cumulative,
        pct_part
      ))
    }
  }

  if (length(x@recommendations) > 0L) {
    push("")
    push("recommendations:")
    for (r in x@recommendations) push("  ! ", r)
  }

  lines
}

method(format.Stage_Comparison, Stage_Comparison) <- function(x, ...) {
  .format_stage_comparison(x)
}

method(print.Stage_Comparison, Stage_Comparison) <- function(x, ...) {
  stages     <- names(x@per_stage_overview)
  match_type <- if (length(stages) > 0L) x@per_stage_overview[[1L]]@match_type else "unknown"
  cli::cli_h1(sprintf("Stage_Comparison ({.field %s}, %d stages)", match_type, length(stages)))
  cli::cli_text(paste(stages, collapse = " -> "))

  for (s in stages) {
    ov <- x@per_stage_overview[[s]]
    n  <- ov@n_records$n_pairs_or_groups %||% NA_integer_
    bc <- ov@coverage$base_coverage
    tc <- ov@coverage$target_coverage
    sc <- ov@score_dist$summary
    cli::cli_text(sprintf(
      "{.strong [%s]}  %d pairs   base=%s  target=%s   score median=%.3f",
      s, as.integer(n),
      if (is.null(bc) || is.na(bc)) "NA" else sprintf("%.1f%%", 100 * bc),
      if (is.null(tc) || is.na(tc)) "NA" else sprintf("%.1f%%", 100 * tc),
      if (!is.null(sc)) sc[["median"]] else NA_real_
    ))
  }

  mc <- x@marginal_coverage
  if (!is.null(mc) && nrow(mc) > 0L) {
    cli::cli_text("{.strong marginal coverage}")
    has_pct <- "base_pct_added" %in% names(mc) && !all(is.na(mc$base_pct_added))
    for (i in seq_len(nrow(mc))) {
      r <- mc[i]
      pct_part <- if (has_pct && !is.na(r$base_pct_added)) {
        sprintf(" (%.1f%%)", 100 * r$base_pct_added)
      } else ""
      cli::cli_text(sprintf(
        "  %s: +%d base%s",
        r$stage, r$base_added, pct_part
      ))
    }
  }

  for (r in x@recommendations) cli::cli_alert_warning(r)

  invisible(x)
}


# ---------------------------------------------------------------------------
# Coercion -- Stage_Comparison
# ---------------------------------------------------------------------------

#' @noRd
.stage_comparison_to_dt <- function(x) {
  mc <- x@marginal_coverage
  if (is.null(mc) || nrow(mc) == 0L) {
    return(data.table::data.table(
      stage             = character(),
      n_pairs_or_groups = integer(),
      base_added        = integer(),
      target_added      = integer(),
      base_cumulative   = integer(),
      target_cumulative = integer()
    ))
  }
  out <- data.table::copy(mc)
  stages <- names(x@per_stage_overview)
  n_pairs <- vapply(stages, function(s) {
    as.integer(x@per_stage_overview[[s]]@n_records$n_pairs_or_groups %||% NA_integer_)
  }, integer(1L))
  # only add n_pairs column if all stages present
  if (length(n_pairs) == nrow(out)) {
    out[, n_pairs_or_groups := n_pairs]
    data.table::setcolorder(out, c("stage", "n_pairs_or_groups"))
  }
  out
}

method(as.data.table.Stage_Comparison, Stage_Comparison) <- function(x, ...) {
  .stage_comparison_to_dt(x)
}

method(as.data.frame.Stage_Comparison, Stage_Comparison) <- function(x, ...) {
  as.data.frame(.stage_comparison_to_dt(x))
}


# ---------------------------------------------------------------------------
# format() / print() -- Embedding_Audit
# ---------------------------------------------------------------------------

#' @noRd
.format_embedding_audit <- function(x) {
  lines <- character()
  push <- function(...) lines <<- c(lines, paste0(...))

  push("<joinery::Embedding_Audit>")
  push("")
  push(sprintf(
    "n_records: %d   n_embedded: %d   coverage_rate: %.1f%%",
    x@n_records, x@n_embedded, 100 * x@coverage_rate
  ))

  ns <- x@norm_summary
  if (length(ns) > 0L && !is.null(ns$quantiles)) {
    q <- ns$quantiles
    push("")
    push("norm quantiles (p05/p25/p50/p75/p95):")
    push(sprintf(
      "  %.4f / %.4f / %.4f / %.4f / %.4f   (median=%.4f, iqr=%.4f)",
      q[1L], q[2L], q[3L], q[4L], q[5L],
      ns$median %||% NA_real_, ns$iqr %||% NA_real_
    ))
  }

  ss <- x@similarity_sample
  if (!is.null(ss) && nrow(ss) > 0L) {
    s <- ss$similarity
    push("")
    push(sprintf(
      "similarity sample: %d pairs   min=%.3f  median=%.3f  max=%.3f",
      nrow(ss),
      min(s, na.rm = TRUE),
      stats::median(s, na.rm = TRUE),
      max(s, na.rm = TRUE)
    ))
  }

  bs <- x@block_summary
  if (!is.null(bs)) {
    sm <- bs$summary
    push("")
    push(sprintf(
      "block summary: %d blocks, top1_share=%.1f%%, min=%d, median=%.1f, max=%d",
      sm$n_blocks, 100 * sm$top1_share,
      sm$min_size, sm$median_size, sm$max_size
    ))
    dist <- utils::head(bs$distribution, 5L)
    push("  top blocks (up to 5):")
    for (i in seq_len(nrow(dist))) {
      push(sprintf(
        "    %s: %d records (%.1f%%)",
        dist$block_key[i], dist$n_records[i], 100 * dist$pct_records[i]
      ))
    }
  }

  ec <- x@est_comparisons
  if (!is.null(ec) && !is.na(ec)) {
    push("")
    push(sprintf("est_comparisons: %.0f", ec))
  }

  if (length(x@recommendations) > 0L) {
    push("")
    push("recommendations:")
    for (r in x@recommendations) push("  ! ", r)
  }

  lines
}

method(format.Embedding_Audit, Embedding_Audit) <- function(x, ...) {
  .format_embedding_audit(x)
}

method(print.Embedding_Audit, Embedding_Audit) <- function(x, ...) {
  cli::cli_h1("Embedding_Audit")
  cli::cli_text(sprintf(
    "n_records: {.val %d}   n_embedded: {.val %d}   coverage_rate: {.val %s}",
    x@n_records, x@n_embedded,
    sprintf("%.1f%%", 100 * x@coverage_rate)
  ))

  ns <- x@norm_summary
  if (length(ns) > 0L && !is.null(ns$quantiles)) {
    q <- ns$quantiles
    cli::cli_text(sprintf(
      "{.strong norm}: median=%.4f, iqr=%.4f (p05=%.4f, p95=%.4f)",
      ns$median %||% NA_real_, ns$iqr %||% NA_real_,
      q[1L], q[5L]
    ))
  }

  ss <- x@similarity_sample
  if (!is.null(ss) && nrow(ss) > 0L) {
    s <- ss$similarity
    cli::cli_text(sprintf(
      "{.strong similarity sample}: %d pairs  min=%.3f  median=%.3f  max=%.3f",
      nrow(ss),
      min(s, na.rm = TRUE),
      stats::median(s, na.rm = TRUE),
      max(s, na.rm = TRUE)
    ))
  }

  bs <- x@block_summary
  if (!is.null(bs)) {
    sm <- bs$summary
    cli::cli_text(
      "{.strong blocks}: {sm$n_blocks} blocks, top1_share={.val {sprintf('%.1f%%', 100*sm$top1_share)}}"
    )
  }

  ec <- x@est_comparisons
  if (!is.null(ec) && !is.na(ec)) {
    cli::cli_text("est_comparisons: {.val {sprintf('%.0f', ec)}}")
  }

  for (r in x@recommendations) cli::cli_alert_warning(r)

  invisible(x)
}


# ---------------------------------------------------------------------------
# Coercion -- Embedding_Audit
# ---------------------------------------------------------------------------

#' @noRd
.embedding_audit_to_dt <- function(x) {
  ns <- x@norm_summary
  bs <- x@block_summary
  ss <- x@similarity_sample

  data.table::data.table(
    n_records         = x@n_records,
    n_embedded        = x@n_embedded,
    coverage_rate     = as.numeric(x@coverage_rate),
    norm_median       = if (!is.null(ns)) as.numeric(ns$median %||% NA_real_) else NA_real_,
    norm_iqr          = if (!is.null(ns)) as.numeric(ns$iqr    %||% NA_real_) else NA_real_,
    similarity_median = if (!is.null(ss) && nrow(ss) > 0L)
                          as.numeric(stats::median(ss$similarity, na.rm = TRUE))
                        else NA_real_,
    similarity_n_pairs = if (!is.null(ss)) as.integer(nrow(ss)) else NA_integer_,
    est_comparisons   = if (!is.null(x@est_comparisons)) as.numeric(x@est_comparisons) else NA_real_,
    n_blocks          = if (!is.null(bs)) as.integer(bs$summary$n_blocks) else NA_integer_,
    block_top1_share  = if (!is.null(bs)) as.numeric(bs$summary$top1_share) else NA_real_,
    n_recommendations = length(x@recommendations)
  )
}

method(as.data.table.Embedding_Audit, Embedding_Audit) <- function(x, ...) {
  .embedding_audit_to_dt(x)
}

method(as.data.frame.Embedding_Audit, Embedding_Audit) <- function(x, ...) {
  as.data.frame(.embedding_audit_to_dt(x))
}


# ---------------------------------------------------------------------------
# format() / print() -- Match_Features
# ---------------------------------------------------------------------------

#' @noRd
.format_match_features <- function(x) {
  lines <- character()
  push  <- function(...) lines <<- c(lines, paste0(...))

  ft <- x@features
  push("<joinery::Match_Features>")
  push(sprintf("  schema         : %s", x@schema))
  push(sprintf("  strategy_class : %s", x@strategy_class))
  push(sprintf("  n_pairs        : %d", if (is.null(ft)) 0L else nrow(ft)))
  push(sprintf("  n_features     : %d", if (is.null(ft)) 0L else ncol(ft)))

  if (length(x@columns) > 0L) {
    push(sprintf("  strategy cols  : %s", paste(x@columns, collapse = ", ")))
  }
  if (length(x@top_n) > 0L) {
    push(sprintf(
      "  top_n          : %s",
      paste(sprintf("%s=%d", names(x@top_n), as.integer(x@top_n)), collapse = ", ")
    ))
  }

  if (!is.null(ft) && nrow(ft) > 0L) {
    push("")
    push("preview:")
    for (r in utils::capture.output(print(utils::head(ft, 5L)))) push("  ", r)
  }
  lines
}

method(format.Match_Features, Match_Features) <- function(x, ...) {
  .format_match_features(x)
}

method(print.Match_Features, Match_Features) <- function(x, ...) {
  ft <- x@features
  cli::cli_h1(sprintf("Match_Features ({.field %s})", x@schema))
  cli::cli_text(sprintf(
    "strategy_class: {.val %s}   n_pairs: {.val %d}   n_features: {.val %d}",
    x@strategy_class,
    if (is.null(ft)) 0L else nrow(ft),
    if (is.null(ft)) 0L else ncol(ft)
  ))
  if (length(x@columns) > 0L) {
    cli::cli_text("strategy columns: {.field {x@columns}}")
  }
  if (!is.null(ft) && nrow(ft) > 0L) {
    cli::cli_text("{.strong preview}")
    print(utils::head(ft, 5L))
  }
  invisible(x)
}

method(as.data.table.Match_Features, Match_Features) <- function(x, ...) {
  if (is.null(x@features)) {
    return(data.table::data.table())
  }
  data.table::copy(x@features)
}

method(as.data.frame.Match_Features, Match_Features) <- function(x, ...) {
  if (is.null(x@features)) return(as.data.frame(data.table::data.table()))
  as.data.frame(data.table::copy(x@features))
}


# `%||%` is imported from rlang via the package-level `@import` directive
# in R/joinery-package.R.
