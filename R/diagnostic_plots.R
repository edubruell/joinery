# ============================================================
# Diagnostic plot functions
# ============================================================
#
# 14 named plot functions, one per diagnostic view, plus
# default plot() S3 methods per class.
#
# Conventions:
#   - Every function returns the plotted data.table invisibly.
#   - theme = "clean" on all tinyplot() calls (overridable via ...).
#   - palette = "okabe" for grouped (by=) plots.
#   - Threshold lines via graphics::abline(lty = 2, col = "grey40", lwd = 1).
#   - User ... overrides win via modifyList(defaults, list(...)).
# ============================================================


# Internal: merge defaults with user dots, then call tinyplot.
# NOTE: `type` lives in call_args, not defaults, so passing type= via ...
# will cause a duplicate-argument error. ... is intended for label/palette/
# limit overrides, not type changes.
.tinyplot_call <- function(call_args, defaults, user_dots) {
  merged <- utils::modifyList(defaults, user_dots)
  do.call(tinyplot::tinyplot, c(call_args, merged))
}


# ---------------------------------------------------------------------------
# Strategy_Audit plots
# ---------------------------------------------------------------------------

#' Bar chart of median token rarity per column
#'
#' @param x A `Strategy_Audit` object from [audit_strategy()].
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the plotted `data.table` (column_rarity_stats).
#' @noRd
rarity_histogram <- function(x, ...) {
  crs <- x@column_rarity_stats
  if (is.null(crs) || nrow(crs) == 0L)
    cli::cli_abort("{.field column_rarity_stats} is NULL or empty")
  dt <- data.table::as.data.table(crs)
  # flip=TRUE swaps axes; tinyplot applies xlab to vertical, ylab to horizontal
  .tinyplot_call(
    call_args  = list(rarity_p50 ~ column, data = dt,
                      type = tinyplot::type_barplot(), flip = TRUE),
    defaults   = list(theme = "clean", xlab = "",
                      ylab = "Median rarity (p50)", main = "Token rarity by column"),
    user_dots  = list(...)
  )
  invisible(dt)
}


#' Bar chart of average tokens per record per column
#'
#' @param x A `Strategy_Audit` object from [audit_strategy()].
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the plotted `data.table` (column_token_stats).
#' @noRd
token_frequency_plot <- function(x, ...) {
  cts <- x@column_token_stats
  if (is.null(cts) || nrow(cts) == 0L)
    cli::cli_abort("{.field column_token_stats} is NULL or empty")
  dt <- data.table::as.data.table(cts)
  .tinyplot_call(
    call_args = list(avg_tokens_per_record ~ column, data = dt,
                     type = tinyplot::type_barplot(), flip = TRUE),
    defaults  = list(theme = "clean", xlab = "",
                     ylab = "Avg. tokens per record", main = "Token frequency by column"),
    user_dots = list(...)
  )
  invisible(dt)
}


#' Bar chart of block sizes (requires block_by on strategy)
#'
#' @param x A `Strategy_Audit` object from [audit_strategy()].
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the plotted `data.table` (block_summary$distribution).
#' @noRd
block_size_plot <- function(x, ...) {
  bs <- x@block_summary
  if (is.null(bs))
    cli::cli_abort("No {.arg block_by} was set on the strategy; {.field block_summary} is NULL")
  dt <- data.table::as.data.table(bs$distribution)
  .tinyplot_call(
    call_args = list(n_records ~ block_key, data = dt,
                     type = tinyplot::type_barplot(), flip = TRUE),
    defaults  = list(theme = "clean", xlab = "",
                     ylab = "Records in block", main = "Block size distribution"),
    user_dots = list(...)
  )
  invisible(dt)
}


#' Bar chart of vocabulary overlap between base and target per column
#'
#' @param x A `Strategy_Audit` object from [audit_strategy()] called with
#'   `target` supplied.
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the plotted `data.table`.
#' @noRd
vocab_overlap_plot <- function(x, ...) {
  vo <- attr(x, "vocab_overlap")
  if (is.null(vo) || length(vo) == 0L)
    cli::cli_abort("No vocab overlap computed. Supply {.arg target} to {.fn audit_strategy}")
  dt <- data.table::data.table(
    column  = names(vo),
    overlap = unname(unlist(vo))
  )
  dt <- dt[!is.na(overlap)]
  if (nrow(dt) == 0L)
    cli::cli_abort("All vocab overlap values are NA")
  .tinyplot_call(
    call_args = list(overlap ~ column, data = dt,
                     type = tinyplot::type_barplot(), flip = TRUE),
    defaults  = list(theme = "clean", xlab = "",
                     ylab = "Vocabulary overlap (base vs target)",
                     main = "Vocab overlap by column", ylim = c(0, 1)),
    user_dots = list(...)
  )
  invisible(dt)
}


# ---------------------------------------------------------------------------
# Embedding_Audit plots
# ---------------------------------------------------------------------------

#' Histogram of sampled pairwise cosine similarities
#'
#' @param x An `Embedding_Audit` object from [audit_strategy()].
#' @param threshold Numeric. Draws a dashed vertical line at the strategy
#'   threshold (default: `attr(x, "threshold")`).
#' @param bins Integer. Number of histogram bins.
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the histogram `data.table` with columns
#'   `bin_lower`, `bin_upper`, `bin_mid`, `count`.
#' @noRd
similarity_histogram <- function(x,
                                 threshold = attr(x, "threshold"),
                                 bins      = 30L,
                                 ...) {
  ss <- x@similarity_sample
  if (is.null(ss) || nrow(ss) == 0L)
    cli::cli_abort("{.field similarity_sample} is NULL or empty")
  sims <- ss$similarity
  sims <- sims[is.finite(sims)]
  if (length(sims) == 0L)
    cli::cli_abort("All similarity values are NA/non-finite")

  breaks <- seq(min(sims), max(sims), length.out = bins + 1L)
  if (length(unique(breaks)) < 2L) {
    breaks <- c(breaks[1L] - 0.5, breaks[1L] + 0.5)
  }
  h <- graphics::hist(sims, breaks = breaks, plot = FALSE)
  dt <- data.table::data.table(
    bin_lower = utils::head(h$breaks, -1L),
    bin_upper = utils::tail(h$breaks, -1L),
    count     = as.integer(h$counts)
  )
  dt[, bin_mid := round((bin_lower + bin_upper) / 2, 4L)]

  .tinyplot_call(
    call_args = list(count ~ bin_mid, data = dt,
                     type = tinyplot::type_barplot()),
    defaults  = list(theme = "clean", xlab = "Cosine similarity",
                     ylab = "Pair count",
                     main = "Sampled pairwise similarity"),
    user_dots = list(...)
  )
  if (!is.null(threshold) && !is.na(threshold)) {
    usr <- graphics::par("usr")
    graphics::lines(rep(threshold, 2L), usr[3:4],
                    lty = 2, col = "grey40", lwd = 1)
  }
  invisible(dt)
}


#' Bar chart of embedding norm quantiles
#'
#' Plots p05/p25/p50/p75/p95 of the embedding vector norms. A norm of 1
#' is annotated; for an L2-normalised strategy all bars should sit on it.
#'
#' @param x An `Embedding_Audit` object from [audit_strategy()].
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the plotted `data.table` (quantile, norm).
#' @noRd
norm_plot <- function(x, ...) {
  ns <- x@norm_summary
  if (length(ns) == 0L || is.null(ns$quantiles) ||
      all(is.na(ns$quantiles))) {
    cli::cli_abort("{.field norm_summary} is empty or all-NA")
  }
  q <- ns$quantiles
  dt <- data.table::data.table(
    quantile = factor(names(q), levels = names(q)),
    norm     = as.numeric(unname(q))
  )
  .tinyplot_call(
    call_args = list(norm ~ quantile, data = dt,
                     type = tinyplot::type_barplot()),
    defaults  = list(theme = "clean", xlab = "Quantile",
                     ylab = "Embedding L2 norm",
                     main = "Embedding norm quantiles"),
    user_dots = list(...)
  )
  usr <- graphics::par("usr")
  graphics::lines(usr[1:2], c(1, 1), lty = 2, col = "grey40", lwd = 1)
  invisible(dt)
}


# ---------------------------------------------------------------------------
# Match_Overview plots
# ---------------------------------------------------------------------------

#' Bar chart of the pre-binned score distribution
#'
#' @param x A `Match_Overview` object from [summarise_matches()].
#' @param threshold Numeric. Draws a dashed vertical line. Defaults to the
#'   threshold stored in `x@score_dist$threshold` when available.
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the plotted `data.table` (histogram with bin_mid column).
#' @noRd
score_histogram <- function(x, threshold = x@score_dist$threshold %||% NA_real_, ...) {
  hist_dt <- data.table::copy(x@score_dist$histogram)
  hist_dt[, bin_mid := round((bin_lower + bin_upper) / 2, 3L)]
  .tinyplot_call(
    call_args = list(count ~ bin_mid, data = hist_dt,
                     type = tinyplot::type_barplot()),
    defaults  = list(theme = "clean", xlab = "Score",
                     ylab = "Count", main = "Score distribution"),
    user_dots = list(...)
  )
  if (!is.na(threshold)) {
    usr <- graphics::par("usr")
    graphics::lines(rep(threshold, 2L), usr[3:4], lty = 2, col = "grey40", lwd = 1)
  }
  invisible(hist_dt)
}


#' Kernel density of the score distribution
#'
#' Expands the pre-binned histogram to approximate raw scores before
#' passing to the density estimator.
#'
#' @param x A `Match_Overview` object from [summarise_matches()].
#' @param threshold Numeric. Draws a dashed vertical line. Defaults to the
#'   threshold stored in `x@score_dist$threshold` when available.
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the `data.table` of expanded scores.
#' @noRd
score_density <- function(x, threshold = x@score_dist$threshold %||% NA_real_, ...) {
  hist_dt <- data.table::copy(x@score_dist$histogram)
  hist_dt[, bin_mid := round((bin_lower + bin_upper) / 2, 3L)]
  scores  <- rep(hist_dt$bin_mid, hist_dt$count)
  dt      <- data.table::data.table(score = scores)
  .tinyplot_call(
    call_args = list(~score, data = dt,
                     type = tinyplot::type_density(alpha = 0.3, bw = "nrd0")),
    defaults  = list(theme = "clean", xlab = "Score", ylab = "Density",
                     main = "Score density", xlim = c(0, 1)),
    user_dots = list(...)
  )
  if (!is.na(threshold)) {
    usr <- graphics::par("usr")
    graphics::lines(rep(threshold, 2L), usr[3:4], lty = 2, col = "grey40", lwd = 1)
  }
  invisible(dt)
}


#' Bar chart of match coverage (base and/or target)
#'
#' @param x A `Match_Overview` object from [summarise_matches()].
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the plotted `data.table`.
#' @noRd
coverage_plot <- function(x, ...) {
  cov  <- x@coverage
  vals <- c(base = cov$base_coverage, target = cov$target_coverage)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0L)
    cli::cli_abort("No coverage data available (both base and target coverage are NA)")
  dt <- data.table::data.table(side = names(vals), coverage = unname(vals))
  .tinyplot_call(
    call_args = list(coverage ~ side, data = dt,
                     type = tinyplot::type_barplot()),
    defaults  = list(theme = "clean", xlab = "", ylab = "Coverage",
                     main = "Match coverage", ylim = c(0, 1)),
    user_dots = list(...)
  )
  # Annotate bars with percentage values
  graphics::text(
    x      = seq_len(nrow(dt)),
    y      = dt$coverage / 2,
    labels = sprintf("%.1f%%", 100 * dt$coverage),
    col    = "white",
    font   = 2L,
    cex    = 1.1
  )
  invisible(dt)
}


#' Bar chart of cluster-size distribution (duplicates only)
#'
#' @param x A `Match_Overview` object from [summarise_matches()] with
#'   `match_type == "duplicates"`.
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the plotted `data.table` (cluster_dist).
#' @noRd
cluster_size_plot <- function(x, ...) {
  if (x@match_type != "duplicates")
    cli::cli_abort("{.fn cluster_size_plot} requires {.code match_type == \"duplicates\"}. Got: {.val {x@match_type}}")
  cd <- x@cluster_dist
  if (is.null(cd) || nrow(cd) == 0L)
    cli::cli_abort("{.field cluster_dist} is NULL or empty")
  dt <- data.table::as.data.table(cd)
  .tinyplot_call(
    call_args = list(n_clusters ~ cluster_size, data = dt,
                     type = tinyplot::type_barplot()),
    defaults  = list(theme = "clean", xlab = "Cluster size",
                     ylab = "Number of clusters", main = "Cluster size distribution"),
    user_dots = list(...)
  )
  invisible(dt)
}


#' Bar chart of candidates-per-record distribution (candidates only)
#'
#' @param x A `Match_Overview` object from [summarise_matches()] with
#'   `match_type == "candidates"`.
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the plotted `data.table` (ambiguity_dist).
#' @noRd
ambiguity_plot <- function(x, ...) {
  if (x@match_type != "candidates")
    cli::cli_abort("{.fn ambiguity_plot} requires {.code match_type == \"candidates\"}. Got: {.val {x@match_type}}")
  ad <- x@ambiguity_dist
  if (is.null(ad) || nrow(ad) == 0L)
    cli::cli_abort("{.field ambiguity_dist} is NULL or empty")
  dt <- data.table::as.data.table(ad)
  .tinyplot_call(
    call_args = list(n_records ~ candidates_per_record, data = dt,
                     type = tinyplot::type_barplot()),
    defaults  = list(theme = "clean", xlab = "Candidates per base record",
                     ylab = "Number of records", main = "Ambiguity distribution"),
    user_dots = list(...)
  )
  invisible(dt)
}


#' Bar chart of top-1 vs top-2 score gap distribution (candidates only)
#'
#' @param x A `Match_Overview` object from [summarise_matches()] with
#'   `match_type == "candidates"`.
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the plotted `data.table` (top_gap_dist with bin_mid).
#' @noRd
top_gap_density <- function(x, ...) {
  if (x@match_type != "candidates")
    cli::cli_abort("{.fn top_gap_density} requires {.code match_type == \"candidates\"}. Got: {.val {x@match_type}}")
  tgd <- x@top_gap_dist
  if (is.null(tgd) || nrow(tgd) == 0L)
    cli::cli_abort("{.field top_gap_dist} is NULL or empty")
  dt <- data.table::copy(tgd)
  dt[, bin_mid := round((bin_lower + bin_upper) / 2, 3L)]
  .tinyplot_call(
    call_args = list(count ~ bin_mid, data = dt,
                     type = tinyplot::type_barplot()),
    defaults  = list(theme = "clean", xlab = "Top-1 vs top-2 score gap",
                     ylab = "Count", main = "Top score gap distribution"),
    user_dots = list(...)
  )
  invisible(dt)
}


# ---------------------------------------------------------------------------
# Match_Explanation plots
# ---------------------------------------------------------------------------

#' Horizontal bar chart of per-column score contributions
#'
#' @param x A `Match_Explanation` object from [explain_match()].
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the plotted `data.table` (per_column_contrib).
#' @noRd
contribution_plot <- function(x, ...) {
  pcc <- x@per_column_contrib
  if (is.null(pcc) || nrow(pcc) == 0L)
    cli::cli_abort("{.field per_column_contrib} is NULL or empty")
  dt <- data.table::as.data.table(pcc)
  .tinyplot_call(
    call_args = list(contribution ~ src_column, data = dt,
                     type = tinyplot::type_barplot(), flip = TRUE),
    defaults  = list(theme = "clean", xlab = "", ylab = "Contribution to score",
                     main = sprintf("Per-column contributions (match %d)", x@match_id)),
    user_dots = list(...)
  )
  invisible(dt)
}


#' Horizontal bar chart of per-token score contributions, coloured by column
#'
#' @param x A `Match_Explanation` object from [explain_match()].
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the plotted `data.table` (shared_tokens with token_label).
#' @noRd
token_contribution_plot <- function(x, ...) {
  st <- x@shared_tokens
  if (is.null(st) || nrow(st) == 0L)
    cli::cli_abort("{.field shared_tokens} is NULL or empty")
  dt <- data.table::copy(data.table::as.data.table(st))
  dt[, token_label := paste0(src_column, ": ", token)]
  data.table::setorder(dt, src_column, -contribution)
  .tinyplot_call(
    call_args = list(contribution ~ token_label | src_column, data = dt,
                     type = tinyplot::type_barplot(), flip = TRUE,
                     palette = "okabe",
                     legend = list(title = "Column")),
    defaults  = list(theme = "clean", xlab = "", ylab = "Token contribution",
                     main = sprintf("Token contributions (match %d)", x@match_id)),
    user_dots = list(...)
  )
  invisible(dt)
}


# ---------------------------------------------------------------------------
# Stage_Comparison plots
# ---------------------------------------------------------------------------

#' Line plot of cumulative base coverage by stage
#'
#' Uses percentage coverage when base was supplied to [compare_stages()],
#' raw record counts otherwise.
#'
#' @param x A `Stage_Comparison` object from [compare_stages()].
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the plotted `data.table` (marginal_coverage with stage_idx).
#' @noRd
stage_coverage_plot <- function(x, ...) {
  mc <- x@marginal_coverage
  if (is.null(mc) || nrow(mc) == 0L)
    cli::cli_abort("{.field marginal_coverage} is NULL or empty")
  mc  <- data.table::copy(mc)
  n   <- nrow(mc)
  mc[, stage_idx := seq_len(n)]
  use_pct  <- "base_pct_cumulative" %in% names(mc) &&
    !all(is.na(mc$base_pct_cumulative))
  # xaxl mapper: numeric index -> stage name
  stage_names <- mc$stage
  xaxl_fn <- function(v) stage_names[as.integer(round(v))]
  if (use_pct) {
    y_min <- max(0, min(mc$base_pct_cumulative, na.rm = TRUE) - 0.05)
    .tinyplot_call(
      call_args = list(base_pct_cumulative ~ stage_idx, data = mc,
                       type = "b",
                       xaxb = seq_len(n), xaxl = xaxl_fn,
                       ylim = c(y_min, 1)),
      defaults  = list(theme = "clean", xlab = "Stage",
                       ylab = "Cumulative base coverage",
                       main = "Marginal coverage by stage"),
      user_dots = list(...)
    )
  } else {
    .tinyplot_call(
      call_args = list(base_cumulative ~ stage_idx, data = mc,
                       type = "b",
                       xaxb = seq_len(n), xaxl = xaxl_fn),
      defaults  = list(theme = "clean", xlab = "Stage",
                       ylab = "Cumulative base records matched",
                       main = "Marginal coverage by stage"),
      user_dots = list(...)
    )
  }
  invisible(mc)
}


#' Grouped bar chart of score distributions by stage
#'
#' @param x A `Stage_Comparison` object from [compare_stages()].
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the plotted `data.table` (score_dist_by_stage with bin_mid).
#' @noRd
stage_score_plot <- function(x, ...) {
  sds <- x@score_dist_by_stage
  if (is.null(sds) || nrow(sds) == 0L)
    cli::cli_abort("{.field score_dist_by_stage} is NULL or empty")
  sds <- data.table::copy(sds)
  sds[, bin_mid := round((bin_lower + bin_upper) / 2, 3L)]
  .tinyplot_call(
    call_args = list(count ~ bin_mid | stage, data = sds,
                     type = tinyplot::type_barplot(), palette = "okabe"),
    defaults  = list(theme = "clean", xlab = "Score",
                     ylab = "Count", main = "Score distribution by stage"),
    user_dots = list(...)
  )
  invisible(sds)
}


# ---------------------------------------------------------------------------
# Strategy_Plan plots
# ---------------------------------------------------------------------------

#' Cost/recall frontier scatter for a strategy plan
#'
#' Plots each candidate block key at (candidate, exact_twin_survival) - the
#' recall axis - with the brute-pair cost in the point labels. The knee is the
#' cheapest candidate whose twin survival stays high.
#'
#' @param x A `Strategy_Plan` object from [plan_strategy()].
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return Invisibly, the plotted `data.table` (frontier).
#' @noRd
frontier_plot <- function(x, ...) {
  fr <- x@frontier
  if (is.null(fr) || nrow(fr) == 0L)
    cli::cli_abort("{.field frontier} is NULL or empty")
  fr <- data.table::copy(fr)[!is.na(exact_twin_survival)]
  if (nrow(fr) == 0L)
    cli::cli_abort("frontier has no candidate with a defined twin survival")
  n  <- nrow(fr)
  fr[, cand_idx := seq_len(n)]
  block_names <- fr$block_key
  xaxl_fn <- function(v) block_names[as.integer(round(v))]
  .tinyplot_call(
    call_args = list(exact_twin_survival ~ cand_idx, data = fr,
                     type = "p", xaxb = seq_len(n), xaxl = xaxl_fn,
                     ylim = c(0, 1)),
    defaults  = list(theme = "clean", xlab = "Candidate block",
                     ylab = "Exact-twin survival",
                     main = "Blocking frontier (recall axis)"),
    user_dots = list(...)
  )
  invisible(fr)
}


# ---------------------------------------------------------------------------
# Default plot() methods (plain S3 dispatch — S7 objects expose S3 classes)
# ---------------------------------------------------------------------------

#' @noRd
plot.Match_Overview <- function(x, ...) score_histogram(x, ...)

#' @noRd
plot.Strategy_Audit <- function(x, ...) rarity_histogram(x, ...)

#' @noRd
plot.Strategy_Plan <- function(x, ...) frontier_plot(x, ...)

#' @noRd
plot.Embedding_Audit <- function(x, ...) similarity_histogram(x, ...)

#' @noRd
plot.Match_Explanation <- function(x, ...) contribution_plot(x, ...)

#' @noRd
plot.Stage_Comparison <- function(x, ...) stage_coverage_plot(x, ...)

#' @noRd
plot.Match_Sample <- function(x, ...) {
  rows <- x@rows
  if (is.null(rows) || nrow(rows) == 0L)
    cli::cli_abort("{.field rows} slot is NULL or empty")
  if (!"score" %in% names(rows))
    cli::cli_abort("{.field rows} does not contain a {.field score} column")
  dt <- data.table::data.table(score = rows$score)
  .tinyplot_call(
    call_args = list(~score, data = dt,
                     type = tinyplot::type_density(alpha = 0.3)),
    defaults  = list(theme = "clean", xlab = "Score", ylab = "Density",
                     main = sprintf("Sample score density (mode: %s)", x@mode)),
    user_dots = list(...)
  )
  invisible(dt)
}
