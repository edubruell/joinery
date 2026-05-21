# ============================================================
# Diagnostics recommendations catalog
# ============================================================
#
# A finite, testable map from (diagnostic_signal, threshold) tuples to
# short, lever-specific suggestion strings. The catalog lives here so
# it can be unit-tested in isolation and refined without touching the
# verbs that consume it.
#
# Each entry is a list:
#   id          - stable identifier (used in tests and surfaced via
#                 `attr(result, "recommendation_ids")`)
#   signal      - name of the computed signal in the dispatch input
#   threshold   - numeric trigger threshold
#   op          - comparison operator: ">", ">=", "<", "<=", "=="
#   lever       - one of the strategy levers in §6 of the design doc
#   message_fn  - function(signal_value) -> formatted character string
#
# The dispatch helper .dispatch_recommendations() takes a named list of
# signal values, applies each catalog entry whose required signal is
# non-NA, and returns:
#   - $ids:      character vector of fired catalog ids
#   - $messages: character vector of formatted messages
# ============================================================


# ---------------------------------------------------------------------------
# Catalog
# ---------------------------------------------------------------------------

#' @noRd
.diagnostics_catalog <- list(
  list(
    id        = "candidates_high_ambiguity",
    signal    = "pct_records_with_ge3_matches",
    threshold = 0.10,
    op        = ">",
    lever     = "max_candidates",
    message_fn = function(v) sprintf(
      "%.1f%% of base records have >= 3 candidate matches; consider `max_candidates` or raising threshold.",
      100 * v
    )
  ),
  list(
    id        = "candidates_weak_decisiveness",
    signal    = "score_top_gap_median",
    threshold = 0.05,
    op        = "<",
    lever     = "threshold / feedback_strength",
    message_fn = function(v) sprintf(
      "median top-1 vs top-2 score gap is %.3f; matches are weakly decisive, consider raising threshold or `feedback_strength`.",
      v
    )
  ),
  list(
    id        = "duplicates_mega_cluster",
    signal    = "max_cluster_size",
    threshold = 50,
    op        = ">=",
    lever     = "preparers / min_rarity",
    message_fn = function(v) sprintf(
      "largest duplicate cluster has %d records; likely stopword or preparer issue (`filter_stopwords()` / higher `min_rarity`).",
      as.integer(v)
    )
  ),
  list(
    id        = "low_coverage_candidates",
    signal    = "base_coverage_candidates",
    threshold = 0.05,
    op        = "<",
    lever     = "threshold / preparers / block_by",
    message_fn = function(v) sprintf(
      "only %.1f%% of base matched; consider relaxing threshold or revisiting preparers/blocking.",
      100 * v
    )
  ),
  list(
    id         = "block_imbalanced",
    signal     = "block_top_share",
    threshold  = 0.70,
    op         = ">",
    lever      = "block_by",
    message_fn = function(v) sprintf(
      "largest block holds %.1f%% of records; blocking may be imbalanced, consider a finer blocking key.",
      100 * v
    )
  ),
  list(
    id         = "high_low_rarity_pressure",
    signal     = "max_pct_low_rarity_tokens",
    threshold  = 0.50,
    op         = ">",
    lever      = "preparers / min_rarity",
    context_fn = function(signals) signals[["worst_rarity_column"]],
    message_fn = function(v, col) sprintf(
      "column `%s` has %.1f%% of unique tokens with rarity below 0.01; consider `filter_stopwords()` or higher `min_rarity`.",
      col, 100 * v
    )
  ),
  list(
    id         = "low_embedding_coverage",
    signal     = "coverage_rate",
    threshold  = 0.90,
    op         = "<",
    lever      = "input quality / assemble_record_text",
    message_fn = function(v) sprintf(
      "embedding coverage is %.1f%%; many records have empty/NA text in the configured columns.",
      100 * v
    )
  ),
  list(
    id         = "unnormalised_embeddings",
    signal     = "norm_iqr",
    threshold  = 0.10,
    op         = ">",
    lever      = "Embedding_Strategy(normalize = TRUE)",
    message_fn = function(v) sprintf(
      "embedding norm IQR is %.3f; vectors are not unit-length, set `normalize = TRUE` on the strategy.",
      v
    )
  ),
  list(
    id         = "consider_calibration_borderline",
    signal     = "pct_pairs_borderline",
    threshold  = 0.10,
    op         = ">",
    lever      = "calibrate_matches",
    message_fn = function(v) sprintf(
      "%.1f%% of pairs score within an epsilon of the decision threshold; consider `calibrate_matches()` to fit a post-retrieval false-positive filter.",
      100 * v
    )
  ),
  list(
    id         = "consider_calibration_ambiguity",
    signal     = "pct_records_with_ge3_matches",
    threshold  = 0.20,
    op         = ">",
    lever      = "calibrate_matches",
    message_fn = function(v) sprintf(
      "%.1f%% of base records have >= 3 candidate matches; once you have a few hundred labelled pairs, `calibrate_matches()` can re-rank them.",
      100 * v
    )
  ),
  list(
    id         = "calibration_low_n_warning",
    signal     = "training_n",
    threshold  = 500,
    op         = "<",
    lever      = "labelling",
    message_fn = function(v) sprintf(
      "filter was fit on only %d labelled pairs; consider expanding the labelled sample to >= 500 for stable calibration.",
      as.integer(v)
    )
  ),
  list(
    id         = "calibration_drift_warning",
    signal     = "stage_dist_tv_distance",
    threshold  = 0.15,
    op         = ">",
    lever      = "labelling / refit",
    message_fn = function(v) sprintf(
      "stage distribution drifted by TV=%.2f vs training; consider refitting the filter on labelled data from the new run.",
      v
    )
  ),
  list(
    id         = "low_yield_stage",
    signal     = "min_stage_base_pct",
    threshold  = 0.01,
    op         = "<",
    lever      = "multi_stage",
    context_fn = function(signals) signals[["low_yield_stage_name"]],
    message_fn = function(v, stage_name) sprintf(
      "stage '%s' added only %.1f%% of base records; consider dropping this stage.",
      stage_name, 100 * v
    )
  )
)


# ---------------------------------------------------------------------------
# Dispatch
# ---------------------------------------------------------------------------

#' @noRd
.compare <- function(value, threshold, op) {
  switch(op,
    ">"  = value >  threshold,
    ">=" = value >= threshold,
    "<"  = value <  threshold,
    "<=" = value <= threshold,
    "==" = value == threshold,
    stop("Unknown comparison op: ", op, call. = FALSE)
  )
}

#' @noRd
.dispatch_recommendations <- function(signals, catalog = .diagnostics_catalog) {
  ids  <- character()
  msgs <- character()

  for (entry in catalog) {
    v <- signals[[entry$signal]]
    if (is.null(v) || length(v) == 0L) next
    v <- v[[1L]]
    if (is.na(v)) next
    if (.compare(v, entry$threshold, entry$op)) {
      ids <- c(ids, entry$id)
      ctx <- if (!is.null(entry$context_fn)) entry$context_fn(signals) else NULL
      msg <- if (!is.null(ctx)) entry$message_fn(v, ctx) else entry$message_fn(v)
      msgs <- c(msgs, msg)
    }
  }

  list(ids = ids, messages = msgs)
}
