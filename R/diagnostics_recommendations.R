# ============================================================
# Diagnostics recommendations catalog (Phase 0.6 §6)
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
  ids <- character()
  msgs <- character()

  for (entry in catalog) {
    v <- signals[[entry$signal]]
    if (is.null(v) || length(v) == 0L) next
    v <- v[[1L]]
    if (is.na(v)) next
    if (.compare(v, entry$threshold, entry$op)) {
      ids <- c(ids, entry$id)
      msgs <- c(msgs, entry$message_fn(v))
    }
  }

  list(ids = ids, messages = msgs)
}
