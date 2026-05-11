# ============================================================
# summarise_matches() — data.table backend (Phase 0.6 M1)
# ============================================================
#
# Implements the post-match overview verb end-to-end on the
# data.table backend. Auto-detects whether the input is a duplicate
# table (presence of `duplicate_group`) or a candidate table
# (presence of `match_id` + `source`), and computes:
#
#   * n_pairs_or_groups, n_records_involved
#   * coverage (when base / target supplied)
#   * score distribution: summary, fixed quantiles, histogram bins
#   * cluster_dist + cluster_summary  (duplicates only)
#   * ambiguity_dist + top_gap_dist   (candidates only)
#   * recommendations (catalog dispatch)
#
# Backend parity (DuckDB, tibble, data.frame) is M2.
# ============================================================


# ---------------------------------------------------------------------------
# Internal: validate and detect match_type
# ---------------------------------------------------------------------------

#' @noRd
.detect_match_type <- function(matches) {
  cols <- names(matches)
  if ("duplicate_group" %in% cols && "id" %in% cols && "score" %in% cols) {
    return("duplicates")
  }
  if (all(c("match_id", "source", "id", "score") %in% cols)) {
    return("candidates")
  }
  stop(
    "`matches` does not look like a joinery match table.\n",
    "Expected either:\n",
    "  - duplicate columns: duplicate_group, id, score (from `detect_duplicates`)\n",
    "  - candidate columns: match_id, source, id, score (from `search_candidates`)",
    call. = FALSE
  )
}


# ---------------------------------------------------------------------------
# Internal: score distribution
# ---------------------------------------------------------------------------

#' @noRd
.score_distribution <- function(scores, bins = 50) {
  if (!is.numeric(scores)) {
    stop("`score` column must be numeric.", call. = FALSE)
  }
  scores <- scores[!is.na(scores)]

  if (length(scores) == 0L) {
    return(list(
      summary   = c(min = NA_real_, q1 = NA_real_, median = NA_real_,
                    mean = NA_real_, q3 = NA_real_, max = NA_real_),
      quantiles = stats::setNames(
        rep(NA_real_, 7),
        c("p05", "p10", "p25", "p50", "p75", "p90", "p95")
      ),
      histogram = data.table::data.table(
        bin_lower = numeric(),
        bin_upper = numeric(),
        count     = integer()
      ),
      threshold = NA_real_
    ))
  }

  q <- stats::quantile(
    scores,
    probs = c(0, 0.25, 0.5, 0.75, 1),
    names = FALSE,
    na.rm = TRUE
  )
  summary_vec <- c(
    min    = q[[1]],
    q1     = q[[2]],
    median = q[[3]],
    mean   = mean(scores),
    q3     = q[[4]],
    max    = q[[5]]
  )

  qq <- stats::quantile(
    scores,
    probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
    names = FALSE,
    na.rm = TRUE
  )
  quantiles_vec <- stats::setNames(
    qq,
    c("p05", "p10", "p25", "p50", "p75", "p90", "p95")
  )

  if (length(unique(scores)) == 1L) {
    only <- unique(scores)
    hist_dt <- data.table::data.table(
      bin_lower = only,
      bin_upper = only,
      count     = length(scores)
    )
  } else {
    breaks <- seq(min(scores), max(scores), length.out = bins + 1L)
    h <- graphics::hist(scores, breaks = breaks, plot = FALSE)
    hist_dt <- data.table::data.table(
      bin_lower = h$breaks[-length(h$breaks)],
      bin_upper = h$breaks[-1L],
      count     = as.integer(h$counts)
    )
  }

  list(
    summary   = summary_vec,
    quantiles = quantiles_vec,
    histogram = hist_dt,
    threshold = NA_real_   # not propagated into match tables; M2 follow-up
  )
}


# ---------------------------------------------------------------------------
# Method: summarise_matches on data.table
# ---------------------------------------------------------------------------

method(
  summarise_matches,
  DT_tbl
) <- function(matches, base = NULL, target = NULL, bins = 50L, ...) {

  match_type <- .detect_match_type(matches)
  dt <- data.table::as.data.table(matches)

  # --- score distribution ------------------------------------------------
  score_dist <- .score_distribution(dt$score, bins = bins)

  # --- per-branch ---------------------------------------------------------
  cluster_dist    <- NULL
  cluster_summary <- NULL
  ambiguity_dist  <- NULL
  top_gap_dist    <- NULL
  signals         <- list()

  if (match_type == "duplicates") {

    n_groups   <- data.table::uniqueN(dt$duplicate_group)
    n_records  <- data.table::uniqueN(dt$id)

    # cluster size distribution
    sizes <- dt[, .(cluster_size = .N), by = "duplicate_group"]
    cluster_dist <- sizes[, .(n_clusters = .N), by = "cluster_size"]
    data.table::setorder(cluster_dist, cluster_size)

    max_cluster_size <- if (nrow(sizes) > 0L) max(sizes$cluster_size) else 0L

    pct_in_cluster <- NA_real_
    if (!is.null(base)) {
      if (nrow(base) > 0L) pct_in_cluster <- n_records / nrow(base)
    }

    cluster_summary <- list(
      max_cluster_size       = as.integer(max_cluster_size),
      pct_records_in_cluster = pct_in_cluster
    )

    base_coverage   <- pct_in_cluster
    target_coverage <- NA_real_

    signals[["max_cluster_size"]] <- as.numeric(max_cluster_size)

  } else { # candidates

    n_pairs    <- data.table::uniqueN(dt$match_id)
    n_records  <- data.table::uniqueN(dt$id)

    base_rows   <- dt[source == "base"]
    target_rows <- dt[source == "target"]

    base_ids   <- unique(base_rows$id)
    target_ids <- unique(target_rows$id)

    base_coverage <- if (!is.null(base) && nrow(base) > 0L) {
      length(base_ids) / nrow(base)
    } else NA_real_
    target_coverage <- if (!is.null(target) && nrow(target) > 0L) {
      length(target_ids) / nrow(target)
    } else NA_real_

    # ambiguity: candidates per base record
    if (nrow(base_rows) > 0L) {
      n_cands_per_base <- base_rows[, .(
        n_candidates = data.table::uniqueN(match_id)
      ), by = "id"]
      ambiguity_dist <- n_cands_per_base[, .(n_records = .N),
                                         by = "n_candidates"]
      data.table::setnames(ambiguity_dist,
                           old = "n_candidates",
                           new = "candidates_per_record")
      data.table::setorder(ambiguity_dist, candidates_per_record)

      pct_ge3 <- mean(n_cands_per_base$n_candidates >= 3L)
      signals[["pct_records_with_ge3_matches"]] <- pct_ge3
    }

    # top_gap: per base record with >= 2 candidates, top1 - top2
    if (nrow(base_rows) > 0L) {
      ordered <- data.table::copy(base_rows)
      data.table::setorder(ordered, id, -score)
      gaps <- ordered[, {
        if (.N >= 2L) list(gap = score[1L] - score[2L])
        else list(gap = NA_real_)
      }, by = "id"]
      gaps_clean <- gaps$gap[!is.na(gaps$gap)]
      if (length(gaps_clean) > 0L) {
        if (length(unique(gaps_clean)) == 1L) {
          top_gap_dist <- data.table::data.table(
            bin_lower = unique(gaps_clean),
            bin_upper = unique(gaps_clean),
            count     = length(gaps_clean)
          )
        } else {
          breaks <- seq(min(gaps_clean), max(gaps_clean), length.out = 21L)
          h <- graphics::hist(gaps_clean, breaks = breaks, plot = FALSE)
          top_gap_dist <- data.table::data.table(
            bin_lower = h$breaks[-length(h$breaks)],
            bin_upper = h$breaks[-1L],
            count     = as.integer(h$counts)
          )
        }
        signals[["score_top_gap_median"]] <- stats::median(gaps_clean)
      }
    }

    if (!is.na(base_coverage)) {
      signals[["base_coverage_candidates"]] <- base_coverage
    }
  }

  # --- assemble n_records & coverage -------------------------------------
  n_records_list <- if (match_type == "duplicates") {
    list(
      n_pairs_or_groups  = as.integer(n_groups),
      n_records_involved = as.integer(n_records)
    )
  } else {
    list(
      n_pairs_or_groups  = as.integer(n_pairs),
      n_records_involved = as.integer(n_records)
    )
  }

  coverage_list <- list(
    base_coverage   = base_coverage,
    target_coverage = target_coverage
  )

  # --- recommendations ----------------------------------------------------
  recs <- .dispatch_recommendations(signals)

  out <- Match_Overview(
    match_type      = match_type,
    n_records       = n_records_list,
    coverage        = coverage_list,
    score_dist      = score_dist,
    cluster_dist    = cluster_dist,
    cluster_summary = cluster_summary,
    ambiguity_dist  = ambiguity_dist,
    top_gap_dist    = top_gap_dist,
    recommendations = recs$messages
  )
  attr(out, "recommendation_ids") <- recs$ids
  out
}
