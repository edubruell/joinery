# ============================================================
# compare_stages() — all backends
# ============================================================
#
# Consumes a multi-stage matches table (with a `stage` column,
# as produced by `multi_stage_search()`) and returns a
# `Stage_Comparison` with:
#   * per_stage_overview  — named list of Match_Overview (one per stage)
#   * marginal_coverage   — data.table: records added by each stage
#   * score_dist_by_stage — long-form histograms for overlay plotting
#   * recommendations     — catalog signals (e.g. low_yield_stage)
#
# Backends: data.table (reference), DuckDB (collect→delegate),
#           tibble/data.frame (thin wrappers via as_DT).
# ============================================================


# ---------------------------------------------------------------------------
# Internal: input validation
# ---------------------------------------------------------------------------

#' @noRd
.validate_stage_col <- function(matches) {
  if (!"stage" %in% names(matches))
    cli::cli_abort(c(
      "{.arg matches} must have a {.field stage} column",
      "i" = "Use {.fn multi_stage_search} to produce multi-stage output"
    ))
}


# ---------------------------------------------------------------------------
# Internal: the multi_stage_search() entity grouping carries its directed edge
# ledger as the `"ledger"` attribute. Translate that ledger into the pairs shape
# (match_id | stage | score | source | id) the per-stage machinery below already
# understands — base = the `from` endpoint, target = the `to` endpoint.
# ---------------------------------------------------------------------------

#' @noRd
.ledger_to_pairs <- function(ledger) {
  ledger <- data.table::as.data.table(ledger)
  if (nrow(ledger) == 0L) {
    return(data.table::data.table(
      match_id = integer(), stage = character(), score = numeric(),
      source = character(), id = character()
    ))
  }
  ledger[, .mid := .I]
  data.table::rbindlist(list(
    ledger[, .(match_id = .mid, stage, score, source = "base",   id = as.character(from))],
    ledger[, .(match_id = .mid, stage, score, source = "target", id = as.character(to))]
  ), use.names = TRUE)
}

#' Collect a compare_stages input to a data.table, preserving any `"ledger"`
#' attribute (which the multi_stage_search grouping rides on, and which
#' collect / as.data.table would otherwise drop). DuckDB ledgers are collected.
#' @noRd
.collect_stage_input <- function(matches) {
  led <- attr(matches, "ledger", exact = TRUE)
  dt  <- if (inherits(matches, "tbl_duckdb_connection")) {
    data.table::as.data.table(dplyr::collect(matches))
  } else {
    data.table::as.data.table(matches)
  }
  if (!is.null(led)) {
    if (inherits(led, "tbl_duckdb_connection")) led <- dplyr::collect(led)
    attr(dt, "ledger") <- data.table::as.data.table(led)
  }
  dt
}


# ---------------------------------------------------------------------------
# Internal: marginal coverage (preserves stage insertion order)
# ---------------------------------------------------------------------------

#' @noRd
.marginal_coverage_dt <- function(dt, match_type, base = NULL, target = NULL) {
  stages   <- unique(dt$stage)
  base_n   <- if (!is.null(base)   && nrow(base)   > 0L) nrow(base)   else NA_integer_
  target_n <- if (!is.null(target) && nrow(target) > 0L) nrow(target) else NA_integer_

  seen_base   <- character()
  seen_target <- character()

  rows <- map(stages, function(s) {
    sdt <- dt[stage == s]

    cur_base <- if (match_type == "candidates") {
      unique(sdt[source == "base"]$id)
    } else {
      unique(sdt$id)
    }
    cur_target <- if (match_type == "candidates") {
      unique(sdt[source == "target"]$id)
    } else {
      character()
    }

    new_base   <- length(setdiff(cur_base,   seen_base))
    new_target <- length(setdiff(cur_target, seen_target))

    seen_base   <<- union(seen_base,   cur_base)
    seen_target <<- union(seen_target, cur_target)

    list(
      stage                 = s,
      base_added            = as.integer(new_base),
      target_added          = if (match_type == "candidates") as.integer(new_target) else NA_integer_,
      base_cumulative       = as.integer(length(seen_base)),
      target_cumulative     = if (match_type == "candidates") as.integer(length(seen_target)) else NA_integer_,
      base_pct_added        = if (!is.na(base_n)) new_base / base_n else NA_real_,
      target_pct_added      = if (!is.na(target_n) && match_type == "candidates") new_target / target_n else NA_real_,
      base_pct_cumulative   = if (!is.na(base_n)) length(seen_base) / base_n else NA_real_,
      target_pct_cumulative = if (!is.na(target_n) && match_type == "candidates") length(seen_target) / target_n else NA_real_
    )
  })

  data.table::rbindlist(rows)
}


# ---------------------------------------------------------------------------
# Internal: long-form per-stage score histograms
# ---------------------------------------------------------------------------

#' @noRd
.score_dist_by_stage_dt <- function(dt, bins = 50L) {
  stages <- unique(dt$stage)

  hists <- map(stages, function(s) {
    h <- .score_distribution(dt[stage == s]$score, bins = bins)$histogram
    if (nrow(h) > 0L) h[, stage := s]
    h
  })

  out <- data.table::rbindlist(hists)
  if (nrow(out) > 0L)
    data.table::setcolorder(out, c("stage", "bin_lower", "bin_upper", "count"))
  out
}


# ---------------------------------------------------------------------------
# Method: compare_stages on data.table (reference implementation)
# ---------------------------------------------------------------------------

method(
  compare_stages,
  DT_tbl
) <- function(matches, base = NULL, target = NULL, bins = 50L, ...) {
  # multi_stage_search grouping: diagnose from its directed ledger (the
  # pairs-equivalent). Other inputs (e.g. multi_stage_dedup output) are used as-is.
  led <- attr(matches, "ledger", exact = TRUE)
  dt <- if (!is.null(led)) {
    .ledger_to_pairs(led)
  } else {
    .validate_stage_col(matches)
    data.table::as.data.table(matches)
  }

  match_type <- .detect_match_type_cols(setdiff(names(dt), "stage"))
  stages     <- unique(dt$stage)

  # per-stage Match_Overview objects
  per_stage <- map(stages, function(s) {
    stage_dt <- dt[stage == s][, stage := NULL][]
    summarise_matches(stage_dt, base = base, target = target, bins = bins, ...)
  })
  names(per_stage) <- stages

  marginal_cov     <- .marginal_coverage_dt(dt, match_type, base, target)
  score_dist_stage <- .score_dist_by_stage_dt(dt, bins)

  # low-yield-stage recommendation signal (only when base is supplied)
  signals <- list()
  if ("base_pct_added" %in% names(marginal_cov) &&
      !all(is.na(marginal_cov$base_pct_added))) {
    min_pct  <- min(marginal_cov$base_pct_added, na.rm = TRUE)
    worst    <- marginal_cov$stage[which.min(marginal_cov$base_pct_added)]
    signals[["min_stage_base_pct"]]   <- min_pct
    signals[["low_yield_stage_name"]] <- worst
  }
  recs <- .dispatch_recommendations(signals)

  out <- Stage_Comparison(
    per_stage_overview  = per_stage,
    marginal_coverage   = marginal_cov,
    score_dist_by_stage = score_dist_stage,
    recommendations     = recs$messages
  )
  attr(out, "recommendation_ids") <- recs$ids
  out
}


# ---------------------------------------------------------------------------
# Method: compare_stages on DuckDB — collect to R and delegate
# ---------------------------------------------------------------------------

method(
  compare_stages,
  Duck_tbl
) <- function(matches, base = NULL, target = NULL, bins = 50L, ...) {
  dt        <- .collect_stage_input(matches)   # preserves the ledger attribute
  base_dt   <- if (!is.null(base))   as_DT(base)   else NULL
  target_dt <- if (!is.null(target)) as_DT(target) else NULL
  compare_stages(dt, base = base_dt, target = target_dt, bins = bins, ...)
}


# ---------------------------------------------------------------------------
# Methods: compare_stages on tibble / data.frame (thin wrappers)
# ---------------------------------------------------------------------------

method(
  compare_stages,
  .jyDF
) <- function(matches, base = NULL, target = NULL, bins = 50L, ...) {
  base_dt   <- if (!is.null(base))   as_DT(base)   else NULL
  target_dt <- if (!is.null(target)) as_DT(target) else NULL
  compare_stages(.collect_stage_input(matches), base = base_dt, target = target_dt, bins = bins, ...)
}

method(
  compare_stages,
  .jyTBL_DF
) <- function(matches, base = NULL, target = NULL, bins = 50L, ...) {
  base_dt   <- if (!is.null(base))   as_DT(base)   else NULL
  target_dt <- if (!is.null(target)) as_DT(target) else NULL
  compare_stages(.collect_stage_input(matches), base = base_dt, target = target_dt, bins = bins, ...)
}

method(
  compare_stages,
  .jyTBL
) <- function(matches, base = NULL, target = NULL, bins = 50L, ...) {
  base_dt   <- if (!is.null(base))   as_DT(base)   else NULL
  target_dt <- if (!is.null(target)) as_DT(target) else NULL
  compare_stages(.collect_stage_input(matches), base = base_dt, target = target_dt, bins = bins, ...)
}
