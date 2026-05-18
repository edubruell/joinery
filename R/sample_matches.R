# ============================================================
# sample_matches() — all backends (Phase 0.6 M5)
# ============================================================
#
# Six sampling modes for user review (Q4: "where should I look first?"):
#   "high"        — top-n rows by score
#   "low"         — bottom-n rows by score (above threshold if given)
#   "borderline"  — n rows closest to a threshold
#   "ambiguous"   — n base records with the most candidate matches
#   "top_gap"     — n records where top-1 vs top-2 gap is smallest
#   "random"      — random n rows (with optional seed)
#
# Backends: data.table (reference), DuckDB (collect→DT),
#           tibble/data.frame thin wrappers.
# ============================================================


# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

.valid_modes <- c("high", "low", "borderline", "ambiguous", "top_gap", "random")

#' @noRd
.validate_sample_args <- function(mode, n) {
  if (!is.character(mode) || length(mode) != 1L || !mode %in% .valid_modes) {
    stop(
      sprintf(
        "`mode` must be one of: %s. Got: %s",
        paste0('"', .valid_modes, '"', collapse = ", "),
        deparse(mode)
      ),
      call. = FALSE
    )
  }
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 1L) {
    stop("`n` must be a positive integer scalar.", call. = FALSE)
  }
  as.integer(n)
}


# ---------------------------------------------------------------------------
# Mode helpers — all accept a data.table and return a data.table subset
# ---------------------------------------------------------------------------

#' @noRd
.sample_high <- function(dt, n) {
  out <- data.table::copy(dt)
  data.table::setorder(out, -score)
  utils::head(out, n)
}

#' @noRd
.sample_low <- function(dt, n, threshold) {
  out <- if (!is.null(threshold)) dt[score >= threshold] else data.table::copy(dt)
  data.table::setorder(out, score)
  utils::head(out, n)
}

#' @noRd
.sample_borderline <- function(dt, n, threshold) {
  out <- data.table::copy(dt)
  out[, .dist_to_thr := abs(score - threshold)]
  data.table::setorder(out, .dist_to_thr)
  out <- utils::head(out, n)
  out[, .dist_to_thr := NULL]
  out
}

#' @noRd
.sample_ambiguous <- function(dt, n, match_type) {
  if (match_type == "duplicates") {
    stop(
      "`mode = 'ambiguous'` is not applicable to duplicate match tables.\n",
      "Ambiguity is only meaningful for candidate matches (from `search_candidates()`).",
      call. = FALSE
    )
  }
  base_rows <- dt[dt[["source"]] == "base"]
  n_cands   <- base_rows[, .(n_candidates = data.table::uniqueN(match_id)), by = "id"]
  data.table::setorder(n_cands, -n_candidates)
  top_base_ids     <- utils::head(n_cands$id, n)
  id_vals          <- dt[["id"]]
  src_vals         <- dt[["source"]]
  selected_mids    <- unique(dt[["match_id"]][which(id_vals %in% top_base_ids & src_vals == "base")])
  mid_vals         <- dt[["match_id"]]
  dt[which(mid_vals %in% selected_mids)]
}

#' @noRd
.sample_top_gap <- function(dt, n, match_type) {
  if (match_type == "candidates") {
    base_rows <- dt[dt[["source"]] == "base"]
    ordered   <- data.table::copy(base_rows)
    data.table::setorder(ordered, id, -score)
    gaps <- ordered[, {
      if (.N >= 2L) list(gap = score[1L] - score[2L]) else list(gap = NA_real_)
    }, by = "id"]
    gaps <- gaps[!is.na(gap)]
    if (nrow(gaps) == 0L) return(dt[0L])
    data.table::setorder(gaps, gap)
    top_ids      <- utils::head(gaps$id, n)
    id_vals      <- dt[["id"]]
    src_vals     <- dt[["source"]]
    top_mids     <- unique(dt[["match_id"]][which(id_vals %in% top_ids & src_vals == "base")])
    mid_vals     <- dt[["match_id"]]
    dt[which(mid_vals %in% top_mids)]
  } else {
    copied <- data.table::copy(dt)
    gaps   <- copied[, {
      sub <- .SD[order(rank)]
      if (.N >= 2L) list(gap = sub$score[1L] - sub$score[2L]) else list(gap = NA_real_)
    }, by = "duplicate_group"]
    gaps <- gaps[!is.na(gap)]
    if (nrow(gaps) == 0L) return(dt[0L])
    data.table::setorder(gaps, gap)
    top_groups <- utils::head(gaps$duplicate_group, n)
    dg_vals    <- dt[["duplicate_group"]]
    dt[which(dg_vals %in% top_groups)]
  }
}

#' @noRd
.sample_random <- function(dt, n, seed) {
  if (!is.null(seed)) set.seed(seed)
  idx <- sample.int(nrow(dt), size = min(n, nrow(dt)), replace = FALSE)
  dt[sort(idx)]
}


# ---------------------------------------------------------------------------
# Methods: sample_matches on data.table (primary)
# ---------------------------------------------------------------------------

method(
  sample_matches,
  DT_tbl
) <- function(matches, mode = "borderline", n = 10L, threshold = NULL,
               seed = NULL, ...) {

  n          <- .validate_sample_args(mode, n)
  dt         <- data.table::as.data.table(matches)
  match_type <- .detect_match_type(dt)

  if (mode == "borderline" && is.null(threshold)) {
    stop(
      "`mode = 'borderline'` requires a `threshold` argument.\n",
      "Supply `threshold = <numeric>` (the strategy threshold used when ",
      "producing this match table).",
      call. = FALSE
    )
  }

  rows <- switch(mode,
    high       = .sample_high(dt, n),
    low        = .sample_low(dt, n, threshold),
    borderline = .sample_borderline(dt, n, threshold),
    ambiguous  = .sample_ambiguous(dt, n, match_type),
    top_gap    = .sample_top_gap(dt, n, match_type),
    random     = .sample_random(dt, n, seed)
  )

  criteria <- list(mode = mode, n = n)
  if (mode %in% c("borderline", "low") && !is.null(threshold)) criteria$threshold <- threshold
  if (mode == "random" && !is.null(seed)) criteria$seed <- seed

  Match_Sample(mode = mode, criteria = criteria, rows = rows)
}


# ---------------------------------------------------------------------------
# Methods: sample_matches on DuckDB (collect → DT)
# ---------------------------------------------------------------------------

method(
  sample_matches,
  Duck_tbl
) <- function(matches, mode = "borderline", n = 10L, threshold = NULL,
               seed = NULL, ...) {
  matches_dt <- data.table::as.data.table(dplyr::collect(matches))
  sample_matches(matches_dt, mode = mode, n = n,
                 threshold = threshold, seed = seed, ...)
}


# ---------------------------------------------------------------------------
# Methods: sample_matches on tibble / data.frame (thin wrappers)
# ---------------------------------------------------------------------------

method(
  sample_matches,
  .jyDF
) <- function(matches, mode = "borderline", n = 10L, threshold = NULL,
               seed = NULL, ...) {
  sample_matches(as_DT(matches), mode = mode, n = n,
                 threshold = threshold, seed = seed, ...)
}

method(
  sample_matches,
  .jyTBL_DF
) <- function(matches, mode = "borderline", n = 10L, threshold = NULL,
               seed = NULL, ...) {
  sample_matches(as_DT(matches), mode = mode, n = n,
                 threshold = threshold, seed = seed, ...)
}

method(
  sample_matches,
  .jyTBL
) <- function(matches, mode = "borderline", n = 10L, threshold = NULL,
               seed = NULL, ...) {
  sample_matches(as_DT(matches), mode = mode, n = n,
                 threshold = threshold, seed = seed, ...)
}
