# ============================================================
# sample_matches() — all backends
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
# Stratification / block-expansion extensions (additive):
#   stratify_by      — character vector. Apply mode within each stratum,
#                      returning `n` rows per stratum.
#   expand_to_block  — logical. After sampling, attach all other rows
#                      from the same block (match_id sharing a base id
#                      for candidates; duplicate_group for dedup).
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
# Stratification + whole-block expansion helpers
# ---------------------------------------------------------------------------

#' @noRd
.validate_stratify_by <- function(stratify_by, cols) {
  if (is.null(stratify_by)) return(NULL)
  if (!is.character(stratify_by) || length(stratify_by) < 1L ||
      any(is.na(stratify_by)) || any(stratify_by == "")) {
    stop(
      "`stratify_by` must be a non-empty character vector of column names.",
      call. = FALSE
    )
  }
  missing_cols <- setdiff(stratify_by, cols)
  if (length(missing_cols)) {
    stop(
      sprintf(
        "`stratify_by` references columns not in the matches table: %s",
        paste0("\"", missing_cols, "\"", collapse = ", ")
      ),
      call. = FALSE
    )
  }
  stratify_by
}

#' @noRd
.expand_to_block <- function(dt, rows, match_type) {
  if (nrow(rows) == 0L) return(rows)
  if (match_type == "candidates") {
    # Block = all match_ids that share the same base id with any sampled match_id.
    sampled_mids <- unique(rows[["match_id"]])
    base_id_col  <- dt[dt[["source"]] == "base" & dt[["match_id"]] %in% sampled_mids,
                      unique(get("id"))]
    if (length(base_id_col) == 0L) return(rows)
    block_mids <- unique(dt[dt[["source"]] == "base" & dt[["id"]] %in% base_id_col,
                             get("match_id")])
    dt[dt[["match_id"]] %in% block_mids]
  } else {
    groups <- unique(rows[["duplicate_group"]])
    if (length(groups) == 0L) return(rows)
    dt[dt[["duplicate_group"]] %in% groups]
  }
}


# ---------------------------------------------------------------------------
# Methods: sample_matches on data.table (primary)
# ---------------------------------------------------------------------------

method(
  sample_matches,
  DT_tbl
) <- function(matches, mode = "borderline", n = 10L, threshold = NULL,
               seed = NULL, stratify_by = NULL, expand_to_block = FALSE, ...) {

  n          <- .validate_sample_args(mode, n)
  dt         <- data.table::as.data.table(matches)
  match_type <- .detect_match_type(dt)
  stratify_by <- .validate_stratify_by(stratify_by, names(dt))

  if (!is.logical(expand_to_block) || length(expand_to_block) != 1L ||
      is.na(expand_to_block)) {
    stop("`expand_to_block` must be a single TRUE or FALSE.", call. = FALSE)
  }

  if (mode == "borderline" && is.null(threshold)) {
    stop(
      "`mode = 'borderline'` requires a `threshold` argument.\n",
      "Supply `threshold = <numeric>` (the strategy threshold used when ",
      "producing this match table).",
      call. = FALSE
    )
  }

  # If we stratify with random mode, set seed once at the outer level so
  # per-stratum draws share the RNG state instead of redrawing the same
  # indices for each stratum.
  inner_seed <- seed
  if (!is.null(stratify_by) && mode == "random" && !is.null(seed)) {
    set.seed(seed)
    inner_seed <- NULL
  }

  sample_one <- function(d) {
    switch(mode,
      high       = .sample_high(d, n),
      low        = .sample_low(d, n, threshold),
      borderline = .sample_borderline(d, n, threshold),
      ambiguous  = .sample_ambiguous(d, n, match_type),
      top_gap    = .sample_top_gap(d, n, match_type),
      random     = .sample_random(d, n, inner_seed)
    )
  }

  if (!is.null(stratify_by)) {
    # Stratify on (column-)combination. Apply mode within each stratum.
    rows <- dt[, sample_one(data.table::copy(.SD)), by = stratify_by,
               .SDcols = setdiff(names(dt), stratify_by)]
    # Restore canonical column order
    keep_cols <- intersect(names(dt), names(rows))
    rows <- rows[, ..keep_cols]
  } else {
    rows <- sample_one(dt)
  }

  if (isTRUE(expand_to_block)) {
    rows <- .expand_to_block(dt, rows, match_type)
  }

  criteria <- list(mode = mode, n = n)
  if (mode %in% c("borderline", "low") && !is.null(threshold)) criteria$threshold <- threshold
  if (mode == "random" && !is.null(seed)) criteria$seed <- seed
  if (!is.null(stratify_by)) criteria$stratify_by <- stratify_by
  if (isTRUE(expand_to_block)) criteria$expand_to_block <- TRUE

  Match_Sample(mode = mode, criteria = criteria, rows = rows)
}


# ---------------------------------------------------------------------------
# Methods: sample_matches on DuckDB (collect → DT)
# ---------------------------------------------------------------------------

method(
  sample_matches,
  Duck_tbl
) <- function(matches, mode = "borderline", n = 10L, threshold = NULL,
               seed = NULL, stratify_by = NULL, expand_to_block = FALSE, ...) {
  matches_dt <- data.table::as.data.table(dplyr::collect(matches))
  sample_matches(matches_dt, mode = mode, n = n,
                 threshold = threshold, seed = seed,
                 stratify_by = stratify_by,
                 expand_to_block = expand_to_block, ...)
}


# ---------------------------------------------------------------------------
# Methods: sample_matches on tibble / data.frame (thin wrappers)
# ---------------------------------------------------------------------------

method(
  sample_matches,
  .jyDF
) <- function(matches, mode = "borderline", n = 10L, threshold = NULL,
               seed = NULL, stratify_by = NULL, expand_to_block = FALSE, ...) {
  sample_matches(as_DT(matches), mode = mode, n = n,
                 threshold = threshold, seed = seed,
                 stratify_by = stratify_by,
                 expand_to_block = expand_to_block, ...)
}

method(
  sample_matches,
  .jyTBL_DF
) <- function(matches, mode = "borderline", n = 10L, threshold = NULL,
               seed = NULL, stratify_by = NULL, expand_to_block = FALSE, ...) {
  sample_matches(as_DT(matches), mode = mode, n = n,
                 threshold = threshold, seed = seed,
                 stratify_by = stratify_by,
                 expand_to_block = expand_to_block, ...)
}

method(
  sample_matches,
  .jyTBL
) <- function(matches, mode = "borderline", n = 10L, threshold = NULL,
               seed = NULL, stratify_by = NULL, expand_to_block = FALSE, ...) {
  sample_matches(as_DT(matches), mode = mode, n = n,
                 threshold = threshold, seed = seed,
                 stratify_by = stratify_by,
                 expand_to_block = expand_to_block, ...)
}
