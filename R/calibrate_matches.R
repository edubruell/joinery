# ============================================================
# calibrate_matches() — Phase 0.7 M5 high-level verb
# ============================================================
#
# One call. Wraps `match_features()` → `fit_filter()` →
# `apply_filter()`. Dispatches on strategy class so the same verb
# serves both token and embedding workflows.
#
# Method dispatch follows the M2 / M3 pattern: backend × strategy.
# The token / embedding feature paths are already pluggable on
# `match_features()`, so most of the work here is plumbing.
# ============================================================


# ---------- helper: forward `match_features()` --------------------

#' @noRd
.calibrate_build_features <- function(matches, strategy, base, id,
                                      target = NULL, target_id = NULL,
                                      ...) {
  dots <- list(...)
  # Filter the ... bag to only the args match_features() understands;
  # anything else (model, threshold, class_weighted, na_fill) is
  # consumed downstream.
  mf_kwargs <- dots[intersect(
    names(dots),
    c("top_n", "include_string_sim", "include_block_stats", "method")
  )]
  do.call(
    match_features,
    c(
      list(matches = matches, strategy = strategy,
           base = base, id = id,
           target = target, target_id = target_id),
      mf_kwargs
    )
  )
}


#' @noRd
.calibrate_matches_impl <- function(matches, strategy, labels,
                                    base, id,
                                    target = NULL, target_id = NULL,
                                    model           = "logistic",
                                    class_weighted  = FALSE,
                                    na_fill         = 0,
                                    threshold       = NULL,
                                    ...) {

  if (missing(labels) || is.null(labels)) {
    stop("`labels` is required for calibrate_matches().", call. = FALSE)
  }

  features <- .calibrate_build_features(
    matches  = matches,
    strategy = strategy,
    base     = base,
    id       = id,
    target   = target,
    target_id = target_id,
    ...
  )

  fm <- fit_filter(
    features        = features,
    labels          = labels,
    model           = model,
    class_weighted  = class_weighted,
    na_fill         = na_fill
  )

  apply_filter(
    features     = features,
    filter_model = fm,
    threshold    = threshold,
    matches      = matches
  )
}


# ---------- data.table methods --------------------------------------

method(
  calibrate_matches,
  list(DT_tbl, Search_Strategy)
) <- function(matches, strategy, labels,
              base, id,
              target = NULL, target_id = NULL,
              model = "logistic",
              class_weighted = FALSE,
              na_fill = 0,
              threshold = NULL,
              ...) {
  .calibrate_matches_impl(
    matches = matches, strategy = strategy, labels = labels,
    base = base, id = id,
    target = target, target_id = target_id,
    model = model, class_weighted = class_weighted,
    na_fill = na_fill, threshold = threshold,
    ...
  )
}

method(
  calibrate_matches,
  list(DT_tbl, Embedding_Strategy)
) <- function(matches, strategy, labels,
              base = NULL, id = NULL,
              target = NULL, target_id = NULL,
              model = "logistic",
              class_weighted = FALSE,
              na_fill = 0,
              threshold = NULL,
              ...) {
  .calibrate_matches_impl(
    matches = matches, strategy = strategy, labels = labels,
    base = base, id = id,
    target = target, target_id = target_id,
    model = model, class_weighted = class_weighted,
    na_fill = na_fill, threshold = threshold,
    ...
  )
}


# ---------- DuckDB methods (collect-and-delegate) -------------------

method(
  calibrate_matches,
  list(Duck_tbl, Search_Strategy)
) <- function(matches, strategy, labels,
              base, id,
              target = NULL, target_id = NULL, ...) {
  matches_dt <- data.table::as.data.table(dplyr::collect(matches))
  base_dt    <- if (inherits(base, "tbl_duckdb_connection")) {
    data.table::as.data.table(dplyr::collect(base))
  } else data.table::as.data.table(base)
  target_dt  <- if (!is.null(target) &&
                    inherits(target, "tbl_duckdb_connection")) {
    data.table::as.data.table(dplyr::collect(target))
  } else if (!is.null(target)) {
    data.table::as.data.table(target)
  } else NULL

  calibrate_matches(
    matches_dt, strategy, labels = labels,
    base = base_dt, id = id,
    target = target_dt, target_id = target_id,
    ...
  )
}

method(
  calibrate_matches,
  list(Duck_tbl, Embedding_Strategy)
) <- function(matches, strategy, labels,
              base = NULL, id = NULL,
              target = NULL, target_id = NULL, ...) {
  matches_dt <- data.table::as.data.table(dplyr::collect(matches))
  base_dt    <- if (!is.null(base) &&
                    inherits(base, "tbl_duckdb_connection")) {
    data.table::as.data.table(dplyr::collect(base))
  } else if (!is.null(base)) data.table::as.data.table(base) else NULL
  target_dt  <- if (!is.null(target) &&
                    inherits(target, "tbl_duckdb_connection")) {
    data.table::as.data.table(dplyr::collect(target))
  } else if (!is.null(target)) data.table::as.data.table(target) else NULL

  calibrate_matches(
    matches_dt, strategy, labels = labels,
    base = base_dt, id = id,
    target = target_dt, target_id = target_id,
    ...
  )
}


# ---------- tibble / data.frame thin wrappers -----------------------

method(
  calibrate_matches,
  list(.jyDF, Search_Strategy)
) <- function(matches, strategy, labels,
              base, id,
              target = NULL, target_id = NULL, ...) {
  calibrate_matches(
    as_DT(matches), strategy, labels = labels,
    base = if (!is.null(base))   as_DT(base)   else NULL,
    id   = id,
    target = if (!is.null(target)) as_DT(target) else NULL,
    target_id = target_id,
    ...
  )
}
method(
  calibrate_matches,
  list(.jyTBL_DF, Search_Strategy)
) <- function(matches, strategy, labels,
              base, id,
              target = NULL, target_id = NULL, ...) {
  calibrate_matches(
    as_DT(matches), strategy, labels = labels,
    base = if (!is.null(base))   as_DT(base)   else NULL,
    id   = id,
    target = if (!is.null(target)) as_DT(target) else NULL,
    target_id = target_id,
    ...
  )
}
method(
  calibrate_matches,
  list(.jyTBL, Search_Strategy)
) <- function(matches, strategy, labels,
              base, id,
              target = NULL, target_id = NULL, ...) {
  calibrate_matches(
    as_DT(matches), strategy, labels = labels,
    base = if (!is.null(base))   as_DT(base)   else NULL,
    id   = id,
    target = if (!is.null(target)) as_DT(target) else NULL,
    target_id = target_id,
    ...
  )
}
method(
  calibrate_matches,
  list(.jyDF, Embedding_Strategy)
) <- function(matches, strategy, labels, ...) {
  calibrate_matches(as_DT(matches), strategy, labels = labels, ...)
}
method(
  calibrate_matches,
  list(.jyTBL_DF, Embedding_Strategy)
) <- function(matches, strategy, labels, ...) {
  calibrate_matches(as_DT(matches), strategy, labels = labels, ...)
}
method(
  calibrate_matches,
  list(.jyTBL, Embedding_Strategy)
) <- function(matches, strategy, labels, ...) {
  calibrate_matches(as_DT(matches), strategy, labels = labels, ...)
}
