# ============================================================
# fit_filter() / apply_filter() -- Phase 0.7 M5
# ============================================================
#
# Baseline post-retrieval false-positive filter.
#
# `fit_filter()` accepts a `Match_Features` object and a labels
# `data.table` (typically from `import_labels()`), joins them on the
# pair keys, fits a `glm(equal ~ ., family = binomial)` over the
# numeric predictor columns, and returns a `Filter_Model`.
#
# `apply_filter()` scores a `Match_Features` table with a fitted
# `Filter_Model` and returns a `Calibrated_Matches` object. The optional
# `matches` argument enriches the raw matches table directly so
# downstream users can keep working with `match_id` / `source` rows.
#
# Threshold selection: Youden's J on the training labels (decision
# 13.7). User-supplied threshold takes precedence.
#
# Predictor selection: all numeric / integer / logical columns of
# `@features` except the id columns (`searched`, `found`, `match_id`)
# and `equal`. `score` and `stage` one-hots are predictors.
# ============================================================


# ---------- label / feature plumbing --------------------------------

#' Identify the pair-side row of a labels table and reduce to
#' one row per pair carrying `(match_id, found, equal)`.
#'
#' For candidates: pair row = `source == "target"` (the candidate).
#' For duplicates: pair row = `rank >= 2L`; `match_id = duplicate_group`,
#' `found = id`.
#'
#' @noRd
.collapse_pair_labels <- function(labels) {
  dt <- data.table::as.data.table(labels)
  if (!"equal" %in% names(dt)) {
    stop("`labels` must contain an `equal` column (0L/1L).", call. = FALSE)
  }
  mt <- .detect_match_type(dt)
  if (mt == "candidates") {
    if (!"source" %in% names(dt)) {
      stop("Candidate labels must have a `source` column.", call. = FALSE)
    }
    out <- dt[dt$source == "target",
              .(match_id, found = id, equal = as.integer(equal))]
  } else {
    if (!"rank" %in% names(dt)) {
      stop("Duplicate labels must have a `rank` column.", call. = FALSE)
    }
    out <- dt[dt$rank >= 2L,
              .(match_id = duplicate_group,
                found    = id,
                equal    = as.integer(equal))]
  }
  out[, found := as.character(found)]
  unique(out, by = c("match_id", "found"))
}


#' Resolve the predictor columns of a `Match_Features` object.
#'
#' Numeric / integer / logical columns minus id columns, `equal`,
#' `stage` (the character form), `searched`, `found`, `match_id`.
#'
#' @noRd
.feature_predictors <- function(features_dt) {
  drop <- c("searched", "found", "match_id", "stage", "equal",
            "tp_prob", "predicted_tp")
  numeric_like <- vapply(features_dt, function(v) {
    is.numeric(v) || is.integer(v) || is.logical(v)
  }, logical(1L))
  cols <- setdiff(names(features_dt)[numeric_like], drop)
  cols
}


#' Build a design matrix (data.table) with NA-filled predictors.
#'
#' @noRd
.build_design <- function(features_dt, predictors, na_fill = 0) {
  out <- data.table::as.data.table(
    features_dt[, predictors, with = FALSE]
  )
  for (col in predictors) {
    v <- out[[col]]
    if (is.logical(v)) v <- as.numeric(v)
    if (anyNA(v))      v[is.na(v)] <- na_fill
    out[[col]] <- v
  }
  # Drop constant predictors at fit time (variance == 0). Caller decides
  # whether to track this -- for prediction we keep all `predictors`.
  out
}


# ---------- Youden's J on a probability vector ----------------------

#' Pick a threshold that maximises Youden's J (TPR - FPR).
#'
#' Walks the score grid in descending order. On ties at the maximum
#' J, returns the highest probability among them (the most stringent
#' cut), which `predict(prob >= thr)` interprets as "fewer kept".
#'
#' @noRd
.youden_j_threshold <- function(prob, equal) {
  if (length(prob) == 0L || length(equal) == 0L) return(0.5)
  if (length(unique(equal)) < 2L) return(0.5)

  ord <- order(prob, decreasing = TRUE)
  p   <- prob[ord]
  y   <- as.integer(equal[ord])

  P <- sum(y == 1L)
  N <- sum(y == 0L)
  if (P == 0L || N == 0L) return(0.5)

  tp_cum <- cumsum(y == 1L)
  fp_cum <- cumsum(y == 0L)
  tpr    <- tp_cum / P
  fpr    <- fp_cum / N
  j      <- tpr - fpr

  best <- which.max(j)
  # Threshold = score at best point; use a small epsilon so that
  # equality goes to "kept" rather than dropped at the boundary.
  thr <- p[best]
  thr
}


# ---------- fit_filter (implementation) -----------------------------

#' @noRd
.fit_filter_impl <- function(features, labels,
                             model          = "logistic",
                             class_weighted = FALSE,
                             na_fill        = 0,
                             ...) {

  if (!S7::S7_inherits(features, Match_Features)) {
    stop("`features` must be a Match_Features object (from match_features()).",
         call. = FALSE)
  }
  if (!identical(model, "logistic")) {
    stop(
      "Only the baseline `model = \"logistic\"` path is implemented in M5. ",
      "Tidymodels support arrives in M6.",
      call. = FALSE
    )
  }

  ft <- data.table::as.data.table(features@features)
  if (nrow(ft) == 0L) {
    stop("`features` contains no rows.", call. = FALSE)
  }

  lab <- .collapse_pair_labels(labels)
  if (nrow(lab) == 0L) {
    stop("No labelled pair rows to fit on.", call. = FALSE)
  }

  ft[, searched := as.character(searched)]
  ft[, found    := as.character(found)]
  if ("match_id" %in% names(ft)) {
    # align types so the join is robust to int / num / char drift
    target_class <- class(ft$match_id)[1L]
    coercer <- switch(
      target_class,
      integer   = as.integer,
      numeric   = as.numeric,
      double    = as.numeric,
      character = as.character,
      identity
    )
    lab[, match_id := suppressWarnings(coercer(match_id))]
  }

  joined <- merge(
    ft, lab,
    by = c("match_id", "found"),
    all.x = FALSE, all.y = FALSE, sort = FALSE
  )
  if (nrow(joined) == 0L) {
    stop(
      "No features rows matched any label row on (match_id, found). ",
      "Make sure `labels` came from the same matches table.",
      call. = FALSE
    )
  }

  predictors <- .feature_predictors(joined)
  if (length(predictors) == 0L) {
    stop("No numeric predictor columns found in `features`.", call. = FALSE)
  }

  X <- .build_design(joined, predictors, na_fill = na_fill)
  y <- as.integer(joined$equal)

  if (length(unique(y)) < 2L) {
    stop(
      "Labels contain only one class -- cannot fit a binary classifier. ",
      "Need both equal == 0L and equal == 1L rows.",
      call. = FALSE
    )
  }

  # Drop predictors that are constant in the training data -- glm would
  # silently produce NA coefficients otherwise. Keep them in
  # @predictors so apply_filter() still emits a column for them (they
  # will contribute zero).
  keep <- vapply(predictors, function(col) {
    v <- X[[col]]
    length(unique(stats::na.omit(v))) > 1L
  }, logical(1L))
  fit_predictors <- predictors[keep]
  if (length(fit_predictors) == 0L) {
    stop("All predictors are constant -- cannot fit a model.", call. = FALSE)
  }

  weights <- NULL
  if (isTRUE(class_weighted)) {
    p1 <- mean(y == 1L)
    p0 <- 1 - p1
    if (p1 == 0 || p0 == 0) {
      stop("Class-weighted fit requires both classes present.", call. = FALSE)
    }
    weights <- ifelse(y == 1L, 1 / (2 * p1), 1 / (2 * p0))
  }

  fit_df <- as.data.frame(X[, fit_predictors, with = FALSE])
  fit_df$.y <- y
  fml <- stats::as.formula(
    paste(".y ~", paste(fit_predictors, collapse = " + "))
  )

  fit <- suppressWarnings(stats::glm(
    fml,
    data    = fit_df,
    family  = stats::binomial(link = "logit"),
    weights = weights
  ))

  stage_dist <- if ("stage" %in% names(joined) &&
                    !all(is.na(joined$stage))) {
    tbl <- table(joined$stage, useNA = "no")
    sd  <- as.numeric(tbl) / sum(tbl)
    names(sd) <- names(tbl)
    sd
  } else NULL

  training_prob <- as.numeric(
    stats::predict(fit, newdata = fit_df, type = "response")
  )

  Filter_Model(
    backend                = "glm",
    fit                    = fit,
    predictors             = predictors,           # full set, for prediction
    model_class            = class(fit)[1L],
    training_n             = nrow(joined),
    training_class_balance = mean(y == 1L),
    training_stage_dist    = stage_dist,
    na_fill                = as.numeric(na_fill),
    class_weighted         = isTRUE(class_weighted),
    training_prob          = training_prob,
    training_equal         = as.integer(y)
  )
}


# ---------- apply_filter (implementation) ---------------------------

#' Compute predictions on a feature data.table using a Filter_Model.
#'
#' @noRd
.predict_filter <- function(features_dt, filter_model) {
  if (!S7::S7_inherits(filter_model, Filter_Model)) {
    stop("`filter_model` must be a Filter_Model object.", call. = FALSE)
  }

  predictors <- filter_model@predictors
  missing_preds <- setdiff(predictors, names(features_dt))
  for (mp in missing_preds) {
    features_dt[, (mp) := filter_model@na_fill]
  }

  X <- .build_design(features_dt, predictors,
                     na_fill = filter_model@na_fill)
  newdf <- as.data.frame(X)

  as.numeric(stats::predict(
    filter_model@fit, newdata = newdf, type = "response"
  ))
}


#' @noRd
.apply_filter_impl <- function(features, filter_model,
                               threshold = NULL,
                               matches   = NULL,
                               ...) {

  if (!S7::S7_inherits(features, Match_Features)) {
    stop("`features` must be a Match_Features object.", call. = FALSE)
  }

  ft <- data.table::copy(data.table::as.data.table(features@features))
  if (nrow(ft) == 0L) {
    out <- data.table::copy(ft)
    out[, tp_prob := numeric()]
    out[, predicted_tp := integer()]
    return(Calibrated_Matches(
      matches          = out,
      filter_model     = filter_model,
      threshold        = if (is.null(threshold)) NA_real_ else as.numeric(threshold),
      threshold_method = if (is.null(threshold)) "youden_j" else "user",
      recommendations  = character()
    ))
  }

  prob <- .predict_filter(ft, filter_model)

  thr_method <- "user"
  thr <- threshold
  if (is.null(thr)) {
    thr_method <- "youden_j"
    thr <- .youden_j_threshold(
      filter_model@training_prob,
      filter_model@training_equal
    )
  }
  thr <- as.numeric(thr)
  if (!is.finite(thr) || thr < 0 || thr > 1) {
    stop("`threshold` must be a finite probability in [0, 1].", call. = FALSE)
  }

  ft[, tp_prob := prob]
  ft[, predicted_tp := as.integer(prob >= thr)]

  if (!is.null(matches)) {
    enriched <- .broadcast_predictions_to_matches(
      matches, ft[, .(match_id, found, tp_prob, predicted_tp)]
    )
  } else {
    enriched <- ft
  }

  Calibrated_Matches(
    matches          = enriched,
    filter_model     = filter_model,
    threshold        = thr,
    threshold_method = thr_method,
    recommendations  = character()
  )
}


#' Broadcast pair-level predictions back onto the raw matches table.
#'
#' For candidate matches: every row of `match_id` (`source == "base"` and
#'   `source == "target"`) receives the pair's `tp_prob` /
#'   `predicted_tp`.
#' For duplicate matches: rank-k rows (where `id == found`) receive the
#'   pair's prediction; rank-1 rows receive `NA_real_` / `NA_integer_`
#'   because there is no single pair-level prediction to attach to
#'   them.
#'
#' @noRd
.broadcast_predictions_to_matches <- function(matches, pair_preds) {
  dt <- data.table::copy(data.table::as.data.table(matches))
  mt <- .detect_match_type(dt)

  pair_preds <- data.table::copy(pair_preds)
  pair_preds[, found := as.character(found)]

  if (mt == "candidates") {
    # join on match_id only -- both base and target rows of the same
    # match_id receive the same prediction.
    pp <- unique(pair_preds[, .(match_id, tp_prob, predicted_tp)],
                 by = "match_id")
    dt <- merge(dt, pp, by = "match_id", all.x = TRUE, sort = FALSE)
  } else {
    # duplicates: join on (duplicate_group, id == found)
    pp <- pair_preds[, .(duplicate_group = match_id,
                         id              = found,
                         tp_prob, predicted_tp)]
    dt[, id := as.character(id)]
    pp[, id := as.character(id)]
    dt <- merge(dt, pp, by = c("duplicate_group", "id"),
                all.x = TRUE, sort = FALSE)
  }
  dt
}
