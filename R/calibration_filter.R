# ============================================================
# fit_filter() / apply_filter()
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
    cli::cli_abort("{.arg labels} must contain an {.field equal} column (0L/1L)")
  }
  mt <- .detect_match_type(dt)
  if (mt == "candidates") {
    if (!"source" %in% names(dt)) {
      cli::cli_abort("Candidate labels must have a {.field source} column")
    }
    out <- dt[dt$source == "target",
              .(match_id, found = id, equal = as.integer(equal))]
  } else {
    if (!"rank" %in% names(dt)) {
      cli::cli_abort("Duplicate labels must have a {.field rank} column")
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
    cli::cli_abort("{.arg features} must be a {.cls Match_Features} object (from {.fn match_features})")
  }
  if (!identical(model, "logistic")) {
    # tidymodels path: accept a parsnip model spec, a fitted parsnip object,
    # or a fitted workflow.
    return(.fit_filter_tidymodels(
      features        = features,
      labels          = labels,
      model           = model,
      class_weighted  = class_weighted,
      na_fill         = na_fill,
      ...
    ))
  }

  ft <- data.table::as.data.table(features@features)
  if (nrow(ft) == 0L) {
    cli::cli_abort("{.arg features} contains no rows")
  }

  lab <- .collapse_pair_labels(labels)
  if (nrow(lab) == 0L) {
    cli::cli_abort("No labelled pair rows to fit on")
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
    cli::cli_abort(c(
      "No features rows matched any label row on (match_id, found)",
      "i" = "Make sure {.arg labels} came from the same matches table"
    ))
  }

  predictors <- .feature_predictors(joined)
  if (length(predictors) == 0L) {
    cli::cli_abort("No numeric predictor columns found in {.arg features}")
  }

  X <- .build_design(joined, predictors, na_fill = na_fill)
  y <- as.integer(joined$equal)

  if (length(unique(y)) < 2L) {
    cli::cli_abort(c(
      "Labels contain only one class — cannot fit a binary classifier",
      "i" = "Need both {.field equal} == 0L and {.field equal} == 1L rows"
    ))
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
    cli::cli_abort("All predictors are constant — cannot fit a model")
  }

  weights <- NULL
  if (isTRUE(class_weighted)) {
    p1 <- mean(y == 1L)
    p0 <- 1 - p1
    if (p1 == 0 || p0 == 0) {
      cli::cli_abort("Class-weighted fit requires both classes present")
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
    cli::cli_abort("{.arg filter_model} must be a {.cls Filter_Model} object")
  }

  predictors <- filter_model@predictors
  missing_preds <- setdiff(predictors, names(features_dt))
  for (mp in missing_preds) {
    features_dt[, (mp) := filter_model@na_fill]
  }

  X <- .build_design(features_dt, predictors,
                     na_fill = filter_model@na_fill)
  newdf <- as.data.frame(X)

  if (filter_model@backend %in% c("parsnip", "workflow")) {
    if (!requireNamespace("parsnip", quietly = TRUE)) {
      cli::cli_abort(c(
        "{.fn apply_filter} with tidymodels requires the {.pkg parsnip} package",
        "i" = "Install it via {.run install.packages(\"parsnip\")}"
      ))
    }
    pr <- stats::predict(filter_model@fit, new_data = newdf,
                         type = "prob")
    # parsnip returns a tibble with .pred_<level> columns; the positive
    # class is `.pred_1` (training fixes `equal` to `factor(..., levels =
    # c(0L, 1L))`, so `1` is always the second level).
    if (!".pred_1" %in% names(pr)) {
      cli::cli_abort(c(
        "{.fn parsnip::predict}() did not return a {.field .pred_1} column",
        "i" = "{.cls Filter_Model} was not trained with the joinery convention"
      ))
    }
    return(as.numeric(pr[[".pred_1"]]))
  }

  as.numeric(stats::predict(
    filter_model@fit, newdata = newdf, type = "response"
  ))
}

# ---------- drift detection ----------------------------------------

#' Compute total-variation distance between two named distributions.
#' Missing categories on either side are treated as zero mass.
#' @noRd
.tv_distance <- function(p, q) {
  if (is.null(p) || is.null(q) || length(p) == 0L || length(q) == 0L) {
    return(NA_real_)
  }
  keys <- union(names(p), names(q))
  pv <- as.numeric(p[keys]); pv[is.na(pv)] <- 0
  qv <- as.numeric(q[keys]); qv[is.na(qv)] <- 0
  0.5 * sum(abs(pv - qv))
}


#' @noRd
.apply_filter_impl <- function(features, filter_model,
                               threshold = NULL,
                               matches   = NULL,
                               ...) {

  if (!S7::S7_inherits(features, Match_Features)) {
    cli::cli_abort("{.arg features} must be a {.cls Match_Features} object")
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
    cli::cli_abort("{.arg threshold} must be a finite probability in [0, 1]")
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

  # ---- covariate drift between training and inference stage dist ----
  signals <- list()
  if (!is.null(filter_model@training_stage_dist) &&
      "stage" %in% names(ft) && !all(is.na(ft$stage))) {
    tbl <- table(ft$stage, useNA = "no")
    if (sum(tbl) > 0L) {
      inf_dist <- as.numeric(tbl) / sum(tbl)
      names(inf_dist) <- names(tbl)
      tv <- .tv_distance(filter_model@training_stage_dist, inf_dist)
      if (!is.na(tv)) signals[["stage_dist_tv_distance"]] <- tv
    }
  }
  recs <- .dispatch_recommendations(signals)

  Calibrated_Matches(
    matches          = enriched,
    filter_model     = filter_model,
    threshold        = thr,
    threshold_method = thr_method,
    recommendations  = recs$messages
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


#' Fit a false-positive filter on labelled match pairs
#'
#' @description
#' Fit a baseline classifier to predict whether each scored pair is a
#' true match (`equal == 1L`) or a false positive (`equal == 0L`).
#' The baseline path uses `stats::glm` with the logit link and no
#' external dependencies. The features object is the input from
#' [match_features()]; labels carry the `equal` column produced by
#' [import_labels()].
#'
#' @param features A [`Match_Features`] object.
#' @param labels A `data.table` / `data.frame` with the matches schema
#'   plus an integer `equal` column (`0L` / `1L`). Typically produced by
#'   [import_labels()].
#' @param model Character scalar (default `"logistic"`) selecting the
#'   baseline `glm()` path. Future M6 work will accept a fitted parsnip
#'   / workflow object here.
#' @param class_weighted Logical scalar. When `TRUE`, fit `glm` with
#'   inverse-class-frequency `weights =`, useful for imbalanced
#'   training sets. Default `FALSE`.
#' @param na_fill Numeric scalar used to impute predictor NAs. Default
#'   `0` (sensible for aIP slot columns where NA means "no token").
#' @param ... Reserved for future expansion.
#'
#' @return A [`Filter_Model`] object.
#'
#' @export
fit_filter <- function(features, labels,
                       model = "logistic",
                       class_weighted = FALSE,
                       na_fill = 0,
                       ...) {
  .fit_filter_impl(
    features       = features,
    labels         = labels,
    model          = model,
    class_weighted = class_weighted,
    na_fill        = na_fill,
    ...
  )
}


#' Apply a fitted filter to match features
#'
#' @description
#' Score a [`Match_Features`] table with a fitted [`Filter_Model`] and
#' return a [`Calibrated_Matches`] object. When `matches` is supplied,
#' the original match table is enriched with `tp_prob` and
#' `predicted_tp` columns and stored in the result's `@matches` slot;
#' when `matches` is `NULL`, the features table itself is enriched and
#' stored.
#'
#' @param features A [`Match_Features`] object.
#' @param filter_model A [`Filter_Model`] produced by [fit_filter()].
#' @param threshold Numeric scalar in [0, 1] or `NULL`. When `NULL`,
#'   the threshold is chosen by Youden's J on the training labels
#'   stored on the [`Filter_Model`]. Decision 13.7 default.
#' @param matches Optional raw matches table to enrich. When supplied,
#'   `tp_prob` / `predicted_tp` are broadcast onto every row of the
#'   pair (candidates: both `source == "base"` and `source == "target"`
#'   rows of a `match_id`; duplicates: every row of a `duplicate_group`).
#' @param ... Reserved for future expansion.
#'
#' @return A [`Calibrated_Matches`] object.
#'
#' @export
apply_filter <- function(features, filter_model,
                         threshold = NULL,
                         matches   = NULL,
                         ...) {
  .apply_filter_impl(
    features      = features,
    filter_model  = filter_model,
    threshold     = threshold,
    matches       = matches,
    ...
  )
}
