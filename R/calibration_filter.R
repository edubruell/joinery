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

#' Detect the match type of a *labels* table, score-agnostically.
#'
#' A labels table carries the pair-identifying columns plus `equal`, but
#' commonly arrives without a `score` column (it is hand-built from a CSV or an
#' agent, and `.collapse_pair_labels()` never reads `score` anyway). The general
#' `.detect_match_type()` requires `score`, so use a relaxed detector here that
#' keys only on the structural pair columns.
#'
#' @noRd
.detect_label_type <- function(labels) {
  cols <- names(labels)
  if (all(c("duplicate_group", "id", "rank") %in% cols)) return("duplicates")
  if (all(c("match_id", "source", "id") %in% cols))       return("candidates")
  cli::cli_abort(c(
    "{.arg labels} does not look like a joinery labels table.",
    "i" = "Expected either:",
    "*" = "duplicate labels: {.field duplicate_group}, {.field id}, {.field rank}, {.field equal}",
    "*" = "candidate labels: {.field match_id}, {.field source}, {.field id}, {.field equal}",
    "i" = "A {.field score} column is not required (it is never read for labels)."
  ))
}

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
  mt <- .detect_label_type(dt)
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
  numeric_like <- map_lgl(features_dt, function(v) {
    is.numeric(v) || is.integer(v) || is.logical(v)
  })
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


#' Reduce a predictor set to a maximal full-rank (linearly independent) subset.
#'
#' The full `match_features` schema is collinear on a given match set, the
#' symmetric `sim_sf_<col>`/`sim_fs_<col>` pair is identical for dedup, and
#' several count predictors are exact linear combinations when a block's records
#' share structure. `glm` silently aliases such columns (NA coefficients) and
#' then warns `prediction from rank-deficient fit` at every `predict()`. Pruning
#' them *before* the fit yields an identical model with no warning.
#'
#' Uses a column-pivoted QR of `[intercept | predictors]`: the first `rank`
#' pivots index the independent columns; the rest are redundant. Original
#' predictor order is preserved.
#'
#' @noRd
.full_rank_predictors <- function(design_dt, predictors) {
  if (length(predictors) <= 1L) return(predictors)
  mm  <- as.matrix(design_dt[, predictors, with = FALSE])
  mm  <- cbind(`(Intercept)` = 1, mm)
  qrd <- qr(mm)                      # LINPACK column pivoting, tol = 1e-7
  if (qrd$rank == ncol(mm)) return(predictors)   # already full rank
  kept <- colnames(mm)[qrd$pivot[seq_len(qrd$rank)]]
  intersect(predictors, kept)        # preserve order, drops the intercept name
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


#' Pick the highest threshold that still achieves a target recall.
#'
#' Recall is monotone-decreasing in the threshold, so we may drop at most
#' `floor((1 - target_recall) * P)` of the `P` positives. Sorting the positive
#' probabilities ascending, the threshold is the smallest *kept* positive's
#' probability, guaranteeing `recall >= target_recall` exactly (ties only ever
#' raise recall). Recall-favouring operating point for a firm panel.
#'
#' @noRd
.target_recall_threshold <- function(prob, equal, target_recall = 0.95) {
  pos <- sort(prob[as.integer(equal) == 1L])
  n   <- length(pos)
  if (n == 0L) return(0.5)
  target_recall <- max(0, min(1, target_recall))
  drop_max <- floor((1 - target_recall) * n)        # positives we may lose
  k        <- min(drop_max + 1L, n)                  # smallest positive we keep
  pos[k]
}

#' Pick the cost-minimising threshold for an asymmetric error cost.
#'
#' `cost_ratio = cost(FN) / cost(FP)`; total cost at a cut keeping the top-`i`
#' scoring pairs is `cost_ratio * FN + FP`. A higher `cost_ratio` makes false
#' negatives dearer and shifts the optimum to a lower threshold (more recall),
#' monotonically. Walks scores descending; on ties takes the highest (most
#' stringent) probability.
#'
#' @noRd
.cost_weighted_threshold <- function(prob, equal, cost_ratio = 1) {
  if (length(prob) == 0L) return(0.5)
  if (length(unique(as.integer(equal))) < 2L) return(0.5)
  ord <- order(prob, decreasing = TRUE)
  p   <- prob[ord]
  y   <- as.integer(equal[ord])
  P   <- sum(y == 1L)
  tp  <- cumsum(y == 1L)
  fp  <- cumsum(y == 0L)
  fn  <- P - tp
  cost <- cost_ratio * fn + fp
  p[which.min(cost)]                                 # first min = highest prob
}

#' Dispatch threshold selection over the declared operating-point rule.
#' @noRd
.select_threshold <- function(prob, equal, rule = "youden",
                              target_recall = 0.95, cost_ratio = 1) {
  switch(
    rule,
    youden        = .youden_j_threshold(prob, equal),
    target_recall = .target_recall_threshold(prob, equal, target_recall),
    cost_weighted = .cost_weighted_threshold(prob, equal, cost_ratio),
    cli::cli_abort(c(
      "Unknown {.arg threshold_rule}: {.val {rule}}",
      "i" = "Use one of {.val youden}, {.val target_recall}, {.val cost_weighted}."
    ))
  )
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
      "Labels contain only one class, cannot fit a binary classifier",
      "i" = "Need both {.field equal} == 0L and {.field equal} == 1L rows"
    ))
  }

  # Drop predictors that are constant in the training data -- glm would
  # silently produce NA coefficients otherwise. Keep them in
  # @predictors so apply_filter() still emits a column for them (they
  # will contribute zero).
  keep <- map_lgl(predictors, function(col) {
    v <- X[[col]]
    length(unique(stats::na.omit(v))) > 1L
  })
  fit_predictors <- predictors[keep]
  if (length(fit_predictors) == 0L) {
    cli::cli_abort("All predictors are constant, cannot fit a model")
  }

  # Then drop perfectly-aliased predictors (linear combinations) so glm is
  # full-rank and never warns `prediction from rank-deficient fit`. The dropped
  # columns stay in @predictors (a superset is harmless for newdata) but leave
  # the formula, so the fit and every predict() align (B3).
  fit_predictors <- .full_rank_predictors(X, fit_predictors)

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
                               threshold     = NULL,
                               threshold_rule = "youden",
                               target_recall = 0.95,
                               cost_ratio    = 1,
                               matches   = NULL,
                               ...) {
  threshold_rule <- match.arg(
    threshold_rule, c("youden", "target_recall", "cost_weighted")
  )

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
      threshold_method = if (!is.null(threshold)) "user"
                         else if (threshold_rule == "youden") "youden_j"
                         else threshold_rule,
      recommendations  = character()
    ))
  }

  prob <- .predict_filter(ft, filter_model)

  # A user-supplied `threshold` always wins; otherwise the operating point is
  # the declared `threshold_rule` evaluated on the training labels (B4).
  thr_method <- "user"
  thr <- threshold
  if (is.null(thr)) {
    # Back-compat: the default rule's method label stays "youden_j"; the new
    # rules report their own name.
    thr_method <- if (threshold_rule == "youden") "youden_j" else threshold_rule
    thr <- .select_threshold(
      filter_model@training_prob,
      filter_model@training_equal,
      rule          = threshold_rule,
      target_recall = target_recall,
      cost_ratio    = cost_ratio
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
#' @return A `Filter_Model` object.
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
#' Score a `Match_Features` table with a fitted `Filter_Model` and
#' return a `Calibrated_Matches` object. When `matches` is supplied,
#' the original match table is enriched with `tp_prob` and
#' `predicted_tp` columns and stored in the result's `@matches` slot;
#' when `matches` is `NULL`, the features table itself is enriched and
#' stored.
#'
#' @param features A `Match_Features` object.
#' @param filter_model A `Filter_Model` produced by [fit_filter()].
#' @param threshold Numeric scalar in (0, 1) or `NULL`. When non-`NULL` it is
#'   used verbatim and overrides `threshold_rule`. When `NULL`, the threshold is
#'   chosen on the training labels per `threshold_rule`. Decision 13.7 default.
#' @param threshold_rule The operating-point rule used when `threshold` is
#'   `NULL`: `"youden"` (default, maximise Youden's J, symmetric error costs),
#'   `"target_recall"` (the highest threshold still achieving `target_recall`),
#'   or `"cost_weighted"` (minimise `cost_ratio * FN + FP`). For a firm panel the
#'   recall-favouring rules are usually the right operating point, splitting one
#'   business across years is worse than admitting a few co-located firms a later
#'   collapse can still catch.
#' @param target_recall Target recall in (0, 1] for
#'   `threshold_rule = "target_recall"`. Default `0.95`.
#' @param cost_ratio `cost(FN) / cost(FP)` for `threshold_rule =
#'   "cost_weighted"`; `> 1` favours recall. Default `1` (symmetric).
#' @param matches Optional raw matches table to enrich. When supplied,
#'   `tp_prob` / `predicted_tp` are broadcast onto every row of the
#'   pair (candidates: both `source == "base"` and `source == "target"`
#'   rows of a `match_id`; duplicates: every row of a `duplicate_group`).
#' @param ... Reserved for future expansion.
#'
#' @return A `Calibrated_Matches` object.
#'
#' @export
apply_filter <- function(features, filter_model,
                         threshold      = NULL,
                         threshold_rule = c("youden", "target_recall",
                                            "cost_weighted"),
                         target_recall  = 0.95,
                         cost_ratio     = 1,
                         matches   = NULL,
                         ...) {
  threshold_rule <- match.arg(threshold_rule)
  .apply_filter_impl(
    features       = features,
    filter_model   = filter_model,
    threshold      = threshold,
    threshold_rule = threshold_rule,
    target_recall  = target_recall,
    cost_ratio     = cost_ratio,
    matches        = matches,
    ...
  )
}
