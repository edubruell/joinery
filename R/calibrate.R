# ============================================================
# calibrate() + Filter_Calibration
# ============================================================
#
# Evaluates a fitted Filter_Model (carried on Calibrated_Matches)
# on a labelled set and returns calibration diagnostics:
#
#   * reliability table (mean predicted vs observed positive rate per bin)
#   * Brier score, log-loss
#   * per-class confusion matrix at the applied threshold
#   * threshold sweep (tpr / fpr / precision / recall / f1 / youden_j)
#
# Surfaces calibration_low_n_warning when training_n is small.
# ============================================================


# ---------- core metric helpers --------------------------------------

#' @noRd
.brier_score <- function(prob, y) {
  if (length(prob) == 0L) return(NA_real_)
  mean((prob - y)^2)
}

#' @noRd
.log_loss <- function(prob, y, eps = 1e-15) {
  if (length(prob) == 0L) return(NA_real_)
  p <- pmin(pmax(prob, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

#' @noRd
.reliability_table <- function(prob, y, bins = 10L) {
  bins <- as.integer(bins)
  if (bins < 2L) bins <- 2L
  if (length(prob) == 0L) {
    return(data.table::data.table(
      bin = integer(), bin_lower = numeric(), bin_upper = numeric(),
      n = integer(), mean_pred = numeric(), obs_pos = numeric()
    ))
  }
  breaks <- seq(0, 1, length.out = bins + 1L)
  # findInterval returns 1..bins for p in (0,1]; 0 for p==0; bins for p==1
  b <- findInterval(prob, breaks, rightmost.closed = TRUE, all.inside = TRUE)
  dt <- data.table::data.table(b = b, p = prob, y = as.numeric(y))
  out <- dt[, list(
    n         = .N,
    mean_pred = mean(p),
    obs_pos   = mean(y)
  ), by = b]
  data.table::setnames(out, "b", "bin")
  out[, bin_lower := breaks[bin]]
  out[, bin_upper := breaks[bin + 1L]]
  data.table::setorder(out, bin)
  data.table::setcolorder(out, c("bin", "bin_lower", "bin_upper",
                                 "n", "mean_pred", "obs_pos"))
  out[]
}

#' @noRd
.confusion_per_class <- function(prob, y, threshold) {
  pred <- as.integer(prob >= threshold)
  data.table::data.table(
    equal   = c(0L, 1L),
    n_pred0 = c(sum(y == 0L & pred == 0L), sum(y == 1L & pred == 0L)),
    n_pred1 = c(sum(y == 0L & pred == 1L), sum(y == 1L & pred == 1L))
  )
}

#' @noRd
.threshold_curve <- function(prob, y, grid = NULL) {
  if (length(prob) == 0L) {
    return(data.table::data.table(
      threshold = numeric(), tpr = numeric(), fpr = numeric(),
      precision = numeric(), recall = numeric(),
      f1 = numeric(), youden_j = numeric()
    ))
  }
  if (is.null(grid)) {
    grid <- sort(unique(c(0, seq(0.01, 0.99, by = 0.01), 1)))
  }
  y_int <- as.integer(y)
  P <- sum(y_int == 1L)
  N <- sum(y_int == 0L)

  out <- lapply(grid, function(t) {
    pred <- as.integer(prob >= t)
    tp <- sum(pred == 1L & y_int == 1L)
    fp <- sum(pred == 1L & y_int == 0L)
    fn <- P - tp
    tn <- N - fp
    tpr <- if (P > 0L) tp / P else NA_real_
    fpr <- if (N > 0L) fp / N else NA_real_
    prec <- if ((tp + fp) > 0L) tp / (tp + fp) else NA_real_
    rec  <- tpr
    f1   <- if (!is.na(prec) && !is.na(rec) && (prec + rec) > 0)
              2 * prec * rec / (prec + rec) else NA_real_
    j    <- if (!is.na(tpr) && !is.na(fpr)) tpr - fpr else NA_real_
    data.table::data.table(
      threshold = t, tpr = tpr, fpr = fpr,
      precision = prec, recall = rec, f1 = f1, youden_j = j
    )
  })
  data.table::rbindlist(out)
}


# ---------- core: evaluate on (prob, equal) --------------------------

#' @noRd
.calibrate_impl <- function(prob, y, threshold, bins = 10L,
                            training_n = NA_integer_) {
  prob <- as.numeric(prob)
  y    <- as.integer(y)
  keep <- !is.na(prob) & !is.na(y)
  prob <- prob[keep]
  y    <- y[keep]

  n <- length(prob)
  cb <- if (n > 0L) mean(y == 1L) else NA_real_

  rel  <- .reliability_table(prob, y, bins = bins)
  br   <- .brier_score(prob, y)
  ll   <- .log_loss(prob, y)
  cm   <- .confusion_per_class(prob, y, threshold)
  tc   <- .threshold_curve(prob, y)

  signals <- list()
  tn <- if (is.na(training_n)) n else training_n
  signals[["training_n"]] <- as.numeric(tn)
  recs <- .dispatch_recommendations(signals)

  Filter_Calibration(
    reliability         = rel,
    brier               = if (is.na(br)) NA_real_ else as.numeric(br),
    log_loss            = if (is.na(ll)) NA_real_ else as.numeric(ll),
    confusion_per_class = cm,
    threshold_curve     = tc,
    threshold           = as.numeric(threshold),
    n_eval              = as.integer(n),
    class_balance       = if (is.na(cb)) NA_real_ else as.numeric(cb),
    recommendations     = recs$messages
  )
}


# ---------- methods --------------------------------------------------

method(calibrate, Calibrated_Matches) <- function(x, labels = NULL,
                                                  bins = 10L, ...) {
  fm  <- x@filter_model
  thr <- x@threshold

  if (is.null(labels)) {
    prob <- fm@training_prob
    y    <- fm@training_equal
    return(.calibrate_impl(prob, y, threshold = thr, bins = bins,
                           training_n = fm@training_n))
  }

  lab <- .collapse_pair_labels(labels)
  ft  <- data.table::as.data.table(x@matches)
  # We need a feature-shaped table to score. If x@matches is the raw
  # matches table (no tp_prob means apply_filter() was never called), we
  # can't proceed -- but Calibrated_Matches always has tp_prob.
  if (!"tp_prob" %in% names(ft)) {
    cli::cli_abort("{.code x@matches} does not carry {.field tp_prob}; cannot evaluate")
  }

  # Determine match type to know how to join.
  mt <- tryCatch(.detect_match_type(ft), error = function(e) "features")
  if (mt == "features") {
    # ft already has match_id + found
    pairs <- unique(ft[, .(match_id, found = as.character(found),
                           tp_prob)], by = c("match_id", "found"))
  } else if (mt == "candidates") {
    pairs <- unique(
      ft[source == "target",
         .(match_id, found = as.character(id), tp_prob)],
      by = c("match_id", "found")
    )
  } else { # duplicates
    pairs <- unique(
      ft[rank >= 2L,
         .(match_id = duplicate_group, found = as.character(id), tp_prob)],
      by = c("match_id", "found")
    )
  }

  if ("match_id" %in% names(pairs) && "match_id" %in% names(lab)) {
    target_class <- class(pairs$match_id)[1L]
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

  joined <- merge(pairs, lab,
                  by = c("match_id", "found"),
                  all.x = FALSE, all.y = FALSE, sort = FALSE)
  if (nrow(joined) == 0L) {
    cli::cli_abort("No labelled rows joined onto the calibrated matches")
  }

  .calibrate_impl(joined$tp_prob, joined$equal,
                  threshold = thr, bins = bins,
                  training_n = fm@training_n)
}


# ---------- tidymodels shim ------------------------------------------

#' @noRd
.tidymodels_available <- function(pkgs = c("recipes", "parsnip")) {
  all(vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE))
}

#' @noRd
.joinery_recipe_impl <- function(features, labels, ...) {
  if (!requireNamespace("recipes", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.fn joinery_recipe} requires the {.pkg recipes} package",
      "i" = "Install it via {.run install.packages(\"recipes\")}"
    ))
  }
  if (!S7::S7_inherits(features, Match_Features)) {
    cli::cli_abort("{.arg features} must be a {.cls Match_Features} object")
  }

  ft  <- data.table::as.data.table(features@features)
  lab <- .collapse_pair_labels(labels)
  ft[, found := as.character(found)]
  if ("match_id" %in% names(ft)) {
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
  joined <- merge(ft, lab,
                  by = c("match_id", "found"),
                  all.x = FALSE, all.y = FALSE, sort = FALSE)
  if (nrow(joined) == 0L) {
    cli::cli_abort("No features rows matched any label row on {.code (match_id, found)}")
  }

  joined[, equal := factor(equal, levels = c(0L, 1L))]
  df <- as.data.frame(joined)

  rec <- recipes::recipe(equal ~ ., data = df)
  id_cols <- intersect(c("searched", "found", "match_id"), names(df))
  if (length(id_cols) > 0L) {
    rec <- recipes::update_role(rec, !!!rlang::syms(id_cols),
                                new_role = "id")
  }
  if ("stage" %in% names(df)) {
    rec <- recipes::update_role(rec, stage, new_role = "stage")
  }
  rec
}
