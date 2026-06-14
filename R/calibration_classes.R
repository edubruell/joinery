# ============================================================
# Calibration artifact classes
# ============================================================
#
# S7 classes that carry the calibration workflow's outputs:
#   - Match_Features      (output of match_features())
#   - Filter_Model        (output of fit_filter())
#   - Calibrated_Matches  (output of apply_filter() / calibrate_matches())
#   - Filter_Calibration  (output of calibrate())
#
# Each class' format/print/as.data.table/as.data.frame methods live
# alongside the class definition. The recommendations() generic is
# declared in generics_diagnostic.R; the methods for the two classes
# that carry recommendations are at the bottom of this file.
#
# ============================================================

#' Match Features Result
#'
#' @description
#' Result of [match_features()]. A wide, one-row-per-pair feature table
#' suitable for downstream calibration / filtering. Schema is documented
#' in `notes/calibration_design.md` and treated as the public API -
#' additions only, never reorder or rename.
#'
#' @slot features `data.table`. The wide feature matrix.
#' @slot schema Character. One of `"token"` (full schema) or
#'   `"embedding"` (reduced schema - no token columns).
#' @slot strategy_class Character. Class name of the strategy used.
#' @slot top_n Named integer. Effective per-column `top_n` (after defaulting).
#' @slot columns Character. Strategy column names in their canonical order.
#' @slot aip_summary Named list or `NULL`. Diagnostic statistics over the
#'   per-token aIP values consumed (token strategies only).
#'
#' @noRd
Match_Features <- new_class(
  "Match_Features",
  properties = list(
    features       = class_any,
    schema         = class_character,
    strategy_class = class_character,
    top_n          = class_any,
    columns        = class_character,
    aip_summary    = class_any
  )
)

#' @noRd
print.Match_Features <- new_external_generic("base", "print", "x")
#' @noRd
format.Match_Features <- new_external_generic("base", "format", "x")
#' @noRd
as.data.table.Match_Features <- new_external_generic(
  "data.table", "as.data.table", "x"
)
#' @noRd
as.data.frame.Match_Features <- new_external_generic(
  "base", "as.data.frame", "x"
)


#' Filter Model
#'
#' @description
#' Wraps a fitted false-positive filter. Returned by [fit_filter()] and
#' consumed by [apply_filter()].
#'
#' @slot backend Character. One of `"glm"`, `"parsnip"`, `"workflow"`.
#' @slot fit The underlying fitted model object.
#' @slot predictors Character. Names of predictor columns the model
#'   expects to see in features tables it scores.
#' @slot model_class Character. `class(fit)[1]`.
#' @slot training_n Integer. Number of labelled rows used to fit.
#' @slot training_class_balance Numeric. Share of `equal == 1L` in
#'   training data (used to detect single-class fits).
#' @slot training_stage_dist Named numeric or `NULL`. Per-stage share
#'   of training rows (for the M6 drift recommendation).
#' @slot na_fill Numeric. Value used to impute predictor NAs (default 0).
#' @slot class_weighted Logical. Whether class weights were applied.
#' @slot training_prob Numeric vector. Fitted probabilities on the
#'   training rows (used by `apply_filter()` for default Youden's-J
#'   threshold selection).
#' @slot training_equal Integer vector. Labels paired with
#'   `training_prob`.
#'
#' @noRd
Filter_Model <- new_class(
  "Filter_Model",
  properties = list(
    backend                = class_character,
    fit                    = class_any,
    predictors             = class_character,
    model_class            = class_character,
    training_n             = class_integer,
    training_class_balance = class_numeric,
    training_stage_dist    = class_any,
    na_fill                = class_numeric,
    class_weighted         = class_logical,
    training_prob          = class_numeric,
    training_equal         = class_integer
  )
)

#' @noRd
print.Filter_Model  <- new_external_generic("base", "print",  "x")
#' @noRd
format.Filter_Model <- new_external_generic("base", "format", "x")


#' Calibrated Matches
#'
#' @description
#' Returned by [apply_filter()] and [calibrate_matches()]. Wraps the
#' original matches table enriched with `tp_prob` and `predicted_tp`,
#' plus calibration metadata.
#'
#' @slot matches `data.table`. The original matches table with two new
#'   columns: `tp_prob` (predicted probability of being a true match)
#'   and `predicted_tp` (integer 0/1 from `tp_prob >= threshold`).
#' @slot filter_model The [`Filter_Model`] used to produce the
#'   predictions.
#' @slot threshold Numeric scalar. Decision threshold applied.
#' @slot threshold_method Character. How `threshold` was chosen
#'   (`"youden_j"`, `"user"`).
#' @slot recommendations Character. Strings from the recommendations catalog.
#'
#' @noRd
Calibrated_Matches <- new_class(
  "Calibrated_Matches",
  properties = list(
    matches          = class_any,
    filter_model     = class_any,
    threshold        = class_numeric,
    threshold_method = class_character,
    recommendations  = class_character
  )
)

#' @noRd
print.Calibrated_Matches  <- new_external_generic("base", "print",  "x")
#' @noRd
format.Calibrated_Matches <- new_external_generic("base", "format", "x")
#' @noRd
as.data.table.Calibrated_Matches <- new_external_generic(
  "data.table", "as.data.table", "x"
)
#' @noRd
as.data.frame.Calibrated_Matches <- new_external_generic(
  "base", "as.data.frame", "x"
)



# ---------------------------------------------------------------------------
# format() / print() -- Match_Features
# ---------------------------------------------------------------------------

#' @noRd
.format_match_features <- function(x) {
  lines <- character()
  push  <- function(...) lines <<- c(lines, paste0(...))

  ft <- x@features
  push("<joinery::Match_Features>")
  push(sprintf("  schema         : %s", x@schema))
  push(sprintf("  strategy_class : %s", x@strategy_class))
  push(sprintf("  n_pairs        : %d", if (is.null(ft)) 0L else nrow(ft)))
  push(sprintf("  n_features     : %d", if (is.null(ft)) 0L else ncol(ft)))

  if (length(x@columns) > 0L) {
    push(sprintf("  strategy cols  : %s", paste(x@columns, collapse = ", ")))
  }
  if (length(x@top_n) > 0L) {
    push(sprintf(
      "  top_n          : %s",
      paste(sprintf("%s=%d", names(x@top_n), as.integer(x@top_n)), collapse = ", ")
    ))
  }

  if (!is.null(ft) && nrow(ft) > 0L) {
    push("")
    push("preview:")
    for (r in utils::capture.output(print(utils::head(ft, 5L)))) push("  ", r)
  }
  lines
}

method(format.Match_Features, Match_Features) <- function(x, ...) {
  .format_match_features(x)
}

method(print.Match_Features, Match_Features) <- function(x, ...) {
  ft <- x@features
  cli::cli_h1(sprintf("Match_Features ({.field %s})", x@schema))
  cli::cli_text(sprintf(
    "strategy_class: {.val %s}   n_pairs: {.val %d}   n_features: {.val %d}",
    x@strategy_class,
    if (is.null(ft)) 0L else nrow(ft),
    if (is.null(ft)) 0L else ncol(ft)
  ))
  if (length(x@columns) > 0L) {
    cli::cli_text("strategy columns: {.field {x@columns}}")
  }
  if (!is.null(ft) && nrow(ft) > 0L) {
    cli::cli_text("{.strong preview}")
    print(utils::head(ft, 5L))
  }
  invisible(x)
}

method(as.data.table.Match_Features, Match_Features) <- function(x, ...) {
  if (is.null(x@features)) {
    return(data.table::data.table())
  }
  data.table::copy(x@features)
}

method(as.data.frame.Match_Features, Match_Features) <- function(x, ...) {
  if (is.null(x@features)) return(as.data.frame(data.table::data.table()))
  as.data.frame(data.table::copy(x@features))
}


# ---------------------------------------------------------------------------
# format() / print() -- Filter_Model
# ---------------------------------------------------------------------------

#' @noRd
.format_filter_model <- function(x) {
  lines <- character()
  push  <- function(...) lines <<- c(lines, paste0(...))

  push("<joinery::Filter_Model>")
  push(sprintf("  backend           : %s", x@backend))
  push(sprintf("  model_class       : %s", x@model_class))
  push(sprintf("  predictors (%d)   : %s",
               length(x@predictors),
               paste(utils::head(x@predictors, 8L),
                     collapse = ", ")))
  if (length(x@predictors) > 8L) {
    push(sprintf("                       ... +%d more",
                 length(x@predictors) - 8L))
  }
  push(sprintf("  training_n        : %d", x@training_n))
  push(sprintf("  class_balance     : %.3f (share of equal == 1L)",
               x@training_class_balance))
  push(sprintf("  class_weighted    : %s", x@class_weighted))
  if (!is.null(x@training_stage_dist) && length(x@training_stage_dist) > 0L) {
    sd <- x@training_stage_dist
    push("  training_stage_dist:")
    for (nm in names(sd)) {
      push(sprintf("    %s: %.3f", nm, sd[[nm]]))
    }
  }
  lines
}

method(format.Filter_Model, Filter_Model) <- function(x, ...) {
  .format_filter_model(x)
}
method(print.Filter_Model, Filter_Model) <- function(x, ...) {
  for (ln in format(x)) cli::cli_text(ln)
  invisible(x)
}


# ---------------------------------------------------------------------------
# format() / print() / coerce -- Calibrated_Matches
# ---------------------------------------------------------------------------

#' @noRd
.format_calibrated_matches <- function(x) {
  lines <- character()
  push  <- function(...) lines <<- c(lines, paste0(...))

  ft <- x@matches
  push("<joinery::Calibrated_Matches>")
  push(sprintf("  threshold        : %.4f  (method: %s)",
               x@threshold, x@threshold_method))
  push(sprintf("  n_rows           : %d",
               if (is.null(ft)) 0L else nrow(ft)))
  if (!is.null(ft) && "predicted_tp" %in% names(ft)) {
    n_kept <- sum(ft$predicted_tp == 1L, na.rm = TRUE)
    n_drop <- sum(ft$predicted_tp == 0L, na.rm = TRUE)
    push(sprintf("  predicted_tp == 1: %d", n_kept))
    push(sprintf("  predicted_tp == 0: %d", n_drop))
  }
  if (!is.null(ft) && "tp_prob" %in% names(ft)) {
    pr <- stats::na.omit(ft$tp_prob)
    if (length(pr) > 0L) {
      qs <- stats::quantile(pr, c(0, .25, .5, .75, 1), names = FALSE)
      push(sprintf(
        "  tp_prob quantiles: %.3f / %.3f / %.3f / %.3f / %.3f",
        qs[1], qs[2], qs[3], qs[4], qs[5]
      ))
    }
  }
  if (length(x@recommendations) > 0L) {
    push("")
    push("recommendations:")
    for (r in x@recommendations) push("  ! ", r)
  }
  lines
}

method(format.Calibrated_Matches, Calibrated_Matches) <- function(x, ...) {
  .format_calibrated_matches(x)
}
method(print.Calibrated_Matches, Calibrated_Matches) <- function(x, ...) {
  cli::cli_h1("Calibrated_Matches")
  for (ln in format(x)) cli::cli_text(ln)
  for (r in x@recommendations) cli::cli_alert_warning(r)
  invisible(x)
}

method(
  as.data.table.Calibrated_Matches,
  Calibrated_Matches
) <- function(x, ...) {
  if (is.null(x@matches)) return(data.table::data.table())
  data.table::copy(x@matches)
}

method(
  as.data.frame.Calibrated_Matches,
  Calibrated_Matches
) <- function(x, ...) {
  if (is.null(x@matches)) return(as.data.frame(data.table::data.table()))
  as.data.frame(data.table::copy(x@matches))
}



# ---------------------------------------------------------------------------
# Filter_Calibration
# ---------------------------------------------------------------------------

#' Filter Calibration Result
#'
#' @description
#' Returned by [calibrate()]. Quality / calibration diagnostics for a
#' fitted [`Filter_Model`] evaluated either on its training fold or on
#' an independently labelled evaluation set.
#'
#' @slot reliability `data.table`. Bin-wise mean predicted probability vs.
#'   observed positive rate (Hosmer-Lemeshow style reliability table).
#' @slot brier Numeric scalar. Mean squared error between `tp_prob` and
#'   `equal`.
#' @slot log_loss Numeric scalar. Binary cross-entropy.
#' @slot confusion_per_class `data.table`. Confusion matrix at the applied
#'   threshold (rows: true `equal`, columns: `predicted_tp`).
#' @slot threshold_curve `data.table`. Threshold sweep: tpr / fpr /
#'   precision / recall / f1 / youden_j across a grid.
#' @slot threshold Numeric scalar. The decision threshold used for the
#'   confusion matrix and headline metrics.
#' @slot n_eval Integer. Number of labelled rows scored.
#' @slot class_balance Numeric. Share of `equal == 1L` in the evaluation set.
#' @slot recommendations Character. Strings from the recommendations catalog.
#'
#' @noRd
Filter_Calibration <- new_class(
  "Filter_Calibration",
  properties = list(
    reliability         = class_any,
    brier               = class_numeric,
    log_loss            = class_numeric,
    confusion_per_class = class_any,
    threshold_curve     = class_any,
    threshold           = class_numeric,
    n_eval              = class_integer,
    class_balance       = class_numeric,
    recommendations     = class_character
  )
)

#' @noRd
print.Filter_Calibration <- new_external_generic("base", "print", "x")
#' @noRd
format.Filter_Calibration <- new_external_generic("base", "format", "x")
#' @noRd
as.data.table.Filter_Calibration <- new_external_generic(
  "data.table", "as.data.table", "x"
)
#' @noRd
as.data.frame.Filter_Calibration <- new_external_generic(
  "base", "as.data.frame", "x"
)

#' @noRd
.format_filter_calibration <- function(x) {
  lines <- character()
  push  <- function(...) lines <<- c(lines, paste0(...))

  push("<joinery::Filter_Calibration>")
  push(sprintf("  n_eval         : %d", x@n_eval))
  push(sprintf("  class_balance  : %.3f (share of equal == 1L)", x@class_balance))
  push(sprintf("  threshold      : %.4f", x@threshold))
  push(sprintf("  brier          : %.4f", x@brier))
  push(sprintf("  log_loss       : %.4f", x@log_loss))

  cm <- x@confusion_per_class
  if (!is.null(cm) && nrow(cm) > 0L) {
    push("")
    push("  confusion (rows=true equal, cols=predicted_tp):")
    for (i in seq_len(nrow(cm))) {
      push(sprintf(
        "    equal=%d  pred=0: %d   pred=1: %d",
        cm$equal[i], cm$n_pred0[i], cm$n_pred1[i]
      ))
    }
  }

  rel <- x@reliability
  if (!is.null(rel) && nrow(rel) > 0L) {
    push("")
    push(sprintf("  reliability (showing %d of %d bins):",
                 min(nrow(rel), 5L), nrow(rel)))
    for (i in seq_len(min(nrow(rel), 5L))) {
      push(sprintf(
        "    bin %d  n=%d   mean_pred=%.3f   obs_pos=%.3f",
        rel$bin[i], rel$n[i], rel$mean_pred[i], rel$obs_pos[i]
      ))
    }
  }

  lines
}

method(format.Filter_Calibration, Filter_Calibration) <- function(x, ...) {
  .format_filter_calibration(x)
}
method(print.Filter_Calibration, Filter_Calibration) <- function(x, ...) {
  cli::cli_h1("Filter_Calibration")
  cli::cli_text(sprintf("n_eval: {.val %d}   class_balance: {.val %s}",
                        x@n_eval,
                        sprintf("%.3f", x@class_balance)))
  cli::cli_text(sprintf("threshold: {.val %s}   brier: {.val %s}   log_loss: {.val %s}",
                        sprintf("%.4f", x@threshold),
                        sprintf("%.4f", x@brier),
                        sprintf("%.4f", x@log_loss)))
  cm <- x@confusion_per_class
  if (!is.null(cm) && nrow(cm) > 0L) {
    cli::cli_text("{.strong confusion}")
    for (i in seq_len(nrow(cm))) {
      cli::cli_bullets(sprintf(
        "equal=%d  pred=0: %d   pred=1: %d",
        cm$equal[i], cm$n_pred0[i], cm$n_pred1[i]
      ))
    }
  }
  for (r in x@recommendations) cli::cli_alert_warning(r)
  invisible(x)
}

method(as.data.table.Filter_Calibration, Filter_Calibration) <- function(x, ...) {
  data.table::data.table(
    n_eval            = x@n_eval,
    class_balance     = x@class_balance,
    threshold         = x@threshold,
    brier             = x@brier,
    log_loss          = x@log_loss,
    n_recommendations = length(x@recommendations)
  )
}
method(as.data.frame.Filter_Calibration, Filter_Calibration) <- function(x, ...) {
  as.data.frame(as.data.table.Filter_Calibration(x))
}


# `%||%` is imported from rlang via the package-level `@import` directive
# in R/joinery-package.R.


# ---------------------------------------------------------------------------
# recommendations() methods for calibration classes
# (generic is declared in generics_diagnostic.R)
# ---------------------------------------------------------------------------
method(recommendations, Calibrated_Matches) <- function(x) x@recommendations
method(recommendations, Filter_Calibration) <- function(x) x@recommendations
