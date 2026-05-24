# ============================================================
# fit_filter() — tidymodels path
# ============================================================
#
# Optional tidymodels backend for `fit_filter()`. Triggered when
# `model =` is a parsnip model spec, a fitted parsnip object, or
# a (fitted or unfitted) `workflow`. Lives in its own file so the
# tidymodels dependency boundary (Suggests-only) is visible at the
# directory level. The dispatcher `.fit_filter_impl()` lives in
# `calibration_filter.R`.
# ============================================================


.fit_filter_tidymodels <- function(features, labels,
                                   model, class_weighted, na_fill, ...) {
  is_parsnip_spec <- inherits(model, c("model_spec"))
  is_parsnip_fit  <- inherits(model, c("model_fit"))
  is_workflow_obj <- inherits(model, "workflow")
  is_workflow_fit <- is_workflow_obj && isTRUE(tryCatch(
    workflows::is_trained_workflow(model),
    error = function(e) FALSE
  ))

  if (!(is_parsnip_spec || is_parsnip_fit || is_workflow_obj)) {
    cli::cli_abort(c(
      "{.arg model} must be {.val \"logistic\"}, a parsnip model spec, a fitted parsnip model, or a (fitted or unfitted) workflow"
    ))
  }
  if (!requireNamespace("parsnip", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.fn fit_filter} with tidymodels requires the {.pkg parsnip} package",
      "i" = "Install it via {.run install.packages(\"parsnip\")}"
    ))
  }

  ft  <- data.table::as.data.table(features@features)
  lab <- .collapse_pair_labels(labels)
  ft[, searched := as.character(searched)]
  ft[, found    := as.character(found)]
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
  joined <- merge(ft, lab, by = c("match_id", "found"),
                  all.x = FALSE, all.y = FALSE, sort = FALSE)
  if (nrow(joined) == 0L) {
    cli::cli_abort(c(
      "No features rows matched any label row on (match_id, found)",
      "i" = "Make sure {.arg labels} came from the same matches table"
    ))
  }

  predictors <- .feature_predictors(joined)
  X <- .build_design(joined, predictors, na_fill = na_fill)
  y <- factor(as.integer(joined$equal), levels = c(0L, 1L))
  df <- as.data.frame(X)
  df$.y <- y

  fml <- stats::as.formula(
    paste(".y ~", paste(predictors, collapse = " + "))
  )

  fit <- if (is_parsnip_fit) {
    model
  } else if (is_workflow_fit) {
    model
  } else if (is_workflow_obj) {
    if (!requireNamespace("workflows", quietly = TRUE)) {
      cli::cli_abort(c(
        "Workflow fitting requires the {.pkg workflows} package",
        "i" = "Install it via {.run install.packages(\"workflows\")}"
      ))
    }
    generics::fit(model, data = df)
  } else {
    parsnip::fit(model, formula = fml, data = df)
  }

  backend <- if (inherits(fit, "workflow")) "workflow" else "parsnip"

  training_prob <- {
    pr <- stats::predict(fit, new_data = df, type = "prob")
    if (!".pred_1" %in% names(pr)) {
      cli::cli_abort(c(
        "{.fn parsnip::predict}() did not return a {.field .pred_1} column",
        "i" = "{.cls Filter_Model} was not trained with the joinery convention"
      ))
    }
    as.numeric(pr[[".pred_1"]])
  }

  stage_dist <- if ("stage" %in% names(joined) &&
                    !all(is.na(joined$stage))) {
    tbl <- table(joined$stage, useNA = "no")
    sd  <- as.numeric(tbl) / sum(tbl)
    names(sd) <- names(tbl)
    sd
  } else NULL

  Filter_Model(
    backend                = backend,
    fit                    = fit,
    predictors             = predictors,
    model_class            = class(fit)[1L],
    training_n             = nrow(joined),
    training_class_balance = mean(as.integer(as.character(y)) == 1L),
    training_stage_dist    = stage_dist,
    na_fill                = as.numeric(na_fill),
    class_weighted         = isTRUE(class_weighted),
    training_prob          = training_prob,
    training_equal         = as.integer(as.character(y))
  )
}
