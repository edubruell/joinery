# ============================================================
# joinery_recipe() — tidymodels recipe builder
# ============================================================
#
# Optional builder for a `recipes::recipe` over the
# `Match_Features` schema, used by the tidymodels path of
# `fit_filter()`. Lives in its own file so the recipes/parsnip
# dependency boundary is visible at the directory level.
# ============================================================


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


#' Build a tidymodels recipe for calibration features
#'
#' @description
#' Construct a pre-configured [recipes::recipe()] suitable for fitting a
#' false-positive filter on the output of [match_features()]. Tags ID
#' columns (`searched`, `found`, `match_id`) with role `"id"`, sets
#' `equal` as the outcome, and keeps every other numeric column as a
#' predictor. Requires the suggested `recipes` package.
#'
#' @param features A [`Match_Features`] object.
#' @param labels A labels `data.table` with `equal` (as for
#'   [fit_filter()]).
#' @param ... Reserved for future expansion.
#'
#' @return A [recipes::recipe()] object.
#'
#' @export
joinery_recipe <- function(features, labels, ...) {
  .joinery_recipe_impl(features, labels, ...)
}
