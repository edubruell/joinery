# ============================================================
# Tests: tidymodels shim
# ============================================================
#
# Tests skip (not fail) when tidymodels packages are absent.
# ============================================================

library(data.table)


make_dedup_features_and_labels <- function() {
  base <- data.table(
    id   = c("a", "b", "c", "d", "e", "f"),
    name = c("john smith", "jon smith", "jane doe",
             "john doe",   "alice cooper", "alyce cooper")
  )
  s <- search_strategy(name ~ word_tokens(), threshold = 0.2)
  dups <- detect_duplicates(base, "id", s)
  mf   <- match_features(dups, s, base = base, id = "id")

  labels <- copy(dups)
  labels[, equal := NA_integer_]
  labels[rank == 1L, equal := 1L]
  labels[id == "c", equal := 0L]
  labels[is.na(equal), equal := 1L]

  list(mf = mf, labels = labels)
}


test_that("joinery_recipe() returns a recipes::recipe object", {
  skip_if_not_installed("recipes")
  bits <- make_dedup_features_and_labels()
  rec  <- joinery_recipe(bits$mf, bits$labels)
  expect_s3_class(rec, "recipe")
})

test_that("joinery_recipe() assigns id role to id columns", {
  skip_if_not_installed("recipes")
  bits <- make_dedup_features_and_labels()
  rec  <- joinery_recipe(bits$mf, bits$labels)
  info <- summary(rec)
  id_roles <- info$role[info$variable %in% c("searched", "found", "match_id")]
  expect_true(all(id_roles == "id"))
  outcome_var <- info$variable[info$role == "outcome"]
  expect_equal(outcome_var, "equal")
})

test_that("joinery_recipe() errors clearly without `recipes` installed", {
  # Only exercise this branch when recipes is genuinely absent.
  if (requireNamespace("recipes", quietly = TRUE)) skip("recipes installed")
  bits <- make_dedup_features_and_labels()
  expect_error(joinery_recipe(bits$mf, bits$labels), "recipes")
})


test_that("fit_filter() accepts a parsnip logistic_reg spec", {
  skip_if_not_installed("parsnip")
  bits <- make_dedup_features_and_labels()
  spec <- parsnip::set_engine(parsnip::logistic_reg(), "glm")
  fm <- fit_filter(bits$mf, bits$labels, model = spec)
  expect_true(S7::S7_inherits(fm, joinery:::Filter_Model))
  expect_true(fm@backend %in% c("parsnip", "workflow"))
  expect_true(length(fm@training_prob) > 0L)
})


test_that("apply_filter() scores using a parsnip-backed filter model", {
  skip_if_not_installed("parsnip")
  bits <- make_dedup_features_and_labels()
  spec <- parsnip::set_engine(parsnip::logistic_reg(), "glm")
  fm   <- fit_filter(bits$mf, bits$labels, model = spec)
  cm   <- apply_filter(bits$mf, fm)
  expect_true(S7::S7_inherits(cm, joinery:::Calibrated_Matches))
  expect_true("tp_prob" %in% names(cm@matches))
  expect_true(all(cm@matches$tp_prob >= 0 & cm@matches$tp_prob <= 1))
})


test_that("fit_filter() rejects an unsupported `model` value", {
  bits <- make_dedup_features_and_labels()
  expect_error(
    fit_filter(bits$mf, bits$labels, model = "not_a_model"),
    "parsnip"
  )
})
