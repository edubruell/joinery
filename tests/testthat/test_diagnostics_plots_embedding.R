# Smoke tests for Embedding_Audit plot functions.
#
# No image snapshots: we verify the functions run without error and return
# the expected invisible data.table.

skip_if_not_installed("tidyllm")
skip_if_not_installed("tibble")
skip_if_not_installed("tinyplot")

library(data.table)


# ---------------------------------------------------------------------------
# Fixture
# ---------------------------------------------------------------------------

fake_embed_basis <- function(dim = 8L) {
  i <- 0L
  function(text, model) {
    vecs <- lapply(seq_along(text), function(k) {
      v <- rep(0, dim); v[((i + k - 1L) %% dim) + 1L] <- 1.0; v
    })
    i <<- i + length(text)
    tibble::tibble(input = text, embeddings = vecs)
  }
}

make_emb_strategy <- function(...) {
  args <- utils::modifyList(
    list(columns = "name", embedding_model = NULL, threshold = 0.5,
         collapse_sep = " ", normalize = TRUE, batch_size = 1000L,
         block_by = NULL),
    list(...)
  )
  do.call(Embedding_Strategy, args)
}

make_data <- function() {
  data.table::data.table(
    id   = paste0("r", 1:20),
    name = paste0("name_", letters[1:20]),
    region = rep(c("north", "south"), each = 10L)
  )
}

audit_fixture <- function(block_by = NULL) {
  audit_strategy(
    make_data(), "id",
    make_emb_strategy(block_by = block_by),
    similarity_n_pairs = 30L
  )
}


# ---------------------------------------------------------------------------
# similarity_histogram
# ---------------------------------------------------------------------------

test_that("similarity_histogram() runs and returns histogram data.table", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_fixture()
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  dt <- similarity_histogram(res)
  expect_s3_class(dt, "data.table")
  expect_true(all(c("bin_lower", "bin_upper", "bin_mid", "count") %in% names(dt)))
  expect_gt(sum(dt$count), 0L)
})

test_that("similarity_histogram() errors when similarity_sample is NULL", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  one_row <- data.table::data.table(id = "a", name = "alpha")
  res <- audit_strategy(one_row, "id", make_emb_strategy())
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(similarity_histogram(res), "similarity_sample")
})


# ---------------------------------------------------------------------------
# norm_plot
# ---------------------------------------------------------------------------

test_that("norm_plot() runs and returns quantile data.table", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_fixture()
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  dt <- norm_plot(res)
  expect_s3_class(dt, "data.table")
  expect_named(dt, c("quantile", "norm"))
  expect_equal(nrow(dt), 5L)
})


# ---------------------------------------------------------------------------
# block_size_plot (reused from token path)
# ---------------------------------------------------------------------------

test_that("block_size_plot() works on Embedding_Audit when blocking is set", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_fixture(block_by = "region")
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  dt <- block_size_plot(res)
  expect_s3_class(dt, "data.table")
  expect_true("n_records" %in% names(dt))
})

test_that("block_size_plot() errors when no blocking was set", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_fixture()
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(block_size_plot(res), "block_by")
})


# ---------------------------------------------------------------------------
# Default plot.Embedding_Audit -> similarity_histogram
# ---------------------------------------------------------------------------

test_that("plot(Embedding_Audit) dispatches to similarity_histogram", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_fixture()
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  dt <- plot(res)
  expect_s3_class(dt, "data.table")
  expect_true("bin_mid" %in% names(dt))
})


# ---------------------------------------------------------------------------
# Error paths constructed via direct class instantiation
# ---------------------------------------------------------------------------

test_that("similarity_histogram errors when all similarities are non-finite", {
  audit <- Embedding_Audit(
    n_records         = 2L,
    n_embedded        = 2L,
    coverage_rate     = 1.0,
    norm_summary      = list(
      quantiles = stats::setNames(rep(1, 5L),
                                  c("p05", "p25", "p50", "p75", "p95")),
      median = 1.0, iqr = 0.0
    ),
    similarity_sample = data.table::data.table(
      base_id = "a", target_id = "b", similarity = NA_real_
    ),
    block_summary     = NULL,
    est_comparisons   = 1,
    recommendations   = character(0)
  )
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(similarity_histogram(audit), "non-finite")
})

test_that("norm_plot errors when norm_summary is all-NA", {
  na_q <- stats::setNames(rep(NA_real_, 5L),
                          c("p05", "p25", "p50", "p75", "p95"))
  audit <- Embedding_Audit(
    n_records         = 0L,
    n_embedded        = 0L,
    coverage_rate     = NA_real_,
    norm_summary      = list(quantiles = na_q, median = NA_real_, iqr = NA_real_),
    similarity_sample = NULL,
    block_summary     = NULL,
    est_comparisons   = NA_real_,
    recommendations   = character(0)
  )
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(norm_plot(audit), "all-NA")
})
