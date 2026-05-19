# Tests for audit_strategy() on Embedding_Strategy (Phase 0.6 M8).
#
# Same fixture-convention as test_audit_strategy.R: one clean fixture
# (zero recommendations) + one trigger fixture per recommendation rule.

skip_if_not_installed("tidyllm")
skip_if_not_installed("tibble")

library(data.table)


# ---------------------------------------------------------------------------
# Fixture builders
# ---------------------------------------------------------------------------

# Deterministic L2-unit embeddings: rotate through `dim` orthogonal basis
# vectors. With normalize=TRUE on the strategy these stay unit-norm; the
# call counter ensures successive calls return different vectors.
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

# Embedder that returns vectors of varying norm. Used to trigger the
# `unnormalised_embeddings` recommendation when `normalize = FALSE`.
fake_embed_varied <- function(dim = 4L) {
  function(text, model) {
    vecs <- lapply(seq_along(text), function(k) {
      # norm cycles through {0.5, 1.0, 1.5, 2.0, 2.5}
      norm  <- 0.5 + (k %% 5L) * 0.5
      v     <- rep(0, dim); v[((k - 1L) %% dim) + 1L] <- norm
      v
    })
    tibble::tibble(input = text, embeddings = vecs)
  }
}

make_emb_strategy <- function(...) {
  args <- utils::modifyList(
    list(
      columns         = "name",
      embedding_model = NULL,
      threshold       = 0.8,
      collapse_sep    = " ",
      normalize       = TRUE,
      batch_size      = 1000L,
      block_by        = NULL
    ),
    list(...)
  )
  do.call(Embedding_Strategy, args)
}

# 20 unique records, all with non-empty text, no blocking.
make_clean_data <- function() {
  data.table::data.table(
    id   = paste0("r", 1:20),
    name = paste0("name_", letters[1:20])
  )
}

# Same data with a balanced block column (10 records per region).
make_blocked_data <- function() {
  dt <- make_clean_data()
  dt[, region := rep(c("north", "south"), each = 10L)]
  dt
}

# 20 records, but 5 have NA name → coverage = 15/20 = 0.75 < 0.90.
make_low_coverage_data <- function() {
  dt <- make_clean_data()
  dt$name[1:5] <- NA_character_
  dt
}

# Imbalanced blocking: 16 records in "majority", 4 in "minority".
# top1_share = 0.80 > 0.70 → fires block_imbalanced.
make_imbalanced_block_data <- function() {
  dt <- make_clean_data()
  dt[, region := c(rep("majority", 16L), rep("minority", 4L))]
  dt
}


# ---------------------------------------------------------------------------
# 1. Return type and basic slots
# ---------------------------------------------------------------------------

test_that("audit_strategy(Embedding_Strategy) returns Embedding_Audit", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_strategy(make_clean_data(), "id", make_emb_strategy())
  expect_true(S7::S7_inherits(res, Embedding_Audit))
  expect_identical(res@n_records, 20L)
  expect_identical(res@n_embedded, 20L)
  expect_equal(res@coverage_rate, 1.0)
})

test_that("clean fixture fires zero recommendations", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_strategy(make_clean_data(), "id", make_emb_strategy())
  expect_equal(length(res@recommendations), 0L)
})


# ---------------------------------------------------------------------------
# 2. Norm summary
# ---------------------------------------------------------------------------

test_that("norm_summary is unit-length when strategy@normalize=TRUE", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_strategy(make_clean_data(), "id", make_emb_strategy())
  ns <- res@norm_summary
  expect_named(ns$quantiles, c("p05", "p25", "p50", "p75", "p95"))
  expect_equal(ns$median, 1.0)
  expect_equal(ns$iqr, 0)
})


# ---------------------------------------------------------------------------
# 3. Similarity sample
# ---------------------------------------------------------------------------

test_that("similarity_sample is a data.table of pair similarities", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_strategy(make_clean_data(), "id", make_emb_strategy(),
                        similarity_n_pairs = 50L)
  ss <- res@similarity_sample
  expect_s3_class(ss, "data.table")
  expect_named(ss, c("base_id", "target_id", "similarity"))
  expect_lte(nrow(ss), 50L)
  expect_true(all(ss$similarity >= -1 - 1e-9 & ss$similarity <= 1 + 1e-9))
})

test_that("similarity_sample is NULL when fewer than 2 embeddable records", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  one_row <- data.table::data.table(id = "a", name = "alpha")
  res <- audit_strategy(one_row, "id", make_emb_strategy())
  expect_null(res@similarity_sample)
})


# ---------------------------------------------------------------------------
# 4. Recommendations -- low_embedding_coverage
# ---------------------------------------------------------------------------

test_that("low_embedding_coverage fires when many records have NA text", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_strategy(make_low_coverage_data(), "id", make_emb_strategy())
  expect_equal(res@n_embedded, 15L)
  expect_equal(res@coverage_rate, 0.75)
  expect_true("low_embedding_coverage" %in% attr(res, "recommendation_ids"))
})


# ---------------------------------------------------------------------------
# 5. Recommendations -- unnormalised_embeddings
# ---------------------------------------------------------------------------

test_that("unnormalised_embeddings fires when norm IQR is large", {
  local_mocked_bindings(embed = fake_embed_varied(4L), .package = "tidyllm")
  res <- audit_strategy(
    make_clean_data(), "id",
    make_emb_strategy(normalize = FALSE)
  )
  expect_gt(res@norm_summary$iqr, 0.10)
  expect_true("unnormalised_embeddings" %in% attr(res, "recommendation_ids"))
})

test_that("unnormalised_embeddings does NOT fire when normalize=TRUE", {
  local_mocked_bindings(embed = fake_embed_varied(4L), .package = "tidyllm")
  res <- audit_strategy(
    make_clean_data(), "id",
    make_emb_strategy(normalize = TRUE)
  )
  expect_false("unnormalised_embeddings" %in% attr(res, "recommendation_ids"))
})


# ---------------------------------------------------------------------------
# 6. Block summary + recommendation -- block_imbalanced
# ---------------------------------------------------------------------------

test_that("block_summary is computed when block_by is set", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_strategy(
    make_blocked_data(), "id",
    make_emb_strategy(block_by = "region")
  )
  expect_false(is.null(res@block_summary))
  expect_equal(res@block_summary$summary$n_blocks, 2L)
  expect_equal(res@block_summary$summary$top1_share, 0.5)
  # est_comparisons = 2 * choose(10, 2) = 90
  expect_equal(res@est_comparisons, 90)
})

test_that("block_imbalanced fires on majority/minority blocks", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_strategy(
    make_imbalanced_block_data(), "id",
    make_emb_strategy(block_by = "region")
  )
  expect_true("block_imbalanced" %in% attr(res, "recommendation_ids"))
})

test_that("est_comparisons defaults to all pairs without blocking", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_strategy(make_clean_data(), "id", make_emb_strategy())
  expect_equal(res@est_comparisons, 20 * 19 / 2)
})


# ---------------------------------------------------------------------------
# 7. Coercion
# ---------------------------------------------------------------------------

test_that("as.data.table.Embedding_Audit returns a single-row summary", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_strategy(make_blocked_data(), "id",
                        make_emb_strategy(block_by = "region"))
  dt <- as.data.table(res)
  expect_s3_class(dt, "data.table")
  expect_equal(nrow(dt), 1L)
  expect_true(all(c("n_records", "n_embedded", "coverage_rate",
                    "norm_median", "norm_iqr",
                    "similarity_median", "similarity_n_pairs",
                    "est_comparisons", "n_blocks", "block_top1_share",
                    "n_recommendations") %in% names(dt)))
  expect_equal(dt$n_records, 20L)
})

test_that("as.data.frame.Embedding_Audit returns a plain data.frame", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_strategy(make_clean_data(), "id", make_emb_strategy())
  df <- as.data.frame(res)
  expect_s3_class(df, "data.frame")
  expect_false(inherits(df, "data.table"))
})


# ---------------------------------------------------------------------------
# 8. format() / print()
# ---------------------------------------------------------------------------

test_that("format.Embedding_Audit produces a character vector", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_strategy(make_clean_data(), "id", make_emb_strategy())
  fmt <- format(res)
  expect_type(fmt, "character")
  expect_true(any(grepl("Embedding_Audit", fmt)))
  expect_true(any(grepl("coverage_rate", fmt)))
})

test_that("print.Embedding_Audit returns invisible(x) without error", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_strategy(make_clean_data(), "id", make_emb_strategy())
  expect_invisible(print(res))
})


# ---------------------------------------------------------------------------
# 9. Backend parity -- tibble / data.frame
# ---------------------------------------------------------------------------

test_that("tibble path returns identical Embedding_Audit (block_by=NULL)", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  ref <- audit_strategy(make_clean_data(), "id", make_emb_strategy())

  tib <- tibble::as_tibble(make_clean_data())
  res <- audit_strategy(tib, "id", make_emb_strategy())

  expect_true(S7::S7_inherits(res, Embedding_Audit))
  expect_identical(res@n_records, ref@n_records)
  expect_identical(res@n_embedded, ref@n_embedded)
  expect_equal(res@coverage_rate, ref@coverage_rate)
})

test_that("data.frame path returns identical Embedding_Audit", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  ref <- audit_strategy(make_clean_data(), "id", make_emb_strategy())

  df  <- as.data.frame(make_clean_data())
  res <- audit_strategy(df, "id", make_emb_strategy())

  expect_true(S7::S7_inherits(res, Embedding_Audit))
  expect_identical(res@n_records, ref@n_records)
})


# ---------------------------------------------------------------------------
# 10. Backend parity -- DuckDB
# ---------------------------------------------------------------------------

test_that("DuckDB path returns identical Embedding_Audit", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  df <- as.data.frame(make_clean_data())
  DBI::dbWriteTable(con, "t1", df)
  duck <- dplyr::tbl(con, "t1")

  res <- audit_strategy(duck, "id", make_emb_strategy(), sample_n = 20L)
  expect_true(S7::S7_inherits(res, Embedding_Audit))
  expect_equal(res@n_records, 20L)
})


# ---------------------------------------------------------------------------
# 11. sample_n parameter
# ---------------------------------------------------------------------------

test_that("sample_n caps n_records to the sampled size", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_strategy(make_clean_data(), "id", make_emb_strategy(),
                        sample_n = 10L)
  expect_equal(res@n_records, 10L)
  expect_equal(res@n_embedded, 10L)
})


# ---------------------------------------------------------------------------
# 12. Branch coverage in .compute_similarity_sample
# ---------------------------------------------------------------------------

test_that("enumerate-all-pairs branch fires when n_pairs >= max_pairs", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  small <- data.table::data.table(
    id = c("a", "b", "c"), name = c("x", "y", "z")
  )
  # n=3 → max_pairs = 3; request 100 → enumerate branch
  res <- audit_strategy(small, "id", make_emb_strategy(),
                        similarity_n_pairs = 100L)
  expect_equal(nrow(res@similarity_sample), 3L)
})

test_that("rejection-sampling branch caps to requested n_pairs", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  # n=20 → max_pairs = 190; request 10 → rejection branch
  res <- audit_strategy(make_clean_data(), "id", make_emb_strategy(),
                        similarity_n_pairs = 10L)
  expect_equal(nrow(res@similarity_sample), 10L)
})


# ---------------------------------------------------------------------------
# 13. Validation errors
# ---------------------------------------------------------------------------

test_that("audit_strategy errors when id column is missing", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  expect_error(
    audit_strategy(make_clean_data(), "no_such_id", make_emb_strategy()),
    "ID column"
  )
})

test_that("audit_strategy errors when block_by column is missing", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  expect_error(
    audit_strategy(make_clean_data(), "id",
                   make_emb_strategy(block_by = "no_such_col")),
    "Blocking columns"
  )
})


# ---------------------------------------------------------------------------
# 14. Threshold attribute round-trip
# ---------------------------------------------------------------------------

test_that("strategy threshold is stashed on the audit object", {
  local_mocked_bindings(embed = fake_embed_basis(8L), .package = "tidyllm")
  res <- audit_strategy(make_clean_data(), "id",
                        make_emb_strategy(threshold = 0.77))
  expect_equal(attr(res, "threshold"), 0.77)
})
