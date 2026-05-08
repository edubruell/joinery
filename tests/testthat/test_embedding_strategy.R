skip_if_not_installed("tidyllm")

# A throwaway provider object — never used because we don't call embed() here.
fake_model <- structure(list(model = "fake"), class = "ollama_model")


# ── Constructor: required arguments ───────────────────────────────────────────

test_that("embedding_strategy() errors when embedding_model is missing", {
  expect_error(
    embedding_strategy(threshold = 0.5),
    "embedding_model is required"
  )
})

test_that("embedding_strategy() errors when threshold is missing", {
  expect_error(
    embedding_strategy(embedding_model = fake_model),
    "threshold is required"
  )
})


# ── Constructor: columns normalisation ────────────────────────────────────────

test_that("embedding_strategy() coerces NULL columns to character(0)", {
  s <- embedding_strategy(embedding_model = fake_model, threshold = 0.5)
  expect_identical(s@columns, character(0))
})

test_that("embedding_strategy() preserves explicit columns", {
  s <- embedding_strategy(
    columns = c("name", "city"),
    embedding_model = fake_model,
    threshold = 0.5
  )
  expect_identical(s@columns, c("name", "city"))
})


# ── Validator: threshold ──────────────────────────────────────────────────────

test_that("embedding_strategy() rejects non-scalar threshold", {
  expect_error(
    embedding_strategy(embedding_model = fake_model, threshold = c(0.5, 0.6)),
    "threshold must be a scalar"
  )
})

test_that("embedding_strategy() rejects non-finite threshold", {
  expect_error(
    embedding_strategy(embedding_model = fake_model, threshold = NA_real_),
    "threshold must be finite"
  )
  expect_error(
    embedding_strategy(embedding_model = fake_model, threshold = Inf),
    "threshold must be finite"
  )
})

test_that("embedding_strategy() rejects out-of-range threshold", {
  expect_error(
    embedding_strategy(embedding_model = fake_model, threshold = -0.1),
    "threshold must be in"
  )
  expect_error(
    embedding_strategy(embedding_model = fake_model, threshold = 1.1),
    "threshold must be in"
  )
})

test_that("embedding_strategy() accepts threshold at boundaries", {
  expect_no_error(embedding_strategy(embedding_model = fake_model, threshold = 0))
  expect_no_error(embedding_strategy(embedding_model = fake_model, threshold = 1))
})


# ── Validator: collapse_sep, normalize, batch_size ────────────────────────────

test_that("embedding_strategy() rejects non-scalar collapse_sep", {
  expect_error(
    embedding_strategy(
      embedding_model = fake_model, threshold = 0.5,
      collapse_sep = c(" ", "_")
    ),
    "collapse_sep must be a scalar"
  )
})

test_that("embedding_strategy() rejects non-scalar normalize", {
  expect_error(
    embedding_strategy(
      embedding_model = fake_model, threshold = 0.5,
      normalize = c(TRUE, FALSE)
    ),
    "normalize must be a scalar logical"
  )
})

test_that("embedding_strategy() rejects non-scalar batch_size", {
  expect_error(
    embedding_strategy(
      embedding_model = fake_model, threshold = 0.5,
      batch_size = c(100, 200)
    ),
    "batch_size must be a scalar"
  )
})

test_that("embedding_strategy() rejects non-positive batch_size", {
  expect_error(
    embedding_strategy(
      embedding_model = fake_model, threshold = 0.5,
      batch_size = 0
    ),
    "batch_size must be a positive finite number"
  )
  expect_error(
    embedding_strategy(
      embedding_model = fake_model, threshold = 0.5,
      batch_size = -10
    ),
    "batch_size must be a positive finite number"
  )
})

test_that("embedding_strategy() rejects non-finite batch_size", {
  expect_error(
    embedding_strategy(
      embedding_model = fake_model, threshold = 0.5,
      batch_size = Inf
    ),
    "batch_size must be a positive finite number"
  )
})


# ── Defaults ──────────────────────────────────────────────────────────────────

test_that("embedding_strategy() applies expected defaults", {
  s <- embedding_strategy(embedding_model = fake_model, threshold = 0.7)
  expect_identical(s@collapse_sep, " ")
  expect_true(s@normalize)
  expect_identical(s@batch_size, 1000)
  expect_null(s@block_by)
})

test_that("embedding_strategy() preserves block_by", {
  s <- embedding_strategy(
    embedding_model = fake_model,
    threshold = 0.5,
    block_by = c("region", "year")
  )
  expect_identical(s@block_by, c("region", "year"))
})


# ── Print method ──────────────────────────────────────────────────────────────

# cli::cli_text writes via the message channel, not stdout. Capture via
# testthat::capture_messages() and strip box-drawing chars for matching.
capture_print <- function(x) {
  msgs <- testthat::capture_messages(print(x))
  joined <- paste(msgs, collapse = "")
  # Strip cli's leading box-drawing prefix and ANSI noise.
  gsub("│", "", joined, fixed = TRUE)
}

test_that("print() on Embedding_Strategy emits expected fields", {
  s <- embedding_strategy(
    columns = c("name", "city"),
    embedding_model = fake_model,
    threshold = 0.85,
    block_by = "region"
  )
  joined <- capture_print(s)

  expect_match(joined, "Embedding_Strategy")
  expect_match(joined, "name, city")
  expect_match(joined, "0\\.85")
  expect_match(joined, "region")
})

test_that("print() on Embedding_Strategy shows 'all' when columns empty", {
  s <- embedding_strategy(embedding_model = fake_model, threshold = 0.5)
  expect_match(capture_print(s), "all")
})

test_that("print() on Embedding_Strategy shows 'none' when block_by NULL", {
  s <- embedding_strategy(embedding_model = fake_model, threshold = 0.5)
  expect_match(capture_print(s), "none")
})
