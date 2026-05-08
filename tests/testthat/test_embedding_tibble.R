skip_if_not_installed("tidyllm")
skip_if_not_installed("tibble")

# Tibble / data.frame embedding methods are thin wrappers that convert to
# data.table, delegate, and convert back. These tests verify dispatch works
# and the return container matches the input.


# ── Fixtures ──────────────────────────────────────────────────────────────────

fake_embed <- function(dim = 4L) {
  function(text, model) {
    vecs <- lapply(seq_along(text), function(i) {
      v <- rep(0, dim); v[((i - 1L) %% dim) + 1L] <- 1.0; v
    })
    tibble::tibble(input = text, embeddings = vecs)
  }
}

base_tbl <- function() {
  tibble::tibble(
    id   = c("A", "B", "C"),
    name = c("alpha beta", "gamma delta", "alpha beta"),
    city = c("Berlin", "Hamburg", "Berlin")
  )
}

target_tbl <- function() {
  tibble::tibble(
    id   = c("X", "Y"),
    name = c("alpha beta", "epsilon zeta"),
    city = c("Berlin", "Munich")
  )
}

base_df <- function() as.data.frame(base_tbl())
target_df <- function() as.data.frame(target_tbl())

make_strategy <- function(...) {
  args <- utils::modifyList(
    list(
      columns         = "name",
      embedding_model = NULL,
      threshold       = 0.0,
      collapse_sep    = " ",
      normalize       = TRUE,
      batch_size      = 1000L,
      block_by        = NULL
    ),
    list(...)
  )
  do.call(Embedding_Strategy, args)
}


# ── compute_embeddings dispatch ───────────────────────────────────────────────

test_that("compute_embeddings() on tibble returns tibble", {
  local_mocked_bindings(embed = fake_embed(), .package = "tidyllm")

  out <- compute_embeddings(base_tbl(), id = "id", strategy = make_strategy())

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("id", "embedding") %in% names(out)))
  expect_equal(nrow(out), 3L)
})

test_that("compute_embeddings() on data.frame returns data.frame (not tibble)", {
  local_mocked_bindings(embed = fake_embed(), .package = "tidyllm")

  out <- compute_embeddings(base_df(), id = "id", strategy = make_strategy())

  expect_s3_class(out, "data.frame")
  expect_false(inherits(out, "tbl_df"))
})


# ── search_candidates dispatch ────────────────────────────────────────────────

test_that("search_candidates() on tibbles returns a tibble", {
  local_mocked_bindings(embed = fake_embed(8L), .package = "tidyllm")

  out <- search_candidates(
    base_tbl(), target_tbl(), "id", "id", make_strategy(threshold = 0.0)
  )

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("match_id", "score", "source", "id", "rank") %in% names(out)))
})

test_that("search_candidates() on data.frames returns a data.frame", {
  local_mocked_bindings(embed = fake_embed(8L), .package = "tidyllm")

  out <- search_candidates(
    base_df(), target_df(), "id", "id", make_strategy(threshold = 0.0)
  )

  expect_s3_class(out, "data.frame")
  expect_false(inherits(out, "tbl_df"))
})


# ── detect_duplicates dispatch ────────────────────────────────────────────────

test_that("detect_duplicates() on tibble returns a tibble", {
  local_mocked_bindings(
    embed = function(text, model) {
      tibble::tibble(
        input = text,
        embeddings = lapply(text, function(t) {
          if (t == "alpha beta") c(1, 0, 0, 0) else c(0, 1, 0, 0)
        })
      )
    },
    .package = "tidyllm"
  )

  out <- detect_duplicates(base_tbl(), id = "id", strategy = make_strategy(threshold = 0.9))

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("duplicate_group", "score", "id", "rank") %in% names(out)))
  expect_setequal(out$id, c("A", "C"))
})

test_that("detect_duplicates() on data.frame returns a data.frame", {
  local_mocked_bindings(
    embed = function(text, model) {
      tibble::tibble(
        input = text,
        embeddings = lapply(text, function(t) {
          if (t == "alpha beta") c(1, 0, 0, 0) else c(0, 1, 0, 0)
        })
      )
    },
    .package = "tidyllm"
  )

  out <- detect_duplicates(base_df(), id = "id", strategy = make_strategy(threshold = 0.9))

  expect_s3_class(out, "data.frame")
  expect_false(inherits(out, "tbl_df"))
})


# ── Backend parity: tibble path produces same results as data.table ──────────

test_that("tibble dispatch produces same matches as data.table backend", {
  local_mocked_bindings(embed = fake_embed(8L), .package = "tidyllm")

  str <- make_strategy(threshold = 0.0)

  out_tbl <- search_candidates(base_tbl(), target_tbl(), "id", "id", str)
  out_dt  <- search_candidates(
    data.table::as.data.table(base_tbl()),
    data.table::as.data.table(target_tbl()),
    "id", "id", str
  )

  # Compare on shared columns; coerce both to data.frame for stable equality.
  expect_equal(
    as.data.frame(out_tbl)[, c("match_id", "score", "source", "id", "rank")],
    as.data.frame(out_dt) [, c("match_id", "score", "source", "id", "rank")]
  )
})


# ── score_embeddings dispatch ────────────────────────────────────────────────

test_that("score_embeddings() on tibble inputs returns a tibble", {
  base_emb <- tibble::tibble(
    id        = c("A", "B"),
    embedding = list(c(1, 0, 0), c(0, 1, 0))
  )
  tgt_emb <- tibble::tibble(
    id        = c("X", "Y"),
    embedding = list(c(1, 0, 0), c(0, 0, 1))
  )

  out <- score_embeddings(base_emb, tgt_emb, make_strategy())

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("base_id", "target_id", "score"))
})
