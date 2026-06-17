skip_if_not_installed("tidyllm")

# A counting fake embed(): records how many texts it was asked to embed, and
# returns content-deterministic unit vectors so reuse cannot change scores.
counting_embed <- function(dim = 8L) {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  calls$texts <- character(0)
  fn <- function(text, model) {
    calls$n <- calls$n + length(text)
    calls$texts <- c(calls$texts, text)
    vecs <- lapply(text, function(t) {
      h <- strtoi(substr(rlang::hash(t), 1L, 6L), base = 16L)
      v <- rep(0, dim)
      v[(h %% dim) + 1L] <- 1.0
      v
    })
    tibble::tibble(input = text, embeddings = vecs)
  }
  list(fn = fn, calls = calls)
}

make_dt <- function() {
  data.table::data.table(
    id   = c("A", "B", "C"),
    name = c("alpha beta", "gamma delta", "epsilon zeta")
  )
}

make_strategy <- function(...) {
  args <- utils::modifyList(
    list(
      columns         = "name",
      embedding_model = NULL,
      threshold       = 0.5,
      collapse_sep    = " ",
      normalize       = TRUE,
      batch_size      = 1000L,
      block_by        = NULL
    ),
    list(...)
  )
  do.call(Embedding_Strategy, args)
}

# Keep the global cache from leaking between tests, and turn reuse on (the suite
# baseline disables it; see setup.R).
local_reuse <- function(env = parent.frame()) {
  withr::local_options(joinery.embedding_reuse = TRUE, .local_envir = env)
  clear_embedding_cache(disk = TRUE)
  withr::defer(clear_embedding_cache(disk = TRUE), envir = env)
}


test_that("a second identical compute_embeddings() call embeds nothing", {
  local_reuse()
  ce <- counting_embed()
  local_mocked_bindings(embed = ce$fn, .package = "tidyllm")

  dt  <- make_dt()
  str <- make_strategy()

  out1 <- compute_embeddings(dt, id = "id", strategy = str)
  expect_equal(ce$calls$n, 3L)

  out2 <- compute_embeddings(dt, id = "id", strategy = str)
  expect_equal(ce$calls$n, 3L) # no new generation

  expect_equal(out1$embedding, out2$embedding)
})


test_that("only a changed record re-embeds (content-hash keying)", {
  local_reuse()
  ce <- counting_embed()
  local_mocked_bindings(embed = ce$fn, .package = "tidyllm")

  str <- make_strategy()
  compute_embeddings(make_dt(), id = "id", strategy = str)
  expect_equal(ce$calls$n, 3L)

  dt2 <- make_dt()
  dt2[id == "B", name := "gamma delta CHANGED"]
  compute_embeddings(dt2, id = "id", strategy = str)
  expect_equal(ce$calls$n, 4L) # one re-embed
})


test_that("normalize = TRUE and FALSE share the raw cache", {
  local_reuse()
  ce <- counting_embed()
  local_mocked_bindings(embed = ce$fn, .package = "tidyllm")

  dt <- make_dt()

  out_n <- compute_embeddings(dt, id = "id", strategy = make_strategy(normalize = TRUE))
  expect_equal(ce$calls$n, 3L)

  out_r <- compute_embeddings(dt, id = "id", strategy = make_strategy(normalize = FALSE))
  expect_equal(ce$calls$n, 3L) # reused, no new generation

  # normalized vectors are unit length; raw ones are what the mock returned
  norms_n <- map_dbl(out_n$embedding, function(v) sqrt(sum(v^2)))
  expect_equal(norms_n, rep(1, 3L))
})


test_that("different models do not share cache entries", {
  local_reuse()
  ce <- counting_embed()
  local_mocked_bindings(embed = ce$fn, .package = "tidyllm")

  dt <- make_dt()
  compute_embeddings(dt, id = "id", strategy = make_strategy(embedding_model = "model-a"))
  expect_equal(ce$calls$n, 3L)

  compute_embeddings(dt, id = "id", strategy = make_strategy(embedding_model = "model-b"))
  expect_equal(ce$calls$n, 6L) # distinct model key -> all re-embed
})


test_that("disk cache survives an in-session clear", {
  withr::local_options(joinery.embedding_cache_dir = withr::local_tempdir())
  local_reuse()
  ce <- counting_embed()
  local_mocked_bindings(embed = ce$fn, .package = "tidyllm")

  dt  <- make_dt()
  str <- make_strategy()

  compute_embeddings(dt, id = "id", strategy = str)
  expect_equal(ce$calls$n, 3L)

  clear_embedding_cache(disk = FALSE)      # env only; disk retained
  compute_embeddings(dt, id = "id", strategy = str)
  expect_equal(ce$calls$n, 3L)             # rehydrated from disk

  clear_embedding_cache(disk = TRUE)       # wipe disk too
  compute_embeddings(dt, id = "id", strategy = str)
  expect_equal(ce$calls$n, 6L)             # forced re-embed
})


test_that("verb-level reuse: a second search_candidates() embeds nothing new", {
  local_reuse()
  ce <- counting_embed()
  local_mocked_bindings(embed = ce$fn, .package = "tidyllm")

  # distinct text across tables so the count reflects 2 base + 2 target
  # (identical text would otherwise reuse across the two compute_embeddings calls)
  base <- data.table::data.table(id = c("A", "B"), name = c("alpha beta", "gamma delta"))
  tgt  <- data.table::data.table(id = c("X", "Y"), name = c("kappa lambda", "mu nu"))
  str  <- make_strategy(threshold = 0.1)

  search_candidates(base, tgt, "id", "id", str)
  n_first <- ce$calls$n
  expect_equal(n_first, 4L) # 2 base + 2 target

  search_candidates(base, tgt, "id", "id", str)
  expect_equal(ce$calls$n, n_first) # nothing re-embedded
})


test_that("verb-level reuse: detect_duplicates() reuses on a second call", {
  local_reuse()
  ce <- counting_embed()
  local_mocked_bindings(embed = ce$fn, .package = "tidyllm")

  dt  <- data.table::data.table(id = c("A", "B", "C"),
                                name = c("alpha beta", "alpha beta", "gamma delta"))
  str <- make_strategy(threshold = 0.1)

  detect_duplicates(dt, "id", str)
  n_first <- ce$calls$n
  expect_equal(n_first, 3L)

  detect_duplicates(dt, "id", str)
  expect_equal(ce$calls$n, n_first)
})


test_that("tibble input reuses on a second call (delegation path)", {
  local_reuse()
  ce <- counting_embed()
  local_mocked_bindings(embed = ce$fn, .package = "tidyllm")

  tb  <- tibble::tibble(id = c("A", "B"), name = c("alpha beta", "gamma delta"))
  str <- make_strategy()

  compute_embeddings(tb, id = "id", strategy = str)
  expect_equal(ce$calls$n, 2L)

  compute_embeddings(tb, id = "id", strategy = str)
  expect_equal(ce$calls$n, 2L)
})
