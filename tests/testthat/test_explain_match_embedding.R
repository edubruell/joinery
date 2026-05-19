# Tests for explain_match() on Embedding_Strategy (Phase 0.6 M8).
#
# Embedding strategy returns pair + score only; per-column and per-token
# attribution slots are NULL.

skip_if_not_installed("tidyllm")
skip_if_not_installed("tibble")

library(data.table)


# ---------------------------------------------------------------------------
# Fixture builders
# ---------------------------------------------------------------------------

# Maps text -> deterministic embedding so we can engineer known matches.
fake_embed_by_text <- function(mapping, dim = 4L, default = c(0, 0, 0, 1)) {
  function(text, model) {
    vecs <- lapply(text, function(t) {
      if (!is.null(mapping[[t]])) mapping[[t]] else default
    })
    tibble::tibble(input = text, embeddings = vecs)
  }
}

make_emb_strategy <- function(...) {
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

base_dt <- function() {
  data.table::data.table(
    id   = c("A", "B", "C"),
    name = c("alpha", "beta",  "alpha")
  )
}

target_dt <- function() {
  data.table::data.table(
    id   = c("X", "Y"),
    name = c("alpha", "gamma")
  )
}

mapping_xy <- list(
  alpha = c(1, 0, 0, 0),
  beta  = c(0, 1, 0, 0),
  gamma = c(0, 0, 1, 0)
)


# ---------------------------------------------------------------------------
# 1. Candidates form
# ---------------------------------------------------------------------------

test_that("explain_match on candidates returns Match_Explanation with NULL contributions", {
  local_mocked_bindings(
    embed = fake_embed_by_text(mapping_xy), .package = "tidyllm"
  )
  strat <- make_emb_strategy()
  cands <- search_candidates(base_dt(), target_dt(), "id", "id", strat)

  ex <- explain_match(cands, strat, match_id = cands$match_id[1L])
  expect_true(S7::S7_inherits(ex, Match_Explanation))
  expect_null(ex@per_column_contrib)
  expect_null(ex@shared_tokens)
  expect_true(!is.null(ex@pair))
  expect_true(nrow(ex@pair) >= 2L)
  expect_type(ex@score, "double")
  expect_identical(ex@score_breakdown$method, "cosine_similarity")
})


# ---------------------------------------------------------------------------
# 2. Duplicates form
# ---------------------------------------------------------------------------

test_that("explain_match on duplicates returns Match_Explanation with NULL contributions", {
  local_mocked_bindings(
    embed = fake_embed_by_text(mapping_xy), .package = "tidyllm"
  )
  strat <- make_emb_strategy()
  dups  <- detect_duplicates(base_dt(), "id", strat)
  skip_if(nrow(dups) == 0L, "no duplicates produced by fixture")

  mid <- dups$duplicate_group[1L]
  ex  <- explain_match(dups, strat, match_id = mid)
  expect_true(S7::S7_inherits(ex, Match_Explanation))
  expect_null(ex@per_column_contrib)
  expect_null(ex@shared_tokens)
  expect_identical(ex@score_breakdown$method, "cosine_similarity")
})


# ---------------------------------------------------------------------------
# 3. print() surfaces the no-attribution note
# ---------------------------------------------------------------------------

test_that("print mentions per-token attribution is unavailable", {
  local_mocked_bindings(
    embed = fake_embed_by_text(mapping_xy), .package = "tidyllm"
  )
  strat <- make_emb_strategy()
  cands <- search_candidates(base_dt(), target_dt(), "id", "id", strat)
  ex    <- explain_match(cands, strat, match_id = cands$match_id[1L])
  fmt   <- format(ex)
  expect_true(any(grepl("not available for embedding", fmt, fixed = TRUE)))
})


# ---------------------------------------------------------------------------
# 4. tibble / data.frame parity
# ---------------------------------------------------------------------------

test_that("tibble matches dispatch to embedding form", {
  local_mocked_bindings(
    embed = fake_embed_by_text(mapping_xy), .package = "tidyllm"
  )
  strat <- make_emb_strategy()
  cands <- search_candidates(
    tibble::as_tibble(base_dt()), tibble::as_tibble(target_dt()),
    "id", "id", strat
  )
  ex <- explain_match(cands, strat, match_id = cands$match_id[1L])
  expect_true(S7::S7_inherits(ex, Match_Explanation))
  expect_null(ex@per_column_contrib)
})

test_that("data.frame matches dispatch to embedding form", {
  local_mocked_bindings(
    embed = fake_embed_by_text(mapping_xy), .package = "tidyllm"
  )
  strat <- make_emb_strategy()
  cands <- search_candidates(
    as.data.frame(base_dt()), as.data.frame(target_dt()),
    "id", "id", strat
  )
  ex <- explain_match(cands, strat, match_id = cands$match_id[1L])
  expect_true(S7::S7_inherits(ex, Match_Explanation))
  expect_null(ex@per_column_contrib)
})


# ---------------------------------------------------------------------------
# 5. Invalid match_id
# ---------------------------------------------------------------------------

test_that("explain_match errors when match_id is not in the table", {
  local_mocked_bindings(
    embed = fake_embed_by_text(mapping_xy), .package = "tidyllm"
  )
  strat <- make_emb_strategy()
  cands <- search_candidates(base_dt(), target_dt(), "id", "id", strat)
  expect_error(explain_match(cands, strat, match_id = 9999L))
})
