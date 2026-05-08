skip_if_not_installed("tidyllm")

# Tests for multi_stage_match() across mixed strategy types:
# token (Search_Strategy) followed by embedding (Embedding_Strategy)
# on residuals. Verifies the validation guard accepts Embedding_Strategy,
# residual extraction works across strategy types, and match_ids are
# globally unique across stages.


# ── Fixtures ──────────────────────────────────────────────────────────────────

# Token-clean pair (A1/A2): identical canonical text → token stage matches.
# Embedding-only pair (B1/B2): no token overlap → token stage misses,
#   embedding stage catches it via mocked vectors.
# Unmatched pair (D1/E2): different across both strategies.
make_base <- function() {
  data.table::data.table(
    id   = c("A1", "B1", "D1"),
    name = c("alpha beta", "synonymA", "totally different one")
  )
}
make_target <- function() {
  data.table::data.table(
    id   = c("A2", "B2", "E2"),
    name = c("alpha beta", "synonymB", "completely unrelated text")
  )
}

# Mock embed(): B1/B2 → identical vector; A1/A2 → identical vector;
# everything else → orthogonal noise. Token stage should consume A1/A2 first;
# the embedding stage runs only on the residual (B1, D1) ↔ (B2, E2).
make_embed_mock <- function(dim = 8L) {
  function(text, model) {
    vecs <- lapply(text, function(t) {
      v <- rep(0, dim)
      if (t %in% c("synonymA", "synonymB")) {
        v[[1]] <- 1.0           # B1/B2 share this vector
      } else if (t == "alpha beta") {
        v[[2]] <- 1.0           # A1/A2 share this vector
      } else {
        # noise: hash-style orthogonal placement
        v[[((sum(utf8ToInt(t)) %% (dim - 2L)) + 3L)]] <- 1.0
      }
      v
    })
    tibble::tibble(input = text, embeddings = vecs)
  }
}

token_strategy <- function() {
  search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold = 0.99
  )
}

emb_strategy <- function(threshold = 0.99) {
  Embedding_Strategy(
    columns         = "name",
    embedding_model = NULL,
    threshold       = threshold,
    collapse_sep    = " ",
    normalize       = TRUE,
    batch_size      = 1000L,
    block_by        = NULL
  )
}


# ── Validation: guard accepts Embedding_Strategy ──────────────────────────────

test_that("multi_stage_match() (data.table) accepts Embedding_Strategy in the list", {
  local_mocked_bindings(embed = make_embed_mock(), .package = "tidyllm")

  out <- multi_stage_match(
    make_base(), make_target(), "id", "id",
    strategies = list(token = token_strategy(), emb = emb_strategy())
  )

  expect_s3_class(out, "data.table")
  expect_true(all(c("match_id", "score", "stage", "source", "id", "rank") %in% names(out)))
})

test_that("multi_stage_match() rejects non-strategy objects", {
  expect_error(
    multi_stage_match(
      make_base(), make_target(), "id", "id",
      strategies = list(token = token_strategy(), bogus = list())
    ),
    "Search_Strategy or Embedding_Strategy"
  )
})

test_that("multi_stage_match() rejects empty strategies list", {
  expect_error(
    multi_stage_match(
      make_base(), make_target(), "id", "id",
      strategies = list()
    ),
    "must not be empty"
  )
})


# ── data.table: token → embedding pipeline ────────────────────────────────────

test_that("token + embedding stages tag matches with stage names", {
  local_mocked_bindings(embed = make_embed_mock(), .package = "tidyllm")

  out <- multi_stage_match(
    make_base(), make_target(), "id", "id",
    strategies = list(token = token_strategy(), emb = emb_strategy())
  )

  expect_setequal(unique(out$stage), c("token", "emb"))

  token_ids <- unique(out[stage == "token", id])
  emb_ids   <- unique(out[stage == "emb",   id])
  expect_true(all(c("A1", "A2") %in% token_ids))
  expect_true(all(c("B1", "B2") %in% emb_ids))
})

test_that("records matched in stage 1 are not re-matched in stage 2", {
  local_mocked_bindings(embed = make_embed_mock(), .package = "tidyllm")

  out <- multi_stage_match(
    make_base(), make_target(), "id", "id",
    strategies = list(token = token_strategy(), emb = emb_strategy())
  )

  # A1 / A2 are matched by tokens; the embedding stage must not surface them.
  expect_false(any(out[stage == "emb", id] %in% c("A1", "A2")))
})

test_that("multi_stage_match() produces globally unique match_ids across stages", {
  local_mocked_bindings(embed = make_embed_mock(), .package = "tidyllm")

  out <- multi_stage_match(
    make_base(), make_target(), "id", "id",
    strategies = list(token = token_strategy(), emb = emb_strategy())
  )

  # match_id is duplicated within a match (one for each side), but each match
  # group must belong to exactly one stage.
  by_match <- out[, .(stages = length(unique(stage))), by = match_id]
  expect_true(all(by_match$stages == 1L))
})

test_that("multi_stage_match() returns empty schema when no stage matches", {
  local_mocked_bindings(embed = make_embed_mock(), .package = "tidyllm")

  # Both strategies set extreme thresholds so nothing matches.
  no_token <- search_strategy(
    name ~ normalize_text() + word_tokens(min_nchar = 50L),
    threshold = 0.99
  )
  no_emb <- emb_strategy(threshold = 0.99999)

  # Disjoint fixture: nothing should match across either stage.
  base_dt   <- data.table::data.table(id = "X", name = "qzzqz")
  target_dt <- data.table::data.table(id = "Y", name = "wkkwk")

  out <- multi_stage_match(
    base_dt, target_dt, "id", "id",
    strategies = list(token = no_token, emb = no_emb)
  )

  expect_equal(nrow(out), 0L)
  expect_true(all(c("match_id", "score", "stage", "source", "id", "rank") %in% names(out)))
})


# ── DuckDB: token → embedding pipeline (parity smoke test) ────────────────────

# DuckDB multi_stage_match() with two embedding stages.
# Token + embedding on DuckDB is not exercised here because
# prepare_search_data() via batch_map is brittle on tiny fixtures (a
# separate batch_duckdb issue). Two embedding stages bypass batch_map and
# still cover validation, residual extraction, and the
# extract_unmatched() lazy-query bug fix.
#
# Single mock; stage separation via threshold:
#   A1/A2 → identical vectors (cosine 1.0) → only matched at strict threshold
#   B1/B2 → cosine ≈ 0.71 → only matched at looser threshold
test_that("DuckDB multi_stage_match() runs two embedding stages end-to-end", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  emb_map <- list(
    `alpha beta`   = c(1, 0, 0, 0),
    synonymA       = c(1, 1, 0, 0),                # |.| = sqrt(2)
    synonymB       = c(1, 1, 0, 0),                # cos(syn,syn) = 1.0
    `totally different one`     = c(0, 0, 1, 0),
    `completely unrelated text` = c(0, 0, 0, 1)
  )
  # Make A1/A2 match at high threshold but B1/B2 only at lower.
  # synonymA / synonymB are identical → cos 1.0; restate so they're close-but-distinct.
  emb_map$synonymA <- c(1, 1, 0, 0)
  emb_map$synonymB <- c(1, 0.4, 0, 0)               # cos ≈ 0.927

  local_mocked_bindings(
    embed = function(text, model) {
      vecs <- lapply(text, function(t) {
        if (!is.null(emb_map[[t]])) emb_map[[t]] else c(0, 0, 0, 1)
      })
      tibble::tibble(input = text, embeddings = vecs)
    },
    .package = "tidyllm"
  )

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:", array = "matrix")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  res <- tryCatch(DBI::dbExecute(con, "INSTALL vss; LOAD vss;"), error = function(e) e)
  if (inherits(res, "error")) skip("DuckDB vss extension not available")

  DBI::dbWriteTable(con, "base_tbl",   as.data.frame(make_base()))
  DBI::dbWriteTable(con, "target_tbl", as.data.frame(make_target()))

  out <- multi_stage_match(
    dplyr::tbl(con, "base_tbl"),
    dplyr::tbl(con, "target_tbl"),
    "id", "id",
    strategies = list(
      stage_a = emb_strategy(threshold = 0.99),  # only A1/A2 (cos 1.0)
      stage_b = emb_strategy(threshold = 0.90)   # then B1/B2 (cos ≈ 0.928)
    )
  )
  out_df <- as.data.frame(out)

  expect_true(all(c("match_id", "score", "stage", "source", "id", "rank") %in% names(out_df)))
  expect_setequal(unique(out_df$stage), c("stage_a", "stage_b"))

  a_ids <- unique(out_df$id[out_df$stage == "stage_a"])
  b_ids <- unique(out_df$id[out_df$stage == "stage_b"])
  expect_true(all(c("A1", "A2") %in% a_ids))
  expect_true(all(c("B1", "B2") %in% b_ids))
  # Stage-1 records do not bleed into stage 2.
  expect_false(any(c("A1", "A2") %in% b_ids))
})
