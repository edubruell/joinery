skip_if_not_installed("tidyllm")

# Tests for multi_stage_search() across mixed strategy types:
# token (Search_Strategy) followed by embedding (Embedding_Strategy) on
# residuals. multi_stage_search() resolves to a cross-source ENTITY GROUPING
# (one row per record) with the directed edge ledger on the "ledger" attribute;
# these tests verify the guard accepts Embedding_Strategy, residual handling
# works across strategy types, and the stage tagging is correct.


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

ent_of <- function(out, i) out[id == i]$entity


# ── Validation: guard accepts Embedding_Strategy ──────────────────────────────

test_that("multi_stage_search() (data.table) returns the entity grouping + ledger", {
  local_mocked_bindings(embed = make_embed_mock(), .package = "tidyllm")

  out <- multi_stage_search(
    make_base(), make_target(), "id", "id",
    strategies = list(token = token_strategy(), emb = emb_strategy())
  )

  expect_s3_class(out, "data.table")
  expect_true(all(c("entity", "id", "rep", "rank", "score", "stage") %in% names(out)))
  expect_false(is.null(attr(out, "ledger")))
  expect_true(all(c("from", "to", "stage", "score") %in% names(attr(out, "ledger"))))
})

test_that("multi_stage_search() rejects non-strategy objects", {
  expect_error(
    multi_stage_search(
      make_base(), make_target(), "id", "id",
      strategies = list(token = token_strategy(), bogus = list())
    ),
    "Search_Strategy.*Embedding_Strategy"
  )
})

test_that("multi_stage_search() rejects empty strategies list", {
  expect_error(
    multi_stage_search(
      make_base(), make_target(), "id", "id",
      strategies = list()
    ),
    "non-empty list"
  )
})


# ── data.table: token → embedding pipeline ────────────────────────────────────

test_that("token + embedding stages link the right pairs into entities", {
  local_mocked_bindings(embed = make_embed_mock(), .package = "tidyllm")

  out <- multi_stage_search(
    make_base(), make_target(), "id", "id",
    strategies = list(token = token_strategy(), emb = emb_strategy())
  )

  # A1/A2 one entity (token stage); B1/B2 one entity (emb stage).
  expect_equal(ent_of(out, "A1"), ent_of(out, "A2"))
  expect_equal(ent_of(out, "B1"), ent_of(out, "B2"))
  expect_true(ent_of(out, "A1") != ent_of(out, "B1"))

  expect_equal(unique(out[id %in% c("A1", "A2")]$stage), "token")
  expect_equal(unique(out[id %in% c("B1", "B2")]$stage), "emb")
})

test_that("records matched in stage 1 are not re-matched in stage 2", {
  local_mocked_bindings(embed = make_embed_mock(), .package = "tidyllm")

  out <- multi_stage_search(
    make_base(), make_target(), "id", "id",
    strategies = list(token = token_strategy(), emb = emb_strategy())
  )
  led <- attr(out, "ledger")
  # A1 / A2 are linked by tokens; the embedding stage must not re-link them.
  emb_ids <- unique(c(led[stage == "emb"]$from, led[stage == "emb"]$to))
  expect_false(any(c("A1", "A2") %in% emb_ids))
})

test_that("each ledger edge belongs to exactly one stage", {
  local_mocked_bindings(embed = make_embed_mock(), .package = "tidyllm")

  out <- multi_stage_search(
    make_base(), make_target(), "id", "id",
    strategies = list(token = token_strategy(), emb = emb_strategy())
  )
  led <- attr(out, "ledger")
  expect_setequal(unique(led$stage), c("token", "emb"))
})

test_that("multi_stage_search() with no matches returns singleton entities", {
  local_mocked_bindings(embed = make_embed_mock(), .package = "tidyllm")

  no_token <- search_strategy(
    name ~ normalize_text() + word_tokens(min_nchar = 50L),
    threshold = 0.99
  )
  no_emb <- emb_strategy(threshold = 0.99999)

  base_dt   <- data.table::data.table(id = "X", name = "qzzqz")
  target_dt <- data.table::data.table(id = "Y", name = "wkkwk")

  out <- multi_stage_search(
    base_dt, target_dt, "id", "id",
    strategies = list(token = no_token, emb = no_emb)
  )

  # Nothing links → every record is its own singleton entity.
  expect_setequal(out$id, c("X", "Y"))
  expect_equal(uniqueN(out$entity), 2L)
  expect_equal(nrow(attr(out, "ledger")), 0L)
})


# ── DuckDB: token → embedding pipeline ────────────────────────────────────────

test_that("DuckDB multi_stage_search() runs token → embedding end-to-end on small tables", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  local_mocked_bindings(embed = make_embed_mock(), .package = "tidyllm")

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:", array = "matrix")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  res <- tryCatch(DBI::dbExecute(con, "INSTALL vss; LOAD vss;"), error = function(e) e)
  if (inherits(res, "error")) skip("DuckDB vss extension not available")

  DBI::dbWriteTable(con, "base_tbl",   as.data.frame(make_base()))
  DBI::dbWriteTable(con, "target_tbl", as.data.frame(make_target()))

  out <- multi_stage_search(
    dplyr::tbl(con, "base_tbl"),
    dplyr::tbl(con, "target_tbl"),
    "id", "id",
    strategies = list(
      token = token_strategy(),
      emb   = emb_strategy(threshold = 0.99)
    )
  )
  grp <- as.data.table(dplyr::collect(out))

  expect_true(all(c("entity", "id", "rep", "rank", "score", "stage") %in% names(grp)))
  expect_equal(grp[id == "A1"]$entity, grp[id == "A2"]$entity)
  expect_equal(grp[id == "B1"]$entity, grp[id == "B2"]$entity)
  expect_equal(unique(grp[id %in% c("A1", "A2")]$stage), "token")
  expect_equal(unique(grp[id %in% c("B1", "B2")]$stage), "emb")
})
