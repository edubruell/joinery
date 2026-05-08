skip_if_not_installed("tidyllm")

# Helper: fake embed() that returns the tidyllm tibble schema
# Each record gets a unit vector of dimension `dim` derived from its position,
# so that cosine similarities are deterministic across calls.
fake_embed <- function(dim = 8L) {
  function(text, model) {
    vecs <- lapply(seq_along(text), function(i) {
      v <- rep(0, dim)
      v[((i - 1L) %% dim) + 1L] <- 1.0
      v
    })
    tibble::tibble(input = text, embeddings = vecs)
  }
}

# Minimal data.table fixtures
make_base <- function() {
  data.table::data.table(
    id   = c("A", "B", "C"),
    name = c("alpha beta", "gamma delta", "alpha beta"),
    city = c("Berlin", "Hamburg", "Berlin")
  )
}

make_target <- function() {
  data.table::data.table(
    id   = c("X", "Y"),
    name = c("alpha beta", "epsilon zeta"),
    city = c("Berlin", "Munich")
  )
}

make_strategy <- function(...) {
  # Bypass embedding_strategy() constructor's tidyllm check with S7 directly.
  # embedding_model = NULL is safe since embed() is always mocked in tests.
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


# ── assemble_record_text() ────────────────────────────────────────────────────

test_that("assemble_record_text() concatenates specified columns", {
  dt <- make_base()
  out <- assemble_record_text(dt, id = "id", columns = c("name", "city"))

  expect_equal(nrow(out), 3L)
  expect_named(out, c("id", "text"))
  expect_equal(out$text[[1]], "alpha beta Berlin")
})

test_that("assemble_record_text() auto-detects character columns when columns = character(0)", {
  dt <- make_base()
  out <- assemble_record_text(dt, id = "id", columns = character(0))

  expect_equal(nrow(out), 3L)
  # Should include both name and city
  expect_true(all(grepl("alpha|gamma", out$text[1:2])))
})

test_that("assemble_record_text() drops NA parts per record", {
  dt <- data.table::data.table(
    id   = c("A", "B"),
    name = c("alpha", NA_character_),
    city = c(NA_character_, "Berlin")
  )
  out <- assemble_record_text(dt, id = "id", columns = c("name", "city"))

  expect_equal(out$text[[1]], "alpha")
  expect_equal(out$text[[2]], "Berlin")
})

test_that("assemble_record_text() errors on missing columns", {
  dt <- make_base()
  expect_error(
    assemble_record_text(dt, id = "id", columns = c("name", "nonexistent")),
    "Columns not found"
  )
})

test_that("assemble_record_text() errors when no character columns available", {
  dt <- data.table::data.table(id = "A", value = 1L)
  expect_error(
    assemble_record_text(dt, id = "id", columns = character(0)),
    "No character-like columns"
  )
})


# ── compute_embeddings() ─────────────────────────────────────────────────────

test_that("compute_embeddings() returns id column + embedding list-column", {
  local_mocked_bindings(embed = fake_embed(8L), .package = "tidyllm")

  dt  <- make_base()
  str <- make_strategy()
  out <- compute_embeddings(dt, id = "id", strategy = str)

  expect_true(data.table::is.data.table(out))
  expect_named(out, c("id", "embedding"))
  expect_equal(nrow(out), 3L)
  expect_type(out$embedding, "list")
  expect_length(out$embedding[[1L]], 8L)
})

test_that("compute_embeddings() L2-normalises when normalize = TRUE", {
  local_mocked_bindings(embed = fake_embed(4L), .package = "tidyllm")

  dt  <- make_base()
  str <- make_strategy(normalize = TRUE)
  out <- compute_embeddings(dt, id = "id", strategy = str)

  norms <- vapply(out$embedding, function(v) sqrt(sum(v^2)), numeric(1L))
  expect_equal(norms, rep(1.0, 3L), tolerance = 1e-10)
})

test_that("compute_embeddings() skips normalisation when normalize = FALSE", {
  local_mocked_bindings(
    embed = function(text, model) {
      tibble::tibble(
        input      = text,
        embeddings = lapply(seq_along(text), function(i) rep(2.0, 4L))
      )
    },
    .package = "tidyllm"
  )

  dt  <- make_base()
  str <- make_strategy(normalize = FALSE)
  out <- compute_embeddings(dt, id = "id", strategy = str)

  expect_equal(out$embedding[[1L]], rep(2.0, 4L))
})

test_that("compute_embeddings() batches correctly", {
  call_count <- 0L
  local_mocked_bindings(
    embed = function(text, model) {
      call_count <<- call_count + 1L
      tibble::tibble(
        input      = text,
        embeddings = lapply(seq_along(text), function(i) rnorm(4L))
      )
    },
    .package = "tidyllm"
  )

  dt  <- make_base()  # 3 records
  str <- make_strategy(batch_size = 2L)
  out <- compute_embeddings(dt, id = "id", strategy = str)

  expect_equal(call_count, 2L)  # ceil(3/2) = 2 batches
  expect_equal(nrow(out), 3L)
})

test_that("compute_embeddings() attaches blocking columns", {
  local_mocked_bindings(embed = fake_embed(4L), .package = "tidyllm")

  dt  <- make_base()
  str <- make_strategy(block_by = "city")
  out <- compute_embeddings(dt, id = "id", strategy = str)

  expect_true("city" %in% names(out))
  expect_equal(out$city, dt$city)
})

test_that("compute_embeddings() errors on missing ID column", {
  local_mocked_bindings(embed = fake_embed(4L), .package = "tidyllm")

  dt  <- make_base()
  str <- make_strategy()
  expect_error(compute_embeddings(dt, id = "nope", strategy = str), "not found")
})

test_that("compute_embeddings() errors on missing blocking column", {
  local_mocked_bindings(embed = fake_embed(4L), .package = "tidyllm")

  dt  <- make_base()
  str <- make_strategy(block_by = "nonexistent")
  expect_error(compute_embeddings(dt, id = "id", strategy = str), "Blocking columns not found")
})


# ── score_embeddings() ────────────────────────────────────────────────────────

test_that("score_embeddings() returns base_id, target_id, score", {
  local_mocked_bindings(embed = fake_embed(4L), .package = "tidyllm")

  str      <- make_strategy()
  base_emb <- compute_embeddings(make_base(),   id = "id", strategy = str)
  tgt_emb  <- compute_embeddings(make_target(), id = "id", strategy = str)

  # Rename to generic 'id' + 'embedding' as score_embeddings expects
  data.table::setnames(base_emb, "id", "id")
  data.table::setnames(tgt_emb,  "id", "id")

  out <- score_embeddings(base_emb, tgt_emb, str)

  expect_true(data.table::is.data.table(out))
  expect_named(out, c("base_id", "target_id", "score"))
  expect_equal(nrow(out), 3L * 2L)  # all pairs
  expect_true(all(out$score >= -1.0 & out$score <= 1.0))
})

test_that("score_embeddings() identical normalised vectors score 1", {
  vec <- c(1, 0, 0, 0)
  base_emb <- data.table::data.table(id = "A", embedding = list(vec))
  tgt_emb  <- data.table::data.table(id = "B", embedding = list(vec))
  str <- make_strategy()

  out <- score_embeddings(base_emb, tgt_emb, str)
  expect_equal(out$score, 1.0, tolerance = 1e-10)
})

test_that("score_embeddings() orthogonal vectors score 0", {
  base_emb <- data.table::data.table(id = "A", embedding = list(c(1, 0, 0, 0)))
  tgt_emb  <- data.table::data.table(id = "B", embedding = list(c(0, 1, 0, 0)))
  str <- make_strategy()

  out <- score_embeddings(base_emb, tgt_emb, str)
  expect_equal(out$score, 0.0, tolerance = 1e-10)
})

test_that("score_embeddings() errors on missing columns", {
  bad <- data.table::data.table(id = "A", wrong_col = list(c(1, 0)))
  str <- make_strategy()

  expect_error(score_embeddings(bad, bad, str), "must have columns")
})


# ── search_candidates() ───────────────────────────────────────────────────────

test_that("search_candidates() returns standard match schema", {
  local_mocked_bindings(embed = fake_embed(8L), .package = "tidyllm")

  str <- make_strategy(threshold = 0.0)  # accept all pairs
  out <- search_candidates(make_base(), make_target(), "id", "id", str)

  expect_true(data.table::is.data.table(out))
  expect_true(all(c("match_id", "score", "source", "id", "rank") %in% names(out)))
  expect_setequal(unique(out$source), c("base", "target"))
  expect_true(all(out$rank >= 1L))
})

test_that("search_candidates() threshold filters low-scoring pairs", {
  local_mocked_bindings(embed = fake_embed(8L), .package = "tidyllm")

  str_all  <- make_strategy(threshold = 0.0)
  str_high <- make_strategy(threshold = 0.99)

  out_all  <- search_candidates(make_base(), make_target(), "id", "id", str_all)
  out_high <- search_candidates(make_base(), make_target(), "id", "id", str_high)

  expect_true(nrow(out_high) <= nrow(out_all))
})

test_that("search_candidates() returns empty result with correct schema when no matches", {
  # Use a stateful mock: each successive embed() call gets a fresh offset so
  # base vectors (dims 1-3) and target vectors (dims 4-5) are fully orthogonal.
  n_embedded <- 0L
  local_mocked_bindings(
    embed = function(text, model) {
      vecs <- lapply(seq_along(text), function(i) {
        v <- rep(0, 16L); v[[n_embedded + i]] <- 1.0; v
      })
      n_embedded <<- n_embedded + length(text)
      tibble::tibble(input = text, embeddings = vecs)
    },
    .package = "tidyllm"
  )

  str <- make_strategy(threshold = 0.5)
  out <- search_candidates(make_base(), make_target(), "id", "id", str)

  expect_equal(nrow(out), 0L)
  expect_true(all(c("match_id", "score", "source", "id", "rank") %in% names(out)))
})

test_that("search_candidates() errors when weights passed", {
  local_mocked_bindings(embed = fake_embed(4L), .package = "tidyllm")

  str <- make_strategy()
  expect_error(
    search_candidates(make_base(), make_target(), "id", "id", str, weights = c(name = 1)),
    "do not support weights"
  )
})

test_that("search_candidates() includes original columns in output", {
  local_mocked_bindings(embed = fake_embed(8L), .package = "tidyllm")

  str <- make_strategy(threshold = 0.0)
  out <- search_candidates(make_base(), make_target(), "id", "id", str)

  expect_true("name" %in% names(out))
})


# ── detect_duplicates() ───────────────────────────────────────────────────────

test_that("detect_duplicates() returns standard duplicate schema", {
  # Records A and C have identical text → identical unit vectors → score 1
  local_mocked_bindings(
    embed = function(text, model) {
      # "alpha beta" always gets vector [1,0,0,0]; others get orthogonal vectors
      vecs <- lapply(text, function(t) {
        if (t == "alpha beta") c(1, 0, 0, 0) else c(0, 1, 0, 0)
      })
      tibble::tibble(input = text, embeddings = vecs)
    },
    .package = "tidyllm"
  )

  str <- make_strategy(threshold = 0.9)
  out <- detect_duplicates(make_base(), id = "id", strategy = str)

  expect_true(data.table::is.data.table(out))
  expect_true(all(c("duplicate_group", "score", "id", "rank") %in% names(out)))
  # A and C are duplicates; B is not matched → only A and C in output
  expect_setequal(out$id, c("A", "C"))
  expect_equal(length(unique(out$duplicate_group)), 1L)
})

test_that("detect_duplicates() returns empty result with correct schema when no pairs exceed threshold", {
  local_mocked_bindings(
    embed = function(text, model) {
      # All orthogonal unit vectors → score 0 for all pairs
      vecs <- lapply(seq_along(text), function(i) {
        v <- rep(0, 4L); v[[i]] <- 1.0; v
      })
      tibble::tibble(input = text, embeddings = vecs)
    },
    .package = "tidyllm"
  )

  dt  <- make_base()[1:3]
  str <- make_strategy(threshold = 0.9)
  out <- detect_duplicates(dt, id = "id", strategy = str)

  expect_equal(nrow(out), 0L)
  expect_true(all(c("duplicate_group", "score", "id", "rank") %in% names(out)))
})

# ── block_by behaviour (data.table) ──────────────────────────────────────────

test_that("score_embeddings() with block_by only scores same-block pairs", {
  # All-identical vectors: without blocking every pair would score 1.0.
  base_emb <- data.table::data.table(
    id        = c("A", "B"),
    embedding = list(c(1, 0, 0), c(1, 0, 0)),
    city      = c("Berlin", "Hamburg")
  )
  tgt_emb <- data.table::data.table(
    id        = c("X", "Y"),
    embedding = list(c(1, 0, 0), c(1, 0, 0)),
    city      = c("Berlin", "Munich")
  )
  str <- make_strategy(block_by = "city")

  out <- score_embeddings(base_emb, tgt_emb, str)

  # Only Berlin↔Berlin pairs should appear (A↔X). Hamburg has no target match;
  # Munich has no base match.
  expect_equal(nrow(out), 1L)
  expect_equal(out$base_id, "A")
  expect_equal(out$target_id, "X")
})

test_that("score_embeddings() with block_by errors on missing block column", {
  base_emb <- data.table::data.table(
    id = "A", embedding = list(c(1, 0, 0)), city = "Berlin"
  )
  tgt_emb <- data.table::data.table(
    id = "X", embedding = list(c(1, 0, 0))  # no city column
  )
  str <- make_strategy(block_by = "city")

  expect_error(
    score_embeddings(base_emb, tgt_emb, str),
    "Blocking columns missing from target_embeddings"
  )
})

test_that("search_candidates() with block_by filters cross-block pairs", {
  # Identical vectors for everyone: without blocking all 3×2 = 6 pairs match.
  local_mocked_bindings(
    embed = function(text, model) {
      tibble::tibble(
        input = text,
        embeddings = lapply(text, function(t) c(1, 0, 0, 0))
      )
    },
    .package = "tidyllm"
  )

  str <- make_strategy(threshold = 0.5, block_by = "city")
  out <- search_candidates(make_base(), make_target(), "id", "id", str)

  base_ids   <- unique(out[source == "base",   id])
  target_ids <- unique(out[source == "target", id])
  # base = (A Berlin, B Hamburg, C Berlin); target = (X Berlin, Y Munich)
  # Hamburg base (B) and Munich target (Y) have no within-block partner.
  expect_false("B" %in% base_ids)
  expect_false("Y" %in% target_ids)
})

test_that("detect_duplicates() with block_by groups only within blocks", {
  # All-identical vectors → without blocking, A/B/C would form one group.
  local_mocked_bindings(
    embed = function(text, model) {
      tibble::tibble(
        input = text,
        embeddings = lapply(text, function(t) c(1, 0, 0, 0))
      )
    },
    .package = "tidyllm"
  )

  str <- make_strategy(threshold = 0.9, block_by = "city")
  out <- detect_duplicates(make_base(), id = "id", strategy = str)

  # A and C share city "Berlin"; B is alone in Hamburg.
  expect_setequal(out$id, c("A", "C"))
  expect_equal(length(unique(out$duplicate_group)), 1L)
})


test_that("detect_duplicates() rank 1 is highest score within group", {
  local_mocked_bindings(
    embed = function(text, model) {
      vecs <- lapply(text, function(t) c(1, 0, 0, 0))
      tibble::tibble(input = text, embeddings = vecs)
    },
    .package = "tidyllm"
  )

  dt <- data.table::data.table(
    id   = c("A", "B", "C"),
    name = c("same", "same", "same")
  )
  str <- make_strategy(threshold = 0.5)
  out <- detect_duplicates(dt, id = "id", strategy = str)

  expect_equal(out$rank[[1L]], 1L)
  expect_true(out$score[[1L]] >= out$score[[nrow(out)]])
})
