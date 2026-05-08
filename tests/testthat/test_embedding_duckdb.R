skip_if_not_installed("tidyllm")
skip_if_not_installed("duckdb")
skip_if_not_installed("DBI")
skip_if_not_installed("dplyr")

# ── Helpers ───────────────────────────────────────────────────────────────────

# Deterministic embedder: each successive call returns fresh orthogonal unit
# vectors in `dim` dimensions, so all embeddings produced in one test are
# orthogonal unless we explicitly reset the counter.
fake_embed_orthogonal <- function(dim = 8L) {
  i <- 0L
  function(text, model) {
    vecs <- lapply(seq_along(text), function(k) {
      v <- rep(0, dim)
      v[((i + k - 1L) %% dim) + 1L] <- 1.0
      v
    })
    i <<- i + length(text)
    tibble::tibble(input = text, embeddings = vecs)
  }
}

# Embedder keyed by text content: same text → same vector. Useful for testing
# duplicate detection where we want known matches.
fake_embed_by_text <- function(mapping, dim = 4L, default = c(0, 0, 0, 1)) {
  function(text, model) {
    vecs <- lapply(text, function(t) {
      if (!is.null(mapping[[t]])) mapping[[t]] else default
    })
    tibble::tibble(input = text, embeddings = vecs)
  }
}

local_duckdb_emb_con <- function(env = parent.frame()) {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:", array = "matrix")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  # vss extension is required for array_cosine_distance.
  tryCatch(DBI::dbExecute(con, "INSTALL vss;"), error = function(e) NULL)
  res <- tryCatch(DBI::dbExecute(con, "LOAD vss;"), error = function(e) e)
  if (inherits(res, "error")) {
    skip("DuckDB vss extension not available")
  }
  con
}

write_duck_tbl <- function(con, df, name = paste0("t_", sample.int(1e9, 1))) {
  DBI::dbWriteTable(con, name, as.data.frame(df))
  dplyr::tbl(con, name)
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

base_df <- function() {
  data.frame(
    id   = c("A", "B", "C"),
    name = c("alpha beta", "gamma delta", "alpha beta"),
    city = c("Berlin", "Hamburg", "Berlin"),
    stringsAsFactors = FALSE
  )
}

target_df <- function() {
  data.frame(
    id   = c("X", "Y"),
    name = c("alpha beta", "epsilon zeta"),
    city = c("Berlin", "Munich"),
    stringsAsFactors = FALSE
  )
}


# ── compute_embeddings (DuckDB) ───────────────────────────────────────────────

test_that("compute_embeddings() adds FLOAT[dim] column to backing table", {
  local_mocked_bindings(embed = fake_embed_orthogonal(8L), .package = "tidyllm")

  con <- local_duckdb_emb_con()
  tbl <- write_duck_tbl(con, base_df(), "base_tbl")
  str <- make_strategy()

  compute_embeddings(tbl, "id", str)

  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(base_tbl);")
  expect_true("embeddings" %in% cols$name)
  # Embeddings populated for every row
  n_null <- DBI::dbGetQuery(
    con, "SELECT COUNT(*) AS n FROM base_tbl WHERE embeddings IS NULL"
  )$n
  expect_equal(n_null, 0L)
})

test_that("compute_embeddings() is idempotent: second call does not re-embed", {
  call_count <- 0L
  local_mocked_bindings(
    embed = function(text, model) {
      call_count <<- call_count + 1L
      tibble::tibble(
        input = text,
        embeddings = lapply(seq_along(text), function(i) {
          v <- rep(0, 4L); v[[i]] <- 1.0; v
        })
      )
    },
    .package = "tidyllm"
  )

  con <- local_duckdb_emb_con()
  tbl <- write_duck_tbl(con, base_df(), "base_tbl")
  str <- make_strategy()

  compute_embeddings(tbl, "id", str)
  first_calls <- call_count
  compute_embeddings(tbl, "id", str)
  expect_equal(call_count, first_calls)
})

test_that("compute_embeddings() errors on missing blocking column", {
  local_mocked_bindings(embed = fake_embed_orthogonal(4L), .package = "tidyllm")

  con <- local_duckdb_emb_con()
  tbl <- write_duck_tbl(con, base_df(), "base_tbl")
  str <- make_strategy(block_by = "nonexistent")

  expect_error(
    compute_embeddings(tbl, "id", str),
    "Blocking columns not found"
  )
})

test_that("compute_embeddings() batches across multiple embed() calls", {
  call_count <- 0L
  local_mocked_bindings(
    embed = function(text, model) {
      call_count <<- call_count + 1L
      tibble::tibble(
        input = text,
        embeddings = lapply(seq_along(text), function(i) {
          v <- rep(0, 4L); v[[((call_count + i) %% 4L) + 1L]] <- 1.0; v
        })
      )
    },
    .package = "tidyllm"
  )

  con <- local_duckdb_emb_con()
  tbl <- write_duck_tbl(con, base_df(), "base_tbl")
  str <- make_strategy(batch_size = 2L)

  compute_embeddings(tbl, "id", str)
  expect_equal(call_count, 2L)  # ceil(3/2)
})


# ── search_candidates (DuckDB) ────────────────────────────────────────────────

test_that("search_candidates() returns standard match schema", {
  local_mocked_bindings(embed = fake_embed_orthogonal(8L), .package = "tidyllm")

  con <- local_duckdb_emb_con()
  base   <- write_duck_tbl(con, base_df(),   "base_tbl")
  target <- write_duck_tbl(con, target_df(), "target_tbl")
  str    <- make_strategy(threshold = 0.0)  # accept all pairs

  out <- search_candidates(base, target, "id", "id", str)
  out_df <- as.data.frame(out)

  expect_true(all(c("match_id", "score", "source", "id", "rank") %in% names(out_df)))
  expect_setequal(unique(out_df$source), c("base", "target"))
  expect_true(all(out_df$rank >= 1L))
})

test_that("search_candidates() returns empty schema when no pairs clear threshold", {
  # Orthogonal unit vectors → cosine 0 → nothing clears 0.5
  local_mocked_bindings(embed = fake_embed_orthogonal(16L), .package = "tidyllm")

  con <- local_duckdb_emb_con()
  base   <- write_duck_tbl(con, base_df(),   "base_tbl")
  target <- write_duck_tbl(con, target_df(), "target_tbl")
  str    <- make_strategy(threshold = 0.5)

  out_df <- as.data.frame(search_candidates(base, target, "id", "id", str))
  expect_equal(nrow(out_df), 0L)
  expect_true(all(c("match_id", "score", "source", "id", "rank") %in% names(out_df)))
})

test_that("search_candidates() rejects weights argument", {
  local_mocked_bindings(embed = fake_embed_orthogonal(4L), .package = "tidyllm")

  con <- local_duckdb_emb_con()
  base   <- write_duck_tbl(con, base_df(),   "base_tbl")
  target <- write_duck_tbl(con, target_df(), "target_tbl")
  str    <- make_strategy()

  expect_error(
    search_candidates(base, target, "id", "id", str, weights = c(name = 1)),
    "do not support weights"
  )
})

test_that("search_candidates() respects block_by in SQL", {
  # Same vector for all → cosine 1.0 → without blocking, all pairs match.
  local_mocked_bindings(
    embed = function(text, model) {
      tibble::tibble(
        input = text,
        embeddings = lapply(text, function(t) c(1, 0, 0, 0))
      )
    },
    .package = "tidyllm"
  )

  con <- local_duckdb_emb_con()
  base   <- write_duck_tbl(con, base_df(),   "base_tbl")
  target <- write_duck_tbl(con, target_df(), "target_tbl")
  str    <- make_strategy(threshold = 0.9, block_by = "city")

  out_df <- as.data.frame(search_candidates(base, target, "id", "id", str))

  # Pull (base_id, target_id) pairs. Only Berlin ↔ Berlin pairings should remain.
  pairs <- unique(data.frame(
    match_id = out_df$match_id,
    source   = out_df$source,
    id       = out_df$id
  ))
  base_in  <- unique(pairs$id[pairs$source == "base"])
  target_in <- unique(pairs$id[pairs$source == "target"])
  # Hamburg (B) and Munich (Y) must not appear: only Berlin records.
  expect_false("B" %in% base_in)
  expect_false("Y" %in% target_in)
})

test_that("search_candidates() preserves original columns in output", {
  local_mocked_bindings(embed = fake_embed_orthogonal(8L), .package = "tidyllm")

  con <- local_duckdb_emb_con()
  base   <- write_duck_tbl(con, base_df(),   "base_tbl")
  target <- write_duck_tbl(con, target_df(), "target_tbl")
  str    <- make_strategy(threshold = 0.0)

  out_df <- as.data.frame(search_candidates(base, target, "id", "id", str))
  expect_true("name" %in% names(out_df))
  expect_true("city" %in% names(out_df))
})


# ── detect_duplicates (DuckDB) ────────────────────────────────────────────────

test_that("detect_duplicates() groups identical-vector records", {
  # "alpha beta" → [1,0,0,0]; everything else → orthogonal.
  local_mocked_bindings(
    embed = fake_embed_by_text(
      mapping = list(`alpha beta` = c(1, 0, 0, 0)),
      default = c(0, 1, 0, 0)
    ),
    .package = "tidyllm"
  )

  con <- local_duckdb_emb_con()
  tbl <- write_duck_tbl(con, base_df(), "base_tbl")
  str <- make_strategy(threshold = 0.9)

  out_df <- as.data.frame(detect_duplicates(tbl, "id", str))

  expect_true(all(c("duplicate_group", "score", "id", "rank") %in% names(out_df)))
  # A and C share text "alpha beta"; B is alone → only A and C in output.
  expect_setequal(out_df$id, c("A", "C"))
  expect_equal(length(unique(out_df$duplicate_group)), 1L)
})

test_that("detect_duplicates() returns empty schema when no pairs exceed threshold", {
  local_mocked_bindings(embed = fake_embed_orthogonal(8L), .package = "tidyllm")

  con <- local_duckdb_emb_con()
  tbl <- write_duck_tbl(con, base_df(), "base_tbl")
  str <- make_strategy(threshold = 0.9)

  out_df <- as.data.frame(detect_duplicates(tbl, "id", str))
  expect_equal(nrow(out_df), 0L)
  expect_true(all(c("duplicate_group", "score", "id", "rank") %in% names(out_df)))
})

# ── drop_joinery_temp_tables() picks up _joinery_emb_ ─────────────────────────

test_that("drop_joinery_temp_tables() removes _joinery_emb_ tables", {
  con <- local_duckdb_emb_con()

  # Create one table in each prefix space and one outside.
  DBI::dbExecute(con, "CREATE TABLE _joinery_tmp_x AS SELECT 1 AS a;")
  DBI::dbExecute(con, "CREATE TABLE _joinery_emb_y AS SELECT 1 AS a;")
  DBI::dbExecute(con, "CREATE TABLE keep_me AS SELECT 1 AS a;")

  removed <- drop_joinery_temp_tables(con)

  expect_true("_joinery_emb_y" %in% removed)
  expect_true("_joinery_tmp_x" %in% removed)
  expect_false("keep_me" %in% removed)
  expect_true("keep_me" %in% DBI::dbListTables(con))
})


test_that("detect_duplicates() respects block_by", {
  # All same vector — without blocking, A/B/C would all be one group.
  local_mocked_bindings(
    embed = function(text, model) {
      tibble::tibble(
        input = text,
        embeddings = lapply(text, function(t) c(1, 0, 0, 0))
      )
    },
    .package = "tidyllm"
  )

  con <- local_duckdb_emb_con()
  tbl <- write_duck_tbl(con, base_df(), "base_tbl")
  str <- make_strategy(threshold = 0.9, block_by = "city")

  out_df <- as.data.frame(detect_duplicates(tbl, "id", str))
  # A, C share city "Berlin" and should be grouped together. B (Hamburg) alone
  # has no duplicate to pair with and should not appear in the output.
  expect_false("B" %in% out_df$id)
  expect_setequal(out_df$id, c("A", "C"))
})
