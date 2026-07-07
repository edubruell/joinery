# Stages 02/03 of the v1.1 subword queue: find_subwords() + Subword_Model
# (fit) and subword_tokens() via the tokenize() protocol (apply).
# Plans: notes/v1.1/02_find_subwords.md, notes/v1.1/03_subword_tokens.md.

skip_if_not_installed("sentencepiece")

# Shared fixture corpus, passed through the same normalization the strategies
# use (the fit must see what the apply step will see). Contains both compound
# and split spellings so the trainer learns pieces that appear in both.
sp_corpus <- function() {
  normalize_text(rep(c(
    "maschinenbau mueller", "gesellschaft schulz", "tischlerei schmidt",
    "holzbau wagner", "metallbau krause", "schreinerei huber",
    "holzbaugesellschaft franke", "metallbaugesellschaft otto",
    "gesellschaft fuer holzbau", "wagner und soehne"
  ), 40))
}

fit_test_model <- function(...) {
  find_subwords(sp_corpus(), vocab_size = 90, ...)
}

# ---------------------------------------------------------------------------
# Fit: find_subwords() / Subword_Model
# ---------------------------------------------------------------------------

test_that("find_subwords() returns a populated Subword_Model", {
  sw <- fit_test_model()
  expect_true(S7::S7_inherits(sw, joinery:::Subword_Model))
  expect_true(is.raw(sw@model_bytes) && length(sw@model_bytes) > 0)
  expect_identical(sw@algorithm, "bpe")
  expect_true(sw@vocab_size > 0)
  expect_true(nrow(sw@vocabulary) == sw@vocab_size)
  expect_false(sw@keep_boundary)
  expect_output(print(sw), "Subword_Model")
})

test_that("the fitted model survives saveRDS and rehydrates from bytes alone", {
  sw <- fit_test_model()
  before <- subword_tokens("MASCHINENBAUGESELLSCHAFT MUELLER", sw)

  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(sw, path)
  restored <- readRDS(path)

  # Clear the session cache so the restored model must rehydrate from its
  # stored bytes, not reuse the pointer loaded for `sw`.
  cache <- joinery:::.sp_cache
  rm(list = ls(cache), envir = cache)

  after <- subword_tokens("MASCHINENBAUGESELLSCHAFT MUELLER", restored)
  expect_identical(before, after)
})

test_that("the rehydration cache serves the same loaded model on repeat use", {
  sw <- fit_test_model()
  first  <- joinery:::.sp_load(sw)
  second <- joinery:::.sp_load(sw)
  expect_identical(first, second)
})

test_that("bpe and unigram both train and differ", {
  # The unigram trainer supports a smaller maximum vocabulary than bpe on a
  # tiny corpus; 40 works for both here.
  bpe <- find_subwords(sp_corpus(), vocab_size = 40, algorithm = "bpe")
  uni <- find_subwords(sp_corpus(), vocab_size = 40, algorithm = "unigram")
  expect_identical(uni@algorithm, "unigram")
  expect_false(identical(bpe@model_bytes, uni@model_bytes))
})

test_that("an impossible vocab_size fails with the friendly training hint", {
  expect_error(
    find_subwords(sp_corpus(), vocab_size = 10000),
    "vocab_size.*more training text|Training the subword vocabulary failed"
  )
})

test_that("find_subwords() validates its inputs", {
  expect_error(find_subwords(sp_corpus(), vocab_size = 2), "vocab_size")
  expect_error(find_subwords(sp_corpus(), columns = "name"), "table input")
  expect_error(find_subwords(character(0)), "no non-empty text")
  expect_error(find_subwords(c(NA_character_, "", " x")[1:2]), "no non-empty text")
})

test_that("find_subwords() on a data.frame pulls the named columns", {
  df <- data.frame(name = sp_corpus(), other = seq_along(sp_corpus()))
  sw <- find_subwords(df, vocab_size = 80, columns = "name")
  expect_true(sw@vocab_size > 0)
  expect_error(find_subwords(df, columns = "missing_col"), "not found")
  expect_error(find_subwords(df), "columns")
})

test_that("find_subwords() dispatches on data.table and tibble input", {
  dt <- data.table::data.table(name = sp_corpus())
  sw_dt <- find_subwords(dt, vocab_size = 80, columns = "name")
  expect_true(S7::S7_inherits(sw_dt, joinery:::Subword_Model))

  skip_if_not_installed("tibble")
  tbl <- tibble::tibble(name = sp_corpus())
  sw_tbl <- find_subwords(tbl, vocab_size = 80, columns = "name")
  expect_identical(sw_dt@vocabulary$subword, sw_tbl@vocabulary$subword)
})

test_that("sample_n caps the training rows", {
  sw <- find_subwords(sp_corpus(), vocab_size = 80, sample_n = 100)
  expect_true(S7::S7_inherits(sw, joinery:::Subword_Model))
})

test_that("find_subwords() aborts with an install hint when sentencepiece is missing", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) FALSE,
    .package = "base"
  )
  expect_error(find_subwords(c("a b", "c d")), "sentencepiece")
})

# ---------------------------------------------------------------------------
# Apply: subword_tokens() / tokenize()
# ---------------------------------------------------------------------------

test_that("subword_tokens() splits compounds into shared pieces", {
  sw <- fit_test_model()
  out <- subword_tokens(
    c("MASCHINENBAUGESELLSCHAFT", "MASCHINENBAU GESELLSCHAFT"), sw
  )
  expect_length(out, 2)
  # The single-word compound and the two-word phrase share pieces.
  expect_true(length(intersect(out[[1]], out[[2]])) > 0)
})

test_that("subword_tokens() equals custom_tokens() on the same model", {
  sw <- fit_test_model()
  txt <- c("HOLZBAU WAGNER KG", "TISCHLEREI SCHMIDT")
  expect_identical(subword_tokens(txt, sw), custom_tokens(txt, sw))
})

test_that("boundary markers strip by default and stay with keep_boundary = TRUE", {
  sw   <- fit_test_model()
  sw_b <- fit_test_model(keep_boundary = TRUE)
  marker <- "\u2581"

  stripped <- unlist(subword_tokens("HOLZBAU WAGNER", sw))
  expect_false(any(grepl(marker, stripped, fixed = TRUE)))
  expect_true(all(nzchar(stripped)))

  kept <- unlist(subword_tokens("HOLZBAU WAGNER", sw_b))
  expect_true(any(grepl(marker, kept, fixed = TRUE)))
})

test_that("NA and empty records yield no tokens", {
  sw <- fit_test_model()
  out <- subword_tokens(c(NA_character_, "", "HOLZBAU"), sw)
  expect_identical(out[[1]], character(0))
  expect_identical(out[[2]], character(0))
  expect_true(length(out[[3]]) > 0)
})

test_that("subword_tokens() rejects a non-model", {
  expect_error(subword_tokens("x", model = list()), "Subword_Model")
})

# ---------------------------------------------------------------------------
# In a strategy: end to end
# ---------------------------------------------------------------------------

subword_fixture <- function() {
  data.frame(
    id = c("a1", "a2", "d1", "d2"),
    firm = c(
      "maschinenbaugesellschaft",    # compound spelling ...
      "maschinenbau gesellschaft",   # ... vs split spelling, no shared word
      "holzbau wagner",              # distractors
      "tischlerei schmidt"
    )
  )
}

test_that("a subword strategy recovers a compound pair word tokens miss", {
  sw <- fit_test_model()
  df <- subword_fixture()

  word_strat <- search_strategy(
    firm ~ normalize_text() + word_tokens(),
    threshold = 0.5
  )
  sub_strat <- search_strategy(
    firm ~ normalize_text() + subword_tokens(sw),
    threshold = 0.5
  )

  word_dups <- detect_duplicates(df, id = "id", strategy = word_strat)
  sub_dups  <- detect_duplicates(df, id = "id", strategy = sub_strat)

  expect_identical(nrow(word_dups), 0L)
  expect_setequal(sub_dups$id, c("a1", "a2"))
})

test_that("subword strategies satisfy the explain_match round-trip", {
  sw <- fit_test_model()
  df <- subword_fixture()
  strat <- search_strategy(
    firm ~ normalize_text() + subword_tokens(sw),
    threshold = 0.5
  )
  dups <- detect_duplicates(df, id = "id", strategy = strat)
  ex <- explain_match(dups, strat, base = df, id = "id",
                      match_id = dups$duplicate_group[1])
  expect_equal(sum(ex@per_column_contrib$contribution), ex@score,
               tolerance = 1e-10)
})

test_that("subword_tokens composes with drop_short_tokens", {
  sw <- fit_test_model()
  strat <- search_strategy(
    firm ~ normalize_text() + subword_tokens(sw) + drop_short_tokens(min_nchar = 2),
    threshold = 0.5
  )
  tok <- prepare_search_data(
    data.table::as.data.table(subword_fixture()), "id", strat
  )
  expect_true(all(nchar(tok$token) >= 2))
})

test_that("the explain_match round-trip holds on the DuckDB backend", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("dbplyr")

  sw <- fit_test_model()
  df <- subword_fixture()
  strat <- search_strategy(
    firm ~ normalize_text() + subword_tokens(sw),
    threshold = 0.5
  )
  duck <- local_duckdb_table(df, "subword_explain_test")
  dups <- detect_duplicates(duck, id = "id", strategy = strat)
  dups_dt <- data.table::as.data.table(dups)
  ex <- explain_match(dups_dt, strat, base = df, id = "id",
                      match_id = dups_dt$duplicate_group[1])
  expect_equal(sum(ex@per_column_contrib$contribution), ex@score,
               tolerance = 1e-10)
})

test_that("subword token tables agree between data.table and DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("dbplyr")

  sw <- fit_test_model()
  df <- subword_fixture()
  strat <- search_strategy(
    firm ~ normalize_text() + subword_tokens(sw),
    threshold = 0.5
  )

  dt_tok <- prepare_search_data(data.table::as.data.table(df), "id", strat)
  duck <- local_duckdb_table(df, "subword_parity_test")
  duck_dt <- data.table::as.data.table(prepare_search_data(duck, "id", strat))

  key_cols <- c("id", "src_column", "token")
  data.table::setkeyv(dt_tok, key_cols)
  data.table::setkeyv(duck_dt, key_cols)
  expect_identical(dt_tok[, ..key_cols], duck_dt[, ..key_cols])
})

test_that("find_subwords() works on a DuckDB table and matches the in-memory fit", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("dbplyr")

  df <- data.frame(name = sp_corpus())
  duck <- local_duckdb_table(df, "subword_fit_test")

  sw_mem  <- find_subwords(df, vocab_size = 80, columns = "name")
  sw_duck <- find_subwords(duck, vocab_size = 80, columns = "name")

  # Same input rows in the same order: the fitted vocabulary must agree.
  expect_identical(sw_mem@vocabulary$subword, sw_duck@vocabulary$subword)
  expect_error(find_subwords(duck, columns = "missing_col"), "not found")
})
