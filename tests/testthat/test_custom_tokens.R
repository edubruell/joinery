# Stage 01 of the v1.1 subword queue: the pluggable tokenizer protocol,
# custom_tokens() + the tokenize() generic (notes/v1.1/01_tokenizer_protocol.md).

test_that("custom_tokens() accepts a bare-function tokenizer", {
  split_vowels <- function(text) strsplit(text, "[aeiou]+")
  out <- custom_tokens(c("plexiglas", "brandenburg"), split_vowels)
  expect_identical(out, list(c("pl", "x", "gl", "s"),
                             c("br", "nd", "nb", "rg")))
})

test_that("custom_tokens() dispatches tokenize() on an S7 tokenizer object", {
  Upper_Splitter <- S7::new_class("Upper_Splitter", properties = list(
    min_nchar = S7::class_numeric
  ))
  S7::method(tokenize, Upper_Splitter) <- function(tokenizer, text, ...) {
    word_tokens(toupper(text), min_nchar = tokenizer@min_nchar)
  }
  tk <- Upper_Splitter(min_nchar = 3)
  out <- custom_tokens(c("ab cde", "fgh ij"), tk)
  expect_identical(out, list("CDE", "FGH"))
})

test_that("custom_tokens() validates the list-of-character contract", {
  txt <- c("a b", "c d")
  # not a list
  expect_error(custom_tokens(txt, function(text) "flat"),
               "must return a list")
  # wrong length
  expect_error(custom_tokens(txt, function(text) list("a")),
               "one list element per input string")
  # non-character element
  expect_error(custom_tokens(txt, function(text) list("a", 1L)),
               "character vectors only")
  # non-character input
  expect_error(custom_tokens(1:3, function(text) as.list(text)))
})

test_that("custom_tokens() allows empty token sets per record", {
  drop_all <- function(text) rep(list(character(0)), length(text))
  out <- custom_tokens(c("x", "y"), drop_all)
  expect_identical(out, list(character(0), character(0)))
})

test_that("a fitted tokenizer rides a strategy formula through prepare_search_data", {
  # The stopwords-loop mechanism: the tokenizer object is bound in this
  # environment and referenced by name inside the formula.
  tk <- function(text) word_tokens(text, min_nchar = 4)
  strat <- search_strategy(
    name ~ normalize_text() + custom_tokens(tk),
    threshold = 0.5
  )
  df <- data.frame(
    id = c("r1", "r2"),
    name = c("Tischlerei Schmidt und Co", "Schmidt Soehne")
  )
  tok <- prepare_search_data(data.table::as.data.table(df), "id", strat)
  expect_identical(sort(names(tok)),
                   sort(c("id", "src_column", "token", "row_id")))
  # min_nchar = 4 drops "und", "Co"
  expect_setequal(tok[tok$id == "r1", ][["token"]],
                  c("TISCHLEREI", "SCHMIDT"))
  expect_setequal(tok[tok$id == "r2", ][["token"]],
                  c("SCHMIDT", "SOEHNE"))
})

test_that("an S7 tokenizer object rides a strategy formula (the stage-02 usage)", {
  # A fitted S7 tokenizer bound in this local env, referenced by name in the
  # formula: the exact shape find_subwords()/subword_tokens() will use.
  Fitted_Splitter <- S7::new_class("Fitted_Splitter", properties = list(
    min_nchar = S7::class_numeric
  ))
  S7::method(tokenize, Fitted_Splitter) <- function(tokenizer, text, ...) {
    word_tokens(text, min_nchar = tokenizer@min_nchar)
  }
  fitted <- Fitted_Splitter(min_nchar = 4)
  strat <- search_strategy(
    name ~ normalize_text() + custom_tokens(fitted),
    threshold = 0.5
  )
  df <- data.frame(
    id = c("r1", "r2"),
    name = c("Tischlerei Schmidt und Co", "Schmidt Soehne")
  )
  tok <- prepare_search_data(data.table::as.data.table(df), "id", strat)
  expect_setequal(tok[tok$id == "r1", ][["token"]],
                  c("TISCHLEREI", "SCHMIDT"))
})

test_that("custom_tokens works through the tibble prepare path", {
  skip_if_not_installed("tibble")
  tk <- function(text) word_tokens(text, min_nchar = 4)
  strat <- search_strategy(
    name ~ normalize_text() + custom_tokens(tk),
    threshold = 0.5
  )
  tbl <- tibble::tibble(
    id = c("r1", "r2"),
    name = c("Tischlerei Schmidt und Co", "Schmidt Soehne")
  )
  tok <- prepare_search_data(tbl, "id", strat)
  tok_dt <- data.table::as.data.table(tok)
  expect_setequal(tok_dt[tok_dt$id == "r2", ][["token"]],
                  c("SCHMIDT", "SOEHNE"))
})

test_that("custom_tokens strategies score and satisfy the explain_match round-trip", {
  tk <- function(text) word_tokens(text, min_nchar = 3)
  strat <- search_strategy(
    name ~ normalize_text() + custom_tokens(tk),
    city ~ normalize_text() + word_tokens(min_nchar = 3),
    threshold = 0.3
  )
  base <- data.frame(
    id_b = c("b1", "b2"),
    name = c("Holzbau Wagner", "Metallbau Krause"),
    city = c("Mannheim", "Heidelberg")
  )
  target <- data.frame(
    id_t = c("t1", "t2"),
    name = c("Wagner Holzbau KG", "Krause Metallbautechnik"),
    city = c("Mannheim", "Heidelberg")
  )
  m <- search_candidates(base, target,
                         base_id = "id_b", target_id = "id_t",
                         strategy = strat)
  expect_true(nrow(m) >= 2)
  ex <- explain_match(m, strat,
                      base = base, id = "id_b",
                      target = target, target_id = "id_t",
                      match_id = m$match_id[1])
  contrib <- sum(ex@per_column_contrib$contribution)
  expect_equal(contrib, ex@score, tolerance = 1e-10)
})

test_that("custom_tokens works on the DuckDB backend (parity)", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("dbplyr")

  tk <- function(text) word_tokens(text, min_nchar = 3)
  strat <- search_strategy(
    name ~ normalize_text() + custom_tokens(tk),
    threshold = 0.5
  )
  df <- data.frame(
    id = c("r1", "r2", "r3"),
    name = c("Tischlerei Schmidt", "Schmidt Soehne", "Holzbau Wagner")
  )

  dt_tok <- prepare_search_data(data.table::as.data.table(df), "id", strat)

  duck <- local_duckdb_table(df, "custom_tok_test")
  duck_tok <- prepare_search_data(duck, "id", strat)
  duck_dt <- data.table::as.data.table(duck_tok)

  key_cols <- c("id", "src_column", "token")
  data.table::setkeyv(dt_tok, key_cols)
  data.table::setkeyv(duck_dt, key_cols)
  expect_identical(dt_tok[, ..key_cols], duck_dt[, ..key_cols])
})
