# find_stopwords(): corpus-global document-frequency stopword discovery on a
# prepared token table. Covers the data.table path, validation, and (when
# duckdb is available) data.table / DuckDB parity.

make_tokens <- function() {
  set.seed(1)
  n <- 120
  streets <- paste0(
    sample(c("hauptstr", "bahnhofstr", "ringstr"), n, TRUE), " ",
    sample(1:9, n, TRUE), " am an"
  )
  dat <- data.table::data.table(
    id     = as.character(seq_len(n)),
    name   = paste0("firma", seq_len(n)),
    street = streets,
    plz2   = sample(c("68", "69"), n, TRUE)
  )
  strat <- search_strategy(
    name   ~ normalize_text + word_tokens(min_nchar = 3),
    street ~ normalize_text + word_tokens(min_nchar = 1),
    weights = c(name = 0.6, street = 0.4), block_by = "plz2", threshold = 0.9
  )
  list(tok = prepare_search_data(dat, "id", strat), strat = strat)
}

test_that("flags corpus-saturating tokens, leaves discriminating ones", {
  sw <- find_stopwords(make_tokens()$tok, max_prop = 0.3)
  expect_s3_class(sw, "data.table")
  expect_identical(names(sw), c("src_column", "token", "df", "n_records", "prop"))
  # "am"/"an" are in every street; flagged. Names are unique; never flagged.
  expect_true(all(c("AM", "AN") %in% sw$token))
  expect_false("name" %in% sw$src_column)
  expect_true(all(sw$prop >= 0.3))
  # sorted by column then descending prop
  expect_false(is.unsorted(rev(sw[src_column == "street", prop])))
})

test_that("top_n unions with the max_prop cut", {
  tok <- make_tokens()$tok
  sw <- find_stopwords(tok, max_prop = 1, top_n = 2)
  expect_true(all(sw[, .N, by = src_column]$N >= 2))
  expect_true(all(c("AM", "AN") %in% sw[src_column == "street", token]))
})

test_that("empty result keeps the schema", {
  tok <- make_tokens()$tok
  sw <- find_stopwords(tok[src_column == "name"], max_prop = 1)
  expect_equal(nrow(sw), 0L)
  expect_identical(names(sw), c("src_column", "token", "df", "n_records", "prop"))
})

test_that("validation rejects bad max_prop / top_n and non-token tables", {
  tok <- make_tokens()$tok
  expect_error(find_stopwords(tok, max_prop = 0), "max_prop")
  expect_error(find_stopwords(tok, max_prop = 1.5), "max_prop")
  expect_error(find_stopwords(tok, top_n = 0), "top_n")
  expect_error(find_stopwords(data.table::data.table(a = 1)), "token table")
})

test_that("by_block requires explicit block_by and reports worst block", {
  tok <- make_tokens()$tok
  expect_error(find_stopwords(tok, by_block = TRUE), "block_by")
  expect_error(find_stopwords(tok, by_block = TRUE, block_by = "nope"), "not found")
  sw <- find_stopwords(tok, max_prop = 0.3, by_block = TRUE, block_by = "plz2")
  expect_true(all(c("AM", "AN") %in% sw$token))
  expect_equal(uniqueN(sw, by = c("src_column", "token")), nrow(sw))
})

test_that("data.table and DuckDB methods agree", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")
  set.seed(2)
  n <- 150
  dat <- data.table::data.table(
    id     = as.character(seq_len(n)),
    name   = paste0("firma", seq_len(n)),
    street = paste0(sample(c("hauptstr", "ringstr"), n, TRUE), " ",
                    sample(1:5, n, TRUE), " am an"),
    plz2   = "68"
  )
  strat <- search_strategy(
    name   ~ normalize_text + word_tokens(min_nchar = 3),
    street ~ normalize_text + word_tokens(min_nchar = 1),
    weights = c(name = 0.6, street = 0.4), block_by = "plz2", threshold = 0.9
  )
  dt_tok  <- prepare_search_data(dat, "id", strat)
  dt_out  <- find_stopwords(dt_tok, max_prop = 0.3)

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "raw", dat)
  dk_tok <- prepare_search_data(dplyr::tbl(con, "raw"), "id", strat)
  dk_out <- find_stopwords(dk_tok, max_prop = 0.3)

  data.table::setorder(dt_out, src_column, token)
  data.table::setorder(dk_out, src_column, token)
  expect_equal(dt_out$token, dk_out$token)
  expect_equal(dt_out$df, as.integer(dk_out$df))
  expect_equal(dt_out$prop, dk_out$prop, tolerance = 1e-9)
})
