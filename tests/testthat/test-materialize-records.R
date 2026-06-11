# materialize_records() — rehydrate-by-id, the positive (semi-join)
# complement of extract_unmatched() (v0.8 Stage 02). Verifies the
# extract_unmatched round-trip, vector/table polymorphism, the "id"-column
# lookup, id-type tolerance, missing-id drop (semi-join, not left join),
# the empty-ids schema contract, and the DuckDB temp-table-JOIN guard
# against the §20 IN-list footgun.

library(data.table)

make_corpus <- function() {
  data.table(
    rec   = c("a", "b", "c", "d", "e"),
    name  = c("Anna", "Bert", "Cara", "Dora", "Egon"),
    city  = c("BER", "HAM", "BER", "MUC", "HAM")
  )
}

# ---------------------------------------------------------------------------
# 1. Round-trip with extract_unmatched (data.table)
# ---------------------------------------------------------------------------

test_that("materialize_records rehydrates an extract_unmatched residual", {
  corpus  <- make_corpus()
  matches <- data.table(id = c("a", "c"))

  residual <- extract_unmatched(corpus, "rec", matches)   # b, d, e
  out      <- materialize_records(corpus, "rec", residual)

  expect_setequal(out$rec, c("b", "d", "e"))
  # all original columns intact
  expect_setequal(names(out), names(corpus))
  # exactly the residual rows, full payload
  setkey(out, rec)
  expect_equal(out[rec == "d", name], "Dora")
})

# ---------------------------------------------------------------------------
# 2. Vector ids == one-column-table ids
# ---------------------------------------------------------------------------

test_that("a bare vector and a one-column table give the same result", {
  corpus <- make_corpus()

  from_vec <- materialize_records(corpus, "rec", c("a", "e"))
  from_tbl <- materialize_records(corpus, "rec", data.table(id = c("a", "e")))

  setkey(from_vec, rec); setkey(from_tbl, rec)
  expect_equal(from_vec, from_tbl)
  expect_setequal(from_vec$rec, c("a", "e"))
})

# ---------------------------------------------------------------------------
# 3. Table ids with an "id" column (resolve_entities output convention)
# ---------------------------------------------------------------------------

test_that("ids read from the 'id' column, not a same-named column", {
  corpus <- make_corpus()
  # carry both an "id" column and a "rec" column; "id" wins
  ent <- data.table(id = c("b", "c"), rec = c("zzz", "yyy"), entity = 1:2)

  out <- materialize_records(corpus, "rec", ent)
  expect_setequal(out$rec, c("b", "c"))
})

# ---------------------------------------------------------------------------
# 4. Id-type tolerance (numeric corpus, character request)
# ---------------------------------------------------------------------------

test_that("numeric-id corpus matches a character id request", {
  corpus <- data.table(rec = c(10L, 20L, 30L), name = c("X", "Y", "Z"))

  out <- materialize_records(corpus, "rec", c("20", "30"))
  expect_setequal(out$name, c("Y", "Z"))
})

# ---------------------------------------------------------------------------
# 5. Missing ids silently dropped (semi-join, not left join)
# ---------------------------------------------------------------------------

test_that("ids absent from the corpus produce no rows", {
  corpus <- make_corpus()

  out <- materialize_records(corpus, "rec", c("a", "ZZ", "QQ"))
  expect_equal(nrow(out), 1L)
  expect_equal(out$rec, "a")
  # never NULL-filled rows for absent ids
  expect_false(anyNA(out$name))
})

# ---------------------------------------------------------------------------
# 6. Empty ids -> zero rows, full schema
# ---------------------------------------------------------------------------

test_that("empty ids returns a zero-row table with the corpus schema", {
  corpus <- make_corpus()
  out <- materialize_records(corpus, "rec", character())
  expect_equal(nrow(out), 0L)
  expect_setequal(names(out), names(corpus))
})

# ---------------------------------------------------------------------------
# 7. DuckDB backend parity + temp-table JOIN (no IN-list footgun)
# ---------------------------------------------------------------------------

test_that("DuckDB materialize_records semi-joins via a temp table, no IN-list", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  corpus <- make_corpus()
  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "corpus", as.data.frame(corpus))

  out <- materialize_records(
    dplyr::tbl(con, "corpus"), "rec", c("b", "d", "e")
  ) |> dplyr::collect() |> data.table::as.data.table()

  expect_setequal(out$rec, c("b", "d", "e"))
  expect_setequal(names(out), names(corpus))
  expect_false(anyNA(out$name))
})

test_that("DuckDB ids supplied as a tbl are joined, not collected", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  corpus <- make_corpus()
  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "corpus", as.data.frame(corpus))
  DBI::dbWriteTable(con, "ids_in", data.frame(id = c("a", "c")))

  out <- materialize_records(
    dplyr::tbl(con, "corpus"), "rec", dplyr::tbl(con, "ids_in")
  ) |> dplyr::collect() |> data.table::as.data.table()

  expect_setequal(out$rec, c("a", "c"))
})

test_that("the generated DuckDB fetch SQL is a JOIN, never an IN-list (§20)", {
  # Structural pin on the contract, independent of timing: the fetch must
  # be a temp-table JOIN and must contain no `id IN (<literal>)`.
  sql <- joinery:::.materialize_join_sql(
    "_out", "corpus", "_joinery_ids_123", '"rec"'
  )
  expect_match(sql, "JOIN")
  expect_match(sql, "ON CAST\\(d\\.\"rec\" AS VARCHAR\\) = r\\.id")
  expect_no_match(sql, "IN \\(")
})

test_that("DuckDB >10k-id fetch completes fast via JOIN (§20 regression)", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  n <- 50000L
  corpus <- data.table(rec = as.character(seq_len(n)),
                       payload = seq_len(n))
  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "big", as.data.frame(corpus))

  ids <- as.character(sample.int(n, 20000L))
  t <- system.time({
    out <- materialize_records(dplyr::tbl(con, "big"), "rec", ids) |>
      dplyr::collect()
  })
  expect_equal(nrow(out), length(unique(ids)))
  # The IN-list footgun pinned cores for >11 min on 84k ids; a temp-table
  # JOIN on 20k returns in well under a second. Generous ceiling.
  expect_lt(t[["elapsed"]], 10)
})
