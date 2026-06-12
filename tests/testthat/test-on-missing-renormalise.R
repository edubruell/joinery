# B1 / §25 — empty-column score ceiling and the on_missing = "renormalise" knob.
#
# A record missing a weighted column c has a hard score ceiling of 1 - weight(c)
# under the default "penalise" policy (the column's weight stays in the
# denominator). "renormalise" redistributes the weight of any column empty on
# BOTH records across the columns present on either side, so two records that
# agree on every present column score 1.0 regardless of which columns are empty.

w <- c(name = 0.6, street = 0.4)

make_strat <- function(on_missing = "penalise", threshold = 0.9) {
  search_strategy(
    name   ~ word_tokens(),
    street ~ word_tokens(),
    weights    = w,
    threshold  = threshold,
    on_missing = on_missing
  )
}

# id 1,2: identical name, street empty on BOTH  -> ceiling 0.6 under penalise
# id 3,4: identical name, identical street      -> full 1.0 either way
toy <- function() {
  data.table::data.table(
    id     = 1:4,
    name   = c("ANNA MUELLER", "ANNA MUELLER", "BERT KLEIN", "BERT KLEIN"),
    street = c("", "", "HAUPTSTRASSE", "HAUPTSTRASSE")
  )
}

test_that("on_missing defaults to penalise and is validated", {
  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1))
  expect_equal(s@on_missing, "penalise")
  expect_error(
    search_strategy(name ~ word_tokens(), on_missing = "nonsense")
  )
})

test_that("data.table: penalise caps empty-column pairs below the threshold", {
  res <- detect_duplicates(data.table::copy(toy()), "id", make_strat("penalise"))
  dt  <- data.table::as.data.table(res)
  # only the present-street pair (3,4) is flagged at threshold 0.9
  expect_setequal(unique(dt$id), c("3", "4"))
  expect_equal(unique(dt$score), 1.0, tolerance = 1e-9)
})

test_that("data.table: renormalise lifts empty-column pairs to 1.0", {
  res <- detect_duplicates(data.table::copy(toy()), "id", make_strat("renormalise"))
  dt  <- data.table::as.data.table(res)
  expect_setequal(unique(dt$id), c("1", "2", "3", "4"))
  expect_equal(unique(dt$score), 1.0, tolerance = 1e-9)
})

test_that("data.table: a one-sided-present column stays a genuine penalty", {
  # street present on both but DIFFERENT -> not empty-on-both -> stays in denom.
  # name matches (0.6), street contributes 0 -> score 0.6 even under renormalise.
  d <- data.table::data.table(
    id     = 1:2,
    name   = c("ANNA MUELLER", "ANNA MUELLER"),
    street = c("HAUPTSTRASSE", "WALDWEG")
  )
  res <- detect_duplicates(d, "id", make_strat("renormalise", threshold = 0.1))
  dt  <- data.table::as.data.table(res)
  expect_equal(unique(dt$score), 0.6, tolerance = 1e-9)
})

test_that("explain_match round-trip holds exactly under renormalise", {
  s   <- make_strat("renormalise", threshold = 0.1)
  d   <- toy()
  m   <- detect_duplicates(data.table::copy(d), "id", s)
  for (g in unique(data.table::as.data.table(m)$duplicate_group)) {
    ex <- explain_match(m, s, base = d, id = "id", match_id = g)
    expect_equal(ex@score, sum(ex@per_column_contrib$contribution), tolerance = 1e-10)
  }
})

test_that("renormalise works on the search (cross) face", {
  base <- data.table::data.table(
    id = 1:2, name = c("ANNA MUELLER", "BERT KLEIN"), street = c("", "HAUPTSTRASSE")
  )
  target <- data.table::data.table(
    tid = 10:11, name = c("ANNA MUELLER", "BERT KLEIN"), street = c("", "WALDWEG")
  )
  s <- search_strategy(
    name ~ word_tokens(), street ~ word_tokens(),
    weights = w, threshold = 0.9, on_missing = "renormalise"
  )
  res <- search_candidates(base, target, "id", "tid", s)
  dt  <- data.table::as.data.table(res)
  # ANNA<->ANNA: street empty both -> renormalised to 1.0 (matched at 0.9)
  # BERT<->BERT: street differs -> 0.6 (below 0.9, dropped)
  expect_true(nrow(dt) > 0)
  expect_true(all(dt$score >= 0.9 - 1e-9))
})

test_that("DuckDB: penalise / renormalise match the data.table backend (dedup)", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::dbWriteTable(con, "toy", as.data.frame(toy()))

  for (om in c("penalise", "renormalise")) {
    s  <- make_strat(om)
    dt_res <- data.table::as.data.table(
      detect_duplicates(data.table::copy(toy()), "id", s)
    )
    dk_res <- data.table::as.data.table(dplyr::collect(
      detect_duplicates(dplyr::tbl(con, "toy"), "id", s)
    ))
    expect_setequal(unique(dt_res$id), unique(as.character(dk_res$id)))
    expect_equal(sort(dt_res$score), sort(dk_res$score), tolerance = 1e-9)
  }
})
