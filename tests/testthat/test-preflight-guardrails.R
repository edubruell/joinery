# D1 — est_comparisons_too_high recommendation (audit signal)
# D2 — id-uniqueness pre-flight surfaced once (not per DuckDB batch)
# (The opt-in `max_comparisons` ceiling was superseded by the always-on token
#  fan-out guard — see test-fanout-guard.R.)

# ---- D1(a): audit_strategy surfaces est_comparisons -------------------------

test_that("audit_strategy fires est_comparisons_too_high on a doomed single block", {
  dt <- data.table::data.table(
    id   = 1:60000,
    name = sample(c("a", "b"), 60000, replace = TRUE)
  )
  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1), threshold = 0.9)
  a <- audit_strategy(dt, "id", s)
  expect_true(a@est_comparisons > 1e9)
  expect_true("est_comparisons_too_high" %in% attr(a, "recommendation_ids"))
})

test_that("audit_strategy does not fire the ceiling when blocking makes it tractable", {
  dt <- data.table::data.table(
    id    = 1:60000,
    name  = sample(c("a", "b"), 60000, replace = TRUE),
    block = sample(1:6000, 60000, replace = TRUE)   # ~10 records/block
  )
  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1),
                       block_by = "block", threshold = 0.9)
  a <- audit_strategy(dt, "id", s)
  expect_false("est_comparisons_too_high" %in% attr(a, "recommendation_ids"))
})

# ---- D2: id-uniqueness pre-flight -------------------------------------------

test_that(".warn_nonunique_id warns only when ids actually collide", {
  expect_warning(.warn_nonunique_id(3L, "id"), "not unique")
  expect_silent(.warn_nonunique_id(0L, "id"))
  expect_silent(.warn_nonunique_id(NA_integer_, "id"))
})

test_that("data.table detect_duplicates warns exactly once on a non-unique id", {
  dt <- data.table::data.table(
    id   = c(1, 1, 2, 3),
    name = c("anna", "anna", "bert", "carl")
  )
  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1), threshold = 0.5)
  warns <- character()
  withCallingHandlers(
    suppressMessages(detect_duplicates(dt, "id", s)),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  expect_equal(sum(grepl("not unique", warns)), 1L)
})

test_that("DuckDB detect_duplicates warns once globally (not per batch)", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dt <- data.frame(id = c(1, 1, 2, 3, 4, 5),
                   name = c("anna", "anna", "bert", "carl", "dora", "emil"),
                   stringsAsFactors = FALSE)
  duckdb::dbWriteTable(con, "t", dt)
  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1), threshold = 0.5)
  warns <- character()
  withCallingHandlers(
    suppressMessages(invisible(dplyr::collect(
      detect_duplicates(dplyr::tbl(con, "t"), "id", s)
    ))),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  expect_equal(sum(grepl("not unique", warns)), 1L)
})
