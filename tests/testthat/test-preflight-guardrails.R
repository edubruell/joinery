# D1 — est_comparisons_too_high recommendation + opt-in max_comparisons ceiling
# D2 — id-uniqueness pre-flight surfaced once (not per DuckDB batch)

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

# ---- D1(b): opt-in max_comparisons ceiling ----------------------------------

test_that(".estimate_self_comparisons sums per-block n*(n-1)/2", {
  expect_equal(.estimate_self_comparisons(10), 45)
  expect_equal(.estimate_self_comparisons(c(10, 10)), 90)
  expect_equal(.estimate_self_comparisons(1), 0)
})

test_that("detect_duplicates aborts over the comparison budget, passes under it", {
  dt <- data.table::data.table(
    id   = 1:5000,
    name = sample(c("a", "b"), 5000, replace = TRUE)
  )
  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1), threshold = 0.9)
  # 5000 records -> ~12.5M comparisons
  expect_error(detect_duplicates(dt, "id", s, max_comparisons = 1e6),
               "exceed")
  # generous budget: runs to completion
  expect_silent(suppressMessages(
    detect_duplicates(dt, "id", s, max_comparisons = 1e9)
  ))
  # default Inf: no ceiling
  expect_silent(suppressMessages(detect_duplicates(dt, "id", s)))
})

test_that("the blocked ceiling abort names the worst offender blocks", {
  dt <- data.table::data.table(
    id   = 1:30000,
    name = sample(c("a", "b"), 30000, replace = TRUE),
    blk  = sample(c("X", "Y", "Z"), 30000, replace = TRUE, prob = c(.6, .3, .1))
  )
  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1),
                       block_by = "blk", threshold = 0.9)
  expect_error(
    detect_duplicates(dt, "id", s, max_comparisons = 1e6),
    "records"   # the per-block offender breakdown is included
  )
})

test_that("DuckDB detect_duplicates honours max_comparisons", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dt <- data.frame(id = 1:5000,
                   name = sample(c("a", "b"), 5000, replace = TRUE),
                   stringsAsFactors = FALSE)
  duckdb::dbWriteTable(con, "big", dt)
  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1), threshold = 0.9)
  expect_error(
    detect_duplicates(dplyr::tbl(con, "big"), "id", s, max_comparisons = 1e6),
    "exceed"
  )
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
