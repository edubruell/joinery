# Multi-source staged entity resolution (v0.8 Stage 07).
#
# multi_stage_search() pools heterogeneous records (multiple sources, or
# multiple vintages of one source), links the same entity across a generic
# `source_by` axis via successive directed-search passes, and resolves the
# accumulated directed ledger into a cross-source ENTITY GROUPING (one row per
# record) via resolve_entities() — the shared output tail. The directed ledger
# rides on the "ledger" attribute. These tests cover: entity-grouping output;
# directed self-search; collapse-and-continue working-set shrink; cross-source
# drift linkage; ledger covered-source reconstruction; rebind = accumulate; the
# generic source axis; and data.table/DuckDB parity.

library(data.table)

pool_fixture <- function() {
  data.table(
    id     = c("a1", "a2", "a3", "b1", "b2", "c1"),
    year = c("2010", "2011", "2012", "2010", "2011", "2010"),
    name   = c("anna meier gmbh", "anna meier gmbh", "anna meier ag",
               "bert klee handel", "bert klee handel", "cara low solo")
  )
}

# ---------------------------------------------------------------------------
# 1. Entity grouping is the deliverable (not loose pairs); ledger rides along
# ---------------------------------------------------------------------------

test_that("the default loop resolves to an entity grouping with a ledger", {
  p <- pool_fixture()
  s <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.6)
  out <- multi_stage_search(p, p, "id", "id", list(only = s),
                            self = TRUE, source_by = "year")

  expect_true(all(c("entity", "id", "rep", "rank", "score", "source",
                    "covered_sources", "n_in_entity", "stage") %in% names(out)))
  expect_equal(nrow(out), 6L)                       # one row per pooled record
  led <- attr(out, "ledger")
  expect_false(is.null(led))
  expect_true(all(c("from", "to", "stage", "score", "source_from",
                    "source_to", "within_source", "direction") %in% names(led)))

  # a1/a2 (identical name, different years) land in one entity.
  expect_equal(out[id == "a1"]$entity, out[id == "a2"]$entity)
})

# ---------------------------------------------------------------------------
# 2. Self-search stays a directed search (source-pair on the edge)
# ---------------------------------------------------------------------------

test_that("self-search edges are directed and carry the source-pair", {
  p <- pool_fixture()
  s <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.6)
  out <- multi_stage_search(p, p, "id", "id", list(only = s),
                            self = TRUE, source_by = "year")
  led <- attr(out, "ledger")

  # No self-loops; every edge has a source-pair; cross-year edges are tagged.
  expect_true(all(led$from != led$to))
  expect_false(any(is.na(led$source_from)))
  expect_false(any(is.na(led$source_to)))
  ab <- led[(from == "a1" & to == "a2") | (from == "a2" & to == "a1")]
  expect_gt(nrow(ab), 0L)
  expect_true(all(ab$within_source == FALSE))       # 2010 vs 2011
})

# ---------------------------------------------------------------------------
# 3. Collapse-and-continue shrinks the working set
# ---------------------------------------------------------------------------

test_that("collapse = 'rep' bridges a drift chain that 'none' leaves split", {
  # a3 ("anna meier ag") drifts from a1/a2 ("anna meier gmbh"): the tight stage
  # links a1~a2; only a carried representative lets the loose stage attach a3.
  p <- pool_fixture()
  tight <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.9)
  loose <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.4)

  none <- multi_stage_search(p, p, "id", "id", list(t = tight, l = loose),
                             self = TRUE, source_by = "year", collapse = "none")
  rep  <- multi_stage_search(p, p, "id", "id", list(t = tight, l = loose),
                             self = TRUE, source_by = "year", collapse = "rep")

  # rep keeps the bridge -> a1/a2/a3 one entity; none drops both endpoints ->
  # a3 cannot attach, so it stays its own entity (strictly more entities).
  expect_equal(rep[id == "a1"]$entity, rep[id == "a3"]$entity)
  expect_true(none[id == "a3"]$entity != none[id == "a1"]$entity)
  expect_gt(uniqueN(none$entity), uniqueN(rep$entity))
})

# ---------------------------------------------------------------------------
# 4. Additive cross-source drift links across stages
# ---------------------------------------------------------------------------

test_that("a tight then looser stage links a drifted cross-source record", {
  p <- data.table(
    id     = c("x10", "x11", "x12"),
    year = c("2010", "2011", "2012"),
    name   = c("schmidt bau gmbh", "schmidt bau gmbh", "schmidt bau ag muenchen")
  )
  tight <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.95)
  loose <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.4)
  out <- multi_stage_search(p, p, "id", "id", list(tight = tight, loose = loose),
                            self = TRUE, source_by = "year", collapse = "rep")
  expect_equal(uniqueN(out$entity), 1L)             # all three drift into one
})

# ---------------------------------------------------------------------------
# 5. Ledger reconstructs covered-sources; no double counting
# ---------------------------------------------------------------------------

test_that("covered_sources counts distinct sources per entity", {
  p <- pool_fixture()
  tight <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.9)
  loose <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.4)
  out <- multi_stage_search(p, p, "id", "id", list(t = tight, l = loose),
                            self = TRUE, source_by = "year", collapse = "rep")
  # a1(2010)/a2(2011)/a3(2012) → 3 distinct sources; c1 singleton → 1.
  a_ent <- out[id == "a1"]$entity
  expect_equal(out[id == "a3"]$entity, a_ent)            # a3 bridged in
  expect_equal(unique(out[entity == a_ent]$covered_sources), 3L)
  expect_equal(out[id == "c1"]$covered_sources, 1L)
  expect_equal(out[id == "c1"]$n_in_entity, 1L)
})

# ---------------------------------------------------------------------------
# 6. Generic source axis: relabelling "source" leaves the entities identical
# ---------------------------------------------------------------------------

test_that("source_by is generic — register axis works like a year axis", {
  p <- pool_fixture()
  s <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.6)
  by_year <- multi_stage_search(p, p, "id", "id", list(only = s),
                                self = TRUE, source_by = "year")

  p2 <- copy(p); data.table::setnames(p2, "year", "register")
  by_reg <- multi_stage_search(p2, p2, "id", "id", list(only = s),
                               self = TRUE, source_by = "register")

  comembers <- function(o) sort(o[, .(m = paste(sort(id), collapse = ",")),
                                  by = entity]$m)
  expect_equal(comembers(by_reg), comembers(by_year))
})

# ---------------------------------------------------------------------------
# 7. collapse = 'union' is rejected (not implemented), bad axes validated
# ---------------------------------------------------------------------------

test_that("unimplemented policy combinations abort clearly", {
  p <- pool_fixture()
  s <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.6)
  expect_error(
    multi_stage_search(p, p, "id", "id", list(only = s),
                       self = TRUE, collapse = "union"),
    "union"
  )
  # rep_rule beyond canonical is honestly gated, not silently downgraded.
  expect_error(
    multi_stage_search(p, p, "id", "id", list(only = s),
                       self = TRUE, rep_rule = "newest"),
    "rep_rule"
  )
})

# ---------------------------------------------------------------------------
# 8. Backend parity: data.table vs DuckDB resolve to the same entities
# ---------------------------------------------------------------------------

test_that("data.table and DuckDB self-search produce the same entities", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")
  con <- local_duckdb_con()

  p <- pool_fixture()
  s <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.6)

  dt_out <- multi_stage_search(p, p, "id", "id", list(only = s),
                               self = TRUE, source_by = "year")

  DBI::dbWriteTable(con, "pool", as.data.frame(p))
  duck_res <- multi_stage_search(dplyr::tbl(con, "pool"), dplyr::tbl(con, "pool"),
                                 "id", "id", list(only = s),
                                 self = TRUE, source_by = "year")
  duck_out <- duck_res |> dplyr::collect() |> as.data.table()

  comembers <- function(o) sort(o[, .(m = paste(sort(id), collapse = ",")),
                                  by = entity]$m)
  expect_equal(comembers(duck_out), comembers(dt_out))

  # The directed ledger round-trips as a collectable DuckDB tbl attribute.
  led <- attr(duck_res, "ledger")
  expect_false(is.null(led))
  led_dt <- as.data.table(dplyr::collect(led))
  expect_true(all(c("from", "to", "stage", "score", "source_from",
                    "source_to") %in% names(led_dt)))
  expect_setequal(led_dt$from, attr(dt_out, "ledger")$from)
})

# ---------------------------------------------------------------------------
# 9. compare_stages() reads the ledger off the grouping
# ---------------------------------------------------------------------------

test_that("compare_stages() consumes the multi_stage_search grouping via its ledger", {
  p <- pool_fixture()
  tight <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.9)
  loose <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.5)
  out <- multi_stage_search(p, p, "id", "id", list(tight = tight, loose = loose),
                            self = TRUE, source_by = "year")
  cmp <- compare_stages(out)
  expect_s3_class(cmp, "joinery::Stage_Comparison")
})
