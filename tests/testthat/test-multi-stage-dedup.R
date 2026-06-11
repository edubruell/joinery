# Staged dedup over a single table (v0.8 Stage 06).
#
# multi_stage_dedup() runs an ordered list of strategies as successive dedup
# passes, accumulates the links each stage finds, and resolves connected
# components ONCE at the end — so a record linked A~B in an early stage and B~C
# in a later, looser stage closes into a single entity {A,B,C}. These tests
# cover: cross-stage transitive closure (the headline guard); the residual
# invariant; an exact front stage reached purely by S7 dispatch; rep_by
# canonicalisation; edge_filter; and that the verb composes the shipped
# primitives (no private CC / rehydrate / exact branch).

library(data.table)

# ---------------------------------------------------------------------------
# 1. Cross-stage transitive closure (the headline guard)
# ---------------------------------------------------------------------------

test_that("links from different stages close into one entity", {
  # A~B share (plz, wz) and name; B~C share only plz and a drifted name.
  # Stage 1 (tight block plz+wz) links A~B; stage 2 (loose block plz) links B~C.
  # The final CC must union them into {A,B,C}.
  dt <- data.table(
    id   = c("A", "B", "C"),
    plz  = c("10", "10", "10"),
    wz   = c("x",  "x",  "y"),
    name = c("anna meier gmbh", "anna meier gmbh", "anna meier ag")
  )
  tight <- search_strategy(name ~ normalize_text() + word_tokens(),
                           block_by = c("plz", "wz"), threshold = 0.9)
  loose <- search_strategy(name ~ normalize_text() + word_tokens(),
                           block_by = "plz", threshold = 0.5)

  out <- multi_stage_dedup(dt, "id", list(tight = tight, loose = loose))

  expect_equal(uniqueN(out$duplicate_group), 1L)   # one entity, not two
  expect_setequal(out$id, c("A", "B", "C"))

  # A single tight-block pass alone cannot reach C.
  one <- detect_duplicates(dt, "id", tight)
  expect_false("C" %in% one$id)
})

# ---------------------------------------------------------------------------
# 2. Residual invariant: a one-shot record survives as no-match
# ---------------------------------------------------------------------------

test_that("records in no multi-record group are not emitted", {
  dt <- data.table(
    id   = c("A", "B", "Z"),
    plz  = c("10", "10", "99"),
    name = c("anna meier", "anna meier", "zzz unique singleton")
  )
  s <- search_strategy(name ~ normalize_text() + word_tokens(),
                       block_by = "plz", threshold = 0.5)
  out <- multi_stage_dedup(dt, "id", list(only = s))
  expect_setequal(out$id, c("A", "B"))             # Z never linked -> dropped
  expect_false("Z" %in% out$id)
})

# ---------------------------------------------------------------------------
# 3. Exact front stage via strategy class (S7 dispatch, no exact branch)
# ---------------------------------------------------------------------------

test_that("an exact_strategy front stage collapses identical records", {
  dt <- data.table(
    id   = c("A", "B", "C", "D"),
    plz  = c("10", "10", "10", "10"),
    name = c("anna meier", "anna meier", "bert klee gmbh", "bert klee ag")
  )
  ex   <- exact_strategy(name ~ normalize_text() + word_tokens(), block_by = "plz")
  fuzz <- search_strategy(name ~ normalize_text() + word_tokens(),
                          block_by = "plz", threshold = 0.5)

  out <- multi_stage_dedup(dt, "id", list(exact = ex, fuzzy = fuzz))

  # A,B are an exact pair (score 1.0, stage "exact"); C,D a fuzzy pair.
  ab <- out[id %in% c("A", "B")]
  expect_equal(uniqueN(ab$duplicate_group), 1L)
  expect_true(all(ab$stage == "exact"))
  expect_equal(unique(ab$score), 1.0)

  cd <- out[id %in% c("C", "D")]
  expect_equal(uniqueN(cd$duplicate_group), 1L)
  expect_true(all(cd$stage == "fuzzy"))
})

# ---------------------------------------------------------------------------
# 4. rep_by canonicalisation picks the intended representative
# ---------------------------------------------------------------------------

test_that("rep_by overrides the id tiebreak for the representative", {
  # B sorts AFTER A by id, so without rep_by the id-canonical rep is A. Give B
  # the smaller rep_by so the rep_by rule must flip the rep to B — proving
  # rep_by actually took effect (not the id tiebreak in disguise).
  dt <- data.table(
    id   = c("A", "B"),
    plz  = c("10", "10"),
    name = c("anna meier", "anna meier"),
    span = c(5L, 1L)                                  # B has the smaller rep_by
  )
  s <- search_strategy(name ~ normalize_text() + word_tokens(),
                       block_by = "plz", threshold = 0.5)

  # Identical scores -> without rep_by the rep is the id-canonical A.
  base <- multi_stage_dedup(dt, "id", list(only = s))
  expect_equal(base[rank == 1L]$id, "A")

  # rep_by = span flips the rep to B (smallest span wins).
  out <- multi_stage_dedup(dt, "id", list(only = s), rep_by = "span")
  expect_equal(out[rank == 1L]$id, "B")
})

# ---------------------------------------------------------------------------
# 5. edge_filter drops only the targeted edge
# ---------------------------------------------------------------------------

test_that("edge_filter removes a single edge without disturbing the rest", {
  dt <- data.table(
    id   = c("A", "B", "C"),
    plz  = c("10", "10", "10"),
    name = c("anna meier", "anna meier", "anna meier")
  )
  s <- search_strategy(name ~ normalize_text() + word_tokens(),
                       block_by = "plz", threshold = 0.5)
  # Drop any edge that touches C -> C is no longer in a group.
  drop_c <- function(edges, stage) edges[from != "C" & to != "C"]
  out <- multi_stage_dedup(dt, "id", list(only = s), edge_filter = drop_c)
  expect_setequal(out$id, c("A", "B"))
  expect_false("C" %in% out$id)
})

# ---------------------------------------------------------------------------
# 6. Empty result has the standard schema
# ---------------------------------------------------------------------------

test_that("no links returns the standard dedup schema with zero rows", {
  dt <- data.table(
    id   = c("A", "B"),
    plz  = c("10", "20"),
    name = c("anna meier", "completely different")
  )
  s <- search_strategy(name ~ normalize_text() + word_tokens(),
                       block_by = "plz", threshold = 0.9)
  out <- multi_stage_dedup(dt, "id", list(only = s))
  expect_equal(nrow(out), 0L)
  expect_true(all(c("duplicate_group", "id", "score", "rank", "stage") %in% names(out)))
})

# ---------------------------------------------------------------------------
# 7. Validation
# ---------------------------------------------------------------------------

test_that("invalid strategy lists abort", {
  dt <- data.table(id = "A", name = "x")
  expect_error(multi_stage_dedup(dt, "id", list()), "non-empty")
  expect_error(multi_stage_dedup(dt, "id", list(1L)), "Exact_Strategy")
})

# ---------------------------------------------------------------------------
# 8. Backend parity: data.table vs DuckDB resolve to the same entities
# ---------------------------------------------------------------------------

test_that("data.table and DuckDB produce the same entities", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")
  con <- local_duckdb_con()

  dt <- data.table(
    id   = c("A", "B", "C", "D", "E"),
    plz  = c("10", "10", "10", "20", "20"),
    wz   = c("x",  "x",  "y",  "z",  "z"),
    name = c("anna meier gmbh", "anna meier gmbh", "anna meier ag",
             "bert klee", "bert klee")
  )
  tight <- search_strategy(name ~ normalize_text() + word_tokens(),
                           block_by = c("plz", "wz"), threshold = 0.9)
  loose <- search_strategy(name ~ normalize_text() + word_tokens(),
                           block_by = "plz", threshold = 0.5)
  strategies <- list(tight = tight, loose = loose)

  dt_out <- multi_stage_dedup(dt, "id", strategies)

  DBI::dbWriteTable(con, "d", as.data.frame(dt))
  duck_out <- multi_stage_dedup(dplyr::tbl(con, "d"), "id", strategies) |>
    dplyr::collect() |> as.data.table()

  # Same id -> same set of co-members (entity labels may differ in value).
  comembers <- function(o) {
    o[, .(mates = paste(sort(id), collapse = ",")), by = duplicate_group][
      , unique(mates)] |> sort()
  }
  expect_equal(comembers(duck_out), comembers(dt_out))
  expect_setequal(duck_out$id, dt_out$id)
})
