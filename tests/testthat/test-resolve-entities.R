# resolve_entities() — the shared connected-components entity kernel
# (v0.8 Stage 01). Verifies grouping, determinism, rep selection, score
# ranking, the detect_duplicates round-trip, backend parity, and the
# empty-edge contract.

library(data.table)

# Small undirected edge list: {a-b, b-c} and {d-e}, plus isolated f.
make_edges <- function() {
  data.table(
    from  = c("a", "b", "d"),
    to    = c("b", "c", "e"),
    score = c(0.90, 0.80, 0.70)
  )
}

# ---------------------------------------------------------------------------
# 1. Two components + a singleton
# ---------------------------------------------------------------------------

test_that("resolve_entities groups components and keeps singletons", {
  edges <- make_edges()
  res <- resolve_entities(
    edges, "from", "to",
    score    = "score",
    vertices = c("a", "b", "c", "d", "e", "f")
  )

  expect_true(all(c("id", "entity", "rep", "rank", "score") %in% names(res)))
  expect_equal(nrow(res), 6L)

  grp <- split(res$id, res$entity)
  # {a,b,c} together, {d,e} together, {f} alone
  sets <- lapply(grp, sort)
  expect_true(list(c("a", "b", "c")) %in% sets ||
                any(vapply(sets, function(s) identical(s, c("a", "b", "c")), logical(1))))
  expect_true(any(vapply(sets, function(s) identical(s, c("d", "e")), logical(1))))
  expect_true(any(vapply(sets, function(s) identical(s, "f"), logical(1))))

  # entity ids dense and globally unique
  expect_setequal(unique(res$entity), seq_len(length(unique(res$entity))))
  # singleton f: rank 1, rep self
  f <- res[id == "f"]
  expect_equal(f$rank, 1L)
  expect_equal(f$rep, "f")
})

# ---------------------------------------------------------------------------
# 2. Order invariance (determinism)
# ---------------------------------------------------------------------------

test_that("output is invariant to edge row order", {
  edges <- make_edges()
  v <- c("a", "b", "c", "d", "e", "f")

  r1 <- resolve_entities(edges, "from", "to", score = "score", vertices = v)
  set.seed(1); shuffled <- edges[sample(.N)]
  r2 <- resolve_entities(shuffled, "from", "to", score = "score", vertices = v)

  setkeyv(r1, "id"); setkeyv(r2, "id")
  expect_equal(r1, r2)
})

# ---------------------------------------------------------------------------
# 3. rep_by picks the intended representative
# ---------------------------------------------------------------------------

test_that("rep_by selects the min-priority member, id tie-break", {
  edges <- data.table(from = c("a", "b"), to = c("b", "c"))  # one component {a,b,c}
  # priority: c is preferred (0), a and b are not (1) -> rep should be c
  verts <- data.table(
    id       = c("a", "b", "c"),
    priority = c(1L, 1L, 0L)
  )
  res <- resolve_entities(edges, "from", "to", vertices = verts, rep_by = "priority")

  expect_equal(unique(res$entity), 1L)
  expect_equal(unique(res$rep), "c")
  expect_equal(res[rank == 1L, id], "c")
})

# ---------------------------------------------------------------------------
# 4. score ranking
# ---------------------------------------------------------------------------

test_that("rank follows descending best score", {
  # a-b (0.9), b-c (0.5): best scores a=0.9, b=0.9, c=0.5
  edges <- data.table(from = c("a", "b"), to = c("b", "c"),
                      score = c(0.9, 0.5))
  res <- resolve_entities(edges, "from", "to", score = "score")
  setkeyv(res, "id")

  expect_equal(res[id == "c", rank], 3L)       # lowest best score -> last
  expect_equal(res[rank == 1L, score], 0.9)
  # within-entity rank strictly follows -score then id
  ord <- res[order(rank)]
  expect_true(all(diff(ord$score) <= 0))
})

# ---------------------------------------------------------------------------
# 5. Round-trip against detect_duplicates (data.table)
# ---------------------------------------------------------------------------

test_that("detect_duplicates grouping matches a direct resolve_entities call", {
  base <- data.table(
    id   = c("a", "b", "c", "d", "e"),
    name = c("alpha", "alpha", "alpha", "beta", "beta")
  )
  strat <- search_strategy(
    name ~ normalize_text + word_tokens(min_nchar = 3),
    weights   = c(name = 1),
    threshold = 0.5
  )
  dup <- detect_duplicates(base, "id", strat)

  # two true groups: {a,b,c} and {d,e}
  by_group <- dup[, .(ids = list(sort(id))), by = duplicate_group]
  sets <- lapply(by_group$ids, identity)
  expect_true(any(vapply(sets, function(s) identical(s, c("a", "b", "c")), logical(1))))
  expect_true(any(vapply(sets, function(s) identical(s, c("d", "e")), logical(1))))
  # rank 1 present in every group
  expect_true(all(dup[, any(rank == 1L), by = duplicate_group]$V1))
})

# ---------------------------------------------------------------------------
# 6. Backend parity (data.table vs DuckDB)
# ---------------------------------------------------------------------------

test_that("data.table and DuckDB resolve to the same grouping", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  edges <- make_edges()                      # {a,b,c}, {d,e}; no singletons here
  v <- c("a", "b", "c", "d", "e")

  r_dt <- resolve_entities(edges, "from", "to", score = "score", vertices = v)

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "edges_in", as.data.frame(edges))
  r_duck <- resolve_entities(
    dplyr::tbl(con, "edges_in"), "from", "to", score = "score"
  ) |> dplyr::collect() |> data.table::as.data.table()

  # compare groupings as sets of id-sets (entity labels may differ)
  sets_dt   <- lapply(split(r_dt$id, r_dt$entity), sort)
  sets_duck <- lapply(split(r_duck$id, r_duck$entity), sort)
  norm <- function(s) sort(vapply(s, paste, collapse = ",", FUN.VALUE = character(1)))
  expect_equal(norm(sets_dt), norm(sets_duck))

  # rep agreement per id
  setkeyv(r_dt, "id"); setkeyv(r_duck, "id")
  expect_equal(r_dt[id %in% r_duck$id, rep], r_duck[, rep])
})

# ---------------------------------------------------------------------------
# 7. Empty edges
# ---------------------------------------------------------------------------

test_that("empty edges with vertices yields all singletons; without, empty", {
  empty <- data.table(from = character(), to = character(), score = numeric())

  # with vertices -> each id its own entity
  res <- resolve_entities(empty, "from", "to", score = "score",
                          vertices = c("a", "b", "c"))
  expect_equal(nrow(res), 3L)
  expect_true(all(res$rank == 1L))
  expect_equal(sort(res$rep), c("a", "b", "c"))
  expect_setequal(res$entity, 1:3)

  # without vertices -> zero rows, correct schema/types
  res0 <- resolve_entities(empty, "from", "to", score = "score")
  expect_equal(nrow(res0), 0L)
  expect_true(all(c("id", "entity", "rep", "rank", "score") %in% names(res0)))
  expect_type(res0$entity, "integer")
})
