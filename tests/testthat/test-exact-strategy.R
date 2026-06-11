# Exact_Strategy (v0.8 Stage 03) — exact (score-1.0) token-set matching that
# permeates the standard verbs by dispatch. detect_duplicates() is the dedup
# face, search_candidates() the cross face; both return the standard schema
# with score == 1.0. Verifies engine-faithful fingerprinting (the Ü->UE
# footgun), the standard output schemas, empty-column robustness (§25),
# residual via extract_unmatched(), containment, and data.table/DuckDB parity.

library(data.table)

ex_strat <- exact_strategy(
  name   ~ normalize_text() + word_tokens(),
  street ~ normalize_text() + word_tokens()
)
fz_strat <- search_strategy(
  name   ~ normalize_text() + word_tokens(),
  street ~ normalize_text() + word_tokens(),
  threshold = 0.9
)

# ---------------------------------------------------------------------------
# 1. detect_duplicates(exact) == the fuzzy score-1.0 set (footgun guard)
# ---------------------------------------------------------------------------

test_that("exact dedup reproduces the fuzzy score-1.0 cliques, not strip_accents", {
  d <- data.table(
    id     = c("r1", "r2", "r3"),
    name   = c("Müller", "Mueller", "Muller"),    # r1/r2 collapse, r3 differs
    street = c("Hauptstr", "Hauptstr", "Hauptstr")
  )

  exact <- detect_duplicates(d, "id", ex_strat)
  fuzzy <- detect_duplicates(d, "id", fz_strat)

  expect_setequal(names(exact), c("duplicate_group", "id", "score", "rank",
                                  "name", "street"))
  expect_true(all(exact$score == 1.0))

  exact_ids <- sort(unique(exact$id))
  fuzzy_one <- sort(unique(fuzzy[abs(score - 1.0) < 1e-9, id]))
  expect_equal(exact_ids, fuzzy_one)
  expect_false("r3" %in% exact_ids)              # De-ASCII keeps UE vs U apart
})

# ---------------------------------------------------------------------------
# 2. Standard dedup schema + resolve_entities grouping
# ---------------------------------------------------------------------------

test_that("exact dedup groups identical-token cliques with rank/group", {
  d <- data.table(
    id     = c("a", "b", "c", "d"),
    name   = c("Anna", "Anna", "Anna", "Zoe"),
    street = c("Weg 1", "Weg 1", "Weg 1", "See 9")
  )
  dups <- detect_duplicates(d, "id", ex_strat)
  # a,b,c form one duplicate_group; d is a non-duplicate (dropped)
  expect_setequal(dups$id, c("a", "b", "c"))
  expect_equal(length(unique(dups$duplicate_group)), 1L)
  expect_equal(sort(dups$rank), 1:3)
})

# ---------------------------------------------------------------------------
# 3. Empty-column robustness (§25): exact links what fuzzy threshold rejects
# ---------------------------------------------------------------------------

test_that("identical name + both-empty street dedups under exact, not fuzzy", {
  d <- data.table(
    id     = c("x", "y"),
    name   = c("Schmidt", "Schmidt"),
    street = c("", "")
  )
  exact <- detect_duplicates(d, "id", ex_strat)
  expect_setequal(exact$id, c("x", "y"))         # exact links them

  fuzzy <- detect_duplicates(d, "id", fz_strat)  # 0.5 name-only < 0.9 -> nothing
  expect_equal(nrow(fuzzy), 0L)
})

# ---------------------------------------------------------------------------
# 4. Cross form via search_candidates + the residual round-trip
# ---------------------------------------------------------------------------

test_that("exact search yields the standard candidate schema, score 1.0", {
  base <- data.table(id = c("b1", "b2"),
                     name = c("Anna Meier", "Bert Klein"),
                     street = c("Hauptstr 1", "Ringweg 2"))
  targ <- data.table(id = c("t1", "t2"),
                     name = c("Anna Meier", "Zoe Funk"),
                     street = c("Hauptstr 1", "Seeweg 9"))

  cand <- search_candidates(base, targ, "id", "id", ex_strat)
  expect_true(all(c("match_id", "score", "source", "id", "rank") %in% names(cand)))
  expect_true(all(cand$score == 1.0))
  expect_equal(nrow(cand), 2L)                    # one pair, base+target rows
  expect_setequal(cand$id, c("b1", "t1"))
})

test_that("exact dedup residual feeds the next stage via extract_unmatched", {
  d <- data.table(
    id     = c("a", "b", "c", "d"),
    name   = c("Anna", "Anna", "Bert", "Cara"),
    street = c("Weg 1", "Weg 1", "Ring 2", "Park 3")
  )
  dups  <- detect_duplicates(d, "id", ex_strat)        # a,b dedup
  resid <- extract_unmatched(d, "id", dups)
  # exact removed the duplicate group members; residual is the complement
  expect_false(anyNA(resid$id))
  expect_true(all(c("c", "d") %in% resid$id))
  expect_length(intersect(resid$id, dups[rank > 1L, id]), 0L)
})

# ---------------------------------------------------------------------------
# 5. Containment forward / bidirectional + min_base_rarity guard
# ---------------------------------------------------------------------------

test_that("forward containment links a contained base, off does not", {
  base <- data.table(id = "b1", name = "Anna Meier", street = "Hauptstr")
  targ <- data.table(id = "t1", name = "Anna Meier Gmbh", street = "Hauptstr 12")

  off <- search_candidates(base, targ, "id", "id", ex_strat)
  expect_equal(nrow(off), 0L)

  fwd_strat <- exact_strategy(name ~ normalize_text() + word_tokens(),
                              street ~ normalize_text() + word_tokens(),
                              containment = "forward")
  fwd <- search_candidates(base, targ, "id", "id", fwd_strat)
  expect_equal(nrow(fwd), 2L)
  expect_equal(fwd[source == "base", id], "b1")
})

test_that("bidirectional catches the reverse; guard drops low-info base", {
  base <- data.table(id = "b1", name = "Anna Meier Gmbh", street = "Hauptstr 12")
  targ <- data.table(id = "t1", name = "Anna Meier", street = "Hauptstr")

  fwd_strat <- exact_strategy(name ~ normalize_text() + word_tokens(),
                              street ~ normalize_text() + word_tokens(),
                              containment = "forward")
  expect_equal(nrow(search_candidates(base, targ, "id", "id", fwd_strat)), 0L)

  bid_strat <- exact_strategy(name ~ normalize_text() + word_tokens(),
                              street ~ normalize_text() + word_tokens(),
                              containment = "bidirectional")
  expect_equal(nrow(search_candidates(base, targ, "id", "id", bid_strat)), 2L)

  gated_strat <- exact_strategy(name ~ normalize_text() + word_tokens(),
                                street ~ normalize_text() + word_tokens(),
                                containment = "forward", min_base_rarity = 1e6)
  base2 <- data.table(id = "b1", name = "Meier", street = "")
  targ2 <- data.table(id = "t1", name = "Meier Anna Gmbh", street = "Hauptstr")
  expect_equal(nrow(search_candidates(base2, targ2, "id", "id", gated_strat)), 0L)
})

# ---------------------------------------------------------------------------
# 6. exact_strategy() constructor + print
# ---------------------------------------------------------------------------

test_that("exact_strategy validates containment and min_base_rarity", {
  expect_error(exact_strategy(name ~ normalize_text(), containment = "nope"))
  expect_error(exact_strategy(name ~ normalize_text(), min_base_rarity = -1))
  s <- exact_strategy(name ~ normalize_text(), block_by = "plz")
  expect_s3_class(s, "joinery::Exact_Strategy")
  expect_equal(s@block_by, "plz")
})

# ---------------------------------------------------------------------------
# 7. Backend parity (data.table vs DuckDB) — dedup + cross + containment
# ---------------------------------------------------------------------------

test_that("exact dedup is identical on data.table and DuckDB (incl. Ü)", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  # b1/b3 (Müller/Mueller -> MUELLER) link; b4 (Muller -> MULLER) must not.
  d <- data.table(id = c("b1", "b2", "b3", "b4"),
                  name = c("Müller", "Bert Klein", "Mueller", "Muller"),
                  street = c("Hauptstr 1", "Ringweg 2", "Hauptstr 1", "Hauptstr 1"))

  dt_dups <- detect_duplicates(d, "id", ex_strat)

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "d", as.data.frame(d))
  dk_dups <- detect_duplicates(dplyr::tbl(con, "d"), "id", ex_strat) |>
    dplyr::collect() |> data.table::as.data.table()

  expect_setequal(dt_dups$id, dk_dups$id)
  expect_true(all(c("b1", "b3") %in% dk_dups$id))
  expect_false("b4" %in% dk_dups$id)
  expect_true(all(dk_dups$score == 1.0))         # entity rows carry score 1.0
  # grouping agrees (entity labels may differ; compare id-sets)
  norm <- function(x) sort(vapply(split(x$id, x$duplicate_group),
                                  function(s) paste(sort(s), collapse = ","),
                                  character(1)))
  expect_equal(norm(dt_dups), norm(dk_dups))
})

test_that("DuckDB exact dedup is empty-column robust and block-aware", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  # §25 on DuckDB: identical name + both-empty street must dedup. Plus a
  # block_by that exercises the per-block resolve_entities recursion: w/z
  # share a name but sit in different blocks, so they must NOT link.
  d <- data.table(
    id     = c("x", "y", "w", "z"),
    plz    = c("10", "10", "20", "30"),
    name   = c("Schmidt", "Schmidt", "Klein", "Klein"),
    street = c("", "", "Ring 1", "Ring 1")
  )
  blk_strat <- exact_strategy(
    name   ~ normalize_text() + word_tokens(),
    street ~ normalize_text() + word_tokens(),
    block_by = "plz"
  )

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "d", as.data.frame(d))
  dk <- detect_duplicates(dplyr::tbl(con, "d"), "id", blk_strat) |>
    dplyr::collect() |> data.table::as.data.table()

  expect_setequal(dk$id, c("x", "y"))            # empty-street pair dedups
  expect_true(all(dk$score == 1.0))
  expect_false(any(c("w", "z") %in% dk$id))      # different blocks -> no link
  expect_equal(length(unique(dk$duplicate_group)), 1L)
})

test_that("exact search + forward containment parity on DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  base <- data.table(id = "b1", name = "Anna Meier", street = "Hauptstr")
  targ <- data.table(id = "t1", name = "Anna Meier Gmbh", street = "Hauptstr 12")
  fwd_strat <- exact_strategy(name ~ normalize_text() + word_tokens(),
                              street ~ normalize_text() + word_tokens(),
                              containment = "forward")

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "base", as.data.frame(base))
  DBI::dbWriteTable(con, "targ", as.data.frame(targ))
  cand <- search_candidates(dplyr::tbl(con, "base"), dplyr::tbl(con, "targ"),
                            "id", "id", fwd_strat) |> dplyr::collect()
  expect_equal(nrow(cand), 2L)
  expect_true(all(cand$score == 1.0))
  expect_setequal(cand$id, c("b1", "t1"))
})
