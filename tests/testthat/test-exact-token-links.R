# exact_token_links() — exact (score-1.0) token-set prefilter (v0.8 Stage
# 03). Verifies engine-faithful fingerprinting (the Ü->UE footgun guard),
# the residual = base \ matched complement, empty-column robustness (§25),
# the cross form, containment (forward + bidirectional + min_base_rarity
# guard), and data.table/DuckDB parity.

library(data.table)

# A strategy over name + street, single block-free.
strat <- search_strategy(
  name   ~ normalize_text() + word_tokens(),
  street ~ normalize_text() + word_tokens(),
  threshold = 0.9
)

# ---------------------------------------------------------------------------
# 1. Engine parity — the footgun guard (Ü -> UE, not strip_accents Ü -> U)
# ---------------------------------------------------------------------------

test_that("self-form links == the detect_duplicates score-1.0 set", {
  d <- data.table(
    id     = c("r1", "r2", "r3"),
    name   = c("Müller", "Mueller", "Muller"),   # r1/r2 collapse, r3 differs
    street = c("Hauptstr", "Hauptstr", "Hauptstr")
  )

  ex <- exact_token_links(d, strat, base_id = "id")

  # detect_duplicates pairs scoring exactly 1.0
  dups <- detect_duplicates(d, "id", strat)
  one_pairs <- dups[abs(score - 1.0) < 1e-9, sort(unique(id))]

  link_ids <- sort(unique(c(ex$links$id_a, ex$links$id_b)))
  expect_equal(link_ids, one_pairs)
  # r1<->r2 link present, neither links to r3 (De-ASCII keeps UE vs U apart)
  expect_true(nrow(ex$links[id_a == "r1" & id_b == "r2"]) == 1L)
  expect_false("r3" %in% link_ids)
})

# ---------------------------------------------------------------------------
# 2. Residual is the exact complement (disjoint, exhaustive)
# ---------------------------------------------------------------------------

test_that("matched ids and residual ids partition all ids", {
  d <- data.table(
    id     = c("a", "b", "c", "d"),
    name   = c("Anna", "Anna", "Bert", "Cara"),
    street = c("Weg 1", "Weg 1", "Ring 2", "Ring 2")
  )
  ex <- exact_token_links(d, strat, base_id = "id")

  matched <- unique(c(ex$links$id_a, ex$links$id_b))
  expect_length(intersect(matched, ex$residual$ids), 0L)        # disjoint
  expect_setequal(c(matched, ex$residual$ids), d$id)             # exhaustive
})

# ---------------------------------------------------------------------------
# 3. Empty-column robustness (§25): identical name, both-empty street
# ---------------------------------------------------------------------------

test_that("identical name + both-empty street is an exact link", {
  d <- data.table(
    id     = c("x", "y"),
    name   = c("Schmidt", "Schmidt"),
    street = c("", "")              # empty token set on both sides
  )
  ex <- exact_token_links(d, strat, base_id = "id")
  expect_equal(nrow(ex$links), 1L)
  expect_equal(sort(c(ex$links$id_a, ex$links$id_b)), c("x", "y"))
})

# ---------------------------------------------------------------------------
# 4. Cross form
# ---------------------------------------------------------------------------

test_that("cross form links persisters and reports both residuals", {
  base <- data.table(id = c("b1", "b2"),
                     name = c("Anna Meier", "Bert Klein"),
                     street = c("Hauptstr 1", "Ringweg 2"))
  targ <- data.table(id = c("t1", "t2"),
                     name = c("Anna Meier", "Zoe Funk"),
                     street = c("Hauptstr 1", "Seeweg 9"))

  ex <- exact_token_links(base, strat, target = targ,
                          base_id = "id", target_id = "id")

  expect_equal(nrow(ex$links), 1L)
  expect_equal(ex$links$base_id, "b1")
  expect_equal(ex$links$target_id, "t1")
  expect_setequal(ex$residual$base, "b2")
  expect_setequal(ex$residual$target, "t2")
})

# ---------------------------------------------------------------------------
# 5. Containment forward (base ⊂ target), absent under "off"
# ---------------------------------------------------------------------------

test_that("forward containment links a contained base, not under off", {
  base <- data.table(id = "b1", name = "Anna Meier", street = "Hauptstr")
  targ <- data.table(id = "t1", name = "Anna Meier Gmbh",
                     street = "Hauptstr 12")          # superset of base tokens

  off <- exact_token_links(base, strat, target = targ,
                           base_id = "id", target_id = "id")
  expect_equal(nrow(off$links), 0L)

  fwd <- exact_token_links(base, strat, target = targ,
                           base_id = "id", target_id = "id",
                           containment = "forward")
  expect_equal(nrow(fwd$links), 1L)
  expect_equal(fwd$links$base_id, "b1")
})

# ---------------------------------------------------------------------------
# 6. Containment bidirectional + min_base_rarity guard
# ---------------------------------------------------------------------------

test_that("bidirectional adds the reverse pair; guard drops low-info base", {
  # base richer than target -> target ⊆ base -> only the reverse direction
  base <- data.table(id = "b1", name = "Anna Meier Gmbh", street = "Hauptstr 12")
  targ <- data.table(id = "t1", name = "Anna Meier", street = "Hauptstr")

  fwd <- exact_token_links(base, strat, target = targ,
                           base_id = "id", target_id = "id",
                           containment = "forward")
  expect_equal(nrow(fwd$links), 0L)          # base not ⊆ target

  bid <- exact_token_links(base, strat, target = targ,
                           base_id = "id", target_id = "id",
                           containment = "bidirectional")
  expect_equal(nrow(bid$links), 1L)          # target ⊆ base caught

  # guard: a 1-token base has tiny rarity mass -> excluded by a high floor
  base2 <- data.table(id = "b1", name = "Meier", street = "")
  targ2 <- data.table(id = "t1", name = "Meier Anna Gmbh", street = "Hauptstr")
  gated <- exact_token_links(base2, strat, target = targ2,
                             base_id = "id", target_id = "id",
                             containment = "forward",
                             min_base_rarity = 1e6)
  expect_equal(nrow(gated$links), 0L)
})

# ---------------------------------------------------------------------------
# 7. Self-form links drop into resolve_entities()
# ---------------------------------------------------------------------------

test_that("self-form links feed resolve_entities to reproduce 1.0 cliques", {
  d <- data.table(
    id     = c("a", "b", "c", "d"),
    name   = c("Anna", "Anna", "Anna", "Zoe"),
    street = c("Weg 1", "Weg 1", "Weg 1", "See 9")
  )
  ex  <- exact_token_links(d, strat, base_id = "id")
  ent <- resolve_entities(ex$links, "id_a", "id_b",
                          vertices = d$id)
  # a,b,c collapse into one entity; d alone
  grp <- split(ent$id, ent$entity)
  sets <- lapply(grp, sort)
  expect_true(any(vapply(sets, function(s) identical(s, c("a","b","c")), logical(1))))
  expect_true(any(vapply(sets, function(s) identical(s, "d"), logical(1))))
})

# ---------------------------------------------------------------------------
# 8. Backend parity (data.table vs DuckDB) — off + containment
# ---------------------------------------------------------------------------

test_that("data.table and DuckDB produce identical links", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  # Include an accented record: b1/b3 (Müller/Mueller -> MUELLER) must link,
  # b4 (Muller -> MULLER) must NOT. This is the cross-backend footgun guard —
  # if DuckDB ever tokenized via strip_accents (Ü->U) instead of the R
  # normalize_text path (Ü->UE), b4 would wrongly join and parity would break.
  base <- data.table(id = c("b1", "b2", "b3", "b4"),
                     name = c("Müller", "Bert Klein", "Mueller", "Muller"),
                     street = c("Hauptstr 1", "Ringweg 2", "Hauptstr 1", "Hauptstr 1"))

  ex_dt <- exact_token_links(base, strat, base_id = "id")
  # sanity: the accented pair links, the strip-collision record does not
  dt_ids <- unique(c(ex_dt$links$id_a, ex_dt$links$id_b))
  expect_true(all(c("b1", "b3") %in% dt_ids))
  expect_false("b4" %in% dt_ids)

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "base", as.data.frame(base))
  ex_dk <- exact_token_links(dplyr::tbl(con, "base"), strat, base_id = "id")
  dk_links <- ex_dk$links |> dplyr::collect() |> data.table::as.data.table()
  dk_resid <- ex_dk$residual$ids |> dplyr::collect()

  norm <- function(dt) sort(paste(dt$id_a, dt$id_b, sep = "|"))
  expect_equal(norm(ex_dt$links), norm(dk_links))
  expect_setequal(ex_dt$residual$ids, dk_resid$id)
})

test_that("DuckDB cross form + forward containment parity", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  base <- data.table(id = "b1", name = "Anna Meier", street = "Hauptstr")
  targ <- data.table(id = "t1", name = "Anna Meier Gmbh", street = "Hauptstr 12")

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "base", as.data.frame(base))
  DBI::dbWriteTable(con, "targ", as.data.frame(targ))

  ex <- exact_token_links(dplyr::tbl(con, "base"), strat,
                          target = dplyr::tbl(con, "targ"),
                          base_id = "id", target_id = "id",
                          containment = "forward")
  links <- ex$links |> dplyr::collect()
  expect_equal(nrow(links), 1L)
  expect_equal(links$base_id, "b1")
  expect_equal(links$target_id, "t1")
})
