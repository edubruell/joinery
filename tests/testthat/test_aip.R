# ============================================================
# Tests: aIP primitive + auxiliary search-side registry
# ============================================================
#
# References:
#   notes/calibration_design.md  (formula, design intent)
#   Doherr (2023), eq. (9)       (formula source)
# ============================================================

library(data.table)


# ---------- fixtures --------------------------------------------------

make_base <- function() {
  # 6 records. Token "common" appears in all 6, "mid" in 3, "rare" in 1.
  data.table(
    id   = paste0("b", 1:6),
    name = c(
      "common mid rare",
      "common mid",
      "common mid",
      "common",
      "common",
      "common"
    )
  )
}

make_aux <- function() {
  # 4 records. Token "common" appears in 4, "mid" in 2, "novel" in 1.
  data.table(
    id   = paste0("a", 1:4),
    name = c(
      "common mid",
      "common mid",
      "common",
      "common novel"
    )
  )
}

simple_strategy <- function(block_by = NULL) {
  search_strategy(
    name ~ word_tokens(),
    threshold = 0.5,
    block_by  = block_by
  )
}


# ---------- registry: schema + values --------------------------------

test_that("prepare_auxiliary_registry returns documented schema (data.table)", {
  R <- prepare_auxiliary_registry(make_base(), "id", simple_strategy())

  expect_s3_class(R, "data.table")
  expect_equal(names(R), c("src_column", "token", "occ", "maxocc"))
  expect_true(all(R$occ >= 1))
  # maxocc is constant within src_column and equals the column max
  expect_equal(unique(R[src_column == "name", maxocc]), max(R$occ))
})

test_that("auxiliary registry counts distinct records (not raw occurrences)", {
  dt <- data.table(
    id   = c("x1", "x2"),
    name = c("alpha alpha", "alpha")     # two records, both contain alpha
  )
  R <- prepare_auxiliary_registry(dt, "id", simple_strategy())
  expect_equal(R[token == "alpha", occ], 2L)
})


# ---------- aIP formula: hand-worked numeric check -------------------

test_that("compute_aip matches eq. (9) on a hand-worked fixture", {
  R <- prepare_auxiliary_registry(make_base(), "id", simple_strategy())
  A <- prepare_auxiliary_registry(make_aux(),  "id", simple_strategy())
  out <- compute_aip(R, A)

  # Base side: occ(common)=6, occ(mid)=3, occ(rare)=1, maxocc_R=6
  # Aux  side: occ(common)=4, occ(mid)=2, occ(novel)=1, maxocc_A=4

  # common: ratio_R = ln 6 / ln 6 = 1,    ratio_A = ln 4 / ln 4 = 1
  #         aIP = 1 - min(1, 1) = 0
  expect_equal(out[token == "common", aip], 0, tolerance = 1e-12)

  # mid: ratio_R = ln 3 / ln 6, ratio_A = ln 2 / ln 4 = 0.5
  #      min = 0.5; aIP = 0.5
  expect_equal(out[token == "mid", aip], 0.5, tolerance = 1e-12)

  # rare: only in R. occ=1 → ratio_R = ln 1 / ln 6 = 0. aIP = 1.
  expect_equal(out[token == "rare", aip], 1, tolerance = 1e-12)

  # novel: only in A. occ=1 → ratio_A = ln 1 / ln 4 = 0. aIP = 1.
  expect_equal(out[token == "novel", aip], 1, tolerance = 1e-12)
})


# ---------- direction independence -----------------------------------

test_that("aIP is direction-independent for tokens in both registries", {
  R <- prepare_auxiliary_registry(make_base(), "id", simple_strategy())
  A <- prepare_auxiliary_registry(make_aux(),  "id", simple_strategy())

  forward  <- compute_aip(R, A)
  backward <- compute_aip(A, R)

  both <- intersect(forward$token, backward$token)
  f <- forward[token %in% both][order(token)]
  b <- backward[token %in% both][order(token)]
  expect_equal(f$aip, b$aip, tolerance = 1e-12)
})


# ---------- edge cases -----------------------------------------------

test_that("aIP is NA when token appears in neither registry", {
  # Constructed indirectly: merge keeps only tokens present in at least one;
  # but explicit NA inputs to compute_aip propagate to NA aIP.
  out <- joinery:::.aip_eq9(
    occ_R    = NA_integer_,
    maxocc_R = NA_integer_,
    occ_A    = NA_integer_,
    maxocc_A = NA_integer_
  )
  expect_true(is.na(out))
})

test_that("maxocc == 1 collapses ratio to 0 (aIP = 1)", {
  out <- joinery:::.aip_eq9(occ_R = 1L, maxocc_R = 1L, occ_A = NA, maxocc_A = NA)
  expect_equal(out, 1)
})

test_that("aIP is 1 when token is unique on its side (occ = 1)", {
  out <- joinery:::.aip_eq9(occ_R = 1L, maxocc_R = 10L, occ_A = NA, maxocc_A = NA)
  expect_equal(out, 1)
})

# ---------- multi-column maxocc partitioning -------------------------

test_that("maxocc is computed per src_column, not globally", {
  dt <- data.table(
    id   = paste0("r", 1:4),
    name = c("alpha alpha", "alpha", "beta", "gamma"),
    city = c("berlin", "paris", "paris", "paris")
  )
  s <- search_strategy(
    name ~ word_tokens(),
    city ~ word_tokens(),
    threshold = 0.5
  )
  R <- prepare_auxiliary_registry(dt, "id", s)

  # name column: alpha in 2 records, beta in 1, gamma in 1 -> maxocc = 2
  # city column: paris in 3, berlin in 1 -> maxocc = 3
  expect_equal(unique(R[src_column == "name", maxocc]), 2L)
  expect_equal(unique(R[src_column == "city", maxocc]), 3L)
})


# ---------- block-by no-op --------------------------------------------

test_that("aIP is block-agnostic: block_by on strategy does not change occ", {
  base <- copy(make_base())
  base[, blk := c("A", "A", "B", "B", "A", "B")]

  R_no_block    <- prepare_auxiliary_registry(base, "id", simple_strategy())
  R_with_block  <- prepare_auxiliary_registry(
    base, "id", simple_strategy(block_by = "blk")
  )

  setkey(R_no_block, src_column, token)
  setkey(R_with_block, src_column, token)
  expect_equal(R_with_block$occ,    R_no_block$occ)
  expect_equal(R_with_block$maxocc, R_no_block$maxocc)
})


# ---------- tibble / data.frame parity -------------------------------

test_that("data.frame and tibble inputs defer to data.table backend", {
  dt_base <- make_base()
  df_base <- as.data.frame(dt_base)

  R_dt <- prepare_auxiliary_registry(dt_base, "id", simple_strategy())
  R_df <- prepare_auxiliary_registry(df_base, "id", simple_strategy())

  expect_s3_class(R_df, "data.frame")
  setkey(R_dt, src_column, token)
  R_df_sorted <- R_df[order(R_df$src_column, R_df$token), ]
  expect_equal(R_df_sorted$occ,    R_dt$occ)
  expect_equal(R_df_sorted$maxocc, R_dt$maxocc)

  if (requireNamespace("tibble", quietly = TRUE)) {
    tbl_base <- tibble::as_tibble(dt_base)
    R_tbl <- prepare_auxiliary_registry(tbl_base, "id", simple_strategy())
    expect_s3_class(R_tbl, "tbl_df")
    R_tbl_sorted <- R_tbl[order(R_tbl$src_column, R_tbl$token), ]
    expect_equal(R_tbl_sorted$occ,    R_dt$occ)
    expect_equal(R_tbl_sorted$maxocc, R_dt$maxocc)
  }
})


# ---------- DuckDB parity --------------------------------------------

test_that("DuckDB and data.table backends produce identical registries", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  con <- local_duckdb_con()
  base <- as.data.frame(make_base())
  DBI::dbWriteTable(con, "base_tbl", base)
  duck <- dplyr::tbl(con, "base_tbl")

  R_dt   <- prepare_auxiliary_registry(make_base(), "id", simple_strategy())
  R_duck <- prepare_auxiliary_registry(duck, "id", simple_strategy())

  R_duck_dt <- as.data.table(dplyr::collect(R_duck))
  setkey(R_dt, src_column, token)
  setkey(R_duck_dt, src_column, token)

  expect_equal(R_duck_dt$occ,    R_dt$occ)
  expect_equal(R_duck_dt$maxocc, R_dt$maxocc)
})

test_that("compute_aip works with mixed DuckDB and data.table inputs", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "base_tbl", as.data.frame(make_base()))
  duck <- dplyr::tbl(con, "base_tbl")

  R_duck <- prepare_auxiliary_registry(duck, "id", simple_strategy())
  A_dt   <- prepare_auxiliary_registry(make_aux(), "id", simple_strategy())

  out <- compute_aip(R_duck, A_dt)
  expect_equal(out[token == "common", aip], 0, tolerance = 1e-12)
  expect_equal(out[token == "mid",    aip], 0.5, tolerance = 1e-12)
})
