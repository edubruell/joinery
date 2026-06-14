# ============================================================
# aIP — absolute identification potential
# ============================================================
#
# Two-piece primitive that feeds the `match_features()` Meta-Vector.
# Implements Doherr (2023) eq. (9); design in
# notes/calibration_design.md.
#
#   aIP(w, st) = 1 − min(
#       ln(occ_R(w, st)) / ln(maxocc_R(st))   if w ∈ R,
#       ln(occ_A(w, st)) / ln(maxocc_A(st))   if w ∈ A )
#
# where:
#   R = base-side  registry (the table being deduped / searched against),
#   A = auxiliary  registry (the search / target side).
#
# Registries share schema:
#   data.table with columns `src_column`, `token`, `occ`, `maxocc`.
#
# `compute_rarity()` is intentionally left untouched — it remains the
# per-block retrieval-time rIP machinery. aIP is cross-table and
# block-agnostic by construction.
# ============================================================


# ---------- data.table method ----------------------------------------

method(
  prepare_auxiliary_registry,
  list(DT_tbl, class_character, Search_Strategy)
) <- function(data, id, strategy) {

  # Strip block_by so prepare_search_data() does not attach block columns
  # we would only end up dropping. aIP is cross-table by design.
  aux_strategy <- strategy
  if (!is.null(aux_strategy@block_by)) {
    S7::prop(aux_strategy, "block_by") <- NULL
  }

  tokens <- prepare_search_data(data, id, aux_strategy)

  .build_registry_from_tokens(tokens)
}


# ---------- DuckDB method --------------------------------------------

method(
  prepare_auxiliary_registry,
  list(Duck_tbl, class_character, Search_Strategy)
) <- function(data, id, strategy,
              control = duckdb_control()) {

  aux_strategy <- strategy
  if (!is.null(aux_strategy@block_by)) {
    S7::prop(aux_strategy, "block_by") <- NULL
  }

  tokens_tbl <- prepare_search_data(
    data, id, aux_strategy,
    control = control
  )

  con   <- tokens_tbl$src$con
  table <- tokens_tbl$lazy_query$x

  registry_table <- paste0("_joinery_aux_registry_", sample.int(1e9, 1))

  sql <- paste0(
    "SELECT src_column, token, occ,\n",
    "       MAX(occ) OVER (PARTITION BY src_column) AS maxocc\n",
    "FROM (\n",
    "  SELECT src_column, token, COUNT(DISTINCT row_id) AS occ\n",
    "  FROM ", table, "\n",
    "  GROUP BY src_column, token\n",
    ") AS _agg"
  )

  DBI::dbExecute(con,
                 paste0("CREATE TEMP TABLE ", registry_table, " AS ", sql))

  dplyr::tbl(con, dbplyr::ident(registry_table))
}


# ---------- tibble / data.frame wrappers ------------------------------

method(
  prepare_auxiliary_registry,
  list(.jyDF, class_character, Search_Strategy)
) <- function(data, id, strategy) {
  out <- prepare_auxiliary_registry(as_DT(data), id, strategy)
  back_to_original(out, data)
}

method(
  prepare_auxiliary_registry,
  list(.jyTBL_DF, class_character, Search_Strategy)
) <- function(data, id, strategy) {
  out <- prepare_auxiliary_registry(as_DT(data), id, strategy)
  back_to_original(out, data)
}

method(
  prepare_auxiliary_registry,
  list(.jyTBL, class_character, Search_Strategy)
) <- function(data, id, strategy) {
  out <- prepare_auxiliary_registry(as_DT(data), id, strategy)
  back_to_original(out, data)
}


# ---------- internal helpers -----------------------------------------

#' Aggregate a long-form token table into the registry schema.
#' @noRd
.build_registry_from_tokens <- function(tokens) {

  tokens <- data.table::as.data.table(tokens)

  # occ = number of distinct records containing this token in this column
  reg <- tokens[
    , .(occ = data.table::uniqueN(row_id)),
    by = c("src_column", "token")
  ]

  reg[, maxocc := max(occ), by = "src_column"]
  data.table::setcolorder(reg, c("src_column", "token", "occ", "maxocc"))
  data.table::setkey(reg, src_column, token)
  reg[]
}


#' Compute absolute identification potential (aIP).
#'
#' Internal primitive - joins base- and auxiliary-side registries and
#' applies Doherr (2023) eq. (9) per `(src_column, token)`.
#'
#' For tokens present in both registries we take the smaller of the two
#' log-ratios (the rarer side dominates). When a token is only in one
#' registry the `min` collapses to that side. Convention:
#'
#'   * `occ == 1 && maxocc >= 1`  → log(1) / log(maxocc) = 0, aIP = 1.
#'   * `maxocc == 1`              → all tokens are equally rare, aIP = 1.
#'   * token in neither registry  → not represented in the output.
#'
#' @param base_registry,aux_registry data.tables (or DuckDB tbls) with
#'   columns `src_column`, `token`, `occ`, `maxocc`. DuckDB tbls are
#'   collected eagerly - registries are small relative to token tables.
#'
#' @return data.table with columns `src_column`, `token`, `aip`,
#'   plus `occ_R`, `maxocc_R`, `occ_A`, `maxocc_A` for debuggability.
#'
#' @noRd
compute_aip <- function(base_registry, aux_registry) {

  R <- .collect_registry(base_registry)
  A <- .collect_registry(aux_registry)

  data.table::setnames(R, c("occ", "maxocc"), c("occ_R", "maxocc_R"))
  data.table::setnames(A, c("occ", "maxocc"), c("occ_A", "maxocc_A"))

  out <- merge(R, A,
               by = c("src_column", "token"),
               all = TRUE)

  out[, aip := .aip_eq9(occ_R, maxocc_R, occ_A, maxocc_A)]
  data.table::setkey(out, src_column, token)
  out[]
}


#' Eq. (9) of Doherr (2023). Vectorised; NA-safe.
#' @noRd
.aip_eq9 <- function(occ_R, maxocc_R, occ_A, maxocc_A) {
  ratio_R <- .log_ratio(occ_R, maxocc_R)
  ratio_A <- .log_ratio(occ_A, maxocc_A)

  # pmin treats NA as missing rather than absorbing; the `min` in eq. (9)
  # is over registries the token actually belongs to, not over a fixed
  # pair. So an NA on one side must fall back to the other.
  m <- pmin(ratio_R, ratio_A, na.rm = TRUE)

  # If both inputs were NA we get Inf from pmin(..., na.rm = TRUE) on an
  # empty set; coerce back to NA so callers see the missing case.
  m[is.infinite(m)] <- NA_real_

  1 - m
}


#' log(occ) / log(maxocc) with documented edge-case behaviour.
#' Returns 0 (→ aIP = 1) when occ = 1 or maxocc = 1.
#' Returns NA when either input is NA.
#' @noRd
.log_ratio <- function(occ, maxocc) {
  out <- rep(NA_real_, length(occ))
  ok  <- !is.na(occ) & !is.na(maxocc)

  # When maxocc == 1, every token in the column is equally rare; treat
  # the ratio as 0 so aIP collapses to 1 (maximum rarity).
  trivial <- ok & maxocc <= 1
  out[trivial] <- 0

  active <- ok & maxocc > 1
  out[active] <- log(occ[active]) / log(maxocc[active])
  out
}


#' Collect a registry into a data.table, accepting DuckDB lazy tbls.
#' @noRd
.collect_registry <- function(reg) {
  if (data.table::is.data.table(reg)) {
    return(data.table::copy(reg))
  }
  if (inherits(reg, "tbl_lazy") || inherits(reg, "tbl_sql")) {
    return(data.table::as.data.table(dplyr::collect(reg)))
  }
  data.table::as.data.table(reg)
}
