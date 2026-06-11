# ============================================================
# exact_token_links() — exact (score-1.0) token-set prefilter
# ============================================================
#
# The first genuinely new primitive of the v0.8 staged-resolution spine.
# Exposes the score-1.0 case of a joinery match as a cheap hash-joinable
# verb returning both the exact links and the unmatched residual, so a
# workflow can do `exact -> fuzzy(residual)` declaratively.
#
# Two faces:
#   - self form  (target = NULL): identical-token-set dedup edges, feeding
#     resolve_entities() with no scoring.
#   - cross form (target given) : base<->target exact links, the Stage-0 of
#     a search.
#
# The fingerprint ALWAYS rides the strategy's preparers via
# prepare_search_data() — never a parallel SQL/string fingerprint — so it
# cannot diverge from the engine's actual score == 1.0 (the Ü->UE vs Ü->U
# footgun). DT_tbl is defined in methods_datatable_prepare.R and Duck_tbl in
# the duckdb methods files; both are collated before this file.
#
# ============================================================


# Fingerprint delimiter: ASCII unit separator (0x1F). Normalized tokens are
# uppercase/ASCII and cannot contain it, so it cannot forge a false equality.
# The DuckDB path uses chr(31) for the same character.
.JOINERY_FP_DELIM <- intToUtf8(31L)  # was: literal ""


# ---- data.table core -------------------------------------------------------

# Build the one-row-per-record fingerprint table:
#   id | <block_by> | fp_<col1> | ... | fp_<colN>
# A column with an empty token set becomes "" (not NA) so empty<->empty is an
# equality (the §25 empty-column-robust dedup).
.exact_fp_wide_dt <- function(tokens, id_col, block_by, strategy_cols, delim) {
  tok <- .collapse_token_set(data.table::as.data.table(tokens), id_col, block_by)

  fp <- tok[
    , .(fp = paste0(sort(unique(token)), collapse = delim)),
    by = c(id_col, block_by, "src_column")
  ]

  lhs <- paste(c(id_col, block_by), collapse = " + ")
  wide <- data.table::dcast(
    fp, stats::as.formula(paste(lhs, "~ src_column")), value.var = "fp"
  )

  # Guarantee every strategy column is present, empty token set -> "".
  for (col in strategy_cols) {
    if (!col %in% names(wide)) wide[[col]] <- NA_character_
    v <- wide[[col]]
    v[is.na(v)] <- ""
    wide[[col]] <- v
  }
  data.table::setnames(wide, strategy_cols, paste0("fp_", strategy_cols))
  wide[]
}

# Set-equality links (containment = "off") on the data.table backend.
.exact_links_off_dt <- function(base, strategy, base_id,
                                target, target_id, block_by, self) {
  cols    <- names(strategy@preparers)
  fp_cols <- paste0("fp_", cols)
  keys    <- c(block_by, fp_cols)

  wb <- .exact_fp_wide_dt(
    prepare_search_data(base, base_id, strategy),
    base_id, block_by, cols, .JOINERY_FP_DELIM
  )

  if (self) {
    left  <- data.table::copy(wb); data.table::setnames(left,  base_id, "id_a")
    right <- data.table::copy(wb); data.table::setnames(right, base_id, "id_b")
    links <- merge(left, right, by = keys, allow.cartesian = TRUE)
    links <- links[id_a < id_b]
    links <- unique(links[, c("id_a", "id_b", block_by), with = FALSE])
  } else {
    wt <- .exact_fp_wide_dt(
      prepare_search_data(target, target_id, strategy),
      target_id, block_by, cols, .JOINERY_FP_DELIM
    )
    data.table::setnames(wb, base_id,   "base_id")
    data.table::setnames(wt, target_id, "target_id")
    links <- merge(wb, wt, by = keys, allow.cartesian = TRUE)
    links <- unique(links[, c("base_id", "target_id", block_by), with = FALSE])
  }
  links[]
}

# Containment links (forward / bidirectional) on the data.table backend.
# Structural subset test — overlap-join on shared tokens, |base ∩ target| ==
# |base| (forward) and/or == |target| (reverse), gated on base rarity mass.
.exact_links_containment_dt <- function(base, strategy, base_id,
                                        target, target_id, block_by, self,
                                        mode, min_base_rarity) {
  # Base side carries rarity (for the guard); target side does not.
  btok <- compute_rarity(prepare_search_data(base, base_id, strategy), strategy)
  btok <- .collapse_token_set(data.table::as.data.table(btok), base_id, block_by)

  if (self) {
    ttok <- data.table::copy(btok)
    tid  <- base_id
  } else {
    ttok <- prepare_search_data(target, target_id, strategy)
    ttok <- .collapse_token_set(data.table::as.data.table(ttok), target_id, block_by)
    tid  <- target_id
  }

  b <- btok[, c(base_id, block_by, "src_column", "token", "rarity"), with = FALSE]
  data.table::setnames(b, base_id, "._bid")
  t <- ttok[, c(tid, block_by, "src_column", "token"), with = FALSE]
  data.table::setnames(t, tid, "._tid")

  # Per-record cardinalities and base rarity mass (over distinct tokens).
  nbase   <- b[, .(n_base = .N, rmass = sum(rarity)), by = "._bid"]
  ntarget <- t[, .(n_target = .N), by = "._tid"]
  bblock  <- unique(b[, c("._bid", block_by), with = FALSE])

  key <- c(block_by, "src_column", "token")
  ov  <- merge(
    b[, c("._bid", key), with = FALSE],
    t[, c("._tid", key), with = FALSE],
    by = key, allow.cartesian = TRUE
  )
  if (nrow(ov) == 0L) {
    return(data.table::data.table(._bid = character(), ._tid = character()))
  }

  nmatch <- ov[, .(n_match = .N), by = c("._bid", "._tid")]
  nmatch <- merge(nmatch, nbase,   by = "._bid")
  nmatch <- merge(nmatch, ntarget, by = "._tid")

  fwd <- nmatch[n_match == n_base]                       # base ⊆ target
  qualifying <- if (mode == "bidirectional") {
    data.table::rbindlist(list(fwd, nmatch[n_match == n_target]))  # + target ⊆ base
  } else {
    fwd
  }

  # Guard on base informativeness; drop self pairs in the self form.
  qualifying <- qualifying[rmass >= min_base_rarity]
  if (self) qualifying <- qualifying[._bid != ._tid]
  qualifying <- unique(qualifying[, .(._bid, ._tid)])

  # Attach the (within-block) block tuple from the base record.
  qualifying <- merge(qualifying, bblock, by = "._bid", all.x = TRUE)
  qualifying[]
}

method(
  exact_token_links,
  list(DT_tbl, Search_Strategy)
) <- function(base, strategy, target = NULL,
              base_id = "id", target_id = base_id,
              containment = c("off", "forward", "bidirectional"),
              min_base_rarity = 0, ...) {

  containment <- match.arg(containment)
  block_by    <- strategy@block_by %||% character()
  self        <- is.null(target)

  base <- data.table::as.data.table(base)
  base[[base_id]] <- as.character(base[[base_id]])
  all_base_ids <- unique(base[[base_id]])

  all_target_ids <- NULL
  if (!self) {
    target <- data.table::as.data.table(target)
    target[[target_id]] <- as.character(target[[target_id]])
    all_target_ids <- unique(target[[target_id]])
  }

  if (containment == "off") {
    links <- .exact_links_off_dt(base, strategy, base_id,
                                 target, target_id, block_by, self)
  } else {
    raw <- .exact_links_containment_dt(base, strategy, base_id,
                                       target, target_id, block_by, self,
                                       containment, min_base_rarity)
    # Shape containment output to match the off-mode link schema.
    if (self) {
      links <- raw[, c("._bid", "._tid", block_by), with = FALSE]
      data.table::setnames(links, c("._bid", "._tid"), c("id_a", "id_b"))
      # normalize undirected edge orientation, drop duplicates
      links <- links[, `:=`(
        lo = pmin(id_a, id_b), hi = pmax(id_a, id_b)
      )][, id_a := lo][, id_b := hi][, c("lo", "hi") := NULL]
      links <- unique(links[id_a < id_b, c("id_a", "id_b", block_by), with = FALSE])
    } else {
      links <- raw[, c("._bid", "._tid", block_by), with = FALSE]
      data.table::setnames(links, c("._bid", "._tid"), c("base_id", "target_id"))
    }
  }

  # Residual = exact complement (base \ matched).
  if (self) {
    matched  <- unique(c(links$id_a, links$id_b))
    residual <- list(ids = setdiff(all_base_ids, matched))
  } else {
    residual <- list(
      base   = setdiff(all_base_ids,   unique(links$base_id)),
      target = setdiff(all_target_ids, unique(links$target_id))
    )
  }

  list(links = links, residual = residual)
}


# ---- DuckDB backend --------------------------------------------------------

if (requireNamespace("duckdb", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("dplyr", quietly = TRUE)) {

  Duck_tbl <- new_S3_class("tbl_duckdb_connection")

  # Build the one-row-per-record fingerprint temp table, COALESCEing empty
  # token sets to '' so empty<->empty is an equality. Returns the table name.
  .exact_fp_wide_duck <- function(con, tokens_tbl, id_col, block_by, strategy_cols) {
    id_q  <- sprintf('"%s"', id_col)
    blk_q <- if (length(block_by)) {
      paste0(", ", paste(sprintf('"%s"', block_by), collapse = ", "))
    } else ""
    blk_grp <- if (length(block_by)) {
      paste0(", ", paste(sprintf('"%s"', block_by), collapse = ", "))
    } else ""

    # per (id, block, src_column) sorted-distinct fingerprint
    fp_tbl <- paste0("_joinery_fp_", sample.int(1e9, 1))
    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE ", fp_tbl, " AS\n",
      "SELECT ", id_q, " AS id", blk_q, ", src_column,\n",
      "       STRING_AGG(token, chr(31) ORDER BY token) AS fp\n",
      "FROM (SELECT DISTINCT ", id_q, ", token, src_column", blk_q,
      " FROM ", tokens_tbl, ") s\n",
      "GROUP BY ", id_q, blk_grp, ", src_column;"
    ))

    # pivot to wide via conditional aggregation
    fp_cols_sql <- paste(
      vapply(strategy_cols, function(c) sprintf(
        "COALESCE(MAX(CASE WHEN src_column = '%s' THEN fp END), '') AS \"fp_%s\"",
        c, c
      ), character(1)),
      collapse = ",\n       "
    )
    wide_tbl <- paste0("_joinery_fpw_", sample.int(1e9, 1))
    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE ", wide_tbl, " AS\n",
      "SELECT id", blk_q, ",\n       ", fp_cols_sql, "\n",
      "FROM ", fp_tbl, "\n",
      "GROUP BY id", blk_grp, ";"
    ))
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", fp_tbl, ";"))
    wide_tbl
  }

  # Distinct ids of a backing table, as a one-column ("id") temp table.
  .duck_all_ids <- function(con, tbl, id_col) {
    out <- paste0("_joinery_ids_", sample.int(1e9, 1))
    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE ", out, " AS\n",
      "SELECT DISTINCT CAST(\"", id_col, "\" AS VARCHAR) AS id FROM ", tbl, ";"
    ))
    out
  }

  method(
    exact_token_links,
    list(Duck_tbl, Search_Strategy)
  ) <- function(base, strategy, target = NULL,
                base_id = "id", target_id = base_id,
                containment = c("off", "forward", "bidirectional"),
                min_base_rarity = 0, ...) {

    containment <- match.arg(containment)
    block_by    <- strategy@block_by %||% character()
    self        <- is.null(target)
    cols        <- names(strategy@preparers)

    con  <- base$src$con
    base <- .materialise_duck_input(base, con)
    base_src <- base$lazy_query$x

    if (!self) {
      target     <- .materialise_duck_input(target, con)
      target_src <- target$lazy_query$x
    }

    drop_later <- character()

    if (containment == "off") {
      btok <- prepare_search_data(base, base_id, strategy)
      bw   <- .exact_fp_wide_duck(con, btok$lazy_query$x, base_id, block_by, cols)
      drop_later <- c(drop_later, btok$lazy_query$x, bw)

      fp_cols <- paste0("fp_", cols)
      on_clause <- paste(c(
        if (length(block_by)) sprintf('a."%s" = b."%s"', block_by, block_by),
        sprintf('a."%s" = b."%s"', fp_cols, fp_cols)
      ), collapse = "\n  AND ")
      blk_out <- if (length(block_by)) {
        paste0(", ", paste(sprintf('a."%s"', block_by), collapse = ", "))
      } else ""

      links_tbl <- paste0("_joinery_tmp_exact_links_", sample.int(1e9, 1))
      if (self) {
        DBI::dbExecute(con, paste0(
          "CREATE TABLE ", links_tbl, " AS\n",
          "SELECT a.id AS id_a, b.id AS id_b", blk_out, "\n",
          "FROM ", bw, " a JOIN ", bw, " b\n  ON ", on_clause, "\n",
          "WHERE a.id < b.id;"
        ))
      } else {
        tw <- .exact_fp_wide_duck(con, prepare_search_data(target, target_id, strategy)$lazy_query$x,
                                  target_id, block_by, cols)
        drop_later <- c(drop_later, tw)
        DBI::dbExecute(con, paste0(
          "CREATE TABLE ", links_tbl, " AS\n",
          "SELECT a.id AS base_id, b.id AS target_id", blk_out, "\n",
          "FROM ", bw, " a JOIN ", tw, " b\n  ON ", on_clause, ";"
        ))
      }
    } else {
      links_tbl <- .exact_links_containment_duck(
        con, base, base_id, target, target_id, strategy, block_by,
        self, containment, min_base_rarity, drop_later_env = environment()
      )
    }

    # Residual = exact complement, as one-column ("id") temp tables.
    if (self) {
      all_ids <- .duck_all_ids(con, base_src, base_id)
      resid   <- paste0("_joinery_tmp_exact_resid_", sample.int(1e9, 1))
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", resid, " AS\n",
        "SELECT id FROM ", all_ids, "\n",
        "WHERE id NOT IN (SELECT id_a FROM ", links_tbl,
        " UNION SELECT id_b FROM ", links_tbl, ");"
      ))
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", all_ids, ";"))
      residual <- list(ids = dplyr::tbl(con, resid))
    } else {
      all_b <- .duck_all_ids(con, base_src,   base_id)
      all_t <- .duck_all_ids(con, target_src, target_id)
      rb <- paste0("_joinery_tmp_exact_residb_", sample.int(1e9, 1))
      rt <- paste0("_joinery_tmp_exact_residt_", sample.int(1e9, 1))
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", rb, " AS SELECT id FROM ", all_b,
        " WHERE id NOT IN (SELECT base_id FROM ", links_tbl, ");"
      ))
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", rt, " AS SELECT id FROM ", all_t,
        " WHERE id NOT IN (SELECT target_id FROM ", links_tbl, ");"
      ))
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", all_b, ";"))
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", all_t, ";"))
      residual <- list(base = dplyr::tbl(con, rb), target = dplyr::tbl(con, rt))
    }

    for (d in unique(drop_later)) {
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", d, ";"))
    }

    list(links = dplyr::tbl(con, links_tbl), residual = residual)
  }

  # DuckDB containment: structural subset test via overlap join + HAVING.
  # Writes the links table (id_a/id_b for self, base_id/target_id for cross)
  # and returns its name. Registers intermediates onto drop_later_env$drop_later.
  .exact_links_containment_duck <- function(con, base, base_id, target, target_id,
                                            strategy, block_by, self, mode,
                                            min_base_rarity, drop_later_env) {
    btok <- compute_rarity(prepare_search_data(base, base_id, strategy), strategy)
    btok_tbl <- btok$lazy_query$x
    if (self) {
      ttok_tbl <- btok_tbl
      tid      <- base_id
    } else {
      ttok     <- prepare_search_data(target, target_id, strategy)
      ttok_tbl <- ttok$lazy_query$x
      tid      <- target_id
    }
    drop_later_env$drop_later <- c(drop_later_env$drop_later, btok_tbl,
                                   if (!self) ttok_tbl)

    bidq <- sprintf('"%s"', base_id)
    tidq <- sprintf('"%s"', tid)
    blk_join <- if (length(block_by)) {
      paste0(" AND ", paste(sprintf('bb."%s" = tt."%s"', block_by, block_by),
                            collapse = " AND "))
    } else ""
    blk_sel <- if (length(block_by)) {
      paste0(", ", paste(sprintf('MAX(bb."%s") AS "%s"', block_by, block_by),
                         collapse = ", "))
    } else ""

    # distinct base/target tokens; per-record cardinalities + base rarity mass
    nb <- paste0("_joinery_cnb_", sample.int(1e9, 1))
    nt <- paste0("_joinery_cnt_", sample.int(1e9, 1))
    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE ", nb, " AS\n",
      "SELECT ", bidq, " AS bid, COUNT(*) AS n_base, SUM(rarity) AS rmass FROM (\n",
      "  SELECT DISTINCT ", bidq, ", src_column, token, ANY_VALUE(rarity) AS rarity\n",
      "  FROM ", btok_tbl, " GROUP BY ", bidq, ", src_column, token\n",
      ") GROUP BY ", bidq, ";"
    ))
    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE ", nt, " AS\n",
      "SELECT ", tidq, " AS tid, COUNT(DISTINCT (src_column, token)) AS n_target\n",
      "FROM ", ttok_tbl, " GROUP BY ", tidq, ";"
    ))

    # overlap count per (bid, tid), carrying block from the base side
    ov <- paste0("_joinery_ov_", sample.int(1e9, 1))
    blk_on <- if (length(block_by)) {
      paste(sprintf('bb."%s" = tt."%s"', block_by, block_by), collapse = " AND ")
    } else "TRUE"
    blk_carry_sel <- if (length(block_by)) {
      paste0(", ", paste(sprintf('bb."%s" AS "%s"', block_by, block_by), collapse = ", "))
    } else ""
    blk_carry_grp <- if (length(block_by)) {
      paste0(", ", paste(sprintf('bb."%s"', block_by), collapse = ", "))
    } else ""
    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE ", ov, " AS\n",
      "SELECT bb.", bidq, " AS bid, tt.", tidq, " AS tid, COUNT(*) AS n_match",
      blk_carry_sel, "\n",
      "FROM (SELECT DISTINCT ", bidq, ", src_column, token",
      if (length(block_by)) paste0(", ", paste(sprintf('"%s"', block_by), collapse = ", ")) else "",
      " FROM ", btok_tbl, ") bb\n",
      "JOIN (SELECT DISTINCT ", tidq, ", src_column, token",
      if (length(block_by)) paste0(", ", paste(sprintf('"%s"', block_by), collapse = ", ")) else "",
      " FROM ", ttok_tbl, ") tt\n",
      "  ON bb.src_column = tt.src_column AND bb.token = tt.token AND ", blk_on, "\n",
      "GROUP BY bb.", bidq, ", tt.", tidq, blk_carry_grp, ";"
    ))

    qual_cond <- if (mode == "bidirectional") {
      "(o.n_match = nb.n_base OR o.n_match = nt.n_target)"
    } else {
      "o.n_match = nb.n_base"
    }
    self_cond <- if (self) "AND o.bid <> o.tid\n" else ""

    blk_out_sel <- if (length(block_by)) {
      paste0(", ", paste(sprintf('o."%s"', block_by), collapse = ", "))
    } else ""

    links_tbl <- paste0("_joinery_tmp_exact_links_", sample.int(1e9, 1))
    if (self) {
      # normalize undirected orientation (least(bid,tid), greatest(bid,tid))
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", links_tbl, " AS\n",
        "SELECT DISTINCT LEAST(o.bid, o.tid) AS id_a, GREATEST(o.bid, o.tid) AS id_b",
        blk_out_sel, "\n",
        "FROM ", ov, " o\n",
        "JOIN ", nb, " nb ON o.bid = nb.bid\n",
        "JOIN ", nt, " nt ON o.tid = nt.tid\n",
        "WHERE ", qual_cond, " ", self_cond,
        "  AND nb.rmass >= ", min_base_rarity, "\n",
        "  AND o.bid <> o.tid;"
      ))
    } else {
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", links_tbl, " AS\n",
        "SELECT DISTINCT o.bid AS base_id, o.tid AS target_id", blk_out_sel, "\n",
        "FROM ", ov, " o\n",
        "JOIN ", nb, " nb ON o.bid = nb.bid\n",
        "JOIN ", nt, " nt ON o.tid = nt.tid\n",
        "WHERE ", qual_cond, "\n",
        "  AND nb.rmass >= ", min_base_rarity, ";"
      ))
    }

    walk(c(nb, nt, ov), function(x) DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", x, ";")))
    links_tbl
  }

}
