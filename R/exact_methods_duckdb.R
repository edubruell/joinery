if (!requireNamespace("duckdb", quietly = TRUE) ||
    !requireNamespace("DBI", quietly = TRUE) ||
    !requireNamespace("dplyr", quietly = TRUE)) {
  return(invisible(NULL))
}

# ============================================================
# DuckDB backend — exact (score-1.0) token-set matching
# ============================================================
#
# detect_duplicates() and search_candidates() methods for Exact_Strategy on
# DuckDB inputs, plus the fingerprint / containment kernel they share. Like
# the data.table backend, exactness reaches the standard verbs by dispatch and
# both methods return the standard schema with score == 1.0.
#
# The fingerprint rides prepare_search_data() (via the proxy strategy), and on
# DuckDB that path batches rows back to the data.table tokenizer — so both
# backends tokenize through normalize_text (Ü->UE), never a SQL strip_accents
# (Ü->U). All link computation is in-SQL; _joinery_* temps are dropped.
# ============================================================

Duck_tbl <- new_S3_class("tbl_duckdb_connection")


# ---- fingerprint / containment kernel --------------------------------------

# One-row-per-record fingerprint temp table, COALESCEing empty token sets to ''
# so empty<->empty is an equality. Returns the table name.
.exact_fp_wide_duck <- function(con, tokens_tbl, id_col, block_by, strategy_cols) {
  id_q  <- sprintf('"%s"', id_col)
  blk_q <- if (length(block_by)) {
    paste0(", ", paste(sprintf('"%s"', block_by), collapse = ", "))
  } else ""

  fp_tbl <- paste0("_joinery_fp_", sample.int(1e9, 1))
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE ", fp_tbl, " AS\n",
    "SELECT ", id_q, " AS id", blk_q, ", src_column,\n",
    "       STRING_AGG(token, chr(31) ORDER BY token) AS fp\n",
    "FROM (SELECT DISTINCT ", id_q, ", token, src_column", blk_q,
    " FROM ", tokens_tbl, ") s\n",
    "GROUP BY ", id_q, blk_q, ", src_column;"
  ))

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
    "GROUP BY id", blk_q, ";"
  ))
  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", fp_tbl, ";"))
  wide_tbl
}

# Distinct ids of a backing table, as a one-column ("id") temp table name.
.duck_all_ids <- function(con, tbl, id_col) {
  out <- paste0("_joinery_ids_", sample.int(1e9, 1))
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE ", out, " AS\n",
    "SELECT DISTINCT CAST(\"", id_col, "\" AS VARCHAR) AS id FROM ", tbl, ";"
  ))
  out
}

# Set-equality links (containment = "off"). Returns the links table name
# (id_a|id_b|<block> for self; base_id|target_id|<block> for cross).
.exact_links_off_duck <- function(con, base_src, base_id, target_src, target_id,
                                  proxy, block_by, self, reg) {
  cols    <- names(proxy@preparers)
  fp_cols <- paste0("fp_", cols)

  btok <- prepare_search_data(dplyr::tbl(con, base_src), base_id, proxy)
  bw   <- .exact_fp_wide_duck(con, btok$lazy_query$x, base_id, block_by, cols)
  reg$drop <- c(reg$drop, btok$lazy_query$x, bw)

  on_clause <- paste(c(
    if (length(block_by)) sprintf('a."%s" = b."%s"', block_by, block_by),
    sprintf('a."%s" = b."%s"', fp_cols, fp_cols)
  ), collapse = "\n  AND ")
  blk_out <- if (length(block_by)) {
    paste0(", ", paste(sprintf('a."%s"', block_by), collapse = ", "))
  } else ""
  # bare block selector + partition key for the self group-by star.
  blk_bare <- if (length(block_by)) {
    paste0(", ", paste(sprintf('"%s"', block_by), collapse = ", "))
  } else ""
  part_sql <- paste(c(
    if (length(block_by)) sprintf('"%s"', block_by),
    sprintf('"%s"', fp_cols)
  ), collapse = ", ")
  # Empty-fingerprint guard: drop records whose every fp column is '' (no tokens
  # in ANY column) — they carry no identity and must not collapse together (else
  # all token-less rows in a block form one spurious entity / N^2 clique).
  empty_all <- paste(sprintf('"%s" = \'\'', fp_cols), collapse = " AND ")

  links_tbl <- paste0("_joinery_tmp_exact_links_", sample.int(1e9, 1))
  if (self) {
    # GROUP-BY star, NOT an all-pairs self-join: set-equality is transitive, so
    # rep = MIN(id) per (block, fp...) group + rep->member edges yield identical
    # connected components at O(N) instead of O(N^2). A K-record identical clique
    # (e.g. a directory-publisher row duplicated thousands of times) collapses in
    # K-1 star edges, not K(K-1)/2 pairs. Mirrors v1 31_collapse_within_year.R.
    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE ", links_tbl, " AS\n",
      "WITH g AS (\n",
      "  SELECT id", blk_bare, ",\n",
      "         MIN(id) OVER (PARTITION BY ", part_sql, ") AS rep\n",
      "  FROM ", bw, "\n",
      "  WHERE NOT (", empty_all, ")\n",
      ")\n",
      "SELECT rep AS id_a, id AS id_b", blk_bare, "\n",
      "FROM g WHERE id <> rep;"
    ))
  } else {
    ttok <- prepare_search_data(dplyr::tbl(con, target_src), target_id, proxy)
    tw   <- .exact_fp_wide_duck(con, ttok$lazy_query$x, target_id, block_by, cols)
    reg$drop <- c(reg$drop, ttok$lazy_query$x, tw)
    # cross/search keeps every base x target pair (each is a distinct candidate);
    # only fully-empty fingerprints are excluded. fp-equality (the join key) means
    # a is empty <=> b is empty, so guarding the base side suffices.
    empty_a <- paste(sprintf('a."%s" = \'\'', fp_cols), collapse = " AND ")
    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE ", links_tbl, " AS\n",
      "SELECT a.id AS base_id, b.id AS target_id", blk_out, "\n",
      "FROM ", bw, " a JOIN ", tw, " b\n  ON ", on_clause, "\n",
      "WHERE NOT (", empty_a, ");"
    ))
  }
  links_tbl
}

# Containment links (forward / bidirectional): structural subset test via
# overlap join + HAVING. Returns the links table name.
.exact_links_containment_duck <- function(con, base_src, base_id, target_src, target_id,
                                          proxy, block_by, self, mode,
                                          min_base_rarity, reg) {
  btok <- compute_rarity(prepare_search_data(dplyr::tbl(con, base_src), base_id, proxy), proxy)
  btok_tbl <- btok$lazy_query$x
  if (self) {
    ttok_tbl <- btok_tbl
    tid      <- base_id
  } else {
    ttok     <- prepare_search_data(dplyr::tbl(con, target_src), target_id, proxy)
    ttok_tbl <- ttok$lazy_query$x
    tid      <- target_id
  }
  reg$drop <- c(reg$drop, btok_tbl, if (!self) ttok_tbl)

  bidq <- sprintf('"%s"', base_id)
  tidq <- sprintf('"%s"', tid)
  blk_extra <- if (length(block_by)) {
    paste0(", ", paste(sprintf('"%s"', block_by), collapse = ", "))
  } else ""

  nb <- paste0("_joinery_cnb_", sample.int(1e9, 1))
  nt <- paste0("_joinery_cnt_", sample.int(1e9, 1))
  ov <- paste0("_joinery_ov_", sample.int(1e9, 1))
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE ", nb, " AS\n",
    "SELECT bid, COUNT(*) AS n_base, SUM(rarity) AS rmass FROM (\n",
    "  SELECT ", bidq, " AS bid, src_column, token, ANY_VALUE(rarity) AS rarity\n",
    "  FROM ", btok_tbl, " GROUP BY ", bidq, ", src_column, token\n",
    ") GROUP BY bid;"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE ", nt, " AS\n",
    "SELECT ", tidq, " AS tid, COUNT(DISTINCT (src_column, token)) AS n_target\n",
    "FROM ", ttok_tbl, " GROUP BY ", tidq, ";"
  ))

  blk_on  <- if (length(block_by)) {
    paste0(" AND ", paste(sprintf('bb."%s" = tt."%s"', block_by, block_by), collapse = " AND "))
  } else ""
  blk_csel <- if (length(block_by)) {
    paste0(", ", paste(sprintf('bb."%s" AS "%s"', block_by, block_by), collapse = ", "))
  } else ""
  blk_cgrp <- if (length(block_by)) {
    paste0(", ", paste(sprintf('bb."%s"', block_by), collapse = ", "))
  } else ""
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE ", ov, " AS\n",
    "SELECT bb.bid AS bid, tt.tid AS tid, COUNT(*) AS n_match", blk_csel, "\n",
    "FROM (SELECT DISTINCT ", bidq, " AS bid, src_column, token", blk_extra,
    " FROM ", btok_tbl, ") bb\n",
    "JOIN (SELECT DISTINCT ", tidq, " AS tid, src_column, token", blk_extra,
    " FROM ", ttok_tbl, ") tt\n",
    "  ON bb.src_column = tt.src_column AND bb.token = tt.token", blk_on, "\n",
    "GROUP BY bb.bid, tt.tid", blk_cgrp, ";"
  ))

  qual_cond <- if (mode == "bidirectional") {
    "(o.n_match = nb.n_base OR o.n_match = nt.n_target)"
  } else {
    "o.n_match = nb.n_base"
  }
  blk_out_sel <- if (length(block_by)) {
    paste0(", ", paste(sprintf('o."%s"', block_by), collapse = ", "))
  } else ""

  links_tbl <- paste0("_joinery_tmp_exact_links_", sample.int(1e9, 1))
  if (self) {
    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE ", links_tbl, " AS\n",
      "SELECT DISTINCT LEAST(o.bid, o.tid) AS id_a, GREATEST(o.bid, o.tid) AS id_b",
      blk_out_sel, "\n",
      "FROM ", ov, " o\n",
      "JOIN ", nb, " nb ON o.bid = nb.bid\n",
      "JOIN ", nt, " nt ON o.tid = nt.tid\n",
      "WHERE ", qual_cond, " AND o.bid <> o.tid\n",
      "  AND nb.rmass >= ", min_base_rarity, ";"
    ))
  } else {
    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE ", links_tbl, " AS\n",
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

# Dispatch to the off / containment link builder for a given form.
.exact_links_duck <- function(con, base_src, base_id, target_src, target_id,
                              strategy, proxy, block_by, self, reg) {
  if (strategy@containment == "off") {
    .exact_links_off_duck(con, base_src, base_id, target_src, target_id,
                          proxy, block_by, self, reg)
  } else {
    .exact_links_containment_duck(con, base_src, base_id, target_src, target_id,
                                  proxy, block_by, self,
                                  strategy@containment, strategy@min_base_rarity, reg)
  }
}


# ---- detect_duplicates (self / dedup face) ---------------------------------

method(
  detect_duplicates,
  list(Duck_tbl, class_character, Exact_Strategy)
) <- function(base_table, id, strategy, debug = FALSE, ...) {

  con  <- base_table$src$con
  id_q <- sprintf('"%s"', id)
  base_table <- .materialise_duck_input(base_table, con)
  base_src   <- base_table$lazy_query$x
  block_by   <- strategy@block_by %||% character()
  proxy      <- .exact_proxy_strategy(strategy)
  tmp <- function(p) paste0(p, "_", sample.int(1e9, 1))

  reg <- new.env(parent = emptyenv()); reg$drop <- character()
  links_tbl <- .exact_links_duck(con, base_src, id, NULL, id,
                                 strategy, proxy, block_by, self = TRUE, reg)

  n_links <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", links_tbl))$n

  empty_dedup <- function() {
    out_name <- tmp("_joinery_tmp_dups")
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", out_name, " AS\n",
      "SELECT bt.", id_q, " AS id,\n",
      "       CAST(NULL AS BIGINT) AS duplicate_group,\n",
      "       CAST(NULL AS DOUBLE) AS score,\n",
      "       CAST(NULL AS BIGINT) AS rank,\n",
      "       bt.* EXCLUDE (", id_q, ")\n",
      "FROM ", base_src, " bt WHERE 1=0;"
    ))
    dplyr::tbl(con, out_name)
  }

  if (n_links == 0L) {
    for (d in unique(reg$drop)) DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", d, ";"))
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", links_tbl, ";"))
    return(empty_dedup())
  }

  # edges carry constant score 1.0 + block cols; resolve per-block.
  blk_sel <- if (length(block_by)) {
    paste0(", ", paste(sprintf('"%s"', block_by), collapse = ", "))
  } else ""
  edges_in <- tmp("_joinery_tmp_edges_in")
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE ", edges_in, " AS\n",
    "SELECT id_a AS a, id_b AS b, 1.0 AS score", blk_sel, " FROM ", links_tbl, ";"
  ))

  ent <- resolve_entities(dplyr::tbl(con, edges_in), "a", "b",
                          score = "score", block_by = block_by, debug = debug)
  ent_name <- ent$lazy_query$x

  out_name <- tmp("_joinery_tmp_dups")
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", out_name, " AS\n",
    "SELECT e.id AS id, e.entity AS duplicate_group, e.score AS score, e.rank AS rank,\n",
    "       bt.* EXCLUDE (", id_q, ")\n",
    "FROM ", ent_name, " e\n",
    "LEFT JOIN ", base_src, " bt ON e.id = bt.", id_q, "\n",
    "ORDER BY duplicate_group, rank;"
  ))

  if (!debug) {
    for (d in unique(c(reg$drop, links_tbl, edges_in, ent_name))) {
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", d, ";"))
    }
  }
  dplyr::tbl(con, out_name)
}


# ---- search_candidates (cross / search face) -------------------------------

method(
  search_candidates,
  list(Duck_tbl, Duck_tbl, class_character, class_character, Exact_Strategy)
) <- function(base_table, target_table, base_id, target_id, strategy,
              debug = FALSE, ...) {

  con <- base_table$src$con
  base_id_q   <- sprintf('"%s"', base_id)
  target_id_q <- sprintf('"%s"', target_id)
  base_table   <- .materialise_duck_input(base_table, con)
  target_table <- .materialise_duck_input(target_table, con)
  base_src   <- base_table$lazy_query$x
  target_src <- target_table$lazy_query$x
  block_by   <- strategy@block_by %||% character()
  proxy      <- .exact_proxy_strategy(strategy)
  tmp <- function(p) paste0(p, "_", sample.int(1e9, 1))

  reg <- new.env(parent = emptyenv()); reg$drop <- character()
  links_tbl <- .exact_links_duck(con, base_src, base_id, target_src, target_id,
                                 strategy, proxy, block_by, self = FALSE, reg)

  n_links <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", links_tbl))$n
  if (n_links == 0L) {
    for (d in unique(c(reg$drop, links_tbl))) DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", d, ";"))
    out_name <- tmp("_joinery_tmp_candidates")
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", out_name, " AS\n",
      "SELECT CAST(NULL AS INTEGER) AS match_id, CAST(NULL AS DOUBLE) AS score,\n",
      "       CAST(NULL AS VARCHAR) AS source, CAST(NULL AS VARCHAR) AS id,\n",
      "       CAST(NULL AS INTEGER) AS rank LIMIT 0;"
    ))
    return(dplyr::tbl(con, out_name))
  }

  # match_id per link, score constant 1.0.
  matched_tbl <- tmp("_joinery_tmp_matched")
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE ", matched_tbl, " AS\n",
    "SELECT ROW_NUMBER() OVER (ORDER BY base_id, target_id) AS match_id,\n",
    "       base_id, target_id, 1.0 AS score FROM ", links_tbl, ";"
  ))

  long_tbl <- tmp("_joinery_tmp_long")
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE ", long_tbl, " AS\n",
    "SELECT match_id, score, 'base' AS source, base_id AS id FROM ", matched_tbl, "\n",
    "UNION ALL\n",
    "SELECT match_id, score, 'target' AS source, target_id AS id FROM ", matched_tbl, ";"
  ))

  base_merge   <- tmp("_joinery_tmp_base_merge")
  target_merge <- tmp("_joinery_tmp_target_merge")
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE ", base_merge, " AS\n",
    "SELECT l.match_id, l.score, l.source, l.id, b.* EXCLUDE (", base_id_q, ")\n",
    "FROM ", long_tbl, " l LEFT JOIN ", base_src, " b ON l.id = b.", base_id_q, "\n",
    "WHERE l.source = 'base';"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE ", target_merge, " AS\n",
    "SELECT l.match_id, l.score, l.source, l.id, t.* EXCLUDE (", target_id_q, ")\n",
    "FROM ", long_tbl, " l LEFT JOIN ", target_src, " t ON l.id = t.", target_id_q, "\n",
    "WHERE l.source = 'target';"
  ))

  cols_base   <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(", base_merge, ");"))$name
  cols_target <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(", target_merge, ");"))$name
  common_cols <- union(cols_base, cols_target)
  sel_base <- paste(vapply(common_cols, function(c)
    if (c %in% cols_base) c else paste0("NULL AS ", c), character(1)), collapse = ", ")
  sel_target <- paste(vapply(common_cols, function(c)
    if (c %in% cols_target) c else paste0("NULL AS ", c), character(1)), collapse = ", ")
  DBI::dbExecute(con, paste0("CREATE OR REPLACE TEMP TABLE ", base_merge,
                             " AS SELECT ", sel_base, " FROM ", base_merge, ";"))
  DBI::dbExecute(con, paste0("CREATE OR REPLACE TEMP TABLE ", target_merge,
                             " AS SELECT ", sel_target, " FROM ", target_merge, ";"))

  out_name <- tmp("_joinery_tmp_candidates")
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", out_name, " AS\n",
    "SELECT match_id, score, source, id,\n",
    "  ROW_NUMBER() OVER (PARTITION BY match_id ORDER BY score DESC NULLS LAST) AS rank,\n",
    "  * EXCLUDE (match_id, score, source, id)\n",
    "FROM (SELECT * FROM ", base_merge, " UNION ALL SELECT * FROM ", target_merge, ")\n",
    "ORDER BY match_id, source, rank;"
  ))

  if (!debug) {
    for (d in unique(c(reg$drop, links_tbl, matched_tbl, long_tbl, base_merge, target_merge))) {
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", d, ";"))
    }
  }
  dplyr::tbl(con, out_name)
}
