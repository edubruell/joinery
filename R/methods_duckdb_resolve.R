if (!requireNamespace("duckdb", quietly = TRUE) ||
    !requireNamespace("DBI", quietly = TRUE) ||
    !requireNamespace("dplyr", quietly = TRUE)) {
  return(invisible(NULL))
}

# ============================================================
# DuckDB backend — entity resolution (connected components)
# ============================================================
#
# `resolve_entities()` method for DuckDB edge-list relations. This is the
# shared connected-components kernel that `detect_duplicates()` delegates
# to; see `R/methods_duckdb_dedup.R`.
#
# The recursive-CTE connected-components step iterates *per block* when
# `block_by` is supplied, so a large corpus never runs one global recursion
# on disk (the v08 §14 performance contract). Edges are assumed to be
# within-block by construction, so both endpoints of an edge share the same
# block tuple.
# ============================================================

method(
  resolve_entities,
  list(Duck_tbl, class_character, class_character)
) <- function(edges, id_a, id_b, score = NULL, vertices = NULL,
              rep_by = NULL, block_by = NULL, debug = FALSE, ...) {

  con <- edges$src$con
  tmp <- function(prefix) paste0(prefix, "_", sample.int(1e9, 1))

  has_score <- !is.null(score)
  block_by  <- block_by %||% character()

  # Materialise so `$lazy_query$x` is a bare table name we can read from.
  edges <- .materialise_duck_input(edges, con)
  edges_src <- edges$lazy_query$x

  a_q <- sprintf('"%s"', id_a)
  b_q <- sprintf('"%s"', id_b)
  sc_sel <- if (has_score) {
    sprintf(', "%s" AS score', score)
  } else {
    ", CAST(NULL AS DOUBLE) AS score"
  }
  blk_sel <- if (length(block_by)) {
    paste0(", ", paste(sprintf('"%s"', block_by), collapse = ", "))
  } else ""

  # ----------------------------------------------------------
  # 1. Symmetric edge set (both directions), carrying score + block cols.
  # ----------------------------------------------------------
  edges_tbl <- tmp("_joinery_tmp_edges")
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE ", edges_tbl, " AS\n",
    "SELECT ", a_q, " AS a, ", b_q, " AS b", sc_sel, blk_sel,
    " FROM ", edges_src, "\n",
    "UNION ALL\n",
    "SELECT ", b_q, " AS a, ", a_q, " AS b", sc_sel, blk_sel,
    " FROM ", edges_src, ";"
  ))

  # ----------------------------------------------------------
  # 2. Connected components — iterate per block.
  # ----------------------------------------------------------
  comp_tbl <- tmp("_joinery_tmp_components")

  cc_select_sql <- function(edges_src) {
    paste0(
      "WITH RECURSIVE cc AS (\n",
      "  SELECT a AS node, a AS label FROM ", edges_src, "\n",
      "  UNION\n",
      "  SELECT e.b AS node, MIN(cc.label) AS label\n",
      "  FROM ", edges_src, " e\n",
      "  JOIN cc ON e.a = cc.node\n",
      "  WHERE cc.label < e.b\n",
      "  GROUP BY e.b\n",
      ")\n",
      "SELECT node AS id, MIN(label) AS root\n",
      "FROM cc\n",
      "GROUP BY node\n"
    )
  }

  cc_wall_start <- Sys.time()

  if (!length(block_by)) {
    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE ", comp_tbl, " AS\n",
      cc_select_sql(edges_tbl), ";"
    ))
  } else {
    bsel <- paste(sprintf('"%s"', block_by), collapse = ", ")
    blocks <- DBI::dbGetQuery(con, paste0(
      "SELECT DISTINCT ", bsel, " FROM ", edges_tbl
    ))

    initialised <- FALSE
    blk_edges <- tmp("_joinery_tmp_blk_edges")

    for (b in seq_len(nrow(blocks))) {
      conds <- vapply(block_by, function(col) {
        v <- blocks[[col]][b]
        if (is.na(v)) {
          sprintf('"%s" IS NULL', col)
        } else {
          sprintf('"%s" = %s', col, DBI::dbQuoteLiteral(con, v))
        }
      }, character(1))
      filter_clause <- paste0(" WHERE ", paste(conds, collapse = " AND "))

      DBI::dbExecute(con, paste0(
        "CREATE OR REPLACE TEMP TABLE ", blk_edges, " AS\n",
        "SELECT a, b FROM ", edges_tbl, filter_clause, ";"
      ))

      if (!initialised) {
        DBI::dbExecute(con, paste0(
          "CREATE TEMP TABLE ", comp_tbl, " AS\n",
          cc_select_sql(blk_edges), ";"
        ))
        initialised <- TRUE
      } else {
        DBI::dbExecute(con, paste0(
          "INSERT INTO ", comp_tbl, "\n",
          cc_select_sql(blk_edges), ";"
        ))
      }
    }

    if (initialised) {
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", blk_edges))
    } else {
      DBI::dbExecute(con, paste0(
        "CREATE TEMP TABLE ", comp_tbl, " AS\n",
        cc_select_sql(edges_tbl), ";"
      ))
    }
  }

  cc_wall_seconds <- as.numeric(
    difftime(Sys.time(), cc_wall_start, units = "secs")
  )

  # ----------------------------------------------------------
  # 3. Optional: fold in singleton vertices (ids with no edge).
  # ----------------------------------------------------------
  vtab_name <- NULL
  if (!is.null(vertices)) {
    vtab_name <- tmp("_joinery_tmp_vertices")
    if (inherits(vertices, "tbl_duckdb_connection")) {
      vmat <- .materialise_duck_input(vertices, con)
      DBI::dbExecute(con, paste0(
        "CREATE TEMP TABLE ", vtab_name, " AS SELECT * FROM ",
        vmat$lazy_query$x, ";"
      ))
    } else {
      DBI::dbWriteTable(con, vtab_name, as.data.frame(vertices), temporary = TRUE)
    }
    # ids present in vertices but absent from any edge become own root.
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", comp_tbl, "\n",
      "SELECT v.id AS id, v.id AS root\n",
      "FROM ", vtab_name, " v\n",
      "WHERE v.id NOT IN (SELECT id FROM ", comp_tbl, ");"
    ))
  }

  # ----------------------------------------------------------
  # 4. Best score per id (max over incident edges).
  # ----------------------------------------------------------
  best_tbl <- tmp("_joinery_best_scores")
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE ", best_tbl, " AS\n",
    "SELECT a AS id, MAX(score) AS score FROM ", edges_tbl, " GROUP BY a;"
  ))

  # ----------------------------------------------------------
  # 5. Final: entity label, rep, rank.
  # ----------------------------------------------------------
  order_terms <- character()
  if (has_score)        order_terms <- c(order_terms, "b.score DESC NULLS LAST")
  if (!is.null(rep_by)) order_terms <- c(order_terms, sprintf('v."%s" ASC', rep_by))
  order_terms <- c(order_terms, "c.id ASC")
  order_sql <- paste(order_terms, collapse = ", ")

  rep_join <- if (!is.null(rep_by)) {
    paste0("LEFT JOIN ", vtab_name, " v ON c.id = v.id\n")
  } else ""

  out_name <- tmp("_joinery_tmp_resolved")
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE ", out_name, " AS\n",
    "SELECT c.id AS id,\n",
    "       DENSE_RANK() OVER (ORDER BY c.root) AS entity,\n",
    "       FIRST_VALUE(c.id) OVER (\n",
    "         PARTITION BY c.root ORDER BY ", order_sql, "\n",
    "       ) AS rep,\n",
    "       ROW_NUMBER() OVER (\n",
    "         PARTITION BY c.root ORDER BY ", order_sql, "\n",
    "       ) AS rank",
    if (has_score) ",\n       b.score AS score\n" else "\n",
    "FROM ", comp_tbl, " c\n",
    "LEFT JOIN ", best_tbl, " b ON c.id = b.id\n",
    rep_join,
    "ORDER BY entity, rank;"
  ))

  # `out_name` already carries exactly id | entity | rep | rank [| score],
  # so the returned tbl keeps a bare table name in `$lazy_query$x`.
  result <- dplyr::tbl(con, out_name)

  if (!debug) {
    drop <- c(edges_tbl, comp_tbl, best_tbl, vtab_name)
    walk(drop[!vapply(drop, is.null, logical(1))], function(tbl) {
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tbl))
    })
  }

  attr(result, "wall_seconds") <- cc_wall_seconds
  result
}
