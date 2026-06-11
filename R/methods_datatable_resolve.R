# ============================================================
# data.table backend — entity resolution (connected components)
# ============================================================
#
# `resolve_entities()` method for in-memory data.table edge lists.
# This is the shared connected-components kernel that `detect_duplicates()`
# delegates to; see `R/methods_datatable_dedup.R`.
#
# ============================================================


# Method: resolve_entities
#------------------------------------------------------------------------------
method(
  resolve_entities,
  list(DT_tbl, class_character, class_character)
) <- function(edges, id_a, id_b, score = NULL, vertices = NULL,
              rep_by = NULL, block_by = NULL, ...) {

  edges <- data.table::as.data.table(edges)

  if (!all(c(id_a, id_b) %in% names(edges))) {
    cli::cli_abort(
      "Endpoint columns {.field {setdiff(c(id_a, id_b), names(edges))}} not found in {.arg edges}."
    )
  }

  has_score <- !is.null(score)
  if (has_score && !score %in% names(edges)) {
    cli::cli_abort("{.arg score} column {.val {score}} not found in {.arg edges}.")
  }

  # --- vertices table (id [+ rep_by]) --------------------------------------
  if (is.null(vertices)) {
    vtab <- NULL
  } else if (is.data.frame(vertices)) {
    vtab <- data.table::as.data.table(vertices)
    if (!"id" %in% names(vtab)) {
      cli::cli_abort("{.arg vertices} table must contain an {.field id} column.")
    }
    vtab <- data.table::copy(vtab)
    vtab[, id := as.character(id)]
    vtab <- unique(vtab, by = "id")
  } else {
    vtab <- data.table::data.table(id = unique(as.character(vertices)))
  }

  if (!is.null(rep_by)) {
    if (is.null(vtab) || !rep_by %in% names(vtab)) {
      cli::cli_abort("{.arg rep_by} ({.val {rep_by}}) must be a column in the {.arg vertices} table.")
    }
  }

  from <- as.character(edges[[id_a]])
  to   <- as.character(edges[[id_b]])

  all_ids <- if (!is.null(vtab)) vtab$id else unique(c(from, to))

  # --- empty: no edges and no vertices -------------------------------------
  if (length(all_ids) == 0L) {
    out <- data.table::data.table(
      id = character(), entity = integer(), rep = character(), rank = integer()
    )
    if (has_score) out[, score := numeric()]
    return(out[])
  }

  # --- connected components ------------------------------------------------
  if (length(from) == 0L) {
    # No edges: every vertex is its own component.
    memb <- data.table::data.table(id = all_ids, cc = seq_along(all_ids))
  } else {
    und <- data.table::data.table(from = c(from, to), to = c(to, from))
    g <- igraph::graph_from_data_frame(und, directed = FALSE, vertices = all_ids)
    comp <- igraph::components(g)
    memb <- data.table::data.table(
      id = names(comp$membership),
      cc = unname(comp$membership)
    )
  }

  # --- best score per id (max over incident edges) -------------------------
  if (has_score && length(from) > 0L) {
    sl <- data.table::rbindlist(list(
      data.table::data.table(id = from, score = edges[[score]]),
      data.table::data.table(id = to,   score = edges[[score]])
    ))
    best <- sl[, .(score = max(score, na.rm = TRUE)), by = "id"]
    memb <- best[memb, on = "id"]            # keep all vertices; NA score for singletons
  } else {
    memb[, score := NA_real_]
  }

  # --- attach rep_by priority ----------------------------------------------
  if (!is.null(rep_by)) {
    memb <- vtab[, c("id", rep_by), with = FALSE][memb, on = "id"]
  }

  # --- within-component order → rank, rep ----------------------------------
  ord_cols <- character()
  ord_dir  <- integer()
  if (has_score)        { ord_cols <- c(ord_cols, "score");  ord_dir <- c(ord_dir, -1L) }
  if (!is.null(rep_by)) { ord_cols <- c(ord_cols, rep_by);   ord_dir <- c(ord_dir,  1L) }
  ord_cols <- c(ord_cols, "id"); ord_dir <- c(ord_dir, 1L)

  data.table::setorderv(memb, ord_cols, ord_dir)
  memb[, rank := seq_len(.N), by = "cc"]
  memb[, rep  := id[1L],      by = "cc"]

  # --- entity = dense rank over component root (smallest member id) --------
  memb[, root := min(id), by = "cc"]
  roots <- sort(unique(memb$root))
  memb[, entity := match(root, roots)]

  data.table::setorderv(memb, c("entity", "rank"))
  out_cols <- c("id", "entity", "rep", "rank")
  if (has_score) out_cols <- c(out_cols, "score")
  memb[, out_cols, with = FALSE][]
}
