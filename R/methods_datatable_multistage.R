# ============================================================
# data.table backend — residual extraction & multi-stage matching
# ============================================================
#
# `extract_unmatched()` and `multi_stage_search()` methods for
# in-memory data.table inputs.
#
# ============================================================


# Method: extract_unmatched
#------------------------------------------------------------------------------
method(
  extract_unmatched,
  list(DT_tbl, class_character, DT_tbl)
) <- function(data, id, matches) {
  dt <- data.table::copy(data)

  if (!id %in% names(dt)) {
    cli::cli_abort("ID column {.field {id}} not found in data")
  }

  if (!"id" %in% names(matches)) {
    cli::cli_abort("{.arg matches} must contain a column named {.field id}")
  }

  # normalize types
  dt[[id]]      <- as.character(dt[[id]])
  matches[, id := as.character(id)]

  matched_ids <- unique(matches[["id"]])

  # Pre-evaluate dt[[id]] outside dt[i] to avoid data.table column-scope
  # resolution treating the `id` symbol as a column reference when the
  # ID column is literally named "id".
  .id_vals <- dt[[id]]
  dt[!(.id_vals %in% matched_ids)]
}

# Method: multi_stage_search
#------------------------------------------------------------------------------
method(
  multi_stage_search,
  list(DT_tbl, DT_tbl, class_character, class_character, class_list)
) <- function(base_table,
              target_table,
              base_id,
              target_id,
              strategies,
              self        = FALSE,
              source_by   = NULL,
              collapse    = c("none", "rep", "union"),
              rep_rule    = c("canonical", "newest", "longest_lived",
                              "most_complete", "union"),
              rebind      = c("explicit", "self", "accumulate"),
              direction   = c("forward", "backward", "bidirectional"),
              edge_filter = NULL,
              rep_by      = NULL,
              ...) {

  pol      <- .check_search_policy(collapse, rebind, direction, rep_rule)
  rep_rule <- pol$rep_rule

  base_dt   <- data.table::copy(base_table)
  base_dt[[base_id]] <- as.character(base_dt[[base_id]])
  if (self) {
    target_dt <- base_dt
    target_id <- base_id
  } else {
    target_dt <- data.table::copy(target_table)
    target_dt[[target_id]] <- as.character(target_dt[[target_id]])
  }

  extra_cols <- c(source_by, rep_by)
  for (cc in extra_cols) {
    if (!cc %in% names(base_dt) || (!self && !cc %in% names(target_dt))) {
      cli::cli_abort("Column {.field {cc}} ({.arg source_by}/{.arg rep_by}) must exist on both tables.")
    }
  }

  staged <- .run_staged_search(
    base = base_dt, target = target_dt, base_id = base_id, target_id = target_id,
    strategies = strategies, self = self, source_by = source_by,
    collapse = pol$collapse, rep_rule = rep_rule, rebind = pol$rebind,
    direction = pol$direction, edge_filter = edge_filter, rep_by = rep_by
  )

  # Pooled vertex / source map: one row per record across both sides.
  vslice <- function(dt, idcol) {
    cols <- c(idcol, extra_cols)
    s <- dt[, cols, with = FALSE]
    data.table::setnames(s, idcol, "id")
    s[, id := as.character(id)]
    s
  }
  vertices <- if (self) {
    vslice(base_dt, base_id)
  } else {
    data.table::rbindlist(list(vslice(base_dt, base_id),
                               vslice(target_dt, target_id)),
                          use.names = TRUE, fill = TRUE)
  }
  vertices <- unique(vertices, by = "id")

  .finalize_search_grouping(staged$ledger, vertices, source_by, rep_by)
}

# Method: multi_stage_dedup
#------------------------------------------------------------------------------
# Staged dedup over a single table: accumulate the links every stage finds and
# resolve connected components ONCE at the end (so A~B in stage 1 + B~C in
# stage 3 -> one entity). Composes the shared engine (R/internal_staging.R) +
# resolve_entities (the only CC) + materialize_records (the only rehydrate).
method(
  multi_stage_dedup,
  list(DT_tbl, class_character, class_list)
) <- function(table, id, strategies,
              rep_by = NULL, edge_filter = NULL, ...) {

  dt <- data.table::copy(table)
  if (!id %in% names(dt)) {
    cli::cli_abort("ID column {.field {id}} not found in {.arg table}.")
  }
  dt[[id]] <- as.character(dt[[id]])
  if (!is.null(rep_by) && !rep_by %in% names(dt)) {
    cli::cli_abort("{.arg rep_by} ({.val {rep_by}}) must be a column in {.arg table}.")
  }

  all_ids <- unique(dt[[id]])

  staged <- .run_staged_dedup(dt, id, strategies, all_ids,
                              edge_filter = edge_filter)
  edges <- staged$edges

  empty_out <- function() {
    out <- data.table::data.table(
      duplicate_group = integer(), id = character(),
      score = numeric(), rank = integer(), stage = character()
    )
    merge(out, dt, by.x = "id", by.y = id, all.x = TRUE, sort = FALSE)[]
  }
  if (nrow(edges) == 0L) return(empty_out())

  # --- final connected components over ALL accumulated edges ----------------
  verts <- if (is.null(rep_by)) {
    all_ids
  } else {
    unique(dt[, c(id, rep_by), with = FALSE], by = id) |>
      data.table::setnames(id, "id")
  }
  ent <- resolve_entities(
    edges    = edges[, .(from, to, score)],
    id_a     = "from",
    id_b     = "to",
    score    = "score",
    vertices = verts,
    rep_by   = rep_by
  )
  ent <- ent[!is.na(score)]                 # drop singletons -> only duplicates
  if (nrow(ent) == 0L) return(empty_out())

  # --- stage that first linked each record ----------------------------------
  stage_levels <- unique(edges$stage)
  long <- data.table::rbindlist(list(
    edges[, .(id = from, stage)],
    edges[, .(id = to,   stage)]
  ))
  long[, so := match(stage, stage_levels)]
  first_stage <- long[, .(stage = stage_levels[min(so)]), by = "id"]

  data.table::setnames(ent, "entity", "duplicate_group")
  result <- ent[, .(id, duplicate_group, score, rank)]
  result <- first_stage[result, on = "id"]
  data.table::setcolorder(result, c("duplicate_group", "id", "score", "rank", "stage"))
  data.table::setkeyv(result, c("duplicate_group", "rank"))

  merge(result, dt, by.x = "id", by.y = id, all.x = TRUE, sort = FALSE)[]
}
