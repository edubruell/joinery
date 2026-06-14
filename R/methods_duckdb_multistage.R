if (!requireNamespace("duckdb", quietly = TRUE) ||
    !requireNamespace("DBI", quietly = TRUE) ||
    !requireNamespace("dplyr", quietly = TRUE)) {
  return(invisible(NULL))
}

Duck_tbl <- new_S3_class("tbl_duckdb_connection")

  
# Method: extract_unmatched for DuckDB
#------------------------------------------------------------------------------
method(
  extract_unmatched,
  list(Duck_tbl, class_character, Duck_tbl)
) <- function(data, id, matches) {
  
  con <- data$src$con
  data    <- .materialise_duck_input(data, con)
  matches <- .materialise_duck_input(matches, con)

  data_tbl    <- data$lazy_query$x
  matches_tbl <- matches$lazy_query$x
  
  id_q <- sprintf('"%s"', id)
  
  # Validate: id column exists in data table
  data_cols <- DBI::dbGetQuery(
    con,
    paste0("PRAGMA table_info(", data_tbl, ");")
  )$name

  if (!id %in% data_cols) {
    cli::cli_abort("ID column {.field {id}} not found in data")
  }

  # Validate: matches table has an 'id' column
  matches_cols <- DBI::dbGetQuery(
    con,
    paste0("PRAGMA table_info(", matches_tbl, ");")
  )$name

  if (!"id" %in% matches_cols) {
    cli::cli_abort("{.arg matches} must contain a column named {.field id}")
  }
  
  # Generate output table name
  out_name <- paste0("_joinery_tmp_unmatched_", sample.int(1e9, 1))
  
  # SQL anti-join: keep rows where id NOT IN matched set
  sql <- paste0(
    "CREATE TABLE ", out_name, " AS\n",
    "SELECT d.*\n",
    "FROM ", data_tbl, " AS d\n",
    "WHERE d.", id_q, " NOT IN (\n",
    "  SELECT DISTINCT id\n",
    "  FROM ", matches_tbl, "\n",
    ");"
  )
  
  DBI::dbExecute(con, sql)
  dplyr::tbl(con, out_name)
}
  
  
# Method: multi_stage_search for DuckDB
#------------------------------------------------------------------------------
# Multi-source staged entity resolution over DuckDB tables. The shared engine
# (R/internal_staging.R) drives the per-stage search_candidates() /
# extract_unmatched() / materialize_records() over DuckDB via S7 dispatch,
# collecting only the matched pairs per stage into the directed ledger. The
# ledger resolves through resolve_entities() (the ONE CC) into the cross-source
# entity grouping; both grouping and ledger are written back as DuckDB temp
# tables, the ledger riding as the `"ledger"` attribute.
method(
  multi_stage_search,
  list(Duck_tbl, Duck_tbl, class_character, class_character, class_list)
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
              control     = duckdb_control(),
              ...) {

  con      <- base_table$src$con
  pol      <- .check_search_policy(collapse, rebind, direction, rep_rule)
  rep_rule <- pol$rep_rule

  base_table <- .materialise_duck_input(base_table, con)
  base_tbl   <- base_table$lazy_query$x
  if (self) {
    target_table <- base_table
    target_id    <- base_id
  } else {
    target_table <- .materialise_duck_input(target_table, con)
  }
  target_tbl <- target_table$lazy_query$x

  staged <- .run_staged_search(
    base = base_table, target = target_table, base_id = base_id,
    target_id = target_id, strategies = strategies, self = self,
    source_by = source_by, collapse = pol$collapse, rep_rule = rep_rule,
    rebind = pol$rebind, direction = pol$direction,
    edge_filter = edge_filter, rep_by = rep_by, control = control
  )

  # Pooled vertex / source map (collect id + source_by + rep_by columns).
  extra_cols <- c(source_by, rep_by)
  proj <- function(idq, cols) {
    sel <- paste0("CAST(", idq, " AS VARCHAR) AS id")
    if (length(cols)) sel <- paste(sel, paste(sprintf('"%s"', cols), collapse = ", "), sep = ", ")
    sel
  }
  bid_q <- sprintf('"%s"', base_id)
  tid_q <- sprintf('"%s"', target_id)
  v_sql <- paste0("SELECT DISTINCT ", proj(bid_q, extra_cols), " FROM ", base_tbl)
  if (!self) {
    v_sql <- paste0(v_sql, "\nUNION\nSELECT DISTINCT ", proj(tid_q, extra_cols),
                    " FROM ", target_tbl)
  }
  vertices <- data.table::as.data.table(DBI::dbGetQuery(con, paste0(v_sql, ";")))

  grouping <- .finalize_search_grouping(staged$ledger, vertices, source_by, rep_by)
  ledger   <- attr(grouping, "ledger", exact = TRUE)

  grp_tbl <- paste0("_joinery_tmp_mss_entities_", sample.int(1e9, 1))
  DBI::dbWriteTable(con, grp_tbl, as.data.frame(grouping))  # ledger attr ignored
  out <- dplyr::tbl(con, grp_tbl)

  led_tbl <- paste0("_joinery_tmp_mss_ledger_", sample.int(1e9, 1))
  DBI::dbWriteTable(con, led_tbl, as.data.frame(ledger))
  attr(out, "ledger") <- dplyr::tbl(con, led_tbl)
  out
}


# Method: multi_stage_dedup for DuckDB
#------------------------------------------------------------------------------
# Staged dedup over a single DuckDB table. The shared engine
# (R/internal_staging.R) drives the per-stage detect_duplicates() /
# materialize_records() over the DuckDB table via S7 dispatch, collecting only
# the small per-stage edge sets to R. The accumulated edges resolve through the
# data.table resolve_entities (the ONE CC — over an edge set tiny relative to
# the corpus, which also sidesteps the DuckDB resolve_entities coverage gap),
# and the grouping joins back to the corpus in SQL.
method(
  multi_stage_dedup,
  list(Duck_tbl, class_character, class_list)
) <- function(table, id, strategies,
              rep_by = NULL, edge_filter = NULL, control = duckdb_control(), ...) {

  con   <- table$src$con
  table <- .materialise_duck_input(table, con)
  data_tbl <- table$lazy_query$x
  id_q  <- sprintf('"%s"', id)

  data_cols <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(", data_tbl, ");"))$name
  if (!id %in% data_cols) {
    cli::cli_abort("ID column {.field {id}} not found in {.arg table}.")
  }
  if (!is.null(rep_by) && !rep_by %in% data_cols) {
    cli::cli_abort("{.arg rep_by} ({.val {rep_by}}) must be a column in {.arg table}.")
  }

  all_ids <- DBI::dbGetQuery(
    con, paste0("SELECT DISTINCT CAST(", id_q, " AS VARCHAR) AS id FROM ", data_tbl, ";")
  )$id

  staged <- .run_staged_dedup(table, id, strategies, all_ids,
                              edge_filter = edge_filter, control = control)
  edges <- staged$edges

  # Join an R-side result table (id + dedup cols) back to the corpus in SQL.
  emit <- function(result) {
    res_tbl <- paste0("_joinery_tmp_msd_result_", sample.int(1e9, 1))
    DBI::dbWriteTable(con, res_tbl, as.data.frame(result), temporary = TRUE)
    out_name <- paste0("_joinery_tmp_multistage_dedup_", sample.int(1e9, 1))
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", out_name, " AS\n",
      "SELECT r.duplicate_group, r.id, r.score, r.rank, r.stage,\n",
      "       d.* EXCLUDE (", id_q, ")\n",
      "FROM ", res_tbl, " AS r\n",
      "JOIN ", data_tbl, " AS d ON CAST(d.", id_q, " AS VARCHAR) = r.id\n",
      "ORDER BY r.duplicate_group, r.rank;"
    ))
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", res_tbl, ";"))
    dplyr::tbl(con, out_name)
  }

  empty_result <- data.table::data.table(
    duplicate_group = integer(), id = character(),
    score = numeric(), rank = integer(), stage = character()
  )
  if (nrow(edges) == 0L) return(emit(empty_result))

  verts <- if (is.null(rep_by)) {
    all_ids
  } else {
    v <- DBI::dbGetQuery(con, paste0(
      "SELECT DISTINCT CAST(", id_q, " AS VARCHAR) AS id, ",
      sprintf('"%s"', rep_by), " FROM ", data_tbl, ";"
    ))
    data.table::as.data.table(v)
  }

  ent <- resolve_entities(
    edges    = edges[, .(from, to, score)],
    id_a     = "from", id_b = "to", score = "score",
    vertices = verts, rep_by = rep_by
  )
  ent <- ent[!is.na(score)]
  if (nrow(ent) == 0L) return(emit(empty_result))

  stage_levels <- unique(edges$stage)
  long <- data.table::rbindlist(list(
    edges[, .(id = from, stage)], edges[, .(id = to, stage)]
  ))
  long[, so := match(stage, stage_levels)]
  first_stage <- long[, .(stage = stage_levels[min(so)]), by = "id"]

  data.table::setnames(ent, "entity", "duplicate_group")
  result <- ent[, .(id, duplicate_group, score, rank)]
  result <- first_stage[result, on = "id"]
  data.table::setcolorder(result, c("duplicate_group", "id", "score", "rank", "stage"))
  emit(result)
}
