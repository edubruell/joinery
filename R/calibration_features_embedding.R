# ============================================================
# match_features() — Embedding_Strategy
# ============================================================
#
# Embedding-side branch of the match_features dispatcher: the
# `.match_features_dt_embedding()` implementation and the S7
# methods dispatching on `Embedding_Strategy` across data.table,
# DuckDB, and tibble / data.frame inputs.
#
# Helpers shared with the token branch (`.pairs_from_matches()`,
# `.string_sim_block()`, `.embedding_norms()`, etc.) live in
# `calibration_features.R`, which Collate loads first.
# ============================================================


#' @noRd
.match_features_dt_embedding <- function(matches, strategy,
                                         base = NULL, id = NULL,
                                         target = NULL, target_id = NULL,
                                         include_block_stats = TRUE,
                                         include_string_sim  = TRUE,
                                         method = "jw",
                                         ...) {
  pair_info  <- .pairs_from_matches(matches)
  pairs      <- pair_info$pairs
  match_type <- pair_info$match_type
  has_stage  <- !all(is.na(pairs$stage))

  # Resolve effective string-similarity columns from strategy / data.
  base_dt <- if (!is.null(base)) {
    bd <- data.table::as.data.table(base)
    if (!is.null(id)) bd[[id]] <- as.character(bd[[id]])
    bd
  } else NULL
  target_dt <- if (!is.null(target)) {
    td <- data.table::as.data.table(target)
    tid <- if (!is.null(target_id)) target_id else id
    if (!is.null(tid)) td[[tid]] <- as.character(td[[tid]])
    td
  } else NULL

  resolved_cols <- if (length(strategy@columns) > 0L) {
    strategy@columns
  } else if (!is.null(base_dt) && !is.null(id)) {
    setdiff(names(base_dt), id)
  } else character()

  if (nrow(pairs) == 0L) {
    return(Match_Features(
      features       = pairs,
      schema         = "embedding",
      strategy_class = "Embedding_Strategy",
      top_n          = integer(),
      columns        = resolved_cols,
      aip_summary    = NULL
    ))
  }

  core <- data.table::copy(pairs[, .(
    searched = as.character(searched),
    found    = as.character(found),
    match_id = match_id,
    stage    = if (has_stage) stage else NA_character_,
    score    = score
  )])

  if (include_block_stats) {
    core[, cnt  := .N, by = searched]
    core[, icnt := data.table::uniqueN(found), by = searched]
    core[, ipos := data.table::frank(score, ties.method = "min") / .N,
         by = searched]
  } else {
    core[, `:=`(cnt = NA_integer_, icnt = NA_integer_, ipos = NA_real_)]
  }

  stage_levels <- if (has_stage) sort(unique(stats::na.omit(pairs$stage))) else character()
  if (length(stage_levels) > 1L) {
    for (lv in stage_levels) {
      colname <- paste0("stage_", make.names(lv))
      core[, (colname) := as.integer(stage == lv)]
    }
  }

  # --- string similarity --------------------------------------------
  if (isTRUE(include_string_sim) && length(resolved_cols) > 0L &&
      !is.null(base_dt) && !is.null(id)) {
    sim_dt <- .string_sim_block(
      pairs      = core[, .(searched, found)],
      base_dt    = base_dt,
      target_dt  = target_dt,
      base_id    = id,
      target_id  = target_id,
      columns    = resolved_cols,
      method     = method
    )
    sim_cols <- setdiff(names(sim_dt), c("searched", "found"))
    for (sc in sim_cols) data.table::set(core, j = sc, value = sim_dt[[sc]])
  }

  # --- cosine_sim (pass-through of score) ---------------------------
  core[, cosine_sim := score]

  # --- embedding norms ----------------------------------------------
  if (!is.null(base_dt) && !is.null(id)) {
    s_ids <- unique(core$searched)
    base_norms <- tryCatch(
      .embedding_norms(base_dt, id, strategy, s_ids),
      error = function(e) NULL
    )
    if (!is.null(base_norms) && nrow(base_norms) > 0L) {
      core[, embedding_norm_s := base_norms$norm[
        match(searched, base_norms$id)
      ]]
    } else {
      core[, embedding_norm_s := NA_real_]
    }
  } else {
    core[, embedding_norm_s := NA_real_]
  }

  tid <- if (!is.null(target_id)) target_id else id
  if (match_type == "candidates" && !is.null(target_dt) && !is.null(tid)) {
    f_ids <- unique(core$found)
    target_norms <- tryCatch(
      .embedding_norms(target_dt, tid, strategy, f_ids),
      error = function(e) NULL
    )
    if (!is.null(target_norms) && nrow(target_norms) > 0L) {
      core[, embedding_norm_f := target_norms$norm[
        match(found, target_norms$id)
      ]]
    } else {
      core[, embedding_norm_f := NA_real_]
    }
  } else if (match_type == "duplicates" && !is.null(base_dt) && !is.null(id)) {
    # dedup: found also comes from base
    f_ids <- unique(core$found)
    base_norms_f <- tryCatch(
      .embedding_norms(base_dt, id, strategy, f_ids),
      error = function(e) NULL
    )
    if (!is.null(base_norms_f) && nrow(base_norms_f) > 0L) {
      core[, embedding_norm_f := base_norms_f$norm[
        match(found, base_norms_f$id)
      ]]
    } else {
      core[, embedding_norm_f := NA_real_]
    }
  } else {
    core[, embedding_norm_f := NA_real_]
  }

  # --- canonical column order ---------------------------------------
  stage_cols   <- grep("^stage_", names(core), value = TRUE)
  ordered_cols <- c(
    "searched", "found", "match_id", "stage", "score",
    "cnt", "icnt", "ipos",
    stage_cols,
    grep("^sim_sf_", names(core), value = TRUE),
    grep("^sim_fs_", names(core), value = TRUE),
    "cosine_sim", "embedding_norm_s", "embedding_norm_f"
  )
  ordered_cols <- intersect(ordered_cols, names(core))
  data.table::setcolorder(core, ordered_cols)

  Match_Features(
    features       = core,
    schema         = "embedding",
    strategy_class = "Embedding_Strategy",
    top_n          = integer(),
    columns        = resolved_cols,
    aip_summary    = NULL
  )
}

# ---------- S7 methods: Embedding_Strategy ---------------------------

method(
  match_features,
  list(DT_tbl, Embedding_Strategy)
) <- function(matches, strategy, base = NULL, id = NULL,
              target = NULL, target_id = NULL,
              include_block_stats = TRUE,
              include_string_sim  = TRUE,
              method = "jw", ...) {

  .match_features_dt_embedding(
    matches             = matches,
    strategy            = strategy,
    base                = base,
    id                  = id,
    target              = target,
    target_id           = target_id,
    include_block_stats = include_block_stats,
    include_string_sim  = include_string_sim,
    method              = method
  )
}

method(
  match_features,
  list(Duck_tbl, Embedding_Strategy)
) <- function(matches, strategy, base = NULL, id = NULL,
              target = NULL, target_id = NULL,
              include_block_stats = TRUE,
              include_string_sim  = TRUE,
              method = "jw", ...) {

  matches_dt <- data.table::as.data.table(dplyr::collect(matches))

  base_dt <- if (!is.null(base) && inherits(base, "tbl_duckdb_connection")) {
    data.table::as.data.table(dplyr::collect(base))
  } else if (!is.null(base)) {
    data.table::as.data.table(base)
  } else NULL

  target_dt <- if (!is.null(target) && inherits(target, "tbl_duckdb_connection")) {
    data.table::as.data.table(dplyr::collect(target))
  } else if (!is.null(target)) {
    data.table::as.data.table(target)
  } else NULL

  match_features(
    matches_dt, strategy,
    base                = base_dt,
    id                  = id,
    target              = target_dt,
    target_id           = target_id,
    include_block_stats = include_block_stats,
    include_string_sim  = include_string_sim,
    method              = method
  )
}

# ---------- tibble / data.frame thin wrappers (Embedding_Strategy) ---

method(
  match_features,
  list(.jyDF, Embedding_Strategy)
) <- function(matches, strategy, ...) {
  match_features(as_DT(matches), strategy, ...)
}

method(
  match_features,
  list(.jyTBL_DF, Embedding_Strategy)
) <- function(matches, strategy, ...) {
  match_features(as_DT(matches), strategy, ...)
}

method(
  match_features,
  list(.jyTBL, Embedding_Strategy)
) <- function(matches, strategy, ...) {
  match_features(as_DT(matches), strategy, ...)
}
