# ============================================================
# match_features()
# ============================================================
#
# Builds a wide, one-row-per-pair feature `data.table` from a joinery
# match result. Schema documented in notes/calibration_design.md and
# treated as the public API — additions only, no reorders or renames.
#
# Core columns (always present):
#   searched, found, match_id, stage, score, cnt, icnt, ipos,
#   stage_<level>      (one-hots, multi-stage only)
#
# Token-strategy columns (Search_Strategy only):
#   scnt, rcnt, r1..rn, m_<col>_1..topN, f_<col>_1..topN,
#   s_<col>_1..topN
#
# String similarity columns (both strategy classes):
#   sim_sf_<col>, sim_fs_<col>      (search→found, found→search)
#
# Embedding-strategy columns (Embedding_Strategy only):
#   cosine_sim, embedding_norm_s, embedding_norm_f
# ============================================================


# ---------- helpers ---------------------------------------------------

#' @noRd
.strip_block_by <- function(strategy) {
  if (!is.null(strategy@block_by)) {
    S7::prop(strategy, "block_by") <- NULL
  }
  strategy
}

#' @noRd
.resolve_top_n <- function(top_n, columns) {
  if (is.null(top_n)) top_n <- list(default = 5L)
  if (is.numeric(top_n) && length(top_n) == 1L && is.null(names(top_n))) {
    top_n <- list(default = as.integer(top_n))
  }
  if (!is.list(top_n)) {
    top_n <- as.list(top_n)
  }
  if (is.null(top_n$default)) top_n$default <- 5L

  out <- map_int(columns, function(col) {
    v <- top_n[[col]]
    if (is.null(v)) v <- top_n$default
    as.integer(v)
  })
  names(out) <- columns
  out
}

#' Build long-form (id, src_column, token) unique pairs.
#' @noRd
.unique_record_tokens <- function(tokens, id_col) {
  dt <- data.table::as.data.table(tokens)[
    , .SD,
    .SDcols = c(id_col, "src_column", "token")
  ]
  data.table::setnames(dt, id_col, "_rid_")
  # The internal key is character; coerce so a numeric/integer64 token id keys
  # the same way the pair ids do (see .pairs_from_matches). Avoids a bmerge
  # "Incompatible join types" when matches carry a non-character id (§B2).
  dt[, `_rid_` := .as_id_chr(`_rid_`)]
  unique(dt[!is.na(token) & nzchar(token)])
}

#' Build the pair frame (searched, found, match_id, score, stage) from matches.
#' @noRd
.pairs_from_matches <- function(matches) {
  dt <- data.table::as.data.table(matches)
  mt <- .detect_match_type(dt)
  has_stage <- "stage" %in% names(dt)

  if (mt == "candidates") {
    base_rows   <- dt[dt$source == "base",   ]
    target_rows <- dt[dt$source == "target", ]
    if (nrow(base_rows) == 0L || nrow(target_rows) == 0L) {
      return(list(
        match_type = mt,
        pairs      = data.table::data.table(
          searched = character(), found = character(),
          match_id = integer(),  score = numeric(),
          stage    = character()
        )
      ))
    }
    cols_pick <- c("match_id", "score", "id", if (has_stage) "stage")
    b <- base_rows[, ..cols_pick]
    t <- target_rows[, ..cols_pick]
    data.table::setnames(b, "id", "searched")
    data.table::setnames(t, "id", "found")
    drop <- intersect(c("score", "stage"), names(t))
    t <- t[, setdiff(names(t), drop), with = FALSE]
    pairs <- merge(b, t, by = "match_id", all = FALSE)
  } else {
    # duplicates
    if (nrow(dt) == 0L) {
      return(list(
        match_type = mt,
        pairs      = data.table::data.table(
          searched = character(), found = character(),
          match_id = integer(),  score = numeric(),
          stage    = character()
        )
      ))
    }
    cols_pick <- c("duplicate_group", "id", "score", "rank",
                   if (has_stage) "stage")
    grp <- dt[, ..cols_pick]
    data.table::setorder(grp, duplicate_group, rank)
    # Drop singletons
    keep <- grp[, .N, by = duplicate_group][N >= 2L, duplicate_group]
    grp <- grp[duplicate_group %in% keep]
    if (nrow(grp) == 0L) {
      return(list(
        match_type = mt,
        pairs      = data.table::data.table(
          searched = character(), found = character(),
          match_id = integer(),  score = numeric(),
          stage    = character()
        )
      ))
    }
    rank1 <- grp[rank == 1L, .(duplicate_group, searched = id,
                               .stage1 = if (has_stage) stage else NA_character_)]
    rest  <- grp[rank >= 2L, ]
    pairs <- merge(rest, rank1, by = "duplicate_group", all.x = TRUE)
    pairs[, found := id]
    pairs[, match_id := duplicate_group]
    if (has_stage) {
      # rank-k stage trumps rank1 stage (per-row stage of the candidate)
      pairs[, stage := stage]
    } else {
      pairs[, stage := NA_character_]
    }
    pairs <- pairs[, .(match_id, score, searched, found, stage)]
  }

  if (!has_stage) pairs[, stage := NA_character_]
  # Coerce the pair ids to the internal character key so a numeric/integer64
  # match id (e.g. a BIGINT surrogate `rid`) merges cleanly against the token
  # table's `_rid_` instead of aborting bmerge with "Incompatible join types"
  # (§B2). .as_id_chr renders integer-valued doubles in plain decimal.
  pairs[, searched := .as_id_chr(searched)]
  pairs[, found    := .as_id_chr(found)]
  list(match_type = mt, pairs = pairs)
}

#' Per-column top-N aIP within an arbitrary token set, padded with NA.
#'
#' @param tokens_dt A data.table with columns `_rid_`, `src_column`, `token`,
#'   `aip` already attached (NA aip is dropped before ranking).
#' @param prefix Character prefix (`"m"`, `"f"`, `"s"`).
#' @param columns Character vector of strategy columns (canonical order).
#' @param top_n Named integer vector of effective top-N per column.
#' @param record_ids Character vector of record ids forming the rows.
#'
#' @return Wide data.table with one row per record id, columns
#'   `<prefix>_<col>_1..topN`. Returns a 0-column data.table when all
#'   per-column top_n are zero.
#' @noRd
.topN_wide <- function(tokens_dt, prefix, columns, top_n, record_ids) {

  base <- data.table::data.table(`_rid_` = record_ids)

  for (col in columns) {
    k <- as.integer(top_n[[col]])
    if (is.na(k) || k <= 0L) next

    sub <- tokens_dt[src_column == col & !is.na(aip)]
    data.table::setorder(sub, `_rid_`, -aip, token)
    sub[, .pos := seq_len(.N), by = "_rid_"]
    sub <- sub[.pos <= k]

    new_cols <- paste0(prefix, "_", col, "_", seq_len(k))

    if (nrow(sub) == 0L) {
      for (cn in new_cols) base[, (cn) := NA_real_]
      next
    }

    wide <- data.table::dcast(
      sub, `_rid_` ~ .pos,
      value.var = "aip",
      fill      = NA_real_
    )
    # ensure all expected columns present
    have <- setdiff(names(wide), "_rid_")
    have_int <- suppressWarnings(as.integer(have))
    rename_map <- paste0(prefix, "_", col, "_", have_int)
    data.table::setnames(wide, have, rename_map)

    missing <- setdiff(new_cols, names(wide))
    for (cn in missing) wide[, (cn) := NA_real_]

    base <- merge(base, wide[, c("_rid_", new_cols), with = FALSE],
                  by = "_rid_", all.x = TRUE)
    # fill any newly created NA-only joins with NA_real_
    for (cn in new_cols) {
      if (!is.numeric(base[[cn]])) base[[cn]] <- as.numeric(base[[cn]])
    }
  }

  base
}


# ---------- string similarity ---------------------------------------

#' Per-pair, per-column string similarity using `stringdist`.
#'
#' @param pairs data.table with columns `searched`, `found` (character ids).
#' @param base_dt data.table with `id_col` and the columns in `columns`.
#' @param target_dt data.table or NULL. When NULL (dedup), `base_dt` is also
#'   used for the found-side lookup.
#' @param base_id character scalar — id column in `base_dt`.
#' @param target_id character scalar — id column in `target_dt` (defaults
#'   to `base_id`).
#' @param columns character vector — columns to compute string similarity on.
#'   Columns absent from `base_dt` or `target_dt` are silently skipped.
#' @param method stringdist method. Scalar applied to every column
#'   (default `"jw"`), or a named character vector for per-column methods
#'   (scalar is the degenerate single-element case).
#'
#' @return data.table with one row per input pair (in input order) carrying
#'   `sim_sf_<col>` and `sim_fs_<col>` columns for each col in `columns`.
#' @noRd
.string_sim_block <- function(pairs, base_dt, target_dt = NULL,
                              base_id, target_id = NULL,
                              columns, method = "jw") {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    cli::cli_abort(c(
      "String similarity columns require the {.pkg stringdist} package.",
      "i" = "Install it with {.code install.packages('stringdist')}.",
      "i" = "Or call {.code match_features(..., include_string_sim = FALSE)}."
    ))
  }
  if (is.null(target_id)) target_id <- base_id
  if (is.null(target_dt)) target_dt <- base_dt

  out <- data.table::data.table(
    searched = as.character(pairs$searched),
    found    = as.character(pairs$found)
  )

  # Build named lookups id -> field value for fast indexing
  base_ids   <- as.character(base_dt[[base_id]])
  target_ids <- as.character(target_dt[[target_id]])

  for (col in columns) {
    if (!col %in% names(base_dt) || !col %in% names(target_dt)) next
    s_vals <- as.character(base_dt[[col]])[match(out$searched, base_ids)]
    f_vals <- as.character(target_dt[[col]])[match(out$found,   target_ids)]

    sim_sf <- stringdist::stringsim(s_vals, f_vals, method = method)
    sim_fs <- stringdist::stringsim(f_vals, s_vals, method = method)

    out[[paste0("sim_sf_", col)]] <- sim_sf
    out[[paste0("sim_fs_", col)]] <- sim_fs
  }

  out
}


#' Compute pre-normalization L2 norm per record by recomputing embeddings
#' under a strategy with `normalize = FALSE`.
#'
#' Returns a `data.table(id, norm)` keyed by `id`. The `id` column is named
#' as supplied (`id_col`).
#'
#' @noRd
.embedding_norms <- function(data, id_col, strategy, ids_needed) {
  s_unnorm <- strategy
  S7::prop(s_unnorm, "normalize") <- FALSE

  data <- data.table::copy(data.table::as.data.table(data))
  data[[id_col]] <- as.character(data[[id_col]])
  subset <- data[data[[id_col]] %in% ids_needed]

  if (nrow(subset) == 0L) {
    return(data.table::data.table(
      id = character(), norm = numeric()
    ))
  }

  emb <- compute_embeddings(subset, id_col, s_unnorm)
  # `compute_embeddings` returns id column named after `id_col`.
  id_vals <- as.character(emb[[id_col]])
  norms <- map_dbl(emb$embedding, function(v) sqrt(sum(v * v)))
  data.table::data.table(id = id_vals, norm = norms)
}


# ---------- core implementation: data.table backend, token strategy ---

#' @noRd
.match_features_dt_token <- function(matches, strategy, base, id,
                                     target = NULL, target_id = NULL,
                                     top_n = NULL,
                                     include_string_sim = TRUE,
                                     include_block_stats = TRUE,
                                     method = "jw") {

  pair_info  <- .pairs_from_matches(matches)
  pairs      <- pair_info$pairs
  match_type <- pair_info$match_type
  has_stage  <- !all(is.na(pairs$stage))

  columns    <- names(strategy@preparers)
  top_n_eff  <- .resolve_top_n(top_n, columns)

  if (nrow(pairs) == 0L) {
    return(Match_Features(
      features       = pairs,
      schema         = "token",
      strategy_class = "Search_Strategy",
      top_n          = top_n_eff,
      columns        = columns,
      aip_summary    = NULL
    ))
  }

  base_dt   <- data.table::as.data.table(base)
  base_dt[[id]] <- as.character(base_dt[[id]])
  target_dt <- if (!is.null(target)) {
    td <- data.table::as.data.table(target)
    if (is.null(target_id)) target_id <- id
    td[[target_id]] <- as.character(td[[target_id]])
    td
  } else NULL

  # --- tokens ---------------------------------------------------------
  s_nb <- .strip_block_by(strategy)
  base_tokens <- prepare_search_data(base_dt, id, s_nb)
  base_tokens_u <- .unique_record_tokens(base_tokens, id)

  if (match_type == "candidates") {
    if (is.null(target_dt)) {
      cli::cli_abort("{.arg target} is required for candidate matches in {.fn match_features}")
    }
    target_tokens <- prepare_search_data(target_dt, target_id, s_nb)
    target_tokens_u <- .unique_record_tokens(target_tokens, target_id)
  } else {
    target_tokens_u <- base_tokens_u  # dedup: searched & found both from base
  }

  # --- registries + aIP ----------------------------------------------
  R <- .build_registry_from_tokens(
    data.table::setnames(
      data.table::copy(base_tokens_u),
      "_rid_", "row_id_placeholder"
    )[, .(src_column, token, row_id = row_id_placeholder)]
  )
  A <- if (match_type == "candidates") {
    .build_registry_from_tokens(
      data.table::setnames(
        data.table::copy(target_tokens_u),
        "_rid_", "row_id_placeholder"
      )[, .(src_column, token, row_id = row_id_placeholder)]
    )
  } else R

  aip_dt <- compute_aip(R, A)[, .(src_column, token, aip)]

  # --- per-record × column × token with aIP attached -----------------
  searched_tok <- merge(base_tokens_u, aip_dt,
                        by = c("src_column", "token"), all.x = TRUE)
  found_tok    <- merge(target_tokens_u, aip_dt,
                        by = c("src_column", "token"), all.x = TRUE)

  # --- m / f / s sets per pair (in long form) ------------------------
  # For each pair: matched tokens, found-only, search-only
  # We build per-pair token sets, then top-N wide.

  searched_ids <- unique(pairs$searched)
  found_ids    <- unique(pairs$found)

  s_set <- searched_tok[`_rid_` %in% searched_ids]
  f_set <- found_tok[   `_rid_` %in% found_ids]

  # Index for lookup
  data.table::setkey(s_set, `_rid_`, src_column, token)
  data.table::setkey(f_set, `_rid_`, src_column, token)

  # For each pair, compute the three sets. Iterate vectorised:
  # join pair-side tokens, then anti-join.
  pair_idx <- pairs[, .(.pair = seq_len(.N), searched, found)]

  # Long-form tokens for searched side: one row per (pair, src_column, token)
  ps <- merge(pair_idx, s_set, by.x = "searched", by.y = "_rid_",
              allow.cartesian = TRUE)
  pf <- merge(pair_idx, f_set, by.x = "found", by.y = "_rid_",
              allow.cartesian = TRUE)

  # Matched: (pair, src_column, token) in both ps and pf
  m_set_long <- merge(
    ps[, .(.pair, src_column, token, aip_s = aip)],
    pf[, .(.pair, src_column, token, aip_f = aip)],
    by = c(".pair", "src_column", "token")
  )
  m_set_long[, aip := pmax(aip_s, aip_f, na.rm = TRUE)]
  m_set_long <- m_set_long[, .(.pair, src_column, token, aip)]

  # Found-only: in pf, not in ps
  fonly_long <- pf[!m_set_long, on = c(".pair", "src_column", "token")][
    , .(.pair, src_column, token, aip)
  ]
  # Search-only ("s" — search-missing): in ps, not in pf
  sonly_long <- ps[!m_set_long, on = c(".pair", "src_column", "token")][
    , .(.pair, src_column, token, aip)
  ]

  # --- assemble top-N wide tables -----------------------------------
  pair_ids <- pair_idx$.pair

  to_rid <- function(dt) {
    out <- data.table::copy(dt)
    data.table::setnames(out, ".pair", "_rid_")
    out
  }

  m_wide <- .topN_wide(to_rid(m_set_long), "m", columns, top_n_eff,
                       record_ids = pair_ids)
  f_wide <- .topN_wide(to_rid(fonly_long), "f", columns, top_n_eff,
                       record_ids = pair_ids)
  s_wide <- .topN_wide(to_rid(sonly_long), "s", columns, top_n_eff,
                       record_ids = pair_ids)

  # --- scnt / rcnt / r1..rn (search-record properties) --------------
  s_tokens_per_pair <- ps[, .(scnt = data.table::uniqueN(token)), by = .pair]

  # repeated across columns: tokens appearing in >1 src_column on search side
  rep_tok <- ps[, .(n_cols = data.table::uniqueN(src_column)),
                by = .(.pair, token)][n_cols > 1L]
  rcnt_per_pair <- rep_tok[, .(rcnt = data.table::uniqueN(token)), by = .pair]

  # r1..rn = per-column max aIP among repeated tokens for that column
  rep_with_col <- merge(
    rep_tok[, .(.pair, token)],
    ps[, .(.pair, src_column, token, aip)],
    by = c(".pair", "token"),
    allow.cartesian = TRUE
  )
  r_max <- rep_with_col[
    , .(max_aip = suppressWarnings(max(aip, na.rm = TRUE))),
    by = .(.pair, src_column)
  ]
  r_max[is.infinite(max_aip), max_aip := NA_real_]
  r_wide <- data.table::dcast(
    r_max, .pair ~ src_column,
    value.var = "max_aip", fill = NA_real_
  )

  # Rename to r1..rn in canonical strategy order
  r_cols <- paste0("r", seq_along(columns))
  for (i in seq_along(columns)) {
    col <- columns[i]
    if (col %in% names(r_wide)) {
      data.table::setnames(r_wide, col, r_cols[i])
    } else {
      r_wide[, (r_cols[i]) := NA_real_]
    }
  }
  r_wide <- r_wide[, c(".pair", r_cols), with = FALSE]

  # --- block stats (cnt/icnt/ipos) ----------------------------------
  if (include_block_stats) {
    blk <- pairs[, .(
      .pair_idx = seq_len(.N),
      searched, found, score
    )]
    blk[, cnt  := .N, by = searched]
    blk[, icnt := data.table::uniqueN(found), by = searched]
    blk[, ipos := data.table::frank(score, ties.method = "min") / .N,
        by = searched]
    block_stats <- data.table::data.table(
      .pair = blk$.pair_idx,
      cnt   = blk$cnt,
      icnt  = blk$icnt,
      ipos  = blk$ipos
    )
  } else {
    block_stats <- data.table::data.table(
      .pair = pair_ids,
      cnt   = NA_integer_,
      icnt  = NA_integer_,
      ipos  = NA_real_
    )
  }

  # --- assemble core columns ----------------------------------------
  core <- pairs[, .(
    .pair    = seq_len(.N),
    searched = as.character(searched),
    found    = as.character(found),
    match_id = match_id,
    stage    = if (has_stage) stage else NA_character_,
    score    = score
  )]
  core <- merge(core, block_stats, by = ".pair", all.x = TRUE)
  core <- merge(core,
                merge(s_tokens_per_pair, rcnt_per_pair, by = ".pair",
                      all.x = TRUE),
                by = ".pair", all.x = TRUE)
  core[is.na(scnt), scnt := 0L]
  core[is.na(rcnt), rcnt := 0L]
  core <- merge(core, r_wide,  by = ".pair", all.x = TRUE)

  # Fill NA r-cols left over from missing pairs
  for (rc in r_cols) {
    if (!rc %in% names(core)) core[, (rc) := NA_real_]
  }

  # --- stage one-hots -----------------------------------------------
  stage_levels <- if (has_stage) sort(unique(stats::na.omit(pairs$stage))) else character()
  if (length(stage_levels) > 1L) {
    for (lv in stage_levels) {
      colname <- paste0("stage_", make.names(lv))
      core[, (colname) := as.integer(stage == lv)]
    }
  }

  # --- merge wide aIP blocks ----------------------------------------
  m_wide <- data.table::setnames(m_wide, "_rid_", ".pair")
  f_wide <- data.table::setnames(f_wide, "_rid_", ".pair")
  s_wide <- data.table::setnames(s_wide, "_rid_", ".pair")

  out <- merge(core,   m_wide, by = ".pair", all.x = TRUE)
  out <- merge(out,    f_wide, by = ".pair", all.x = TRUE)
  out <- merge(out,    s_wide, by = ".pair", all.x = TRUE)

  data.table::setorder(out, .pair)
  out[, .pair := NULL]

  # --- string similarity --------------------------------------------
  if (isTRUE(include_string_sim) && length(columns) > 0L) {
    sim_dt <- .string_sim_block(
      pairs      = out[, .(searched, found)],
      base_dt    = base_dt,
      target_dt  = target_dt,
      base_id    = id,
      target_id  = target_id,
      columns    = columns,
      method     = method
    )
    sim_cols <- setdiff(names(sim_dt), c("searched", "found"))
    for (sc in sim_cols) data.table::set(out, j = sc, value = sim_dt[[sc]])
  }

  # --- canonical column order ---------------------------------------
  stage_cols <- grep("^stage_", names(out), value = TRUE)
  ordered_cols <- c(
    "searched", "found", "match_id", "stage", "score",
    "cnt", "icnt", "ipos",
    stage_cols,
    "scnt", "rcnt", r_cols,
    grep("^m_",      names(out), value = TRUE),
    grep("^f_",      names(out), value = TRUE),
    grep("^s_",      names(out), value = TRUE),
    grep("^sim_sf_", names(out), value = TRUE),
    grep("^sim_fs_", names(out), value = TRUE)
  )
  ordered_cols <- intersect(ordered_cols, names(out))
  data.table::setcolorder(out, ordered_cols)

  aip_summary <- list(
    n_tokens   = nrow(aip_dt),
    aip_median = if (nrow(aip_dt) > 0L) stats::median(aip_dt$aip, na.rm = TRUE) else NA_real_,
    aip_p05    = if (nrow(aip_dt) > 0L) unname(stats::quantile(aip_dt$aip, .05, na.rm = TRUE)) else NA_real_,
    aip_p95    = if (nrow(aip_dt) > 0L) unname(stats::quantile(aip_dt$aip, .95, na.rm = TRUE)) else NA_real_
  )

  Match_Features(
    features       = out,
    schema         = "token",
    strategy_class = "Search_Strategy",
    top_n          = top_n_eff,
    columns        = columns,
    aip_summary    = aip_summary
  )
}

# ---------- S7 methods: Search_Strategy ------------------------------

method(
  match_features,
  list(DT_tbl, Search_Strategy)
) <- function(matches, strategy, base, id,
              target = NULL, target_id = NULL,
              top_n = NULL,
              include_string_sim  = TRUE,
              include_block_stats = TRUE,
              method = "jw", ...) {

  if (missing(base) || is.null(base)) {
    cli::cli_abort("{.arg base} is required for {.fn match_features} on a {.cls Search_Strategy}")
  }
  if (missing(id) || is.null(id)) {
    cli::cli_abort("{.arg id} is required for {.fn match_features}")
  }

  .match_features_dt_token(
    matches             = matches,
    strategy            = strategy,
    base                = base,
    id                  = id,
    target              = target,
    target_id           = target_id,
    top_n               = top_n,
    include_string_sim  = include_string_sim,
    include_block_stats = include_block_stats,
    method              = method
  )
}

method(
  match_features,
  list(Duck_tbl, Search_Strategy)
) <- function(matches, strategy, base, id,
              target = NULL, target_id = NULL,
              top_n = NULL,
              include_string_sim  = TRUE,
              include_block_stats = TRUE,
              method = "jw", ...) {

  matches_dt <- data.table::as.data.table(dplyr::collect(matches))

  base_dt <- if (inherits(base, "tbl_duckdb_connection")) {
    data.table::as.data.table(dplyr::collect(base))
  } else {
    data.table::as.data.table(base)
  }

  target_dt <- if (!is.null(target)) {
    if (inherits(target, "tbl_duckdb_connection")) {
      data.table::as.data.table(dplyr::collect(target))
    } else {
      data.table::as.data.table(target)
    }
  } else NULL

  match_features(
    matches_dt, strategy,
    base                = base_dt,
    id                  = id,
    target              = target_dt,
    target_id           = target_id,
    top_n               = top_n,
    include_string_sim  = include_string_sim,
    include_block_stats = include_block_stats,
    method              = method
  )
}

# ---------- tibble / data.frame thin wrappers (Search_Strategy) ------

method(
  match_features,
  list(.jyDF, Search_Strategy)
) <- function(matches, strategy, base, id,
              target = NULL, target_id = NULL, ...) {
  match_features(
    as_DT(matches), strategy,
    base   = if (!is.null(base))   as_DT(base)   else NULL,
    id     = id,
    target = if (!is.null(target)) as_DT(target) else NULL,
    target_id = target_id,
    ...
  )
}

method(
  match_features,
  list(.jyTBL_DF, Search_Strategy)
) <- function(matches, strategy, base, id,
              target = NULL, target_id = NULL, ...) {
  match_features(
    as_DT(matches), strategy,
    base   = if (!is.null(base))   as_DT(base)   else NULL,
    id     = id,
    target = if (!is.null(target)) as_DT(target) else NULL,
    target_id = target_id,
    ...
  )
}

method(
  match_features,
  list(.jyTBL, Search_Strategy)
) <- function(matches, strategy, base, id,
              target = NULL, target_id = NULL, ...) {
  match_features(
    as_DT(matches), strategy,
    base   = if (!is.null(base))   as_DT(base)   else NULL,
    id     = id,
    target = if (!is.null(target)) as_DT(target) else NULL,
    target_id = target_id,
    ...
  )
}
