# ============================================================
# explain_match() — all backends (Phase 0.6 M4)
# ============================================================
#
# Attribution diagnostic (Q3). Reconstructs per-column and per-token
# contributions to a single match score.
#
# Two calling forms, dispatched on the second positional argument:
#
#   Ergonomic:   explain_match(matches, strategy, base, id,
#                              target = NULL, target_id = NULL, match_id = 1L)
#                Runs the full tokenisation + rarity pipeline on base/target
#                to obtain corpus-level rarity, then filters to the pair.
#
#   Power-user:  explain_match(matches, tokens_dt, id, strategy, match_id = 1L)
#                Accepts a pre-computed tokens+rarity table (same schema as
#                compute_rarity() output).  Fast when the user has already
#                materialised the token table.  Produces identical results to
#                the ergonomic form if the same corpus was used.
#
# Backends: data.table (reference), DuckDB (collect to R → delegate),
#   tibble/data.frame thin wrappers.
# ============================================================


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Extract the top-2 record IDs for a match group / pair.
#'
#' Returns list(lhs_id, rhs_id, match_type).
#' For duplicates: rank-1 and rank-2 records in the group.
#' For candidates: base-side and target-side records.
#'
#' @noRd
.get_pair_ids <- function(matches, match_id) {
  mt <- .detect_match_type(matches)

  if (mt == "duplicates") {
    # Use which() to avoid data.table column scope collision (variable `match_id`
    # shares name with column `match_id`/`duplicate_group`).
    idx <- which(matches[["duplicate_group"]] == match_id)
    grp <- matches[idx, ]
    if (nrow(grp) < 1L) {
      stop(sprintf("match_id %d not found in `matches` (duplicate_group column).",
                   match_id), call. = FALSE)
    }
    grp_ord <- grp[order(grp[["rank"]]), ]
    lhs_id  <- as.character(grp_ord[["id"]][1L])
    rhs_id  <- if (nrow(grp_ord) >= 2L) as.character(grp_ord[["id"]][2L]) else lhs_id

  } else { # candidates
    idx <- which(matches[["match_id"]] == match_id)
    grp <- matches[idx, ]
    if (nrow(grp) == 0L) {
      stop(sprintf("match_id %d not found in `matches`.", match_id), call. = FALSE)
    }
    lhs_id <- as.character(grp[["id"]][grp[["source"]] == "base"][1L])
    rhs_id <- as.character(grp[["id"]][grp[["source"]] == "target"][1L])
  }

  list(lhs_id = lhs_id, rhs_id = rhs_id, match_type = mt)
}


#' Build the pair data.table from a matches table for a given match_id.
#'
#' For candidates, keeps `source` so the caller can tell which record is base
#' vs target.  For duplicates, strips infrastructure columns (duplicate_group,
#' score, rank).
#'
#' @noRd
.pair_dt_from_matches <- function(matches, match_id, pair_info) {
  mt <- pair_info$match_type

  if (mt == "duplicates") {
    infra <- c("duplicate_group", "score", "rank")
    # Use which() to avoid data.table column scope collision
    idx  <- which(matches[["duplicate_group"]] == match_id &
                    matches[["rank"]] %in% c(1L, 2L))
    rows <- matches[idx, ]
  } else {
    # Keep "source" for candidates — informative (base vs target)
    infra <- c("match_id", "score", "rank")
    idx   <- which(matches[["match_id"]] == match_id)
    rows  <- matches[idx, ]
  }

  keep <- setdiff(names(rows), infra)
  data.table::as.data.table(rows)[, keep, with = FALSE]
}


#' Resolve column weights from a Search_Strategy.
#' @noRd
.resolve_weights_explain <- function(strategy) {
  if (length(strategy@weights) > 0) {
    return(strategy@weights)
  }
  cols <- names(strategy@preparers)
  w    <- rep(1 / length(cols), length(cols))
  names(w) <- cols
  w
}


# ---------------------------------------------------------------------------
# Ergonomic form: DT matches + Search_Strategy
# ---------------------------------------------------------------------------

method(
  explain_match,
  list(DT_tbl, Search_Strategy)
) <- function(matches, x, base, id, target = NULL, target_id = NULL,
              match_id = 1L, ...) {

  if (missing(base) || is.null(base)) {
    stop("`base` is required for the ergonomic form of explain_match().",
         call. = FALSE)
  }
  if (missing(id) || is.null(id)) {
    stop("`id` (name of the ID column in `base`) is required.", call. = FALSE)
  }

  base_dt   <- data.table::as.data.table(base)
  target_dt <- if (!is.null(target)) data.table::as.data.table(target) else NULL
  if (is.null(target_id)) target_id <- id

  match_id  <- as.integer(match_id)
  pair_info <- .get_pair_ids(matches, match_id)
  lhs_id    <- pair_info$lhs_id
  rhs_id    <- pair_info$rhs_id
  mt        <- pair_info$match_type

  # ---- Reconstruct tokens + corpus-level rarity ----------------------------
  if (mt == "duplicates") {
    tokens_full   <- prepare_search_data(base_dt, id, x)
    tokens_rarity <- compute_rarity(tokens_full, x)

    # Pre-evaluate id column outside `[` to avoid data.table column scope collision
    .id_vals   <- tokens_rarity[[id]]
    lhs_tokens <- tokens_rarity[.id_vals == lhs_id, ]
    rhs_tokens <- tokens_rarity[.id_vals == rhs_id, ]
    id_lhs <- id
    id_rhs <- id

  } else { # candidates
    if (is.null(target_dt)) {
      stop("`target` is required for explaining candidate matches.", call. = FALSE)
    }
    base_tokens   <- prepare_search_data(base_dt,   id,        x)
    base_tokens[, side := "base"]
    target_tokens <- prepare_search_data(target_dt, target_id, x)
    target_tokens[, side := "target"]

    # unified id column (search_candidates pattern); pre-evaluate to avoid scope collision
    .base_uid   <- base_tokens[[id]]
    .target_uid <- target_tokens[[target_id]]
    base_tokens[,   uid := .base_uid]
    target_tokens[, uid := .target_uid]

    all_tokens <- data.table::rbindlist(
      list(base_tokens, target_tokens), use.names = TRUE, fill = TRUE
    )
    all_rarity <- compute_rarity(all_tokens, x)

    # Pre-evaluate to avoid data.table column scope collision
    .side_vals <- all_rarity$side
    .uid_vals  <- all_rarity$uid
    lhs_tokens <- all_rarity[.side_vals == "base"   & .uid_vals == lhs_id, ]
    rhs_tokens <- all_rarity[.side_vals == "target" & .uid_vals == rhs_id, ]
    id_lhs <- "uid"
    id_rhs <- "uid"
  }

  if (nrow(lhs_tokens) == 0L || nrow(rhs_tokens) == 0L) {
    stop(sprintf(
      "No tokens found for pair (%s, %s). Check that `base`/`target` contain these IDs.",
      lhs_id, rhs_id
    ), call. = FALSE)
  }

  # ---- Attribution ---------------------------------------------------------
  weights     <- .resolve_weights_explain(x)
  attr_result <- .pair_attribution_dt(lhs_tokens, rhs_tokens, id_lhs, id_rhs, x, weights)

  # ---- Build pair table ----------------------------------------------------
  pair_dt <- .pair_dt_from_matches(data.table::as.data.table(matches), match_id, pair_info)

  Match_Explanation(
    match_id           = match_id,
    pair               = pair_dt,
    per_column_contrib = attr_result$per_column_contrib,
    shared_tokens      = attr_result$shared_tokens,
    score              = attr_result$score,
    score_breakdown    = attr_result$score_breakdown
  )
}


# ---------------------------------------------------------------------------
# Power-user form: DT matches + DT tokens-with-rarity
# ---------------------------------------------------------------------------

method(
  explain_match,
  list(DT_tbl, DT_tbl)
) <- function(matches, x, id, strategy, match_id = 1L, ...) {

  if (missing(id) || is.null(id)) {
    stop("`id` (name of the ID column in the tokens table `x`) is required.",
         call. = FALSE)
  }
  if (missing(strategy) || is.null(strategy)) {
    stop("`strategy` is required for the power-user form of explain_match().",
         call. = FALSE)
  }
  if (!id %in% names(x)) {
    stop(sprintf("ID column '%s' not found in the tokens table `x`.", id),
         call. = FALSE)
  }

  match_id  <- as.integer(match_id)
  pair_info <- .get_pair_ids(data.table::as.data.table(matches), match_id)
  lhs_id    <- pair_info$lhs_id
  rhs_id    <- pair_info$rhs_id

  # Pre-evaluate to avoid data.table column scope collision
  .id_vals   <- x[[id]]
  lhs_tokens <- x[.id_vals == lhs_id, ]
  rhs_tokens <- x[.id_vals == rhs_id, ]

  if (nrow(lhs_tokens) == 0L || nrow(rhs_tokens) == 0L) {
    stop(sprintf(
      "No tokens found for pair (%s, %s) in the provided tokens table.",
      lhs_id, rhs_id
    ), call. = FALSE)
  }

  weights     <- .resolve_weights_explain(strategy)
  attr_result <- .pair_attribution_dt(lhs_tokens, rhs_tokens, id, id, strategy, weights)
  pair_dt     <- .pair_dt_from_matches(data.table::as.data.table(matches), match_id, pair_info)

  Match_Explanation(
    match_id           = match_id,
    pair               = pair_dt,
    per_column_contrib = attr_result$per_column_contrib,
    shared_tokens      = attr_result$shared_tokens,
    score              = attr_result$score,
    score_breakdown    = attr_result$score_breakdown
  )
}


# ---------------------------------------------------------------------------
# DuckDB ergonomic form: Duck_tbl matches + Search_Strategy
# ---------------------------------------------------------------------------

method(
  explain_match,
  list(Duck_tbl, Search_Strategy)
) <- function(matches, x, base, id, target = NULL, target_id = NULL,
              match_id = 1L, ...) {

  # Collect the matches table to R, then delegate to the DT method.
  # Rationale: explain_match is an interactive diagnostic called on one pair;
  # the cost of collecting the full matches table is acceptable.
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
  } else {
    NULL
  }

  explain_match(
    matches_dt, x,
    base      = base_dt,
    id        = id,
    target    = target_dt,
    target_id = target_id,
    match_id  = match_id,
    ...
  )
}


# ---------------------------------------------------------------------------
# Thin wrappers: data.frame / tibble matches + Search_Strategy
# ---------------------------------------------------------------------------

method(
  explain_match,
  list(.jyDF, Search_Strategy)
) <- function(matches, x, base, id, target = NULL, target_id = NULL,
              match_id = 1L, ...) {
  explain_match(
    as_DT(matches), x,
    base = if (!is.null(base)) as_DT(base) else NULL,
    id = id,
    target = if (!is.null(target)) as_DT(target) else NULL,
    target_id = target_id,
    match_id = match_id,
    ...
  )
}

method(
  explain_match,
  list(.jyTBL_DF, Search_Strategy)
) <- function(matches, x, base, id, target = NULL, target_id = NULL,
              match_id = 1L, ...) {
  explain_match(
    as_DT(matches), x,
    base = if (!is.null(base)) as_DT(base) else NULL,
    id = id,
    target = if (!is.null(target)) as_DT(target) else NULL,
    target_id = target_id,
    match_id = match_id,
    ...
  )
}

method(
  explain_match,
  list(.jyTBL, Search_Strategy)
) <- function(matches, x, base, id, target = NULL, target_id = NULL,
              match_id = 1L, ...) {
  explain_match(
    as_DT(matches), x,
    base = if (!is.null(base)) as_DT(base) else NULL,
    id = id,
    target = if (!is.null(target)) as_DT(target) else NULL,
    target_id = target_id,
    match_id = match_id,
    ...
  )
}

method(
  explain_match,
  list(.jyDF, DT_tbl)
) <- function(matches, x, id, strategy, match_id = 1L, ...) {
  explain_match(as_DT(matches), x, id = id, strategy = strategy,
                match_id = match_id, ...)
}

method(
  explain_match,
  list(.jyTBL_DF, DT_tbl)
) <- function(matches, x, id, strategy, match_id = 1L, ...) {
  explain_match(as_DT(matches), x, id = id, strategy = strategy,
                match_id = match_id, ...)
}

method(
  explain_match,
  list(.jyTBL, DT_tbl)
) <- function(matches, x, id, strategy, match_id = 1L, ...) {
  explain_match(as_DT(matches), x, id = id, strategy = strategy,
                match_id = match_id, ...)
}


# ============================================================
# Embedding_Strategy form (Phase 0.6 M8)
# ============================================================
#
# Returns pair + score only. Per-token attribution is not available for
# embedding matches; `per_column_contrib` and `shared_tokens` are NULL
# and `score_breakdown$method` is `"cosine_similarity"`. The
# `Match_Explanation` print method surfaces this explicitly.
# ============================================================

method(
  explain_match,
  list(DT_tbl, Embedding_Strategy)
) <- function(matches, x, match_id = 1L, ...) {

  matches_dt <- data.table::as.data.table(matches)
  match_id   <- as.integer(match_id)

  pair_info <- .get_pair_ids(matches_dt, match_id)
  pair_dt   <- .pair_dt_from_matches(matches_dt, match_id, pair_info)

  mt <- pair_info$match_type
  if (mt == "duplicates") {
    idx <- which(matches_dt[["duplicate_group"]] == match_id)
  } else {
    idx <- which(matches_dt[["match_id"]] == match_id)
  }
  score_val <- as.numeric(matches_dt[["score"]][idx][1L])

  Match_Explanation(
    match_id           = match_id,
    pair               = pair_dt,
    per_column_contrib = NULL,
    shared_tokens      = NULL,
    score              = score_val,
    score_breakdown    = list(
      method = "cosine_similarity",
      note   = "per-token attribution is not available for embedding strategies"
    )
  )
}

method(
  explain_match,
  list(Duck_tbl, Embedding_Strategy)
) <- function(matches, x, match_id = 1L, ...) {
  matches_dt <- data.table::as.data.table(dplyr::collect(matches))
  explain_match(matches_dt, x, match_id = match_id, ...)
}

method(
  explain_match,
  list(.jyDF, Embedding_Strategy)
) <- function(matches, x, match_id = 1L, ...) {
  explain_match(as_DT(matches), x, match_id = match_id, ...)
}

method(
  explain_match,
  list(.jyTBL_DF, Embedding_Strategy)
) <- function(matches, x, match_id = 1L, ...) {
  explain_match(as_DT(matches), x, match_id = match_id, ...)
}

method(
  explain_match,
  list(.jyTBL, Embedding_Strategy)
) <- function(matches, x, match_id = 1L, ...) {
  explain_match(as_DT(matches), x, match_id = match_id, ...)
}
