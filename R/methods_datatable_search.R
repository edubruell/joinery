# ============================================================
# data.table backend — cross-table candidate search
# ============================================================
#
# `search_candidates()` method for in-memory data.table inputs.
#
# ============================================================


# Method: search_candidates
#------------------------------------------------------------------------------
method(
  search_candidates,
  list(DT_tbl, DT_tbl, class_character, class_character, Search_Strategy)
) <- function(base_table,
              target_table,
              base_id,
              target_id,
              strategy,
              weights = NULL) {

  # --- 0. Copy inputs -------------------------------------------------------
  base_dt   <- data.table::copy(base_table)
  target_dt <- data.table::copy(target_table)
  base_dt[[base_id]]     <- as.character(base_dt[[base_id]])
  target_dt[[target_id]] <- as.character(target_dt[[target_id]])

  # --- 1. Prepare token tables ----------------------------------------------
  base_tokens <- prepare_search_data(base_dt, base_id, strategy)
  base_tokens[, side := "base"]

  target_tokens <- prepare_search_data(target_dt, target_id, strategy)
  target_tokens[, side := "target"]

  # unified id per side
  base_tokens[, uid := base_tokens[[base_id]]]
  target_tokens[, uid := target_tokens[[target_id]]]

  # --- 2. Compute rarity -----------------------------------------------------
  all_tokens <- rbindlist(list(base_tokens, target_tokens), use.names = TRUE, fill = TRUE)
  all_tokens <- compute_rarity(all_tokens, strategy)
  if (strategy@min_rarity > 0) {
    all_tokens <- all_tokens[rarity >= strategy@min_rarity]
  }

  # split back
  base_tokens   <- all_tokens[side == "base"]
  target_tokens <- all_tokens[side == "target"]

  # --- 3. Determine column weights ------------------------------------------
  if (is.null(weights)) {
    if (length(strategy@weights) > 0) {
      weights <- strategy@weights
    } else {
      cols <- names(strategy@preparers)
      weights <- rep(1 / length(cols), length(cols))
      names(weights) <- cols
    }
  }

  missing_w <- setdiff(unique(all_tokens$src_column), names(weights))
  if (length(missing_w) > 0) {
    cli::cli_abort("Weights missing for columns: {.field {missing_w}}")
  }

  # --- 4. Compute pairwise similarity ---------------------------------------
  # All rIP, smoothing, joins, and weighting are delegated to the helper
  scored <- .score_token_pairs(
    lhs_tokens = base_tokens,
    rhs_tokens = target_tokens,
    id_lhs     = "uid",
    id_rhs     = "uid",
    strategy   = strategy,
    weights    = weights
  )

  # Apply threshold
  thr <- strategy@threshold
  if (is.null(thr)) cli::cli_abort("Strategy must define a threshold")
  scored <- scored[score >= thr]

  # Apply containment limit
  if (is.finite(strategy@max_candidates)) {
    scored <- scored[
      order(-score),
      head(.SD, strategy@max_candidates),
      by = lhs_id
    ]
  }

  # No matches
  if (nrow(scored) == 0) {
    return(data.table(
      match_id = integer(),
      score    = numeric(),
      source   = character(),
      id       = character(),
      rank     = integer()
    ))
  }

  # --- 5. Assign match IDs ---------------------------------------------------
  scored[, match_id := .I]

  # --- 6. Expand to long form ------------------------------------------------
  long <- rbindlist(list(
    scored[, .(match_id, score, source = "base",   id = lhs_id)],
    scored[, .(match_id, score, source = "target", id = rhs_id)]
  ))

  # --- 7. Attach original data ------------------------------------------------
  base_dt2   <- data.table::copy(base_dt)
  target_dt2 <- data.table::copy(target_dt)

  base_dt2[, id := base_dt2[[base_id]]]
  target_dt2[, id := target_dt2[[target_id]]]

  base_long <- merge(
    long[source == "base"],
    base_dt2,
    by = "id",
    all.x = TRUE,
    sort = FALSE
  )

  target_long <- merge(
    long[source == "target"],
    target_dt2,
    by = "id",
    all.x = TRUE,
    sort = FALSE
  )

  out <- rbindlist(list(base_long, target_long), use.names = TRUE, fill = TRUE)

  # --- 8. Rank within match --------------------------------------------------
  out[, rank := rank(-score, ties.method = "first"), by = match_id]
  data.table::setorder(out, match_id, source, rank)

  out[]
}
