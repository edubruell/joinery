# ============================================================
# data.table backend — duplicate detection & removal
# ============================================================
#
# `detect_duplicates()` and `deduplicate_table()` methods for
# in-memory data.table inputs.
#
# ============================================================


# Method: detect_duplicates
#------------------------------------------------------------------------------
method(
  detect_duplicates,
  list(DT_tbl, class_character ,Search_Strategy)
) <- function(base_table, id, strategy, weights = NULL) {

  dt <- data.table::copy(base_table)
  dt[[id]] <- as.character(dt[[id]])

  # --- 1. Prepare token table ---------------------------------------------
  tokens <- prepare_search_data(
    data     = dt,
    id       = id,
    strategy = strategy
  )

  # --- 2. Compute rarity ---------------------------------------------------
  tokens <- compute_rarity(tokens, strategy)
  if (strategy@min_rarity > 0) {
    tokens <- tokens[rarity >= strategy@min_rarity]
  }

  # --- 3. Determine weights -----------------------------------------------
  if (is.null(weights)) {
    if (length(strategy@weights) > 0) {
      weights <- strategy@weights
    } else {
      cols <- names(strategy@preparers)
      weights <- rep(1 / length(cols), length(cols))
      names(weights) <- cols
    }
  }

  # Guarantee weight coverage
  missing_w <- setdiff(unique(tokens$src_column), names(weights))
  if (length(missing_w) > 0) {
    cli::cli_abort("Weights missing for columns: {.field {missing_w}}")
  }

  # --- 4. Compute pairwise scores through helper ----------------------------
  scored <- .score_token_pairs(
    lhs_tokens = tokens,
    rhs_tokens = tokens,
    id_lhs     = id,
    id_rhs     = id,
    strategy   = strategy,
    weights    = weights
  )

  # Remove self matches
  scored <- scored[lhs_id != rhs_id]

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

  if (nrow(scored) == 0L) {
    return(data.table(
      duplicate_group = integer(),
      id              = character(),
      score           = numeric(),
      rank            = integer()
    ))
  }

  # --- 5. Connected components ----------------------------------------------
  edges <- scored[, .(
    from = lhs_id,
    to   = rhs_id
  )]

  edges <- rbind(edges, edges[, .(from = to, to = from)])

  all_ids <- unique(tokens[[id]])

  g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = all_ids)
  comp <- igraph::components(g)

  membership_dt <- data.table(
    id              = names(comp$membership),
    duplicate_group = unname(comp$membership)
  )

  # --- 6. Scores and ranks --------------------------------------------------
  scored_long <- rbindlist(list(
    scored[, .(id = lhs_id, score)],
    scored[, .(id = rhs_id, score)]
  ))

  best <- scored_long[
    , .(score = max(score, na.rm = TRUE)),
    by = id
  ]

  result <- membership_dt[best, on = "id"]

  result[, rank := rank(-score, ties.method = "first"), by = duplicate_group]
  setkeyv(result, c("duplicate_group", "rank"))

  # --- 7. Attach original data ----------------------------------------------
  result <- merge(
    result,
    dt,
    by.x = "id",
    by.y = id,
    all.x = TRUE,
    sort = FALSE
  )

  result[]
}


# Method: deduplicate_table
#------------------------------------------------------------------------------
method(
  deduplicate_table,
  list(DT_tbl, DT_tbl, class_character)
) <- function(base_table, duplicates, id) {
  dt <- data.table::copy(base_table)

  if (!id %in% names(dt)) {
    cli::cli_abort("ID {.field {id}} not found in base_table")
  }
  duplicate_ids <- duplicates[rank!=1L,]$id
  to_remove <- as.character(dt[[id]]) %in% duplicate_ids

  dt[!to_remove,][]
}
