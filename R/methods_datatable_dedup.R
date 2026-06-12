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
) <- function(base_table, id, strategy, weights = NULL, max_comparisons = Inf) {

  dt <- data.table::copy(base_table)
  dt[[id]] <- as.character(dt[[id]])

  # --- 0. Opt-in comparison-budget ceiling (D1) ---------------------------
  # Estimate Sum_b n_b*(n_b-1)/2 from per-block distinct-id counts on the raw
  # input (cheap, pre-tokenisation) and abort before the doomed overlap join.
  if (is.finite(max_comparisons)) {
    block_by <- strategy@block_by %||% character()
    if (length(block_by)) {
      # distinct (id, block) then count rows per block = records per block.
      # Avoids get(id) inside j, which would resolve to the `id` *column*.
      id_blocks <- unique(dt[, c(id, block_by), with = FALSE])
      bc <- id_blocks[, .(n = .N), by = block_by]
      data.table::setorder(bc, -n)
      est  <- .estimate_self_comparisons(bc$n)
      topn <- utils::head(bc, 3L)
      key  <- do.call(paste, c(topn[, block_by, with = FALSE], sep = ", "))
      top_blocks <- sprintf("%s: %d records", key, topn$n)
    } else {
      top_blocks <- NULL
      est <- .estimate_self_comparisons(data.table::uniqueN(dt[[id]]))
    }
    .enforce_comparison_budget(est, max_comparisons, top_blocks)
  }

  # --- 1. Prepare token table ---------------------------------------------
  tokens <- prepare_search_data(
    data     = dt,
    id       = id,
    strategy = strategy
  )

  # --- 2. Compute rarity ---------------------------------------------------
  # Thin the token table BEFORE the overlap join (inside .score_token_pairs):
  # the rarity floor and document-frequency cap are the cheap pre-join levers.
  tokens <- compute_rarity(tokens, strategy)
  tokens <- .rarity_prefilter_dt(tokens, strategy)

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

  # --- 5. Resolve entities from scored edges --------------------------------
  # Delegate connected components + best-score + rank to the shared kernel.
  # `vertices = all ids` reproduces the singleton-aware component labelling
  # this method has always used; singletons (non-duplicates) carry NA score
  # and are dropped below so only duplicate records are returned.
  ent <- resolve_entities(
    edges    = scored[, .(from = lhs_id, to = rhs_id, score = score)],
    id_a     = "from",
    id_b     = "to",
    score    = "score",
    vertices = unique(tokens[[id]])
  )

  ent <- ent[!is.na(score)]
  data.table::setnames(ent, "entity", "duplicate_group")
  result <- ent[, .(id, duplicate_group, score, rank)]
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
