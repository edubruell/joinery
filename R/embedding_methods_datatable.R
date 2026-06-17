# Method: compute_embeddings for data.table and Embedding_Strategy
#--------------------------------------------------------------------------
method(
  compute_embeddings,
  list(DT_tbl, class_character, Embedding_Strategy)
) <- function(data, id, strategy) {

  if (!requireNamespace("tidyllm", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.fn compute_embeddings} requires the {.pkg tidyllm} package",
      "i" = "Install it via {.run install.packages(\"tidyllm\")}"
    ))
  }

  dt <- data.table::copy(data)

  if (!id %in% names(dt)) {
    cli::cli_abort("ID column {.field {id}} not found in data")
  }

  # Validate blocking columns if present
  block_by <- strategy@block_by
  if (!is.null(block_by)) {
    missing_cols <- setdiff(block_by, names(dt))
    if (length(missing_cols) > 0) {
      cli::cli_abort("Blocking columns not found in data: {.field {missing_cols}}")
    }
  }

  assembled <- assemble_record_text(
    data = dt,
    id = id,
    columns = strategy@columns,
    sep = strategy@collapse_sep
  )

  n_records <- nrow(assembled)
  pnum      <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)

  # Reuse: look up raw (pre-normalization) vectors keyed by (model, content-hash).
  model_key <- .embedding_model_key(strategy@embedding_model)
  keys      <- .embedding_keys(assembled$text, model_key)
  cached    <- .embedding_cache_get(keys)

  miss <- map_lgl(cached, is.null)
  n_new    <- sum(miss)
  n_reused <- n_records - n_new

  # Embed only the cache misses, in batches; store the raw vectors.
  if (n_new > 0L) {
    miss_idx  <- which(miss)
    miss_text <- assembled$text[miss_idx]

    batch_size    <- strategy@batch_size
    batch_starts  <- seq.int(1L, n_new, by = batch_size)
    batch_ends    <- pmin(batch_starts + batch_size - 1L, n_new)
    total_batches <- length(batch_starts)

    cli::cli_alert_info(
      "Computing Embeddings for {pnum(n_new)} records in {pnum(total_batches)} batches ({pnum(n_reused)} reused):"
    )

    miss_vecs <- vector("list", n_new)
    for (b in seq_along(batch_starts)) {
      start <- batch_starts[[b]]
      end   <- batch_ends[[b]]

      embeddings_tbl <- tidyllm::embed(
        miss_text[start:end],
        strategy@embedding_model
      )

      cli::cli_alert_info("Embedding Batch {pnum(b)}/{pnum(total_batches)}")
      miss_vecs[start:end] <- embeddings_tbl$embeddings
    }

    .embedding_cache_put(keys[miss_idx], miss_vecs)
    cached[miss_idx] <- miss_vecs
  } else {
    cli::cli_alert_info(
      "Computing Embeddings for {pnum(n_records)} records ({pnum(n_reused)} reused, 0 new)."
    )
  }

  result <- data.table::data.table(
    id        = assembled$id,
    embedding = cached
  )

  # Normalize on read: the cache holds raw vectors, so normalize = TRUE/FALSE
  # share one cache and the returned values are unchanged from before.
  if (strategy@normalize) {
    result[, embedding := map(embedding, function(vec) {
      norm <- sqrt(sum(vec^2))
      if (norm > 0) vec / norm else vec
    })]
  }

  # Add blocking columns if specified
  if (!is.null(block_by)) {
    block_dt <- dt[, c(id, block_by), with = FALSE]
    data.table::setnames(block_dt, id, "id")

    result <- merge(result, block_dt, by = "id", all.x = TRUE)
  }

  data.table::setnames(result, "id", id)
  result[]
}


# Method: score_embeddings for data.table and Embedding_Strategy
#--------------------------------------------------------------------------
method(
  score_embeddings,
  list(DT_tbl, DT_tbl, Embedding_Strategy)
) <- function(base_embeddings, target_embeddings, strategy) {

  base_dt <- data.table::as.data.table(base_embeddings)
  target_dt <- data.table::as.data.table(target_embeddings)

  # Validate columns
  if (!all(c("id", "embedding") %in% names(base_dt))) {
    cli::cli_abort("base_embeddings must have columns: {.field id}, {.field embedding}")
  }
  if (!all(c("id", "embedding") %in% names(target_dt))) {
    cli::cli_abort("target_embeddings must have columns: {.field id}, {.field embedding}")
  }

  block_by <- strategy@block_by

  # Perform join (blocked or cartesian)
  if (!is.null(block_by) && length(block_by) > 0) {
    # Validate blocking columns are present
    missing_base <- setdiff(block_by, names(base_dt))
    missing_target <- setdiff(block_by, names(target_dt))

    if (length(missing_base) > 0) {
      cli::cli_abort("Blocking columns missing from base_embeddings: {.field {missing_base}}")
    }
    if (length(missing_target) > 0) {
      cli::cli_abort("Blocking columns missing from target_embeddings: {.field {missing_target}}")
    }

    # Blocked join: only compare records with matching block values
    pairs <- base_dt[target_dt, on = block_by, allow.cartesian = TRUE, nomatch = NULL]
  } else {
    # Cartesian join: compare all pairs
    base_dt[, .join_key := 1L]
    target_dt[, .join_key := 1L]
    pairs <- base_dt[target_dt, on = ".join_key", allow.cartesian = TRUE]
  }

  # Compute cosine similarity
  pairs[, score := mapply(function(b, t) {
    sum(b * t)  # Dot product (vectors already normalized if strategy@normalize)
  }, embedding, i.embedding, SIMPLIFY = TRUE)]

  # Clean up and return
  result <- pairs[, .(base_id = id, target_id = i.id, score)]
  result[]
}


# Method: search_candidates for data.table and Embedding_Strategy
#--------------------------------------------------------------------------
method(
  search_candidates,
  list(DT_tbl, DT_tbl, class_character, class_character, Embedding_Strategy)
) <- function(base_table,
              target_table,
              base_id,
              target_id,
              strategy,
              threshold = NULL,
              weights = NULL) {

  # Validate no unsupported arguments
  if (!is.null(weights)) {
    cli::cli_abort("Embedding strategies do not support weights")
  }

  thr <- threshold %||% strategy@threshold

  # Compute embeddings for both tables
  base_emb <- compute_embeddings(base_table, base_id, strategy)
  target_emb <- compute_embeddings(target_table, target_id, strategy)

  # Score all pairs
  scores <- score_embeddings(base_emb, target_emb, strategy)

  # Filter by threshold
  scores <- scores[score >= thr]

  if (nrow(scores) == 0) {
    # Return empty result with proper schema
    return(data.table::data.table(
      match_id = integer(),
      score = numeric(),
      source = character(),
      id = character(),
      rank = integer()
    ))
  }

  # Assign match IDs (connected components via base_id)
  data.table::setorder(scores, base_id, -score)
  scores[, match_id := .GRP, by = base_id]

  # Build output: one row per record per match
  base_dt <- data.table::as.data.table(base_table)
  target_dt <- data.table::as.data.table(target_table)

  base_matches <- scores[, .(match_id, score, id = base_id)]
  base_matches[, source := "base"]
  base_matches <- base_matches[base_dt, on = c(id = base_id), nomatch = NULL]
  base_matches[, rank := frank(-score, ties.method = "min"), by = match_id]

  target_matches <- scores[, .(match_id, score, id = target_id)]
  target_matches[, source := "target"]
  target_matches <- target_matches[target_dt, on = c(id = target_id), nomatch = NULL]
  target_matches[, rank := frank(-score, ties.method = "min"), by = match_id]

  result <- data.table::rbindlist(
    list(base_matches, target_matches),
    use.names = TRUE,
    fill = TRUE
  )

  data.table::setorder(result, match_id, source, rank)
  result[]
}


# Method: detect_duplicates for data.table and Embedding_Strategy
#--------------------------------------------------------------------------
method(
  detect_duplicates,
  list(DT_tbl, class_character, Embedding_Strategy)
) <- function(base_table, id, strategy, threshold = NULL) {
  thr <- threshold %||% strategy@threshold

  # Compute embeddings
  embeddings <- compute_embeddings(base_table, id, strategy)

  # Self-join to find similar pairs
  emb_dt <- data.table::as.data.table(embeddings)

  block_by <- strategy@block_by

  # Perform self-join (blocked or cartesian)
  if (!is.null(block_by) && length(block_by) > 0) {
    # Blocked self-join: only compare within same blocks
    pairs <- emb_dt[emb_dt, on = block_by, allow.cartesian = TRUE, nomatch = NULL]
  } else {
    # Cartesian self-join: compare all pairs
    emb_dt[, .join_key := 1L]
    pairs <- emb_dt[emb_dt, on = ".join_key", allow.cartesian = TRUE]
  }

  # Remove self-pairs and compute scores
  pairs <- pairs[id < i.id]  # Keep each pair once

  pairs[, score := mapply(function(b, t) {
    sum(b * t)
  }, embedding, i.embedding, SIMPLIFY = TRUE)]

  # Filter by threshold
  pairs <- pairs[score >= thr]

  if (nrow(pairs) == 0) {
    # Return empty result
    return(data.table::data.table(
      duplicate_group = integer(),
      score = numeric(),
      id = character(),
      rank = integer()
    ))
  }

  # Build edge list for connected components
  edges <- pairs[, .(id1 = id, id2 = i.id, score)]

  # Find connected components using igraph
  if (!requireNamespace("igraph", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.fn detect_duplicates} requires the {.pkg igraph} package",
      "i" = "Install it via {.run install.packages(\"igraph\")}"
    ))
  }

  g <- igraph::graph_from_data_frame(
    edges[, .(id1, id2)],
    directed = FALSE
  )

  components <- igraph::components(g)
  membership_dt <- data.table::data.table(
    id = names(components$membership),
    duplicate_group = as.integer(components$membership)
  )

  # Join scores back
  all_ids <- unique(c(edges$id1, edges$id2))
  result_dt <- membership_dt[data.table::as.data.table(base_table),
                             on = "id", nomatch = NULL]

  # Add scores and ranks
  id_scores <- rbind(
    edges[, .(id = id1, score)],
    edges[, .(id = id2, score)]
  )
  id_scores <- id_scores[, .(score = max(score)), by = id]

  result_dt <- id_scores[result_dt, on = "id"]
  result_dt[, rank := frank(-score, ties.method = "min"), by = duplicate_group]

  data.table::setorder(result_dt, duplicate_group, rank)
  result_dt[]
}
