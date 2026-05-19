# ============================================================
# data.table methods for joinery
# ============================================================
#
# This file implements the full data.table backend for joinery’s S7-based
# record-linkage engine. 
#
# These methods interpret the backend-agnostic Search_Strategy IR and execute it
# using data.table operations. The core workflow follows the
# joinery pipeline:
#
#       1. Column-wise preprocessing via S7 Step pipelines
#       2. Long-form token table construction (id × column × token × row_id)
#       3. Optional blocking via strategy@block_by
#       4. Rarity computation (inverse_freq, tfidf, bm25, etc.)
#       5. Token-overlap joins (self-joins for duplicates; cross-joins for
#          candidate search)
#       6. rIP-based scoring with column weights
#       7. Threshold filtering and connected-component grouping
#
# ============================================================


DT_tbl <- new_S3_class("data.table")

method(
  prepare_search_data,
  list(DT_tbl, class_character, Search_Strategy)
) <- function(data, id, strategy) {
  
  dt <- data.table::copy(data)

  if (!id %in% names(dt)) {
    stop(sprintf("ID column '%s' not found in data", id), call. = FALSE)
  }

  .check_reserved_names(names(dt), id)

  preparers <- strategy@preparers
  block_by  <- strategy@block_by
  
  # --------------------------------------------------------------------
  # Compute total work in advance: sum over all (columns * steps * chunks)
  # --------------------------------------------------------------------
  chunk_size <- 50000L
  
  total_work <- 0L
  for (prep in preparers) {
    col <- prep@column
    n   <- length(dt[[col]])
    n_chunks <- ceiling(n / chunk_size)
    n_steps  <- length(prep@steps)
    total_work <- total_work + n_chunks * n_steps
  }
  
  # Create progress bar
  progress_env <- rlang::env(parent = asNamespace("cli"))
  pb <- progress_init(total = total_work, .envir = progress_env)
  
  # --------------------------------------------------------------------
  # Helper to apply one step with chunking + progress
  # --------------------------------------------------------------------
  apply_step_r <- function(acc, step) {
    
    fn   <- get(step@name, mode = "function")
    args <- step@args
    n    <- length(acc)
    
    if (n < chunk_size) {
      # one-shot application
      progress_update(pb, amount = 1L, .envir = progress_env)
      return(do.call(fn, c(list(acc), args)))
    }
    
    # chunked mode
    idx <- seq.int(1L, n, by = chunk_size)
    out_list <- vector("list", length(idx))
    
    for (i in seq_along(idx)) {
      from <- idx[i]
      to   <- min(from + chunk_size - 1L, n)
      
      out_list[[i]] <- do.call(fn, c(list(acc[from:to]), args))
      
      # known amount of work: one chunk processed
      progress_update(pb, amount = 1L, .envir = progress_env)
    }
    
    unlist(out_list, recursive = FALSE)
  }
  
  # --------------------------------------------------------------------
  # Construct token tables
  # --------------------------------------------------------------------
  token_list <- map(preparers, function(prep) {
    
    col <- prep@column
    if (!col %in% names(dt)) {
      stop(sprintf("Column '%s' not found in data", col), call. = FALSE)
    }
    
    tokens <- Reduce(
      f    = function(acc, step) apply_step_r(acc, step),
      x    = prep@steps,
      init = dt[[col]]
    )
    
    if (!is.list(tokens)) tokens <- as.list(tokens)
    
    lens <- lengths(tokens)
    
    out <- data.table::data.table(
      src_column = col,
      token  = unlist(tokens, use.names = FALSE),
      row_id = rep(seq_len(nrow(dt)), times = lens)
    )
    
    out[[id]] <- rep(dt[[id]], times = lens)
    data.table::setcolorder(out, c(id, "src_column", "token", "row_id"))
    out
  })
  
  tokens <- data.table::rbindlist(token_list, use.names = TRUE, fill = TRUE)
  
  # --------------------------------------------------------------------
  # Add block_by columns
  # --------------------------------------------------------------------
  if (!is.null(block_by)) {
    missing <- setdiff(block_by, names(dt))
    if (length(missing) > 0) {
      stop("Blocking columns not found: ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    
    block_dt <- dt[, c(id, block_by), with = FALSE]
    tokens <- merge(tokens, block_dt, by = id, all.x = TRUE)
  }
  
  progress_finish(pb, progress_env)
  
  tokens[]
}


# Method: compute_rarity for data.table and Search_Strategy
#------------------------------------------------------------------------------
method(
  compute_rarity,
  list(DT_tbl, Search_Strategy)
) <- function(tokens, strategy) {
  
  dt <- data.table::copy(tokens)
  rarity_method <- strategy@rarity
  block_by      <- strategy@block_by
  
  # Grouping keys: block + column + token
  by_keys <- c(block_by, "src_column", "token")
  
  # Ensure block columns exist if block_by was specified
  if (!is.null(block_by)) {
    missing <- setdiff(block_by, names(dt))
    if (length(missing) > 0) {
      stop("Block columns missing: ", paste(missing, collapse = ", "))
    }
  }
  
  # Compute freq + df + N per block/column/token ---------------------------
  dt[, freq := .N, by = by_keys]
  
  # df = number of distinct rows in this block/column where token appears
  dt[, df := uniqueN(row_id), by = by_keys]
  
  # N = total rows in this block/column
  # (we attach once per group; downstream summed over matches)
  dt[, N := uniqueN(row_id), by = c(block_by, "src_column")]
  
  # Apply rarity formula ---------------------------------------------------
  dt[, rarity := {
    f  <- freq
    d  <- df
    n  <- N
    
    switch(
      rarity_method,
      "inverse_freq" = 1 / f,
      "tfidf" = {
        tf <- f / sum(f)
        idf <- log(1 + n / d)
        tf * idf
      },
      "smoothed_inverse_freq" = 1 / (f + 1),
      "bm25"         = log((n - d + 0.5) / (d + 0.5)),
      stop("Unknown rarity method: ", rarity_method)
    )
  }]
  
  dt[]
}


#' Internal helper: compute pairwise scores between two token tables
#' @noRd
.score_token_pairs <- function(
    lhs_tokens,
    rhs_tokens,
    id_lhs,
    id_rhs,
    strategy,
    weights
) {
  
  block_by <- strategy@block_by %||% character()
  by_cols  <- c("src_column", "token", block_by)
  
  # Validate weights
  missing_w <- setdiff(
    unique(c(lhs_tokens$src_column, rhs_tokens$src_column)),
    names(weights)
  )
  if (length(missing_w) > 0) {
    stop("Weights missing for columns: ", paste(missing_w, collapse = ", "))
  }
  
  lhs_tokens[, weight := weights[src_column]]
  rhs_tokens[, weight := weights[src_column]]
  
  # Base rIP
  lhs_tokens[, rIP := rarity / sum(rarity), by = c(id_lhs, "src_column")]
  rhs_tokens[, rIP := rarity / sum(rarity), by = c(id_rhs, "src_column")]
  
  # Optional smoothing
  sm <- strategy@smoothing@method
  if (sm == "log") {
    lhs_tokens[, rIP := {x <- log1p(rIP); x / sum(x)}, by = c(id_lhs, "src_column")]
    rhs_tokens[, rIP := {x <- log1p(rIP); x / sum(x)}, by = c(id_rhs, "src_column")]
  } else if (sm == "softmax") {
    t <- strategy@smoothing@temperature
    lhs_tokens[, rIP := {ex <- exp(rIP / t); ex / sum(ex)}, by = c(id_lhs, "src_column")]
    rhs_tokens[, rIP := {ex <- exp(rIP / t); ex / sum(ex)}, by = c(id_rhs, "src_column")]
  } else if (sm == "offset") {
    a <- strategy@smoothing@alpha
    lhs_tokens[, rIP := {x <- rIP + a; x / sum(x)}, by = c(id_lhs, "src_column")]
    rhs_tokens[, rIP := {x <- rIP + a; x / sum(x)}, by = c(id_rhs, "src_column")]
  }
  
  # Prepare RHS for join
  rhs <- rhs_tokens[, c(id_rhs, "row_id", "src_column", "token", block_by), with = FALSE]
  data.table::setnames(rhs, c(id_rhs, "row_id"), c("rhs_id", "rhs_row"))
  
  # Token-overlap join
  joined <- lhs_tokens[
    rhs,
    on = by_cols,
    allow.cartesian = TRUE,
    nomatch = 0L
  ]
  
  # Compute scores with optional feedback adjustment
  if (strategy@feedback_strength > 0) {
    # Need total rIP per LHS record for overlap calculation
    total_rip <- lhs_tokens[
      , .(total_rip = sum(rIP)),
      by = c(id_lhs)
    ]
    
    # Compute raw score and matched rIP
    scored <- joined[
      , .(
        raw_score = sum(rIP * weight, na.rm = TRUE),
        matched_rip = sum(rIP, na.rm = TRUE)
      ),
      by = .(lhs_id = get(id_lhs), rhs_id)
    ]
    
    # Join total rIP
    scored <- merge(scored, total_rip, by.x = "lhs_id", by.y = id_lhs, all.x = TRUE)
    
    # Compute overlap share and adjusted score
    s <- strategy@feedback_strength
    scored[, overlap_share := matched_rip / total_rip]
    scored[, score := raw_score * (1 - s * (1 - overlap_share))]
    
    # Clean up intermediate columns
    scored[, c("raw_score", "matched_rip", "total_rip", "overlap_share") := NULL]
  } else {
    # Standard scoring without feedback
    scored <- joined[
      , .(score = sum(rIP * weight, na.rm = TRUE)),
      by = .(lhs_id = get(id_lhs), rhs_id)
    ]
  }
  
  scored[]
}



#' Internal helper: per-token attribution for a single matched pair
#'
#' Reuses the rIP / smoothing / feedback logic from `.score_token_pairs()` but
#' returns per-token contributions instead of an aggregate score.  Both
#' `lhs_tokens` and `rhs_tokens` must already carry a `rarity` column
#' (output of `compute_rarity()`).
#'
#' @noRd
.pair_attribution_dt <- function(
    lhs_tokens,
    rhs_tokens,
    id_lhs,
    id_rhs,
    strategy,
    weights
) {
  lhs <- data.table::copy(lhs_tokens)
  rhs <- data.table::copy(rhs_tokens)

  block_by <- strategy@block_by %||% character()
  by_cols  <- c("src_column", "token", block_by)

  # Validate weights — same check as .score_token_pairs()
  missing_w <- setdiff(
    unique(c(lhs$src_column, rhs$src_column)),
    names(weights)
  )
  if (length(missing_w) > 0) {
    stop("Weights missing for columns: ", paste(missing_w, collapse = ", "))
  }

  # Weights
  lhs[, weight := weights[src_column]]
  rhs[, weight := weights[src_column]]

  # Base rIP — same formula as .score_token_pairs()
  lhs[, rIP := rarity / sum(rarity), by = c(id_lhs, "src_column")]
  rhs[, rIP := rarity / sum(rarity), by = c(id_rhs, "src_column")]

  # Smoothing — identical branches to .score_token_pairs()
  sm <- strategy@smoothing@method
  if (sm == "log") {
    lhs[, rIP := {v <- log1p(rIP); v / sum(v)}, by = c(id_lhs, "src_column")]
    rhs[, rIP := {v <- log1p(rIP); v / sum(v)}, by = c(id_rhs, "src_column")]
  } else if (sm == "softmax") {
    t <- strategy@smoothing@temperature
    lhs[, rIP := {ex <- exp(rIP / t); ex / sum(ex)}, by = c(id_lhs, "src_column")]
    rhs[, rIP := {ex <- exp(rIP / t); ex / sum(ex)}, by = c(id_rhs, "src_column")]
  } else if (sm == "offset") {
    a <- strategy@smoothing@alpha
    lhs[, rIP := {v <- rIP + a; v / sum(v)}, by = c(id_lhs, "src_column")]
    rhs[, rIP := {v <- rIP + a; v / sum(v)}, by = c(id_rhs, "src_column")]
  }

  # Token overlap join
  rhs_join <- rhs[, c(id_rhs, "src_column", "token", block_by), with = FALSE]
  data.table::setnames(rhs_join, id_rhs, "__rhs_id__")

  joined <- lhs[rhs_join, on = by_cols, nomatch = 0L, allow.cartesian = TRUE]
  joined[, contribution := rIP * weight]

  # Shared tokens table
  shared_tokens <- joined[, .(
    src_column   = src_column,
    token        = token,
    rarity       = rarity,
    rIP          = rIP,
    weight       = weight,
    contribution = contribution
  )]
  data.table::setorder(shared_tokens, src_column, -contribution)
  shared_tokens[]

  # Per-column aggregation
  per_column_contrib <- shared_tokens[, .(
    contribution    = sum(contribution),
    n_shared_tokens = .N
  ), by = "src_column"]
  data.table::setorder(per_column_contrib, -contribution)

  raw_score <- sum(per_column_contrib$contribution)

  # Feedback adjustment — same formula as .score_token_pairs()
  feedback_factor <- 1.0
  overlap_share   <- NA_real_

  if (strategy@feedback_strength > 0) {
    total_rip_lhs <- sum(lhs$rIP)
    matched_rip   <- sum(joined$rIP)
    overlap_share <- if (total_rip_lhs > 0) matched_rip / total_rip_lhs else 0
    s             <- strategy@feedback_strength
    feedback_factor <- 1 - s * (1 - overlap_share)
  }

  list(
    shared_tokens      = shared_tokens,
    per_column_contrib = per_column_contrib,
    score              = raw_score * feedback_factor,
    score_breakdown    = list(
      smoothing_method  = strategy@smoothing@method,
      feedback_strength = strategy@feedback_strength,
      feedback_factor   = feedback_factor,
      overlap_share     = overlap_share
    )
  )
}


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
    stop("Weights missing for columns: ", paste(missing_w, collapse = ", "))
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
  if (is.null(thr)) stop("Strategy must define a threshold.")
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
    stop(sprintf("ID '%s' not found in base_table", id), call. = FALSE)
  }      
  duplicate_ids <- duplicates[rank!=1L,]$id
  to_remove <- as.character(dt[[id]]) %in% duplicate_ids
  
  dt[!to_remove,][]
}


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
    stop("Weights missing for columns: ", paste(missing_w, collapse = ", "))
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
  if (is.null(thr)) stop("Strategy must define a threshold.")
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

# Method: extract_unmatched 
#------------------------------------------------------------------------------
method(
  extract_unmatched,
  list(DT_tbl, class_character, DT_tbl)
) <- function(data, id, matches) {
  dt <- data.table::copy(data)
  
  if (!id %in% names(dt)) {
    stop(sprintf("ID column '%s' not found in data", id), call. = FALSE)
  }
  
  if (!"id" %in% names(matches)) {
    stop("`matches` must contain a column named 'id'", call. = FALSE)
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

# Method: multi_stage_match
#------------------------------------------------------------------------------
method(
  multi_stage_match,
  list(DT_tbl, DT_tbl, class_character, class_character, class_list)
) <- function(base_table,
              target_table,
              base_id,
              target_id,
              strategies,
              ...) {

  # ---- VALIDATION ----------------------------------------------------------
  c("strategies must be a list"    =  is.list(strategies),
    "strategies must not be empty" =  length(strategies) > 0) |> 
    validate_inputs()
  
  # If names missing:  assign "strategy_1", "strategy_2", …
  if (is.null(names(strategies)) || any(names(strategies) == "")) {
    names(strategies) <- paste0("strategy_", seq_along(strategies))
  }
  
  # Ensure all elements are Search_Strategy or Embedding_Strategy
  valid_strategy <- function(s) S7_inherits(s, Search_Strategy) || S7_inherits(s, Embedding_Strategy)
  c("strategies must be a list of Search_Strategy or Embedding_Strategy objects" =
      is.list(strategies) && all(sapply(strategies, valid_strategy))
  ) |> validate_inputs()
  
  # ---- PREP ----------------------------------------------------------------
  base_res   <- data.table::copy(base_table)
  target_res <- data.table::copy(target_table)
  
  all_matches   <- list()
  match_counter <- 0L
  
  # ---- MAIN LOOP -----------------------------------------------------------
  for (stage_name in names(strategies)) {
    strategy <- strategies[[stage_name]]
    
    # Run stage matching
    stage_matches <- search_candidates(
      base_res,
      target_res,
      base_id,
      target_id,
      strategy = strategy
    )
    
    if (nrow(stage_matches) > 0) {
      # Label stage
      stage_matches[, stage := stage_name]
      
      # Make match_id globally unique across stages
      # original match_id resets inside search_candidates
      stage_matches[, match_id := match_id + match_counter]
      
      match_counter <- max(stage_matches$match_id)
      
      all_matches[[stage_name]] <- stage_matches
      
      # Remove matched rows (per side)
      base_res <- extract_unmatched(
        base_res, base_id, stage_matches[source == "base"]
      )
      target_res <- extract_unmatched(
        target_res, target_id, stage_matches[source == "target"]
      )
      
      # Stop if one side is empty
      if (nrow(base_res) == 0L || nrow(target_res) == 0L) break
    }
  }
  
  # ---- RETURN --------------------------------------------------------------
  if (length(all_matches) == 0L) {
    # Empty-structure return (schema only)
    return(data.table::data.table(
      match_id = integer(),
      score    = numeric(),
      stage    = character(),
      source   = character(),
      id       = character(),
      rank     = integer()
    ))
  }
  
  out <- data.table::rbindlist(all_matches, use.names = TRUE, fill = TRUE)
  data.table::setorder(out, match_id, stage, source, rank)
  out[]
}

# Method: .inspect_tokens  
# (The dot is so  so we can use enysm on the column and do not get in trouble
#  with class_character)
#------------------------------------------------------------------------------
method(
  .inspect_tokens,
  list(DT_tbl, class_character, Search_Strategy, class_character)
) <- function(data, id, strategy, column) {
  dt <- data.table::copy(data)
  # --- Validate inputs -----------------------------------------------------
  if (!id %in% names(dt)) {
    stop(sprintf("ID column '%s' not found in data", id), call. = FALSE)
  }
  if (!column %in% names(dt)) {
    stop(sprintf("Column '%s' not found in data", column), call. = FALSE)
  }
  if (!column %in% names(strategy@preparers)) {
    stop(sprintf("Column '%s' not found in strategy preparers", column), call. = FALSE)
  }
  
  # --- 1. Create single-column strategy for efficiency ---------------------
  single_col_strategy <- copy(strategy)
  single_col_strategy@preparers <- list(strategy@preparers[[column]])
  names(single_col_strategy@preparers) <- column
  
  # --- 2. Prepare tokens via joinery's interpreter -------------------------
  tokens <- prepare_search_data(
    data     = dt,
    id       = id,
    strategy = single_col_strategy
  )
  
  # --- 3. Join back to retrieve the original strings -----------------------
  dt_join <- dt[, c(id, column), with = FALSE]
  
  merged <- merge(
    tokens,
    dt_join,
    by = id,
    all.x = TRUE,
    sort = FALSE
  )
  
  # --- 4. Count occurrences (token × original string) ----------------------
  res <- merged[
    ,
    .(n = .N),
    by = c("token", column)
  ]
  
  res[]
}


# ============================================================

