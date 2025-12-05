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


# Method: detect_duplicates 
#------------------------------------------------------------------------------
method(
  detect_duplicates,
  list(DT_tbl, class_character ,Search_Strategy)
) <- function(base_table, id, strategy, weights = NULL) {
  
  dt <- data.table::copy(base_table)
  
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
  
  # Add weight column
  tokens[, weight := weights[src_column]]
  
  #Compute Identification Potential based on rarity metric
  tokens[, rIP := rarity / sum(rarity), by = .(get(id), src_column)]
  
  # --- 4. Self-join on shared tokens (within blocks) -----------------------
  
  block_by <- strategy@block_by %||% character()
  by_cols  <- c("src_column", "token", block_by)
  
  rhs <- tokens[, c(id, "row_id", "src_column", "token", block_by), with = FALSE]
  
  id2  <- paste0(id, "_2")
  row2 <- "row_id_2"
  
  data.table::setnames(rhs, id,  id2)
  data.table::setnames(rhs, "row_id", row2)
  
  joined <- tokens[
    rhs,
    on = by_cols,
    allow.cartesian = TRUE,
    nomatch = 0
  ]
  
  # Remove self matches
  joined <- joined[joined[[id]] != joined[[id2]]]
  
  scored <- joined[
    , .(score = sum(rIP * weight, na.rm = TRUE)),
    by = c(id, id2)
  ]
  
  # Apply threshold
  thr <- strategy@threshold
  if (is.null(thr)) stop("Strategy must define a threshold.")
  scored <- scored[score >= thr]
  
  if (nrow(scored) == 0L) {
    return(data.table(
      duplicate_group = integer(),
      id              = character(),
      score           = numeric(),
      rank            = integer()
    ))
  }
  
  # --- 6. Build connected components (robust edges) ------------------------
  edges <- scored[, .(
    from = .SD[[1]],
    to   = .SD[[2]]
  ), .SDcols = c(id, id2)]
  
  
  # Mirror edges for safety
  edges <- rbind(edges, edges[, .(from = to, to = from)])
  
  # Ensure all nodes included
  all_ids <- unique(c(tokens[[id]]))
  
  g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = all_ids)
  comp <- igraph::components(g)
  
  membership_dt <- data.table(
    id              = names(comp$membership),
    duplicate_group = unname(comp$membership)
  )
  
  # --- 7. Insert scores & ranks -------------------------------------------
  scored_long <- rbindlist(list(
    scored[, .(id = get(id),  score)],
    scored[, .(id = get(id2), score)]
  ))
  
  best <- scored_long[
    , .(score = max(score, na.rm = TRUE)),
    by = id
  ]
  
  result <- membership_dt[best, on = "id"]
  
  result[, rank := rank(-score, ties.method = "first"), by = duplicate_group]
  
  data.table::setkeyv(result, c("duplicate_group", "rank"))
  
  # ---8. Attach original data -----------------------------------------------
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
    stop(sprintf("ID '%s' not found in base_table", col), call. = FALSE)
  }      
  duplicate_ids <- duplicates[rank!=1L,]$id
  to_remove <- dt[[id]] %in% duplicate_ids
  
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
  
  block_by <- strategy@block_by %||% character()
  
  # --- 1. Prepare token tables ----------------------------------------------
  base_tokens <- prepare_search_data(base_dt,   base_id,   strategy)
  base_tokens[, side := "base"]
  
  target_tokens <- prepare_search_data(target_dt, target_id, strategy)
  target_tokens[, side := "target"]
  
  # Add a unified key for side-specific IDs
  base_tokens[, uid := base_tokens[[base_id]]]
  target_tokens[, uid := target_tokens[[target_id]]]
  
  # --- 2. Compute rarity -----------------------------------------------------
  all_tokens <- rbindlist(list(base_tokens, target_tokens), use.names = TRUE, fill = TRUE)
  all_tokens <- compute_rarity(all_tokens, strategy)
  if (strategy@min_rarity > 0) {
    all_tokens <- all_tokens[rarity >= strategy@min_rarity]
  }
  
  # Split back
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
  
  base_tokens[, weight := weights[src_column]]
  target_tokens[, weight := weights[src_column]]
  
  # --- 4. Compute rIP per record × column -----------------------------------
  base_tokens[,  rIP := rarity / sum(rarity), by = .(uid, src_column)]
  target_tokens[, rIP := rarity / sum(rarity), by = .(uid, src_column)]
  
  # --- 5. Cross-table join on shared tokens (respecting block_by) ------------
  by_cols <- c("src_column", "token", block_by)
  
  rhs <- target_tokens[, c("uid", "row_id", "src_column", "token", block_by), with = FALSE]
  data.table::setnames(rhs, "uid",    "uid2")
  data.table::setnames(rhs, "row_id", "row_id2")
  
  joined <- base_tokens[
    rhs,
    on = by_cols,
    allow.cartesian = TRUE,
    nomatch = 0L
  ]
  
  # --- 6. Compute pairwise similarity ----------------------------------------
  scored <- joined[
    , .(score = sum(rIP * weight, na.rm = TRUE)),
    by = .(uid, uid2)
  ]
  
  thr <- strategy@threshold
  if (is.null(thr)) stop("Strategy must define a threshold.")
  scored <- scored[score >= thr]
  
  if (nrow(scored) == 0) {
    return(data.table(
      match_id = integer(),
      score    = numeric(),
      source   = character(),
      id       = character(),
      rank     = integer()
    ))
  }
  
  # --- 7. Assign match IDs ---------------------------------------------------
  scored[, match_id := .I]
  
  # --- 8. Expand to long form (base + target rows per match) -----------------
  long <- rbindlist(list(
    scored[, .(match_id, score, source = "base",   id = uid)],
    scored[, .(match_id, score, source = "target", id = uid2)]
  ))
  
  # --- 9. Attach original base and target metadata ---------------------------
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
  
  # --- 10. Rank within match_id ---------------------------------------------
  out[, rank := rank(-score, ties.method = "first"), by = match_id]
  
  # Standardize ordering
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
  
  matched_ids <- unique(matches[["id"]])
  
  # keep rows whose ID is NOT in matched_ids
  dt[!(dt[[id]] %in% matched_ids), ][]
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
  
  # Ensure all elements are Search_Strategy
  c("strategies must be a list of Search_Strategy objects" = is.list(strategies) && all(sapply(strategies, S7_inherits, Search_Strategy))
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
