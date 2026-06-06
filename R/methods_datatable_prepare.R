# ============================================================
# data.table backend — preprocessing & rarity
# ============================================================
#
# Implements the entry points to the data.table backend:
# `prepare_search_data()` (column-wise Step pipeline → long-form
# token table) and `compute_rarity()` (block- and column-aware
# rarity computation). Also defines the shared internal helpers
# `.score_token_pairs()` and `.pair_attribution_dt()` consumed by
# the dedup, candidate-search, and explanation paths.
#
# ============================================================


DT_tbl <- new_S3_class("data.table")

method(
  prepare_search_data,
  list(DT_tbl, class_character, Search_Strategy)
) <- function(data, id, strategy) {

  dt <- data.table::copy(data)

  if (!id %in% names(dt)) {
    cli::cli_abort("ID column {.field {id}} not found in data")
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
      cli::cli_abort("Column {.field {col}} not found in data")
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
      cli::cli_abort("Blocking columns not found: {.field {missing}}")
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
      cli::cli_abort("Block columns missing: {.field {missing}}")
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
      cli::cli_abort("Unknown rarity method: {.val {rarity_method}}")
    )
  }]

  dt[]
}


#' Internal helper: collapse a token table to set semantics
#'
#' Scoring treats each record's tokens as a SET, not a bag: a token repeated
#' within one record's column must contribute once, not once per occurrence.
#' Without this, the token-overlap join multiplies a shared token's rIP by the
#' product of its per-record multiplicities, inflating scores past the
#' `sum(weights)` ceiling (e.g. "Fritzel … Fritzel … Fritzel" scoring 2.8).
#'
#' Deduping happens here, in the scoring path, rather than in
#' `prepare_search_data()` / `compute_rarity()` on purpose: `inverse_freq`
#' rarity is `1 / freq` where `freq` is the corpus term-frequency, so collapsing
#' upstream would silently redefine the rarity metric. `rarity` is constant per
#' `(block, src_column, token)`, so `unique()` retains the correct value.
#' @noRd
.collapse_token_set <- function(tokens, id_col, block_by) {
  keys <- c(id_col, "src_column", "token", block_by)
  unique(tokens, by = keys)
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

  # Set semantics: collapse within-record token multiplicity before scoring.
  lhs_tokens <- .collapse_token_set(lhs_tokens, id_lhs, block_by)
  rhs_tokens <- .collapse_token_set(rhs_tokens, id_rhs, block_by)

  # Validate weights
  missing_w <- setdiff(
    unique(c(lhs_tokens$src_column, rhs_tokens$src_column)),
    names(weights)
  )
  if (length(missing_w) > 0) {
    cli::cli_abort("Weights missing for columns: {.field {missing_w}}")
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
  block_by <- strategy@block_by %||% character()
  by_cols  <- c("src_column", "token", block_by)

  # Set semantics: collapse within-record token multiplicity before
  # attribution, identically to .score_token_pairs(), so the round-trip
  # contract sum(per_column_contrib$contribution) * feedback_factor == score
  # continues to hold.
  lhs <- .collapse_token_set(data.table::copy(lhs_tokens), id_lhs, block_by)
  rhs <- .collapse_token_set(data.table::copy(rhs_tokens), id_rhs, block_by)

  # Validate weights — same check as .score_token_pairs()
  missing_w <- setdiff(
    unique(c(lhs$src_column, rhs$src_column)),
    names(weights)
  )
  if (length(missing_w) > 0) {
    cli::cli_abort("Weights missing for columns: {.field {missing_w}}")
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
