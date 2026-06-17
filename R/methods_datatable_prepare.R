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
) <- function(data, id, strategy, warn_nonunique_id = TRUE,
              explode_token_blocks = TRUE) {

  dt <- data.table::copy(data)

  if (!id %in% names(dt)) {
    cli::cli_abort("ID column {.field {id}} not found in data")
  }

  .check_reserved_names(names(dt), id)

  # Non-unique id is a data problem the caller usually doesn't know they have:
  # rows sharing an id are folded into one record (their tokens are pooled), and
  # before the block-merge fix below it caused an opaque cartesian crash. Warn
  # once, with the count, so the duplication is visible rather than silent.
  # warn_nonunique_id is set FALSE by the DuckDB per-batch tokenizer, which sees
  # only a partial slice of the ids; that backend runs one global check up front
  # instead (D2). See .warn_nonunique_id() in internal_validation.R.
  if (warn_nonunique_id) {
    .warn_nonunique_id(sum(duplicated(dt[[id]])), id)
  }

  preparers <- strategy@preparers

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
  # Only the PLAIN (literal-column) block entries are merged here from the raw
  # data; token-blocking specs become the derived `._btok` column in the
  # explosion below. .plain_block_cols() drops any block_on_tokens() spec.
  plain_block <- .plain_block_cols(strategy)
  if (length(plain_block)) {
    missing <- setdiff(plain_block, names(dt))
    if (length(missing) > 0) {
      cli::cli_abort("Blocking columns not found: {.field {missing}}")
    }

    # Block attributes are per-record, so attach one row per id. Deduping by id
    # here is mandatory, not just tidy: if `id` is non-unique (duplicate rows in
    # `data`), a raw `dt[, c(id, block_by)]` carries one row per duplicate, and
    # the many-to-one block merge becomes many-to-many — a cartesian explosion
    # that trips data.table's allow.cartesian guard deep in merge() with an
    # opaque error. `unique(by = id)` keeps the merge strictly many-to-one.
    block_dt <- unique(dt[, c(id, plain_block), with = FALSE], by = id)
    tokens <- merge(tokens, block_dt, by = id, all.x = TRUE)
  }

  # Token-blocking (Feature A): explode each record's token rows against its
  # surviving rare blocking-tokens into a derived `._btok` block column. This
  # runs strictly AFTER the non-unique-id guard above, so the id repetition the
  # explosion introduces (one id per `._btok` value) never trips that guard -
  # the repetition is the mechanism, not a data bug. See
  # notes/region_free_linking.md section 4.5.
  #
  # explode_token_blocks = FALSE is the DuckDB per-batch path: a batch is a
  # partial slice of the ids, so its global-df cut on blocking tokens would be
  # batch-local (wrong). The DuckDB prepare method runs the explosion once,
  # globally, on the assembled token table instead. See methods_duckdb_prepare.R.
  if (explode_token_blocks) {
    tokens <- .explode_token_blocks_dt(tokens, dt, id, strategy)
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
  # Effective block columns of the token table: plain block columns plus the
  # derived `._btok` (token-blocking). The cost axis (block-local df) groups by
  # these, exactly as the overlap join does. See .block_cols().
  block_by      <- .block_cols(strategy)
  rarity_scope  <- strategy@rarity_scope

  # Grouping keys: block + column + token
  by_keys <- c(block_by, "src_column", "token")

  # Ensure block columns exist if block_by was specified
  if (length(block_by)) {
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

  # Global (corpus-wide) freq/df/N - only the rarity metric uses these.
  # The block-local df above stays the cost axis (max_token_df, fan-out
  # guard, prefilter all keep reading it); only the informativeness measure
  # follows rarity_scope. See notes/region_free_linking.md section 5.2.
  if (rarity_scope == "global") {
    dt[, freq_global := .N, by = c("src_column", "token")]
    dt[, df_global := uniqueN(row_id), by = c("src_column", "token")]
    dt[, N_global := uniqueN(row_id), by = "src_column"]
  }

  # Apply rarity formula ---------------------------------------------------
  dt[, rarity := {
    if (rarity_scope == "global") {
      f <- freq_global
      d <- df_global
      n <- N_global
    } else {
      f <- freq
      d <- df
      n <- N
    }

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

#' Internal helper: apply the pre-join rarity / document-frequency cut
#'
#' joinery's single biggest cheap lever for a slow/dense linkage is to thin the
#' token table **before** the `(src_column, token, block)` equi-join - never
#' after scoring, where it would save nothing. This helper applies both cut
#' axes in one predicate on a `compute_rarity()` output (which carries `rarity`
#' and `df`): `min_rarity` floors the rarity metric, `max_token_df` caps raw
#' document frequency. It is the data.table mirror of `.rarity_prefilter_sql()`
#' on the DuckDB backend - keep the two predicates identical.
#' @noRd
.rarity_prefilter_dt <- function(tokens, strategy) {
  if (strategy@min_rarity > 0 || is.finite(strategy@max_token_df)) {
    tokens <- tokens[rarity >= strategy@min_rarity & df <= strategy@max_token_df]
  }
  tokens
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

  # Effective block columns of the token table (plain + derived `._btok`).
  block_by <- .block_cols(strategy)
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

  # rIP / smoothing / feedback are normalised per record PER BLOCK. With plain
  # blocking a record sits in exactly one block, so c(id, src_column) and
  # c(id, src_column, block_by) are equivalent - behaviour is unchanged. Under
  # token-blocking (`._btok`) a record is exploded into several blocks, each
  # carrying its full original token set; normalising per block keeps each
  # block's score equal to the un-exploded score, and the final per-pair score
  # is the max over the blocks the pair co-occurs in (see the aggregation
  # below). See notes/region_free_linking.md section 4.
  rip_lhs <- c(id_lhs, "src_column", block_by)
  rip_rhs <- c(id_rhs, "src_column", block_by)

  # Base rIP
  lhs_tokens[, rIP := rarity / sum(rarity), by = rip_lhs]
  rhs_tokens[, rIP := rarity / sum(rarity), by = rip_rhs]

  # Optional smoothing
  sm <- strategy@smoothing@method
  if (sm == "log") {
    lhs_tokens[, rIP := {x <- log1p(rIP); x / sum(x)}, by = rip_lhs]
    rhs_tokens[, rIP := {x <- log1p(rIP); x / sum(x)}, by = rip_rhs]
  } else if (sm == "softmax") {
    t <- strategy@smoothing@temperature
    lhs_tokens[, rIP := {ex <- exp(rIP / t); ex / sum(ex)}, by = rip_lhs]
    rhs_tokens[, rIP := {ex <- exp(rIP / t); ex / sum(ex)}, by = rip_rhs]
  } else if (sm == "offset") {
    a <- strategy@smoothing@alpha
    lhs_tokens[, rIP := {x <- rIP + a; x / sum(x)}, by = rip_lhs]
    rhs_tokens[, rIP := {x <- rIP + a; x / sum(x)}, by = rip_rhs]
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

  # Compute scores with optional feedback adjustment. Aggregation is per pair
  # PER BLOCK (block_by, which under token-blocking is `._btok`), then collapsed
  # to one row per pair by taking the max over blocks. With plain blocking each
  # pair sits in one block, so this is a no-op; under token-blocking it dedups a
  # pair that co-blocks under several `._btok` keys to its single best score.
  if (strategy@feedback_strength > 0) {
    # Need total rIP per LHS record PER BLOCK for the overlap calculation.
    total_rip <- lhs_tokens[
      , .(total_rip = sum(rIP)),
      by = c(id_lhs, block_by)
    ]

    # Compute raw score and matched rIP per (pair, block).
    scored <- joined[
      , .(
        raw_score = sum(rIP * weight, na.rm = TRUE),
        matched_rip = sum(rIP, na.rm = TRUE)
      ),
      by = c(id_lhs, "rhs_id", block_by)
    ]

    # Join total rIP on (lhs record, block), then rename the lhs id column.
    scored <- merge(scored, total_rip, by = c(id_lhs, block_by), all.x = TRUE)
    data.table::setnames(scored, id_lhs, "lhs_id")

    # Compute overlap share and adjusted score
    s <- strategy@feedback_strength
    scored[, overlap_share := matched_rip / total_rip]
    scored[, score := raw_score * (1 - s * (1 - overlap_share))]

    # Clean up intermediate columns
    scored[, c("raw_score", "matched_rip", "total_rip", "overlap_share") := NULL]
  } else {
    # Standard scoring without feedback, per (pair, block).
    scored <- joined[
      , .(score = sum(rIP * weight, na.rm = TRUE)),
      by = c(id_lhs, "rhs_id", block_by)
    ]
    data.table::setnames(scored, id_lhs, "lhs_id")
  }

  # Collapse per-block scores to one best row per pair (no-op for plain
  # blocking). Done before on_missing so the denominator is per pair.
  if (length(block_by)) {
    scored <- scored[order(-score), .SD[1L], by = .(lhs_id, rhs_id)]
    scored[, (block_by) := NULL]
  }

  # on_missing = "renormalise": rescale each pair's score by the weight of the
  # columns actually present (on either side), so a column empty on BOTH records
  # no longer caps the score at 1 - weight(col). See B1 / §25.
  if (.strategy_on_missing(strategy) == "renormalise") {
    z <- .present_col_denominator(
      lhs_tokens, rhs_tokens, id_lhs, id_rhs, weights, scored
    )
    scored <- merge(scored, z, by = c("lhs_id", "rhs_id"), all.x = TRUE)
    scored[is.na(z_pair) | z_pair <= 0, z_pair := 1]
    scored[, score := score / z_pair]
    scored[, z_pair := NULL]
  }

  scored[]
}

#' Read the `on_missing` policy off any strategy, defaulting to "penalise".
#'
#' Robust to strategy classes that predate / lack the slot (e.g. an
#' `Exact_Strategy` never reaches the token scorer, but keep the read total).
#' @noRd
.strategy_on_missing <- function(strategy) {
  if ("on_missing" %in% S7::prop_names(strategy)) {
    om <- strategy@on_missing
    if (length(om)) return(om[[1]])
  }
  "penalise"
}

#' Per-pair present-column weight denominator for `on_missing = "renormalise"`.
#'
#' `z_pair = WA + WB - Wboth`, the total weight of columns present on *either*
#' record of the pair (presence = the column has >= 1 token). A column empty on
#' both sides is excluded from the denominator (its weight is redistributed); a
#' column present on one side only stays in it (a genuine penalty). Computed
#' only for the candidate pairs in `scored`.
#' @noRd
.present_col_denominator <- function(lhs_tokens, rhs_tokens,
                                     id_lhs, id_rhs, weights, scored) {
  lhs_pres <- unique(lhs_tokens[, .(lhs_id = get(id_lhs), src_column)])
  rhs_pres <- unique(rhs_tokens[, .(rhs_id = get(id_rhs), src_column)])
  lhs_pres[, w := weights[src_column]]
  rhs_pres[, w := weights[src_column]]

  wa <- lhs_pres[, .(wa = sum(w)), by = lhs_id]
  wb <- rhs_pres[, .(wb = sum(w)), by = rhs_id]

  pairs <- unique(scored[, .(lhs_id, rhs_id)])

  # Columns present on both sides of a candidate pair -> Wboth.
  pl <- lhs_pres[pairs, on = "lhs_id", allow.cartesian = TRUE]
  both <- pl[rhs_pres, on = c("rhs_id", "src_column"), nomatch = 0L]
  wboth <- both[, .(wboth = sum(w)), by = .(lhs_id, rhs_id)]

  out <- merge(pairs, wa, by = "lhs_id", all.x = TRUE)
  out <- merge(out, wb, by = "rhs_id", all.x = TRUE)
  out <- merge(out, wboth, by = c("lhs_id", "rhs_id"), all.x = TRUE)
  out[is.na(wboth), wboth := 0]
  out[, z_pair := wa + wb - wboth]
  out[, .(lhs_id, rhs_id, z_pair)]
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
  # Effective block columns of the token table (plain + derived `._btok`).
  block_by <- .block_cols(strategy)

  # Token-blocking: a record is exploded across several `._btok` blocks, each
  # carrying its full token set. The scorer takes the max over blocks, so
  # attribution must explain that single best block. Restrict both sides to one
  # `._btok` they share (every shared block yields the same contribution), then
  # drop `._btok` so the rest of the attribution math runs exactly as before and
  # the round-trip contract still holds. See .score_token_pairs().
  if (.JOINERY_BTOK_COL %in% block_by) {
    lhs_tokens <- data.table::as.data.table(lhs_tokens)
    rhs_tokens <- data.table::as.data.table(rhs_tokens)
    shared_bt <- intersect(
      unique(lhs_tokens[[.JOINERY_BTOK_COL]]),
      unique(rhs_tokens[[.JOINERY_BTOK_COL]])
    )
    # The scorer normalises rIP per `._btok` block and, for a pair that
    # co-blocks under several rare tokens, keeps the MAX-scoring block (block
    # local rarity differs between blocks). Attribution must explain that same
    # block, or explain_match's score would disagree with the reported match
    # score. Evaluate every shared block and return the best one (recursing so
    # each single-block call runs the standard math below). Each recursive call
    # sees exactly one shared `._btok`, so the recursion is one level deep.
    if (length(shared_bt) > 1L) {
      cand <- lapply(shared_bt, function(pick) {
        .pair_attribution_dt(
          lhs_tokens[lhs_tokens[[.JOINERY_BTOK_COL]] == pick],
          rhs_tokens[rhs_tokens[[.JOINERY_BTOK_COL]] == pick],
          id_lhs, id_rhs, strategy, weights
        )
      })
      scores <- vapply(cand, function(r) r$score, numeric(1))
      return(cand[[which.max(scores)]])
    }
    if (length(shared_bt)) {
      pick <- shared_bt[[1L]]
      lhs_tokens <- lhs_tokens[lhs_tokens[[.JOINERY_BTOK_COL]] == pick]
      rhs_tokens <- rhs_tokens[rhs_tokens[[.JOINERY_BTOK_COL]] == pick]
    }
    block_by <- setdiff(block_by, .JOINERY_BTOK_COL)
  }
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

  # on_missing = "renormalise": divide raw_score AND every per-token/per-column
  # contribution by the same present-column denominator the scorer uses, so the
  # round-trip contract (sum(contributions) * feedback_factor == score) and
  # cross-check against .score_token_pairs() both hold exactly. z = total weight
  # of columns present on either record (columns empty on both are excluded).
  if (.strategy_on_missing(strategy) == "renormalise") {
    present <- union(unique(lhs$src_column), unique(rhs$src_column))
    z_pair  <- sum(weights[present])
    if (!is.finite(z_pair) || z_pair <= 0) z_pair <- 1
    shared_tokens[, contribution := contribution / z_pair]
    per_column_contrib[, contribution := contribution / z_pair]
    raw_score <- raw_score / z_pair
  }

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
