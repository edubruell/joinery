# ============================================================
# audit_strategy() — all backends (Phase 0.6 M3)
# ============================================================
#
# Pre-match diagnostic (Q1). Runs prepare_search_data +
# compute_rarity, computes per-column token/rarity stats,
# optional block size distribution and comparison-count estimate,
# optional vocabulary overlap with a target table.
#
# Backends: data.table (reference), DuckDB (sample to R then
#   delegate), tibble/data.frame (convert via as_DT).
# ============================================================


# Tokens with rarity below this threshold are considered "low rarity"
# (so frequent they carry almost no discriminating power).
.LOW_RARITY_THRESHOLD <- 0.01


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' @noRd
.compute_na_rates <- function(data, strategy) {
  cols <- names(strategy@preparers)
  n    <- nrow(data)
  stats::setNames(
    vapply(cols, function(col) {
      if (!col %in% names(data)) return(NA_real_)
      sum(is.na(data[[col]])) / n
    }, numeric(1L)),
    cols
  )
}


#' @noRd
.compute_column_token_stats <- function(tokens, na_rates, n_records) {
  stats_dt <- tokens[, .(
    n_tokens            = .N,
    n_unique_tokens     = data.table::uniqueN(token)
  ), by = "src_column"]

  data.table::setnames(stats_dt, "src_column", "column")
  stats_dt[, pct_unique            := n_unique_tokens / n_tokens]
  stats_dt[, avg_tokens_per_record := n_tokens / n_records]

  # Join pre-tokenisation NA rates
  na_dt <- data.table::data.table(
    column  = names(na_rates),
    na_rate = unname(na_rates)
  )
  stats_dt <- merge(stats_dt, na_dt, by = "column", all.y = TRUE)

  # Columns that produced no tokens (all-NA) get zero counts
  stats_dt[is.na(n_tokens), n_tokens            := 0L]
  stats_dt[is.na(n_unique_tokens), n_unique_tokens := 0L]
  stats_dt[is.na(avg_tokens_per_record), avg_tokens_per_record := 0]
  stats_dt[n_tokens == 0L, pct_unique := NA_real_]

  stats_dt[, n_tokens         := as.integer(n_tokens)]
  stats_dt[, n_unique_tokens  := as.integer(n_unique_tokens)]
  stats_dt[, pct_unique       := as.numeric(pct_unique)]
  stats_dt[, na_rate          := as.numeric(na_rate)]
  stats_dt[, avg_tokens_per_record := as.numeric(avg_tokens_per_record)]

  data.table::setcolorder(stats_dt, c(
    "column", "n_tokens", "n_unique_tokens", "pct_unique",
    "na_rate", "avg_tokens_per_record"
  ))
  data.table::setorder(stats_dt, column)
  stats_dt[]
}


#' @noRd
.compute_column_rarity_stats <- function(tokens) {
  # Work on unique (src_column, token) pairs — rarity is per token type
  unique_tok <- unique(tokens[, .(src_column, token, rarity)])

  stats_dt <- unique_tok[, {
    r <- rarity[!is.na(rarity)]
    q <- if (length(r) > 0L) {
      stats::quantile(r, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), names = FALSE)
    } else {
      rep(NA_real_, 5L)
    }
    pct_low <- if (length(r) > 0L) mean(r < .LOW_RARITY_THRESHOLD) else NA_real_
    .(
      rarity_p05     = q[1L],
      rarity_p25     = q[2L],
      rarity_p50     = q[3L],
      rarity_p75     = q[4L],
      rarity_p95     = q[5L],
      pct_low_rarity = pct_low
    )
  }, by = "src_column"]

  data.table::setnames(stats_dt, "src_column", "column")
  data.table::setorder(stats_dt, column)
  stats_dt[]
}


#' @noRd
.compute_block_summary <- function(tokens, block_by, id_col) {
  # Count distinct records per block (de-duplicate on id within each block)
  unique_ids <- unique(tokens[, c(id_col, block_by), with = FALSE])

  if (length(block_by) == 1L) {
    unique_ids[, block_key := as.character(unique_ids[[block_by]])]
  } else {
    # Multi-column block: paste columns together
    cols_to_paste <- lapply(block_by, function(b) as.character(unique_ids[[b]]))
    unique_ids[, block_key := do.call(paste, c(cols_to_paste, list(sep = ", ")))]
  }

  block_counts <- unique_ids[, .(n_records = .N), by = "block_key"]
  total_n      <- sum(block_counts$n_records)
  block_counts[, pct_records := n_records / total_n]
  data.table::setorder(block_counts, -n_records)
  block_counts[, n_records := as.integer(n_records)]

  n_blocks   <- nrow(block_counts)
  top1_share <- if (n_blocks > 0L) block_counts$pct_records[1L] else NA_real_
  min_size   <- if (n_blocks > 0L) min(block_counts$n_records)  else NA_integer_
  max_size   <- if (n_blocks > 0L) max(block_counts$n_records)  else NA_integer_
  med_size   <- if (n_blocks > 0L) stats::median(block_counts$n_records) else NA_real_

  list(
    distribution = block_counts[, .(block_key, n_records, pct_records)],
    summary      = list(
      n_blocks    = as.integer(n_blocks),
      top1_share  = as.numeric(top1_share),
      min_size    = as.integer(min_size),
      median_size = as.numeric(med_size),
      max_size    = as.integer(max_size)
    )
  )
}


#' @noRd
.compute_vocab_overlap <- function(base_tokens, target_tokens) {
  cols <- unique(base_tokens$src_column)
  stats::setNames(
    vapply(cols, function(col) {
      base_vocab   <- unique(base_tokens[src_column == col, token])
      target_vocab <- unique(target_tokens[src_column == col, token])
      if (length(base_vocab) == 0L) return(NA_real_)
      mean(base_vocab %in% target_vocab)
    }, numeric(1L)),
    cols
  )
}


# ---------------------------------------------------------------------------
# data.table method (reference implementation)
# ---------------------------------------------------------------------------

method(
  audit_strategy,
  list(DT_tbl, class_character, Search_Strategy)
) <- function(data, id, strategy, target = NULL, sample_n = NULL, ...) {

  dt <- data.table::copy(data.table::as.data.table(data))

  # --- 1. Optional sampling --------------------------------------------------
  if (!is.null(sample_n)) {
    sample_n <- as.integer(sample_n)
    if (sample_n < nrow(dt)) {
      dt <- dt[sample.int(nrow(dt), sample_n)]
    }
  }

  n_records <- nrow(dt)

  # --- 2. NA rates (pre-tokenisation) ----------------------------------------
  na_rates <- .compute_na_rates(dt, strategy)

  # --- 3. Prepare token table ------------------------------------------------
  tokens <- prepare_search_data(dt, id, strategy)

  # --- 4. Compute rarity -----------------------------------------------------
  tokens <- compute_rarity(tokens, strategy)

  # --- 5. Column token stats -------------------------------------------------
  col_tok_stats <- .compute_column_token_stats(tokens, na_rates, n_records)

  # --- 6. Column rarity stats ------------------------------------------------
  col_rar_stats <- .compute_column_rarity_stats(tokens)

  # --- 7. Block summary (if block_by set) ------------------------------------
  block_by    <- strategy@block_by
  block_summ  <- NULL
  est_comp    <- as.numeric(n_records) * (n_records - 1L) / 2

  if (!is.null(block_by)) {
    block_summ  <- .compute_block_summary(tokens, block_by, id)
    bc          <- block_summ$distribution$n_records
    est_comp    <- sum(as.numeric(bc) * (bc - 1L) / 2)
  }

  # --- 8. Vocab overlap (if target supplied) ---------------------------------
  vocab_overlap <- NULL
  if (!is.null(target)) {
    tgt_dt     <- data.table::as.data.table(target)
    tgt_tokens <- prepare_search_data(tgt_dt, id, strategy)
    tgt_tokens <- compute_rarity(tgt_tokens, strategy)
    vocab_overlap <- .compute_vocab_overlap(tokens, tgt_tokens)
  }

  # --- 9. Signals and recommendations ----------------------------------------
  signals <- list()

  if (!is.null(block_summ)) {
    signals[["block_top_share"]] <- block_summ$summary$top1_share
  }

  if (nrow(col_rar_stats) > 0L) {
    worst_idx <- which.max(col_rar_stats$pct_low_rarity)
    signals[["max_pct_low_rarity_tokens"]] <- col_rar_stats$pct_low_rarity[worst_idx]
    signals[["worst_rarity_column"]]       <- col_rar_stats$column[worst_idx]
  }

  recs <- .dispatch_recommendations(signals)

  # --- 10. Assemble result ---------------------------------------------------
  out <- Strategy_Audit(
    n_records           = as.integer(n_records),
    block_summary       = block_summ,
    column_token_stats  = col_tok_stats,
    column_rarity_stats = col_rar_stats,
    est_comparisons     = as.numeric(est_comp),
    recommendations     = recs$messages
  )
  attr(out, "recommendation_ids") <- recs$ids
  attr(out, "vocab_overlap")      <- vocab_overlap
  out
}


# ---------------------------------------------------------------------------
# DuckDB method: sample to R, delegate to data.table
# ---------------------------------------------------------------------------

method(
  audit_strategy,
  list(Duck_tbl, class_character, Search_Strategy)
) <- function(data, id, strategy, target = NULL, sample_n = NULL, ...) {

  con      <- data$src$con
  tbl_name <- data$lazy_query$x

  # Determine how many rows to pull
  if (is.null(sample_n)) {
    n_total  <- DBI::dbGetQuery(
      con, paste0("SELECT COUNT(*) AS n FROM \"", tbl_name, "\"")
    )$n
    sample_n <- as.integer(n_total)
  } else {
    sample_n <- as.integer(sample_n)
  }

  # Pull sample to R as data.table
  dt_sample <- data.table::as.data.table(
    DBI::dbGetQuery(
      con,
      paste0(
        "SELECT * FROM \"", tbl_name, "\" USING SAMPLE ",
        sample_n, " ROWS"
      )
    )
  )

  # Handle target: pull sample if it is also a DuckDB table
  target_dt <- if (!is.null(target)) {
    if (inherits(target, "tbl_duckdb_connection")) {
      t_con      <- target$src$con
      t_tbl_name <- target$lazy_query$x
      t_n <- DBI::dbGetQuery(
        t_con, paste0("SELECT COUNT(*) AS n FROM \"", t_tbl_name, "\"")
      )$n
      data.table::as.data.table(
        DBI::dbGetQuery(
          t_con,
          paste0("SELECT * FROM \"", t_tbl_name, "\" USING SAMPLE ",
                 min(sample_n, t_n), " ROWS")
        )
      )
    } else {
      data.table::as.data.table(target)
    }
  } else {
    NULL
  }

  # Delegate to data.table method (sampling already done)
  audit_strategy(dt_sample, id, strategy,
                 target = target_dt, sample_n = NULL, ...)
}


# ---------------------------------------------------------------------------
# Tibble / data.frame thin wrappers
# ---------------------------------------------------------------------------

method(
  audit_strategy,
  list(.jyDF, class_character, Search_Strategy)
) <- function(data, id, strategy, target = NULL, sample_n = NULL, ...) {
  target_dt <- if (!is.null(target)) as_DT(target) else NULL
  audit_strategy(as_DT(data), id, strategy,
                 target = target_dt, sample_n = sample_n, ...)
}

method(
  audit_strategy,
  list(.jyTBL_DF, class_character, Search_Strategy)
) <- function(data, id, strategy, target = NULL, sample_n = NULL, ...) {
  target_dt <- if (!is.null(target)) as_DT(target) else NULL
  audit_strategy(as_DT(data), id, strategy,
                 target = target_dt, sample_n = sample_n, ...)
}

method(
  audit_strategy,
  list(.jyTBL, class_character, Search_Strategy)
) <- function(data, id, strategy, target = NULL, sample_n = NULL, ...) {
  target_dt <- if (!is.null(target)) as_DT(target) else NULL
  audit_strategy(as_DT(data), id, strategy,
                 target = target_dt, sample_n = sample_n, ...)
}
