
# Suppress R CMD check NOTEs for data.table NSE variables used with :=
utils::globalVariables(c(
  # Token table / prepare_search_data
  "src_column", "token", "row_id",
  # Rarity / scoring internals
  "freq", "df", "N", "rarity", "weight", "rIP",
  "raw_score", "matched_rip", "total_rip", "overlap_share",
  "rhs_id", "side", "uid", "feedback_factor",
  # Output columns
  "score", "rank", "match_id", "duplicate_group", "source", "stage",
  "contribution",
  # audit_strategy.R
  "column", "n_tokens", "n_unique_tokens", "avg_tokens_per_record",
  "pct_unique", "n_records", "pct_records", "block_key", "na_rate",
  # sample_matches.R
  ".dist_to_thr", "n_candidates", "gap", "id",
  # compare_stages.R / summarise_matches.R
  "n_pairs_or_groups", "cluster_size",
  # batch_duckdb.R / DuckDB internals
  "batch_id", "blocks", "n", "..block_cols",
  "row_count", "row_start", "row_end", "min_rn", "max_rn", "block_size",
  # diagnostics_plots.R
  "bin_lower", "bin_upper", "bin_mid",
  "stage_idx", "base_pct_cumulative",
  "token_label", "overlap",
  # preparers.R
  "tokens", "len_text", "ngrams",
  # aip.R
  "occ", "maxocc", "occ_R", "maxocc_R", "occ_A", "maxocc_A", "aip",
  # match_features.R
  "_rid_", ".pair", ".pos", "..cols_pick",
  "row_id_placeholder", "aip_s", "aip_f",
  "n_cols", "max_aip", "cnt", "icnt", "ipos", "scnt", "rcnt",
  "searched", "found",
  "cosine_sim", "embedding_norm_s", "embedding_norm_f",
  # labelling.R
  "equal", ".block_default",
  # fit_filter.R / calibrate_matches.R
  "tp_prob", "predicted_tp", ".block_default",
  # purrr-shim dot
  "."
))

# Column names the package creates internally during matching.
# Users must not name their ID column or data columns with any of these.
.JOINERY_RESERVED_COLS <- c(
  "src_column", "token", "row_id",
  "freq", "df", "N", "rarity", "weight", "rIP",
  "raw_score", "matched_rip", "total_rip", "overlap_share",
  "feedback_factor", "side", "uid",
  "score", "rank", "duplicate_group", "match_id", "source",
  "stage", "contribution"
)

.check_reserved_names <- function(data_cols, id_col, call = rlang::caller_env()) {
  all_user_cols <- union(data_cols, id_col)
  bad <- intersect(all_user_cols, .JOINERY_RESERVED_COLS)
  if (length(bad) > 0L) {
    cli::cli_abort(
      paste0(
        "Column name(s) ", paste(bad, collapse = ", "),
        " conflict with joinery's internal column names. ",
        "Rename the conflicting column(s) before matching."
      ),
      call = call
    )
  }
  invisible(NULL)
}

#' Validate Input Conditions for R Functions
#'
#' This internal function validates specified conditions for function inputs and stops the function execution if any condition is not met. It uses a named vector of predicates where each name is the error message associated with the predicate condition.
#'
#' @param .predicates A named vector where each element is a logical condition and the name of each element is the corresponding error message to be displayed if the condition is FALSE.
#' @return None; the function will stop execution and throw an error if a validation fails.
#' @examples
#' validate_inputs(c(
#'   "Input must be numeric" = is.numeric(5),
#'   "Input must be integer" = 5 == as.integer(5)
#' ))
#' @noRd
validate_inputs <- function(.predicates) {
  # Use lapply to iterate over predicates and stop on the first failure
  results <- lapply(names(.predicates), function(error_msg) {
    if (!.predicates[[error_msg]]) {
      stop(error_msg)
    }
  })
}


#' Initialize a CLI progress indicator
#'
#' Creates either a determinate progress bar (when `total` is known) or an
#' indeterminate spinner (when `total` is NULL). This keeps the UX predictable
#' for pipelines where total work cannot be precomputed.
#'
#' @param total Optional integer. If NULL or NA, a spinner is created.
#' @param .envir Environment for cli bookkeeping.
#'
#' @return A list with `type` ("bar" or "spinner") and `id`.
#'
#' @noRd
progress_init <- function(total = NULL, .envir = parent.frame()) {
  if (!is.null(total) && is.finite(total)) {
    id <- cli::cli_progress_bar(
      total = total,
      clear = FALSE,
      .auto_close = FALSE,
      .envir = .envir
    )
    return(list(type = "bar", id = id))
  }
  
  id <- cli::cli_progress_bar(
    total = NA,
    clear = FALSE,
    .auto_close = FALSE,
    .envir = .envir
  )
  list(type = "spinner", id = id)
}


#' Update a CLI progress indicator
#'
#' @param pb Progress handle from `progress_init()`.
#' @param amount Integer increment. Used only for determinate bars.
#' @param .envir Environment for cli bookkeeping.
#'
#' @noRd
progress_update <- function(pb, amount = 1L, .envir = parent.frame()) {
  if (is.null(pb)) return(invisible(NULL))
  if (pb$type == "bar") {
    cli::cli_progress_update(id = pb$id, inc = amount, .envir = .envir)
  } else {
    cli::cli_progress_update(id = pb$id, .envir = .envir)
  }
}


#' Finalize a CLI progress indicator
#'
#' @param pb Progress handle.
#' @param .envir Environment for cli bookkeeping.
#'
#' @noRd
progress_finish <- function(pb, .envir = parent.frame()) {
  if (is.null(pb)) return(invisible(NULL))
  cli::cli_progress_done(id = pb$id, .envir = .envir)
}

