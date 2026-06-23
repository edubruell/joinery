
# Suppress R CMD check NOTEs for data.table NSE variables used with :=
utils::globalVariables(c(
  # Token table / prepare_search_data
  "src_column", "token", "row_id",
  # Rarity / scoring internals
  "freq", "df", "N", "rarity", "weight", "rIP",
  # methods_*_prepare.R (global rarity scope)
  "freq_global", "df_global", "N_global",
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
  # internal_chunking.R
  "oversized",
  # internal_fanout.R (token-overlap fan-out guard)
  "mass", "df_b",
  # strategy_blocking.R (token-blocking explosion)
  "._btok", "._bdf", "._brar", "._row",
  # exact_methods_datatable.R / exact containment
  "n_match", "n_base", "n_target", "n_base_c", "n_target_c",
  "rmass", "lhs_id", "._bid", "._tid",
  # exact_methods_datatable.R (set-equality fingerprint links)
  "._id", "._rep",
  # internal_staging.R (staged resolve / ledger)
  "from", "to", "id_a", "id_b", "covered_sources", "n_in_entity",
  "within_source", "source_from", "source_to", ".mid",
  # plan_strategy.R (blocking-frontier / cost-curve / discriminativeness math)
  "bin", "block", "brute_pairs", "cand_idx", "df_max", "df_t",
  "exact_twin_survival", "hi", "lo", "mid", "nb", "nt", "p", "rarity_t",
  "so", "w", "z_pair",
  # purrr-shim dot
  "."
))

# Column names the package creates internally during matching.
# Users must not name their ID column or data columns with any of these.
.JOINERY_RESERVED_COLS <- c(
  "src_column", "token", "row_id",
  "freq", "df", "N", "rarity", "weight", "rIP",
  "freq_global", "df_global", "N_global",
  "raw_score", "matched_rip", "total_rip", "overlap_share",
  "feedback_factor", "side", "uid",
  "score", "rank", "duplicate_group", "match_id", "source",
  "stage", "contribution",
  # strategy_blocking.R (token-blocking derived block column)
  "._btok"
)

# Materialise a DuckDB lazy input into a temp table if it is not
# already backed by a single named table.
#
# joinery's DuckDB backend frequently reads `data$lazy_query$x` to
# emit SQL like `SELECT ... FROM <name>`. That field is a length-1
# character only when `data` is a bare `tbl(con, "name")`; filtered
# inputs (`tbl(con, "name") |> filter(...)`) carry a nested lazy
# query there instead, which breaks the string interpolation.
#
# Returns a DuckDB tbl whose `$lazy_query$x` is guaranteed to be a
# length-1 character. The temp table (when created) lives for the
# lifetime of the DBI connection.
.materialise_duck_input <- function(data, con = NULL) {
  con <- con %||% data$src$con
  x <- data$lazy_query$x
  if (is.character(x) && length(x) == 1L) {
    return(data)
  }
  tmp_in <- paste0("_joinery_input_", sample.int(1e9, 1))
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE ", tmp_in, " AS ",
    dbplyr::sql_render(data)
  ))
  cli::cli_inform(c(
    i = "Materialised filtered DuckDB input as temp table {.field {tmp_in}}."
  ))
  dplyr::tbl(con, tmp_in)
}

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


# ---------------------------------------------------------------------------
# Pre-flight checks surfaced once, at the top of a dedup/search run (D1 / D2)
# ---------------------------------------------------------------------------

# D2: warn once that `id` is non-unique. Rows sharing an id are pooled into one
# record (tokens merged), which is usually a data bug the caller doesn't know
# about. Surfaced once at the run entry point (and suppressed in the DuckDB
# per-batch tokenizer, which would otherwise warn once per batch on a partial
# view of the ids). `n_dup_ids` is the count of *duplicate* id values.
.warn_nonunique_id <- function(n_dup_ids, id_col) {
  if (is.na(n_dup_ids) || n_dup_ids <= 0L) return(invisible(FALSE))
  cli::cli_warn(c(
    "!" = "{.arg id} column {.field {id_col}} is not unique: \\
           {n_dup_ids} duplicate value{?s}.",
    "i" = "Rows sharing an id are treated as one record (tokens pooled). \\
           De-duplicate the input or supply a unique id if that is not intended."
  ))
  invisible(TRUE)
}

