# ============================================================
# data.frame / tibble wrappers for joinery (DT backend)
# ============================================================
#
# This file provides S7 method implementations that enable joinery to operate
# on base data.frames and tibbles while delegating all heavy lifting to the
# data.table backend.
#
# The workflow is:
#
#   - Convert incoming data.frame / tibble objects to data.table
#       via as_DT()
#
#   - Call the DT_tbl method implementation
#       (prepare_search_data, compute_rarity, detect_duplicates,
#        search_candidates, deduplicate_table, extract_unmatched,
#        multi_stage_search, .inspect_tokens)
#
#   - Convert results back to the original container type
#       using back_to_original()
#
# No IR interpretation occurs here; this file only provides thin compatibility
# layers so that users can pass data.frame or tibble inputs without having
# data.table as their primary data structure.
#
# All matching logic, tokenization, rarity computation, and scoring are
# performed in the DT backend implemented in methods_datatable.R.
#
# ============================================================

.jyDF      <- S7::new_S3_class("data.frame")
.jyTBL_DF  <- S7::new_S3_class("tbl_df")  # tibble
.jyTBL     <- S7::new_S3_class("tbl")     # tibble parent


#' @noRd
as_DT <- function(x) data.table::as.data.table(x)

#' @noRd
back_to_original <- function(result, template) {
  
  cls <- class(template)
  
  # tibble -> tibble if available, else faux tibble
  if ("tbl_df" %in% cls || "tbl" %in% cls) {
    if (requireNamespace("tibble", quietly = TRUE)) {
      return(tibble::as_tibble(result))
    } else {
      out <- as.data.frame(result)
      class(out) <- c("tbl_df", "tbl", "data.frame")
      return(out)
    }
  }
  
  # data.frame -> data.frame
  if ("data.frame" %in% cls) {
    return(as.data.frame(result))
  }
  
  # default: return as-is
  result
}

################################################################################
# prepare_search_data
################################################################################

method(
  prepare_search_data,
  list(.jyDF, class_character, Search_Strategy)
) <- function(data, id, strategy) {
  out <- prepare_search_data(as_DT(data), id, strategy)
  back_to_original(out, data)
}

method(
  prepare_search_data,
  list(.jyTBL_DF, class_character, Search_Strategy)
) <- function(data, id, strategy) {
  out <- prepare_search_data(as_DT(data), id, strategy)
  back_to_original(out, data)
}

method(
  prepare_search_data,
  list(.jyTBL, class_character, Search_Strategy)
) <- function(data, id, strategy) {
  out <- prepare_search_data(as_DT(data), id, strategy)
  back_to_original(out, data)
}

################################################################################
# compute_rarity
################################################################################

method(
  compute_rarity,
  list(.jyDF, Search_Strategy)
) <- function(tokens, strategy) {
  out <- compute_rarity(as_DT(tokens), strategy)
  back_to_original(out, tokens)
}

method(
  compute_rarity,
  list(.jyTBL_DF, Search_Strategy)
) <- function(tokens, strategy) {
  out <- compute_rarity(as_DT(tokens), strategy)
  back_to_original(out, tokens)
}

method(
  compute_rarity,
  list(.jyTBL, Search_Strategy)
) <- function(tokens, strategy) {
  out <- compute_rarity(as_DT(tokens), strategy)
  back_to_original(out, tokens)
}

################################################################################
# detect_duplicates
################################################################################

method(
  detect_duplicates,
  list(.jyDF, class_character, Search_Strategy)
) <- function(base_table, id, strategy) {
  out <- detect_duplicates(as_DT(base_table), id, strategy)
  back_to_original(out, base_table)
}

method(
  detect_duplicates,
  list(.jyTBL_DF, class_character, Search_Strategy)
) <- function(base_table, id, strategy) {
  out <- detect_duplicates(as_DT(base_table), id, strategy)
  back_to_original(out, base_table)
}

method(
  detect_duplicates,
  list(.jyTBL, class_character, Search_Strategy)
) <- function(base_table, id, strategy) {
  out <- detect_duplicates(as_DT(base_table), id, strategy)
  back_to_original(out, base_table)
}

# Exact_Strategy (the exact, score-1.0 dedup face). The DT methods take `...`
# (block/containment guards live on the strategy, not the call), so forward it.
method(
  detect_duplicates,
  list(.jyDF, class_character, Exact_Strategy)
) <- function(base_table, id, strategy, ...) {
  out <- detect_duplicates(as_DT(base_table), id, strategy, ...)
  back_to_original(out, base_table)
}

method(
  detect_duplicates,
  list(.jyTBL_DF, class_character, Exact_Strategy)
) <- function(base_table, id, strategy, ...) {
  out <- detect_duplicates(as_DT(base_table), id, strategy, ...)
  back_to_original(out, base_table)
}

method(
  detect_duplicates,
  list(.jyTBL, class_character, Exact_Strategy)
) <- function(base_table, id, strategy, ...) {
  out <- detect_duplicates(as_DT(base_table), id, strategy, ...)
  back_to_original(out, base_table)
}

################################################################################
# search_candidates
################################################################################

method(
  search_candidates,
  list(.jyDF, .jyDF, class_character, class_character, Search_Strategy)
) <- function(base_table, target_table, base_id, target_id, strategy, weights = NULL) {
  out <- search_candidates(as_DT(base_table), as_DT(target_table), base_id, target_id, strategy, weights)
  back_to_original(out, base_table)
}

method(
  search_candidates,
  list(.jyTBL_DF, .jyTBL_DF, class_character, class_character, Search_Strategy)
) <- function(base_table, target_table, base_id, target_id, strategy, weights = NULL) {
  out <- search_candidates(as_DT(base_table), as_DT(target_table), base_id, target_id, strategy, weights)
  back_to_original(out, base_table)
}

method(
  search_candidates,
  list(.jyTBL, .jyTBL, class_character, class_character, Search_Strategy)
) <- function(base_table, target_table, base_id, target_id, strategy, weights = NULL) {
  out <- search_candidates(as_DT(base_table), as_DT(target_table), base_id, target_id, strategy, weights)
  back_to_original(out, base_table)
}

# Exact_Strategy (the exact, score-1.0 search face). The DT methods take `...`,
# not weights; forward it.
method(
  search_candidates,
  list(.jyDF, .jyDF, class_character, class_character, Exact_Strategy)
) <- function(base_table, target_table, base_id, target_id, strategy, ...) {
  out <- search_candidates(as_DT(base_table), as_DT(target_table), base_id, target_id, strategy, ...)
  back_to_original(out, base_table)
}

method(
  search_candidates,
  list(.jyTBL_DF, .jyTBL_DF, class_character, class_character, Exact_Strategy)
) <- function(base_table, target_table, base_id, target_id, strategy, ...) {
  out <- search_candidates(as_DT(base_table), as_DT(target_table), base_id, target_id, strategy, ...)
  back_to_original(out, base_table)
}

method(
  search_candidates,
  list(.jyTBL, .jyTBL, class_character, class_character, Exact_Strategy)
) <- function(base_table, target_table, base_id, target_id, strategy, ...) {
  out <- search_candidates(as_DT(base_table), as_DT(target_table), base_id, target_id, strategy, ...)
  back_to_original(out, base_table)
}

################################################################################
# deduplicate_table
################################################################################

method(
  deduplicate_table,
  list(.jyDF, .jyDF, class_character)
) <- function(base_table, duplicates, id) {
  out <- deduplicate_table(as_DT(base_table), as_DT(duplicates), id)
  back_to_original(out, base_table)
}

method(
  deduplicate_table,
  list(.jyTBL_DF, .jyTBL_DF, class_character)
) <- function(base_table, duplicates, id) {
  out <- deduplicate_table(as_DT(base_table), as_DT(duplicates), id)
  back_to_original(out, base_table)
}

method(
  deduplicate_table,
  list(.jyTBL, .jyTBL, class_character)
) <- function(base_table, duplicates, id) {
  out <- deduplicate_table(as_DT(base_table), as_DT(duplicates), id)
  back_to_original(out, base_table)
}

################################################################################
# extract_unmatched
################################################################################

method(
  extract_unmatched,
  list(.jyDF, class_character, .jyDF)
) <- function(data, id, matches) {
  out <- extract_unmatched(as_DT(data), id, as_DT(matches))
  back_to_original(out, data)
}

method(
  extract_unmatched,
  list(.jyTBL_DF, class_character, .jyTBL_DF)
) <- function(data, id, matches) {
  out <- extract_unmatched(as_DT(data), id, as_DT(matches))
  back_to_original(out, data)
}

method(
  extract_unmatched,
  list(.jyTBL, class_character, .jyTBL)
) <- function(data, id, matches) {
  out <- extract_unmatched(as_DT(data), id, as_DT(matches))
  back_to_original(out, data)
}

################################################################################
# materialize_records
################################################################################
#
# Rehydrate-by-id semi-join; the complement of extract_unmatched(). `ids` may be
# a character vector or a matches table, neither a dispatch arg, so forward as-is.

method(
  materialize_records,
  list(.jyDF, class_character)
) <- function(data, id, ids, ...) {
  out <- materialize_records(as_DT(data), id, ids, ...)
  back_to_original(out, data)
}

method(
  materialize_records,
  list(.jyTBL_DF, class_character)
) <- function(data, id, ids, ...) {
  out <- materialize_records(as_DT(data), id, ids, ...)
  back_to_original(out, data)
}

method(
  materialize_records,
  list(.jyTBL, class_character)
) <- function(data, id, ids, ...) {
  out <- materialize_records(as_DT(data), id, ids, ...)
  back_to_original(out, data)
}

################################################################################
# multi_stage_search
################################################################################

# Re-attach the directed ledger that the entity-grouping return carries as an
# attribute (as_tibble / as.data.frame drop custom attributes).
.ms_search_back <- function(out, template) {
  ledger <- attr(out, "ledger", exact = TRUE)
  res <- back_to_original(out, template)
  if (!is.null(ledger)) attr(res, "ledger") <- ledger
  res
}

method(
  multi_stage_search,
  list(.jyDF, .jyDF, class_character, class_character, class_list)
) <- function(base_table, target_table, base_id, target_id, strategies, ...) {
  out <- multi_stage_search(as_DT(base_table), as_DT(target_table), base_id, target_id, strategies, ...)
  .ms_search_back(out, base_table)
}

method(
  multi_stage_search,
  list(.jyTBL_DF, .jyTBL_DF, class_character, class_character, class_list)
) <- function(base_table, target_table, base_id, target_id, strategies, ...) {
  out <- multi_stage_search(as_DT(base_table), as_DT(target_table), base_id, target_id, strategies, ...)
  .ms_search_back(out, base_table)
}

method(
  multi_stage_search,
  list(.jyTBL, .jyTBL, class_character, class_character, class_list)
) <- function(base_table, target_table, base_id, target_id, strategies, ...) {
  out <- multi_stage_search(as_DT(base_table), as_DT(target_table), base_id, target_id, strategies, ...)
  .ms_search_back(out, base_table)
}

################################################################################
# multi_stage_dedup
################################################################################

method(
  multi_stage_dedup,
  list(.jyDF, class_character, class_list)
) <- function(table, id, strategies, ...) {
  out <- multi_stage_dedup(as_DT(table), id, strategies, ...)
  back_to_original(out, table)
}

method(
  multi_stage_dedup,
  list(.jyTBL_DF, class_character, class_list)
) <- function(table, id, strategies, ...) {
  out <- multi_stage_dedup(as_DT(table), id, strategies, ...)
  back_to_original(out, table)
}

method(
  multi_stage_dedup,
  list(.jyTBL, class_character, class_list)
) <- function(table, id, strategies, ...) {
  out <- multi_stage_dedup(as_DT(table), id, strategies, ...)
  back_to_original(out, table)
}

################################################################################
# .inspect_tokens
################################################################################

method(
  .inspect_tokens,
  list(.jyDF, class_character, Search_Strategy, class_character)
) <- function(data, id, strategy, column) {
  out <- .inspect_tokens(as_DT(data), id, strategy, column)
  back_to_original(out, data)
}

method(
  .inspect_tokens,
  list(.jyTBL_DF, class_character, Search_Strategy, class_character)
) <- function(data, id, strategy, column) {
  out <- .inspect_tokens(as_DT(data), id, strategy, column)
  back_to_original(out, data)
}

method(
  .inspect_tokens,
  list(.jyTBL, class_character, Search_Strategy, class_character)
) <- function(data, id, strategy, column) {
  out <- .inspect_tokens(as_DT(data), id, strategy, column)
  back_to_original(out, data)
}


