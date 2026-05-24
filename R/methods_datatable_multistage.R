# ============================================================
# data.table backend — residual extraction & multi-stage matching
# ============================================================
#
# `extract_unmatched()` and `multi_stage_match()` methods for
# in-memory data.table inputs.
#
# ============================================================


# Method: extract_unmatched
#------------------------------------------------------------------------------
method(
  extract_unmatched,
  list(DT_tbl, class_character, DT_tbl)
) <- function(data, id, matches) {
  dt <- data.table::copy(data)

  if (!id %in% names(dt)) {
    cli::cli_abort("ID column {.field {id}} not found in data")
  }

  if (!"id" %in% names(matches)) {
    cli::cli_abort("{.arg matches} must contain a column named {.field id}")
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
