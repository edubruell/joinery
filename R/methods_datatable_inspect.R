# ============================================================
# data.table backend — token inspection
# ============================================================
#
# `.inspect_tokens()` method for in-memory data.table inputs.
# (The leading dot lets `inspect_tokens()` capture the column
# argument via `rlang::ensym()` without clashing with
# class_character dispatch.)
#
# ============================================================

method(
  .inspect_tokens,
  list(DT_tbl, class_character, Search_Strategy, class_character)
) <- function(data, id, strategy, column) {
  dt <- data.table::copy(data)
  # --- Validate inputs -----------------------------------------------------
  if (!id %in% names(dt)) {
    cli::cli_abort("ID column {.field {id}} not found in data")
  }
  if (!column %in% names(dt)) {
    cli::cli_abort("Column {.field {column}} not found in data")
  }
  if (!column %in% names(strategy@preparers)) {
    cli::cli_abort("Column {.field {column}} not found in strategy preparers")
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
