# ============================================================
# Stopword discovery — the "find" half of the find → reprepare loop
# ============================================================
#
# `filter_stopwords()` (in preparer_tokens.R) is the in-chain step that
# *removes* known stopwords. It cannot decide *which* tokens are stopwords,
# because a preparer is a per-record function with no view of the corpus.
# "Which tokens are too common to discriminate" is inherently a corpus-global
# question — document frequency across all records.
#
# `find_stopwords()` answers it. Run it once on the output of
# `prepare_search_data()`, inspect the high-frequency tokens it returns, then
# drop the offenders into a `filter_stopwords(<those>)` step and re-prepare:
#
#   tok <- prepare_search_data(data, "id", strategy)
#   sw  <- find_stopwords(tok, max_prop = 0.3)
#   strategy2 <- search_strategy(
#     street ~ normalize_text + word_tokens(min_nchar = 1) +
#              filter_stopwords(sw[src_column == "street", token]),
#     ...
#   )
#
# Beyond noise reduction this is the practical cure for the dense-block join
# blowup: ultra-common short tokens ("1", "am", "an", bare house numbers from a
# low `min_nchar`) appear in a large fraction of records, so the token-overlap
# self-join over a dense block becomes near-cartesian. Filtering them collapses
# the join back to a tractable size. See notes/v08_side_learnings_and_vibes.md.
# ============================================================


#' Discover candidate stopwords from a prepared token table
#'
#' Scores every `(src_column, token)` by its document frequency — the share of
#' records in that column whose value contains the token — and returns the
#' tokens common enough to be poor discriminators. These are stopword
#' candidates: feed them to [`filter_stopwords()`] in the preparer chain and
#' re-run `prepare_search_data()`.
#'
#' Document frequency is computed corpus-wide by default (`by_block = FALSE`),
#' i.e. across all blocks. This matches the intuition of a stopword as a
#' globally common term. With `by_block = TRUE` the share is computed within
#' each block and a token is returned if it crosses `max_prop` in *any* block,
#' reported at its maximum block-level share — useful when a token is rare
#' overall but saturates a single dense block.
#'
#' @param tokens A token table produced by [`prepare_search_data()`]
#'   (data.table or DuckDB backend). Must contain `src_column`, `token`, and
#'   `row_id`.
#' @param max_prop Numeric in `(0, 1]`. Return tokens whose document-frequency
#'   share is at least this value. Default `0.3` (token appears in ≥30% of a
#'   column's records).
#' @param top_n Optional integer. If supplied, instead of (or in addition to)
#'   the `max_prop` cut, keep at most the `top_n` most frequent tokens per
#'   column. When both are given, the union is returned.
#' @param by_block Logical. Compute the share within each block rather than
#'   corpus-wide. Requires `block_by` to name the block columns (the token
#'   table also carries the id column, so they cannot be inferred safely).
#'   Default `FALSE`.
#' @param block_by Character vector of block columns. Required when
#'   `by_block = TRUE`; pass the strategy's `block_by`. Ignored otherwise.
#'
#' @return A `data.table` with one row per flagged `(src_column, token)`:
#'   `src_column`, `token`, `df` (distinct records containing the token),
#'   `n_records` (records in the column / block), and `prop = df / n_records`.
#'   Sorted by `src_column` then descending `prop`. Empty (zero-row) when
#'   nothing crosses the threshold.
#'
#' @seealso [`filter_stopwords()`] to apply the result in a preparer chain.
#' @export
find_stopwords <- new_generic(
  "find_stopwords", "tokens",
  function(tokens, max_prop = 0.3, top_n = NULL, by_block = FALSE,
           block_by = NULL) {
    S7_dispatch()
  }
)


.find_stopwords_validate <- function(max_prop, top_n) {
  if (!is.numeric(max_prop) || length(max_prop) != 1L ||
      is.na(max_prop) || max_prop <= 0 || max_prop > 1) {
    cli::cli_abort("{.arg max_prop} must be a single number in {.code (0, 1]}.")
  }
  if (!is.null(top_n)) {
    if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) || top_n < 1) {
      cli::cli_abort("{.arg top_n} must be a single positive integer or {.code NULL}.")
    }
  }
  invisible(NULL)
}

# Reserved non-block columns on a token table; anything else is block_by / id.
.token_table_reserved <- c("src_column", "token", "row_id")

# Validate the block_by columns supplied for by_block = TRUE. They must be named
# explicitly (the id column is also present on the token table, so a "guess all
# non-reserved columns" default would silently treat id as a block, putting each
# record in its own block) and must exist on the token table.
.find_stopwords_block_by <- function(block_by, token_cols) {
  if (is.null(block_by) || !length(block_by)) {
    cli::cli_abort(c(
      "{.arg block_by} is required when {.code by_block = TRUE}.",
      "i" = "Pass the strategy's {.code block_by} (e.g. {.code c(\"plz2\", \"wz08_3\")})."
    ))
  }
  if (!is.character(block_by)) {
    cli::cli_abort("{.arg block_by} must be a character vector.")
  }
  missing <- setdiff(block_by, token_cols)
  if (length(missing)) {
    cli::cli_abort("Block column{?s} not found on {.arg tokens}: {.field {missing}}.")
  }
  block_by
}


# Method: find_stopwords for a data.table token table
#------------------------------------------------------------------------------
method(find_stopwords, new_S3_class("data.table")) <- function(
    tokens, max_prop = 0.3, top_n = NULL, by_block = FALSE, block_by = NULL) {

  .find_stopwords_validate(max_prop, top_n)
  required <- c("src_column", "token", "row_id")
  missing  <- setdiff(required, names(tokens))
  if (length(missing)) {
    cli::cli_abort(c(
      "{.arg tokens} is not a prepared token table.",
      "x" = "Missing column{?s}: {.field {missing}}.",
      "i" = "Pass the output of {.fn prepare_search_data}."
    ))
  }

  dt <- data.table::copy(tokens)

  grp <- "src_column"
  if (by_block) {
    block_by <- .find_stopwords_block_by(block_by, names(dt))
    grp <- c("src_column", block_by)
  }

  # df = distinct records containing the token; n = distinct records in the
  # group; prop = share. Computed as aggregates (no window), so this stays
  # cheap even on the dense blocks that make the scoring join explode.
  df  <- dt[, .(df = data.table::uniqueN(row_id)), by = c(grp, "token")]
  nn  <- dt[, .(n_records = data.table::uniqueN(row_id)), by = grp]
  out <- df[nn, on = grp]
  out[, prop := df / n_records]

  keep <- out$prop >= max_prop
  if (!is.null(top_n)) {
    out[, .rank := data.table::frank(-prop, ties.method = "first"), by = grp]
    keep <- keep | out$.rank <= as.integer(top_n)
    out[, .rank := NULL]
  }
  out <- out[keep]

  if (by_block) {
    # Collapse to one row per (src_column, token) at its worst block.
    data.table::setorder(out, src_column, token, -prop)
    out <- unique(out, by = c("src_column", "token"))
    out <- out[, .(src_column, token, df, n_records, prop)]
  } else {
    out <- out[, .(src_column, token, df, n_records, prop)]
  }
  data.table::setorder(out, src_column, -prop)
  out[]
}


# Method: find_stopwords for a DuckDB token table
#------------------------------------------------------------------------------
# Registered unconditionally so S7::methods_register() picks it up on a real
# install. A conditional registration only survives under load_all, which is
# why an installed package failed to dispatch on DuckDB tables. The body uses
# duckdb / DBI / dplyr, but any tbl_duckdb_connection input already implies
# those Suggests packages are loaded.
method(find_stopwords, new_S3_class("tbl_duckdb_connection")) <- function(
    tokens, max_prop = 0.3, top_n = NULL, by_block = FALSE, block_by = NULL) {

  .find_stopwords_validate(max_prop, top_n)
  con   <- tokens$src$con
  table <- tokens$lazy_query$x
  cols  <- dplyr::tbl_vars(tokens)
  required <- c("src_column", "token", "row_id")
  missing  <- setdiff(required, cols)
  if (length(missing)) {
    cli::cli_abort(c(
      "{.arg tokens} is not a prepared token table.",
      "x" = "Missing column{?s}: {.field {missing}}.",
      "i" = "Pass the output of {.fn prepare_search_data}."
    ))
  }

  grp_cols <- "src_column"
  if (by_block) {
    block_by <- .find_stopwords_block_by(block_by, cols)
    grp_cols <- c("src_column", block_by)
  }
  grp_sql <- paste(grp_cols, collapse = ", ")

  # Aggregate df / n via GROUP BY (not windows): one pass each, join on the
  # group key. QUALIFY handles the optional top_n. Filtering happens in SQL so
  # only the flagged rows cross back into R.
  top_clause <- if (!is.null(top_n)) {
    paste0(
      "\n  QUALIFY ROW_NUMBER() OVER (PARTITION BY ", grp_sql,
      " ORDER BY prop DESC) <= ", as.integer(top_n),
      " OR prop >= ", max_prop
    )
  } else {
    paste0("\n  WHERE prop >= ", max_prop)
  }

  sql <- paste0(
    "WITH _df AS (\n",
    "  SELECT ", grp_sql, ", token, COUNT(DISTINCT row_id) AS df\n",
    "  FROM ", table, " GROUP BY ", grp_sql, ", token\n",
    "),\n",
    "_n AS (\n",
    "  SELECT ", grp_sql, ", COUNT(DISTINCT row_id) AS n_records\n",
    "  FROM ", table, " GROUP BY ", grp_sql, "\n",
    ")\n",
    "SELECT _df.src_column, _df.token, _df.df, _n.n_records,\n",
    "       _df.df::DOUBLE / _n.n_records AS prop\n",
    "FROM _df JOIN _n USING (", grp_sql, ")",
    top_clause,
    "\n  ORDER BY _df.src_column, prop DESC"
  )

  out <- data.table::as.data.table(DBI::dbGetQuery(con, sql))

  if (by_block && nrow(out)) {
    data.table::setorder(out, src_column, token, -prop)
    out <- unique(out, by = c("src_column", "token"))
    out <- out[, .(src_column, token, df, n_records, prop)]
    data.table::setorder(out, src_column, -prop)
  }
  out[]
}
