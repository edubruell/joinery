# ============================================================
# Token-blocking spec (block_on_tokens) for joinery
# ============================================================
#
# Defines the Block_On_Tokens S7 class - a blocking *spec* that can stand in
# place of a literal column name inside a strategy's `block_by`. Where a plain
# character entry blocks two records iff they share a literal column value, a
# Block_On_Tokens entry blocks two records iff they share any of a designated
# column's (rare) tokens. This is region-free: a mover that crosses a plz2
# boundary still co-blocks with its earlier self through its distinctive name
# token. See notes/region_free_linking.md section 4.
#
# The mechanism (prepare_search_data): each record's token rows are exploded
# against its set of surviving rare blocking-tokens, materialising a derived
# `._btok` block column. The existing overlap join on
# c("src_column", "token", ._btok) then matches records sharing a rare
# blocking-token - reusing the join / rarity / prefilter / fan-out machinery
# unchanged. `._btok` is just another block column downstream.
#
# This file holds the class, the block_on_tokens() constructor, the
# .is_token_block() predicate, and - critically - .block_cols(), the single
# resolver that maps a `block_by` spec to the *effective* block-column names
# the downstream join / rarity / fan-out code must use.
# ============================================================


# The materialised derived block column produced by the explosion. A reserved
# name (registered in .JOINERY_RESERVED_COLS); chosen with a leading "._" so it
# cannot collide with a user column.
.JOINERY_BTOK_COL <- "._btok"


# ---------------------------------------------------------------------------
# Block_On_Tokens class
# ---------------------------------------------------------------------------

#' Token-Blocking Spec Class
#'
#' @description
#' An S7 class describing a region-free blocking key: block on a designated
#' column's (rare) tokens rather than on a literal column value. Used inside a
#' strategy's `block_by`, mixed freely with plain character column names.
#'
#' @slot column Character scalar - the column whose tokens become block keys.
#' @slot max_df Numeric scalar - global document-frequency cap selecting which
#'   tokens are eligible block keys (common tokens dropped as keys).
#' @slot min_rarity Numeric scalar - global rarity floor on eligible block keys.
#' @slot preparer NULL or a `Search_Preparer` for the blocking column. NULL
#'   defers to the default pipeline (normalize_text + word_tokens) or the
#'   column's scored preparer when it is also a scored column.
#' @slot min_nchar Integer scalar - min token length for the default preparer.
#'
#' @seealso [block_on_tokens()]
#'
#' @noRd
Block_On_Tokens <- new_class(
  "Block_On_Tokens",
  properties = list(
    column     = class_character,
    max_df     = class_numeric,
    min_rarity = class_numeric,
    preparer   = class_any,
    min_nchar  = class_numeric
  ),
  validator = function(self) {
    if (length(self@column) != 1 || !nzchar(self@column)) {
      return("column must be a single non-empty string")
    }
    if (length(self@max_df) != 1 || self@max_df < 1) {
      return("max_df must be a scalar >= 1 (Inf allowed)")
    }
    if (length(self@min_rarity) != 1 || !is.finite(self@min_rarity) ||
        self@min_rarity < 0) {
      return("min_rarity must be a non-negative finite scalar")
    }
    if (!is.null(self@preparer) && !S7_inherits(self@preparer, Search_Preparer)) {
      return("preparer must be NULL or a Search_Preparer object")
    }
  }
)


# ---------------------------------------------------------------------------
# Predicate + resolver (the single source of truth for effective block columns)
# ---------------------------------------------------------------------------

#' Is `x` a token-blocking spec?
#' @noRd
.is_token_block <- function(x) S7_inherits(x, Block_On_Tokens)


#' Normalise a `block_by` value to a list of entries.
#'
#' `block_by` may be NULL, a character vector, a single `Block_On_Tokens`, or a
#' list mixing the two. This collapses all forms to a plain list (character
#' entries stay length-1 strings, token-block specs stay objects) so callers can
#' iterate uniformly. NULL / empty returns `list()`.
#' @noRd
.block_by_list <- function(block_by) {
  if (is.null(block_by) || length(block_by) == 0L) return(list())
  if (.is_token_block(block_by)) return(list(block_by))
  if (is.character(block_by)) return(as.list(block_by))
  # already a list (possibly mixed)
  as.list(block_by)
}


#' Extract the token-blocking specs from a strategy's `block_by`.
#' @noRd
.token_block_specs <- function(strategy) {
  Filter(.is_token_block, .block_by_list(strategy@block_by))
}


#' Resolve a strategy's `block_by` to the EFFECTIVE block-column names.
#'
#' This is the single source of truth that every downstream consumer of the
#' *token table* (compute_rarity, the overlap join, the fan-out guard, the
#' scorer, attribution) must call instead of reading `strategy@block_by`
#' directly. Each `Block_On_Tokens` spec resolves to the materialised `._btok`
#' column (added once even if several specs are present - they union into one
#' `._btok`); plain character entries pass through unchanged, in order.
#'
#' Note: this is the JOIN / RARITY / FAN-OUT axis. Entity resolution
#' (`resolve_entities`) must keep partitioning by the PLAIN block columns only
#' (see `.plain_block_cols()`); partitioning the connected-components recursion
#' by `._btok` would wrongly split a record that matched under two different
#' block-tokens into separate components.
#' @noRd
.block_cols <- function(strategy) {
  entries <- .block_by_list(strategy@block_by)
  if (length(entries) == 0L) return(character())
  plain <- character()
  has_tok <- FALSE
  for (e in entries) {
    if (.is_token_block(e)) has_tok <- TRUE
    else plain <- c(plain, e)
  }
  if (has_tok) c(plain, .JOINERY_BTOK_COL) else plain
}


#' The PLAIN (non-token) block columns of a strategy's `block_by`.
#'
#' The columns that exist on the raw input and that entity resolution should
#' partition by. Drops every `Block_On_Tokens` spec. Empty character when there
#' are no plain block columns.
#' @noRd
.plain_block_cols <- function(strategy) {
  entries <- .block_by_list(strategy@block_by)
  if (length(entries) == 0L) return(character())
  out <- character()
  for (e in entries) if (!.is_token_block(e)) out <- c(out, e)
  out
}


# ---------------------------------------------------------------------------
# Constructor
# ---------------------------------------------------------------------------

#' Block on a Column's Rare Tokens (region-free blocking)
#'
#' @description
#' Build a token-blocking key for use inside a strategy's `block_by`. Where a
#' plain column name blocks two records only when they share a literal value,
#' `block_on_tokens()` blocks them when they share **any** of a designated
#' column's (rare) tokens. This is **region-free**: a record that drifts across
#' a region boundary - a firm that moves to a new postcode, say - still
#' co-blocks with its earlier self through a distinctive name token, and so
#' becomes a candidate where a literal block would never compare them.
#'
#' Hand it to `block_by` in place of (or mixed with) a column name:
#'
#' ```r
#' # fully region-free - share a rare name token, regardless of place
#' search_strategy(name ~ normalize_text + word_tokens(min_nchar = 3),
#'                 block_by = block_on_tokens("name", max_df = 50))
#'
#' # region-bounded - share a rare name token AND sit in the same plz2
#' search_strategy(name ~ normalize_text + word_tokens(min_nchar = 3),
#'                 block_by = list(block_on_tokens("name", max_df = 50), "plz2"))
#' ```
#'
#' `max_df` and `min_rarity` select which tokens are eligible block keys, using
#' the **global** (corpus-wide) document frequency: a token appearing in more
#' than `max_df` records, or whose global rarity falls below `min_rarity`, is
#' dropped as a key. This is where "block on the distinctive words, not the
#' common ones" lives - a franchise name ("ALDI") is globally common, fails the
#' cap, and never becomes a block key, while a distinctive brand survives. A
#' record with no surviving block key is **unreachable via token-blocking in
#' this stage** (it contributes no token-block rows).
#'
#' Token-blocking is the densest operation in the package: every pair sharing a
#' surviving key is materialised. It is safe **only** behind a real `max_df` (or
#' `min_rarity`) plus the always-on fan-out guard. Passing neither cap is a
#' loud warning, not an error, but you almost always want one.
#'
#' @param column The column whose tokens become block keys (for example
#'   `"name"`).
#' @param max_df Numeric scalar. Global document-frequency cap: tokens appearing
#'   in more than `max_df` records corpus-wide are dropped as block keys.
#'   Default `Inf` (no cap - see the density warning above).
#' @param min_rarity Numeric scalar. Global rarity floor: tokens whose
#'   corpus-wide rarity falls below this are dropped as block keys. Default `0`.
#' @param preparer Optional preprocessing pipeline for the blocking column,
#'   given as a one-sided or two-sided formula like the `column ~ steps` you
#'   pass to [search_strategy()] (for example `~ normalize_text +
#'   word_tokens(min_nchar = 4)`). Default `NULL` reuses the column's own scored
#'   preparer when `column` is also a scored column, else falls back to
#'   `normalize_text + word_tokens(min_nchar = min_nchar)`.
#' @param min_nchar Integer scalar. Minimum token length for the default
#'   preparer. Default `3L`.
#'
#' @return A `Block_On_Tokens` spec, to be placed in `block_by`.
#'
#' @seealso [search_strategy()]
#'
#' @export
block_on_tokens <- function(column,
                            max_df     = Inf,
                            min_rarity = 0,
                            preparer   = NULL,
                            min_nchar  = 3L) {

  check_string(column)
  check_number_decimal(max_df, min = 1, allow_infinite = TRUE)
  check_number_decimal(min_rarity, min = 0)
  check_number_whole(min_nchar, min = 0)

  prep_obj <- NULL
  if (!is.null(preparer)) {
    if (rlang::is_formula(preparer)) {
      # Reuse the shared column ~ steps parser. A one-sided formula carries the
      # steps on the RHS; we slot the blocking column on the LHS so the parser
      # produces a Search_Preparer named for it.
      rhs <- rlang::f_rhs(preparer)
      fml <- stats::as.formula(
        call("~", rlang::sym(column), rhs),
        env = rlang::f_env(preparer)
      )
      prep_obj <- .parse_strategy_formulas(list(fml))[[1L]]
    } else if (S7_inherits(preparer, Search_Preparer)) {
      prep_obj <- preparer
    } else {
      cli::cli_abort(
        "{.arg preparer} must be {.code NULL}, a {.code ~ steps} formula, or a {.cls Search_Preparer}."
      )
    }
  }

  # Capless-token-block guard (notes/region_free_linking.md sections 4.5 / 9):
  # a token block with neither a finite df cap nor a positive rarity floor
  # selects EVERY token as a block key - the densest join in the package. Warn
  # loudly here (the constructor fires reliably); audit_strategy() /
  # plan_strategy() could also surface this on the assembled strategy.
  if (!is.finite(max_df) && min_rarity <= 0) {
    cli::cli_warn(c(
      "!" = "{.fn block_on_tokens} on {.field {column}} has no key selection \\
             (no finite {.arg max_df}, no positive {.arg min_rarity}).",
      "i" = "Every token becomes a block key - the densest operation in the \\
             package. Set a {.arg max_df} (or {.arg min_rarity}) so common \\
             tokens are dropped as keys."
    ))
  }

  Block_On_Tokens(
    column     = column,
    max_df     = max_df,
    min_rarity = min_rarity,
    preparer   = prep_obj,
    min_nchar  = as.integer(min_nchar)
  )
}


# ---------------------------------------------------------------------------
# Rendering helper (used by print.Search_Strategy)
# ---------------------------------------------------------------------------

#' Render a single `block_by` entry readably for print methods.
#' @noRd
.format_block_entry <- function(e) {
  if (!.is_token_block(e)) return(as.character(e))
  parts <- character()
  if (is.finite(e@max_df))     parts <- c(parts, sprintf("max_df=%s", format(e@max_df)))
  if (e@min_rarity > 0)        parts <- c(parts, sprintf("min_rarity=%s", format(e@min_rarity)))
  inner <- if (length(parts)) paste0(", ", paste(parts, collapse = ", ")) else ""
  sprintf("block_on_tokens(%s%s)", e@column, inner)
}

#' Render a whole `block_by` spec as a comma-joined string.
#' @noRd
.format_block_by <- function(block_by) {
  entries <- .block_by_list(block_by)
  paste(vapply(entries, .format_block_entry, character(1)), collapse = ", ")
}


# ---------------------------------------------------------------------------
# The explosion (data.table) - materialise the `._btok` block column
# ---------------------------------------------------------------------------

#' Resolve the Step pipeline a token block uses to tokenize its column.
#'
#' Precedence: the spec's own `preparer` if given; else the column's *scored*
#' preparer when `column` is also a scored column of the strategy; else the
#' default `normalize_text + word_tokens(min_nchar = min_nchar)`.
#' @noRd
.btok_steps <- function(spec, strategy) {
  if (!is.null(spec@preparer)) return(spec@preparer@steps)
  scored <- strategy@preparers[[spec@column]]
  if (!is.null(scored)) return(scored@steps)
  list(
    Step(name = "normalize_text", args = list()),
    Step(name = "word_tokens", args = list(min_nchar = spec@min_nchar))
  )
}


#' Apply an ordered list of Steps to a character vector -> list-of-tokens.
#'
#' Mirrors the Reduce/apply_step_r loop in the data.table prepare method (one
#' column, no chunking/progress - the blocking column is one extra pass).
#' @noRd
.apply_btok_steps <- function(values, steps) {
  out <- Reduce(
    f = function(acc, step) {
      fn <- get(step@name, mode = "function")
      do.call(fn, c(list(acc), step@args))
    },
    x = steps,
    init = values
  )
  if (!is.list(out)) out <- as.list(out)
  out
}


#' Build the per-record surviving blocking-token table for one token block.
#'
#' Returns a data.table `id | ._btok` of (record, surviving block key) pairs,
#' where surviving means global df <= max_df AND global rarity >= min_rarity.
#' Global df is the corpus-wide distinct-record count per blocking token, the
#' same `df_global` axis Feature B uses. A record with no surviving block key
#' contributes no rows (unreachable via token-blocking in this stage).
#' @noRd
.btok_surviving_dt <- function(dt, id, spec, strategy) {
  if (!spec@column %in% names(dt)) {
    cli::cli_abort("Token-blocking column {.field {spec@column}} not found in data")
  }
  steps  <- .btok_steps(spec, strategy)
  tokens <- .apply_btok_steps(dt[[spec@column]], steps)
  lens   <- lengths(tokens)

  bt <- data.table::data.table(
    ._btok = unlist(tokens, use.names = FALSE),
    ._row  = rep(seq_len(nrow(dt)), times = lens)
  )
  bt[["_id_"]] <- rep(dt[[id]], times = lens)
  bt <- bt[!is.na(`._btok`) & nzchar(`._btok`)]
  if (nrow(bt) == 0L) {
    return(data.table::data.table(`._btok` = character(), id = dt[[id]][0]))
  }

  # Global df = distinct records carrying the blocking token corpus-wide.
  bt[, `._bdf` := data.table::uniqueN(`._row`), by = "._btok"]

  # Global rarity floor (only when requested) - mirror the inverse_freq /
  # tfidf-style metric on the corpus-wide blocking-token frequencies, using the
  # strategy's rarity method so the floor means the same thing as min_rarity.
  if (spec@min_rarity > 0) {
    bt[, `._brar` := .btok_global_rarity(`._btok`, `._row`, strategy@rarity)]
    keep <- bt[`._bdf` <= spec@max_df & `._brar` >= spec@min_rarity]
  } else {
    keep <- bt[`._bdf` <= spec@max_df]
  }
  if (nrow(keep) == 0L) {
    return(data.table::data.table(`._btok` = character(), id = dt[[id]][0]))
  }

  out <- unique(keep[, .(id = get("_id_"), `._btok`)])
  out
}


#' Global blocking-token rarity, matching the strategy's rarity metric.
#'
#' Corpus-wide (no block); used only as the `min_rarity` selection floor for
#' eligible block keys. `freq`/`df`/`N` are the global blocking-token counts.
#' @noRd
.btok_global_rarity <- function(btok, row, rarity_method) {
  d <- data.table::data.table(btok = btok, row = row)
  d[, freq := .N, by = "btok"]
  d[, df := data.table::uniqueN(row), by = "btok"]
  N <- data.table::uniqueN(d$row)
  f <- d$freq; dd <- d$df
  switch(
    rarity_method,
    "inverse_freq"          = 1 / f,
    "smoothed_inverse_freq" = 1 / (f + 1),
    "tfidf"                 = (f / sum(f)) * log(1 + N / dd),
    "bm25"                  = log((N - dd + 0.5) / (dd + 0.5)),
    1 / f
  )
}


#' Explode a token table by its records' surviving block keys -> `._btok`.
#'
#' The heart of Feature A. `tokens` is the long-form token table (already
#' carrying any plain block columns); `dt` is the raw input. For every
#' token-block spec in the strategy, build the per-record surviving block-key
#' table and INNER-join it onto `tokens` by id, materialising the `._btok`
#' column - a per-record cross product (token rows x surviving block keys).
#' Multiple specs union into the single `._btok` (one resolved column).
#'
#' MUST run strictly after the non-unique-id guard (`.warn_nonunique_id`): the
#' explosion repeats a record's id across its `._btok` values, which is NOT a
#' data problem, so it must not reach that guard (which already ran on the raw,
#' pre-explosion input). See notes/region_free_linking.md section 4.5.
#'
#' A record with no surviving block key drops out of `tokens` entirely for this
#' stage (inner join) - it is unreachable via token-blocking, by design.
#' @noRd
.explode_token_blocks_dt <- function(tokens, dt, id, strategy) {
  specs <- .token_block_specs(strategy)
  if (length(specs) == 0L) return(tokens)

  surv_list <- lapply(specs, function(s) .btok_surviving_dt(dt, id, s, strategy))
  surv <- data.table::rbindlist(surv_list, use.names = TRUE)
  surv <- unique(surv)

  # Inner join: drop token rows of records with no surviving block key, and
  # fan each surviving record's tokens across its block keys.
  data.table::setnames(surv, "id", id)
  exploded <- merge(tokens, surv, by = id, allow.cartesian = TRUE)
  exploded[]
}
