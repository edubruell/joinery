# ============================================================
# rarity_distribution(), all backends
# ============================================================
#
# Read-side pre-match helper (v0.8 Stage 04, plan item A4). Runs
# prepare_search_data + compute_rarity and reports, per (column[,
# block]), the token df/rarity distribution plus the top-df offender
# list, so a user can SET min_rarity / max_token_df from the real
# token distribution instead of guessing.
#
# Deliberately scoring-free: no token-overlap join, no .score_token_pairs
# / .score_pairs_sql. This is the cheap distribution lookup that
# plan_strategy() (Stage 08, A7) will subsume with a full min_rarity
# cost curve, keep the surface small so the two don't duplicate.
#
# Backends: data.table (reference), DuckDB (collect then delegate, like
#   audit_strategy), tibble/data.frame (as_DT wrappers).
# ============================================================


#' Token Rarity Distribution Result
#'
#' @description
#' Result of [rarity_distribution()]. Carries the per-`(column[, block])`
#' document-frequency / rarity summary, the top document-frequency offender
#' list, and the rarity method that produced it.
#'
#' @slot distribution `data.table`. One row per `(src_column[, block])`:
#'   `n_tokens` (distinct token types), `df_max` (worst fan-out), `top_token`
#'   (the highest-df token), `rarity_min` / `rarity_p50` / `rarity_max`, and
#'   `suggested_min_rarity`, the rarity of `top_token`; setting `min_rarity`
#'   just above it drops that column/block's worst fan-out driver.
#' @slot offenders `data.table`. The top `n_offenders` tokens by document
#'   frequency across all `(column[, block])`: `src_column`, `block` (when
#'   `block_by` is set), `token`, `df`, `rarity`.
#' @slot n_offenders Integer. How many offenders were requested.
#' @slot rarity Character scalar. The strategy's rarity method.
#' @slot blocked Logical. Whether the strategy carried `block_by`.
#'
#' @noRd
Rarity_Distribution <- new_class(
  "Rarity_Distribution",
  properties = list(
    distribution = class_any,
    offenders    = class_any,
    n_offenders  = class_integer,
    rarity       = class_character,
    blocked      = class_logical
  )
)


#' @noRd
print.Rarity_Distribution <- new_external_generic("base", "print", "x")


# ---------------------------------------------------------------------------
# Internal: compute the distribution + offenders from a rarity'd token table
# ---------------------------------------------------------------------------

#' @noRd
.rarity_distribution_core <- function(tokens, strategy, n_offenders) {

  # Use plain block cols for display (skip derived ._btok token-blocking keys).
  block_by <- .plain_block_cols(strategy)
  by_keys  <- c(block_by, "src_column")

  # Per-token document frequency / rarity within (block, column, token). These
  # already exist when `tokens` is a compute_rarity() output; recompute defensively
  # on the unique token rows so the verb works on any rarity'd token frame.
  tok <- unique(tokens[, c(by_keys, "token", "df", "rarity"), with = FALSE])

  # A single, stable block key string for display / grouping.
  if (length(block_by) > 0L) {
    if (length(block_by) == 1L) {
      tok[, block := as.character(tok[[block_by]])]
    } else {
      parts <- lapply(block_by, function(b) as.character(tok[[b]]))
      tok[, block := do.call(paste, c(parts, list(sep = ", ")))]
    }
  } else {
    tok[, block := NA_character_]
  }

  grp <- c("block", "src_column")

  # Per (column[, block]) distribution. top_token = highest-df token; its rarity
  # is the suggested floor (set min_rarity just above it to drop the worst driver).
  distribution <- tok[, {
    o <- order(-df)
    .(
      n_tokens             = .N,
      df_max               = df[o][1L],
      top_token            = token[o][1L],
      rarity_min           = min(rarity),
      rarity_p50           = stats::median(rarity),
      rarity_max           = max(rarity),
      suggested_min_rarity = rarity[o][1L]
    )
  }, by = grp]
  data.table::setorder(distribution, -df_max)

  # Global top-df offender list (the fan-out drivers to eyeball).
  off <- tok[order(-df)][seq_len(min(n_offenders, nrow(tok)))]
  offenders <- off[, c("src_column", "block", "token", "df", "rarity"), with = FALSE]

  # Drop the synthetic block column when unblocked, for a clean schema.
  if (length(block_by) == 0L) {
    distribution[, block := NULL]
    offenders[, block := NULL]
  }

  list(distribution = distribution[], offenders = offenders[])
}


# ---------------------------------------------------------------------------
# data.table method (reference implementation)
# ---------------------------------------------------------------------------

method(
  rarity_distribution,
  list(DT_tbl, class_character, Search_Strategy)
) <- function(data, id, strategy, n_offenders = 20L, ...) {

  n_offenders <- as.integer(n_offenders)
  dt <- data.table::as.data.table(data)

  # Scoring-free: tokenize + rarity only. No overlap join, no scoring helper.
  tokens <- prepare_search_data(dt, id, strategy)
  tokens <- compute_rarity(tokens, strategy)

  core <- .rarity_distribution_core(tokens, strategy, n_offenders)

  Rarity_Distribution(
    distribution = core$distribution,
    offenders    = core$offenders,
    n_offenders  = n_offenders,
    rarity       = strategy@rarity,
    blocked      = !is.null(strategy@block_by)
  )
}


# ---------------------------------------------------------------------------
# DuckDB method: collect to R, delegate to data.table (mirrors audit_strategy)
# ---------------------------------------------------------------------------

method(
  rarity_distribution,
  list(Duck_tbl, class_character, Search_Strategy)
) <- function(data, id, strategy, n_offenders = 20L, sample_n = NULL, ...) {

  con      <- data$src$con
  data     <- .materialise_duck_input(data, con)
  tbl_name <- data$lazy_query$x

  if (is.null(sample_n)) {
    n_total  <- DBI::dbGetQuery(
      con, paste0("SELECT COUNT(*) AS n FROM \"", tbl_name, "\"")
    )$n
    sample_n <- as.integer(n_total)
  } else {
    sample_n <- as.integer(sample_n)
  }

  dt_sample <- data.table::as.data.table(
    DBI::dbGetQuery(
      con,
      paste0("SELECT * FROM \"", tbl_name, "\" USING SAMPLE ", sample_n, " ROWS")
    )
  )

  rarity_distribution(dt_sample, id, strategy, n_offenders = n_offenders, ...)
}


# ---------------------------------------------------------------------------
# Tibble / data.frame thin wrappers
# ---------------------------------------------------------------------------

method(
  rarity_distribution,
  list(.jyDF, class_character, Search_Strategy)
) <- function(data, id, strategy, n_offenders = 20L, ...) {
  rarity_distribution(as_DT(data), id, strategy, n_offenders = n_offenders, ...)
}

method(
  rarity_distribution,
  list(.jyTBL_DF, class_character, Search_Strategy)
) <- function(data, id, strategy, n_offenders = 20L, ...) {
  rarity_distribution(as_DT(data), id, strategy, n_offenders = n_offenders, ...)
}

method(
  rarity_distribution,
  list(.jyTBL, class_character, Search_Strategy)
) <- function(data, id, strategy, n_offenders = 20L, ...) {
  rarity_distribution(as_DT(data), id, strategy, n_offenders = n_offenders, ...)
}


# ---------------------------------------------------------------------------
# print
# ---------------------------------------------------------------------------

method(print.Rarity_Distribution, Rarity_Distribution) <- function(x, ...) {
  cli::cli_h1("Rarity_Distribution")
  cli::cli_text("rarity method: {.val {x@rarity}}{if (x@blocked) ' (per block)' else ''}")

  d <- x@distribution
  if (!is.null(d) && nrow(d) > 0L) {
    cli::cli_text("{.strong per-column distribution}")
    for (i in seq_len(nrow(d))) {
      blk <- if (x@blocked) sprintf(" [block %s]", d$block[i]) else ""
      cli::cli_bullets(sprintf(
        "{.field %s}%s: %d tokens, df_max=%d (%s), rarity p50=%.4g, suggested min_rarity >~ %.4g",
        d$src_column[i], blk, d$n_tokens[i], d$df_max[i],
        d$top_token[i], d$rarity_p50[i], d$suggested_min_rarity[i]
      ))
    }
  }

  o <- x@offenders
  if (!is.null(o) && nrow(o) > 0L) {
    cli::cli_text("{.strong top-df offenders} (fan-out drivers)")
    for (i in seq_len(min(nrow(o), 10L))) {
      blk <- if (x@blocked) sprintf(" [%s]", o$block[i]) else ""
      cli::cli_bullets(sprintf(
        "{.field %s}%s: '%s' df=%d, rarity=%.4g",
        o$src_column[i], blk, o$token[i], o$df[i], o$rarity[i]
      ))
    }
  }

  invisible(x)
}
