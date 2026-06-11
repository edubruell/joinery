# ============================================================
# Exact Strategy Class for joinery
# ============================================================
#
# Defines the Exact_Strategy S7 class — the exact (score-1.0) operating point
# of the token-overlap kernel, declared as a strategy rather than a verb so it
# permeates the standard apply verbs (detect_duplicates / search_candidates)
# by S7 dispatch, exactly like Embedding_Strategy.
#
# Exact strategies share the *tokenization* half of Search_Strategy (the same
# `column ~ steps` preparers, blocking, and rarity), but replace the *matching*
# half: instead of weighted rIP scoring + threshold, two records link iff every
# column's token set is equal within a block (set-equality) — optionally
# relaxed to token-set containment. This is empty-column robust (identical-name
# / both-empty-street pairs link, which a weighted threshold rejects) and is a
# single hash join, not a scored pass.
#
# The matching kernel (fingerprint + containment) lives in
# exact_methods_<backend>.R; this file is the class, constructor, and the
# shared plumbing both backends use.
# ============================================================


# ---------------------------------------------------------------------------
# Exact_Strategy class
# ---------------------------------------------------------------------------

#' Exact Strategy Class
#'
#' @description
#' An S7 class representing exact (score-1.0) token-set matching. A distinct
#' strategy type from `Search_Strategy`; it shares the preparer/blocking/rarity
#' tokenization but matches by token-set equality (or containment) rather than
#' weighted scoring.
#'
#' @slot preparers Named list of `Search_Preparer` objects (one per column).
#' @slot block_by NULL or a character vector of blocking variables.
#' @slot rarity Character scalar rarity metric — used only by the containment
#'   guard (`min_base_rarity`); ignored under set-equality.
#' @slot containment One of `"off"`, `"forward"`, `"bidirectional"`.
#' @slot min_base_rarity Numeric. Containment guard: drop links whose base
#'   record carries summed rarity mass below this floor.
#'
#' @seealso [exact_strategy()]
#'
#' @noRd
Exact_Strategy <- new_class(
  "Exact_Strategy",
  properties = list(
    preparers       = class_list,
    block_by        = class_any,
    rarity          = class_character,
    containment     = class_character,
    min_base_rarity = class_numeric
  ),
  validator = function(self) {
    if (length(self@containment) != 1 ||
        !self@containment %in% c("off", "forward", "bidirectional")) {
      return("containment must be one of 'off', 'forward', 'bidirectional'")
    }
    if (length(self@min_base_rarity) != 1 ||
        !is.finite(self@min_base_rarity) || self@min_base_rarity < 0) {
      return("min_base_rarity must be a non-negative finite scalar")
    }
  }
)


#' @noRd
method(print.Search_Strategy, Exact_Strategy) <- function(x, ...) {
  cli::cli_text("{.strong <joinery::Exact_Strategy>}")

  bullets <- character(length(x@preparers))
  if (length(x@preparers) > 0) {
    for (i in seq_along(x@preparers)) {
      prep <- x@preparers[[i]]
      step_labels <- map_chr(prep@steps, function(s) {
        if (length(s@args) == 0) sprintf("%s()", s@name)
        else sprintf("%s(%s)", s@name,
                     paste(map_chr(s@args, function(a) deparse(a, nlines = 1)),
                           collapse = ", "))
      })
      bullets[[i]] <- sprintf("{.field %s}: %s", prep@column,
                              paste(step_labels, collapse = " -> "))
    }
    cli::cli_text()
    cli::cli_text("{.strong columns}")
    cli::cli_bullets(bullets)
  }

  cli::cli_text()
  if (is.null(x@block_by)) cli::cli_text("blocking: none")
  else cli::cli_text("blocking: {paste(x@block_by, collapse = ', ')}")
  cli::cli_text("matching: exact set-equality")
  if (x@containment == "off") {
    cli::cli_text("containment: off")
  } else {
    cli::cli_text("containment: {x@containment} (min_base_rarity={format(x@min_base_rarity)})")
  }
  invisible(x)
}


# ---------------------------------------------------------------------------
# Shared plumbing
# ---------------------------------------------------------------------------

# Fingerprint delimiter: ASCII unit separator (0x1F). Normalized tokens are
# uppercase/ASCII and cannot contain it, so it cannot forge a false equality.
# The DuckDB path uses chr(31) for the same character.
.JOINERY_FP_DELIM <- intToUtf8(31L)


# An Exact_Strategy shares Search_Strategy's tokenization half. The token
# engine (prepare_search_data / compute_rarity) dispatches on Search_Strategy,
# so the exact methods build a minimal Search_Strategy proxy carrying the same
# preparers / block_by / rarity to reach it. The proxy's scoring slots
# (weights, threshold, smoothing, …) are inert — exact matching never scores.
.exact_proxy_strategy <- function(strategy) {
  Search_Strategy(
    preparers         = strategy@preparers,
    weights           = numeric(),
    block_by          = strategy@block_by,
    rarity            = strategy@rarity,
    threshold         = 1,
    min_rarity        = 0,
    max_token_df      = Inf,
    smoothing         = smooth_rip_identity(),
    max_candidates    = Inf,
    feedback_strength = 0
  )
}


# ---------------------------------------------------------------------------
# Constructor
# ---------------------------------------------------------------------------

#' Define an Exact Matching Strategy
#'
#' @description
#' Creates an [Exact_Strategy] for exact (score-1.0) token-set matching. Hand
#' it to the standard apply verbs — [detect_duplicates()] for the dedup face,
#' [search_candidates()] for the cross-table face — exactly like a
#' [search_strategy()]; exactness permeates by dispatch. Both return the
#' standard schema with `score == 1.0`.
#'
#' Two records link iff **every column's token set is equal** within the same
#' block. This is the same object as a fuzzy score of exactly 1.0, but reached
#' by one hash join instead of a scored pass, and it is **empty-column robust**:
#' two records with identical names and both-empty streets link, which a
#' weighted threshold silently rejects (the `1 - weight(col)` ceiling).
#'
#' Use it as the cheap front stage of a staged workflow
#' (`exact -> fuzzy(residual)`): the residual comes from [extract_unmatched()],
#' and `multi_stage_dedup()` / `multi_stage_search()` thread it automatically
#' when you pass `list(exact_strategy(...), search_strategy(...))`.
#'
#' @param ... Two-sided formulas `column ~ step1 + step2`, identical in form to
#'   [search_strategy()].
#' @param block_by Optional character vector of blocking columns.
#' @param rarity Character scalar rarity metric, used only by the containment
#'   guard. Default `"inverse_freq"`.
#' @param containment One of `"off"` (set-equality, default), `"forward"`
#'   (link `base ⊆ target`), or `"bidirectional"` (either direction). Data-shape
#'   dependent — over-links on noisy corpora, so never the default.
#' @param min_base_rarity Numeric containment guard: drop links whose base
#'   record carries summed rarity mass below this floor. Default `0`.
#'
#' @return An [Exact_Strategy] object.
#'
#' @seealso [search_strategy()], [detect_duplicates()], [search_candidates()],
#'   [extract_unmatched()].
#'
#' @export
exact_strategy <- function(...,
                           block_by        = NULL,
                           rarity          = "inverse_freq",
                           containment     = c("off", "forward", "bidirectional"),
                           min_base_rarity = 0) {

  containment <- match.arg(containment)
  check_number_decimal(min_base_rarity, min = 0)
  check_string(rarity)
  if (!is.null(block_by)) check_character(block_by)

  preparers <- .parse_strategy_formulas(rlang::list2(...))

  Exact_Strategy(
    preparers       = preparers,
    block_by        = block_by,
    rarity          = rarity,
    containment     = containment,
    min_base_rarity = min_base_rarity
  )
}
