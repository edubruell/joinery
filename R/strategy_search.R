# ============================================================
# Search_Strategy class, formula parser, search_strategy() constructor
# ============================================================
#
# Top-level IR object that bundles preparers, weights, blocking,
# rarity, smoothing, threshold, and tuning knobs into a single
# declarative specification.
# ============================================================


#' Search Strategy Class
#'
#' @description
#' An S7 class capturing all **metadata** necessary to perform heuristic,
#' token-based record linkage in the joinery package.
#'
#' A `Search_Strategy` does **not** execute any matching itself.
#' Instead, it stores:
#'
#' - A list of [`Search_Preparer`] objects, one per column.
#' - Optional named numeric **weights** used in similarity scoring.
#' - Optional **block_by** variable(s) restricting candidate searches to blocks.
#' - A **rarity** method governing how token rarity is computed
#'   (e.g., `"inverse_freq"`, `"tfidf"`).
#' - A smoothing configuration that describes how rIP values are transformed
#'   before scoring.
#'
#' All operational behavior (tokenization, rarity computation,
#' duplicate detection, candidate search) is handled by S7 generics such
#' as `prepare_search_data()`, `compute_rarity()`,
#' `detect_duplicates()`, and `search_candidates()`.
#'
#' @slot preparers A list of `Search_Preparer` objects, named by column.
#' @slot weights   A named numeric vector (validated in wrapper).
#' @slot block_by  NULL or a character vector of blocking variables.
#' @slot rarity    A character scalar describing the rarity method.
#' @slot threshold  A numeric scalar containing the match or deduplication threshold
#' @slot min_rarity  Numeric scalar between 0 and Inf.
#'   Tokens with rarity below this value are removed before scoring.
#' @slot smoothing A [Smoothing] object describing how rIP should be smoothed
#'   within each record and column before scoring.
#' @slot max_candidates Numeric scalar specifying the maximum number of candidate
#'   matches to retain per record. Default is `Inf` (no limit). When finite,
#'   only the top `max_candidates` highest scoring matches are kept.
#' @slot feedback_strength Numeric scalar controlling feedback weighted scoring.
#'   Default is `0` (disabled). Positive values adjust scores based on token
#'   overlap patterns.
#'
#' @seealso [search_strategy()]
#'
#' @noRd
Search_Strategy <- new_class("Search_Strategy",
                             properties = list(
                               preparers = class_list,
                               weights   = class_any,
                               block_by  = class_any,
                               rarity    = class_character,
                               threshold = class_numeric,
                               min_rarity = class_any,
                               smoothing = Smoothing,
                               max_candidates = class_numeric,
                               feedback_strength = class_numeric
                             )
)


#' Print a Search_Strategy Object
#'
#' @noRd
print.Search_Strategy <- new_external_generic("base", "print", "x")


#' @noRd
method(print.Search_Strategy, Search_Strategy) <- function(x, ...) {

  cli::cli_text("{.strong <joinery::Search_Strategy>}")

  # ------------------------------------------------------------------
  # Columns and their step pipelines (compact bullets)
  # ------------------------------------------------------------------
  bullets <- character(length(x@preparers))

  if (length(x@preparers) > 0) {
    for (i in seq_along(x@preparers)) {
      prep <- x@preparers[[i]]

      step_labels <- vapply(
        prep@steps,
        function(s) {
          if (length(s@args) == 0) {
            sprintf("%s()", s@name)
          } else {
            arg_names <- names(s@args)
            args_fmt <- vapply(seq_along(s@args), function(j) {
              val <- deparse(s@args[[j]], nlines = 1)
              if (is.null(arg_names) || arg_names[j] == "") {
                val
              } else {
                sprintf("%s = %s", arg_names[j], val)
              }
            }, character(1))
            sprintf("%s(%s)", s@name, paste(args_fmt, collapse = ", "))
          }
        },
        character(1)
      )

      bullets[[i]] <- sprintf(
        "{.field %s}: %s",
        prep@column,
        paste(step_labels, collapse = " -> ")
      )
    }

    cli::cli_text()
    cli::cli_text("{.strong columns}")
    cli::cli_bullets(bullets)
  } else {
    cli::cli_text()
    cli::cli_text("{.strong columns}")
    cli::cli_text("none")
  }

  # ------------------------------------------------------------------
  # Other metadata (one line each)
  # ------------------------------------------------------------------

  cli::cli_text()

  # blocking
  if (is.null(x@block_by)) {
    cli::cli_text("blocking: none")
  } else {
    cli::cli_text("blocking: {paste(x@block_by, collapse = ', ')}")
  }

  # weights
  if (length(x@weights) == 0) {
    cli::cli_text("weights: none")
  } else {
    w <- paste(
      sprintf("%s=%s", names(x@weights), format(x@weights)),
      collapse = ", "
    )
    cli::cli_text("weights: {w}")
  }

  # rarity
  cli::cli_text("rarity: {x@rarity} (min={format(x@min_rarity)})")

  # smoothing
  sm <- x@smoothing
  if (sm@method == "none") {
    cli::cli_text("smoothing: none")
  } else if (inherits(sm, "Smoothing_Offset")) {
    cli::cli_text("smoothing: offset(alpha={format(sm@alpha)})")
  } else if (inherits(sm, "Smoothing_Softmax")) {
    cli::cli_text("smoothing: softmax(temp={format(sm@temperature)})")
  } else {
    cli::cli_text("smoothing: {sm@method}")
  }

  # threshold
  thr <- x@threshold
  cli::cli_text("threshold: {if (is.null(thr)) 'none' else format(thr)}")

  # max_candidates
  if (is.finite(x@max_candidates)) {
    cli::cli_text("max_candidates: {format(x@max_candidates)}")
  } else {
    cli::cli_text("max_candidates: none")
  }

  # feedback_strength
  if (x@feedback_strength > 0) {
    cli::cli_text("feedback_strength: {format(x@feedback_strength)}")
  } else {
    cli::cli_text("feedback_strength: none")
  }

  invisible(x)
}


# ---------------------------------------------------------------------------
# Helpers for building Search_Strategy
# ---------------------------------------------------------------------------

#' Convert Expression to Step Object
#'
#' @description
#' Converts a quoted expression (symbol or call) into a Step object containing
#' the function name and arguments.
#'
#' @param expr A symbol or call representing a preprocessing step.
#'
#' @return A Step object.
#'
#' @noRd
expr_to_step <- function(expr) {
  stopifnot(rlang::is_call(expr) || rlang::is_symbol(expr))

  if (rlang::is_symbol(expr)) {
    name <- as.character(expr)
    return(Step(name = name, args = list()))
  }

  name <- as.character(expr[[1]])
  args <- as.list(expr[-1])

  Step(name = name, args = args)
}


#' Define a Search Strategy for Record Linkage
#'
#' @description
#' Creates a [Search_Strategy] object that specifies how columns should be
#' preprocessed for token index based record linkage, along with optional
#' weights, blocking variables, rarity computation method, rIP smoothing,
#' and similarity threshold.
#'
#' @param ... Two sided formulas of the form `column ~ preprocessing_steps`.
#'   The left hand side names the column; the right hand side contains one or
#'   more function calls to apply in sequence (for example
#'   `name ~ normalize_text() + word_tokens(min_nchar = 3)`).
#' @param block_by Optional character vector of column names to use for blocking.
#'   Candidate searches will be restricted to records sharing the same blocking
#'   key values. Default is `NULL` (no blocking).
#' @param weights Optional named numeric vector of weights for similarity scoring.
#'   Names should correspond to columns. Default is `numeric()` (uniform weights).
#' @param rarity Character scalar specifying the rarity computation method.
#'   Default is `"inverse_freq"`.
#' @param min_rarity Numeric scalar specifying the minimum rarity value required
#'   for a token to be included in similarity scoring. Tokens with rarity below
#'   this threshold are filtered out. Default is `0`.
#' @param threshold Numeric scalar specifying the minimum relative identification
#'   potential required for two records to be considered matches. Default is `0.9`.
#' @param smoothing A [Smoothing] object created by one of the
#'   [smooth_rip] helpers that controls how rIP values are smoothed before
#'   scoring. Default is [smooth_rip_identity()].
#' @param max_candidates Numeric scalar specifying the maximum number of candidate
#'   matches to retain per record. Default is `Inf` (no limit). When finite,
#'   only the top `max_candidates` highest scoring matches are kept per record.
#' @param feedback_strength Numeric scalar controlling feedback weighted scoring.
#'   Default is `0` (disabled). Positive values adjust scores based on the
#'   proportion of matched tokens.
#'
#' @return A [Search_Strategy] object.
#'
#' @export
search_strategy <- function(...,
                            block_by   = NULL,
                            weights    = numeric(),
                            rarity     = "inverse_freq",
                            min_rarity = 0,
                            threshold  = 0.9,
                            smoothing  = smooth_rip_identity(),
                            max_candidates = Inf,
                            feedback_strength = 0) {

  check_number_decimal(min_rarity, min = 0)
  check_number_decimal(threshold)
  check_number_decimal(max_candidates, min = 0, allow_infinite = TRUE)
  if (is.finite(max_candidates) && max_candidates <= 0) {
    cli::cli_abort("{.arg max_candidates} must be positive")
  }
  check_number_decimal(feedback_strength, min = 0)

  if (!S7_inherits(smoothing, Smoothing)) {
    cli::cli_abort("{.arg smoothing} must be a {.cls Smoothing} object created by a {.fn smooth_rip_*} helper")
  }

  fmls <- rlang::list2(...)

  flatten_plus_calls <- function(expr) {
    if (rlang::is_call(expr, "+")) {
      c(flatten_plus_calls(expr[[2]]), flatten_plus_calls(expr[[3]]))
    } else {
      list(expr)
    }
  }

  preparers <- map(fmls, function(fml) {

    if (!rlang::is_formula(fml)) {
      cli::cli_abort("All arguments to {.fn search_strategy} must be formulas")
    }

    col <- rlang::as_string(rlang::f_lhs(fml))
    rhs <- rlang::f_rhs(fml)

    if (rlang::is_call(rhs, "+")) {
      steps <- flatten_plus_calls(rhs)
    } else {
      steps <- list(rhs)
    }

    steps <- map(steps, expr_to_step)

    Search_Preparer(col, steps)
  })

  names(preparers) <- map_chr(preparers, function(p) p@column)

  if (!is.null(block_by)) {
    check_character(block_by)
  }
  if (length(weights) > 0 &&
      (is.null(names(weights)) || any(names(weights) == ""))) {
    cli::cli_abort("{.arg weights} must be a named numeric vector")
  }
  if (length(weights) > 0) {
    preparer_cols <- map_chr(preparers, function(p) p@column)
    bad_wt <- setdiff(names(weights), preparer_cols)
    if (length(bad_wt) > 0L) {
      cli::cli_abort("{.arg weights} names not found in any preparer column: {.field {bad_wt}}")
    }
  }
  check_string(rarity)

  Search_Strategy(
    preparers  = preparers,
    weights    = weights,
    block_by   = block_by,
    rarity     = rarity,
    threshold  = threshold,
    min_rarity = min_rarity,
    smoothing  = smoothing,
    max_candidates = max_candidates,
    feedback_strength = feedback_strength
  )
}
