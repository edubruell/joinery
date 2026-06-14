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
#' @slot max_token_df Numeric scalar between 1 and Inf. Tokens appearing in
#'   more than this many records within their `(block, column)` are removed
#'   before scoring (a raw document-frequency cap, the blunt companion to the
#'   rarity-metric `min_rarity`). Default `Inf` (off).
#' @slot smoothing A [Smoothing] object describing how rIP should be smoothed
#'   within each record and column before scoring.
#' @slot max_candidates Numeric scalar specifying the maximum number of candidate
#'   matches to retain per record. Default is `Inf` (no limit). When finite,
#'   only the top `max_candidates` highest scoring matches are kept.
#' @slot max_fanout Numeric scalar. Budget on the estimated number of
#'   intermediate token-overlap-join rows (`sum df^2` for dedup, `sum
#'   df_base*df_target` for search, over `(column, block, token)`). The
#'   always-on guard against a hot/boilerplate token fanning a block into a
#'   quadratic join. Default `5e7`; `Inf` disables.
#' @slot on_fanout One of `"cap"` (default, auto-drop the hyper-common tokens
#'   that bust the budget and warn), `"abort"` (stop with an actionable error),
#'   or `"off"` (no guard).
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
                               max_token_df = new_property(class_numeric, default = Inf),
                               smoothing = Smoothing,
                               max_candidates = class_numeric,
                               max_fanout = new_property(class_numeric, default = 5e7),
                               on_fanout = new_property(class_character, default = "cap"),
                               feedback_strength = class_numeric,
                               on_missing = new_property(class_character, default = "penalise")
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

      step_labels <- map_chr(
        prep@steps,
        function(s) {
          if (length(s@args) == 0) {
            sprintf("%s()", s@name)
          } else {
            arg_names <- names(s@args)
            args_fmt <- map_chr(seq_along(s@args), function(j) {
              val <- deparse(s@args[[j]], nlines = 1)
              if (is.null(arg_names) || arg_names[j] == "") {
                val
              } else {
                sprintf("%s = %s", arg_names[j], val)
              }
            })
            sprintf("%s(%s)", s@name, paste(args_fmt, collapse = ", "))
          }
        }
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
  if (is.finite(x@max_token_df)) {
    cli::cli_text("rarity: {x@rarity} (min={format(x@min_rarity)}, max_token_df={format(x@max_token_df)})")
  } else {
    cli::cli_text("rarity: {x@rarity} (min={format(x@min_rarity)})")
  }

  # fan-out guard
  if (identical(x@on_fanout, "off") || !is.finite(x@max_fanout)) {
    cli::cli_text("fan-out guard: off")
  } else {
    cli::cli_text("fan-out guard: {x@on_fanout} at {format(x@max_fanout, scientific = FALSE, big.mark = ',')}")
  }

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

  # on_missing (only worth printing when non-default)
  if (length(x@on_missing) && x@on_missing == "renormalise") {
    cli::cli_text("on_missing: renormalise (present-column weight redistribution)")
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


#' Parse `column ~ steps` formulas into a named list of Search_Preparers
#'
#' Shared by [search_strategy()] and [exact_strategy()] — both consume the
#' same `column ~ step1 + step2` tokenization grammar; only the matching half
#' differs. Keeping one parser prevents the two constructors from drifting.
#' @noRd
.parse_strategy_formulas <- function(fmls) {
  flatten_plus_calls <- function(expr) {
    if (rlang::is_call(expr, "+")) {
      c(flatten_plus_calls(expr[[2]]), flatten_plus_calls(expr[[3]]))
    } else {
      list(expr)
    }
  }

  preparers <- map(fmls, function(fml) {
    if (!rlang::is_formula(fml)) {
      cli::cli_abort("All strategy arguments must be {.cls formula}s of the form {.code column ~ steps}.")
    }

    col <- rlang::as_string(rlang::f_lhs(fml))
    rhs <- rlang::f_rhs(fml)

    steps <- if (rlang::is_call(rhs, "+")) flatten_plus_calls(rhs) else list(rhs)
    steps <- map(steps, expr_to_step)

    Search_Preparer(col, steps)
  })

  names(preparers) <- map_chr(preparers, function(p) p@column)
  preparers
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
#' @param max_token_df Numeric scalar specifying the maximum raw document
#'   frequency a token may have within its `(block, column)` to be kept. Tokens
#'   appearing in more than `max_token_df` records are dropped *before* the
#'   token-overlap join, so a single hyper-common token (a house number,
#'   `STRASSE`) can't fan out a block even at `min_rarity = 0`. The blunt
#'   document-frequency companion to the rarity-metric `min_rarity`; the two
#'   cut on different axes and compose. Default is `Inf` (off). See
#'   [rarity_distribution()] to choose a value from the token distribution.
#' @param threshold Numeric scalar specifying the minimum relative identification
#'   potential required for two records to be considered matches. Default is `0.9`.
#' @param smoothing A [Smoothing] object created by one of the
#'   [smooth_rip] helpers that controls how rIP values are smoothed before
#'   scoring. Default is [smooth_rip_identity()].
#' @param max_candidates Numeric scalar specifying the maximum number of candidate
#'   matches to retain per record. Default is `Inf` (no limit). When finite,
#'   only the top `max_candidates` highest scoring matches are kept per record.
#' @param max_fanout Numeric scalar budgeting the estimated number of
#'   intermediate token-overlap-join rows (`sum df^2` for dedup, `sum
#'   df_base*df_target` for search). The always-on guard against a hot or
#'   boilerplate token fanning one block into a quadratic join (the failure mode
#'   `min_rarity` / `max_token_df` also address, but here by default). Estimated
#'   cheaply from the token document-frequency histogram before the join — no
#'   pairs are materialised. Default `5e7`; set `Inf` (or `on_fanout = "off"`)
#'   to disable. See [rarity_distribution()] / [plan_strategy()] to choose a value.
#' @param on_fanout What to do when the estimated fan-out exceeds `max_fanout`:
#'   `"cap"` (default) auto-drops the smallest set of hyper-common tokens needed
#'   to get under budget — they carry near-zero rarity, so scores barely move —
#'   and emits a loud warning naming what was dropped; `"abort"` stops with an
#'   actionable error instead; `"off"` disables the guard entirely.
#' @param feedback_strength Numeric scalar controlling feedback weighted scoring.
#'   Default is `0` (disabled). Positive values adjust scores based on the
#'   proportion of matched tokens.
#' @param on_missing How to score a pair when a weighted column is **empty on
#'   both records**. With `"penalise"` (default) the column still counts against
#'   the score. For example, if `Strasse` has weight 0.3 and a record's street
#'   is blank, that record's score can never rise above 0.7, so a threshold of
#'   0.8 will never match it even on a perfect name. `"renormalise"` removes
#'   that ceiling: it spreads the weight of any column blank on *both* sides
#'   across the columns that are present (a column present on only one side
#'   still counts as a genuine mismatch). This is powerful but aggressive, since
#'   it turns a record with no street into a name-only matcher, so it is opt-in
#'   and never the default. If you mainly want to handle empty columns, the
#'   safer route is to run an [exact_strategy()] stage first, whose matches do
#'   not depend on weights or thresholds.
#'
#' @return A [Search_Strategy] object.
#'
#' @export
search_strategy <- function(...,
                            block_by   = NULL,
                            weights    = numeric(),
                            rarity     = "inverse_freq",
                            min_rarity = 0,
                            max_token_df = Inf,
                            threshold  = 0.9,
                            smoothing  = smooth_rip_identity(),
                            max_candidates = Inf,
                            max_fanout = 5e7,
                            on_fanout = c("cap", "abort", "off"),
                            feedback_strength = 0,
                            on_missing = c("penalise", "renormalise")) {

  on_missing <- match.arg(on_missing)
  on_fanout  <- match.arg(on_fanout)
  check_number_decimal(min_rarity, min = 0)
  check_number_decimal(max_token_df, min = 1, allow_infinite = TRUE)
  check_number_decimal(max_fanout, min = 1, allow_infinite = TRUE)
  check_number_decimal(threshold)
  check_number_decimal(max_candidates, min = 0, allow_infinite = TRUE)
  if (is.finite(max_candidates) && max_candidates <= 0) {
    cli::cli_abort("{.arg max_candidates} must be positive")
  }
  check_number_decimal(feedback_strength, min = 0)

  if (!S7_inherits(smoothing, Smoothing)) {
    cli::cli_abort("{.arg smoothing} must be a {.cls Smoothing} object created by a {.fn smooth_rip_*} helper")
  }

  preparers <- .parse_strategy_formulas(rlang::list2(...))

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
    max_token_df = max_token_df,
    smoothing  = smoothing,
    max_candidates = max_candidates,
    max_fanout = max_fanout,
    on_fanout  = on_fanout,
    feedback_strength = feedback_strength,
    on_missing = on_missing
  )
}
