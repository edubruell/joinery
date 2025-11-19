
#' Step Class
#'
#' @description
#' An S7 class representing a single preprocessing step with a function name
#' and its arguments in joinery's IR.
#'
#' @slot name A character scalar identifying the step function (e.g., "normalize_text").
#' @slot args A list of arguments to pass to the function.
#'
#' @noRd
Step <- new_class("Step",
                  properties = list(
                    name = class_character,   # e.g., "normalize_text"
                    args = class_list         # list of arguments (quoted)
                  )
)



#' Search Preparer Class
#'
#' @description
#' An S7 class representing the preprocessing definition for a **single column**
#' in a joinery record‐linkage workflow.  
#'
#' A `Search_Preparer` does **not** perform any computation directly.  
#' Instead, it stores:
#'
#' - The **column** name to which preprocessing applies.
#' - An ordered list of of functions in the internal representation  of  joinery 
#'   that will be applied to the column
#'   during the preparation phase (e.g., `normalize_text()`,
#'   `word_tokens()`, `generate_ngrams()`, etc.).
#'
#' The actual execution happens inside backend-specific methods for
#' `prepare_search_data()`.
#'
#' @slot column A character scalar naming the column.
#' @slot steps  A list of functions applied in order.
#'
#' @seealso [search_strategy()]
#'
#' @noRd
Search_Preparer <- new_class("Search_Preparer",
                             properties = list(
                               column = class_character,
                               steps  = class_list
                             )
)

#' Print a Search_Preparer Object
#'
#' @noRd
print.Search_Preparer <- new_external_generic("base", "print", "x")

#' @noRd
method(print.Search_Preparer, Search_Preparer) <- function(x, ...) {
  cat("<joinery::Search_Preparer>\n")
  cat("  Column: ", x@column, "\n", sep = "")
  cat("  Steps:\n")
  
  invisible(imap(x@steps, function(step, idx) {
    
    arg_names <- names(step@args)
    arg_vals  <- map_chr(step@args, ~ deparse(.x, nlines = 1))
    
    # Format arguments
    if (length(step@args) == 0) {
      arg_str <- ""
    } else if (is.null(arg_names) || all(arg_names == "")) {
      # Unnamed args: print positionally
      arg_str <- paste(arg_vals, collapse = ", ")
    } else {
      # Named args: name = value
      formatted <- ifelse(
        arg_names == "",
        arg_vals,                             # unnamed argument
        paste0(arg_names, " = ", arg_vals)    # named argument
      )
      arg_str <- paste(formatted, collapse = ", ")
    }
    
    # Print line
    if (arg_str == "") {
      cat("    - ", idx, ": ", step@name, "()\n", sep="")
    } else {
      cat("    - ", idx, ": ", step@name, "(", arg_str, ")\n", sep="")
    }
  }))
}



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
                               min_rarity = class_any 
                             )
)


#' Print a Search_Strategy Object
#'
#' @noRd
print.Search_Strategy <- new_external_generic("base", "print", "x")


#' @noRd
method(print.Search_Strategy, Search_Strategy) <- function(x, ...) {
  cat("<joinery::Search_Strategy>\n")
  
  # Preparers
  cat("\n  Columns prepared: ", length(x@preparers), "\n", sep = "")
  walk(x@preparers, function(p) {
    cat("    - ", p@column, " (", length(p@steps), " steps)\n", sep = "")
  })
  
  # Weights
  cat("\n  Weights:\n")
  if (length(x@weights) == 0) {
    cat("    (none)\n")
  } else {
    invisible(imap(x@weights, function(w, nm) {
      cat("    - ", nm, ": ", w, "\n", sep = "")
    }))
  }
  
  # Blocking
  cat("\n  Blocking: ")
  if (is.null(x@block_by)) {
    cat("none\n")
  } else {
    cat(paste(x@block_by, collapse = ", "), "\n")
  }
  
  # Rarity
  cat("\n  Rarity Metric: ", x@rarity, "\n", sep = "")
  cat("\n  Min rarity: ", x@min_rarity, "\n", sep="")
  #Threshold
  cat("\n  Threshold: ", if (is.null(x@threshold)) "(none)" else x@threshold, "\n", sep = "")
}


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
#' Creates a `Search_Strategy` object that specifies how columns should be
#' preprocessed for token-index-based record linkage, along with optional weights,
#' blocking variables, rarity computation method, and similarity threshold.
#'
#' @param ... Two-sided formulas of the form `column ~ preprocessing_steps`.
#'   The left-hand side names the column; the right-hand side contains one or
#'   more function calls to apply in sequence (e.g., 
#'   `name ~ normalize_text + word_tokens(min_nchar = 3)`).
#' @param block_by Optional character vector of column names to use for blocking.
#'   Candidate searches will be restricted to records sharing the same blocking
#'   key values. Default is `NULL` (no blocking).
#' @param weights Optional named numeric vector of weights for similarity scoring.
#'   Names should correspond to columns. Default is `numeric()` (uniform weights).
#' @param rarity Character scalar specifying the rarity computation method.
#'   Default is `"inverse_freq"`.
#' @param threshold Numeric scalar specifying the minimum relative indentification 
#'   potential required for two records to be considered matches. Default is `0.9`.
#'
#' @param min_rarity Numeric scalar specifying the minimum rarity value required
#'   for a token to be included in similarity scoring. Tokens with rarity below
#'   this threshold are filtered out. Default is `0`.
#'
#'
#' @return A `Search_Strategy` object.
#'
#' @export
search_strategy <- function(...,
                            block_by = NULL,
                            weights  = numeric(),
                            rarity   = "inverse_freq",
                            min_rarity = 0,
                            threshold = 0.9) {
  
  if (!is.numeric(min_rarity)) {
    rlang::abort("min_rarity  must be numeric.")
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
      rlang::abort("All arguments to search_strategy() must be formulas.")
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
  
  if (!is.null(block_by) && !is.character(block_by)) {
    rlang::abort("block_by must be NULL or a character vector.")
  }
  if (length(weights) > 0 &&
      (is.null(names(weights)) || any(names(weights) == ""))) {
    rlang::abort("weights must be a named numeric vector.")
  }
  if (!is.character(rarity) || length(rarity) != 1) {
    rlang::abort("rarity must be a single character string.")
  }
  
  Search_Strategy(
    preparers = preparers,
    weights   = weights,
    block_by  = block_by,
    rarity    = rarity,
    threshold = threshold,
    min_rarity = min_rarity
    
  )
}


