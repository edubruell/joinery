# ============================================================
# Step and Search_Preparer classes
# ============================================================
#
# Foundations every strategy builds on:
# - Step: a single preprocessing operation (function name + quoted args)
# - Search_Preparer: an ordered pipeline of Steps for one column
# ============================================================


# ---------------------------------------------------------------------------
# Step class
# ---------------------------------------------------------------------------

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


# ---------------------------------------------------------------------------
# Search_Preparer class
# ---------------------------------------------------------------------------

#' Search Preparer Class
#'
#' @description
#' An S7 class representing the preprocessing definition for a **single column**
#' in a joinery record-linkage workflow.
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
