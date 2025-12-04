
#' Validate Input Conditions for R Functions
#'
#' This internal function validates specified conditions for function inputs and stops the function execution if any condition is not met. It uses a named vector of predicates where each name is the error message associated with the predicate condition.
#'
#' @param .predicates A named vector where each element is a logical condition and the name of each element is the corresponding error message to be displayed if the condition is FALSE.
#' @return None; the function will stop execution and throw an error if a validation fails.
#' @examples
#' validate_inputs(c(
#'   "Input must be numeric" = is.numeric(5),
#'   "Input must be integer" = 5 == as.integer(5)
#' ))
#' @noRd
validate_inputs <- function(.predicates) {
  # Use lapply to iterate over predicates and stop on the first failure
  results <- lapply(names(.predicates), function(error_msg) {
    if (!.predicates[[error_msg]]) {
      stop(error_msg)
    }
  })
}


#' Initialize a CLI progress indicator
#'
#' Creates either a determinate progress bar (when `total` is known) or an
#' indeterminate spinner (when `total` is NULL). This keeps the UX predictable
#' for pipelines where total work cannot be precomputed.
#'
#' @param total Optional integer. If NULL or NA, a spinner is created.
#' @param .envir Environment for cli bookkeeping.
#'
#' @return A list with `type` ("bar" or "spinner") and `id`.
#'
#' @noRd
progress_init <- function(total = NULL, .envir = parent.frame()) {
  if (!is.null(total) && is.finite(total)) {
    id <- cli::cli_progress_bar(
      total = total,
      clear = FALSE,
      .auto_close = FALSE,
      .envir = .envir
    )
    return(list(type = "bar", id = id))
  }
  
  id <- cli::cli_progress_spinner(
    clear = FALSE,
    .auto_close = FALSE,
    .envir = .envir
  )
  list(type = "spinner", id = id)
}


#' Update a CLI progress indicator
#'
#' @param pb Progress handle from `progress_init()`.
#' @param amount Integer increment. Used only for determinate bars.
#' @param .envir Environment for cli bookkeeping.
#'
#' @noRd
progress_update <- function(pb, amount = 1L, .envir = parent.frame()) {
  if (is.null(pb)) return(invisible(NULL))
  if (pb$type == "bar") {
    cli::cli_progress_update(id = pb$id, inc = amount, .envir = .envir)
  } else {
    cli::cli_progress_update(id = pb$id, .envir = .envir)
  }
}


#' Finalize a CLI progress indicator
#'
#' @param pb Progress handle.
#' @param .envir Environment for cli bookkeeping.
#'
#' @noRd
progress_finish <- function(pb, .envir = parent.frame()) {
  if (is.null(pb)) return(invisible(NULL))
  cli::cli_progress_done(id = pb$id, .envir = .envir)
}

