
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

