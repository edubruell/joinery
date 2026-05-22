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

  id <- cli::cli_progress_bar(
    total = NA,
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
