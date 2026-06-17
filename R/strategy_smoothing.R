# ============================================================
# Smoothing classes and smooth_rip_*() constructors
# ============================================================
#
# Configures how relative identification potential (rIP) is transformed
# within each record and column before scoring. Backends inspect the
# `method` slot and additional parameters when applying the transform.
# ============================================================


#' rIP Smoothing Configuration
#'
#' @description
#' Base S7 class that describes how relative identification potential (rIP)
#' should be smoothed within a record and column during scoring.
#'
#' Concrete subclasses implement specific smoothing rules.
#' Backends inspect the `method` slot and possibly additional parameters
#' when transforming rIP values before scoring.
#'
#' @slot method A character scalar describing the smoothing method.
#'
#' @noRd
Smoothing <- new_class(
  "Smoothing",
  properties = list(
    method = class_character
  )
)

#' Identity rIP Smoothing (no transformation)
#'
#' @description
#' S7 class that represents the default behaviour where rIP values are left
#' unchanged apart from the usual normalization inside each record and column.
#'
#' This is the default for [search_strategy()] and behaves as if no smoothing
#' was configured.
#'
#' @noRd
Smoothing_None <- new_class(
  "Smoothing_None",
  parent = Smoothing,
  properties = list()
)

#' Log rIP Smoothing
#'
#' @description
#' S7 class that represents log based rIP smoothing.
#' Typical backends will apply a transformation of the form
#' `rIP := log1p(rIP)` followed by renormalization within each record and column.
#'
#' This reduces the dominance of very large rIP values while keeping the
#' relative ordering of tokens similar.
#'
#' @noRd
Smoothing_Log <- new_class(
  "Smoothing_Log",
  parent = Smoothing,
  properties = list()
)

#' Offset rIP Smoothing
#'
#' @description
#' S7 class that represents offset based rIP smoothing with a constant offset
#' parameter `alpha`.
#' Typical backends will apply `rIP := rIP + alpha` followed by renormalization
#' within each record and column.
#'
#' This can slightly lift very small rIP values and compress the range of rIP.
#'
#' @slot alpha Numeric scalar giving the offset to add before renormalization.
#'
#' @noRd
Smoothing_Offset <- new_class(
  "Smoothing_Offset",
  parent = Smoothing,
  properties = list(
    alpha = class_numeric
  )
)

#' Softmax rIP Smoothing
#'
#' @description
#' S7 class that represents softmax style rIP smoothing with a temperature
#' parameter.
#' Typical backends will compute
#' `rIP := exp(rIP / temperature) / sum(exp(rIP / temperature))`
#' within each record and column.
#'
#' Smaller `temperature` values sharpen the distribution; larger values
#' flatten it.
#'
#' @slot temperature Numeric scalar controlling the softness of the transform.
#'
#' @noRd
Smoothing_Softmax <- new_class(
  "Smoothing_Softmax",
  parent = Smoothing,
  properties = list(
    temperature = class_numeric
  )
)


#' rIP Smoothing Helpers
#'
#' @name smooth_rip
#' @rdname smooth_rip
#' @title Configure rIP smoothing for a search strategy
#'
#' @description
#' Helper functions that construct S7 `Smoothing` objects used by
#' [search_strategy()] to control how relative identification potential (rIP)
#' is smoothed before scoring.
#'
#' All helpers are pure configuration; they do not perform any computation
#' by themselves. Backend methods for `detect_duplicates()` and
#' `search_candidates()` interpret the resulting `Smoothing` object.
#'
#' @return An object inheriting from `Smoothing` that can be passed to
#'   the `smoothing` argument of [search_strategy()].
#'
#' @seealso [search_strategy()]
NULL

#' @describeIn smooth_rip Identity rIP smoothing (no transformation beyond
#'   standard per record normalization). This is the default.
#'
#' @export
smooth_rip_identity <- function() {
  Smoothing_None(method = "none")
}

#' @describeIn smooth_rip Logarithmic rIP smoothing.
#'   Backends typically apply `log1p(rIP)` and then renormalize within
#'   each record and column.
#'
#' @export
smooth_rip_log <- function() {
  Smoothing_Log(method = "log")
}

#' @describeIn smooth_rip Offset based rIP smoothing with a constant offset
#'   `alpha` that is added to all rIP values before renormalization.
#'
#' @param alpha Numeric scalar; offset that is added to rIP values prior to
#'   normalization. Must be non negative.
#'
#' @export
smooth_rip_offset <- function(alpha = 0.5) {
  check_number_decimal(alpha, min = 0)
  Smoothing_Offset(method = "offset", alpha = alpha)
}

#' @describeIn smooth_rip Softmax style rIP smoothing with a temperature
#'   parameter that controls how sharp or flat the transformed distribution is.
#'
#' @param temperature Numeric scalar; softmax temperature parameter.
#'   Must be strictly positive.
#'
#' @export
smooth_rip_softmax <- function(temperature = 1) {
  check_number_decimal(temperature, min = 0)
  if (temperature <= 0) {
    cli::cli_abort("{.arg temperature} must be strictly positive")
  }
  Smoothing_Softmax(method = "softmax", temperature = temperature)
}
