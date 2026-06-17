#' @keywords internal
#'
#' @section Package options:
#'
#' joinery reads a small number of global options. Set them with
#' [options()]; all have working defaults, so you only touch them to change
#' behaviour.
#'
#' Embedding strategies embed each record once and reuse the vector on later
#' calls, so a multi-stage run does not pay the (expensive) embedding cost again.
#' Two options control that reuse:
#'
#' \describe{
#'   \item{`joinery.embedding_reuse`}{Logical, default `TRUE`. When `TRUE`, the
#'     data.table and tibble backends keep a per-session cache of embedding
#'     vectors keyed by model and record content, and reuse a vector whenever the
#'     same text is embedded again. Set to `FALSE` to embed fresh every time, for
#'     example when benchmarking or using a non-deterministic model. (The DuckDB
#'     backend reuses through its own persisted `embeddings` column and ignores
#'     this option.) The session cache grows as you embed more distinct records
#'     and is only released at the end of the session or by
#'     [clear_embedding_cache()]; call that to reclaim memory in a long-running
#'     session.}
#'   \item{`joinery.embedding_cache_dir`}{Character path, default unset (`NULL`).
#'     When set, the embedding cache also writes each vector to this directory, so
#'     reuse survives across R sessions. When unset, the cache lives only in the
#'     current session. The cache is keyed by record content, so a changed record
#'     re-embeds on its own; clear stale files with
#'     [clear_embedding_cache()] (`disk = TRUE`).}
#' }
#'
#' @seealso [clear_embedding_cache()], [embedding_strategy()]
"_PACKAGE"

## usethis namespace: start
#' @import stringi
#' @rawNamespace import(rlang, except = `:=`)
#' @import data.table
#' @import S7
## usethis namespace: end
NULL

# S7 objects use package-qualified S3 classes ("joinery::ClassName").
# Plain `plot.ClassName` methods don't dispatch for those.
# registerS3method wires them up correctly.
.onLoad <- function(libname, pkgname) {
  S7::methods_register()
  pkg <- asNamespace(pkgname)
  registerS3method("plot", "joinery::Match_Overview",    plot.Match_Overview,    envir = pkg)
  registerS3method("plot", "joinery::Strategy_Audit",    plot.Strategy_Audit,    envir = pkg)
  registerS3method("plot", "joinery::Match_Explanation", plot.Match_Explanation, envir = pkg)
  registerS3method("plot", "joinery::Match_Sample",      plot.Match_Sample,      envir = pkg)
  registerS3method("plot", "joinery::Stage_Comparison",  plot.Stage_Comparison,  envir = pkg)
  registerS3method("plot", "joinery::Embedding_Audit",   plot.Embedding_Audit,   envir = pkg)
  registerS3method("plot", "joinery::Strategy_Plan",      plot.Strategy_Plan,     envir = pkg)
}

# For whoever read this far down the namespace.
#' @noRd
two_joins <- function() {
  cli::cli_text("{.emph two joins in the morning, two joins at night}")
  cli::cli_text("{.emph it makes me feel alright}")
  invisible()
}
