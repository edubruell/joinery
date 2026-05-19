#' @keywords internal
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
}
