# PURPOSE: Package-level plumbing -- imports, global bindings, load-time options, and the
#   deprecated `%>%` re-export.
# ROLE: No user-facing function lives here; the file exists for its roxygen tags and .onLoad().
# KEY CONSTRAINTS:
#   - The `. = NULL` binding exists only so purrr/rlang `~ .` lambdas pass R CMD check. `.` is
#     NEVER a magrittr placeholder in this package: a stranded one would silently read as a wrong
#     answer instead of erroring.
#   - `%>%` is re-exported for one deprecation cycle only; every internal pipe is `|>`.
# See: CLAUDE.md section ggfacto architecture.

#' Pipe operator (deprecated)
#'
#' Re-exported from magrittr for backward compatibility. Deprecated: use the base pipe
#' \code{|>} instead. ggfacto uses \code{|>} everywhere internally, and this re-export will be
#' removed in a future release, taking the magrittr dependency with it.
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return Pipe an object forward into a function or call expression.
NULL

# Rlang .data to bind data masking variable in dplyr
#' @keywords internal
#' @importFrom rlang .data
NULL

# Bindings for globals R CMD check cannot see. `.` is no longer a magrittr placeholder
# anywhere (the package uses |>), but purrr/rlang `~ .` lambdas still name it.
. = NULL
globalVariables(c(":="))


#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # DESIGN: NOTHING is seeded. `options(x = NULL)` REMOVES an option rather than setting one, and
  #   every reader here states its own default -- getOption("ggfacto.widget_lib_dir", "libs") -- so
  #   "unset" stays distinguishable from "set to the default". The print format is not among them:
  #   a summary obeys `options(tabxplor.print)`, which tabxplor seeds itself.

  # Bind the R-ggfacto gettext catalog to the package's compiled .mo (harmless if absent -> English).
  po <- system.file("po", package = pkgname)
  if (nzchar(po)) try(bindtextdomain("R-ggfacto", po), silent = TRUE)

  # knitr is a soft dependency: register the methods by hand rather than declare them in NAMESPACE,
  # so loading ggfacto without knitr installed still works.
  if (requireNamespace("knitr", quietly = TRUE)) {
    registerS3method("knit_print", "ggfacto_widget", knit_print.ggfacto_widget,
                     envir = asNamespace("knitr"))
    registerS3method("knit_print", "ggfacto_summary", knit_print.ggfacto_summary,
                     envir = asNamespace("knitr"))
  }

  invisible()
}
