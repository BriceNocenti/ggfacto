# PURPOSE: build the pkgdown site into docs/, to review it locally. .Rbuildignore'd.
#
#   Rscript dev/build_site.R
#
# ROLE: CI builds the same site (.github/workflows/pkgdown.yaml) and deploys it from main; this is
#   the local twin. The home page, pkgdown/index.md, is knitted from pkgdown/index.Rmd first, with
#   its interactive graph written to pkgdown/assets/ -- neither is committed.
# DESIGN: one English site. The guide (vignettes/articles/ggfacto.Rmd, "Get started") and its French
#   twin (ggfacto-fr.Rmd) are web-only articles, .Rbuildignore'd: the reference pages would be
#   English either way, R shipping no bilingual .Rd.
# WARNING: index.Rmd is knitted in a SUBPROCESS against the working tree (load_all): its setup pins
#   options and LANGUAGE, which must not leak into the site build, and `library(ggfacto)` alone would
#   document whatever version happens to be installed.

stopifnot(requireNamespace("pkgdown", quietly = TRUE), file.exists("DESCRIPTION"))

index <- c(
  'pkgload::load_all(".", quiet = TRUE, export_all = FALSE, helpers = FALSE)',
  'rmarkdown::render("pkgdown/index.Rmd", knit_root_dir = getwd(), quiet = TRUE)'
)
if (system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(paste(index, collapse = "; ")))) != 0L)
  stop("pkgdown/index.md could not be knitted")

# Wipe first: docs/ is never cleaned by a build, so pages of an earlier layout would linger.
pkgdown::clean_site(".")
pkgdown::build_site(".", devel = FALSE, preview = FALSE)
source("dev/site_prune.R")   # CLAUDE.md is not a site page -- see that file
message("done. Site: docs/index.html")
