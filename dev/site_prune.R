# PURPOSE: drop from a built site the pages that are not site pages.
# ROLE: sourced right after the build -- by dev/build_site.R locally, and by the "Build site" step of
#   .github/workflows/pkgdown.yaml on CI, which guards the call with file.exists(): dev/ is not on
#   main, where CLAUDE.md is gone too and there is nothing to prune. Run it from the package root.
# WHY: pkgdown renders EVERY root *.md it does not already handle itself (see pkgdown:::package_mds:
#   only README / NEWS / LICENCE / cran-comments are spared), so CLAUDE.md -- the maintainer's guide
#   and roadmap -- becomes docs/CLAUDE.html, is listed in sitemap.xml, and is indexed section by
#   section in the site's search box; pkgdown >= 2.2 also copies it as docs/CLAUDE.md, for llms.txt.
#   pkgdown offers no way to exclude it, so the only lever is here.
# WARNING: rebuild the search index rather than editing it -- build_search() reads the html actually
#   present in docs/, so deleting the page first is what keeps the two in step.

local({
  private <- c("CLAUDE.html", "CLAUDE.md")
  if (!dir.exists("docs")) stop("run from the package root, after the site is built")

  unlink(file.path("docs", private))
  pkgdown::build_search(".")

  sitemap <- file.path("docs", "sitemap.xml")
  if (file.exists(sitemap)) {
    lines <- readLines(sitemap, warn = FALSE)
    writeLines(lines[!grepl("/CLAUDE.html</loc>", lines, fixed = TRUE)], sitemap)
  }
  message("pruned from the site: ", paste(private, collapse = ", "))
})
