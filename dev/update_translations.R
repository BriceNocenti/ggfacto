# dev/update_translations.R -- the sanctioned ggfacto R-message translation workflow.
# .Rbuildignore'd. Run from the package root after adding/changing any gettext()/gettextf() string.
#
#   Rscript dev/update_translations.R          # extract -> normalise -> merge -> compile
#
# Domain "R-ggfacto". Sources of truth:
#   po/R-ggfacto.pot   extraction template (regenerated here)
#   po/R-fr.po         French catalogue (translators fill the msgstr; NEVER auto-overwrite by hand)
#   inst/po/fr/LC_MESSAGES/R-ggfacto.mo   compiled catalogue (committed; what the package loads)
#
# Copied from ~/github/tabxplor/dev/update_translations.R, domain apart -- the two packages' tables
# sit side by side, so their workflow and their French terminology are one
# (see ~/github/tabxplor/dev/french_glossary.md).
#
# WHY the norm_u() step: potools extracts R string tokens verbatim, so a non-ASCII msgid written as a
# \uXXXX escape in R source (required by the ASCII-source rule, test-non-ascii.R) lands in the
# catalogue as a LITERAL backslash-u sequence -- which R's runtime gettext (which passes the
# *evaluated* character) never matches. norm_u() rewrites those escapes to the real UTF-8 character
# so the .mo key == the runtime lookup. Idempotent; safe to re-run.
#
# NOTE: potools cannot see gettext(variable). Every translatable string in ggfacto is a LITERAL inside
# a function, so no extraction anchor is needed -- keep it that way.

stopifnot(requireNamespace("potools", quietly = TRUE))
dir <- normalizePath(".")

norm_u <- function(f) {
  if (!file.exists(f)) return(invisible())
  x <- readLines(f, encoding = "UTF-8", warn = FALSE)
  m <- gregexpr("\\\\\\\\u[0-9a-fA-F]{4}", x, perl = TRUE)     # "\\uXXXX" (two backslash bytes)
  regmatches(x, m) <- lapply(regmatches(x, m), function(h)
    vapply(h, function(one) intToUtf8(strtoi(sub("^\\\\\\\\u", "", one), 16L)), character(1)))
  writeLines(enc2utf8(x), f, useBytes = TRUE)
}

message("1/4  extract  -> po/R-ggfacto.pot")
potools::po_extract(dir)

message("2/4  normalise \\uXXXX escapes -> real UTF-8 (msgids must match runtime gettext)")
norm_u(file.path(dir, "po", "R-ggfacto.pot"))

message("3/4  merge     -> po/*.po (preserves existing msgstr, flags new/obsolete)")
if (!length(list.files(file.path(dir, "po"), pattern = "^R-fr[.]po$")))
  potools::po_create("fr", dir)
potools::po_update(dir)
for (po in list.files(file.path(dir, "po"), pattern = "\\.po$", full.names = TRUE)) norm_u(po)

message("4/4  compile   -> inst/po/*/LC_MESSAGES/R-ggfacto.mo")
potools::po_compile(dir)

message("done. Review po/R-fr.po (fill any blank msgstr, resolve fuzzies), then re-run to compile.")
