# PURPOSE: the gettext plumbing -- the text domain, its cache flush, the language resolver, and the
#   scope that makes `lang =` an argument rather than an accident of the session.
# ROLE: everything ggfacto RENDERS in words goes through here: the tables, and the tooltips and axis
#   titles of the graphs, whose builders and renderer run inside with_gda_lang(). Its model is tabxplor's
#   (R/fmt_class.R: legend_resolve_lang / flush_gettext_cache / with_legend_lang), deliberately, so
#   two packages whose tables sit side by side speak with one voice.
# KEY CONSTRAINTS:
#   - WHAT IS TRANSLATED IS WHAT A READER READS AS PROSE: the legend lines, the axis heading, the
#     summary row's label, "Rows" / "Columns" / "Total". NOT a column name, and not a `col_var` --
#     those become the tibble's own names ("coord_Axe 1"), and a name that changed with the language
#     could not be indexed. It is tabxplor's rule too (dev/french_glossary.md).
#   - THE CACHE FLUSH MUST RE-BIND OUR OWN DOMAIN. glibc keys its translation cache on
#     (domain, msgid), so tabxplor's flush leaves "R-ggfacto" cached and the second language switch
#     of a session silently no-ops.
#   - A TRANSLATABLE STRING IS NEVER A TOP-LEVEL LITERAL: gettext() must run at render, or the build
#     locale is frozen into the package. Every one of them lives inside a function.
#   - ONE STRING LITERAL PER gettextf() CALL, and no edge whitespace inside gettext(): potools
#     extracts what it SEES, gettext() looks up what was EVALUATED, and xgettext strips edges.
# See: dev/update_translations.R for the workflow, and ~/github/tabxplor/dev/french_glossary.md for
#   the French terminology the two packages share.

#' @keywords internal
#' @noRd
GGFACTO_DOMAIN <- "R-ggfacto"

# Flush gettext's cache of already-translated strings, so a mid-session LANGUAGE change is honoured.
# ⚠ IT MUST RE-BIND OUR OWN DOMAIN -- see the file header.
#' @keywords internal
#' @noRd
flush_gettext_cache <- function() {
  try({
    po <- system.file("po", package = "ggfacto")
    bindtextdomain(GGFACTO_DOMAIN, tempdir())
    if (nzchar(po)) bindtextdomain(GGFACTO_DOMAIN, po)
  }, silent = TRUE)
  invisible(NULL)
}

# NULL / "auto" -> the session's message language; "fr" / "en" -> that one.
#' @keywords internal
#' @noRd
gda_resolve_lang <- function(lang = NULL) {
  if (is.null(lang) || identical(lang, "") || identical(lang, "auto"))
    lang <- getOption("ggfacto.lang", getOption("tabxplor.lang", "auto"))
  lang <- tolower(as.character(lang)[1])
  if (lang %in% c("fr", "french", "francais", "fran\u00e7ais")) return("fr")
  if (lang %in% c("en", "english"))                            return("en")
  # auto: the MESSAGE-language signals first (an English R on a French system must stay English).
  src <- c(Sys.getenv("LANGUAGE"), Sys.getlocale("LC_MESSAGES"), Sys.getenv("LC_MESSAGES"),
           Sys.getenv("LANG"), Sys.getenv("LC_ALL"))
  src <- src[nzchar(src)]
  probe <- if (length(src)) src[1] else Sys.getlocale("LC_CTYPE")
  if (grepl("(^|[^a-z])fr|franc", probe, ignore.case = TRUE)) "fr" else "en"
}

# Run `f(lg)` with LANGUAGE set for the gettext lookups, flushing the cache on both sides. Every
# interpret function wraps its WHOLE body in this: the words it renders are built with the table, so
# the language must be decided before the first one is written.
#' @keywords internal
#' @noRd
with_gda_lang <- function(lang, f) {
  lg  <- gda_resolve_lang(lang)
  old <- Sys.getenv("LANGUAGE", unset = NA_character_)
  flush_gettext_cache(); Sys.setenv(LANGUAGE = lg); flush_gettext_cache()
  on.exit({
    if (is.na(old)) Sys.unsetenv("LANGUAGE") else Sys.setenv(LANGUAGE = old)
    flush_gettext_cache()
  }, add = TRUE)
  f(lg)
}

# French writes a decimal comma. The rest of French typography (the thin space before `:` and `%`)
# is the translator's, in the msgstr, where it belongs.
#' @keywords internal
#' @noRd
gda_num <- function(v, lg) {
  s <- trimws(format(v, trim = TRUE))
  if (identical(lg, "fr")) s <- gsub("[.]", ",", s)
  s
}
