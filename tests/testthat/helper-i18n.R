# Whether a mid-session LANGUAGE switch can translate is a property of the ENVIRONMENT, not of
# ggfacto: R must be built with NLS, and GNU gettext ignores LANGUAGE entirely when the message
# locale is "C"/"POSIX". That is exactly the state under R CMD check on Linux (check.R forces
# LANGUAGE=en, and testthat's local_reproducible_output() sets LANG/LANGUAGE to "C" for every
# test_that() block) -- and on the CRAN farm. So every French assertion is guarded by this, and each
# i18n feature is tested TWICE: an unguarded English block, plus a guarded French one.
skip_if_no_gettext <- function() {
  # 1. the compiled catalog is installed (a fresh checkout before dev/update_translations.R has none)
  po <- system.file("po", "fr", "LC_MESSAGES", "R-ggfacto.mo", package = "ggfacto")
  testthat::skip_if(!nzchar(po) || !file.exists(po), "R-ggfacto fr catalog not compiled")

  # 2. R itself can do native language support
  testthat::skip_if_not(capabilities("NLS"), "R built without NLS")

  # 3. a LANGUAGE switch actually reaches the catalog here
  can_translate <- function() {
    old <- Sys.getenv("LANGUAGE", unset = NA_character_)
    on.exit({
      if (is.na(old)) Sys.unsetenv("LANGUAGE") else Sys.setenv(LANGUAGE = old)
      ggfacto:::flush_gettext_cache()
    }, add = TRUE)
    ggfacto:::flush_gettext_cache()
    Sys.setenv(LANGUAGE = "fr")
    ggfacto:::flush_gettext_cache()
    # ⚠ the probe msgid must be one the package actually still emits: a retired msgid would make
    # every French test SKIP instead of fail. "Above mean ctr" is the summary row's own label.
    sentinel <- "Above mean ctr"
    !identical(gettext(sentinel, domain = "R-ggfacto"), sentinel)
  }
  testthat::skip_if_not(can_translate(), "gettext cannot honour LANGUAGE here (locale is C)")
}
