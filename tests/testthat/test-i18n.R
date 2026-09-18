# PURPOSE: the French catalogue reaches what a reader reads, and nothing else.
# ROLE: the shipped contract of R/i18n.R. Every feature is asserted twice -- an UNGUARDED English
#   block (which also proves no accidental global switch) and a GUARDED French twin.
# KEY CONSTRAINTS:
#   - ⚠ A green local suite does not mean a green CI: this box is fr_FR.UTF-8, and R CMD check forces
#     LANGUAGE=en with a C message locale, where gettext cannot translate at all. Hence
#     skip_if_no_gettext() on every French assertion (helper-i18n.R).
#   - WHAT IS TRANSLATED IS PROSE, never a name: a column name or a col_var becomes the tibble's own
#     name, and one that changed with the language could not be indexed.
#   - French strings are written as \uXXXX escapes here too (test-non-ascii.R covers tests/).
# See: R/i18n.R, dev/update_translations.R.

fx_axes <- function(...) mca_interpret(fx_mca(), axes = 1, ...)

test_that("the axis heading, the summary row and the footer stay English when asked in English", {
  t <- fx_axes(complete = TRUE, lang = "en")
  expect_match(as.character(t$Axe)[1], "of variance", fixed = TRUE)
  expect_true("Above mean ctr" %in% as.character(t$Question))
  # the legend's WORDS are set at build; the sentence around them is tabxplor's, built at render
  foot <- tabxplor::tab_footer_text(t, lang = "en")
  expect_match(foot[[1]], "[Cc]ontribution to the variance of the axis")
  expect_true(any(startsWith(foot, "contrib: ")))
  # ... and no French leaked in (which is what an accidental global switch would look like)
  expect_false(any(grepl("de variance", as.character(t$Axe), fixed = TRUE)))
  expect_false(any(grepl("[Cc]ontribution \u00e0 la variance", foot)))
})

test_that("lang = 'fr' translates the prose, and the decimal comma with it", {
  skip_if_no_gettext()
  t <- fx_axes(complete = TRUE, lang = "fr")
  expect_match(as.character(t$Axe)[1], "de variance", fixed = TRUE)
  expect_match(as.character(t$Axe)[1], ",", fixed = TRUE)          # 23,4 -- gda_num()
  expect_true("Contrib. > moyenne" %in% as.character(t$Question))
  foot <- tabxplor::tab_footer_text(t, lang = "fr")
  expect_true(any(grepl("[Cc]ontribution \u00e0 la variance de l'axe", foot)))
  expect_true(any(grepl("qualit\u00e9 de repr\u00e9sentation", foot)))
  # it is genuinely the ARGUMENT, not the ambient locale
  expect_false(identical(as.character(t$Axe)[1], as.character(fx_axes(lang = "en")$Axe)[1]))
})

test_that("a column name is never translated, whatever the language", {
  # A name that changed with the language could not be indexed -- and it is tabxplor's rule too.
  skip_if_no_gettext()
  en <- pca_interpret(fx_pca(), axes = 1, lang = "en")
  fr <- pca_interpret(fx_pca(), axes = 1, lang = "fr")
  expect_identical(names(en), names(fr))
  expect_true(all(c("coord_Axe 1", "contrib_Axe 1", "cos2_Axe 1") %in% names(fr)))
  expect_identical(names(mca_interpret(fx_mca(), axes = 1, lang = "fr")),
                   names(mca_interpret(fx_mca(), axes = 1, lang = "en")))
})

test_that("a CA's margins translate, and the threshold label follows the threshold in French too", {
  skip_if_no_gettext()
  expect_true(all(c("Lignes", "Colonnes") %in%
                    as.character(ca_interpret(fx_ca(), lang = "fr")$Variable)))
  expect_true("Toutes modalit\u00e9s" %in%
                as.character(fx_axes(min_contrib = 0, lang = "fr")$Question))
  expect_true(any(grepl("^Contrib[.] > 5",
                        as.character(fx_axes(min_contrib = 5, lang = "fr")$Question))))
})

test_that("the language switch is scoped: it leaves LANGUAGE as it found it", {
  # with_gda_lang() sets a process-wide environment variable; a leak would make every later table of
  # the session French.
  old <- Sys.getenv("LANGUAGE", unset = NA_character_)
  invisible(fx_axes(lang = "fr"))
  new <- Sys.getenv("LANGUAGE", unset = NA_character_)
  expect_identical(old, new)
})


test_that("the clustering tree states its cut in English", {
  words <- ggfacto:::clust_tree_caption(6, 0.686, 3, lang = "en")
  expect_identical(words[["caption"]],
                   "6 clusters: the between-cluster inertia is 68.6% of the inertia of axes 1 to 3")
  expect_identical(words[["title"]], "Hierarchical clustering")
  expect_match(ggfacto:::clust_tree_caption(4, 0.5, 1, lang = "en")[["caption"]], "of axis 1$")
})

test_that("... and in French, with the course's words", {
  skip_if_no_gettext()
  words <- ggfacto:::clust_tree_caption(6, 0.686, 3, lang = "fr")
  expect_identical(words[["caption"]], paste0(
    "6 classes\u202f: la variance inter repr\u00e9sente 68,6\u202f% de la variance des ",
    "axes 1 \u00e0 3"))
  expect_identical(words[["title"]], "Classification ascendante hi\u00e9rarchique")
})
