# Guards the seam of R/knit.R: a widget must leave the markdown pandoc reads, keep the aspect
# ratio its producer resolved, and keep its JavaScript binding. Every failure mode here is silent
# in a rendered document, which is why they are tested rather than eyeballed.

skip_if_no_render <- function() {
  skip_if_not_installed("knitr")
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("htmlwidgets")
  skip_if(!rmarkdown::pandoc_available("2.0"), "pandoc not available")
}

# The chunk body, self-contained: rmarkdown evaluates it in a fresh environment, so it cannot see
# the suite's fixtures.
WIDGET_CODE <- paste(
  "res <- FactoMineR::CA(as.matrix(tabxplor::tab(forcats::gss_cat, race, marital)), graph = FALSE)",
  "ggfacto::ggi(ggfacto::ggca(res))",
  sep = "\n"
)

# Render a document holding the chunks given as `chunks` (a character vector of full chunk blocks),
# and return its html plus the directory rmarkdown put the auxiliary files in.
render_widget_doc <- function(chunks, widget_dir = "auto", .env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  rmd <- file.path(dir, "doc.Rmd")

  writeLines(c("---", "title: t",
               "output:", "  html_document:", "    self_contained: false", "---", "",
               chunks), rmd)

  # rmarkdown::html_document puts its libraries in "<name>_files", where bookdown uses "libs".
  withr::local_options(list(ggfacto.widget_dir = widget_dir,
                            ggfacto.widget_lib_dir = "doc_files"), .local_envir = .env)
  suppressWarnings(rmarkdown::render(rmd, quiet = TRUE, envir = new.env(parent = globalenv())))

  list(html = paste(readLines(file.path(dir, "doc.html"), warn = FALSE), collapse = "\n"),
       files = file.path(dir, "doc_files", "figure-html"))
}

chunk <- function(label, opts = "") {
  c(paste0("```{r ", label, if (nzchar(opts)) paste0(", ", opts), "}"), WIDGET_CODE, "```", "")
}


test_that("with the option set, the payload leaves the document and an iframe replaces it", {
  skip_if_no_render()
  local_null_device()

  res <- render_widget_doc(c(
    chunk("ac-graph", "fig.width = 3, fig.height = 9"),   # fig.* must NOT win over the plot
    chunk("hidden-graph", "results = 'hide'")
  ))

  expect_match(res$html, "<iframe", fixed = TRUE)
  expect_false(grepl('type="application/json"', res$html, fixed = TRUE))
  expect_true(file.exists(file.path(res$files, "widget_ac-graph.html")))

  # Every script the frame loads must resolve, or it shows a blank graph and says nothing. The
  # page points out of its own directory at the document's shared library folder, so this checks
  # the two halves agree: the path ggfacto wrote, and the files the document actually shipped.
  page <- paste(readLines(file.path(res$files, "widget_ac-graph.html"), warn = FALSE),
                collapse = "\n")
  refs <- gsub('^(src|href)="|"$', "", regmatches(
    page, gregexpr('(src|href)="[^"]*"', page))[[1]])
  refs <- refs[startsWith(refs, "..")]
  expect_gt(length(refs), 2L)
  expect_true(all(file.exists(file.path(res$files, refs))),
              info = paste(refs[!file.exists(file.path(res$files, refs))], collapse = ", "))

  # Under a name of its own, so a version clash with the document cannot resolve it away.
  expect_true(any(grepl("ggfacto-girafe-binding", refs)))

  # results = "hide" throws the output away, so a file would only be orphaned.
  expect_false(file.exists(file.path(res$files, "widget_hidden-graph.html")))

  # The invariant that matters for geometrical data analysis: axes that lose their common scale
  # cannot be interpreted, so the ratio comes from the plot and the chunk cannot contradict it.
  ratio <- attr(quietly(ggca(fx_ca())), "height_width_ratio", exact = TRUE)
  expect_false(is.null(ratio))
  got <- regmatches(res$html, regexpr("aspect-ratio:1/[0-9.]+", res$html))
  expect_length(got, 1L)
  expect_equal(as.numeric(sub("aspect-ratio:1/", "", got)), ratio, tolerance = 1e-4)
})


test_that("without the option, nothing changes and the widget stays inline", {
  skip_if_no_render()
  local_null_device()

  res <- render_widget_doc(chunk("ac-inline"), widget_dir = NULL)

  expect_true(grepl('type="application/json"', res$html, fixed = TRUE))
  expect_false(file.exists(file.path(res$files, "widget_ac-inline.html")))
})


test_that("an unlabelled chunk is refused rather than given a name that drifts", {
  # knitr's unnamed-chunk-N shifts as soon as a chunk is added above it, which would silently
  # rename every widget file downstream of the edit.
  skip_if_not_installed("knitr")
  local_null_device()

  w <- quietly(ggi(quietly(ggca(fx_ca()))))
  withr::local_options(list(ggfacto.widget_dir = withr::local_tempdir()))

  # Outside a chunk, opts_current$get("label") is NULL -- the same condition as an unlabelled one.
  expect_error(knit_print.ggfacto_widget(w), "needs a label")
})
