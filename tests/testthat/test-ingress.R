# PURPOSE: Lock the ingress -- the entry points that compute an analysis, and what the analysis
#   remembers of its input: the `<VAR>.NA` levels, the `excl` rule, and the rows it was fitted on.
# ROLE: multiple_correspondence_analysis(), principal_component_analysis() and
#   correspondence_analysis() are the only functions that compute an analysis; R/ingress.R is what
#   every later function uses to take the microdata back (the other half: test-clust.R).
# KEY CONSTRAINTS:
#   - What is asserted is the FITTED OBJECT's slots, because everything downstream reads those
#     directly ($call$X, $call$row.w, $call$excl, $call$quali, $source). They are the real interface.
#   - The source-row contract is pinned in both directions: every pipe shape that CAN be proved is
#     recorded, and every one that cannot falls back to "the fitted rows only" -- never a guess.
# See: CLAUDE.md section ggfacto architecture > The FactoMineR contract.

# --- the names ----------------------------------------------------------------------------------

test_that("the short names are the same functions as the long ones", {
  expect_identical(MCA2, multiple_correspondence_analysis)
  expect_identical(PCA2, principal_component_analysis)
})

# --- active_vars accepts what tidyselect accepts ------------------------------------------------

test_that("MCA2 takes active_vars by position, by name and by all_of()", {
  # The three spellings the examples and the README use must select the same six columns.
  by_index <- MCA2(fx_tea(), 1:6, ncp = 2)
  by_name  <- MCA2(fx_tea(), tidyselect::all_of(fx_active()), ncp = 2)

  expect_identical(colnames(by_index$call$X), colnames(by_name$call$X))
  expect_identical(by_index$call$quali, by_name$call$quali)
  expect_equal(by_index$eig, by_name$eig)
})

test_that("MCA2 keeps the active variables as the first columns of $call$X", {
  # Four separate places re-derive active_vars as `colnames($call$X)[1:length($call$quali)]`.
  # That arithmetic is only correct while the active variables come first.
  res <- fx_mca()
  expect_identical(colnames(res$call$X)[seq_along(res$call$quali)], fx_active())
})

test_that("PCA2 takes active_vars by position and by name alike", {
  by_index <- PCA2(mtcars, 1:7)
  by_name  <- PCA2(mtcars, tidyselect::all_of(names(mtcars)[1:7]))
  expect_equal(by_index$eig, by_name$eig)
})

# --- weights ------------------------------------------------------------------------------------

test_that("MCA2 passes wt through to row.w unchanged", {
  # The weight is normalised by FactoMineR, so what must hold is proportionality to the input and
  # the fact that it is recoverable from the fitted object at all.
  res <- fx_mca_wt()
  expect_length(res$call$row.w, nrow(fx_tea_wt()))
  expect_equal(res$call$row.w / sum(res$call$row.w),
               fx_tea_wt()$w / sum(fx_tea_wt()$w))
})

test_that("an unweighted MCA2 still exposes a row.w, so downstream code has one channel", {
  # tooltips.R always reads res.mca$call$row.w; it must exist even with no wt =.
  expect_length(fx_mca()$call$row.w, nrow(fx_tea()))
  expect_true(all(fx_mca()$call$row.w > 0))
})

test_that("weighting changes the coordinates it is supposed to change", {
  # Guards against wt = being silently dropped, which would look like success everywhere else.
  expect_false(isTRUE(all.equal(unname(fx_mca()$var$coord),
                                unname(fx_mca_wt()$var$coord))))
})

test_that("PCA2 accepts wt and reaches row.w", {
  d <- mtcars[1:7]
  d$w <- rep(c(0.5, 1.5), length.out = nrow(d))
  res <- PCA2(d, active_vars = 1:7, wt = "w")
  expect_length(res$call$row.w, nrow(d))
  expect_equal(res$call$row.w / sum(res$call$row.w), d$w / sum(d$w))
})

test_that("a zero weight leaves its row out, and the whole data frame still aligns", {
  # FactoMineR's MCA() crashes on a zero weight; in a survey it marks an out-of-scope row.
  d <- fx_tea(); d$w <- c(0, 0, rep(1, nrow(d) - 2))
  expect_message(res <- MCA2(d, 1:6, wt = "w"), "2 row")
  expect_equal(nrow(res$call$X), nrow(d) - 2)
  expect_identical(res$source$rows, 3:nrow(d))
  clust <- dplyr::mutate(d, cl = hierarchical_clust(res, ncp = 2, nb_clust = 3, tree = FALSE))$cl
  expect_true(all(is.na(clust[1:2])) && !anyNA(clust[-(1:2)]))
  expect_no_error(quietly(ggmca(res, d, sup_vars = "SPC")))
})

test_that("a missing or negative weight is refused in words", {
  d <- fx_tea(); d$w <- c(NA, rep(1, nrow(d) - 1))
  expect_error(MCA2(d, 1:6, wt = "w"), "positive or zero")
  d$w[1] <- -1
  expect_error(MCA2(d, 1:6, wt = "w"), "positive or zero")
})

test_that("a PCA leaves a zero-weight row out, and keeps its supplementary rows", {
  # FactoMineR's PCA() gives a zero-weight row infinite coordinates.
  d <- mtcars[1:7]; d$w <- c(0, rep(1, 31))
  res <- suppressMessages(PCA2(d, 1:7, wt = "w", ind.sup = 30:32))
  expect_true(all(is.finite(res$ind$coord)))
  expect_equal(nrow(res$ind$coord), 28)
  expect_identical(rownames(res$ind.sup$coord), rownames(mtcars)[30:32])
})

# --- missing answers and excl: specific MCA -----------------------------------------------------

excluded <- function(res) names(res$call$Xtot)[res$call$excl]

test_that("a missing answer becomes a level named <VAR>.NA, excluded by default", {
  res <- MCA2(fx_tea_na(), 1:6)
  expect_true("breakfast.NA" %in% levels(res$call$X$breakfast))
  expect_setequal(excluded(res), c("breakfast.NA", "lunch.NA"))
  # `NA` and "NA" are the same request
  expect_identical(MCA2(fx_tea_na(), 1:6, excl = "NA")$call$excl, res$call$excl)
})

test_that("excl names one variable's missing values, or none at all", {
  expect_identical(excluded(MCA2(fx_tea_na(), 1:6, excl = "lunch.NA")), "lunch.NA")
  res <- MCA2(fx_tea_na(), 1:6, excl = NULL)
  expect_length(res$call$excl, 0L)
  expect_true("breakfast.NA" %in% rownames(res$var$coord))
})

test_that("excl matches level names exactly, never as a regex", {
  # "Not." as a regex matched every "Not.*" level; exactly, it matches none, and says so.
  expect_warning(res <- MCA2(fx_tea(), 1:6, excl = "Not."), "no active level")
  expect_length(res$call$excl, 0L)

  # a level holding regex metacharacters is found, and only it
  d <- fx_tea()
  levels(d$breakfast)[1] <- "65 ans et + (oui?)"
  expect_identical(excluded(MCA2(d, 1:6, excl = "65 ans et + (oui?)")), "65 ans et + (oui?)")
  expect_setequal(excluded(MCA2(d, 1:6, excl = c(NA, "Not.lunch", "evening"))),
                  c("Not.lunch", "evening"))
})

test_that("excl reaches FactoMineR as positions, identical to a hand-built specific MCA", {
  # FactoMineR renames a level shared by two variables `var_lv`: positions are the only safe input.
  d   <- ggfacto:::na_levels(as.data.frame(fx_tea_na()[1:6]), fx_active())
  pos <- which(unlist(lapply(d, levels)) %in% c("breakfast.NA", "lunch.NA"))
  ref <- FactoMineR::MCA(d, excl = pos, graph = FALSE, ncp = Inf)
  expect_equal(MCA2(fx_tea_na(), 1:6)$eig, ref$eig)
})

test_that("excl actually removes the excluded levels from the drawn cloud", {
  # The user-visible consequence: an excluded level must not appear among the plotted points.
  plot_data <- md(MCA2(fx_tea(), 1:6, excl = "Not.breakfast", ncp = 2))
  expect_false("Not.breakfast" %in% as.character(plot_data$vars_data$lvs))
  expect_true("Not.lunch" %in% as.character(plot_data$vars_data$lvs))
})

test_that("with no missing answer, the default excl leaves $call$excl empty", {
  expect_length(fx_mca()$call$excl, 0L)
})

# --- source rows: which rows of the named data frame were analysed ----------------------------

src <- function(expr, data) {
  e   <- rlang::enexpr(expr)
  env <- rlang::caller_env()
  ggfacto:::source_rows(e, env, data)
}

test_that("a bare data frame records every row", {
  expect_identical(fx_mca()$source, list(n = nrow(fx_tea()), rows = NULL, wt = NULL, name = NULL))
  tea <- fx_tea()
  expect_identical(MCA2(tea, 1:6)$source$name, "tea")
})

test_that("every provable pipe shape records the rows it kept", {
  d <- fx_tea()
  young <- which(d$age < 30)
  expect_identical(fx_mca_young()$source, list(n = nrow(d), rows = young, wt = NULL, name = "d"))

  expect_identical(MCA2(d[which(d$age < 30), ], 1:6)$source$rows, young)
  expect_identical(MCA2(d[d$age < 30 & !is.na(d$age), ], 1:6)$source$rows, young)
  expect_identical(MCA2(subset(d, age < 30), 1:6)$source$rows, young)
  expect_identical(
    (d |> dplyr::mutate(age2 = age * 2) |> dplyr::filter(age2 < 60) |> MCA2(1:6))$source$rows,
    young)
  # a reordering is a subset too: the rows, in their new order
  expect_identical((d |> dplyr::arrange(age) |> MCA2(1:6))$source$rows,
                   order(d$age, method = "radix"))

  f <- function() {
    local_data <- fx_tea()
    local_data |> dplyr::filter(age < 30) |> MCA2(1:6)
  }
  expect_identical(f()$source$rows, young)
})

test_that("tidyr::drop_na() records the complete rows", {
  d <- fx_tea_na()
  expect_identical((d |> tidyr::drop_na(breakfast) |> MCA2(1:6))$source$rows,
                   which(!is.na(d$breakfast)))
})

test_that("a pipe that cannot be proved records nothing beyond the fitted rows", {
  d <- fx_tea()
  n_young <- sum(d$age < 30)
  fitted_only <- function(res, n) {
    expect_identical(res$source[c("n", "rows", "name")], list(n = n, rows = NULL, name = NULL))
  }

  # the id column is lost
  fitted_only(d |> dplyr::select(1:6, age) |> dplyr::filter(age < 30) |> MCA2(1:6), n_young)
  # random: the re-run is another sample
  set.seed(1)
  fitted_only(d |> dplyr::slice_sample(n = 200) |> MCA2(1:6), 200L)
  # %>% hides the expression behind `.`
  `%>%` <- magrittr::`%>%`
  fitted_only(d %>% dplyr::filter(age < 30) %>% MCA2(1:6), n_young)
  # the root is not a named data frame
  fitted_only(MCA2(dplyr::filter(fx_tea(), age < 30), 1:6), n_young)

  # a base subset on a condition with NA yields rows of NA: no id can be proved for them
  x <- data.frame(a = c(1, NA, 3, 4), b = letters[1:4])
  expect_null(src(x[x$a > 2, ], x[x$a > 2, ])$rows)
})

test_that("the fit records the name of its weight column", {
  d <- fx_tea_wt()
  expect_identical(MCA2(d, 1:6, wt = w)$source$wt, "w")
  expect_identical(MCA2(d, 1:6, wt = "w")$source$wt, "w")
  cars <- mtcars
  cars$w <- rep(1:2, 16)
  expect_identical(PCA2(cars, 1:7, wt = w)$source$wt, "w")
  expect_null(fx_mca()$source$wt)
})

# --- taking the data back: the one gate -------------------------------------------------------

test_that("ggmca takes the whole data frame back after an analysis of a subset", {
  d     <- fx_tea()
  whole <- md(fx_mca_young(), d, sup_vars = "SPC")
  sub   <- md(fx_mca_young(), d[d$age < 30, ], sup_vars = "SPC")
  expect_identical(whole$vars_data$`Dim 1`, sub$vars_data$`Dim 1`)
})

test_that("ggmca refuses data that is not the analysed data, and says why", {
  d <- fx_tea()
  expect_error(md(fx_mca(), d[1:100, ], sup_vars = "SPC"), "fitted on 300 rows")
  # an analysis of a subset kept apart names the data frame it was made on
  young <- dplyr::filter(d, age < 30)
  expect_error(md(MCA2(young, 1:6), d, sup_vars = "SPC"), "rows of `young`")
  expect_error(md(fx_mca(), dplyr::arrange(d, age), sup_vars = "SPC"), "reordered or modified")
  expect_error(md(fx_mca(), sup_vars = "SPC"), "pass it second")
})

test_that("ggmca without data draws the active variables alone", {
  local_null_device()
  expect_s3_class(quietly(ggmca(fx_mca())), "ggplot")
})

# --- axes_names: supplied by the user, not by the analysis --------------------------------------

test_that("a fitted object carries no axes_names of its own", {
  # axes_names is NOT something MCA2()/PCA2() compute: it is the user's argument to ggmca(), which
  # ggmca_plot() writes onto the stripped model. theme_facto() must therefore read it defensively.
  expect_false("axes_names" %in% names(fx_mca()))
  expect_false("axes_names" %in% names(PCA2(mtcars, 1:7)))
})

test_that("axes_names given to ggmca reaches the axis titles", {
  local_null_device()
  p <- quietly(ggmca(fx_mca(), fx_tea(), axes_names = c("first axis", "second axis")))
  labs <- ggplot2::ggplot_build(p)$plot$labels
  expect_true(any(grepl("first axis",  unlist(labs), fixed = TRUE)))
  expect_true(any(grepl("second axis", unlist(labs), fixed = TRUE)))
})

test_that("no axes_names still yields axis titles carrying the eigenvalue percentages", {
  # theme_facto()'s other job: the axis title always states the axis's share of variance.
  local_null_device()
  labs <- ggplot2::ggplot_build(quietly(ggmca(fx_mca(), fx_tea())))$plot$labels
  expect_true(any(grepl("%", unlist(labs), fixed = TRUE)))
})

test_that("ncp is honoured, and the default keeps every axis", {
  # The default is Inf: `res$eig` is how one chooses how many axes to interpret, and FactoMineR
  # truncates it to `ncp`. Clustering takes its own `ncp`, in hierarchical_clust().
  expect_equal(ncol(MCA2(fx_tea(), 1:6, ncp = 2)$ind$coord), 2L)
  expect_equal(ncol(fx_mca()$ind$coord), nrow(fx_mca()$eig))
  expect_equal(sum(fx_mca()$eig[, 2]), 100, tolerance = 1e-6)
})

# --- correspondence_analysis: a table, then its analysis --------------------------------------

test_that("correspondence_analysis reads a tab's counts and keeps its variables' names", {
  t   <- tabxplor::tab(forcats::gss_cat, race, marital)
  res <- correspondence_analysis(t)
  expect_identical(names(dimnames(res$call$X)), c("race", "marital"))
  expect_equal(res$eig, FactoMineR::CA(as.matrix(t), graph = FALSE, ncp = Inf)$eig)
  # the counts, whatever the table displays
  pct <- tabxplor::tab(forcats::gss_cat, race, marital, pct = "row")
  expect_equal(correspondence_analysis(pct)$eig, res$eig)
})

test_that("ca_interpret names the two margins a correspondence_analysis kept", {
  withr::local_options(tabxplor.print = "console")
  txt <- utils::capture.output(print(ca_interpret(correspondence_analysis(
    tabxplor::tab(forcats::gss_cat, race, marital))), n = Inf, width = Inf))
  expect_true(any(grepl("race", txt, fixed = TRUE)))
  expect_false(any(grepl("Rows", txt, fixed = TRUE)))
})
