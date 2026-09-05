# PURPOSE: Lock the contract of the two ingress normalisers, MCA2() and PCA2().
# ROLE: These are the only functions in the package that compute an analysis, and their whole job is
#   to normalise a call into FactoMineR's: tidyselect for active_vars and wt, and a regex for excl
#   that promotes NA to a level and then excludes it, which is specific MCA.
# KEY CONSTRAINTS:
#   - What is asserted is the FITTED OBJECT's slots, because everything downstream reads those
#     directly ($call$X, $call$row.w, $call$excl, $call$quali). They are the real interface.
#   - Weights ride one channel: the user's column reaches FactoMineR as row.w and is read back from
#     the fitted object, never from the data. See tests/testthat/test-tooltips.R for the other half.
# See: CLAUDE.md section ggfacto architecture > The FactoMineR contract.

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

# --- excl: specific MCA -------------------------------------------------------------------------

test_that("excl marks the matching levels in $call$excl, which is what makes it a specific MCA", {
  # excl takes a REGEX matched against level names, not level indices: this is the whole feature.
  res <- MCA2(fx_tea(), 1:6, excl = "Not.", ncp = 2)
  expect_true(length(res$call$excl) > 0)
  excluded <- names(res$call$Xtot)[res$call$excl]
  expect_true(all(grepl("Not\\.", excluded)))
})

test_that("excl actually removes the excluded levels from the drawn cloud", {
  # The user-visible consequence: an excluded level must not appear among the plotted points.
  plot_data <- suppressMessages(ggmca_data(MCA2(fx_tea(), 1:6, excl = "Not.", ncp = 2), fx_tea()))
  expect_false(any(grepl("^Not\\.", as.character(plot_data$vars_data$lvs))))
})

test_that("no excl leaves $call$excl empty", {
  expect_length(fx_mca()$call$excl, 0L)
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
  # truncates it to `ncp`. One lowers `ncp` only to feed HCPC(), which clusters on the axes kept.
  expect_equal(ncol(MCA2(fx_tea(), 1:6, ncp = 2)$ind$coord), 2L)
  expect_equal(ncol(fx_mca()$ind$coord), nrow(fx_mca()$eig))
  expect_equal(sum(fx_mca()$eig[, 2]), 100, tolerance = 1e-6)
})
