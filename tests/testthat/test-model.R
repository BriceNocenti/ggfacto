# PURPOSE: Lock the one reader of an MCA -- mca_model() -- and what it gives back to the data frame,
#   axis_coord().
# ROLE: multiple_correspondence_analysis() feeds FactoMineR the answer profiles; every consumer then
#   reads the model. These tests pin that the profile fit IS the individuals' analysis, and that the
#   model is the same whatever engine made the fit.
# KEY CONSTRAINTS:
#   - The reference is FactoMineR::MCA() run on the individuals (mca_ind(), helper-fixtures.R).
#   - GDAtools orients some axes the other way, as any engine may: its fits are compared up to the
#     sign of each axis.
# See: CLAUDE.md section ggfacto architecture > The FactoMineR contract.

# A weighted tea[1:6] with missing answers, so the weights, the `<VAR>.NA` levels and `excl` all act.
fx_tea_na_wt <- function() fx("tea_na_wt", function() {
  d <- fx_tea_na()
  d$w <- withr::with_seed(1, round(stats::runif(nrow(d), 0.2, 3), 3))
  d
})

test_that("the fit on the answer profiles is the individuals' analysis", {
  d   <- fx_tea_na_wt()
  res <- fit_mca(d, 1:6, wt = "w")
  ind <- mca_ind(d, 1:6, wt = "w")
  expect_lt(nrow(res$call$X), nrow(d))
  expect_equal(res$eig, ind$eig, tolerance = 1e-10)
  for (slot in c("coord", "contrib", "cos2", "v.test", "eta2")) {
    expect_equal(unname(res$var[[slot]]), unname(ind$var[[slot]]), tolerance = 1e-10)
  }
  expect_equal(unname(as.matrix(axis_coord(res, 1:5))), unname(ind$ind$coord[, 1:5]),
               tolerance = 1e-10)
})

test_that("the fit remembers each individual's profile and weight", {
  d   <- fx_tea_na_wt()
  res <- fit_mca(d, 1:6, wt = "w")
  expect_length(res$source$key, nrow(d))
  expect_identical(res$source$w, d$w)
  expect_equal(res$call$row.w, as.vector(rowsum(d$w, res$source$key, reorder = TRUE)))
  expect_null(fx_mca()$source$w)
  expect_equal(sum(fx_mca()$call$row.w), nrow(fx_tea()))
})

test_that("the model is the same for a profile fit and for FactoMineR on individuals", {
  d  <- fx_tea_na_wt()
  m  <- mca_model(fit_mca(d, 1:6, wt = "w"))
  mi <- mca_model(mca_ind(d, 1:6, wt = "w"))
  expect_identical(m$X, mi$X)
  expect_identical(m$key, mi$key)
  expect_equal(m$levels, mi$levels)
  expect_equal(m$coord, mi$coord, tolerance = 1e-10)
  expect_equal(m$w, mi$w)
})

test_that("a GDAtools speMCA() gives the model of the equivalent specific MCA", {
  skip_if_not_installed("GDAtools")
  spe <- mca_model(GDAtools::speMCA(fx_tea()[1:6], excl = 3))
  gg  <- mca_model(fit_mca(fx_tea(), 1:6, excl = "Not.tea time"))
  expect_identical(spe$X, gg$X)
  expect_identical(spe$key, gg$key)
  expect_equal(spe$levels, gg$levels)
  expect_equal(spe$eig, gg$eig, tolerance = 1e-10, ignore_attr = TRUE)
  # up to the sign of each axis, which each engine chooses its own way
  a <- seq_len(ncol(spe$coord))
  flip <- sign(colSums(spe$var$coord[, a] * gg$var$coord[, a]))
  expect_equal(t(t(spe$coord) * flip), gg$coord[, a], tolerance = 1e-10)
})

test_that("a GDAtools csMCA() is read over its subcloud", {
  skip_if_not_installed("GDAtools")
  women <- fx_tea()$sex == "F"
  m <- mca_model(GDAtools::csMCA(fx_tea()[1:6], subcloud = women))
  expect_identical(m$source$rows, which(women))
  expect_equal(m$n, sum(women))
  expect_equal(sum(m$levels$n[m$levels$vars == "breakfast"]), sum(women))
})

test_that("the ingress refuses what the profiles cannot carry, and says why", {
  expect_error(fit_mca(fx_tea(), 1), "two active variables")
  expect_error(fit_mca(fx_tea(), 1:6, ind.sup = 1:10), "ind.sup")
  expect_error(fit_mca(fx_tea(), 1:6, quali.sup = 7), "quali.sup")
  expect_error(mca_model(FactoMineR::MCA(fx_tea()[1:6], ind.sup = 1:10, graph = FALSE)),
               "supplementary individuals")
})

# --- axis_coord() ------------------------------------------------------------------------------

test_that("axis_coord writes each individual's coordinate into the data frame", {
  d <- fx_tea() |>
    dplyr::mutate(axe1 = axis_coord(fx_mca(), 1), axis_coord(fx_mca(), c(a2 = 2, a3 = 3)))
  ind <- mca_ind(fx_tea(), 1:6)
  expect_equal(d$axe1, unname(ind$ind$coord[, 1]), tolerance = 1e-10)
  expect_equal(d$a3,   unname(ind$ind$coord[, 3]), tolerance = 1e-10)
  expect_named(axis_coord(fx_mca(), 1:2), c("axis1", "axis2"))
  expect_error(axis_coord(fx_mca(), 50), "keeps")
})

test_that("axis_coord lines up with the whole data frame, NA outside the analysed subset", {
  young <- fx_tea()$age < 30
  out <- dplyr::mutate(fx_tea(), axe1 = axis_coord(fx_mca_young(), 1))$axe1
  expect_true(all(is.na(out[!young])))
  expect_false(anyNA(out[young]))
  expect_identical(axis_coord(fx_mca_young(), 1), out)
})

test_that("axis_coord reads a PCA's individuals and a CA's levels", {
  expect_equal(axis_coord(fx_pca(), 2), unname(fx_pca()$ind$coord[, 2]))
  ca  <- correspondence_analysis(tabxplor::tab(forcats::gss_cat, relig, partyid))
  gss <- forcats::gss_cat |> dplyr::mutate(ax = axis_coord(ca, 1))
  by_level <- axis_coord(ca, 1)
  expect_identical(gss$ax, unname(by_level[as.character(gss$relig)]))
})

test_that("name_axes() names the axes in order, or one by its number, and they are printed", {
  res <- name_axes(fx_mca(), "first", "")
  expect_equal(res$axes_names, c("first", ""))
  res <- name_axes(res, "3" = "third")
  expect_equal(res$axes_names, c("first", "", "third"))
  expect_equal(name_axes(res, "one")$axes_names, c("one", "", "third"))
  expect_identical(name_axes(res), res)
  expect_error(name_axes(res, "x" = "a"), "by its number")
  expect_error(name_axes(res, 1), "character strings")
  expect_error(name_axes(res, "99" = "a"), "cannot be named")
  expect_error(name_axes(res, "a", "1" = "b"), "only one name")

  p <- suppressMessages(ggfacto(res, axes = c(1, 3)))
  expect_equal(p$labels$x, "Axe 1 (23.4%): first")
  expect_match(p$labels$y, "third$")
  heads <- unique(as.character(interpret(res, axes = 1:3, eig = FALSE, lang = "en")$Axe))
  expect_match(heads[1], " \u2014 first$")
  expect_no_match(heads[2], "\u2014")
  expect_match(heads[3], " \u2014 third$")

  ca <- name_axes(fx_ca(), "rows / cols")
  expect_match(as.character(interpret(ca, eig = FALSE, lang = "en")$Axe)[1], "\u2014 rows / cols$")
})
