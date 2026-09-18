# PURPOSE: Check that every graph entry point returns a real, buildable ggplot, and that the render
#   hints it carries survive the round trip into ggi() and ggsave2().
# ROLE: Guards the plot-object seam. ggmca_plot() and ggca() attach css_hover, css_tooltip and
#   height_width_ratio to the returned object; ggi()/ggsave2() read them back.
# KEY CONSTRAINTS:
#   - The hints are ATTRIBUTES. They were list slots bolted on with append(), which flattened the
#     S7 ggplot into a plain list: ggplot_build(), grid.draw() and so ggsave2() stopped dispatching,
#     and print() only drew by accident while dumping the list to stdout. Never reintroduce that.
#   - Every test that draws opens local_null_device() first, or Rscript writes Rplots.pdf into the
#     working directory and R CMD check reports a NOTE.
#   - Assertions stay at the level a user can see -- the object builds, the layers are there, the
#     widget is a girafe, the file is written. ggplot internals are not the contract.
# See: CLAUDE.md section ggfacto architecture > The plot-object seam.

# --- every entry point builds -------------------------------------------------------------------

test_that("the MCA graphs build", {
  local_null_device()
  expect_s3_class(quietly(ggmca(fx_mca(), fx_tea())), "ggplot")
  expect_s3_class(quietly(ggmca(fx_mca(), fx_tea(), sup_vars = "SPC")), "ggplot")
  expect_no_error(ggplot2::ggplot_build(quietly(ggmca(fx_mca(), fx_tea()))))
})

test_that("the teaching graphs build, including on all-binary variables", {
  # ggmca_initial_dims() used to fail outright whenever every active variable was binary -- the
  # ordinary MCA input -- because the x* columns are one per level of a variable group and a
  # battery of yes/no items never produces an x2.
  local_null_device()
  expect_no_error(ggplot2::ggplot_build(quietly(ggmca_initial_dims(fx_mca(), fx_tea()))))
  expect_no_error(ggplot2::ggplot_build(quietly(ggmca_with_base_ref(fx_mca()))))
})

test_that("the teaching graphs build when FactoMineR renames levels (y/n, shared)", {
  local_null_device()
  d <- fx_tea()[1:6]
  d$always    <- factor(ifelse(d$always == "always", "y", "n"))
  d$breakfast <- factor(ifelse(d$breakfast == "breakfast", "yes", "no"))
  d$lunch     <- factor(ifelse(d$lunch == "lunch", "yes", "no"))
  res <- MCA2(d, 1:6)
  expect_no_error(ggplot2::ggplot_build(quietly(ggmca_initial_dims(res, d))))
  expect_no_error(ggplot2::ggplot_build(quietly(ggmca_with_base_ref(res, d))))
})

test_that("keep = selects a few variables on both teaching graphs", {
  # `keep` on ggmca_initial_dims() called purrr::keep() unqualified and died with
  # "could not find function keep" -- a hard error on a documented argument.
  local_null_device()
  three <- fx_active()[1:3]
  expect_no_error(ggplot2::ggplot_build(quietly(ggmca_initial_dims(fx_mca(), fx_tea(), keep = three))))
  expect_no_error(ggplot2::ggplot_build(quietly(ggmca_with_base_ref(fx_mca(), keep = three))))
})

test_that("ggca builds", {
  local_null_device()
  expect_s3_class(quietly(ggca(fx_ca())), "ggplot")
  expect_no_error(ggplot2::ggplot_build(quietly(ggca(fx_ca()))))
})

test_that("ggpca_cor_circle returns a ggplot when not interactive, a widget when it is", {
  local_null_device()
  expect_s3_class(quietly(ggpca_cor_circle(fx_pca(), interactive = FALSE)), "ggplot")
  expect_s3_class(quietly(ggpca_cor_circle(fx_pca(), interactive = TRUE)), "girafe")
})

test_that("the text and labels types build on a plain call", {
  local_null_device()
  for (ty in c("text", "labels")) {
    expect_no_error(ggplot2::ggplot_build(quietly(ggmca(fx_mca(), fx_tea(), type = ty))))
  }
})

test_that("type = 'points' builds on a plain call and with a table", {
  # `points` sizes its points by wcount. wcount used to exist only once a tooltip table had been
  # built, so the plain call failed; it is now derived from the column margin on every path.
  local_null_device()
  expect_no_error(ggplot2::ggplot_build(quietly(ggmca(fx_mca(), fx_tea(), type = "points"))))
  expect_no_error(ggplot2::ggplot_build(
    quietly(ggmca(fx_mca(), fx_tea(), sup_vars = "SPC", type = "points"))))
  expect_no_error(ggplot2::ggplot_build(
    quietly(ggmca(fx_mca(), fx_tea(), active_tables = "active", type = "points"))))
})

test_that("every documented type builds, and an undocumented one errors", {
  # The `type` vocabulary is the user-facing contract; "numbers" was removed because it mapped an
  # aesthetic to a column nothing ever created and so failed in every configuration.
  local_null_device()
  expect_error(quietly(ggmca(fx_mca(), fx_tea(), type = "numbers")), "unknown type")
})

test_that("type = 'facets' builds with the arguments its example passes", {
  # facets draws one panel per level of the first sup_var, over the profile cloud, so it needs both
  # sup_vars and profiles = TRUE. That is how the roxygen example calls it.
  local_null_device()
  expect_no_error(ggplot2::ggplot_build(quietly(
    ggmca(fx_mca(), fx_tea(), sup_vars = "SPC", type = "facets",
          ellipses = 0.5, profiles = TRUE))))
})

test_that("xlim/ylim and text_repel build, which a plain call does not reach", {
  local_null_device()
  expect_no_error(ggplot2::ggplot_build(
    quietly(ggmca(fx_mca(), fx_tea(), xlim = c(-1, 1), ylim = c(-1, 1)))))
  expect_no_error(ggplot2::ggplot_build(
    quietly(ggmca(fx_mca(), fx_tea(), text_repel = TRUE))))
})

test_that("profiles and ellipses build", {
  local_null_device()
  expect_no_error(ggplot2::ggplot_build(
    quietly(ggmca(fx_mca(), fx_tea_clust(), clust = "clust", profiles = TRUE))))
  expect_no_error(ggplot2::ggplot_build(
    quietly(ggmca(fx_mca(), fx_tea(), sup_vars = "SPC", profiles = TRUE, ellipses = 0.5))))
  expect_no_error(ggplot2::ggplot_build(
    quietly(ggmca(fx_mca(), fx_tea(), sup_vars = "SPC", profiles = TRUE, type = "facets"))))
})

test_that("ellipses cover every individual, and need no profiles", {
  # An ellipse describes the individuals of a level, not the profiles that happen to be drawn:
  # max_profiles thins the cloud, never the ellipses.
  local_null_device()
  p <- quietly(ggmca(fx_mca(), fx_tea(), sup_vars = "SPC", ellipses = 0.5))
  expect_true(any(vapply(p$layers, function(l) inherits(l$stat, "StatEllipse"), logical(1))))

  pd <- md(fx_mca(), fx_tea(), sup_vars = "SPC", profiles = TRUE, max_profiles = 5)
  coord <- quietly(ggmca_plot(pd, ellipses = 0.5, get_data = TRUE))$ellipses_coord
  expect_equal(nrow(coord), sum(!is.na(fx_tea()$SPC)))
  expect_lte(nrow(pd$ind_data), 5L)
})

# The ellipse a layer draws, as ggplot2 builds it.
ellipse_of <- function(p) {
  i <- which(vapply(p$layers, function(l) inherits(l$stat, "StatEllipse"), logical(1)))
  ggplot2::ggplot_build(p)$data[[i]][c("x", "y", "group")]
}

test_that("the ellipses are weighted by the survey weights, and unchanged without", {
  local_null_device()
  by_hand <- function(res, data, weighted) {
    pd    <- md(res, data, sup_vars = "SPC")
    coord <- quietly(ggmca_plot(pd, ellipses = 0.5, get_data = TRUE))$ellipses_coord
    aes   <- if (weighted) {
      ggplot2::aes(.data$`Dim 1`, .data$`Dim 2`, group = .data$lvs, weight = .data$row.w)
    } else {
      ggplot2::aes(.data$`Dim 1`, .data$`Dim 2`, group = .data$lvs)
    }
    ellipse_of(ggplot2::ggplot(coord, aes) +
                 ggplot2::stat_ellipse(type = "t", level = 0.5, segments = 360))
  }
  pw <- quietly(ggmca(fx_mca_wt(), fx_tea_wt(), sup_vars = "SPC", ellipses = 0.5))
  expect_equal(ellipse_of(pw), by_hand(fx_mca_wt(), fx_tea_wt(), weighted = TRUE))
  expect_false(isTRUE(all.equal(ellipse_of(pw), by_hand(fx_mca_wt(), fx_tea_wt(), FALSE))))
  pu <- quietly(ggmca(fx_mca(), fx_tea(), sup_vars = "SPC", ellipses = 0.5))
  expect_equal(ellipse_of(pu), by_hand(fx_mca(), fx_tea(), weighted = FALSE))
})

test_that("facets draw the profiles of each level, with or without profiles = TRUE", {
  local_null_device()
  p <- quietly(ggmca(fx_mca(), fx_tea(), sup_vars = "SPC", type = "facets"))
  facet_counts <- ggplot2::ggplot_build(p)$data[[1]]
  expect_gt(nrow(facet_counts), nlevels(fx_tea()$SPC))
  expect_error(quietly(ggmca(fx_mca(), fx_tea(), type = "facets")), "supplementary variable")
})

test_that("axes_reverse = 1:2 reverses both axes", {
  local_null_device()
  pd <- fx_pd_sup()
  flipped <- quietly(ggmca_plot(pd, axes_reverse = 1:2, get_data = TRUE))$vars_data
  plain   <- quietly(ggmca_plot(pd, get_data = TRUE))$vars_data
  expect_equal(flipped$`Dim 1`, -plain$`Dim 1`)
  expect_equal(flipped$`Dim 2`, -plain$`Dim 2`)
})

# --- the seam -----------------------------------------------------------------------------------

test_that("a graph is a real ggplot, not a list wearing its class", {
  # The regression that broke ggsave2(): append() flattened the S7 object, so the class was right
  # but no ggplot generic dispatched on it.
  local_null_device()
  p <- quietly(ggmca(fx_mca(), fx_tea()))
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_no_error(grid::grid.draw(p))
})

test_that("the render hints ride as attributes and survive `+`", {
  local_null_device()
  p <- quietly(ggmca(fx_mca(), fx_tea()))
  expect_false(is.null(attr(p, "height_width_ratio", exact = TRUE)))
  # Users are invited to extend the plot with +; the hints must not fall off when they do.
  expect_false(is.null(attr(p + ggplot2::labs(title = "x"), "height_width_ratio", exact = TRUE)))
})

test_that("ggca carries its own css_tooltip", {
  local_null_device()
  pc <- quietly(ggca(fx_ca()))
  expect_false(is.null(attr(pc, "css_tooltip", exact = TRUE)))
})

test_that("printing a graph draws it without printing anything", {
  # print() used to dump the whole flattened list to stdout and draw only as a side effect of
  # print.default() recursing into element [[1]].
  local_null_device()
  p <- quietly(ggmca(fx_mca(), fx_tea()))
  expect_output(print(p), NA)
})

test_that("ggi returns a girafe widget for both graph families", {
  local_null_device()
  expect_s3_class(quietly(ggi(quietly(ggmca(fx_mca(), fx_tea())))), "girafe")
  expect_s3_class(quietly(ggi(quietly(ggca(fx_ca())))), "girafe")
})

test_that("the ggfacto_widget tag sits where neither dispatch nor htmlwidgets is broken", {
  # class(x)[1] is what htmlwidgets reads to find the JavaScript binding, and knit_print.htmlwidget
  # is what wins if our tag lands after "htmlwidget". Both mistakes are silent: the first gives a
  # blank graph, the second leaves the payload inline.
  local_null_device()
  w <- quietly(ggi(quietly(ggca(fx_ca()))))
  expect_identical(class(w), c("girafe", "ggfacto_widget", "htmlwidget"))
  expect_s3_class(w, "ggfacto_widget")
})

test_that("ggsave2 writes a non-empty file", {
  # Broken before the hints became attributes: grid.draw() could not dispatch on the flattened list.
  local_null_device()
  dir <- withr::local_tempdir()
  quietly(ggsave2(quietly(ggmca(fx_mca(), fx_tea())), dir = dir, name = "mca", replace = TRUE))
  f <- file.path(dir, "mca.png")
  expect_true(file.exists(f))
  expect_gt(file.info(f)$size, 1000)
})

test_that("ggi and ggsave2 tolerate a plain ggplot, which carries no hints at all", {
  local_null_device()
  plain <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) + ggplot2::geom_point()
  expect_s3_class(quietly(ggi(plain)), "girafe")
  dir <- withr::local_tempdir()
  expect_no_error(quietly(ggsave2(plain, dir = dir, name = "plain", replace = TRUE)))
})

# --- data_id banding ----------------------------------------------------------------------------

test_that("hover ids are banded so a whole cluster lights up together", {
  # Active variables from 1000, HCPC clusters and answer profiles from 10000: every point of one
  # cluster shares an id, which is what makes hovering any of them highlight them all.
  plot_data <- fx_pd_clust()
  actives <- plot_data$vars_data$id[plot_data$vars_data$color_group == "active_vars"]
  expect_true(all(actives >= 1000L))
  clust_ids <- plot_data$vars_data$id[plot_data$vars_data$vars == "clust"]
  expect_true(all(clust_ids >= 10000L))
  expect_true(all(plot_data$ind_data$id >= 10000L))
})

# --- 3D, behind Suggests ------------------------------------------------------------------------

test_that("the 3D graphs build when plotly is available", {
  skip_if_not_installed("plotly")
  local_null_device()
  expect_s3_class(quietly(ggmca_3d(fx_mca(), fx_tea())), "plotly")
  expect_s3_class(quietly(ggpca_3d(fx_pca())), "plotly")
})

test_that("ggmca_3d draws in two dimensions too, with no 3D attribute left over", {
  skip_if_not_installed("plotly")
  local_null_device()
  p <- quietly(ggmca_3d(fx_mca(), fx_tea(), axes = 1:2))
  expect_s3_class(p, "plotly")
  expect_no_warning(plotly::plotly_build(p))
})
