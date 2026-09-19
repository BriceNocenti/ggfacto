# PURPOSE: Lock ggfacto(), the one graph verb: it draws each analysis as its own function does,
#   refuses in words what an analysis does not take, and makes a graph interactive on demand.
# ROLE: The verb the course teaches beside interpret(). Its methods only route: a graph that
#   ggfacto() draws is the one ggmca(), ggca() or ggpca() draws.
# See: CLAUDE.md section ggfacto architecture > How a graph is built.

dims_of <- function(p) lapply(ggplot2::ggplot_build(p)$data, dim)

test_that("ggfacto() draws each analysis as its own function does", {
  local_null_device()
  expect_identical(dims_of(ggfacto(fx_mca(), fx_tea_clust(), sup_vars = SPC, clust = clust)),
                   dims_of(ggmca(fx_mca(), fx_tea_clust(), sup_vars = SPC, clust = clust)))
  expect_identical(dims_of(ggfacto(fx_ca_multi())), dims_of(ggca(fx_ca_multi())))
  expect_identical(dims_of(ggfacto(fx_pca2(), fx_cars(), sup_vars = cyl, ellipses = 0.5)),
                   dims_of(ggpca(fx_pca2(), fx_cars(), sup_vars = cyl, ellipses = 0.5,
                                 profiles = FALSE)))
})

test_that("a PCA alone is its circle; individuals, levels or clusters make it a biplot", {
  local_null_device()
  expect_identical(dims_of(ggfacto(fx_pca2())), dims_of(ggpca_cor_circle(fx_pca2())))
  expect_identical(dims_of(ggfacto(fx_pca2(), fx_cars())), dims_of(ggpca_cor_circle(fx_pca2())))
  bi <- ggfacto(fx_pca2(), fx_cars(), profiles = TRUE)
  expect_identical(dims_of(bi), dims_of(ggpca(fx_pca2(), fx_cars())))
  d <- fx_cars()
  d$cah <- hierarchical_clust(fx_pca2(), ncp = 2, nb_clust = 3, tree = FALSE)
  expect_false(is.null(ggfacto(fx_pca2(), d, clust = cah, get_data = TRUE)$profiles_coord))
})

test_that("ggfacto() draws a GDAtools fit too", {
  skip_if_not_installed("GDAtools")
  local_null_device()
  spe <- GDAtools::speMCA(fx_tea()[1:6], excl = 3)
  expect_s3_class(ggfacto(spe, fx_tea(), sup_vars = SPC), "ggfacto_plot")
})

test_that("an argument its analysis does not take is refused in words", {
  expect_error(ggfacto(fx_ca_multi(), fx_gss_wt()), "reads its table")
  expect_error(ggfacto(fx_ca_multi(), sup_vars = marital), "in its table")
  expect_error(ggfacto(fx_ca_multi(), profiles = TRUE), "no individuals")
  expect_error(ggfacto(fx_ca_multi(), active_tables = "sup"), "active_tables")
  expect_error(ggfacto(fx_ca_multi(), ellipses = 0.5), "ellipses")
  expect_error(ggfacto(fx_pca2(), active_tables = NULL), "active_tables")
  expect_error(ggfacto(lm(mpg ~ hp, mtcars)), "draws a multiple correspondence")
})

test_that("the expert arguments pass through, and a misspelled one errors", {
  local_null_device()
  vd <- ggfacto(fx_mca(), fx_tea(), sup_vars = SPC, discard_levels = "employee",
                get_data = TRUE)$vars_data
  expect_false("employee" %in% vd$lvs)
  expect_false(any(ggfacto(fx_ca_multi(), show_sup = FALSE, get_data = TRUE)$vars_data$role
                   == "sup"))
  expect_error(ggfacto(fx_mca(), fx_tea(), profile_max = 10), "unused argument")
})

test_that("the data frame is asked for in words, and a cluster takes a bare name", {
  expect_error(ggfacto(fx_mca(), sup_vars = SPC), "ggmca\\(\\) needs the data frame")
  local_null_device()
  expect_identical(ggfacto(fx_mca(), fx_tea_clust(), clust = clust, get_data = TRUE)$vars_data,
                   ggmca(fx_mca(), fx_tea_clust(), clust = "clust", get_data = TRUE)$vars_data)
})

test_that("interactive = TRUE gives the widget, and cannot be asked of the data frames", {
  local_null_device()
  expect_s3_class(ggfacto(fx_mca(), interactive = TRUE), "girafe")
  expect_s3_class(ggfacto(fx_ca_multi(), interactive = TRUE), "girafe")
  expect_s3_class(ggfacto(fx_mca()), "ggfacto_plot")
  expect_error(ggfacto(fx_mca(), interactive = TRUE, get_data = TRUE), "cannot be interactive")
})
