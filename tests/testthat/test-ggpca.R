# PURPOSE: Lock the graph of the individuals of a PCA -- ggpca() -- and the correlation circle.
# ROLE: The PCA's half of the shared plot model: its individuals are the points of the cloud, its
#   supplementary levels the barycentres of theirs, and its tooltips the means of the active
#   variables, coloured as clust_tab() colours them.
# KEY CONSTRAINTS:
#   - A supplementary level is FactoMineR's quali.sup point, pinned to 1e-10.
#   - A mean cell is tab()'s: its mean and its colour, each level against its own variable's total.
# See: CLAUDE.md section ggfacto architecture > The plot model.

pca_pd <- function(...) suppressMessages(pca_plot_data(
  fx_pca2(), fx_cars(), rlang::quo(c(cyl, gear)), rlang::quo(), "^.{0}", "^.+$",
  character(), character(), TRUE, TRUE, 5000, ...))

test_that("the individuals are the points of the cloud, named when the data names them", {
  ind <- pca_pd(NULL)$ind_data
  expect_equal(nrow(ind), nrow(fx_cars()))
  expect_equal(sum(ind$count), nrow(fx_cars()))
  expect_equal(sum(ind$wcount), sum(fx_cars()$w))
  expect_true(any(grepl("<b>Mazda RX4</b>", ind$interactive_text, fixed = TRUE)))
  unnamed <- fx_cars()
  rownames(unnamed) <- NULL
  res <- PCA2(unnamed, tidyselect::all_of(fx_pca_vars()))
  txt <- suppressMessages(ggpca(res, get_data = TRUE))$profiles_coord$interactive_text
  expect_true(any(grepl("Individual n\u00b01</b>", txt, fixed = TRUE)))
})

test_that("a supplementary level sits where FactoMineR puts it", {
  d  <- fx_cars()
  fm <- FactoMineR::PCA(cbind(d[fx_pca_vars()], cyl = d$cyl), quali.sup = 7, row.w = d$w,
                        graph = FALSE)
  vd <- pca_pd(NULL)$vars_data
  got <- as.matrix(vd[vd$vars == "cyl", c("Dim 1", "Dim 2", "Dim 3")])
  expect_equal(unname(got), unname(fm$quali.sup$coord[, 1:3]), tolerance = 1e-10)
})

test_that("a supplementary level's tooltip holds tab()'s means and colours", {
  d <- fx_cars()
  d$gear[1:5] <- NA
  pd <- suppressMessages(pca_plot_data(fx_pca2(), d, rlang::quo(c(cyl, gear)), rlang::quo(),
                                       "^.{0}", "^.+$", character(), character(), TRUE, TRUE,
                                       5000, NULL))
  vd <- pd$vars_data
  for (sv in c("cyl", "gear")) {
    direct <- tabxplor::tab(d, !!rlang::sym(sv), tidyselect::all_of(fx_pca_vars()), wt = w,
                            color = "difference", na = "drop")
    lvs <- as.character(direct[[1]])
    for (v in fx_pca_vars()) {
      for (lv in setdiff(lvs, "Total")) {
        body <- vd$interactive_text[vd$vars == sv & as.character(vd$lvs) == lv]
        line <- regmatches(body, regexpr(paste0("\n", v, ": [^\n]*"), body))
        # the colour is read on the whole column: a cell alone has no Total to be measured against
        colour <- tabxplor::fmt_get_color_code(direct[[v]])[lvs == lv]
        mean   <- vctrs::field(direct[[v]], "mean")[lvs == lv]
        nums <- as.numeric(regmatches(line, gregexpr("-?[0-9.]+(?=</b>|$)", line, perl = TRUE))[[1]])
        expect_equal(nums[length(nums)], mean, tolerance = 0.01)
        expect_identical(grepl("<font color", line), !is.na(colour))
        if (!is.na(colour)) expect_true(grepl(colour, line, fixed = TRUE))
      }
    }
  }
})

test_that("the clusters of a PCA colour its individuals, linked to their label", {
  d <- fx_cars()
  d$clust <- hierarchical_clust(fx_pca2(), ncp = 2, nb_clust = 3, tree = FALSE)
  pd <- suppressMessages(ggpca(fx_pca2(), d, clust = clust, get_data = TRUE))
  labels <- pd$vars_data[pd$vars_data$vars == "clust", ]
  expect_true(all(labels$id >= 10000L))
  for (i in seq_len(nrow(labels))) {
    ids <- pd$profiles_coord$id[as.character(pd$profiles_coord$clust) == labels$lvs[i]]
    expect_true(length(ids) > 0 && all(ids == labels$id[i]))
  }
})

test_that("a cap on individuals of equal weight keeps an evenly spread sample", {
  pts <- cloud_points(seq_len(32), rep(1, 32), rep(1, 32), 8)
  expect_length(pts$drawn, 8L)
  expect_true(all(table(cut(pts$first[pts$drawn], c(0, 8, 16, 24, 32))) >= 1))
})

test_that("every type builds, ellipses too, and equal weights draw small points", {
  local_null_device()
  for (type in c("text", "labels", "points", "facets")) {
    expect_no_error(ggplot2::ggplot_build(
      suppressMessages(ggpca(fx_pca2(), fx_cars(), sup_vars = cyl, type = type))))
  }
  expect_no_error(ggplot2::ggplot_build(
    suppressMessages(ggpca(fx_pca2(), fx_cars(), sup_vars = cyl, ellipses = 0.5))))
  res  <- PCA2(fx_cars(), tidyselect::all_of(fx_pca_vars()))
  size <- ggplot2::ggplot_build(ggpca(res))$data
  size <- unlist(lapply(size, function(d) d$size))
  expect_true(all(abs(size[!is.na(size)] - 1.5) < 1e-9 | size[!is.na(size)] == 5))
})

test_that("ggpca() refuses anything but a PCA, and asks for the data frame in words", {
  expect_error(ggpca(fx_mca()), "principal component analysis")
  expect_error(ggpca(fx_pca2(), sup_vars = cyl), "ggpca\\(\\) needs the data frame")
})

test_that("the correlation circle is a ggplot, made interactive by ggi() or at once", {
  local_null_device()
  p <- ggpca_cor_circle(fx_pca2())
  expect_s3_class(p, c("ggfacto_plot", "ggplot"))
  expect_identical(attr(p, "height_width_ratio"), 1)
  expect_s3_class(ggi(p), "girafe")
  expect_s3_class(ggpca_cor_circle(fx_pca2(), interactive = TRUE), "girafe")
})

test_that("the circle's tooltips name every axis, from the tenth on too", {
  local_null_device()
  d <- as.data.frame(matrix(stats::rnorm(300 * 11), 300))
  res <- withr::with_seed(1, PCA2(d, 1:11))
  txt <- ggplot2::ggplot_build(ggpca_cor_circle(res))$data
  txt <- unlist(lapply(txt, function(x) x$tooltip))
  expect_true(any(grepl("Coord axe 10:", gsub(unbrk, " ", txt), fixed = TRUE)))
})

test_that("a supplementary level's tooltip is stable", {
  vd <- pca_pd("en")$vars_data
  expect_snapshot(cat(vd$interactive_text[vd$vars == "cyl"][1]))
})
