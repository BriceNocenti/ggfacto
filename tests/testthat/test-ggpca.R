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
  res    <- PCA2(fx_cars(), tidyselect::all_of(fx_pca_vars()))
  layers <- ggplot2::ggplot_build(ggpca(res))$data
  points <- layers[[which(vapply(layers, nrow, 1L) == nrow(fx_cars()))[1]]]
  expect_true(all(abs(points$size - 1.5) < 1e-9))
})

test_that("the biplot rescales the circle onto the cloud, and keeps the correlations' directions", {
  local_null_device()
  pd <- ggpca(fx_pca2(), get_data = TRUE)
  cloud  <- pca_model(fx_pca2())$coord[, c("Dim 1", "Dim 2")]
  radius <- stats::quantile(sqrt(rowSums(cloud^2)), 0.9, names = FALSE)
  cor    <- fx_pca2()$var$coord[, 1:2]
  expect_equal(unname(as.matrix(pd$vectors_coord[c("Dim 1", "Dim 2")])), unname(cor * radius))
  expect_match(pd$vectors_coord$interactive_text[1], "^<b>mpg</b>\nmean \\(cv\\): [0-9.]+ \\([0-9]+%\\)\nCoord")
  expect_match(pd$mean_point_data$interactive_text, "n: 32", fixed = TRUE)
  expect_match(pd$mean_point_data$interactive_text, "mpg: [0-9.]+ \\([0-9]+%\\)")
  expect_false(grepl("Frequency", pd$mean_point_data$interactive_text))
  expect_null(ggpca(fx_pca2(), variables = FALSE, get_data = TRUE)$vectors_coord)
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

test_that("the biplot's circle is graduated, its axes read in the individuals' colour", {
  local_null_device()
  bi <- ggpca(fx_pca2(), fx_cars())
  b  <- ggplot2::ggplot_build(bi)
  expect_true(any(vapply(b$data, function(d) identical(sort(as.character(d$label)),
                                                      c("-1", "-1", "1", "1")), logical(1))))
  expect_identical(bi$theme$axis.text$colour, "#aaaaaa")
  expect_false(identical(ggpca_cor_circle(fx_pca2())$theme$axis.text$colour, "#aaaaaa"))
})

test_that("an arrow is hoverable, and its projections, shown at hover, never catch the pointer", {
  local_null_device()
  svg <- ggi(ggpca(fx_pca2(), fx_cars()))$x$html
  # the projections carry no hover id: a stylesheet shows them while their arrow is hovered
  expect_false(grepl("data-id='reveal-", svg, fixed = TRUE))
  expect_match(svg, "stroke-opacity='0'[^>]*stroke-dasharray[^>]*data-reveal='1001'")
  # hidden until then, the gold box included
  expect_match(svg, "[data-reveal] { pointer-events:none; fill-opacity:0; stroke-opacity:0; }",
               fixed = TRUE)
  expect_match(svg, "polygon[data-reveal] { fill:#ffe348 !important; }", fixed = TRUE)
  expect_match(svg, ":has([data-id='1001'].hover_data_svg_", fixed = TRUE)
  expect_match(svg, "stroke='#34515E'[^>]*data-id='1001'")
  # drawn for good, the projections are plain elements, nothing to reveal
  expect_false(grepl("data-reveal", ggi(ggpca_cor_circle(fx_pca2(), proj = TRUE))$x$html))
})

test_that("an individual's tooltip gives its coordinates on the two axes drawn", {
  pd  <- ggpca(fx_pca2(), axes = c(1, 3), get_data = TRUE)
  txt <- pd$profiles_coord$interactive_text[1]
  expect_match(txt, "Coord axe 1: -?[0-9.]+\nCoord axe 3: -?[0-9.]+\n\n")
  mca <- suppressMessages(ggmca(fx_mca(), profiles = TRUE, get_data = TRUE))
  expect_match(mca$profiles_coord$interactive_text[1], "Coord axe 2:", fixed = TRUE)
})

test_that("a supplementary level's tooltip is stable", {
  vd <- pca_pd("en")$vars_data
  expect_snapshot(cat(vd$interactive_text[vd$vars == "cyl"][1]))
})

test_that("a missing value sits at its variable's weighted mean, silently, and is counted", {
  d <- fx_cars()
  d$mpg[c(2, 5, 9)] <- NA
  d$hp[5] <- NA
  v <- fx_pca_vars()
  expect_no_warning(res <- PCA2(d, tidyselect::all_of(v), wt = w))
  expect_equal(res$source$na, list(mpg = c(2L, 5L, 9L), hp = 5L))
  expect_equal(res$call$X$mpg[2], stats::weighted.mean(d$mpg, d$w, na.rm = TRUE))

  # the table describes the observed values, and counts the missing ones
  tb <- interpret(res, axes = 1)
  obs <- !is.na(d$mpg)
  expect_equal(vctrs::field(tb$mean_Variables, "mean")[1], stats::weighted.mean(d$mpg, d$w, na.rm = TRUE))
  expect_equal(vctrs::field(tb$sd_Variables, "var")[1],
               weighted.var(d$mpg[obs], wt = res$call$row.w[obs]))
  expect_equal(vctrs::field(tb$NA_Variables, "n"), c(3L, 0L, 1L, rep(0L, length(v) - 3L), 3L))
  expect_false("NA_Variables" %in% names(interpret(fx_pca2(), axes = 1)))

  # the arrow's tooltip counts them; an individual's prints what was observed
  pd <- suppressMessages(pca_plot_data(res, d, rlang::quo(), rlang::quo(), "^.{0}", "^.+$",
                                       character(), character(), TRUE, TRUE, 5000, "en"))
  mpg <- pd$vectors$data$interactive_text[pd$vectors$data$name == "mpg"]
  expect_match(mpg, "missing: 3 (9.4%), placed at the mean", fixed = TRUE)
  expect_no_match(pd$vectors$data$interactive_text[pd$vectors$data$name == "disp"], "missing")
  wag <- pd$ind_data$interactive_text[grepl("<b>Mazda RX4 Wag</b>", pd$ind_data$interactive_text)]
  expect_match(wag, "mpg: NA", fixed = TRUE)

  # the data frame with its missing values is still the analysed one
  cl <- dplyr::mutate(d, cl = hierarchical_clust(res, ncp = 2, nb_clust = 3))$cl
  expect_equal(sum(!is.na(cl)), nrow(d))
})

test_that("a missing value's line speaks French", {
  skip_if_no_gettext()
  d <- fx_cars()
  d$mpg[2] <- NA
  res <- PCA2(d, tidyselect::all_of(fx_pca_vars()), wt = w)
  pd <- suppressMessages(pca_plot_data(res, d, rlang::quo(), rlang::quo(), "^.{0}", "^.+$",
                                       character(), character(), TRUE, TRUE, 5000, "fr"))
  expect_match(pd$vectors$data$interactive_text[1], "valeurs manquantes : 1 (3,1", fixed = TRUE)
})

test_that("na = \"drop\" leaves out the rows with a missing value", {
  d <- fx_cars()
  d$mpg[c(2, 5)] <- NA
  expect_message(PCA2(d, tidyselect::all_of(fx_pca_vars()), wt = w, na = "drop"),
                 "2 row\\(s\\) with a missing value")
  res <- suppressMessages(PCA2(d, tidyselect::all_of(fx_pca_vars()), wt = w, na = "drop"))
  expect_equal(nrow(res$ind$coord), nrow(d) - 2L)
  expect_null(res$source$na)
  expect_equal(is_in_analysis(res), !seq_len(nrow(d)) %in% c(2, 5))
})

test_that("interpret() of a PCA with supplementary individuals weighs the active ones", {
  res <- PCA2(fx_cars(), tidyselect::all_of(fx_pca_vars()), wt = w, ind.sup = 1:3)
  tb <- interpret(res, axes = 1)
  d  <- fx_cars()[-(1:3), ]
  expect_equal(vctrs::field(tb$mean_Variables, "mean")[1], stats::weighted.mean(d$mpg, d$w))
})
