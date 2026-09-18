# PURPOSE: Lock the clustering step -- hierarchical_clust() and the table that describes its
#   clusters, clust_tab().
# ROLE: The last step of the MCA/PCA workflow. The clusters are a column of the data frame, written
#   with mutate(); these tests pin that the column lines up with the rows it describes, whatever
#   subset the analysis was made on.
# KEY CONSTRAINTS:
#   - Tables are tabxplor's job: clust_tab() builds tabxplor::fmt() columns and lets tabxplor render
#     them. Numbers are asserted as facts rather than as digits wherever possible.
#   - hierarchical_clust() must equal what the course did by hand: HCPC() on an analysis refitted
#     with `ncp` axes.
# See: CLAUDE.md section ggfacto architecture > The FactoMineR contract.

hc <- function(...) hierarchical_clust(..., tree = FALSE)

# --- hierarchical_clust ------------------------------------------------------------------------

test_that("hierarchical_clust equals HCPC on an analysis refitted with ncp axes", {
  by_hand <- FactoMineR::HCPC(MCA2(fx_tea(), 1:6, ncp = 3), nb.clust = 4,
                              graph = FALSE)$data.clust$clust
  expect_identical(hc(fx_mca(), ncp = 3, nb.clust = 4), by_hand)

  pca     <- PCA2(mtcars, 1:7)
  by_hand <- FactoMineR::HCPC(PCA2(mtcars, 1:7, ncp = 2), nb.clust = 3,
                              graph = FALSE)$data.clust$clust
  expect_identical(unname(hc(pca, ncp = 2, nb.clust = 3)), unname(by_hand))
})

test_that("hierarchical_clust asks for ncp, and says why", {
  expect_error(hc(fx_mca(), nb.clust = 4), "number of axes")
  expect_error(hc(fx_ca(), ncp = 2), "multiple correspondence analysis")
})

test_that("hierarchical_clust draws the tree when it cuts it itself", {
  local_null_device()
  expect_true(formals(hierarchical_clust)$tree == quote(nb.clust == -1))
  expect_s3_class(hierarchical_clust(fx_mca(), ncp = 3), "factor")
})

test_that("in mutate(), the clusters line up with the whole data frame, NA outside the subset", {
  d   <- fx_tea()
  out <- d |> dplyr::mutate(cl = hc(fx_mca_young(), ncp = 3, nb.clust = 4))
  young <- d$age < 30
  expect_true(all(is.na(out$cl[!young])))
  expect_false(anyNA(out$cl[young]))
  expect_identical(as.character(out$cl[young]),
                   as.character(hc(fx_mca_young(), ncp = 3, nb.clust = 4)[young]))
})

test_that("in mutate() on the subset itself, every row gets its cluster", {
  sub <- fx_tea() |> dplyr::filter(age < 30)
  out <- sub |> dplyr::mutate(cl = hc(fx_mca_young(), ncp = 3, nb.clust = 4))
  expect_false(anyNA(out$cl))
})

test_that("a reordered analysis writes each cluster back on its own row", {
  d   <- fx_tea()
  res <- d |> dplyr::arrange(age) |> MCA2(1:6)
  out <- d |> dplyr::mutate(cl = hc(res, ncp = 3, nb.clust = 4))
  # the same clusters as on the sorted data, sorted back
  sorted <- d |> dplyr::arrange(age) |> dplyr::mutate(cl = hc(res, ncp = 3, nb.clust = 4))
  expect_identical(as.character(out$cl[order(d$age, method = "radix")]), as.character(sorted$cl))
})

test_that("outside mutate(), the clusters line up with the data frame the analysis started from", {
  out <- hc(fx_mca_young(), ncp = 3, nb.clust = 4)
  expect_length(out, nrow(fx_tea()))
  expect_identical(which(!is.na(out)), which(fx_tea()$age < 30))
})

test_that("hierarchical_clust refuses rows it cannot line up, and says why", {
  d <- fx_tea()
  # no rows recorded (%>% pipe): the whole data frame is longer than the analysis
  `%>%` <- magrittr::`%>%`
  res <- d %>% dplyr::filter(age < 30) %>% MCA2(1:6)
  expect_error(dplyr::mutate(d, cl = hc(res, ncp = 3, nb.clust = 4)), "native pipe")
  # the data frame was reordered after the analysis
  expect_error(dplyr::mutate(dplyr::arrange(d, age), cl = hc(fx_mca(), ncp = 3, nb.clust = 4)),
               "reordered or modified")
  # grouped
  expect_error(dplyr::mutate(dplyr::group_by(d, SPC), cl = hc(fx_mca(), ncp = 3, nb.clust = 4)),
               "ungroup")
})

# --- clust_tab ---------------------------------------------------------------------------------

rows <- function() tidyselect::all_of(fx_active())

test_that("clust_tab accepts the clusters as a bare name, a string and a vector", {
  d <- fx_tea_clust()
  by_symbol <- clust_tab(d, rows(), clust)
  by_string <- clust_tab(d, rows(), "clust")
  by_vector <- clust_tab(d, rows(), d$clust)

  expect_s3_class(by_symbol, "tbl_df")
  expect_identical(as.character(by_symbol), as.character(by_string))
  expect_identical(as.character(by_symbol), as.character(by_vector))
})

test_that("clust_tab has one column per cluster plus the population total", {
  tab <- clust_tab(fx_tea_clust(), rows(), clust)
  expect_gte(ncol(tab), nlevels(fx_tea_clust()$clust))
  expect_true(any(vapply(tab, inherits, logical(1), "tabxplor_fmt")))
})

test_that("clust_tab weights its percentages", {
  d <- fx_tea_clust()
  d$w <- rep(c(0.5, 1.5), length.out = nrow(d))
  expect_false(identical(as.character(clust_tab(d, rows(), clust)),
                         as.character(clust_tab(d, rows(), clust, wt = w))))
})

test_that("clust_tab leaves out the rows without a cluster", {
  d <- fx_tea() |> dplyr::mutate(cl = hc(fx_mca_young(), ncp = 3, nb.clust = 4))
  tab <- clust_tab(d, rows(), cl)
  expect_false(any(grepl("^NA$", names(tab))))
  expect_identical(as.character(tab),
                   as.character(clust_tab(dplyr::filter(d, !is.na(cl)), rows(), cl)))
})

test_that("clust_tab hides excluded levels, the missing answers first", {
  d <- fx_tea_na()
  d$clust <- fx_tea_clust()$clust
  lvs <- function(tab) as.character(tab$lvs)
  expect_false(any(grepl("\\.NA$", lvs(clust_tab(d, rows(), clust)))))
  expect_true("breakfast.NA" %in% lvs(clust_tab(d, rows(), clust, excl = NULL)))
  expect_false("Not.lunch" %in% lvs(clust_tab(d, rows(), clust, excl = c(NA, "Not.lunch"))))
})

# --- the former `cah` name --------------------------------------------------------------------

test_that("`cah =` still works, and says it is now `clust =`", {
  e <- ggfacto:::deprecated_args_warned
  rm(list = ls(envir = e), envir = e)
  expect_warning(pd <- md(fx_mca(), fx_tea_clust(), cah = "clust", profiles = TRUE), "clust")
  expect_identical(pd$clust, "clust")
})

test_that("clust_tab cleans the cluster names once, for the level rows and the population rows", {
  d <- fx_tea_clust()
  d$clust <- forcats::fct_relabel(d$clust, ~ paste0(.x, "-Cluster ", .x))
  tab <- clust_tab(d, rows(), clust)
  expect_false(any(grepl("^[0-9]-", names(tab))))
  expect_true(all(paste0("Cluster ", levels(fx_tea_clust()$clust)) %in% names(tab)))
  expect_true(any(grepl("^1-", names(clust_tab(d, rows(), clust, cleannames = FALSE)))))
})
