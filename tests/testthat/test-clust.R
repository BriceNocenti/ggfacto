# PURPOSE: Lock the clustering step -- hierarchical_clust() and the table that describes its
#   clusters, clust_tab() (and HCPC_tab(), its former form).
# ROLE: The last step of the workflow. The clusters are a column of the data frame, written with
#   mutate(); these tests pin that they are FactoMineR::HCPC()'s, that one tree serves every cut,
#   and that the column lines up with the rows it describes, whatever subset the analysis was made on.
# KEY CONSTRAINTS:
#   - Tables are tabxplor's job: clust_tab() builds tabxplor::fmt() columns and lets tabxplor render
#     them. Numbers are asserted as facts rather than as digits wherever possible.
#   - hierarchical_clust() computes HCPC()'s clusters itself: every analysis is compared with HCPC()
#     run on the same analysis, fitted with `ncp` axes.
#   - The tree cache is shared by the whole run (the fixtures fill it): a test counting trees
#     empties it first.
# See: CLAUDE.md section ggfacto architecture > The FactoMineR contract.

hc <- function(...) hierarchical_clust(..., tree = FALSE)
hcpc <- function(fit, ...) {
  FactoMineR::HCPC(fit, graph = FALSE, description = FALSE, ...)$data.clust
}

# A CA with enough levels on both margins to be worth clustering.
fx_ca_relig <- function() fx("ca_relig", function() {
  correspondence_analysis(tabxplor::tab(forcats::gss_cat, relig, partyid))
})

# --- hierarchical_clust ------------------------------------------------------------------------

test_that("hierarchical_clust gives HCPC()'s clusters for an MCA, weighted or not", {
  # tea[1:6] is binary: 300 individuals on 35 distinct points, and symmetric enough to tie
  for (k in c(-1, 4, 6)) {
    expect_identical(hc(fx_mca(), ncp = 3, nb_clust = k),
                     hcpc(MCA2(fx_tea(), 1:6, ncp = 3), nb.clust = k)$clust)
  }
  expect_identical(hc(fx_mca(), ncp = 3, nb_clust = 4, consol = FALSE),
                   hcpc(MCA2(fx_tea(), 1:6, ncp = 3), nb.clust = 4, consol = FALSE)$clust)
  expect_identical(hc(fx_mca_wt(), ncp = 3, nb_clust = 5),
                   hcpc(MCA2(fx_tea_wt(), 1:6, wt = "w", ncp = 3), nb.clust = 5)$clust)
})

test_that("hierarchical_clust gives HCPC()'s clusters for a PCA", {
  pca <- FactoMineR::PCA(fx_pca()$call$X, ncp = 2, graph = FALSE)
  expect_identical(unname(hc(fx_pca(), ncp = 2, nb_clust = 3)),
                   unname(hcpc(pca, nb.clust = 3)$clust))
})

test_that("hierarchical_clust gives HCPC()'s clusters for each margin of a CA", {
  ca <- FactoMineR::CA(fx_ca_relig()$call$X, ncp = 2, graph = FALSE)
  for (margin in c("rows", "columns")) {
    new <- hc(fx_ca_relig(), ncp = 2, nb_clust = 4, margin = margin)
    ref <- hcpc(ca, nb.clust = 4, cluster.CA = margin)
    # WARNING: compared BY NAME. HCPC() returns a CA's rows in its own order, sorted along the first
    #   axis (it tests `cluster.CA == "row"`, never true), and its columns in theirs.
    expect_identical(unname(new), unname(ref$clust[match(names(new), rownames(ref))]))
  }
})

test_that("consol = \"weighted\" counts an individual weighted 2 as two copies of it", {
  d <- fx_tea()
  d$w <- rep(1:2, length.out = nrow(d))
  copies <- rep(seq_len(nrow(d)), d$w)
  by_weight <- hc(MCA2(d, 1:6, wt = "w"), ncp = 3, nb_clust = 5, consol = "weighted")
  by_copies <- hc(MCA2(d[copies, ], 1:6), ncp = 3, nb_clust = 5, consol = "weighted")
  expect_identical(by_weight, by_copies[match(seq_len(nrow(d)), copies)])
})

test_that("consol = \"weighted\" leaves each individual in the cluster of its nearest weighted centre", {
  cl <- hc(fx_mca_wt(), ncp = 3, nb_clust = 5, consol = "weighted")
  X  <- fx_mca_wt()$ind$coord[, 1:3]
  w  <- fx_mca_wt()$call$row.w
  centres <- rowsum(X * w, cl) / as.vector(rowsum(w, cl))
  d <- vapply(seq_len(nrow(centres)), function(k) rowSums(sweep(X, 2, centres[k, ])^2),
              numeric(nrow(X)))
  expect_identical(max.col(-d, ties.method = "first"), as.integer(cl))
})

test_that("in mutate(), a CA gives each individual the cluster of its level, NA outside", {
  gss <- forcats::gss_cat |> dplyr::mutate(cl = hc(fx_ca_relig(), ncp = 2, nb_clust = 4))
  by_level <- hc(fx_ca_relig(), ncp = 2, nb_clust = 4)
  expect_identical(gss$cl, unname(by_level[as.character(gss$relig)]))

  ca  <- forcats::gss_cat |> dplyr::filter(relig != "None") |>
    tabxplor::tab(relig, partyid) |> correspondence_analysis()
  gss <- forcats::gss_cat |> dplyr::mutate(cl = hc(ca, ncp = 2, nb_clust = 4))
  expect_true(all(is.na(gss$cl[gss$relig == "None"])))
  expect_false(anyNA(gss$cl[gss$relig != "None"]))
})

test_that("hierarchical_clust asks for what it needs, and says why", {
  expect_error(hc(fx_mca(), nb_clust = 4), "number of axes")
  expect_error(hc(mtcars, ncp = 2), "correspondence analysis")
  expect_error(hc(fx_mca(), ncp = 50), "keeps")
  expect_error(hc(fx_mca(), ncp = 3, nb_clust = 35), "distinct points")
  expect_error(hc(fx_mca(), ncp = 3, nb_clust = 1), "at least 2")
  expect_error(hc(fx_mca(), ncp = 3, nb_clust = 4, consol = "yes"), "weighted")
  expect_error(hc(FactoMineR::MCA(fx_tea()[1:6], ind.sup = 1:10, graph = FALSE), ncp = 3),
               "supplementary individuals")
  expect_error(dplyr::mutate(forcats::gss_cat, cl = hc(fx_ca(), ncp = 2, nb_clust = 2)),
               "correspondence_analysis")
})

test_that("hierarchical_clust draws the tree when it cuts it itself", {
  local_null_device()
  expect_true(formals(hierarchical_clust)$tree == quote(nb_clust == -1))
  expect_s3_class(hierarchical_clust(fx_mca(), ncp = 3), "factor")
  expect_s3_class(hierarchical_clust(fx_ca_relig(), ncp = 2), "factor")
})

test_that("in mutate(), the clusters line up with the whole data frame, NA outside the subset", {
  d   <- fx_tea()
  out <- d |> dplyr::mutate(cl = hc(fx_mca_young(), ncp = 3, nb_clust = 4))
  young <- d$age < 30
  expect_true(all(is.na(out$cl[!young])))
  expect_false(anyNA(out$cl[young]))
  expect_identical(as.character(out$cl[young]),
                   as.character(hc(fx_mca_young(), ncp = 3, nb_clust = 4)[young]))
})

test_that("in mutate() on the subset itself, every row gets its cluster", {
  sub <- fx_tea() |> dplyr::filter(age < 30)
  out <- sub |> dplyr::mutate(cl = hc(fx_mca_young(), ncp = 3, nb_clust = 4))
  expect_false(anyNA(out$cl))
})

test_that("a reordered analysis writes each cluster back on its own row", {
  d   <- fx_tea()
  res <- d |> dplyr::arrange(age) |> MCA2(1:6)
  out <- d |> dplyr::mutate(cl = hc(res, ncp = 3, nb_clust = 4))
  # the same clusters as on the sorted data, sorted back
  sorted <- d |> dplyr::arrange(age) |> dplyr::mutate(cl = hc(res, ncp = 3, nb_clust = 4))
  expect_identical(as.character(out$cl[order(d$age, method = "radix")]), as.character(sorted$cl))
})

test_that("outside mutate(), the clusters line up with the data frame the analysis started from", {
  out <- hc(fx_mca_young(), ncp = 3, nb_clust = 4)
  expect_length(out, nrow(fx_tea()))
  expect_identical(which(!is.na(out)), which(fx_tea()$age < 30))
})

test_that("hierarchical_clust refuses rows it cannot line up, and says why", {
  d <- fx_tea()
  # no rows recorded (%>% pipe): the whole data frame is longer than the analysis
  `%>%` <- magrittr::`%>%`
  res <- d %>% dplyr::filter(age < 30) %>% MCA2(1:6)
  expect_error(dplyr::mutate(d, cl = hc(res, ncp = 3, nb_clust = 4)), "native pipe")
  # the data frame was reordered after the analysis
  expect_error(dplyr::mutate(dplyr::arrange(d, age), cl = hc(fx_mca(), ncp = 3, nb_clust = 4)),
               "reordered or modified")
  # grouped
  expect_error(dplyr::mutate(dplyr::group_by(d, SPC), cl = hc(fx_mca(), ncp = 3, nb_clust = 4)),
               "ungroup")
})

# --- one tree, many cuts ------------------------------------------------------------------------

trees <- function() ggfacto:::clust_cache$trees
no_trees <- function() assign("trees", NULL, envir = ggfacto:::clust_cache)

test_that("a tree is built once, then cut as many times as asked", {
  no_trees()
  hc(fx_mca(), ncp = 3, nb_clust = 4)
  six <- hc(fx_mca(), ncp = 3, nb_clust = 6)
  expect_length(trees(), 1L)
  # a cut of the kept tree is the cut of a fresh one
  no_trees()
  expect_identical(hc(fx_mca(), ncp = 3, nb_clust = 6), six)
  # another number of axes is another tree
  hc(fx_mca(), ncp = 2, nb_clust = 4)
  expect_length(trees(), 2L)
})

test_that("options(ggfacto.clust_cache) bounds the trees kept, and 0 keeps none", {
  no_trees()
  withr::local_options(ggfacto.clust_cache = 2)
  for (ncp in 2:4) hc(fx_mca(), ncp = ncp, nb_clust = 4)
  expect_length(trees(), 2L)
  no_trees()
  withr::local_options(ggfacto.clust_cache = 0)
  hc(fx_mca(), ncp = 3, nb_clust = 4)
  expect_length(trees(), 0L)
})

test_that("names name the clusters, in the order they are given", {
  base     <- hc(fx_mca(), ncp = 3, nb_clust = 4)
  in_order <- hc(fx_mca(), ncp = 3, names = c("A", "B", "C", "D"))
  expect_identical(levels(in_order), c("A", "B", "C", "D"))
  expect_identical(as.integer(in_order), as.integer(base))

  # as in fct_recode(), "new name" = old number, the order of the vector being the order of levels
  recoded <- hc(fx_mca(), ncp = 3, names = c("D" = 4, "A" = 1, "B" = 2, "C" = 3))
  expect_identical(levels(recoded), c("D", "A", "B", "C"))
  expect_identical(as.character(recoded), as.character(in_order))
  # ... or the other way round, which reads as well
  expect_identical(hc(fx_mca(), ncp = 3, names = c("4" = "D", "1" = "A", "2" = "B", "3" = "C")),
                   recoded)
  expect_identical(levels(hc(fx_mca(), ncp = 3, names = c("Petit \u00e9cran" = 1, "B" = 2,
                                                          "C" = 3, "D" = 4)))[1],
                   "Petit \u00e9cran")
  # a correspondence analysis names its clusters of levels the same way
  ca <- hc(fx_ca_relig(), ncp = 2, names = c("a", "b", "c", "d"))
  expect_identical(levels(ca), c("a", "b", "c", "d"))
  expect_identical(names(ca), rownames(fx_ca_relig()$row$coord))
})

test_that("names are refused unless they name each cluster once", {
  expect_error(hc(fx_mca(), ncp = 3, nb_clust = -1, names = c("A", "B")), "nb_clust")
  expect_error(hc(fx_mca(), ncp = 3, nb_clust = 4, names = c("A", "B")), "4 clusters")
  expect_error(hc(fx_mca(), ncp = 3, names = c("A", "A", "B")), "own name")
  expect_error(hc(fx_mca(), ncp = 3, names = c("A" = 1, "B" = 1, "C" = 3)), "fct_recode")
})

test_that("the tree states the share of the inertia its clusters keep", {
  pts <- ggfacto:::clust_points(fx_mca_wt(), 3, "rows")
  t   <- ggfacto:::ward_tree(pts$coord, pts$w, pts$answers)
  h   <- rev(t$tree$height)
  # the gains add up to the inertia of the ncp axes
  expect_equal(sum(h), sum(fx_mca_wt()$eig[1:3, 1]))
  # uncut by a k-means, the share is the gains of the splits kept
  cut <- ggfacto:::cut_ward_tree(t, pts$coord, pts$w, 4, FALSE)
  expect_equal(cut$between, sum(h[seq_len(cut$nb_clust - 1)]) / sum(h))
  # consolidated, it is still 1 - within / total, with weighted centres
  cut <- ggfacto:::cut_ward_tree(t, pts$coord, pts$w, 4, TRUE)
  X <- pts$coord; w <- pts$w; cl <- as.integer(cut$clust)
  centres <- rowsum(X * w, cl) / as.vector(rowsum(w, cl))
  within  <- sum(w * rowSums((X - centres[cl, ])^2))
  total   <- sum(w * rowSums(sweep(X, 2, colSums(X * w) / sum(w))^2))
  expect_equal(cut$between, 1 - within / total)
})

# --- clust_tab ---------------------------------------------------------------------------------

rows <- function() tidyselect::all_of(fx_active())

test_that("clust_tab takes the variables, the weights and the rows from the analysis", {
  d <- fx_tea_wt()
  d$clust <- hc(fx_mca_wt(), ncp = 3, nb_clust = 4)
  new <- clust_tab(fx_mca_wt(), d, clust)
  old <- suppressWarnings(HCPC_tab(d, rows(), clust, wt = w))
  expect_s3_class(new, "ggfacto_summary")
  expect_identical(as.character(new), as.character(old))
  # the weights are the analysis's: unweighted, the table differs
  expect_false(identical(as.character(new),
                         as.character(suppressWarnings(HCPC_tab(d, rows(), clust)))))
  expect_error(clust_tab(fx_mca_wt(), d, clust, wt = w), "weights of the analysis")
})

test_that("clust_tab accepts the clusters as a bare name, a string and a vector", {
  d <- fx_tea_clust()
  by_symbol <- clust_tab(fx_mca(), d, clust)
  expect_identical(as.character(by_symbol), as.character(clust_tab(fx_mca(), d, "clust")))
  expect_identical(as.character(by_symbol), as.character(clust_tab(fx_mca(), d, d$clust)))
})

test_that("clust_tab describes the rows of an analysed subset, given the whole data frame", {
  d <- fx_tea() |> dplyr::mutate(cl = hc(fx_mca_young(), ncp = 3, nb_clust = 4))
  tab <- clust_tab(fx_mca_young(), d, cl)
  expect_false(any(grepl("^NA$", names(tab))))
  expect_identical(as.character(tab),
                   as.character(clust_tab(fx_mca_young(), dplyr::filter(d, age < 30), cl)))
})

test_that("clust_tab of a PCA grades its means, and puts the population under them", {
  d <- mtcars[1:7]
  names(d)[names(d) == "wt"] <- "weight"
  d$clust <- hc(fx_pca(), ncp = 2, nb_clust = 3)
  tab <- clust_tab(fx_pca(), d, clust)
  expect_setequal(as.character(tab$variables), names(d)[1:7])
  expect_true(any(!is.na(tabxplor::fmt_get_color_code(tab$`1`))))
  expect_true("% of population" %in% as.character(tabxplor::get_footer_tabs(tab)[[1]]$lvs))
})

test_that("clust_tab sends a correspondence analysis to tab()", {
  expect_error(clust_tab(fx_ca_relig(), forcats::gss_cat, relig), "tab\\(data, clust")
})

test_that("a binary variable is one row read down the clusters, two read across them", {
  lvs <- function(tab) as.character(tab$lvs)
  expect_false("Not.breakfast" %in% lvs(clust_tab(fx_mca(), fx_tea_clust(), clust)))
  expect_true(all(c("breakfast", "Not.breakfast") %in%
                    lvs(clust_tab(fx_mca(), fx_tea_clust(), clust, pct = "row"))))
})

test_that("a number is a mean row, unless `shape` cuts it into levels", {
  d <- fx_tea_clust()
  n_rows <- function(...) sum(clust_tab(fx_mca(), d, clust, row_vars = c(sex, age), ...)$variables
                              == "age")
  expect_identical(n_rows(), 1L)
  expect_identical(n_rows(shape = "sd_bands"), 4L)
  expect_identical(n_rows(shape = c(age = "quintiles")), 5L)
})

test_that("clust_tab hides excluded levels, the missing answers first", {
  res <- MCA2(fx_tea_na(), 1:6)
  d   <- fx_tea_na()
  d$clust <- hc(res, ncp = 3, nb_clust = 4)
  lvs <- function(tab) as.character(tab$lvs)
  expect_false(any(grepl("\\.NA$", lvs(clust_tab(res, d, clust)))))
  expect_true("breakfast.NA" %in% lvs(clust_tab(res, d, clust, excl = NULL)))
  expect_false("Not.lunch" %in% lvs(clust_tab(res, d, clust, excl = c(NA, "Not.lunch"))))
})

test_that("clust_tab cleans the cluster names once, for the level rows and the population rows", {
  d <- fx_tea_clust()
  d$clust <- forcats::fct_relabel(d$clust, ~ paste0(.x, "-Cluster ", .x))
  tab <- clust_tab(fx_mca(), d, clust)
  expect_false(any(grepl("^[0-9]-", names(tab))))
  expect_true(all(paste0("Cluster ", levels(fx_tea_clust()$clust)) %in% names(tab)))
  expect_true(any(grepl("^1-", names(clust_tab(fx_mca(), d, clust, cleannames = FALSE)))))
})

test_that("the former form -- HCPC_tab(), or a data frame first -- still works, saying so once", {
  e <- ggfacto:::deprecated_args_warned
  rm(list = ls(envir = e), envir = e)
  d   <- fx_tea_clust()
  new <- as.character(clust_tab(fx_mca(), d, clust))
  expect_warning(old <- HCPC_tab(d, rows(), clust), "clust_tab\\(res, data, clust\\)")
  expect_identical(as.character(old), new)
  expect_no_warning(by_position <- clust_tab(d, rows(), clust))
  expect_identical(as.character(by_position), new)
  expect_identical(as.character(clust_tab(data = d, row_vars = rows(), clust = "clust")), new)
  expect_identical(as.character(d |> clust_tab(rows(), clust)), new)
  # rows without a cluster are left out
  d$clust[1:10] <- NA
  expect_identical(as.character(HCPC_tab(d, rows(), clust)),
                   as.character(HCPC_tab(d[-(1:10), ], rows(), clust)))
})

# --- the former `cah` name --------------------------------------------------------------------

test_that("`cah =` still works, and says it is now `clust =`", {
  e <- ggfacto:::deprecated_args_warned
  rm(list = ls(envir = e), envir = e)
  expect_warning(pd <- md(fx_mca(), fx_tea_clust(), cah = "clust", profiles = TRUE), "clust")
  expect_identical(pd$clust, "clust")
})
