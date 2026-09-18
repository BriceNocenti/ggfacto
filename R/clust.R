# PURPOSE: Hierarchical clustering on the axes of an analysis, and the table that describes its
#   clusters -- hierarchical_clust(), clust_tab(), and HCPC_tab(), the former form of the table.
# ROLE: The last step of the workflow. hierarchical_clust() clusters the individuals of an MCA or a
#   PCA, or the levels of one margin of a CA, and returns the clusters as a factor, to be written
#   into the data frame with mutate(); clust_tab() and ggmca(clust =) read that column back.
#   resolve_clust() is the one reader of a `clust =` argument, shared with R/mca-data.R.
# KEY CONSTRAINTS:
#   - The clusters are FactoMineR::HCPC()'s, computed here for their memory: Ward's tree is built on
#     the DISTINCT points of the cloud by fastcluster, in memory linear in their number -- an MCA's
#     answer profiles, read from its model (R/model.R); the point order, the cut and, by default,
#     the unweighted k-means consolidation on the individuals are HCPC()'s, and test-clust.R pins
#     the equality with HCPC() run on individuals. `consol = "weighted"` opts in to a weighted
#     k-means.
#   - The tree is built once and cut many times: ward_tree() is memoised for the session on the
#     content it is built from, so another `nb_clust` or `names` costs only cut_ward_tree(). That
#     split is also the seam a jamovi module keeps in its state.
#   - The clusters are a column of the data frame, never an HCPC object, written back by
#     to_data_rows() (R/model.R): aligned on the mutate() they are written in, else on the frame the
#     analysis started from.
#   - clust_tab() reads the analysis -- its active variables, weights and fitted rows -- so nothing
#     is given twice. HCPC_tab() and a data frame given first are the former form, on one builder.
# See: CLAUDE.md section ggfacto architecture > The FactoMineR contract, and
#   dev/hierarchical_clustering.md for the design of the workflow and the options weighed.

#' Hierarchical Clustering on the Axes of an Analysis
#'
#' @description
#' Clusters the individuals of a principal component analysis or of a multiple correspondence
#' analysis, or the levels of one margin of a correspondence analysis, on the first `ncp` axes. The
#' clusters are those of \code{FactoMineR::\link[FactoMineR]{HCPC}}: Ward's hierarchical
#' clustering, cut into `nb_clust` clusters, then consolidated by k-means. Use it inside
#' \code{dplyr::mutate()} to add them to the data frame:
#'
#' `data <- data |> mutate(clust = hierarchical_clust(res, ncp = 3, nb_clust = 6))`
#'
#' To choose the number of clusters, look at the tree first: `hierarchical_clust(res, ncp = 3)`.
#' The tree is built once: cutting it again, into another number of clusters or with names, is
#' instant. Name the clusters with `names`, then describe them with \code{\link{clust_tab}} and
#' draw them with \code{\link{ggmca}(clust = )}.
#'
#' @param res An analysis made with \code{\link{multiple_correspondence_analysis}},
#' \code{\link{principal_component_analysis}} or \code{\link{correspondence_analysis}} (or with
#' \code{FactoMineR::MCA()}, \code{PCA()} or \code{CA()}, or \code{GDAtools::speMCA()} or
#' \code{csMCA()}, whose subcloud alone is clustered).
#' @param ncp The number of axes to cluster on: the first ones, those worth interpreting (see the
#' eigenvalues under \code{\link{interpret}}). There is no default: on every axis, the
#' clustering would follow noise.
#' @param nb_clust The number of clusters. With `-1`, the default, the tree is cut where the gain in
#' between-cluster inertia drops the most. Given `names`, it is the number of names.
#' @param tree Should the clustering tree be drawn, to choose the number of clusters? By default,
#' only when `nb_clust = -1`. Its leaves are the distinct points of the cloud: the answer profiles
#' of a multiple correspondence analysis, the levels of a correspondence analysis. Each bar of the
#' inertia gains carries the number of clusters it makes, and the title the share of the inertia of
#' the `ncp` axes that lies between the clusters drawn.
#' @param consol The k-means consolidation, which moves each individual to its nearest cluster once
#' the tree is cut. `TRUE`, the default, is \code{FactoMineR::\link[FactoMineR]{HCPC}}'s k-means,
#' which counts every individual once, weights or not. `"weighted"` counts each by its weight (the
#' survey weights, or a level's count in a correspondence analysis) with a simpler k-means, Lloyd's,
#' which can place a few individuals differently even without weights. `FALSE` keeps the clusters as
#' the tree cuts them.
#' @param margin For a correspondence analysis, the levels to cluster: `"rows"`, the default, or
#' `"columns"`.
#' @param names The names of the clusters, in the order the levels should take: either
#' `c("Name 1", "Name 2", ...)`, for clusters 1, 2, etc., or `c("Name" = 2, "Other name" = 1, ...)`,
#' as in \code{forcats::fct_recode()}. Clusters are numbered along the first axis, so the names
#' belong to one cut of one tree: write them after looking at that cut.
#'
#' @details
#' The tree is kept in memory for the session, under a key made of everything it is built from ---
#' the coordinates on the `ncp` axes, the weights, the answer profiles --- so a tree made from other data is
#' never reused. The last 20 trees are kept; `options(ggfacto.clust_cache = 50)` keeps more, and
#' `options(ggfacto.clust_cache = 0)` none.
#'
#' @return A factor with the clusters, `"1"`, `"2"`, etc. or their `names`, numbered along the first
#' axis and returned invisibly: a bare call only draws the tree. Inside \code{dplyr::mutate()}, it
#' has one value per row of that data frame, and `NA` on the rows the analysis did not use (when it
#' was made on a subset of the population). Outside \code{mutate()}, it has one value per row of the
#' data frame the analysis started from. For a correspondence analysis, each row of the data frame
#' gets the cluster of its level (`NA` for a level outside the table); outside \code{mutate()},
#' there is one value per level, named after it.
#' @export
#'
#' @examples
#' data(tea, package = "FactoMineR")
#' res.mca <- multiple_correspondence_analysis(tea, 1:18)
#'
#' # The tree, to choose the number of clusters, then the clusters, written into the data frame
#' hierarchical_clust(res.mca, ncp = 3)
#' tea <- tea |>
#'   dplyr::mutate(clust = hierarchical_clust(res.mca, ncp = 3, nb_clust = 6))
#'
#' clust_tab(res.mca, tea, clust)
#' ggmca(res.mca, tea, clust = clust)
#'
#' # Named, in the order of your choice (the tree is not built again)
#' tea <- tea |>
#'   dplyr::mutate(clust = hierarchical_clust(res.mca, ncp = 3, names = c(
#'     "Cluster A" = 1, "Cluster B" = 2, "Cluster C" = 3, "Cluster E" = 5, "Cluster D" = 4,
#'     "Cluster F" = 6
#'   )))
#'
#' # On a subset of the population: the other rows get NA
#' res.mca_young <- tea |>
#'   dplyr::filter(age < 30) |>
#'   multiple_correspondence_analysis(1:18)
#' tea <- tea |>
#'   dplyr::mutate(clust_young = hierarchical_clust(res.mca_young, ncp = 3, nb_clust = 4))
#'
#' # A correspondence analysis clusters the levels of one margin: each individual gets the
#' # cluster of its level
#' res.ca <- forcats::gss_cat |>
#'   tabxplor::tab(relig, partyid) |>
#'   correspondence_analysis()
#' gss <- forcats::gss_cat |>
#'   dplyr::mutate(relig_clust = hierarchical_clust(res.ca, ncp = 2, nb_clust = 4))
hierarchical_clust <- function(res, ncp, nb_clust = -1, tree = nb_clust == -1, consol = TRUE,
                               margin = "rows", names = NULL) {
  if (!(is_mca(res) || inherits(res, c("PCA", "CA")))) stop(
    "hierarchical_clust() clusters the individuals of a principal component analysis or of a ",
    "multiple correspondence analysis, or the levels of a correspondence analysis.", call. = FALSE)
  if (missing(ncp)) stop(
    "`ncp` is required: the number of axes to cluster on, those worth interpreting (read the ",
    "eigenvalues under interpret()). On every axis, the clustering would follow noise.",
    call. = FALSE)
  if (!is.null(names)) {
    if (missing(nb_clust)) nb_clust <- length(names) else if (nb_clust == -1) stop(
      "`names` names the clusters of one cut: give their number with `nb_clust`, or leave it out ",
      "to take the number of names.", call. = FALSE)
  }
  force(tree)
  if (!(nb_clust == -1 || nb_clust >= 2)) stop(
    "`nb_clust` is the number of clusters, at least 2, or -1 to cut the tree where the gain in ",
    "between-cluster inertia drops the most.", call. = FALSE)
  if (!(isTRUE(consol) || isFALSE(consol) || identical(consol, "weighted"))) stop(
    "`consol` is TRUE (FactoMineR's k-means), \"weighted\" (a k-means counting each individual by ",
    "its weight) or FALSE (no consolidation).", call. = FALSE)

  pts <- clust_points(res, ncp, margin)
  hc  <- cut_ward_tree(cached_ward_tree(pts$coord, pts$w, pts$groups), pts$coord, pts$w,
                       nb_clust, consol)
  if (tree) plot_clust_tree(hc, labels = inherits(res, "CA"), ncp = ncp)
  clust <- if (is.null(names)) hc$clust else name_clusters(hc$clust, names)

  # invisible: a bare call, made to look at the tree, must not print one cluster per individual
  if (inherits(res, "CA")) {
    names(clust) <- rownames(pts$coord)
    target <- tryCatch(dplyr::pick(tidyselect::any_of(pts$vars)), error = function(e) NULL)
    return(invisible(if (is.null(target)) clust else {
      level_values(clust, target, pts$vars, "hierarchical_clust")
    }))
  }
  invisible(to_data_rows(pts$fit, clust, pts$vars, "hierarchical_clust"))
}

# The points HCPC() clusters, one per fitted individual (or per level of a CA), their weights, the
# distinct points they group into, and the variables that locate them in a data frame.
clust_points <- function(res, ncp, margin) {
  if (inherits(res, "CA")) {
    margin  <- match.arg(margin, c("rows", "columns"))
    rows    <- margin == "rows"
    coord   <- if (rows) res$row$coord else res$col$coord
    w       <- (if (rows) res$call$marge.row else res$call$marge.col) * sum(res$call$X)
    vars    <- names(dimnames(res$call$X))[if (rows) 1L else 2L]
    vars    <- vars[!is.na(vars) & nzchar(vars)]     # FactoMineR::CA() keeps no variable names
    groups  <- NULL
    fit     <- res
  } else if (is_mca(res)) {
    fit     <- mca_model(res)
    coord   <- fit$coord[fit$key, , drop = FALSE]
    w       <- fit_weights(fit)
    vars    <- fit$vars
    groups  <- fit$key
  } else {
    if (length(res$call$ind.sup) != 0) stop(
      "hierarchical_clust() does not handle supplementary individuals.", call. = FALSE)
    fit     <- res
    coord   <- res$ind$coord
    w       <- fit_weights(res)
    vars    <- active_names(res)
    groups  <- as.integer(vctrs::vec_group_id(res$call$X[vars]))
  }
  if (ncp > ncol(coord)) stop(
    "`ncp` is ", ncp, ", and the analysis keeps ", ncol(coord), " axes.", call. = FALSE)
  list(coord = coord[, seq_len(ncp), drop = FALSE], w = w, groups = groups, vars = vars,
       fit = fit)
}

clust_cache <- new.env(parent = emptyenv())

# DESIGN: the tree is ~90 % of the time and cutting it the rest, so a tree is kept for the session.
#   The key is the CONTENT it is built from, never the analysis object: a cached tree cannot be
#   stale, a refit hits it, and a jamovi state can store the same key beside the same tree.
# WARNING: every input of ward_tree() must be in the key -- the dimnames too (a CA's leaves carry
#   its row names). The last `ggfacto.clust_cache` trees are kept, first in first out.
cached_ward_tree <- function(coord, w, groups) {
  size <- getOption("ggfacto.clust_cache", 20)
  if (!isTRUE(size >= 1)) return(ward_tree(coord, w, groups))
  key   <- rlang::hash(list(coord, w, groups))
  trees <- clust_cache$trees
  if (is.null(trees[[key]])) {
    trees[[key]] <- ward_tree(coord, w, groups)
    clust_cache$trees <- trees[seq.int(max(1L, length(trees) - size + 1L), length(trees))]
  }
  trees[[key]]
}

# Ward's tree of FactoMineR::HCPC() -- its point order, from its code by F. Husson, G. Le Ray and
# Q. Molto (credited in DESCRIPTION) -- built on the distinct points. A plain list, so it can be
# stored as it is.
ward_tree <- function(coord, w, groups) {
  # DESIGN: the points in HCPC()'s order, sorted along the first axis: it decides which pair merges
  #   on a tie and where the k-means starts, so the clusters are HCPC()'s own and not a variant.
  ord <- order(coord[, 1])
  X   <- coord[ord, , drop = FALSE]
  # DESIGN: the leaves are the DISTINCT points (`groups`: an MCA's answer profiles), weighted by
  #   their individuals: Ward merges identical points first, at no cost, so the tree above them is
  #   the same (leaves numbered in HCPC()'s order).
  leaf  <- if (is.null(groups)) seq_len(nrow(X)) else vctrs::vec_group_id(groups[ord])
  first <- which(!duplicated(leaf))
  lw    <- as.vector(rowsum(w[ord], leaf))
  tree  <- fastcluster::hclust.vector(X[first, , drop = FALSE], method = "ward", members = lw)
  # WARNING: fastcluster's Ward height is sqrt(2 x the inertia gain); HCPC() plots and cuts the
  #   gain, as a share of the total weight.
  tree$height <- tree$height^2 / 2 / sum(w)
  if (!is.null(groups)) tree$labels <- NULL        # only a CA's leaves, its levels, are drawn named
  list(tree = tree, ord = ord, leaf = leaf, first = first, lw = lw)
}

# HCPC()'s cut and consolidation of a tree ward_tree() built from the same points.
cut_ward_tree <- function(t, coord, w, nb_clust, consol) {
  n <- nrow(coord)
  X <- coord[t$ord, , drop = FALSE]
  if (nb_clust >= length(t$first)) stop(
    "`nb_clust` must be less than the number of distinct points to cluster, ", length(t$first), ".",
    call. = FALSE)
  if (nb_clust == -1) nb_clust <- auto_nb_clust(rev(t$tree$height), n, ncol(X))

  h   <- rev(t$tree$height)
  cut <- (h[nb_clust - 1] + h[nb_clust]) / 2
  cl  <- unname(stats::cutree(t$tree, h = cut))
  if (identical(consol, "weighted")) {
    km      <- weighted_kmeans(X[t$first, , drop = FALSE], t$lw, cl)
    cl      <- km$cluster[t$leaf]
    centers <- km$centers
  } else {
    cl      <- cl[t$leaf]
    centers <- rowsum(X, cl) / tabulate(cl)
    # DESIGN: HCPC()'s consolidation, the default, is an UNWEIGHTED k-means on the individuals, kept
    #   as is so the clusters stay HCPC()'s, survey weights or not.
    if (isTRUE(consol)) {
      km      <- stats::kmeans(X, centers = centers, iter.max = 10)
      cl      <- km$cluster
      centers <- km$centers
    }
  }
  cl  <- order(order(centers[, 1]))[cl]
  out <- integer(n)
  out[t$ord] <- cl
  list(clust = factor(out), tree = t$tree, cut = cut, nb_clust = max(cl), leaf_clust = cl[t$first],
       between = between_share(X, w[t$ord], cl))
}

# The share of the inertia of the points that lies between the clusters, with weighted centres,
# whatever the consolidation: what a partition keeps of the cloud.
between_share <- function(X, w, cl) {
  g  <- colSums(X * w) / sum(w)
  wk <- as.vector(rowsum(w, cl))
  gk <- rowsum(X * w, cl) / wk
  sum(wk * rowSums(sweep(gk, 2, g)^2)) / sum(w * rowSums(sweep(X, 2, g)^2))
}

# Why this exists: stats::kmeans() takes no weights. Lloyd's algorithm with weighted centres, from
# the tree's clusters, on the distinct points: identical individuals always share their nearest
# centre, so it is the same k-means as on the individuals. A cluster that empties is dropped.
weighted_kmeans <- function(X, w, cl, iter.max = 100) {
  centers <- function(cl) rowsum(X * w, cl) / as.vector(rowsum(w, cl))
  for (i in seq_len(iter.max)) {
    ctr <- centers(cl)
    d   <- vapply(seq_len(nrow(ctr)), function(k) rowSums(sweep(X, 2, ctr[k, ])^2),
                  numeric(nrow(X)))
    new <- max.col(-d, ties.method = "first")
    if (identical(new, cl)) break
    cl <- new
  }
  cl <- match(cl, sort(unique(cl)))
  list(cluster = cl, centers = centers(cl))
}

# HCPC()'s rule for `nb_clust = -1` (its auto.cut.tree(), defaults min = 3, max = 10): the number of
# clusters whose last split gained the most within-cluster inertia relative to the next one.
auto_nb_clust <- function(gain, n, d) {
  gain  <- c(gain, numeric(n - 1 - length(gain)))    # the merges of identical points, at no cost
  intra <- rev(cumsum(rev(gain)))
  norm  <- seq_along(intra)^(2 / d) * intra
  max   <- min(max(min(10, round(n / 2)), 3), n - 1)
  quot  <- vapply(3:max, function(k) abs((norm[k - 1] - norm[k]) / (norm[k] - norm[k + 1])),
                  numeric(1))
  which.max(quot) + 2L
}

# The clusters renamed by `names`: positional, as in fct_recode() ("Name" = 1), or the other way
# round ("1" = "Name") when only that reading fits. The order of `nm` is the order of the levels.
name_clusters <- function(clust, nm) {
  ids  <- levels(clust)
  from <- ids
  to   <- as.character(nm)
  keys <- names(nm)
  if (!is.null(keys) && any(nzchar(keys))) {
    fwd <- setequal(to, ids) && !anyDuplicated(to)
    rev <- setequal(keys, ids) && !anyDuplicated(keys)
    if (!all(nzchar(keys)) || fwd == rev) stop(
      "`names` maps each cluster, 1 to ", length(ids), ", to its name, once, as in fct_recode(): ",
      "`names = c(\"First name\" = 1, \"Second name\" = 2, ...)`.", call. = FALSE)
    if (fwd) {
      from <- to
      to   <- keys
    } else {
      from <- keys
    }
  }
  if (length(to) != length(ids)) stop(
    "`names` has ", length(to), " names, and the tree is cut into ", length(ids), " clusters.",
    call. = FALSE)
  if (anyNA(to) || !all(nzchar(to)) || anyDuplicated(to)) stop(
    "`names` must give each cluster its own name.", call. = FALSE)
  factor(to[match(as.character(clust), from)], levels = to)
}

# Why this exists: HCPC()'s tree plot -- the inertia gains top right, one rectangle per cluster --
# drawn from the tree ward_tree() builds, each bar labelled with the number of clusters it makes.
plot_clust_tree <- function(hc, labels, ncp) {
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op))
  graphics::layout(matrix(c(rep(c(2, 4, 4, 4, 4), 4), 1, 3, 3, 3, 3), 5), respect = TRUE)
  gain <- rev(hc$tree$height)
  gain <- gain[seq_len(min(15, length(gain)))]
  graphics::par(mar = c(1, 2, 0.75, 0))
  mids <- graphics::barplot(gain, col = ifelse(seq_along(gain) < hc$nb_clust, "black", "grey"),
                            space = 0.9)
  graphics::axis(1, at = mids, labels = seq_along(gain) + 1L, tick = FALSE, line = -0.9,
                 cex.axis = 0.55, gap.axis = -1)
  graphics::par(mar = c(0.5, 2, 0.75, 0))
  graphics::plot.new()
  words <- clust_tree_caption(hc$nb_clust, hc$between, ncp)
  graphics::text(0.5, 0.62, words[["title"]], cex = 2)
  graphics::text(0.5, 0.12, words[["caption"]], cex = 1.1)
  graphics::plot.new()
  graphics::legend("top", words[["gain"]])
  graphics::plot(hc$tree, labels = if (labels) NULL else FALSE, hang = -1,
                 main = "", xlab = "", ylab = "", sub = "")
  stats::rect.hclust(hc$tree, h = hc$cut, border = unique(hc$leaf_clust[hc$tree$order]))
}

# The words of the tree plot, in the session's language (or `lang`).
clust_tree_caption <- function(k, between, ncp, lang = NULL) {
  with_gda_lang(lang, function(lg) {
    pct <- gda_num(round(100 * between, 1), lg)
    c(title   = gettext("Hierarchical clustering"),
      gain    = gettext("inertia gain"),
      caption = if (ncp == 1) {
        gettextf("%d clusters: the between-cluster inertia is %s%% of the inertia of axis 1",
                 k, pct)
      } else {
        gettextf("%d clusters: the between-cluster inertia is %s%% of the inertia of axes 1 to %d",
                 k, pct, ncp)
      })
  })
}

#' Describe Clusters with One Table
#'
#' @description
#' One table describing every cluster: each variable's levels down the page, the clusters across it,
#' and a colour saying at a glance which levels a cluster is made of. Give it the analysis, the data
#' frame and the clusters, in the order of \code{\link{ggmca}}: the active variables, the weights
#' and the rows the analysis was made on are its own. Numeric variables come in as mean rows,
#' coloured by their difference to the mean in standard deviations; the last two rows give each
#' cluster's share of the population and its size (under the table when every row is a mean, as in
#' a principal component analysis).
#'
#' @param res The analysis the clusters were made on, with
#' \code{\link{multiple_correspondence_analysis}} or \code{\link{principal_component_analysis}}
#' (or \code{FactoMineR::MCA()} or \code{PCA()}, or \code{GDAtools::speMCA()} or \code{csMCA()}). For a correspondence analysis, cross
#' the clusters with the other variable of the table with \code{tabxplor::tab()} instead.
#' @param data The data frame, with the clusters. The whole data frame will do when the analysis
#' was made on a subset of it: only the rows the analysis used are described.
#' @param clust The variable with the clusters, typically made with \code{\link{hierarchical_clust}},
#' as a bare name or a string.
#' @param row_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> The variables to describe the
#' clusters with: by default, the active variables of the analysis. Numeric ones become mean rows,
#' unless `shape` (passed on to \code{tabxplor::tab()}) cuts them into levels, e.g.
#' `shape = "sd_bands"`, or `shape = c(AGE = "quintiles")` for that one.
#' @param pct `"col"` (default) reads each cluster as a distribution: of the people in this cluster,
#' what percentage are in this level. `"row"` reads each level as a distribution across clusters.
#' @param excl The levels not to show, matched exactly by name; their individuals still count in the
#' percentages. `NA`, the default, hides the missing values (and the levels named `<VAR>.NA`);
#' `excl = NULL` shows every level.
#' @param color The colour measure, see \code{\link[tabxplor]{tab}}. With `"difference"` (the
#' default), percentages are coloured by their difference with the whole population, and means by
#' their difference in standard deviations --- but not both in one table, which a single ladder
#' cannot grade: there, means stay uncoloured, and `"ratio"` colours every row.
#' @param row_tot The name of the row giving each cluster's share of the population.
#' @param cleannames Set to \code{FALSE} to keep the level and cluster names as they are, prefix
#' numbers like \code{"1-"} and text in parentheses included.
#' @param ... Additional arguments to pass to \code{\link[tabxplor]{tab}}.
#' @param wt Not used: the table is weighted with the weights of the analysis. It is there for the
#' former form, `clust_tab(data, row_vars, clust, wt)`, still read as \code{\link{HCPC_tab}}.
#'
#' @return A \code{tabxplor} table --- see [ggfacto_summary] for how it prints.
#' @export
#' @seealso [ggfacto_summary], [hierarchical_clust()], [interpret()].
#'
#' @examples
#' data(tea, package = "FactoMineR")
#' res.mca <- multiple_correspondence_analysis(tea, 1:18)
#' tea <- tea |>
#'   dplyr::mutate(clust = hierarchical_clust(res.mca, ncp = 3, nb_clust = 6))
#'
#' # ONE option decides how every tabxplor table prints, an interpretation table included.
#' # In a script it goes once, at the top, beside the library() calls.
#' options(tabxplor.print = "html")
#'
#' # The clusters, by the active variables
#' clust_tab(res.mca, tea, clust)
#'
#' # ... and by other variables
#' clust_tab(res.mca, tea, clust, row_vars = c(sex, SPC, age), pct = "row")
#'
#' # A principal component analysis: the means of each cluster
#' res.pca <- principal_component_analysis(mtcars, 1:7)
#' cars <- mtcars |>
#'   dplyr::mutate(clust = hierarchical_clust(res.pca, ncp = 2, nb_clust = 3))
#' clust_tab(res.pca, cars, clust)
clust_tab <- function(res, data, clust, row_vars, pct = "col", excl = NA, color = "difference",
                      row_tot = "% of population", cleannames = TRUE, ..., wt) {
  # The former form, a data frame first, binds to HCPC_tab()'s signature argument for argument.
  if (missing(res) || is.data.frame(res)) {
    call <- sys.call()
    call[[1]] <- HCPC_tab
    if ("res" %in% names(call)) names(call)[names(call) == "res"] <- "data"
    return(eval(call, parent.frame()))
  }
  if (!(is_mca(res) || inherits(res, c("PCA", "CA")))) stop(
    "clust_tab() describes the clusters of an analysis: give it first, then the data frame and the ",
    "clusters, as in `clust_tab(res.mca, data, clust)`.", call. = FALSE)
  if (inherits(res, "CA")) stop(
    "A correspondence analysis clusters the levels of one variable: describe the clusters by the ",
    "other one with tabxplor, e.g. `tab(data, clust, other_variable, pct = \"row\", ",
    "color = \"contrib\")`.", call. = FALSE)
  need_data(missing(data), "the clusters", "clust_tab")
  if (!missing(wt)) stop(
    "clust_tab() weights the table with the weights of the analysis: leave out `wt`.",
    call. = FALSE)

  clust <- resolve_clust(rlang::enquo(clust), data)
  if (length(clust$name) == 0) stop("`clust` is required: the variable with the clusters.",
                                    call. = FALSE)
  view     <- fit_view(res)
  data     <- align_to_fit(view, clust$data)
  row_vars <- if (missing(row_vars)) {
    view$vars
  } else {
    names(tidyselect::eval_select(rlang::enquo(row_vars), data))
  }

  w  <- fit_weights(if (is_mca(res)) view else res)
  wt <- character()
  if (any(w != 1)) {
    wt <- if (is.null(view$source$wt)) "row.w" else view$source$wt
    data[[wt]] <- w
  }
  clust_tab_build(data, row_vars, clust$name, wt, excl, color, pct, row_tot, cleannames, ...)
}

#' Describe Clusters with One Table: the Former Form
#'
#' @description
#' Deprecated. `HCPC_tab(data, row_vars, clust, wt)`, and `clust_tab()` given a data frame first,
#' are the former form of \code{\link{clust_tab}}, which now takes the analysis first and reads the
#' active variables, the weights and the rows from it: `clust_tab(res.mca, data, clust)`.
#'
#' @inheritParams clust_tab
#' @param data A data frame.
#' @param row_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> The variables to describe the
#' clusters with. Numeric ones become mean rows.
#' @param clust The variable with the clusters, as a bare name or a string, or the clusters
#' themselves. Rows without a cluster are left out.
#' @param wt The weight variable. Leave empty for unweighted results.
#'
#' @return A \code{tabxplor} table --- see [ggfacto_summary] for how it prints.
#' @keywords internal
#' @export
HCPC_tab <- function(data, row_vars = character(), clust, wt, excl = NA,
                     color = "difference", pct = "col", row_tot = "% of population",
                     cleannames = TRUE, ...) {
  deprecated_fn("HCPC_tab", msg = str_c(
    "HCPC_tab() and clust_tab(data, ...) are deprecated: use clust_tab(res, data, clust)."))
  row_vars <- names(tidyselect::eval_select(rlang::enquo(row_vars), data))
  wt       <- if (missing(wt)) character() else as.character(rlang::ensym(wt))
  clust    <- resolve_clust(rlang::enquo(clust), data)
  if (length(clust$name) == 0) stop("`clust` is required: the variable with the clusters.",
                                    call. = FALSE)
  clust_tab_build(clust$data, row_vars, clust$name, wt, excl, color, pct, row_tot, cleannames,
                  ...)
}

# The table itself, from names alone -- the variables, the clusters and the weights as strings.
# It is tabxplor's: a number enters as a MEAN column and is transposed into a mean row, a factor
# needs no transposition at all.
clust_tab_build <- function(data, row_vars, clust, wt, excl, color, pct, row_tot, cleannames,
                            ...) {
  data <- data |>
    dplyr::select(tidyselect::all_of(row_vars), tidyselect::all_of(wt),
                  clust = tidyselect::all_of(clust)) |>
    dplyr::filter(!is.na(.data$clust))
  # WARNING: the cluster names are cleaned HERE, once, for every block: the level rows and the
  #   population rows are two tab() calls, and names cleaned in one only would bind as two sets of
  #   columns ("1-Petit ecran" beside "Petit ecran").
  if (cleannames) data$clust <- forcats::fct_relabel(as.factor(data$clust),
                                                     ~ str_remove_all(., cleannames_condition()))

  # A number is a mean row, unless `shape` asks tabxplor to cut it into levels.
  dots   <- list(...)
  is_num <- purrr::map_lgl(data[row_vars], is.numeric)
  cut    <- if (is.null(dots$shape)) character() else {
    if (is.null(names(dots$shape))) row_vars[is_num] else intersect(names(dots$shape), row_vars)
  }
  num_vars <- row_vars[is_num & !row_vars %in% cut]
  fct_vars <- setdiff(row_vars, num_vars)

  # Excluded levels are HIDDEN, not dropped: their individuals still count in each cluster's
  # percentages, which is why a column may not sum to 100.
  data <- na_levels(data, fct_vars)
  ex   <- excl_levels(data, fct_vars[!is_num[fct_vars]], excl)
  data <- data |>
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(names(ex)),
      ~ forcats::fct_other(., drop = ex[[dplyr::cur_column()]], other_level = "Remove levels")
    ))
  wt <- if (length(wt) == 0) rlang::expr(NA) else rlang::sym(wt)

  blocks <- list()
  if (length(fct_vars) != 0) {
    fct_tab <- tabxplor::tab(data, row_vars = tidyselect::all_of(fct_vars), col_vars = "clust",
                             wt = !!wt, pct = pct, tot = "col", na = "drop",
                             cleannames = cleannames, color = color, ...) |>
      dplyr::ungroup()
    # tabxplor names a single row variable's column after it, where several share `row_var`
    if (!"row_var" %in% names(fct_tab)) fct_tab <- fct_tab |>
      dplyr::rename(levels = tidyselect::all_of(fct_vars)) |>
      dplyr::mutate(row_var = fct_vars, .before = 1)
    # DESIGN: a BINARY variable says everything in one row only when each cluster is read as a
    #   distribution (`pct = "col"`): its second level is then the first one upside down. Read
    #   across clusters, the second level is a distribution of its own.
    if (pct == "col") {
      binary  <- fct_vars[purrr::map_lgl(data[fct_vars], ~ nlevels(as.factor(.)) == 2L)]
      var_col <- as.character(fct_tab$row_var)
      fct_tab <- fct_tab[!(var_col %in% binary) | !duplicated(var_col), , drop = FALSE]
    }
    blocks <- c(blocks, list(fct_tab))
  }
  if (length(num_vars) != 0) {
    num_tab <- rlang::exec(tabxplor::tab, data, row_vars = "clust", col_vars = num_vars,
                           wt = wt, na = "drop", color = color,
                           !!!dots[setdiff(names(dots), "shape")]) |>
      tabxplor::tab_transpose(name = "levels") |>
      dplyr::mutate(row_var = as.character(.data$levels), .before = 1)
    blocks <- c(blocks, list(num_tab))
  }

  # The two summary rows are a fact about the CLUSTERS alone -- their share of the population and
  # their size -- so they come from their own one-variable table, transposed once. They are DISPLAY
  # rows (`row_kind`) with no comparison, and carry the table's own scale and colour measure: a
  # column claiming something else would reconcile BOTH away when bound.
  pop_tab <- tabxplor::tab(data, row_vars = "clust", wt = !!wt, pct = "all") |>
    tabxplor::tab_transpose(name = "levels")
  pop_tab   <- dplyr::filter(pop_tab, as.character(.data$levels) %in% c("pct", "n"))
  pop_kinds <- as.character(pop_tab$levels)          # "pct" / "n" -- already the row_kind vocabulary
  pop_tab <- pop_tab |>
    dplyr::mutate(
      row_var = "Total",
      .before = 1,
      dplyr::across(
        where(tabxplor::is_fmt),
        ~ dplyr::mutate(., diff = NA_real_, ratio = NA_real_) |>
          tabxplor::set_row_kind(pop_kinds) |>
          tabxplor::set_scale("level_pct") |>
          tabxplor::set_pct_type(pct) |>
          tabxplor::set_color(color)
      ),
      levels = forcats::fct_recode(factor(pop_kinds),
                                   !!!purrr::set_names(c("pct", "n"), c(row_tot, "n")))
    )
  tidy <- function(x) {
    x |>
      dplyr::mutate(dplyr::across(c("row_var", "levels"), ~ forcats::as_factor(as.character(.)))) |>
      dplyr::rename("variables" = "row_var", "lvs" = "levels", "Ensemble" = "Total")
  }
  # DESIGN: when every row is a mean (a PCA), the population goes UNDER the table: bound into it,
  #   its percentages would make each column `mixed`, which an additive ladder cannot grade.
  all_means <- length(fct_vars) == 0
  if (!all_means) blocks <- c(blocks, list(pop_tab))

  out <- purrr::reduce(blocks, dplyr::bind_rows) |>
    dplyr::filter(!str_detect(as.character(.data$levels), "Remove levels")) |>
    tidy() |>
    dplyr::group_by(.data$variables)

  # It joins the summary family by its return alone: one table, the format decided at print time.
  # No legend words: the colours grade a difference, which is exactly what tabxplor's own legend
  # says. `tooltips`: this IS a crosstab, so the count behind each percentage is worth hovering for.
  gda_summary(out, footer = if (all_means) tidy(pop_tab), tooltips = TRUE)
}


# The one reader of a `clust =` argument: a bare column name, a string, or the clusters themselves
# (one per row of `data`, then added as a column). Returns the column's name and the data holding it.
resolve_clust <- function(clust, data) {
  none <- list(name = character(), data = data)
  if (rlang::quo_is_missing(clust) || rlang::quo_is_null(clust)) return(none)

  expr <- rlang::quo_get_expr(clust)
  if (is.symbol(expr) && as.character(expr) %in% names(data)) {
    return(list(name = as.character(expr), data = data))
  }
  value <- tryCatch(rlang::eval_tidy(clust), error = function(e) NULL)
  if (length(value) == 0 && !is.null(value)) return(none)
  if (is.character(value) && length(value) == 1 && value %in% names(data)) {
    return(list(name = value, data = data))
  }
  if ((is.factor(value) || is.character(value)) && length(value) == nrow(data)) {
    name <- if ("clust" %in% names(data)) ".clust" else "clust"
    data[[name]] <- as.factor(value)
    return(list(name = name, data = data))
  }
  stop("`clust` must name the column of `data` that holds the clusters (e.g. `clust = cah`), ",
       "made with hierarchical_clust().", call. = FALSE)
}
