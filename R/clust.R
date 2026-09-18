# PURPOSE: Hierarchical clustering on the axes of an analysis, and the table that describes its
#   clusters -- hierarchical_clust(), clust_tab().
# ROLE: The last step of the workflow. hierarchical_clust() clusters the individuals of an MCA or a
#   PCA, or the levels of one margin of a CA, and returns the clusters as a factor, to be written
#   into the data frame with mutate(); clust_tab() and ggmca(clust =) read that column back.
#   resolve_clust() is the one reader of a `clust =` argument, shared with R/mca-data.R.
# KEY CONSTRAINTS:
#   - The clusters are FactoMineR::HCPC()'s, computed here for their memory: HCPC() builds its tree
#     from n x n matrices. Ward's tree is built on the DISTINCT points of the cloud by fastcluster,
#     in memory linear in their number; the point order, the cut and the k-means consolidation are
#     HCPC()'s, and test-clust.R pins the equality.
#   - The clusters are a column of the data frame, never an HCPC object: renaming them is then an
#     ordinary fct_recode(), and every function that describes them reads the same column.
#   - hierarchical_clust() aligns its result by context -- on the data frame of the mutate() it is
#     called in, through R/ingress.R, else on the data frame the analysis started from -- so the
#     rows outside an analysed subset get NA, and a reordered data frame is refused. A CA's levels
#     are matched by name: each individual gets the cluster of its level.
#   - clust_tab() is tabxplor's: a number enters as a MEAN column and is transposed into a mean row,
#     a factor needs no transposition at all. It hands back a summary of the family described in
#     ?ggfacto_summary.
# See: CLAUDE.md section ggfacto architecture > The FactoMineR contract.

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
#' The clusters can then be renamed with \code{forcats::fct_recode()}, described with
#' \code{\link{clust_tab}} and drawn with \code{\link{ggmca}(clust = )}.
#'
#' Individuals giving the same answers share one point, and the tree is built on these distinct
#' points: the memory it needs grows with their number, not with its square.
#'
#' @param res An analysis made with \code{\link{multiple_correspondence_analysis}},
#' \code{\link{principal_component_analysis}} or \code{\link{correspondence_analysis}} (or with
#' \code{FactoMineR::MCA()}, \code{FactoMineR::PCA()} or \code{FactoMineR::CA()}).
#' @param ncp The number of axes to cluster on: the first ones, those worth interpreting (see the
#' eigenvalues under \code{\link{mca_interpret}}). There is no default: on every axis, the
#' clustering would follow noise.
#' @param nb_clust The number of clusters. With `-1`, the default, the tree is cut where the gain in
#' between-cluster inertia drops the most.
#' @param tree Should the clustering tree be drawn, to choose the number of clusters? By default,
#' only when `nb_clust = -1`. Its leaves are the distinct points of the cloud: the answer profiles
#' of a multiple correspondence analysis, the levels of a correspondence analysis.
#' @param consol Set to `FALSE` to keep the clusters as the tree cuts them, without the k-means
#' consolidation that then moves each individual to its nearest cluster.
#' @param margin For a correspondence analysis, the levels to cluster: `"rows"`, the default, or
#' `"columns"`.
#'
#' @return A factor with the clusters, `"1"`, `"2"`, etc., numbered along the first axis and
#' returned invisibly: a bare call only draws the tree. Inside \code{dplyr::mutate()}, it has one
#' value per row of that data frame, and `NA` on the rows the analysis did not use (when it was made
#' on a subset of the population). Outside \code{mutate()}, it has one value per row of the data
#' frame the analysis started from. For a correspondence analysis, each row of the data frame gets
#' the cluster of its level (`NA` for a level outside the table); outside \code{mutate()}, there is
#' one value per level, named after it.
#' @export
#'
#' @examples
#' data(tea, package = "FactoMineR")
#' res.mca <- multiple_correspondence_analysis(tea, 1:18)
#'
#' tea <- tea |>
#'   dplyr::mutate(clust = hierarchical_clust(res.mca, ncp = 3, nb_clust = 6))
#'
#' clust_tab(tea, tidyselect::all_of(names(tea)[1:18]), clust)
#' ggmca(res.mca, tea, clust = clust, profiles = TRUE)
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
                               margin = "rows") {
  if (!inherits(res, c("MCA", "PCA", "CA"))) stop(
    "hierarchical_clust() clusters the individuals of a principal component analysis or of a ",
    "multiple correspondence analysis, or the levels of a correspondence analysis.", call. = FALSE)
  if (missing(ncp)) stop(
    "`ncp` is required: the number of axes to cluster on, those worth interpreting (read the ",
    "eigenvalues under mca_interpret(), pca_interpret() or ca_interpret()). On every axis, the ",
    "clustering would follow noise.", call. = FALSE)
  if (!(nb_clust == -1 || nb_clust >= 2)) stop(
    "`nb_clust` is the number of clusters, at least 2, or -1 to cut the tree where the gain in ",
    "between-cluster inertia drops the most.", call. = FALSE)

  pts <- clust_points(res, ncp, margin)
  hc  <- ward_clusters(pts$coord, pts$w, pts$answers, nb_clust, consol)
  if (tree) plot_clust_tree(hc, labels = inherits(res, "CA"))
  clust <- hc$clust

  expand <- function(n, idx) {
    out <- factor(rep(NA_character_, n), levels = levels(clust))
    out[idx] <- clust
    out
  }

  # Inside mutate(): the data frame being written into is at hand, so the rows are aligned -- and
  # verified -- on it. Outside, on the data frame the analysis started from.
  target <- tryCatch(dplyr::pick(tidyselect::any_of(pts$vars)), error = function(e) NULL)
  if (!is.null(target) && ncol(dplyr::cur_group()) != 0) stop(
    "hierarchical_clust() must be used in an ungrouped mutate(): call dplyr::ungroup() first.",
    call. = FALSE)

  # invisible: a bare call, made to look at the tree, must not print one cluster per individual
  if (inherits(res, "CA")) {
    names(clust) <- rownames(pts$coord)
    return(invisible(if (is.null(target)) clust else level_clusters(clust, target, pts$vars)))
  }
  if (!is.null(target)) return(invisible(expand(nrow(target), fit_rows(res, target))))
  invisible(if (is.null(res$source$rows)) clust else expand(res$source$n, res$source$rows))
}

# The points HCPC() clusters, their weights, and the variables that locate them in a data frame.
clust_points <- function(res, ncp, margin) {
  if (inherits(res, "CA")) {
    margin  <- match.arg(margin, c("rows", "columns"))
    rows    <- margin == "rows"
    coord   <- if (rows) res$row$coord else res$col$coord
    w       <- (if (rows) res$call$marge.row else res$call$marge.col) * sum(res$call$X)
    vars    <- names(dimnames(res$call$X))[if (rows) 1L else 2L]
    vars    <- vars[!is.na(vars) & nzchar(vars)]     # FactoMineR::CA() keeps no variable names
    answers <- NULL
  } else {
    if (length(res$call$ind.sup) != 0) stop(
      "hierarchical_clust() does not handle supplementary individuals.", call. = FALSE)
    coord   <- res$ind$coord
    w       <- if (is.null(res$call$row.w.init)) res$call$row.w else res$call$row.w.init
    vars    <- active_names(res)
    answers <- res$call$X[vars]
  }
  if (ncp > ncol(coord)) stop(
    "`ncp` is ", ncp, ", and the analysis keeps ", ncol(coord), " axes.", call. = FALSE)
  list(coord = coord[, seq_len(ncp), drop = FALSE], w = w, answers = answers, vars = vars)
}

# The clusters of FactoMineR::HCPC() -- its point order, its cut rule and its consolidation, by its
# authors (F. Husson, J. Josse, J. Pages) -- with Ward's tree built on the distinct points.
ward_clusters <- function(coord, w, answers, nb_clust, consol) {
  n <- nrow(coord)
  # DESIGN: the points in HCPC()'s order, sorted along the first axis: it decides which pair merges
  #   on a tie and where the k-means starts, so the clusters are HCPC()'s own and not a variant.
  ord <- order(coord[, 1])
  X   <- coord[ord, , drop = FALSE]
  # DESIGN: the leaves are the DISTINCT points, weighted by their individuals: Ward merges identical
  #   points first, at no cost, so the tree above them is the same (ids keep HCPC()'s order).
  leaf  <- if (is.null(answers)) seq_len(n) else vctrs::vec_group_id(answers[ord, , drop = FALSE])
  first <- which(!duplicated(leaf))
  if (nb_clust >= length(first)) stop(
    "`nb_clust` must be less than the number of distinct points to cluster, ", length(first), ".",
    call. = FALSE)

  tree <- fastcluster::hclust.vector(X[first, , drop = FALSE], method = "ward",
                                     members = as.vector(rowsum(w[ord], leaf)))
  # WARNING: fastcluster's Ward height is sqrt(2 x the inertia gain); HCPC() plots and cuts the
  #   gain, as a share of the total weight.
  tree$height <- tree$height^2 / 2 / sum(w)
  if (nb_clust == -1) nb_clust <- auto_nb_clust(rev(tree$height), n, ncol(X))

  h   <- rev(tree$height)
  cut <- (h[nb_clust - 1] + h[nb_clust]) / 2
  cl  <- stats::cutree(tree, h = cut)[leaf]
  centers <- rowsum(X, cl) / tabulate(cl)
  # DESIGN: HCPC()'s consolidation is an UNWEIGHTED k-means on the individuals, kept as is so the
  #   clusters stay HCPC()'s, survey weights or not.
  if (consol) {
    km      <- stats::kmeans(X, centers = centers, iter.max = 10)
    cl      <- km$cluster
    centers <- km$centers
  }
  cl  <- order(order(centers[, 1]))[cl]
  out <- integer(n)
  out[ord] <- cl
  list(clust = factor(out), tree = tree, cut = cut, nb_clust = max(cl), leaf_clust = cl[first])
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

# Why this exists: HCPC()'s tree plot -- the inertia gains top right, one rectangle per cluster --
# drawn from the tree ward_clusters() builds. A CA's leaves are its levels, and carry their names.
plot_clust_tree <- function(hc, labels) {
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op))
  graphics::par(mar = c(0.5, 2, 0.75, 0))
  graphics::layout(matrix(c(rep(c(2, 4, 4, 4, 4), 4), 1, 3, 3, 3, 3), 5), respect = TRUE)
  gain <- rev(hc$tree$height)
  gain <- gain[seq_len(min(15, length(gain)))]
  graphics::barplot(gain, col = ifelse(seq_along(gain) < hc$nb_clust, "black", "grey"),
                    space = 0.9)
  graphics::plot.new()
  graphics::text(0.5, 0.5, "Hierarchical clustering", cex = 2)
  graphics::plot.new()
  graphics::legend("top", "inertia gain  ")
  graphics::plot(hc$tree, labels = if (labels) NULL else FALSE, hang = -1,
                 main = "", xlab = "", ylab = "", sub = "")
  stats::rect.hclust(hc$tree, h = hc$cut, border = unique(hc$leaf_clust[hc$tree$order]))
}

# Why this exists: a CA clusters LEVELS; in mutate(), each individual gets the cluster of its level.
level_clusters <- function(clust, target, var) {
  if (length(var) == 0 || !var %in% names(target)) stop(
    "In mutate(), hierarchical_clust() gives each individual the cluster of its level, which needs ",
    if (length(var) == 0) {
      "the name of the variable: make the analysis with correspondence_analysis(), which keeps it."
    } else {
      str_c("the variable `", var, "` in the data frame.")
    },
    call. = FALSE)
  lv  <- as.character(target[[var]])
  lv[is.na(lv)] <- "NA"                            # tab() names the missing answers' level "NA"
  idx <- match(lv, names(clust))
  if (all(is.na(idx))) stop(
    "No level of `", var, "` in the data frame is a level of the correspondence analysis. Was its ",
    "table made with `cleannames = TRUE`?", call. = FALSE)
  unname(clust[idx])
}


#' Describe Clusters with One Table
#'
#' @description
#' One table describing every cluster: each variable's levels down the page, the clusters across it,
#' and a colour saying at a glance which levels a cluster is made of. Numeric variables come in as
#' mean rows, and the last two rows give each cluster's share of the population and its size.
#' `HCPC_tab()` is an older name for the same function.
#'
#' @param data A data frame.
#' @param row_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> The variables to describe the
#' clusters with, typically the active variables of the MCA. Numeric ones become mean rows.
#' @param clust The variable with the clusters, typically made with \code{\link{hierarchical_clust}},
#' as a bare name or a string. Rows without a cluster (outside the analysed population) are left
#' out.
#' @param wt The weight variable. Leave empty for unweighted results.
#' @param excl The levels not to show, matched exactly by name; their individuals still count in the
#' percentages. `NA`, the default, hides the missing values (and the levels named `<VAR>.NA`);
#' `excl = NULL` shows every level.
#' @param color The colour measure, see \code{\link[tabxplor]{tab}}. With `"difference"` (the
#' default) the mean rows stay uncoloured: a difference of means and a difference of percentages
#' have no ladder in common. Use `"ratio"` to colour every row, means included.
#' @param pct `"col"` (default) reads each cluster as a distribution: of the people in this cluster,
#' what percentage are in this level. `"row"` reads each level as a distribution across clusters.
#' @param row_tot The name of the row giving each cluster's share of the population.
#' @param cleannames Set to \code{FALSE} to keep the level and cluster names as they are, prefix
#' numbers like \code{"1-"} and text in parentheses included.
#' @param ... Additional arguments to pass to \code{\link[tabxplor]{tab}}.
#'
#' @return A \code{tabxplor} table --- see [ggfacto_summary] for how it prints.
#' @export
#' @seealso [ggfacto_summary], [hierarchical_clust()], [mca_interpret()].
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
#' clust_tab(tea, tidyselect::all_of(names(tea)[1:18]), clust)
clust_tab <- function(data, row_vars = character(), clust, wt, excl = NA,
                      color = "difference", pct = "col", row_tot = "% of population",
                      cleannames = TRUE, ...) {
  row_vars <- names(tidyselect::eval_select(rlang::enquo(row_vars), data))
  wt       <- if (missing(wt)) character() else as.character(rlang::ensym(wt))

  clust <- resolve_clust(rlang::enquo(clust), data)
  if (length(clust$name) == 0) stop("`clust` is required: the variable with the clusters.",
                                    call. = FALSE)
  data <- clust$data |>
    dplyr::select(tidyselect::all_of(row_vars), tidyselect::all_of(wt),
                  clust = tidyselect::all_of(clust$name)) |>
    dplyr::filter(!is.na(.data$clust))
  # WARNING: the cluster names are cleaned HERE, once, for every block: the level rows and the
  #   population rows are two tab() calls, and names cleaned in one only would bind as two sets of
  #   columns ("1-Petit ecran" beside "Petit ecran").
  if (cleannames) data$clust <- forcats::fct_relabel(as.factor(data$clust),
                                                     ~ str_remove_all(., cleannames_condition()))

  # Excluded levels are HIDDEN, not dropped: their individuals still count in each cluster's
  # percentages, which is why a column may not sum to 100.
  fct_names <- row_vars[!purrr::map_lgl(data[row_vars], is.numeric)]
  data <- na_levels(data, fct_names)
  ex   <- excl_levels(data, fct_names, excl)
  data <- data |>
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(names(ex)),
      ~ forcats::fct_other(., drop = ex[[dplyr::cur_column()]], other_level = "Remove levels")
    ))

  if (length(wt) == 0) {
    wt <- rlang::expr(NA)
  } else {
    wt <- rlang::sym(wt)
  }

  # A NUMBER cannot be a row of levels: it enters as a MEAN column and is transposed into a mean row.
  # A FACTOR is built the way round the table is read -- levels down, clusters across -- so it needs
  # no transposition at all: tabxplor stacks several row variables into one table by itself.
  is_num   <- purrr::map_lgl(dplyr::select(data, tidyselect::all_of(row_vars)), is.numeric)
  fct_vars <- row_vars[!is_num]
  num_vars <- row_vars[is_num]

  blocks <- list()

  if (length(fct_vars) != 0) {
    fct_tab <- tabxplor::tab(data,
                             row_vars   = tidyselect::all_of(fct_vars),
                             col_vars   = "clust",
                             wt         = !!wt,
                             pct        = pct,
                             tot        = "col",
                             na         = "drop",
                             cleannames = cleannames,
                             color      = color,
                             ...) |>
      dplyr::ungroup()

    # A BINARY variable says everything in one row: the second level is the first one upside down.
    binary  <- names(which(purrr::map_lgl(
      dplyr::select(data, tidyselect::all_of(fct_vars)),
      ~ nlevels(as.factor(.)) == 2L
    )))
    var_col <- as.character(fct_tab$row_var)
    fct_tab <- fct_tab[!(var_col %in% binary) | !duplicated(var_col), , drop = FALSE]

    blocks <- c(blocks, list(fct_tab))
  }

  if (length(num_vars) != 0) {
    num_tab <- tabxplor::tab(data,
                             row_vars = "clust",
                             col_vars = tidyselect::all_of(num_vars),
                             wt       = !!wt,
                             na       = "drop",
                             color    = color) |>
      tabxplor::tab_transpose(name = "levels") |>
      dplyr::mutate(row_var = as.character(.data$levels), .before = 1)

    blocks <- c(blocks, list(num_tab))
  }

  # The two summary rows are a fact about the CLUSTERS alone -- their share of the population and
  # their size -- so they come from their own one-variable table, transposed once.
  # They are DISPLAY rows, not data: `row_kind` says so, and they carry no comparison (a display row
  # has nothing to be a deviation FROM), so the colour engine leaves them alone. They also carry the
  # table's own scale and colour measure, because binding a column that claims something else would
  # reconcile BOTH away -- the level rows would come out `mixed` and uncoloured.
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
  blocks <- c(blocks, list(pop_tab))

  out <- purrr::reduce(blocks, dplyr::bind_rows) |>
    dplyr::filter(!str_detect(as.character(.data$levels), "Remove levels")) |>
    dplyr::mutate(dplyr::across(c("row_var", "levels"), ~ forcats::as_factor(as.character(.)))) |>
    dplyr::rename("variables" = "row_var", "lvs" = "levels", "Ensemble" = "Total") |>
    dplyr::group_by(.data$variables)

  # It joins the summary family by its return alone: one table, the format decided at print time.
  # No eigenvalues -- a cluster description says nothing about axes -- and no legend words: the
  # colours grade a difference of percentages, which is exactly what tabxplor's own legend says.
  # `tooltips`: this IS a crosstab, so the count behind each percentage is worth hovering for.
  gda_summary(out, tooltips = TRUE)
}

#' @rdname clust_tab
#' @export
HCPC_tab <- clust_tab


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
