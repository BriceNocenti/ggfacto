# PURPOSE: Hierarchical clustering on the axes of an analysis, and the table that describes its
#   clusters -- hierarchical_clust(), clust_tab().
# ROLE: The last step of the MCA/PCA workflow. hierarchical_clust() returns the clusters as a factor,
#   to be written into the data frame with mutate(); clust_tab() and ggmca(clust =) read that column
#   back. resolve_clust() is the one reader of a `clust =` argument, shared with R/mca-data.R.
# KEY CONSTRAINTS:
#   - The clusters are a column of the data frame, never an HCPC object: renaming them is then an
#     ordinary fct_recode(), and every function that describes them reads the same column.
#   - hierarchical_clust() aligns its result by context -- on the data frame of the mutate() it is
#     called in, through R/ingress.R, else on the data frame the analysis started from -- so the
#     rows outside an analysed subset get NA, and a reordered data frame is refused.
#   - clust_tab() is tabxplor's: a number enters as a MEAN column and is transposed into a mean row,
#     a factor needs no transposition at all. It hands back a summary of the family described in
#     ?ggfacto_summary.
# See: CLAUDE.md section ggfacto architecture > The FactoMineR contract.

#' Hierarchical Clustering on the Axes of an Analysis
#'
#' @description
#' Clusters the individuals of a multiple correspondence analysis or of a principal component
#' analysis on its first `ncp` axes, with \code{FactoMineR::\link[FactoMineR]{HCPC}}, and returns
#' the clusters, one per row of the data. Use it inside \code{dplyr::mutate()} to add them to the
#' data frame:
#'
#' `data <- data |> mutate(clust = hierarchical_clust(res, ncp = 3, nb.clust = 6))`
#'
#' To choose the number of clusters, look at the tree first: `hierarchical_clust(res, ncp = 3)`.
#' The clusters can then be renamed with \code{forcats::fct_recode()}, described with
#' \code{\link{clust_tab}} and drawn with \code{\link{ggmca}(clust = )}.
#'
#' @param res An analysis made with \code{\link{multiple_correspondence_analysis}} or
#' \code{\link{principal_component_analysis}} (or with \code{FactoMineR::MCA()} or
#' \code{FactoMineR::PCA()}), keeping all its axes.
#' @param ncp The number of axes to cluster on: the first ones, those worth interpreting (see the
#' eigenvalues under \code{\link{mca_interpret}}). There is no default: on every axis, the
#' clustering would follow noise.
#' @param nb.clust The number of clusters. With `-1`, the default, the tree is cut where the gain in
#' between-cluster inertia drops the most.
#' @param tree Should the clustering tree be drawn, to choose the number of clusters? By default,
#' only when `nb.clust = -1`.
#' @param ... Additional arguments to pass to \code{FactoMineR::\link[FactoMineR]{HCPC}}: with many
#' individuals, `kk = 100` clusters them first into 100 groups, which is much faster.
#'
#' @return A factor with the clusters, `"1"`, `"2"`, etc., returned invisibly: a bare call only
#' draws the tree. Inside \code{dplyr::mutate()}, it has one
#' value per row of that data frame, and `NA` on the rows the analysis did not use (when it was made
#' on a subset of the population). Outside \code{mutate()}, it has one value per row of the data
#' frame the analysis started from.
#' @export
#'
#' @examples
#' data(tea, package = "FactoMineR")
#' res.mca <- multiple_correspondence_analysis(tea, 1:18)
#'
#' tea <- tea |>
#'   dplyr::mutate(clust = hierarchical_clust(res.mca, ncp = 3, nb.clust = 6))
#'
#' clust_tab(tea, tidyselect::all_of(names(tea)[1:18]), clust)
#' ggmca(res.mca, tea, clust = clust, profiles = TRUE)
#'
#' # On a subset of the population: the other rows get NA
#' res.mca_young <- tea |>
#'   dplyr::filter(age < 30) |>
#'   multiple_correspondence_analysis(1:18)
#' tea <- tea |>
#'   dplyr::mutate(clust_young = hierarchical_clust(res.mca_young, ncp = 3, nb.clust = 4))
hierarchical_clust <- function(res, ncp, nb.clust = -1, tree = nb.clust == -1, ...) {
  if (!inherits(res, c("MCA", "PCA"))) stop(
    "hierarchical_clust() clusters the individuals of a multiple correspondence analysis or of a ",
    "principal component analysis.", call. = FALSE)
  if (missing(ncp)) stop(
    "`ncp` is required: the number of axes to cluster on, those worth interpreting (read the ",
    "eigenvalues under mca_interpret() or pca_interpret()). On every axis, the clustering would ",
    "follow noise.", call. = FALSE)
  if (inherits(res, "PCA") && length(res$call$ind.sup) != 0) stop(
    "hierarchical_clust() does not handle supplementary individuals.", call. = FALSE)

  # DESIGN: the analysis is refitted on its first `ncp` axes from its own call -- the very data,
  #   weights and exclusions -- rather than having its slots truncated: HCPC() reads several of them,
  #   and a refit is what the course did by hand, at the cost of one more analysis.
  fit <- if (inherits(res, "MCA")) {
    FactoMineR::MCA(res$call$X[res$call$quali], ncp = ncp, row.w = res$call$row.w,
                    excl = res$call$excl, graph = FALSE)
  } else {
    FactoMineR::PCA(res$call$X[rownames(res$var$coord)], ncp = ncp,
                    scale.unit = res$call$scale.unit, row.w = res$call$row.w.init,
                    col.w = res$call$col.w, graph = FALSE)
  }
  # description = FALSE: HCPC() would describe each cluster with chi2 tests the result never shows,
  # and their "approximation may be incorrect" warnings would land in the student's mutate().
  hc <- FactoMineR::HCPC(fit, nb.clust = nb.clust, graph = FALSE, description = FALSE, ...)
  if (tree) plot(hc, choice = "tree")
  clust <- hc$data.clust$clust

  expand <- function(n, idx) {
    out <- factor(rep(NA_character_, n), levels = levels(clust))
    out[idx] <- clust
    out
  }

  # Inside mutate(): the data frame being written into is at hand, so the rows are aligned -- and
  # verified -- on it. Outside, on the data frame the analysis started from.
  target <- tryCatch(dplyr::pick(tidyselect::any_of(active_names(res))),
                     error = function(e) NULL)
  if (!is.null(target)) {
    if (ncol(dplyr::cur_group()) != 0) stop(
      "hierarchical_clust() must be used in an ungrouped mutate(): call dplyr::ungroup() first.",
      call. = FALSE)
    return(invisible(expand(nrow(target), fit_rows(res, target))))
  }
  # invisible: a bare call, made to look at the tree, must not print one cluster per individual
  invisible(if (is.null(res$source$rows)) clust else expand(res$source$n, res$source$rows))
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
#'   dplyr::mutate(clust = hierarchical_clust(res.mca, ncp = 3, nb.clust = 6))
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
