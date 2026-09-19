# PURPOSE: ggfacto() -- the one graph verb of the three analyses, as interpret() is their one table.
# ROLE: An S3 generic dispatching on the analysis to ggmca(), ggca() or ggpca(). Its signature holds
#   the arguments the analyses share, so an editor completes them; the others go through `...` to
#   the analysis's own function, whose page documents them.
# KEY CONSTRAINTS:
#   - Each method repeats the generic's formals in their order (R CMD check), with its own defaults.
#   - A missing argument travels as a missing promise and a tidyselect one through {{ }}, so the
#     analysis's function sees exactly what the user wrote. `...` reaches functions with fixed
#     formals: a misspelled argument errors.
#   - An argument an analysis does not take is refused in words, never dropped silently.
#   - `interactive = TRUE` returns ggi()'s widget; by default the graph is a ggplot, to which an
#     expert adds elements with `+` before calling ggi().
# See: CLAUDE.md section ggfacto architecture > How a graph is built.

#' The Graph of an Analysis
#'
#' @description
#' The graph of a multiple correspondence analysis, a correspondence analysis or a principal
#' component analysis, in the plane of two axes --- one verb for the three, as
#' \code{\link{interpret}} is their one table:
#'
#' \itemize{
#'   \item a \strong{multiple correspondence analysis} draws its active levels, and behind them the
#'   cloud of its individuals as answer profiles (see \code{\link{ggmca}});
#'   \item a \strong{correspondence analysis} draws the levels of its two variables, and of the
#'   supplementary variables its table holds (see \code{\link{ggca}});
#'   \item a \strong{principal component analysis} draws the cloud of its individuals, its variables
#'   being drawn by \code{\link{ggpca_cor_circle}} (see \code{\link{ggpca}}).
#' }
#'
#' Hovering a point shows the data behind it: a level's crosstabs, coloured by their deviations
#' from the mean, an individual's answers or values, a supplementary level's percentages or means.
#' Supplementary variables and clusters are added from the data frame, for an MCA or a PCA, and
#' from the table, for a CA.
#'
#' @param res An analysis made with \code{\link{multiple_correspondence_analysis}},
#' \code{\link{correspondence_analysis}} or \code{\link{principal_component_analysis}} (or with
#' \code{FactoMineR::MCA()}, \code{CA()} or \code{PCA()}, or \code{GDAtools::speMCA()} or
#' \code{csMCA()}).
#' @param data The data frame the analysis was made on, in which to find the supplementary
#' variables and the clusters, for an MCA or a PCA: the whole data frame, even when the analysis was
#' made on a subset of it. A CA reads its table instead.
#' @param sup_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> The supplementary variables, as
#' in `tab()`: `sup_vars = c(SEX, AGE)`. For a CA, they are the table's other variables:
#' `tab(data, c(relig, marital), c(partyid, race))`.
#' @param clust The clusters, made with \code{\link{hierarchical_clust}}: for an MCA or a PCA, the
#' column of `data` that holds them (`clust = cah`); for a CA, the clusters of the levels of one
#' margin (`clust = hierarchical_clust(res, ncp = 2, nb_clust = 4)`).
#' @param axes The axes to draw, as a numeric vector of length 2.
#' @param axes_reverse `1` to invert left and right, `2` to invert up and down, `1:2` for both.
#' @param type How the levels are drawn: \code{"text"}, \code{"labels"} or \code{"points"}, and
#' \code{"facets"} for one graph per level of the first supplementary variable (MCA, PCA). By
#' default \code{"text"}, and \code{"points"} for a CA.
#' @param profiles Should the cloud of the individuals be drawn? For an MCA, as answer profiles, by
#' default when `clust` is given; for a PCA, always by default.
#' @param active_tables The crosstabs in the tooltips of an MCA: see \code{\link{ggmca}}.
#' @param ellipses A number between 0 and 1 draws a concentration ellipse around the individuals of
#' each level of the first supplementary variable: \code{0.5} holds half of them.
#' @param title The title of the graph.
#' @param xlim,ylim Horizontal and vertical limits, as numeric vectors of length 2.
#' @param text_size Size of text.
#' @param size_scale_max The size of the largest point. By default, computed from the spread of the
#' weights of the points drawn.
#' @param lang \code{NULL} (the session's language), \code{"en"} or \code{"fr"}: the language of
#' the tooltips and of the axis titles.
#' @param interactive Set to \code{TRUE} to get the interactive graph at once, as
#' \code{\link{ggi}} makes it. By default, a \pkg{ggplot2} graph, to which elements can be added
#' with `+`, before passing it to \code{\link{ggi}}.
#' @param ... Further arguments of the analysis's own graph function, which document them:
#' \code{\link{ggmca}} (such as `tooltip_vars`, `max_profiles`, `color_groups`), \code{\link{ggca}}
#' (`show_sup`, `uppercase`, `tooltips`) or \code{\link{ggpca}}.
#'
#' @details An argument that an analysis does not take stops with an explanation: `data`,
#' `sup_vars`, `profiles`, `active_tables` and `ellipses` for a CA, `active_tables` for a PCA.
#'
#' @return A \code{\link[ggplot2:ggplot]{ggplot}} object, or an html widget with
#' `interactive = TRUE`.
#' @export
#'
#' @examples
#' \donttest{
#' data(tea, package = "FactoMineR")
#' res.mca <- multiple_correspondence_analysis(tea, 1:18)
#' tea <- tea |>
#'   dplyr::mutate(clust = hierarchical_clust(res.mca, ncp = 3, nb_clust = 5))
#' ggfacto(res.mca, tea, sup_vars = SPC, clust = clust, interactive = TRUE)
#'
#' gss <- forcats::gss_cat |>
#'   dplyr::filter(!relig %in% c("No answer", "Don't know", "Not applicable"),
#'                 !partyid %in% c("No answer", "Don't know"))
#' res.ca <- correspondence_analysis(tabxplor::tab(gss, c(relig, marital), partyid))
#' ggfacto(res.ca, interactive = TRUE)
#'
#' cars <- mtcars
#' cars$cyl <- factor(cars$cyl)
#' res.pca <- principal_component_analysis(cars, c(mpg, disp, hp, drat, wt, qsec))
#' ggfacto(res.pca, cars, sup_vars = cyl, ellipses = 0.5)
#' }
ggfacto <- function(res, data, sup_vars, clust, axes = c(1, 2), axes_reverse = NULL, type,
                    profiles, active_tables, ellipses = NULL, title, xlim, ylim,
                    text_size = 3.5, size_scale_max = NULL, lang = NULL, interactive = FALSE,
                    ...) {
  UseMethod("ggfacto")
}

#' @export
#' @noRd
ggfacto.MCA <- function(res, data, sup_vars, clust, axes = c(1, 2), axes_reverse = NULL,
                        type = "text", profiles = NULL, active_tables = "active", ellipses = NULL,
                        title, xlim, ylim, text_size = 3.5, size_scale_max = NULL, lang = NULL,
                        interactive = FALSE, ...) {
  graph_out(ggmca(res, data, sup_vars = {{ sup_vars }}, clust = {{ clust }},
                  active_tables = active_tables, axes = axes, axes_reverse = axes_reverse,
                  type = type, profiles = profiles, ellipses = ellipses, title = title,
                  xlim = xlim, ylim = ylim, text_size = text_size,
                  size_scale_max = size_scale_max, lang = lang, ...),
            interactive, ...names())
}

#' @export
#' @noRd
ggfacto.speMCA <- ggfacto.MCA

#' @export
#' @noRd
ggfacto.CA <- function(res, data, sup_vars, clust, axes = c(1, 2), axes_reverse = NULL,
                       type = "points", profiles, active_tables, ellipses = NULL,
                       title, xlim, ylim, text_size = 3.5, size_scale_max = NULL, lang = NULL,
                       interactive = FALSE, ...) {
  refuse <- function(given, why) if (given) stop(why, call. = FALSE)
  refuse(!missing(data), str_c(
    "A correspondence analysis reads its table, not the data frame: ggfacto(res) is enough."))
  refuse(!missing(sup_vars), str_c(
    "The supplementary variables of a correspondence analysis are in its table: ",
    "tab(data, c(relig, marital), c(partyid, race)) |> correspondence_analysis()."))
  refuse(!missing(profiles), "A correspondence analysis has no individuals to draw (`profiles`).")
  refuse(!missing(active_tables), str_c(
    "`active_tables` is for a multiple correspondence analysis: a level of a correspondence ",
    "analysis shows its profile, set with `tooltips =`."))
  refuse(!is.null(ellipses), "A correspondence analysis has no individuals to draw ellipses of.")
  graph_out(ggca(res, axes = axes, axes_reverse = axes_reverse, type = type, clust = {{ clust }},
                 title = title, xlim = xlim, ylim = ylim, text_size = text_size,
                 size_scale_max = size_scale_max, lang = lang, ...),
            interactive, ...names())
}

#' @export
#' @noRd
ggfacto.PCA <- function(res, data, sup_vars, clust, axes = c(1, 2), axes_reverse = NULL,
                        type = "text", profiles = TRUE, active_tables, ellipses = NULL,
                        title, xlim, ylim, text_size = 3.5, size_scale_max = NULL, lang = NULL,
                        interactive = FALSE, ...) {
  if (!missing(active_tables)) stop(
    "`active_tables` is for a multiple correspondence analysis: a supplementary level of a ",
    "principal component analysis shows the means of the active variables.", call. = FALSE)
  graph_out(ggpca(res, data, sup_vars = {{ sup_vars }}, clust = {{ clust }}, axes = axes,
                  axes_reverse = axes_reverse, type = type, profiles = profiles,
                  ellipses = ellipses, title = title, xlim = xlim, ylim = ylim,
                  text_size = text_size, size_scale_max = size_scale_max, lang = lang, ...),
            interactive, ...names())
}

#' @export
#' @noRd
ggfacto.default <- function(res, data, sup_vars, clust, axes = c(1, 2), axes_reverse = NULL,
                            type, profiles, active_tables, ellipses = NULL, title, xlim, ylim,
                            text_size = 3.5, size_scale_max = NULL, lang = NULL,
                            interactive = FALSE, ...) {
  stop("ggfacto() draws a multiple correspondence, correspondence or principal component ",
       "analysis, made with multiple_correspondence_analysis(), correspondence_analysis() or ",
       "principal_component_analysis().", call. = FALSE)
}

# The graph as asked: the ggplot, or its widget.
#' @keywords internal
#' @noRd
graph_out <- function(p, interactive, dots) {
  if (!interactive) return(p)
  if ("get_data" %in% dots) stop(
    "`get_data = TRUE` returns the data frames of the graph, which cannot be interactive.",
    call. = FALSE)
  ggi(p)
}
