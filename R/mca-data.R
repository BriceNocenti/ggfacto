# PURPOSE: The MCA entry point and the MCA's graph -- multiple_correspondence_analysis() (alias
#   MCA2()), ggmca(), ggmca_data(), and mca_plot_data(), the MCA's builder of the shared plot model.
# ROLE: Fits the MCA on the answer profiles, then turns the analysis plus its microdata into the
#   plot model (R/plot-model.R) the one renderer draws (R/plot-render.R). ggmca() is pure
#   orchestration of the two halves.
# KEY CONSTRAINTS:
#   - Everything is read through the analysis's model (R/model.R), never its slots: the levels by
#     position, the weights from `source$w`, so a tooltip always describes the population the
#     analysis was fitted on. `data` comes back through R/ingress.R's align_to_fit(), cut down to
#     the fitted rows, and only when a supplementary variable, a cluster or a tooltip variable needs
#     it.
#   - ggmca()'s signature is exactly ggmca_data()'s plus ggmca_plot()'s, with zero overlap. A new
#     argument belongs to one half and must be routed to that half only. That is why the model is
#     `plot_data` and the microdata is `data`: one name each, no collision. `lang` is the data
#     half's: the model carries it, so an edited model renders in the language it was built in.
#   - Every number of a supplementary level, a tooltip or a profile is an aggregation over the
#     units (profile x non-active answers), never a loop over profiles or individuals. Only
#     `individuals` keeps one row per individual, for the ellipses.
#   - Several arguments are inert on their own and only act alongside an enabling one --
#     keep_levels/discard_levels need sup_vars, tooltip_vars/tooltip_vars_1lv need a table to be
#     built at all. tests/testthat/test-ggmca-data.R pins both halves.
# See: CLAUDE.md section ggfacto architecture > The plot model.

#' Multiple Correspondence Analysis
#' @description A user-friendly wrapper around \code{\link[FactoMineR]{MCA}}, made to
#'  work with \pkg{ggfacto} functions like \code{\link{ggmca}}, \code{\link{interpret}} and
#'  \code{\link{hierarchical_clust}}. Variables are selected the way of the `tidyverse`, as in
#'  \code{tabxplor::tab()}. Supplementary variables are not given here: they are added afterwards,
#'  in \code{\link{ggmca}}. `MCA2()` is a shorter name for the same function.
#'
#' @param data The data frame. To analyse a subset of the population, filter it inside the call with
#'  the native pipe, `data |> dplyr::filter(...) |> multiple_correspondence_analysis(...)`: the
#'  analysis then remembers which rows it used, so that \code{\link{hierarchical_clust}} and
#'  \code{\link{ggmca}} can be given the whole data frame afterwards.
#' @param active_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> The active variables.
#' @param wt <\link[tidyr:tidyr_tidy_select]{tidy-select}> The weight variable, if any.
#' @param excl The levels to exclude from the calculation of the axes (specific multiple
#'  correspondence analysis), matched exactly by name. The missing values of each active variable
#'  become a level named `<VAR>.NA`, and `NA`, the default, excludes all of them: `excl = NA` for
#'  missing values only, `excl = c(NA, "Other")` to exclude a level too, `excl = "DIPLOMA.NA"` for
#'  the missing values of one variable only, `excl = NULL` to keep every level.
#' @param ncp The number of axes to keep. All of them by default: the eigenvalue table is how one
#'   chooses how many axes to interpret, and a truncated one cannot show the drop --- it also
#'   renormalises Benzecri's modified rate over the axes it kept, so the same axis gets a different
#'   rate. To cluster on the first axes, give \code{\link{hierarchical_clust}} its own `ncp`.
#' @param graph By default no graph is made, since the result can be plotted with
#'  \code{\link{ggmca}}.
#' @param ... Additional arguments to pass to \code{\link[FactoMineR]{MCA}}, except those that
#'  index its rows or columns (`ind.sup`, `quali.sup`, `quanti.sup`, `tab.disj`).
#'
#' @return A `MCA` object from \pkg{FactoMineR}, fitted on the distinct answer profiles (the
#'  combinations of active answers), each weighted by its individuals: the eigenvalues and every
#'  result on the levels are the individuals', and `$ind` has one row per profile. Use
#'  \code{\link{axis_coord}} and \code{\link{hierarchical_clust}} to write coordinates and clusters
#'  into the data frame (`FactoMineR::HCPC()` would cluster the profiles). One more element,
#'  `source`, records the rows of `data` that were analysed, their weights and their profiles.
#' @export
#'
#' @examples
#' data(tea, package = "FactoMineR")
#' res.mca <- multiple_correspondence_analysis(tea, 1:18)
#' interpret(res.mca)
#'
#' ggmca(res.mca, tea, sup_vars = SPC, ylim = c(NA, 1.2)) |>
#'   ggi() # to make the graph interactive
#'
#' # A subset of the population: the analysis remembers which rows it used
#' res.mca_young <- tea |>
#'   dplyr::filter(age < 30) |>
#'   multiple_correspondence_analysis(1:18)
multiple_correspondence_analysis <- function(data, active_vars, wt, excl = NA, ncp = Inf,
                                             graph = FALSE, ...) {
  # WARNING: the caller's frame is captured HERE, before any promise is forced: rlang::caller_env()
  #   evaluated lazily inside source_rows() would name the wrong frame.
  expr   <- rlang::enexpr(data)
  env    <- rlang::caller_env()
  wt     <- tidyselect::eval_select(rlang::enquo(wt), data)
  stopifnot(length(wt) < 2)
  source <- source_rows(expr, env, data, wt = if (length(wt) != 0) names(wt))

  active_vars <- names(tidyselect::eval_select(rlang::enquo(active_vars), data))
  if (length(active_vars) < 2) stop(
    "An MCA needs at least two active variables.", call. = FALSE)
  indexed <- intersect(...names(), c("ind.sup", "quali.sup", "quanti.sup", "tab.disj", "row.w"))
  if (length(indexed) != 0) stop(
    "multiple_correspondence_analysis() fits the answer profiles, so `",
    str_c(indexed, collapse = "`, `"), "` cannot be passed to FactoMineR::MCA(). Supplementary ",
    "variables are drawn by ggmca(sup_vars = ).", call. = FALSE)
  wt <- if (length(wt) != 0) data[[wt]] else NULL
  w  <- usable_weights(data, wt, source)

  X <- na_levels(as.data.frame(w$data[active_vars]), active_vars)
  # DESIGN: FactoMineR is fed the DISTINCT answer profiles, each weighted by the sum of its
  #   individuals: identical rows of the indicator table leave X'X unchanged, hence every eigenvalue
  #   and every result on the levels (exact to 5e-13, dev/analysis_engine.md section 4), for a
  #   fraction of the time and memory. `source$key` maps the individuals back to them.
  key   <- as.integer(vctrs::vec_group_id(X))
  first <- which(!duplicated(key))
  pw    <- if (is.null(w$wt)) tabulate(key) else as.vector(rowsum(w$wt, key, reorder = TRUE))

  res <- FactoMineR::MCA(X[first, , drop = FALSE], ncp = ncp, row.w = pw, graph = graph,
                         excl = excl_index(X, active_vars, excl), ...)
  res$source <- c(w$source, list(key = key, w = w$wt))
  res
}

#' @rdname multiple_correspondence_analysis
#' @export
MCA2 <- multiple_correspondence_analysis










#' Readable and Interactive graph for multiple correspondence analysis
#' @description A readable, complete and beautiful graph for multiple
#' correspondence analysis made with \code{\link{multiple_correspondence_analysis}}.
#' \code{\link{ggfacto}} is the same graph, for any analysis.
#' Interactive tooltips, appearing when hovering near points with mouse,
#' allow to keep in mind many important data (tables of active variables,
#' and additional chosen variables) while reading the graph.
#' Profiles of answers (from the graph of "individuals") are drawn in the back,
#' and can be coloured by the clusters of \code{\link{hierarchical_clust}}.
#' Since it is made in the spirit of \code{\link[ggplot2]{ggplot2}}, it is possible to
#' change theme or add another plot elements with \code{+}. Then, interactive
#' tooltips won't appear until you pass the result through \code{\link{ggi}}.
#' Step-by-step functions : use \link{ggmca_data} to get the data frames with every
#' parameter in a MCA printing, then modify, and pass to \link{ggmca_plot}
#' to draw the graph.
#' @param res.mca An object created with \code{\link{multiple_correspondence_analysis}} or
#' \code{FactoMineR::\link[FactoMineR]{MCA}}.
#' @param data The data frame the analysis was made on, in which to find the supplementary
#' variables and the clusters: the whole data frame, even when the analysis was made on a subset
#' of it with \code{\link{multiple_correspondence_analysis}}. Only needed with `sup_vars`,
#' `clust` or the tooltip variables.
#' @param sup_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> The supplementary variables to
#' draw, as in `tab()`: `sup_vars = c(SEXE, AGE)` (strings work too). They need not be given to the
#' analysis before.
#' @param tooltip_vars_1lv <\link[tidyr:tidyr_tidy_select]{tidy-select}> Variables whose first level
#' (a factor), or weighted mean (a number), is added at the top of the tooltips.
#' @param tooltip_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> Variables whose levels are all
#' added at the bottom of the tooltips.
#' @param active_tables The coloured crosstabs shown in the tooltips. `"active"`, the default,
#' crosses each active variable with the others: it is the Burt table the analysis was computed
#' from, so a level at the edge of the cloud shows many colours and one near the centre few.
#' `"sup"` crosses each supplementary variable with the active ones, `c("active", "sup")` does both,
#' and `NULL` none. Percentages are coloured blue when over-represented and red when
#' under-represented, as in \code{tabxplor::tab(color = "diff")}.
#' @param axes The axes to print, as a numeric vector of length 2.
#' @param axes_names Names of all the axes (not just the two selected ones),
#' as a character vector.
#' @param axes_reverse Possibility to reserve the coordinates of the axes by providing
#' a numeric vector : `1` to invert left and right ; `2` to invert up and down ;
#' `1:2` to invert both.
#' @param xlim,ylim Horizontal and vertical axes limits,
#' as double vectors of length 2.
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like \code{"1-"}, and text in parentheses.
#' @param text_repel By default, labels are moved so that they do not overlap, with
#'  \code{ggrepel::\link[ggrepel]{geom_text_repel}}. Set to \code{FALSE} to print each label
#'  exactly at its point, which is faster to draw.
#' @param out_lims_move When \code{TRUE}, the levels out of \code{xlim} or
#'  \code{ylim} are not removed, but moved to the edges of the graph.
#' @param title The title of the graph.
#' @param type Determines the way \code{sup_vars} are printed.
#'    \itemize{
#'    \item \code{"text"} : colored text
#'    \item \code{"points"} : colored points with text legends
#'    \item \code{"labels"} : colored labels
#'    \item \code{"active_vars_only"} : the active levels alone, and the answer profiles
#'    \item \code{"facets"} : one graph of profiles of answer for each levels of the
#'    first \code{sup_vars} (`profiles = TRUE` is not needed). A different color is used for
#'    each.
#'  }
#' @param keep_levels A regex, or a vector of them, matching the supplementary levels to keep:
#' the others are discarded.
#' @param discard_levels A regex, or a vector of them, matching the supplementary levels to
#' discard.
#' @param profiles When set to \code{TRUE}, profiles of answers are drawn in the back
#' of the graph with light-grey points. When hovering with mouse in the interactive
#' version (passed in \code{\link{ggi}}), the answers of individuals to active variables
#' will appears. By default, they are drawn when \code{clust} is given: each profile takes the
#' colour of its cluster, and to hover near one point lights all the points of its cluster.
#' @param profiles_tooltip_discard A regex pattern to remove useless levels
#' among interactive tooltips for profiles of answers (ex. : levels expressing
#' "no" answers).
#' @param clust The variable of `data` holding the clusters, typically made with
#' \code{\link{hierarchical_clust}}, as a bare name (`clust = cah_culture`) or a string. The
#' clusters are drawn as a supplementary variable, and the answer profiles of one cluster are
#' coloured alike and linked at mouse hover (unless `profiles = FALSE`).
#' @param cah,cah_color_groups Deprecated former names of `clust` and `clust_color_groups`.
#' @param max_profiles The maximum number of profiles points to print. Default to 5000.
#' @param dat Deprecated former name of `data`. Still accepted, with a warning;
#' use `data` instead.
#' @param color_groups By default, there is one color group for all the levels
#' of each `sup_vars`. It is  possible to color `sup_vars` with groups created
#' upon their levels, with a regex matched against each level name (the groups are printed in the
#' console with `options(ggfacto.verbose = TRUE)`).
#' For exemple, `color_groups = "^."`  makes the groups upon the first character
#'  of each levels (uselful when their begin by numbers).
#'  \code{color_groups = "^.{3}"} upon the first three characters.
#'  \code{color_groups = "NB.+$"} takes anything between the `"NB"` and the end of levels
#'  names, etc.
#' @param clust_color_groups Color groups for the `clust` variable (the clusters).
#' @param shift_colors Change colors of the \code{sup_vars} points.
#' @param colornames_recode A named character vector with
#' \code{\link[forcats]{fct_recode}} style to rename the colour groups. They are printed in the
#' console with `options(ggfacto.verbose = TRUE)`.
#' @param text_size Size of text.
#' @param size_scale_max The size of the largest point. By default, computed from the spread of
#' the weights of the points drawn, so that the median answer profile stays visible.
#' @param dist_labels When \code{type = points}, the distance of labels
#' from points.
#' @param right_margin A margin at the right, in cm. Useful to read tooltips
#'  over points placed at the right of the graph without formatting problems.
#' @param actives_in_bold Set to `TRUE` to set active variables in bold font
#' (and sup variables in plain).
#' @param sup_in_italic Set the supplementary levels in italics, as in every graph of the package.
#' `FALSE` sets them upright.
#' @param ellipses Set to a number between 0 and 1 to draw a concentration ellipse for
#' each level of the first \code{sup_vars}. \code{0.95} draw ellipses containing 95% of the
#' individuals of each category. \code{0.5} draw median-ellipses, containing half
#' the individuals of each category. Every individual counts, whether or not
#' `profiles = TRUE`.
#' @param color_profiles By default, if \code{clust} is provided, profiles are
#' colored based on clust levels (HCPC clusters). Set do \code{FALSE} to avoid this behaviour.
#' You can also give a character vector with only some of the levels of
#' the `clust` variable .
#' @param base_profiles_color The base color for answers profiles. Default to gray.
#' Set to `NULL` to discard profiles. With `color_profiles`, set to `NULL` to discard the
#' non-colored profiles.
#' @param alpha_profiles The alpha (transparency, between 0 and 1) for profiles of answer.
#' @param scale_color_light A scale color for sup vars points
#' @param scale_color_dark A scale color for sup vars texts
#' @param use_theme By default, a specific \code{ggplot2} theme is used.
#' Set to \code{FALSE} to customize your own \code{\link[ggplot2:theme]{theme}}.
#' @param get_data Returns the data frame to create the plot instead of the plot itself.
#' @param lang \code{NULL} (the session's language), \code{"en"} or \code{"fr"}: the language of
#' the tooltips and of the axis titles.
#'
#' @return A \code{\link[ggplot2:ggplot]{ggplot}} object to be printed in the
#' `RStudio` Plots pane. Possibility to add other gg objects with \code{+}.
#' Sending the result through \code{\link{ggi}} will draw the
#' interactive graph in the Viewer pane using \code{\link[ggiraph]{girafe}}.
#' @export
#'
#' @examples
#' \donttest{
#' data(tea, package = "FactoMineR")
#' res.mca <- multiple_correspondence_analysis(tea, 1:18)
#'
#' # Interactive graph for multiple correspondence analysis :
#' ggmca(res.mca, tea, sup_vars = SPC, ylim = c(NA, 1.2)) |>
#'   ggi() # to make the graph interactive
#'
#' # Hover a level: its crosstabs with every other active variable, the Burt table the analysis
#' #  was computed from. Points near the middle show few colours, points at the edges plenty.
#' ggmca(res.mca, ylim = c(NA, 1.2)) |>
#'   ggi()
#'
#' # Graph with colored clusters (hierarchical clustering on the first three axes)
#' tea <- tea |>
#'   dplyr::mutate(clust = hierarchical_clust(res.mca, ncp = 3, nb_clust = 6))
#' ggmca(res.mca, tea, clust = clust)
#'
#' # Concentration ellipses for each levels of a supplementary variable :
#' ggmca(res.mca, tea, sup_vars = SPC, ylim = c(NA, 1.2),
#'   ellipses = 0.5, profiles = TRUE)
#'
#' # Graph of profiles of answer for each levels of a supplementary variable :
#' ggmca(res.mca, tea, sup_vars = SPC, ylim = c(NA, 1.2),
#'   type = "facets", ellipses = 0.5, profiles = TRUE)
#' }
ggmca <-
  function(res.mca, data, sup_vars, active_tables = "active", tooltip_vars_1lv, tooltip_vars,
           axes = c(1,2), axes_names = NULL, axes_reverse = NULL,
           type = c("text", "labels", "points", "active_vars_only", "facets"),

           color_groups = "^.{0}", clust_color_groups =  "^.+$",
           keep_levels, discard_levels, cleannames = TRUE,

           profiles = NULL, profiles_tooltip_discard = "^Pas |^Non |^Not |^No ",
           clust, max_profiles = 5000,
           alpha_profiles = 0.7, color_profiles = TRUE, base_profiles_color = "#aaaaaa",

           text_repel = TRUE, title, actives_in_bold = NULL, sup_in_italic = TRUE,
           ellipses = NULL,
           xlim, ylim, out_lims_move = FALSE,

           shift_colors = 0, colornames_recode,
           scale_color_light = material_colors_light(),
           scale_color_dark  = material_colors_dark(),
           text_size = 3.5, size_scale_max = NULL, dist_labels = c("auto", 0.04),
           right_margin = 0, use_theme = TRUE, get_data = FALSE, lang = NULL,
           dat, cah, cah_color_groups
  ) {
    # Renamed in 0.4.0; they sit last so no positional call can reach them.
    if (!missing(dat) && missing(data)) data <- renamed_arg(dat, "dat", "data", "ggmca")
    clust <- if (!missing(cah)) {
      rlang::quo(!!renamed_arg(cah, "cah", "clust", "ggmca"))
    } else {
      rlang::enquo(clust)
    }
    if (!missing(cah_color_groups)) clust_color_groups <-
      renamed_arg(cah_color_groups, "cah_color_groups", "clust_color_groups", "ggmca")

    plot_data <- mca_plot_data(
      res.mca, data, sup_vars = rlang::enquo(sup_vars), active_tables = active_tables,
      tooltip_vars_1lv = rlang::enquo(tooltip_vars_1lv), tooltip_vars = rlang::enquo(tooltip_vars),
      color_groups = color_groups, clust_color_groups = clust_color_groups,
      keep_levels = keep_levels, discard_levels = discard_levels, cleannames = cleannames,
      profiles = profiles, profiles_tooltip_discard = profiles_tooltip_discard,
      clust = clust, max_profiles = max_profiles, lang = lang, fn = "ggmca"
    )

    ggmca_plot(plot_data = plot_data,
               axes = axes, axes_names = axes_names, axes_reverse = axes_reverse,
               type = type,
               text_repel = text_repel, title = title,
               actives_in_bold = actives_in_bold, sup_in_italic = sup_in_italic,
               ellipses = ellipses,
               xlim = xlim, ylim = ylim, out_lims_move = out_lims_move,
               color_profiles = color_profiles, base_profiles_color = base_profiles_color,
               alpha_profiles = alpha_profiles,
               shift_colors = shift_colors, colornames_recode = colornames_recode,
               scale_color_light = scale_color_light,
               scale_color_dark  = scale_color_dark,
               text_size = text_size, size_scale_max = size_scale_max,
               dist_labels = dist_labels, right_margin = right_margin,
               use_theme = use_theme, get_data = get_data
    )
  }

#' @describeIn ggmca get the data frames with all parameters to print a MCA graph
# @inheritParams ggmca
#' @return A list to pass to \link{ggmca_plot}: `vars_data` (one row per level), `ind_data`
#'  (one row per answer profile, with `profiles = TRUE`), `individuals` (one row per individual,
#'  with `sup_vars`), `res`, `clust` and `lang`.
#' @export
ggmca_data <-
  function(res.mca, data, sup_vars, active_tables = "active", tooltip_vars_1lv, tooltip_vars,

           color_groups = "^.{0}", clust_color_groups =  "^.+$",
           keep_levels, discard_levels, cleannames = TRUE,

           profiles = NULL, profiles_tooltip_discard = "^Pas |^Non |^Not |^No ",
           clust, max_profiles = 5000, lang = NULL,
           dat, cah, cah_color_groups
  ) {
    # Renamed in 0.4.0; they sit last so no positional call can reach them.
    if (!missing(dat) && missing(data)) data <- renamed_arg(dat, "dat", "data", "ggmca_data")
    clust <- if (!missing(cah)) {
      rlang::quo(!!renamed_arg(cah, "cah", "clust", "ggmca_data"))
    } else {
      rlang::enquo(clust)
    }
    if (!missing(cah_color_groups)) clust_color_groups <-
      renamed_arg(cah_color_groups, "cah_color_groups", "clust_color_groups", "ggmca_data")

    mca_plot_data(
      res.mca, data, sup_vars = rlang::enquo(sup_vars), active_tables = active_tables,
      tooltip_vars_1lv = rlang::enquo(tooltip_vars_1lv), tooltip_vars = rlang::enquo(tooltip_vars),
      color_groups = color_groups, clust_color_groups = clust_color_groups,
      keep_levels = keep_levels, discard_levels = discard_levels, cleannames = cleannames,
      profiles = profiles, profiles_tooltip_discard = profiles_tooltip_discard,
      clust = clust, max_profiles = max_profiles, lang = lang, fn = "ggmca_data"
    )
  }

# The MCA's plot model, which ggmca() and ggmca_data() both return through: the active levels, the
# supplementary and cluster levels, the answer profiles, the individuals. `fn` names the caller a
# refusal speaks of.
#' @keywords internal
#' @noRd
mca_plot_data <- function(res.mca, data, sup_vars, active_tables, tooltip_vars_1lv, tooltip_vars,
                          color_groups, clust_color_groups, keep_levels, discard_levels,
                          cleannames, profiles, profiles_tooltip_discard, clust, max_profiles,
                          lang, fn) {
  if (missing(keep_levels))    keep_levels    <- character()
  if (missing(discard_levels)) discard_levels <- character()
  active_tables <- if (isFALSE(active_tables)) character() else as.character(active_tables)
  stopifnot(length(max_profiles) < 2)
  selected <- list(sup = sup_vars, lv1 = tooltip_vars_1lv, all = tooltip_vars)

  m           <- mca_model(res.mca)
  active_vars <- m$vars

  # The microdata is needed for anything that is not an active variable. It is taken back through
  # the one gate (R/ingress.R), which cuts it down to the fitted rows, in the fitted order.
  if (quo_given(clust) || any(purrr::map_lgl(selected, quo_given))) {
    need_data(missing(data), "the supplementary variables and the clusters", fn)
    cl    <- resolve_clust(clust, data)
    data  <- align_to_fit(m, cl$data)
    clust <- cl$name
  } else {
    data  <- NULL
    clust <- character()
  }
  selected <- purrr::map(selected, select_vars, data = data)
  sup_vars         <- setdiff(selected$sup, active_vars)
  tooltip_vars_1lv <- setdiff(selected$lv1, active_vars)
  tooltip_vars     <- setdiff(selected$all, c(active_vars, tooltip_vars_1lv))
  if (is.null(profiles)) profiles <- length(clust) != 0
  if (length(clust) != 0 && !clust %in% sup_vars) sup_vars <- c(sup_vars, clust)

  with_gda_lang(lang, function(lg) {
    # The units: the answer profiles, crossed with the answers every non-active variable gives.
    extra <- extra_columns(data, c(sup_vars, tooltip_vars_1lv, tooltip_vars), tooltip_vars_1lv)
    units <- cloud_units(m, extra)

    # WARNING: FactoMineR's rows are bound by POSITION (the model's levels): it renames levels.
    lv <- m$levels[m$levels$kept, ]
    vars_data <- dplyr::bind_rows(
      vars_rows(lv$vars, clean_levels(lv$lvs, cleannames), "active",
                id = as.integer(forcats::as_factor(lv$vars)) + 1000L, wcount = lv$wn,
                coord = m$var$coord, contrib = m$var$contrib),
      sup_rows(units, m$coord[units$..profile, , drop = FALSE], sup_vars, m$vs, clust,
               color_groups, clust_color_groups, keep_levels, discard_levels, cleannames),
      central_row(m$W, ncol(m$var$coord))
    )

    # The tooltips: the Burt table's blocks for the crossed levels, a header for every level.
    crossed <- unique(c(if ("active" %in% active_tables) active_vars,
                        if ("sup"    %in% active_tables) sup_vars,
                        intersect(active_tables, c(active_vars, sup_vars))))
    counted <- setdiff(c(active_vars, sup_vars), crossed)
    tip_units <- units
    tip_units[active_vars] <- active_factors(m, units$..profile)
    shown <- c(active_vars, names(extra))
    tip_units[shown] <- purrr::map(tip_units[shown], clean_factor, cleannames = cleannames)
    blocks <- c(list(tip_block(active_vars, gettext("Active variables:"), binary = TRUE)),
                purrr::map(tooltip_vars, \(v) tip_block(v, gettextf("Distribution by %s:", v))))
    tips <- interactive_tooltips(tip_units, crossed, counted, blocks, tooltip_vars_1lv)
    central <- vars_data$role == "central"
    i <- vctrs::vec_match(
      data.frame(vars = ifelse(central, NA, vars_data$vars),
                 lvs  = ifelse(central, NA, as.character(vars_data$lvs))),
      data.frame(vars = tips$vars, lvs = tips$lvs))
    vars_data$begin_text       <- tips$begin_text[i]
    vars_data$interactive_text <- tips$interactive_text[i]

    # The answer profiles, drawn, and the individuals the ellipses and facets group.
    ind_data <- individuals <- NULL
    if (profiles || length(sup_vars) != 0) {
      pts <- cloud_points(mca_point_group(m), m$count, m$wn, max_profiles)
    }
    if (length(sup_vars) != 0) {
      individuals <- individuals_table(
        pts$nb[pts$point[m$key]], if (is.null(m$w)) rep(1, m$n) else m$w,
        m$coord[m$key, , drop = FALSE],
        purrr::map(extra[sup_vars], clean_factor, cleannames = cleannames))
    }
    if (profiles) {
      clusters <- if (length(clust) != 0) {
        point_clusters(pts$point[units$..profile], units$..wn,
                       clean_factor(units[[clust]], cleannames), length(pts$first))
      }
      ind_data <- points_table(pts, m$coord, clusters, vars_data[vars_data$role == "clust", ])

      # An excluded answer, or one matching profiles_tooltip_discard, is left out of the tooltip.
      labels <- clean_levels(m$levels$lvs, cleannames)
      labels[!m$levels$kept] <- NA_character_
      if (length(profiles_tooltip_discard) != 0) {
        labels[str_detect(labels, profiles_tooltip_discard) %in% TRUE] <- NA_character_
      }
      first   <- pts$first[pts$drawn]
      answers <- purrr::map(seq_len(m$Q), \(q) labels[m$codes[first, q]])
      ind_data$interactive_text <- point_tooltips(profile_headings(ind_data), ind_data$count,
                                                  ind_data$wcount, answers)
    }

    plot_model(vars_data, ind_data, individuals,
               res = list(eig = m$eig, axes_names = res.mca$axes_names), clust = clust,
               lang = lg)
  })
}

# Why this exists: the points an MCA draws are the answer profiles whose KEPT answers differ --
# answers that differ only in an excluded level share one point.
#' @keywords internal
#' @noRd
mca_point_group <- function(m) {
  kept <- m$codes
  kept[!m$levels$kept[m$codes]] <- 0L
  as.integer(vctrs::vec_group_id(as.data.frame(kept)))
}

# The first lines of a profile's tooltip: its cluster, and its rank (within its cluster).
#' @keywords internal
#' @noRd
profile_headings <- function(ind_data) {
  bold <- function(x) paste0("<b>", x, "</b>")
  if (!"clust" %in% names(ind_data)) {
    return(list(bold(gettextf("Answer profile n\u00b0%s", ind_data$nb))))
  }
  in_clust <- vctrs::vec_group_id(ind_data$clust)
  rank <- paste0(stats::ave(ind_data$nb, in_clust, FUN = seq_along), "/",
                 stats::ave(ind_data$nb, in_clust, FUN = length))
  list(bold(gettextf("Cluster: %s", ind_data$clust)),
       bold(gettextf("Answer profile n\u00b0%s", rank)))
}
