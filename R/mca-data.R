# PURPOSE: The MCA entry point and the data half of its graph -- multiple_correspondence_analysis()
#   (alias MCA2()), ggmca(), ggmca_data().
# ROLE: Turns a FactoMineR MCA plus its microdata into the plot model R/mca-plot.R draws:
#   list(vars_data, ind_data, individuals, res.mca, clust), flat tables with no list-column.
#   ggmca() itself is pure orchestration of the two halves and holds no logic.
# KEY CONSTRAINTS:
#   - Weights are read back from res.mca$call$row.w, never from `data`, so a tooltip always
#     describes the population the analysis was actually fitted on; `data` itself comes back
#     through R/ingress.R's align_to_fit(), cut down to the fitted rows, and only when a
#     supplementary variable, a cluster or a tooltip variable needs it.
#   - ggmca()'s signature is exactly ggmca_data()'s plus ggmca_plot()'s, with zero overlap. A new
#     argument belongs to one half and must be routed to that half only. That is why the model is
#     `plot_data` and the microdata is `data`: one name each, no collision.
#   - vars_data always carries wcount, derived from the column margin when the tooltip crosstabs
#     do not supply it: type = "points" sizes by it unconditionally.
#   - An answer profile is a group of individuals: its membership is one integer vector
#     (answer_profiles()), and every per-profile number -- counts, cluster, coordinates -- is one
#     vectorised aggregation over it, never a nested tibble per profile.
#   - A cluster's hover id is matched by its NAME, for its label and its profiles alike.
#   - Several arguments are inert on their own and only act alongside an enabling one --
#     keep_levels/discard_levels need sup_vars, tooltip_vars/tooltip_vars_1lv need a table to be
#     built at all, clust needs profiles. tests/testthat/test-ggmca-data.R pins both halves.
#   - varsup() is vendored from GDAtools 1.7.2 (credited in place) and is the only extractor that
#     dispatches on the analysis object's class.
# See: CLAUDE.md section ggfacto architecture > The plot model.

#' Multiple Correspondence Analysis
#' @description A user-friendly wrapper around \code{\link[FactoMineR]{MCA}}, made to
#'  work with \pkg{ggfacto} functions like \code{\link{ggmca}}, \code{\link{mca_interpret}} and
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
#' @param ... Additional arguments to pass to \code{\link[FactoMineR]{MCA}}.
#'
#' @return A `MCA` object from \pkg{FactoMineR}, with one more element, `source`, which records the
#'  rows of `data` that were analysed.
#' @export
#'
#' @examples
#' data(tea, package = "FactoMineR")
#' res.mca <- multiple_correspondence_analysis(tea, 1:18)
#' mca_interpret(res.mca)
#'
#' ggmca(res.mca, tea, sup_vars = "SPC", ylim = c(NA, 1.2)) |>
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
  source <- source_rows(expr, env, data)

  active_vars <- names(tidyselect::eval_select(rlang::enquo(active_vars), data))
  wt          <- tidyselect::eval_select(rlang::enquo(wt), data)
  stopifnot(length(wt) < 2)
  wt <- if (length(wt) != 0) data[[wt]] else NULL

  data <- na_levels(as.data.frame(data[active_vars]), active_vars)

  res <- FactoMineR::MCA(data, ncp = ncp, row.w = wt, graph = graph,
                         excl = excl_index(data, active_vars, excl), ...)
  res$source <- source
  res
}

#' @rdname multiple_correspondence_analysis
#' @export
MCA2 <- multiple_correspondence_analysis










#' Readable and Interactive graph for multiple correspondence analysis
#' @description A readable, complete and beautiful graph for multiple
#' correspondence analysis made with \code{FactoMineR::\link[FactoMineR]{MCA}}.
#' Interactive tooltips, appearing when hovering near points with mouse,
#' allow to keep in mind many important data (tables of active variables,
#' and additional chosen variables) while reading the graph.
#' Profiles of answers (from the graph of "individuals") are drawn in the back,
#' and can be linked to \code{FactoMineR::\link[FactoMineR]{HCPC}} classes.
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
#' @param sup_vars A character vector of supplementary qualitative variables
#' to print (they don't need to be passed in \code{\link[FactoMineR]{MCA}} before).
#' @param tooltip_vars_1lv A character vectors of variables, whose first level
#' (if character/factor) or weighted_mean (if numeric) will be added
#' at the top of interactive tooltips.
#' @param tooltip_vars A character vector of variables (character/factors),
#' whose complete levels will be added at the bottom of interactive tooltips.
#' @param active_tables Should colored crosstables be added in interactive tooltips ?
#' `active_tables = "sup"` crosses each `sup_vars` with active variables.
#' `active_tables = "active"` crosses each active_variables with the other ones,
#' giving results closely related with the burt table used to calculate multiple
#' correspondance analysis. It may take time to calculate with many variables.
#' `active_tables = c("active", "sup")` do both. In tooltips, percentages are colored
#' in blue when spread from mean is positive (over-representations), and in red when
#' spread from mean is negative (under-representations), like in
#' \code{\link[tabxplor]{tab}} with `color = "diff"`.
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
#' @param out_lims_move When \code{TRUE}, the points out of \code{xlim} or
#'  \code{ylim} are not removed, but moved at the edges of the graph.
#' @param title The title of the graph.
#' @param type Determines the way \code{sup_vars} are printed.
#'    \itemize{
#'    \item \code{"text"} : colored text
#'    \item \code{"points"} : colored points with text legends
#'    \item \code{"labels"} : colored labels
#'    \item \code{"active_vars_only"} : no \code{sup_vars}
#'    \item \code{"facets"} : one graph of profiles of answer for each levels of the
#'    first \code{sup_vars} (`profiles = TRUE` is not needed). A different color is used for
#'    each.
#'  }
#' @param keep_levels A character vector of variables levels to keep : others
#' will be discarded.
#' @param discard_levels A character vector of variables levels to discard.
#' @param profiles When set to \code{TRUE}, profiles of answers are drawn in the back
#' of the graph with light-grey points. When hovering with mouse in the interactive
#' version (passed in \code{\link{ggi}}), the answers of individuals to active variables
#' will appears. If \code{clust} is provided, to hover near one point will color all the
#' points of the same \code{\link[FactoMineR]{HCPC}} class.
#' @param profiles_tooltip_discard A regex pattern to remove useless levels
#' among interactive tooltips for profiles of answers (ex. : levels expressing
#' "no" answers).
#' @param clust The variable of `data` holding the clusters, typically made with
#' \code{\link{hierarchical_clust}}, as a bare name (`clust = cah_culture`) or a string. The
#' clusters are drawn as a supplementary variable and, with `profiles = TRUE`, the answer profiles
#' of one cluster are coloured alike and linked at mouse hover.
#' @param cah,cah_color_groups Deprecated former names of `clust` and `clust_color_groups`.
#' @param max_profiles The maximum number of profiles points to print. Default to 5000.
#' @param dat Deprecated former name of `data`. Still accepted, with a warning;
#' use `data` instead.
#' @param color_groups By default, there is one color group for all the levels
#' of each `sup_vars`. It is  possible to color `sup_vars` with groups created
#' upon their levels, with a regex matched against each level name.
#' For exemple, `color_groups = "^."`  makes the groups upon the first character
#'  of each levels (uselful when their begin by numbers).
#'  \code{color_groups = "^.{3}"} upon the first three characters.
#'  \code{color_groups = "NB.+$"} takes anything between the `"NB"` and the end of levels
#'  names, etc.
#' @param clust_color_groups Color groups for the `clust` variable (the clusters).
#' @param shift_colors Change colors of the \code{sup_vars} points.
#' @param colornames_recode A named character vector with
#' \code{\link[forcats]{fct_recode}} style to rename the levels of the color
#' variable if needed (levels used for colors are printed in console message
#' whenever the function is used).
#' @param text_size Size of text.
#' @param size_scale_max Size of points.
#' @param dist_labels When \code{type = points}, the distance of labels
#' from points.
#' @param right_margin A margin at the right, in cm. Useful to read tooltips
#'  over points placed at the right of the graph without formatting problems.
#' @param actives_in_bold Set to `TRUE` to set active variables in bold font
#' (and sup variables in plain).
#' @param sup_in_italic Set to `TRUE` to set sup variables in italics.
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
#' ggmca(res.mca, tea, sup_vars = "SPC", ylim = c(NA, 1.2)) |>
#'   ggi() # to make the graph interactive
#'
#' # Interactive graph with access to all crosstables between active variables (burt table).
#' #  Spread from mean are colored and, usually, points near the middle will have less
#' #  colors, and points at the edges will have plenty. It may takes time to print, but
#' #  helps to interpret the MCA in close proximity with the underlying data.
#' ggmca(res.mca, ylim = c(NA, 1.2), active_tables = "active") |>
#'   ggi()
#'
#' # Graph with colored clusters (hierarchical clustering on the first three axes)
#' tea <- tea |>
#'   dplyr::mutate(clust = hierarchical_clust(res.mca, ncp = 3, nb_clust = 6))
#' ggmca(res.mca, tea, clust = clust, profiles = TRUE)
#'
#' # Concentration ellipses for each levels of a supplementary variable :
#' ggmca(res.mca, tea, sup_vars = "SPC", ylim = c(NA, 1.2),
#'   ellipses = 0.5, profiles = TRUE)
#'
#' # Graph of profiles of answer for each levels of a supplementary variable :
#' ggmca(res.mca, tea, sup_vars = "SPC", ylim = c(NA, 1.2),
#'   type = "facets", ellipses = 0.5, profiles = TRUE)
#' }
ggmca <-
  function(res.mca, data, sup_vars, active_tables, tooltip_vars_1lv, tooltip_vars,
           axes = c(1,2), axes_names = NULL, axes_reverse = NULL,
           type = c("text", "labels", "points", "active_vars_only", "facets"),

           color_groups = "^.{0}", clust_color_groups =  "^.+$",
           keep_levels, discard_levels, cleannames = TRUE,

           profiles = FALSE, profiles_tooltip_discard = "^Pas |^Non |^Not |^No ",
           clust, max_profiles = 5000,
           alpha_profiles = 0.7, color_profiles = TRUE, base_profiles_color = "#aaaaaa",

           text_repel = TRUE, title, actives_in_bold = NULL, sup_in_italic = FALSE,
           ellipses = NULL,
           xlim, ylim, out_lims_move = FALSE,

           shift_colors = 0, colornames_recode,
           scale_color_light = material_colors_light(),
           scale_color_dark  = material_colors_dark(),
           text_size = 3.5, size_scale_max = 4, dist_labels = c("auto", 0.04),
           right_margin = 0, use_theme = TRUE, get_data = FALSE,
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

    plot_data <- ggmca_data(
      data = data,
      res.mca = res.mca, sup_vars = sup_vars,
      active_tables = active_tables, tooltip_vars_1lv = tooltip_vars_1lv, tooltip_vars = tooltip_vars,
      cleannames = cleannames,
      keep_levels = keep_levels, discard_levels = discard_levels,
      profiles = profiles, profiles_tooltip_discard = profiles_tooltip_discard,
      clust = !!clust, max_profiles = max_profiles,
      color_groups = color_groups, clust_color_groups = clust_color_groups
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
#'  with `sup_vars`), `res.mca` and `clust`.
#' @export
ggmca_data <-
  function(res.mca, data, sup_vars, active_tables, tooltip_vars_1lv, tooltip_vars,

           color_groups = "^.{0}", clust_color_groups =  "^.+$",
           keep_levels, discard_levels, cleannames = TRUE,

           profiles = FALSE, profiles_tooltip_discard = "^Pas |^Non |^Not |^No ",
           clust, max_profiles = 5000,
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

    if (missing(sup_vars))          sup_vars          <- character()
    if (missing(active_tables))     active_tables     <- character()
    if (missing(tooltip_vars_1lv))  tooltip_vars_1lv  <- character()
    if (missing(tooltip_vars))      tooltip_vars      <- character()
    if (missing(keep_levels))       keep_levels       <- character()
    if (missing(discard_levels))    discard_levels    <- character()

    # The microdata is needed for anything that is not an active variable. It is taken back through
    # the one gate (R/ingress.R), which cuts it down to the fitted rows, in the fitted order.
    if (!rlang::quo_is_missing(clust) && !rlang::quo_is_null(clust) ||
        length(c(sup_vars, tooltip_vars_1lv, tooltip_vars)) != 0) {
      need_data(missing(data), "the supplementary variables and the clusters", "ggmca")
      clust <- resolve_clust(clust, data)
      data  <- align_to_fit(res.mca, clust$data)
      clust <- clust$name
    } else {
      clust <- character()
    }
    if (length(clust) != 0 && !clust %in% sup_vars) sup_vars <- c(sup_vars, clust)
    stopifnot(length(max_profiles) < 2)

    active_vars <- str_c(colnames(res.mca$call$X)[1:length(res.mca$call$quali)])
    excl <- names(res.mca$call$Xtot)[res.mca$call$excl]

    if (length(sup_vars)    != 0 )      sup_vars <- sup_vars |>
      purrr::discard(\(x) x %in% active_vars)
    if (length(tooltip_vars_1lv) != 0 ) tooltip_vars_1lv <- tooltip_vars_1lv |>
      purrr::discard(\(x) x %in% active_vars)
    if (length(tooltip_vars) != 0 )     tooltip_vars <- tooltip_vars |>
      purrr::discard(\(x) x %in% active_vars | x %in% tooltip_vars_1lv)

    clean_lvs <- function(x) if (cleannames) str_remove_all(x, cleannames_condition()) else x


    # Active variables --------------------------------------------------------------------
    active_var_levels <-
      purrr::map(active_vars, ~ dplyr::pull(res.mca$call$X, .) |>
                   as.factor() |> levels()) |>
      purrr::set_names(active_vars) |>
      purrr::imap_dfr(~ tibble::tibble(vars = .y, lvs = .x))


    freqs    <- tibble::enframe(res.mca$call$marge.col, "lvs", "freq")
    coords   <- tibble::as_tibble(res.mca$var$coord, rownames = "lvs")
    contribs <- tibble::as_tibble(res.mca$var$contrib, rownames = "lvs") |>
      dplyr::rename_with(~ str_replace(., "^Dim ", "contrib"))

    active_vars_data <- active_var_levels |>
      dplyr::left_join(freqs, by = "lvs") |>
      dplyr::left_join(coords, by = "lvs") |>
      dplyr::left_join(contribs, by = "lvs")

    # DESIGN: wcount is computed here, from the raw margin, so it exists on EVERY path -- the
    # tooltip crosstabs supply it only for the variables they actually tabulate, but type =
    # "points" sizes by it unconditionally. marge.col is the column margin, so
    # marge.col * (n active variables) * population is the level's weighted count: the very number
    # the crosstabs produce when they are built. Kept in its own mutate() because the next one
    # overwrites `freq`.
    active_vars_data <- active_vars_data |>
      dplyr::mutate(wcount = .data$freq * length(active_vars) *
                      sum(res.mca$call$row.w, na.rm = TRUE))

    active_vars_data <- active_vars_data |>
      dplyr::group_by(.data$vars) |>
      dplyr::mutate(freq = round(.data$freq/sum(.data$freq) * 100, 0)) |>
      dplyr::ungroup()

    active_vars_data <- active_vars_data |>
      dplyr::filter(!is.na(.data$`Dim 1`)) |>  #Remove excluded levels of active variables
      dplyr::mutate(lvs = str_remove(.data$lvs, str_c("^", .data$vars, "_")))

    if (cleannames == TRUE) active_vars_data <- active_vars_data |>
      dplyr::mutate(lvs = forcats::fct_relabel(.data$lvs, ~ str_remove_all(., cleannames_condition())))

    active_vars_data <- active_vars_data |>
      dplyr::mutate(color_group = factor("active_vars"),
                    id = as.integer(forcats::as_factor(.data$vars)) + 1000L)


    # Supplementary variables -------------------------------------------------------------
    if (length(sup_vars) != 0) {

      sup_vars_data <- purrr::map(sup_vars, ~ varsup(res.mca, data[[.]]) ) |>
        purrr::set_names(sup_vars)

      sup_vars_data <-
        purrr::imap(sup_vars_data,
                    ~ tibble::as_tibble(.x$coord, rownames = "lvs") |>
                      dplyr::mutate(vars = .y) |>
                      dplyr::select("vars", tidyselect::everything())
        )

      # color_group depending on nb of supplementary variables and nb of characters
      #  indicated in color_groups
      if (length(clust) > 0 & length(color_groups) != 1 &
          length(color_groups) == length(sup_vars) - 1L) {

        color_groups_base <- rep(NA_character_, length(sup_vars))
        color_groups_base[sup_vars != clust] <-
          vctrs::vec_recycle(color_groups, length(sup_vars) - 1L)

        color_groups <- color_groups_base

      } else {
        color_groups <- vctrs::vec_recycle(color_groups, length(sup_vars))
      }

      if (length(clust) > 0 ) {
        color_groups[sup_vars == clust] <- clust_color_groups
      }

      sup_vars_data <- sup_vars_data |>
        purrr::map2(color_groups,
                    ~ dplyr::mutate(.x, color_group = forcats::as_factor(str_c(
                      .data$vars, "_", str_extract(.data$lvs, .y)
                    ) |>
                      str_remove("_$")
                    ))
        )


      if (length(keep_levels   ) >= 1L) sup_vars_data <- sup_vars_data |>
        purrr::map(~ dplyr::filter(., str_detect(.data$lvs, keep_levels)
        ) )
      if (length(discard_levels) >= 1L) sup_vars_data <- sup_vars_data |>
        purrr::map(
          ~ dplyr::filter(., !str_detect(.data$lvs,
                                                  str_c(discard_levels,
                                                                 collapse = "|"))
          )
        )

      if (cleannames) sup_vars_data <- sup_vars_data |>
        purrr::map(~ dplyr::mutate(
          .,
          lvs = forcats::fct_relabel(.data$lvs, ~ str_remove_all(., cleannames_condition()))
        ))

      sup_vars_data <- sup_vars_data |>
        dplyr::bind_rows() |>
        dplyr::mutate(id = dplyr::row_number())

      # WARNING: a cluster's hover id is matched by NAME, the same way for its label here and for
      #   its profiles below -- never as.integer(lvs): a factor's codes follow its level order, which
      #   cleannames re-sorts alphabetically ("1", "10", "11", "2", ...), and the label then lit up
      #   another cluster's profiles. Clusters share one id, so hovering any point lights them all.
      if (length(clust) != 0) {
        clust_levels <- clean_lvs(levels(forcats::fct_drop(as.factor(data[[clust]]))))
        is_clust <- sup_vars_data$vars == clust
        sup_vars_data$id[is_clust] <- 10000L +
          match(as.character(sup_vars_data$lvs[is_clust]), clust_levels)
      }

      vars_data <- dplyr::bind_rows(active_vars_data, sup_vars_data)

    } else {
      vars_data <- active_vars_data
    }

    #Add central point
    vars_data <- vars_data |>
      dplyr::add_row(vars        = "All",
                     lvs         = factor("Central point"),
                     color_group = factor("Central point"),
                     # The central point stands for the whole population, so that is its weight.
                     wcount      = sum(res.mca$call$row.w, na.rm = TRUE)) |>
      dplyr::mutate(dplyr::across(
        tidyselect::starts_with("Dim "),
        ~ dplyr::if_else(.data$lvs == "Central point", 0, .)
      ))

    vars_data <- vars_data |>
      dplyr::relocate(tidyselect::starts_with("Dim "), tidyselect::starts_with("contrib"),
                      .after = dplyr::last_col())


    # The microdata of tooltips and profiles ----------------------------------------------
    non_active_vars <- c(sup_vars, tooltip_vars_1lv, tooltip_vars)
    data <- if (length(non_active_vars) != 0) {
      dplyr::bind_cols(tibble::as_tibble(res.mca$call$X[active_vars]),
                       dplyr::select(data, tidyselect::all_of(non_active_vars)))
    } else {
      tibble::as_tibble(res.mca$call$X[active_vars])
    }

    data <- data |>
      dplyr::mutate(dplyr::across(where(is.character), as.factor)) |>
      dplyr::mutate(dplyr::across(where(is.factor), forcats::fct_drop)) |>
      tibble::add_column(row.w = res.mca$call$row.w)

    # The excluded levels are merged into one "Remove_levels", which the tooltips and the profiles
    # leave out.
    active_vars_excl <- purrr::map(data[active_vars], \(x) levels(x)[levels(x) %in% excl])
    active_vars_excl <- active_vars_excl[lengths(active_vars_excl) != 0]

    data <- data |>
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(names(active_vars_excl)),
        ~ forcats::fct_relevel(., active_vars_excl[[dplyr::cur_column()]], after = Inf) |>
          forcats::fct_recode(rlang::splice(purrr::set_names(active_vars_excl[[dplyr::cur_column()]],
                                                             "Remove_levels")))
      ))

    #When MCA() added variable name at the beginning of levels names, remove it
    data <- data |>
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(active_vars),
        ~ forcats::fct_relabel(., ~ str_remove(., paste0("^", dplyr::cur_column(), "_")))
      ))

    if (cleannames == TRUE) data <- data |>
      dplyr::mutate(dplyr::across(
        where(~is.factor(.) | is.character(.)),
        ~ forcats::fct_relabel(., ~str_remove_all(., cleannames_condition()))
      ))


    # Interactive tooltips (sup/active) ----
    if ("active" %in% active_tables | "sup" %in% active_tables) {
      vars_to_keep <- character()
      if ("active" %in% active_tables) vars_to_keep <- c(vars_to_keep, active_vars)
      if ("sup"    %in% active_tables) vars_to_keep <- c(vars_to_keep, sup_vars)
      active_tables <- vars_to_keep
    }

    # Calculate crosstabs for variables in active_tables, and frequencies for sup_vars
    # If crosstabs are not calculated for active_vars, retrieve the info in active_vars_data
    active_vars_without_crosstables <- active_vars[!active_vars %in% active_tables]

    tables_to_do <- c(active_tables[!active_tables %in% sup_vars], sup_vars)
    if(length(tables_to_do) != 0) {

      tips <- interactive_tooltips(data,
                                   sup_vars         = sup_vars,
                                   active_vars      = active_vars,
                                   active_tables    = active_tables,
                                   tooltip_vars_1lv = tooltip_vars_1lv,
                                   tooltip_vars     = tooltip_vars
      )

      # The crosstabs carry their own wcount; where they have one it wins, and the margin-derived
      # one computed above fills in the variables they did not tabulate.
      vars_data <- vars_data |>
        dplyr::left_join(tips, by = c("vars", "lvs"), suffix = c("_pre", "")) |>
        dplyr::mutate(wcount = dplyr::coalesce(.data$wcount, .data$wcount_pre)) |>
        dplyr::select(-"wcount_pre")

    } else {
      vars_data <- vars_data |>
        dplyr::mutate(begin_text = NA_character_, interactive_text = NA_character_)
    }

    if (length(active_vars_without_crosstables) != 0) {
      vars_data <- vars_data |>
        dplyr::mutate(begin_text = dplyr::if_else(
          .data$vars %in% active_vars_without_crosstables,
          true  = paste0("<b>", .data$lvs,"</b>\n", .data$vars,
                         "\nFrequency: ", paste0(.data$freq, "%")),
          false = .data$begin_text
        ))
    }

    #If no entire table have been calculated, we don't have the data for mean point
    #   => we use the data available in res.mca
    if (length(active_tables) == 0) {
      mean_point_interactive_text <- vars_data |>
        dplyr::filter(.data$color_group == "active_vars") |>
        dplyr::mutate(
          text = str_c("\n", .data$lvs, " : ", .data$freq,"%")
        ) |>
        dplyr::summarise(
          text = str_c(.data$text, collapse = "")
        ) |>
        dplyr::pull("text")

      mean_point_interactive_text <-
        str_c("<b>Central point</b>",
                       "\nFrequency: 100%",
                       "\n\n<b>Active variables :</b>",
                       mean_point_interactive_text)

      vars_data <- vars_data |>
        dplyr::mutate(begin_text = dplyr::if_else(
          .data$lvs == "Central point",
          true  = mean_point_interactive_text,
          false = .data$begin_text
        ))
    }

    vars_data <- dplyr::select(vars_data, -tidyselect::any_of("freq"))


    #  Profiles of answers ----
    # The answer profiles are needed for their own points and, through `individuals`, for the
    # ellipses and facets of the first supplementary variable.
    ind_data    <- NULL
    individuals <- NULL
    if (profiles || length(sup_vars) != 0) {
      prof <- answer_profiles(data[active_vars], data$row.w, max_profiles)
      ind_coord <- tibble::as_tibble(res.mca$ind$coord)

      if (length(sup_vars) != 0) {
        individuals <- dplyr::bind_cols(
          tibble::tibble(nb = prof$nb[prof$key], row.w = data$row.w),
          ind_coord,
          data[sup_vars]
        )
      }
    }

    if (profiles) {
      drawn <- prof$drawn
      first <- prof$first[drawn]
      ind_data <- tibble::tibble(nb     = seq_along(drawn),
                                 count  = prof$count[drawn],
                                 wcount = prof$wcount[drawn])

      # DESIGN: a profile's cluster is the weighted plurality of its individuals, missing ones
      #   left out. Clusters made on the analysis are pure within a profile (its individuals share
      #   one point), so the rule only decides for clusters made elsewhere or partly missing, and
      #   every profile with one clustered individual is coloured.
      if (length(clust) != 0) {
        cl <- data[[clust]]
        by_clust <- tapply(data$row.w, list(factor(prof$key, seq_along(prof$first)), cl), sum,
                           default = 0)
        plural <- max.col(by_clust, ties.method = "first")
        plural[rowSums(by_clust) == 0] <- NA
        ind_data$clust <- factor(levels(cl)[plural[drawn]], levels(cl))
        ind_data$id    <- 10000L + as.integer(ind_data$clust)
      } else {
        ind_data$id    <- 10000L + ind_data$nb
      }

      # An excluded answer, or one matching profiles_tooltip_discard, is left out of the tooltip.
      answers <- purrr::map(unname(as.list(data[first, active_vars])), \(a) {
        a <- as.character(a)
        a[a == "Remove_levels"] <- NA_character_
        if (length(profiles_tooltip_discard) != 0) {
          a[str_detect(a, profiles_tooltip_discard) %in% TRUE] <- NA_character_
        }
        a
      })

      counts <- tibble::tibble(
        count  = str_c("n: ", format(round(ind_data$count, 0), trim = TRUE, big.mark = " ")),
        wcount = dplyr::if_else(ind_data$count == ind_data$wcount,
                                true  = "",
                                false = str_c("weighted n: ",
                                              format(round(ind_data$wcount, 0),
                                                     trim = TRUE, big.mark = " "), "\n"))
      )

      heading <- if (length(clust) != 0) {
        in_clust <- vctrs::vec_group_id(ind_data$clust)
        tibble::tibble(
          clust      = str_c("<b>Cluster: ", ind_data$clust, "</b>"),
          profile_nb = str_c("<b>Answer profile n", "\u00b0",
                             stats::ave(ind_data$nb, in_clust, FUN = seq_along), "/",
                             stats::ave(ind_data$nb, in_clust, FUN = length), "</b>")
        )
      } else {
        tibble::tibble(profile_nb = str_c("<b>Answer profile n", "\u00b0", ind_data$nb, "</b>"))
      }

      frags <- c(as.list(heading), as.list(counts), answers)
      names(frags) <- paste0("f", seq_along(frags))
      ind_data$interactive_text <- tidyr::unite(tibble::as_tibble(frags), "text",
                                                tidyselect::everything(), sep = "\n",
                                                na.rm = TRUE)$text

      ind_data <- dplyr::bind_cols(ind_data, ind_coord[first, ])
    }


    list("vars_data"   = vars_data,
         "ind_data"    = ind_data,
         "individuals" = individuals,
         "res.mca"     = list(eig = res.mca$eig, axes_names = res.mca$axes_names),
         "clust"       = clust)
  }


# Why this exists: an answer profile is a group of individuals, so every per-profile number is one
# vectorised aggregation over their membership vector -- never a nested tibble per profile.
# `nb` ranks the profiles by weighted count; `drawn` holds the first `max_profiles` of them.
answer_profiles <- function(answers, row.w, max_profiles) {
  # DESIGN: the key is the answers the tooltip SHOWS (excluded ones merged, names cleaned): the
  #   individuals of such a group share one point, even under `excl`, so it is one profile.
  key    <- as.integer(vctrs::vec_group_id(answers))
  first  <- which(!duplicated(key))
  count  <- tabulate(key, length(first))
  wcount <- as.vector(rowsum(row.w, key, reorder = TRUE))

  drawn <- order(-wcount)
  if (length(max_profiles) != 0) drawn <- drawn[seq_len(min(max_profiles, length(drawn)))]
  nb <- rep(NA_integer_, length(first))
  nb[drawn] <- seq_along(drawn)

  list(key = key, first = first, count = count, wcount = wcount, drawn = drawn, nb = nb)
}






#Code taken from function varsup() of package GDAtools 1.7.2 : thanks to Nicolas Robette
#' @keywords internal
varsup <- function (resmca, var){
  dichotom <- function (data, out = "numeric") {
    if (!is.data.frame(data))
      data <- data.frame(data)
    res <- matrix(nrow = nrow(data), ncol = length(levels(data[,
                                                               1])))
    for (i in 1:ncol(data)) {
      if (is.factor(data[, i]) == FALSE)
        data[, i] <- factor(data[, i])
      nlevels <- length(levels(data[, i]))
      temp <- matrix(nrow = nrow(data), ncol = nlevels)
      for (j in 1:nlevels) {
        temp[, j] <- 0
        temp[data[, i] == levels(data[, i])[j], j] <- 1
      }
      colnames(temp) <- paste(names(data)[i], levels(data[, i]), sep = ".")
      if (i == 1)
        res <- temp
      else res <- cbind(res, temp)
    }
    res <- as.data.frame(res)
    if (out == "factor")
      for (i in 1:ncol(res)) res[, i] <- as.factor(res[, i])
    res
  }

  type <- attr(resmca, "class")[1]
  if (type %in% c("MCA", "stMCA", "multiMCA"))
    eigen <- resmca$eig[, "eigenvalue"]
  if (type %in% c("speMCA", "csMCA"))
    eigen <- resmca$eig$eigen
  if (type == "stMCA") {
    if (resmca$call$input.mca %in% c("MCA", "speMCA",
                                     "csMCA"))
      type <- resmca$call$input.mca
  }
  if (type == "multiMCA") {
    classe_afm <- class(resmca$my.mca[[1]])[1]
    if (classe_afm %in% c("MCA", "speMCA", "csMCA"))
      type <- classe_afm
    if (classe_afm == "csMCA") {
      resmca$call$row.w <- resmca$my.mca[[1]]$call$row.w
      resmca$call$subcloud <- resmca$my.mca[[1]]$call$subcloud
    }
  }
  if (type %in% c("MCA", "speMCA")) {
    wt <- resmca$call$row.w
    v <- factor(var)
    n <- sum(wt)
    FK <- colSums(wt * (dichotom(as.data.frame(v), out = "numeric")))/n
    ind <- resmca$ind$coord
    coord <- stats::aggregate(wt * ind, list(v), sum)[, -1]/n/FK
    vrc <- stats::aggregate(wt * ind * ind, list(v), sum)[, -1]/n/FK -
      coord * coord
    for (i in 1:resmca$call$ncp) coord[, i] <- coord[, i]/resmca$svd$vs[i]
    cos2 <- coord * coord/((1/FK) - 1)
    weight = n * FK
  }
  if (type == "csMCA") {
    wt <- resmca$call$row.w
    n <- sum(wt)
    v <- factor(var)
    FK <- colSums(wt * (dichotom(as.data.frame(v), out = "numeric")))/n
    wt <- wt[resmca$call$subcloud]
    n.w <- sum(wt)
    v <- factor(var[resmca$call$subcloud])
    fK <- colSums(wt * (dichotom(as.data.frame(v), out = "numeric")))/n.w
    ind <- resmca$ind$coord
    coord <- stats::aggregate(wt * ind, list(v), sum)[-1]/n.w/fK
    vrc <- stats::aggregate(wt * ind * ind, list(v), sum)[, -1]/n.w/fK -
      coord * coord
    for (i in 1:resmca$call$ncp) coord[, i] <- coord[, i]/resmca$svd$vs[i]
    cos2 <- coord * coord * FK * FK/fK/(1 - fK)
    weight <- length(wt) * fK
  }
  names(weight) <- levels(v)
  rownames(coord) <- levels(v)
  rownames(cos2) <- levels(v)
  wi <- apply(vrc, 2, stats::weighted.mean, w = weight)
  be <- eigen[1:resmca$call$ncp] - wi
  eta2 <- be/eigen[1:resmca$call$ncp]
  vrc <- rbind(vrc, wi, be, eigen[1:resmca$call$ncp], eta2)
  vrc <- round(vrc, 6)
  rownames(vrc) <- c(levels(v), "within", "between",
                     "total", "eta2")
  coord <- round(coord, 6)
  typic <- sqrt(cos2) * sqrt(length(v) - 1)
  typic <- (((abs(coord) + coord)/coord) - 1) * typic
  pval <- 2 * (1 - stats::pnorm(abs(as.matrix(typic))))
  #cor <- sapply(as.data.frame(ind), function(x) assoc.catcont(v, x, wt, nperm = NULL)$cor)
  list(weight = round(weight, 1), coord = coord, cos2 = round(cos2, 6),
       var = round(vrc, 6), typic = round(typic, 6), pval = round(pval, 6)#,
       #cor = cor
  )
}
