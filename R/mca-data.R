# PURPOSE: The MCA entry point and the data half of its graph -- MCA2(), ggmca(), ggmca_data().
# ROLE: Turns a FactoMineR MCA plus its microdata into the plot model R/mca-plot.R draws:
#   list(vars_data, ind_data, res.mca, cah). ggmca() itself is pure orchestration of the two
#   halves and holds no logic.
# KEY CONSTRAINTS:
#   - Weights are read back from res.mca$call$row.w, never from `dat`, so a tooltip always
#     describes the population the analysis was actually fitted on.
#   - ggmca()'s signature is exactly ggmca_data()'s plus ggmca_plot()'s, with zero overlap. A new
#     argument belongs to one half and must be routed to that half only.
#   - varsup() is vendored from GDAtools 1.7.2 (credited in place) and is the only extractor that
#     dispatches on the analysis object's class.
# See: CLAUDE.md section ggfacto architecture > The plot model.

#' Multiple Correspondence Analysis
#' @description A user-friendly wrapper around \code{\link[FactoMineR]{MCA}}, made to
#'  work better with \pkg{ggfacto} functions like \code{\link{ggmca}}. All variables can
#'  be selected by many different expressions, in the way of the `tidyverse`.
#'  No supplementary vars are to be provided here, since they can be added afterward
#'  in \code{\link{ggmca}}.
#'
#' @param data The data frame.
#' @param active_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}>
# @param sup_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}>
# @param sup_quanti <\link[tidyr:tidyr_tidy_select]{tidy-select}>
#' @param wt <\link[tidyr:tidyr_tidy_select]{tidy-select}>
#' @param graph By default no graph is made, since the result can be ploted with
#'  \code{\link{ggmca}}.
#' @param ncp The number of axes to keep. Default to 5.
#' @param excl A character vector of regular expressions to exclude "junk" categories.
#' Any level of an active variable with any of the detected patterns is not taken into
#' account in the calculation of axes (which is called specific multiple correspondence analysis).
#' @param ... Additionnal arguments to pass to \code{\link[FactoMineR]{MCA}}.
#'
#' @return A `res.mca` object, with all the data necessary to draw the MCA.
#' @export
#'
#' @examples  data(tea, package = "FactoMineR")
#' res.mca <- MCA2(tea, active_vars = 1:18)
#'
#' res.mca %>%
#'   ggmca(tea, sup_vars = c("SPC"), ylim = c(NA, 1.2), text_repel = TRUE) %>%
#'   ggi() #to make the graph interactive
MCA2 <- function(data, active_vars, #sup_vars, sup_quanti,
                 wt, excl, ncp = 5, graph = FALSE, ...) {
  active_vars <- tidyselect::eval_select(rlang::enquo(active_vars), data)
  #sup_vars    <- tidyselect::eval_select(rlang::enquo(sup_vars)   , data)
  #sup_quanti  <- tidyselect::eval_select(rlang::enquo(sup_quanti) , data)
  wt          <- tidyselect::eval_select(rlang::enquo(wt)         , data)
  stopifnot(length(wt) < 2)

  vars <- active_vars #c(active_vars, sup_vars, sup_quanti)
  wt   <- if (length(wt) != 0) { data[[wt]] } else {NULL}
  data <- data[vars]

  new_excl <- character()
  if (!missing(excl)) {
    if (any(is.na(excl))) {
      data <- data |>
        dplyr::mutate(dplyr::across(tidyselect::all_of(names(active_vars)),
                                    ~ forcats::fct_na_value_to_level(., "NA")
        ))
      new_excl <- c("NA", paste0(names(active_vars), "_NA"))
      excl     <- c(excl[!is.na(excl)])
    }

    if (length(excl) != 0) {
      lvs <- purrr::imap_dfr(data, ~ tibble::tibble(var = .y, lvs = levels(.x)))
      lvs <- lvs |>
        dplyr::mutate(excl = str_detect(.data$lvs, paste0(excl, collapse = "|")),
                      lvs2 = paste0(.data$var, "_", .data$lvs)
        ) |>
        dplyr::filter(excl)

      new_excl <- c(lvs$lvs, lvs$lvs2, new_excl)
    }
  }

  FactoMineR::MCA(
    data,
    ncp = ncp,
    #quali.sup  = if(length(sup_vars  ) != 0) { which(names(data) %in% names(sup_vars  )) } else {NULL},
    #quanti.sup = if(length(sup_quanti) != 0) { which(names(data) %in% names(sup_quanti)) } else {NULL},
    row.w = wt,
    graph = graph,
    excl  = if (length(new_excl) != 0) {new_excl} else {NULL},
    ...
  )
}










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
#' @param res.mca An object created with \code{FactoMineR::\link[FactoMineR]{MCA}}.
#' @param dat The data in which to find the supplementary variables, etc.
#' @param sup_vars A character vectors of supplementary qualitative variables
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
#' @param text_repel When \code{TRUE} the graph is not interactive anymore,
#'  but the resulting image is better to print because points and labels don't
#'  overlaps. It uses \code{ggrepel::\link[ggrepel]{geom_text_repel}}.
#' @param out_lims_move When \code{TRUE}, the points out of \code{xlim} or
#'  \code{ylim} are not removed, but moved at the edges of the graph.
#' @param title The title of the graph.
#' @param type Determines the way \code{sup_vars} are printed.
#'    \itemize{
#'    \item \code{"text"} : colored text
#'    \item \code{"points"} : colored points with text legends
#'    \item \code{"labels"} : colored labels
#'    \item \code{"active_vars_only"} : no \code{sup_vars}
#'    \item \code{"numbers"} : colored labels of prefix numbers, with small names
#'    \item \code{"facets"} : one graph of profiles of answer for each levels of the
#'    first \code{sup_vars}. A different color is used for each.
#'  }
#' @param keep_levels A character vector of variables levels to keep : others
#' will be discarded.
#' @param discard_levels A character vector of variables levels to discard.
#' @param profiles When set to \code{TRUE}, profiles of answers are drawn in the back
#' of the graph with light-grey points. When hovering with mouse in the interactive
#' version (passed in \code{\link{ggi}}), the answers of individuals to active variables
#' will appears. If \code{cah} is provided, to hover near one point will color all the
#' points of the same \code{\link[FactoMineR]{HCPC}} class.
#' @param profiles_tooltip_discard A regex pattern to remove useless levels
#' among interactive tooltips for profiles of answers (ex. : levels expressing
#' "no" answers).
#' @param cah A HCPC clusters variable made with \code{\link[FactoMineR]{HCPC}}
#' on `res.mca`, to link the answers-profiles points who share the same HCPC class
#' (will be colored the same color and linked at mouse hover).
#' @param max_profiles The maximum number of profiles points to print. Default to 5000.
#' @param color_groups By default, there is one color group for all the levels
#' of each `sup_vars`. It is  possible to color `sup_vars` with groups created
#' upon their levels, with a regex matched against each level name.
#' For exemple, `color_groups = "^."`  makes the groups upon the first character
#'  of each levels (uselful when their begin by numbers).
#'  \code{color_groups = "^.{3}"} upon the first three characters.
#'  \code{color_groups = "NB.+$"} takes anything between the `"NB"` and the end of levels
#'  names, etc.
#' @param cah_color_groups Color groups for the `cah` variable (HCPC clusters).
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
#' the individuals of each category. Note that, if `max_profiles` is provided, ellipses
#' won't be made with all individuals.
#' @param color_profiles By default, if \code{cah} is provided, profiles are
#' colored based on cah levels (HCPC clusters). Set do \code{FALSE} to avoid this behaviour.
#' You can also give a character vector with only some of the levels of
#' the `cah` variable .
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
#' res.mca <- MCA2(tea, active_vars = 1:18)
#'
#' # Interactive graph for multiple correspondence analysis :
#' res.mca |>
#'   ggmca(tea, sup_vars = c("SPC"), ylim = c(NA, 1.2), text_repel = TRUE) |>
#'   ggi() #to make the graph interactive
#'
#' # Interactive graph with access to all crosstables between active variables (burt table).
#' #  Spread from mean are colored and, usually, points near the middle will have less
#' #  colors, and points at the edges will have plenty. It may takes time to print, but
#' #  helps to interpret the MCA in close proximity with the underlying data.
#' res.mca |>
#'   ggmca(tea, ylim = c(NA, 1.2), active_tables = "active", text_repel = TRUE) |>
#'   ggi()
#'
#' # Graph with colored HCPC clusters
#' cah <- FactoMineR::HCPC(res.mca, nb.clust = 6, graph = FALSE)
#' tea$clust <- cah$data.clust$clust
#' ggmca(res.mca, tea, cah = "clust", profiles = TRUE, text_repel = TRUE)
#'
#' # Concentration ellipses for each levels of a supplementary variable :
#' ggmca(res.mca, tea, sup_vars = "SPC", ylim = c(NA, 1.2),
#'   ellipses = 0.5, text_repel = TRUE, profiles = TRUE)
#'
#' # Graph of profiles of answer for each levels of a supplementary variable :
#' ggmca(res.mca, tea, sup_vars = "SPC", ylim = c(NA, 1.2),
#'   type = "facets", ellipses = 0.5, profiles = TRUE)
#' }
ggmca <-
  function(res.mca, dat, sup_vars, active_tables, tooltip_vars_1lv, tooltip_vars,
           axes = c(1,2), axes_names = NULL, axes_reverse = NULL,
           type = c("text", "labels", "points", "numbers", "facets"),

           color_groups = "^.{0}", cah_color_groups =  "^.+$",
           keep_levels, discard_levels, cleannames = TRUE,

           profiles = FALSE, profiles_tooltip_discard = "^Not |^No |^Pas |^Non ",
           cah, max_profiles = 5000,
           alpha_profiles = 0.7, color_profiles = TRUE, base_profiles_color = "#aaaaaa",

           text_repel = FALSE, title, actives_in_bold = NULL, sup_in_italic = FALSE,
           ellipses = NULL,
           xlim, ylim, out_lims_move = FALSE,

           shift_colors = 0, colornames_recode,
           scale_color_light = material_colors_light(),
           scale_color_dark  = material_colors_dark(),
           text_size = 3.5, size_scale_max = 4, dist_labels = c("auto", 0.04),
           right_margin = 0, use_theme = TRUE, get_data = FALSE
  ) {

    data <- ggmca_data(
      dat = dat,
      res.mca = res.mca, sup_vars = sup_vars,
      active_tables = active_tables, tooltip_vars_1lv = tooltip_vars_1lv, tooltip_vars = tooltip_vars,
      cleannames = cleannames,
      keep_levels = keep_levels, discard_levels = discard_levels,
      profiles = profiles, profiles_tooltip_discard = profiles_tooltip_discard,
      cah = cah, max_profiles = max_profiles,
      color_groups = color_groups, cah_color_groups = cah_color_groups
    )

    ggmca_plot(data = data,
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
#' @return A list containing the data frames to pass to \link{ggmca_plot}.
#' @export
ggmca_data <-
  function(res.mca, dat, sup_vars, active_tables, tooltip_vars_1lv, tooltip_vars,

           color_groups = "^.{0}", cah_color_groups =  "^.+$",
           keep_levels, discard_levels, cleannames = TRUE,

           profiles = FALSE, profiles_tooltip_discard = "^Pas |^Non |^Not |^No ",
           cah, max_profiles = 5000
  ) {
    if (missing(sup_vars))          sup_vars          <- character()
    if (missing(active_tables))     active_tables     <- character()
    if (missing(tooltip_vars_1lv))  tooltip_vars_1lv  <- character()
    if (missing(tooltip_vars))      tooltip_vars      <- character()
    if (missing(keep_levels))       keep_levels       <- character()
    if (missing(discard_levels))    discard_levels    <- character()
    if (missing(cah) ) {
      cah <- character()
    } else if (length(cah) == 0) {
      cah <- character()
    } else if(! cah %in% sup_vars) {
      # warning(cah, " was not found among the supplementary variables of the mca")
      #cah <- character()
      sup_vars <- c(sup_vars, cah)
    }
    stopifnot(length(max_profiles) < 2)

    active_vars <- str_c(colnames(res.mca$call$X)[1:length(res.mca$call$quali)])
    excl <- names(res.mca$call$Xtot)[res.mca$call$excl]

    if (length(sup_vars)    != 0 )      sup_vars <- sup_vars |>
      purrr::discard(\(x) x %in% active_vars)
    if (length(tooltip_vars_1lv) != 0 ) tooltip_vars_1lv <- tooltip_vars_1lv |>
      purrr::discard(\(x) x %in% active_vars) #| . %in% sup_vars
    if (length(tooltip_vars) != 0 )     tooltip_vars <- tooltip_vars |>
      purrr::discard(\(x) x %in% active_vars | x %in% tooltip_vars_1lv) #| . %in% sup_vars


    #if (names_darker == "auto") {      # if (type[1] == "points") names_darker <- TRUE
    # if (type[1] %in% c("active_vars_only", "labels", "text")) names_darker <- FALSE
    #}






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
      dplyr::left_join(contribs, by = "lvs") |>
      tidyr::nest(contribs = tidyselect::starts_with("contrib"))

    active_vars_data <- active_vars_data |>
      dplyr::group_by(.data$vars) |>
      dplyr::mutate(freq = round(.data$freq/sum(.data$freq) * 100, 0)) |>
      dplyr::ungroup()

    dimensions <- names(active_vars_data)[str_detect(names(active_vars_data), "Dim ")] |>
      purrr::set_names() |>
      purrr::map_dfc(~ 0)

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

      sup_vars_data <- purrr::map(sup_vars, ~ varsup(res.mca, dat[[.]]) ) |>
        purrr::set_names(sup_vars)

      # Do something with "within" et "between" variance ? ($var)

      sup_vars_data <-
        purrr::imap(sup_vars_data,
                    ~ tibble::as_tibble(.x$coord, rownames = "lvs") |>
                      dplyr::mutate(vars = .y) |>
                      dplyr::select("vars", tidyselect::everything())
        )

      # color_group depending on nb of supplementary variables and nb of characters
      #  indicated in color_groups
      if (length(cah) > 0 & length(color_groups) != 1 &
          length(color_groups) == length(sup_vars) - 1L) {

        color_groups_base <- rep(NA_character_, length(sup_vars))
        color_groups_base[sup_vars != cah] <-
          vctrs::vec_recycle(color_groups, length(sup_vars) - 1L)

        color_groups <- color_groups_base

      } else {
        color_groups <- vctrs::vec_recycle(color_groups, length(sup_vars))
      }

      if (length(cah) > 0 ) {
        color_groups[sup_vars == cah] <- cah_color_groups
      }
      # print(purrr::set_names(color_groups, sup_vars))

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

      dimensions <- names(sup_vars_data[[1]]) |>
        purrr::keep(\(x) str_detect(x, "Dim ")) |>
        purrr::set_names() |>
        purrr::map_dfc(~ 0)


      #Make that, if HCPC is in sup_vars AND in profiles, ggiraph data_id are the same :
      # les deux seront colores lorsqu'on survolera l'un ou l'autre
      #sup_vars_data <- sup_vars_data %>% purrr::imap(~ dplyr::mutate(.x, sup_var = .y))
      if (length(cah) != 0) {
        if (cah %in% sup_vars) sup_vars_data <- sup_vars_data |>
            (\(l) purrr::map_if(l, names(l) == cah,
                          ~ dplyr::mutate(., cah_id = as.integer(.data$lvs) + 10000L),
                          .else = ~ dplyr::mutate(., cah_id = NA_integer_)))()
      }

      #Bind sup_vars data
      sup_vars_data <- sup_vars_data |> dplyr::bind_rows()

      # ID numbers to use with ggiraph to highlight elements at hover
      if (length(cah) != 0) {
        if (cah %in% sup_vars) {
          sup_vars_data <- sup_vars_data |>
            dplyr::mutate(id = dplyr::if_else(is.na(.data$cah_id),
                                              dplyr::row_number(),
                                              .data$cah_id))
        } else {
          sup_vars_data <- sup_vars_data |> dplyr::mutate(id = dplyr::row_number())
        }
      } else {
        sup_vars_data <- sup_vars_data |> dplyr::mutate(id = dplyr::row_number())
      }

      #Useful functions :
      #fct_relevel_quiet <- purrr::quietly(forcats::fct_relevel)
      # bind_cols_quiet   <- purrr::quietly(dplyr::bind_cols)
      #tab_spread <- function(data) dplyr::mutate_at(data, dplyr::vars(-1, -ncol(data)), ~. - dplyr::last(.))
      # tab_spread_chr <- function(data) {
      #   dplyr::mutate_at(data, dplyr::vars(-1, -tidyselect::any_of("Total")), ~ dplyr::case_when(
      #     dplyr::row_number() == nrow(data) ~ str_c(., "%"),
      #     . - dplyr::last(.) > 0         ~ str_c("(", str_pad(str_c("+" , sign(. - dplyr::last(.)) * (. - dplyr::last(.))), 3 + get_digits(.)), "%) ", str_pad(., 2 + get_digits(.)), "%"),
      #     TRUE                    ~ str_c("(", str_pad(str_c(" -", sign(. - dplyr::last(.)) * (. - dplyr::last(.))), 4 + get_digits(.)), "%) ", str_pad(., 2 + get_digits(.)), "%") )
      #   )
      # }

      vars_data <- dplyr::bind_rows(active_vars_data, sup_vars_data)

    } else {
      vars_data <- active_vars_data
      #sup_vars_data <- NULL
    }

    #Add central point
    vars_data <- vars_data |>
      dplyr::add_row(vars        = "All",
                     lvs         = factor("Central point"),
                     color_group = factor("Central point")) |>
      dplyr::mutate(dplyr::across(
        tidyselect::starts_with("Dim "),
        ~ dplyr::if_else(.data$lvs == "Central point", 0, .)
      ))

    #Reorder variables in vars_data
    vars_data <- vars_data |>
      dplyr::relocate(tidyselect::starts_with("Dim "), tidyselect::any_of("contribs"),
                      .after = dplyr::last_col())





    ###Prepare data for tooltips and profiles ---
    non_active_vars <- c(sup_vars, tooltip_vars_1lv, tooltip_vars)
    if (length(non_active_vars) != 0 ) {
      dat  <- dplyr::bind_cols(tibble::as_tibble(res.mca$call$X[active_vars]),
                               dplyr::select(dat, tidyselect::all_of(non_active_vars)))
    } else {
      dat <- tibble::as_tibble(res.mca$call$X[active_vars])
    }
    #sel3 <- tooltip_vars[!tooltip_vars %in% c(sel1, active_vars)]
    #dat3 <- dat %>% dplyr::select(tidyselect::all_of(sel3))

    dat <- dat |>
      dplyr::mutate(dplyr::across(where(is.character), as.factor)) |>
      dplyr::mutate(dplyr::across(where(is.factor), forcats::fct_drop)) |>
      tibble::add_column(row.w = res.mca$call$row.w)

    #Remove excluded levels (now, or after by renaming them here)
    excl_levels <-
      purrr::imap_dfr(dat[active_vars],
                      ~ tibble::tibble(active_vars = .y, lvs = levels(.x))
                      ) |>
      #dplyr::mutate(lvs2 = str_c(.data$active_vars, "_",.data$ lvs)) |>
      dplyr::filter(.data$lvs %in% excl) #| .data$lvs2 %in% excl

    excl_levels <- excl_levels |>
      dplyr::group_by(active_vars) |>
      dplyr::summarise(excl = list(.data$lvs), .groups = "drop")

    excl_levels <- purrr::set_names(excl_levels$excl, excl_levels$active_vars)

    active_var_real_levels <-
      purrr::imap(dat[active_vars], ~ tibble::tibble(active_vars = .y, lvs = levels(.x)))

    active_vars_excl <- active_var_real_levels |>
      purrr::map(~ dplyr::filter(., .data$lvs %in% excl) |> dplyr::pull(.data$lvs))
    active_vars_excl <- active_vars_excl[purrr::map_lgl(active_vars_excl, ~ length(.) != 0)]

    dat <- dat |>
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(names(active_vars_excl)),
        ~ forcats::fct_relevel(., active_vars_excl[[dplyr::cur_column()]], after = Inf) |>
          forcats::fct_recode(rlang::splice(purrr::set_names(active_vars_excl[[dplyr::cur_column()]],
                                                             "Remove_levels")))
      ))

    #When MCA() added variable name at the beginning of levels names, remove it
    dat <- dat |>
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(active_vars),
        ~ forcats::fct_relabel(., ~ str_remove(., paste0("^", dplyr::cur_column(), "_")))
      ))

    if (cleannames == TRUE) dat <- dat |>
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

      interactive_text <- interactive_tooltips(dat,
                                               sup_vars         = sup_vars,
                                               active_vars      = active_vars,
                                               active_tables    = active_tables,
                                               tooltip_vars_1lv = tooltip_vars_1lv,
                                               tooltip_vars     = tooltip_vars
      )

      text_vars <- names(interactive_text)[purrr::map_lgl(interactive_text, is.character)]

      vars_data <- vars_data |> dplyr::left_join(interactive_text, by = c("vars", "lvs"))

    } else {
      text_vars <- "begin_text"
      vars_data <- vars_data |> dplyr::mutate(begin_text = NA_character_)
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
    # If no active tables, we still want to calculate wcounts

    #If no entire table have been calculated, we don't have the data for mean point
    #   => we use the data available in res.mca

    # if(length(tables_to_do) == 0) {
    if (length(active_tables) == 0) {
      mean_point_interactive_text <- vars_data |>
        dplyr::ungroup() |>
        dplyr::filter(.data$color_group == "active_vars") |>
        dplyr::mutate(
          text = str_c("\n", .data$lvs, " : ", .data$freq,"%")
        ) |>
        dplyr::summarise(
          text = str_c(.data$text, collapse = "")
        ) |>
        dplyr::pull(.data$text)

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

      # mean_point_data <-
      #   tibble::tibble(!!!dimensions := 0,
      #                  color_group = "0", numbers = 0, wcount = 1,
      #                  interactive_text = mean_point_interactive_text)
      # scale_color_named_vector <- character()
      # type <- "active_vars_only"
    }

    if (sum(vars_data$lvs == "Central point", na.rm = TRUE) >= 2) {

      vars_data <- vars_data |>
        dplyr::filter(!(.data$lvs == "Central point" & duplicated(.data$lvs)))
    }


    vars_data <- dplyr::select(vars_data, -tidyselect::any_of("freq"))
    vars_data <- tidyr::nest(vars_data, interactive_text = tidyselect::all_of(text_vars))















    # res.mca$var$cos2 %>% tibble::as_tibble(rownames = "lvs") %>% dplyr::mutate_at(-1, ~ tabxplor::as_pct(.)) # %>% dplyr::rowwise() %>% dplyr::mutate(Total = sum(dplyr::c_across(`Dim 1`:`Dim 8`)))
    # res.mca$var$v.test %>% tibble::as_tibble(rownames = "lvs")
    # res.mca$var$eta2 %>% tibble::as_tibble(rownames = "lvs") %>% dplyr::mutate_at(-1, ~ tabxplor::as_pct(.))

    # #Quality of representation, calculated by % of the variance of questions (must be done with all axes in res.mca !)
    # res.mca$var$cos2 %>% tibble::as_tibble(rownames = NULL) %>%
    #   purrr::map2_dfc(res.mca$eig[1:length(.), 1], ~.x*.y) %>% dplyr::rowwise() %>%
    #   dplyr::mutate(Total = sum(dplyr::c_across(1:length(.)))) %>% dplyr::ungroup() %>%
    #   dplyr::mutate_all(~ tabxplor::as_pct(./Total)) %>% dplyr::bind_cols(tibble::as_tibble(res.mca$var$cos2, rownames = "lvs")[1]) %>%
    #   dplyr::select(lvs, tidyselect::everything())

    # # Add tables of crossed active vars in tooltips
    #  interactive_text <- interactive_tooltips(dat, sup_vars, active_vars,
    #                                           tooltip_vars_1lv, tooltip_vars)
    #
    #  interactive_text <- interactive_text %>%
    #    purrr::map(~ tidyr::nest(., interactive_text = names(.)[purrr::map_lgl(., is.character)] ))






    #  Profiles of answers ----
    #; weighted : nb of individuals * weight variable
    if (profiles) {
      ind_data <- dplyr::bind_cols(
        dplyr::select(dat, -tidyselect::any_of(c(tooltip_vars_1lv[!tooltip_vars_1lv %in% sup_vars],
                                                 tooltip_vars[!tooltip_vars %in% sup_vars]))),
        tibble::as_tibble(res.mca$ind$coord)
      ) # |>
      # # re put base active vars, without removing `excl = `
      # dplyr::select(-tidyselect::all_of(active_vars)) |>
      # dplyr::bind_cols(tibble::as_tibble(res.mca$call$X[active_vars]))

      # ind_data <-
      #   tibble::as_tibble(res.mca$call$X[c(res.mca$call$quali, which(names(res.mca$call$X) == cah),
      #                                      which(names(res.mca$call$X) %in% sup_vars) )]) %>%
      #   tibble::add_column(row.w = res.mca$call$row.w) %>%
      #   dplyr::bind_cols(tibble::as_tibble(res.mca$ind$coord))

      coord_names <- colnames(res.mca$ind$coord)

      # if (cleannames == TRUE) ind_data <- ind_data %>%
      #   dplyr::mutate(dplyr::across(
      #     where(is.factor),
      #     ~ forcats::fct_relabel(~ str_remove_all(., cleannames_condition()))
      #     ))

      # If NA in HCPC clust : for each combination of active_vars, we attribute
      #  the majotity class (>50%)

      if (length(cah) != 0) {
        #cah_levels <- dplyr::pull(ind_data, !!rlang::sym(cah) ) %>% levels()

        # ind_data_save <- ind_data
        # ind_data <- ind_data_save

        # # add false NAs to test
        # samp <- ind_data |>
        #   dplyr::mutate(rn = dplyr::row_number()) |>
        #   dplyr::group_by(!!!rlang::syms(active_vars)) |>
        #   dplyr::mutate(n = dplyr::n()) |>
        #   dplyr::ungroup() |>
        #   dplyr::select(rn, n) |>
        #   dplyr::filter(n >=2 ) |>
        #   dplyr::slice_sample(n = 15) |>
        #   dplyr::pull(rn)
        #
        # ind_data <- ind_data |>
        #   dplyr::mutate(
        #        cah_culture = dplyr::if_else(!dplyr::row_number() %in% samp,
        #                                  cah_culture, factor(NA))
        #   )
        # ind_data <- ind_data |> dplyr::mutate(sup1 = cah_culture)
        # sup_vars <- c(sup_vars, "sup1")

        cah_any_NA <- dplyr::pull(ind_data, cah) |> is.na() |> any()
        if (cah_any_NA) {
          ind_data <- ind_data |> complete_cah(cah = cah, active_vars = active_vars)
        }
        # data |> tibble::as_tibble() |> tabxplor::tab(cah_culture)
        # data |> tabxplor::tab(cah, wt = count)

        cah_levels <- dplyr::pull(ind_data, cah) |> levels()

        ind_data <- ind_data |>
          dplyr::mutate(!!rlang::sym(cah) := as.character(!!rlang::sym(cah) ) ) |>
          tidyr::nest(sup_vars = tidyselect::all_of(sup_vars),
                      row.w    = "row.w",
                      coord    = tidyselect::all_of(coord_names),
                      cah      = !!rlang::sym(cah)
          ) |>
          dplyr::mutate(
            count  = purrr::map_int(.data$row.w, ~ nrow(.)),

            wcount = purrr::map_dbl(.data$row.w, ~ sum(., na.rm = TRUE)),

            cah    = purrr::map_chr(.data$cah, ~ dplyr::first(dplyr::pull(., 1))) |>
              as.factor() |> forcats::fct_relevel(cah_levels)
          ) |>
          dplyr::arrange(-.data$wcount)
        # 0.661149 secs (much longer in data.table here)



        if (length(max_profiles) != 0) ind_data <- ind_data |> dplyr::slice(1:max_profiles)

        ind_data <- ind_data |>
          dplyr::mutate(nb = dplyr::row_number(),
                        cah_id = as.integer(.data$cah)) |>
          dplyr::group_by(.data$cah) |>
          dplyr::mutate(nb_in_cah = dplyr::row_number(),
                        nb_tot_cah = dplyr::n()) |>
          dplyr::ungroup() |>
          dplyr::mutate(coord = purrr::map(.data$coord, ~ .[1,])) |>
          tidyr::unnest("coord")


      } else {
        ind_data <- ind_data |>
          tidyr::nest(sup_vars = tidyselect::all_of(sup_vars),
                      row.w    = .data$row.w,
                      coord    = tidyselect::all_of(coord_names)

          ) |>
          dplyr::mutate(count  = purrr::map_int(.data$row.w, ~ nrow(.)),
                        wcount = purrr::map_dbl(.data$row.w, ~ sum(., na.rm = TRUE))
          ) |>
          dplyr::arrange(-.data$wcount)

        if (length(max_profiles) != 0) ind_data <- ind_data |> dplyr::slice(1:max_profiles)

        ind_data <- ind_data |>
          dplyr::mutate(nb    = dplyr::row_number(),
                        coord = purrr::map(.data$coord, ~ .[1,])) |>
          tidyr::unnest(.data$coord)
      }

      ind_data <- ind_data |>
        dplyr::mutate(dplyr::across(tidyselect::all_of(active_vars),
                                    ~ fct_detect_replace(., profiles_tooltip_discard, "#"))) |>
        dplyr::mutate(dplyr::across(where(is.factor), as.character))


      if (length(cah) != 0) {
        ind_data <- ind_data |>
          dplyr::mutate(
            cah_base = .data$cah,
            count_base = .data$count,
            wcount_base = .data$wcount,
            cah        = str_c("<b>Cah: ", .data$cah, "</b>"),
            profile_nb = str_c("<b>Answer profile n",
                                        "\u00b0",
                                        .data$nb_in_cah, "/", .data$nb_tot_cah, "</b>"),
            count      = str_c("n: ", format(round(.data$count, 0),
                                                      trim = TRUE, big.mark = " ")),
            wcount     = dplyr::if_else(
              condition = .data$count == .data$wcount,
              true      = "",
              false     = str_c("weighted n: ",
                                         format(round(.data$wcount, 0),
                                                trim = TRUE, big.mark = " "), "\n")
            )

          ) |>
          tidyr::nest(interactive_text = tidyselect::all_of(c("cah", "profile_nb", "count", "wcount",
                                                              active_vars))) |>
          dplyr::rename("cah" = "cah_base", "count" = "count_base",
                        "wcount" = "wcount_base")

      } else {
        ind_data <- ind_data |>
          dplyr::mutate(
            count_base  = .data$count,
            wcount_base = .data$wcount,
            profile_nb  = str_c("<b>Answer profile n",
                                         "\u00b0",
                                         nb = .data$nb,  "</b>"),
            count       = str_c("n: ", format(round(.data$count, 0),
                                                       trim = TRUE, big.mark = " ")),
            wcount      = dplyr::if_else(
              condition = .data$count == .data$wcount,
              true      = "",
              false     = str_c("weighted n: ",
                                         format(round(.data$wcount, 0),
                                                trim = TRUE, big.mark = " "), "\n")
            )
          ) |>
          tidyr::nest(interactive_text = c("profile_nb", "count", "wcount",
                                           tidyselect::all_of(active_vars))) |>
          dplyr::rename("count" = "count_base", "wcount" = "wcount_base")
      }

      ind_data <- ind_data |>
        dplyr::select(-tidyselect::any_of(c("nb_in_cah", "nb_tot_cah"))) |>
        dplyr::relocate(tidyselect::any_of(c("nb", "count", "wcount", "cah", "cah_id",
                                             "interactive_text", "sup_vars", "row.w")),
                        .before = 1)

    } else {
      ind_data <- NULL
    }


    data <- list("vars_data"= vars_data,
                 "ind_data" = ind_data,
                 "res.mca"  = list(eig = res.mca$eig, axes_names = res.mca$axes_names),
                 "cah"      = cah
    )

    data
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


#' @keywords internal
complete_cah <- function(data, cah, active_vars, treshold = 0.5) {
  data.table::setDT(data)


group_count <- cah_pct  <- cah_max <- rn <-  cah_counts <- NULL

  # data[, cah_base := eval(str2expression(cah))]
  data[, cah_counts := .N, by = c(active_vars, cah)]
  data[, group_count := .N, by = eval(active_vars)]
  data[, rn := 1:.N]
  data[, cah_pct := dplyr::if_else(
    !is.na(eval(str2expression(cah))),
    true  = eval(str2expression("cah_counts"))/eval(str2expression("group_count")),
    false = eval(str2expression("cah_counts"))/eval(str2expression("group_count")) - 0.01
    )]
  data[, cah_max := dplyr::first(eval(str2expression("rn"))) - 1L +
         dplyr::first(which(eval(str2expression("cah_pct")) >= treshold), default = NA_real_),
       by = eval(active_vars)]
  data[, eval(cah) := eval(str2expression(cah))[eval(str2expression("cah_max"))] ]

  # data |>
  #   dplyr::mutate(group = paste0(!!!rlang::syms(active_vars)) |>
  #                   forcats::as_factor() |> as.integer()) |>
  #   dplyr::select(group,
  #                 group_count, cah_counts, cah_base, cah_culture, cah_pct,
  #                 cah_max ) |>
  #   tibble::as_tibble() |>
  #   tabxplor::new_tab() |>
  #   dplyr::filter(cah_pct < 1)  |>
  #   dplyr::group_by(group) |>
  #   dplyr::arrange(.by_group = TRUE) |>
  #   print(n = 900)

  data[, cah_counts := NULL]
  data[, group_count := NULL]
  data[, cah_pct := NULL]
  data[, cah_max := NULL]
  data[, rn := NULL]

  data.table::setDF(data)
  data <- data |> tibble::as_tibble()
  data
}
