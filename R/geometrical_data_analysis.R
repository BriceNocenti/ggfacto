#Import data.table in NAMESPACE :
#' Internal data.table methods
#' @import data.table
#' @keywords internal
#' @name tabxplor-data.table
NULL




# Geometrical data analysis -------------------------------------------------------



## MCA----


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
      data <- data %>%
        dplyr::mutate(dplyr::across(tidyselect::all_of(names(active_vars)),
                                    ~ forcats::fct_na_value_to_level(., "NA")
        ))
      new_excl <- c("NA", paste0(names(active_vars), "_NA"))
      excl     <- c(excl[!is.na(excl)])
    }

    if (length(excl) != 0) {
      lvs <- purrr::imap_dfr(data, ~ tibble::tibble(var = .y, lvs = levels(.x)))
      lvs <- lvs %>%
        dplyr::mutate(excl = stringr::str_detect(.data$lvs, paste0(excl, collapse = "|")),
                      lvs2 = paste0(.data$var, "_", .data$lvs)
        ) %>%
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
#' upon their levels with \code{\link[stringr]{str_extract}} and regexes.
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
#' interactive graph in the Viewer pane using \code{\link[ggiraph]{ggiraph}}.
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

    active_vars <- stringr::str_c(colnames(res.mca$call$X)[1:length(res.mca$call$quali)])
    excl <- names(res.mca$call$Xtot)[res.mca$call$excl]

    if (length(sup_vars)    != 0 )      sup_vars <- sup_vars %>%
      purrr::discard(. %in% active_vars)
    if (length(tooltip_vars_1lv) != 0 ) tooltip_vars_1lv <- tooltip_vars_1lv %>%
      purrr::discard(. %in% active_vars) #| . %in% sup_vars
    if (length(tooltip_vars) != 0 )     tooltip_vars <- tooltip_vars %>%
      purrr::discard(. %in% active_vars | . %in% tooltip_vars_1lv) #| . %in% sup_vars


    #if (names_darker == "auto") {      # if (type[1] == "points") names_darker <- TRUE
    # if (type[1] %in% c("active_vars_only", "labels", "text")) names_darker <- FALSE
    #}






    # Active variables --------------------------------------------------------------------
    active_var_levels <-
      purrr::map(active_vars, ~ dplyr::pull(res.mca$call$X, .) %>%
                   as.factor() %>% levels()) %>%
      purrr::set_names(active_vars) %>%
      purrr::imap_dfr(~ tibble::tibble(vars = .y, lvs = .x))


    freqs    <- tibble::enframe(res.mca$call$marge.col, "lvs", "freq")
    coords   <- tibble::as_tibble(res.mca$var$coord, rownames = "lvs")
    contribs <- tibble::as_tibble(res.mca$var$contrib, rownames = "lvs") %>%
      dplyr::rename_with(~ stringr::str_replace(., "^Dim ", "contrib"))

    active_vars_data <- active_var_levels %>%
      dplyr::left_join(freqs, by = "lvs") %>%
      dplyr::left_join(coords, by = "lvs") %>%
      dplyr::left_join(contribs, by = "lvs") %>%
      tidyr::nest(contribs = tidyselect::starts_with("contrib"))

    active_vars_data <- active_vars_data %>%
      dplyr::group_by(.data$vars) %>%
      dplyr::mutate(freq = round(.data$freq/sum(.data$freq) * 100, 0)) %>%
      dplyr::ungroup()

    dimensions <- names(active_vars_data)[stringr::str_detect(names(active_vars_data), "Dim ")] %>%
      purrr::set_names(.) %>%
      purrr::map_dfc(~ 0)

    active_vars_data <- active_vars_data %>%
      dplyr::filter(!is.na(.data$`Dim 1`)) %>%  #Remove excluded levels of active variables
      dplyr::mutate(lvs = stringr::str_remove(.data$lvs, stringr::str_c("^", .data$vars, "_")))

    if (cleannames == TRUE) active_vars_data <- active_vars_data %>%
      dplyr::mutate(lvs = forcats::fct_relabel(.data$lvs, ~ stringr::str_remove_all(., cleannames_condition())))

    active_vars_data <- active_vars_data %>%
      dplyr::mutate(color_group = factor("active_vars"),
                    id = as.integer(forcats::as_factor(.data$vars)) + 1000L)








    # Supplementary variables -------------------------------------------------------------
    if (length(sup_vars) != 0) {

      sup_vars_data <- purrr::map(sup_vars, ~ varsup(res.mca, dat[[.]]) ) %>%
        purrr::set_names(sup_vars)

      # Do something with "within" et "between" variance ? ($var)

      sup_vars_data <-
        purrr::imap(sup_vars_data,
                    ~ tibble::as_tibble(.x$coord, rownames = "lvs") %>%
                      dplyr::mutate(vars = .y) %>%
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

      sup_vars_data <- sup_vars_data %>%
        purrr::map2(color_groups,
                    ~ dplyr::mutate(.x, color_group = forcats::as_factor(stringr::str_c(
                      .data$vars, "_", stringr::str_extract(.data$lvs, .y)
                    ) %>%
                      stringr::str_remove("_$")
                    ))
        )


      if (length(keep_levels   ) >= 1L) sup_vars_data <- sup_vars_data %>%
        purrr::map(~ dplyr::filter(., stringr::str_detect(.data$lvs, keep_levels)
        ) )
      if (length(discard_levels) >= 1L) sup_vars_data <- sup_vars_data %>%
        purrr::map(
          ~ dplyr::filter(., !stringr::str_detect(.data$lvs,
                                                  stringr::str_c(discard_levels,
                                                                 collapse = "|"))
          )
        )

      if (cleannames) sup_vars_data <- sup_vars_data %>%
        purrr::map(~ dplyr::mutate(
          .,
          lvs = forcats::fct_relabel(.data$lvs, ~ stringr::str_remove_all(., cleannames_condition()))
        ))

      dimensions <- names(sup_vars_data[[1]]) %>%
        purrr::keep(stringr::str_detect(., "Dim ")) %>%
        purrr::set_names(.) %>%
        purrr::map_dfc(~ 0)


      #Make that, if HCPC is in sup_vars AND in profiles, ggiraph data_id are the same :
      # les deux seront colores lorsqu'on survolera l'un ou l'autre
      #sup_vars_data <- sup_vars_data %>% purrr::imap(~ dplyr::mutate(.x, sup_var = .y))
      if (length(cah) != 0) {
        if (cah %in% sup_vars) sup_vars_data <- sup_vars_data %>%
            purrr::map_if(names(.) == cah,
                          ~ dplyr::mutate(., cah_id = as.integer(.data$lvs) + 10000L),
                          .else = ~ dplyr::mutate(., cah_id = NA_integer_))
      }

      #Bind sup_vars data
      sup_vars_data <- sup_vars_data %>% dplyr::bind_rows()

      # ID numbers to use with ggiraph to highlight elements at hover
      if (length(cah) != 0) {
        if (cah %in% sup_vars) {
          sup_vars_data <- sup_vars_data %>%
            dplyr::mutate(id = dplyr::if_else(is.na(.data$cah_id),
                                              dplyr::row_number(),
                                              .data$cah_id))
        } else {
          sup_vars_data <- sup_vars_data %>% dplyr::mutate(id = dplyr::row_number())
        }
      } else {
        sup_vars_data <- sup_vars_data %>% dplyr::mutate(id = dplyr::row_number())
      }

      #Useful functions :
      #fct_relevel_quiet <- purrr::quietly(forcats::fct_relevel)
      # bind_cols_quiet   <- purrr::quietly(dplyr::bind_cols)
      #tab_spread <- function(data) dplyr::mutate_at(data, dplyr::vars(-1, -ncol(data)), ~. - dplyr::last(.))
      # tab_spread_chr <- function(data) {
      #   dplyr::mutate_at(data, dplyr::vars(-1, -tidyselect::any_of("Total")), ~ dplyr::case_when(
      #     dplyr::row_number() == nrow(data) ~ stringr::str_c(., "%"),
      #     . - dplyr::last(.) > 0         ~ stringr::str_c("(", stringr::str_pad(stringr::str_c("+" , sign(. - dplyr::last(.)) * (. - dplyr::last(.))), 3 + get_digits(.)), "%) ", stringr::str_pad(., 2 + get_digits(.)), "%"),
      #     TRUE                    ~ stringr::str_c("(", stringr::str_pad(stringr::str_c(" -", sign(. - dplyr::last(.)) * (. - dplyr::last(.))), 4 + get_digits(.)), "%) ", stringr::str_pad(., 2 + get_digits(.)), "%") )
      #   )
      # }

      vars_data <- dplyr::bind_rows(active_vars_data, sup_vars_data)

    } else {
      vars_data <- active_vars_data
      #sup_vars_data <- NULL
    }

    #Add central point
    vars_data <- vars_data %>%
      dplyr::add_row(vars        = "All",
                     lvs         = factor("Central point"),
                     color_group = factor("Central point")) %>%
      dplyr::mutate(dplyr::across(
        tidyselect::starts_with("Dim "),
        ~ dplyr::if_else(.data$lvs == "Central point", 0, .)
      ))

    #Reorder variables in vars_data
    vars_data <- vars_data %>%
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

    dat <- dat %>%
      dplyr::mutate(dplyr::across(where(is.character), as.factor)) %>%
      dplyr::mutate(dplyr::across(where(is.factor), forcats::fct_drop)) %>%
      tibble::add_column(row.w = res.mca$call$row.w)

    #Remove excluded levels (now, or after by renaming them here)
    excl_levels <-
      purrr::imap_dfr(dat[active_vars],
                      ~ tibble::tibble(active_vars = .y, lvs = levels(.x))
                      ) |>
      #dplyr::mutate(lvs2 = stringr::str_c(.data$active_vars, "_",.data$ lvs)) |>
      dplyr::filter(.data$lvs %in% excl) #| .data$lvs2 %in% excl

    excl_levels <- excl_levels |>
      dplyr::group_by(active_vars) |>
      dplyr::summarise(excl = list(.data$lvs), .groups = "drop")

    excl_levels <- purrr::set_names(excl_levels$excl, excl_levels$active_vars)

    active_var_real_levels <-
      purrr::imap(dat[active_vars], ~ tibble::tibble(active_vars = .y, lvs = levels(.x)))

    active_vars_excl <- active_var_real_levels %>%
      purrr::map(~ dplyr::filter(., .data$lvs %in% excl) %>% dplyr::pull(.data$lvs))
    active_vars_excl <- active_vars_excl[purrr::map_lgl(active_vars_excl, ~ length(.) != 0)]

    dat <- dat %>%
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(names(active_vars_excl)),
        ~ forcats::fct_relevel(., active_vars_excl[[dplyr::cur_column()]], after = Inf) %>%
          forcats::fct_recode(rlang::splice(purrr::set_names(active_vars_excl[[dplyr::cur_column()]],
                                                             "Remove_levels")))
      ))

    #When MCA() added variable name at the beginning of levels names, remove it
    dat <- dat %>%
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(active_vars),
        ~ forcats::fct_relabel(., ~ stringr::str_remove(., paste0("^", dplyr::cur_column(), "_")))
      ))

    if (cleannames == TRUE) dat <- dat %>%
      dplyr::mutate(dplyr::across(
        where(~is.factor(.) | is.character(.)),
        ~ forcats::fct_relabel(., ~stringr::str_remove_all(., cleannames_condition()))
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

      vars_data <- vars_data %>% dplyr::left_join(interactive_text, by = c("vars", "lvs"))

    } else {
      text_vars <- "begin_text"
      vars_data <- vars_data %>% dplyr::mutate(begin_text = NA_character_)
    }

    if (length(active_vars_without_crosstables) != 0) {
      vars_data <- vars_data %>%
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
      mean_point_interactive_text <- vars_data %>%
        dplyr::ungroup() |>
        dplyr::filter(.data$color_group == "active_vars") %>%
        dplyr::mutate(
          text = stringr::str_c("\n", .data$lvs, " : ", .data$freq,"%")
        ) %>%
        dplyr::summarise(
          text = stringr::str_c(.data$text, collapse = "")
        ) %>%
        dplyr::pull(.data$text)

      mean_point_interactive_text <-
        stringr::str_c("<b>Central point</b>",
                       "\nFrequency: 100%",
                       "\n\n<b>Active variables :</b>",
                       mean_point_interactive_text)

      vars_data <- vars_data %>%
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
      #     ~ forcats::fct_relabel(~ stringr::str_remove_all(., cleannames_condition()))
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

        ind_data <- ind_data %>%
          dplyr::mutate(!!rlang::sym(cah) := as.character(!!rlang::sym(cah) ) ) |>
          tidyr::nest(sup_vars = tidyselect::all_of(sup_vars),
                      row.w    = "row.w",
                      coord    = tidyselect::all_of(coord_names),
                      cah      = !!rlang::sym(cah)
          ) %>%
          dplyr::mutate(
            count  = purrr::map_int(.data$row.w, ~ nrow(.)),

            wcount = purrr::map_dbl(.data$row.w, ~ sum(., na.rm = TRUE)),

            cah    = purrr::map_chr(.data$cah, ~ dplyr::first(dplyr::pull(., 1))) |>
              as.factor() |> forcats::fct_relevel(cah_levels)
          ) %>%
          dplyr::arrange(-.data$wcount)
        # 0.661149 secs (much longer in data.table here)



        if (length(max_profiles) != 0) ind_data <- ind_data %>% dplyr::slice(1:max_profiles)

        ind_data <- ind_data %>%
          dplyr::mutate(nb = dplyr::row_number(),
                        cah_id = as.integer(.data$cah)) %>%
          dplyr::group_by(.data$cah) %>%
          dplyr::mutate(nb_in_cah = dplyr::row_number(),
                        nb_tot_cah = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(coord = purrr::map(.data$coord, ~ .[1,])) %>%
          tidyr::unnest("coord")


      } else {
        ind_data <- ind_data %>%
          tidyr::nest(sup_vars = tidyselect::all_of(sup_vars),
                      row.w    = .data$row.w,
                      coord    = tidyselect::all_of(coord_names)

          ) %>%
          dplyr::mutate(count  = purrr::map_int(.data$row.w, ~ nrow(.)),
                        wcount = purrr::map_dbl(.data$row.w, ~ sum(., na.rm = TRUE))
          ) %>%
          dplyr::arrange(-.data$wcount)

        if (length(max_profiles) != 0) ind_data <- ind_data %>% dplyr::slice(1:max_profiles)

        ind_data <- ind_data %>%
          dplyr::mutate(nb    = dplyr::row_number(),
                        coord = purrr::map(.data$coord, ~ .[1,])) %>%
          tidyr::unnest(.data$coord)
      }

      ind_data <- ind_data %>%
        dplyr::mutate(dplyr::across(tidyselect::all_of(active_vars),
                                    ~ fct_detect_replace(., profiles_tooltip_discard, "#"))) %>%
        dplyr::mutate(dplyr::across(where(is.factor), as.character))


      if (length(cah) != 0) {
        ind_data <- ind_data %>%
          dplyr::mutate(
            cah_base = .data$cah,
            count_base = .data$count,
            wcount_base = .data$wcount,
            cah        = stringr::str_c("<b>Cah: ", .data$cah, "</b>"),
            profile_nb = stringr::str_c("<b>Answer profile n",
                                        stringi::stri_unescape_unicode("\\u00b0"),
                                        .data$nb_in_cah, "/", .data$nb_tot_cah, "</b>"),
            count      = stringr::str_c("n: ", format(round(.data$count, 0),
                                                      trim = TRUE, big.mark = " ")),
            wcount     = dplyr::if_else(
              condition = .data$count == .data$wcount,
              true      = "",
              false     = stringr::str_c("weighted n: ",
                                         format(round(.data$wcount, 0),
                                                trim = TRUE, big.mark = " "), "\n")
            )

          ) %>%
          tidyr::nest(interactive_text = tidyselect::all_of(c("cah", "profile_nb", "count", "wcount",
                                                              active_vars))) %>%
          dplyr::rename("cah" = "cah_base", "count" = "count_base",
                        "wcount" = "wcount_base")

      } else {
        ind_data <- ind_data %>%
          dplyr::mutate(
            count_base  = .data$count,
            wcount_base = .data$wcount,
            profile_nb  = stringr::str_c("<b>Answer profile n",
                                         stringi::stri_unescape_unicode("\\u00b0"),
                                         nb = .data$nb,  "</b>"),
            count       = stringr::str_c("n: ", format(round(.data$count, 0),
                                                       trim = TRUE, big.mark = " ")),
            wcount      = dplyr::if_else(
              condition = .data$count == .data$wcount,
              true      = "",
              false     = stringr::str_c("weighted n: ",
                                         format(round(.data$wcount, 0),
                                                trim = TRUE, big.mark = " "), "\n")
            )
          ) %>%
          tidyr::nest(interactive_text = c("profile_nb", "count", "wcount",
                                           tidyselect::all_of(active_vars))) %>%
          dplyr::rename("count" = "count_base", "wcount" = "wcount_base")
      }

      ind_data <- ind_data %>%
        dplyr::select(-tidyselect::any_of(c("nb_in_cah", "nb_tot_cah"))) %>%
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






#' @describeIn ggmca print MCA graph from data frames with parameters
# @inheritParams ggmca
#' @param data A list of data frames made with \link{ggmca_data}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @export
ggmca_plot <- function(data,
                       axes = c(1,2), axes_names = NULL, axes_reverse = NULL,
                       type = c("text", "points", "labels", "active_vars_only", "numbers", "facets"),
                       text_repel = FALSE, title, ellipses = NULL,
                       actives_in_bold = NULL, sup_in_italic = FALSE,
                       xlim, ylim, out_lims_move = FALSE,
                       color_profiles = TRUE, base_profiles_color = "#aaaaaa",
                       alpha_profiles = 0.7,
                       shift_colors = 0, colornames_recode,
                       scale_color_light = material_colors_light(),
                       scale_color_dark  = material_colors_dark(),
                       text_size = 3.5, size_scale_max = 4, dist_labels = c("auto", 0.04),
                       right_margin = 0, use_theme = TRUE, get_data = FALSE) {

  vars_data        <- data$vars_data
  ind_data         <- data$ind_data
  #active_vars_data <- data$active_vars_data
  #sup_vars_data    <- data$sup_vars_data
  #mean_point_data  <- data$mean_point_data
  cah              <- data$cah
  sup_vars         <- vars_data %>%
    dplyr::filter(!.data$color_group %in% c("active_vars", "Central point")) |>
    dplyr::pull(.data$vars) |> unique()
  res.mca          <- data$res.mca

  if (!is.null(axes_names)) res.mca$axes_names <- axes_names
  if (!is.null(ellipses)) stopifnot(ellipses > 0 & ellipses <= 1)

  dim1 <- rlang::sym(stringr::str_c("Dim ", axes[1]))
  dim2 <- rlang::sym(stringr::str_c("Dim ", axes[2]))

  contrib1 <- rlang::sym(stringr::str_c("contrib", axes[1]))
  contrib2 <- rlang::sym(stringr::str_c("contrib", axes[2]))

  # if (length(color_profiles) == 0) {
  #   if (length(cah) != 0) {
  #     color_profiles <- levels(as.factor(dplyr::pull(ind_data, cah)))
  #   } else {
  #     color_profiles <- character()
  #   }
  #
  # } else {
  if (length(cah) > 0 & !is.null(ind_data)) {
    if (is.logical(color_profiles)) if (! color_profiles) {
      color_profiles <- character()
    } else {
      color_profiles <- levels(as.factor(dplyr::pull(ind_data, cah)))
    }
    #}
  }

  if (missing(colornames_recode)) colornames_recode <- character()

  if (length(actives_in_bold) == 0) actives_in_bold <- length(sup_vars) == 0

  if (!length(axes_reverse) == 0) {
    if (!axes_reverse %in% 1:2) stop("axes_reverse must be 1, 2 or 1:2")

    dims_reverse <- unique(c(rlang::as_name(dim1), rlang::as_name(dim2))[axes_reverse])

    reverse_axe <- function(coord) {
      coord %>% dplyr::mutate(dplyr::across(tidyselect::all_of(dims_reverse), ~ - .))
    }

    vars_data <- reverse_axe(vars_data)
    if (!is.null(ind_data)) ind_data <- reverse_axe(ind_data)
  }



  #Add contribs in tooltips for active_vars ----
  vars_data <- vars_data %>%
    dplyr::mutate(
      contribs = purrr::map_if(
        .data$contribs, !purrr::map_lgl(.data$contribs, is.null),
        ~ dplyr::mutate(., text = stringr::str_c(
          "\nContrib axe ", axes[1], " : ", stringr::str_pad(round(!!contrib1, 0), 2), "%",
          "\nContrib axe ", axes[2], " : ", stringr::str_pad(round(!!contrib2, 0), 2), "%"
        )) %>%
          dplyr::pull("text"),

        .else = ~ ""
      ) %>%
        purrr::flatten_chr(),

      interactive_text = purrr::map2(
        .data$interactive_text, .data$contribs,
        ~ dplyr::mutate(.x, begin_text = stringr::str_c(.data$begin_text, .y))
      )
    ) %>%
    dplyr::select(-"contribs") # ??????????????????



  # Collapse the interactive tooltips dataframes
  vars_data <-  vars_data |>
    dplyr::mutate(interactive_text = dplyr::bind_rows(.data$interactive_text) |>
                    tidyr::unite("interactive_text", sep = "\n", na.rm = TRUE) |>
                    dplyr::pull("interactive_text"),
    )
  # vars_data <- vars_data %>%
  #   dplyr::mutate(interactive_text = purrr::map_chr(
  #     .data$interactive_text,
  #     ~ tibble::deframe(tidyr::unite(., "interactive_text", sep = "\n", na.rm = TRUE))
  #   ))

  #Add linebreak at end if text finish by html </font>, otherwise no line breaks
  vars_data <- vars_data %>%
    dplyr::mutate(interactive_text = stringr::str_replace(.data$interactive_text,
                                                          "</font>$", "</font>\\u202f") %>%
                    stringi::stri_unescape_unicode())

  # Set colors :
  if (type[1] == "facets" | !is.null(ellipses)) {
    vars_data <- vars_data %>%
      dplyr::mutate(color_group = forcats::as_factor(dplyr::if_else(
        condition = .data$vars == sup_vars[1],
        true      = paste0(.data$color_group, "_", .data$lvs), #forcats::fct_expand(paste0(.data$color_group, "_", .data$lvs) %>% as.factor(),
        #                    levels(.data$color_group)),
        false     = as.character(.data$color_group)
      )))
  }

  if (length(colornames_recode) > 0) vars_data <- vars_data %>%
    dplyr::mutate(color_group = forcats::fct_recode(.data$color_group,
                                                    !!!colornames_recode))
  if (shift_colors != 0) vars_data <- vars_data %>%
    dplyr::mutate(color_group = forcats::fct_shift(.data$color_group, shift_colors))
  colorvar_recode <- levels(vars_data$color_group)
  colorvar_recode <- colorvar_recode[!colorvar_recode %in% c("active_vars", "Central point")]
  if (length(colorvar_recode) >= 2) {
    message(stringr::str_c("colors based on the following categories (rename with colornames_recode): '",
                           stringr::str_c(colorvar_recode, collapse = "', '"), "'",
                           collapse = ""))
  }

  if (length(scale_color_light) == 1 ) {
    scale_color_light <- vctrs::vec_recycle(scale_color_light, length(colorvar_recode))
  }
  if (length(scale_color_dark) == 1 ) {
    scale_color_dark <- vctrs::vec_recycle(scale_color_dark, length(colorvar_recode))
  }


  scale_color_points <- scale_color_light %>%
    purrr::set_names(colorvar_recode[1:length(scale_color_light)])

  scale_color_names <- scale_color_dark %>%
    purrr::set_names(stringr::str_c("names_", colorvar_recode[1:length(scale_color_dark)]))

  if(length(scale_color_light) > length(scale_color_dark)) {
    scale_color_light <- scale_color_light[1:length(scale_color_dark)]
  } else if (length(scale_color_light) < length(scale_color_dark)) {
    scale_color_dark <- scale_color_dark[1:length(scale_color_light)]
  }

  if (length(colorvar_recode[-(1:length(scale_color_light))]) > 0) {
    levels_in_more <- colorvar_recode[-(1:length(scale_color_light))]
    scale_color_points <- scale_color_points %>%
      append(rep(.[length(.)], length(levels_in_more)) %>%
               purrr::set_names(levels_in_more))
    scale_color_names <- scale_color_names %>%
      append(rep(.[length(.)], length(levels_in_more)) %>%
               purrr::set_names(levels_in_more))
    warning(stringr::str_c("too much colors, all the last ones were set to last color. Max ", length(scale_color_light)))
  }

  if (is.null(base_profiles_color)) base_profiles_color <- "#ffffff"

  scale_color_named_vector <- c(scale_color_points, scale_color_names)
  scale_color_named_vector <- scale_color_named_vector[!is.na(names(scale_color_named_vector))]
  scale_color_named_vector <- c(scale_color_named_vector,
                                "base_profiles_color" = base_profiles_color,
                                "active_vars"         = "black",
                                "Central point"       = "black"
  )

  if (type[1] %in% c("points", "numbers"))  vars_data <- vars_data %>%
    dplyr::mutate(colorvar_names = as.factor(stringr::str_c("names_", .data$color_group)))
  #} else { sup_vars_data <- sup_vars_data %>% dplyr::mutate(colorvar_names =  color_group) }






  #Calculate limits of graph (arguments to be passed in ggi() to set htmlwidget size)
  min_max_lims <- dplyr::select(vars_data, !!dim1, !!dim2)

  if (!missing(xlim)) min_max_lims <- min_max_lims %>%
    tibble::add_row(!!dim1 := xlim[1]) %>% tibble::add_row(!!dim1 := xlim[2])
  if (!missing(ylim)) min_max_lims <- min_max_lims %>%
    tibble::add_row(!!dim2 := ylim[1]) %>% tibble::add_row(!!dim2 := ylim[2])
  heigth_width_ratio <- min_max_lims %>%
    dplyr::summarise_all(~ max(., na.rm = TRUE) - min(., na.rm = TRUE), .groups = "drop")
  min_max_lims <-
    dplyr::bind_rows(dplyr::summarise_all(min_max_lims,
                                          ~ min(., na.rm = TRUE),
                                          .groups = "drop"),
                     dplyr::summarise_all(min_max_lims,
                                          ~ max(., na.rm = TRUE),
                                          .groups = "drop"))
  width_range <- dplyr::pull(heigth_width_ratio, 1)[1]
  heigth_width_ratio <- heigth_width_ratio %>%
    dplyr::summarise(heigth_width_ratio = !!dim2/!!dim1, .groups = "drop") %>%
    tibble::deframe()

  if (dist_labels[1] == "auto") dist_labels <- width_range/40

  theme_acm_with_lims <-
    if (use_theme) {
      if (!missing(xlim) & !missing(ylim))  {
        theme_facto(res = res.mca, axes = axes, no_color_scale = TRUE,
                    size_scale_max = size_scale_max,  # legend.position = "bottom",
                    xlim = c(xlim[1], xlim[2]), ylim = c(ylim[1], ylim[2]))
      } else if (!missing(xlim) ) {
        theme_facto(res = res.mca, axes = axes, no_color_scale = TRUE,
                    size_scale_max = size_scale_max,  # legend.position = "bottom",
                    xlim = c(xlim[1], xlim[2]) )
      } else if (!missing(ylim) )  {
        theme_facto(res = res.mca, axes = axes, no_color_scale = TRUE,
                    size_scale_max = size_scale_max,  # legend.position = "bottom",
                    ylim = c(ylim[1], ylim[2]))
      } else {
        theme_facto(res = res.mca, axes = axes, no_color_scale = TRUE,
                    size_scale_max = size_scale_max)
      } # legend.position = "bottom",

    } else {
      NULL
    }

  outlims <- function(data, lim, dim) {
    dim <- rlang::enquo(dim)
    if (!is.na(lim[1])) data <- data %>% dplyr::filter(!!dim > lim[1])
    if (!is.na(lim[2])) data <- data %>% dplyr::filter(!!dim < lim[2])
    return(data)
  }

  if (text_repel == FALSE | out_lims_move == FALSE) {
    if (!missing(xlim)) vars_data <- vars_data %>% outlims(xlim, !!dim1)
    if (!missing(ylim)) vars_data <- vars_data %>% outlims(ylim, !!dim2)
  }





  #Profiles :
  if (!is.null(ind_data)) {
    ind_data <-  ind_data |>
      dplyr::mutate(interactive_text = dplyr::bind_rows(.data$interactive_text) |>
                      tidyr::unite("interactive_text", sep = "\n", na.rm = TRUE) |>
                      dplyr::pull("interactive_text") |>
                      stringr::str_remove_all("\n#"),
      )

    # ind_data <- ind_data %>%
    #   dplyr::mutate(interactive_text = purrr::map_chr(
    #     .data$interactive_text,
    #     ~ tibble::deframe(tidyr::unite(., "interactive_text", sep = "\n", na.rm = TRUE))
    #   ) %>%
    #     stringr::str_remove_all("\n#")
    #   )

    if (length(cah) != 0) { #& type[1] != "facets"

      if (length(color_profiles) == 0 ) {
        if (!is.null(base_profiles_color) ) {

          #Discard the points that are out of limits
          profiles_coord <- ind_data
          if (!missing(xlim)) profiles_coord <- profiles_coord %>% outlims(xlim, !!dim1)
          if (!missing(ylim)) profiles_coord <- profiles_coord %>% outlims(ylim, !!dim2)

          profiles <- ggiraph::geom_point_interactive(
            data = profiles_coord,
            ggplot2::aes(x = !!dim1, y = !!dim2, size = .data$wcount,
                         tooltip = .data$interactive_text, data_id = .data$cah_id + 10000),
            color = base_profiles_color, na.rm = TRUE, inherit.aes = FALSE,
            show.legend = FALSE, alpha = alpha_profiles
          )
        } else {
          profiles <- NULL
        }


      } else {
        ind_cah_levels <- ind_data %>% dplyr::pull(cah) %>% unique() %>%
          purrr::discard(is.na(.)) %>% purrr::discard(. == "NA")

        not_in_color_profiles <- ind_cah_levels %>%
          purrr::discard(. %in% color_profiles) %>%
          purrr::set_names(rep("base_profiles_color", length(.) ))

        if (cah %in% sup_vars) {
          sup_cah_colorvar <- vars_data %>%
            dplyr::select("lvs", "color_group") %>%
            dplyr::filter(stringr::str_detect(.data$color_group, paste0("^", cah))) %>%
            dplyr::mutate(color_group = .data$lvs %>% purrr::set_names(.data$color_group)) %>%
            dplyr::pull("color_group") |>
            forcats::fct_drop()

          sup_cah_colorvar <- purrr::set_names(as.character(sup_cah_colorvar),
                                               names(sup_cah_colorvar))

          color_profiles_in_colorvar <- sup_cah_colorvar %>%
            purrr::keep(. %in% ind_cah_levels) %>%
            purrr::keep(. %in% color_profiles)

          color_profiles_not_in_colorvar <- color_profiles %>%
            purrr::keep(. %in% ind_cah_levels) %>%
            purrr::discard(. %in% sup_cah_colorvar)

        } else {
          color_profiles_in_colorvar <- character()
          color_profiles_not_in_colorvar <-  color_profiles %>%
            purrr::keep(. %in% ind_cah_levels)
        }


        if (length(color_profiles_not_in_colorvar) != 0) {
          named_color_profiles <- color_profiles_not_in_colorvar %>%
            purrr::keep(!is.null(names(.)))

          if (length(named_color_profiles) != 0 ) {
            new_colors_in_scale <- names(named_color_profiles) %>%
              purrr::set_names(named_color_profiles)

            named_color_profiles <- named_color_profiles %>%
              purrr::set_names(., .)

            scale_color_named_vector <- scale_color_named_vector %>%
              append(new_colors_in_scale)
          }


          unnamed_color_profiles <- color_profiles_not_in_colorvar %>%
            purrr::keep(is.null(names(.)))

          if (length(unnamed_color_profiles) > 0) {
            remaining_colors <- material_colors_light() %>%
              purrr::discard(. %in% scale_color_named_vector)

            unnamed_color_profiles <- unnamed_color_profiles %>%
              purrr::set_names(., .)

            scale_color_named_vector <- scale_color_named_vector %>%
              append(purrr::set_names(remaining_colors[1:length(unnamed_color_profiles)], unnamed_color_profiles))

            if  (length(remaining_colors) < length(unnamed_color_profiles)) {
              stop("Not enough colors in scale to color profiles.")
            }
          }
        } else {
          named_color_profiles   <- character()
          unnamed_color_profiles <- character()
        }

        cah_colorvar_recode <- named_color_profiles %>%
          append(unnamed_color_profiles) %>%
          append(not_in_color_profiles) %>%
          append(color_profiles_in_colorvar)


        ind_data <- ind_data %>%
          dplyr::mutate(color_group = forcats::fct_recode(.data$cah,
                                                          !!!cah_colorvar_recode))
        # ind_data |> dplyr::select(color_group) |> print(n = 40)

        #Discard the points that are out of limits
        profiles_coord <- ind_data
        if (!missing(xlim)) profiles_coord <- profiles_coord %>% outlims(xlim, !!dim1)
        if (!missing(ylim)) profiles_coord <- profiles_coord %>% outlims(ylim, !!dim2)

        profiles <- ggiraph::geom_point_interactive(
          data = profiles_coord,
          ggplot2::aes(x = !!dim1, y = !!dim2, size = .data$wcount,
                       tooltip = .data$interactive_text,
                       data_id = .data$cah_id + 10000, color = .data$color_group),
          na.rm = TRUE, inherit.aes = FALSE, show.legend = FALSE,
          alpha = alpha_profiles, stroke = 0
        )
      }

    } else { # If length(cah) == 0
      if (!is.null(base_profiles_color) ) {

        #Discard the points that are out of limits
        profiles_coord <- ind_data
        if (!missing(xlim)) profiles_coord <- profiles_coord %>% outlims(xlim, !!dim1)
        if (!missing(ylim)) profiles_coord <- profiles_coord %>% outlims(ylim, !!dim2)

        profiles <-
          ggiraph::geom_point_interactive(
            data = profiles_coord,
            ggplot2::aes(x = !!dim1, y = !!dim2, size = .data$wcount,
                         tooltip = .data$interactive_text,
                         data_id = .data$nb + 10000),
            color = base_profiles_color, na.rm = TRUE, inherit.aes = FALSE,
            show.legend = FALSE, alpha = alpha_profiles
          )
      } else {
        profiles <- NULL
      }
    }


    if(type[1] == "facets" | !is.null(ellipses) ) {

      ind_data <- ind_data %>%
        dplyr::mutate(sup_vars = purrr::map(.data$sup_vars,
                                            ~ dplyr::select(., !!rlang::sym(sup_vars[1])))) %>%
        tidyr::unnest(c(.data$sup_vars, .data$row.w))

      supvar1_lvs <-
        dplyr::filter(vars_data, .data$vars == sup_vars[1]) %>%
        dplyr::pull(.data$lvs) %>% as.character() %>% purrr::set_names(.)

      supvar1_colorvar <- dplyr::filter(vars_data, .data$vars == sup_vars[1]) %>%
        dplyr::select(.data$lvs, .data$color_group)
      supvar1_colorvar <- as.character(supvar1_colorvar$color_group) %>% purrr::set_names(supvar1_colorvar$lvs)

      supvar1_infos <- dplyr::filter(vars_data, .data$vars == sup_vars[1]) %>%
        dplyr::mutate(nam = .data$lvs) %>%
        dplyr::select(.data$nam, .data$lvs, .data$color_group, .data$id) %>%
        tidyr::nest(infos = c(.data$lvs, .data$color_group, .data$id))
      supvar1_infos <- supvar1_infos$infos %>% purrr::set_names(supvar1_infos$nam)

      if (!is.null(ellipses)) {
        ellipses_coord <- ind_data %>%
          dplyr::select(!!dim1, !!dim2, .data$row.w, tidyselect::all_of(sup_vars[1]), tidyselect::any_of("lvs")) %>%
          dplyr::mutate(infos = supvar1_infos[as.character(!!rlang::sym(sup_vars[1]))],
          ) %>%
          tidyr::unnest(cols = c(.data$infos)) %>%
          dplyr::filter(!is.na(.data$lvs))


        ellipses <-
          if (type[1] == "facets") {
            ggiraph::geom_path_interactive(data = ellipses_coord,
                                           ggplot2::aes(x = !!dim1, y = !!dim2,
                                                        group = .data$lvs, data_id = .data$id),
                                           color = "black",
                                           stat = "ellipse",
                                           type = "t", level = ellipses, size = 1,
                                           segments = 360, alpha = 1, inherit.aes = FALSE)
          } else {
            ggplot2::geom_path(data = ellipses_coord,
                               ggplot2::aes(x = !!dim1, y = !!dim2,
                                            group = .data$lvs,
                                            color = .data$color_group),
                               stat = "ellipse",
                               type = "t", level = ellipses, size = 1,
                               segments = 360, alpha = 1, inherit.aes = FALSE)
          }

        # ggplot2::stat_ellipse(data = ind_data,
        #                       ggplot2::aes(x = !!dim1, y = !!dim2,
        #                                    group = !!rlang::sym(sup_vars[1]),
        #                                    color = !!rlang::sym(sup_vars[1]) ),
        #                       type = "t", level = ellipses, size = 1,
        #                       segments = 360, alpha = 1)

      } else {
        ellipses <- NULL
      }

      if(type[1] == "facets") {
        ind_data <- ind_data %>%
          tidyr::nest(row.w = .data$row.w) %>%
          dplyr::mutate(count  = purrr::map_int(.data$row.w, ~ nrow(.)),
                        wcount = purrr::map_dbl(.data$row.w, ~ sum(., na.rm = TRUE))
          ) %>%
          #dplyr::select(-.data$row.w) %>%
          dplyr::arrange(!!rlang::sym(sup_vars[1]), -.data$wcount) %>%
          dplyr::mutate(lvs = purrr::map(!!rlang::sym(sup_vars[1]),
                                         ~ supvar1_lvs[as.character(.)]
          ) %>% unlist(),

          color_group = purrr::map(!!rlang::sym(sup_vars[1]),
                                   ~ supvar1_colorvar[as.character(.)]
          ) %>% unlist()
          ) %>%
          dplyr::filter(!is.na(.data$lvs))
      }
    }


  } else {
    profiles <- NULL
    ellipses <- NULL
  }





  #Draw plot  -----------------------------------------------------

  # If type is text, put the active_vars on the same base than suplementary vars, to avoid overlapping of the two.
  #if (type[1] == "text" & length(sup_vars) != 0) {
  vars_data <- vars_data %>%
    dplyr::mutate(
      face = dplyr::case_when(
        color_group == "active_vars" & actives_in_bold ~ "bold" ,
        color_group == "active_vars"                   ~ "plain",
        sup_in_italic & actives_in_bold                ~ "italic" ,
        sup_in_italic                                  ~ "bold.italic" ,
        actives_in_bold                                ~ "plain",
        TRUE                                           ~ "bold" ,
      ))
  #}


  #Mean point:
  mean_point_data  <- dplyr::filter(vars_data, .data$lvs == "Central point")
  mean_point_graph <-
    ggiraph::geom_point_interactive(
      data = mean_point_data,
      ggplot2::aes(x = !!dim1, y = !!dim2, tooltip = .data$interactive_text),
      color = "black", fill = "#eeeeee",
      shape = 3, size = 5, stroke = 1.5,
      na.rm = TRUE, inherit.aes = FALSE
    )
  vars_data <- vars_data %>% dplyr::filter(.data$lvs != "Central point")

  #Theme
  if (!missing(title)) {
    title_graph <- ggplot2::labs(title = title) #stringr::str_c("Les Active variables de l'ACM sur les axes ",axes[1], " et ", axes[2] )
  } else {
    title_graph <- NULL
  }

  graph_theme_acm <-
    list(theme_acm_with_lims,
         ggplot2::scale_colour_manual(values = scale_color_named_vector,
                                      aesthetics = c("colour", "fill")),
         ggplot2::theme(plot.margin = ggplot2::margin(r = right_margin,
                                                      unit = "cm")),
         title_graph)


  #Separate graph for active_vars with type != "text"
  if (type[1] %in% c("points", "labels", "numbers")) {
    active_graph <-
      if (text_repel == FALSE) {
        ggiraph::geom_text_interactive(
          data = dplyr::filter(vars_data, .data$color_group == "active_vars"),
          ggplot2::aes(x = !!dim1, y = !!dim2, label = .data$lvs, fontface = .data$face,
                       tooltip = .data$interactive_text, data_id = .data$id),
          color = "black",
          size = text_size, na.rm = TRUE, inherit.aes = FALSE
        )
      } else {
        ggiraph::geom_text_repel_interactive(
          data = dplyr::filter(vars_data, .data$color_group == "active_vars"),
          ggplot2::aes(x = !!dim1, y = !!dim2, label = .data$lvs, fontface = .data$face,
                       tooltip = .data$interactive_text, data_id = .data$id),
          color = "black", alpha = dplyr::if_else(type[1] == "points", 0.8, 1),
          size = text_size,
          direction = "both", force = 0.5, force_pull = 1, point.padding = 0, box.padding = 0, point.size = NA,
          arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines")),
          min.segment.length = 0.01, #0.4,
          na.rm = TRUE, inherit.aes = FALSE
        ) #, box.padding = 0
      }
  }


  if (get_data) return(
    list(vars_data = vars_data, mean_point_data = mean_point_data,
         profiles_coord = if (length(profiles) != 0) {profiles_coord} else {NULL},
         ellipses_coord = if (length(ellipses) != 0) {ellipses_coord} else {NULL},
         graph_theme_acm = graph_theme_acm)
  )



  #The final plots
  if (type[1] == "text") {

    if (length(cah) > 0 ) {
      cah_data  <- vars_data |> dplyr::filter(.data$vars == cah)
      vars_data <- vars_data |> dplyr::filter(.data$vars != cah)
      if (text_repel == FALSE) {
        graph_cah <-
          ggiraph::geom_label_interactive(
            data = cah_data,
            ggplot2::aes(label = .data$lvs, color = .data$color_group,
                         tooltip = .data$interactive_text),
            fill = grDevices::rgb(1, 1, 1, alpha = 0.9),
            fontface = "bold", size = text_size, na.rm = TRUE
          )

      } else {
        graph_cah <-
          # list(
          # geom_segment(
          #   data = acm_cah |>
          #     mutate(!!dim1 = pmin(1.3, pmax(!!dim1, -0.9)),
          #            !!dim2 = pmin(1.3, pmax(!!dim2, -0.85)),
          #            start1  = pmin(0.95, pmax(!!dim1, -0.5)),
          #            start2  = pmin(1.25, pmax(!!dim2, -0.775)),
          #     ),
          #   ggplot2::aes(x = start1, xend = !!dim1, y = start2, yend = !!dim2,
          #                color = color_group),
          #   arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "lines")), na.rm = TRUE
        # ),
        ggiraph::geom_label_repel_interactive(
          data = cah_data,
          ggplot2::aes(label = .data$lvs, color = .data$color_group,
                       tooltip = .data$interactive_text),
          fill = grDevices::rgb(1, 1, 1, alpha = 0.9),
          direction = "both", force = 0.5, force_pull = 1, point.padding = 0, point.size = NA,
          arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines")),
          fontface = "bold", size = text_size, na.rm = TRUE #,
          #box.padding = 0,
        )
        #)
      }
    } else {
      graph_cah <- NULL
    }

    if (text_repel == FALSE) {
      graph_text <-
        ggiraph::geom_text_interactive(
          ggplot2::aes(fontface = .data$face, tooltip = .data$interactive_text),
          size = text_size, na.rm = TRUE
        )
    } else {
      graph_text <-
        ggiraph::geom_text_repel_interactive(
          ggplot2::aes(fontface = .data$face, tooltip = .data$interactive_text),
          size = text_size, na.rm = TRUE, #fontface = "bold"
          direction = "both", # segment.alpha = 0.5,
          min.segment.length = 0.01, #0.4,
          force = 0.5, force_pull = 1, point.padding = 0,  box.padding = 0, point.size = NA,
          arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines"))
        ) # point.padding = 0.25, segment.colour = "black",
    }


    plot_output <-
      ggplot2::ggplot(vars_data,
                      ggplot2::aes(x = !!dim1, y = !!dim2, label = .data$lvs,
                                   color = .data$color_group, data_id = .data$id)) +
      graph_theme_acm + profiles + ellipses + graph_text + graph_cah +
      mean_point_graph




  } else if (type[1] == "points") {
    #If active vars too, points in gray

    plot_output <-
      ggplot2::ggplot(dplyr::filter(vars_data, .data$color_group != "active_vars"),
                      ggplot2::aes(x = !!dim1, y = !!dim2, label = .data$lvs,
                                   color = .data$color_group, data_id = .data$id)) +
      graph_theme_acm + profiles + active_graph + ellipses +
      ggiraph::geom_text_repel_interactive(
        ggplot2::aes(color = .data$colorvar_names, tooltip = .data$interactive_text),
        size = text_size, hjust = "left",  segment.alpha = 0.2, #segment.colour = "black",
        direction = "both", nudge_x = dist_labels[1], point.padding = 0.25,
        na.rm = TRUE, fontface = "plain"
      ) + # ifelse(names_darker == TRUE, "plain", "bold")
      ggiraph::geom_point_interactive(
        ggplot2::aes(size = .data$wcount, fill = .data$color_group,
                     tooltip = .data$interactive_text),
        shape = 18, na.rm = TRUE
      ) +
      mean_point_graph

    # css_hover <- ggiraph::girafe_css("fill:gold;stroke:orange;",
    #                                  text = "color:gold4;stroke:none;")
    # plot_output <- plot_output %>% append(c("css_hover" = css_hover)) #retrieves class ggplot2::ggplot after



  } else if (type[1] == "labels") {
    if (text_repel == FALSE) {
      graph_labels <-
        ggiraph::geom_label_interactive(
          ggplot2::aes(fontface = .data$face, tooltip = .data$interactive_text),
          size = text_size, fontface = "bold", na.rm = TRUE
        )
    } else {
      graph_labels <-
        ggiraph::geom_label_repel_interactive(
          ggplot2::aes(fontface = .data$face, tooltip = .data$interactive_text),
          size = text_size, fontface = "bold", na.rm = TRUE,
          direction = "both", #segment.alpha = 0.5,
          min.segment.length = 0.01,
          force = 0.5, force_pull = 1, point.padding = 0,  box.padding = 0, point.size = NA,
          arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines"))
        ) #point.padding = 0, segment.colour = "black"
    }
    plot_output <-
      ggplot2::ggplot(dplyr::filter(vars_data, .data$color_group != "active_vars"),
                      ggplot2::aes(x = !!dim1, y = !!dim2, label = .data$lvs,
                                   color = .data$color_group, data_id = .data$id)) +
      graph_theme_acm + profiles + active_graph + ellipses + graph_labels +
      mean_point_graph



  } else if (type[1] == "numbers") {
    plot_output <-
      ggplot2::ggplot(dplyr::filter(vars_data, .data$color_group != "active_vars"),
                      ggplot2::aes(x = !!dim1, y = !!dim2,
                                   tooltip = .data$interactive_text,
                                   data_id = .data$id)) +
      graph_theme_acm + profiles + ellipses +
      ggiraph::geom_label_interactive(
        data = dplyr::filter(vars_data, .data$color_group == "active_vars"),
        ggplot2::aes(x = !!dim1, y = !!dim2, label = .data$lvs,
                     tooltip = .data$interactive_text, data_id = .data$id + 1000),
        size = text_size, color = "black", na.rm = TRUE, inherit.aes = FALSE
      ) +
      ggiraph::geom_text_interactive(
        ggplot2::aes(label = .data$lvs, color = .data$color_group),  #colorvar_names
        size = text_size/1.2, hjust = "left", nudge_x = dist_labels[1],
        na.rm = TRUE
      ) + #fontface = "bold"
      ggiraph::geom_label_interactive(
        ggplot2::aes(label = .data$numbers, color = .data$color_group),
        size = text_size*1.2, fontface = "bold", na.rm = TRUE
      ) +
      mean_point_graph


  } else if(type[1] == "facets") {
    #facets : profiles by sup_vars, no active vars
    #for each sup_var, for the first ?

    plot_output <-
      ggplot2::ggplot(data = ind_data,
                      ggplot2::aes(x = !!dim1, y = !!dim2, size = .data$wcount,
                                   color = .data$color_group, group = .data$lvs)) +
      ggplot2::geom_point(na.rm = TRUE, show.legend = FALSE) +
      ggiraph::geom_point_interactive(
        data = dplyr::filter(vars_data, .data$vars == sup_vars[1]),
        ggplot2::aes(x = !!dim1, y = !!dim2, group = .data$lvs,
                     data_id = .data$id, tooltip = .data$interactive_text),
        color = "black", shape = 17, size = 0, stroke = 10,
        inherit.aes = FALSE, na.rm = TRUE, show.legend = FALSE
      ) +
      ggplot2::facet_wrap(ggplot2::vars(.data$lvs), scales = "fixed") +
      graph_theme_acm + ellipses


    css_hover <- ggiraph::girafe_css("stroke:orange;stroke-width:2;",
                                     text = "color:gold4;stroke:none;")

    plot_output <- plot_output %>%
      append(c("css_hover" = css_hover))

  } else { stop('unknown type of graph') }

  #Add informations in the ggplot2::ggplot object, to be used into ggi()
  # (without losing ggplot2::ggplot class)

  # if(!is.null(ellipses) & type[1] != "facets") {
  #   css_hover <- ggiraph::girafe_css("stroke:orange;stroke-width:2;",
  #                                    text = "color:gold4;stroke:none;")
  #   plot_output <- plot_output %>%
  #     append(c("css_hover" = css_hover))
  # }

  plot_output <- plot_output %>%
    append(c("heigth_width_ratio" = heigth_width_ratio)) %>%
    `attr<-`("class", c("gg", "ggplot"))

  return(plot_output)

}





# ggmca(res.mca, sup_vars = c("NBSALAacm", "DIPLOMEacm", "TPSINFOacm"),
#              dist_labels = 0.04, names_darker = TRUE) %>% ggi()
#
# ggmca(res.mca, sup_vars = c("PE0"), nb_char_for_color = 1,
#              dist_labels = 0.04, names_darker = TRUE) %>% ggi()

# ggmca(res.mca, split_var = PE, dplyr::filter = "^62",
#              , type="normal") +
#   scale_color_discrete()  + ylim(-0.3,NA)

# ggmca(res.mca, split_var = PE0, nb_char_for_color = 1,
#              dist_labels = 0.04, type = "ggplotly") +
#   ylim(NA, 1) + xlim (NA, 1.2); ggi("ggplotly")
#
# ggmca(res.mca, split_var = PE3_ord, dplyr::filter = "^.3C",
#              nb_char_for_color = 1,
#              dist_labels = 0.04, type = "ggplotly") +
#   ylim(NA, 0) + xlim (NA, 1) ; ggi("ggplotly")









#' Plot Initial Dimensions (Active Variables) of Multiple Correspondence Analysis
#'
#' @description
#' This function mostly have an educational value : it shows the
#' initial dimensions of the Multiple Correspondence Analysis (active variables)
#' in their initial reference frame. It shows the n dimensional space before the
#' analysis is done. To see initial dimensions axes in the space built by the
#' analysis (principal axes), use \code{\link[ggfacto]{ggmca_with_base_ref}}.
#'
#' @param res.mca An object created with \code{FactoMineR::\link[FactoMineR]{MCA}}.
#' @param data The data in which to find the supplementary variables, etc.
#' @param proj_just Horizontal justification of text of the coordinates on axes,
#' as a character vector of length 2 (x and y).
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like \code{"1-"}, and text in parentheses.
#' @param keep A character vector of the name of active variables to keep.
#'
#' @return A \code{\link[ggplot2:ggplot]{ggplot}} object to be printed in the
#' `RStudio` Plots pane. Possibility to add other gg objects with \code{+}.
#' Sending the result through \code{\link{ggi}} will draw the
#' interactive graph in the Viewer pane using \code{\link[ggiraph]{ggiraph}}.
#' @export
#'
#' @examples
#' \donttest{
#' data(tea, package = "FactoMineR")
#' res.mca <- MCA2(tea, active_vars = 1:18)
#' ggmca_initial_dims(res.mca, data = tea)
#' }
ggmca_initial_dims <- function(res.mca = res.mca, data, proj_just = c(1.5, 2),
                               cleannames = TRUE, keep = NULL) {

  mca_excl_done <- names(res.mca$call$Xtot)[res.mca$call$excl]

  row.w <- res.mca$call$row.w

  active_vars <- stringr::str_c(colnames(res.mca$call$X)[1:length(res.mca$call$quali)])

  active_var_levels <- purrr::map(active_vars, ~ dplyr::pull(data, .) %>%
                                    as.factor() %>%
                                    forcats::fct_na_value_to_level("NA") |>
                                    levels()
  ) |>
    purrr::set_names(active_vars) %>%
    purrr::imap_dfr(~ tibble::tibble(vars = .y, lvs2 = .x))  |>
    dplyr::mutate(vars = forcats::as_factor(.data$vars))

  active_var_levels_disordered <-
    purrr::map(active_vars, ~ dplyr::pull(res.mca$call$X, .) %>%
                 as.factor() %>% levels()) %>%
    purrr::set_names(active_vars) %>%
    purrr::imap_dfr(~ tibble::tibble(
      vars = .y,
      lvs  = .x,
      lvs2 = stringr::str_remove_all(.x, paste0("^", .y, "_") ),
    )) |>
    dplyr::mutate(vars = forcats::as_factor(.data$vars))

  active_var_levels <- active_var_levels |>
    dplyr::left_join(active_var_levels_disordered,
                     by = c("vars", "lvs2"),
                     relationship = "one-to-one") |>
    dplyr::filter(!.data$lvs %in% mca_excl_done) |>
    dplyr::group_by(.data$vars) |>
    dplyr::group_split() %>%
    purrr::set_names(purrr::map_chr(., ~ as.character(dplyr::first(.$vars)))) |>
    purrr::map(~ .$lvs)


  if(length(keep) > 0) active_var_levels <- active_var_levels |>
    keep(names(active_var_levels) %in% keep)

  active_var_levels_not_zero <-
    active_var_levels |>
    purrr::imap_dfr(~ tibble::tibble(vars = .y,
                                     lvs = sort(.x, decreasing = TRUE),
                                     #rn  = length(.x):1
    )
    ) |>
    dplyr::mutate(vars = forcats::as_factor(.data$vars)) |>
    dplyr::group_by(.data$vars) |>
    dplyr::mutate(max_lv = dplyr::n(),
                  vars_group = ceiling((dplyr::row_number()-1)/2) ) |>
    dplyr::ungroup()


  active_var_level0 <-
    active_var_levels_not_zero |>
    dplyr::filter(.data$vars_group == 0) |>
    dplyr::mutate(max_group = ceiling((.data$max_lv-1)/2)) |>
    dplyr::group_by(.data$vars) |>
    dplyr::group_split() |>
    purrr::map_dfr(~ tibble::tibble(vars = .$vars,
                                    lvs = .$lvs,
                                    vars_group = 1:.$max_group,
                                    max_lv = .$max_lv)
    )

  active_var_levels_not_zero <- active_var_levels_not_zero |>
    dplyr::filter(.data$vars_group != 0)

  active_var_level_grouped <- active_var_level0 |>
    dplyr::bind_rows(active_var_levels_not_zero) |>
    dplyr::arrange(.data$vars)

  active_var_level_grouped <-
    active_var_level_grouped |>
    dplyr::mutate(vars_group = paste0(
      .data$vars, " (",
      dplyr::case_when(
        .data$max_lv <= 3   ~ "",

        .data$vars_group == 1 ~ paste0(.data$vars_group*2 - 1, "-",
                                       pmin(.data$vars_group*2 + 1, .data$max_lv)),

        .data$vars_group*2 == .data$max_lv ~ paste0(.data$max_lv),

        TRUE ~ paste0(.data$vars_group*2, "-", .data$vars_group*2 + 1)
      ),
      ")"

    ) |>
      stringr::str_remove(" *\\(\\)$")
    ) |>
    dplyr::mutate(vars_group = forcats::as_factor(.data$vars_group)) |>
    dplyr::group_by(.data$vars_group) |>
    dplyr::group_split() %>%
    purrr::set_names(purrr::map_chr(., ~ as.character(dplyr::first(.$vars_group)))) |>
    purrr::map(~ list(vars = .$vars[1], vars_group = .$vars_group[1], lvs = .$lvs))



  # Table disjonctive
  disj <-
    purrr::pmap(active_var_level_grouped |> purrr::transpose(),
         ~ {
           disj <- res.mca$call$Xtot |>
             dplyr::select(tidyselect::all_of(..3) ) |>
             tibble::as_tibble() |>
             tibble::add_column(row.w = row.w)
           # disj <- dplyr::select(disj, tidyselect::all_of(..3) )

           if (cleannames) {
             disj <- disj |>
               dplyr::rename_with(~ stringr::str_remove_all(., cleannames_condition()))
           }

           disj |>
             dplyr::group_by(!!!rlang::syms(names(disj)[names(disj) != "row.w"])) |>
             dplyr::summarise(n = dplyr::n(),
                              wn = sum(row.w, na.rm = TRUE),
                              .groups = "drop") |>
             dplyr::mutate(freq = .data$wn/sum(.data$wn, na.rm = TRUE) ) |>
             dplyr::filter(!dplyr::if_all(-tidyselect::all_of(c("n", "wn", "freq")), ~ . == 0L)) |> # Remove NA line (only zeros)
             dplyr::rowwise() |>
             dplyr::mutate(lvs = which(dplyr::c_across(tidyselect::everything()) == "1") |>
                             dplyr::first()) |>
             dplyr::ungroup() %>%
             dplyr::mutate(lvs = names(.)[.data$lvs]) |>
             dplyr::rename_with(~ paste0("x", 0:(length(.)-1)),
                                .cols = -tidyselect::all_of(c("n", "wn", "lvs", "freq"))) |>
             dplyr::mutate(vars = ..1, vars_group = ..2, .before = 1)

           #dplyr::mutate(dplyr::across(tidyselect::starts_with("x"), ~ . * n / sum(n), .names = "mean_{.col}"))


         }
    )
  # disj[c("VIDEOS", "MUSIQUE",  "LIVRES (1-3)", "LIVRES (4)")]

  # Point moyen (barycentre)
  disj <-
    purrr::imap_dfr(
      disj,
      ~  {
        mean <- tibble::tibble(name = paste0("mean_x", (nrow(.x)-1):0),
                               freq = .x$freq
        ) |>
          tidyr::pivot_wider(names_from = "name", values_from = "freq") |>
          dplyr::mutate(vars_group = .y, .before = 1)

        .x |>
          dplyr::left_join(mean, by = "vars_group", relationship = "many-to-one")
      }
    ) |>
    dplyr::mutate(dplyr::across(
      tidyselect::starts_with(c("x", "mean_x")),
      ~ tidyr::replace_na(., 0)
    )) |>
    dplyr::mutate(vars_group = forcats::as_factor(.data$vars_group)) |>
    dplyr::select("vars", "vars_group", "lvs", tidyselect::starts_with("x"),
                  "n", "freq", tidyselect::starts_with("mean_x"))

  #  disj |> dplyr::filter(vars %in% c("VIDEOS", "MUSIQUE", "LIVRES")) |> new_tab() |> dplyr::group_by(vars)

  if (length(unique(disj$vars_group)) > 1) {
    #(
    disj |>
      ggplot2::ggplot(
        ggplot2::aes(x = .data$x2, y = .data$x1,
                     group = .data$vars_group, color = .data$vars)) +
      ggplot2::geom_polygon(
        ggplot2::aes(fill = .data$vars), color = NA, alpha = 0.1) +
      ggplot2::geom_segment(
        ggplot2::aes(xend = .data$x2 , yend = .data$x1), x = 0, y = 0,
        size = 0.75, linetype = "dashed"
      ) +
      ggiraph::geom_point_interactive(
        ggplot2::aes(size    = .data$n,
                     tooltip = paste0(round(.data$freq*100), "%",
                                      " (n=", .data$n, ")"))
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(x = dplyr::if_else(.data$x0 == 0, .data$x2 * .data$freq, NA),
                     y = .data$x1 * .data$freq,
                     xend = .data$mean_x2, yend = .data$mean_x1),
        na.rm = TRUE, color =  "black", linetype = "dashed") +
      ggplot2::geom_point(
        data = disj |>
          dplyr::group_by(.data$vars_group) |>
          dplyr::slice(1) |>
          dplyr::ungroup(),
        ggplot2::aes(x = .data$mean_x2, y = .data$mean_x1),
        color = "black", fill = "#eeeeee", shape = 3, size = 5, stroke = 1.5
      ) +
      ggrepel::geom_text_repel(
        ggplot2::aes(label = .data$lvs),
        hjust = "inward", size = 3, color = "black", #point.padding = 0.1,
        point.size = NA, nudge_x = 0.075, min.segment.length = Inf,
        direction = "y" #, force = 0.5, force_pull = 1,
      ) +
      # ggplot2::geom_text(data = disj |> dplyr::group_by(vars_group) |> dplyr::slice(1) |> dplyr::ungroup(),
      #             ggplot2::aes(x = mean_x1, y = mean_x2),
      #           label = "Mean point (weighted barycenter)",
      #           nudge_y = 0.06, size = 5, color = "black") +
      ggplot2::geom_text(data = disj |> dplyr::filter(!(.data$x1 == 0 & .data$x2 == 0)), # Projections
                         ggplot2::aes(x     = .data$x2 * .data$freq,
                                      y     = .data$x1 * .data$freq,
                                      hjust = dplyr::if_else(.data$x2 == 1, 0.5, proj_just[1]),
                                      vjust = dplyr::if_else(.data$x2 == 1, proj_just[2], 0.5),
                                      label = round(.data$freq, 2) ),
                         na.rm = TRUE,  color = "black", fontface = "bold") +
      # geom_label(ggplot2::aes(label = paste0("n=", n, " (", round(freq*100), "%)")),
      #            hjust = 0, nudge_x = 0.1, size = 3,
      #            fontface = "bold") +
      ggplot2::scale_x_continuous("", breaks = seq(0, 1, 0.25), lim = c(-0.4, 1.20) ) +
      ggplot2::scale_y_continuous("", breaks = seq(0, 1, 0.25), lim = c(-0.2, 1.10) ) +
      ggplot2::scale_size_area(max_size = 8, guide = "none") +
      ggplot2::scale_color_discrete(guide = "none") +
      ggplot2::scale_fill_discrete(guide = "none") +
      ggplot2::facet_wrap(ggplot2::vars(.data$vars_group)) + # NO .data$ ?
      # coord_fixed() +
      # coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     panel.spacing = ggplot2::unit(0, "cm")
                     #panel.grid.major = ggplot2::element_line(color = "grey80", linetype = "dotted")
      )  #) |>
    #ggi()


    # Juste une variable
  } else {

    if (! "x2" %in% names(disj)) disj <- disj |>
        dplyr::mutate(x2 = 0, mean_x2 = 0)

    #print(disj)
    #print(nrow(disj) > 2)
    #     print(if(nrow(disj) > 2) {seq(0, 1, 0.25)} else {0}
    # )

    disj |>
      ggplot2::ggplot(
        ggplot2::aes(x = .data$x2, y = .data$x1, group =.data$ vars_group,
                                   color = .data$vars)) +
      ggplot2::geom_polygon(ggplot2::aes(fill = .data$vars), color = NA, alpha = 0.1) +
      ggplot2::geom_segment(
        ggplot2::aes(xend = .data$x2 , yend = .data$x1), x = 0, y = 0,
        size = 0.75, linetype = "dashed"
      ) +
      ggplot2::geom_point(ggplot2::aes(size = .data$n)) +
      ggplot2::geom_segment(
        ggplot2::aes(x = dplyr::if_else(.data$x0 == 0, .data$x2 * .data$freq, NA),
                     y = .data$x1 * .data$freq,
                     xend = .data$mean_x2, yend = .data$mean_x1),
        na.rm = TRUE, color =  "black", linetype = "dashed"
      ) +
      ggplot2::geom_point(
        data = disj |>
          dplyr::group_by(.data$vars_group) |>
          dplyr::slice(1) |>
          dplyr::ungroup(),
        ggplot2::aes(x = .data$mean_x2, y = .data$mean_x1),
        color = "black", fill = "#eeeeee", shape = 3, size = 5, stroke = 1.5
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = .data$lvs), nudge_y = 0.06, size = 5, color = "black"
      ) +
      ggplot2::geom_text(
        data = disj |>
          dplyr::group_by(.data$vars_group) |>
          dplyr::slice(1) |>
          dplyr::ungroup(),
        ggplot2::aes(x = .data$mean_x2, y = .data$mean_x1),
        label = "Mean point (weighted barycenter)",
        nudge_y = 0.06, size = 5, color = "black"
      ) +
      ggplot2::geom_text(
        data = disj |> dplyr::filter(!(.data$x1 == 0 & .data$x2 == 0)),
        ggplot2::aes(x     = .data$x2 * .data$freq,
                     y     = .data$x1 * .data$freq,
                     hjust = dplyr::if_else(.data$x2 == 1, 0.5, proj_just[1]),
                     vjust = dplyr::if_else(.data$x2 == 1, proj_just[2], 0.5),
                     label = round(.data$freq, 2) ),
        na.rm = TRUE,  color = "black", fontface = "bold"
      ) +
      ggplot2::geom_label(
        ggplot2::aes(label = paste0(round(.data$freq*100), "%",
                                    "( n=", .data$n, ")")),
        hjust = 0, nudge_x = 0.035, size = 4,
        fontface = "bold"
      ) +
      ggplot2::scale_x_continuous(
        "", breaks = if(nrow(disj) > 2) {seq(0, 1, 0.25)} else {0},
        lim = if(nrow(disj) > 2) {c(-0.15, 1.20)} else {c(-0.5, 0.5)}
      ) +
      ggplot2::scale_y_continuous("", breaks = seq(0, 1, 0.25) ) +
      ggplot2::scale_size_area(max_size = 12, guide = "none") +
      ggplot2::scale_color_discrete(guide = "none") +
      ggplot2::scale_fill_discrete(guide = "none") +
      ggplot2::theme_minimal()

  }

}





#' Plot Initial Dimensions (Active Variables) on a Multiple Correspondence Analyses
#'
#' @description This function mostly have an educational value : it shows the
#' initial dimensions of the Multiple Correspondence Analysis (active variables)
#' in the space built by the analysis (principal axes). To see initial
#' dimensions in their initial reference frame, use \code{\link[ggfacto]{ggmca_initial_dims}}.
#' @param res.mca An object created with \code{FactoMineR::\link[FactoMineR]{MCA}}.
#' @param axes The axes to print, as a numeric vector of length 2.
#' @param keep A character vector of the name of active variables to keep.
#'
#' @return A \code{\link[ggplot2:ggplot]{ggplot}} object to be printed in the
#' `RStudio` Plots pane. Possibility to add other gg objects with \code{+}.
#' Sending the result through \code{\link{ggi}} will draw the
#' interactive graph in the Viewer pane using \code{\link[ggiraph]{ggiraph}}.
#' @export
#'
#' @examples
#' \donttest{
#' data(tea, package = "FactoMineR")
#' res.mca <- MCA2(tea, active_vars = 1:18)
#' ggmca_with_base_ref(res.mca)
#'
#' # It is more readable to select just a few active variables
#' lv2_vars <- dplyr::select(tea[1:18], where(~ nlevels(.) == 2)) |> names()
#' ggmca_with_base_ref(res.mca, keep = lv2_vars)
#'
#' lv3_vars <- dplyr::select(tea[1:18], where(~ nlevels(.) == 3)) |> names()
#' ggmca_with_base_ref(res.mca, keep = lv3_vars)
#'
#' lv4_vars <- dplyr::select(tea[1:18], where(~ nlevels(.) == 4)) |> names()
#' ggmca_with_base_ref(res.mca, keep = lv4_vars)
#'
#' lv6_vars <- dplyr::select(tea[1:18], where(~ nlevels(.) == 6)) |> names()
#' ggmca_with_base_ref(res.mca, keep = lv6_vars)
#' }
ggmca_with_base_ref <- function(res.mca = res.mca, axes = c(1, 2),
                                keep = NULL) {

  dim1 <- rlang::sym(stringr::str_c("Dim ", axes[1]))
  dim2 <- rlang::sym(stringr::str_c("Dim ", axes[2]))

  active_vars <-
    stringr::str_c(colnames(res.mca$call$X)[1:length(res.mca$call$quali)])


  active_var_levels <-
    purrr::map(active_vars, ~ dplyr::pull(res.mca$call$X, .) %>%
                 as.factor() %>% levels()) %>%
    purrr::set_names(active_vars) %>%
    purrr::imap_dfr(~ tibble::tibble(vars = .y, lvs = .x))

  freqs <- tibble::enframe(res.mca$call$marge.col * length(active_vars),
                           "lvs", "freq")
  freqs <- active_var_levels |> dplyr::left_join(freqs, by = "lvs") |>
    dplyr::mutate(lvs = stringr::str_remove_all(.data$lvs,
                                                cleannames_condition()))


  vars_data <- ggmca_data(res.mca)$vars_data # get_data = TRUE
  acm_orga_from_base_ref <- vars_data |>
    dplyr::filter(.data$color_group == "active_vars")

  if(length(keep) > 0) acm_orga_from_base_ref <- acm_orga_from_base_ref |>
    dplyr::filter(.data$vars %in% keep)

  acm_orga_from_base_ref <- acm_orga_from_base_ref |>
    dplyr::mutate(vars = forcats::fct_drop(.data$vars))


  acm_orga_from_base_ref <- acm_orga_from_base_ref |>
    dplyr::left_join(freqs, by = c("vars", "lvs")) |>
    dplyr::mutate(lvs = forcats::as_factor(.data$lvs)) |> # to keep order
    dplyr::group_by(.data$vars) |>
    dplyr::arrange(dplyr::desc(.data$lvs), .by_group = TRUE) |>
    dplyr::select(
      tidyselect::everything() & -tidyselect::starts_with("Dim "),
      tidyselect::all_of(c(as.character(dim1), as.character(dim2)))
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("Dim "),
        ~ dplyr::if_else(dplyr::row_number() != 1, dplyr::first(.), NA),
        .names = "start_{.col}"
      ),


      dplyr::across(
        tidyselect::starts_with("Dim "), # right angles of vars vectors vars vects
        ~ dplyr::if_else(
          dplyr::row_number() != 1,
          true  = (.*1/16 + dplyr::first(.) * 15/16),
          false = NA_real_),
        .names = "start_angle_{.col}"
      ),


      dplyr::across(
        tidyselect::starts_with("Dim "), # projections of mean point on vars vects
        ~ dplyr::if_else(
          dplyr::row_number() != 1,
          true  = (.*.data$freq + dplyr::first(.) * (1-.data$freq)),
          #(.*freq + dplyr::first(.) * dplyr::first(freq))/(freq + dplyr::first(freq)),
          false = NA_real_),
        .names = "proj_{.col}"
      ),
    ) |>
    dplyr::rename_with(~stringr::str_remove(., "_Dim "),
                       .cols = tidyselect::starts_with("proj_Dim")) |>
    dplyr::rename_with(~stringr::str_remove(., "_Dim "),
                       .cols = tidyselect::starts_with("start_angle_Dim")) |>
    dplyr::mutate(
      ang_x = .data$start_angle1 - dplyr::first(!!dim1),  # 2 left 3 down
      ang_y = .data$start_angle2 - dplyr::first(!!dim2),  # 2 left 3 down

      ang_ld_x =  dplyr::nth(.data$start_angle1, 2L) +
        dplyr::nth(.data$ang_x, 3L) - dplyr::first(!!dim1), # left down

      ang_ld_y =  dplyr::nth(.data$start_angle2, 2L) +
        dplyr::nth(.data$ang_y, 3L) - dplyr::first(!!dim2), # left down

      # right angle on zero var
      start_angle12_x = dplyr::nth(.data$start_angle1, 2L) + dplyr::nth(.data$ang_x, 3L),

      start_angle12_y = dplyr::nth(.data$start_angle2, 2L) + dplyr::nth(.data$ang_y, 3L),


      # # right angles on mean point
      # moy_angle1 = dplyr::if_else(
      #          dplyr::row_number() != 1,
      #          true  = 0 - ang_x,
      #          false = NA_real_),
      #
      # moy_angle2 = dplyr::if_else(
      #          dplyr::row_number() != 1,
      #          true  = 0 - ang_y,
      #          false = NA_real_),
      #
      # moy_angle12_x = dplyr::nth(moy_angle1, 2L) - dplyr::nth(ang_x, 3L),
      #
      # moy_angle12_y = dplyr::nth(moy_angle2, 2L) - dplyr::nth(ang_y, 3L),


      # right angle on projections
      proj_angle_x = dplyr::if_else( #
        dplyr::row_number() != 1,
        true  = .data$proj1 - .data$ang_x,
        false = NA_real_),

      proj_angle_y = dplyr::if_else(
        dplyr::row_number() != 1,
        true  = .data$proj2 - .data$ang_y,
        false = NA_real_),


      proj_angle_b_x = dplyr::if_else(
        dplyr::row_number() != 1,
        true  = .data$proj1 + dplyr::if_else(dplyr::row_number() == 2,
                                       true  = dplyr::nth(.data$ang_x, 3),
                                       false = dplyr::nth(.data$ang_x, 2) ),
        false = NA_real_),

      proj_angle_b_y = dplyr::if_else(
        dplyr::row_number() != 1,
        true  = .data$proj2 + dplyr::if_else(dplyr::row_number() == 2,
                                       true  = dplyr::nth(.data$ang_y, 3),
                                       false = dplyr::nth(.data$ang_y, 2) ),
        false = NA_real_),

      proj_angle_c_x = dplyr::if_else(
        dplyr::row_number() != 1,
        true  = .data$proj_angle_x + dplyr::if_else(dplyr::row_number() == 2,
                                              true  = dplyr::nth(.data$ang_x, 3),
                                              false = dplyr::nth(.data$ang_x, 2) ),
        false = NA_real_),

      proj_angle_c_y = dplyr::if_else(
        dplyr::row_number() != 1,
        true  = .data$proj_angle_y + dplyr::if_else(dplyr::row_number() == 2,
                                                    true  = dplyr::nth(.data$ang_y, 3),
                                                    false = dplyr::nth(.data$ang_y, 2) ),
        false = NA_real_),

    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      "vars", "lvs", "freq", #"wcount",
      "Dim 1", "Dim 2", "start_Dim 1", "start_Dim 2", "proj1", "proj2",
      tidyselect::everything() & -tidyselect::any_of(
        c("color_group", "id", "cah_id", "interactive_text", "face")
      )
    )


  keep_has_names <- all(!is.null(names(keep)))

  color_scale <-  if (keep_has_names) {
    ggplot2::scale_color_manual(values = purrr::set_names(names(keep), keep),
                                aesthetics = c("colour", "fill"),
                                na.value = "grey70" )

  } else if (length(unique(acm_orga_from_base_ref$vars)) <= 12) {
    ggplot2::scale_color_manual(values = purrr::set_names(material_colors_light(), NULL),
                                aesthetics = c("colour", "fill"),
                                na.value = "grey70")
  } else {
    ggplot2::scale_color_discrete(aesthetics = c("colour", "fill"),
                                  na.value = "grey70")
  }


  acm_orga_from_base_ref |>
    ggplot2::ggplot(ggplot2::aes(x = !!dim1, y = !!dim2)) +
    ggfacto::theme_facto(res.mca, no_color_scale = TRUE) +
    #acm_orga_1_cah$graph_theme_acm +
    ggplot2::geom_point(
      data = tibble::tibble(!!dim1 := 0, !!dim2 := 0),
      color = "black", fill = "#eeeeee", shape = 3, size = 5,
      stroke = 1.5, na.rm = TRUE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = !!dim1, yend = !!dim2   ,
                   x = .data$`start_Dim 1` , y = .data$`start_Dim 2`,
                   color = .data$vars, group = .data$vars),
      size = 1, arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "lines")), na.rm = TRUE
    ) +
    ggplot2::geom_segment( # projections
      ggplot2::aes(xend = .data$proj1 , yend = .data$proj2,
                   color = .data$vars, group = .data$vars),
      x = 0, y = 0, size = 0.5, linetype = "dashed",
    ) +
    #   ggplot2::geom_segment(data = mid_point_test, # projections
    #   ggplot2::aes(color = vars, group = vars),
    #   xend = 0, yend = 0, size = 0.5, linetype = "dashed",
    # ) +
    ggplot2::geom_segment( # right angle on vars vectors
      ggplot2::aes(xend = .data$start_angle12_x , yend = .data$start_angle12_y,
                   x = .data$start_angle1 , y = .data$start_angle2,
                   color = .data$vars, group = .data$vars),
      size = 0.5
    ) +
    #   ggplot2::geom_segment( # right angle on mean point
    #   ggplot2::aes(xend = .data$moy_angle12_x , yend = .data$moy_angle12_y,
    #                x = .data$moy_angle1 , y = .data$moy_angle2,
    #                color = .data$vars, group = .data$vars),
    #   size = 0.5
    # ) +

    ggplot2::geom_segment( # right angle on projections
      ggplot2::aes(xend = .data$proj_angle_x, yend = .data$proj_angle_y,
                   x = .data$proj_angle_c_x , y = .data$proj_angle_c_y,
                   color = .data$vars, group = .data$vars),
      size = 0.5
    ) +
    ggplot2::geom_segment( # right angle on projections
      ggplot2::aes(xend = .data$proj_angle_b_x , yend = .data$proj_angle_b_y,
                   x = .data$proj_angle_c_x , y = .data$proj_angle_c_y,
                   color = .data$vars, group = .data$vars),
      size = 0.5
    ) +
    ggplot2::geom_polygon(
      ggplot2::aes(fill = .data$vars, group = .data$vars),
      size = 0.5, color = NA, alpha = 0.2,
    ) +
    ggrepel::geom_label_repel(
      ggplot2::aes(x = dplyr::if_else(!!dim1 > 0, !!dim1 + 0.03, !!dim1 - 0.03),
                   label = .data$lvs, fontface = "bold", color = .data$vars),
      size = 3, na.rm = TRUE, direction = "y", fill = grDevices::rgb(1, 1, 1, alpha = 0.7),
      min.segment.length = Inf,
      force = 0.5, force_pull = 1, point.padding = 0.1, box.padding = 0, hjust = "outward"
    ) +
    color_scale

}








#
#
# ggmca_cah <- function(res.mca, dat, cah, axes = c(1, 2), text_size = 3,
#                       color_groups, ...) {
#
#   dim1 <- rlang::sym(stringr::str_c("Dim ", axes[1]))
#   dim2 <- rlang::sym(stringr::str_c("Dim ", axes[2]))
#
#   if (missing(color_groups)) color_groups <- "^.{1}"
#
#   acm <- res.mca |>
#     ggmca(dat = pc_AGD,
#           cah = cah,
#           axes = axes,
#           color_groups = color_groups,
#           profiles = TRUE,
#           get_data = TRUE,
#           ...
#     )
#
#
#   acm_cah <- acm$vars_data |>
#     dplyr::filter(stringr::str_detect(color_group, paste0("^", cah)))
#   acm_vars <- new_tab(acm$vars_data) |>
#     dplyr::filter(vars != cah) |>
#     dplyr::mutate(face = dplyr::if_else(color_group != "variables_actives", "italic", "bold") )
#   #     certains profils n'ont pas de cah : corriger dans ggmca_data
#   cah_id_recode <- acm$profiles_coord |> dplyr::select(cah, cah_id) |>
#     dplyr::filter(!is.na(cah)) |>
#     dplyr::distinct() |>
#     dplyr::mutate(recode_vect = purrr::set_names(as.character(cah), cah_id)) |> dplyr::pull(recode_vect)
#   cah_color <- acm_cah |>
#     dplyr::select(lvs, color_group) |>
#     dplyr::mutate(recode_vect = purrr::set_names(as.character(lvs), color_group)) |> dplyr::pull(recode_vect)
#   acm_profiles <- acm$profiles_coord |>
#     dplyr::mutate(
#       cah = map2_chr(
#         cah, sup_vars,
#         ~ dplyr::case_when(
#           !is.na(.x)                      ~ .x,
#           all(is.na(dplyr::pull(.y, !!rlang::sym(cah))) | dplyr::pull(.y, !!rlang::sym(cah)) == dplyr::first(dplyr::pull(.y, !!rlang::sym(cah))))
#           ~ as.character(dplyr::first(dplyr::pull(.y, !!rlang::sym(cah)))),
#           TRUE                            ~ NA_character_)
#       ),
#
#       cah_id = forcats::fct_recode(cah, !!!cah_id_recode) |> as.integer(),
#
#       color_group = forcats::fct_recode(cah, !!!cah_color)
#     ) |>
#     dplyr::filter(!is.na(cah))
#   # acm_profiles |> slice_sample(n = 20)
#
#   cah_name_with_pct <- acm_cah |>
#     dplyr::select(lvs, wcount) |>
#     dplyr::mutate(pct = round(wcount/sum(wcount)*100), 0) |>
#     dplyr::mutate(recode_vect = purrr::set_names(as.character(lvs), paste0(lvs, " (", pct, "%)"))) |>
#     dplyr::pull(recode_vect)
#   acm_cah <- acm_cah |>
#     dplyr::mutate(lvs = forcats::fct_recode(lvs, !!!cah_name_with_pct))
#
#   # heigth_width_ratio <- (0.8 + 1.2) / (0.8 + 1.1)
#
#
#   ggplot(acm_vars, ggplot2::aes(x = !!dim1, y = !!dim2)) +
#     acm$graph_theme_acm +
#     ggplot2::geom_point(
#       data = acm_profiles, ggplot2::aes(size = wcount, color = color_group),
#       na.rm = TRUE, show.legend = FALSE, stroke = 0, alpha = 0.5
#     ) +
#     ggplot2::geom_point(
#       data = acm$mean_point_data,
#       color = "black", fill = "#eeeeee", shape = 3, size = 5, stroke = 1.5, na.rm = TRUE
#     ) +
#     ggrepel::geom_text_repel(
#       ggplot2::aes(label = lvs, fontface = face),
#       size = text_size, na.rm = TRUE, direction = "both",  min.segment.length = 0.01,
#       force = 0.5, force_pull = 1, point.padding = 0, box.padding = 0,
#       point.size = NA, arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines"))
#     ) +
#     # ggplot2::geom_segment(
#     #   data = acm_cah |>
#     #     dplyr::mutate(!!dim1 = pmin(1.3, pmax(!!dim1, -0.9)),
#     #            !!dim2 = pmin(1.3, pmax(!!dim2, -0.85)),
#     #            start1  = pmin(0.95, pmax(!!dim1, -0.5)),
#     #            start2  = pmin(1.25, pmax(!!dim2, -0.775)),
#     #     ),
#     #   ggplot2::aes(x = start1, xend = !!dim1, y = start2, yend = !!dim2,
#     #                color = color_group),
#     #   arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "lines")), na.rm = TRUE
#     # ) +
#   ggrepel::geom_label_repel(
#     data = acm_cah,
#     ggplot2::aes(label = lvs, color = color_group), fill = rgb(1, 1, 1, alpha = 0.7),
#     direction = "y", force = 0.5, force_pull = 1, point.padding = 0, point.size = NA,
#     arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines")),
#     fontface = "bold", size = text_size, na.rm = TRUE
#   )
# }








#'  Interactive 3D Plot for Multiple Correspondence Analyses (plotly::)
#'
#' @param res.mca An object created with \code{FactoMineR::\link[FactoMineR]{MCA}}.
#' @param dat The data in which to find the cah variable, etc.
#' @param cah A variable made with \code{\link[FactoMineR]{HCPC}}, to link
#' the answers-profiles points who share the same HCPC class (will be colored
#' the same color and linked at mouse hover).
#' @param axes The axes to print, as a numeric vector of length 3.
#' @param base_zoom The base level of zoom.
#' @param remove_buttons Set to TRUE to remove buttons to change view.
#' @param cone_size The size of the conic arrow at the end of each axe.
#' @param view The starting point of view (in 3D) :
#'      \itemize{
#'    \item \code{"Plane 1-2"} : Axes 1 and 2.
#'    \item \code{"Plane 1-3"} : Axes 1 and 3.
#'    \item \code{"Plane 2-3"} : Axes 2 and 3.
#'    \item \code{"All"      } : A 3D perspective with Axes 1, 2, 3.
#'  }
#' @param camera_view Possibility to add a (replace `view`)
#' @param aspectratio_from_eig Set to `TRUE` to modify axes length based on
#' eigenvalues.
#' @param title The title of the graph.
#' @param ind_name.size The size of the names of individuals.
#' @param max_point_size The size of the biggest point.
#' @param ... Additional arguments to pass to \code{\link[ggfacto:ggmca]{ggmca}}.
#'
#' @return A \code{\link[plotly]{plotly}} html interactive 3d (or 2d) graph.
#' @export
#'
#' @examples
#' \donttest{
#' data(tea, package = "FactoMineR")
#' res.mca <- MCA2(tea, active_vars = 1:18)
#' ggmca_3d(res.mca)
#'
#' # 3D graph with colored HCPC clusters (cah)
#' res.mca_3axes <- MCA2(tea, active_vars = 1:18, ncp = 3)
#' cah <- FactoMineR::HCPC(res.mca_3axes, nb.clust = 6, graph = FALSE)
#' tea$clust <- cah$data.clust$clust
#' ggmca_3d(res.mca, dat = tea, cah = "clust")
#' }
ggmca_3d <- function(res.mca, dat, cah, axes = 1:3, # color_groups,
                     base_zoom = 1, remove_buttons = FALSE, cone_size = 0.15,
                     view = "All",
                     camera_view, aspectratio_from_eig = FALSE, title,
                     ind_name.size = 10, max_point_size = 30, # ind.size = 4,
                     ...) {
  requireNamespace("plotly", quietly = TRUE)

  if (missing(cah)) cah <- character()

  D2 <- length(axes) == 2 ; stopifnot(length(axes) %in% 2:3 )
  if (D2) axes <- c(axes, NA)

  dim1 <- rlang::sym(stringr::str_c("Dim ", axes[1]))
  dim2 <- rlang::sym(stringr::str_c("Dim ", axes[2]))

  # if (missing(color_groups)) color_groups <- "^.{1}"

  acm <- res.mca |>
    ggmca(dat = dat,
          cah = cah,
          # color_groups = color_groups,
          profiles = TRUE,
          get_data = TRUE,
          ...
    )

  acm_cah <- acm$vars_data |>
    dplyr::filter(stringr::str_detect(.data$color_group, paste0("^", cah)))
  acm_vars <- tabxplor::new_tab(acm$vars_data) |>
    dplyr::filter(!.data$vars %in% cah) |>
    dplyr::mutate(face = dplyr::if_else(.data$color_group != "variables_actives", "italic", "bold") )
  acm_profiles <- acm$profiles_coord

  if(length(cah) > 0) {
    acm_profiles <- acm_profiles |> dplyr::filter(!is.na(cah))

    cah_name_with_pct <- acm_cah |>
      dplyr::select("lvs", "wcount") |>
      dplyr::mutate(pct = round(.data$wcount/sum(.data$wcount)*100), 0) |>
      dplyr::mutate(recode_vect = purrr::set_names(as.character(.data$lvs),
                                                   paste0(.data$lvs, " (",
                                                          .data$pct, "%)"))) |>
      dplyr::pull("recode_vect")

    acm_cah <- acm_cah |>
      dplyr::mutate(lvs = forcats::fct_recode(.data$lvs, !!!cah_name_with_pct),
                    lvs = paste0("<b>", .data$lvs, "</b>"))
  }


  # heigth_width_ratio <- (0.8 + 1.2) / (0.8 + 1.1)


  plot_range <-
    dplyr::bind_rows(dplyr::select(acm_profiles, tidyselect::starts_with("Dim ")),
                     dplyr::select(acm_cah, tidyselect::starts_with("Dim ")),
                     dplyr::select(acm_vars, tidyselect::starts_with("Dim ")),
                     #dplyr::select(base_axis_in_princ, tidyselect::starts_with("Dim."))
    ) |>
    purrr::map(~ range(.) |> abs() |> max())
  plot_range <- plot_range |> purrr::map(~ c(-., .))

  princ_axes <-
    plot_range |> purrr::map(~  scales::breaks_extended(n = 4)(.)) |>
    purrr::imap_dfr(~ tibble::tibble(name = .y, !!rlang::sym(.y) := .x, base_coord = .x)) |>
    dplyr::mutate(dplyr::across(tidyselect::starts_with("Dim "), ~ tidyr::replace_na(., 0)),
                  name    = forcats::as_factor(.data$name),  #name = paste0(name, ".", base_coord)
                  pair_id = as.integer(.data$name),

    ) |>
    dplyr::mutate(name = stringr::str_replace(.data$name, "Dim ", "Axe ") ) |>
    dplyr::select("name", "pair_id", "base_coord", tidyselect::starts_with("Dim ") )

  plot_range <-
    purrr::map2(plot_range,
                princ_axes |>
                  dplyr::group_by(.data$name) |>
                  dplyr::group_split() |>
                  purrr::map(~.$base_coord |> range()),
                ~ range(c(.x, .y))
    )




  #   # Base ggplot 2D
  # ggplot(acm_vars, ggplot2::aes(x = !!dim1, y = !!dim2)) +
  #   acm$graph_theme_acm +
  #   ggplot2::geom_point(
  #     data = acm_profiles, ggplot2::aes(size = wcount, color = color_group),
  #     na.rm = TRUE, show.legend = FALSE, stroke = 0, alpha = 0.5
  #   ) +
  #   ggplot2::geom_point(
  #     data = acm$mean_point_data,
  #     color = "black", fill = "#eeeeee", shape = 3, size = 5, stroke = 1.5, na.rm = TRUE
  #   ) +
  #   ggrepel::geom_text_repel(
  #     ggplot2::aes(label = lvs, fontface = face),
  #     size = text_size, na.rm = TRUE, direction = "both",  min.segment.length = 0.01,
  #     force = 0.5, force_pull = 1, point.padding = 0, box.padding = 0,
  #     point.size = NA, arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines"))
  #   ) +
  #   # ggplot2::geom_segment(
  #   #   data = acm_cah |>
  #   #     dplyr::mutate(!!dim1 = pmin(1.3, pmax(!!dim1, -0.9)),
  #   #            !!dim2 = pmin(1.3, pmax(!!dim2, -0.85)),
  #   #            start1  = pmin(0.95, pmax(!!dim1, -0.5)),
  #   #            start2  = pmin(1.25, pmax(!!dim2, -0.775)),
  #   #     ),
  #   #   ggplot2::aes(x = start1, xend = !!dim1, y = start2, yend = !!dim2,
  #   #                color = color_group),
  #   #   arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "lines")), na.rm = TRUE
  #   # ) +
  # ggrepel::geom_label_repel(
  #   data = acm_cah,
  #   ggplot2::aes(label = lvs, color = color_group), fill = grDevices::rgb(1, 1, 1, alpha = 0.7),
  #   direction = "y", force = 0.5, force_pull = 1, point.padding = 0, point.size = NA,
  #   arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines")),
  #   fontface = "bold", size = text_size, na.rm = TRUE
  # )


  if (length(cah) > 0) {
    acm_lv <- acm_cah$color_group |> forcats::fct_drop() |> levels()
    acm_lv <- purrr::set_names(acm_lv,
                               material_colors_light()[1:length(acm_lv)],

    )

    acm_cah <- acm_cah |>
      dplyr::mutate(
        color_group = forcats::fct_recode(.data$color_group, !!!acm_lv) |>
          forcats::fct_drop()
      )

    acm_profiles <- acm_profiles |>
      dplyr::mutate(
        color_group = forcats::fct_recode(.data$color_group, !!!acm_lv) |>
          forcats::fct_drop()
      )

  } else {
    acm_profiles <- acm_profiles |> dplyr::mutate(color_group = factor("#bbbbbb"))
  }

  acm_profiles <- acm_profiles |>
    dplyr::mutate(
      size_scaled = scales::abs_area(max = max_point_size)(.data$wcount),
      size_scaled =
        .data$size_scaled/max(.data$size_scaled, na.rm = TRUE)*max_point_size,
    )
  # acm_profiles |>
  #   dplyr::select(wcount, size_scaled ) |>
  #   dplyr::slice(4500:5000) |>
  #   print(n = 900)




  # Axes
  axes_common_infos <- list(
    showspikes     = FALSE, # projections lines
    showgrid       = FALSE,
    zeroline       = FALSE,
    showticklabels = FALSE #,

    # backgroundcolor="rgb(200, 200, 230",
    # gridcolor="rgb(255,255,255)",
    # zerolinecolor="rgb(255,255,255"

    # ticketmode = 'array',
    # ticktext   = c("Huey", "Dewey", "Louie"),
    # tickvals   = c(0,25,50),
    # range      = c(-25,75)

    # nticks = 4,
  )

  axes_params <- purrr::map(plot_range,
                            ~ c(list(range = ., title = ""), axes_common_infos)
  )







  ## Assemble plot ----

  dim1 <- rlang::sym(stringr::str_c("Dim ", axes[1]))
  dim2 <- rlang::sym(stringr::str_c("Dim ", axes[2]))
  dim3 <- if (D2) {NULL} else {rlang::sym(stringr::str_c("Dim ", axes[3]))}


  # To get a fixed aspect ratio, put a point in max range * aspectratio on all axes
  if (aspectratio_from_eig) {
    aspectratio <- list(x = res.mca$svd$vs[axes[1]],
                        y = res.mca$svd$vs[axes[2]],
                        z = if (D2) {NULL} else {res.mca$svd$vs[axes[3]]}
    )

  } else {
    aspectratio <- list(x = 1, y = 1, z =if (D2) {NULL} else {1})
  }

  aspectratio_range <- tibble::as_tibble(plot_range) |>
    ## dplyr::mutate(Dim.2 = Dim.2 * 2) |>  # test
    #dplyr::mutate(dplyr::across(tidyselect::everything(), ~pmax(!!!rlang::syms(names(plot_range))))) |>
    dplyr::mutate(dplyr::across(axes[1], ~ . * aspectratio[[1]]),
                  dplyr::across(axes[2], ~ . * aspectratio[[2]]),
                  dplyr::across(if (D2) {NULL} else {axes[3]}, ~ . * aspectratio[[3]]),
    )
  if (D2) aspectratio_range <- aspectratio_range |> dplyr::select(-"Dim.3")

  #     se calcule ensuite, pour chaque axe, par rapport a son propre range ?




  # camera_title <- names(camera_view)

  if (!missing(camera_view)) {
    camera_view <- camera_view |>
      purrr::set_names(paste0("scene", 1:length(camera_view)) |>
                         stringr::str_replace("scene1", "scene") )
    scene_name <- names(camera_view)

  } else {
    scene_name <- "scene"
  }

  # i <- 1
  dual_plots <- vector("list", length(scene_name))
  for (i in 1:length(scene_name)) {

    dual_plots[[i]] <- plotly::plot_ly(scene = scene_name[i])

    # Individus colores selon CAH
    dual_plots[[i]] <- dual_plots[[i]] |>
      plotly::add_trace(
        data = acm_profiles, scene = scene_name[i],
        x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),  # color = df$color_col
        text = ~interactive_text,
        #textfont = list(color = "#00600f", size = ind_name.size),  # "#0077c2"
        marker   = list(color = ~color_group, size = ~size_scaled),  # "#0077c2"
        # hovertemplate = paste(
        #   "<b>%{text}</b><br>", # <br>
        #   "%{yaxis.title.text}: %{y:$,.0f}<br>",
        #   "%{xaxis.title.text}: %{x:.0%}<br>",
        #   #"Number Employed: %{marker.size:,}",
        #   "<extra></extra>"
        # ),
        hoverinfo = "text",
        hoverlabel = list(align = "right"),
        # text = ~paste("Price: ", price, '$<br>Cut:', cut),
        type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
        mode = "markers", showlegend = FALSE, inherit = FALSE)


    # variables actives
    dual_plots[[i]] <- dual_plots[[i]] |>
      plotly::add_trace(
        data = acm_vars, scene = scene_name[i],
        x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),  # color = df$color_col
        text = ~lvs,
        textfont = list(color = "black", size = ind_name.size),  # "#0077c2"
        hoverinfo = "skip",
        # text = ~paste("Price: ", price, '$<br>Cut:', cut),
        type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
        mode = "text", showlegend = FALSE, inherit = FALSE)

    # labels cah
    if (length(cah) > 0) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = acm_cah, scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),  # color = df$color_col
          text = ~lvs,
          textfont = list(color = ~color_group, size = ind_name.size),  # "#0077c2"
          hoverinfo = "skip",
          # text = ~paste("Price: ", price, '$<br>Cut:', cut),
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "text", showlegend = FALSE, inherit = FALSE)
    }

    # Axes principaux de l'ACP
    dual_plots[[i]] <- dual_plots[[i]] |>
      plotly::add_trace(
        data = princ_axes |>
          dplyr::filter(.data$name %in% paste0("Axe ", axes) ) |>
          dplyr::group_by(.data$name) |>
          dplyr::slice(-dplyr::n()) |> dplyr::ungroup(),
        # dplyr::mutate(remove_last_if_not_1 = dplyr::row_number() == dplyr::n() & base_coord != 1) |>
        # dplyr::filter(!remove_last_if_not_1) |> dplyr::ungroup(),
        scene = scene_name[i],
        x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),
        marker = list(color  = "black",
                      symbol = "cross",
                      size = 5), # 3
        text = ~base_coord, textfont = list(color = "black", size = 10),
        textposition = "bottom center", hoverinfo = "skip",
        type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
        mode = 'markers+text', showlegend = FALSE, inherit = FALSE) |>
      plotly::add_trace(
        data = princ_axes |>
          dplyr::filter(.data$name %in% paste0("Axe ", axes) ) |>
          dplyr::group_by(.data$name) |> dplyr::slice(1, dplyr::n()) |>
          dplyr::mutate(
            name = dplyr::if_else(
              dplyr::row_number() == 1,
              true  = "",
              false = paste0("<b>", .data$name, "</b>")
            )
          ) |>
          dplyr::ungroup(),
        scene = scene_name[i],
        x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ pair_id,
        line = list(color  = "black", width = 5),
        text = ~name, textfont = list(color = "black", size = 15),
        #textposition = "top center",
        hoverinfo = "skip",
        type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
        mode = 'lines+text', showlegend = FALSE, inherit = FALSE)

    if (!D2) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace( # cone au bout des axes
          data = princ_axes |>
            dplyr::filter(.data$name %in% paste0("Axe ", axes) ) |>
            dplyr::group_by(.data$name) |>
            dplyr::slice(dplyr::n()) |>
            dplyr::ungroup(),
          scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ name,
          u = ~eval(dim1)*9/10, v = ~eval(dim2)*9/10, w = ~eval(dim3)*9/10,
          sizeref = cone_size, sizemode = "absolute",

          colorscale = list(list(0, "black"), list(1, "black")), #autocolorscale = FALSE,
          showscale = FALSE, hoverinfo = "skip",
          # lighting  = list(ambient = 1), lightposition= list(x=0, y=0, z=1e5),
          type = "cone", anchor = "center", #dplyr::if_else(max(princ_axes_print) == 1, "tip", "center"),
          showlegend = FALSE, inherit = FALSE
        )
    }

    # To get a fixed aspect ratio, put a point in max ranges on all axes
    if (!D2) { # Also in 2D ?
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = aspectratio_range, scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),  # color = df$color_col
          hoverinfo = "skip", opacity = 0, visible = TRUE,
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "text", showlegend = FALSE, inherit = FALSE
        )
    }



    # # Plan Axe 1/Axe 2 et projections des points
    # if ("projections" %in% type) {
    #   dual_plots[[i]] <- dual_plots[[i]] |>
    #     plotly::add_trace(data = dplyr::bind_rows(ind_coords, dplyr::mutate(ind_coords, Dim.3 = 0)),
    #                       scene = scene_name[i],
    #                       x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ name,
    #                       line   = list(color  = "#9575cd"), # dash = "longdash", width = 4  #( "dash" | "dashdot" | "dot" | "longdash" | "longdashdot" | "solid" )
    #                       type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
    #                       mode = "lines", showlegend = FALSE, inherit = FALSE,
    #                       hoverinfo = "skip") |>
    #     plotly::add_trace(data = dplyr::mutate(ind_coords, Dim.3 = 0), scene = scene_name[i],
    #                       x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),
    #                       marker   = list(color  = "#9575cd", size = 2),  # "#65499c"
    #                       type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
    #                       mode = "markers", showlegend = FALSE, inherit = FALSE,
    #                       hoverinfo = "skip")
    # }
    #
    #
    # if ("main_plan" %in% type) {
    #   dual_plots[[i]] <- dual_plots[[i]] |>
    #     plotly::add_trace(data = planDf, scene = scene_name[i],
    #                       x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),
    #                       opacity = 0.5, #color = "A", colorscale = c("A" = "#65499c"), #vertexcolor  = "#65499c",
    #                       facecolor = rep('#CFC0E8', nrow(planDf)), # "#65499c"
    #                       hoverinfo = "skip",
    #                       type = "mesh3d", showlegend = FALSE, inherit = FALSE)
    # }

  }





  if (!D2) {
    # Buttons to set plans
    plan12  <- paste0("Plane ", axes[1], "-", axes[2]) # 1-2
    plan13  <- paste0("Plane ", axes[1], "-", axes[3]) # 1-3
    plan23  <- paste0("Plane ", axes[2], "-", axes[3]) # 2-3
    plan123 <-  "All"

    if (!remove_buttons) {
      updatemenus <- list(
        list(
          active = -1,
          # switch(view,
          #        plan12 = 0,
          #        "Plane 1-3" = 1,
          #        "Plane 2-3" = 2,
          #        "All"       = 3,
          #        stop("'view' argument is not recognized")) , # -1,
          type   = 'buttons', # uirevision  = FALSE, # showactive = FALSE, # visible  = TRUE,
          buttons = list(
            list(
              label = plan12,
              method = "relayout",
              args = list(list(scene = list(
                xaxis = axes_params[[axes[1]]],
                yaxis = axes_params[[axes[2]]],
                zaxis = axes_params[[axes[3]]],
                aspectratio = aspectratio, aspectmode = "data",
                camera = list(
                  center = list(x =  0   , y =  0  , z = 0  ),
                  eye    = list(x =  0   , y =  0  , z = base_zoom),
                  up     = list(x =  0   , y =  1  , z = 0  ),
                  projection = "orthographic"
                )
              )
              )) #,
              # args2 = list(list(scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
              #                                 aspectratio = = aspectratio, aspectmode = "data",
              #                                 camera = list(
              #                                   center = list(x =  0   , y =  0  , z = 0  ),
              #                                   eye    = list(x =  0   , y =  0  , z = base_zoom),
              #                                   up     = list(x =  0   , y =  1  , z = 0  )
              #                                 )
              # )
              # ))
            ),
            #list(list(shapes = list(cluster0, c(), c())))),

            list(
              label = plan13,
              method = "relayout",
              args = list(list(scene = list(
                xaxis = axes_params[[axes[1]]],
                yaxis = axes_params[[axes[2]]],
                zaxis = axes_params[[axes[3]]],
                aspectratio = aspectratio, aspectmode = "data",
                camera = list(
                  center = list(x =  0   , y =  0  , z = 0  ),
                  eye    = list(x =  0   , y = -base_zoom  , z = 0  ),
                  up     = list(x =  0   , y =  0  , z = 1  ),
                  projection = "orthographic"
                )
              )
              )
              ) #,
              # args2 = list(list(scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
              #                        aspectratio = = aspectratio, aspectmode = "data",
              #                        camera = list(
              #                          center = list(x =  0   , y =  0  , z = 0  ),
              #                          eye    = list(x =  0   , y = -base_zoom  , z = 0  ),
              #                          up     = list(x =  0   , y =  0  , z = 1  )
              #                        )
              # )
              # )
              #)
            ),
            #list(list(shapes = list(c(), cluster1, c())))),

            list(
              label = plan23,
              method = "relayout",
              args = list(list(scene = list(
                xaxis = axes_params[[axes[1]]],
                yaxis = axes_params[[axes[2]]],
                zaxis = axes_params[[axes[3]]],
                aspectratio = aspectratio, aspectmode = "data",
                camera = list(
                  center = list(x =  0   , y =  0  , z = 0  ),
                  eye    = list(x =  base_zoom , y =  0  , z = 0  ),
                  up     = list(x =  0   , y =  0  , z = 1  ),
                  projection = "orthographic"
                )
              )
              )
              ) #,
              # args2 = list(list(scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
              #                               aspectratio = = aspectratio, aspectmode = "data",
              #                               camera = list(
              #                                 center = list(x =  0   , y =  0  , z = 0  ),
              #                                 eye    = list(x =  base_zoom , y =  0  , z = 0  ),
              #                                 up     = list(x =  0   , y =  0  , z = 1  )
              #                               )
              # )
              # )
              # )
            ),

            list(
              label = plan123,
              method = "relayout",
              args = list(list(scene = list(
                xaxis = axes_params[[axes[1]]],
                yaxis = axes_params[[axes[2]]],
                zaxis = axes_params[[axes[3]]],
                aspectratio = aspectratio, aspectmode = "data",
                camera = list(
                  center = list(x =  0   , y =  0  , z = 0  ),
                  eye    = list(x = base_zoom/6, y = -base_zoom, z = base_zoom),
                  #         list(x =  0.6 * base_zoom, y = -0.4 * base_zoom, z = 0.7 * base_zoom),
                  up     = list(x =  0   , y =  0  , z = 1  ),
                  projection = "orthographic"
                )
              )
              )) #,
              # args2 = list(list(scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
              #                               aspectratio = = aspectratio, aspectmode = "data",
              #                               camera = list(
              #                                 center = list(x =  0   , y =  0  , z = 0  ),
              #                                 eye    = list(x = base_zoom/6, y = -base_zoom, z = base_zoom),
              #                                 #         list(x =  0.6 * base_zoom, y = -0.4 * base_zoom, z = 0.7 * base_zoom),
              #                                 up     = list(x =  0   , y =  0  , z = 1  )
              #                               )
              # )
              # ))
            ) # ,
            #list(list(shapes = c()))),

          )
        )
      )

    } else {
      updatemenus <- NULL
    }


  } else { # 2D
    updatemenus <- NULL
  }


  if (!D2 & !missing(camera_view)) {
    scenes <- purrr::map(camera_view,
                         ~ list(
                           xaxis = axes_params[[axes[1]]],
                           yaxis = axes_params[[axes[2]]],
                           zaxis = axes_params[[axes[3]]],
                           aspectratio = aspectratio, aspectmode = "data",

                           #aspectratio = list(x = 1, y = 1, z = 1),
                           #domain = list(x = c(0, 0.5), y = c(0, 1)),
                           camera = .x
                         )
    )

  } else if (!D2) {
    scenes <- list("scene" = dplyr::case_when(
      view == plan12   ~ list(
        xaxis = axes_params[[axes[1]]],
        yaxis = axes_params[[axes[2]]],
        zaxis = axes_params[[axes[3]]],
        aspectratio = aspectratio, aspectmode = "data",
        camera = list(
          center = list(x =  0   , y =  0  , z = 0  ),
          eye    = list(x =  0   , y =  0  , z = base_zoom),
          up     = list(x =  0   , y =  1  , z = 0  ),
          projection = "orthographic"
        )
      ),
      view == plan13   ~ list(
        xaxis = axes_params[[axes[1]]],
        yaxis = axes_params[[axes[2]]],
        zaxis = axes_params[[axes[3]]],
        aspectratio = aspectratio, aspectmode = "data",
        camera = list(
          center = list(x =  0   , y =  0  , z = 0  ),
          eye    = list(x =  0   , y =  - base_zoom  , z = 0),
          up     = list(x =  0   , y =  0  , z = 1  ),
          projection = "orthographic"
        )
      ),
      view == plan23   ~ list(
        xaxis = axes_params[[axes[1]]],
        yaxis = axes_params[[axes[2]]],
        zaxis = axes_params[[axes[3]]],
        aspectratio = aspectratio, aspectmode = "data",
        camera = list(
          center = list(x =  0   , y =  0  , z = 0  ),
          eye    = list(x = base_zoom, y =  0  , z = 0),
          up     = list(x =  0   , y =  0  , z = 1  ),
          projection = "orthographic"
        )
      ),
      view == plan123  ~ list(
        xaxis = axes_params[[axes[1]]],
        yaxis = axes_params[[axes[2]]],
        zaxis = axes_params[[axes[3]]],
        aspectratio = aspectratio, aspectmode = "data",
        camera = list(
          center = list(x =  0   , y =  0  , z = 0  ),
          eye    = list(x = base_zoom/6, y = -base_zoom, z = base_zoom),
          #         list(x =  0.6 * base_zoom, y = -0.4 * base_zoom, z = 0.7 * base_zoom),
          up     = list(x =  0   , y =  0  , z = 1  ),
          projection = "orthographic"
        )
      ),

      TRUE ~ list(a = NULL)
    )

    )
    #print(scenes)
    if (is.null(scenes$scene[[1]])) stop(paste0("view argument must be among: ",
                                                paste0(
                                                  paste0("'", c(plan12, plan13, plan23, plan123), "'"),
                                                  collapse = ", "),
                                                collapse = ""
    ))


  } else {  # 2D
    scenes <- list("scene" = list(
      xaxis = axes_params[[axes[1]]],
      yaxis = axes_params[[axes[2]]],
      aspectratio = aspectratio, aspectmode = "data" #,
    ))
  }


  final_plots <-
    plotly::subplot(purrr::list_flatten(dual_plots), margin = 0.1, #0,
                    nrows = ceiling(length(scene_name)/2L)
    )
  final_plots <- do.call(plotly::layout,

                         c(list(p           = final_plots,
                                margin      = list(b = 0, l = 0, r = 0, t = 0),
                                updatemenus = updatemenus
                         ),
                         if (missing(title)) {NULL} else {list(title = title)},
                         scenes
                         )
  )

  #print(aspectratio)


  # final_plots$data$ind_coords         <- ind_coords
  # final_plots$data$base_axis_in_princ <- base_axis_in_princ
  # final_plots$data$princ_axes         <- princ_axes
  # final_plots$data$mean_projs         <- mean_projs
  # final_plots$data$planDf             <- planDf

  final_plots

  # plotly::layout(#title = "Title",
  #   scene = list(title = ,
  #                xaxis = axx, yaxis = axy, zaxis = axz,
  #                #domain = list(x = c(0, 0.5), y = c(0, 1)),
  #                    camera = list(
  #                      center = list(x =  0   , y =  0  , z = 0  ),
  #                      eye    = list(x =  0   , y =  0  , z = 1),
  #                      up     = list(x =  0   , y =  1  , z = 0  )
  #                    )
  #       )#,
  #     ) |>
  #     plotly::layout(#title = "Title",
  #       scene2 = list(title = ,
  #                     xaxis = axx, yaxis = axy, zaxis = axz,
  #                     #domain = list(x = c(0.5, 1), y = c(0, 1)),
  #                     camera = list(
  #                       center = list(x =  0   , y =  0  , z = 0  ),
  #                       eye    = list(x =  0   , y = -1  , z = 0  ),
  #                       up     = list(x =  0   , y =  0  , z = 1  )
  #                     )
  #       )
  #     )


}

















#' Benzecri's modified rate of variance
#'
#' @param res.mca The result of \link[FactoMineR]{MCA}.
#' @param fmt By default, the result is given as a numeric vector. Set to `TRUE` to have
#' a \pkg{tabxplor} \code{link[tabxplor]{fmt}} vector instead.
#'
#' @return A numeric vector (or fmt vector with `fmt = TRUE`).
#' @export
#'
#' @examples
#' data(tea, package = "FactoMineR")
#' res.mca <- MCA2(tea, active_vars = 1:18)
#' benzecri_mrv(res.mca)
benzecri_mrv <- function(res.mca, fmt = FALSE) {
  Q   <- length(res.mca$call$quali)
  eig <- purrr::keep(res.mca$eig[, 1], res.mca$eig[, 1] > 1/Q)
  eig <- (Q/(Q-1))^2 * (eig - 1/Q)^2
  eig <- eig/sum(eig)

  if (fmt) {
    tabxplor::fmt(pct = eig, n = 0, type = "all")
  } else {
    purrr::set_names(eig * 100, paste0("Dim ", 1:length(eig)) )
  }
}











#' Helper table to interpret multiple correspondence analysis
#' @description A table to help to interpret the meaning of axes in multiple
#' correspondence analysis (MCA), based on Brigitte Le Roux, \emph{Analyse geometrique des
#' donnees multidimensionnelles}, Dunod, Paris, 2014 / Brigitte Le Roux and Henri Rouanet,
#' \emph{Geometric data analysis : from correspondence analysis to structured data
#' analysis}, Kluwer, Boston, 2004. Only levels whose relative contribution to the
#' variance of axis is superior to the mean contribution are kept. The spread between
#' positive levels and negative levels of the same variable is calculated in percentages
#' of the variance of the question/variable.
#' @param res.mca An object created with \code{FactoMineR::\link[FactoMineR]{MCA}},
#' @param axes The axes to interpret, as an integer vector. Default to the first five axes.
#' @param type By default, a html table is printed. Set to \code{"console"} to print in
#' console or axes the numbers as a data.frame.
#'
#' @return An html table (or a \code{tibble}).
#' @export
#' @examples \donttest{
#' data(tea, package = "FactoMineR")
#' res.mca <- MCA2(tea, active_vars = 1:18)
#' mca_interpret(res.mca)
#' }
mca_interpret <- function(res.mca = res.mca,
                          axes = 1:min(res.mca$call$ncp, 5),
                          type = c("html", "console")) {
  if (type[1] == "html") requireNamespace("kableExtra", quietly = TRUE)

  contrib1 <- res.mca$var$contrib[,axes] %>%
    tibble::as_tibble(rownames = "levels") %>%
    tidyr::pivot_longer(-.data$levels, names_prefix ="Dim ", names_to = "Axe",
                        values_to = "Contrib_mod") %>%
    dplyr::select(.data$Axe, tidyselect::everything()) %>% dplyr::arrange(.data$Axe) %>%
    dplyr::mutate(eig_value = res.mca$eig[as.integer(.data$Axe),1],
                  pct       = round(res.mca$eig[as.integer(.data$Axe),2], 1))

  data <- res.mca$call$X[res.mca$call$quali]
  var_names <- names(data) %>% purrr::set_names(.)
  var_names <- purrr::map(var_names, ~ levels(dplyr::pull(data, .x)) ) %>%
    purrr::imap(
      ~ rep(.y, length(.x)) %>% purrr::set_names(.x)
    ) %>%
    purrr::flatten_chr()

  contrib1 <- contrib1 %>%
    dplyr::mutate(Question = var_names[.data$levels]) %>%
    dplyr::group_by(.data$Axe, .data$Question) %>%
    dplyr::mutate(contrib_q = sum(.data$Contrib_mod))

  #Coordonnees et frequences des levels (pour calculer contribution des ecarts)
  coord_fk <- dplyr::left_join(
    tibble::as_tibble(res.mca$var$coord[, axes], rownames = "levels"),
    tibble::tibble(levels = names(res.mca$call$marge.col),
                   fk = res.mca$call$marge.col),
    by = "levels"
  ) %>%
    tidyr::pivot_longer(c(-.data$levels, -.data$fk),
                        names_prefix = "Dim ", names_to = "Axe",
                        values_to = "coord") %>%
    dplyr::arrange(.data$Axe)


  #Choisir les levels > a la moyenne, trier par coordonnees positives/negatives
  contribsup <- contrib1 %>% dplyr::left_join(coord_fk, by = c("Axe", "levels")) %>%
    dplyr::with_groups(NULL, ~ dplyr::mutate(., mean_ctr = mean(.data$Contrib_mod))) %>%
    dplyr::filter(.data$Contrib_mod >= .data$mean_ctr) %>%
    dplyr::arrange(.data$Axe, dplyr::desc(.data$contrib_q),
                   dplyr::desc(.data$Contrib_mod)) %>%
    #dplyr::arrange(dplyr::desc(contrib_q)) %>%
    dplyr::mutate(levels_2 = .data$levels, ctr_neg = .data$Contrib_mod,
                  ctr_pos  = .data$Contrib_mod, fneg = .data$fk, fpos = .data$fk,
                  coord_neg = .data$coord, coord_pos = .data$coord) %>%
    dplyr::select(-.data$Contrib_mod) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("levels", "ctr_neg", "fneg",
                                                     "coord_neg")),
                                ~ ifelse(.data$coord <= 0, ., NA))) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("levels_2", "ctr_pos", "fpos",
                                                     "coord_pos")),
                                ~ ifelse(.data$coord > 0, ., NA))) %>%
    dplyr::ungroup()


  #Ajouter les ecarts par Question (en % de la contribution de la question) :
  contribsup <- contribsup %>%
    dplyr::group_by(.data$Axe, .data$Question) %>%
    dplyr::mutate(coord_ecart_neg = stats::weighted.mean(.data$coord_neg,.data$fneg,
                                                         na.rm = TRUE),
                  coord_ecart_pos = stats::weighted.mean(.data$coord_pos,.data$fpos,
                                                         na.rm = T),
                  poids_ecart_neg = sum(.data$fneg, na.rm = T),
                  poids_ecart_pos = sum(.data$fpos, na.rm = T)  ) %>%
    dplyr::mutate(poids_ecart = 1/( 1/.data$poids_ecart_neg + 1/.data$poids_ecart_pos) ) %>%
    dplyr::mutate(spread = .data$poids_ecart * 100 *
                    (.data$coord_ecart_pos - .data$coord_ecart_neg)^2 /
                    (.data$eig_value*.data$contrib_q/100  ) ) %>%
    dplyr::select(-.data$coord,-.data$fk,-.data$coord_ecart_neg, -.data$coord_ecart_pos,
                  -.data$poids_ecart_neg, -.data$poids_ecart_pos, -.data$poids_ecart) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(spread = ifelse(is.na(.data$spread), NA, .data$spread) )

  #Contributions totales (positif/negatif sur l'axe), contrib de l'ecart total :
  total <- contribsup %>%
    dplyr::group_by(.data$Axe) %>%
    dplyr::summarise(ctr_neg = sum(.data$ctr_neg, na.rm = TRUE),
                     ctr_pos = sum(.data$ctr_pos, na.rm = TRUE),
                     coord_neg = stats::weighted.mean(.data$coord_neg, .data$fneg,
                                                      na.rm = TRUE),
                     coord_pos = stats::weighted.mean(.data$coord_pos, .data$fpos,
                                                      na.rm = TRUE),
                     poids_neg = sum(.data$fneg, na.rm = TRUE),
                     poids_pos = sum(.data$fpos, na.rm = TRUE),
                     poids_ecart = 1/( 1/.data$poids_neg + 1/.data$poids_pos), #fii' = 1/(1/fi + 1/fi').
                     spread = .data$poids_ecart * 100 *
                       (.data$coord_pos - .data$coord_neg)^2/mean(.data$eig_value) # = fii' (y l - y ')^2/??l )
    ) %>% dplyr::select(-.data$coord_neg, -.data$coord_pos, -.data$poids_neg,
                        -.data$poids_pos, - .data$poids_ecart) %>%
    tibble::add_column(Question = "All levels") %>%
    dplyr::mutate(contrib_q = .data$ctr_neg + .data$ctr_pos)

  # #Total general (contributions sur l'axe positif + sur l'axe negatif)
  # total2 <- contribsup %>%
  #   dplyr::group_by(.data$Axe) %>%
  #   dplyr::summarise(contrib_q = sum(.data$ctr_neg, na.rm = TRUE) + sum(.data$ctr_pos, na.rm = TRUE))
  # #total2 <-  dplyr::bind_rows(total2, total2["Axe"])

  final_tab <- contribsup %>%
    dplyr::select(-.data$fneg, -.data$fpos, -.data$coord_neg, -.data$coord_pos,
                  -.data$eig_value) %>%
    dplyr::bind_rows(total) %>%
    dplyr::arrange(.data$Axe) %>%
    dplyr::select(tidyselect::all_of(c("Axe", "pct", "Question", "contrib" = "contrib_q",
                                       "Positive_levels" = "levels_2", "  " =  "ctr_pos",
                                       "Negative_levels" = "levels", "   " = "ctr_neg",
                                       "spread")))

  if (type[1] == "html") {
    final_tab <- final_tab %>% dplyr::group_by(.data$Axe)

    new_group <- dplyr::group_indices(final_tab)
    new_group <- which(new_group != dplyr::lag(new_group, default = 0))

    last_row <- nrow(final_tab)

    totrows   <- final_tab %>%
      dplyr::mutate(row = dplyr::row_number(),
                    row = row == max(row)) %>%
      dplyr::pull(row) %>% which()

    questions <- final_tab %>% dplyr::group_by(.data$Axe, .data$Question) %>%
      dplyr::group_indices()
    questions <- which(questions != dplyr::lag(questions, default = 0) &
                         !is.na(dplyr::pull(final_tab, .data$Question)))
    questions <- questions[!questions %in% new_group]


    final_tab <- final_tab %>%
      dplyr::mutate(dplyr::across(where(is.numeric),
                                  ~ tidyr::replace_na(stringr::str_c(round(., 1), "%"), ""))) %>%
      dplyr::mutate(dplyr::across(where(is.character),
                                  ~ tidyr::replace_na(., ""))) %>%
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(c("Question", "contrib", "spread")),
        ~ dplyr::if_else(condition = .data$Question != dplyr::lag(.data$Question, default = ".novalue."),
                         true      = .,
                         false     = "")
      )) %>%
      dplyr::mutate(Axe = dplyr::case_when(
        condition = dplyr::row_number() == 1    ~ paste0("Axe ", .data$Axe, ": ", pct),
        condition = dplyr::row_number() == 2    ~ "of variance",
        TRUE                             ~ ""
      )) %>%
      dplyr::rename(" " = "Axe") %>%
      dplyr::select(-tidyselect::all_of("pct"))


    final_tab <- final_tab %>%
      kableExtra::kable(format = "html") %>%
      kableExtra::kable_classic(lightable_options = "hover",
                                #bootstrap_options = c("hover", "condensed", "responsive", "bordered"), #"striped",
                                full_width = FALSE,
                                html_font = "DejaVu Sans Condensed", # row_label_position
                                fixed_thead = TRUE)

    final_tab <- final_tab %>%
      kableExtra::row_spec(
        0, bold = TRUE,
        extra_css = "border-top: 0px solid ; border-bottom: 1px solid ;"
      ) %>%
      kableExtra::row_spec(totrows, bold = TRUE) %>%
      kableExtra::column_spec(c(1, 4, 6, 8), border_left = TRUE) %>%
      kableExtra::column_spec(8, border_right = TRUE) %>%
      kableExtra::row_spec(questions, extra_css = "border-top: 1px solid ;") %>%
      kableExtra::column_spec(1, bold = TRUE,
                              extra_css = "border-top: 0px solid ; border-bottom: 0px solid ;") %>%

      kableExtra::row_spec(new_group, extra_css = "border-top: 2px solid ;") %>%
      kableExtra::row_spec(last_row, extra_css = "border-bottom: 2px solid ;")

  }

  final_tab
}























# # PCA ----


#' Principal Component Analysis
#' @description A user-friendly wrapper around \code{\link[FactoMineR]{PCA}}, made to
#'  work better with \pkg{ggfacto} functions like \code{\link{ggpca_cor_circle}}.
#'  All variables can be selected by many different expressions, in the way of
#'  the `tidyverse`. No supplementary vars are to be provided here,
#'  since they can be added afterward.
#' @param data The data frame.
#' @param active_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> The names
#'  of the active variables.
#' @param wt The name of the row weight variable
#' @param col.w The weights of the columns, as a numeric vector of the same
#' length than `active_vars.`
#' @param ind_name Possibly, a variable with the names of the individuals.
#' @param scale.unit A boolean, if `TRUE` (value set by default) then data are
#' scaled to unit variance.
#' @param ind.sup A vector indicating the indexes of the supplementary individuals.
#' @param ncp Number of dimensions kept in the results (by default 5).
#' @param graph A boolean, set to `TRUE` to display the base graph.
#' @param ... Additional arguments to pass to \code{\link[FactoMineR]{PCA}}.
#'
#' @return A `res.pca` object, with all the data necessary to draw the PCA.
#' @export
#'
#' @examples
#' active_vars <- c("mpg", "cyl", "hp", "drat", "qsec")
#' res.pca <- PCA2(mtcars, tidyselect::all_of(active_vars) )
#'
PCA2 <- function(data, active_vars, wt, col.w = NULL, ind_name, scale.unit = TRUE,
                 ind.sup = NULL, ncp = 5, graph = FALSE, ...) {
  active_vars <- names(tidyselect::eval_select(rlang::enquo(active_vars), data))

  wt <- if (missing(wt)) {character()} else {as.character(rlang::ensym(wt))}
  stopifnot(length(wt) <= 1)
  stopifnot(is.integer(ind.sup) | is.null(ind.sup))

  # if(length(col.w) == 0) {
  #   col.w <- NULL
  #
  # } else if (all(is.na(col.w))) {
  #   col.w <- NULL
  #
  # } else {
  #   col.w <- tidyr::replace_na(col.w, 1L)
  # }


  # ind.sup <- rlang::enquo(ind.sup)


  # vars <- active_vars #c(active_vars, sup_vars, sup_quanti)
  wt   <- if (length(wt) != 0) { data[[wt]] } else {NULL}

  if (!missing(ind_name)) {
    ind_name <- as.character(rlang::ensym(ind_name))
    data <- data |> tibble::column_to_rownames(var = ind_name)

  } else {
    data <- data |> as.data.frame()
  }

  vars_not_num <- purrr::map_lgl(data[active_vars], ~ !is.numeric(.))
  if (any(vars_not_num)) stop(
    paste0("some active variables are not numeric: ",
           paste0(names(vars_not_num)[vars_not_num], collapse = ", ")
    )
  )

  data <- data[active_vars]

  if (length(ind.sup) > 0) wt <- wt[-ind.sup]


  FactoMineR::PCA(data,
                  scale.unit = scale.unit,
                  ncp = ncp,
                  row.w = wt,
                  graph = graph,
                  ind.sup = ind.sup,
                  col.w = col.w,
                  ...)
}

# PCA2(mtcars, 1:5, wt = wt) |>
#   ggpca_cor_circle(interactive = FALSE)
#
# mtcars |> rownames_to_column("name") |> as_tibble() |>
# PCA2(2:8, wt = EXTRID, ind_name = name) |>
#   ggpca_cor_circle(interactive = FALSE)
#
#
# PCA2(ee_sal19_sample, all_of(variables_actives), wt = EXTRID, ind.sup = 1:100) |>
#   ggpca_cor_circle(interactive = FALSE)
#
# PCA2(ee_sal19_sample, all_of(c("SALAIRE", "ADFE", "AGE")),
#      wt = EXTRID, col.w = c(10, 1, 1)) |>
#   ggpca_cor_circle(interactive = FALSE)
#
# PCA2(ee_sal19_sample, ends_with(c("SALAIRE", "ADFE", "AGE")),
#      wt = EXTRID) |>
#   ggpca_cor_circle(interactive = FALSE)





#' Correlation Circle Plot for Principal Component Analysis
#'
#' @param res.pca The result of \code{\link[FactoMineR:PCA]{FactoMineR::PCA}}.
#' @param axes The axes to print, as a numeric vector of length 2.
#' @param proj Set to `TRUE` to print projections of vectors over the two axes.
#' @param interactive By default an html interactive plot is done. Set to `FALSE`
#' to get a normal \code{\link[ggplot2]{ggplot}} graph.
#' @param text_size Size of the texte.
#'
#' @return A \code{\link[ggplot2]{ggplot}}.
#' @export
#'
#' @examples
#'
#' data(mtcars, package = "datasets")
#' mtcars <- mtcars[1:7] |> dplyr::rename(weight = wt)
#' res.pca <- FactoMineR::PCA(mtcars, graph = FALSE)
#' ggpca_cor_circle(res.pca, interactive = FALSE)
#'
ggpca_cor_circle <- function(res.pca, axes = c(1, 2),
                             proj = FALSE, interactive = TRUE, text_size = 3) {
  requireNamespace("plotly", quietly = TRUE)
  if (exists("axes_names", where = res.pca)) {
    first_axe_title  <-
      stringr::str_c(
        "Axe ", axes[1]," (", round(res.pca$eig[axes[1],2], 1),
        "%)",
        if (!is.null(res.pca$axes_names[axes[1]]) ) paste0(" : ", res.pca$axes_names[axes[1]])
      )
    second_axe_title <-
      stringr::str_c(
        "Axe ", axes[2]," (", round(res.pca$eig[axes[2],2], 1),
        "%)",
        if (!is.null(res.pca$axes_names[axes[2]]) ) paste0(" : ", res.pca$axes_names[axes[2]])
      )
  } else {
    first_axe_title  <-
      stringr::str_c("Axe ", axes[1]," (",
                     round(res.pca$eig[axes[1],2], 1), "%)")
    second_axe_title <-
      stringr::str_c("Axe ", axes[2]," (",
                     round(res.pca$eig[axes[2],2], 1), "%)")
  }


  dim1 <- rlang::sym(paste0("Dim.", axes[1]))
  dim2 <- rlang::sym(paste0("Dim.", axes[2]))

  if (interactive) proj <- FALSE

  data_circle <- res.pca$var$coord |> as.data.frame() |> tibble::rownames_to_column("name") |>
    tibble::as_tibble() |> dplyr::mutate(id = as.integer(as.factor(.data$name)))

  #unbrk <- stringi::stri_unescape_unicode("\\u202f") # unbreakable space

  interactive_txt <-
    data_circle |>
    dplyr::select("name", tidyselect::starts_with("Dim.")) |>
    dplyr::mutate(
      name = paste0("<b>", .data$name, "</b>\n"),

      dplyr::across(
        tidyselect::starts_with("Dim."),
        ~ paste0("Coord", unbrk, "Axe", unbrk, stringr::str_sub(dplyr::cur_column(), -1, -1),
                 ":", unbrk,
                 stringr::str_pad(round(., 2), width = 5, side = "left") |>
                   stringr::str_replace_all("-", paste0(unbrk, "-")),
                 " (cor",
                 stringr::str_pad(round(.*100, 0), width = 3, side = "left"),
                 "%)\n") |>
          stringr::str_replace_all("-", paste0(unbrk, "-") ) |>
          stringr::str_replace_all(" ", paste0(unbrk, unbrk, unbrk))
      )) |>
    tidyr::unite(col = "interactive_text", sep = "")


  data_circle <- data_circle |> dplyr::bind_cols(interactive_txt)

  # dplyr::mutate(interactive_text = paste0(
  # "<b>", name, "</b>\n",
  #   "Coord Axe ", axes[1], ": ", round(!!dim1, 2), " (cor ", round(!!dim1*100, 0), "%)\n",
  #   "Coord Axe ", axes[2], ": ", round(!!dim2, 2), " (cor ", round(!!dim2*100, 0), "%)\n"
  # ))
  #
  data_proj   <- dplyr::bind_rows(
    data_circle |>
      dplyr::mutate(name  = paste0("x=", round(!!dim1, 2)),
                    Proj1 = !!dim1,
                    Proj2 = 0),
    data_circle |>
      dplyr::mutate(name  = paste0("y=", round(!!dim2, 2)),
                    Proj1 = 0 ,
                    Proj2 = !!dim2),
  )

  plot_proj_base <- if(proj) {
    list(
      ggplot2::geom_segment(
        ggplot2::aes(xend = .data$Proj1, yend = .data$Proj2),
        data = data_proj,
        linewidth = 0.5, linetype = "dashed"
      ),  # color = "grey30",
      ggrepel::geom_label_repel(
        ggplot2::aes(x = .data$Proj1, y = .data$Proj2, label = .data$name),
        data = data_proj,
        fill = grDevices::rgb(1, 1, 1, alpha = 0.5), label.size = 0.05, fontface = "bold",
        #hjust = "outward",
        size = text_size, nudge_y = -0.075, na.rm = TRUE)
    )
  } else if (interactive) {
    list(
      ggiraph::geom_segment_interactive(
        ggplot2::aes(xend = .data$Proj1, yend = .data$Proj2, data_id = .data$id),
        data = data_proj,
        linewidth = 0.5, linetype = "dashed", color = NA
      )#,

    )
  } else {
    NULL
  }


  plot_proj_int <- if (interactive) {
    list(
      ggiraph::geom_label_repel_interactive(
        ggplot2::aes(x = .data$Proj1, y = .data$Proj2, label = .data$name,
                     data_id = .data$id),
        data = data_proj,
        #fill = grDevices::rgb(1, 1, 1, alpha = 0.7),
        label.size = 0.05, fontface = "bold", size = text_size,
        nudge_y = -0.075, na.rm = TRUE, color = NA, fill = NA)
    )
  } else {
    NULL
  }


  plot_output <- data_circle |>
    ggplot2::ggplot(ggplot2::aes(x = !!dim1, y = !!dim2)) +
    ggforce::geom_circle(ggplot2::aes(x0 = 0, y0 = 0, r = 1), color = "#d32f2f", linewidth = 1) +
    ggplot2::geom_hline(yintercept = 0, color="#d32f2f", linetype = "solid") +
    ggplot2::geom_vline(xintercept = 0, color="#d32f2f", linetype = "solid") +
    ggplot2::labs(x = first_axe_title, y =  second_axe_title) +
    ggplot2::coord_fixed() +
    ggplot2::scale_x_continuous(minor_breaks = seq(-1, 1, by = 0.1)) +
    ggplot2::scale_y_continuous(minor_breaks = seq(-1, 1, by = 0.1)) +
    ggplot2::theme_minimal()  +
    ggplot2::theme(legend.position = "none",
                   panel.grid.minor = ggplot2::element_line(linewidth = 0.3, color="gray80"),
                   panel.grid.major = ggplot2::element_line(linewidth = 0.3, color="gray60"),
                   strip.text = ggplot2::element_text(face = "bold"), #Titles of facets
                   plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"), #Center titre of graph
                   axis.title.x = ggplot2::element_text(size = 12, hjust = 1),
                   axis.title.y = ggplot2::element_text(size = 12, hjust = 1),
                   text = ggplot2::element_text(family = "sans") #"DejaVu Sans Condensed"
    ) +
    plot_proj_base +
    ggplot2::geom_segment(
      ggplot2::aes(xend = !!dim1, yend = !!dim2),
      x = 0, y = 0, color = "#0077c2",
      arrow = grid::arrow(length = ggplot2::unit(0.25, "cm")), linewidth = 1
    ) +
    ggiraph::geom_label_repel_interactive(
      ggplot2::aes(x = !!dim1, y = !!dim2, label = .data$name,
                   tooltip = .data$interactive_text, data_id = .data$id),
      data = data_circle |> dplyr::mutate(!!dim1 := dplyr::if_else(!!dim1 > 0, !!dim1 + 0.03, !!dim1 - 0.03)),
      fill = grDevices::rgb(1, 1, 1, alpha = 0.5), label.size = 0, size = text_size,
      color = "#0077c2", fontface = "bold", hjust = "outward", # nudge_y = -0.1,
      direction = "y", force = 0.5, force_pull = 1, point.padding = 0, box.padding = 0, point.size = NA,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines")),
      min.segment.length = 0.1, #0.4,
      na.rm = TRUE

    ) + # parse = TRUE
    plot_proj_int


  # css_hover <- ggiraph::girafe_css("stroke:orange;stroke-width:2;", #,
  #                                  # line = "line-style:dashed;",
  #                                  text = "color:#000000;stroke:none;" #color:black
  #                                  )
  # # plot_output <- plot_output |> append(c("css_hover" = css_hover)) |>
  #   `attr<-`("class", c("gg", "ggplot"))
  #


  if (interactive) {
    plot_output <- plot_output |> ggi()

  }
  plot_output


}




# ind.size    <- 4
# ind_name.size <- 1
# princ_axes_print = -3:3
# base_axe_n_breaks = 10
# # camera_view = list(
# #   "Plan 1-2" = list(
# #     center = list(x =  0   , y =  0  , z = 0  ),
# #     eye    = list(x =  0   , y =  0  , z = 1),
# #     up     = list(x =  0   , y =  1  , z = 0  )
# #   ),
# #   "Plan 1-3" = list(
# #     center = list(x =  0   , y =  0  , z = 0  ),
# #     eye    = list(x =  0   , y = -1  , z = 0  ),
# #     up     = list(x =  0   , y =  0  , z = 1  )
# #   )
# # )
# # camera_view <- list(
# #  "Base" = list(
# #   center = list(x =  0   , y =  0  , z = 0  ),
# #   eye    = list(x =  0.6 , y = -0.4, z = 0.7),
# #   up     = list(x =  0   , y =  0  , z = 1  )
# # )
# # )
# center = TRUE
# var_names_on = "var"
# base_zoom = 1
# remove_buttons = FALSE
# view = "Plane 1-2"
# aspectratio_from_eig = FALSE
# always_make_ind_tooltips = FALSE
# axes = c(1, 2, 3)
# type = c("var", "ind", "main_plan", "projections")
# var_color = "#4D4D4D"

# camera_view = list(
#   "Plan 1-2" = list(
#     center = list(x =  0   , y =  0  , z = 0  ),
#     eye    = list(x =  0   , y =  0  , z = 1  ),
#     up     = list(x =  0   , y =  1  , z = 0  )
#   ),
#   "Plan 1-3" = list(
#     center = list(x =  0   , y =  0  , z = 0  ),
#     eye    = list(x =  0   , y = -1  , z = 0  ),
#     up     = list(x =  0   , y =  0  , z = 1  )
#   )
# ),






#' Interactive 3D Plot for Principal Component Analyses (plotly::)
#'
#' @param res.pca The result of \code{\link[FactoMineR:PCA]{FactoMineR::PCA}}.
#' @param axes The axes to print, as a numeric vector of length 3 (or 2).
#' @param princ_axes_print The breaks of the principal axes.
#' @param base_axe_n_breaks The number of breaks in initial variables axes.
#' @param ind.size The size of the points of individuals.
#' @param ind_name.size The size of the names of individuals.
#' @param title Plot title.
#' @param center By default the plot is centered on the central point. Set to
#' `FALSE` to center on the origin of all variables (zero coordinates).
#' @param var_names_on By default `"var"` the names of variables are drawn upon
#' the initial axes. Set to `"cor"` to draw them upon correlation vectors instead.
#' @param base_zoom The base level of zoom.
#' @param remove_buttons Set to TRUE to remove buttons to change view.
#' @param cone_size The size of the conic arrow at the end of each axe.
#' @param view The starting point of view (in 3D) :
#'      \itemize{
#'    \item \code{"Plane 1-2"} : Axes 1 and 2.
#'    \item \code{"Plane 1-3"} : Axes 1 and 3.
#'    \item \code{"Plane 2-3"} : Axes 2 and 3.
#'    \item \code{"All"      } : A 3D perspective with Axes 1, 2, 3.
#'  }
#' @param type Which elements of the graph to print, among :
#' #'      \itemize{
#'    \item \code{"var"        } : initial variables axes, with breaks
#'    \item \code{"cor"        } : normalized correlation vectors (length = 1)
#'    \item \code{"cor_sphere" } : a 3D sphere of standard deviation 1
#'    \item \code{"ind"        } : points of individuals
#'    \item \code{"ind_name"   } : names of individuals
#'    \item \code{"main_plan"  } : the plan 1-2.
#'    \item \code{"projections"} : projections of mean point on initial variables
#'    \item \code{"V"          } : vectors of the V transition matrix
#'    \item \code{"vs"         } : vectors of the matrix of singular values
#    \item \code{"all"        } : all elements.
#'  }
#' @param camera_view Possibility to add a (replace `view`)
#' @param aspectratio_from_eig Set to `TRUE` to modify axes length based on
#' eigenvalues.
#' @param always_make_ind_tooltips Set to `TRUE` to add interactive toolips for
#'  individuals.
#' @param var_color The color of the initial variables/dimensions
#' @param max_ind The maximun number of individuals to print.
#' @param max_ind_seed The random seed used to sample individuals.
#'
#' @return A \code{\link[plotly]{plotly}} html interactive 2d or 3d graph.
#' @export
#'
#'@examples
#' \donttest{
#' data(mtcars, package = "datasets")
#' mtcars <- mtcars[1:7] |> dplyr::rename(weight = wt)
#' res.pca <- FactoMineR::PCA(mtcars, graph = FALSE)
#'
#' # Variables and individuals
#' ggpca_3d(res.pca)
#'
#' # Circle of correlation 3D
#' ggpca_3d(res.pca, type = c("cor", "cor_sphere"),
#'          var_names_on = "cor", base_zoom = 0.6,
#'          princ_axes_print = -1:1, view = "All"
#' )
#' }
ggpca_3d <- function(res.pca, axes = c(1, 2, 3),
                     princ_axes_print = -3:3, base_axe_n_breaks = 10,
                     ind.size = 4, ind_name.size = 3, title, center = TRUE,
                     var_names_on = "var", #"cor",
                     base_zoom = 1, remove_buttons = FALSE, cone_size = 0.33,
                     view = "All", # c("Plane 1-2", "Plane 1-3", "Plane 2-3", "All")
                     type = c("var", "ind", "main_plan", "projections"),
                     camera_view, aspectratio_from_eig = FALSE,
                     always_make_ind_tooltips = FALSE,
                     var_color = "#4D4D4D", max_ind = 500, max_ind_seed
) {
  requireNamespace("plotly", quietly = TRUE)

  D2 <- length(axes) == 2 ; stopifnot(length(axes) %in% 2:3 )
  if (D2) axes <- c(axes, NA)

  ind_coords <- res.pca$ind$coord |> as.data.frame() |>
    tibble::rownames_to_column("name") |>
    tibble::as_tibble() |>
    dplyr::left_join(tibble::rownames_to_column(res.pca$call$X, "name"),
                     by = "name") |>
    tibble::add_column(wt = res.pca$call$row.w)

  if (max_ind < nrow(ind_coords)) {
    if (!missing(max_ind_seed)) set.seed(max_ind_seed)
    ind_coords <- ind_coords |> dplyr::slice_sample(n = max_ind)
  }


  df_base <- res.pca$call$X |>
    tibble::rownames_to_column("name") |>
    tibble::as_tibble()

  if (!is.null(res.pca$call$quali.sup) ) {
    quali_sup <- names(res.pca$call$quali.sup$quali.sup)
    df_base <- df_base |> dplyr::select(-tidyselect::all_of(quali_sup))
  } else {
    quali_sup <- character()
  }
  if (!is.null(  res.pca$call$quanti.sup) ) {
    quanti.sup <- names(res.pca$call$quanti.sup)
    df_base <- df_base |> dplyr::select(-tidyselect::all_of(quanti.sup))
  } else {
    quanti.sup <- character()
  }

  active_vars <- colnames(df_base)[!colnames(df_base) == "name"]

  active_means <- if(center) {
    res.pca$call$centre |> purrr::set_names(active_vars)
  } else {
    rep(0, length(res.pca$call$centre)) |> purrr::set_names(active_vars)
  }

  # # (Probleme : res.pca$var n'est pas tout a fait aligne avec les axes de depart...)
  # # (ca ne vient pas non plus de la fonction de calcul des ind.sup : resultats identiques)
  # # Solution : var_coords est fait pour le cercle des correlations, ou il faut corriger
  # #  les vecteurs normes en fonction de l'importance des axes ?
  var_coords <- res.pca$var$coord |> as.data.frame() |>
    tibble::rownames_to_column("name")
  # # # manually :
  # # coord.var <- t(t(as.matrix(res.pca$svd$V)) * res.pca$svd$vs) # vs^2 = eig # eig <- eig[1:ncp]
  # # rownames(coord.var) <- active_vars ; colnames(coord.var) <- paste("Dim",  c(1:ncol(res.pca$svd$V)), sep = ".")
  # # coord.var
  #
  V_coords <-  res.pca$svd$V |> as.data.frame()
  colnames(V_coords) <- colnames(var_coords[, names(var_coords) != "name"])
  V_coords <- V_coords |>
    tibble::add_column(name = var_coords$name, .before = 1)
  #
  # # The operation is this one (and is ok) :
  # var_coords
  # V_coords |>
  #   dplyr::mutate(Dim.1 = Dim.1*res.pca$svd$vs[1],
  #          Dim.2 = Dim.2*res.pca$svd$vs[2],
  #          Dim.3 = Dim.3*res.pca$svd$vs[3],
  #          )
  #
  # # # Par rapport a Brigitte Le Roux, Analyse geometrique..., (cf. Chap. 6 Exercice 6.1)
  # #                # X0: coordonnees de base centrees.
  # # scale.unit = T # X0r: X0 * 1/sqrt(vjj) : coordonnees c. reduites (matrice diag des variances)
  # # res.pca$eig    # Lambda "\\u039b" : matrice diagonale des valeurs propres
  # # res.pca$svd$vs # Ksi "\\u039e" : matrice diagonale des valeurs singulieres (mais, ici, vecteur) ;
  # #                      (les elements sont notes Ksi-j  "\\u03be" )
  # # res.pca$svd$U
  # # res.pca$svd$V  # A: matrice des vecteurs propres normes.
  # # ind$coord      # Y: coordonnees principales. Y = X0A    Inv: X0=Yt(A) (ou X0r)
  # # var$coords     # B: coeffs de regression B = A Ksi (b1j = sqrt(Lamda1) * a1j = Ksi1 * a1j)
  #
  # # # var$coords calculation is equivalent to Britte Le Roux
  # # t(t(as.matrix(res.pca$svd$V)) * res.pca$svd$vs)
  # # as.matrix(res.pca$svd$V) %*% diag(res.pca$svd$vs)
  #
  # # # var_coord et V sont tous les deux sur la sphere de rayon 1 ecart-type
  # # # (mais la sphere est aplatie le long de l'axe 2 : pas de fixed ratio ?)
  # # # install.packages("pracma")
  # # theta = seq(0, pi, length.out = 25)
  # # phi   = seq(-pi,   pi, length.out = 25)
  # # meshgrid_sphere <- pracma::meshgrid(theta, phi)
  # # theta <- meshgrid_sphere[[1]] #|> c()
  # # phi   <- meshgrid_sphere[[2]] #|> c()
  # # r = 1
  # # sphere_data <- tibble::tibble(
  # #   x = r * sin(theta) * cos(phi),
  # #   y = r * sin(theta) * sin(phi),
  # #   z = r * cos(theta),
  # # )
  # #
  # # SALAIRE_3D_cor |>
  # #   plotly::add_surface(data = sphere_data,
  # #     x = ~x, y = ~y, z = ~z,
  # #     opacity = 0.3,
  # #     colorscale = list(list(0, "#ba2d65"), list(1, "#ba2d65")), showscale = FALSE,
  # #     showlegend = FALSE, inherit = FALSE
  # #   )
  #
  # ## code :
  # # # (cor is the same by default, with scale.unit = TRUE)
  # # X <- t(t(as.matrix(X)) - centre)
  # # if (scale.unit) {
  # #   ecart.type <- ec.tab(X, row.w)
  # #   X <- t(t(X)/ecart.type)
  # # }
  # # dist2.var <- as.vector(crossprod(rep(1, nrow(X)), as.matrix(X^2 * row.w)))
  # # cor.var <- coord.var/sqrt(dist2)



  base_axis_coords <-
    purrr::imap_dfr(
      dplyr::select(df_base, -"name"),
      ~ tibble::tibble(
        !!rlang::sym(.y) := scales::breaks_extended(n = base_axe_n_breaks)(if (center) {.x} else {c(0, .x)}),
        name       = .y,
        base_coord = !!rlang::sym(.y)
      )
    ) |>
    dplyr::select("name", tidyselect::everything()) |>
    dplyr::mutate(dplyr::across(
      tidyselect::everything(),
      ~ tidyr::replace_na(., active_means[dplyr::cur_column()])
    ),
    name = paste0(.data$name, "_", .data$base_coord)
    )

  ind.sup_coords <- base_axis_coords |> dplyr::select(-"base_coord") |>
    PCA_ind.sup_coord(res.pca) # center = !center

  base_axis_in_princ <- ind.sup_coords |>
    as.data.frame() |> tibble::rownames_to_column("name") |>
    # dplyr::filter(!stringr::str_detect(name, "mean$")) |>
    dplyr::left_join(base_axis_coords, by = "name") |>
    dplyr::mutate(
      name    = forcats::as_factor(stringr::str_remove(.data$name, "_[^_]+$")),
      pair_id = as.integer(as.factor(.data$name)) #,
    )


  # plot_ly(x=res.pca$ind$coord[, "Dim.1"], y=res.pca$ind$coord[, "Dim.2"], z=res.pca$ind$coord[, "Dim.3"], type="scatter3d", mode="markers")


  princ_axes <-
    colnames(ind_coords)[stringr::str_detect(colnames(ind_coords), "Dim.")] |>
    purrr::map_dfr(~ tibble::tibble(
      !!rlang::sym(.x) := princ_axes_print,
      base_coord = princ_axes_print,
      name       = stringr::str_replace(.x, "Dim.", "Axe "),
    )
    ) |>
    dplyr::mutate(
      dplyr::across(tidyselect::starts_with("Dim."), ~ tidyr::replace_na(., 0)),
      name    = forcats::as_factor(.data$name),  #name = paste0(name, ".", base_coord)
      pair_id = as.integer(.data$name),

    ) |>
    dplyr::select("name", "pair_id", "base_coord", tidyselect::starts_with("Dim.") )


  # Coordonnees des projections du point moyen sur les axes de depart dans princ
  mean_point <- res.pca$call$centre |> diag() |> as.data.frame()
  colnames(mean_point) <- active_vars ; rownames(mean_point) <- active_vars
  mean_projs <- PCA_ind.sup_coord(mean_point, res.pca)
  # mean_projs <- mapply(FUN = `*`, as.data.frame(mean_projs), res.pca$call$ecart.type) |>
  mean_projs <- mean_projs |> as.data.frame() |>
    tibble::rownames_to_column("name") |> tibble::as_tibble()
  mean_projs <- dplyr::bind_rows(
    mean_projs,
    dplyr::mutate(mean_projs, dplyr::across(where(is.double), ~ 0))
  ) |>
    dplyr::arrange(.data$name)

  plot_range <-
    dplyr::bind_rows(
      dplyr::select(ind_coords, tidyselect::starts_with("Dim.")),
      dplyr::select(base_axis_in_princ, tidyselect::starts_with("Dim."))
    ) |>
    purrr::map(~ range(.) |> abs() |> max())
  plot_range <- plot_range  |> purrr::map(~ c(-., .))


  #     Plan 1-2
  planDf <-
    dplyr::bind_cols(
      data.frame(Dim.1 = rep(range(princ_axes_print), 2),
                 Dim.2 = rep(range(princ_axes_print), each = 2)
      ) |> tibble::as_tibble(),

      dplyr::select(princ_axes, tidyselect::starts_with("Dim.") &
                      -tidyselect::all_of(c("Dim.1", "Dim.2")) ) |>
        dplyr::slice(1:4) |>
        dplyr::mutate(dplyr::across(tidyselect::everything(), ~ 0L))
    )


  # Tooltips at point hover
  if ("ind" %in% type | always_make_ind_tooltips) {


    ind_tooltips_active_vars <- ind_coords |>
      dplyr::select("name", tidyselect::all_of(active_vars)) |>
      dplyr::mutate(
        dplyr::across(tidyselect::all_of(active_vars), # -name,
                      ~ format(., justify = "right", digits = 1, big.mark = " ", trim = TRUE) #,  # nsmall = 0,
        ),

        dplyr::across(tidyselect::all_of(active_vars), stringr::str_length, .names = "{.col}_length_str"),

        max_length = pmax(!!!rlang::syms(paste0(active_vars, "_length_str")), na.rm = TRUE),

        dplyr::across(
          tidyselect::all_of(active_vars),
          ~ paste0(dplyr::cur_column(), ": ",
                   stringr::str_pad(., width = max_length,  side = "left")) |>
            stringr::str_replace("(^[^\\.]+\\.)", paste0(unbrk, "\\1") ) #,
        )
      ) |>
      dplyr::select(-tidyselect::ends_with("_length_str"), -"max_length")


    ind_tooltips_active_diff <- ind_coords |>
      dplyr::select("name", tidyselect::all_of(active_vars)) |>
      dplyr::mutate(
        dplyr::across(tidyselect::all_of(active_vars), ~ . - active_means[dplyr::cur_column()]),

        dplyr::across(tidyselect::all_of(active_vars),
                      ~ paste0(dplyr::if_else(. >= 0, "+", ""), #paste0(unbrk, "-")
                               format(., justify = "right", digits = 1, big.mark = " ", trim = TRUE) ) |>
                        stringr::str_replace("\\+ ", "\\+")
        ),

        dplyr::across(tidyselect::all_of(active_vars), stringr::str_length, .names = "{.col}_length_str"),
        max_length = pmax(!!!rlang::syms(paste0(active_vars, "_length_str")), na.rm = TRUE),

        dplyr::across(tidyselect::all_of(active_vars),
                      ~ stringr::str_pad(., width = max_length,  side = "left") |>
                        stringr::str_replace("-", paste0(unbrk, "-")) |>
                        stringr::str_replace("(^[^\\.]+\\.)", paste0(unbrk, "\\1") )
        )
      ) |>
      dplyr::select(-tidyselect::ends_with("_length_str"), -"max_length") |>
      dplyr::rename_with(~ paste0(., "_diff"), .cols = tidyselect::all_of(active_vars))

    ind_tooltips_active_vars <-  ind_tooltips_active_vars |>
      dplyr::left_join(ind_tooltips_active_diff, by = "name") |>
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(active_vars),
        ~ paste0(., " (", rlang::eval_tidy(rlang::sym(paste0(dplyr::cur_column(), "_diff"))), ")<br>" )
      )) |>
      dplyr::select(-tidyselect::ends_with("_diff"))


    ind_contrib <- res.pca$ind$contrib |> as.data.frame() |> tibble::rownames_to_column("name") |>
      tibble::as_tibble() |>
      dplyr::rename_with(~ stringr::str_replace(., "Dim.", "ctr."), .cols = tidyselect::starts_with("Dim."))

    ind_cos2 <- res.pca$ind$cos2 |> as.data.frame() |> tibble::rownames_to_column("name") |>
      tibble::as_tibble() |>
      dplyr::rename_with(~ stringr::str_replace(., "Dim.", "cos2."), .cols = tidyselect::starts_with("Dim."))


    ind_tooltips_coords <-  ind_coords |>
      dplyr::select("name", tidyselect::starts_with("Dim.")) |>
      dplyr::left_join(ind_contrib, by = "name") |>
      dplyr::left_join(ind_cos2, by = "name") |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::starts_with("Dim."),
          ~ paste0("Axe ", stringr::str_remove(dplyr::cur_column(), "Dim."),  ": " ,
                   format(round(., 1), justify = "right", nsmall = 1)#,  # nsmall = 0,
          )
        ),

        dplyr::across(
          tidyselect::starts_with("ctr."),
          ~ paste0(" ; contrib: ",
                   format(round(., 1), justify = "right", nsmall = 1),  # nsmall = 0,
                   "%"
          )
        ),

        dplyr::across(
          tidyselect::starts_with("cos2."),
          ~ paste0(" ; cos2: ",
                   format(round(.*100, 0), justify = "right", nsmall = 0),  # nsmall = 0,
                   "%<br>"
          )
        ),

        dplyr::across(tidyselect::any_of(dplyr::first(tidyselect::starts_with("Dim."))), ~ paste0("<br>", .) )
      )
    ind_coords_order <- purrr::set_names(names(ind_tooltips_coords)[-1] |> stringr::str_sub(-1, -1) ,
                                         names(ind_tooltips_coords)[-1]
    ) |> sort() |> names()
    ind_tooltips_coords <- ind_tooltips_coords |>
      dplyr::select("name", tidyselect::all_of(ind_coords_order))

    ind_tooltips_quali_sup <- ind_coords |>
      dplyr::select("name", tidyselect::all_of(quali_sup)) |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(quali_sup),
          ~ paste0(dplyr::cur_column(), ": ", ., "<br>") # stringr::str_pad(., max(stringr::str_length(.)), side = "right")
        ),

        dplyr::across(
          tidyselect::any_of(dplyr::first(quali_sup, default = "NO__VAR_:")),
          ~ paste0("<br><b>Quali sup:</b><br>", .)
        )
      )

    ind_tooltips_quanti_sup <- ind_coords |>
      dplyr::select("name", tidyselect::all_of(quanti.sup)) |>
      dplyr::mutate(
        dplyr::across(tidyselect::all_of(quanti.sup),
                      ~ paste0(dplyr::cur_column(), ": " ,
                               format(., justify = "right", digits = 1, big.mark= " "),
                               "<br>"
                      )
        ),

        dplyr::across(tidyselect::any_of(dplyr::first(quanti.sup, default = "NO__VAR_:")),
                      ~ paste0("<br><b>Quanti sup:</b><br>", .)
        )
      )

    ind_tooltips <- ind_tooltips_active_vars |>
      dplyr::left_join(ind_tooltips_coords    , by = "name") |>
      dplyr::left_join(ind_tooltips_quali_sup , by = "name") |>
      dplyr::left_join(ind_tooltips_quanti_sup, by = "name") |>
      dplyr::mutate(name2 = paste0("<b>", .data$name, "</b><br>"), .after = 1) |>
      tidyr::unite(col = "tooltip", tidyselect::everything() & -"name",
                   remove = TRUE, sep = "") # , na.rm = TRUE

    ind_tooltips <- ind_tooltips |>
      dplyr::mutate(
        tooltip = stringr::str_replace_all(.data$tooltip, " ",
                                           paste0(unbrk, unbrk, unbrk))
      )

    ind_coords <- ind_coords |> dplyr::left_join(ind_tooltips, by = "name")
  }




  # Axes
  axes_common_infos <- list(
    showspikes     = FALSE, # projections lines
    showgrid       = FALSE,
    zeroline       = FALSE,
    showticklabels = FALSE #,

    # backgroundcolor="rgb(200, 200, 230",
    # gridcolor="rgb(255,255,255)",
    # zerolinecolor="rgb(255,255,255"

    # ticketmode = 'array',
    # ticktext   = c("Huey", "Dewey", "Louie"),
    # tickvals   = c(0,25,50),
    # range      = c(-25,75)

    # nticks = 4,
  )

  axes_params <- purrr::map(
    plot_range,
    ~ c(list(range = ., title = ""), axes_common_infos)
  )







  ## Assemble plot ----

  dim1 <- rlang::sym(stringr::str_c("Dim.", axes[1]))
  dim2 <- rlang::sym(stringr::str_c("Dim.", axes[2]))
  dim3 <- if (D2) {NULL} else {rlang::sym(stringr::str_c("Dim.", axes[3]))}


  # To get a fixed aspect ratio, put a point in max range * aspectratio on all axes
  if (aspectratio_from_eig) {
    aspectratio <- list(x = res.pca$svd$vs[axes[1]],
                        y = res.pca$svd$vs[axes[2]],
                        z = if (D2) {NULL} else {res.pca$svd$vs[axes[3]]}
    )

  } else {
    aspectratio <- list(x = 1, y = 1, z =if (D2) {NULL} else {1})
  }

  aspectratio_range <- tibble::as_tibble(plot_range) |>
    ## dplyr::mutate(Dim.2 = Dim.2 * 2) |>  # test
    #dplyr::mutate(dplyr::across(tidyselect::everything(), ~pmax(!!!rlang::syms(names(plot_range))))) |>
    dplyr::mutate(
      dplyr::across(axes[1], ~ . * aspectratio[[1]]),
      dplyr::across(axes[2], ~ . * aspectratio[[2]]),
      dplyr::across(if (D2) {NULL} else {axes[3]}, ~ . * aspectratio[[3]]),
    )
  if (D2) aspectratio_range <- aspectratio_range |> dplyr::select(-"Dim.3")

  #     se calcule ensuite, pour chaque axe, par rapport a son propre range ?




  # camera_title <- names(camera_view)

  if (!missing(camera_view)) {
    camera_view <- camera_view |>
      purrr::set_names(paste0("scene", 1:length(camera_view)) |>
                         stringr::str_replace("scene1", "scene") )
    scene_name <- names(camera_view)

  } else {
    scene_name <- "scene"
  }

  # i <- 1
  dual_plots <- vector("list", length(scene_name))
  for (i in 1:length(scene_name)) {

    dual_plots[[i]] <- plotly::plot_ly(scene = scene_name[i])

    # Individus
    if ("ind" %in% type) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = ind_coords, scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),  # color = df$color_col
          text = ~tooltip,
          #textfont = list(color = "#00600f", size = ind_name.size),  # "#0077c2"
          marker   = list(color = "#00600f", size = ind.size),  # "#0077c2"
          # hovertemplate = paste(
          #   "<b>%{text}</b><br>", # <br>
          #   "%{yaxis.title.text}: %{y:$,.0f}<br>",
          #   "%{xaxis.title.text}: %{x:.0%}<br>",
          #   #"Number Employed: %{marker.size:,}",
          #   "<extra></extra>"
          # ),
          hoverinfo = "text",
          hoverlabel = list(align = "right"),
          # text = ~paste("Price: ", price, '$<br>Cut:', cut),
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "markers", showlegend = FALSE, inherit = FALSE)
    }

    if ("ind_name" %in% type) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = ind_coords, scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),  # color = df$color_col
          text = ~name,
          textfont = list(color = "#00600f", size = ind_name.size),  # "#0077c2"
          hoverinfo = "skip",
          # text = ~paste("Price: ", price, '$<br>Cut:', cut),
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "text", showlegend = FALSE, inherit = FALSE)

    }

    # Variables : referentiels de depart
    if ("var" %in% type) {
      c("#4D4D4D", "black")
      dual_plots[[i]] <- dual_plots[[i]] |>
        # plotly::add_trace(data = base_axis_in_princ |> dplyr::group_by(name) |> dplyr::slice(-dplyr::n()) |> dplyr::ungroup(),
        #                   scene = scene_name[i],
        #                   x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ pair_id,
        #                   line   = list(color  = "black", width = 5),
        #                   marker = list(color  = "black",
        #                                 symbol = "cross",
        #                                 size = 5), # 3, # in 2D : "line-ns-open"
        #                   type = "scatter3d", mode = "lines+markers", showlegend = FALSE, inherit = FALSE,
        #                   hoverinfo = "skip") |>
        plotly::add_trace(
          data = base_axis_in_princ |>
            dplyr::group_by(.data$name) |> dplyr::slice(-dplyr::n()) |>
            dplyr::mutate(base_coord = format(.data$base_coord, trim = TRUE, digits = 1)) |>
            dplyr::ungroup(),
          scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),
          marker = list(color  = var_color,
                        symbol = "cross",
                        size = 5), # 3, # in 2D : "line-ns-open"
          text = ~base_coord, textfont = list(color = var_color, size = 10),
          textposition = "bottom center", hoverinfo = "skip",
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = 'markers+text', showlegend = FALSE, inherit = FALSE) |>
        plotly::add_trace(
          data = base_axis_in_princ |> dplyr::group_by(.data$name) |>
            dplyr::slice(1, dplyr::n()) |>
            dplyr::mutate(name = dplyr::if_else(
              dplyr::row_number() != 1 & var_names_on == "var",
              true  = paste0("<b>", .data$name, "</b>"),
              false = ""
            )) |>
            dplyr::ungroup(),
          scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ pair_id,
          line = list(color  = var_color, width = 5), # "black"
          text = ~name, textfont = list(color = var_color, size = 15),  # "black"
          #textposition = "top center",
          hoverinfo = "skip",
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = 'lines+text', showlegend = FALSE, inherit = FALSE)

      if (!D2) {
        dual_plots[[i]] <- dual_plots[[i]] |>
          plotly::add_trace( # cone au bout des axes
            data = base_axis_in_princ |> dplyr::group_by(.data$name) |>
              dplyr::mutate(is_last = dplyr::row_number() == dplyr::n(),
                            is_min  = .data$base_coord == min(.data$base_coord) ) |>
              dplyr::ungroup() |> dplyr::filter(.data$is_last | .data$is_min) |>
              dplyr::select("name", "is_last",  tidyselect::starts_with("Dim.")) |>
              dplyr::mutate(is_last = dplyr::if_else(.data$is_last, "", "_o")) |>
              tidyr::pivot_wider(names_from  = "is_last",
                                 values_from = tidyselect::starts_with("Dim."),
                                 names_sep = "") |>
              dplyr::mutate(dplyr::across(
                tidyselect::ends_with("_o"),
                ~ rlang::eval_tidy(
                  rlang::sym(stringr::str_remove(dplyr::cur_column(), "_o"))
                ) - .
              )),
            scene = scene_name[i],
            x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ name,
            u = ~eval(rlang::sym(stringr::str_c("Dim.", axes[1], "_o"))),
            v = ~eval(rlang::sym(stringr::str_c("Dim.", axes[2], "_o"))),
            w = ~eval(rlang::sym(stringr::str_c("Dim.", axes[3], "_o"))),
            sizeref = cone_size, sizemode = "absolute",

            colorscale = list(list(0, var_color), list(1, var_color)), # "black"
            showscale = FALSE, hoverinfo = "skip",
            # lighting  = list(ambient = 1), lightposition= list(x=0, y=0, z=1e5),
            type = "cone", anchor = "center", showlegend = FALSE, inherit = FALSE
          )
      }
    }


    # V : vecteurs propres normees (A Brigitte Le Roux) : 1 ecart-type sur axes de depart
    if ("V" %in% type) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = dplyr::bind_rows(
            V_coords,
            V_coords |>  dplyr::mutate(dplyr::across(tidyselect::starts_with("Dim."), ~ 0))
          ) |> dplyr::mutate(
            id_pair = as.integer(as.factor(.data$name)),
            name    = dplyr::if_else(dplyr::row_number() == 1L, "<b>V</b>", NA_character_)
          ),
          scene = scene_name[i],
          x = ~eval(dim1)*9/10, y = ~eval(dim2)*9/10, z = ~eval(dim3)*9/10, split = ~ id_pair,

          line   = list(color  = "black", width = 5),
          #text = ~name, textfont = list(color = "black", size = 15),
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "lines", showlegend = FALSE, inherit = FALSE,
          hoverinfo = "skip")

      if (!D2) {
        dual_plots[[i]] <- dual_plots[[i]] |>
          plotly::add_trace(
            data = V_coords,
            scene = scene_name[i],
            x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ name,
            u = ~eval(dim1), v = ~eval(dim2), w = ~eval(dim3),
            sizeref = cone_size, sizemode = "absolute",
            colorscale = list(list(0, "black"), list(1,"black")), #autocolorscale = FALSE,
            showscale = FALSE, hoverinfo = "skip",
            # lighting  = list(ambient = 1), lightposition= list(x=0, y=0, z=1e5),
            type = "cone", anchor = "tip", showlegend = FALSE, inherit = FALSE
          )
      }
    }


    # Correlations des variables : cor (1 ecart type, mais un peu decale /axes de depart)
    if ("cor" %in% type) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = dplyr::bind_rows(var_coords |>  dplyr::mutate(dplyr::across(tidyselect::starts_with("Dim."), ~ 0)),
                                  var_coords
          ) |>
            dplyr::mutate(id_pair = as.integer(as.factor(.data$name))) |>
            dplyr::group_by(.data$name) |>
            dplyr::mutate(
              name = dplyr::if_else(
                dplyr::row_number() != 1 & var_names_on == "cor",
                true  = paste0("<b>", .data$name, "</b>"),
                false = "")
              #dplyr::if_else(dplyr::row_number() == 1L, "<b>cor</b>", NA_character_)
            ) |> dplyr::ungroup(),
          scene = scene_name[i],
          x = ~eval(dim1)*9/10, y = ~eval(dim2)*9/10, z = ~eval(dim3)*9/10,
          split = ~ id_pair,
          line   = list(color  = "#0077c2", width = 5), # c("#42a5f5", "#0077c2")
          text = ~name, textfont = list(color = "#0077c2", size = 15), # "#4D4D4D"
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "lines+text", showlegend = FALSE, inherit = FALSE,
          hoverinfo = "skip")

      if (!D2) {
        dual_plots[[i]] <- dual_plots[[i]] |>
          plotly::add_trace(
            data = var_coords,
            scene = scene_name[i],
            x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ name,
            u = ~eval(dim1), v = ~eval(dim2), w = ~eval(dim3),
            sizeref = cone_size, sizemode = "absolute",
            colorscale = list(list(0, "#0077c2"), list(1, "#0077c2")), #autocolorscale = FALSE,
            showscale = FALSE, hoverinfo = "skip",
            # lighting  = list(ambient = 1), lightposition= list(x=0, y=0, z=1e5),
            type = "cone", anchor = "tip", showlegend = FALSE, inherit = FALSE
          )
      }
    }

    if (!D2 & "cor_sphere" %in% type) {
      #arg cor_sphere_resolution ?
      theta = seq(0, 2*pi, length.out = 15)
      phi   = seq(0,   pi, length.out = 15)

      sphere_data <- tibble::tibble(
        x = outer(cos(theta), sin(phi)) ,
        y = outer(sin(theta), sin(phi)) ,
        z = outer(rep(1, 15), cos(phi)),
      )
      ## alternative
      # theta = seq(0, pi, length.out = 25)
      # phi   = seq(-pi,   pi, length.out = 25)
      # meshgrid_sphere <- pracma::meshgrid(theta, phi) # outer works ?
      # theta <-  meshgrid_sphere[[1]] #|> c()
      # phi   <-  meshgrid_sphere[[2]] #|> c()
      # r = 1
      # sphere_data <- tibble::tibble(
      #   x = r * sin(theta) * cos(phi),
      #   y = r * sin(theta) * sin(phi),
      #   z = r * cos(theta),
      # )

      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = sphere_data,
          scene = scene_name[i],
          x = ~x, y = ~y, z = ~z,
          opacity = 0.1,
          colorscale = list(list(0, "#ba2d65"), list(1, "#ba2d65")), showscale = FALSE,
          type = "surface", showlegend = FALSE, inherit = FALSE, hoverinfo = "skip" #,
        )

    }


    # Axes principaux de l'ACP
    dual_plots[[i]] <- dual_plots[[i]] |>
      # plotly::add_trace(data = princ_axes,  scene = scene_name[i],
      #                   x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ pair_id,
      #                   line = list(color  = "#d32f2f", width = 5),
      #                   marker = list(color  = "#d32f2f",
      #                                 symbol = "cross",
      #                                 size = 5), # 3
      #                   type = "scatter3d", mode = "lines+markers", showlegend = FALSE, inherit = FALSE,
      #                   hoverinfo = "skip") |>
      # add_trace(data = princ_axes |> dplyr::filter(base_coord <= 1 & base_coord >= -1), #
      #         scene = scene_name[i],
      #         x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ pair_id,
    #         line   = list(color  = "#d32f2f", width = 5),
    #         type = "scatter3d", mode = "lines", showlegend = FALSE, inherit = FALSE,
    #         hoverinfo = "skip") |>
    plotly::add_trace(
      data = princ_axes |>
        dplyr::filter(.data$name %in% paste0("Axe ", axes) ) |>
        dplyr::group_by(.data$name) |>
        dplyr::slice(-dplyr::n()) |>
        dplyr::ungroup(),
      # dplyr::mutate(remove_last_if_not_1 = dplyr::row_number() == dplyr::n() & base_coord != 1) |>
      # dplyr::filter(!remove_last_if_not_1) |> dplyr::ungroup(),
      scene = scene_name[i],
      x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),
      marker = list(color  = "#d32f2f",
                    symbol = "cross",
                    size = 5), # 3
      text = ~base_coord, textfont = list(color = "#d32f2f", size = 10),
      textposition = "bottom center", hoverinfo = "skip",
      type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
      mode = 'markers+text', showlegend = FALSE, inherit = FALSE) |>
      plotly::add_trace(
        data = princ_axes |>
          dplyr::filter(.data$name %in% paste0("Axe ", axes) ) |>
          dplyr::group_by(.data$name) |> dplyr::slice(1, dplyr::n()) |>
          dplyr::mutate(
            name = dplyr::if_else(dplyr::row_number() == 1,
                                  true  = "",
                                  false  = paste0("<b>", .data$name, "</b>"))
          ) |>
          dplyr::ungroup(),
        scene = scene_name[i],
        x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ pair_id,
        line = list(color  = "#d32f2f", width = 5),
        text = ~name, textfont = list(color = "#d32f2f", size = 15),
        #textposition = "top center",
        hoverinfo = "skip",
        type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
        mode = 'lines+text', showlegend = FALSE, inherit = FALSE)

    if (!D2) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace( # cone au bout des axes
          data = princ_axes |>
            dplyr::filter(.data$name %in% paste0("Axe ", axes) ) |>
            dplyr::group_by(.data$name) |>
            dplyr::slice(dplyr::n()) |>
            dplyr::ungroup(),
          scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ name,
          u = ~eval(dim1)*9/10, v = ~eval(dim2)*9/10, w = ~eval(dim3)*9/10,
          sizeref = cone_size, sizemode = "absolute",

          colorscale = list(list(0, "#d32f2f"), list(1, "#d32f2f")), #autocolorscale = FALSE,
          showscale = FALSE, hoverinfo = "skip",
          # lighting  = list(ambient = 1), lightposition= list(x=0, y=0, z=1e5),
          type = "cone", anchor = dplyr::if_else(max(princ_axes_print) == 1, "tip", "center"),
          showlegend = FALSE, inherit = FALSE
        )
    }

    # To get a fixed aspect ratio, put a point in max ranges on all axes
    if (!D2) { # Also in 2D ?
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = aspectratio_range, scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),  # color = df$color_col
          hoverinfo = "skip", opacity = 0, visible = TRUE,
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "text", showlegend = FALSE, inherit = FALSE
        )
    }


    # Valeurs singulieres (racine carree des valeurs propres)
    if ("vs" %in% type) {
      vs_diag <- diag(res.pca$svd$vs) |>
        magrittr::set_colnames(paste0("Dim.", 1:length(res.pca$svd$vs))) |>
        magrittr::set_rownames(paste0("Dim.", 1:length(res.pca$svd$vs))) |>
        as.data.frame() |> tibble::rownames_to_column("name") |> tibble::as_tibble() |>
        dplyr::mutate(name = stringr::str_replace(.data$name, "Dim.", "vs"))

      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = dplyr::bind_rows(vs_diag,
                                  vs_diag |>  dplyr::mutate(dplyr::across(tidyselect::starts_with("Dim."), ~ 0))
          ) |> dplyr::mutate(id_pair = as.integer(as.factor(.data$name)),
                             name    = dplyr::if_else(dplyr::row_number() == 1L, "<b>vs</b>", NA_character_)
          ),
          scene = scene_name[i],
          x = ~Dim.1*9/10, y = ~Dim.2*9/10, z = ~Dim.3*9/10, split = ~ id_pair,
          line   = list(color  = "#9e9d24", width = 10),
          text = ~name, textfont = list(color = "#9e9d24", size = 15),
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "lines+text", showlegend = FALSE, inherit = FALSE,
          hoverinfo = "skip")

      if (!D2) {
        dual_plots[[i]] <- dual_plots[[i]] |>
          plotly::add_trace(
            data = vs_diag,
            scene = scene_name[i],
            x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ name,
            u = ~eval(dim1), v = ~eval(dim2), w = ~eval(dim3),
            sizeref = cone_size, sizemode = "absolute",

            colorscale = list(list(0, "#9e9d24"), list(1,"#9e9d24")), #autocolorscale = FALSE,
            showscale = FALSE, hoverinfo = "skip",
            # lighting  = list(ambient = 1), lightposition= list(x=0, y=0, z=1e5),
            type = "cone", anchor = "tip", showlegend = FALSE, inherit = FALSE
          )
      }
    }

    # Point moyen et projections du point moyen sur les axes de depart
    if (!center) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = mean_projs,  scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ name,
          line   = list(color = "#f57c00", dash = "dash", width = 3), # "#bb4d00"
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "lines", showlegend = FALSE, inherit = FALSE,
          hoverinfo = "skip")
    }

    # Plan Axe 1/Axe 2 et projections des points
    if ("projections" %in% type) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = dplyr::bind_rows(ind_coords, dplyr::mutate(ind_coords, Dim.3 = 0)),
          scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ name,
          line   = list(color  = "#9575cd"), # dash = "longdash", width = 4  #( "dash" | "dashdot" | "dot" | "longdash" | "longdashdot" | "solid" )
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "lines", showlegend = FALSE, inherit = FALSE,
          hoverinfo = "skip") |>
        plotly::add_trace(
          data = dplyr::mutate(ind_coords, Dim.3 = 0), scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),
          marker   = list(color  = "#9575cd", size = 2),  # "#65499c"
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "markers", showlegend = FALSE, inherit = FALSE,
          hoverinfo = "skip")
    }


    if ("main_plan" %in% type) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = planDf, scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),
          opacity = 0.5, #color = "A", colorscale = c("A" = "#65499c"), #vertexcolor  = "#65499c",
          facecolor = rep('#CFC0E8', nrow(planDf)), # "#65499c"
          hoverinfo = "skip",
          type = "mesh3d", showlegend = FALSE, inherit = FALSE)
    }
  }

  # cat(
  # paste0("c(",
  #   paste0(
  #     paste0("'",
  #       c(material_colors_lighter(by = 0.25)[1],
  #       material_colors_lighter(by = 0.20)[1],
  #       material_colors_lighter(by = 0.15)[1],
  #       material_colors_lighter(by = 0.10)[1],
  #       material_colors_lighter(by = 0.05)[1]
  #     ),
  #     "'" #,
  #     ),
  #     collapse = ", "
  #   ),
  #   ")"
  #   )
  # )
  c('#DDD3EF', '#CFC0E8', '#C0ADE1', '#B29ADB', '#A388D4')


  # plotly::layout(# title = "Title",
  #   scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
  #                #              # domain=list(x=c(0,0.5),y=c(0.5,1),
  #                #              # aspectratio = list(x=1, y=1, z=2)))
  #                camera = list(
  #                  center = list(x =  0   , y =  0  , z = 0  ),
  #                  eye    = list(x =  0.6 , y = -0.4, z = 0.7),
  #                  up     = list(x =  0   , y =  0  , z = 1  ) # ,
  #                  #projection = list(type = 'orthographic')
  #                )
  #                #              #
  #                #              # dragmode = "turntable",
  #                #              # annotations =
  #                #
  #   ) #,
  #
  #   # scene2 = ,
  #   # margin = list(t = 30, r = 30, l = 30, b = 30, padding = 2)
  # )


  # dual_referential_3D_plot |>
  #   # Layout
  #   plotly::layout(# title = "Title",
  #          scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
  #                       #              # domain=list(x=c(0,0.5),y=c(0.5,1),
  #                       #              # aspectratio = list(x=1, y=1, z=2)))
  #                       camera = list(
  #                         center = list(x =  0   , y =  0  , z = 0  ),
  #                         eye    = list(x =  0.6 , y = -0.4, z = 0.7),
  #                         up     = list(x =  0   , y =  0  , z = 1  ) # ,
  #                         #projection = list(type = 'orthographic')
  #                       )
  #                       #              #
  #                       #              # dragmode = "turntable",
  #                       #              # annotations =
  #                       #
  #          ) #,
  #
  #          # scene2 = ,
  #          # margin = list(t = 30, r = 30, l = 30, b = 30, padding = 2)
  #   )



  # # Plan 1-2
  # dual_plots[[1]] |>
  #   plotly::layout(#title = "Title",
  #          scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
  #                       camera = list(
  #                         center = list(x =  0   , y =  0  , z = 0  ),
  #                         eye    = list(x =  0   , y =  0  , z = 1  ),
  #                         up     = list(x =  0   , y =  1  , z = 0  )
  #                       )
  #          )
  #   )
  #
  # # Plan 1-3
  # dual_plots[[2]] |>
  #   plotly::layout(#title = "Title",
  #          scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
  #                       camera = list(
  #                         center = list(x =  0   , y =  0  , z = 0  ),
  #                         eye    = list(x =  0   , y = -1  , z = 0  ),
  #                         up     = list(x =  0   , y =  0  , z = 1  )
  #                       )
  #          )
  #   )
  #



  if (!D2) {
    # Buttons to set plans
    plan12  <- paste0("Plane ", axes[1], "-", axes[2]) # 1-2
    plan13  <- paste0("Plane ", axes[1], "-", axes[3]) # 1-3
    plan23  <- paste0("Plane ", axes[2], "-", axes[3]) # 2-3
    plan123 <-  "All"

    if (!remove_buttons) {
      updatemenus <- list(
        list(
          active = -1,
          # switch(view,
          #        plan12 = 0,
          #        "Plane 1-3" = 1,
          #        "Plane 2-3" = 2,
          #        "All"       = 3,
          #        stop("'view' argument is not recognized")) , # -1,
          type   = 'buttons', # uirevision  = FALSE, # showactive = FALSE, # visible  = TRUE,
          buttons = list(
            list(
              label = plan12,
              method = "relayout",
              args = list(list(scene = list(
                xaxis = axes_params[[axes[1]]],
                yaxis = axes_params[[axes[2]]],
                zaxis = axes_params[[axes[3]]],
                aspectratio = aspectratio, aspectmode = "data",
                camera = list(
                  center = list(x =  0   , y =  0  , z = 0  ),
                  eye    = list(x =  0   , y =  0  , z = base_zoom),
                  up     = list(x =  0   , y =  1  , z = 0  ),
                  projection = "orthographic"
                )
              )
              )) #,
              # args2 = list(list(scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
              #                                 aspectratio = = aspectratio, aspectmode = "data",
              #                                 camera = list(
              #                                   center = list(x =  0   , y =  0  , z = 0  ),
              #                                   eye    = list(x =  0   , y =  0  , z = base_zoom),
              #                                   up     = list(x =  0   , y =  1  , z = 0  )
              #                                 )
              # )
              # ))
            ),
            #list(list(shapes = list(cluster0, c(), c())))),

            list(
              label = plan13,
              method = "relayout",
              args = list(list(scene = list(
                xaxis = axes_params[[axes[1]]],
                yaxis = axes_params[[axes[2]]],
                zaxis = axes_params[[axes[3]]],
                aspectratio = aspectratio, aspectmode = "data",
                camera = list(
                  center = list(x =  0   , y =  0  , z = 0  ),
                  eye    = list(x =  0   , y = -base_zoom  , z = 0  ),
                  up     = list(x =  0   , y =  0  , z = 1  ),
                  projection = "orthographic"
                )
              )
              )
              ) #,
              # args2 = list(list(scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
              #                        aspectratio = = aspectratio, aspectmode = "data",
              #                        camera = list(
              #                          center = list(x =  0   , y =  0  , z = 0  ),
              #                          eye    = list(x =  0   , y = -base_zoom  , z = 0  ),
              #                          up     = list(x =  0   , y =  0  , z = 1  )
              #                        )
              # )
              # )
              #)
            ),
            #list(list(shapes = list(c(), cluster1, c())))),

            list(
              label = plan23,
              method = "relayout",
              args = list(list(scene = list(
                xaxis = axes_params[[axes[1]]],
                yaxis = axes_params[[axes[2]]],
                zaxis = axes_params[[axes[3]]],
                aspectratio = aspectratio, aspectmode = "data",
                camera = list(
                  center = list(x =  0   , y =  0  , z = 0  ),
                  eye    = list(x =  base_zoom , y =  0  , z = 0  ),
                  up     = list(x =  0   , y =  0  , z = 1  ),
                  projection = "orthographic"
                )
              )
              )
              ) #,
              # args2 = list(list(scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
              #                               aspectratio = = aspectratio, aspectmode = "data",
              #                               camera = list(
              #                                 center = list(x =  0   , y =  0  , z = 0  ),
              #                                 eye    = list(x =  base_zoom , y =  0  , z = 0  ),
              #                                 up     = list(x =  0   , y =  0  , z = 1  )
              #                               )
              # )
              # )
              # )
            ),

            list(
              label = plan123,
              method = "relayout",
              args = list(list(scene = list(
                xaxis = axes_params[[axes[1]]],
                yaxis = axes_params[[axes[2]]],
                zaxis = axes_params[[axes[3]]],
                aspectratio = aspectratio, aspectmode = "data",
                camera = list(
                  center = list(x =  0   , y =  0  , z = 0  ),
                  eye    = list(x = base_zoom/6, y = -base_zoom, z = base_zoom),
                  #         list(x =  0.6 * base_zoom, y = -0.4 * base_zoom, z = 0.7 * base_zoom),
                  up     = list(x =  0   , y =  0  , z = 1  ),
                  projection = "orthographic"
                )
              )
              )) #,
              # args2 = list(list(scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
              #                               aspectratio = = aspectratio, aspectmode = "data",
              #                               camera = list(
              #                                 center = list(x =  0   , y =  0  , z = 0  ),
              #                                 eye    = list(x = base_zoom/6, y = -base_zoom, z = base_zoom),
              #                                 #         list(x =  0.6 * base_zoom, y = -0.4 * base_zoom, z = 0.7 * base_zoom),
              #                                 up     = list(x =  0   , y =  0  , z = 1  )
              #                               )
              # )
              # ))
            ) # ,
            #list(list(shapes = c()))),

          )
        )
      )

    } else {
      updatemenus <- NULL
    }


  } else { # 2D
    updatemenus <- NULL
  }


  if (!D2 & !missing(camera_view)) {
    scenes <- purrr::map(
      camera_view,
      ~ list(xaxis = axes_params[[axes[1]]],
             yaxis = axes_params[[axes[2]]],
             zaxis = axes_params[[axes[3]]],
             aspectratio = aspectratio, aspectmode = "data",

             #aspectratio = list(x = 1, y = 1, z = 1),
             #domain = list(x = c(0, 0.5), y = c(0, 1)),
             camera = .x
      )
    )

  } else if (!D2) {
    scenes <- list("scene" = dplyr::case_when(
      view == plan12   ~ list(
        xaxis = axes_params[[axes[1]]],
        yaxis = axes_params[[axes[2]]],
        zaxis = axes_params[[axes[3]]],
        aspectratio = aspectratio, aspectmode = "data",
        camera = list(
          center = list(x =  0   , y =  0  , z = 0  ),
          eye    = list(x =  0   , y =  0  , z = base_zoom),
          up     = list(x =  0   , y =  1  , z = 0  ),
          projection = "orthographic"
        )
      ),
      view == plan13   ~ list(
        xaxis = axes_params[[axes[1]]],
        yaxis = axes_params[[axes[2]]],
        zaxis = axes_params[[axes[3]]],
        aspectratio = aspectratio, aspectmode = "data",
        camera = list(
          center = list(x =  0   , y =  0  , z = 0  ),
          eye    = list(x =  0   , y =  - base_zoom  , z = 0),
          up     = list(x =  0   , y =  0  , z = 1  ),
          projection = "orthographic"
        )
      ),
      view == plan23   ~ list(
        xaxis = axes_params[[axes[1]]],
        yaxis = axes_params[[axes[2]]],
        zaxis = axes_params[[axes[3]]],
        aspectratio = aspectratio, aspectmode = "data",
        camera = list(
          center = list(x =  0   , y =  0  , z = 0  ),
          eye    = list(x = base_zoom, y =  0  , z = 0),
          up     = list(x =  0   , y =  0  , z = 1  ),
          projection = "orthographic"
        )
      ),
      view == plan123  ~ list(
        xaxis = axes_params[[axes[1]]],
        yaxis = axes_params[[axes[2]]],
        zaxis = axes_params[[axes[3]]],
        aspectratio = aspectratio, aspectmode = "data",
        camera = list(
          center = list(x =  0   , y =  0  , z = 0  ),
          eye    = list(x = base_zoom/6, y = -base_zoom, z = base_zoom),
          #         list(x =  0.6 * base_zoom, y = -0.4 * base_zoom, z = 0.7 * base_zoom),
          up     = list(x =  0   , y =  0  , z = 1  ),
          projection = "orthographic"
        )
      ),

      TRUE ~ list(a = NULL)
    )

    )
    #print(scenes)
    if (is.null(scenes$scene[[1]])) stop(paste0(
      "view argument must be among: ",
      paste0(
        paste0("'", c(plan12, plan13, plan23, plan123), "'"), collapse = ", "),
      collapse = ""
    ))


  } else {  # 2D
    scenes <- list("scene" = list(
      xaxis = axes_params[[axes[1]]],
      yaxis = axes_params[[axes[2]]],
      aspectratio = aspectratio, aspectmode = "data" #,
    ))
  }


  final_plots <-
    plotly::subplot(purrr::list_flatten(dual_plots), margin = 0.1, #0,
                    nrows = ceiling(length(scene_name)/2L)
    )
  final_plots <- do.call(plotly::layout,

                         c(list(p           = final_plots,
                                margin      = list(b = 0, l = 0, r = 0, t = 0),
                                updatemenus = updatemenus
                         ),
                         if (missing(title)) {NULL} else {list(title = title)},
                         scenes
                         )
  )

  #print(aspectratio)


  final_plots$data$ind_coords         <- ind_coords
  final_plots$data$base_axis_in_princ <- base_axis_in_princ
  final_plots$data$princ_axes         <- princ_axes
  final_plots$data$mean_projs         <- mean_projs
  final_plots$data$planDf             <- planDf

  final_plots

  # plotly::layout(#title = "Title",
  #   scene = list(title = ,
  #                xaxis = axx, yaxis = axy, zaxis = axz,
  #                #domain = list(x = c(0, 0.5), y = c(0, 1)),
  #                    camera = list(
  #                      center = list(x =  0   , y =  0  , z = 0  ),
  #                      eye    = list(x =  0   , y =  0  , z = 1),
  #                      up     = list(x =  0   , y =  1  , z = 0  )
  #                    )
  #       )#,
  #     ) |>
  #     plotly::layout(#title = "Title",
  #       scene2 = list(title = ,
  #                     xaxis = axx, yaxis = axy, zaxis = axz,
  #                     #domain = list(x = c(0.5, 1), y = c(0, 1)),
  #                     camera = list(
  #                       center = list(x =  0   , y =  0  , z = 0  ),
  #                       eye    = list(x =  0   , y = -1  , z = 0  ),
  #                       up     = list(x =  0   , y =  0  , z = 1  )
  #                     )
  #       )
  #     )

}










#' Colored Table to Help Interpretation of Principal Component Analysis
#'
#' @param res.pca The result of \code{\link[FactoMineR:PCA]{FactoMineR::PCA}}.
#' @param axes The axes to print, as a numeric vector.
#'
#' @return A tibble of class tabxplor
#' @export
#'
#'@examples
#'
#' data(mtcars, package = "datasets")
#' mtcars <- mtcars[1:7] |> dplyr::rename(weight = wt)
#' res.pca <- FactoMineR::PCA(mtcars, graph = FALSE)
#' pca_interpret(res.pca)
#'
pca_interpret <- function(res.pca, axes = 1:3) {
  n_acp <- nrow(res.pca$ind$coord)

  var_data <- res.pca$var |>
    purrr::imap_dfr(~ .x[, axes] |>
                      tibble::as_tibble(rownames = "variable") |>
                      dplyr::mutate(type := factor(.y))) |>
    dplyr::filter(.data$type != "cor") |>  # no need for correlation since scale.unit = TRUE
    dplyr::mutate(type = forcats::fct_relevel(.data$type, "coord", "contrib", "cos2") |> # reorder types
                    forcats::fct_recode("ctr" = "contrib"),
                  variable = forcats::as_factor(.data$variable)
    ) |>
    dplyr::arrange(.data$type)

  var_data <- var_data |>
    tidyr::pivot_wider(names_from = "type",
                       values_from = tidyselect::starts_with("Dim."),
                       names_sort = TRUE) |>
    dplyr::rename_with(~stringr::str_remove(., "_coord") |>
                         stringr::str_replace("Dim\\.([^_]+)_(.+)", "\\2.\\1")
    )

  var_data |>
    tibble::add_row(variable = factor("Total")) |>
    dplyr::mutate(dplyr::across(where(is.numeric) & tidyselect::starts_with("ctr"),
                                ~ dplyr::if_else(variable != "Total", ., 100/(dplyr::n() - 1) )) # mean(., na.rm = TRUE)
    ) |>
    dplyr::mutate(
      dplyr::across(where(is.numeric) & tidyselect::starts_with("Dim"),
                    #~ round(., 2)
                    ~ tabxplor::fmt(n         = rep(n_acp, length(.)),
                                    type      = "mean",

                                    mean      = ., # dplyr::if_else(variable != "Total" , ., 0),
                                    diff      = dplyr::case_when(
                                      variable == "Total" ~ 1,
                                      . > 0    ~ 3,
                                      . < 0    ~ 1/9,
                                      . == 0   ~ 1,
                                    ),
                                    in_totrow = variable == "Total",
                                    in_refrow = variable == "Total",
                                    digits  = 2L,

                                    col_var   =  stringr::str_extract(dplyr::cur_column(), "\\.[^\\.]+$"), # dplyr::cur_column(),
                                    color     = "diff",
                                    ref = "tot",
                                    #comp_all = FALSE
                    )
      ),

      dplyr::across(where(is.numeric) & tidyselect::starts_with("ctr"),
                    ~ tabxplor::fmt(n         = rep(n_acp, length(.)),
                                    type      = "col",  # display = "pct",

                                    pct       = dplyr::if_else(variable == "Total", 1, ./100),
                                    ctr       = ./100,
                                    in_totrow = variable == "Total",

                                    col_var   = stringr::str_extract(dplyr::cur_column(), "\\.[^\\.]+$"), # dplyr::cur_column(),
                                    color     = "contrib",
                                    ref = "tot",
                    )),

      dplyr::across(where(is.numeric) & tidyselect::starts_with("cos2"),
                    ~ tabxplor::fmt(n    = rep(n_acp, length(.)),
                                    type = "row", # display = "pct",

                                    pct  = .,
                                    diff = . - 0.5,
                                    in_totrow = variable == "Total",
                                    in_refrow = variable == "Total",

                                    col_var   = stringr::str_extract(dplyr::cur_column(), "\\.[^\\.]+$"), # dplyr::cur_column(),
                                    color     = "diff",
                                    ref = "tot",
                                    #comp_all  = FALSE
                    )
      ),


    )

}



#' Simple Mean and SD Summary
#'
#' @param data A data.frame.
#' @param vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> The names of the
#' numeric variables to compute means and sds with.
#' @param wt The name of the weight variable, if needed.
#'
#' @return A data.frame.
#' @export
#'
#' @examples
#' mean_sd_tab(mtcars, 1:7)
mean_sd_tab <- function(data, vars, wt) {
  vars <- names(tidyselect::eval_select(rlang::enquo(vars), data))

  not_num <- data |>
    dplyr::select(tidyselect::all_of(vars)) |>
    purrr::map_lgl(~ !is.numeric(.))

  if(any(not_num)) {
    stop(paste0("some vars are not numeric: ",
                paste0(names(not_num)[not_num], collapse = ", ")
    ))
  }

  if (missing(wt)) {
    tabs <- data |>
      dplyr::summarise(
        dplyr::across(tidyselect::all_of(vars),
                      ~ mean(., na.rm = TRUE),
                      .names = "{.col};mean"
        ),

        dplyr::across(tidyselect::all_of(vars),
                      ~ sqrt(var(., na.rm = TRUE)),
                      .names = "{.col};sd"
        ),
      )

  } else {
    wt <- rlang::ensym(wt)

    tabs <- data |>
      dplyr::summarise(
        dplyr::across(tidyselect::all_of(vars),
                      ~ stats::weighted.mean(., w = !!wt, na.rm = TRUE),
                      .names = "{.col};mean"),

        dplyr::across(tidyselect::all_of(vars),
                      ~ sqrt(weighted.var(., wt = !!wt, na.rm = TRUE)),
                      .names = "{.col};sd"),
        )

  }

  tabs |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::ends_with(";mean"),
        ~ rlang::eval_tidy(rlang::sym(stringr::str_replace(dplyr::cur_column(), ";mean", ";sd"))) / .,
        .names = "{.col};sd/mean"),
    ) |>
    dplyr::rename_with(~ stringr::str_replace(., "mean;sd/mean", "sd/mean"),
                       .cols = tidyselect::contains("mean;sd/mean")) |>
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = c("variables", "type"),
                        names_sep = ";" ) |>
    tidyr::pivot_wider(names_from = "type", values_from = "value")
}














# CA ----

# axes = c(1,2)
# filter = ""
# uppercase = "col"
# show_sup = FALSE
# tooltips = "row"
# rowtips_subtitle = "% en ligne"
# coltips_subtitle = "% en colonne"
# rowcolor_numbers = 0
# colcolor_numbers = 0
# dist_labels = 0.12
# text_size = 3.5
# right_margin = 0
# size_scale_max = 8

#' Readable and Interactive graph for simple correspondence analysis
#' @description A readable, complete and beautiful graph for simple
#' correspondence analysis made with \code{FactoMineR::\link[FactoMineR]{CA}}.
#' Interactive tooltips, appearing when hovering on  points with mouse, allow to
#' keep in mind all the content of the table while reading the graph. Since it is
#' made in the spirit of \code{\link[ggplot2]{ggplot2}}, it is possible to change
#' theme or add another plot elements with +. Then, interactive
#' tooltips won't appear until you pass the result through \code{\link{ggi}}.
#'
#' @param res.ca An object created with \code{FactoMineR::\link[FactoMineR]{CA}}.
#' @param axes The axes to print, as a numeric vector of length 2.
#' @param show_sup When \code{TRUE} show supplementary rows and cols.
#' @param xlim,ylim Horizontal and vertical axes limits,
#' as double vectors of length 2.
#' @param out_lims_move When \code{TRUE}, the points out of \code{xlim} or
#'  \code{ylim} are not removed, but moved at the edges of the graph.
#' @param type Determines the way the two variables of the table are printed.
#'    \itemize{
#'    \item \code{"points"} : colored points with text legends
#'    \item \code{"text"} : colored text
#'    \item \code{"labels"} : colored labels
#'  }
#' @param text_repel When \code{TRUE} the graph is not interactive anymore,
#'  but the resulting image is better to print because points and labels don't
#'  overlaps. It uses \code{ggrepel::\link[ggrepel]{geom_text_repel}}.
#' @param uppercase Print \code{"row"} var or \code{"col"} var labels with
#' uppercase.
#' @param tooltips Choose the content of interactive tooltips at mouse hover :
#'  \code{"col"} for the table of columns percentages, \code{"row"} for line
#'  percentages, default to \code{c("row", "col")} for both.
#' @param rowtips_subtitle,coltips_subtitle The subtitles used before the table
#' in interactive tooltips.
#' @param rowcolor_numbers,colcolor_numbers If row var or col var levels are
#' prefixed with numbers(ex. : \code{"1-"} ), the number of digits to use
#' to create classes that will be used to add colors to points.
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like \code{"1-"}, and text in parentheses.
#' @param filter Regex patterns to discard levels of row or col variables.
#' @param title The title of the graph.
#' @param text_size Size of text.
#' @param dist_labels When \code{type = "points"}, the distance of text and
#' labels from points.
#' @param right_margin A margin at the right, in cm. Useful to read tooltips
#'  over points placed at the right of the graph without formatting problems.
#' @param size_scale_max Size of points.
#' @param use_theme By default, a specific \code{ggplot2} theme is used.
#' Set to \code{FALSE} to customize your own \code{\link[ggplot2:theme]{theme}}.
#'
#' @return A \code{\link[ggplot2:ggplot]{ggplot}} object to be printed in the
#' `RStudio` Plots pane. Possibility to add other gg objects with \code{+}.
#' Sending the result  through \code{\link{ggi}} will draw the
#' interactive graph in the Viewer pane using \code{\link[ggiraph]{ggiraph}}.
#' @export
#'
#' @examples # Make the correspondence analysis :
#' \donttest{
#' tabs <- table(forcats::gss_cat$race, forcats::gss_cat$marital)[-4,]
#' # tabs <- tabxplor::tab_plain(forcats::gss_cat, race, marital, df = TRUE)
#' res.ca <- FactoMineR::CA(tabs, graph = FALSE)
#'
#' # Interactive plot :
#' graph.ca <- ggca(res.ca,
#'                  title = "Race by marital : correspondence analysis",
#'                  tooltips = c("row", "col"))
#' ggi(graph.ca) #to make the plot interactive
#'
#' # Image plot :
#' ggca(res.ca,
#'      title = "Race by marical : correspondence analysis",
#'      text_repel = TRUE)
#'      }
ggca <-
  function(res.ca = res.ca, axes = c(1,2), show_sup = FALSE, xlim, ylim,
           out_lims_move = FALSE,
           type = c("points", "text", "labels"), text_repel = FALSE, uppercase = "col",
           tooltips = c("row", "col"),
           rowtips_subtitle = "Row pct", coltips_subtitle = "Column pct",
           rowcolor_numbers = 0, colcolor_numbers = 0, cleannames = TRUE, filter = "",
           title,
           text_size = 3.5, dist_labels = c("auto", 0.12), right_margin = 0,
           size_scale_max = 8, use_theme = TRUE) {  #, repel_max_iter = 10000

    dim1 <- rlang::sym(stringr::str_c("Dim ", axes[1])) #rlang::expr(eval(parse(text = paste0("`Dim ", axes[1],"`"))))
    dim2 <- rlang::sym(stringr::str_c("Dim ", axes[2])) #rlang::expr(eval(parse(text = paste0("`Dim ", axes[2],"`"))))


    #Lignes :
    row_coord <- res.ca$row$coord %>% tibble::as_tibble(rownames = "lvs") %>%
      dplyr::mutate(colorvar = "Active_row") %>%
      dplyr::bind_rows(res.ca$row.sup$coord %>%
                         tibble::as_tibble(rownames = "lvs") %>%
                         dplyr::mutate(colorvar = "Sup_row") )
    row_coord <- row_coord  %>%
      dplyr::bind_cols(freq = rowSums(res.ca$call$Xtot) / sum(rowSums(res.ca$call$Xtot))) %>%
      dplyr::mutate(numbers = dplyr::case_when(
        stringr::str_detect(.data$lvs, "^[^- ]+-(?![[:lower:]])|^[^- ]+(?<![[:lower:]])-")
        ~ stringr::str_extract(.data$lvs, "^[^- ]+"),
        TRUE ~ "" ))

    # Remove words in parenthesis and numbers
    if (cleannames == TRUE) row_coord <- row_coord %>%
      dplyr::mutate(lvs = stringr::str_remove_all(.data$lvs, cleannames_condition()))

    # Variable de couleur (colorvar) selon nb de caracteres indiques
    row_coord <- row_coord  %>%
      dplyr::mutate(row_colorvar = as.factor(stringr::str_sub(.data$numbers, 1,
                                                              rowcolor_numbers)))
    row_colorvar_recode <- levels(row_coord$row_colorvar)
    names(row_colorvar_recode) <- stringr::str_c(1:nlevels(row_coord$row_colorvar))
    row_coord <- row_coord %>%
      dplyr::mutate(row_colorvar = forcats::fct_recode(.data$row_colorvar,
                                                       !!!row_colorvar_recode)) %>%
      dplyr::mutate(colorvar = ifelse(.data$colorvar == "Sup_row", .data$colorvar,
                                      stringr::str_c(.data$colorvar,
                                                     .data$row_colorvar))) %>%
      dplyr::select(-.data$row_colorvar) %>%
      # Afficher informations interactives au survol d'un point
      dplyr::mutate(interactive_text = stringr::str_c("<b>", .data$lvs, "</b>", "\n",
                                                      "Frequency: ",
                                                      round(.data$freq*100, 0), "%"),
                    lvs = stringr::str_replace_all(.data$lvs, "[^[:alnum:][:punct:]]",
                                                   " ") %>% stringr::str_squish()  )

    if ("row" %in% tooltips) {
      #Calculer les % par ligne (de la variable colonne)
      row_frequencies <- res.ca$call$Xtot %>% tibble::as_tibble() %>%
        tibble::add_row(!!!colSums(res.ca$call$Xtot))
      row_frequencies <- row_frequencies %>%
        dplyr::mutate_all(~ ./rowSums(row_frequencies)) %>%
        dplyr::rename_all(~ stringr::str_remove_all(., cleannames_condition()))
      row_residuals <- row_frequencies %>%
        dplyr::mutate_all(~ . - .[nrow(row_frequencies)]) %>%
        dplyr::mutate_all(~ dplyr::case_when(
          round(.*100,0) >= 0 ~ stringr::str_c("+", round(.*100, 0), "%"),
          . < 0 ~ stringr::str_c(unbrk, #Unbreakable space
                                 "-", round(abs(.)*100, 0), "%")
        )) %>% dplyr::slice(-nrow(row_frequencies))
      row_frequencies <- row_frequencies %>%
        dplyr::slice(-nrow(row_frequencies)) %>%
        dplyr::mutate_all(~ stringr::str_c(round(.*100, 0), "%")) %>%
        dplyr::mutate_all(~dplyr::case_when(
          stringr::str_length(.) >= 3 ~ .,
          stringr::str_length(.) < 3 ~ stringr::str_c(
            unbrk, unbrk, . #2 unbreakable spaces
          ),
        ))
      row_frequencies <- row_frequencies %>%
        dplyr::bind_rows(row_residuals) %>%
        dplyr::mutate(number_of_rows = dplyr::row_number())
      row_frequencies <- row_frequencies %>%
        dplyr::mutate_at(dplyr::vars(-.data$number_of_rows), ~dplyr::case_when(
          number_of_rows > nrow(row_frequencies)/2 ~ NA_character_,
          TRUE ~ stringr::str_c("(",.[number_of_rows + nrow(row_frequencies)/2],") ", .),
        )) %>%
        dplyr::slice(1:(nrow(row_frequencies)/2)) %>% dplyr::select(-.data$number_of_rows)
      row_frequencies <- purrr::map_dfc(1:ncol(row_frequencies),
                                        ~dplyr::mutate_all(row_frequencies[.x],
                                                           function(.) stringr::str_c(colnames(row_frequencies)[.x], " : ", .)
                                        ))
      row_frequencies <- row_frequencies %>%
        tidyr::unite("row_text", sep = "\n") %>% dplyr::pull(.data$row_text)
      row_coord <- row_coord %>%
        dplyr::mutate(interactive_text = stringr::str_c(
          .data$interactive_text, "\n\n", rowtips_subtitle, " :\n", row_frequencies))
    }



    #Colonnes :
    col_coord <- res.ca$col$coord %>% tibble::as_tibble (rownames = "lvs") %>%
      dplyr::mutate(colorvar = "Active_col") %>%
      dplyr::bind_rows(res.ca$col.sup$coord %>%
                         tibble::as_tibble(rownames = "lvs") %>%
                         dplyr::mutate(colorvar = "Sup_col") ) %>%
      dplyr::bind_cols(freq = rowSums(t(res.ca$call$Xtot)) / sum(rowSums(t(res.ca$call$Xtot))))
    col_coord <- col_coord %>%
      dplyr::mutate(numbers = dplyr::case_when(
        stringr::str_detect(.data$lvs, "^[^- ]+-(?![[:lower:]])|^[^- ]+(?<![[:lower:]])-")
        ~ stringr::str_extract(.data$lvs, "^[^- ]+"),
        TRUE ~ "" ))

    # Enlever les mots entre parentheses et les nombres
    if (cleannames == TRUE) col_coord <- col_coord %>%
      dplyr::mutate(lvs = stringr::str_remove_all(.data$lvs, cleannames_condition()))

    # Variable de couleur (colorvar) selon nb de caracteres indiques
    col_coord <- col_coord %>%
      dplyr::mutate(col_colorvar = as.factor(stringr::str_sub(.data$numbers, 1,
                                                              colcolor_numbers)))
    col_colorvar_recode <- levels(col_coord$col_colorvar)
    names(col_colorvar_recode) <- stringr::str_c(1:nlevels(col_coord$col_colorvar))
    col_coord <- col_coord %>%
      dplyr::mutate(col_colorvar = forcats::fct_recode(.data$col_colorvar,
                                                       !!!col_colorvar_recode)) %>%
      dplyr::mutate(colorvar = ifelse(.data$colorvar == "Sup_col", .data$colorvar,
                                      stringr::str_c(.data$colorvar, .data$col_colorvar))) %>%
      dplyr::select(-.data$col_colorvar) %>%
      # Afficher informations interactives au survol d'un point
      dplyr::mutate(interactive_text = stringr::str_c("<b>", .data$lvs, "</b>", "\n",
                                                      "Frequency: ",
                                                      round(.data$freq*100, 0), "%"),
                    lvs = stringr::str_replace_all(.data$lvs, "[^[:alnum:][:punct:]]",
                                                   " ") %>% stringr::str_squish()
      )


    if ("col" %in% tooltips) {
      # Calculer les % par colonne (de la variable en ligne)
      col_frequencies <- res.ca$call$Xtot %>% t %>% tibble::as_tibble() %>%
        tibble::add_row(!!!rowSums(res.ca$call$Xtot))
      col_frequencies <- col_frequencies %>% dplyr::mutate_all(~ ./rowSums(col_frequencies)) %>%
        dplyr::rename_all(~ stringr::str_remove_all(., cleannames_condition()))
      col_residuals <- col_frequencies %>%
        dplyr::mutate_all(~ . - .[nrow(col_frequencies)]) %>%
        dplyr::mutate_all(~ dplyr::case_when(
          round(.*100,0) >= 0 ~ stringr::str_c("+", round(.*100, 0), "%"),
          . < 0 ~ stringr::str_c(unbrk, #unbreakable space
                                 "-", round(abs(.)*100, 0), "%")
        )) %>% dplyr::slice(-nrow(col_frequencies))
      col_frequencies <- col_frequencies %>%
        dplyr::slice(-nrow(col_frequencies)) %>%
        dplyr::mutate_all(~ stringr::str_c(round(.*100, 0), "%")) %>%
        dplyr::mutate_all(~dplyr::case_when(
          stringr::str_length(.) >= 3 ~ .,
          stringr::str_length(.) < 3 ~ stringr::str_c(
            unbrk, unbrk, .), #Two unbreakable spaces
        ))
      col_frequencies <- col_frequencies %>%
        dplyr::bind_rows(col_residuals) %>%
        dplyr::mutate(number_of_rows = dplyr::row_number())
      col_frequencies <- col_frequencies %>%
        dplyr::mutate_at(dplyr::vars(-.data$number_of_rows), ~dplyr::case_when(
          number_of_rows > nrow(col_frequencies)/2 ~ NA_character_,
          TRUE ~ stringr::str_c("(",.[.data$number_of_rows + nrow(col_frequencies)/2],") ", .),
        )) %>%
        dplyr::slice(1:(nrow(col_frequencies)/2)) %>% dplyr::select(-.data$number_of_rows)
      col_frequencies <- purrr::map_dfc(1:ncol(col_frequencies),
                                        ~ dplyr::mutate_all(col_frequencies[.x],
                                                            function(.) stringr::str_c(colnames(col_frequencies)[.x], " : ", .)
                                        ))
      col_frequencies <- col_frequencies %>%
        tidyr::unite("col_text", sep = "\n") %>% dplyr::pull(.data$col_text)
      col_coord <- col_coord %>%
        dplyr::mutate(interactive_text = stringr::str_c(
          .data$interactive_text, "\n\n", coltips_subtitle, " :\n", col_frequencies))
    }

    if (show_sup == FALSE) {
      row_coord <- row_coord  %>%
        dplyr::filter(!stringr::str_detect(.data$colorvar, "Sup"))
      col_coord <- col_coord %>%
        dplyr::filter(!stringr::str_detect(.data$colorvar, "Sup"))
    }


    # Le Central point et son texte interactive :
    col_freq_text <- rowSums(res.ca$call$Xtot) %>%
      tibble::enframe(name = "lvs", value = "freq") %>%
      dplyr::mutate(freq = stringr::str_c(round(.data$freq/sum(.data$freq)*100, 0), "%")) %>%
      dplyr::mutate(lvs = stringr::str_remove_all(.data$lvs, cleannames_condition())) %>%
      tidyr::unite("row_freq", sep = ": ") %>%  dplyr::pull(.data$row_freq) %>%
      stringr::str_c(collapse = "\n")

    row_freq_text <- rowSums(t(res.ca$call$Xtot)) %>%
      tibble::enframe(name = "lvs", value = "freq") %>%
      dplyr::mutate(freq = stringr::str_c(round(.data$freq/sum(.data$freq)*100, 0), "%")) %>%
      dplyr::mutate(lvs = stringr::str_remove_all(.data$lvs, cleannames_condition())) %>%
      tidyr::unite("col_freq", sep = ": ") %>% dplyr::pull(.data$col_freq) %>%
      stringr::str_c(collapse = "\n")

    mean_point_data <- row_coord %>% dplyr::slice(1) %>%
      dplyr::mutate_at(dplyr::vars(tidyselect::starts_with("Dim")), ~ 0) %>%
      dplyr::mutate(lvs = NA_character_, freq = 1, colorvar = "Central_point",
                    numbers = NA_character_) %>%
      dplyr::mutate(interactive_text = stringr::str_c(
        "<b>Central point</b>\nFrequency: ", stringr::str_c(.data$freq*100, "%")))

    #if ("row" %in% tooltips) {     }     if ("col" %in% tooltips) {        }
    mean_point_data <- mean_point_data %>%
      dplyr::mutate(interactive_text = stringr::str_c(.data$interactive_text, "\n\n",
                                                      rowtips_subtitle, " :\n",
                                                      row_freq_text,
                                                      "\n\n", coltips_subtitle, " :\n",
                                                      col_freq_text))

    # Option pour afficher les lvs en majuscule (colonnes ou lignes) :
    if ("row" %in% uppercase) {
      row_coord <- row_coord  %>%
        dplyr::mutate(lvs = stringr::str_to_upper(.data$lvs, locale = "en"))
    }
    if ("col" %in% uppercase) {
      col_coord <- col_coord %>%
        dplyr::mutate(lvs = stringr::str_to_upper(.data$lvs, locale = "en"))
    }


    all_coord <- row_coord %>%
      dplyr::bind_rows(col_coord) %>%
      dplyr::mutate(colorvar = as.factor(.data$colorvar),
                    colorvar_names = as.factor(stringr::str_c("names_", .data$colorvar)),
                    id = dplyr::row_number()      )



    #Calculer les limites du graphique (argument a passer dans ggi pour regler la taille du htmlwidget)
    min_max_lims <- dplyr::select(all_coord, !!dim1, !!dim2)

    if (!missing(xlim)) min_max_lims <- min_max_lims %>%  tibble::add_row(!!dim1 := xlim[1]) %>% tibble::add_row(!!dim1 := xlim[2])
    if (!missing(ylim)) min_max_lims <- min_max_lims %>%  tibble::add_row(!!dim2 := ylim[1]) %>% tibble::add_row(!!dim2 := ylim[2])
    heigth_width_ratio <- min_max_lims %>% dplyr::summarise_all(~ max(., na.rm = TRUE) - min(., na.rm = TRUE), .groups = "drop")
    min_max_lims <-
      dplyr::bind_rows(dplyr::summarise_all(min_max_lims, ~ min(., na.rm = TRUE), .groups = "drop"),
                       dplyr::summarise_all(min_max_lims, ~ max(., na.rm = TRUE), .groups = "drop"))
    width_range <- dplyr::pull(heigth_width_ratio, 1)[1]
    heigth_width_ratio <- heigth_width_ratio %>% dplyr::summarise(heigth_width_ratio = !!dim2/!!dim1, .groups = "drop") %>% tibble::deframe()
    if (dist_labels[1] == "auto") dist_labels <- width_range/50

    theme_acm_with_lims <-
      if (use_theme) {
        if (!missing(xlim) & !missing(ylim))  {

          theme_facto(res = res.ca, axes = axes, no_color_scale = TRUE, size_scale_max = size_scale_max,  # legend.position = "bottom",
                      xlim = c(xlim[1], xlim[2]), ylim = c(ylim[1], ylim[2]))
        }
        else if (!missing(xlim) ) {
          theme_facto(res = res.ca, axes = axes, no_color_scale = TRUE, size_scale_max = size_scale_max,  # legend.position = "bottom",
                      xlim = c(xlim[1], xlim[2]) )
        }
        else if (!missing(ylim) )  {
          theme_facto(res = res.ca, axes = axes, no_color_scale = TRUE, size_scale_max = size_scale_max,  # legend.position = "bottom",
                      ylim = c(ylim[1], ylim[2]))
        }
        else {
          theme_facto(res = res.ca, axes = axes, no_color_scale = TRUE, size_scale_max = size_scale_max)  # legend.position = "bottom",
        }
      } else {
        NULL
      }



    outlims <- function(data, lim, dim) {
      dim <- rlang::enquo(dim)
      if (!is.na(lim[1])) data <- data %>% dplyr::filter(!!dim > lim[1])
      if (!is.na(lim[2])) data <- data %>% dplyr::filter(!!dim < lim[2])
      return(data)
    }

    if (text_repel == FALSE | out_lims_move == FALSE) {
      if (!missing(xlim)) all_coord <- all_coord %>% outlims(xlim, !!dim1)
      if (!missing(ylim)) all_coord <- all_coord %>% outlims(ylim, !!dim2)
    }


    scale_color_named_vector <-
      c("Central_point" = "black",   # Material colors :
        "Active_col1" = "#3f51b5", # Indigo 500
        "Active_col2" = "#673ab7", # Deep purple 500
        "Active_col3" = "#1976d2", # Blue 700
        "Active_col4" = "#7b1fa2", # Purple 700
        "Active_row1" = "#43a047", # Green 600
        "Active_row2" = "#f57c00", # Orange 700
        "Active_row3" = "#c0ca33", # Lime 600
        "Active_row4" = "#f4511e", # Deep orange 600
        "Active_row5" = "#7cb342", # Light green 600
        "Active_row6" = "#e53935", # Red 600
        "Active_row7" = "#fbc02d", # Jaune 700
        "Active_row8" = "#26a69a", # Teal 400

        "Sup_col"    =  "#b0bec5", # Blue grey 200
        "Sup_row"    =  "#bcaaa4", # Brown 200

        "names_Point_moyen" = "black",
        "names_Active_col1" = "#000051", # Indigo 900 Dark
        "names_Active_col2" = "#000063", # Deep purple 900 Dark
        "names_Active_col3" = "#002171", # Blue 900 Dark
        "names_Active_col4" = "#12005e", # Purple 900 Dark
        "names_Active_row1" = "#00600f", # Green 700 Dark
        "names_Active_row2" = "#bb4d00", # Orange 700 Dark
        "names_Active_row3" = "#7c8500", # Lime 700 Dark
        "names_Active_row4" = "#ac0800", # Deep orange 700 Dark
        "names_Active_row5" = "#4b830d", # Light green 600 Dark
        "names_Active_row6" = "#ab000d", # Red 600 Dark
        "names_Active_row7" = "#c49000", # Jaune 700 Dark
        "names_Active_row8" = "#00766c", # Teal 400 Dark

        "names_Sup_col" = "#808e95", # Blue grey 200 Dark
        "names_Sup_row" = "#8c7b75" # Brown 200 Dark
      )


    if (!missing(title)) {
      title_graph <- ggplot2::labs(title = title) #stringr::str_c("Les Active variables de l'ACM sur les axes ",axes[1], " et ", axes[2] )
    } else {
      title_graph <- NULL
    }

    graph_mean_point <-
      ggiraph::geom_point_interactive(
        data = mean_point_data,
        ggplot2::aes(x = !!dim1, y = !!dim2, tooltip = .data$interactive_text),
        color = "black", shape = 3, size = 5, stroke = 1.5, fill = "black", na.rm = TRUE
      )

    graph_theme_acm <-
      list(theme_acm_with_lims,
           ggplot2::scale_colour_manual(values = scale_color_named_vector,
                                        aesthetics = c("colour", "fill")),
           ggplot2::theme(plot.margin = ggplot2::margin(r = right_margin, unit = "cm")),
           title_graph)


    #Sorties :
    if (type[1] == "points") {
      plot_output <- ggplot2::ggplot() + graph_theme_acm +
        ggrepel::geom_text_repel(
          data = all_coord,
          ggplot2::aes(x = !!dim1, y = !!dim2, label = .data$lvs,
                       color = .data$colorvar_names),
          size = text_size, hjust = "left", nudge_x = dist_labels, direction = "y",
          segment.colour = "black",
          segment.alpha = 0.2, point.padding = 0.25, na.rm = TRUE
        ) + #0.25, # min.segment.length = 0.8, max.iter = 10000 #repel_max_iter #fontface = "bold", max.iter = 50000
        ggiraph::geom_point_interactive(
          data = all_coord,
          ggplot2::aes(x = !!dim1, y = !!dim2, size = .data$freq,
                       color = .data$colorvar, shape = .data$colorvar,
                       tooltip = .data$interactive_text, #fill = .data$colorvar,
                       data_id = .data$id),
          stroke = 1.5, na.rm = TRUE
        ) +
        graph_mean_point +
        ggplot2::scale_shape_manual(values = c(
          #"Central_point" = 1,
          "Active_col1" = 17,
          "Active_col2" = 17,
          "Active_col3" = 17,
          "Active_col4" = 17,
          "Active_row1" = 18,
          "Active_row2" = 18,
          "Active_row3" = 18,
          "Active_row4" = 18,
          "Sup_col"    = 17,
          "Sup_row"    = 18  ))

      css_hover <- ggiraph::girafe_css("fill:gold;stroke:orange;",
                                       text = "color:gold4;stroke:none;")
      plot_output <- plot_output %>% append(c("css_hover" = css_hover))

    } else if (type[1] == "text") {
      if (text_repel == FALSE) {
        graph_text <-
          ggiraph::geom_text_interactive(data = all_coord,
                                         ggplot2::aes(x = !!dim1, y = !!dim2,
                                                      label   = .data$lvs,
                                                      color   = .data$colorvar,
                                                      tooltip = .data$interactive_text,
                                                      data_id = .data$id),
                                         size = text_size, fontface = "bold",  na.rm = TRUE)
      } else {
        graph_text <-
          ggrepel::geom_text_repel(data = all_coord,
                                   ggplot2::aes(x = !!dim1, y = !!dim2,
                                                label = .data$lvs,
                                                color = .data$colorvar),
                                   size = text_size, na.rm = TRUE, fontface = "bold",
                                   direction = "both", # segment.alpha = 0.5,# point.padding = 0.25, segment.colour = "black",
                                   min.segment.length = 0.4, arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines")))
      }
      plot_output <- ggplot2::ggplot() + graph_theme_acm + graph_text + graph_mean_point


    } else if (type[1] == "labels") {
      if (text_repel == FALSE) {
        graph_text <-
          ggiraph::geom_label_interactive(data = all_coord,
                                          ggplot2::aes(x = !!dim1, y = !!dim2,
                                                       label   = .data$lvs,
                                                       color   = .data$colorvar,
                                                       tooltip = .data$interactive_text,
                                                       data_id = .data$id),
                                          size = text_size, fontface = "bold",  na.rm = TRUE)
      } else {
        graph_text <-
          ggrepel::geom_label_repel(data = all_coord,
                                    ggplot2::aes(x = !!dim1, y = !!dim2,
                                                 label = .data$lvs,
                                                 color = .data$colorvar),
                                    size = text_size, na.rm = TRUE, fontface = "bold",
                                    direction = "both", # segment.alpha = 0.5,# point.padding = 0.25, segment.colour = "black",
                                    min.segment.length = 0.5, arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines")))
      }
      plot_output <- ggplot2::ggplot() + graph_theme_acm + graph_text + graph_mean_point
    }

    #Add informations in the ggplot2::ggplot object, to be used into ggi() (without losing ggplot2::ggplot class)
    css_tooltip <- "text-align:right;padding:4px;border-radius:5px;background-color:#eeeeee;color:black;" #
    plot_output <- plot_output %>% append(c("css_tooltip" = css_tooltip)) %>%
      append(c("heigth_width_ratio" = heigth_width_ratio)) %>%
      `attr<-`("class", c("gg", "ggplot"))
    return(plot_output)
  }

# (ggca(res.ca, show_sup = TRUE,
#          rowcolor_numbers = 4,
#          rowtips_subtitle = "Groupe socio-pro\nil y a 5 ans")  +
#     xlim(c(-0.7,1.2)) + ylim(c(-0.7,0.5))) %>%
#   ggi("ggiraph")

# girafe_plot %>%
#   frameWidget(width = "120%")
# saveWidget("Girafe.html")

#
# FES2017 %>%
#   dplyr::mutate() %>%
#   tabw(CSER, PR2017ALL1, wt = w5, tot = "no",
#        rare_to_other = TRUE, subtext = champ_inscrits) %>%
#   purrr::flatten_df() %>% dplyr::mutate(dplyr::across(tidyselect::where(rlang::is_decimal), as.double)) %>% tibble::column_to_rownames(colnames(.)[1]) %>%
#   FactoMineR::CA() %>%
#   ggca(size_scale_max = 6) %>%  #+ ggtitle("Vote au premier tour 2017 en fonction de la CSP : analyse des correspondances")) %>%
#   ggi("ggiraph")








# # HCPC ----




# data <- pc_AGD
# wt <- expr(POND)
# vars <- variables_actives

# exclure_categories <- c(NA, "NA", "3-Livre: 1-9")
# excl = exclure_categories


#' Multiple Tables for Hierarchical Clusters
#'
#' @param data A data frame.
#' @param row_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> The row variables
#' of the table, to cross with the clusters. Typically, actives variables of the MCA.
#' @param clust In columns, the variable with the clusters, typically made with hierarchical
#' clustering functions like \code{\link[FactoMineR]{HCPC}} (object
#' `res$data.clust$clust`). Can be either a symbol or a character vector of
#' length 1 (for vars in `data`), or an external variable (not in `data`)
#' provided its length is equal to the number of rows of `data`.
#' @param wt The name of the weight variable. Leave empty for unweighted results.
#' @param excl The name of the levels to exclude, as a character vector.
# @param recode_helper Set to `TRUE` to print a helper to recode levels.
#' @param color The type of colors to print, see \code{\link[tabxplor]{tab}}.
#' @param pct The type of percentages to print, see \code{\link[tabxplor]{tab}}.
#' Default to column percentages
#' @param row_tot The name of the total line (frequencies of each cluster)
#' @param ... Additional arguments to pass to \code{\link[tabxplor]{tab_many}}.
#'
#' @return A \code{tibble} of class \code{tab}, possibly with colored reading helpers.
#' @export
#'
#'@examples
#'
#' data(tea, package = "FactoMineR")
#' res.mca_3axes <- MCA2(tea, active_vars = 1:18, ncp = 3)
#' cah <- FactoMineR::HCPC(res.mca_3axes, nb.clust = 6, graph = FALSE)
#' tea$clust <- cah$data.clust$clust
#' HCPC_tab(tea, row_vars = all_of(names(tea)[1:18]), clust = "clust") #|>
#' #tabxplor::tab_kable()
#'
HCPC_tab <- function(data, row_vars = character(), clust, wt,
                     excl = character(), # recode_helper = FALSE,
                     color = "diff", pct = "col",
                     row_tot = "% of population",
                     ...) {
  #active <- names(CAH$data.clust)[names(CAH$data.clust) != "clust"]

  row_vars <- tidyselect::eval_select(rlang::enquo(row_vars), data)
  row_vars <- names(row_vars)

  if (missing(wt)) {
    wt <- character()
  } else {
    wt <- as.character(rlang::ensym(wt))
  }

  clust <- rlang::enquo(clust)

  safe_clust <- purrr::safely(rlang::eval_tidy)(clust)

  if (is.null(safe_clust$error)) {
    clust_is_var <- (is.factor(safe_clust$result) |
                       is.character(safe_clust$result)) &
      length(safe_clust$result) == nrow(data)

  } else {
    clust_is_var <- FALSE
  }

  if (clust_is_var) {
    # clust <- safe_clust$result
    data <- data |>
      dplyr::select(tidyselect::all_of(row_vars), tidyselect::all_of(wt) ) |>
      levels_to_na(tidyselect::all_of(row_vars), excl = excl,
                   levels_to = "Remove levels") |>
      tibble::add_column(clust = safe_clust$result )

  } else {
    data <- data |> dplyr::select(tidyselect::all_of(row_vars),
                                  tidyselect::all_of(wt),
                                  clust = !!clust ) |>
      levels_to_na(tidyselect::all_of(row_vars), excl = excl,
                   levels_to = "Remove levels")
  }


  if (length(wt) == 0) {
    wt <- rlang::expr(NA)
  } else {
    wt <- rlang::sym(wt)
  }

  first_lvs <- dplyr::select(data, tidyselect::all_of(row_vars)) |>
    purrr::map_chr(~ dplyr::if_else(nlevels(.) == 2L, "first", "all"))

  #if(recode_helper) tabxplor:::fct_recode_helper(data, "clust")

  cah_actives_tab <- tabxplor::tab_many(data, "clust", tidyselect::all_of(row_vars),
                                        pct = dplyr::if_else(pct == "row", "col", "row"),
                                        wt = !!wt,
                                        na = "drop", cleannames = TRUE, color = color,
                                        levels = first_lvs, add_n = FALSE,
                                        ...) |>
    dplyr::rename_with(~ dplyr::if_else(stringr::str_detect(., "Total_", ), "Total", .)) |>
    dplyr::relocate(.data$Total, .after = tidyselect::last_col()) |>
    dplyr::mutate(
      Total = dplyr::mutate(.data$Total,
                            wn = dplyr::if_else(is.na(.data$wn), as.double(.data$n), .data$wn)),
      Total = vctrs::`field<-`(.data$Total, "pct",
                               vctrs::field(.data$Total, "wn") /
                                 dplyr::last(vctrs::field(.data$Total, "wn"))) |>
        tabxplor::set_col_var("Total")
    )

  col_var <- tabxplor::get_col_var(cah_actives_tab)[tabxplor::get_col_var(cah_actives_tab) != ""]
  col_var_total <- purrr::set_names(dplyr::last(names(col_var)), "Total" )
  col_var <- col_var[-length(col_var)]
  col_var <- c(purrr::set_names(names(col_var), col_var), col_var_total)

  cah_actives_tab <- cah_actives_tab |>
    tab_transpose() |>
    dplyr::rename("lvs" = "variables") |>
    dplyr::mutate(variables = forcats::fct_recode(.data$lvs, !!!col_var), .before = 1) |>
    dplyr::rename("Ensemble" = "Total")

  cah_actives_tab <- cah_actives_tab |>
    dplyr::filter(!stringr::str_detect(.data$lvs, "Remove levels")) |>
    dplyr::mutate(
      lvs = forcats::fct_recode(.data$lvs, !!!purrr::set_names("Total", row_tot)),

      ## not a good idea : unbreakable spaces should be used at the end, in tab_kable()
      # lvs = forcats::fct_relabel(.data$lvs,
      #                            ~ stringr::str_replace_all(., " ", unbrk))
    )

  n_rows <- dplyr::filter(cah_actives_tab, tabxplor::is_totrow(cah_actives_tab)) |>
    dplyr::mutate(
      variables = factor("Total"),
      lvs = factor("n"),
      dplyr::across(where(tabxplor::is_fmt),
                    ~ dplyr::mutate(., display = "n", in_totrow = FALSE))
    )
  cah_actives_tab <- dplyr::bind_rows(cah_actives_tab, n_rows) |>
    dplyr::group_by(.data$variables)


  cah_actives_tab <- cah_actives_tab |>
    dplyr::mutate(dplyr::across(
      where(tabxplor::is_fmt), ~ dplyr::if_else(.$display == "mean",
                               true  = dplyr::mutate(., diff = 0, digits = 2L) |>
                                 tabxplor::as_totrow(),
                               false = .)
    ))



  cah_actives_tab
}










# Others ----








#' A ggplot2 Theme for Geometrical Data Analysis
#'
#' @param res An object created with \code{FactoMineR::\link[FactoMineR]{MCA}},
#' \code{\link[FactoMineR]{CA}}, etc.
#' @param axes The axes to print, as a numeric vector of length 2.
#' @param legend.position One of \code{c("none", "left", "right", "bottom", "top")}.
#' @param no_color_scale When TRUE, you can provide color_scale next without warning.
#' @param size_scale_max Maximum size of the points.
#' @param xlim Horizontal axe limits.
#' @param ylim Vertical axe limits.
#'
#' @return A list of ggplot2 objects.
#'
#' @export
theme_facto <- function(res, axes = c(1,2), # res = res.mca
                        legend.position = c("none", "left", "right", "bottom", "top"),
                        no_color_scale = FALSE, size_scale_max = 8, xlim, ylim) {  #no_size_scale = FALSE
  if (exists("axes_names", where = res)) {
    first_axe_title  <-
      stringr::str_c(
        "Axe ", axes[1]," (", round(res$eig[axes[1],2], 1),
        "%)",
        if (!is.null(res$axes_names[axes[1]]) ) paste0(" : ", res$axes_names[axes[1]])
      )
    second_axe_title <-
      stringr::str_c(
        "Axe ", axes[2]," (", round(res$eig[axes[2],2], 1),
        "%)",
        if (!is.null(res$axes_names[axes[2]]) ) paste0(" : ", res$axes_names[axes[2]])
      )
  } else {
    first_axe_title  <-
      stringr::str_c("Axe ", axes[1]," (",
                     round(res$eig[axes[1],2], 1), "%)")
    second_axe_title <-
      stringr::str_c("Axe ", axes[2]," (",
                     round(res$eig[axes[2],2], 1), "%)")
  }



  if (no_color_scale == FALSE) {
    scale_color_acm <- ggplot2::scale_color_brewer(palette = "Dark2") #material_colors_light() ?
    scale_fill_acm <- ggplot2::scale_fill_brewer(palette = "Dark2")
  } else {
    scale_color_acm <- NULL
    scale_fill_acm <- NULL
  }

  if (!missing(xlim) & !missing(ylim)) {coord_graph <- ggplot2::coord_fixed(xlim = xlim, ylim = ylim) }
  else if (!missing(xlim) ) { coord_graph <- ggplot2::coord_fixed(xlim = xlim ) }
  else if (!missing(ylim) ) { coord_graph <- ggplot2::coord_fixed(ylim = ylim ) }
  else { coord_graph <- ggplot2::coord_fixed() }

  #if (no_size_scale == FALSE) {
  scale_size <- ggplot2::scale_size_area(max_size = size_scale_max)
  #} else {
  #  scale_size <- NULL
  #}
  list(
    ggplot2::geom_hline(yintercept = 0, color="black", linetype = "dashed"), # Horizontal axe
    ggplot2::geom_vline(xintercept = 0, color="black", linetype = "dashed"), # Vertical axe
    ggplot2::labs(x = first_axe_title, y =  second_axe_title),
    scale_size,
    scale_color_acm, #Color palette
    scale_fill_acm,
    coord_graph, #Assure that proportion between the two axes are kept
    ggplot2::theme_minimal(),
    ggplot2::theme(legend.position = legend.position[1],
                   panel.grid.minor = ggplot2::element_blank(), #element_line(size = 0.05, color="gray96"),
                   panel.grid.major = ggplot2::element_blank(), #element_line(size = 0.05, color="gray96"),
                   strip.text = ggplot2::element_text(face = "bold"), #Titles of facets
                   plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"), #Center titre of graph
                   axis.title.x = ggplot2::element_text(size = 12, hjust = 1),
                   axis.title.y = ggplot2::element_text(size = 12, hjust = 1),
                   text = ggplot2::element_text(family = "sans") #"DejaVu Sans Condensed"
    )
  )
}

# theme_ac <- function(axes = c(1,2), res = res.ca) {
#   res.ca <- res.ca
#   list(
#     geom_hline(yintercept = 0, color="black", linetype = "dashed"), #Axe horizontal
#     geom_vline(xintercept = 0, color="black", linetype = "dashed"), #Axe vertical
#     labs(x = paste0("Axe ", axes[1]," (", round(res.ca$eig[axes[1],2], 1), "%)"), # Titres des axes
#          y = paste0("Axe ", axes[2]," (", round(res.ca$eig[axes[2],2], 1), "%)") ),
#     scale_size_area(max_size = 10), #Echelle de taille des points
#     scale_color_brewer(palette = "Dark2"), #Palette de couleurs
#     scale_fill_brewer(palette = "Dark2"),
#     coord_fixed(), #Assurer que les proportions relatives des deux axes sont respectees
#     theme_minimal(),
#     theme(legend.position = "none",
#           panel.grid.minor = element_blank(), #element_line(size = 0.05, color="gray96"),
#           panel.grid.major = element_blank(), #element_line(size = 0.05, color="gray96"),
#           strip.text = element_text(face = "bold"), #Titre des facets
#           plot.title = element_text(hjust = 0.5, face = "bold"), #Centrer le titre du graphique
#           axis.title.x = element_text(size = 12, hjust = 1),
#           axis.title.y = element_text(size = 12, hjust = 1) )
#   )
# }

#' Title Scale color light for MCA.
#'
#' @return A character vector of color codes, with color names.
#' @export
#'
#' @examples material_colors_light()
material_colors_light <- function() {
  c(    # Material colors :
    "Deep purple 300" = "#9575cd", #"#7e57c2",
    "Orange 700"      = "#f57c00",
    "Light green 600" = "#7cb342",

    "Teal 400"        = "#26a69a",
    "Red 700"         = "#d32f2f",
    "Lime 800"        = "#9e9d24",

    #"Jaune 800"       = "#f9a825",  #reserved for hover effect
    "Brown 400"       = "#8d6e63",
    "Purple 300"      = "#ba68c8",

    "Pink 300"        = "#f06292",
    "Green 800"       = "#388e3c",
    "Blue 400"        = "#42a5f5",

    "Blue Grey 500"   = "#607d8b"
  )
}

#' Title Scale color dark for MCA.
#'
#' @return A character vector of color codes, with color names.
#' @export
#'
#' @examples material_colors_dark()
material_colors_dark <- function() {
  c(
    "Deep purple 300"  =    "#65499c", #"#4d2c91",
    "Orange 700"       =    "#bb4d00",
    "Light green 600"  =    "#4b830d",

    "Teal 400"         =    "#00766c",
    "Red 700"          =    "#9a0007",
    "Lime 800"         =    "#6c6f00",

    #"Jaune 800"        =    "#c17900",  #reserved for hover effect
    "Brown 400"        =    "#5f4339",
    "Purple 300"       =    "#883997",

    "Pink 300"         =    "#ba2d65",
    "Green 800"        =    "#00600f",
    "Blue 400"         =    "#0077c2",

    "Blue Grey 500"    =    "#34515e"
  )
}



# data(tea, package = "FactoMineR")
# res.mca <- MCA2(tea, active_vars = 1:18)
# plot <- res.mca %>%
#   ggmca(tea, sup_vars = c("SPC"), ylim = c(NA, 1.2), text_repel = TRUE)
# width = NULL
# height = NULL
# keep_ratio = TRUE
# savewidget = FALSE
# dir = NULL
# name = "Plot"
# replace = FALSE
# open = rlang::is_interactive()
# iframe = NULL
# # pixel_width

#' Pass a MCA plot into a html interactive plot
#' @param plot The plot, created with \link{ggmca} or \link{ggca}.
#' @param width The width in centimeters. Default to printing device's size.
#' @param height The height in centimeters. Default to printing device's size.
#' @param keep_ratio By default, the height is forced based of the relative
#' size of the MCA's axes. Set to \code{FALSE} to avoid this behavior.
#' @param savewidget Should the html widget be saved on disk ?
#' @param dir If saved as file, the directory in which to save the html widget.
#' Default to temporary directory. Set global option \code{"ggfacto.export_dir"}
#' with \code{link[base:options](options)} to change default directory.
#' @param open Should the resulting file be opened at once ?
#' @param name The name of the file to save.
#' @param replace Replace file ? By default, number added to find a new name.
#' @param iframe Create an html frame around the plot to ensure fixed
#' dimensions. Useful when opening the plot in a web browser (but will produce a blank
#' graph with \pkg{rmarkdown}). This is default behavior with \code{savewidget = TRUE}.
#' Require package \code{\link[widgetframe:widgetframe]{widgetframe}}.
#' @param pixel_width The width in pixels for
#'  \code{\link[widgetframe:widgetframe]{widgetframe}}.
#' @param ... Additional arguments to pass to \code{\link[ggiraph:girafe]{girafe}} and
#' \code{\link[ggiraph:dsvg]{dsvg}}. \code{fonts} can be used to provide text fonts.
#'
#' @return An html plot.
#' @export
#'
# @examples
ggi <- function(plot = ggplot2::last_plot(),
                width = NULL, height = NULL, keep_ratio = TRUE,
                savewidget = FALSE, dir = NULL, name = "Plot", replace = FALSE,
                open = rlang::is_interactive(),
                iframe = NULL, pixel_width, ...
) {

  if (is.null(iframe)) iframe <- savewidget

  if ("css_hover" %in% names(plot)) {
    css_hover <- plot$css_hover
  } else {
    css_hover <- ggiraph::girafe_css("fill:#d2b200;stroke:orange;",
                                     text  = "color:gold4;stroke:none;",
                                     point = "fill:gold;stroke:orange;",
                                     area  = "fill:#ffe348")
  }


  if ("css_tooltip" %in% names(plot)) {
    css_tooltip <- plot$css_tooltip
  } else {
    css_tooltip <- "color:#000000;text-align:right;padding:4px;border-radius:5px;background-color:#eeeeee;"
  }

  # if(.Platform$OS.type == "windows") {
  #   css_tooltip <-
  #     paste0(css_tooltip, "font-family:", grDevices::windowsFonts("sans"), ";") #%>%
  #   #stringr::str_replace("DejaVu Sans Condensed", "DejaVu Sans")
  # }

  if (is.null(width)) { #   if (missing(width)) {
    width <- grDevices::dev.size("in")[1]

  } else {
    width <- width/2.54
  }

  if (keep_ratio == TRUE & !is.null(plot$heigth_width_ratio)) {
    height <- width * plot$heigth_width_ratio

  } else {
    if (is.null(height)) { #     if (missing(height)) {
      height <- grDevices::dev.size("in")[2]
    } else {
      height = height/2.54
    }
  }

  # if (is.null(plot$heigth_width_ratio)) height <- NULL

  widget <-
    ggiraph::girafe(ggobj = plot,
                    width_svg = width,
                    height_svg = height , #if_else(missing(height), width/2.563 * plot$heigth_width_ratio, height/2.563)
                    # fonts = ifelse(.Platform$OS.type == "windows",
                    #                grDevices::windowsFonts("sans") %>%
                    #                  purrr::map(~stringr::str_replace(., "DejaVu Sans Condensed",
                    #                                                   "DejaVu Sans")),
                    #                NULL
                    # ),  #list(sans = "DejaVu Sans Condensed") #grDevices::windowsFonts("sans")
                    ...
    ) %>%
    ggiraph::girafe_options(ggiraph::opts_tooltip(css = css_tooltip), #, use_fill = TRUE, #use_stroke = FALSE, # = border color of the tooltip #color:white; border-color:black; opacity:1 ; background-color:transparent
                            ggiraph::opts_hover(css = css_hover)
                            # ggiraph::opts_zoom(max = 5) # bugue pas mal
                            # ggiraph::opts_hover(css = girafe_css(css = "fill:purple;stroke:black;", text = "stroke:none;fill:red;font-style:bold;")) #    point = NULL, line, area, image
                            # ggiraph::opts_hover_inv(css = "opacity:0.1;"),
                            # ggiraph::opts_sizing(rescale = FALSE)
                            # ggiraph::opts_sizing(rescale = TRUE, width = 0.7), #between 0 and 1
                            # ggiraph::opts_toolbar(saveaspng = FALSE)
    )


  if (iframe == TRUE) {
    requireNamespace("widgetframe", quietly = TRUE)
    if (missing(pixel_width)) pixel_width <- grDevices::dev.size("px")[1]

    widget <-
      widgetframe::frameWidget(widget, width = pixel_width,
                               options = widgetframe::frameOptions(
                                 title = name,
                                 name = name
                               ))
    #Title and name options : options = widgetframe::frameOptions(name = "Graphique")
  }

  if (savewidget == FALSE) {
    return(widget)

  } else {
    path <- plot_path(dir = dir, name = name, extension = "html", replace = replace)

    if (iframe == FALSE) {
      requireNamespace("htmlwidgets", quietly = TRUE)
      htmlwidgets::saveWidget(widget, path, title = name)

    } else {
      requireNamespace("widgetframe", quietly = TRUE)
      widgetframe::saveWidgetframe(widget, path, selfcontained = TRUE)
    }

    if (open == TRUE) file.show(path)

    return(invisible(widget))
  }
}



#' Save a plot as image
#' @param plot The plot, created with \pkg{ggplot2}.
#' @param xt The extension name, when saving as image (interactive graph will
#' always be .html).
#' @param dpi The resolution.
#' @param width The width in centimeters.
#' @param height The height in centimeters. By default, \code{width/1.41}.
#' @param scale Fixed ratio between horizontal and vertical axes.
#' @param dir If saved as file, the directory in which to save the html widget.
#' Default to temporary directory. Set global option \code{"ggfacto.export_dir"}
#' with \code{link[base:options]{options}} to change default directory.
#' @param open Should the resulting file be opened at once ?
#' @param name The name of the file to save.
#' @param replace Replace file ? By default, number added to find a new name.

#'
#' @return Creates a file, and opens it in `RStudio` viewer, as a side effect.
#' @export
#'
ggsave2 <- function(plot = ggplot2::last_plot(),
                    dir = NULL, name = "Plot", xt = "png", dpi = 600,
                    width = 21, height, scale = 1,
                    replace = FALSE, open = rlang::is_interactive()) {

  if (missing(height)) {
    if (exists("plot$heigth_width_ratio")) {
      height <- width * plot$heigth_width_ratio
    } else {
      height <- width / 1.418919
    }
  }

  path <- plot_path(dir = dir, name = name, extension = xt, replace = replace)

  ggplot2::ggsave(path, plot = plot, height = height, width = width, units = "cm",
                  scale = scale, dpi = dpi)

  if (open == TRUE) file.show(path)

  invisible(plot)
}





# res.ca <- FES2017 %>%
#   dplyr::mutate() %>%
#   tab(CSER, PR2017ALL1, wt = w5, tot = "no",
#        rare_to_other = TRUE, subtext = champ_inscrits) %>%
#   purrr::flatten_df() %>% dplyr::mutate(dplyr::across(tidyselect::where(is_decimal), as.double)) %>% tibble::column_to_rownames(colnames(.)[1]) %>%
#   FactoMineR::CA()
#
# PR1ac <- ggca(res.ca) + ggplot2::ggplot
#
# ggi(PR1ac, "ggiraph", pixel_width = 700) %>%
#   widgetframe::saveWidgetframe("widget.html", selfcontained = TRUE) ; file.show("widget.html")








#To add : - colomn with frequencies divided one by another to see if logit brings
#something more than the cross-table

#' Modified odd ratios plot from `finalfit`
# Licence MIT : https://finalfit.org/LICENSE-text.html
# Thanks to Ewen M Harrison.
#'
#' @param .data Data frame.
#' @param dependent Character vector of length 1: name of dependent variable
#' (must have 2 levels).
#' @param explanatory Character vector of any length: name(s) of explanatory variables.
#' @param random_effect Character vector of length 1, name of random effect variable.
#' @param factorlist Option to provide output directly from \code{summary_factorlist()}.
#' @param glmfit 	Option to provide output directly from \code{glmmulti()} and \code{glmmixed()}.
#' @param confint_type One of \code{c("profile", "default")} for GLM models or
#' \code{c("default", "Wald", "profile", "boot")} for \code{glmer models}.
#' Note \code{"default" == "Wald"}.
#' @param remove_ref 	Logical. Remove reference level for factors.
#' @param break_scale Manually specify x-axis breaks in format \code{c(0.1, 1, 10)}.
#' @param column_space 	Adjust table column spacing.
#' @param dependent_label Main label for plot.
#' @param prefix Plots are titled by default with the dependent variable. This adds
#' text before that label.
#' @param suffix Plots are titled with the dependent variable. This adds text after
#' that label.
#' @param table_text_size Alter font size of table text.
#' @param title_text_size Alter font size of title text.
#' @param plot_opts A list of arguments to be appended to the ggplot call by \code{"+"}.
#' @param table_opts A list of arguments to be appended to the ggplot table call by
#'  \code{"+"}.
#' @param return_df To return the dataframe.
#' @param ... Other parameters.

#' @return The odd ratios plot as a \code{ggplot2} object.
#' @export
#'
# @examples
pers_or_plot <-
  function (.data, dependent, explanatory, random_effect = NULL,
            factorlist = NULL, glmfit = NULL, confint_type = NULL, remove_ref = FALSE,
            break_scale = NULL, column_space = c(-0.5, 0, 0.2), dependent_label = NULL,
            prefix = "", suffix = ": OR (95% CI, p-value)",
            table_text_size = 5, title_text_size = 18, plot_opts = NULL,
            table_opts = NULL, return_df = FALSE, ...) {
    requireNamespace("finalfit", quietly = TRUE)

    # sansF <- grDevices::windowsFonts("sans")
    # grDevices::windowsFonts(sans = windowsFont("TT Arial"))
    # grDevices::windowsFonts() %>% print()

    if (!is.null(factorlist)) {
      if (is.null(factorlist$Total))
        stop("summary_factorlist function must include total_col=TRUE")
      if (is.null(factorlist$fit_id))
        stop("summary_factorlist function must include fit_id=TRUE")
    }
    if (is.null(factorlist)) {
      factorlist = finalfit::summary_factorlist(.data, dependent, explanatory,
                                                total_col = TRUE, fit_id = TRUE)
    }
    if (remove_ref) {
      factorlist = factorlist %>%
        dplyr::mutate(label = ifelse(.data$label == "", NA, .data$label)) %>%
        tidyr::fill(.data$label) %>%
        dplyr::group_by(.data$label) %>%
        dplyr::filter(dplyr::row_number() != 1 | dplyr::n() > 2) %>%
        finalfit::rm_duplicate_labels()
    }
    # if (is.null(breaks)) {
    #   breaks = scales::pretty_breaks()
    # }
    if (is.null(confint_type) && is.null(random_effect)) {
      confint_type = "profile"
    }
    else if (is.null(confint_type) && (!is.null(random_effect) |
                                       inherits(glmfit, "glmerMod"))) {
      confint_type = "default"
    }
    if (is.null(glmfit) && is.null(random_effect)) {
      glmfit = finalfit::glmmulti(.data, dependent, explanatory)
      glmfit_df_c = finalfit::fit2df(glmfit, condense = TRUE, estimate_suffix = " (multivariable)",
                                     confint_type = confint_type, ...)
    }
    else if (is.null(glmfit) && !is.null(random_effect)) {
      glmfit = finalfit::glmmixed(.data, dependent, explanatory, random_effect)
      glmfit_df_c = finalfit::fit2df(glmfit, condense = TRUE, estimate_suffix = " (multilevel)",
                                     confint_type = confint_type, ...)
    }
    if (!is.null(glmfit) && is.null(random_effect)) {
      glmfit_df_c = finalfit::fit2df(glmfit, condense = TRUE, estimate_suffix = " (multivariable)",
                                     confint_type = confint_type, estimate_name = "OR",
                                     exp = TRUE, ...)
    }
    else if (!is.null(glmfit) && !is.null(random_effect)) {
      glmfit_df_c = finalfit::fit2df(glmfit, condense = TRUE, estimate_suffix = " (multilevel)",
                                     confint_type = confint_type, estimate_name = "OR",
                                     exp = TRUE, ...)
    }
    glmfit_df = finalfit::fit2df(glmfit, condense = FALSE, confint_type = confint_type,
                                 estimate_name = "OR", exp = TRUE, ...)
    df.out = finalfit::finalfit_merge(factorlist, glmfit_df_c)
    df.out = finalfit::finalfit_merge(df.out, glmfit_df, ref_symbol = "1.0")
    df.out$Total = stringr::str_remove(df.out$Total, " \\(.*\\)") %>%
      as.numeric()
    df.out$Total[which(df.out$levels %in% c("Mean (SD)",
                                            "Median (IQR)"))] = dim(.data)[1]
    df.out$levels[which(df.out$levels %in% c("Mean (SD)",
                                             "Median (IQR)"))] = "-"
    if (any(is.na(df.out$label))) {
      remove_rows = which(is.na(df.out$label))
      df.out = df.out[-remove_rows, ]
    }
    else {
      df.out
    }


    #Added :
    if (return_df == FALSE) {
      log_range <- max(as.numeric(df.out$OR)) + max(1/as.numeric(df.out$OR))
      if (missing(break_scale)) {
        break_scale <- dplyr::case_when(
          log_range < 4/8  ~ 16,
          log_range < 4/4  ~ 8,
          log_range < 4/2  ~ 4,
          log_range < 4    ~ 2,
          log_range < 4*2  ~ 1,
          log_range < 4*4  ~ 1/2,
          log_range < 4*8  ~ 1/4,
          log_range < 4*16 ~ 1/8,
          TRUE             ~ 1/16)
      }

      inverse_breaks <-
        sort((1:max(round(1/as.numeric(df.out$OR, 0))*2*break_scale)),
             decreasing = T)/break_scale
      legend_ticks_breaks <- c(1/inverse_breaks,
                               1:(max(round(as.numeric(df.out$OR), 0)*2*break_scale))/break_scale, 1) %>%
        unique() %>% sort()
      legend_ticks_labels <- ifelse(legend_ticks_breaks < 1,
                                    yes = stringr::str_c("1/", inverse_breaks),
                                    no = stringr::str_remove_all(as.character(
                                      legend_ticks_breaks), "0+$|\\.$"))

      #unbrk <- stringi::stri_unescape_unicode("\\u202f")

      df.out <- df.out %>%
        dplyr::mutate(freq = (
          (as.numeric(stringr::str_remove(df.out[, 5], " \\(.*\\)"))/.data$Total*100) %>%
            round(0) %>% stringr::str_c("%") %>% stringr::str_pad(4)
        )) %>%
        dplyr::mutate(levels = stringr::str_c(.data$levels, " (", .data$freq, ")")) %>%
        dplyr::mutate(`OR (multivariable)` = dplyr::case_when(
          `OR (multivariable)` == "-" ~ "Reference",                               #There were unbreakable spaces.
          OR >= 1 & p <  0.001 ~ stringr::str_c(        format(round(  as.numeric(OR), digits = 2), nsmall = 2), "***"),
          OR >= 1 & p <  0.005 ~ stringr::str_c(        format(round(  as.numeric(OR), digits = 2), nsmall = 2), "**" , paste0(rep(unbrk, 2), collapse = "")),            #Unbreakable space
          OR >= 1 & p <  0.01  ~ stringr::str_c(        format(round(  as.numeric(OR), digits = 2), nsmall = 2), "*"  , paste0(rep(unbrk, 4), collapse = "")),
          OR >= 1 & p >= 0.01  ~ stringr::str_c(        format(round(  as.numeric(OR), digits = 2), nsmall = 2),        paste0(rep(unbrk, 6), collapse = "")),
          OR <  1 & p <  0.001 ~ stringr::str_c("1 / ", format(round(1/as.numeric(OR), digits = 2), nsmall = 2), "***"),
          OR <  1 & p <  0.005 ~ stringr::str_c("1 / ", format(round(1/as.numeric(OR), digits = 2), nsmall = 2), "**" , paste0(rep(unbrk, 2), collapse = "")),
          OR <  1 & p <  0.01  ~ stringr::str_c("1 / ", format(round(1/as.numeric(OR), digits = 2), nsmall = 2), "*"  , paste0(rep(unbrk, 4), collapse = "")),
          OR <  1 & p >= 0.01  ~ stringr::str_c("1 / ", format(round(1/as.numeric(OR), digits = 2), nsmall = 2),        paste0(rep(unbrk, 6), collapse = ""))
        )) %>%
        dplyr::mutate(color = as.factor(dplyr::case_when(
          `OR (multivariable)` == "Reference" ~ "Reference",
          TRUE ~ "Autre"))) %>%
        dplyr::mutate(index = .data$index + 1) %>%
        tibble::add_row(fit_id = stringr::str_c("Title", 1:2), label = "",
                        Total = 0, index = 0:1,
                        .before = 1) #Two empty lines

      #Two lines of the original function :
      df.out$levels = as.character(df.out$levels)
      df.out$fit_id = factor(df.out$fit_id, levels = df.out$fit_id[order(-df.out$index)])

      first_row <- df.out[1,] %>%
        tibble::add_row(fit_id = "Title1", label = "Variable", Total = 0, index = 0,
                        levels = "Levels",
                        `OR (multivariable)` = "Odds ratio", # stringi::stri_unescape_unicode("Odds ratio (IC \\u00e0 95%, \\u00e9chelle logarithmique)")
                        .before = 1) %>%
        tibble::add_row(fit_id = "Title2", label = "", Total = 0, index = 1,
                        levels = stringr::str_c("(% ", colnames(df.out)[which( #stringr::str_to_lower(
                          colnames(df.out) == "Total") - 1], ")"),
                        .before = 2) %>%
        dplyr::slice(1:2)

      g1 = ggplot2::ggplot(df.out, ggplot2::aes(x = as.numeric(.data$OR),
                                                xmin = as.numeric(.data$L95),
                                                xmax = as.numeric(.data$U95),
                                                y = .data$fit_id)) +
        ggplot2::geom_point(ggplot2::aes(size = .data$Total, fill = .data$color),
                            shape = 22, na.rm = TRUE) + #"darkblue"
        ggplot2::geom_vline(xintercept = 1, linetype = "longdash",
                            colour = "black") +
        ggplot2::geom_point(data = dplyr::slice(dplyr::select(df.out, 1), 1),
                            ggplot2::aes(x = 1, y = .data$fit_id),
                            shape = 15, color = "white", size = 16,
                            inherit.aes = FALSE, na.rm = TRUE) +
        ggplot2::geom_point(ggplot2::aes(size = .data$Total, fill = .data$color),
                            shape = 22, na.rm = TRUE) + #"darkblue"
        ggplot2::geom_errorbarh(height = 0.2, na.rm = TRUE) +
        #geom_point(ggplot2::aes(size = Total/2), color = "#222222", shape = 4) +
        ggplot2::annotate("text", x = 0, y = first_row$fit_id[1],
                          label = " (95% IC, log scale)", #" / rapport de chances",
                          hjust = 0,
                          size = table_text_size, fontface = "bold", na.rm = TRUE) +
        # ggplot2::annotate("text", x = 0, y = first_row$fit_id[2],
        #                   label = stringi::stri_unescape_unicode(" (IC \\u00e0 95%, \\u00e9chelle logarithmique)"),
        #                   hjust = 0,
        #                   size = table_text_size, fontface = "bold") +
        ggplot2::scale_x_continuous(trans = "log10", breaks = legend_ticks_breaks,
                                    labels = legend_ticks_labels) +
        ggplot2::scale_fill_manual(values = c(Autre = "#333333", Reference = "#999999")) +
        #xlab("Odds ratio (95% CI, log scale)") +
        ggplot2::theme_classic(14) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(), #element_text(),
                       axis.title.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                       axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       legend.position = "none", plot.margin = ggplot2::unit(c(0.25,0.25,0.25,-0.275), "cm"))
      t1 = ggplot2::ggplot(df.out, ggplot2::aes(x = as.numeric(.data$OR),
                                                y = .data$fit_id)) +
        ggplot2::annotate("text", x = column_space[1], y = df.out$fit_id,
                          label = df.out[, 2], hjust = 0, size = table_text_size, na.rm = TRUE) +
        ggplot2::annotate("text", x = column_space[2], y = df.out$fit_id,
                          label = df.out[, 3], hjust = 1, size = table_text_size, na.rm = TRUE) +
        ggplot2::annotate("text", x = column_space[3], y = df.out$fit_id,
                          label = df.out[, 8], hjust = 1, size = table_text_size, na.rm = TRUE) +
        ggplot2::annotate("text", x = column_space[1], y = first_row$fit_id,
                          label = first_row[, 2], hjust = 0, size = table_text_size,
                          fontface = "bold", na.rm = TRUE) +
        ggplot2::annotate("text", x = column_space[2], y = first_row$fit_id,
                          label = first_row[, 3], hjust = 1, size = table_text_size,
                          fontface = "bold", na.rm = TRUE) +
        ggplot2::annotate("text", x = column_space[3], y = first_row$fit_id,
                          label = first_row[, 8], hjust = 1, size = table_text_size,
                          fontface = "bold.italic", na.rm = TRUE) +
        ggplot2::theme_classic(14) +
        ggplot2::theme(
          #text = ggplot2::element_text(family = "sans"), #if ("arial" %in% names(grDevices::windowsFonts())) { "arial" } else { "sans" }),
          axis.title.x = ggplot2::element_blank(), #element_text(colour = "white"),
          axis.text.x = ggplot2::element_text(colour = "white"), axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
          line = ggplot2::element_blank(), plot.margin = ggplot2::unit(c(0.25,-0.275, 0.25,0.25), "cm"))

      g1 = g1 + plot_opts
      t1 = t1 + table_opts
      # title = plot_title(.data, dependent, dependent_label = dependent_label,
      #                    prefix = prefix, suffix = suffix)


      #plot.out <-
      gridExtra::grid.arrange(t1, g1, ncol = 2, widths = c(3, 2)#,
                              # top = grid::textGrob(title, x = 0.02, y = 0.2, gp = grid::gpar(fontsize = title_text_size),
                              #                      just = "left")
      )

      # grDevices::windowsFonts(sans = windowsFont(sansF[[1]]))
      #
      # plot.out

    } else {
      df.out
    }
  }

# OBJ_logit_plot <- glm.data %>%
#   pers_or_plot("OBJVRAIacm", explanatory, table_text_size = 4)









# Internal functions ---------------------------------------------------------------------



#' @keywords internal
plot_path <- function(dir = NULL, name = "Plot", extension = "png", replace = FALSE) {
  if (is.null(dir)) {
    dir <- getOption("ggfacto.export_dir")
    if (is.null(dir)) {
      dir <- tempdir()
    }
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  #if (dirname(path) != getwd() & dirname(path) != ".") {
  #   dir_path <- dirname(path)
  #   if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  # }

  path <- file.path(dir, basename(name))

  path_name <- stringr::str_remove(path, "\\..+$")
  if (! stringr::str_detect(path, "\\..+$")) path <- stringr::str_c(path, ".", extension)
  if (replace == FALSE) {
    i <- 0
    file_do_not_exist <- FALSE
    while (file_do_not_exist == FALSE) {
      if (file.exists(path)) {
        i = i+1
        path <- stringr::str_c(path_name, i, ".", extension)
      } else {
        path <-
          stringr::str_c(path_name, dplyr::if_else(i == 0,
                                                   "",
                                                   stringr::str_c(i)),
                         ".", extension)
        file_do_not_exist <- TRUE
      }
    }
  }
  writeLines(path)
  return(path)
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
interactive_tooltips <- function(dat,
                                 sup_vars         = character(),
                                 active_tables    = character(),
                                 active_vars,
                                 #active_vars_data,
                                 tooltip_vars_1lv = character(),
                                 tooltip_vars     = character()#,
                                 #excl             = character(),
                                 #cleannames       = FALSE
) {
  sup_list <- c(tooltip_vars_1lv, active_vars, tooltip_vars)
  vars <- c(active_tables[!active_tables %in% sup_vars], sup_vars)
  #c(sup_vars[!sup_vars %in% active_tables], sup_vars) #vars <- c(active_vars, sup_vars)


  #Tooltip vars with only the first level kept
  if (length(tooltip_vars_1lv) != 0) {
    tooltip_vars_1lv_3levels <-
      purrr::map_lgl(dat, ~ nlevels(.) >= 3) &
      colnames(dat) %in% tooltip_vars_1lv
    if (any(tooltip_vars_1lv_3levels)) dat <- dat %>%
        dplyr::mutate_if(tooltip_vars_1lv_3levels,
                         ~ forcats::fct_other(
                           .,
                           keep = levels(.)[1],
                           other_level = "Other_levels"
                         ))
    dat <- dat %>%
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(tooltip_vars_1lv),
        ~ forcats::fct_recode(., rlang::splice(purrr::set_names(levels(.)[-1], "Remove_levels")))
      ))
  }


  #To remove the second level when active vars just have two
  active_vars_2levels <-
    purrr::map_lgl(dat[active_vars],
                   ~ is.factor(.) & length(levels(.)[levels(.) != "Remove_levels"]) == 2)

  active_vars_2levels <- names(active_vars_2levels[active_vars_2levels])

  active_vars_2levels <- dplyr::select(dat, tidyselect::all_of(active_vars_2levels)) %>%
    purrr::map(~ levels(.)[2]) %>%
    purrr::imap(~ c(.x, paste0(.y, "_", .x))) |>
    purrr::flatten_chr()

  tabs <- rep(list(NULL), length(vars))


  if (any(vars %in% active_tables)) {
    tabs_active_tables <-
      withr::with_options(list(tabxplor.output_kable = FALSE), {
        tabxplor::tab_many(dat,
                           row_vars = tidyselect::all_of(vars[vars %in% active_tables]),
                           col_vars = tidyselect::all_of(sup_list),
                           na       = "drop",
                           wt       = "row.w",
                           pct      = "row",
                           color    = "diff",
                           add_n    = FALSE # ,
        )
      })


    if (is.data.frame(tabs_active_tables)) tabs_active_tables <- list(tabs_active_tables)


    tabs[vars %in% active_tables] <- tabs_active_tables %>%
      purrr::map(
        ~ dplyr::rename_with(., ~ "lvs", 1) %>%
          dplyr::rename_with(~ dplyr::if_else(stringr::str_detect(., "^Total_"), "Total", .)) %>%
          dplyr::select(-tidyselect::starts_with("Remove_levels"),
                        -tidyselect::any_of(active_vars_2levels)) %>%
          dplyr::filter(!.data$lvs == "Remove_levels")
      )
  }

  if (any(!vars %in% active_tables)) {
    tabs_no_active_tables <-
      withr::with_options(list(tabxplor.output_kable = FALSE), {
        tabxplor::tab_many(dat,
                           row_vars = tidyselect::all_of(vars[!vars %in% active_tables]),
                           na       = "drop",
                           wt       = "row.w",
                           pct      = "col",
                           add_n    = FALSE # ,
        )
      })

    if (is.data.frame(tabs_no_active_tables)) tabs_no_active_tables <- list(tabs_no_active_tables)

    tabs[!vars %in% active_tables] <- tabs_no_active_tables %>%
      purrr::map(
        ~ dplyr::rename_with(., ~ "lvs", 1) %>%
          dplyr::rename_with(~ dplyr::if_else(stringr::str_detect(., "^Total_"), "Total", .)) %>%
          #dplyr::select(-tidyselect::any_of("n")) %>%
          dplyr::filter(!.data$lvs == "Remove_levels")
      )
  }

  tabs <- purrr::set_names(tabs, vars)

  # tabs <- purrr::map_if(
  #   vars, vars %in% active_tables,
  #   ~ withr::with_options(list(tabxplor.output_kable = FALSE), {
  #     tabxplor::tab_many(dat, !!rlang::sym(.), sup_list[sup_list != .],
  #                        na = "drop", wt = "row.w", pct = "row", color = "diff") %>%
  #       dplyr::rename_with(~ "lvs", 1) %>%
  #       dplyr::select(-tidyselect::starts_with("Remove_levels"),
  #                     -tidyselect::any_of(active_vars_2levels)) %>%
  #       dplyr::filter(!.data$lvs == "Remove_levels")
  #   }),
  #
  #   .else =
  #     ~ withr::with_options(list(tabxplor.output_kable = FALSE), {
  #       tabxplor::tab(dat, !!rlang::sym(.), na = "drop", wt = "row.w", pct = "col") %>% #tot = c("row", "col")
  #         dplyr::rename_with(~ "lvs", 1) %>%
  #         dplyr::select(-any_of("n")) %>%
  #         dplyr::filter(!.data$lvs == "Remove_levels")
  #     })
  #
  # ) %>%
  #   purrr::set_names(vars)

  # sup_vars_count <-
  #   purrr::map(tabs, ~ dplyr::mutate(dplyr::select(., lvs, Total),
  #                                    Total = vctrs::field(.data$Total, "wn") ))

  #col_vars_levels <- purrr::map(tabs, ~ tabxplor::tab_get_vars(.)$col_vars_levels)

  # tooltip_first_levels <- col_vars_levels %>%
  #   purrr::map(~ .[names(.) %in% tooltip_vars] %>% purrr::map(dplyr::first) %>%
  #                purrr::flatten_chr()
  #   )


  # active_first_variable <- col_vars_levels %>%
  #   purrr::map(~ .[names(.) %in% active_vars] %>%
  #                dplyr::first() %>% dplyr::first()
  #   )

  # color_code_vector <- function(var) {
  #   color_selection <- tabxplor:::fmt_color_selection(var) %>% purrr::map(which)
  #
  #   color_styles <- tabxplor:::select_in_color_style(length(color_selection))
  #   color_styles <- tabxplor:::get_color_style("color_code", type = "text", theme = "light")[color_styles]
  #
  #   color_positions <- color_selection %>%
  #     purrr::map2(color_styles, ~ purrr::set_names(.x, stringr::str_to_upper(.y))) %>%
  #     purrr::flatten_int()
  #
  #   no_color <- 1:length(var)
  #   no_color <- purrr::set_names(no_color[!no_color %in% color_positions], NA_character_)
  #
  #   names(sort(c(color_positions, no_color)))
  # }
  # #replace by tabxplor::fmt_get_color_code()

  format_pct <- function(diff, pct, colname, color_code) {
    pct <- stringr::str_pad(pct, 3, pad = "@")

    pct[!is.na(color_code)] <-
      stringr::str_c("<font color=\"",
                     color_code[!is.na(color_code)],
                     "\">",
                     "<b>",
                     pct[!is.na(color_code)],
                     "</b>",
                     "</font>"
      )

    pct[is.na(color_code)] <- paste0(unbrk, pct[is.na(color_code)])

    #pct <- paste0("\t", pct)

    dplyr::case_when(
      diff == 0 ~ paste0(colname, ": ", pct), #"\n"

      diff >  0 ~ paste0(
        colname, ": ", #"\n",
        "(", stringr::str_pad(paste0(
          "+" , abs(diff)), 3, pad = "@"),"%) ",
        pct),

      diff <  0 ~ paste0(
        colname, ": ", #"\n",
        "(", stringr::str_pad(paste0(
          "-" , abs(diff)), 4, pad = "@"),"%) ",
        pct)
    ) %>%
      stringr::str_replace_all(
        "@",
        paste0(unbrk, unbrk, collapse = "")
      ) #%>%
     # stringi::stri_unescape_unicode()
  }


  tooltip_vars_1lv_levels <- purrr::map_chr(tooltip_vars_1lv, ~ levels(dat[[.]])[1])

  first_active <- levels(dat[[active_vars[[1]]]])
  first_active <- purrr::map(first_active, ~ c(., paste0(., "_", active_vars[[1]]))) %>%
    purrr::flatten_chr()

  last_var_with_active_tables <- tidyr::replace_na(
    1:length(vars) != dplyr::last(which(vars %in% active_tables)),
    FALSE
  )

  if (length(tooltip_vars) != 0) {
    tooltip_first_levels <-
      purrr::imap_dfr(dplyr::select(dat, tidyselect::all_of(tooltip_vars)),
                      ~ tibble::tibble(vars = .y, lvs = c(levels(.x)[1], paste0(levels(.x)[1], "_", .y) )))
    tooltip_first_levels <- purrr::set_names(tooltip_first_levels$vars, tooltip_first_levels$lvs)

  } else {
    tooltip_first_levels <- character()
  }


  interactive_text <-
    purrr::imap(tabs,
                ~ dplyr::mutate(.x, vars = factor(.y)) %>%
                  dplyr::relocate(.data$vars, .before = 1)
    ) %>%
    purrr::map_if(last_var_with_active_tables, ~ dplyr::filter(., lvs != "Total")) %>%
    dplyr::bind_rows()



  # interactive_text %>%
  #   dplyr::mutate(dplyr::across(
  #     where(tabxplor::is_fmt),
  #     ~ format_pct(diff       = round(vctrs::field(., "diff") * 100, 0),
  #                  pct        = format(.),
  #                  colname    = dplyr::cur_column(),
  #                  color_code = color_code_vector(.))
  #   )) %>%
  #   dplyr::select(1:4)
  #


  interactive_text <- interactive_text %>%
    dplyr::mutate(vars = dplyr::if_else(stringr::str_detect(.data$lvs, "^Total"),
                                        true  = factor("All", c(levels(.data$vars), "All")),
                                        false = .data$vars),

                  lvs  = dplyr::if_else(stringr::str_detect(.data$lvs, "^Total"),
                                        true  = factor("Central point", c(levels(.data$lvs), "Central point")),
                                        false = .data$lvs)
    )

  if ("n" %in% names(interactive_text)) {
    interactive_text <- interactive_text %>%
      dplyr::mutate(wn = vctrs::field(.data$wn, "wn"))  |>
      dplyr::rename("wcount" = "wn")

  } else  {
    interactive_text <- interactive_text %>%
      dplyr::mutate(n     = vctrs::field(.data$Total, "n"),
                    Total = vctrs::field(.data$Total, "wn")) |>
      dplyr::rename("wcount" = "Total")
  }

  interactive_text <- interactive_text %>%
    dplyr::mutate(actives_text = dplyr::if_else(vars %in% active_tables,
                                                true  = "\n<b>Active variables:</b>",
                                                false = NA_character_)) %>%
    dplyr::mutate(begin_text = paste0(
      "<b>", .data$lvs,"</b>",
      dplyr::if_else(.data$lvs != "Central point", true = paste0("\n", .data$vars), false = ""),
      "\nFrequency (n=", .data$n, "): ",
      paste0(format(round(.data$wcount / dplyr::last(.data$wcount) * 100, 0)), "%")
    ) ) %>%
    dplyr::select(-.data$n) %>%
    dplyr::select(.data$vars, .data$lvs, .data$wcount, .data$begin_text,
                  tidyselect::any_of(tooltip_vars_1lv_levels),
                  tidyselect::any_of("actives_text"),
                  tidyselect::any_of(first_active),
                  tidyselect::everything()) %>%
    dplyr::mutate(dplyr::across(
      where(tabxplor::is_fmt),
      ~ format_pct(diff       = round(vctrs::field(., "diff") * 100, 0),
                   pct        = format(.),
                   colname    = dplyr::cur_column(),
                   color_code = tabxplor::fmt_get_color_code(.))
    )) %>%
    dplyr::mutate(dplyr::across(
      tidyselect::any_of(names(tooltip_first_levels)),
      ~ dplyr::if_else(.data$vars %in% active_tables,
                       true  = paste0("\n<b>Distribution by ",
                                      tooltip_first_levels[dplyr::cur_column()],
                                      ":</b>" , "\n", .),
                       false = NA_character_
      )
    ))

  interactive_text
}


# data <- ind_data

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




# coords  <- axes_principaux_df
# res.pca <- res.pca

#' @keywords internal
PCA_princ_coord_in_base <- function(coords, res.pca) {

  # # # Par rapport a Brigitte Le Roux, Analyse geometrique..., (cf. Chap. 6 Exercice 6.1)
  # #                # X0: coordonnees de base centrees.
  # # scale.unit = T # X0r: X0 * 1/sqrt(vjj) : coordonnees c. reduites (matrice diag des variances)
  # # res.pca$eig    # Lambda "\\u039b" : matrice diagonale des valeurs propres
  # # res.pca$svd$vs # Ksi "\\u039e" : matrice diagonale des valeurs singulieres (mais, ici, vecteur) ;
  # #                      (les elements sont notes Ksi-j  "\\u03be" )
  # # res.pca$svd$U
  # # res.pca$svd$V  # A: matrice des vecteurs propres normes.
  # # ind$coord      # Y: coordonnees principales. Y = X0A    Inv: X0=Yt(A) (ou X0r)
  # # var$coords     # B: coeffs de regression B = A Ksi (b1j = sqrt(Lamda1) * a1j = Ksi1 * a1j)


  coords_mat <- coords |> dplyr::select(where(is.double)) |> as.matrix()    # Y

  # dplyr::rowwise()|>
  # dplyr::mutate(princ_coord = paste0(dplyr::c_across(where(is.numeric)), collapse = ";") ) |>
  # dplyr::ungroup() |>
  # dplyr::mutate(name = paste0(name, "|", princ_coord)) |>
  # dplyr::select(-princ_coord) |>
  # tibble::column_to_rownames("name") |>
  # as.matrix()

  df_base <- res.pca$call$X # coordonnees de base + nom des variables actives
  mean_mat <- purrr::map_dbl(df_base, mean) |> as.matrix() |> t()
  # sd_mat   <- purrr::map_dbl(df_base, sd) |> as.matrix() |> t()   ; sd_mat

  A_named <- res.pca$svd$V |> as.data.frame() # A, named from df_base to keep it after
  rownames(A_named) <- names(df_base)

  tA  <- t(A_named) # t(A)           # t(res.pca$svd$V)
  x0j <- (coords_mat %*% tA) # X0 = Y*t(A)

  princ_coord_in_base <- (x0j + mean_mat[col(x0j)]) |> tibble::as_tibble()


  # princ_coord_in_base <- (x0j + mean_mat[col(x0j)]) |> # ajouter moyenne a chaque colonne
  #   as.data.frame() |> tibble::rownames_to_column("name") |> tibble::as_tibble() |>
  #   # dplyr::rename_with(stringr::str_to_lower) |>
  #   dplyr::mutate(princ_coord = stringr::str_extract(name, "[^|]+$"),
  #          name        = as.factor(stringr::str_extract(name, "^[^|]+")),
  #   ) |>
  #   separate_wider_delim(cols  = princ_coord,
  #                        delim = ";",
  #                        names = colnames(coords)) |>
  #   dplyr::mutate(dplyr::across(tidyselect::all_of(colnames(coords)), as.integer)) |>
  #   dplyr::select(name, tidyselect::everything())

  return(dplyr::bind_cols(coords, princ_coord_in_base))
}


# X.ind.sup <- base_axis_coords |> dplyr::select(-base_coord)

#' @keywords internal
PCA_ind.sup_coord <- function(X.ind.sup, res.pca, center = TRUE) { #no_sd = FALSE
  df_base <- res.pca$call$X |>
    tibble::rownames_to_column("name") |> tibble::as_tibble()

  if (!is.null(res.pca$call$quali.sup) ) {
    df_base <- df_base |>
      dplyr::select(-tidyselect::all_of(names(res.pca$call$quali.sup$quali.sup)))
  }
  if (!is.null(res.pca$call$quanti.sup) ) {
    df_base <- df_base |>
      dplyr::select(-tidyselect::all_of(names(res.pca$call$quanti.sup)))
  }

  active_vars <- colnames(df_base)[!colnames(df_base) == "name"]

  if ("name" %in% names(X.ind.sup)) {
    X.ind.sup  <- X.ind.sup |> #as.data.frame() |> tibble::rownames_to_column("name") |>
      dplyr::select("name", tidyselect::all_of(active_vars)) |>
      tibble::column_to_rownames("name") |>
      as.matrix()
  } else {
    X.ind.sup <- as.matrix(X.ind.sup)
    # X.ind.sup |> tibble::rownames_to_column("name") |> as_
  }

  if (center) {
    centre <- res.pca$call$centre
  } else {
    centre <- rep(0, length(res.pca$call$centre))
  }

  ecart.type <- res.pca$call$ecart.type
  # if (no_sd) ecart.type <- rep(1, length(centre))

  X.ind.sup <- t(t(X.ind.sup) - centre)
  X.ind.sup <- t(t(X.ind.sup)/ecart.type)
  coord.ind.sup <- t(t(X.ind.sup) * res.pca$call$col.w)
  coord.ind.sup <- crossprod(t(coord.ind.sup), res.pca$svd$V)
  # coord.ind.sup <- coord.ind.sup[, 1:ncp, drop = F]
  colnames(coord.ind.sup) <- paste("Dim", c(1:ncol(coord.ind.sup)), sep = ".")
  coord.ind.sup

}



