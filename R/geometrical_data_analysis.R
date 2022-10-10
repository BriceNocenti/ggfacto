

# Geometrical data analysis AC/ACM -------------------------------------------------------



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
                                    ~ forcats::fct_explicit_na(., "NA")
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
#' Since it is made in the spirit of \code{\link{ggplot2}}, it is possible to
#' change theme or add another plot elements with \code{+}. Then, interactive
#' tooltips won't appear until you pass the result through \code{\link{ggi}}.
#' Step-by-step functions : use \link{ggmca_data} to get the data frames with every
#' parameter in a MCA printing, then modify, and pass to \link{ggmca_plot}
#' to draw the graph.
#' @param res.mca An object created with \code{FactoMineR::\link[FactoMineR]{MCA}}.
#' @param dat The data in which to find the supplementary variables, etc.
#' @param sup_vars A character vectors of supplementary qualitative variables
#' to print. They must have been passed in \code{\link[FactoMineR]{MCA}} before.
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
#' @param cah A variable made with \code{\link[FactoMineR]{HCPC}}, to link
#' the answers-profiles points who share the same ICPC class (will be colored
#' together at mouse hover).
#' @param max_profiles The maximum number of profiles points to print. Default to 5000.
#' @param color_groups By default, their is one color group by `sup_vars`. It is otherwise
#'  possible to color `sup_vars` with groups created upon their levels with
#'  \code{\link[stringr]{str_extract}} and regexes. For exemple, `color_groups = "^."`
#'   makes the groups upon the first character of each levels (uselful when their begin
#'   by numbers). `color_groups = "^.{3}"` upon the first three characters.
#'  `color_groups = "NB.+$"`takes anything between the `"NB"` and the end of levels
#'  names, etc.
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
#' @param actives_in_bold Should active variables be in bold font, or sup variables ?
#' @param ellipses Set to a number between 0 and 1 to draw a concentration ellipse for
#' each level of the first \code{sup_vars}. \code{0.95} draw ellipses containing 95% of the
#' individuals of each category. \code{0.5} draw median-ellipses, containing half
#' the individuals of each category. Note that, if `max_profiles` is provided, ellipses
#' won't be made with all individuals.
#' @param color_profiles If \code{cah} is provided, you can give a character vector of
#' levels name of this `cah` variable to color profiles of answers on that basis.
#' You can also set `color_profiles = TRUE`, but the result is usually difficult to read :
#' better to draw the profiles of the CAH classes one by one.
#' @param base_profiles_color The base color for answers profiles. Default to gray.
#' Set to `NULL` to discard profiles. With `color_profiles`, set to `NULL` to discard the
#' non-colored profiles.
#' @param scale_color_light A scale color for sup vars points
#' @param scale_color_dark A scale color for sup vars texts
#' @param use_theme By default, a specific \pkg{ggplot2} theme is used.
#' Set to \code{FALSE} to customize your own \code{\link[ggplot2:theme]{theme}}.
#' @param get_data Returns the data frame to create the plot instead of the plot itself.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object to be printed in the
#' `RStudio` Plots pane. Possibility to add other gg objects with \code{+}.
#' Sending the result through \code{\link{ggi}} will draw the
#' interactive graph in the Viewer pane using \code{\link{ggiraph}}.
#' @export
#'
#' @examples
#' \donttest{
#' data(tea, package = "FactoMineR")
#' res.mca <- MCA2(tea, active_vars = 1:18)
#'
#' # Interactive graph for multiple correspondence analysis :
#' res.mca %>%
#'   ggmca(tea, sup_vars = c("SPC"), ylim = c(NA, 1.2), text_repel = TRUE) %>%
#'   ggi() #to make the graph interactive
#'
#' # Interactive graph with access to all crosstables between active variables (burt table).
#' #  Spread from mean are colored and, usually, points near the middle will have less
#' #  colors, and points at the edges will have plenty. It may takes time to print, but
#' #  helps to interpret the MCA in close proximity with the underlying data.
#' res.mca %>%
#'   ggmca(tea, ylim = c(NA, 1.2), active_tables = "active", text_repel = TRUE) %>%
#'   ggi()
#'
#' #Concentration ellipses for each levels of a supplementary variable :
#' ggmca(res.mca, tea, sup_vars = "SPC", ylim = c(NA, 1.2),
#'   ellipses = 0.5, text_repel = TRUE, profiles = TRUE)
#'
#' #Graph of profiles of answer for each levels of a supplementary variable :
#' ggmca(res.mca, tea, sup_vars = "SPC", ylim = c(NA, 1.2),
#'   type = "facets", ellipses = 0.5, profiles = TRUE)
#'
#' }
ggmca <-
  function(res.mca, dat, sup_vars, active_tables, tooltip_vars_1lv, tooltip_vars,
           axes = c(1,2), axes_names = NULL, axes_reverse = NULL,
           type = c("text", "labels", "points", "numbers", "facets"),

           color_groups = "^.{0}", keep_levels, discard_levels, cleannames = TRUE,

           profiles = FALSE, profiles_tooltip_discard = "^Not |^No |^Pas |^Non ",
           cah, max_profiles = 5000,


           text_repel = FALSE, title, actives_in_bold = NULL, ellipses = NULL,
           xlim, ylim, out_lims_move = FALSE,
           color_profiles, base_profiles_color = "#dddddd",
           shift_colors = 0, colornames_recode,
           scale_color_light = material_colors_light(),
           scale_color_dark  = material_colors_dark(),
           text_size = 3, size_scale_max = 4, dist_labels = c("auto", 0.04),
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
      color_groups = color_groups
    )

    ggmca_plot(data = data,
               axes = axes, axes_names = axes_names, axes_reverse = axes_reverse,
               type = type,
               text_repel = text_repel, title = title, actives_in_bold = actives_in_bold,
               ellipses = ellipses,
               xlim = xlim, ylim = ylim, out_lims_move = out_lims_move,
               color_profiles = color_profiles, base_profiles_color = base_profiles_color,
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

           color_groups = "^.{0}", keep_levels, discard_levels, cleannames = TRUE,

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






    #Active variables --------------------------------------------------------------------
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








    #Supplementary variables -------------------------------------------------------------
    if (length(sup_vars) != 0) {

      sup_vars_data <- purrr::map(sup_vars, ~ varsup(res.mca, dat[[.]]) ) %>%
        purrr::set_names(sup_vars)

      # Do something with "within" et "between" variance ? ($var) ----

      sup_vars_data <-
        purrr::imap(sup_vars_data,
                    ~ tibble::as_tibble(.x$coord, rownames = "lvs") %>%
                      dplyr::mutate(vars = .y) %>%
                      dplyr::select(.data$vars, tidyselect::everything())
        )

      # color_group depending on nb of supplementary variables and nb of characters
      #  indicated in color_groups
      color_groups <- vctrs::vec_recycle(color_groups, length(sup_vars))
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


      #Make that, if ICPC is in sup_vars AND in profiles, ggiraph data_id are the same :
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
        ~ dplyr::if_else(lvs == "Central point", 0, .)
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
      purrr::imap_dfr(dat[active_vars], ~ tibble::tibble(active_vars = .y, lvs = levels(.x))) |>
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






    ### Interactive tooltips (sup/active) ----
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


    #If no entire table have been calculated, we don't have the data for mean point
    #   => we use the data available in res.mca
    if (length(active_tables) == 0 ) {
      mean_point_interactive_text <- vars_data %>%
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






    ### Profiles of answers ----
    #; weighted : nb of individuals * weight variable
    if (profiles) {
      ind_data <- dplyr::bind_cols(
        dplyr::select(dat, -tidyselect::any_of(c(tooltip_vars_1lv[!tooltip_vars_1lv %in% sup_vars],
                                                 tooltip_vars[!tooltip_vars %in% sup_vars]))),
        tibble::as_tibble(res.mca$ind$coord)
      )

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

      if (length(cah) != 0) {
        #cah_levels <- dplyr::pull(ind_data, !!rlang::sym(cah) ) %>% levels()

        ind_data <- ind_data %>%
          tidyr::nest(sup_vars = tidyselect::all_of(sup_vars),
                      row.w    = .data$row.w,
                      coord    = tidyselect::all_of(coord_names),
                      cah      = !!rlang::sym(cah)
          ) %>%
          dplyr::mutate(
            count  = purrr::map_int(.data$row.w, ~ nrow(.)),

            wcount = purrr::map_dbl(.data$row.w, ~ sum(., na.rm = TRUE)),

            cah    = purrr::map_if(
              .data$cah,
              purrr::map_lgl(.data$cah,
                             ~ length(unique(dplyr::pull(., !!rlang::sym(cah)))) == 1),
              ~ unique(dplyr::pull(., !!rlang::sym(cah))),
              .else = ~ factor(NA_character_, levels(dplyr::pull(., !!rlang::sym(cah))))
            ) %>% unlist()

          ) %>%
          dplyr::arrange(-.data$wcount)

        if (length(max_profiles) != 0) ind_data <- ind_data %>% dplyr::slice(1:max_profiles)

        ind_data <- ind_data %>%
          dplyr::mutate(nb = dplyr::row_number(), cah_id = as.integer(.data$cah)) %>%
          dplyr::group_by(.data$cah) %>%
          dplyr::mutate(nb_in_cah = dplyr::row_number(), nb_tot_cah = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(coord = purrr::map(.data$coord, ~ .[1,])) %>%
          tidyr::unnest(.data$coord)


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
                       text_repel = FALSE, title, actives_in_bold = NULL, ellipses = NULL,
                       xlim, ylim, out_lims_move = FALSE,
                       color_profiles, base_profiles_color = "#dddddd",
                       shift_colors = 0, colornames_recode,
                       scale_color_light = material_colors_light(),
                       scale_color_dark  = material_colors_dark(),
                       text_size = 3, size_scale_max = 4, dist_labels = c("auto", 0.04),
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

  if (missing(color_profiles))    color_profiles    <- character()
  if (is.logical(color_profiles)) if (! color_profiles) {
    color_profiles <- character()
  } else {
    color_profiles <- levels(as.factor(dplyr::pull(ind_data, cah)))
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
          dplyr::pull(text),

        .else = ~ ""
      ) %>%
        purrr::flatten_chr(),

      interactive_text = purrr::map2(
        .data$interactive_text, .data$contribs,
        ~ dplyr::mutate(.x, begin_text = stringr::str_c(.data$begin_text, .y))
      )
    ) %>%
    dplyr::select(-.data$contribs)


  # Collapse the interactive tooltips dataframes
  vars_data <- vars_data %>%
    dplyr::mutate(interactive_text = purrr::map_chr(
      .data$interactive_text,
      ~ tibble::deframe(tidyr::unite(., "interactive_text", sep = "\n", na.rm = TRUE))
    ))

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
    ind_data <- ind_data %>%
      dplyr::mutate(interactive_text = purrr::map_chr(
        .data$interactive_text,
        ~ tibble::deframe(tidyr::unite(., "interactive_text", sep = "\n", na.rm = TRUE))
      ) %>%
        stringr::str_remove_all("\n#")
      )

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
            show.legend = FALSE
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
            dplyr::select(.data$lvs, .data$color_group) %>%
            dplyr::filter(stringr::str_detect(.data$color_group, paste0("^", cah))) %>%
            dplyr::mutate(color_group = .data$lvs %>% purrr::set_names(.data$color_group)) %>%
            dplyr::pull(.data$color_group)

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


        #Discard the points that are out of limits
        profiles_coord <- ind_data
        if (!missing(xlim)) profiles_coord <- profiles_coord %>% outlims(xlim, !!dim1)
        if (!missing(ylim)) profiles_coord <- profiles_coord %>% outlims(ylim, !!dim2)

        profiles <- ggiraph::geom_point_interactive(
          data = profiles_coord,
          ggplot2::aes(x = !!dim1, y = !!dim2, size = .data$wcount,
                       tooltip = .data$interactive_text,
                       data_id = .data$cah_id + 10000, color = .data$color_group),
          na.rm = TRUE, inherit.aes = FALSE, show.legend = FALSE, stroke = 0 #, alpha = 0.5
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
            show.legend = FALSE
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
      graph_theme_acm + profiles  + ellipses + graph_text +
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




#' Benzecri's modified rate of variance
#'
#' @param res.mca The result of \link[FactoMineR]{MCA}.
#' @param fmt By default, the result is given as a numeric vector. Set to `TRUE` to have
#' a \pkg{tabxplor} \code{link[tabxplor]{fmt}} vector instead.
#'
#' @return A numeric vector (or fmt vector with `fmt = TRUE`).
#' @export
#'
# @examples
benzecri_mrv <- function(res.mca, fmt = FALSE) {
  Q   <- length(res.mca$call$quali)
  eig <- purrr::keep(res.mca$eig[, 1], res.mca$eig[, 1] > 1/Q)
  eig <- (Q/(Q-1))^2 * (eig - 1/Q)^2
  eig <- eig/sum(eig)

  if (fmt) {
    fmt(pct = eig, n = 0, type = "all")
  } else {
    eig * 100
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
  if (type[1] == "html") requireNamespace("kableExtra")

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
      kableExtra::kable() %>%
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
#' made in the spirit of \code{\link{ggplot2}}, it is possible to change
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
#' @param use_theme By default, a specific \pkg{ggplot2} theme is used.
#' Set to \code{FALSE} to customize your own \code{\link[ggplot2:theme]{theme}}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object to be printed in the
#' `RStudio` Plots pane. Possibility to add other gg objects with \code{+}.
#' Sending the result  through \code{\link{ggi}} will draw the
#' interactive graph in the Viewer pane using \code{\link{ggiraph}}.
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
          . < 0 ~ stringr::str_c(stringi::stri_unescape_unicode("\\u202f"), #Unbreakable space
                                 "-", round(abs(.)*100, 0), "%")
        )) %>% dplyr::slice(-nrow(row_frequencies))
      row_frequencies <- row_frequencies %>%
        dplyr::slice(-nrow(row_frequencies)) %>%
        dplyr::mutate_all(~ stringr::str_c(round(.*100, 0), "%")) %>%
        dplyr::mutate_all(~dplyr::case_when(
          stringr::str_length(.) >= 3 ~ .,
          stringr::str_length(.) < 3 ~ stringr::str_c(
            stringi::stri_unescape_unicode("\\u202f\\u202f"), . #2 unbreakable spaces
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
          . < 0 ~ stringr::str_c(stringi::stri_unescape_unicode("\\u202f"), #unbreakable space
                                 "-", round(abs(.)*100, 0), "%")
        )) %>% dplyr::slice(-nrow(col_frequencies))
      col_frequencies <- col_frequencies %>%
        dplyr::slice(-nrow(col_frequencies)) %>%
        dplyr::mutate_all(~ stringr::str_c(round(.*100, 0), "%")) %>%
        dplyr::mutate_all(~dplyr::case_when(
          stringr::str_length(.) >= 3 ~ .,
          stringr::str_length(.) < 3 ~ stringr::str_c(
            stringi::stri_unescape_unicode("\\u202f\\u202f"), .), #Two unbreakable spaces
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


    # Le Central point et son texte interactif :
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
                       tooltip = .data$interactive_text, fill = .data$colorvar,
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
    css_tooltip <- "text-align:right;padding:4px;border-radius:5px;background-color:#eeeeee;color:white;" #
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




#' Pass a MCA plot into a html interactive plot
#' @param plot The plot, created with \link{ggmca} or \link{ggca}.
#' @param width The width in centimeters. Default to printing device's size.
#' @param height The height in centimeters. Default to printing device's size.
#' @param keep_ratio By default, the height is forced based of the relative
#' size of the MCA's axes. Set to \code{FALSE} to avoid this behavior.
#' @param savewidget Should the html widget be saved on disk ?
#' @param dir If saved as file, the directory in which to save the html widget.
#' Default to temporary directory. Set global option \code{"ggfacto.export_dir"}
#' with \code{link[base:options]{options}} to change default directory.
#' @param open Should the resulting file be opened at once ?
#' @param name The name of the file to save.
#' @param replace Replace file ? By default, number added to find a new name.
#' @param iframe Create an html frame around the plot to ensure fixed
#' dimensions. Useful when opening the plot in a web browser (but will produce a blank
#' graph with \pkg{rmarkdown}). This is default behavior with \code{savewidget = TRUE}.
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
    cat("no_css_in_object")
    css_tooltip <- "color:#000000;text-align:right;padding:4px;border-radius:5px;background-color:#eeeeee;"
  }

  # if(.Platform$OS.type == "windows") {
  #   css_tooltip <-
  #     paste0(css_tooltip, "font-family:", grDevices::windowsFonts("sans"), ";") #%>%
  #   #stringr::str_replace("DejaVu Sans Condensed", "DejaVu Sans")
  # }

  if (missing(width)) {
    width <- grDevices::dev.size("in")[1]

  } else {
    width <- width/2.54
  }

  if (keep_ratio == TRUE & !is.null(plot$heigth_width_ratio)) {
    height <- width * plot$heigth_width_ratio

  } else {
    if (missing(height)) {
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
    ggiraph::girafe_options(ggiraph::opts_tooltip(css = css_tooltip, use_fill = TRUE), #, #use_stroke = FALSE, # = border color of the tooltip #color:white; border-color:black; opacity:1 ; background-color:transparent
                            ggiraph::opts_hover(css = css_hover)
                            # ggiraph::opts_zoom(max = 5) # bugue pas mal
                            # ggiraph::opts_hover(css = girafe_css(css = "fill:purple;stroke:black;", text = "stroke:none;fill:red;font-style:bold;")) #    point = NULL, line, area, image
                            # ggiraph::opts_hover_inv(css = "opacity:0.1;"),
                            # ggiraph::opts_sizing(rescale = FALSE)
                            # ggiraph::opts_sizing(rescale = TRUE, width = 0.7), #between 0 and 1
                            # ggiraph::opts_toolbar(saveaspng = FALSE)
    )


  if (iframe == TRUE) {
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
      htmlwidgets::saveWidget(widget, path, title = name)
    } else {
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
    requireNamespace("finalfit")
    requireNamespace("ggplot2")

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

      ubs <- stringi::stri_unescape_unicode("\\u202f")

      df.out <- df.out %>%
        dplyr::mutate(freq = (
          (as.numeric(stringr::str_remove(df.out[, 5], " \\(.*\\)"))/.data$Total*100) %>%
            round(0) %>% stringr::str_c("%") %>% stringr::str_pad(4)
        )) %>%
        dplyr::mutate(levels = stringr::str_c(.data$levels, " (", .data$freq, ")")) %>%
        dplyr::mutate(`OR (multivariable)` = dplyr::case_when(
          `OR (multivariable)` == "-" ~ "Reference",                               #There were unbreakable spaces.
          OR >= 1 & p <  0.001 ~ stringr::str_c(        format(round(  as.numeric(OR), digits = 2), nsmall = 2), "***"),
          OR >= 1 & p <  0.005 ~ stringr::str_c(        format(round(  as.numeric(OR), digits = 2), nsmall = 2), "**" , paste0(rep(ubs, 2), collapse = "")),            #Unbreakable space
          OR >= 1 & p <  0.01  ~ stringr::str_c(        format(round(  as.numeric(OR), digits = 2), nsmall = 2), "*"  , paste0(rep(ubs, 4), collapse = "")),
          OR >= 1 & p >= 0.01  ~ stringr::str_c(        format(round(  as.numeric(OR), digits = 2), nsmall = 2),        paste0(rep(ubs, 6), collapse = "")),
          OR <  1 & p <  0.001 ~ stringr::str_c("1 / ", format(round(1/as.numeric(OR), digits = 2), nsmall = 2), "***"),
          OR <  1 & p <  0.005 ~ stringr::str_c("1 / ", format(round(1/as.numeric(OR), digits = 2), nsmall = 2), "**" , paste0(rep(ubs, 2), collapse = "")),
          OR <  1 & p <  0.01  ~ stringr::str_c("1 / ", format(round(1/as.numeric(OR), digits = 2), nsmall = 2), "*"  , paste0(rep(ubs, 4), collapse = "")),
          OR <  1 & p >= 0.01  ~ stringr::str_c("1 / ", format(round(1/as.numeric(OR), digits = 2), nsmall = 2),        paste0(rep(ubs, 6), collapse = ""))
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
                        levels = stringi::stri_unescape_unicode("Modalit\\u00e9"),
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
                          label = stringi::stri_unescape_unicode(" (IC \\u00e0 95%, \\u00e9chelle logarithmique)"), #" / rapport de chances",
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
                         color    = "diff"
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
                           pct      = "col"
        )
      })

    if (is.data.frame(tabs_no_active_tables)) tabs_no_active_tables <- list(tabs_no_active_tables)

    tabs[!vars %in% active_tables] <- tabs_no_active_tables %>%
      purrr::map(
        ~ dplyr::rename_with(., ~ "lvs", 1) %>%
          dplyr::rename_with(~ dplyr::if_else(stringr::str_detect(., "^Total_"), "Total", .)) %>%
          dplyr::select(-tidyselect::any_of("n")) %>%
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

    pct[is.na(color_code)] <- paste0("\\u202f", pct[is.na(color_code)])

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
        "\\u202f\\u202f"
      ) %>%
      stringi::stri_unescape_unicode()
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
    ) %>%
    dplyr::mutate(n     = vctrs::field(.data$Total, "n"),
                  Total = vctrs::field(.data$Total, "wn")) %>%
    dplyr::rename("wcount" = "Total") %>%
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


