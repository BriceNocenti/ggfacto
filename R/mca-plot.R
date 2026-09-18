# PURPOSE: ggmca_plot() -- the rendering half of the MCA graph.
# ROLE: Consumes the plot model built by R/mca-data.R and emits a ggplot2/ggiraph object. It never
#   sees the FactoMineR result, only the stripped list(eig, axes_names) it is handed.
# KEY CONSTRAINTS:
#   - The returned object carries render hints (css_hover, height_width_ratio) as ATTRIBUTES,
#     read back by ggi()/ggsave2(). They must not become list slots: append() flattens the S7
#     ggplot into a plain list, and ggplot_build()/grid.draw()/ggsave2() stop dispatching on it.
#   - Its first argument is `plot_data`, the model from ggmca_data(); `data` is reserved for the
#     microdata across the ggmca_* family and survives only as a soft-deprecated alias.
#   - theme_facto() is always called with no_color_scale = TRUE, so the manual palette built here
#     wins over the theme's fallback.
#   - This file must keep sorting AFTER mca-data.R in the C locale: `@describeIn ggmca` names
#     the merged help topic after whichever block roxygen reads first, and that is file order.
# See: CLAUDE.md section ggfacto architecture > The plot-object seam.

#' @describeIn ggmca print MCA graph from data frames with parameters
# @inheritParams ggmca
#' @param plot_data A list of data frames made with \link{ggmca_data}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @export
ggmca_plot <- function(plot_data,
                       axes = c(1,2), axes_names = NULL, axes_reverse = NULL,
                       type = c("text", "points", "labels", "active_vars_only", "facets"),
                       text_repel = TRUE, title, ellipses = NULL,
                       actives_in_bold = NULL, sup_in_italic = FALSE,
                       xlim, ylim, out_lims_move = FALSE,
                       color_profiles = TRUE, base_profiles_color = "#aaaaaa",
                       alpha_profiles = 0.7,
                       shift_colors = 0, colornames_recode,
                       scale_color_light = material_colors_light(),
                       scale_color_dark  = material_colors_dark(),
                       text_size = 3.5, size_scale_max = 4, dist_labels = c("auto", 0.04),
                       right_margin = 0, use_theme = TRUE, get_data = FALSE,
                       data) {
  # `data` was renamed `plot_data` in 0.4.0, freeing `data` for the microdata across the ggmca_*
  # family; it sits last so no positional call can reach it.
  if (!missing(data) && missing(plot_data)) {
    plot_data <- renamed_arg(data, "data", "plot_data", "ggmca_plot")
  }

  vars_data        <- plot_data$vars_data
  ind_data         <- plot_data$ind_data
  #active_vars_data <- plot_data$active_vars_data
  #sup_vars_data    <- plot_data$sup_vars_data
  #mean_point_data  <- plot_data$mean_point_data
  clust              <- plot_data$clust
  sup_vars         <- vars_data |>
    dplyr::filter(!.data$color_group %in% c("active_vars", "Central point")) |>
    dplyr::pull("vars") |> unique()
  res.mca          <- plot_data$res.mca

  if (!is.null(axes_names)) res.mca$axes_names <- axes_names
  if (!is.null(ellipses)) stopifnot(ellipses > 0 & ellipses <= 1)

  dim1 <- rlang::sym(str_c("Dim ", axes[1]))
  dim2 <- rlang::sym(str_c("Dim ", axes[2]))

  contrib1 <- rlang::sym(str_c("contrib", axes[1]))
  contrib2 <- rlang::sym(str_c("contrib", axes[2]))

  # if (length(color_profiles) == 0) {
  #   if (length(clust) != 0) {
  #     color_profiles <- levels(as.factor(dplyr::pull(ind_data, clust)))
  #   } else {
  #     color_profiles <- character()
  #   }
  #
  # } else {
  if (length(clust) > 0 & !is.null(ind_data)) {
    if (is.logical(color_profiles)) if (! color_profiles) {
      color_profiles <- character()
    } else {
      color_profiles <- levels(as.factor(dplyr::pull(ind_data, clust)))
    }
    #}
  }

  if (missing(colornames_recode)) colornames_recode <- character()

  if (length(actives_in_bold) == 0) actives_in_bold <- length(sup_vars) == 0

  if (!length(axes_reverse) == 0) {
    if (!axes_reverse %in% 1:2) stop("axes_reverse must be 1, 2 or 1:2")

    dims_reverse <- unique(c(rlang::as_name(dim1), rlang::as_name(dim2))[axes_reverse])

    reverse_axe <- function(coord) {
      coord |> dplyr::mutate(dplyr::across(tidyselect::all_of(dims_reverse), ~ - .))
    }

    vars_data <- reverse_axe(vars_data)
    if (!is.null(ind_data)) ind_data <- reverse_axe(ind_data)
  }



  #Add contribs in tooltips for active_vars ----
  vars_data <- vars_data |>
    dplyr::mutate(
      contribs = purrr::map_if(
        .data$contribs, !purrr::map_lgl(.data$contribs, is.null),
        ~ dplyr::mutate(., text = str_c(
          "\nContrib axe ", axes[1], " : ", str_pad(round(!!contrib1, 0), 2), "%",
          "\nContrib axe ", axes[2], " : ", str_pad(round(!!contrib2, 0), 2), "%"
        )) |>
          dplyr::pull("text"),

        .else = ~ ""
      ) |>
        purrr::flatten_chr(),

      interactive_text = purrr::map2(
        .data$interactive_text, .data$contribs,
        ~ dplyr::mutate(.x, begin_text = str_c(.data$begin_text, .y))
      )
    ) |>
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
  vars_data <- vars_data |>
    dplyr::mutate(interactive_text = str_replace(.data$interactive_text,
                                                 "</font>$", paste0("</font>", unbrk)))

  # Set colors :
  if (type[1] == "facets" | !is.null(ellipses)) {
    vars_data <- vars_data |>
      dplyr::mutate(color_group = forcats::as_factor(dplyr::if_else(
        condition = .data$vars == sup_vars[1],
        true      = paste0(.data$color_group, "_", .data$lvs), #forcats::fct_expand(paste0(.data$color_group, "_", .data$lvs) |> as.factor(),
        #                    levels(.data$color_group)),
        false     = as.character(.data$color_group)
      )))
  }

  if (length(colornames_recode) > 0) vars_data <- vars_data |>
    dplyr::mutate(color_group = forcats::fct_recode(.data$color_group,
                                                    !!!colornames_recode))
  if (shift_colors != 0) vars_data <- vars_data |>
    dplyr::mutate(color_group = forcats::fct_shift(.data$color_group, shift_colors))
  colorvar_recode <- levels(vars_data$color_group)
  colorvar_recode <- colorvar_recode[!colorvar_recode %in% c("active_vars", "Central point")]
  if (length(colorvar_recode) >= 2) {
    message(str_c("colors based on the following categories (rename with colornames_recode): '",
                           str_c(colorvar_recode, collapse = "', '"), "'",
                           collapse = ""))
  }

  if (length(scale_color_light) == 1 ) {
    scale_color_light <- vctrs::vec_recycle(scale_color_light, length(colorvar_recode))
  }
  if (length(scale_color_dark) == 1 ) {
    scale_color_dark <- vctrs::vec_recycle(scale_color_dark, length(colorvar_recode))
  }


  scale_color_points <- scale_color_light |>
    purrr::set_names(colorvar_recode[1:length(scale_color_light)])

  scale_color_names <- scale_color_dark |>
    purrr::set_names(str_c("names_", colorvar_recode[1:length(scale_color_dark)]))

  if(length(scale_color_light) > length(scale_color_dark)) {
    scale_color_light <- scale_color_light[1:length(scale_color_dark)]
  } else if (length(scale_color_light) < length(scale_color_dark)) {
    scale_color_dark <- scale_color_dark[1:length(scale_color_light)]
  }

  if (length(colorvar_recode[-(1:length(scale_color_light))]) > 0) {
    levels_in_more <- colorvar_recode[-(1:length(scale_color_light))]
    scale_color_points <- scale_color_points |>
      (\(v) append(v, rep(v[length(v)], length(levels_in_more)) |>
               purrr::set_names(levels_in_more)))()
    scale_color_names <- scale_color_names |>
      (\(v) append(v, rep(v[length(v)], length(levels_in_more)) |>
               purrr::set_names(levels_in_more)))()
    warning(str_c("too much colors, all the last ones were set to last color. Max ", length(scale_color_light)))
  }

  if (is.null(base_profiles_color)) base_profiles_color <- "#ffffff"

  scale_color_named_vector <- c(scale_color_points, scale_color_names)
  scale_color_named_vector <- scale_color_named_vector[!is.na(names(scale_color_named_vector))]
  scale_color_named_vector <- c(scale_color_named_vector,
                                "base_profiles_color" = base_profiles_color,
                                "active_vars"         = "black",
                                "Central point"       = "black"
  )

  if (type[1] == "points")  vars_data <- vars_data |>
    dplyr::mutate(colorvar_names = as.factor(str_c("names_", .data$color_group)))
  #} else { sup_vars_data <- sup_vars_data %>% dplyr::mutate(colorvar_names =  color_group) }






  #Calculate limits of graph (arguments to be passed in ggi() to set htmlwidget size)
  min_max_lims <- dplyr::select(vars_data, !!dim1, !!dim2)

  if (!missing(xlim)) min_max_lims <- min_max_lims |>
    tibble::add_row(!!dim1 := xlim[1]) |> tibble::add_row(!!dim1 := xlim[2])
  if (!missing(ylim)) min_max_lims <- min_max_lims |>
    tibble::add_row(!!dim2 := ylim[1]) |> tibble::add_row(!!dim2 := ylim[2])
  height_width_ratio <- min_max_lims |>
    dplyr::summarise_all(~ max(., na.rm = TRUE) - min(., na.rm = TRUE), .groups = "drop")
  min_max_lims <-
    dplyr::bind_rows(dplyr::summarise_all(min_max_lims,
                                          ~ min(., na.rm = TRUE),
                                          .groups = "drop"),
                     dplyr::summarise_all(min_max_lims,
                                          ~ max(., na.rm = TRUE),
                                          .groups = "drop"))
  width_range <- dplyr::pull(height_width_ratio, 1)[1]
  height_width_ratio <- height_width_ratio |>
    dplyr::summarise(height_width_ratio = !!dim2/!!dim1, .groups = "drop") |>
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

  if (text_repel == FALSE | out_lims_move == FALSE) {
    if (!missing(xlim)) vars_data <- vars_data |> outlims(xlim, !!dim1)
    if (!missing(ylim)) vars_data <- vars_data |> outlims(ylim, !!dim2)
  }





  #Profiles :
  if (!is.null(ind_data)) {
    ind_data <-  ind_data |>
      dplyr::mutate(interactive_text = dplyr::bind_rows(.data$interactive_text) |>
                      tidyr::unite("interactive_text", sep = "\n", na.rm = TRUE) |>
                      dplyr::pull("interactive_text") |>
                      str_remove_all("\n#"),
      )

    # ind_data <- ind_data %>%
    #   dplyr::mutate(interactive_text = purrr::map_chr(
    #     .data$interactive_text,
    #     ~ tibble::deframe(tidyr::unite(., "interactive_text", sep = "\n", na.rm = TRUE))
    #   ) %>%
    #     str_remove_all("\n#")
    #   )

    if (length(clust) != 0) { #& type[1] != "facets"

      if (length(color_profiles) == 0 ) {
        if (!is.null(base_profiles_color) ) {

          #Discard the points that are out of limits
          profiles_coord <- ind_data
          if (!missing(xlim)) profiles_coord <- profiles_coord |> outlims(xlim, !!dim1)
          if (!missing(ylim)) profiles_coord <- profiles_coord |> outlims(ylim, !!dim2)

          profiles <- ggiraph::geom_point_interactive(
            data = profiles_coord,
            ggplot2::aes(x = !!dim1, y = !!dim2, size = .data$wcount,
                         tooltip = .data$interactive_text, data_id = .data$clust_id + 10000),
            color = base_profiles_color, na.rm = TRUE, inherit.aes = FALSE,
            show.legend = FALSE, alpha = alpha_profiles
          )
        } else {
          profiles <- NULL
        }


      } else {
        ind_clust_levels <- ind_data |> dplyr::pull(clust) |> unique() |>
          purrr::discard(is.na) |> purrr::discard(\(x) x == "NA")

        not_in_color_profiles <- ind_clust_levels |>
          purrr::discard(\(x) x %in% color_profiles) |>
          (\(v) purrr::set_names(v, rep("base_profiles_color", length(v))))()

        if (clust %in% sup_vars) {
          sup_clust_colorvar <- vars_data |>
            dplyr::select("lvs", "color_group") |>
            dplyr::filter(str_detect(.data$color_group, paste0("^", clust))) |>
            dplyr::mutate(color_group = .data$lvs |> purrr::set_names(.data$color_group)) |>
            dplyr::pull("color_group") |>
            forcats::fct_drop()

          sup_clust_colorvar <- purrr::set_names(as.character(sup_clust_colorvar),
                                               names(sup_clust_colorvar))

          color_profiles_in_colorvar <- sup_clust_colorvar |>
            purrr::keep(\(x) x %in% ind_clust_levels) |>
            purrr::keep(\(x) x %in% color_profiles)

          color_profiles_not_in_colorvar <- color_profiles |>
            purrr::keep(\(x) x %in% ind_clust_levels) |>
            purrr::discard(\(x) x %in% sup_clust_colorvar)

        } else {
          color_profiles_in_colorvar <- character()
          color_profiles_not_in_colorvar <-  color_profiles |>
            purrr::keep(\(x) x %in% ind_clust_levels)
        }


        if (length(color_profiles_not_in_colorvar) != 0) {
          named_color_profiles <- color_profiles_not_in_colorvar |>
            (\(v) purrr::keep(v, !is.null(names(v))))()

          if (length(named_color_profiles) != 0 ) {
            new_colors_in_scale <- names(named_color_profiles) |>
              purrr::set_names(named_color_profiles)

            named_color_profiles <- named_color_profiles |>
              purrr::set_names()

            scale_color_named_vector <- scale_color_named_vector |>
              append(new_colors_in_scale)
          }


          unnamed_color_profiles <- color_profiles_not_in_colorvar |>
            (\(v) purrr::keep(v, is.null(names(v))))()

          if (length(unnamed_color_profiles) > 0) {
            remaining_colors <- material_colors_light() |>
              purrr::discard(\(x) x %in% scale_color_named_vector)

            unnamed_color_profiles <- unnamed_color_profiles |>
              purrr::set_names()

            scale_color_named_vector <- scale_color_named_vector |>
              append(purrr::set_names(remaining_colors[1:length(unnamed_color_profiles)], unnamed_color_profiles))

            if  (length(remaining_colors) < length(unnamed_color_profiles)) {
              stop("Not enough colors in scale to color profiles.")
            }
          }
        } else {
          named_color_profiles   <- character()
          unnamed_color_profiles <- character()
        }

        clust_colorvar_recode <- named_color_profiles |>
          append(unnamed_color_profiles) |>
          append(not_in_color_profiles) |>
          append(color_profiles_in_colorvar)


        ind_data <- ind_data |>
          dplyr::mutate(color_group = forcats::fct_recode(.data$clust,
                                                          !!!clust_colorvar_recode))
        # ind_data |> dplyr::select(color_group) |> print(n = 40)

        #Discard the points that are out of limits
        profiles_coord <- ind_data
        if (!missing(xlim)) profiles_coord <- profiles_coord |> outlims(xlim, !!dim1)
        if (!missing(ylim)) profiles_coord <- profiles_coord |> outlims(ylim, !!dim2)

        profiles <- ggiraph::geom_point_interactive(
          data = profiles_coord,
          ggplot2::aes(x = !!dim1, y = !!dim2, size = .data$wcount,
                       tooltip = .data$interactive_text,
                       data_id = .data$clust_id + 10000, color = .data$color_group),
          na.rm = TRUE, inherit.aes = FALSE, show.legend = FALSE,
          alpha = alpha_profiles, stroke = 0
        )
      }

    } else { # If length(clust) == 0
      if (!is.null(base_profiles_color) ) {

        #Discard the points that are out of limits
        profiles_coord <- ind_data
        if (!missing(xlim)) profiles_coord <- profiles_coord |> outlims(xlim, !!dim1)
        if (!missing(ylim)) profiles_coord <- profiles_coord |> outlims(ylim, !!dim2)

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

      ind_data <- ind_data |>
        dplyr::mutate(sup_vars = purrr::map(.data$sup_vars,
                                            ~ dplyr::select(., !!rlang::sym(sup_vars[1])))) |>
        tidyr::unnest(c("sup_vars", "row.w"))

      supvar1_lvs <-
        dplyr::filter(vars_data, .data$vars == sup_vars[1]) |>
        dplyr::pull("lvs") |> as.character() |> purrr::set_names()

      supvar1_colorvar <- dplyr::filter(vars_data, .data$vars == sup_vars[1]) |>
        dplyr::select("lvs", "color_group")
      supvar1_colorvar <- as.character(supvar1_colorvar$color_group) |> purrr::set_names(supvar1_colorvar$lvs)

      supvar1_infos <- dplyr::filter(vars_data, .data$vars == sup_vars[1]) |>
        dplyr::mutate(nam = .data$lvs) |>
        dplyr::select("nam", "lvs", "color_group", "id") |>
        tidyr::nest(infos = c("lvs", "color_group", "id"))
      supvar1_infos <- supvar1_infos$infos |> purrr::set_names(supvar1_infos$nam)

      if (!is.null(ellipses)) {
        ellipses_coord <- ind_data |>
          dplyr::select(!!dim1, !!dim2, "row.w", tidyselect::all_of(sup_vars[1]), tidyselect::any_of("lvs")) |>
          dplyr::mutate(infos = supvar1_infos[as.character(!!rlang::sym(sup_vars[1]))],
          ) |>
          tidyr::unnest(cols = c("infos")) |>
          dplyr::filter(!is.na(.data$lvs))


        ellipses <-
          if (type[1] == "facets") {
            ggiraph::geom_path_interactive(data = ellipses_coord,
                                           ggplot2::aes(x = !!dim1, y = !!dim2,
                                                        group = .data$lvs, data_id = .data$id),
                                           color = "black",
                                           stat = "ellipse",
                                           type = "t", level = ellipses, linewidth = 1,
                                           segments = 360, alpha = 1, inherit.aes = FALSE)
          } else {
            ggplot2::geom_path(data = ellipses_coord,
                               ggplot2::aes(x = !!dim1, y = !!dim2,
                                            group = .data$lvs,
                                            color = .data$color_group),
                               stat = "ellipse",
                               type = "t", level = ellipses, linewidth = 1,
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
        ind_data <- ind_data |>
          tidyr::nest(row.w = "row.w") |>
          dplyr::mutate(count  = purrr::map_int(.data$row.w, ~ nrow(.)),
                        wcount = purrr::map_dbl(.data$row.w, ~ sum(., na.rm = TRUE))
          ) |>
          #dplyr::select(-.data$row.w) %>%
          dplyr::arrange(!!rlang::sym(sup_vars[1]), -.data$wcount) |>
          dplyr::mutate(lvs = purrr::map(!!rlang::sym(sup_vars[1]),
                                         ~ supvar1_lvs[as.character(.)]
          ) |> unlist(),

          color_group = purrr::map(!!rlang::sym(sup_vars[1]),
                                   ~ supvar1_colorvar[as.character(.)]
          ) |> unlist()
          ) |>
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
  vars_data <- vars_data |>
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
  vars_data <- vars_data |> dplyr::filter(.data$lvs != "Central point")

  #Theme
  if (!missing(title)) {
    title_graph <- ggplot2::labs(title = title) #str_c("Les Active variables de l'ACM sur les axes ",axes[1], " et ", axes[2] )
  } else {
    title_graph <- NULL
  }

  # WARNING: with no supplementary variable, "points" and "labels" map no colour at all (the active
  #   levels are drawn in fixed black): their sup layers are not built, and neither is the manual
  #   scale, which ggplot2 would otherwise warn shares no level with the (empty) data.
  sup_data <- dplyr::filter(vars_data, .data$color_group != "active_vars")
  has_sup  <- nrow(sup_data) != 0
  colour_scale <- if (has_sup || !type[1] %in% c("points", "labels")) {
    ggplot2::scale_colour_manual(values = scale_color_named_vector,
                                 aesthetics = c("colour", "fill"))
  }

  graph_theme_acm <-
    list(theme_acm_with_lims,
         colour_scale,
         ggplot2::theme(plot.margin = ggplot2::margin(r = right_margin,
                                                      unit = "cm")),
         title_graph)


  #Separate graph for active_vars with type != "text"
  if (type[1] %in% c("points", "labels")) {
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

    if (length(clust) > 0 ) {
      clust_data  <- vars_data |> dplyr::filter(.data$vars == clust)
      vars_data <- vars_data |> dplyr::filter(.data$vars != clust)
      if (text_repel == FALSE) {
        graph_clust <-
          ggiraph::geom_label_interactive(
            data = clust_data,
            ggplot2::aes(label = .data$lvs, color = .data$color_group,
                         tooltip = .data$interactive_text),
            fill = grDevices::rgb(1, 1, 1, alpha = 0.9),
            fontface = "bold", size = text_size, na.rm = TRUE
          )

      } else {
        graph_clust <-
          # list(
          # geom_segment(
          #   data = acm_clust |>
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
          data = clust_data,
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
      graph_clust <- NULL
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
      graph_theme_acm + profiles + ellipses + graph_text + graph_clust +
      mean_point_graph




  } else if (type[1] == "points") {
    #If active vars too, points in gray
    sup_points <- if (has_sup) list(
      ggiraph::geom_text_repel_interactive(
        ggplot2::aes(color = .data$colorvar_names, tooltip = .data$interactive_text),
        size = text_size, hjust = "left",  segment.alpha = 0.2, #segment.colour = "black",
        direction = "both", nudge_x = dist_labels[1], point.padding = 0.25,
        na.rm = TRUE, fontface = "plain"
      ),
      ggiraph::geom_point_interactive(
        ggplot2::aes(size = .data$wcount, fill = .data$color_group,
                     tooltip = .data$interactive_text),
        shape = 18, na.rm = TRUE
      )
    )

    plot_output <-
      ggplot2::ggplot(sup_data,
                      ggplot2::aes(x = !!dim1, y = !!dim2, label = .data$lvs,
                                   color = .data$color_group, data_id = .data$id)) +
      graph_theme_acm + profiles + active_graph + ellipses + sup_points +
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
      ggplot2::ggplot(sup_data,
                      ggplot2::aes(x = !!dim1, y = !!dim2, label = .data$lvs,
                                   color = .data$color_group, data_id = .data$id)) +
      graph_theme_acm + profiles + active_graph + ellipses + (if (has_sup) graph_labels) +
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

    attr(plot_output, "css_hover") <- css_hover

  } else { stop('unknown type of graph') }

  # DESIGN: render hints ride on the ggplot object as attributes, read back by ggi() and ggsave2().
  # They must NOT be list slots: append() would flatten the S7 ggplot into a plain list, and the
  # ggplot generics (ggplot_build(), grid.draw(), and so ggsave2()) would stop dispatching on it.

  attr(plot_output, "height_width_ratio") <- height_width_ratio

  return(plot_output)

}
