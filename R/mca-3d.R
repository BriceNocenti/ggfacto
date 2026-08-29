# PURPOSE: ggmca_3d() -- the plotly 3D (or 2D) view of an MCA cloud.
# ROLE: Uses ggmca(get_data = TRUE) purely as a data getter, then draws with plotly instead of
#   ggplot2. Nothing in the 2D path depends on this file.
# KEY CONSTRAINTS:
#   - plotly is a Suggests: the entry point is guarded with requireNamespace().
#   - profiles = TRUE is forced -- a 3D cloud with no individuals in it has nothing to show.
# See: CLAUDE.md section Repository Map.

#'  Interactive 3D Plot for Multiple Correspondence Analyses (plotly::)
#'
#' @param res.mca An object created with \code{FactoMineR::\link[FactoMineR]{MCA}}.
#' @param data The data in which to find the cah variable, etc.
#' @param cah A variable made with \code{\link[FactoMineR]{HCPC}}, to link
#' the answers-profiles points who share the same HCPC class (will be colored
#' the same color and linked at mouse hover).
#' @param axes The axes to print, as a numeric vector of length 3.
#' @param dat Deprecated former name of `data`. Still accepted, with a warning;
#' use `data` instead.
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
#' ggmca_3d(res.mca, data = tea, cah = "clust")
#' }
ggmca_3d <- function(res.mca, data, cah, axes = 1:3, # color_groups,
                     base_zoom = 1, remove_buttons = FALSE, cone_size = 0.15,
                     view = "All",
                     camera_view, aspectratio_from_eig = FALSE, title,
                     ind_name.size = 10, max_point_size = 30, # ind.size = 4,
                     ...,
                     dat) {
  requireNamespace("plotly", quietly = TRUE)

  # `dat` was renamed `data` in 0.4.0; after `...` it can only be supplied by name.
  if (!missing(dat) && missing(data)) data <- renamed_arg(dat, "dat", "data", "ggmca_3d")

  if (missing(cah)) cah <- character()

  D2 <- length(axes) == 2 ; stopifnot(length(axes) %in% 2:3 )
  if (D2) axes <- c(axes, NA)

  dim1 <- rlang::sym(str_c("Dim ", axes[1]))
  dim2 <- rlang::sym(str_c("Dim ", axes[2]))

  # if (missing(color_groups)) color_groups <- "^.{1}"

  acm <- res.mca |>
    ggmca(data = data,
          cah = cah,
          # color_groups = color_groups,
          profiles = TRUE,
          get_data = TRUE,
          ...
    )

  acm_cah <- acm$vars_data |>
    dplyr::filter(str_detect(.data$color_group, paste0("^", cah)))
  # a plain plotting tibble: no fmt column, so the tab class bought nothing
  acm_vars <- acm$vars_data |>
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


  # height_width_ratio <- (0.8 + 1.2) / (0.8 + 1.1)


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
    dplyr::mutate(name = str_replace(.data$name, "Dim ", "Axe ") ) |>
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

  dim1 <- rlang::sym(str_c("Dim ", axes[1]))
  dim2 <- rlang::sym(str_c("Dim ", axes[2]))
  dim3 <- if (D2) {NULL} else {rlang::sym(str_c("Dim ", axes[3]))}


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
                         str_replace("scene1", "scene") )
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
