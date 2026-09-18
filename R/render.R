# PURPOSE: The shared rendering and export layer -- theme_facto(), the material palettes, ggi(),
#   ggsave2(), and the internals plot_path() and outlims().
# ROLE: What every 2D graph ends in. theme_facto() supplies the axis titles carrying eigenvalue
#   percentages, ggi() turns a ggfacto ggplot into a girafe widget, ggsave2() writes an image.
# KEY CONSTRAINTS:
#   - theme_facto() returns a LIST of ggplot objects, not a theme: it is `+`-ed as a whole.
#   - "Jaune 800" is commented out of both palettes on purpose. It is reserved for the ggiraph
#     hover colour, and a point already painted with it could not be seen to highlight.
#   - ggi()/ggsave2() read css_hover, css_tooltip and height_width_ratio from the plot object's
#     ATTRIBUTES. They are absent on a plain ggplot, so every read must tolerate NULL.
#   - ggi() hands the girafe to as_ggfacto_widget() (R/knit.R), which records the aspect ratio it
#     just resolved. That attribute is the ONLY geometry a knitted <iframe> has to go on, so it
#     must keep being written here, where width and height are both known.
# See: CLAUDE.md section ggfacto architecture > The plot-object seam.

#' A ggplot2 Theme for Geometrical Data Analysis
#'
#' @param res An analysis, made with \code{\link{multiple_correspondence_analysis}},
#' \code{\link{correspondence_analysis}}, \code{\link{principal_component_analysis}}, FactoMineR or
#' GDAtools.
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
  pct <- eig_table(res)[, 2]
  if (exists("axes_names", where = res)) {
    first_axe_title  <-
      str_c(
        "Axe ", axes[1]," (", round(pct[axes[1]], 1),
        "%)",
        if (!is.null(res$axes_names[axes[1]]) ) paste0(" : ", res$axes_names[axes[1]])
      )
    second_axe_title <-
      str_c(
        "Axe ", axes[2]," (", round(pct[axes[2]], 1),
        "%)",
        if (!is.null(res$axes_names[axes[2]]) ) paste0(" : ", res$axes_names[axes[2]])
      )
  } else {
    first_axe_title  <-
      str_c("Axe ", axes[1]," (",
                     round(pct[axes[1]], 1), "%)")
    second_axe_title <-
      str_c("Axe ", axes[2]," (",
                     round(pct[axes[2]], 1), "%)")
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

#' Light Material palette for MCA points
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

#' Dark Material palette for MCA level names
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
#' @param savewidget Should the html widget be saved on disk ? The file is standalone:
#' one single \code{.html} carrying its own JavaScript, ready to be sent to someone.
#' @param dir If saved as file, the directory in which to save the html widget.
#' Default to temporary directory. Set global option \code{"ggfacto.export_dir"}
#' with \code{link[base:options](options)} to change default directory.
#' @param open Should the resulting file be opened at once ?
#' @param name The name of the file to save.
#' @param replace Replace file ? By default, number added to find a new name.
#' @param ... Additional arguments to pass to \code{\link[ggiraph:girafe]{girafe}} and
#' \code{\link[ggiraph:dsvg]{dsvg}}. \code{fonts} can be used to provide text fonts.
#'
#' @return An html plot, of class \code{ggfacto_widget}. In a \pkg{knitr} document,
#' setting option \code{"ggfacto.widget_dir"} writes it to its own file and embeds an
#' \code{<iframe>} instead of the widget itself: see \link{ggfacto_widget}.
#' @export
#'
# @examples
ggi <- function(plot = ggplot2::last_plot(),
                width = NULL, height = NULL, keep_ratio = TRUE,
                savewidget = FALSE, dir = NULL, name = "Plot", replace = FALSE,
                open = rlang::is_interactive(), ...
) {

  # Render hints are attributes set by ggmca_plot()/ggca(); a plain ggplot carries none, so every
  # read below must tolerate NULL.
  if (!is.null(attr(plot, "css_hover", exact = TRUE))) {
    css_hover <- attr(plot, "css_hover", exact = TRUE)
  } else {
    css_hover <- ggiraph::girafe_css("fill:#d2b200;stroke:orange;",
                                     text  = "color:gold4;stroke:none;",
                                     point = "fill:gold;stroke:orange;",
                                     area  = "fill:#ffe348")
  }


  if (!is.null(attr(plot, "css_tooltip", exact = TRUE))) {
    css_tooltip <- attr(plot, "css_tooltip", exact = TRUE)
  } else {
    css_tooltip <- "color:#000000;text-align:right;padding:4px;border-radius:5px;background-color:#eeeeee;"
  }

  # if(.Platform$OS.type == "windows") {
  #   css_tooltip <-
  #     paste0(css_tooltip, "font-family:", grDevices::windowsFonts("sans"), ";") #%>%
  #   #str_replace("DejaVu Sans Condensed", "DejaVu Sans")
  # }

  if (is.null(width)) { #   if (missing(width)) {
    width <- grDevices::dev.size("in")[1]

  } else {
    width <- width/2.54
  }

  if (keep_ratio == TRUE & !is.null(attr(plot, "height_width_ratio", exact = TRUE))) {
    height <- width * attr(plot, "height_width_ratio", exact = TRUE)

  } else {
    if (is.null(height)) { #     if (missing(height)) {
      height <- grDevices::dev.size("in")[2]
    } else {
      height = height/2.54
    }
  }

  # if (is.null(plot$height_width_ratio)) height <- NULL

  widget <-
    ggiraph::girafe(ggobj = plot,
                    width_svg = width,
                    height_svg = height , #if_else(missing(height), width/2.563 * plot$height_width_ratio, height/2.563)
                    # fonts = ifelse(.Platform$OS.type == "windows",
                    #                grDevices::windowsFonts("sans") %>%
                    #                  purrr::map(~str_replace(., "DejaVu Sans Condensed",
                    #                                                   "DejaVu Sans")),
                    #                NULL
                    # ),  #list(sans = "DejaVu Sans Condensed") #grDevices::windowsFonts("sans")
                    ...
    ) |>
    ggiraph::girafe_options(ggiraph::opts_tooltip(css = css_tooltip), #, use_fill = TRUE, #use_stroke = FALSE, # = border color of the tooltip #color:white; border-color:black; opacity:1 ; background-color:transparent
                            ggiraph::opts_hover(css = css_hover)
                            # ggiraph::opts_zoom(max = 5) # bugue pas mal
                            # ggiraph::opts_hover(css = girafe_css(css = "fill:purple;stroke:black;", text = "stroke:none;fill:red;font-style:bold;")) #    point = NULL, line, area, image
                            # ggiraph::opts_hover_inv(css = "opacity:0.1;"),
                            # ggiraph::opts_sizing(rescale = FALSE)
                            # ggiraph::opts_sizing(rescale = TRUE, width = 0.7), #between 0 and 1
                            # ggiraph::opts_toolbar(saveaspng = FALSE)
    )


  widget <- as_ggfacto_widget(widget, ratio = height / width)

  if (savewidget == FALSE) {
    return(widget)

  } else {
    path <- plot_path(dir = dir, name = name, extension = "html", replace = replace)

    if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
      stop("`savewidget = TRUE` requires the htmlwidgets package.", call. = FALSE)
    }
    htmlwidgets::saveWidget(widget, path, selfcontained = TRUE, title = name)

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
    if (!is.null(attr(plot, "height_width_ratio", exact = TRUE))) {
      height <- width * attr(plot, "height_width_ratio", exact = TRUE)
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

  path_name <- str_remove(path, "\\..+$")
  if (! str_detect(path, "\\..+$")) path <- str_c(path, ".", extension)
  if (replace == FALSE) {
    i <- 0
    file_do_not_exist <- FALSE
    while (file_do_not_exist == FALSE) {
      if (file.exists(path)) {
        i = i+1
        path <- str_c(path_name, i, ".", extension)
      } else {
        path <-
          str_c(path_name, dplyr::if_else(i == 0,
                                                   "",
                                                   str_c(i)),
                         ".", extension)
        file_do_not_exist <- TRUE
      }
    }
  }
  writeLines(path)
  return(path)
}

# Why this exists: shared by ggmca_plot() and ggca(), which both drop the points falling outside
# xlim/ylim before drawing -- but only when the labels are not repelled, since a repelled label
# needs its anchor point to exist.
#' @keywords internal
outlims <- function(data, lim, dim) {
  dim <- rlang::enquo(dim)
  if (!is.na(lim[1])) data <- data |> dplyr::filter(!!dim > lim[1])
  if (!is.na(lim[2])) data <- data |> dplyr::filter(!!dim < lim[2])
  return(data)
}
