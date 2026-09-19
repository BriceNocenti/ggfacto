# PURPOSE: The shared rendering and export layer -- theme_facto() and its axis titles, the material
#   palettes, ggi(), ggsave2(), as_ggfacto_plot(), and the internal plot_path().
# ROLE: What every 2D graph ends in. as_ggfacto_plot() makes it a ggfacto graph (its class, its
#   render hints), theme_facto() supplies the axis titles carrying eigenvalue percentages, ggi()
#   turns it into a girafe widget (a widget passes through), with the stylesheet that shows an
#   element while another is hovered (reveal_on_hover()) and the rule keeping a tooltip's colours
#   against the host page's (tooltip_ink()), or saves it as a standalone page that
#   fills its window or frame (fill_page(), R/knit.R); ggsave2() writes an image.
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
  first_axe_title  <- axis_title(res, axes[1])
  second_axe_title <- axis_title(res, axes[2])

  if (no_color_scale == FALSE) {
    scale_color_acm <- ggplot2::scale_color_brewer(palette = "Dark2") #material_colors_light() ?
    scale_fill_acm <- ggplot2::scale_fill_brewer(palette = "Dark2")
  } else {
    scale_color_acm <- NULL
    scale_fill_acm <- NULL
  }

  # the axes keep their proportions, whatever the limits
  coord_graph <- ggplot2::coord_fixed(xlim = if (!missing(xlim)) xlim,
                                      ylim = if (!missing(ylim)) ylim)

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

# The title of an axis: its number, its percentage of variance, and the name the user gave it in
# `res$axes_names`, in the language in effect.
#' @keywords internal
#' @noRd
axis_title <- function(res, a) {
  pct <- gda_num(round(eig_table(res)[a, 2], 1), gda_resolve_lang(NULL))
  nm  <- res$axes_names[a]
  if (length(nm) == 1 && !is.na(nm) && nzchar(nm)) {
    gettextf("Axe %s (%s%%): %s", a, pct, nm)
  } else {
    gettextf("Axe %s (%s%%)", a, pct)
  }
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



#' Make a graph interactive
#' @param plot The graph, made with \code{\link{ggfacto}} (or \code{\link{ggmca}},
#' \code{\link{ggca}}, \code{\link{ggpca}}, \code{\link{ggpca_cor_circle}}), with or without
#' \pkg{ggplot2} elements added with \code{+}. A graph that is already interactive is returned as it
#' is.
#' @param width The width in centimeters. Default to printing device's size.
#' @param height The height in centimeters. Default to printing device's size.
#' @param keep_ratio By default, the height is forced based of the relative
#' size of the MCA's axes. Set to \code{FALSE} to avoid this behavior.
#' @param savewidget Should the html widget be saved on disk ? The file is standalone:
#' one single \code{.html} carrying its own JavaScript, ready to be sent to someone, or shown
#' in an \code{<iframe>}: the graph fills the window or the frame it is opened in.
#' @param dir If saved as file, the directory in which to save the html widget.
#' Default to temporary directory. Set global option \code{"ggfacto.export_dir"}
#' with \code{link[base:options](options)} to change default directory.
#' @param open Should the resulting file be opened at once ?
#' @param name The name of the file to save.
#' @param replace Replace file ? By default, number added to find a new name.
#' @param ... Additional arguments to pass to \code{\link[ggiraph:girafe]{girafe}} and
#' \code{\link[ggiraph:dsvg]{dsvg}}. The widget embeds the Liberation Sans font alone;
#' \code{font_set} (see \code{\link[gdtools:font_set]{gdtools::font_set()}}) embeds others.
#' @param iframe,pixel_width Deprecated and ignored: the widget sizes itself.
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
                open = rlang::is_interactive(), ..., iframe, pixel_width
) {
  if (!missing(iframe) || !missing(pixel_width)) deprecated_notice(
    "ggi::iframe", "ggi(iframe =, pixel_width =) are deprecated and ignored.")

  # DESIGN: a widget passes through, so `ggpca_cor_circle(interactive = TRUE) |> ggi()` and a
  #   second ggi() cost nothing; it can still be saved.
  widget <- if (inherits(plot, "htmlwidget")) plot else girafe_widget(plot, width, height,
                                                                        keep_ratio, ...)

  if (savewidget == FALSE) {
    return(widget)

  } else {
    path <- plot_path(dir = dir, name = name, extension = "html", replace = replace)

    if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
      stop("`savewidget = TRUE` requires the htmlwidgets package.", call. = FALSE)
    }
    # WARNING: saved from its own directory: saveWidget() writes its `<name>_files` library beside
    #   the file but unlinks it relative to the working directory, and would leave it behind.
    withr::with_dir(dirname(path), htmlwidgets::saveWidget(fill_page(widget), basename(path),
                                                           selfcontained = TRUE, title = name))

    if (open == TRUE) file.show(path)

    return(invisible(widget))
  }
}



# The girafe of a ggplot, sized from its own ratio when it has one. Render hints are attributes set
# by the graph builders; a plain ggplot carries none, so every read tolerates NULL.
# DESIGN: `font_set` embeds Liberation Sans alone, the one family theme_facto() draws with, where
#   ggiraph's default embeds sans, serif, mono and symbol: 3.6 MB for a standalone widget instead of
#   9.2 MB. `ggi(font_set =)` passes another through `...`.
#' @keywords internal
#' @noRd
girafe_widget <- function(plot, width, height, keep_ratio,
                          font_set = gdtools::font_set(sans = gdtools::font_liberation("sans")),
                          ...) {
  css_hover <- attr(plot, "css_hover", exact = TRUE)
  if (is.null(css_hover)) {
    # DESIGN: an svg text takes its colour from `fill`: dark gold on a label's light yellow box,
    #   where the generic yellow fill made the text vanish into it.
    css_hover <- ggiraph::girafe_css("fill:#d2b200;stroke:orange;",
                                     text  = "fill:#8b7500;stroke:none;",
                                     point = "fill:gold;stroke:orange;",
                                     line  = "stroke:#d2b200;",
                                     area  = "fill:#ffe348")
  }
  css_tooltip <- attr(plot, "css_tooltip", exact = TRUE)
  if (is.null(css_tooltip)) {
    # DESIGN: `white-space:nowrap` -- a tooltip keeps its full width near the right edge, where it
    #   was squeezed into the space left and its lines wrapped; at full width, ggiraph measures it
    #   and places it to the left of the pointer instead.
    css_tooltip <- str_c("color:#000000;text-align:right;padding:4px;border-radius:5px;",
                         "background-color:#eeeeee;white-space:nowrap;")
  }

  width <- if (is.null(width)) grDevices::dev.size("in")[1] else width / 2.54
  ratio <- attr(plot, "height_width_ratio", exact = TRUE)
  height <- if (keep_ratio && !is.null(ratio)) {
    width * ratio
  } else if (is.null(height)) {
    grDevices::dev.size("in")[2]
  } else {
    height / 2.54
  }

  widget <- ggiraph::girafe(ggobj = plot, width_svg = width, height_svg = height,
                            font_set = font_set, ...) |>
    ggiraph::girafe_options(ggiraph::opts_tooltip(css = css_tooltip),
                            ggiraph::opts_hover(css = css_hover))
  widget$x$html <- tooltip_ink(reveal_on_hover(widget$x$html))
  as_ggfacto_widget(widget, ratio = height / width)
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

# WARNING: an element shown only while another is hovered (a PCA vector's projections) must carry no
#   hover id, or ggiraph lets it catch the pointer and passing near it lights its vector. It is drawn
#   transparent with the id `reveal-<id>`, turned here into a plain attribute, and a stylesheet shows
#   it while an element of hover id <id> is hovered (css :has(), which every current browser reads).
#' @keywords internal
#' @noRd
reveal_on_hover <- function(svg) {
  ids <- unique(regmatches(svg, gregexpr("(?<=data-id='reveal-)[^']+(?=')", svg, perl = TRUE))[[1]])
  if (length(ids) == 0) return(svg)
  svgid <- regmatches(svg, regexpr("(?<=<svg )[^>]*?id='\\K[^']+", svg, perl = TRUE))
  svg   <- gsub("data-id='reveal-([^']+)'", "data-reveal='\\1'", svg)
  rules <- paste0("#", svgid, ":has([data-id='", ids, "'].hover_data_", svgid, ") [data-reveal='",
                  ids, "'] { stroke-opacity:1; fill-opacity:1; }", collapse = "\n")
  # the revealed elements wear the hover's colours, as a hovered arrow and its label do
  style <- paste0("<style>[data-reveal] { pointer-events:none; fill-opacity:0; stroke-opacity:0; }\n",
                  "line[data-reveal] { stroke:#d2b200 !important; }\n",
                  "polygon[data-reveal] { fill:#ffe348 !important; }\n",
                  "text[data-reveal] { fill:#8b7500 !important; }\n", rules, "</style>")
  sub("(<svg [^>]*>)", paste0("\\1", style), svg)
}

# WARNING: a tooltip's bold text inherits its colour -- a graded cell is `<font color><b>`, and a
#   host page's own rule on `b` (bootstrap's `strong, b`, black, gold in dark mode) otherwise wins
#   over inheritance and blanks every colour. `!important`: bootstrap's dark-mode rule is the more
#   specific. ggiraph's tooltip div is classed `tooltip_<svg id>`; a <style> in an svg is global.
#' @keywords internal
#' @noRd
tooltip_ink <- function(svg) {
  svgid <- regmatches(svg, regexpr("(?<=<svg )[^>]*?id='\\K[^']+", svg, perl = TRUE))
  if (length(svgid) == 0) return(svg)
  style <- paste0("<style>.tooltip_", svgid, " b, .tooltip_", svgid,
                  " strong { color:inherit !important; }</style>")
  sub("(<svg [^>]*>)", paste0("\\1", style), svg)
}

# Why this exists: every 2D graph ends here, so that it is a ggplot that knows what it is -- its
# class for knit_print() (R/knit.R), its render hints for ggi() and ggsave2().
# DESIGN: the class is PREPENDED and the hints ride as ATTRIBUTES, which both survive `+`. Never list
#   slots, and never an append(): that flattens the S7 ggplot into a plain list, and ggplot_build(),
#   grid.draw() and so ggsave2() stop dispatching on it. `ratio` is height / width, NULL when the
#   graph has no opinion.
#' @keywords internal
#' @noRd
as_ggfacto_plot <- function(p, ratio = NULL, css_hover = NULL) {
  if (!is.null(ratio) && (length(ratio) != 1 || !is.finite(ratio) || ratio <= 0)) ratio <- NULL
  attr(p, "height_width_ratio") <- ratio
  if (!is.null(css_hover)) attr(p, "css_hover") <- css_hover
  class(p) <- unique(c("ggfacto_plot", class(p)))
  p
}
