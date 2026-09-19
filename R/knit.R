# PURPOSE: The seams between ggfacto's graphs and knitr: a widget's payload kept out of the markdown
#   pandoc has to read, and a static graph drawn at its own aspect ratio.
# ROLE: as_ggfacto_widget() tags every widget the package returns; knit_print.ggfacto_widget()
#   writes it to its own file and emits an <iframe> instead, when the document asks for it.
#   knit_print.ggfacto_plot() draws a ggfacto ggplot at the chunk's width and its own height.
# KEY CONSTRAINTS:
#   - Tagging PREPENDS the class, so print.girafe / print.plotly still dispatch by inheritance and
#     interactive display is untouched. Never put the method on `girafe` or `htmlwidget`: that
#     would hijack widgets other packages built.
#   - The externalisation is opt-in through option "ggfacto.widget_dir". Unset, the method falls
#     through to htmlwidgets' own knit_print and the output is byte-for-byte what it always was.
#   - Geometry is resolved in R, once, and written into the iframe as a CSS aspect-ratio. There is
#     no JavaScript height negotiation: a graph whose axes lose their scale cannot be interpreted.
# See: CLAUDE.md section ggfacto architecture > The plot-object seam.


#' Widgets written to their own file
#'
#' Every interactive graph \pkg{ggfacto} returns -- \link{ggi} (and so \code{ggfacto(interactive
#' = TRUE)}), \link{ggmca_3d}, \link{ggpca_3d} -- carries the class \code{ggfacto_widget}. In a
#' \pkg{knitr} document, setting
#'
#' \code{options(ggfacto.widget_dir = "auto")}
#'
#' makes such a widget write itself to \code{widget_<chunk label>.html} and put an
#' \code{<iframe>} in the document instead of several megabytes of inline JSON. Use
#' \code{"auto"} to follow the chunk's \code{fig.path}, so the files travel with the document
#' exactly like its figures do, or give a directory path of your own.
#'
#' This matters for books: \pkg{bookdown} merges every chapter into one markdown file and hands
#' it to a single \pkg{pandoc} call, and a multi-megabyte raw HTML block on one line is what that
#' reader handles worst.
#'
#' The chunk must have a label, since the label names the file. Widgets in a chunk with
#' \code{results = "hide"} are left alone: nothing would reference the file.
#'
#' The frames share one copy of the JavaScript, which the document itself ships. Option
#' \code{"ggfacto.widget_lib_dir"} says where, relative to the document: the default
#' \code{"libs"} is what a \pkg{bookdown} book uses, while a plain
#' \code{rmarkdown::html_document} with \code{self_contained = FALSE} puts them in
#' \code{"<name>_files"}. If it is wrong the frames say so rather than coming out blank.
#'
#' Unset (the default), the option changes nothing and the widget is embedded inline.
#'
#' @name ggfacto_widget
NULL


# Tag a widget so knit_print.ggfacto_widget() can find it, and record the aspect ratio its
# producer resolved. `ratio` is height/width; NULL when the producer has no opinion (the 3D
# plotly graphs), in which case the chunk's fig.* options decide.
as_ggfacto_widget <- function(widget, ratio = NULL) {
  if (!is.null(ratio) && (!is.finite(ratio) || ratio <= 0)) ratio <- NULL
  attr(widget, "ggfacto_ratio") <- ratio

  # WARNING: the tag goes AFTER the widget's own name class and BEFORE "htmlwidget". htmlwidgets
  # reads class(x)[1] to find the JavaScript binding, so prepending would silently strip the
  # ggiraph/plotly library from the page and leave a blank graph; appending would let
  # knit_print.htmlwidget win the dispatch and the widget would stay inline.
  cl <- class(widget)
  at <- match("htmlwidget", cl)
  class(widget) <- append(cl, "ggfacto_widget", after = if (is.na(at)) length(cl) else at - 1L)

  widget
}


# knitr's knit_print generic; registered in .onLoad() so knitr stays a soft dependency.
knit_print.ggfacto_widget <- function(x, ...) {
  dir <- getOption("ggfacto.widget_dir")

  # results="hide" throws the output away, so writing a file would only orphan it.
  if (is.null(dir) || identical(knitr::opts_current$get("results"), "hide")) {
    return(NextMethod())
  }

  label <- knitr::opts_current$get("label")
  if (is.null(label) || grepl("^unnamed-chunk-", label)) {
    stop("A chunk holding a ggfacto widget needs a label, which names its file. ",
         "Label this chunk, or unset option \"ggfacto.widget_dir\".", call. = FALSE)
  }

  if (identical(dir, "auto")) dir <- knitr::opts_current$get("fig.path")
  if (is.null(dir) || !nzchar(dir)) dir <- "."
  dir <- sub("/+$", "", dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  file <- file.path(dir, paste0("widget_", label, ".html"))
  deps <- save_widget_page(x, file, libdir = widget_libdir(dir))

  # WARNING: the JavaScript is shared, so somebody has to ship it. Handing the dependencies to
  # knitr as chunk metadata makes the DOCUMENT carry them, exactly as it did when the widget was
  # inline -- which is also what makes the build copy them to the output directory. Without this
  # the frames come out blank, and nothing says so.
  knitr::asis_output(widget_iframe(x, src = file, label = label), meta = deps)
}


# Where the widget page looks for the shared JavaScript: the document's library directory, reached
# by climbing out of `dir`. Inlining the libraries instead would cost 8 MB per graph -- pandoc
# base64-encodes the fonts too -- so they are shared, and this path is the price.
# WARNING: option "ggfacto.widget_lib_dir" names that directory RELATIVE TO THE DOCUMENT, and each
# output format picks its own. The default suits a bookdown book ("libs" beside the chapters); a
# plain rmarkdown::html_document with self_contained = FALSE puts them in "<name>_files" instead.
# Get it wrong and the frames come out blank -- which is why the page says so, see widget_guard().
widget_libdir <- function(dir) {
  lib <- getOption("ggfacto.widget_lib_dir", "libs")
  up <- setdiff(strsplit(dir, "/+")[[1]], c("", "."))
  paste(c(rep("..", length(up)), lib), collapse = "/")
}


# Turn the one silent failure mode into a visible one. If the shared scripts did not load, the
# frame would otherwise just be blank, and a blank frame in a course page looks like a graph that
# has not finished loading rather than a build that needs fixing.
widget_guard <- function() {
  paste0(
    "<script>window.addEventListener(\"load\",function(){",
    "if(!window.HTMLWidgets){document.body.innerHTML=",
    "'<p style=\"font:13px sans-serif;color:#b71c1c;padding:1em\">",
    "Interactive graph unavailable: the shared JavaScript files were not found. ",
    "See ?ggfacto_widget, option \"ggfacto.widget_lib_dir\".</p>';}});</script>"
  )
}


# Write the widget as a page of its own, pointing at the shared library directory.
#
# DESIGN: the page is assembled from htmltools primitives rather than by htmlwidgets::saveWidget().
# saveWidget() insists on copying the libraries into a directory BELOW the page -- it refuses a
# libdir reached by "..", and it would otherwise write 23 MB beside each of a book's graphs.
# Rewriting each dependency's src as a plain `href` renders the same <script>/<link> tags with no
# copying at all; shipping the files is then the document's job, done by knit_print()'s metadata.
save_widget_page <- function(x, file, libdir) {
  # The page IS the graph. Without this it keeps htmlwidgets' standalone default of a 960x500 box
  # in a margined body, which inside a frame of another size overflows and breaks the ratio.
  # WARNING: htmlwidgets::resolveSizing() reads the TOP LEVEL of sizingPolicy for the standalone
  # page's own div, and the `browser` scope only for the runtime resizing script. Both have to be
  # set, or the div keeps the 960x500 default.
  x$width <- NULL
  x$height <- NULL
  for (scope in list(NULL, "browser")) {
    sp <- if (is.null(scope)) x$sizingPolicy else x$sizingPolicy[[scope]]
    sp$defaultWidth <- "100%"
    sp$defaultHeight <- "100%"
    sp$padding <- 0
    sp$fill <- TRUE
    if (is.null(scope)) x$sizingPolicy <- sp else x$sizingPolicy[[scope]] <- sp
  }

  rendered <- htmltools::renderTags(htmltools::as.tags(x, standalone = TRUE))
  deps <- lapply(rendered$dependencies, prefix_dependency)

  # "<name>-<version>" is the folder name htmltools and rmarkdown both give a dependency, so the
  # page can point at the shared copy the document ships without either side consulting the other.
  href_deps <- lapply(deps, function(d) {
    d$src <- list(href = paste0(libdir, "/", d$name, "-", d$version))
    d
  })

  writeLines(c(
    "<!DOCTYPE html>", "<html>", "<head>", '<meta charset="utf-8"/>',
    paste0("<title>", htmltools::htmlEscape(tools::file_path_sans_ext(basename(file))),
           "</title>"),
    # A percentage height needs ancestors that have one.
    "<style>html,body{height:100%;margin:0;padding:0;overflow:hidden;background:#fff;}",
    "#htmlwidget_container{height:100%;}</style>",
    widget_guard(),
    as.character(htmltools::renderDependencies(href_deps, srcType = "href")),
    as.character(rendered$head),
    "</head>", "<body>", as.character(rendered$html), "</body>", "</html>"
  ), file, useBytes = TRUE)

  deps
}


# WARNING: rename every dependency, because htmltools resolves a name clash by keeping the HIGHEST
# version and dropping the other. A frame is a separate document loading its own scripts, so it
# must get the exact version it was rendered against: plotly asks for jQuery 3.5.1 while the
# gitbook template ships 3.6.0, and the page would have pointed at a directory that never existed.
# Under a name of its own no widget dependency can be resolved away -- and since the payload has
# left the document, nothing else claims those files, so nothing is shipped twice.
prefix_dependency <- function(d) {
  d$name <- paste0("ggfacto-", d$name)
  d
}


# Build the <iframe>. Its aspect ratio is the graph's own, so the axes keep the scale the
# producer gave them; its width is the column's, so every graph of the document lines up.
widget_iframe <- function(x, src, label) {
  ratio <- attr(x, "ggfacto_ratio", exact = TRUE)

  if (is.null(ratio)) {
    fig_width  <- knitr::opts_current$get("fig.width")
    fig_height <- knitr::opts_current$get("fig.height")
    ratio <- if (!is.null(fig_width) && !is.null(fig_height) && fig_width > 0) {
      fig_height / fig_width
    } else {
      5 / 7   # knitr's default device, which is what dev.size() reports in a bare chunk
    }
  }

  # Only a percentage is honoured. knitr fills out.width in with a pixel count derived from
  # fig.width whether the author asked for one or not, and a fixed pixel width would fight the
  # column it sits in; a percentage is always deliberate.
  width <- knitr::opts_current$get("out.width")
  if (is.null(width) || !grepl("%$", paste0(width))) width <- "100%"

  paste0(
    "<iframe src=\"", src, "\" title=\"", label, "\" loading=\"lazy\" ",
    "style=\"width:", width, "; max-width:100%; aspect-ratio:1/",
    format(ratio, digits = 6), "; border:0; display:block;\"></iframe>"
  )
}


#' Graphs knitted at their own aspect ratio
#'
#' Every graph \pkg{ggfacto} draws with \pkg{ggplot2} (\link{ggfacto}, \link{ggmca}, \link{ggca},
#' \link{ggpca}, \link{ggpca_cor_circle}...) carries the class \code{ggfacto_plot}, and knows the
#' ratio of its axes: an axis twice as long as the other is drawn twice as long, so the cloud keeps
#' the scale it is interpreted with. In a \pkg{knitr} or Quarto document, such a graph is drawn at
#' the chunk's \code{fig.width} and at the height this ratio gives: no \code{fig.height} to compute
#' by hand. Captions (\code{fig.cap}), alignment, \code{out.width} and cross-references work as for
#' any figure.
#'
#' A chunk that sets its own \code{fig.height} or \code{fig.asp} (different from the document's
#' default) keeps it. The class changes nothing else: the object is still a ggplot, to which
#' \pkg{ggplot2} elements can be added with \code{+}, and \link{ggi} still makes it interactive.
#'
#' @name ggfacto_plot
NULL

# DESIGN: the graph writes its own figure file under the chunk's fig.path and hands it back through
#   include_graphics(), so it passes the plot hook like any figure (captions, fig.align, out.width,
#   Quarto cross-references). The device is the chunk's first `dev`.
# WARNING: an explicit size is detected against the DOCUMENT default (opts_chunk), since the chunk's
#   resolved options cannot tell a default from a choice: a chunk asking for its own fig.height or
#   fig.asp keeps it, and so does anything this method cannot honour, through NextMethod().
#   `results = "hide"` is one: knitr drops an include_graphics() there, where it keeps a plot.
knit_print.ggfacto_plot <- function(x, options = knitr::opts_current$get(), ...) {
  ratio <- attr(x, "height_width_ratio", exact = TRUE)
  doc   <- knitr::opts_chunk$get()
  asked <- !identical(options$fig.asp, doc$fig.asp) ||
    (is.null(options$fig.asp) && !identical(options$fig.height, doc$fig.height))
  dev   <- knit_device(options$dev)
  if (is.null(ratio) || asked || is.null(dev) || is.null(options$fig.width) ||
      options$fig.show %in% "hide" || options$results %in% "hide") {
    return(NextMethod())
  }

  file <- knitr::fig_path(paste0(".", dev$ext), options,
                          number = paste0("ggfacto-", knit_counter(options$label)))
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  dev_args <- options$dev.args
  if (!is.list(dev_args) || any(names(dev_args) %in% options$dev)) dev_args <- list()
  do.call(ggplot2::ggsave, c(list(filename = file, plot = x, device = dev$fun,
                                  width = options$fig.width, height = options$fig.width * ratio,
                                  units = "in", dpi = if (is.null(options$dpi)) 72 else options$dpi),
                             dev_args))
  knitr::include_graphics(file)
}

# The devices a knitted graph can be written with, from knitr's `dev` names.
knit_device <- function(dev) {
  dev <- if (is.character(dev) && length(dev) != 0) dev[1] else ""
  switch(dev,
         png = , ragg_png = list(ext = "png", fun = "png"),
         jpeg = list(ext = "jpeg", fun = "jpeg"),
         tiff = list(ext = "tiff", fun = "tiff"),
         svg = , svglite = list(ext = "svg", fun = "svg"),
         pdf = list(ext = "pdf", fun = "pdf"),
         cairo_pdf = list(ext = "pdf", fun = grDevices::cairo_pdf),
         NULL)
}

# A figure number per chunk, so two graphs of one chunk never share a file.
knit_state <- new.env(parent = emptyenv())
knit_counter <- function(label) {
  if (!identical(knit_state$label, label)) {
    knit_state$label <- label
    knit_state$n     <- 0L
  }
  knit_state$n <- knit_state$n + 1L
  knit_state$n
}
