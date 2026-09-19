# PURPOSE: ggmca_plot() -- the one renderer, which draws the plot model of any of the three
#   analyses (R/plot-model.R), built by R/mca-data.R, R/ca.R or R/pca.R.
# ROLE: The rendering half of every 2D graph: levels as text, labels or points, the points of the
#   cloud, the ellipses and facets of a supplementary variable, the central point, the theme.
# KEY CONSTRAINTS:
#   - It never sees a fitted object, only the model's stripped `res` (eigenvalues, axis names).
#   - Rows are drawn by `role`; a row with no colour group (NA) is drawn in neutral ink, through the
#     colour scale's `na.value` -- the MCA's active levels and the central point.
#   - A level's tooltip arrives as a header and a body: the contribution lines of the two axes drawn
#     are this half's, inserted between them, in the model's language (`plot_data$lang`).
#   - Ellipses and facets read `individuals`, not the drawn points: an ellipse covers every
#     individual of its level, and neither needs profiles.
#   - The graph leaves through as_ggfacto_plot() (R/render.R): a real ggplot, its class and its
#     render hints as attributes.
#   - This file must keep sorting AFTER mca-data.R in the C locale: `@describeIn ggmca` names the
#     merged help topic after whichever block roxygen reads first, and that is file order.
# See: CLAUDE.md section ggfacto architecture > How a graph is built.

#' @describeIn ggmca draws a plot model --- the one \code{ggmca_data()} returns, and the one the
#'   correspondence and principal component graphs build internally.
# @inheritParams ggmca
#' @param plot_data A list of data frames made with \link{ggmca_data}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @export
ggmca_plot <- function(plot_data,
                       axes = c(1,2), axes_names = NULL, axes_reverse = NULL,
                       type = c("text", "points", "labels", "active_vars_only", "facets"),
                       text_repel = TRUE, title, ellipses = NULL,
                       actives_in_bold = NULL, sup_in_italic = TRUE,
                       xlim, ylim, out_lims_move = FALSE,
                       color_profiles = TRUE, base_profiles_color = "#aaaaaa",
                       alpha_profiles = 0.7,
                       shift_colors = 0, colornames_recode,
                       scale_color_light = material_colors_light(),
                       scale_color_dark  = material_colors_dark(),
                       text_size = 3.5, size_scale_max = NULL, dist_labels = c("auto", 0.04),
                       right_margin = 0, use_theme = TRUE, get_data = FALSE,
                       data) {
  # `data` was renamed `plot_data` in 0.4.0, freeing `data` for the microdata across the ggmca_*
  # family; it sits last so no positional call can reach it.
  if (!missing(data) && missing(plot_data)) {
    plot_data <- renamed_arg(data, "data", "plot_data", "ggmca_plot")
  }
  types <- c("text", "points", "labels", "active_vars_only", "facets")
  type  <- type[1]
  if (!type %in% types) stop(
    "unknown type of graph: \"", type, "\". Use one of ",
    str_c("\"", types, "\"", collapse = ", "), ".", call. = FALSE)
  if (!is.null(ellipses)) stopifnot(ellipses > 0 & ellipses <= 1)
  if (missing(colornames_recode)) colornames_recode <- character()
  xlim  <- if (missing(xlim))  NULL else xlim
  ylim  <- if (missing(ylim))  NULL else ylim
  title <- if (missing(title)) NULL else title

  with_gda_lang(plot_data$lang, function(lg) {
  vars_data   <- plot_data$vars_data
  ind_data    <- plot_data$ind_data
  individuals <- plot_data$individuals
  clust       <- plot_data$clust
  res         <- plot_data$res
  if (!is.null(axes_names)) res$axes_names <- axes_names
  if (type == "active_vars_only") {
    vars_data <- vars_data[vars_data$role %in% c("active", "central"), ]
    type      <- "text"
  }
  sup_vars <- unique(vars_data$vars[vars_data$role %in% c("sup", "clust")])
  if (is.null(actives_in_bold)) actives_in_bold <- length(sup_vars) == 0

  d1   <- paste("Dim", axes[1])
  d2   <- paste("Dim", axes[2])
  dim1 <- rlang::sym(d1)
  dim2 <- rlang::sym(d2)

  if (length(axes_reverse) != 0) {
    if (!all(axes_reverse %in% 1:2)) stop("axes_reverse must be 1, 2 or 1:2")
    flip <- function(x) {
      if (is.null(x)) return(x)
      dplyr::mutate(x, dplyr::across(tidyselect::all_of(unique(c(d1, d2)[axes_reverse])), ~ -.))
    }
    vars_data   <- flip(vars_data)
    ind_data    <- flip(ind_data)
    individuals <- flip(individuals)
  }

  # A PCA's active variables as vectors: on the circle of correlations, or rescaled onto the cloud
  # of individuals in a biplot -- the circle then holding the cloud's 90 % closest points.
  vec    <- plot_data$vectors
  vd     <- NULL
  radius <- NULL
  if (!is.null(vec)) {
    radius <- if (is.null(vec$cloud)) vec$radius else {
      stats::quantile(sqrt(rowSums(vec$cloud[, c(d1, d2), drop = FALSE]^2)), 0.9, names = FALSE)
    }
    vd <- dplyr::mutate(vec$data, dplyr::across(tidyselect::starts_with("Dim "), ~ . * radius))
    if (length(axes_reverse) != 0) vd <- flip(vd)
  }

  # The tooltip of a level: its header, the contributions to the two axes drawn, then its body.
  c1 <- vars_data[[str_c("contrib", axes[1])]]
  c2 <- vars_data[[str_c("contrib", axes[2])]]
  contrib_text <- if (is.null(c1) || is.null(c2)) "" else dplyr::if_else(
    is.na(c1), "",
    str_c("\n", gettextf("Contrib axe %s: %s%%", axes[1], str_pad(round(c1, 0), 2)),
          "\n", gettextf("Contrib axe %s: %s%%", axes[2], str_pad(round(c2, 0), 2))))
  vars_data$interactive_text <-
    tidyr::unite(tibble::tibble(head = str_c(vars_data$begin_text, contrib_text),
                                body = vars_data$interactive_text),
                 "text", tidyselect::everything(), sep = "\n", na.rm = TRUE)$text
  vars_data$interactive_text <- ggiraph_text(vars_data$interactive_text)
  vars_data <- dplyr::select(vars_data, -"begin_text", -tidyselect::starts_with("contrib"))

  # Colours: one key per colour group, the levels of the first sup var each their own under
  # ellipses and facets; the points of the cloud keep their cluster's key.
  if (type == "facets" || !is.null(ellipses)) {
    first_sup <- vars_data$vars %in% sup_vars[1]
    vars_data$color_group <- forcats::as_factor(ifelse(
      first_sup, paste0(vars_data$color_group, "_", vars_data$lvs),
      as.character(vars_data$color_group)))
  }
  nv  <- nrow(vars_data)
  key <- forcats::as_factor(c(as.character(vars_data$color_group),
                              if (!is.null(ind_data)) as.character(ind_data$color_group)))
  if (length(colornames_recode) != 0) key <- forcats::fct_recode(key, !!!colornames_recode)
  if (shift_colors != 0) key <- forcats::fct_shift(key, shift_colors)
  vars_data$color_group <- key[seq_len(nv)]
  if (!is.null(ind_data)) ind_data$color_group <- as.character(key[-seq_len(nv)])
  keys <- levels(forcats::fct_drop(key))
  if (length(keys) >= 2 && isTRUE(getOption("ggfacto.verbose"))) message(
    "colors based on the following categories (rename with colornames_recode): '",
    str_c(keys, collapse = "', '"), "'")
  palette <- colour_palette(keys, scale_color_light, scale_color_dark, base_profiles_color)

  # The geometry: the ratio of the axes drawn, over every point the panel holds.
  facet_data <- NULL
  if (type == "facets" || !is.null(ellipses)) {
    sup1 <- ellipse_individuals(vars_data, individuals, sup_vars[1], type)
    if (type == "facets") facet_data <- facet_points(sup1, sup_vars[1])
  }
  circle <- if (!is.null(radius)) c(-radius, radius)
  ratio <- plot_ratio(c(vars_data[[d1]], ind_data[[d1]], facet_data[[d1]], vd[[d1]], circle),
                      c(vars_data[[d2]], ind_data[[d2]], facet_data[[d2]], vd[[d2]], circle),
                      xlim, ylim)
  if (type == "facets") {
    layout <- ggplot2::wrap_dims(length(unique(facet_data$lvs)))
    ratio  <- ratio * layout[1] / layout[2]
  }
  width_range <- diff(range(c(xlim, vars_data[[d1]], ind_data[[d1]], circle), na.rm = TRUE))
  if (dist_labels[1] == "auto") dist_labels <- width_range / 40
  dist_labels <- as.numeric(dist_labels[1])

  # The size scale: the drawn points' weights say how large the largest must be.
  size_w <- if (type == "points") {
    vars_data$wcount[!is.na(vars_data$color_group) & vars_data$role != "central"]
  } else if (type == "facets") {
    facet_data$wcount
  } else {
    ind_data$wcount
  }
  if (is.null(size_scale_max)) size_scale_max <- auto_size_max(size_w)

  # Levels outside the limits are dropped, or moved to the edges with out_lims_move; points of the
  # cloud are always dropped.
  vars_data <- limit_rows(vars_data, d1, xlim, out_lims_move)
  vars_data <- limit_rows(vars_data, d2, ylim, out_lims_move)

  # The points of the cloud: an answer profile of an MCA, an individual of a PCA.
  profiles <- NULL
  if (!is.null(ind_data)) {
    pc <- limit_rows(limit_rows(ind_data, d1, xlim), d2, ylim)
    cl <- if ("clust" %in% names(pc)) as.character(pc$clust) else rep(NA_character_, nrow(pc))
    coloured <- !is.na(pc$color_group) &
      (if (isTRUE(color_profiles)) TRUE else cl %in% color_profiles)
    pc$color_group <- ifelse(coloured, pc$color_group, "base_profiles_color")
    # a point's coordinates on the two axes drawn, after its counts
    coords <- paste0(gettextf("Coord axe %s: %s", axes[1], format(round(pc[[d1]], 2), nsmall = 2)),
                     "\n",
                     gettextf("Coord axe %s: %s", axes[2], format(round(pc[[d2]], 2), nsmall = 2)))
    pc$interactive_text <- ggiraph_text(ifelse(
      grepl("\n\n", pc$interactive_text, fixed = TRUE),
      str_replace(pc$interactive_text, "\n\n", paste0("\n", coords, "\n\n")),
      paste0(pc$interactive_text, "\n", coords)))
    profiles <- ggiraph::geom_point_interactive(
      data = pc,
      ggplot2::aes(x = !!dim1, y = !!dim2, size = .data$wcount, colour = .data$color_group,
                   tooltip = .data$interactive_text, data_id = .data$id),
      na.rm = TRUE, inherit.aes = FALSE, show.legend = FALSE, alpha = alpha_profiles,
      stroke = if (length(clust) != 0) 0 else 0.5
    )
  }

  # Ellipses: the individuals of each level of the first supplementary variable.
  ellipses_layer <- NULL
  if (!is.null(ellipses) && !is.null(sup1)) {
    # DESIGN: stat_ellipse()'s robust t-ellipse (MASS::cov.trob), weighted by the survey weights
    #   since ggplot2 4.0.0: an unweighted analysis draws the same ellipse as before.
    ellipses_coord <- dplyr::select(sup1, !!dim1, !!dim2, "row.w", "lvs", "color_group", "id")
    ellipses_layer <- if (type == "facets") {
      ggiraph::geom_path_interactive(
        data = ellipses_coord,
        ggplot2::aes(x = !!dim1, y = !!dim2, group = .data$lvs, data_id = .data$id,
                     weight = .data$row.w),
        color = "black", stat = "ellipse", type = "t", level = ellipses, linewidth = 1,
        segments = 360, alpha = 1, inherit.aes = FALSE)
    } else {
      ggplot2::geom_path(
        data = ellipses_coord,
        ggplot2::aes(x = !!dim1, y = !!dim2, group = .data$lvs, color = .data$color_group,
                     weight = .data$row.w),
        stat = "ellipse", type = "t", level = ellipses, linewidth = 1, segments = 360,
        alpha = 1, inherit.aes = FALSE)
    }
  }

  # DESIGN: italics mark a supplementary level in every analysis, colour meaning something else in
  #   each; clusters are not supplementary variables, and stay upright.
  vars_data$face <- dplyr::case_when(
    vars_data$role == "active" & actives_in_bold ~ "bold",
    vars_data$role == "active"                   ~ "plain",
    vars_data$role == "sup" & sup_in_italic & actives_in_bold ~ "italic",
    vars_data$role == "sup" & sup_in_italic                   ~ "bold.italic",
    actives_in_bold                              ~ "plain",
    TRUE                                         ~ "bold"
  )

  mean_point_data  <- vars_data[vars_data$role == "central", ]
  vars_data        <- vars_data[vars_data$role != "central", ]
  mean_point_graph <- ggiraph::geom_point_interactive(
    data = mean_point_data,
    ggplot2::aes(x = !!dim1, y = !!dim2, tooltip = .data$interactive_text),
    color = "black", fill = "#eeeeee", shape = 3, size = 5, stroke = 1.5,
    na.rm = TRUE, inherit.aes = FALSE
  )

  graph_theme_acm <- list(
    if (use_theme) theme_facto(res, axes = axes, no_color_scale = TRUE,
                               size_scale_max = size_scale_max, xlim = xlim, ylim = ylim),
    # the palette's own names as limits: a level with no colour group (NA) is drawn in black, and
    # a graph with none at all raises no "no shared levels" warning
    ggplot2::scale_colour_manual(values = palette, limits = names(palette),
                                 aesthetics = c("colour", "fill"), na.value = "black"),
    ggplot2::theme(plot.margin = ggplot2::margin(r = right_margin, unit = "cm")),
    if (!is.null(title)) ggplot2::labs(title = title),
    if (!is.null(vec)) vector_frame(biplot = !is.null(vec$cloud), base_profiles_color)
  )

  if (get_data) return(list(
    vars_data = vars_data, mean_point_data = mean_point_data,
    profiles_coord = if (!is.null(profiles)) pc,
    ellipses_coord = if (!is.null(ellipses_layer)) ellipses_coord,
    vectors_coord = vd, graph_theme_acm = graph_theme_acm
  ))
  vl <- if (!is.null(vec)) vector_layers(vd, radius, dim1, dim2, isTRUE(vec$proj), text_size,
                                         biplot = !is.null(vec$cloud))

  labels <- function(d, label = FALSE, ...) {
    level_labels(d, dim1, dim2, label = label, repel = text_repel, text_size = text_size, ...)
  }
  neutral  <- vars_data[is.na(vars_data$color_group) & vars_data$role != "clust", ]
  coloured <- vars_data[!is.na(vars_data$color_group) & vars_data$role != "clust", ]
  clusters <- vars_data[vars_data$role == "clust", ]
  clust_labels <- if (nrow(clusters) != 0) {
    labels(clusters, label = TRUE, fill = grDevices::rgb(1, 1, 1, alpha = 0.9))
  }

  css_hover <- NULL
  plot_output <- if (type == "text") {
    ggplot2::ggplot() + graph_theme_acm + vl$circle + profiles + ellipses_layer + vl$arrows +
      labels(vars_data[vars_data$role != "clust", ]) + clust_labels + vl$labels +
      mean_point_graph

  } else if (type == "points") {
    names_data <- coloured
    names_data$color_group <- paste0("names_", names_data$color_group)
    ggplot2::ggplot() + graph_theme_acm + vl$circle + profiles + ellipses_layer + vl$arrows +
      labels(neutral, colour = "black", alpha = 0.8) +
      (if (nrow(coloured) != 0) list(
        ggiraph::geom_text_repel_interactive(
          data = names_data,
          ggplot2::aes(x = !!dim1, y = !!dim2, label = .data$lvs, colour = .data$color_group,
                       tooltip = .data$interactive_text, data_id = .data$id),
          size = text_size, hjust = "left", segment.alpha = 0.2, direction = "both",
          nudge_x = dist_labels, point.padding = 0.25, na.rm = TRUE, fontface = "plain",
          inherit.aes = FALSE),
        ggiraph::geom_point_interactive(
          data = coloured,
          ggplot2::aes(x = !!dim1, y = !!dim2, size = .data$wcount, colour = .data$color_group,
                       tooltip = .data$interactive_text, data_id = .data$id),
          shape = 18, na.rm = TRUE, inherit.aes = FALSE)
      )) +
      clust_labels + vl$labels + mean_point_graph

  } else if (type == "labels") {
    ggplot2::ggplot() + graph_theme_acm + vl$circle + profiles + ellipses_layer + vl$arrows +
      labels(neutral, colour = "black") +
      (if (nrow(coloured) != 0) labels(coloured, label = TRUE)) +
      clust_labels + vl$labels + mean_point_graph

  } else {
    # facets: the points of each level of the first sup var, and that level's mark
    css_hover <- ggiraph::girafe_css("stroke:orange;stroke-width:2;",
                                     text = "color:gold4;stroke:none;")
    ggplot2::ggplot(data = facet_data,
                    ggplot2::aes(x = !!dim1, y = !!dim2, size = .data$wcount,
                                 color = .data$color_group, group = .data$lvs)) +
      ggplot2::geom_point(na.rm = TRUE, show.legend = FALSE) +
      ggiraph::geom_point_interactive(
        data = vars_data[vars_data$vars %in% sup_vars[1], ],
        ggplot2::aes(x = !!dim1, y = !!dim2, group = .data$lvs,
                     data_id = .data$id, tooltip = .data$interactive_text),
        color = "black", shape = 17, size = 0, stroke = 10,
        inherit.aes = FALSE, na.rm = TRUE, show.legend = FALSE
      ) +
      ggplot2::facet_wrap(ggplot2::vars(.data$lvs), scales = "fixed") +
      graph_theme_acm + ellipses_layer + vl$circle + vl$arrows + vl$labels
  }

  as_ggfacto_plot(plot_output, ratio, css_hover)
  })
}


# The vectors of a PCA's variables: the circle, the arrows and their labels, and each arrow's
# projections on the two axes, drawn with `proj`, else transparent and shown while the arrow is
# hovered (ggi(), reveal_on_hover()). In a biplot, the circle carries its own -1 / 1 graduations,
# inside it, with inward ticks: the axes' values are the individuals'.
#' @keywords internal
#' @noRd
vector_layers <- function(vd, radius, dim1, dim2, proj, text_size, biplot = FALSE) {
  ink  <- "#34515e"
  x    <- vd[[rlang::as_name(dim1)]]
  y    <- vd[[rlang::as_name(dim2)]]
  turn <- seq(0, 2 * pi, length.out = 361)
  circle <- data.frame(x = radius * cos(turn), y = radius * sin(turn))
  projections <- data.frame(
    id = paste0("reveal-", rep(vd$id, 2)), x0 = rep(x, 2), y0 = rep(y, 2),
    x = c(x, rep(0, length(x))), y = c(rep(0, length(y)), y),
    label = c(paste0("x=", round(x / radius, 2)), paste0("y=", round(y / radius, 2))))
  shown <- if (proj) 1 else 0
  # drawn for good, projections are plain elements; otherwise ggi() reveals them at hover
  reveal <- if (proj) NULL else quote(.data$id)
  tips <- vd
  tips[[rlang::as_name(dim1)]] <- x + sign(x) * 0.03 * radius
  tick <- 0.035 * radius
  # DESIGN: each tick stands just aside its axis line, radial and starting on the circle, with its
  #   value on the same side: an angle of asin(side / radius) off the axis.
  side  <- 0.012 * radius
  angle <- c(0, pi, pi / 2, -pi / 2) + c(1, -1, -1, 1) * asin(side / radius)
  graduations <- if (biplot) data.frame(
    x = radius * cos(angle), y = radius * sin(angle),
    xend = (radius - tick) * cos(angle), yend = (radius - tick) * sin(angle),
    lx = c(radius - 0.6 * tick, -radius + 0.6 * tick, 0.85 * tick, 0.85 * tick),
    ly = c(0.85 * tick, 0.85 * tick, radius - 0.6 * tick, -radius + 0.6 * tick),
    label = c("1", "-1", "1", "-1"), hjust = c(1, 0, 0, 0), vjust = c(0, 0, 1, 0))
  list(
    circle = list(
      ggplot2::geom_path(data = circle, ggplot2::aes(x = .data$x, y = .data$y), colour = ink,
                         linewidth = 0.5, alpha = 0.8, inherit.aes = FALSE),
      if (biplot) list(
        ggplot2::geom_segment(data = graduations,
                              ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend,
                                           yend = .data$yend),
                              colour = ink, linewidth = 1.2, inherit.aes = FALSE),
        ggplot2::geom_text(data = graduations,
                           ggplot2::aes(x = .data$lx, y = .data$ly, label = .data$label,
                                        hjust = .data$hjust, vjust = .data$vjust),
                           colour = ink, fontface = "bold", size = text_size * 0.96,
                           inherit.aes = FALSE))
    ),
    arrows = list(
      # dashed like the main axes, and carrying no hover id of their own: see reveal_on_hover()
      ggiraph::geom_segment_interactive(
        data = projections,
        ggplot2::aes(x = .data$x0, y = .data$y0, xend = .data$x, yend = .data$y,
                     data_id = !!reveal),
        colour = "black", alpha = shown, linewidth = 0.5, linetype = "dashed",
        inherit.aes = FALSE),
      ggiraph::geom_label_repel_interactive(
        data = projections,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$label, data_id = !!reveal),
        colour = grDevices::adjustcolor(ink, alpha.f = shown),
        fill = grDevices::adjustcolor("white", alpha.f = shown), linewidth = 0,
        fontface = "bold", size = text_size * 0.85, na.rm = TRUE, inherit.aes = FALSE),
      # an arrow lights up with its name, and shows its tooltip too
      ggiraph::geom_segment_interactive(
        data = vd, ggplot2::aes(x = 0, y = 0, xend = !!dim1, yend = !!dim2,
                                tooltip = .data$interactive_text, data_id = .data$id),
        colour = ink, linewidth = 0.8, inherit.aes = FALSE,
        arrow = ggplot2::arrow(length = ggplot2::unit(0.22, "cm")))
    ),
    labels = ggiraph::geom_label_repel_interactive(
      data = tips,
      ggplot2::aes(x = !!dim1, y = !!dim2, label = .data$name, tooltip = .data$interactive_text,
                   data_id = .data$id),
      colour = ink, fill = grDevices::rgb(1, 1, 1, alpha = 0.7), linewidth = 0,
      fontface = "bold", size = text_size, hjust = "outward", direction = "y", force = 0.5,
      force_pull = 1, point.padding = 0, box.padding = 0, point.size = NA,
      min.segment.length = 0.1, na.rm = TRUE, inherit.aes = FALSE)
  )
}

# The frame of a graph of vectors: on the circle of correlations, a light grid to read them on; in a
# biplot, the axes' values in the colour of the individuals, whose scale they are -- the circle
# carrying the correlations' own.
#' @keywords internal
#' @noRd
vector_frame <- function(biplot, base_profiles_color = "#aaaaaa") {
  if (biplot) return(ggplot2::theme(
    axis.text = ggplot2::element_text(colour = if (is.null(base_profiles_color)) "grey50" else
      base_profiles_color)))
  list(ggplot2::scale_x_continuous(minor_breaks = seq(-1, 1, by = 0.1)),
       ggplot2::scale_y_continuous(minor_breaks = seq(-1, 1, by = 0.1)),
       ggplot2::theme(panel.grid.major = ggplot2::element_line(linewidth = 0.3, colour = "grey85"),
                      panel.grid.minor = ggplot2::element_line(linewidth = 0.2, colour = "grey93")))
}

# The text or label layer of some levels, repelled or at their points, their colour from their
# group unless a fixed one is given.
#' @keywords internal
#' @noRd
level_labels <- function(d, dim1, dim2, label, repel, text_size, colour = NULL, ...) {
  mapping <- ggplot2::aes(x = !!dim1, y = !!dim2, label = .data$lvs, colour = .data$color_group,
                          fontface = .data$face, tooltip = .data$interactive_text,
                          data_id = .data$id)
  if (!is.null(colour)) mapping$colour <- NULL
  if (label) d$face <- ifelse(grepl("italic", d$face), "bold.italic", "bold")
  geom <- if (label) {
    if (repel) ggiraph::geom_label_repel_interactive else ggiraph::geom_label_interactive
  } else {
    if (repel) ggiraph::geom_text_repel_interactive  else ggiraph::geom_text_interactive
  }
  args <- c(list(data = d, mapping = mapping, size = text_size, na.rm = TRUE,
                 inherit.aes = FALSE),
            if (!is.null(colour)) list(colour = colour),
            if (repel) list(direction = "both", force = 0.5, force_pull = 1, point.padding = 0,
                            box.padding = 0, point.size = NA, min.segment.length = 0.01,
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines"))),
            list(...))
  do.call(geom, args)
}

# WARNING: ggiraph turns "\n" into <br/> only when a tooltip does not both start and end with an
#   HTML tag -- such a text it takes for raw HTML, and its lines collapse into one. A tooltip ending
#   on a tag (a coloured last cell) gets a trailing space, so its lines are always kept.
#' @keywords internal
#' @noRd
ggiraph_text <- function(x) ifelse(grepl(">\\s*$", x), paste0(x, unbrk), x)

# The manual palette: each key its light colour for points, its dark one for names, and the base
# colour of the points of the cloud. A key beyond the palette takes its last colour.
#' @keywords internal
#' @noRd
colour_palette <- function(keys, light, dark, base) {
  if (length(light) == 1) light <- rep(light, max(1L, length(keys)))
  if (length(dark)  == 1) dark  <- rep(dark,  max(1L, length(keys)))
  k <- min(length(light), length(dark))
  if (length(keys) > k) warning(
    "too much colors, all the last ones were set to last color. Max ", k, call. = FALSE)
  at <- pmin(seq_along(keys), k)
  c(stats::setNames(unname(light[at]), keys),
    stats::setNames(unname(dark[at]), str_c("names_", keys)),
    base_profiles_color = if (is.null(base)) "#ffffff" else base)
}

# The height / width ratio of a panel holding `x` and `y`, a limit given overriding the data.
#' @keywords internal
#' @noRd
plot_ratio <- function(x, y, xlim = NULL, ylim = NULL) {
  span <- function(v, lim) {
    r <- range(v, na.rm = TRUE)
    if (length(lim) == 2) r[!is.na(lim)] <- lim[!is.na(lim)]
    diff(r)
  }
  span(y, ylim) / span(x, xlim)
}

# DESIGN: the largest point a graph draws is sized so that its median one stays visible -- the
#   weights of answer profiles are very skewed, one holding a tenth of the population where most
#   hold one individual. Calibrated on the course's data: tea keeps 4, pc_AGD gets 14 where the
#   course chose 12 by hand. Equal weights (an unweighted PCA) are all drawn small.
#' @keywords internal
#' @noRd
auto_size_max <- function(w) {
  w <- w[is.finite(w) & w > 0]
  if (length(w) == 0) return(4)
  if (max(w) - min(w) <= 1e-9 * max(w)) return(1.5)
  min(max(sqrt(max(w) / stats::median(w)), 4), 20)
}

# The rows inside a limit: the others dropped, or moved to its edge.
#' @keywords internal
#' @noRd
limit_rows <- function(d, dim, lim, move = FALSE) {
  if (is.null(d) || length(lim) != 2) return(d)
  v <- d[[dim]]
  if (move) {
    if (!is.na(lim[1])) v <- pmax(v, lim[1])
    if (!is.na(lim[2])) v <- pmin(v, lim[2])
    d[[dim]] <- v
    return(d)
  }
  d[((is.na(lim[1]) | v > lim[1]) & (is.na(lim[2]) | v < lim[2])) %in% TRUE, ]
}

# The individuals of each level of the first supplementary variable, with that level's name,
# colour and hover id: what the ellipses and the facets are drawn from.
#' @keywords internal
#' @noRd
ellipse_individuals <- function(vars_data, individuals, sup1, type) {
  if (is.null(individuals) || length(sup1) == 0 || is.na(sup1)) {
    if (type == "facets") stop("type = \"facets\" needs a supplementary variable (sup_vars)",
                               call. = FALSE)
    warning("ellipses need a supplementary variable (sup_vars): none drawn", call. = FALSE)
    return(NULL)
  }
  levels_1 <- vars_data[vars_data$vars %in% sup1, ]
  lv <- match(as.character(individuals[[sup1]]), as.character(levels_1$lvs))
  out <- individuals
  out$lvs         <- levels_1$lvs[lv]
  out$color_group <- levels_1$color_group[lv]
  out$id          <- levels_1$id[lv]
  out[!is.na(lv), ]
}

# One point per drawn point of the cloud within each level of the first supplementary variable,
# sized by its count there. Individuals are ordered by point first, so points tied on weight keep
# the rank order of the cloud.
#' @keywords internal
#' @noRd
facet_points <- function(sup1_individuals, sup1) {
  drawn <- sup1_individuals[order(sup1_individuals$nb), ]
  drawn <- drawn[!is.na(drawn$nb), ]
  group <- vctrs::vec_group_id(drawn[c("nb", "lvs")])
  out <- drawn[!duplicated(group), ]
  out$count  <- tabulate(group)
  out$wcount <- as.vector(rowsum(drawn$row.w, group, reorder = TRUE))
  out <- out[order(out[[sup1]], -out$wcount), ]
  out$lvs <- as.character(out$lvs)
  out
}
