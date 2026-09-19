# PURPOSE: The PCA entry point and its 2D graphs -- principal_component_analysis() (alias PCA2()),
#   ggpca_cor_circle(), ggpca() and its builder pca_plot_data(), the shared projector
#   PCA_ind.sup_coord(), and the deprecated mean_sd_tab().
# ROLE: principal_component_analysis() is the tidyselect ingress normaliser (row and column weights,
#   supplementary and named individuals, and the source rows R/ingress.R records).
#   ggpca_cor_circle() draws the variables; ggpca() draws the individuals through the shared plot
#   model (R/plot-model.R) and the one renderer. PCA_ind.sup_coord() projects arbitrary raw rows
#   into principal space and is shared with R/pca-3d.R.
# KEY CONSTRAINTS:
#   - Both graphs return a ggplot; ggpca_cor_circle(interactive = TRUE) returns ggi()'s widget.
#   - The individuals are the points of ggpca()'s cloud, distinct ones merged (pca_model(),
#     R/model.R); a supplementary level is the plain weighted barycentre of its individuals,
#     FactoMineR's quali.sup point, never divided by sqrt(eigenvalue) as in an MCA.
#   - A tooltip mean is coloured by its standardized difference from its block's mean, the colour
#     clust_tab() gives a PCA's clusters: the reader meets one ladder for one quantity.
#   - The circle is one geom_path() over 361 points, deliberately not ggforce::geom_circle(),
#     which re-evaluated its aes once per row of the plot data.
# See: CLAUDE.md section ggfacto architecture > The plot model.

#' Principal Component Analysis
#' @description A user-friendly wrapper around \code{\link[FactoMineR]{PCA}}, made to
#'  work with \pkg{ggfacto} functions like \code{\link{ggpca_cor_circle}},
#'  \code{\link{interpret}} and \code{\link{hierarchical_clust}}. Variables are selected the
#'  way of the `tidyverse`, as in \code{tabxplor::tab()}. `PCA2()` is a shorter name for the same
#'  function.
#' @param data The data frame. To analyse a subset of the population, filter it inside the call with
#'  the native pipe, `data |> dplyr::filter(...) |> principal_component_analysis(...)`: the analysis
#'  then remembers which rows it used, for \code{\link{hierarchical_clust}}.
#' @param active_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> The names
#'  of the active variables.
#' @param wt <\link[tidyr:tidyr_tidy_select]{tidy-select}> The weight variable, if any.
#' @param col.w The weights of the columns, as a numeric vector of the same
#' length than `active_vars.`
#' @param ind_name <\link[tidyr:tidyr_tidy_select]{tidy-select}> Possibly, the variable holding the
#'  names of the individuals.
#' @param scale.unit A boolean, if `TRUE` (value set by default) then data are
#' scaled to unit variance.
#' @param ind.sup A vector indicating the indexes of the supplementary individuals.
#' @param ncp Number of dimensions kept in the results. All of them by default: the eigenvalue
#'   table is how one chooses how many axes to interpret, and a truncated one cannot show the drop.
#'   To cluster on the first axes, give \code{\link{hierarchical_clust}} its own `ncp`.
#' @param graph A boolean, set to `TRUE` to display the base graph.
#' @param ... Additional arguments to pass to \code{\link[FactoMineR]{PCA}}.
#'
#' @return A `PCA` object from \pkg{FactoMineR}, with one more element, `source`, which records the
#'  rows of `data` that were analysed.
#' @export
#'
#' @examples
#' active_vars <- c("mpg", "cyl", "hp", "drat", "qsec")
#' res.pca <- principal_component_analysis(mtcars, tidyselect::all_of(active_vars))
#' interpret(res.pca)
principal_component_analysis <- function(data, active_vars, wt, col.w = NULL, ind_name,
                                         scale.unit = TRUE, ind.sup = NULL, ncp = Inf,
                                         graph = FALSE, ...) {
  # WARNING: the caller's frame is captured HERE, before any promise is forced: rlang::caller_env()
  #   evaluated lazily inside source_rows() would name the wrong frame.
  expr   <- rlang::enexpr(data)
  env    <- rlang::caller_env()
  wt     <- select_vars(rlang::enquo(wt), data)
  stopifnot(length(wt) <= 1)
  source <- source_rows(expr, env, data, wt = if (length(wt) != 0) wt)
  active_vars <- names(tidyselect::eval_select(rlang::enquo(active_vars), data))

  stopifnot(is.integer(ind.sup) | is.null(ind.sup))

  wt   <- if (length(wt) != 0) { data[[wt]] } else {NULL}
  w    <- usable_weights(data, wt, source, keep = ind.sup)
  data <- w$data; wt <- w$wt; source <- w$source
  if (length(ind.sup) > 0) ind.sup <- match(ind.sup, w$kept)

  ind_name <- select_vars(rlang::enquo(ind_name), data)
  data <- as.data.frame(data)
  if (length(ind_name) != 0) rownames(data) <- as.character(data[[ind_name]])

  vars_not_num <- purrr::map_lgl(data[active_vars], ~ !is.numeric(.))
  if (any(vars_not_num)) stop(
    paste0("some active variables are not numeric: ",
           paste0(names(vars_not_num)[vars_not_num], collapse = ", ")
    )
  )

  data <- data[active_vars]

  if (length(ind.sup) > 0) wt <- wt[-ind.sup]

  res <- FactoMineR::PCA(data,
                         scale.unit = scale.unit,
                         ncp = ncp,
                         row.w = wt,
                         graph = graph,
                         ind.sup = ind.sup,
                         col.w = col.w,
                         ...)
  res$source <- source
  res
}

#' @rdname principal_component_analysis
#' @export
PCA2 <- principal_component_analysis

#' Correlation Circle Plot for Principal Component Analysis
#'
#' @param res.pca An analysis made with \code{\link{principal_component_analysis}} or
#' \code{\link[FactoMineR:PCA]{FactoMineR::PCA}}.
#' @param axes The axes to print, as a numeric vector of length 2.
#' @param proj Set to `TRUE` to print projections of vectors over the two axes.
#' @param interactive Set to `TRUE` to get the interactive graph at once, as \code{\link{ggi}}
#' would make it: hovering a variable shows its coordinates on the axes, and its projections.
#' By default, a \code{\link[ggplot2]{ggplot}}, to which elements can be added with `+` before
#' passing it to \code{\link{ggi}}.
#' @param text_size Size of the texte.
#' @param lang \code{NULL} (the session's language), \code{"en"} or \code{"fr"}.
#'
#' @return A \code{\link[ggplot2]{ggplot}}, or an html widget with `interactive = TRUE`.
#' @export
#'
#' @examples
#' data(mtcars, package = "datasets")
#' mtcars <- mtcars[1:7] |> dplyr::rename(weight = wt)
#' res.pca <- principal_component_analysis(mtcars, 1:7)
#' ggpca_cor_circle(res.pca)
#' ggpca_cor_circle(res.pca) |> ggi()  # interactive
ggpca_cor_circle <- function(res.pca, axes = c(1, 2),
                             proj = FALSE, interactive = FALSE, text_size = 3, lang = NULL) {
  p <- with_gda_lang(lang, function(lg) {
  dim1 <- rlang::sym(paste0("Dim.", axes[1]))
  dim2 <- rlang::sym(paste0("Dim.", axes[2]))

  data_circle <- res.pca$var$coord |> as.data.frame() |> tibble::rownames_to_column("name") |>
    tibble::as_tibble() |> dplyr::mutate(id = as.integer(as.factor(.data$name)))

  coord_line <- function(v, axis) {
    gettextf("Coord axe %s: %s (cor. %s%%)", axis,
             str_pad(format(round(v, 2), nsmall = 2), width = 5, side = "left"),
             str_pad(round(v * 100, 0), width = 3, side = "left")) |>
      str_replace_all(" ", unbrk)
  }
  lines <- purrr::imap(dplyr::select(data_circle, tidyselect::starts_with("Dim.")),
                       \(v, col) coord_line(v, sub("^Dim\\.", "", col)))
  data_circle$interactive_text <- paste0("<b>", data_circle$name, "</b>\n",
                                         do.call(paste, c(lines, sep = "\n")))

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

  # DESIGN: the projections are drawn when asked for; otherwise they are drawn invisible, and
  #   appear at mouse hover once the graph passes through ggi().
  plot_proj <- if (proj) {
    list(
      ggplot2::geom_segment(
        ggplot2::aes(xend = .data$Proj1, yend = .data$Proj2),
        data = data_proj, linewidth = 0.5, linetype = "dashed"
      ),
      ggrepel::geom_label_repel(
        ggplot2::aes(x = .data$Proj1, y = .data$Proj2, label = .data$name),
        data = data_proj,
        fill = grDevices::rgb(1, 1, 1, alpha = 0.5), label.size = 0.05, fontface = "bold",
        size = text_size, nudge_y = -0.075, na.rm = TRUE)
    )
  } else {
    list(
      ggiraph::geom_segment_interactive(
        ggplot2::aes(xend = .data$Proj1, yend = .data$Proj2, data_id = .data$id),
        data = data_proj, linewidth = 0.5, linetype = "dashed", color = NA
      ),
      ggiraph::geom_label_repel_interactive(
        ggplot2::aes(x = .data$Proj1, y = .data$Proj2, label = .data$name,
                     data_id = .data$id),
        data = data_proj, label.size = 0.05, fontface = "bold", size = text_size,
        nudge_y = -0.075, na.rm = TRUE, color = NA, fill = NA)
    )
  }

  data_circle |>
    ggplot2::ggplot(ggplot2::aes(x = !!dim1, y = !!dim2)) +
    ggplot2::geom_path(
      data = data.frame(angle = seq(0, 2 * pi, length.out = 361)) |>
        dplyr::mutate(x = cos(.data$angle), y = sin(.data$angle)),
      mapping = ggplot2::aes(x = .data$x, y = .data$y), inherit.aes = FALSE,
      color = "#d32f2f", linewidth = 1) +
    ggplot2::geom_hline(yintercept = 0, color="#d32f2f", linetype = "solid") +
    ggplot2::geom_vline(xintercept = 0, color="#d32f2f", linetype = "solid") +
    ggplot2::labs(x = axis_title(res.pca, axes[1]), y = axis_title(res.pca, axes[2])) +
    ggplot2::coord_fixed() +
    ggplot2::scale_x_continuous(minor_breaks = seq(-1, 1, by = 0.1)) +
    ggplot2::scale_y_continuous(minor_breaks = seq(-1, 1, by = 0.1)) +
    ggplot2::theme_minimal()  +
    ggplot2::theme(legend.position = "none",
                   panel.grid.minor = ggplot2::element_line(linewidth = 0.3, color="gray80"),
                   panel.grid.major = ggplot2::element_line(linewidth = 0.3, color="gray60"),
                   strip.text = ggplot2::element_text(face = "bold"),
                   plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 12, hjust = 1),
                   axis.title.y = ggplot2::element_text(size = 12, hjust = 1),
                   text = ggplot2::element_text(family = "sans")
    ) +
    plot_proj +
    ggplot2::geom_segment(
      ggplot2::aes(xend = !!dim1, yend = !!dim2),
      x = 0, y = 0, color = "#0077c2",
      arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "cm")), linewidth = 1
    ) +
    ggiraph::geom_label_repel_interactive(
      ggplot2::aes(x = !!dim1, y = !!dim2, label = .data$name,
                   tooltip = .data$interactive_text, data_id = .data$id),
      data = data_circle |>
        dplyr::mutate(!!dim1 := dplyr::if_else(!!dim1 > 0, !!dim1 + 0.03, !!dim1 - 0.03)),
      fill = grDevices::rgb(1, 1, 1, alpha = 0.5), label.size = 0, size = text_size,
      color = "#0077c2", fontface = "bold", hjust = "outward",
      direction = "y", force = 0.5, force_pull = 1, point.padding = 0, box.padding = 0,
      point.size = NA, arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines")),
      min.segment.length = 0.1, na.rm = TRUE
    )
  })

  p <- as_ggfacto_plot(p, ratio = 1)
  if (interactive) ggi(p) else p
}


#' Readable and Interactive Graph of the Individuals of a Principal Component Analysis
#'
#' @description The cloud of the individuals of a principal component analysis, in the plane of
#' two axes, with the levels of supplementary variables at the barycentre of their individuals and
#' the clusters of \code{\link{hierarchical_clust}} coloured. Hovering an individual shows its value
#' on each active variable; hovering a supplementary level shows the mean of each active variable
#' among its individuals, coloured by its standardized difference from the population's (blue
#' above, red below), as \code{\link{clust_tab}} does. The active variables are drawn by
#' \code{\link{ggpca_cor_circle}}. \code{\link{ggfacto}} is the same graph, for any analysis.
#'
#' @param res.pca An analysis made with \code{\link{principal_component_analysis}} or
#' \code{FactoMineR::PCA()}.
#' @param data The data frame the analysis was made on, in which to find the supplementary
#' variables and the clusters: the whole data frame, even when the analysis was made on a subset of
#' it. Only needed with `sup_vars` or `clust`.
#' @param sup_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> The supplementary variables, as in
#' `tab()`: each level is drawn at the weighted barycentre of its individuals.
#' @param clust The variable of `data` holding the clusters, typically made with
#' \code{\link{hierarchical_clust}}: the individuals of one cluster are coloured alike and linked
#' at mouse hover.
#' @param profiles By default the individuals are drawn: `FALSE` draws the supplementary levels
#' alone.
#' @param max_profiles The maximum number of individuals to draw: the heaviest first, and, among
#' individuals of equal weight, an evenly spread sample.
#' @param type Determines the way \code{sup_vars} are printed: \code{"text"}, \code{"labels"},
#' \code{"points"}, or \code{"facets"} (one graph of the individuals of each level of the first
#' \code{sup_vars}).
#' @inheritParams ggmca
#'
#' @return A \code{\link[ggplot2:ggplot]{ggplot}} object, to which elements can be added with
#' \code{+}. Sending it through \code{\link{ggi}} draws the interactive graph.
#' @export
#'
#' @examples
#' \donttest{
#' cars <- mtcars
#' cars$cyl <- factor(cars$cyl)
#' res.pca <- principal_component_analysis(cars, c(mpg, disp, hp, drat, wt, qsec))
#' ggpca(res.pca, cars, sup_vars = cyl) |>
#'   ggi()
#'
#' cars <- cars |>
#'   dplyr::mutate(clust = hierarchical_clust(res.pca, ncp = 2, nb_clust = 3))
#' ggpca(res.pca, cars, clust = clust)
#' }
ggpca <- function(res.pca, data, sup_vars, axes = c(1, 2), axes_names = NULL,
                  axes_reverse = NULL, type = c("text", "labels", "points", "facets"),
                  color_groups = "^.{0}", clust_color_groups = "^.+$",
                  keep_levels, discard_levels, cleannames = TRUE,
                  profiles = TRUE, clust, max_profiles = 5000,
                  alpha_profiles = 0.7, color_profiles = TRUE, base_profiles_color = "#aaaaaa",
                  text_repel = TRUE, title, sup_in_italic = FALSE, ellipses = NULL,
                  xlim, ylim, out_lims_move = FALSE,
                  shift_colors = 0, colornames_recode,
                  scale_color_light = material_colors_light(),
                  scale_color_dark  = material_colors_dark(),
                  text_size = 3.5, size_scale_max = NULL, dist_labels = c("auto", 0.04),
                  right_margin = 0, use_theme = TRUE, get_data = FALSE, lang = NULL) {
  plot_data <- pca_plot_data(
    res.pca, data, sup_vars = rlang::enquo(sup_vars), clust = rlang::enquo(clust),
    color_groups = color_groups, clust_color_groups = clust_color_groups,
    keep_levels = keep_levels, discard_levels = discard_levels, cleannames = cleannames,
    profiles = profiles, max_profiles = max_profiles, lang = lang
  )
  ggmca_plot(plot_data, axes = axes, axes_names = axes_names, axes_reverse = axes_reverse,
             type = match.arg(type), text_repel = text_repel, title = title,
             ellipses = ellipses, sup_in_italic = sup_in_italic,
             xlim = xlim, ylim = ylim, out_lims_move = out_lims_move,
             color_profiles = color_profiles, base_profiles_color = base_profiles_color,
             alpha_profiles = alpha_profiles, shift_colors = shift_colors,
             colornames_recode = colornames_recode,
             scale_color_light = scale_color_light, scale_color_dark = scale_color_dark,
             text_size = text_size, size_scale_max = size_scale_max, dist_labels = dist_labels,
             right_margin = right_margin, use_theme = use_theme, get_data = get_data)
}

# The PCA's plot model: its individuals as the points of the cloud, the supplementary levels at the
# barycentre of theirs, the central point. There is no active level: the circle draws the variables.
#' @keywords internal
#' @noRd
pca_plot_data <- function(res.pca, data, sup_vars, clust, color_groups, clust_color_groups,
                          keep_levels, discard_levels, cleannames, profiles, max_profiles, lang) {
  if (!inherits(res.pca, "PCA")) stop(
    "ggpca() draws a principal component analysis, made with principal_component_analysis() ",
    "or FactoMineR::PCA().", call. = FALSE)
  if (missing(keep_levels))    keep_levels    <- character()
  if (missing(discard_levels)) discard_levels <- character()
  m <- pca_model(res.pca)

  if (quo_given(clust) || quo_given(sup_vars)) {
    need_data(missing(data), "the supplementary variables and the clusters", "ggpca")
    cl    <- resolve_clust(clust, data)
    data  <- align_to_fit(res.pca, cl$data)
    if (length(res.pca$call$ind.sup) != 0) data <- data[-res.pca$call$ind.sup, , drop = FALSE]
    clust <- cl$name
  } else {
    data  <- NULL
    clust <- character()
  }
  sup_vars <- setdiff(select_vars(sup_vars, data), m$vars)
  if (length(clust) != 0 && !clust %in% sup_vars) sup_vars <- c(sup_vars, clust)

  with_gda_lang(lang, function(lg) {
    extra <- extra_columns(data, sup_vars)
    units <- cloud_units(m, extra)
    vars_data <- dplyr::bind_rows(
      sup_rows(units, m$coord[units$..profile, , drop = FALSE], sup_vars, NULL, clust,
               color_groups, clust_color_groups, keep_levels, discard_levels, cleannames),
      central_row(m$W, ncol(m$coord))
    )

    # A supplementary level's tooltip: the means of the active variables among its individuals.
    tip_units <- units
    tip_units[m$vars] <- m$X[units$..profile, , drop = FALSE]
    tip_units[names(extra)] <- purrr::map(tip_units[names(extra)], clean_factor,
                                          cleannames = cleannames)
    tips <- interactive_tooltips(
      tip_units, sup_vars, character(),
      list(tip_block(m$vars, gettext("Active variables:"), colour_means = TRUE)))
    central <- vars_data$role == "central"
    i <- vctrs::vec_match(
      data.frame(vars = ifelse(central, NA, vars_data$vars),
                 lvs  = ifelse(central, NA, as.character(vars_data$lvs))),
      data.frame(vars = tips$vars, lvs = tips$lvs))
    vars_data$begin_text       <- tips$begin_text[i]
    vars_data$interactive_text <- tips$interactive_text[i]

    pts <- cloud_points(seq_along(m$count), m$count, m$wn, max_profiles)
    individuals <- if (length(sup_vars) != 0) individuals_table(
      pts$nb[m$key], if (is.null(m$w)) rep(1, m$n) else m$w, m$coord[m$key, , drop = FALSE],
      purrr::map(extra[sup_vars], clean_factor, cleannames = cleannames))
    ind_data <- if (profiles) {
      clusters <- if (length(clust) != 0) {
        point_clusters(units$..profile, units$..wn, clean_factor(units[[clust]], cleannames),
                       length(m$count))
      }
      out <- points_table(pts, m$coord, clusters, vars_data[vars_data$role == "clust", ])
      out$interactive_text <- point_tooltips(individual_headings(m, pts, out), out$count,
                                             out$wcount, individual_lines(m, pts$drawn))
      out
    }

    plot_model(vars_data, ind_data, individuals,
               res = list(eig = m$eig, axes_names = res.pca$axes_names), clust = clust,
               lang = lg)
  })
}

# The first lines of an individual's tooltip: its cluster, then its name or its row in the data.
#' @keywords internal
#' @noRd
individual_headings <- function(m, pts, ind_data) {
  bold <- function(x) paste0("<b>", x, "</b>")
  first <- pts$first[pts$drawn]
  who <- if (is.null(m$names)) gettextf("Individual n\u00b0%s", m$rows[first]) else m$names[first]
  if (!"clust" %in% names(ind_data)) return(list(bold(who)))
  list(bold(gettextf("Cluster: %s", ind_data$clust)), bold(who))
}

# An individual's value on each active variable, against the population's mean, coloured by its
# standardized difference: one tabxplor record per variable, the drawn points and the population.
#' @keywords internal
#' @noRd
individual_lines <- function(m, drawn) {
  w <- m$wn
  purrr::map(m$vars, function(v) {
    x    <- m$X[[v]]
    mean <- sum(w * x) / sum(w)
    var  <- sum(w * (x - mean)^2) / sum(w)
    k    <- length(drawn)
    digits <- max(0L, min(3L, 2L - floor(log10(abs(mean) + 1e-12))))
    f <- tabxplor::fmt(
      n = as.integer(c(m$count[drawn], m$n)), wn = c(w[drawn], m$W),
      mean = c(x[drawn], mean), var = c(rep(0, k), var), diff = c(x[drawn] - mean, 0),
      scale = "level_mean", display = "mean", digits = digits,
      row_kind = c(rep("data", k), "total"), ref = "tot", col_var = v, color = "difference")
    format_mean(x[drawn] - mean, format(f)[seq_len(k)], v,
                tabxplor::fmt_get_color_code(f)[seq_len(k)])
  })
}

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


#' Simple Mean and SD Summary (deprecated)
#'
#' @description
#' One row per numeric variable: its base, its mean, its standard deviation, and its coefficient of
#' variation --- the standard deviation as a percentage of the mean, which is what lets two variables
#' measured in different units be compared for how dispersed they are.
#'
#' \strong{Deprecated}: \code{\link{interpret}} now opens with the same three figures, taken
#' from the analysis itself, so the description and the interpretation are one table and cannot
#' disagree. Use it instead; this function still works and will be removed in a future release.
#'
#' @param data A data.frame.
#' @param vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> The names of the
#' numeric variables to compute means and sds with.
#' @param wt The name of the weight variable, if needed.
#'
#' @return A \code{tabxplor} table --- see [ggfacto_summary] for how it prints.
#' @export
#' @seealso [ggfacto_summary], [interpret()].
#'
#' @examples
#' mean_sd_tab(mtcars, 1:7)
mean_sd_tab <- function(data, vars, wt) {
  deprecated_fn("mean_sd_tab", "interpret")
  vars <- names(tidyselect::eval_select(rlang::enquo(vars), data))

  not_num <- data |>
    dplyr::select(tidyselect::all_of(vars)) |>
    purrr::map_lgl(~ !is.numeric(.))

  if(any(not_num)) {
    stop(paste0("some vars are not numeric: ",
                paste0(names(not_num)[not_num], collapse = ", ")
    ))
  }

  w <- if (missing(wt)) NULL else dplyr::pull(data, !!rlang::ensym(wt))

  stats <- purrr::map_dfr(purrr::set_names(vars), function(v) {
    x  <- dplyr::pull(data, tidyselect::all_of(v))
    ok <- !is.na(x)
    if (is.null(w)) {
      tibble::tibble(n = sum(ok), mean = mean(x[ok]), var = stats::var(x[ok]))
    } else {
      tibble::tibble(n = sum(ok),
                     mean = stats::weighted.mean(x, w = w, na.rm = TRUE),
                     var  = weighted.var(x, wt = w, na.rm = TRUE))
    }
  }, .id = "variables")

  # ONE `fmt` record per variable, printed three times: the mean, then the two quantities tabxplor
  # DERIVES from the same variance -- the standard deviation and the coefficient of variation. Nothing
  # is stored twice, and the three columns cannot disagree.
  col <- function(display, digits) tabxplor::fmt(
    n = stats$n, scale = "level_mean", mean = stats$mean, var = stats$var,
    color = "no", display = display, digits = digits)

  out <- tibble::tibble(
    "variables" = tabxplor::new_lvl(forcats::as_factor(stats$variables), role = "level"),
    "n"         = tabxplor::fmt(n = stats$n, scale = "level_n", color = "no"),
    "mean"      = col("mean", 2L),
    "sd"        = col("sd"  , 2L),
    "sd/mean"   = col("cv"  , 0L)
  )

  gda_summary(tabxplor::new_tab(out, meta = list(render_extras = list(n = "no"))),
              glossary = gda_cv_line())
}
