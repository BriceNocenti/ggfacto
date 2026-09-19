# PURPOSE: The PCA entry point and its 2D graphs -- principal_component_analysis() (alias PCA2()),
#   ggpca_cor_circle(), ggpca() and its builder pca_plot_data(), the shared projector
#   PCA_ind.sup_coord(), and the deprecated mean_sd_tab().
# ROLE: principal_component_analysis() is the tidyselect ingress normaliser (row and column weights,
#   supplementary and named individuals, and the source rows R/ingress.R records).
#   ggpca_cor_circle() draws the variables; ggpca() draws the individuals through the shared plot
#   model (R/plot-model.R) and the one renderer. PCA_ind.sup_coord() projects arbitrary raw rows
#   into principal space and is shared with R/pca-3d.R.
# KEY CONSTRAINTS:
#   - Both graphs are drawn by the one renderer from variable_vectors(): the circle of
#     correlations, or the biplot, where the same arrows are rescaled onto the cloud of individuals
#     and only their directions read. Both return a ggplot; `interactive = TRUE` returns the widget.
#   - The individuals are the points of ggpca()'s cloud, distinct ones merged (pca_model(),
#     R/model.R); a supplementary level is the plain weighted barycentre of its individuals,
#     FactoMineR's quali.sup point, never divided by sqrt(eigenvalue) as in an MCA.
#   - A tooltip mean is coloured by its standardized difference from its block's mean, the colour
#     clust_tab() gives a PCA's clusters: the reader meets one ladder for one quantity.
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
#' @description The active variables of a principal component analysis as arrows in the circle of
#' correlations: the coordinate of a variable on an axis is its correlation with it. Hovering a
#' variable shows its coordinates, and its projections on the two axes. \code{\link{ggfacto}} draws
#' it for a principal component analysis, and draws the same arrows over the cloud of individuals
#' when individuals, supplementary variables or clusters are asked for.
#'
#' @param res.pca An analysis made with \code{\link{principal_component_analysis}} or
#' \code{\link[FactoMineR:PCA]{FactoMineR::PCA}}.
#' @param axes The axes to print, as a numeric vector of length 2.
#' @param proj Set to `TRUE` to print projections of vectors over the two axes.
#' @param interactive Set to `TRUE` to get the interactive graph at once, as \code{\link{ggi}}
#' would make it. By default, a \code{\link[ggplot2]{ggplot}}, to which elements can be added with
#' `+` before passing it to \code{\link{ggi}}.
#' @param text_size Size of the text.
#' @param lang \code{NULL} (the session's language), \code{"en"} or \code{"fr"}.
#' @param axes_names Names of all the axes, as a character vector.
#' @param axes_reverse `1` to invert left and right, `2` to invert up and down, `1:2` for both.
#' @param title The title of the graph.
#' @param xlim,ylim Horizontal and vertical limits, as numeric vectors of length 2.
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
ggpca_cor_circle <- function(res.pca, axes = c(1, 2), proj = FALSE, interactive = FALSE,
                             text_size = 3.5, lang = NULL, axes_names = NULL, axes_reverse = NULL,
                             title, xlim, ylim) {
  if (!inherits(res.pca, "PCA")) stop(
    "ggpca_cor_circle() draws a principal component analysis, made with ",
    "principal_component_analysis() or FactoMineR::PCA().", call. = FALSE)
  plot_data <- with_gda_lang(lang, function(lg) {
    pd <- plot_model(vars_rows(character(), character(), character(),
                               coord = matrix(0, 0, ncol(res.pca$var$coord))),
                     res = list(eig = eig_table(res.pca), axes_names = res.pca$axes_names),
                     lang = lg)
    pd$vectors <- list(data = variable_vectors(res.pca), radius = 1, proj = proj)
    pd
  })
  p <- ggmca_plot(plot_data, axes = axes, axes_names = axes_names, axes_reverse = axes_reverse,
                  text_size = text_size, title = title, xlim = xlim, ylim = ylim)
  if (interactive) ggi(p) else p
}

# The active variables as vectors: their correlations with the axes (the circle's coordinates),
# the renderer scaling them onto the cloud of individuals in a biplot, each with its tooltip -- its
# mean (cv) first, then its coordinates.
#' @keywords internal
#' @noRd
variable_vectors <- function(res.pca, m = pca_model(res.pca)) {
  coord <- as.matrix(res.pca$var$coord)
  colnames(coord) <- paste("Dim", seq_len(ncol(coord)))
  lines <- purrr::map(seq_len(ncol(coord)), function(k) {
    gettextf("Coord axe %s: %s (cor. %s%%)", k,
             str_pad(format(round(coord[, k], 2), nsmall = 2), width = 5),
             str_pad(round(coord[, k] * 100, 0), width = 3)) |>
      str_replace_all(" ", unbrk)
  })
  dplyr::bind_cols(
    tibble::tibble(name = rownames(coord), id = 1000L + seq_len(nrow(coord)),
                   interactive_text = paste0(
                     "<b>", rownames(coord), "</b>\n",
                     gettextf("mean (cv): %s", purrr::map_chr(rownames(coord), \(v) mean_cv(
                       m$X[[v]], m$wn))), "\n",
                     do.call(paste, c(lines, sep = "\n")))),
    tibble::as_tibble(coord))
}


#' Readable and Interactive Graph of the Individuals of a Principal Component Analysis
#'
#' @description The biplot of a principal component analysis: the cloud of its individuals in the
#' plane of two axes, the levels of supplementary variables at the barycentre of their individuals,
#' the clusters of \code{\link{hierarchical_clust}} coloured, and the active variables as arrows,
#' the circle of correlations rescaled onto the cloud. Only the DIRECTION of an arrow reads there:
#' an individual lies towards the variables it scores high on; the length of an arrow and the
#' distance between an arrow and an individual mean nothing, and \code{\link{ggpca_cor_circle}}
#' draws the correlations at their own scale. Hovering an individual shows its value on each active
#' variable; hovering a supplementary level shows the mean of each active variable among its
#' individuals, coloured by its standardized difference from the population's (blue above, red
#' below), as \code{\link{clust_tab}} does. \code{\link{ggfacto}} draws it for a principal
#' component analysis given individuals, supplementary variables or clusters.
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
#' and the variables alone.
#' @param variables By default the active variables are drawn as arrows. Set to `FALSE` to draw
#' the individuals alone.
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
                  text_repel = TRUE, title, sup_in_italic = TRUE, ellipses = NULL,
                  xlim, ylim, out_lims_move = FALSE,
                  shift_colors = 0, colornames_recode,
                  scale_color_light = material_colors_light(),
                  scale_color_dark  = material_colors_dark(),
                  text_size = 3.5, size_scale_max = NULL, dist_labels = c("auto", 0.04),
                  right_margin = 0, use_theme = TRUE, get_data = FALSE, lang = NULL,
                  variables = TRUE) {
  plot_data <- pca_plot_data(
    res.pca, data, sup_vars = rlang::enquo(sup_vars), clust = rlang::enquo(clust),
    color_groups = color_groups, clust_color_groups = clust_color_groups,
    keep_levels = keep_levels, discard_levels = discard_levels, cleannames = cleannames,
    profiles = profiles, max_profiles = max_profiles, lang = lang, variables = variables
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
# barycentre of theirs, the central point, and the active variables as vectors (`vectors`).
#' @keywords internal
#' @noRd
pca_plot_data <- function(res.pca, data, sup_vars, clust, color_groups, clust_color_groups,
                          keep_levels, discard_levels, cleannames, profiles, max_profiles, lang,
                          variables = TRUE) {
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
    # the central point is the population: its size, and the mean (cv) of each active variable
    vars_data$begin_text[central] <- paste0(
      "<b>", gettext("Central point"), "</b>\n", gettextf("n: %s", m$n),
      if (!isTRUE(all.equal(m$n, m$W))) paste0("\n", gettextf("weighted n: %s", round(m$W))))
    vars_data$interactive_text[central] <- paste0(
      "\n<b>", gettext("mean (cv):"), "</b>\n",
      paste0(m$vars, ": ", purrr::map_chr(m$vars, \(v) mean_cv(m$X[[v]], m$wn)),
             collapse = "\n"))

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

    pd <- plot_model(vars_data, ind_data, individuals,
                     res = list(eig = m$eig, axes_names = res.pca$axes_names), clust = clust,
                     lang = lg)
    # the arrows are scaled onto the whole cloud, drawn or not, so the geometry does not depend on
    # `profiles` or `max_profiles`
    if (variables) pd$vectors <- list(data = variable_vectors(res.pca, m), cloud = m$coord,
                                      proj = FALSE)
    pd
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
    digits <- mean_digits(mean)
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
