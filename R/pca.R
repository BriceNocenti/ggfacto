# PURPOSE: The PCA entry point and its 2D graph -- principal_component_analysis() (alias PCA2()),
#   ggpca_cor_circle(), the shared projector PCA_ind.sup_coord(), and the deprecated mean_sd_tab().
# ROLE: principal_component_analysis() is the tidyselect ingress normaliser (row and column weights,
#   supplementary and named individuals, and the source rows R/ingress.R records);
#   ggpca_cor_circle() draws the correlation circle. PCA_ind.sup_coord() projects arbitrary raw rows
#   into principal space and is shared with R/pca-3d.R.
# KEY CONSTRAINTS:
#   - ggpca_cor_circle(interactive = TRUE), the default, calls ggi() itself and returns a girafe
#     widget, not a ggplot. Pass interactive = FALSE to get a `+`-able object.
#   - The circle is one geom_path() over 361 points, deliberately not ggforce::geom_circle(),
#     which re-evaluated its aes once per row of the plot data.
# See: CLAUDE.md section Repository Map.

#' Principal Component Analysis
#' @description A user-friendly wrapper around \code{\link[FactoMineR]{PCA}}, made to
#'  work with \pkg{ggfacto} functions like \code{\link{ggpca_cor_circle}},
#'  \code{\link{pca_interpret}} and \code{\link{hierarchical_clust}}. Variables are selected the
#'  way of the `tidyverse`, as in \code{tabxplor::tab()}. `PCA2()` is a shorter name for the same
#'  function.
#' @param data The data frame. To analyse a subset of the population, filter it inside the call with
#'  the native pipe, `data |> dplyr::filter(...) |> principal_component_analysis(...)`: the analysis
#'  then remembers which rows it used, for \code{\link{hierarchical_clust}}.
#' @param active_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> The names
#'  of the active variables.
#' @param wt The name of the row weight variable
#' @param col.w The weights of the columns, as a numeric vector of the same
#' length than `active_vars.`
#' @param ind_name Possibly, a variable with the names of the individuals.
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
#' pca_interpret(res.pca)
principal_component_analysis <- function(data, active_vars, wt, col.w = NULL, ind_name,
                                         scale.unit = TRUE, ind.sup = NULL, ncp = Inf,
                                         graph = FALSE, ...) {
  # WARNING: the caller's frame is captured HERE, before any promise is forced: rlang::caller_env()
  #   evaluated lazily inside source_rows() would name the wrong frame.
  expr   <- rlang::enexpr(data)
  env    <- rlang::caller_env()
  wt     <- if (missing(wt)) {character()} else {as.character(rlang::ensym(wt))}
  stopifnot(length(wt) <= 1)
  source <- source_rows(expr, env, data, wt = if (length(wt) != 0) wt)
  active_vars <- names(tidyselect::eval_select(rlang::enquo(active_vars), data))

  stopifnot(is.integer(ind.sup) | is.null(ind.sup))

  wt   <- if (length(wt) != 0) { data[[wt]] } else {NULL}
  w    <- usable_weights(data, wt, source, keep = ind.sup)
  data <- w$data; wt <- w$wt; source <- w$source
  if (length(ind.sup) > 0) ind.sup <- match(ind.sup, w$kept)

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
      str_c(
        "Axe ", axes[1]," (", round(res.pca$eig[axes[1],2], 1),
        "%)",
        if (!is.null(res.pca$axes_names[axes[1]]) ) paste0(" : ", res.pca$axes_names[axes[1]])
      )
    second_axe_title <-
      str_c(
        "Axe ", axes[2]," (", round(res.pca$eig[axes[2],2], 1),
        "%)",
        if (!is.null(res.pca$axes_names[axes[2]]) ) paste0(" : ", res.pca$axes_names[axes[2]])
      )
  } else {
    first_axe_title  <-
      str_c("Axe ", axes[1]," (",
                     round(res.pca$eig[axes[1],2], 1), "%)")
    second_axe_title <-
      str_c("Axe ", axes[2]," (",
                     round(res.pca$eig[axes[2],2], 1), "%)")
  }


  dim1 <- rlang::sym(paste0("Dim.", axes[1]))
  dim2 <- rlang::sym(paste0("Dim.", axes[2]))

  if (interactive) proj <- FALSE

  data_circle <- res.pca$var$coord |> as.data.frame() |> tibble::rownames_to_column("name") |>
    tibble::as_tibble() |> dplyr::mutate(id = as.integer(as.factor(.data$name)))


  interactive_txt <-
    data_circle |>
    dplyr::select("name", tidyselect::starts_with("Dim.")) |>
    dplyr::mutate(
      name = paste0("<b>", .data$name, "</b>\n"),

      dplyr::across(
        tidyselect::starts_with("Dim."),
        ~ paste0("Coord", unbrk, "Axe", unbrk, str_sub(dplyr::cur_column(), -1, -1),
                 ":", unbrk,
                 str_pad(round(., 2), width = 5, side = "left") |>
                   str_replace_all("-", paste0(unbrk, "-")),
                 " (cor",
                 str_pad(round(.*100, 0), width = 3, side = "left"),
                 "%)\n") |>
          str_replace_all("-", paste0(unbrk, "-") ) |>
          str_replace_all(" ", paste0(unbrk, unbrk, unbrk))
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
    ggplot2::geom_path(
      data = data.frame(angle = seq(0, 2 * pi, length.out = 361)) |>
        dplyr::mutate(x = cos(.data$angle), y = sin(.data$angle)),
      mapping = ggplot2::aes(x = .data$x, y = .data$y), inherit.aes = FALSE,
      color = "#d32f2f", linewidth = 1) +
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
      arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "cm")), linewidth = 1
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
#' \strong{Deprecated}: \code{\link{pca_interpret}} now opens with the same three figures, taken
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
#' @seealso [ggfacto_summary], [pca_interpret()].
#'
#' @examples
#' mean_sd_tab(mtcars, 1:7)
mean_sd_tab <- function(data, vars, wt) {
  deprecated_fn("mean_sd_tab", "pca_interpret")
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
