# PURPOSE: ggpca_3d() -- the plotly 2D/3D PCA scene.
# ROLE: Draws the initial variable axes with their breaks, the normalized correlation vectors and
#   sphere, the individuals, the main plane and the mean-point projections. Reads the SVD
#   directly (res.pca$svd$V / $vs) rather than the derived coordinates.
# KEY CONSTRAINTS:
#   - plotly is a Suggests: the entry point is guarded with requireNamespace().
#   - The Le Roux notation key for the change of frame between the initial and principal spaces is
#     stated once, above the coordinate block. Keep it with the code it explains.
# See: CLAUDE.md section Repository Map.

#' Interactive 3D Plot for Principal Component Analyses (plotly::)
#'
#' @param res.pca The result of \code{\link[FactoMineR:PCA]{FactoMineR::PCA}}.
#' @param axes The axes to print, as a numeric vector of length 3 (or 2).
#' @param princ_axes_print The breaks of the principal axes.
#' @param base_axe_n_breaks The number of breaks in initial variables axes.
#' @param ind.size The size of the points of individuals.
#' @param ind_name.size The size of the names of individuals.
#' @param title Plot title.
#' @param center By default the plot is centered on the central point. Set to
#' `FALSE` to center on the origin of all variables (zero coordinates).
#' @param var_names_on By default `"var"` the names of variables are drawn upon
#' the initial axes. Set to `"cor"` to draw them upon correlation vectors instead.
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
#' @param type Which elements of the graph to print, among :
#' #'      \itemize{
#'    \item \code{"var"        } : initial variables axes, with breaks
#'    \item \code{"cor"        } : normalized correlation vectors (length = 1)
#'    \item \code{"cor_sphere" } : a 3D sphere of standard deviation 1
#'    \item \code{"ind"        } : points of individuals
#'    \item \code{"ind_name"   } : names of individuals
#'    \item \code{"main_plan"  } : the plan 1-2.
#'    \item \code{"projections"} : projections of mean point on initial variables
#'    \item \code{"V"          } : vectors of the V transition matrix
#'    \item \code{"vs"         } : vectors of the matrix of singular values
#    \item \code{"all"        } : all elements.
#'  }
#' @param camera_view Possibility to add a (replace `view`)
#' @param aspectratio_from_eig Set to `TRUE` to modify axes length based on
#' eigenvalues.
#' @param always_make_ind_tooltips Set to `TRUE` to add interactive toolips for
#'  individuals.
#' @param var_color The color of the initial variables/dimensions
#' @param max_ind The maximun number of individuals to print.
#' @param max_ind_seed The random seed used to sample individuals.
#'
#' @return A \code{\link[plotly]{plotly}} html interactive 2d or 3d graph.
#' @export
#'
#' @examplesIf requireNamespace("plotly", quietly = TRUE)
#' \donttest{
#' data(mtcars, package = "datasets")
#' mtcars <- mtcars[1:7] |> dplyr::rename(weight = wt)
#' res.pca <- FactoMineR::PCA(mtcars, graph = FALSE)
#'
#' # Variables and individuals
#' ggpca_3d(res.pca)
#'
#' # Circle of correlation 3D
#' ggpca_3d(res.pca, type = c("cor", "cor_sphere"),
#'          var_names_on = "cor", base_zoom = 0.6,
#'          princ_axes_print = -1:1, view = "All"
#' )
#' }
ggpca_3d <- function(res.pca, axes = c(1, 2, 3),
                     princ_axes_print = -3:3, base_axe_n_breaks = 10,
                     ind.size = 4, ind_name.size = 3, title, center = TRUE,
                     var_names_on = "var", #"cor",
                     base_zoom = 1, remove_buttons = FALSE, cone_size = 0.33,
                     view = "All", # c("Plane 1-2", "Plane 1-3", "Plane 2-3", "All")
                     type = c("var", "ind", "main_plan", "projections"),
                     camera_view, aspectratio_from_eig = FALSE,
                     always_make_ind_tooltips = FALSE,
                     var_color = "#4D4D4D", max_ind = 500, max_ind_seed
) {
  requireNamespace("plotly", quietly = TRUE)

  D2 <- length(axes) == 2 ; stopifnot(length(axes) %in% 2:3 )
  if (D2) axes <- c(axes, NA)

  ind_coords <- res.pca$ind$coord |> as.data.frame() |>
    tibble::rownames_to_column("name") |>
    tibble::as_tibble() |>
    dplyr::left_join(tibble::rownames_to_column(res.pca$call$X, "name"),
                     by = "name") |>
    tibble::add_column(wt = res.pca$call$row.w)

  if (max_ind < nrow(ind_coords)) {
    if (!missing(max_ind_seed)) set.seed(max_ind_seed)
    ind_coords <- ind_coords |> dplyr::slice_sample(n = max_ind)
  }


  df_base <- res.pca$call$X |>
    tibble::rownames_to_column("name") |>
    tibble::as_tibble()

  if (!is.null(res.pca$call$quali.sup) ) {
    quali_sup <- names(res.pca$call$quali.sup$quali.sup)
    df_base <- df_base |> dplyr::select(-tidyselect::all_of(quali_sup))
  } else {
    quali_sup <- character()
  }
  if (!is.null(  res.pca$call$quanti.sup) ) {
    quanti.sup <- names(res.pca$call$quanti.sup)
    df_base <- df_base |> dplyr::select(-tidyselect::all_of(quanti.sup))
  } else {
    quanti.sup <- character()
  }

  active_vars <- colnames(df_base)[!colnames(df_base) == "name"]

  active_means <- if(center) {
    res.pca$call$centre |> purrr::set_names(active_vars)
  } else {
    rep(0, length(res.pca$call$centre)) |> purrr::set_names(active_vars)
  }

  # # (Probleme : res.pca$var n'est pas tout a fait aligne avec les axes de depart...)
  # # (ca ne vient pas non plus de la fonction de calcul des ind.sup : resultats identiques)
  # # Solution : var_coords est fait pour le cercle des correlations, ou il faut corriger
  # #  les vecteurs normes en fonction de l'importance des axes ?
  var_coords <- res.pca$var$coord |> as.data.frame() |>
    tibble::rownames_to_column("name")
  # # # manually :
  # # coord.var <- t(t(as.matrix(res.pca$svd$V)) * res.pca$svd$vs) # vs^2 = eig # eig <- eig[1:ncp]
  # # rownames(coord.var) <- active_vars ; colnames(coord.var) <- paste("Dim",  c(1:ncol(res.pca$svd$V)), sep = ".")
  # # coord.var
  #
  V_coords <-  res.pca$svd$V |> as.data.frame()
  colnames(V_coords) <- colnames(var_coords[, names(var_coords) != "name"])
  V_coords <- V_coords |>
    tibble::add_column(name = var_coords$name, .before = 1)
  #
  # # The operation is this one (and is ok) :
  # var_coords
  # V_coords |>
  #   dplyr::mutate(Dim.1 = Dim.1*res.pca$svd$vs[1],
  #          Dim.2 = Dim.2*res.pca$svd$vs[2],
  #          Dim.3 = Dim.3*res.pca$svd$vs[3],
  #          )
  #
  # # # Par rapport a Brigitte Le Roux, Analyse geometrique..., (cf. Chap. 6 Exercice 6.1)
  # #                # X0: coordonnees de base centrees.
  # # scale.unit = T # X0r: X0 * 1/sqrt(vjj) : coordonnees c. reduites (matrice diag des variances)
  # # res.pca$eig    # Lambda "\\u039b" : matrice diagonale des valeurs propres
  # # res.pca$svd$vs # Ksi "\\u039e" : matrice diagonale des valeurs singulieres (mais, ici, vecteur) ;
  # #                      (les elements sont notes Ksi-j  "\\u03be" )
  # # res.pca$svd$U
  # # res.pca$svd$V  # A: matrice des vecteurs propres normes.
  # # ind$coord      # Y: coordonnees principales. Y = X0A    Inv: X0=Yt(A) (ou X0r)
  # # var$coords     # B: coeffs de regression B = A Ksi (b1j = sqrt(Lamda1) * a1j = Ksi1 * a1j)
  #
  # # # var$coords calculation is equivalent to Britte Le Roux
  # # t(t(as.matrix(res.pca$svd$V)) * res.pca$svd$vs)
  # # as.matrix(res.pca$svd$V) %*% diag(res.pca$svd$vs)
  #
  # # # var_coord et V sont tous les deux sur la sphere de rayon 1 ecart-type
  # # # (mais la sphere est aplatie le long de l'axe 2 : pas de fixed ratio ?)
  # # # install.packages("pracma")
  # # theta = seq(0, pi, length.out = 25)
  # # phi   = seq(-pi,   pi, length.out = 25)
  # # meshgrid_sphere <- pracma::meshgrid(theta, phi)
  # # theta <- meshgrid_sphere[[1]] #|> c()
  # # phi   <- meshgrid_sphere[[2]] #|> c()
  # # r = 1
  # # sphere_data <- tibble::tibble(
  # #   x = r * sin(theta) * cos(phi),
  # #   y = r * sin(theta) * sin(phi),
  # #   z = r * cos(theta),
  # # )
  # #
  # # SALAIRE_3D_cor |>
  # #   plotly::add_surface(data = sphere_data,
  # #     x = ~x, y = ~y, z = ~z,
  # #     opacity = 0.3,
  # #     colorscale = list(list(0, "#ba2d65"), list(1, "#ba2d65")), showscale = FALSE,
  # #     showlegend = FALSE, inherit = FALSE
  # #   )
  #
  # ## code :
  # # # (cor is the same by default, with scale.unit = TRUE)
  # # X <- t(t(as.matrix(X)) - centre)
  # # if (scale.unit) {
  # #   ecart.type <- ec.tab(X, row.w)
  # #   X <- t(t(X)/ecart.type)
  # # }
  # # dist2.var <- as.vector(crossprod(rep(1, nrow(X)), as.matrix(X^2 * row.w)))
  # # cor.var <- coord.var/sqrt(dist2)



  base_axis_coords <-
    purrr::imap_dfr(
      dplyr::select(df_base, -"name"),
      ~ tibble::tibble(
        !!rlang::sym(.y) := scales::breaks_extended(n = base_axe_n_breaks)(if (center) {.x} else {c(0, .x)}),
        name       = .y,
        base_coord = !!rlang::sym(.y)
      )
    ) |>
    dplyr::select("name", tidyselect::everything()) |>
    dplyr::mutate(dplyr::across(
      tidyselect::everything(),
      ~ tidyr::replace_na(., active_means[dplyr::cur_column()])
    ),
    name = paste0(.data$name, "_", .data$base_coord)
    )

  ind.sup_coords <- base_axis_coords |> dplyr::select(-"base_coord") |>
    PCA_ind.sup_coord(res.pca) # center = !center

  base_axis_in_princ <- ind.sup_coords |>
    as.data.frame() |> tibble::rownames_to_column("name") |>
    # dplyr::filter(!str_detect(name, "mean$")) |>
    dplyr::left_join(base_axis_coords, by = "name") |>
    dplyr::mutate(
      name    = forcats::as_factor(str_remove(.data$name, "_[^_]+$")),
      pair_id = as.integer(as.factor(.data$name)) #,
    )


  # plot_ly(x=res.pca$ind$coord[, "Dim.1"], y=res.pca$ind$coord[, "Dim.2"], z=res.pca$ind$coord[, "Dim.3"], type="scatter3d", mode="markers")


  princ_axes <-
    colnames(ind_coords)[str_detect(colnames(ind_coords), "Dim.")] |>
    purrr::map_dfr(~ tibble::tibble(
      !!rlang::sym(.x) := princ_axes_print,
      base_coord = princ_axes_print,
      name       = str_replace(.x, "Dim.", "Axe "),
    )
    ) |>
    dplyr::mutate(
      dplyr::across(tidyselect::starts_with("Dim."), ~ tidyr::replace_na(., 0)),
      name    = forcats::as_factor(.data$name),  #name = paste0(name, ".", base_coord)
      pair_id = as.integer(.data$name),

    ) |>
    dplyr::select("name", "pair_id", "base_coord", tidyselect::starts_with("Dim.") )


  # Coordonnees des projections du point moyen sur les axes de depart dans princ
  mean_point <- res.pca$call$centre |> diag() |> as.data.frame()
  colnames(mean_point) <- active_vars ; rownames(mean_point) <- active_vars
  mean_projs <- PCA_ind.sup_coord(mean_point, res.pca)
  # mean_projs <- mapply(FUN = `*`, as.data.frame(mean_projs), res.pca$call$ecart.type) |>
  mean_projs <- mean_projs |> as.data.frame() |>
    tibble::rownames_to_column("name") |> tibble::as_tibble()
  mean_projs <- dplyr::bind_rows(
    mean_projs,
    dplyr::mutate(mean_projs, dplyr::across(where(is.double), ~ 0))
  ) |>
    dplyr::arrange(.data$name)

  plot_range <-
    dplyr::bind_rows(
      dplyr::select(ind_coords, tidyselect::starts_with("Dim.")),
      dplyr::select(base_axis_in_princ, tidyselect::starts_with("Dim."))
    ) |>
    purrr::map(~ range(.) |> abs() |> max())
  plot_range <- plot_range  |> purrr::map(~ c(-., .))


  #     Plan 1-2
  planDf <-
    dplyr::bind_cols(
      data.frame(Dim.1 = rep(range(princ_axes_print), 2),
                 Dim.2 = rep(range(princ_axes_print), each = 2)
      ) |> tibble::as_tibble(),

      dplyr::select(princ_axes, tidyselect::starts_with("Dim.") &
                      -tidyselect::all_of(c("Dim.1", "Dim.2")) ) |>
        dplyr::slice(1:4) |>
        dplyr::mutate(dplyr::across(tidyselect::everything(), ~ 0L))
    )


  # Tooltips at point hover
  if ("ind" %in% type | always_make_ind_tooltips) {


    ind_tooltips_active_vars <- ind_coords |>
      dplyr::select("name", tidyselect::all_of(active_vars)) |>
      dplyr::mutate(
        dplyr::across(tidyselect::all_of(active_vars), # -name,
                      ~ format(., justify = "right", digits = 1, big.mark = " ", trim = TRUE) #,  # nsmall = 0,
        ),

        dplyr::across(tidyselect::all_of(active_vars), str_length, .names = "{.col}_length_str"),

        max_length = pmax(!!!rlang::syms(paste0(active_vars, "_length_str")), na.rm = TRUE),

        dplyr::across(
          tidyselect::all_of(active_vars),
          ~ paste0(dplyr::cur_column(), ": ",
                   str_pad(., width = max_length,  side = "left")) |>
            str_replace("(^[^\\.]+\\.)", paste0(unbrk, "\\1") ) #,
        )
      ) |>
      dplyr::select(-tidyselect::ends_with("_length_str"), -"max_length")


    ind_tooltips_active_diff <- ind_coords |>
      dplyr::select("name", tidyselect::all_of(active_vars)) |>
      dplyr::mutate(
        dplyr::across(tidyselect::all_of(active_vars), ~ . - active_means[dplyr::cur_column()]),

        dplyr::across(tidyselect::all_of(active_vars),
                      ~ paste0(dplyr::if_else(. >= 0, "+", ""), #paste0(unbrk, "-")
                               format(., justify = "right", digits = 1, big.mark = " ", trim = TRUE) ) |>
                        str_replace("\\+ ", "\\+")
        ),

        dplyr::across(tidyselect::all_of(active_vars), str_length, .names = "{.col}_length_str"),
        max_length = pmax(!!!rlang::syms(paste0(active_vars, "_length_str")), na.rm = TRUE),

        dplyr::across(tidyselect::all_of(active_vars),
                      ~ str_pad(., width = max_length,  side = "left") |>
                        str_replace("-", paste0(unbrk, "-")) |>
                        str_replace("(^[^\\.]+\\.)", paste0(unbrk, "\\1") )
        )
      ) |>
      dplyr::select(-tidyselect::ends_with("_length_str"), -"max_length") |>
      dplyr::rename_with(~ paste0(., "_diff"), .cols = tidyselect::all_of(active_vars))

    ind_tooltips_active_vars <-  ind_tooltips_active_vars |>
      dplyr::left_join(ind_tooltips_active_diff, by = "name") |>
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(active_vars),
        ~ paste0(., " (", rlang::eval_tidy(rlang::sym(paste0(dplyr::cur_column(), "_diff"))), ")<br>" )
      )) |>
      dplyr::select(-tidyselect::ends_with("_diff"))


    ind_contrib <- res.pca$ind$contrib |> as.data.frame() |> tibble::rownames_to_column("name") |>
      tibble::as_tibble() |>
      dplyr::rename_with(~ str_replace(., "Dim.", "ctr."), .cols = tidyselect::starts_with("Dim."))

    ind_cos2 <- res.pca$ind$cos2 |> as.data.frame() |> tibble::rownames_to_column("name") |>
      tibble::as_tibble() |>
      dplyr::rename_with(~ str_replace(., "Dim.", "cos2."), .cols = tidyselect::starts_with("Dim."))


    ind_tooltips_coords <-  ind_coords |>
      dplyr::select("name", tidyselect::starts_with("Dim.")) |>
      dplyr::left_join(ind_contrib, by = "name") |>
      dplyr::left_join(ind_cos2, by = "name") |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::starts_with("Dim."),
          ~ paste0("Axe ", str_remove(dplyr::cur_column(), "Dim."),  ": " ,
                   format(round(., 1), justify = "right", nsmall = 1)#,  # nsmall = 0,
          )
        ),

        dplyr::across(
          tidyselect::starts_with("ctr."),
          ~ paste0(" ; contrib: ",
                   format(round(., 1), justify = "right", nsmall = 1),  # nsmall = 0,
                   "%"
          )
        ),

        dplyr::across(
          tidyselect::starts_with("cos2."),
          ~ paste0(" ; cos2: ",
                   format(round(.*100, 0), justify = "right", nsmall = 0),  # nsmall = 0,
                   "%<br>"
          )
        ),

        dplyr::across(tidyselect::any_of(dplyr::first(tidyselect::starts_with("Dim."))), ~ paste0("<br>", .) )
      )
    ind_coords_order <- purrr::set_names(names(ind_tooltips_coords)[-1] |> str_sub(-1, -1) ,
                                         names(ind_tooltips_coords)[-1]
    ) |> sort() |> names()
    ind_tooltips_coords <- ind_tooltips_coords |>
      dplyr::select("name", tidyselect::all_of(ind_coords_order))

    ind_tooltips_quali_sup <- ind_coords |>
      dplyr::select("name", tidyselect::all_of(quali_sup)) |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(quali_sup),
          ~ paste0(dplyr::cur_column(), ": ", ., "<br>") # str_pad(., max(str_length(.)), side = "right")
        ),

        dplyr::across(
          tidyselect::any_of(dplyr::first(quali_sup, default = "NO__VAR_:")),
          ~ paste0("<br><b>Quali sup:</b><br>", .)
        )
      )

    ind_tooltips_quanti_sup <- ind_coords |>
      dplyr::select("name", tidyselect::all_of(quanti.sup)) |>
      dplyr::mutate(
        dplyr::across(tidyselect::all_of(quanti.sup),
                      ~ paste0(dplyr::cur_column(), ": " ,
                               format(., justify = "right", digits = 1, big.mark= " "),
                               "<br>"
                      )
        ),

        dplyr::across(tidyselect::any_of(dplyr::first(quanti.sup, default = "NO__VAR_:")),
                      ~ paste0("<br><b>Quanti sup:</b><br>", .)
        )
      )

    ind_tooltips <- ind_tooltips_active_vars |>
      dplyr::left_join(ind_tooltips_coords    , by = "name") |>
      dplyr::left_join(ind_tooltips_quali_sup , by = "name") |>
      dplyr::left_join(ind_tooltips_quanti_sup, by = "name") |>
      dplyr::mutate(name2 = paste0("<b>", .data$name, "</b><br>"), .after = 1) |>
      tidyr::unite(col = "tooltip", tidyselect::everything() & -"name",
                   remove = TRUE, sep = "") # , na.rm = TRUE

    ind_tooltips <- ind_tooltips |>
      dplyr::mutate(
        tooltip = str_replace_all(.data$tooltip, " ",
                                           paste0(unbrk, unbrk, unbrk))
      )

    ind_coords <- ind_coords |> dplyr::left_join(ind_tooltips, by = "name")
  }




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

  axes_params <- purrr::map(
    plot_range,
    ~ c(list(range = ., title = ""), axes_common_infos)
  )







  ## Assemble plot ----

  dim1 <- rlang::sym(str_c("Dim.", axes[1]))
  dim2 <- rlang::sym(str_c("Dim.", axes[2]))
  dim3 <- if (D2) {NULL} else {rlang::sym(str_c("Dim.", axes[3]))}


  # To get a fixed aspect ratio, put a point in max range * aspectratio on all axes
  if (aspectratio_from_eig) {
    aspectratio <- list(x = res.pca$svd$vs[axes[1]],
                        y = res.pca$svd$vs[axes[2]],
                        z = if (D2) {NULL} else {res.pca$svd$vs[axes[3]]}
    )

  } else {
    aspectratio <- list(x = 1, y = 1, z =if (D2) {NULL} else {1})
  }

  aspectratio_range <- tibble::as_tibble(plot_range) |>
    ## dplyr::mutate(Dim.2 = Dim.2 * 2) |>  # test
    #dplyr::mutate(dplyr::across(tidyselect::everything(), ~pmax(!!!rlang::syms(names(plot_range))))) |>
    dplyr::mutate(
      dplyr::across(axes[1], ~ . * aspectratio[[1]]),
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

    # Individus
    if ("ind" %in% type) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = ind_coords, scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),  # color = df$color_col
          text = ~tooltip,
          #textfont = list(color = "#00600f", size = ind_name.size),  # "#0077c2"
          marker   = list(color = "#00600f", size = ind.size),  # "#0077c2"
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
    }

    if ("ind_name" %in% type) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = ind_coords, scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),  # color = df$color_col
          text = ~name,
          textfont = list(color = "#00600f", size = ind_name.size),  # "#0077c2"
          hoverinfo = "skip",
          # text = ~paste("Price: ", price, '$<br>Cut:', cut),
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "text", showlegend = FALSE, inherit = FALSE)

    }

    # Variables : referentiels de depart
    if ("var" %in% type) {
      c("#4D4D4D", "black")
      dual_plots[[i]] <- dual_plots[[i]] |>
        # plotly::add_trace(data = base_axis_in_princ |> dplyr::group_by(name) |> dplyr::slice(-dplyr::n()) |> dplyr::ungroup(),
        #                   scene = scene_name[i],
        #                   x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ pair_id,
        #                   line   = list(color  = "black", width = 5),
        #                   marker = list(color  = "black",
        #                                 symbol = "cross",
        #                                 size = 5), # 3, # in 2D : "line-ns-open"
        #                   type = "scatter3d", mode = "lines+markers", showlegend = FALSE, inherit = FALSE,
        #                   hoverinfo = "skip") |>
        plotly::add_trace(
          data = base_axis_in_princ |>
            dplyr::group_by(.data$name) |> dplyr::slice(-dplyr::n()) |>
            dplyr::mutate(base_coord = format(.data$base_coord, trim = TRUE, digits = 1)) |>
            dplyr::ungroup(),
          scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),
          marker = list(color  = var_color,
                        symbol = "cross",
                        size = 5), # 3, # in 2D : "line-ns-open"
          text = ~base_coord, textfont = list(color = var_color, size = 10),
          textposition = "bottom center", hoverinfo = "skip",
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = 'markers+text', showlegend = FALSE, inherit = FALSE) |>
        plotly::add_trace(
          data = base_axis_in_princ |> dplyr::group_by(.data$name) |>
            dplyr::slice(1, dplyr::n()) |>
            dplyr::mutate(name = dplyr::if_else(
              dplyr::row_number() != 1 & var_names_on == "var",
              true  = paste0("<b>", .data$name, "</b>"),
              false = ""
            )) |>
            dplyr::ungroup(),
          scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ pair_id,
          line = list(color  = var_color, width = 5), # "black"
          text = ~name, textfont = list(color = var_color, size = 15),  # "black"
          #textposition = "top center",
          hoverinfo = "skip",
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = 'lines+text', showlegend = FALSE, inherit = FALSE)

      if (!D2) {
        dual_plots[[i]] <- dual_plots[[i]] |>
          plotly::add_trace( # cone au bout des axes
            data = base_axis_in_princ |> dplyr::group_by(.data$name) |>
              dplyr::mutate(is_last = dplyr::row_number() == dplyr::n(),
                            is_min  = .data$base_coord == min(.data$base_coord) ) |>
              dplyr::ungroup() |> dplyr::filter(.data$is_last | .data$is_min) |>
              dplyr::select("name", "is_last",  tidyselect::starts_with("Dim.")) |>
              dplyr::mutate(is_last = dplyr::if_else(.data$is_last, "", "_o")) |>
              tidyr::pivot_wider(names_from  = "is_last",
                                 values_from = tidyselect::starts_with("Dim."),
                                 names_sep = "") |>
              dplyr::mutate(dplyr::across(
                tidyselect::ends_with("_o"),
                ~ rlang::eval_tidy(
                  rlang::sym(str_remove(dplyr::cur_column(), "_o"))
                ) - .
              )),
            scene = scene_name[i],
            x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ name,
            u = ~eval(rlang::sym(str_c("Dim.", axes[1], "_o"))),
            v = ~eval(rlang::sym(str_c("Dim.", axes[2], "_o"))),
            w = ~eval(rlang::sym(str_c("Dim.", axes[3], "_o"))),
            sizeref = cone_size, sizemode = "absolute",

            colorscale = list(list(0, var_color), list(1, var_color)), # "black"
            showscale = FALSE, hoverinfo = "skip",
            # lighting  = list(ambient = 1), lightposition= list(x=0, y=0, z=1e5),
            type = "cone", anchor = "center", showlegend = FALSE, inherit = FALSE
          )
      }
    }


    # V : vecteurs propres normees (A Brigitte Le Roux) : 1 ecart-type sur axes de depart
    if ("V" %in% type) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = dplyr::bind_rows(
            V_coords,
            V_coords |>  dplyr::mutate(dplyr::across(tidyselect::starts_with("Dim."), ~ 0))
          ) |> dplyr::mutate(
            id_pair = as.integer(as.factor(.data$name)),
            name    = dplyr::if_else(dplyr::row_number() == 1L, "<b>V</b>", NA_character_)
          ),
          scene = scene_name[i],
          x = ~eval(dim1)*9/10, y = ~eval(dim2)*9/10, z = ~eval(dim3)*9/10, split = ~ id_pair,

          line   = list(color  = "black", width = 5),
          #text = ~name, textfont = list(color = "black", size = 15),
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "lines", showlegend = FALSE, inherit = FALSE,
          hoverinfo = "skip")

      if (!D2) {
        dual_plots[[i]] <- dual_plots[[i]] |>
          plotly::add_trace(
            data = V_coords,
            scene = scene_name[i],
            x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ name,
            u = ~eval(dim1), v = ~eval(dim2), w = ~eval(dim3),
            sizeref = cone_size, sizemode = "absolute",
            colorscale = list(list(0, "black"), list(1,"black")), #autocolorscale = FALSE,
            showscale = FALSE, hoverinfo = "skip",
            # lighting  = list(ambient = 1), lightposition= list(x=0, y=0, z=1e5),
            type = "cone", anchor = "tip", showlegend = FALSE, inherit = FALSE
          )
      }
    }


    # Correlations des variables : cor (1 ecart type, mais un peu decale /axes de depart)
    if ("cor" %in% type) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = dplyr::bind_rows(var_coords |>  dplyr::mutate(dplyr::across(tidyselect::starts_with("Dim."), ~ 0)),
                                  var_coords
          ) |>
            dplyr::mutate(id_pair = as.integer(as.factor(.data$name))) |>
            dplyr::group_by(.data$name) |>
            dplyr::mutate(
              name = dplyr::if_else(
                dplyr::row_number() != 1 & var_names_on == "cor",
                true  = paste0("<b>", .data$name, "</b>"),
                false = "")
              #dplyr::if_else(dplyr::row_number() == 1L, "<b>cor</b>", NA_character_)
            ) |> dplyr::ungroup(),
          scene = scene_name[i],
          x = ~eval(dim1)*9/10, y = ~eval(dim2)*9/10, z = ~eval(dim3)*9/10,
          split = ~ id_pair,
          line   = list(color  = "#0077c2", width = 5), # c("#42a5f5", "#0077c2")
          text = ~name, textfont = list(color = "#0077c2", size = 15), # "#4D4D4D"
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "lines+text", showlegend = FALSE, inherit = FALSE,
          hoverinfo = "skip")

      if (!D2) {
        dual_plots[[i]] <- dual_plots[[i]] |>
          plotly::add_trace(
            data = var_coords,
            scene = scene_name[i],
            x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ name,
            u = ~eval(dim1), v = ~eval(dim2), w = ~eval(dim3),
            sizeref = cone_size, sizemode = "absolute",
            colorscale = list(list(0, "#0077c2"), list(1, "#0077c2")), #autocolorscale = FALSE,
            showscale = FALSE, hoverinfo = "skip",
            # lighting  = list(ambient = 1), lightposition= list(x=0, y=0, z=1e5),
            type = "cone", anchor = "tip", showlegend = FALSE, inherit = FALSE
          )
      }
    }

    if (!D2 & "cor_sphere" %in% type) {
      #arg cor_sphere_resolution ?
      theta = seq(0, 2*pi, length.out = 15)
      phi   = seq(0,   pi, length.out = 15)

      sphere_data <- tibble::tibble(
        x = outer(cos(theta), sin(phi)) ,
        y = outer(sin(theta), sin(phi)) ,
        z = outer(rep(1, 15), cos(phi)),
      )
      ## alternative
      # theta = seq(0, pi, length.out = 25)
      # phi   = seq(-pi,   pi, length.out = 25)
      # meshgrid_sphere <- pracma::meshgrid(theta, phi) # outer works ?
      # theta <-  meshgrid_sphere[[1]] #|> c()
      # phi   <-  meshgrid_sphere[[2]] #|> c()
      # r = 1
      # sphere_data <- tibble::tibble(
      #   x = r * sin(theta) * cos(phi),
      #   y = r * sin(theta) * sin(phi),
      #   z = r * cos(theta),
      # )

      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = sphere_data,
          scene = scene_name[i],
          x = ~x, y = ~y, z = ~z,
          opacity = 0.1,
          colorscale = list(list(0, "#ba2d65"), list(1, "#ba2d65")), showscale = FALSE,
          type = "surface", showlegend = FALSE, inherit = FALSE, hoverinfo = "skip" #,
        )

    }


    # Axes principaux de l'ACP
    dual_plots[[i]] <- dual_plots[[i]] |>
      # plotly::add_trace(data = princ_axes,  scene = scene_name[i],
      #                   x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ pair_id,
      #                   line = list(color  = "#d32f2f", width = 5),
      #                   marker = list(color  = "#d32f2f",
      #                                 symbol = "cross",
      #                                 size = 5), # 3
      #                   type = "scatter3d", mode = "lines+markers", showlegend = FALSE, inherit = FALSE,
      #                   hoverinfo = "skip") |>
      # add_trace(data = princ_axes |> dplyr::filter(base_coord <= 1 & base_coord >= -1), #
      #         scene = scene_name[i],
      #         x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ pair_id,
    #         line   = list(color  = "#d32f2f", width = 5),
    #         type = "scatter3d", mode = "lines", showlegend = FALSE, inherit = FALSE,
    #         hoverinfo = "skip") |>
    plotly::add_trace(
      data = princ_axes |>
        dplyr::filter(.data$name %in% paste0("Axe ", axes) ) |>
        dplyr::group_by(.data$name) |>
        dplyr::slice(-dplyr::n()) |>
        dplyr::ungroup(),
      # dplyr::mutate(remove_last_if_not_1 = dplyr::row_number() == dplyr::n() & base_coord != 1) |>
      # dplyr::filter(!remove_last_if_not_1) |> dplyr::ungroup(),
      scene = scene_name[i],
      x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),
      marker = list(color  = "#d32f2f",
                    symbol = "cross",
                    size = 5), # 3
      text = ~base_coord, textfont = list(color = "#d32f2f", size = 10),
      textposition = "bottom center", hoverinfo = "skip",
      type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
      mode = 'markers+text', showlegend = FALSE, inherit = FALSE) |>
      plotly::add_trace(
        data = princ_axes |>
          dplyr::filter(.data$name %in% paste0("Axe ", axes) ) |>
          dplyr::group_by(.data$name) |> dplyr::slice(1, dplyr::n()) |>
          dplyr::mutate(
            name = dplyr::if_else(dplyr::row_number() == 1,
                                  true  = "",
                                  false  = paste0("<b>", .data$name, "</b>"))
          ) |>
          dplyr::ungroup(),
        scene = scene_name[i],
        x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ pair_id,
        line = list(color  = "#d32f2f", width = 5),
        text = ~name, textfont = list(color = "#d32f2f", size = 15),
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

          colorscale = list(list(0, "#d32f2f"), list(1, "#d32f2f")), #autocolorscale = FALSE,
          showscale = FALSE, hoverinfo = "skip",
          # lighting  = list(ambient = 1), lightposition= list(x=0, y=0, z=1e5),
          type = "cone", anchor = dplyr::if_else(max(princ_axes_print) == 1, "tip", "center"),
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


    # Valeurs singulieres (racine carree des valeurs propres)
    if ("vs" %in% type) {
      vs_diag <- diag(res.pca$svd$vs)
      dimnames(vs_diag) <- rep(list(paste0("Dim.", 1:length(res.pca$svd$vs))), 2L)
      vs_diag <- vs_diag |>
        as.data.frame() |> tibble::rownames_to_column("name") |> tibble::as_tibble() |>
        dplyr::mutate(name = str_replace(.data$name, "Dim.", "vs"))

      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = dplyr::bind_rows(vs_diag,
                                  vs_diag |>  dplyr::mutate(dplyr::across(tidyselect::starts_with("Dim."), ~ 0))
          ) |> dplyr::mutate(id_pair = as.integer(as.factor(.data$name)),
                             name    = dplyr::if_else(dplyr::row_number() == 1L, "<b>vs</b>", NA_character_)
          ),
          scene = scene_name[i],
          x = ~Dim.1*9/10, y = ~Dim.2*9/10, z = ~Dim.3*9/10, split = ~ id_pair,
          line   = list(color  = "#9e9d24", width = 10),
          text = ~name, textfont = list(color = "#9e9d24", size = 15),
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "lines+text", showlegend = FALSE, inherit = FALSE,
          hoverinfo = "skip")

      if (!D2) {
        dual_plots[[i]] <- dual_plots[[i]] |>
          plotly::add_trace(
            data = vs_diag,
            scene = scene_name[i],
            x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ name,
            u = ~eval(dim1), v = ~eval(dim2), w = ~eval(dim3),
            sizeref = cone_size, sizemode = "absolute",

            colorscale = list(list(0, "#9e9d24"), list(1,"#9e9d24")), #autocolorscale = FALSE,
            showscale = FALSE, hoverinfo = "skip",
            # lighting  = list(ambient = 1), lightposition= list(x=0, y=0, z=1e5),
            type = "cone", anchor = "tip", showlegend = FALSE, inherit = FALSE
          )
      }
    }

    # Point moyen et projections du point moyen sur les axes de depart
    if (!center) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = mean_projs,  scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ name,
          line   = list(color = "#f57c00", dash = "dash", width = 3), # "#bb4d00"
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "lines", showlegend = FALSE, inherit = FALSE,
          hoverinfo = "skip")
    }

    # Plan Axe 1/Axe 2 et projections des points
    if ("projections" %in% type) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = dplyr::bind_rows(ind_coords, dplyr::mutate(ind_coords, Dim.3 = 0)),
          scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3), split = ~ name,
          line   = list(color  = "#9575cd"), # dash = "longdash", width = 4  #( "dash" | "dashdot" | "dot" | "longdash" | "longdashdot" | "solid" )
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "lines", showlegend = FALSE, inherit = FALSE,
          hoverinfo = "skip") |>
        plotly::add_trace(
          data = dplyr::mutate(ind_coords, Dim.3 = 0), scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),
          marker   = list(color  = "#9575cd", size = 2),  # "#65499c"
          type = if (D2) {"scatter"} else {"scatter3d"},  # type = "scatter3d",
          mode = "markers", showlegend = FALSE, inherit = FALSE,
          hoverinfo = "skip")
    }


    if ("main_plan" %in% type) {
      dual_plots[[i]] <- dual_plots[[i]] |>
        plotly::add_trace(
          data = planDf, scene = scene_name[i],
          x = ~eval(dim1), y = ~eval(dim2), z = ~eval(dim3),
          opacity = 0.5, #color = "A", colorscale = c("A" = "#65499c"), #vertexcolor  = "#65499c",
          facecolor = rep('#CFC0E8', nrow(planDf)), # "#65499c"
          hoverinfo = "skip",
          type = "mesh3d", showlegend = FALSE, inherit = FALSE)
    }
  }

  # cat(
  # paste0("c(",
  #   paste0(
  #     paste0("'",
  #       c(material_colors_lighter(by = 0.25)[1],
  #       material_colors_lighter(by = 0.20)[1],
  #       material_colors_lighter(by = 0.15)[1],
  #       material_colors_lighter(by = 0.10)[1],
  #       material_colors_lighter(by = 0.05)[1]
  #     ),
  #     "'" #,
  #     ),
  #     collapse = ", "
  #   ),
  #   ")"
  #   )
  # )
  c('#DDD3EF', '#CFC0E8', '#C0ADE1', '#B29ADB', '#A388D4')


  # plotly::layout(# title = "Title",
  #   scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
  #                #              # domain=list(x=c(0,0.5),y=c(0.5,1),
  #                #              # aspectratio = list(x=1, y=1, z=2)))
  #                camera = list(
  #                  center = list(x =  0   , y =  0  , z = 0  ),
  #                  eye    = list(x =  0.6 , y = -0.4, z = 0.7),
  #                  up     = list(x =  0   , y =  0  , z = 1  ) # ,
  #                  #projection = list(type = 'orthographic')
  #                )
  #                #              #
  #                #              # dragmode = "turntable",
  #                #              # annotations =
  #                #
  #   ) #,
  #
  #   # scene2 = ,
  #   # margin = list(t = 30, r = 30, l = 30, b = 30, padding = 2)
  # )


  # dual_referential_3D_plot |>
  #   # Layout
  #   plotly::layout(# title = "Title",
  #          scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
  #                       #              # domain=list(x=c(0,0.5),y=c(0.5,1),
  #                       #              # aspectratio = list(x=1, y=1, z=2)))
  #                       camera = list(
  #                         center = list(x =  0   , y =  0  , z = 0  ),
  #                         eye    = list(x =  0.6 , y = -0.4, z = 0.7),
  #                         up     = list(x =  0   , y =  0  , z = 1  ) # ,
  #                         #projection = list(type = 'orthographic')
  #                       )
  #                       #              #
  #                       #              # dragmode = "turntable",
  #                       #              # annotations =
  #                       #
  #          ) #,
  #
  #          # scene2 = ,
  #          # margin = list(t = 30, r = 30, l = 30, b = 30, padding = 2)
  #   )



  # # Plan 1-2
  # dual_plots[[1]] |>
  #   plotly::layout(#title = "Title",
  #          scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
  #                       camera = list(
  #                         center = list(x =  0   , y =  0  , z = 0  ),
  #                         eye    = list(x =  0   , y =  0  , z = 1  ),
  #                         up     = list(x =  0   , y =  1  , z = 0  )
  #                       )
  #          )
  #   )
  #
  # # Plan 1-3
  # dual_plots[[2]] |>
  #   plotly::layout(#title = "Title",
  #          scene = list(xaxis = axx, yaxis = axy, zaxis = axz,
  #                       camera = list(
  #                         center = list(x =  0   , y =  0  , z = 0  ),
  #                         eye    = list(x =  0   , y = -1  , z = 0  ),
  #                         up     = list(x =  0   , y =  0  , z = 1  )
  #                       )
  #          )
  #   )
  #



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
    scenes <- purrr::map(
      camera_view,
      ~ list(xaxis = axes_params[[axes[1]]],
             yaxis = axes_params[[axes[2]]],
             zaxis = axes_params[[axes[3]]],
             aspectratio = aspectratio, aspectmode = "data",

             #aspectratio = list(x = 1, y = 1, z = 1),
             #domain = list(x = c(0, 0.5), y = c(0, 1)),
             camera = .x
      )
    )

  } else if (!D2) {
    scenes <- list("scene" = first_case(
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
    if (is.null(scenes$scene[[1]])) stop(paste0(
      "view argument must be among: ",
      paste0(
        paste0("'", c(plan12, plan13, plan23, plan123), "'"), collapse = ", "),
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


  final_plots$data$ind_coords         <- ind_coords
  final_plots$data$base_axis_in_princ <- base_axis_in_princ
  final_plots$data$princ_axes         <- princ_axes
  final_plots$data$mean_projs         <- mean_projs
  final_plots$data$planDf             <- planDf

  # No aspect ratio: a 3D scene has no 2D one to preserve, so a knitted iframe falls back on the
  # chunk's fig.width/fig.height.
  as_ggfacto_widget(final_plots)

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
