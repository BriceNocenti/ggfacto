# PURPOSE: The CA entry point and its graph -- correspondence_analysis(), ggca().
# ROLE: correspondence_analysis() is the ingress normaliser of a crosstab: it takes the table the
#   student made with tabxplor::tab(), and gives FactoMineR the counts it needs. ggca() is the CA
#   counterpart of ggmca(), but a single function: it rebuilds coordinates, tooltips and its colour
#   vector inline rather than going through a data/plot split.
# KEY CONSTRAINTS:
#   - A CA reads COUNTS, whatever the table displays: a tab() in percentages still gives its
#     weighted counts, and its Total row and column are dropped.
#   - Its tooltips are the row and column percentages of the source table, computed here. It does
#     NOT go through interactive_tooltips(), which is shaped for the MCA's Burt-table crosstabs.
#   - Like ggmca_plot(), it carries css_tooltip and height_width_ratio on the returned object as
#     ATTRIBUTES, so the object stays a real ggplot and the ggplot generics keep dispatching.
# See: CLAUDE.md section ggfacto architecture > How a graph is built, for why this is not split.

#' Correspondence Analysis of a Crosstab
#'
#' @description Makes the correspondence analysis of a crosstab with
#' \code{FactoMineR::\link[FactoMineR]{CA}}: first make the table with \code{tabxplor::tab()},
#' then analyse it. The analysis reads the (weighted) counts of the table, whatever it displays,
#' without its Total row and column. To leave out some rows or columns, filter the table before, with
#' \code{dplyr::filter()} and \code{dplyr::select()}.
#'
#' @param table A crosstab made with \code{tabxplor::tab()}, with one row variable and one column
#' variable. A matrix or a \code{table} of counts works too.
#' @param ncp The number of axes to keep. All of them by default.
#' @param ... Additional arguments to pass to \code{\link[FactoMineR]{CA}}.
#'
#' @return A `CA` object from \pkg{FactoMineR}, which remembers the names of the two variables, so
#' that \code{\link{ca_interpret}} can print them.
#' @export
#'
#' @examples
#' tableau <- tabxplor::tab(forcats::gss_cat, race, marital)
#' res.ca <- correspondence_analysis(tableau)
#' ca_interpret(res.ca)
#' ggca(res.ca) |> ggi()
correspondence_analysis <- function(table, ncp = Inf, ...) {
  if (inherits(table, "tabxplor_tab")) {
    cols <- names(table)[purrr::map_lgl(table, tabxplor::is_fmt) & !tabxplor::is_totcol(table)]
    rows <- !tabxplor::is_totrow(table)
    X <- vapply(cols, function(col) {
      x <- table[[col]]
      dplyr::coalesce(vctrs::field(x, "wn"), as.numeric(vctrs::field(x, "n")))
    }, numeric(nrow(table)))
    X <- X[rows, , drop = FALSE]
    rownames(X) <- as.character(table[[1]][rows])
    col_var <- unique(tabxplor::get_col_var(table)[cols])
    if (length(col_var) != 1L || !nzchar(col_var)) stop(
      "correspondence_analysis() needs a crosstab of ONE row variable and ONE column variable.",
      call. = FALSE)
    names(dimnames(X)) <- c(names(table)[1], col_var)
  } else {
    X <- as.matrix(table)
  }

  res <- FactoMineR::CA(X, ncp = ncp, graph = FALSE, ...)
  # WARNING: FactoMineR::CA() drops `names(dimnames())` from every matrix it keeps; they are written
  #   back on `call$X`, where ca_interpret() reads the names of the two variables.
  names(dimnames(res$call$X)) <- names(dimnames(X))
  res
}


#' Readable and Interactive graph for simple correspondence analysis
#' @description A readable, complete and beautiful graph for simple
#' correspondence analysis made with \code{FactoMineR::\link[FactoMineR]{CA}}.
#' Interactive tooltips, appearing when hovering on  points with mouse, allow to
#' keep in mind all the content of the table while reading the graph. Since it is
#' made in the spirit of \code{\link[ggplot2]{ggplot2}}, it is possible to change
#' theme or add another plot elements with +. Then, interactive
#' tooltips won't appear until you pass the result through \code{\link{ggi}}.
#'
#' @param res.ca An object created with \code{\link{correspondence_analysis}} or
#' \code{FactoMineR::\link[FactoMineR]{CA}}.
#' @param axes The axes to print, as a numeric vector of length 2.
#' @param show_sup When \code{TRUE} show supplementary rows and cols.
#' @param xlim,ylim Horizontal and vertical axes limits,
#' as double vectors of length 2.
#' @param out_lims_move When \code{TRUE}, the points out of \code{xlim} or
#'  \code{ylim} are not removed, but moved at the edges of the graph.
#' @param type Determines the way the two variables of the table are printed.
#'    \itemize{
#'    \item \code{"points"} : colored points with text legends
#'    \item \code{"text"} : colored text
#'    \item \code{"labels"} : colored labels
#'  }
#' @param text_repel By default, labels are moved so that they do not overlap, with
#'  \code{ggrepel::\link[ggrepel]{geom_text_repel}}. Set to \code{FALSE} to print each label
#'  exactly at its point, which is faster to draw.
#' @param uppercase Print \code{"row"} var or \code{"col"} var labels with
#' uppercase.
#' @param tooltips Choose the content of interactive tooltips at mouse hover :
#'  \code{"col"} for the table of columns percentages, \code{"row"} for line
#'  percentages, default to \code{c("row", "col")} for both.
#' @param rowtips_subtitle,coltips_subtitle The subtitles used before the table
#' in interactive tooltips.
#' @param rowcolor_numbers,colcolor_numbers If row var or col var levels are
#' prefixed with numbers(ex. : \code{"1-"} ), the number of digits to use
#' to create classes that will be used to add colors to points.
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like \code{"1-"}, and text in parentheses.
#' @param filter Regex patterns to discard levels of row or col variables.
#' @param title The title of the graph.
#' @param text_size Size of text.
#' @param dist_labels When \code{type = "points"}, the distance of text and
#' labels from points.
#' @param right_margin A margin at the right, in cm. Useful to read tooltips
#'  over points placed at the right of the graph without formatting problems.
#' @param size_scale_max Size of points.
#' @param use_theme By default, a specific \code{ggplot2} theme is used.
#' Set to \code{FALSE} to customize your own \code{\link[ggplot2:theme]{theme}}.
#'
#' @return A \code{\link[ggplot2:ggplot]{ggplot}} object to be printed in the
#' `RStudio` Plots pane. Possibility to add other gg objects with \code{+}.
#' Sending the result  through \code{\link{ggi}} will draw the
#' interactive graph in the Viewer pane using \code{\link[ggiraph]{girafe}}.
#' @export
#'
#' @examples
#' \donttest{
#' tableau <- tabxplor::tab(forcats::gss_cat, race, marital)
#' res.ca  <- correspondence_analysis(tableau)
#'
#' # Interactive plot :
#' graph.ca <- ggca(res.ca, title = "Race by marital status: correspondence analysis")
#' ggi(graph.ca) # to make the plot interactive
#' }
ggca <-
  function(res.ca, axes = c(1,2), show_sup = FALSE, xlim, ylim,
           out_lims_move = FALSE,
           type = c("points", "text", "labels"), text_repel = TRUE, uppercase = "col",
           tooltips = c("row", "col"),
           rowtips_subtitle = "Row pct", coltips_subtitle = "Column pct",
           rowcolor_numbers = 0, colcolor_numbers = 0, cleannames = TRUE, filter = "",
           title,
           text_size = 3.5, dist_labels = c("auto", 0.12), right_margin = 0,
           size_scale_max = 8, use_theme = TRUE) {  #, repel_max_iter = 10000

    dim1 <- rlang::sym(str_c("Dim ", axes[1])) #rlang::expr(eval(parse(text = paste0("`Dim ", axes[1],"`"))))
    dim2 <- rlang::sym(str_c("Dim ", axes[2])) #rlang::expr(eval(parse(text = paste0("`Dim ", axes[2],"`"))))


    #Lignes :
    row_coord <- res.ca$row$coord |> tibble::as_tibble(rownames = "lvs") |>
      dplyr::mutate(colorvar = "Active_row") |>
      dplyr::bind_rows(res.ca$row.sup$coord |>
                         tibble::as_tibble(rownames = "lvs") |>
                         dplyr::mutate(colorvar = "Sup_row") )
    row_coord <- row_coord  |>
      dplyr::bind_cols(freq = rowSums(res.ca$call$Xtot) / sum(rowSums(res.ca$call$Xtot))) |>
      dplyr::mutate(numbers = dplyr::case_when(
        str_detect(.data$lvs, "^[^- ]+-(?![[:lower:]])|^[^- ]+(?<![[:lower:]])-")
        ~ str_extract(.data$lvs, "^[^- ]+"),
        TRUE ~ "" ))

    # Remove words in parenthesis and numbers
    if (cleannames == TRUE) row_coord <- row_coord |>
      dplyr::mutate(lvs = str_remove_all(.data$lvs, cleannames_condition()))

    # Variable de couleur (colorvar) selon nb de caracteres indiques
    row_coord <- row_coord  |>
      dplyr::mutate(row_colorvar = as.factor(str_sub(.data$numbers, 1,
                                                              rowcolor_numbers)))
    row_colorvar_recode <- levels(row_coord$row_colorvar)
    names(row_colorvar_recode) <- str_c(1:nlevels(row_coord$row_colorvar))
    row_coord <- row_coord |>
      dplyr::mutate(row_colorvar = forcats::fct_recode(.data$row_colorvar,
                                                       !!!row_colorvar_recode)) |>
      dplyr::mutate(colorvar = ifelse(.data$colorvar == "Sup_row", .data$colorvar,
                                      str_c(.data$colorvar,
                                                     .data$row_colorvar))) |>
      dplyr::select(-"row_colorvar") |>
      # Afficher informations interactives au survol d'un point
      dplyr::mutate(interactive_text = str_c("<b>", .data$lvs, "</b>", "\n",
                                                      "Frequency: ",
                                                      round(.data$freq*100, 0), "%"),
                    lvs = str_replace_all(.data$lvs, "[^[:alnum:][:punct:]]",
                                                   " ") |> str_squish()  )

    if ("row" %in% tooltips) {
      #Calculer les % par ligne (de la variable colonne)
      row_frequencies <- res.ca$call$Xtot |> tibble::as_tibble() |>
        tibble::add_row(!!!colSums(res.ca$call$Xtot))
      row_frequencies <- row_frequencies |>
        dplyr::mutate_all(~ ./rowSums(row_frequencies)) |>
        dplyr::rename_all(~ str_remove_all(., cleannames_condition()))
      row_residuals <- row_frequencies |>
        dplyr::mutate_all(~ . - .[nrow(row_frequencies)]) |>
        dplyr::mutate_all(~ dplyr::case_when(
          round(.*100,0) >= 0 ~ str_c("+", round(.*100, 0), "%"),
          . < 0 ~ str_c(unbrk, #Unbreakable space
                                 "-", round(abs(.)*100, 0), "%")
        )) |> dplyr::slice(-nrow(row_frequencies))
      row_frequencies <- row_frequencies |>
        dplyr::slice(-nrow(row_frequencies)) |>
        dplyr::mutate_all(~ str_c(round(.*100, 0), "%")) |>
        dplyr::mutate_all(~dplyr::case_when(
          str_length(.) >= 3 ~ .,
          str_length(.) < 3 ~ str_c(
            unbrk, unbrk, . #2 unbreakable spaces
          ),
        ))
      row_frequencies <- row_frequencies |>
        dplyr::bind_rows(row_residuals) |>
        dplyr::mutate(number_of_rows = dplyr::row_number())
      row_frequencies <- row_frequencies |>
        dplyr::mutate_at(dplyr::vars(-"number_of_rows"), ~dplyr::case_when(
          number_of_rows > nrow(row_frequencies)/2 ~ NA_character_,
          TRUE ~ str_c("(",.[number_of_rows + nrow(row_frequencies)/2],") ", .),
        )) |>
        dplyr::slice(1:(nrow(row_frequencies)/2)) |> dplyr::select(-"number_of_rows")
      row_frequencies <- purrr::map_dfc(1:ncol(row_frequencies),
                                        ~dplyr::mutate_all(row_frequencies[.x],
                                                           function(.) str_c(colnames(row_frequencies)[.x], " : ", .)
                                        ))
      row_frequencies <- row_frequencies |>
        tidyr::unite("row_text", sep = "\n") |> dplyr::pull("row_text")
      row_coord <- row_coord |>
        dplyr::mutate(interactive_text = str_c(
          .data$interactive_text, "\n\n", rowtips_subtitle, " :\n", row_frequencies))
    }



    #Colonnes :
    col_coord <- res.ca$col$coord |> tibble::as_tibble (rownames = "lvs") |>
      dplyr::mutate(colorvar = "Active_col") |>
      dplyr::bind_rows(res.ca$col.sup$coord |>
                         tibble::as_tibble(rownames = "lvs") |>
                         dplyr::mutate(colorvar = "Sup_col") ) |>
      dplyr::bind_cols(freq = rowSums(t(res.ca$call$Xtot)) / sum(rowSums(t(res.ca$call$Xtot))))
    col_coord <- col_coord |>
      dplyr::mutate(numbers = dplyr::case_when(
        str_detect(.data$lvs, "^[^- ]+-(?![[:lower:]])|^[^- ]+(?<![[:lower:]])-")
        ~ str_extract(.data$lvs, "^[^- ]+"),
        TRUE ~ "" ))

    # Enlever les mots entre parentheses et les nombres
    if (cleannames == TRUE) col_coord <- col_coord |>
      dplyr::mutate(lvs = str_remove_all(.data$lvs, cleannames_condition()))

    # Variable de couleur (colorvar) selon nb de caracteres indiques
    col_coord <- col_coord |>
      dplyr::mutate(col_colorvar = as.factor(str_sub(.data$numbers, 1,
                                                              colcolor_numbers)))
    col_colorvar_recode <- levels(col_coord$col_colorvar)
    names(col_colorvar_recode) <- str_c(1:nlevels(col_coord$col_colorvar))
    col_coord <- col_coord |>
      dplyr::mutate(col_colorvar = forcats::fct_recode(.data$col_colorvar,
                                                       !!!col_colorvar_recode)) |>
      dplyr::mutate(colorvar = ifelse(.data$colorvar == "Sup_col", .data$colorvar,
                                      str_c(.data$colorvar, .data$col_colorvar))) |>
      dplyr::select(-"col_colorvar") |>
      # Afficher informations interactives au survol d'un point
      dplyr::mutate(interactive_text = str_c("<b>", .data$lvs, "</b>", "\n",
                                                      "Frequency: ",
                                                      round(.data$freq*100, 0), "%"),
                    lvs = str_replace_all(.data$lvs, "[^[:alnum:][:punct:]]",
                                                   " ") |> str_squish()
      )


    if ("col" %in% tooltips) {
      # Calculer les % par colonne (de la variable en ligne)
      col_frequencies <- res.ca$call$Xtot |> t() |> tibble::as_tibble() |>
        tibble::add_row(!!!rowSums(res.ca$call$Xtot))
      col_frequencies <- col_frequencies |> dplyr::mutate_all(~ ./rowSums(col_frequencies)) |>
        dplyr::rename_all(~ str_remove_all(., cleannames_condition()))
      col_residuals <- col_frequencies |>
        dplyr::mutate_all(~ . - .[nrow(col_frequencies)]) |>
        dplyr::mutate_all(~ dplyr::case_when(
          round(.*100,0) >= 0 ~ str_c("+", round(.*100, 0), "%"),
          . < 0 ~ str_c(unbrk, #unbreakable space
                                 "-", round(abs(.)*100, 0), "%")
        )) |> dplyr::slice(-nrow(col_frequencies))
      col_frequencies <- col_frequencies |>
        dplyr::slice(-nrow(col_frequencies)) |>
        dplyr::mutate_all(~ str_c(round(.*100, 0), "%")) |>
        dplyr::mutate_all(~dplyr::case_when(
          str_length(.) >= 3 ~ .,
          str_length(.) < 3 ~ str_c(
            unbrk, unbrk, .), #Two unbreakable spaces
        ))
      col_frequencies <- col_frequencies |>
        dplyr::bind_rows(col_residuals) |>
        dplyr::mutate(number_of_rows = dplyr::row_number())
      col_frequencies <- col_frequencies |>
        dplyr::mutate_at(dplyr::vars(-"number_of_rows"), ~dplyr::case_when(
          number_of_rows > nrow(col_frequencies)/2 ~ NA_character_,
          TRUE ~ str_c("(",.[.data$number_of_rows + nrow(col_frequencies)/2],") ", .),
        )) |>
        dplyr::slice(1:(nrow(col_frequencies)/2)) |> dplyr::select(-"number_of_rows")
      col_frequencies <- purrr::map_dfc(1:ncol(col_frequencies),
                                        ~ dplyr::mutate_all(col_frequencies[.x],
                                                            function(.) str_c(colnames(col_frequencies)[.x], " : ", .)
                                        ))
      col_frequencies <- col_frequencies |>
        tidyr::unite("col_text", sep = "\n") |> dplyr::pull("col_text")
      col_coord <- col_coord |>
        dplyr::mutate(interactive_text = str_c(
          .data$interactive_text, "\n\n", coltips_subtitle, " :\n", col_frequencies))
    }

    if (show_sup == FALSE) {
      row_coord <- row_coord  |>
        dplyr::filter(!str_detect(.data$colorvar, "Sup"))
      col_coord <- col_coord |>
        dplyr::filter(!str_detect(.data$colorvar, "Sup"))
    }


    # Le Central point et son texte interactive :
    col_freq_text <- rowSums(res.ca$call$Xtot) |>
      tibble::enframe(name = "lvs", value = "freq") |>
      dplyr::mutate(freq = str_c(round(.data$freq/sum(.data$freq)*100, 0), "%")) |>
      dplyr::mutate(lvs = str_remove_all(.data$lvs, cleannames_condition())) |>
      tidyr::unite("row_freq", sep = ": ") |>  dplyr::pull("row_freq") |>
      str_c(collapse = "\n")

    row_freq_text <- rowSums(t(res.ca$call$Xtot)) |>
      tibble::enframe(name = "lvs", value = "freq") |>
      dplyr::mutate(freq = str_c(round(.data$freq/sum(.data$freq)*100, 0), "%")) |>
      dplyr::mutate(lvs = str_remove_all(.data$lvs, cleannames_condition())) |>
      tidyr::unite("col_freq", sep = ": ") |> dplyr::pull("col_freq") |>
      str_c(collapse = "\n")

    mean_point_data <- row_coord |> dplyr::slice(1) |>
      dplyr::mutate_at(dplyr::vars(tidyselect::starts_with("Dim")), ~ 0) |>
      dplyr::mutate(lvs = NA_character_, freq = 1, colorvar = "Central_point",
                    numbers = NA_character_) |>
      dplyr::mutate(interactive_text = str_c(
        "<b>Central point</b>\nFrequency: ", str_c(.data$freq*100, "%")))

    #if ("row" %in% tooltips) {     }     if ("col" %in% tooltips) {        }
    mean_point_data <- mean_point_data |>
      dplyr::mutate(interactive_text = str_c(.data$interactive_text, "\n\n",
                                                      rowtips_subtitle, " :\n",
                                                      row_freq_text,
                                                      "\n\n", coltips_subtitle, " :\n",
                                                      col_freq_text))

    # Option pour afficher les lvs en majuscule (colonnes ou lignes) :
    if ("row" %in% uppercase) {
      row_coord <- row_coord  |>
        dplyr::mutate(lvs = str_to_upper(.data$lvs, locale = "en"))
    }
    if ("col" %in% uppercase) {
      col_coord <- col_coord |>
        dplyr::mutate(lvs = str_to_upper(.data$lvs, locale = "en"))
    }


    all_coord <- row_coord |>
      dplyr::bind_rows(col_coord) |>
      dplyr::mutate(colorvar = as.factor(.data$colorvar),
                    colorvar_names = as.factor(str_c("names_", .data$colorvar)),
                    id = dplyr::row_number()      )



    #Calculer les limites du graphique (argument a passer dans ggi pour regler la taille du htmlwidget)
    min_max_lims <- dplyr::select(all_coord, !!dim1, !!dim2)

    if (!missing(xlim)) min_max_lims <- min_max_lims |>  tibble::add_row(!!dim1 := xlim[1]) |> tibble::add_row(!!dim1 := xlim[2])
    if (!missing(ylim)) min_max_lims <- min_max_lims |>  tibble::add_row(!!dim2 := ylim[1]) |> tibble::add_row(!!dim2 := ylim[2])
    height_width_ratio <- min_max_lims |> dplyr::summarise_all(~ max(., na.rm = TRUE) - min(., na.rm = TRUE), .groups = "drop")
    min_max_lims <-
      dplyr::bind_rows(dplyr::summarise_all(min_max_lims, ~ min(., na.rm = TRUE), .groups = "drop"),
                       dplyr::summarise_all(min_max_lims, ~ max(., na.rm = TRUE), .groups = "drop"))
    width_range <- dplyr::pull(height_width_ratio, 1)[1]
    height_width_ratio <- height_width_ratio |> dplyr::summarise(height_width_ratio = !!dim2/!!dim1, .groups = "drop") |> tibble::deframe()
    if (dist_labels[1] == "auto") dist_labels <- width_range/50

    theme_acm_with_lims <-
      if (use_theme) {
        if (!missing(xlim) & !missing(ylim))  {

          theme_facto(res = res.ca, axes = axes, no_color_scale = TRUE, size_scale_max = size_scale_max,  # legend.position = "bottom",
                      xlim = c(xlim[1], xlim[2]), ylim = c(ylim[1], ylim[2]))
        }
        else if (!missing(xlim) ) {
          theme_facto(res = res.ca, axes = axes, no_color_scale = TRUE, size_scale_max = size_scale_max,  # legend.position = "bottom",
                      xlim = c(xlim[1], xlim[2]) )
        }
        else if (!missing(ylim) )  {
          theme_facto(res = res.ca, axes = axes, no_color_scale = TRUE, size_scale_max = size_scale_max,  # legend.position = "bottom",
                      ylim = c(ylim[1], ylim[2]))
        }
        else {
          theme_facto(res = res.ca, axes = axes, no_color_scale = TRUE, size_scale_max = size_scale_max)  # legend.position = "bottom",
        }
      } else {
        NULL
      }



    if (text_repel == FALSE | out_lims_move == FALSE) {
      if (!missing(xlim)) all_coord <- all_coord |> outlims(xlim, !!dim1)
      if (!missing(ylim)) all_coord <- all_coord |> outlims(ylim, !!dim2)
    }


    scale_color_named_vector <-
      c("Central_point" = "black",   # Material colors :
        "Active_col1" = "#3f51b5", # Indigo 500
        "Active_col2" = "#673ab7", # Deep purple 500
        "Active_col3" = "#1976d2", # Blue 700
        "Active_col4" = "#7b1fa2", # Purple 700
        "Active_row1" = "#43a047", # Green 600
        "Active_row2" = "#f57c00", # Orange 700
        "Active_row3" = "#c0ca33", # Lime 600
        "Active_row4" = "#f4511e", # Deep orange 600
        "Active_row5" = "#7cb342", # Light green 600
        "Active_row6" = "#e53935", # Red 600
        "Active_row7" = "#fbc02d", # Jaune 700
        "Active_row8" = "#26a69a", # Teal 400

        "Sup_col"    =  "#b0bec5", # Blue grey 200
        "Sup_row"    =  "#bcaaa4", # Brown 200

        "names_Point_moyen" = "black",
        "names_Active_col1" = "#000051", # Indigo 900 Dark
        "names_Active_col2" = "#000063", # Deep purple 900 Dark
        "names_Active_col3" = "#002171", # Blue 900 Dark
        "names_Active_col4" = "#12005e", # Purple 900 Dark
        "names_Active_row1" = "#00600f", # Green 700 Dark
        "names_Active_row2" = "#bb4d00", # Orange 700 Dark
        "names_Active_row3" = "#7c8500", # Lime 700 Dark
        "names_Active_row4" = "#ac0800", # Deep orange 700 Dark
        "names_Active_row5" = "#4b830d", # Light green 600 Dark
        "names_Active_row6" = "#ab000d", # Red 600 Dark
        "names_Active_row7" = "#c49000", # Jaune 700 Dark
        "names_Active_row8" = "#00766c", # Teal 400 Dark

        "names_Sup_col" = "#808e95", # Blue grey 200 Dark
        "names_Sup_row" = "#8c7b75" # Brown 200 Dark
      )


    if (!missing(title)) {
      title_graph <- ggplot2::labs(title = title) #str_c("Les Active variables de l'ACM sur les axes ",axes[1], " et ", axes[2] )
    } else {
      title_graph <- NULL
    }

    graph_mean_point <-
      ggiraph::geom_point_interactive(
        data = mean_point_data,
        ggplot2::aes(x = !!dim1, y = !!dim2, tooltip = .data$interactive_text),
        color = "black", shape = 3, size = 5, stroke = 1.5, fill = "black", na.rm = TRUE
      )

    graph_theme_acm <-
      list(theme_acm_with_lims,
           ggplot2::scale_colour_manual(values = scale_color_named_vector,
                                        aesthetics = c("colour", "fill")),
           ggplot2::theme(plot.margin = ggplot2::margin(r = right_margin, unit = "cm")),
           title_graph)


    #Sorties :
    if (type[1] == "points") {
      plot_output <- ggplot2::ggplot() + graph_theme_acm +
        ggrepel::geom_text_repel(
          data = all_coord,
          ggplot2::aes(x = !!dim1, y = !!dim2, label = .data$lvs,
                       color = .data$colorvar_names),
          size = text_size, hjust = "left", nudge_x = dist_labels, direction = "y",
          segment.colour = "black",
          segment.alpha = 0.2, point.padding = 0.25, na.rm = TRUE
        ) + #0.25, # min.segment.length = 0.8, max.iter = 10000 #repel_max_iter #fontface = "bold", max.iter = 50000
        ggiraph::geom_point_interactive(
          data = all_coord,
          ggplot2::aes(x = !!dim1, y = !!dim2, size = .data$freq,
                       color = .data$colorvar, shape = .data$colorvar,
                       tooltip = .data$interactive_text, #fill = .data$colorvar,
                       data_id = .data$id),
          stroke = 1.5, na.rm = TRUE
        ) +
        graph_mean_point +
        ggplot2::scale_shape_manual(values = c(
          #"Central_point" = 1,
          "Active_col1" = 17,
          "Active_col2" = 17,
          "Active_col3" = 17,
          "Active_col4" = 17,
          "Active_row1" = 18,
          "Active_row2" = 18,
          "Active_row3" = 18,
          "Active_row4" = 18,
          "Sup_col"    = 17,
          "Sup_row"    = 18  ))

      css_hover <- ggiraph::girafe_css("fill:gold;stroke:orange;",
                                       text = "color:gold4;stroke:none;")
      attr(plot_output, "css_hover") <- css_hover

    } else if (type[1] == "text") {
      if (text_repel == FALSE) {
        graph_text <-
          ggiraph::geom_text_interactive(data = all_coord,
                                         ggplot2::aes(x = !!dim1, y = !!dim2,
                                                      label   = .data$lvs,
                                                      color   = .data$colorvar,
                                                      tooltip = .data$interactive_text,
                                                      data_id = .data$id),
                                         size = text_size, fontface = "bold",  na.rm = TRUE)
      } else {
        graph_text <-
          ggrepel::geom_text_repel(data = all_coord,
                                   ggplot2::aes(x = !!dim1, y = !!dim2,
                                                label = .data$lvs,
                                                color = .data$colorvar),
                                   size = text_size, na.rm = TRUE, fontface = "bold",
                                   direction = "both", # segment.alpha = 0.5,# point.padding = 0.25, segment.colour = "black",
                                   min.segment.length = 0.4, arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines")))
      }
      plot_output <- ggplot2::ggplot() + graph_theme_acm + graph_text + graph_mean_point


    } else if (type[1] == "labels") {
      if (text_repel == FALSE) {
        graph_text <-
          ggiraph::geom_label_interactive(data = all_coord,
                                          ggplot2::aes(x = !!dim1, y = !!dim2,
                                                       label   = .data$lvs,
                                                       color   = .data$colorvar,
                                                       tooltip = .data$interactive_text,
                                                       data_id = .data$id),
                                          size = text_size, fontface = "bold",  na.rm = TRUE)
      } else {
        graph_text <-
          ggrepel::geom_label_repel(data = all_coord,
                                    ggplot2::aes(x = !!dim1, y = !!dim2,
                                                 label = .data$lvs,
                                                 color = .data$colorvar),
                                    size = text_size, na.rm = TRUE, fontface = "bold",
                                    direction = "both", # segment.alpha = 0.5,# point.padding = 0.25, segment.colour = "black",
                                    min.segment.length = 0.5, arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines")))
      }
      plot_output <- ggplot2::ggplot() + graph_theme_acm + graph_text + graph_mean_point
    }

    # DESIGN: render hints ride on the ggplot object as attributes, read back by ggi() and
    # ggsave2(). They must NOT be list slots: append() would flatten the S7 ggplot into a plain
    # list and the ggplot generics would stop dispatching on it. See R/mca-plot.R for the twin.
    css_tooltip <- "text-align:right;padding:4px;border-radius:5px;background-color:#eeeeee;color:black;" #
    attr(plot_output, "css_tooltip")        <- css_tooltip
    attr(plot_output, "height_width_ratio") <- height_width_ratio
    return(plot_output)
  }
