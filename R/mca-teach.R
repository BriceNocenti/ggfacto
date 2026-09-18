# PURPOSE: The two pedagogical MCA plots -- ggmca_initial_dims() and ggmca_with_base_ref().
# ROLE: They exist to show what MCA does geometrically, not to report a result: one draws the
#   active variables in their initial reference frame, the other draws that frame inside the
#   space the analysis built. They are each other's converse and cross-reference each other.
# KEY CONSTRAINTS:
#   - Both are only readable on a handful of variables at a time; `keep =` is the intended usage,
#     not a refinement.
#   - ggmca_initial_dims() plots x2 against x1, where the x* columns are one per level of a
#     variable GROUP. A battery of binary variables never yields an x2, so it is defaulted before
#     the branch rather than inside one of them.
#   - ggmca_with_base_ref() takes `data` in second position for consistency with the rest of the
#     ggmca_* family, and forwards it; it draws only active variables, read from res.mca, so the
#     argument changes no output. A bare numeric there is routed to `axes` for back-compatibility.
# See: CLAUDE.md section What ggfacto is, and why.

#' Plot Initial Dimensions (Active Variables) of Multiple Correspondence Analysis
#'
#' @description
#' This function mostly have an educational value : it shows the
#' initial dimensions of the Multiple Correspondence Analysis (active variables)
#' in their initial reference frame. It shows the n dimensional space before the
#' analysis is done. To see initial dimensions axes in the space built by the
#' analysis (principal axes), use \code{\link[ggfacto]{ggmca_with_base_ref}}.
#'
#' @param res.mca An object created with \code{\link{multiple_correspondence_analysis}} or
#' \code{FactoMineR::\link[FactoMineR]{MCA}}.
#' @param data Optionally, the data frame the analysis was made on, whose order of levels is then
#' used.
#' @param proj_just Horizontal justification of text of the coordinates on axes,
#' as a character vector of length 2 (x and y).
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like \code{"1-"}, and text in parentheses.
#' @param keep A character vector of the name of active variables to keep.
#'
#' @return A \code{\link[ggplot2:ggplot]{ggplot}} object to be printed in the
#' `RStudio` Plots pane. Possibility to add other gg objects with \code{+}.
#' Sending the result through \code{\link{ggi}} will draw the
#' interactive graph in the Viewer pane using \code{\link[ggiraph]{girafe}}.
#' @export
#'
#' @examples
#' \donttest{
#' data(tea, package = "FactoMineR")
#' res.mca <- multiple_correspondence_analysis(tea, 1:18)
#' ggmca_initial_dims(res.mca, tea)
#' }
ggmca_initial_dims <- function(res.mca, data, proj_just = c(1.5, 2),
                               cleannames = TRUE, keep = NULL) {

  mca_excl_done <- names(res.mca$call$Xtot)[res.mca$call$excl]

  row.w <- res.mca$call$row.w

  active_vars <- str_c(colnames(res.mca$call$X)[1:length(res.mca$call$quali)])

  # the order of the levels is the data's own; a missing answer is named as MCA2() names it
  has_data <- !missing(data)
  active_var_levels <- purrr::map(active_vars, function(v) {
    x <- if (has_data) data[[v]] else res.mca$call$X[[v]]
    levels(forcats::fct_na_value_to_level(as.factor(x), str_c(v, ".NA")))
  }) |>
    purrr::set_names(active_vars) |>
    purrr::imap_dfr(~ tibble::tibble(vars = .y, lvs2 = .x))  |>
    dplyr::mutate(vars = forcats::as_factor(.data$vars))

  active_var_levels_disordered <-
    purrr::map(active_vars, ~ dplyr::pull(res.mca$call$X, .) |>
                 as.factor() |> levels()) |>
    purrr::set_names(active_vars) |>
    purrr::imap_dfr(~ tibble::tibble(
      vars = .y,
      lvs  = .x,
      lvs2 = str_remove_all(.x, paste0("^", .y, "_") ),
    )) |>
    dplyr::mutate(vars = forcats::as_factor(.data$vars))

  active_var_levels <- active_var_levels |>
    dplyr::left_join(active_var_levels_disordered,
                     by = c("vars", "lvs2"),
                     relationship = "one-to-one") |>
    dplyr::filter(!.data$lvs %in% mca_excl_done) |>
    dplyr::group_by(.data$vars) |>
    dplyr::group_split() |>
    (\(g) purrr::set_names(g, purrr::map_chr(g, ~ as.character(dplyr::first(.$vars)))))() |>
    purrr::map(~ .$lvs)


  if(length(keep) > 0) active_var_levels <- active_var_levels |>
    purrr::keep(names(active_var_levels) %in% keep)

  active_var_levels_not_zero <-
    active_var_levels |>
    purrr::imap_dfr(~ tibble::tibble(vars = .y,
                                     lvs = sort(.x, decreasing = TRUE),
                                     #rn  = length(.x):1
    )
    ) |>
    dplyr::mutate(vars = forcats::as_factor(.data$vars)) |>
    dplyr::group_by(.data$vars) |>
    dplyr::mutate(max_lv = dplyr::n(),
                  vars_group = ceiling((dplyr::row_number()-1)/2) ) |>
    dplyr::ungroup()


  active_var_level0 <-
    active_var_levels_not_zero |>
    dplyr::filter(.data$vars_group == 0) |>
    dplyr::mutate(max_group = ceiling((.data$max_lv-1)/2)) |>
    dplyr::group_by(.data$vars) |>
    dplyr::group_split() |>
    purrr::map_dfr(~ tibble::tibble(vars = .$vars,
                                    lvs = .$lvs,
                                    vars_group = 1:.$max_group,
                                    max_lv = .$max_lv)
    )

  active_var_levels_not_zero <- active_var_levels_not_zero |>
    dplyr::filter(.data$vars_group != 0)

  active_var_level_grouped <- active_var_level0 |>
    dplyr::bind_rows(active_var_levels_not_zero) |>
    dplyr::arrange(.data$vars)

  active_var_level_grouped <-
    active_var_level_grouped |>
    dplyr::mutate(vars_group = paste0(
      .data$vars, " (",
      dplyr::case_when(
        .data$max_lv <= 3   ~ "",

        .data$vars_group == 1 ~ paste0(.data$vars_group*2 - 1, "-",
                                       pmin(.data$vars_group*2 + 1, .data$max_lv)),

        .data$vars_group*2 == .data$max_lv ~ paste0(.data$max_lv),

        TRUE ~ paste0(.data$vars_group*2, "-", .data$vars_group*2 + 1)
      ),
      ")"

    ) |>
      str_remove(" *\\(\\)$")
    ) |>
    dplyr::mutate(vars_group = forcats::as_factor(.data$vars_group)) |>
    dplyr::group_by(.data$vars_group) |>
    dplyr::group_split() |>
    (\(g) purrr::set_names(g, purrr::map_chr(g, ~ as.character(dplyr::first(.$vars_group)))))() |>
    purrr::map(~ list(vars = .$vars[1], vars_group = .$vars_group[1], lvs = .$lvs))



  # Table disjonctive
  disj <-
    purrr::pmap(active_var_level_grouped |> purrr::transpose(),
         ~ {
           disj <- res.mca$call$Xtot |>
             dplyr::select(tidyselect::all_of(..3) ) |>
             tibble::as_tibble() |>
             tibble::add_column(row.w = row.w)
           # disj <- dplyr::select(disj, tidyselect::all_of(..3) )

           if (cleannames) {
             disj <- disj |>
               dplyr::rename_with(~ str_remove_all(., cleannames_condition()))
           }

           disj |>
             dplyr::group_by(!!!rlang::syms(names(disj)[names(disj) != "row.w"])) |>
             dplyr::summarise(n = dplyr::n(),
                              wn = sum(row.w, na.rm = TRUE),
                              .groups = "drop") |>
             dplyr::mutate(freq = .data$wn/sum(.data$wn, na.rm = TRUE) ) |>
             dplyr::filter(!dplyr::if_all(-tidyselect::all_of(c("n", "wn", "freq")), ~ . == 0L)) |> # Remove NA line (only zeros)
             dplyr::rowwise() |>
             dplyr::mutate(lvs = which(dplyr::c_across(tidyselect::everything()) == "1") |>
                             dplyr::first()) |>
             dplyr::ungroup() |>
             (\(d) dplyr::mutate(d, lvs = names(d)[.data$lvs]))() |>
             dplyr::rename_with(~ paste0("x", 0:(length(.)-1)),
                                .cols = -tidyselect::all_of(c("n", "wn", "lvs", "freq"))) |>
             dplyr::mutate(vars = ..1, vars_group = ..2, .before = 1)

           #dplyr::mutate(dplyr::across(tidyselect::starts_with("x"), ~ . * n / sum(n), .names = "mean_{.col}"))


         }
    )
  # disj[c("VIDEOS", "MUSIQUE",  "LIVRES (1-3)", "LIVRES (4)")]

  # Point moyen (barycentre)
  disj <-
    purrr::imap_dfr(
      disj,
      ~  {
        mean <- tibble::tibble(name = paste0("mean_x", (nrow(.x)-1):0),
                               freq = .x$freq
        ) |>
          tidyr::pivot_wider(names_from = "name", values_from = "freq") |>
          dplyr::mutate(vars_group = .y, .before = 1)

        .x |>
          dplyr::left_join(mean, by = "vars_group", relationship = "many-to-one")
      }
    ) |>
    dplyr::mutate(dplyr::across(
      tidyselect::starts_with(c("x", "mean_x")),
      ~ tidyr::replace_na(., 0)
    )) |>
    dplyr::mutate(vars_group = forcats::as_factor(.data$vars_group)) |>
    dplyr::select("vars", "vars_group", "lvs", tidyselect::starts_with("x"),
                  "n", "freq", tidyselect::starts_with("mean_x"))

  #  disj |> dplyr::filter(vars %in% c("VIDEOS", "MUSIQUE", "LIVRES")) |> new_tab() |> dplyr::group_by(vars)

  # WARNING: the x* columns are one per level of a variable GROUP, so a battery of binary variables
  # never produces an x2 -- and both branches below plot x2 against x1. Default it before branching.
  if (! "x2" %in% names(disj)) disj <- disj |> dplyr::mutate(x2 = 0, mean_x2 = 0)

  if (length(unique(disj$vars_group)) > 1) {
    #(
    disj |>
      ggplot2::ggplot(
        ggplot2::aes(x = .data$x2, y = .data$x1,
                     group = .data$vars_group, color = .data$vars)) +
      ggplot2::geom_polygon(
        ggplot2::aes(fill = .data$vars), color = NA, alpha = 0.1) +
      ggplot2::geom_segment(
        ggplot2::aes(xend = .data$x2 , yend = .data$x1), x = 0, y = 0,
        linewidth = 0.75, linetype = "dashed"
      ) +
      ggiraph::geom_point_interactive(
        ggplot2::aes(size    = .data$n,
                     tooltip = paste0(round(.data$freq*100), "%",
                                      " (n=", .data$n, ")"))
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(x = dplyr::if_else(.data$x0 == 0, .data$x2 * .data$freq, NA),
                     y = .data$x1 * .data$freq,
                     xend = .data$mean_x2, yend = .data$mean_x1),
        na.rm = TRUE, color =  "black", linetype = "dashed") +
      ggplot2::geom_point(
        data = disj |>
          dplyr::group_by(.data$vars_group) |>
          dplyr::slice(1) |>
          dplyr::ungroup(),
        ggplot2::aes(x = .data$mean_x2, y = .data$mean_x1),
        color = "black", fill = "#eeeeee", shape = 3, size = 5, stroke = 1.5
      ) +
      ggrepel::geom_text_repel(
        ggplot2::aes(label = .data$lvs),
        hjust = "inward", size = 3, color = "black", #point.padding = 0.1,
        point.size = NA, nudge_x = 0.075, min.segment.length = Inf,
        direction = "y" #, force = 0.5, force_pull = 1,
      ) +
      # ggplot2::geom_text(data = disj |> dplyr::group_by(vars_group) |> dplyr::slice(1) |> dplyr::ungroup(),
      #             ggplot2::aes(x = mean_x1, y = mean_x2),
      #           label = "Mean point (weighted barycenter)",
      #           nudge_y = 0.06, size = 5, color = "black") +
      ggplot2::geom_text(data = disj |> dplyr::filter(!(.data$x1 == 0 & .data$x2 == 0)), # Projections
                         ggplot2::aes(x     = .data$x2 * .data$freq,
                                      y     = .data$x1 * .data$freq,
                                      hjust = dplyr::if_else(.data$x2 == 1, 0.5, proj_just[1]),
                                      vjust = dplyr::if_else(.data$x2 == 1, proj_just[2], 0.5),
                                      label = round(.data$freq, 2) ),
                         na.rm = TRUE,  color = "black", fontface = "bold") +
      # geom_label(ggplot2::aes(label = paste0("n=", n, " (", round(freq*100), "%)")),
      #            hjust = 0, nudge_x = 0.1, size = 3,
      #            fontface = "bold") +
      ggplot2::scale_x_continuous("", breaks = seq(0, 1, 0.25), lim = c(-0.4, 1.20) ) +
      ggplot2::scale_y_continuous("", breaks = seq(0, 1, 0.25), lim = c(-0.2, 1.10) ) +
      ggplot2::scale_size_area(max_size = 8, guide = "none") +
      ggplot2::scale_color_discrete(guide = "none") +
      ggplot2::scale_fill_discrete(guide = "none") +
      ggplot2::facet_wrap(ggplot2::vars(.data$vars_group)) + # NO .data$ ?
      # coord_fixed() +
      # coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     panel.spacing = ggplot2::unit(0, "cm")
                     #panel.grid.major = ggplot2::element_line(color = "grey80", linetype = "dotted")
      )  #) |>
    #ggi()


    # Juste une variable
  } else {

    #print(disj)
    #print(nrow(disj) > 2)
    #     print(if(nrow(disj) > 2) {seq(0, 1, 0.25)} else {0}
    # )

    disj |>
      ggplot2::ggplot(
        ggplot2::aes(x = .data$x2, y = .data$x1, group =.data$ vars_group,
                                   color = .data$vars)) +
      ggplot2::geom_polygon(ggplot2::aes(fill = .data$vars), color = NA, alpha = 0.1) +
      ggplot2::geom_segment(
        ggplot2::aes(xend = .data$x2 , yend = .data$x1), x = 0, y = 0,
        linewidth = 0.75, linetype = "dashed"
      ) +
      ggplot2::geom_point(ggplot2::aes(size = .data$n)) +
      ggplot2::geom_segment(
        ggplot2::aes(x = dplyr::if_else(.data$x0 == 0, .data$x2 * .data$freq, NA),
                     y = .data$x1 * .data$freq,
                     xend = .data$mean_x2, yend = .data$mean_x1),
        na.rm = TRUE, color =  "black", linetype = "dashed"
      ) +
      ggplot2::geom_point(
        data = disj |>
          dplyr::group_by(.data$vars_group) |>
          dplyr::slice(1) |>
          dplyr::ungroup(),
        ggplot2::aes(x = .data$mean_x2, y = .data$mean_x1),
        color = "black", fill = "#eeeeee", shape = 3, size = 5, stroke = 1.5
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = .data$lvs), nudge_y = 0.06, size = 5, color = "black"
      ) +
      ggplot2::geom_text(
        data = disj |>
          dplyr::group_by(.data$vars_group) |>
          dplyr::slice(1) |>
          dplyr::ungroup(),
        ggplot2::aes(x = .data$mean_x2, y = .data$mean_x1),
        label = "Mean point (weighted barycenter)",
        nudge_y = 0.06, size = 5, color = "black"
      ) +
      ggplot2::geom_text(
        data = disj |> dplyr::filter(!(.data$x1 == 0 & .data$x2 == 0)),
        ggplot2::aes(x     = .data$x2 * .data$freq,
                     y     = .data$x1 * .data$freq,
                     hjust = dplyr::if_else(.data$x2 == 1, 0.5, proj_just[1]),
                     vjust = dplyr::if_else(.data$x2 == 1, proj_just[2], 0.5),
                     label = round(.data$freq, 2) ),
        na.rm = TRUE,  color = "black", fontface = "bold"
      ) +
      ggplot2::geom_label(
        ggplot2::aes(label = paste0(round(.data$freq*100), "%",
                                    "( n=", .data$n, ")")),
        hjust = 0, nudge_x = 0.035, size = 4,
        fontface = "bold"
      ) +
      ggplot2::scale_x_continuous(
        "", breaks = if(nrow(disj) > 2) {seq(0, 1, 0.25)} else {0},
        lim = if(nrow(disj) > 2) {c(-0.15, 1.20)} else {c(-0.5, 0.5)}
      ) +
      ggplot2::scale_y_continuous("", breaks = seq(0, 1, 0.25) ) +
      ggplot2::scale_size_area(max_size = 12, guide = "none") +
      ggplot2::scale_color_discrete(guide = "none") +
      ggplot2::scale_fill_discrete(guide = "none") +
      ggplot2::theme_minimal()

  }

}





#' Plot Initial Dimensions (Active Variables) on a Multiple Correspondence Analyses
#'
#' @description This function mostly have an educational value : it shows the
#' initial dimensions of the Multiple Correspondence Analysis (active variables)
#' in the space built by the analysis (principal axes). To see initial
#' dimensions in their initial reference frame, use \code{\link[ggfacto]{ggmca_initial_dims}}.
#' @param res.mca An object created with \code{FactoMineR::\link[FactoMineR]{MCA}}.
#' @param data The data the analysis was made on. Optional: this graph draws only
#' active variables, which are read from `res.mca`, so it changes nothing. It is
#' accepted so that every `ggmca_*` function takes `(res.mca, data)`.
#' @param axes The axes to print, as a numeric vector of length 2.
#' @param keep A character vector of the name of active variables to keep.
#'
#' @return A \code{\link[ggplot2:ggplot]{ggplot}} object to be printed in the
#' `RStudio` Plots pane. Possibility to add other gg objects with \code{+}.
#' Sending the result through \code{\link{ggi}} will draw the
#' interactive graph in the Viewer pane using \code{\link[ggiraph]{girafe}}.
#' @export
#'
#' @examples
#' \donttest{
#' data(tea, package = "FactoMineR")
#' res.mca <- multiple_correspondence_analysis(tea, 1:18)
#' ggmca_with_base_ref(res.mca)
#'
#' # It is more readable to select just a few active variables
#' lv2_vars <- dplyr::select(tea[1:18], where(~ nlevels(.) == 2)) |> names()
#' ggmca_with_base_ref(res.mca, keep = lv2_vars)
#'
#' lv3_vars <- dplyr::select(tea[1:18], where(~ nlevels(.) == 3)) |> names()
#' ggmca_with_base_ref(res.mca, keep = lv3_vars)
#'
#' lv4_vars <- dplyr::select(tea[1:18], where(~ nlevels(.) == 4)) |> names()
#' ggmca_with_base_ref(res.mca, keep = lv4_vars)
#'
#' lv6_vars <- dplyr::select(tea[1:18], where(~ nlevels(.) == 6)) |> names()
#' ggmca_with_base_ref(res.mca, keep = lv6_vars)
#' }
ggmca_with_base_ref <- function(res.mca, data, axes = c(1, 2),
                                keep = NULL) {

  # DESIGN: `data` took second position in 0.4.0 so every ggmca_* entry point reads
  # (res.mca, data, ...). A pre-0.4.0 call passed `axes` positionally there; a bare numeric is
  # never microdata and a data frame is never an `axes`, so the two are told apart safely.
  if (!missing(data) && is.numeric(data) && !is.data.frame(data) && length(data) <= 3) {
    axes <- renamed_arg(data, "axes in 2nd position", "axes =", "ggmca_with_base_ref")
    data <- NULL
  }
  if (missing(data)) data <- NULL

  dim1 <- rlang::sym(str_c("Dim ", axes[1]))
  dim2 <- rlang::sym(str_c("Dim ", axes[2]))

  active_vars <-
    str_c(colnames(res.mca$call$X)[1:length(res.mca$call$quali)])


  active_var_levels <-
    purrr::map(active_vars, ~ dplyr::pull(res.mca$call$X, .) |>
                 as.factor() |> levels()) |>
    purrr::set_names(active_vars) |>
    purrr::imap_dfr(~ tibble::tibble(vars = .y, lvs = .x))

  freqs <- tibble::enframe(res.mca$call$marge.col * length(active_vars),
                           "lvs", "freq")
  freqs <- active_var_levels |> dplyr::left_join(freqs, by = "lvs") |>
    dplyr::mutate(lvs = str_remove_all(.data$lvs,
                                                cleannames_condition()))


  vars_data <- (if (is.null(data)) ggmca_data(res.mca) else
                  ggmca_data(res.mca, data))$vars_data
  acm_orga_from_base_ref <- vars_data |>
    dplyr::filter(.data$color_group == "active_vars")

  if(length(keep) > 0) acm_orga_from_base_ref <- acm_orga_from_base_ref |>
    dplyr::filter(.data$vars %in% keep)

  acm_orga_from_base_ref <- acm_orga_from_base_ref |>
    dplyr::mutate(vars = forcats::fct_drop(.data$vars))


  acm_orga_from_base_ref <- acm_orga_from_base_ref |>
    dplyr::left_join(freqs, by = c("vars", "lvs")) |>
    dplyr::mutate(lvs = forcats::as_factor(.data$lvs)) |> # to keep order
    dplyr::group_by(.data$vars) |>
    dplyr::arrange(dplyr::desc(.data$lvs), .by_group = TRUE) |>
    dplyr::select(
      tidyselect::everything() & -tidyselect::starts_with("Dim "),
      tidyselect::all_of(c(as.character(dim1), as.character(dim2)))
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("Dim "),
        ~ dplyr::if_else(dplyr::row_number() != 1, dplyr::first(.), NA),
        .names = "start_{.col}"
      ),


      dplyr::across(
        tidyselect::starts_with("Dim "), # right angles of vars vectors vars vects
        ~ dplyr::if_else(
          dplyr::row_number() != 1,
          true  = (.*1/16 + dplyr::first(.) * 15/16),
          false = NA_real_),
        .names = "start_angle_{.col}"
      ),


      dplyr::across(
        tidyselect::starts_with("Dim "), # projections of mean point on vars vects
        ~ dplyr::if_else(
          dplyr::row_number() != 1,
          true  = (.*.data$freq + dplyr::first(.) * (1-.data$freq)),
          #(.*freq + dplyr::first(.) * dplyr::first(freq))/(freq + dplyr::first(freq)),
          false = NA_real_),
        .names = "proj_{.col}"
      ),
    ) |>
    dplyr::rename_with(~str_remove(., "_Dim "),
                       .cols = tidyselect::starts_with("proj_Dim")) |>
    dplyr::rename_with(~str_remove(., "_Dim "),
                       .cols = tidyselect::starts_with("start_angle_Dim")) |>
    dplyr::mutate(
      ang_x = .data$start_angle1 - dplyr::first(!!dim1),  # 2 left 3 down
      ang_y = .data$start_angle2 - dplyr::first(!!dim2),  # 2 left 3 down

      ang_ld_x =  dplyr::nth(.data$start_angle1, 2L) +
        dplyr::nth(.data$ang_x, 3L) - dplyr::first(!!dim1), # left down

      ang_ld_y =  dplyr::nth(.data$start_angle2, 2L) +
        dplyr::nth(.data$ang_y, 3L) - dplyr::first(!!dim2), # left down

      # right angle on zero var
      start_angle12_x = dplyr::nth(.data$start_angle1, 2L) + dplyr::nth(.data$ang_x, 3L),

      start_angle12_y = dplyr::nth(.data$start_angle2, 2L) + dplyr::nth(.data$ang_y, 3L),


      # # right angles on mean point
      # moy_angle1 = dplyr::if_else(
      #          dplyr::row_number() != 1,
      #          true  = 0 - ang_x,
      #          false = NA_real_),
      #
      # moy_angle2 = dplyr::if_else(
      #          dplyr::row_number() != 1,
      #          true  = 0 - ang_y,
      #          false = NA_real_),
      #
      # moy_angle12_x = dplyr::nth(moy_angle1, 2L) - dplyr::nth(ang_x, 3L),
      #
      # moy_angle12_y = dplyr::nth(moy_angle2, 2L) - dplyr::nth(ang_y, 3L),


      # right angle on projections
      proj_angle_x = dplyr::if_else( #
        dplyr::row_number() != 1,
        true  = .data$proj1 - .data$ang_x,
        false = NA_real_),

      proj_angle_y = dplyr::if_else(
        dplyr::row_number() != 1,
        true  = .data$proj2 - .data$ang_y,
        false = NA_real_),


      proj_angle_b_x = dplyr::if_else(
        dplyr::row_number() != 1,
        true  = .data$proj1 + dplyr::if_else(dplyr::row_number() == 2,
                                       true  = dplyr::nth(.data$ang_x, 3),
                                       false = dplyr::nth(.data$ang_x, 2) ),
        false = NA_real_),

      proj_angle_b_y = dplyr::if_else(
        dplyr::row_number() != 1,
        true  = .data$proj2 + dplyr::if_else(dplyr::row_number() == 2,
                                       true  = dplyr::nth(.data$ang_y, 3),
                                       false = dplyr::nth(.data$ang_y, 2) ),
        false = NA_real_),

      proj_angle_c_x = dplyr::if_else(
        dplyr::row_number() != 1,
        true  = .data$proj_angle_x + dplyr::if_else(dplyr::row_number() == 2,
                                              true  = dplyr::nth(.data$ang_x, 3),
                                              false = dplyr::nth(.data$ang_x, 2) ),
        false = NA_real_),

      proj_angle_c_y = dplyr::if_else(
        dplyr::row_number() != 1,
        true  = .data$proj_angle_y + dplyr::if_else(dplyr::row_number() == 2,
                                                    true  = dplyr::nth(.data$ang_y, 3),
                                                    false = dplyr::nth(.data$ang_y, 2) ),
        false = NA_real_),

    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      "vars", "lvs", "freq", #"wcount",
      "Dim 1", "Dim 2", "start_Dim 1", "start_Dim 2", "proj1", "proj2",
      tidyselect::everything() & -tidyselect::any_of(
        c("color_group", "id", "clust_id", "interactive_text", "face")
      )
    )


  keep_has_names <- all(!is.null(names(keep)))

  color_scale <-  if (keep_has_names) {
    ggplot2::scale_color_manual(values = purrr::set_names(names(keep), keep),
                                aesthetics = c("colour", "fill"),
                                na.value = "grey70" )

  } else if (length(unique(acm_orga_from_base_ref$vars)) <= 12) {
    ggplot2::scale_color_manual(values = purrr::set_names(material_colors_light(), NULL),
                                aesthetics = c("colour", "fill"),
                                na.value = "grey70")
  } else {
    ggplot2::scale_color_discrete(aesthetics = c("colour", "fill"),
                                  na.value = "grey70")
  }


  acm_orga_from_base_ref |>
    ggplot2::ggplot(ggplot2::aes(x = !!dim1, y = !!dim2)) +
    theme_facto(res.mca, no_color_scale = TRUE) +
    #acm_orga_1_clust$graph_theme_acm +
    ggplot2::geom_point(
      data = tibble::tibble(!!dim1 := 0, !!dim2 := 0),
      color = "black", fill = "#eeeeee", shape = 3, size = 5,
      stroke = 1.5, na.rm = TRUE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = !!dim1, yend = !!dim2   ,
                   x = .data$`start_Dim 1` , y = .data$`start_Dim 2`,
                   color = .data$vars, group = .data$vars),
      linewidth = 1, arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "lines")), na.rm = TRUE
    ) +
    ggplot2::geom_segment( # projections
      ggplot2::aes(xend = .data$proj1 , yend = .data$proj2,
                   color = .data$vars, group = .data$vars),
      x = 0, y = 0, linewidth = 0.5, linetype = "dashed",
    ) +
    #   ggplot2::geom_segment(data = mid_point_test, # projections
    #   ggplot2::aes(color = vars, group = vars),
    #   xend = 0, yend = 0, size = 0.5, linetype = "dashed",
    # ) +
    ggplot2::geom_segment( # right angle on vars vectors
      ggplot2::aes(xend = .data$start_angle12_x , yend = .data$start_angle12_y,
                   x = .data$start_angle1 , y = .data$start_angle2,
                   color = .data$vars, group = .data$vars),
      linewidth = 0.5
    ) +
    #   ggplot2::geom_segment( # right angle on mean point
    #   ggplot2::aes(xend = .data$moy_angle12_x , yend = .data$moy_angle12_y,
    #                x = .data$moy_angle1 , y = .data$moy_angle2,
    #                color = .data$vars, group = .data$vars),
    #   size = 0.5
    # ) +

    ggplot2::geom_segment( # right angle on projections
      ggplot2::aes(xend = .data$proj_angle_x, yend = .data$proj_angle_y,
                   x = .data$proj_angle_c_x , y = .data$proj_angle_c_y,
                   color = .data$vars, group = .data$vars),
      linewidth = 0.5
    ) +
    ggplot2::geom_segment( # right angle on projections
      ggplot2::aes(xend = .data$proj_angle_b_x , yend = .data$proj_angle_b_y,
                   x = .data$proj_angle_c_x , y = .data$proj_angle_c_y,
                   color = .data$vars, group = .data$vars),
      linewidth = 0.5
    ) +
    ggplot2::geom_polygon(
      ggplot2::aes(fill = .data$vars, group = .data$vars),
      linewidth = 0.5, color = NA, alpha = 0.2,
    ) +
    ggrepel::geom_label_repel(
      ggplot2::aes(x = dplyr::if_else(!!dim1 > 0, !!dim1 + 0.03, !!dim1 - 0.03),
                   label = .data$lvs, fontface = "bold", color = .data$vars),
      size = 3, na.rm = TRUE, direction = "y", fill = grDevices::rgb(1, 1, 1, alpha = 0.7),
      min.segment.length = Inf,
      force = 0.5, force_pull = 1, point.padding = 0.1, box.padding = 0, hjust = "outward"
    ) +
    color_scale

}
