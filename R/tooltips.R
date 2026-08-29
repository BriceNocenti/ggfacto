# PURPOSE: interactive_tooltips() -- the hover text behind every MCA point.
# ROLE: The feature the package exists for. Builds one tabxplor crosstab per variable and renders
#   its cells as HTML, keyed by (vars, lvs) for ggmca_data() to join onto the coordinates.
# KEY CONSTRAINTS:
#   - The "Frequency" denominator is pop_wcount, the population. It is NOT the last row of the
#     bound tables, which gave some levels a frequency above 100%.
#   - Padding relies on the str_pad() shim's exact stringr semantics: tooltip numbers align under
#     a monospace font, so a vector `width` and a non-space fill are both load-bearing.
#   - tabxplor fmt cells become <font color> through fmt_get_color_code(). Never reach into
#     tabxplor internals for the colour, as an earlier version did.
# See: CLAUDE.md section ggfacto architecture > The tooltip is the package.

#' @keywords internal
interactive_tooltips <- function(dat,
                                 sup_vars         = character(),
                                 active_tables    = character(),
                                 active_vars,
                                 #active_vars_data,
                                 tooltip_vars_1lv = character(),
                                 tooltip_vars     = character()#,
                                 #excl             = character(),
                                 #cleannames       = FALSE
) {
  sup_list <- c(tooltip_vars_1lv, active_vars, tooltip_vars)
  vars <- c(active_tables[!active_tables %in% sup_vars], sup_vars)
  #c(sup_vars[!sup_vars %in% active_tables], sup_vars) #vars <- c(active_vars, sup_vars)


  #Tooltip vars with only the first level kept
  if (length(tooltip_vars_1lv) != 0) {
    tooltip_vars_1lv_3levels <-
      purrr::map_lgl(dat, ~ nlevels(.) >= 3) &
      colnames(dat) %in% tooltip_vars_1lv
    if (any(tooltip_vars_1lv_3levels)) dat <- dat |>
        dplyr::mutate_if(tooltip_vars_1lv_3levels,
                         ~ forcats::fct_other(
                           .,
                           keep = levels(.)[1],
                           other_level = "Other_levels"
                         ))
    dat <- dat |>
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(tooltip_vars_1lv),
        ~ forcats::fct_recode(., rlang::splice(purrr::set_names(levels(.)[-1], "Remove_levels")))
      ))
  }


  #To remove the second level when active vars just have two
  active_vars_2levels <-
    purrr::map_lgl(dat[active_vars],
                   ~ is.factor(.) & length(levels(.)[levels(.) != "Remove_levels"]) == 2)

  active_vars_2levels <- names(active_vars_2levels[active_vars_2levels])

  active_vars_2levels <- dplyr::select(dat, tidyselect::all_of(active_vars_2levels)) |>
    purrr::map(~ levels(.)[2]) |>
    purrr::imap(~ c(.x, paste0(.y, "_", .x))) |>
    purrr::flatten_chr()

  tabs <- rep(list(NULL), length(vars))

  # THE denominator of every "Frequency" line: the population, not `last(wcount)` -- the last row of
  # the bound tables is whatever the last variable's last level happens to be, which gave a level a
  # frequency above 100%.
  pop_wcount <- sum(dat[["row.w"]], na.rm = TRUE)


  # One table per variable: `output_list = TRUE` is tab_many()'s old shape, asked for by name (tab()
  # merges several row variables into one table by default).
  # Each branch hands on the SAME two plain numbers, `n` and `wcount`, taken from wherever that
  # table keeps its base -- the Total column when there are crosstabs, the count columns when there
  # are none. Reading them after the bind cannot work: the two shapes have no column in common.
  if (any(vars %in% active_tables)) {
    tabs_active_tables <-
      withr::with_options(list(tabxplor.output_kable = FALSE), {
        tabxplor::tab(dat,
                      row_vars    = tidyselect::all_of(vars[vars %in% active_tables]),
                      col_vars    = tidyselect::all_of(sup_list),
                      wt          = "row.w",
                      na          = "drop",
                      pct         = "row",
                      color       = "difference",
                      output_list = TRUE)
      })

    tabs[vars %in% active_tables] <- tabs_active_tables |>
      purrr::map(
        ~ dplyr::rename_with(., ~ "lvs", 1) |>
          dplyr::select(-tidyselect::any_of(active_vars_2levels)) |>
          dplyr::filter(!.data$lvs == "Remove_levels") |>
          dplyr::mutate(
            n      = as.double(vctrs::field(.data$Total, "n")),
            wcount = dplyr::coalesce(as.double(vctrs::field(.data$Total, "wn")), .data$n)
          ) |>
          dplyr::select(-tidyselect::any_of("Total"))
      )
  }

  if (any(!vars %in% active_tables)) {
    tabs_no_active_tables <-
      withr::with_options(list(tabxplor.output_kable = FALSE), {
        tabxplor::tab(dat,
                      row_vars    = tidyselect::all_of(vars[!vars %in% active_tables]),
                      wt          = "row.w",
                      na          = "drop",
                      pct         = "col",
                      output_list = TRUE)
      })

    tabs[!vars %in% active_tables] <- tabs_no_active_tables |>
      purrr::map(
        ~ dplyr::rename_with(., ~ "lvs", 1) |>
          dplyr::filter(!.data$lvs == "Remove_levels") |>
          dplyr::mutate(
            wcount = dplyr::coalesce(as.double(vctrs::field(.data$n, "wn")),
                                     as.double(vctrs::field(.data$n, "n"))),
            n      = as.double(vctrs::field(.data$n, "n"))
          ) |>
          dplyr::select(-tidyselect::any_of(c("wn", "pct")))
      )
  }

  tabs <- purrr::set_names(tabs, vars)

  # tabxplor renames a level that collides with one of the data's own column names, appending "_lv"
  # (a level named "breakfast" of a variable named "breakfast"). The plot's `lvs` never sees that
  # rename, so the join below would drop every such level's tooltip: undo it on the exact levels it
  # can have applied to.
  unlv <- function(x) {
    x <- as.character(x)
    hit <- str_detect(x, "_lv$") & str_remove(x, "_lv$") %in% names(dat)
    dplyr::if_else(hit, str_remove(x, "_lv$"), x)
  }
  # WARNING: through fct_relabel(), so `lvs` stays a FACTOR -- everything downstream picks the
  #   tooltip pieces out by `is.character()`, and a character `lvs` would be nested away with them.
  tabs <- purrr::map(tabs, ~ if (is.null(.)) . else
                             dplyr::mutate(., lvs = forcats::fct_relabel(factor(.data$lvs), unlv)))

  # tabs <- purrr::map_if(
  #   vars, vars %in% active_tables,
  #   ~ withr::with_options(list(tabxplor.output_kable = FALSE), {
  #     tabxplor::tab_many(dat, !!rlang::sym(.), sup_list[sup_list != .],
  #                        na = "drop", wt = "row.w", pct = "row", color = "diff") %>%
  #       dplyr::rename_with(~ "lvs", 1) %>%
  #       dplyr::select(-tidyselect::starts_with("Remove_levels"),
  #                     -tidyselect::any_of(active_vars_2levels)) %>%
  #       dplyr::filter(!.data$lvs == "Remove_levels")
  #   }),
  #
  #   .else =
  #     ~ withr::with_options(list(tabxplor.output_kable = FALSE), {
  #       tabxplor::tab(dat, !!rlang::sym(.), na = "drop", wt = "row.w", pct = "col") %>% #tot = c("row", "col")
  #         dplyr::rename_with(~ "lvs", 1) %>%
  #         dplyr::select(-any_of("n")) %>%
  #         dplyr::filter(!.data$lvs == "Remove_levels")
  #     })
  #
  # ) %>%
  #   purrr::set_names(vars)

  # sup_vars_count <-
  #   purrr::map(tabs, ~ dplyr::mutate(dplyr::select(., lvs, Total),
  #                                    Total = vctrs::field(.data$Total, "wn") ))

  #col_vars_levels <- purrr::map(tabs, ~ tabxplor::tab_get_vars(.)$col_vars_levels)

  # tooltip_first_levels <- col_vars_levels %>%
  #   purrr::map(~ .[names(.) %in% tooltip_vars] %>% purrr::map(dplyr::first) %>%
  #                purrr::flatten_chr()
  #   )


  # active_first_variable <- col_vars_levels %>%
  #   purrr::map(~ .[names(.) %in% active_vars] %>%
  #                dplyr::first() %>% dplyr::first()
  #   )

  # color_code_vector <- function(var) {
  #   color_selection <- tabxplor:::fmt_color_selection(var) %>% purrr::map(which)
  #
  #   color_styles <- tabxplor:::select_in_color_style(length(color_selection))
  #   color_styles <- tabxplor:::get_color_style("color_code", type = "text", theme = "light")[color_styles]
  #
  #   color_positions <- color_selection %>%
  #     purrr::map2(color_styles, ~ purrr::set_names(.x, str_to_upper(.y))) %>%
  #     purrr::flatten_int()
  #
  #   no_color <- 1:length(var)
  #   no_color <- purrr::set_names(no_color[!no_color %in% color_positions], NA_character_)
  #
  #   names(sort(c(color_positions, no_color)))
  # }
  # #replace by tabxplor::fmt_get_color_code()

  format_pct <- function(diff, pct, colname, color_code) {
    pct <- str_pad(pct, 3, pad = "@")

    pct[!is.na(color_code)] <-
      str_c("<font color=\"",
                     color_code[!is.na(color_code)],
                     "\">",
                     "<b>",
                     pct[!is.na(color_code)],
                     "</b>",
                     "</font>"
      )

    pct[is.na(color_code)] <- paste0(unbrk, pct[is.na(color_code)])

    #pct <- paste0("\t", pct)

    dplyr::case_when(
      diff == 0 ~ paste0(colname, ": ", pct), #"\n"

      diff >  0 ~ paste0(
        colname, ": ", #"\n",
        "(", str_pad(paste0(
          "+" , abs(diff)), 3, pad = "@"),"%) ",
        pct),

      diff <  0 ~ paste0(
        colname, ": ", #"\n",
        "(", str_pad(paste0(
          "-" , abs(diff)), 4, pad = "@"),"%) ",
        pct)
    ) |>
      str_replace_all(
        "@",
        paste0(unbrk, unbrk, collapse = "")
      ) #|>
  }


  tooltip_vars_1lv_levels <- purrr::map_chr(tooltip_vars_1lv, ~ levels(dat[[.]])[1])

  first_active <- levels(dat[[active_vars[[1]]]])
  first_active <- purrr::map(first_active, ~ c(., paste0(., "_", active_vars[[1]]))) |>
    purrr::flatten_chr()

  last_var_with_active_tables <- tidyr::replace_na(
    1:length(vars) != dplyr::last(which(vars %in% active_tables)),
    FALSE
  )

  if (length(tooltip_vars) != 0) {
    tooltip_first_levels <-
      purrr::imap_dfr(dplyr::select(dat, tidyselect::all_of(tooltip_vars)),
                      ~ tibble::tibble(vars = .y, lvs = c(levels(.x)[1], paste0(levels(.x)[1], "_", .y) )))
    tooltip_first_levels <- purrr::set_names(tooltip_first_levels$vars, tooltip_first_levels$lvs)

  } else {
    tooltip_first_levels <- character()
  }


  interactive_text <-
    purrr::imap(tabs,
                ~ dplyr::mutate(.x, vars = factor(.y)) |>
                  dplyr::relocate(.data$vars, .before = 1)
    ) |>
    purrr::map_if(last_var_with_active_tables, ~ dplyr::filter(., lvs != "Total")) |>
    dplyr::bind_rows()



  # interactive_text %>%
  #   dplyr::mutate(dplyr::across(
  #     where(tabxplor::is_fmt),
  #     ~ format_pct(diff       = round(vctrs::field(., "diff") * 100, 0),
  #                  pct        = format(.),
  #                  colname    = dplyr::cur_column(),
  #                  color_code = color_code_vector(.))
  #   )) %>%
  #   dplyr::select(1:4)
  #


  interactive_text <- interactive_text |>
    dplyr::mutate(vars = dplyr::if_else(str_detect(.data$lvs, "^Total"),
                                        true  = factor("All", c(levels(.data$vars), "All")),
                                        false = .data$vars),

                  lvs  = dplyr::if_else(str_detect(.data$lvs, "^Total"),
                                        true  = factor("Central point", c(levels(.data$lvs), "Central point")),
                                        false = .data$lvs)
    )

  # `n` / `wcount` are already plain numbers, normalised per table above.

  interactive_text <- interactive_text |>
    dplyr::mutate(actives_text = dplyr::if_else(vars %in% active_tables,
                                                true  = "\n<b>Active variables:</b>",
                                                false = NA_character_)) |>
    dplyr::mutate(begin_text = paste0(
      "<b>", .data$lvs,"</b>",
      dplyr::if_else(.data$lvs != "Central point", true = paste0("\n", .data$vars), false = ""),
      "\nFrequency (n=", .data$n, "): ",
      paste0(format(round(.data$wcount / pop_wcount * 100, 0)), "%")
    ) ) |>
    dplyr::select(-.data$n) |>
    dplyr::select(.data$vars, .data$lvs, .data$wcount, .data$begin_text,
                  tidyselect::any_of(tooltip_vars_1lv_levels),
                  tidyselect::any_of("actives_text"),
                  tidyselect::any_of(first_active),
                  tidyselect::everything()) |>
    dplyr::mutate(dplyr::across(
      where(tabxplor::is_fmt),
      ~ format_pct(diff       = round(vctrs::field(., "diff") * 100, 0),
                   pct        = format(.),
                   colname    = dplyr::cur_column(),
                   color_code = tabxplor::fmt_get_color_code(.))
    )) |>
    dplyr::mutate(dplyr::across(
      tidyselect::any_of(names(tooltip_first_levels)),
      ~ dplyr::if_else(.data$vars %in% active_tables,
                       true  = paste0("\n<b>Distribution by ",
                                      tooltip_first_levels[dplyr::cur_column()],
                                      ":</b>" , "\n", .),
                       false = NA_character_
      )
    ))

  interactive_text
}
