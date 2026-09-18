# PURPOSE: interactive_tooltips() -- the hover text behind every MCA level.
# ROLE: The feature the package exists for. Crosses the `active_tables` variables with the active
#   (and tooltip) variables in ONE tabxplor crosstab, renders its cells as HTML, and returns one row
#   per (vars, lvs) -- its weighted count, its header, its body -- for ggmca_data() to join onto the
#   coordinates.
# KEY CONSTRAINTS:
#   - The crosstabs are one tabxplor::tab() over the STACKED variables (stacked_crosstab()):
#     tabxplor's cost is per (row var x col var) pair, so 15 crossed variables are 15 pairs
#     instead of 225, for the same cells.
#   - A variable with no crosstab only needs its counts for the header line: it is counted, not
#     tabulated (level_counts()).
#   - The "Frequency" denominator is pop_wcount, the population. It is NOT any table's Total row,
#     which gave some levels a frequency above 100%.
#   - Padding relies on the str_pad() shim's exact stringr semantics: tooltip numbers align under
#     a monospace font, so a vector `width` and a non-space fill are both load-bearing.
#   - tabxplor fmt cells become <font color> through fmt_get_color_code(). Never reach into
#     tabxplor internals for the colour.
# See: CLAUDE.md section ggfacto architecture > The tooltip is the package.

#' @keywords internal
interactive_tooltips <- function(dat,
                                 sup_vars         = character(),
                                 active_tables    = character(),
                                 active_vars,
                                 tooltip_vars_1lv = character(),
                                 tooltip_vars     = character()
) {
  sup_list <- c(tooltip_vars_1lv, active_vars, tooltip_vars)
  vars     <- c(active_tables[!active_tables %in% sup_vars], sup_vars)
  crossed  <- vars[vars %in% active_tables]
  counted  <- vars[!vars %in% active_tables]

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

  # THE denominator of every "Frequency" line: the population, computed once before any binding.
  pop_wcount <- sum(dat[["row.w"]], na.rm = TRUE)

  # The central point is the Total row of the last crossed variable or, with no crosstab at all,
  # the total of the first counted one.
  tips <- dplyr::bind_rows(
    if (length(crossed) != 0) stacked_crosstab(dat, crossed, sup_list) |>
      dplyr::select(-tidyselect::any_of(active_vars_2levels)),
    if (length(counted) != 0) level_counts(dat, counted, central = length(crossed) == 0)
  ) |>
    dplyr::filter(.data$lvs != "Remove_levels") |>
    dplyr::mutate(vars = forcats::as_factor(.data$vars), lvs = forcats::as_factor(.data$lvs))

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

    dplyr::case_when(
      diff == 0 ~ paste0(colname, ": ", pct),

      diff >  0 ~ paste0(
        colname, ": ",
        "(", str_pad(paste0(
          "+" , abs(diff)), 3, pad = "@"),"%) ",
        pct),

      diff <  0 ~ paste0(
        colname, ": ",
        "(", str_pad(paste0(
          "-" , abs(diff)), 4, pad = "@"),"%) ",
        pct)
    ) |>
      str_replace_all(
        "@",
        paste0(unbrk, unbrk, collapse = "")
      )
  }

  tooltip_vars_1lv_levels <- purrr::map_chr(tooltip_vars_1lv, ~ levels(dat[[.]])[1])

  first_active <- levels(dat[[active_vars[[1]]]])
  first_active <- purrr::map(first_active, ~ c(., paste0(., "_", active_vars[[1]]))) |>
    purrr::flatten_chr()

  if (length(tooltip_vars) != 0) {
    tooltip_first_levels <-
      purrr::imap_dfr(dplyr::select(dat, tidyselect::all_of(tooltip_vars)),
                      ~ tibble::tibble(vars = .y, lvs = c(levels(.x)[1], paste0(levels(.x)[1], "_", .y) )))
    tooltip_first_levels <- purrr::set_names(tooltip_first_levels$vars, tooltip_first_levels$lvs)

  } else {
    tooltip_first_levels <- character()
  }

  tips <- tips |>
    dplyr::mutate(actives_text = dplyr::if_else(vars %in% active_tables,
                                                true  = "\n<b>Active variables:</b>",
                                                false = NA_character_)) |>
    dplyr::mutate(begin_text = paste0(
      "<b>", .data$lvs,"</b>",
      dplyr::if_else(.data$lvs != "Central point", true = paste0("\n", .data$vars), false = ""),
      "\nFrequency (n=", .data$n, "): ",
      paste0(format(round(.data$wcount / pop_wcount * 100, 0)), "%")
    ) ) |>
    dplyr::select(-"n") |>
    dplyr::select("vars", "lvs", "wcount", "begin_text",
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

  # DESIGN: the tooltip leaves here as a header and a body. The plot half inserts the contribution
  # lines of the axes it draws between them, the one part of a tooltip the data half cannot know.
  frags <- setdiff(names(tips), c("vars", "lvs", "wcount", "begin_text"))
  body  <- rep(NA_character_, nrow(tips))
  if (length(frags) != 0) {
    has_text <- rowSums(!is.na(tips[frags])) != 0
    body[has_text] <- tidyr::unite(tips[has_text, frags], "text", tidyselect::everything(),
                                   sep = "\n", na.rm = TRUE)$text
  }

  tibble::tibble(vars = tips$vars, lvs = tips$lvs, wcount = tips$wcount,
                 begin_text = tips$begin_text, interactive_text = body)
}


# Why this exists: one tab() for every crossed variable (see the header). Each individual appears
# once per variable, its level coded, never labelled, so nothing is parsed back.
stacked_crosstab <- function(dat, crossed, col_vars) {
  rows <- purrr::map(crossed, \(v) which(!is.na(dat[[v]])))
  lvls <- purrr::map(crossed, \(v) levels(as.factor(dat[[v]])))
  lookup <- tibble::tibble(vars = rep(crossed, lengths(lvls)), lvs = unlist(lvls),
                           code = paste0(".lv", seq_along(unlist(lvls))))
  offset <- cumsum(c(0L, lengths(lvls)))[seq_along(crossed)]
  codes  <- purrr::pmap(list(crossed, rows, offset),
                        \(v, r, o) o + as.integer(as.factor(dat[[v]])[r]))

  long <- vctrs::vec_slice(dat[unique(c(col_vars, "row.w"))], unlist(rows))
  long$.var <- factor(rep(crossed, lengths(rows)), crossed)
  long$.row <- factor(lookup$code[unlist(codes)], lookup$code)

  # WARNING: `tab_vars` is what keeps this equal to one tab() per variable: each block is compared
  #   to its OWN Total (comp = "tab"). One stacked Total averages the blocks, and a variable with
  #   missing values would then be measured against the wrong reference. A single block needs no
  #   split -- and tabxplor 2.0.1 fails on a one-level `tab_vars` with totaltab = "no".
  blocks <- if (length(crossed) > 1) ".var" else character()
  tabs <- withr::with_options(list(tabxplor.output_kable = FALSE), {
    tabxplor::tab(long,
                  row_vars = ".row",
                  col_vars = tidyselect::all_of(col_vars),
                  tab_vars = tidyselect::all_of(blocks),
                  wt       = "row.w",
                  na       = "drop",
                  pct      = "row",
                  color    = "difference",
                  totaltab = "no")
  }) |>
    dplyr::ungroup()
  if (length(blocks) == 0) tabs$.var <- crossed

  i <- match(as.character(tabs$.row), lookup$code)
  central <- is.na(i) & as.character(tabs$.var) %in% dplyr::last(crossed)
  keep <- !is.na(i) | central
  tabs <- tabs[keep, ]
  i    <- i[keep]
  central <- central[keep]

  n <- as.double(vctrs::field(tabs$Total, "n"))
  dplyr::bind_cols(
    tibble::tibble(vars = dplyr::if_else(central, "All",           lookup$vars[i]),
                   lvs  = dplyr::if_else(central, "Central point", lookup$lvs[i])),
    dplyr::select(tabs, -".var", -".row", -"Total"),
    tibble::tibble(n = n,
                   wcount = dplyr::coalesce(as.double(vctrs::field(tabs$Total, "wn")), n))
  )
}


# Why this exists: the header line of a variable with no crosstab needs its counts, not a table.
level_counts <- function(dat, counted, central = FALSE) {
  counts <- purrr::map(counted, function(v) {
    x  <- as.factor(dat[[v]])
    ok <- !is.na(x)
    tibble::tibble(vars   = v,
                   lvs    = levels(x),
                   n      = as.double(tabulate(as.integer(x[ok]), nlevels(x))),
                   wcount = as.vector(tapply(dat$row.w[ok], x[ok], sum, default = 0)))
  })
  counts <- dplyr::bind_rows(counts)
  first  <- counts[counts$vars == counted[1], ]
  counts <- counts[counts$n != 0, ]
  if (central) counts <- tibble::add_row(counts, vars = "All", lvs = "Central point",
                                         n = sum(first$n), wcount = sum(first$wcount))
  counts
}
