# PURPOSE: stacked_crosstab() -- one tabxplor::tab() over STACKED row variables, kept as a lead for
#   speeding up tabxplor itself. ggfacto no longer calls it: its tooltips are built from the answer
#   profiles (R/tooltips.R).
# ROLE: tabxplor's cost is per (row variable x column variable) pair, ~30 ms whatever the rows. Each
#   individual here appears once per crossed variable, its level coded, and `tab_vars` splits the
#   table back into one block per variable, each compared to its OWN Total: 15 pairs instead of
#   225 for 15 variables, for byte-identical cells. A tab() that stacked its own row variables this
#   way would gain the same factor.
# To try it: devtools::load_all(); source("dev/stacked_crosstab.R"); `dat` needs a `row.w` column.

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
