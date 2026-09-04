# PURPOSE: the two tables that describe the DATA rather than interpret an axis -- mean_sd_tab() and
#   HCPC_tab().
# ROLE: what the variables look like before the analysis, and what a cluster is made of. The
#   interpretation tables of the analyses themselves are in R/interpret.R.
# KEY CONSTRAINTS:
#   - Tables are tabxplor's job: both build tabxplor::fmt() columns, and both hand back a summary of
#     the family described in ?ggfacto_summary -- one table, the format decided at print time.
#   - HCPC_tab() reads a cluster column of the data, never an HCPC object: a number enters as a MEAN
#     column and is transposed into a mean row, a factor needs no transposition at all.
# See: CLAUDE.md section ggfacto architecture > Tables are tabxplor's.



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
              legend = paste("sd/mean: the coefficient of variation, i.e. the standard deviation as a",
                             "percentage of the mean -- comparable between variables measured in",
                             "different units. Void where the mean is not strictly positive."))
}














#' Multiple Tables for Hierarchical Clusters
#'
#' @description
#' One table describing every cluster: each variable's levels down the page, the clusters across it,
#' and a colour saying at a glance which levels a cluster is made of. Numeric variables come in as
#' mean rows, and the last two rows give each cluster's share of the population and its size.
#'
#' @param data A data frame.
#' @param row_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> The variables to describe the
#' clusters with, typically the active variables of the MCA. Numeric ones become mean rows.
#' @param clust The variable with the clusters, typically made with hierarchical
#' clustering functions like \code{\link[FactoMineR]{HCPC}} (object
#' `res$data.clust$clust`). Can be either a symbol or a character vector of
#' length 1 (for vars in `data`), or an external variable (not in `data`)
#' provided its length is equal to the number of rows of `data`.
#' @param wt The name of the weight variable. Leave empty for unweighted results.
#' @param excl The name of the levels to exclude, as a character vector.
#' @param color The colour measure, see \code{\link[tabxplor]{tab}}. With `"difference"` (the
#' default) the mean rows stay uncoloured: a difference of means and a difference of percentages
#' have no ladder in common. Use `"ratio"` to colour every row, means included.
#' @param pct `"col"` (default) reads each cluster as a distribution: of the people in this cluster,
#' what percentage are in this level. `"row"` reads each level as a distribution across clusters.
#' @param row_tot The name of the row giving each cluster's share of the population.
#' @param ... Additional arguments to pass to \code{\link[tabxplor]{tab}}.
#'
#' @return A \code{tabxplor} table --- see [ggfacto_summary] for how it prints.
#' @export
#' @seealso [ggfacto_summary], [mca_interpret()].
#'
#'@examples
#'
#' data(tea, package = "FactoMineR")
#' res.mca_3axes <- MCA2(tea, active_vars = 1:18, ncp = 3)
#' cah <- FactoMineR::HCPC(res.mca_3axes, nb.clust = 6, graph = FALSE)
#' tea$clust <- cah$data.clust$clust
#' HCPC_tab(tea, row_vars = all_of(names(tea)[1:18]), clust = "clust") #|>
#' #tabxplor::tab_export()
#'
HCPC_tab <- function(data, row_vars = character(), clust, wt,
                     excl = character(),
                     color = "difference", pct = "col",
                     row_tot = "% of population",
                     ...) {

  row_vars <- tidyselect::eval_select(rlang::enquo(row_vars), data)
  row_vars <- names(row_vars)

  if (missing(wt)) {
    wt <- character()
  } else {
    wt <- as.character(rlang::ensym(wt))
  }

  clust <- rlang::enquo(clust)

  safe_clust <- purrr::safely(rlang::eval_tidy)(clust)

  if (is.null(safe_clust$error)) {
    clust_is_var <- (is.factor(safe_clust$result) |
                       is.character(safe_clust$result)) &
      length(safe_clust$result) == nrow(data)

  } else {
    clust_is_var <- FALSE
  }

  if (clust_is_var) {
    data <- data |>
      dplyr::select(tidyselect::all_of(row_vars), tidyselect::all_of(wt) ) |>
      levels_to_na(tidyselect::all_of(row_vars), excl = excl,
                   levels_to = "Remove levels") |>
      tibble::add_column(clust = safe_clust$result )

  } else {
    data <- data |> dplyr::select(tidyselect::all_of(row_vars),
                                  tidyselect::all_of(wt),
                                  clust = !!clust ) |>
      levels_to_na(tidyselect::all_of(row_vars), excl = excl,
                   levels_to = "Remove levels")
  }

  if (length(wt) == 0) {
    wt <- rlang::expr(NA)
  } else {
    wt <- rlang::sym(wt)
  }

  # A NUMBER cannot be a row of levels: it enters as a MEAN column and is transposed into a mean row.
  # A FACTOR is built the way round the table is read -- levels down, clusters across -- so it needs
  # no transposition at all: tabxplor stacks several row variables into one table by itself.
  is_num   <- purrr::map_lgl(dplyr::select(data, tidyselect::all_of(row_vars)), is.numeric)
  fct_vars <- row_vars[!is_num]
  num_vars <- row_vars[is_num]

  blocks <- list()

  if (length(fct_vars) != 0) {
    fct_tab <- tabxplor::tab(data,
                             row_vars   = tidyselect::all_of(fct_vars),
                             col_vars   = "clust",
                             wt         = !!wt,
                             pct        = pct,
                             tot        = "col",
                             na         = "drop",
                             cleannames = TRUE,
                             color      = color,
                             ...) |>
      dplyr::ungroup()

    # A BINARY variable says everything in one row: the second level is the first one upside down.
    binary  <- names(which(purrr::map_lgl(
      dplyr::select(data, tidyselect::all_of(fct_vars)),
      ~ nlevels(as.factor(.)) == 2L
    )))
    var_col <- as.character(fct_tab$row_var)
    fct_tab <- fct_tab[!(var_col %in% binary) | !duplicated(var_col), , drop = FALSE]

    blocks <- c(blocks, list(fct_tab))
  }

  if (length(num_vars) != 0) {
    num_tab <- tabxplor::tab(data,
                             row_vars = "clust",
                             col_vars = tidyselect::all_of(num_vars),
                             wt       = !!wt,
                             na       = "drop",
                             color    = color) |>
      tabxplor::tab_transpose(name = "levels") |>
      dplyr::mutate(row_var = as.character(.data$levels), .before = 1)

    blocks <- c(blocks, list(num_tab))
  }

  # The two summary rows are a fact about the CLUSTERS alone -- their share of the population and
  # their size -- so they come from their own one-variable table, transposed once.
  # They are DISPLAY rows, not data: `row_kind` says so, and they carry no comparison (a display row
  # has nothing to be a deviation FROM), so the colour engine leaves them alone. They also carry the
  # table's own scale and colour measure, because binding a column that claims something else would
  # reconcile BOTH away -- the level rows would come out `mixed` and uncoloured.
  pop_tab <- tabxplor::tab(data, row_vars = "clust", wt = !!wt, pct = "all") |>
    tabxplor::tab_transpose(name = "levels")
  pop_tab   <- dplyr::filter(pop_tab, as.character(.data$levels) %in% c("pct", "n"))
  pop_kinds <- as.character(pop_tab$levels)          # "pct" / "n" -- already the row_kind vocabulary
  pop_tab <- pop_tab |>
    dplyr::mutate(
      row_var = "Total",
      .before = 1,
      dplyr::across(
        where(tabxplor::is_fmt),
        ~ dplyr::mutate(., diff = NA_real_, ratio = NA_real_) |>
          tabxplor::set_row_kind(pop_kinds) |>
          tabxplor::set_scale("level_pct") |>
          tabxplor::set_pct_type(pct) |>
          tabxplor::set_color(color)
      ),
      levels = forcats::fct_recode(factor(pop_kinds),
                                   !!!purrr::set_names(c("pct", "n"), c(row_tot, "n")))
    )
  blocks <- c(blocks, list(pop_tab))

  out <- purrr::reduce(blocks, dplyr::bind_rows) |>
    dplyr::filter(!str_detect(as.character(.data$levels), "Remove levels")) |>
    dplyr::mutate(dplyr::across(c("row_var", "levels"), ~ forcats::as_factor(as.character(.)))) |>
    dplyr::rename("variables" = "row_var", "lvs" = "levels", "Ensemble" = "Total") |>
    dplyr::group_by(.data$variables)

  # It joins the summary family by its return alone: one table, the format decided at print time.
  # No eigenvalues -- a cluster description says nothing about axes.
  gda_summary(out)
}
