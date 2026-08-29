# PURPOSE: Everything that returns a table rather than a plot -- benzecri_mrv(), mca_interpret(),
#   pca_interpret(), mean_sd_tab(), HCPC_tab().
# ROLE: The reading aids: what an axis means, what a cluster is made of, what the variables look
#   like before the analysis.
# KEY CONSTRAINTS:
#   - Tables are tabxplor's job: every one here builds tabxplor::fmt() columns. mca_interpret() is
#     the only one that also RENDERS -- its type = "html" contract is a finished table -- so it is
#     the only caller of tab_html(), through the internal mca_interpret_tab().
#   - mca_interpret() implements Le Roux and Rouanet's reading: only levels contributing above the
#     mean are kept, and a spread is stated in percent of the question's own variance. That same
#     mean contribution is what the html table's colour ladder is graded against.
# See: CLAUDE.md section ggfacto architecture > Tables are tabxplor's.

#' Benzecri's modified rate of variance
#'
#' @param res.mca The result of \link[FactoMineR]{MCA}.
#' @param fmt By default, the result is given as a numeric vector. Set to `TRUE` to have
#' a \pkg{tabxplor} \code{link[tabxplor]{fmt}} vector instead.
#'
#' @return A numeric vector (or fmt vector with `fmt = TRUE`).
#' @export
#'
#' @examples
#' data(tea, package = "FactoMineR")
#' res.mca <- MCA2(tea, active_vars = 1:18)
#' benzecri_mrv(res.mca)
benzecri_mrv <- function(res.mca, fmt = FALSE) {
  Q   <- length(res.mca$call$quali)
  eig <- purrr::keep(res.mca$eig[, 1], res.mca$eig[, 1] > 1/Q)
  eig <- (Q/(Q-1))^2 * (eig - 1/Q)^2
  eig <- eig/sum(eig)

  if (fmt) {
    tabxplor::fmt(pct = eig, n = 0, scale = "level_pct", pct_type = "all")
  } else {
    purrr::set_names(eig * 100, paste0("Dim ", 1:length(eig)) )
  }
}











#' Helper table to interpret multiple correspondence analysis
#' @description A table to help to interpret the meaning of axes in multiple
#' correspondence analysis (MCA), based on Brigitte Le Roux, \emph{Analyse geometrique des
#' donnees multidimensionnelles}, Dunod, Paris, 2014 / Brigitte Le Roux and Henri Rouanet,
#' \emph{Geometric data analysis : from correspondence analysis to structured data
#' analysis}, Kluwer, Boston, 2004. Only levels whose relative contribution to the
#' variance of axis is superior to the mean contribution are kept. The spread between
#' positive levels and negative levels of the same variable is calculated in percentages
#' of the variance of the question/variable.
#' @param res.mca An object created with \code{FactoMineR::\link[FactoMineR]{MCA}},
#' @param axes The axes to interpret, as an integer vector. Default to the first five axes.
#' @param type By default (\code{"html"}), a table rendered by \pkg{tabxplor}: it opens in the
#' Viewer when printed, and is an html table when knitted. Set to \code{"console"} for the plain
#' numeric \code{tibble} that table is built from.
#' @param spread Set to \code{TRUE} to add the spread column to the html table. The
#' \code{"console"} tibble always carries it.
#'
#' @return An html table (or a \code{tibble} with \code{type = "console"}).
#' @export
#' @examples \donttest{
#' data(tea, package = "FactoMineR")
#' res.mca <- MCA2(tea, active_vars = 1:18)
#' mca_interpret(res.mca)
#' }
mca_interpret <- function(res.mca,
                          axes = 1:min(res.mca$call$ncp, 5),
                          type = c("html", "console"),
                          spread = FALSE) {

  contrib1 <- res.mca$var$contrib[,axes] |>
    tibble::as_tibble(rownames = "levels") |>
    tidyr::pivot_longer(-"levels", names_prefix ="Dim ", names_to = "Axe",
                        values_to = "Contrib_mod") |>
    dplyr::select("Axe", tidyselect::everything()) |> dplyr::arrange(.data$Axe) |>
    dplyr::mutate(eig_value = res.mca$eig[as.integer(.data$Axe),1],
                  pct       = round(res.mca$eig[as.integer(.data$Axe),2], 1))

  data <- res.mca$call$X[res.mca$call$quali]
  var_names <- purrr::set_names(names(data))
  var_names <- purrr::map(var_names, ~ levels(dplyr::pull(data, .x)) ) |>
    purrr::imap(
      ~ rep(.y, length(.x)) |> purrr::set_names(.x)
    ) |>
    purrr::flatten_chr()

  contrib1 <- contrib1 |>
    dplyr::mutate(Question = var_names[.data$levels]) |>
    dplyr::group_by(.data$Axe, .data$Question) |>
    dplyr::mutate(contrib_q = sum(.data$Contrib_mod))

  #Coordonnees et frequences des levels (pour calculer contribution des ecarts)
  coord_fk <- dplyr::left_join(
    tibble::as_tibble(res.mca$var$coord[, axes], rownames = "levels"),
    tibble::tibble(levels = names(res.mca$call$marge.col),
                   fk = res.mca$call$marge.col),
    by = "levels"
  ) |>
    tidyr::pivot_longer(c(-"levels", -"fk"),
                        names_prefix = "Dim ", names_to = "Axe",
                        values_to = "coord") |>
    dplyr::arrange(.data$Axe)


  #Choisir les levels > a la moyenne, trier par coordonnees positives/negatives
  contribsup <- contrib1 |> dplyr::left_join(coord_fk, by = c("Axe", "levels")) |>
    dplyr::with_groups(NULL, ~ dplyr::mutate(., mean_ctr = mean(.data$Contrib_mod))) |>
    dplyr::filter(.data$Contrib_mod >= .data$mean_ctr) |>
    dplyr::arrange(.data$Axe, dplyr::desc(.data$contrib_q),
                   dplyr::desc(.data$Contrib_mod)) |>
    #dplyr::arrange(dplyr::desc(contrib_q)) %>%
    dplyr::mutate(levels_2 = .data$levels, ctr_neg = .data$Contrib_mod,
                  ctr_pos  = .data$Contrib_mod, fneg = .data$fk, fpos = .data$fk,
                  coord_neg = .data$coord, coord_pos = .data$coord) |>
    dplyr::select(-"Contrib_mod") |>
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("levels", "ctr_neg", "fneg",
                                                     "coord_neg")),
                                ~ ifelse(.data$coord <= 0, ., NA))) |>
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("levels_2", "ctr_pos", "fpos",
                                                     "coord_pos")),
                                ~ ifelse(.data$coord > 0, ., NA))) |>
    dplyr::ungroup()


  #Ajouter les ecarts par Question (en % de la contribution de la question) :
  contribsup <- contribsup |>
    dplyr::group_by(.data$Axe, .data$Question) |>
    dplyr::mutate(coord_ecart_neg = stats::weighted.mean(.data$coord_neg,.data$fneg,
                                                         na.rm = TRUE),
                  coord_ecart_pos = stats::weighted.mean(.data$coord_pos,.data$fpos,
                                                         na.rm = T),
                  poids_ecart_neg = sum(.data$fneg, na.rm = T),
                  poids_ecart_pos = sum(.data$fpos, na.rm = T)  ) |>
    dplyr::mutate(poids_ecart = 1/( 1/.data$poids_ecart_neg + 1/.data$poids_ecart_pos) ) |>
    dplyr::mutate(spread = .data$poids_ecart * 100 *
                    (.data$coord_ecart_pos - .data$coord_ecart_neg)^2 /
                    (.data$eig_value*.data$contrib_q/100  ) ) |>
    dplyr::select(-"coord",-"fk",-"coord_ecart_neg", -"coord_ecart_pos",
                  -"poids_ecart_neg", -"poids_ecart_pos", -"poids_ecart") |>
    dplyr::ungroup() |>
    dplyr::mutate(spread = ifelse(is.na(.data$spread), NA, .data$spread) )

  # The mean contribution the filter above tested against: the html table grades its colours on it.
  mean_ctr <- contribsup$mean_ctr[1]

  #Contributions totales (positif/negatif sur l'axe), contrib de l'ecart total :
  total <- contribsup |>
    dplyr::group_by(.data$Axe) |>
    dplyr::summarise(ctr_neg = sum(.data$ctr_neg, na.rm = TRUE),
                     ctr_pos = sum(.data$ctr_pos, na.rm = TRUE),
                     coord_neg = stats::weighted.mean(.data$coord_neg, .data$fneg,
                                                      na.rm = TRUE),
                     coord_pos = stats::weighted.mean(.data$coord_pos, .data$fpos,
                                                      na.rm = TRUE),
                     poids_neg = sum(.data$fneg, na.rm = TRUE),
                     poids_pos = sum(.data$fpos, na.rm = TRUE),
                     poids_ecart = 1/( 1/.data$poids_neg + 1/.data$poids_pos), #fii' = 1/(1/fi + 1/fi').
                     spread = .data$poids_ecart * 100 *
                       (.data$coord_pos - .data$coord_neg)^2/mean(.data$eig_value) # = fii' (y l - y ')^2/??l )
    ) |> dplyr::select(-"coord_neg", -"coord_pos", -"poids_neg",
                        -"poids_pos", - "poids_ecart") |>
    tibble::add_column(Question = "All levels") |>
    dplyr::mutate(contrib_q = .data$ctr_neg + .data$ctr_pos)

  # #Total general (contributions sur l'axe positif + sur l'axe negatif)
  # total2 <- contribsup %>%
  #   dplyr::group_by(.data$Axe) %>%
  #   dplyr::summarise(contrib_q = sum(.data$ctr_neg, na.rm = TRUE) + sum(.data$ctr_pos, na.rm = TRUE))
  # #total2 <-  dplyr::bind_rows(total2, total2["Axe"])

  final_tab <- contribsup |>
    dplyr::select(-"fneg", -"fpos", -"coord_neg", -"coord_pos",
                  -"eig_value") |>
    dplyr::bind_rows(total) |>
    dplyr::arrange(.data$Axe) |>
    dplyr::select(tidyselect::all_of(c("Axe", "pct", "Question", "contrib" = "contrib_q",
                                       "Positive_levels" = "levels_2", "  " =  "ctr_pos",
                                       "Negative_levels" = "levels", "   " = "ctr_neg",
                                       "spread")))

  if (type[1] != "html") return(final_tab)

  tabxplor::tab_html(
    mca_interpret_tab(final_tab, mean_ctr = mean_ctr, mrv = benzecri_mrv(res.mca),
                      n_ind = nrow(res.mca$call$X), spread = spread),
    var_names = "rows",
    # Every figure already has a cell of its own, so a hover box would only repeat it; and the
    # generated legend describes a crosstab's over/under-representation, which an axis has no
    # notion of -- mca_interpret_legend() says what the colour means here instead.
    tooltips = FALSE, color_legend = FALSE
  )
}


# Why this exists: the html branch needs a tabxplor table, and a function is what the test suite can
# address -- the rendered string is 7 kB of stylesheet plus markup, and nothing to snapshot.
mca_interpret_tab <- function(final_tab, mean_ctr, mrv, n_ind, spread = FALSE) {
  pos_col <- "  "   # the two contribution columns are named with two and three spaces, so that
  neg_col <- "   "  # they print header-less beside the level they belong to

  # --- one row per question and rank, the two sides of the axis side by side ----------------------
  # A run id, not dplyr::group_by(): the table is already sorted by the question's contribution to
  # the axis, and grouping would hand the blocks back in alphabetical order.
  block <- cumsum(final_tab$Axe      != dplyr::lag(final_tab$Axe     , default = "") |
                  final_tab$Question != dplyr::lag(final_tab$Question, default = ""))
  pad   <- function(x, k) c(x, rep(x[NA_integer_][1], k))

  packed <- purrr::map_dfr(split(final_tab, block), function(q) {
    # WARNING: keyed on the CONTRIBUTIONS, never on the level names -- the "All levels" row carries
    # both figures and no name at all, and would come back empty.
    pos <- q[!is.na(q[[pos_col]]), ]
    neg <- q[!is.na(q[[neg_col]]), ]
    n   <- max(nrow(pos), nrow(neg), 1L)
    tibble::tibble(
      Axe     = q$Axe[1]    , pct    = q$pct[1]   , Question = q$Question[1],
      contrib = q$contrib[1], spread = q$spread[1],
      pos_lv  = pad(pos$Positive_levels, n - nrow(pos)),
      pos_ctr = pad(pos[[pos_col]]     , n - nrow(pos)),
      neg_lv  = pad(neg$Negative_levels, n - nrow(neg)),
      neg_ctr = pad(neg[[neg_col]]     , n - nrow(neg))
    )
  })

  # The axis heading states the raw eigenvalue percentage AND Benzecri's modified rate, which is
  # the number that corrects it -- an MCA's raw percentages understate the first axes badly.
  # An axis below the 1/Q cutoff has no modified rate, and simply does not get the clause.
  packed <- packed |>
    dplyr::mutate(mrv = mrv[as.integer(.data$Axe)]) |>
    dplyr::group_by(.data$Axe) |>
    dplyr::mutate(axis  = paste0("Axe ", .data$Axe, ": ",
                                 .data$pct[!is.na(.data$pct)][1], "% of variance",
                                 ifelse(is.na(.data$mrv[1]), "",
                                        paste0(" (mod. ", round(.data$mrv[1]), "%)"))),
                  first = .data$Question != dplyr::lag(.data$Question, default = ".")) |>
    dplyr::ungroup()

  is_tot   <- packed$Question == "All levels"
  row_kind <- ifelse(is_tot, "total", "data")
  blank    <- function(x) tidyr::replace_na(as.character(x), "")

  # --- the cells ---------------------------------------------------------------------------------
  # DESIGN: `pct` is what prints, `ctr` is what the colour is graded against -- so the sign of the
  #   coordinate rides the colour without ever reaching the page. `color = "contrib"` reads
  #   ctr / (the total row's ctr) and its ladder is declared over both halves, so a negative-side
  #   level lands on the under-represented (red) half at the same x1 / x2 / x5 / x10 steps.
  # WARNING: the total row's `ctr` is the MEAN contribution, not the sum it displays. It is the
  #   reference every other cell is divided by, and it is Le Roux and Rouanet's own threshold.
  ctr_fmt <- function(v, col_var, sign = 1) tabxplor::fmt(
    n        = rep(n_ind, length(v)),
    scale    = "level_pct", pct_type = "col",
    pct      = v / 100,
    ctr      = ifelse(is_tot, mean_ctr / 100, sign * v / 100),
    row_kind = row_kind,
    ref      = "tot",
    col_var  = col_var,
    color    = "contrib",
    digits   = 1L
  )

  # A question's own figures are carried in every cell and DISPLAYED once: `display` is a per-cell
  # field and "blank" one of its tokens, so a repeat is hidden without the value being dropped.
  qst_fmt <- function(v, col_var) tabxplor::fmt(
    n        = rep(n_ind, length(v)),
    scale    = "level_pct", pct_type = "col",
    pct      = v / 100,
    row_kind = row_kind,
    ref      = "tot",
    col_var  = col_var,
    color    = "no",
    digits   = 1L,
    display  = ifelse(packed$first, "pct", "blank")
  )

  # DESIGN: the AXIS is the row variable and the question is its level. Only a row variable is
  #   turned vertically where the rotation saves width, and only the innermost label column draws
  #   the thick rule -- as the single label column the axis gets both, so the axes are the blocks
  #   the eye sees and the questions no longer cut the table into forty.
  out <- tibble::tibble(
    "Axe"      = tabxplor::new_lvl(forcats::as_factor(packed$axis), role = "var"),
    "Question" = tabxplor::new_lvl(
      forcats::as_factor(dplyr::if_else(packed$first, packed$Question, "")), role = "level"),
    "contrib"  = qst_fmt(packed$contrib, "Question"),
    # WARNING: "" and never NA -- a character NA renders as the literal string "NA".
    "Positive_levels" = blank(packed$pos_lv),
    "  "              = ctr_fmt(packed$pos_ctr, "Positive"),
    "Negative_levels" = blank(packed$neg_lv),
    "   "             = ctr_fmt(packed$neg_ctr, "Negative", sign = -1),
    .name_repair = "minimal"
  )
  if (spread) out[["spread"]] <- qst_fmt(packed$spread, "Spread")

  # DESIGN: `render_extras$n = "no"`, or tabxplor materialises a synthetic empty base-count column
  #   at render time -- there is one population here, and nothing for a count to say about it.
  tabxplor::new_tab(out, meta = list(render_extras = list(n = "no")),
                    subtext = mca_interpret_legend())
}


# The colour legend, written here because tabxplor's own describes a crosstab: over- and
# under-representation against independence, which an axis has no notion of. The swatch classes and
# the breaks are tabxplor's, read at call time, so the words cannot drift from the palette.
mca_interpret_legend <- function() {
  breaks <- tabxplor::get_color_breaks()[["contrib"]]
  swatch <- function(prefix) paste0("<span class=\"", prefix, seq_along(breaks), "\">\u00d7",
                                    breaks, "</span>", collapse = " ")
  paste0("Colour: the level's contribution to the variance of the axis, as a multiple of the mean ",
         "contribution -- ", swatch("p"), " on the positive side of the axis, ", swatch("m"),
         " on the negative side. Only levels contributing more than the mean are kept ",
         "(Le Roux and Rouanet).")
}























#' Colored Table to Help Interpretation of Principal Component Analysis
#'
#' @param res.pca The result of \code{\link[FactoMineR:PCA]{FactoMineR::PCA}}.
#' @param axes The axes to print, as a numeric vector.
#'
#' @return A tibble of class tabxplor
#' @export
#'
#'@examples
#'
#' data(mtcars, package = "datasets")
#' mtcars <- mtcars[1:7] |> dplyr::rename(weight = wt)
#' res.pca <- FactoMineR::PCA(mtcars, graph = FALSE)
#' pca_interpret(res.pca)
#'
pca_interpret <- function(res.pca, axes = 1:3) {
  n_acp <- nrow(res.pca$ind$coord)

  var_data <- res.pca$var |>
    purrr::imap_dfr(~ .x[, axes] |>
                      tibble::as_tibble(rownames = "variable") |>
                      dplyr::mutate(type := factor(.y))) |>
    dplyr::filter(.data$type != "cor") |>  # no need for correlation since scale.unit = TRUE
    dplyr::mutate(type = forcats::fct_relevel(.data$type, "coord", "contrib", "cos2") |> # reorder types
                    forcats::fct_recode("ctr" = "contrib"),
                  variable = forcats::as_factor(.data$variable)
    ) |>
    dplyr::arrange(.data$type)

  var_data <- var_data |>
    tidyr::pivot_wider(names_from = "type",
                       values_from = tidyselect::starts_with("Dim."),
                       names_sort = TRUE) |>
    dplyr::rename_with(~str_remove(., "_coord") |>
                         str_replace("Dim\\.([^_]+)_(.+)", "\\2.\\1")
    )

  var_data |>
    tibble::add_row(variable = factor("Total")) |>
    dplyr::mutate(dplyr::across(where(is.numeric) & tidyselect::starts_with("ctr"),
                                ~ dplyr::if_else(variable != "Total", ., 100/(dplyr::n() - 1) )) # mean(., na.rm = TRUE)
    ) |>
    dplyr::mutate(
      dplyr::across(where(is.numeric) & tidyselect::starts_with("Dim"),
                    #~ round(., 2)
                    # A COORDINATE IS A DEVIATION, in standard deviations of the axis: `mean_diff`
                    # is the scale that says so, and its ladder (0.1 / 0.2 / 0.4 / 0.8 SD) then makes
                    # the colour's INTENSITY the size of the coordinate. `var = 1` because a PCA axis
                    # is already standardized.
                    ~ tabxplor::fmt(n         = rep(n_acp, length(.)),
                                    scale     = "mean_diff",

                                    mean      = .,
                                    diff      = dplyr::if_else(variable == "Total", 0, .),
                                    var       = 1,
                                    row_kind  = dplyr::if_else(variable == "Total", "total", "data"),
                                    in_refrow = variable == "Total",
                                    digits  = 2L,

                                    col_var   =  str_extract(dplyr::cur_column(), "\\.[^\\.]+$"),
                                    color     = "difference",
                                    ref = "tot",
                    )
      ),

      dplyr::across(where(is.numeric) & tidyselect::starts_with("ctr"),
                    ~ tabxplor::fmt(n         = rep(n_acp, length(.)),
                                    scale     = "level_pct", pct_type = "col",

                                    pct       = dplyr::if_else(variable == "Total", 1, ./100),
                                    ctr       = ./100,
                                    row_kind  = dplyr::if_else(variable == "Total", "total", "data"),

                                    col_var   = str_extract(dplyr::cur_column(), "\\.[^\\.]+$"), # dplyr::cur_column(),
                                    color     = "contrib",
                                    ref = "tot",
                    )),

      dplyr::across(where(is.numeric) & tidyselect::starts_with("cos2"),
                    ~ tabxplor::fmt(n    = rep(n_acp, length(.)),
                                    scale = "level_pct", pct_type = "row",

                                    pct  = .,
                                    diff = . - 0.5,
                                    row_kind  = dplyr::if_else(variable == "Total", "total", "data"),
                                    in_refrow = variable == "Total",

                                    col_var   = str_extract(dplyr::cur_column(), "\\.[^\\.]+$"), # dplyr::cur_column(),
                                    color     = "difference",
                                    ref = "tot",
                    )
      ),


    )

}



#' Simple Mean and SD Summary
#'
#' @param data A data.frame.
#' @param vars <\link[tidyr:tidyr_tidy_select]{tidy-select}> The names of the
#' numeric variables to compute means and sds with.
#' @param wt The name of the weight variable, if needed.
#'
#' @return A data.frame.
#' @export
#'
#' @examples
#' mean_sd_tab(mtcars, 1:7)
mean_sd_tab <- function(data, vars, wt) {
  vars <- names(tidyselect::eval_select(rlang::enquo(vars), data))

  not_num <- data |>
    dplyr::select(tidyselect::all_of(vars)) |>
    purrr::map_lgl(~ !is.numeric(.))

  if(any(not_num)) {
    stop(paste0("some vars are not numeric: ",
                paste0(names(not_num)[not_num], collapse = ", ")
    ))
  }

  if (missing(wt)) {
    tabs <- data |>
      dplyr::summarise(
        dplyr::across(tidyselect::all_of(vars),
                      ~ mean(., na.rm = TRUE),
                      .names = "{.col};mean"
        ),

        dplyr::across(tidyselect::all_of(vars),
                      ~ sqrt(var(., na.rm = TRUE)),
                      .names = "{.col};sd"
        ),
      )

  } else {
    wt <- rlang::ensym(wt)

    tabs <- data |>
      dplyr::summarise(
        dplyr::across(tidyselect::all_of(vars),
                      ~ stats::weighted.mean(., w = !!wt, na.rm = TRUE),
                      .names = "{.col};mean"),

        dplyr::across(tidyselect::all_of(vars),
                      ~ sqrt(weighted.var(., wt = !!wt, na.rm = TRUE)),
                      .names = "{.col};sd"),
        )

  }

  tabs |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::ends_with(";mean"),
        ~ rlang::eval_tidy(rlang::sym(str_replace(dplyr::cur_column(), ";mean", ";sd"))) / .,
        .names = "{.col};sd/mean"),
    ) |>
    dplyr::rename_with(~ str_replace(., "mean;sd/mean", "sd/mean"),
                       .cols = tidyselect::contains("mean;sd/mean")) |>
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = c("variables", "type"),
                        names_sep = ";" ) |>
    tidyr::pivot_wider(names_from = "type", values_from = "value")
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
#' @return A \code{tibble} of class \code{tab}, possibly with colored reading helpers.
#' @export
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

  purrr::reduce(blocks, dplyr::bind_rows) |>
    dplyr::filter(!str_detect(as.character(.data$levels), "Remove levels")) |>
    dplyr::mutate(dplyr::across(c("row_var", "levels"), ~ forcats::as_factor(as.character(.)))) |>
    dplyr::rename("variables" = "row_var", "lvs" = "levels", "Ensemble" = "Total") |>
    dplyr::group_by(.data$variables)
}
