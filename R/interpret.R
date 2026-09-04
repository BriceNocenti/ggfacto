# PURPOSE: the interpretation tables -- mca_interpret(), ca_interpret(), pca_interpret(), and the
#   contract they share with HCPC_tab() and mean_sd_tab().
# ROLE: reading a factorial analysis WITHOUT the cloud: which points build an axis, on which side,
#   and how well the axis represents them. One family, one output contract.
# KEY CONSTRAINTS:
#   - Every summary is ONE tabxplor table, never a list: gda_summary() tags it `ggfacto_summary`
#     (the subclass hook of tabxplor::new_tab()) and hangs the eigenvalues off it as a subordinate
#     table (tabxplor::set_footer_tabs()), which every exporter then renders under it.
#   - The FORMAT is a print-time decision, `options(ggfacto.print)`: print/knit_print pick the
#     renderer, so the object stays a table one can pipe, filter and export.
#   - Le Roux and Rouanet's rule, everywhere: only a point contributing MORE than the mean
#     contribution of its own set is kept. In an MCA there is one set (the active levels); in a CA
#     there are two (the rows, the columns), whose means differ.
#   - A NUMBER IS NORMALISED BEFORE tabxplor GRADES IT: `ctr` holds the MULTIPLE of the mean
#     contribution and every summary row holds exactly 1, so a table carrying several summary rows
#     (one per axis, or one per set) cannot colour against the wrong one.
#   - ONLY WHAT HAS A LADDER IS COLOURED. The contribution has Le Roux and Rouanet's threshold on
#     every family; a coordinate and a cos2 have one only in a PCA, where the coordinate IS a
#     correlation and the cloud has few axes. In an MCA or a CA they print and do not colour --
#     measured: the PCA's 50 % cos2 rule turns an MCA table entirely red.
# See: CLAUDE.md section ggfacto architecture > Tables are tabxplor's.

# === SECTION: the shared contract =================================================================

#' The interpretation tables, and how they print
#'
#' @description
#' \code{\link{mca_interpret}}, \code{\link{ca_interpret}}, \code{\link{pca_interpret}},
#' \code{\link{HCPC_tab}} and \code{\link{mean_sd_tab}} all return \strong{one}
#' \code{tabxplor} table, so it can be piped, filtered and exported like any other. What differs is
#' only how it is \emph{shown}:
#'
#' \code{options(ggfacto.print = "html")} --- the default --- draws it with
#' \code{\link[tabxplor]{tab_html}}: the Viewer pane in RStudio/Positron, a real html table when
#' knitted. \code{"md"} prints a markdown pipe table (\code{\link[tabxplor]{tab_md}}), which is what
#' to set when the reader is a text file or a language model. \code{"console"} prints the plain
#' \code{tabxplor} grid. The option is ggfacto's own and is read at print time, so it can be set
#' after the table is built; \code{options(tabxplor.print)} governs ordinary crosstabs and is left
#' alone.
#'
#' An analysis-of-axes summary carries the \strong{eigenvalues} as a subordinate table
#' (\code{\link[tabxplor]{set_footer_tabs}}), which every medium renders under it: the percentage of
#' variance of each axis, its cumulated percentage, and for an MCA Benzecri's modified rate --- the
#' numbers the rule for choosing how many axes to interpret is read on.
#'
#' \code{eig = FALSE} leaves them out, for a document that shows them already or prints the summary
#' several times to comment it column by column.
#'
#' \code{complete = TRUE} widens an MCA or CA summary: each side of the axis gains the point's
#' \strong{coordinate} (its sign says which pole, its size how far out) and its \strong{cos2} (the
#' share of the point's own variance the axis holds), plus the \emph{spread} between the two sides.
#' Neither is coloured there: a coordinate in axis standard deviations has no conventional cut-off,
#' and an MCA cloud has so many axes that every cos2 is small --- the 50 \% / 75 \% rule a
#' \code{\link{pca_interpret}} table reads does not transfer. Both are read by comparing the points
#' shown; only the contribution carries an absolute threshold.
#'
#' @section Exporting by hand:
#' A call written by hand needs \code{color_legend = FALSE}: tabxplor's generated legend describes a
#' crosstab's contribution to the chi-squared, which an axis has no notion of, and these tables carry
#' a legend of their own.
#'
#' \code{mca_interpret(res.mca) |> tab_md(css = FALSE, print = FALSE, color_legend = FALSE)}
#'
#' @section After a dplyr verb:
#' The subclass is not carried by dplyr (only a table's \code{tabxplor} attributes are), so a summary
#' that has been through \code{mutate()} prints as an ordinary \code{tabxplor} table --- the
#' eigenvalues still render under it, the format choice falls back to \code{options(tabxplor.print)}.
#'
#' @name ggfacto_summary
#' @seealso [mca_interpret()], [ca_interpret()], [pca_interpret()], [HCPC_tab()], [mean_sd_tab()].
NULL


# "html" | "md" | "console". NOT seeded at load, so "unset" is distinguishable and ggfacto's own
# default (html) does not depend on what tabxplor's option happens to be.
#' @keywords internal
#' @noRd
gda_print_format <- function() {
  v <- getOption("ggfacto.print", "html")
  if (length(v) != 1L || !v %in% c("html", "md", "console")) "html" else v
}

# Tag a finished table as a ggfacto summary: the legend, the eigenvalues under it, the exporter
# options only ggfacto knows, and the subclass its print methods dispatch on.
# DESIGN: the render options ride a PLAIN attribute, not `meta`. dplyr drops both it and the class
#   together, so the options can never outlive the methods that read them.
#' @keywords internal
#' @noRd
gda_summary <- function(tabs, eig = NULL, legend = character(), var_names = NULL) {
  if (length(legend)) attr(tabs, "subtext") <- legend
  if (!is.null(eig))  tabs <- tabxplor::set_footer_tabs(tabs, list(eig))
  attr(tabs, "ggfacto_render") <- list(var_names = var_names)
  class(tabs) <- unique(c("ggfacto_summary", class(tabs)))
  tabs
}

#' @keywords internal
#' @noRd
gda_plain <- function(x) {
  class(x) <- setdiff(class(x), "ggfacto_summary")
  x
}

# One renderer call, so html and md cannot drift on the options only ggfacto knows: no tooltip (every
# figure already has a cell of its own) and no generated legend (the table brings its own).
#' @keywords internal
#' @noRd
gda_render <- function(x, format, ...) {
  o <- attr(x, "ggfacto_render") %||% list()
  x <- gda_plain(x)
  args <- utils::modifyList(list(color_legend = FALSE, var_names = o$var_names), rlang::list2(...))
  if (identical(format, "md")) do.call(tabxplor::tab_md, c(list(x, css = FALSE), args))
  else                         do.call(tabxplor::tab_html, c(list(x, tooltips = FALSE), args))
}

#' @method print ggfacto_summary
#' @param x A table returned by one of the functions of [ggfacto_summary].
#' @param ... Passed to the renderer (\code{\link[tabxplor]{tab_html}} /
#'   \code{\link[tabxplor]{tab_md}}) or to the console print method.
#' @return \code{x} invisibly (or the rendered object, for html).
#' @rdname ggfacto_summary
#' @export
print.ggfacto_summary <- function(x, ...) {
  fmt <- gda_print_format()
  if (identical(fmt, "html")) {
    out <- gda_render(x, "html", ...)
    print(out)
    return(invisible(out))
  }
  if (identical(fmt, "md")) {
    gda_render(x, "md", ...)
    return(invisible(x))
  }
  # "console": tabxplor's own grid, whatever options(tabxplor.print) says -- the reader asked for it.
  withr::with_options(list(tabxplor.print = "console"), print(gda_plain(x), ...))
  invisible(x)
}

# knitr is a soft dependency: registered by hand in .onLoad(), like knit_print.ggfacto_widget().
#' @keywords internal
#' @noRd
knit_print.ggfacto_summary <- function(x, ...) {
  fmt <- gda_print_format()
  if (identical(fmt, "html")) return(knitr::knit_print(gda_render(x, "html")))
  if (identical(fmt, "md"))
    return(knitr::asis_output(paste0(gda_render(x, "md", print = FALSE), "\n\n")))
  withr::with_options(list(tabxplor.print = "console"),
                      knitr::knit_print(gda_plain(x), ...))
}


# === SECTION: the eigenvalues =====================================================================

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


# The eigenvalue table, the subordinate table every axes summary carries. `n_axes` rows at most, and
# when there are more the LAST one is shown anyway, under an ellipsis row -- so the reader always
# knows how many axes the analysis has, whether it has nine or forty.
# DESIGN: no barplot. The numbers ARE the rule -- a cumulated percentage cannot be read off a bar --
#   and `% variance` carries a data bar (tabxplor::set_bars()), which is the barplot, inside the table.
# WARNING: every column takes a `col_var`, and the two Benzecri ones share theirs: a column with none
#   opens no block, so no vertical rule would say that the modified rate and its cumulation belong
#   together.
#' @keywords internal
#' @noRd
gda_eig_tab <- function(eig, n_ind, mrv = NULL, n_axes = 8L, color = TRUE) {
  n    <- nrow(eig)
  k    <- max(1L, min(n, as.integer(n_axes)))
  rows <- seq_len(k)
  gap  <- k < n                                   # is the last axis beyond what we show?
  # the ellipsis row, then the last axis. `row_kind = "blank"` is the vocabulary tabxplor already has
  # for a row that is a display device rather than data.
  idx  <- if (gap) c(rows, NA_integer_, n) else rows
  kind <- ifelse(is.na(idx), "blank", "data")
  pick <- function(v) if (gap) c(v[rows], NA_real_, v[[n]]) else v[rows]

  pctf <- function(v, col_var) tabxplor::fmt(
    n = rep(n_ind, length(v)), scale = "level_pct", pct_type = "col", pct = v / 100,
    row_kind = kind, col_var = col_var, color = "no", digits = 1L)

  out <- tibble::tibble(
    "Axe" = tabxplor::new_lvl(
      forcats::as_factor(ifelse(is.na(idx), "...", paste("Axe", idx))), role = "level"),
    # a variance, and it says so: `display = "var"` (tabxplor phase 6 stopped calling it "mean-var").
    "eigenvalue" = tabxplor::fmt(
      n = rep(n_ind, length(idx)), scale = "level_mean", var = pick(eig[, 1]), display = "var",
      row_kind = kind, col_var = "Variance", color = "no", digits = 3L),
    "% variance" = pctf(pick(eig[, 2]), "Variance"),
    "cumul."     = pctf(pick(eig[, 3]), "Variance")
  )

  if (!is.null(mrv)) {
    m <- rep(NA_real_, n)
    m[seq_len(min(n, length(mrv)))] <- as.numeric(mrv)[seq_len(min(n, length(mrv)))]
    cm <- cumsum(ifelse(is.na(m), 0, m)) * ifelse(is.na(m), NA, 1)
    out[["Benzecri's modified rate"]] <- pctf(pick(m),  "Benzecri")
    out[["cumul. mod."]]              <- pctf(pick(cm), "Benzecri")
  }

  # The Total row states what the axes add up to. ⚠ `res$eig` stops at `ncp`, so the sum is the
  # variance the ANALYSIS kept, not the cloud's -- the label says so rather than claiming the whole.
  tot <- tibble::tibble(
    "Axe" = tabxplor::new_lvl(forcats::as_factor(if (n < 1L) "Total" else "Total"), role = "level"),
    "eigenvalue" = tabxplor::fmt(n = n_ind, scale = "level_mean", var = sum(eig[, 1]),
                                 display = "var", row_kind = "total", col_var = "Variance",
                                 color = "no", digits = 3L),
    "% variance" = tabxplor::fmt(n = n_ind, scale = "level_pct", pct_type = "col", pct = 1,
                                 row_kind = "total", col_var = "Variance", color = "no", digits = 1L),
    "cumul."     = tabxplor::fmt(n = n_ind, scale = "level_pct", pct_type = "col", pct = NA_real_,
                                 row_kind = "total", col_var = "Variance", color = "no", digits = 1L)
  )
  if (!is.null(mrv)) {
    tot[["Benzecri's modified rate"]] <- tabxplor::fmt(
      n = n_ind, scale = "level_pct", pct_type = "col", pct = 1, row_kind = "total",
      col_var = "Benzecri", color = "no", digits = 1L)
    tot[["cumul. mod."]] <- tabxplor::fmt(
      n = n_ind, scale = "level_pct", pct_type = "col", pct = NA_real_, row_kind = "total",
      col_var = "Benzecri", color = "no", digits = 1L)
  }

  out <- dplyr::bind_rows(out, tot)
  out <- tabxplor::new_tab(out, meta = list(render_extras = list(n = "no")))
  if (color) out <- tabxplor::set_bars(out, "% variance")
  out
}


# === SECTION: the two poles of an axis ============================================================

# `long` holds one row per (axis, point): axis, set, group, level, coord, ctr (in %), cos2, fk (the
# mass), eig, pct. `set` is the population a contribution is a share OF -- one in an MCA, two in a
# CA -- and its factor order is the order its blocks are printed in.
# WARNING: the mean contribution is a fact about a SET, not about the table: the rows of a CA and its
#   columns each sum to 100 % over a different number of points, so one pooled mean would keep too
#   many of one and too few of the other.
#' @keywords internal
#' @noRd
gda_poles <- function(long, min_contrib = NULL) {
  long <- long |>
    dplyr::group_by(.data$axis, .data$set, .data$group) |>
    dplyr::mutate(contrib_q = sum(.data$ctr)) |>
    dplyr::group_by(.data$set) |>
    dplyr::mutate(mean_ctr = mean(.data$ctr)) |>
    dplyr::ungroup()

  # The threshold. NULL is Le Roux and Rouanet's -- the mean contribution of the point's own set; a
  # number is read on the SCALE THAT PRINTS, i.e. in percent (`min_contrib = 5` keeps what
  # contributes 5 % or more), and 0 keeps everything.
  cut <- if (is.null(min_contrib)) long$mean_ctr else rep(as.numeric(min_contrib)[1], nrow(long))
  long$cut <- cut

  kept <- long |>
    dplyr::filter(.data$ctr >= .data$cut) |>
    dplyr::mutate(pos = .data$coord > 0) |>
    dplyr::arrange(.data$axis, .data$set, dplyr::desc(.data$contrib_q), dplyr::desc(.data$ctr))

  if (nrow(kept) == 0L) return(kept[0, ])

  # An EMPTY side has nothing to sum: sum(numeric(0)) is 0, which would read as "contributes
  # nothing" where the truth is "has no point above the mean at all".
  sum0 <- function(v) if (length(v)) sum(v) else NA_real_

  # The spread of Le Roux and Rouanet: the share of the group's own contribution to the axis that the
  # gap between its positive and its negative levels accounts for. A group with only one side has no
  # gap, so weighted.mean() of nothing gives NaN and the spread is NA -- deliberately.
  gap <- function(coord, fk, pos, eig, share) {
    cp <- stats::weighted.mean(coord[pos],  fk[pos])
    cn <- stats::weighted.mean(coord[!pos], fk[!pos])
    w  <- 1 / (1 / sum(fk[pos]) + 1 / sum(fk[!pos]))     # f_ii' = 1/(1/f_i + 1/f_i')
    out <- w * 100 * (cp - cn)^2 / (eig * share)
    if (is.na(out) || is.nan(out)) NA_real_ else out
  }

  run <- cumsum(kept$axis            != dplyr::lag(kept$axis           , default = "") |
                as.character(kept$set) != dplyr::lag(as.character(kept$set), default = "") |
                kept$group           != dplyr::lag(kept$group          , default = ""))

  pad <- function(x, k) c(x, rep(x[NA_integer_][1], k))

  data_rows <- purrr::imap_dfr(split(kept, run), function(q, i) {
    p <- q[q$pos, ]; n <- q[!q$pos, ]
    k <- max(nrow(p), nrow(n), 1L)
    tibble::tibble(
      axis = q$axis[1], pct = q$pct[1], eig = q$eig[1],
      set = q$set[1], group = q$group[1], is_tot = FALSE,
      contrib = q$contrib_q[1], mean_ctr = q$mean_ctr[1],
      spread  = gap(q$coord, q$fk, q$pos, q$eig[1], q$contrib_q[1] / 100),
      block   = as.integer(i),
      pos_lv  = pad(p$level, k - nrow(p)), pos_ctr   = pad(p$ctr  , k - nrow(p)),
      pos_cd  = pad(p$coord, k - nrow(p)), pos_cos2  = pad(p$cos2 , k - nrow(p)),
      neg_lv  = pad(n$level, k - nrow(n)), neg_ctr   = pad(n$ctr  , k - nrow(n)),
      neg_cd  = pad(n$coord, k - nrow(n)), neg_cos2  = pad(n$cos2 , k - nrow(n))
    )
  })

  # THE LABEL FOLLOWS THE THRESHOLD, or it lies: this row totals what was KEPT, which is "all levels"
  # only when nothing was filtered out.
  tot_label <- if (is.null(min_contrib)) gettext("Above mean ctr")
               else if (min_contrib <= 0) gettext("All levels")
               else gettextf("Above %s%%", format(min_contrib))

  # One total row per (axis, set): the two sides' summed contributions -- the pair that says whether
  # an axis opposes two poles or a specific group to the average -- and the spread of the whole axis.
  tot_rows <- kept |>
    dplyr::group_by(.data$axis, .data$set) |>
    dplyr::summarise(
      pct = dplyr::first(.data$pct), eig = dplyr::first(.data$eig),
      mean_ctr = dplyr::first(.data$mean_ctr),
      pos_ctr = sum0(.data$ctr[.data$pos]), neg_ctr = sum0(.data$ctr[!.data$pos]),
      spread  = gap(.data$coord, .data$fk, .data$pos, dplyr::first(.data$eig), 1),
      .groups = "drop") |>
    dplyr::mutate(group = tot_label, is_tot = TRUE, block = .Machine$integer.max,
                  contrib = rowSums(cbind(.data$pos_ctr, .data$neg_ctr), na.rm = TRUE),
                  pos_lv = NA_character_, pos_cd = NA_real_, pos_cos2 = NA_real_,
                  neg_lv = NA_character_, neg_cd = NA_real_, neg_cos2 = NA_real_)

  dplyr::bind_rows(data_rows, tot_rows) |>
    dplyr::arrange(.data$axis, .data$set, .data$block) |>
    dplyr::mutate(first = .data$group != dplyr::lag(.data$group, default = "."))
}


# The tabxplor table the packed poles become. Shared by MCA and CA: what differs is the second index
# column's name, whether a `contrib` column is meaningful, and the axis heading.
# DESIGN: `pct` prints, `ctr` colours -- so the SIGN of the coordinate rides the colour without ever
#   reaching the page: `ctr` holds the multiple of the mean contribution, negated on the negative
#   side, and the total row holds exactly 1. `color = "contrib"` reads ctr / (the total row's ctr),
#   so a table with several total rows -- one per axis, or one per set -- cannot grade against the
#   wrong one, and the x1 / x2 / x5 / x10 ladder is Le Roux and Rouanet's own threshold.
#' @keywords internal
#' @noRd
gda_poles_tab <- function(packed, axis_label, group_name, n_ind, legend, eig_tab,
                          contrib = TRUE, complete = FALSE, color = TRUE) {
  kind  <- ifelse(packed$is_tot, "total", "data")
  blank <- function(x) tidyr::replace_na(as.character(x), "")

  ctr_col <- function(v, col_var, sign = 1) tabxplor::fmt(
    n = rep(n_ind, length(v)), scale = "level_pct", pct_type = "col",
    pct = v / 100, ctr = ifelse(packed$is_tot, 1, sign * v / packed$mean_ctr),
    row_kind = kind, ref = "tot", col_var = col_var,
    color = if (color) "contrib" else "no", digits = 1L)

  # WARNING: NEITHER OF THE TWO IS COLOURED, and that is the point. The only ABSOLUTE threshold a
  #   factorial axis has is Le Roux and Rouanet's, on the contribution: a coordinate in axis standard
  #   deviations has no conventional cut-off, and the cos2 thresholds the PCA reads (50 % to name an
  #   axis on, 75 % to prefer a variable) DO NOT transfer to an MCA, where the cloud has many axes
  #   and every cos2 is structurally small -- measured on nine binary questions, every retained
  #   level came out between 10 % and 48 %, i.e. entirely below the threshold and entirely red.
  #   Both are read by comparison between the points shown, so both print and neither colours: a
  #   ladder that does not fit the quantity is a signal that is plausible and false.
  coord_col <- function(v, col_var) tabxplor::fmt(
    n = rep(n_ind, length(v)), scale = "level_mean", mean = v,
    row_kind = kind, col_var = col_var, color = "no", digits = 2L)

  # cos2 sums to 100 % over the AXES -- along a ROW of the analysis's own cos2 matrix, which is what
  # `pct_type = "row"` says.
  cos2_col <- function(v, col_var) tabxplor::fmt(
    n = rep(n_ind, length(v)), scale = "level_pct", pct_type = "row", pct = v,
    row_kind = kind, col_var = col_var, color = "no", digits = 0L)

  # A group's own figure is carried in every cell and DISPLAYED once: `display` is a per-cell field
  # and "blank" one of its tokens, so a repeat is hidden without the value being dropped.
  plain_col <- function(v, col_var) tabxplor::fmt(
    n = rep(n_ind, length(v)), scale = "level_pct", pct_type = "col",
    pct = v / 100, row_kind = kind, ref = "tot", col_var = col_var,
    color = "no", digits = 1L, display = ifelse(packed$first, "pct", "blank"))

  # DESIGN: the AXIS is the row variable and the group is its level. Only a row variable is turned
  #   vertically where the rotation saves width, and only the innermost label column draws the thick
  #   rule -- as the single label column the axis gets both, so the axes are the blocks the eye sees.
  out <- tibble::tibble("Axe" = tabxplor::new_lvl(forcats::as_factor(axis_label), role = "var"))
  out[[group_name]] <- tabxplor::new_lvl(
    forcats::as_factor(dplyr::if_else(packed$first, packed$group, "")), role = "level")
  if (contrib) out[["contrib"]] <- plain_col(packed$contrib, group_name)

  # WARNING: "" and never NA -- a character NA renders as the literal string "NA".
  if (complete) {
    out[["Positive_levels"]] <- blank(packed$pos_lv)
    out[["ctr"]]             <- ctr_col(packed$pos_ctr, "Positive")
    out[["coord"]]           <- coord_col(packed$pos_cd, "Positive")
    out[["cos2"]]            <- cos2_col(packed$pos_cos2, "Positive")
    out[["Negative_levels"]] <- blank(packed$neg_lv)
    out[["ctr "]]            <- ctr_col(packed$neg_ctr, "Negative", sign = -1)
    out[["coord "]]          <- coord_col(packed$neg_cd, "Negative")
    out[["cos2 "]]           <- cos2_col(packed$neg_cos2, "Negative")
    out[["spread"]]          <- plain_col(packed$spread, "Spread")
  } else {
    # the two contribution columns are named with two and three spaces, so that they print
    # header-less beside the level they belong to
    out[["Positive_levels"]] <- blank(packed$pos_lv)
    out[["  "]]              <- ctr_col(packed$pos_ctr, "Positive")
    out[["Negative_levels"]] <- blank(packed$neg_lv)
    out[["   "]]             <- ctr_col(packed$neg_ctr, "Negative", sign = -1)
  }

  # DESIGN: `render_extras$n = "no"`, or tabxplor materialises a synthetic empty base-count column
  #   at render time -- there is one population here, and nothing for a count to say about it.
  tabs <- tabxplor::new_tab(out, meta = list(render_extras = list(n = "no")))
  gda_summary(tabs, eig = eig_tab, legend = legend, var_names = "rows")
}


# The footer: ONE LINE PER STATISTIC, giving its full name and nothing else. How to read it lives in
# the course and in the exploration skill, not under every table.
# DESIGN: PLAIN TEXT, no colour swatch. `subtext` is fixed when the table is built, before the medium
#   is known, so a `<span class="p1">` written here would reach a markdown file and an Excel cell as
#   raw markup -- which it used to do. The break values are read from tabxplor at call time, so the
#   ladder cannot drift from the palette.
# WARNING: this exists at all only because a foreign package cannot replace the WORD of tabxplor's
#   own generated legend -- which for `color = "contrib"` says "contribution to Chi2", and a factorial
#   axis has no chi-squared. See ~/github/tabxplor/dev/legend_and_side_tables.md.
#' @keywords internal
#' @noRd
gda_poles_legend <- function(complete = FALSE, color = TRUE) {
  ladder <- paste0("\u00d7", tabxplor::get_color_breaks()[["contrib"]], collapse = " ")
  ctr <- if (color)
    gettextf("contrib: contribution to the variance of the axis, vs the mean contribution: %s",
             ladder)
  else gettext("contrib: contribution to the variance of the axis")
  if (!complete) return(ctr)
  c(ctr,
    gettext("coord: coordinate on the axis"),
    gettext("cos2: quality of representation"),
    gettext("spread: share of the group's contribution the gap between its two sides accounts for"))
}


# === SECTION: multiple correspondence analysis =====================================================

# res.mca -> one row per (axis, active level). `set` is a single value: every active level of an MCA
# belongs to one population whose contributions sum to 100 % per axis.
#' @keywords internal
#' @noRd
mca_interpret_data <- function(res.mca, axes) {
  data      <- res.mca$call$X[res.mca$call$quali]
  var_names <- purrr::map(purrr::set_names(names(data)),
                          ~ levels(dplyr::pull(data, .x))) |>
    purrr::imap(~ purrr::set_names(rep(.y, length(.x)), .x)) |>
    purrr::flatten_chr()

  lv  <- rownames(res.mca$var$contrib)
  purrr::map_dfr(axes, function(a) tibble::tibble(
    axis  = as.character(a),
    set   = factor("levels"),
    group = unname(var_names[lv]),
    level = lv,
    coord = res.mca$var$coord[, a],
    ctr   = res.mca$var$contrib[, a],
    cos2  = res.mca$var$cos2[, a],
    fk    = unname(res.mca$call$marge.col[lv]),
    eig   = res.mca$eig[a, 1],
    pct   = round(res.mca$eig[a, 2], 1)
  ))
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
#'
#' The eigenvalues of the axes travel under the table, Benzecri's modified rate beside them.
#' @param res.mca An object created with \code{FactoMineR::\link[FactoMineR]{MCA}}.
#' @param axes The axes to interpret, as an integer vector. Default to the first five axes.
#' @param complete Set to \code{TRUE} for the fuller summary: each side of the axis gains the
#' level's coordinate and its cos2, and the table gains the spread between the two sides.
#' @param eig The eigenvalues travel under the table. Set to \code{FALSE} in a document that already
#' shows them, or that prints the summary several times to comment it column by column.
#' @param type Deprecated. The output format is now \code{options(ggfacto.print)}, or an explicit
#' \code{\link[tabxplor]{tab_md}} / \code{\link[tabxplor]{tab_html}} call --- see
#' [ggfacto_summary].
#' @param spread Deprecated. Folded into \code{complete}.
#'
#' @return A \code{tabxplor} table --- see [ggfacto_summary] for how it prints.
#' @export
#' @seealso [ggfacto_summary], [ca_interpret()], [pca_interpret()], [benzecri_mrv()].
#' @examples \donttest{
#' data(tea, package = "FactoMineR")
#' res.mca <- MCA2(tea, active_vars = 1:18)
#' mca_interpret(res.mca)
#' mca_interpret(res.mca, axes = 1:2, complete = TRUE)
#' }
mca_interpret <- function(res.mca,
                          axes = 1:min(res.mca$call$ncp, 5),
                          complete = FALSE,
                          min_contrib = NULL,
                          color = TRUE,
                          eig = TRUE,
                          n_axes = 8L,
                          type = NULL,
                          spread = NULL) {
  if (!is.null(spread)) complete <- renamed_arg(spread, "spread", "complete", "mca_interpret")
  if (!is.null(type))   renamed_arg(type, "type", "options(ggfacto.print)", "mca_interpret")

  long   <- mca_interpret_data(res.mca, axes)
  packed <- gda_poles(long, min_contrib)

  mrv     <- benzecri_mrv(res.mca)
  eig_tab <- gda_eig_tab(res.mca$eig, n_ind = nrow(res.mca$call$X), mrv = mrv,
                         n_axes = n_axes, color = color)

  # The axis heading states the raw eigenvalue percentage AND Benzecri's modified rate, which is the
  # number that corrects it -- an MCA's raw percentages understate the first axes badly. An axis
  # below the 1/Q cutoff has no modified rate, and simply does not get the clause.
  m     <- mrv[as.integer(packed$axis)]
  label <- ifelse(is.na(m),
                  gettextf("Axe %s: %s%% of variance", packed$axis, packed$pct),
                  gettextf("Axe %s: %s%% of variance (mod. %s%%)",
                           packed$axis, packed$pct, round(m)))

  gda_poles_tab(packed, axis_label = label, group_name = gettext("Question"),
                n_ind = nrow(res.mca$call$X),
                legend = gda_poles_legend(complete, color),
                eig_tab = if (eig) eig_tab, contrib = TRUE, complete = complete, color = color)
}


# === SECTION: simple correspondence analysis ======================================================

# res.ca -> one row per (axis, point), the ROWS and the COLUMNS as two sets: each margin's
# contributions sum to 100 % per axis over a different number of points, so each has its own mean.
#' @keywords internal
#' @noRd
ca_interpret_data <- function(res.ca, axes, var_names) {
  one <- function(side, nm) {
    p  <- res.ca[[side]]
    lv <- rownames(p$coord)
    mg <- if (side == "row") res.ca$call$marge.row else res.ca$call$marge.col
    purrr::map_dfr(axes, function(a) tibble::tibble(
      axis = as.character(a), group = nm, level = lv,
      coord = p$coord[, a], ctr = p$contrib[, a], cos2 = p$cos2[, a],
      fk = unname(mg[seq_along(lv)]),
      eig = res.ca$eig[a, 1], pct = round(res.ca$eig[a, 2], 1)))
  }
  out <- dplyr::bind_rows(one("row", var_names[[1]]), one("col", var_names[[2]]))
  # `set` IS the group here, and its factor order is the order the two blocks print in.
  dplyr::mutate(out, set = factor(.data$group, levels = var_names))
}


#' Helper table to interpret simple correspondence analysis
#' @description
#' The counterpart of \code{\link{mca_interpret}} for a simple correspondence analysis: per axis, the
#' row points and the column points whose contribution to its variance is above the mean, the
#' positive side facing the negative one. The two margins are two populations --- each sums to 100 %
#' of the axis over a different number of points --- so each has its own threshold and its own
#' summary row, whose two figures say whether the axis opposes two poles or one specific group to the
#' average of the population.
#'
#' The eigenvalues of the axes travel under the table.
#' @param res.ca An object created with \code{FactoMineR::\link[FactoMineR]{CA}}.
#' @param axes The axes to interpret, as an integer vector.
#' @param complete Set to \code{TRUE} for the fuller summary: each side of the axis gains the point's
#' coordinate and its cos2, and the table gains the spread between the two sides.
#' @param eig The eigenvalues travel under the table. Set to \code{FALSE} in a document that already
#' shows them, or that prints the summary several times to comment it column by column.
#' @param crosstab The crosstab the analysis was made from, as a \code{tabxplor} table, to travel
#' under the summary --- a correspondence analysis shows the STRUCTURE of the deviations of a
#' crosstab and says nothing of their size, so the table is what keeps a reading honest. Build it
#' with \code{tab(..., color = "contrib", display = "ctr", test = TRUE)} to see the same
#' contributions cell by cell. It also names the two margins.
#'
#' @return A \code{tabxplor} table --- see [ggfacto_summary] for how it prints.
#' @export
#' @seealso [ggfacto_summary], [mca_interpret()], [ggca()].
#' @examples \donttest{
#' crosstab <- tabxplor::tab(forcats::gss_cat, race, marital, pct = "row")
#' res.ca   <- FactoMineR::CA(as.matrix(crosstab), graph = FALSE)
#' ca_interpret(res.ca, crosstab = crosstab)
#' }
ca_interpret <- function(res.ca, axes = 1:2, complete = FALSE, min_contrib = NULL,
                         vars = NULL, color = TRUE, eig = TRUE, n_axes = 8L) {
  # ⚠ THE VARIABLES' NAMES CANNOT BE RECOVERED: FactoMineR::CA() drops `names(dimnames())` from every
  #   matrix it keeps (`call$X`, `call$Xtot`), so even a table built with as.table() arrives anonymous.
  #   `vars =` is the only way in; without it the two words that are always true.
  nm <- if (is.null(vars)) NULL else as.character(vars)
  if (length(nm) != 2L || !all(nzchar(nm)) || anyNA(nm)) nm <- c(gettext("Rows"), gettext("Columns"))

  axes   <- axes[axes <= nrow(res.ca$eig)]
  long   <- ca_interpret_data(res.ca, axes, nm)
  packed <- gda_poles(long, min_contrib)

  n_ind   <- round(sum(res.ca$call$Xtot))
  eig_tab <- gda_eig_tab(res.ca$eig, n_ind = n_ind, n_axes = n_axes, color = color)
  label   <- gettextf("Axe %s: %s%% of variance", packed$axis, packed$pct)

  gda_poles_tab(packed, axis_label = label, group_name = gettext("Variable"), n_ind = n_ind,
                legend = gda_poles_legend(complete, color), eig_tab = if (eig) eig_tab,
                contrib = FALSE, complete = complete, color = color)
}


# === SECTION: principal component analysis ========================================================

#' Colored Table to Help Interpretation of Principal Component Analysis
#'
#' @description
#' One row per active variable, one block of three columns per axis: its coordinate --- which under
#' \code{scale.unit} IS its correlation with the axis ---, its contribution to the variance of the
#' axis, and its cos2, the share of its own variance the axis holds.
#'
#' The eigenvalues of the axes travel under the table.
#' @param res.pca The result of \code{\link[FactoMineR:PCA]{FactoMineR::PCA}}.
#' @param axes The axes to print, as a numeric vector.
#' @param eig The eigenvalues travel under the table. Set to \code{FALSE} in a document that already
#' shows them, or that prints the summary several times to comment it column by column.
#'
#' @return A \code{tabxplor} table --- see [ggfacto_summary] for how it prints.
#' @export
#' @seealso [ggfacto_summary], [mca_interpret()], [ggpca_cor_circle()].
#'
#'@examples
#'
#' data(mtcars, package = "datasets")
#' mtcars <- mtcars[1:7] |> dplyr::rename(weight = wt)
#' res.pca <- FactoMineR::PCA(mtcars, graph = FALSE)
#' pca_interpret(res.pca)
#'
pca_interpret <- function(res.pca, axes = 1:3, color = TRUE, eig = TRUE, n_axes = 8L) {
  n_acp <- nrow(res.pca$ind$coord)
  axes  <- axes[axes <= nrow(res.pca$eig)]
  n_var <- nrow(res.pca$var$coord)

  is_tot <- c(rep(FALSE, n_var), TRUE)
  kind   <- ifelse(is_tot, "total", "data")
  pad    <- function(v) c(v, NA_real_)
  nn     <- rep(n_acp, n_var + 1L)

  out <- tibble::tibble("variable" = tabxplor::new_lvl(
    forcats::as_factor(c(rownames(res.pca$var$coord), gettext("Total"))), role = "level"))

  # WHAT THE VARIABLES LOOK LIKE, before the analysis: the block mean_sd_tab() used to be, now the
  # first thing the reader meets. The numbers are FactoMineR's own (`call$centre` / `call$ecart.red`
  # are the mean and the sd it centred and scaled with), so the table cannot disagree with the
  # analysis it describes. ONE fmt record printed three times -- the sd and the coefficient of
  # variation are tokens tabxplor DERIVES from the same variance.
  ctr_pca <- res.pca$call$centre %||% rep(NA_real_, n_var)
  sd_pca  <- res.pca$call$ecart.red %||% rep(NA_real_, n_var)
  univ <- function(display, digits) tabxplor::fmt(
    n = nn, scale = "level_mean", mean = pad(ctr_pca), var = pad(sd_pca^2),
    row_kind = kind, col_var = gettext("Variables"), color = "no",
    display = display, digits = digits)
  out[[paste0("mean_",    gettext("Variables"))]] <- univ("mean", 2L)
  out[[paste0("sd_",      gettext("Variables"))]] <- univ("sd"  , 2L)
  out[[paste0("sd/mean_", gettext("Variables"))]] <- univ("cv"  , 0L)

  # DESIGN: the column is named `<statistic>_<col_var>`, and tabxplor strips the suffix at export
  #   (tab_col_var_header) -- the same rule that turns "Other_race" into "Other". So the tibble keeps
  #   unique names one can index, and html, markdown and Excel show a bare `coord` under an `Axe 1`
  #   span. The axes are named once, by the span, and never repeated in every header.
  for (a in axes) {
    k  <- sub("^Dim\\.?", "", colnames(res.pca$var$coord)[a])
    cv <- gettextf("Axe %s", k)

    # A COORDINATE IS A DEVIATION, in standard deviations of the axis: `mean_diff` is the scale that
    # says so, and its ladder (0.1 / 0.2 / 0.4 / 0.8 SD) then makes the colour's INTENSITY the size
    # of the coordinate. `var = 1` because a PCA axis is already standardized -- and because under
    # scale.unit the coordinate is a correlation, the ladder reads it end to end.
    out[[paste0("coord_", cv)]] <- tabxplor::fmt(
      n = nn, scale = "mean_diff",
      mean = pad(res.pca$var$coord[, a]),
      diff = ifelse(is_tot, 0, pad(res.pca$var$coord[, a])), var = 1,
      row_kind = kind, in_refrow = is_tot, ref = "tot",
      col_var = cv, color = if (color) "difference" else "no", digits = 2L)

    # WARNING: NOT coloured, even under `color = TRUE`. The coordinate beside it already says which
    #   variables build the axis, and grading the same fact twice is two saturated channels for one
    #   reading. The total row prints 100 % -- the contributions of an axis sum to it.
    ctr <- pad(res.pca$var$contrib[, a])
    out[[paste0("contrib_", cv)]] <- tabxplor::fmt(
      n = nn, scale = "level_pct", pct_type = "col",
      pct = ifelse(is_tot, 1, ctr / 100),
      row_kind = kind, ref = "tot", col_var = cv, color = "no")

    out[[paste0("cos2_", cv)]] <- tabxplor::fmt(
      n = nn, scale = "level_pct", pct_type = "row",
      pct = pad(res.pca$var$cos2[, a]), diff = pad(res.pca$var$cos2[, a]) - 0.5,
      row_kind = kind, in_refrow = is_tot, ref = "tot",
      col_var = cv, color = if (color) "difference" else "no")
  }

  eig_tab <- gda_eig_tab(res.pca$eig, n_ind = n_acp, n_axes = n_axes, color = color)
  legend <- c(
    gettext("coord: the variable's coordinate on the axis -- its correlation with it"),
    gettext("contrib: its contribution to the variance of the axis; an axis sums to 100 %"),
    gettext("cos2: quality of representation -- the share of the variable's own variance this axis holds; a variable sums to 100 % over all the axes"),
    gettext("sd/mean: coefficient of variation -- the standard deviation as a percentage of the mean"))

  gda_summary(tabxplor::new_tab(out, meta = list(render_extras = list(n = "no"))),
              eig = if (eig) eig_tab, legend = legend)
}
