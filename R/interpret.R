# PURPOSE: the interpretation tables -- interpret() and its three builders, mca_interpret(),
#   ca_interpret() and pca_interpret(), and eigenvalues(), the table they hang under theirs -- and
#   the contract they share with clust_tab() and mean_sd_tab(). interpret() dispatches on the
#   analysis; an MCA is read through its model (R/model.R), whatever engine made it.
# ROLE: reading a factorial analysis WITHOUT the cloud: which points build an axis, on which side,
#   and how well the axis represents them. One family, one output contract.
# KEY CONSTRAINTS:
#   - Every summary is ONE tabxplor table, never a list: gda_summary() tags it `ggfacto_summary`
#     (the subclass hook of tabxplor::new_tab()) and hangs the eigenvalues off it as a subordinate
#     table (tabxplor::set_footer_tabs()), which every exporter then renders under it.
#   - The FORMAT is a print-time decision, and it is TABXPLOR'S: `options(tabxplor.print)` governs a
#     summary exactly as it governs a crosstab, so there is ONE option to teach and to set. The
#     object stays a table one can pipe, filter and export.
#   - Le Roux and Rouanet's rule, everywhere: only a point contributing MORE than the mean
#     contribution of its own set is kept. In an MCA there is one set (the active levels); in a CA
#     there are two (the rows, the columns), whose means differ.
#   - A NUMBER IS NORMALISED BEFORE tabxplor GRADES IT: `ctr` holds the MULTIPLE of the mean
#     contribution and every summary row holds exactly 1, so a table carrying several summary rows
#     (one per axis, or one per set) cannot colour against the wrong one.
#   - ONLY WHAT HAS A LADDER IS COLOURED. The contribution has Le Roux and Rouanet's threshold on
#     every family; a COORDINATE has one only in a PCA, where it IS a correlation and the cloud has
#     few axes. In an MCA or a CA it prints and does not colour -- measured: the PCA's 50 % cos2 rule
#     turns an MCA table entirely red. ⚠ In a PCA the CONTRIBUTION
#     is the one left uncoloured: the coordinate beside it already says which variables build the
#     axis, and grading one fact twice is two saturated channels for one reading. And a cos2 is
#     never graded, on any of the three: its 50 % rule is about a whole cloud, not about a cell.
#   - THE THRESHOLD IS AN ARGUMENT, AND ITS LABEL FOLLOWS IT. `min_contrib` moves the filter; the
#     summary row is "Above mean ctr", "All levels" or "Above 5%" accordingly, and in a CA -- which
#     has two sets -- the margin's name leads it ("Rows: above mean ctr"). A label naming a set
#     it does not total is the one thing this table must never do.
#   - THE COLOUR LEGEND IS TABXPLOR'S, SAYING GGFACTO'S NOUNS. `set_legend_words()` re-states what
#     the ladder grades -- a factorial axis has no chi-squared -- and changes nothing else, so the
#     sentence is built at render, coloured, in the palette and register of the medium that prints
#     it. Under it, one plain glossary line per statistic the colours do NOT grade.
#   - WHAT IS TRANSLATED IS PROSE, NEVER A NAME (R/i18n.R): the legend's words, the glossary, the
#     axis heading, the summary row's label, "Rows"/"Columns"/"Total". A column name or a `col_var`
#     becomes the tibble's own name -- "coord_Axe 1" -- and one that changed with the language could
#     not be indexed.
#   - A COLUMN IS NAMED `<statistic>_<col_var>`, and tabxplor strips the suffix at export
#     (tab_col_var_header): the tibble keeps unique names one can index, and html, markdown and Excel
#     show a bare `coord` under an `Axe 1` span.
# See: CLAUDE.md section ggfacto architecture > Tables are tabxplor's.

# === SECTION: the shared contract =================================================================

#' The interpretation tables, and how they print
#'
#' @description
#' \code{\link{interpret}} (for the three analyses), \code{\link{clust_tab}} and
#' \code{\link{mean_sd_tab}} all return \strong{one}
#' \code{tabxplor} table, so it can be piped, filtered and exported like any other. What differs is
#' only how it is \emph{shown}:
#'
#' \code{options(tabxplor.print = "html")} draws it with \code{\link[tabxplor]{tab_html}}: the
#' Viewer pane in RStudio/Positron, a real html table when knitted. The default, \code{"console"},
#' prints the plain \code{tabxplor} grid. It is the same option that governs an ordinary crosstab
#' --- one to set, once, at the top of a script --- and it is read at print time, so it can be set
#' after the table is built. For a text file or a language model, pipe the table into
#' \code{\link[tabxplor]{tab_md}} explicitly. An html summary of \emph{axes} carries no hover
#' tooltip --- every figure one would reveal already has a column of its own --- while
#' \code{\link{clust_tab}}, being a crosstab of percentages, keeps them: the count behind each one
#' is worth hovering for.
#'
#' An analysis-of-axes summary carries the \strong{eigenvalues} as a subordinate table
#' (\code{\link[tabxplor]{set_footer_tabs}}), which every medium renders under it: the percentage of
#' variance of each axis, its cumulated percentage, and for an MCA Benzecri's modified rate --- the
#' numbers the rule for choosing how many axes to interpret is read on.
#'
#' \code{eig = FALSE} leaves them out, for a document that shows them already or prints the summary
#' several times to comment it column by column; \code{n_axes} says how many of them to print. When
#' some axes are left out --- by \code{n_axes}, or because \code{ncp} truncated the analysis --- a
#' final row states how many the cloud has (\code{... of 27}). A table showing every axis carries no
#' such row. The \code{Total} row is always the whole cloud: 100 \% and the total inertia.
#'
#' \code{min_contrib} moves the threshold: \code{NULL} (the default) keeps the points contributing
#' more than the mean --- Le Roux and Rouanet's rule --- \code{0} keeps them all, and a number keeps
#' what contributes at least that many percent. The summary row's label follows it, so it can never
#' name a set it does not total --- and in a correspondence analysis, where each axis carries two
#' such rows, it leads with the margin's own name (\code{Rows: above mean ctr}, or the name
#' \code{vars} gave it). \code{color = FALSE} builds the table with no colour measure at all.
#'
#' \code{lang} is \code{NULL} (the session's language), \code{"en"} or \code{"fr"}: it
#' translates what a reader reads as prose --- the axis heading, the summary row's label, the words
#' the colour legend uses and the glossary lines under it. \strong{Column names are never
#' translated}: they are the tibble's own names, and a name that changed with the language could not
#' be indexed. These words are fixed when the table is \emph{built}, so an export asking for the
#' other language (\code{tab_md(lang = )}) gets tabxplor's grammar translated and ggfacto's nouns as
#' they were written: \strong{build the table in the language you will print it in}.
#'
#' \code{complete = TRUE} widens an MCA or CA summary: each side of the axis gains the point's
#' \strong{coordinate} (its sign says which pole, its size how far out) and its \strong{cos2} (the
#' share of the point's own variance the axis holds), plus the \emph{spread} between the two sides.
#' Neither is coloured there: a coordinate in axis standard deviations has no conventional cut-off,
#' and an MCA cloud has so many axes that every cos2 is small --- the 50 \% / 75 \% rule a
#' \code{\link{interpret}} table of a PCA reads does not transfer. Both are read by comparing the points
#' shown; only the contribution carries an absolute threshold.
#'
#' @section The footer:
#' The \strong{colour legend is tabxplor's, saying ggfacto's nouns} --- a factorial axis has no
#' chi-squared, so \code{\link[tabxplor]{set_legend_words}} re-states what the ladder grades and
#' nothing else. It is therefore built at render, in the language and the palette of the call that
#' prints it, with its coloured swatches, in all five media. \strong{Nothing to suppress}: a call
#' written by hand is just \code{interpret(res.mca) |> tab_md(css = FALSE, print = FALSE)}.
#' Under it, one plain line names each statistic the colours do \emph{not} grade.
#'
#' @section After a dplyr verb:
#' The subclass is not carried by dplyr (only a table's \code{tabxplor} attributes are), so a summary
#' that has been through \code{mutate()} prints as an ordinary \code{tabxplor} table --- the
#' eigenvalues still render under it, and the format is the same \code{options(tabxplor.print)}
#' either way. What is lost is only the hover policy and the margin names.
#'
#' @name ggfacto_summary
#' @seealso [interpret()], [clust_tab()], [mean_sd_tab()].
NULL


# Tag a finished table as a ggfacto summary: the legend's words, the glossary lines, the
# table under it, the exporter options only ggfacto knows, and the subclass its print methods
# dispatch on.
# DESIGN: the render options ride a PLAIN attribute, not `meta`. dplyr drops both it and the class
#   together, so the options can never outlive the methods that read them.
# WARNING: `set_subtext()` ONLY when there are glossary lines. It hands back the whole template, so
#   an unconditional call would REPLACE the one a table already carries -- and clust_tab()'s comes
#   from tab(), where the caller may have put its own `subtext =` notes through the `...`.
#' @keywords internal
#' @noRd
gda_summary <- function(tabs, footer = NULL, glossary = character(), var_names = NULL,
                        words = NULL, tooltips = FALSE) {
  if (!is.null(words))  tabs <- do.call(tabxplor::set_legend_words, c(list(tabs), words))
  if (length(glossary)) tabs <- tabxplor::set_subtext(tabs, glossary)
  # DESIGN: the one setter of a summary's footer -- the eigenvalues of an axes summary, the
  #   population of an all-mean cluster table -- since set_footer_tabs() replaces what is there.
  if (!is.null(footer)) tabs <- tabxplor::set_footer_tabs(tabs, list(footer))
  attr(tabs, "ggfacto_render") <- list(var_names = var_names, tooltips = tooltips)
  class(tabs) <- unique(c("ggfacto_summary", class(tabs)))
  tabs
}

#' @keywords internal
#' @noRd
gda_plain <- function(x) {
  class(x) <- setdiff(class(x), "ggfacto_summary")
  x
}

# THE one reading of options(tabxplor.print) for a summary -- the three media tabxplor's own router
# names: "html" (with "kable", its pre-2.0.0 synonym), "md", or the console. There is no ggfacto
# option: a summary and a crosstab obey the same one, so a script sets it once.
# WARNING: `getOption()`'s default, never a bare read. tabxplor seeds the option in its .onLoad(), but ggfacto reaches
#   it through `tabxplor::` alone, so `library(ggfacto)` leaves the namespace unloaded and the option
#   UNSET -- on which `switch()` stops. Measured.
# DESIGN: an unknown value falls back to the console, which is what tabxplor's own router does with
#   it -- a summary and the `tab()` two lines above must not disagree about a typo.
#' @keywords internal
#' @noRd
gda_medium <- function() {
  switch(getOption("tabxplor.print", "console"),
         html = , kable = "html", md = "md", "console")
}

# The ONE html render, so print and knit_print cannot drift on the options only ggfacto knows.
# Nothing to suppress in the footer -- the colour legend is tabxplor's, saying ggfacto's words.
# DESIGN: an AXIS SUMMARY carries no tooltip -- every figure it hides already has a column of its
#   own, so the hover would repeat the row. A CLUSTER description is an ordinary crosstab of
#   percentages, and the count behind each one is worth hovering for: clust_tab() asks for them.
#' @keywords internal
#' @noRd
gda_render <- function(x, ...) {
  o <- attr(x, "ggfacto_render")                  # NULL on a plain table: `$` then gives NULL
  args <- utils::modifyList(list(var_names = o$var_names, tooltips = isTRUE(o$tooltips)),
                            rlang::list2(...))
  do.call(tabxplor::tab_html, c(list(gda_plain(x)), args))
}

#' @method print ggfacto_summary
#' @param x A table returned by one of the functions of [ggfacto_summary].
#' @param ... Passed to \code{\link[tabxplor]{tab_html}}, or to the console print method.
#' @return \code{x} invisibly (or the rendered object, for html).
#' @rdname ggfacto_summary
#' @export
print.ggfacto_summary <- function(x, ...) {
  medium <- gda_medium()
  if (identical(medium, "html")) {
    out <- gda_render(x, ...)
    print(out)
    return(invisible(out))
  }
  # The medium is STATED for the delegated call, never left to be re-read: `library(ggfacto)` alone
  # leaves the option unset (see gda_medium()). Console and markdown both delegate -- only html has
  # arguments of its own, which is why it is the one branch here.
  withr::with_options(list(tabxplor.print = medium), print(gda_plain(x), ...))
  invisible(x)
}

# knitr is a soft dependency: registered by hand in .onLoad(), like knit_print.ggfacto_widget().
#' @keywords internal
#' @noRd
knit_print.ggfacto_summary <- function(x, ...) {
  medium <- gda_medium()
  if (identical(medium, "html")) return(knitr::knit_print(gda_render(x)))
  withr::with_options(list(tabxplor.print = medium),
                      knitr::knit_print(gda_plain(x), ...))
}


# === SECTION: the eigenvalues =====================================================================

#' Benzecri's modified rate of variance
#'
#' @param res.mca A multiple correspondence analysis, made with
#' \code{\link{multiple_correspondence_analysis}} (or \code{FactoMineR::MCA()},
#' \code{GDAtools::speMCA()} or \code{csMCA()}).
#' @param fmt By default, the result is given as a numeric vector. Set to `TRUE` to have
#' a \pkg{tabxplor} \code{link[tabxplor]{fmt}} vector instead.
#'
#' @return A numeric vector (or fmt vector with `fmt = TRUE`).
#' @export
#'
#' @examples
#' data(tea, package = "FactoMineR")
#' res.mca <- multiple_correspondence_analysis(tea, 1:18)
#' benzecri_mrv(res.mca)
benzecri_mrv <- function(res.mca, fmt = FALSE) {
  Q   <- length(res.mca$call$quali)
  eig <- eig_table(res.mca)[, 1]
  eig <- eig[eig > 1/Q]
  eig <- (Q/(Q-1))^2 * (eig - 1/Q)^2
  eig <- eig/sum(eig)

  if (fmt) {
    tabxplor::fmt(pct = eig, n = 0, scale = "level_pct", pct_type = "all")
  } else {
    purrr::set_names(eig * 100, paste0("Dim ", 1:length(eig)) )
  }
}


# The eigenvalue table, the subordinate table every axes summary carries: `n_axes` rows at most, and
# an ellipsis row STATING HOW MANY AXES THE CLOUD HAS whenever some are missing from the display.
# DESIGN: the numbers ARE the rule -- a cumulated percentage cannot be read off a bar -- and the
#   data bar behind `% variance` (tabxplor::set_bars()) carries only the SHAPE of the decline, which
#   is the barplot the course draws beside this table, at no cost in width.
# DESIGN: `n_total` is the cloud's axis count, and it is passed in rather than read off `eig`, which
#   `ncp` truncates. It is what makes the ellipsis honest in the two cases that differ: `n_axes` cut
#   the display, or `ncp` cut the analysis. The ellipsis appears iff `n_total > k`, so a table that
#   shows every axis carries none -- an ellipsis over nothing is a lie about the tail.
# WARNING: the last axis is NOT shown after the ellipsis. It gave a row whose numbers cannot be read
#   against the ones above (nothing bridges the gap), where the count alone answers the only question
#   the reader has: how many axes are there. So the label carries the count instead.
# DESIGN: the bar's ceiling is the column's LARGEST axis, `set_bars()`'s default, because the elbow
#   is what a reader looks for there. A ceiling of 100 % was measured and refused: an MCA's raw rates
#   are diluted by construction (`tea`: 9.9, 8.1, 6.0, 5.2 %), so every scree would flatten into
#   stubs. `color = FALSE` draws none -- a blue bar under a table asked for without colour surprises.
#   Total and ellipsis rows take none by construction: tabxplor bars `row_kind == "data"` only.
# WARNING: every column takes a `col_var`, and the two Benzecri ones share theirs: a column with none
#   opens no block, so no vertical rule would say that the modified rate and its cumulation belong
#   together.
#' @keywords internal
#' @noRd
gda_eig_tab <- function(eig, n_ind, mrv = NULL, n_axes = 8L, n_total = NULL, color = TRUE) {
  n    <- nrow(eig)
  # never fewer axes than `eig` holds: a caller's count is a hint, `eig` is a fact.
  n_total <- max(as.integer(n_total), n)
  k    <- max(1L, min(n, as.integer(n_axes)))
  rows <- seq_len(k)
  gap  <- n_total > k                             # are there axes the table does not show?
  # the ellipsis row. `row_kind = "blank"` is the vocabulary tabxplor already has for a row that is a
  # display device rather than data.
  idx  <- if (gap) c(rows, NA_integer_) else rows
  kind <- ifelse(is.na(idx), "blank", "data")
  pick <- function(v) if (gap) c(v[rows], NA_real_) else v[rows]

  # DESIGN: the ellipsis row PRINTS its ellipsis, in every column and not in the label alone -- a row
  #   of blanks reads as missing data where the point is that axes are missing. `"...{tok}"` is a
  #   display template whose token is NA there, so only the literal survives; the unit line is
  #   unmoved, fmt_display_label() polling `row_kind == "data"` / `"total"` alone.
  dots <- function(tok) ifelse(kind == "blank", paste0("...{", tok, "}"), tok)

  pctf <- function(v, col_var) tabxplor::fmt(
    n = rep(n_ind, length(v)), scale = "level_pct", pct_type = "col", pct = v / 100,
    row_kind = kind, col_var = col_var, color = "no", digits = 1L, display = dots("pct"))

  out <- tibble::tibble(
    "Axe" = tabxplor::new_lvl(
      forcats::as_factor(ifelse(is.na(idx), gettextf("... of %s", n_total),
                                paste("Axe", idx))), role = "level"),
    # a variance, and it says so: `display = "var"` (tabxplor phase 6 stopped calling it "mean-var").
    "eigenvalue" = tabxplor::fmt(
      n = rep(n_ind, length(idx)), scale = "level_mean", var = pick(eig[, 1]),
      display = dots("var"), row_kind = kind, col_var = "Variance", color = "no", digits = 3L),
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

  # DESIGN: the Total row is the WHOLE CLOUD -- the axes shown plus those the ellipsis counts, which
  #   `n_total` makes honest -- so it is 100 % and the total inertia, whatever `ncp` kept. The inertia
  #   is read off one axis (eigenvalue / its share), which truncation cannot reach: `sum(eig[, 1])`
  #   gave 6.875 of a PCA's 7 under FactoMineR's default `ncp = 5`.
  inertia <- eig[1, 1] * 100 / eig[1, 2]
  if (!is.finite(inertia) || inertia <= 0) inertia <- sum(eig[, 1])
  tot <- tibble::tibble(
    "Axe" = tabxplor::new_lvl(forcats::as_factor("Total"), role = "level"),
    "eigenvalue" = tabxplor::fmt(n = n_ind, scale = "level_mean", var = inertia,
                                 display = "var", row_kind = "total", col_var = "Variance",
                                 color = "no", digits = 3L),
    "% variance" = tabxplor::fmt(n = n_ind, scale = "level_pct", pct_type = "col", pct = 1,
                                 row_kind = "total", col_var = "Variance", color = "no", digits = 1L),
    "cumul."     = tabxplor::fmt(n = n_ind, scale = "level_pct", pct_type = "col", pct = NA_real_,
                                 row_kind = "total", col_var = "Variance", color = "no", digits = 1L)
  )
  if (!is.null(mrv)) {
    # 1 as well, but for its own reason: a modified rate is normalised over the axes above 1/Q that
    # `eig` HOLDS, so its column sums to 1 whatever the truncation -- truncation moves each RATE.
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

# The eigenvalue table of any analysis -- the one interpret() hangs under its table and
# eigenvalues() prints alone: the individuals it counts, Benzecri's rate for an MCA, and the cloud's
# axis count, which `ncp` does not touch.
#' @keywords internal
#' @noRd
eig_tab_of <- function(res, n_axes = 8L, color = TRUE, model = NULL) {
  if (is_mca(res)) {
    m <- if (is.null(model)) mca_model(res) else model
    # an MCA has (active levels - questions) axes, read off `$var$coord`'s ROWS, which `ncp` keeps
    return(gda_eig_tab(m$eig, n_ind = m$n, mrv = benzecri_mrv(res), n_axes = n_axes,
                       n_total = nrow(m$var$coord) - m$Q, color = color))
  }
  if (inherits(res, "CA")) {
    # a correspondence analysis has min(rows, columns) - 1 axes, counted on its ACTIVE table
    return(gda_eig_tab(res$eig, n_ind = round(sum(res$call$X)), n_axes = n_axes,
                       n_total = min(dim(res$call$X)) - 1L, color = color))
  }
  if (inherits(res, "PCA")) {
    # a PCA has min(active variables, individuals - 1) axes
    n <- nrow(res$ind$coord)
    return(gda_eig_tab(res$eig, n_ind = n, n_axes = n_axes,
                       n_total = min(nrow(res$var$coord), n - 1L), color = color))
  }
  stop("eigenvalues() reads a multiple correspondence, correspondence or principal component ",
       "analysis.", call. = FALSE)
}

#' The Eigenvalues of an Analysis
#'
#' @description The table of the eigenvalues of the axes, the one \code{\link{interpret}} prints
#' under its table: the variance of each axis, its percentage and the cumulated percentage, and for
#' a multiple correspondence analysis Benzecri's modified rate, which corrects the raw percentages.
#' It is read to choose how many axes to interpret.
#'
#' @param res An analysis made with \code{\link{multiple_correspondence_analysis}},
#' \code{\link{correspondence_analysis}} or \code{\link{principal_component_analysis}} (or with
#' \code{FactoMineR::MCA()}, \code{CA()} or \code{PCA()}, or \code{GDAtools::speMCA()} or
#' \code{csMCA()}).
#' @param n_axes How many axes to print. When some are left out, a last row states how many the
#' cloud has.
#' @param color Set to \code{FALSE} to draw no data bar behind the percentages.
#' @param lang \code{NULL} (the session's language), \code{"en"} or \code{"fr"}.
#'
#' @return A \pkg{tabxplor} table, printed as \code{options(tabxplor.print)} says.
#' @export
#' @seealso [interpret()], [benzecri_mrv()].
#'
#' @examples
#' data(tea, package = "FactoMineR")
#' res.mca <- multiple_correspondence_analysis(tea, 1:18)
#' eigenvalues(res.mca)
eigenvalues <- function(res, n_axes = 8L, color = TRUE, lang = NULL) {
  with_gda_lang(lang, function(lg) eig_tab_of(res, n_axes = n_axes, color = color))
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
  # DESIGN: a CA has TWO sets, so its label NAMES the margin it totals. Without it an axis carries the
  #   same word twice -- one row for the rows' contributions, one for the columns' -- and nothing says
  #   which is which. `vars =` is what makes that name a variable's rather than "Rows" / "Columns". An
  #   MCA has one set and keeps the bare label, which the courses and the exploration skill quote.
  bare <- if (is.null(min_contrib))  gettext("Above mean ctr")
          else if (min_contrib <= 0) gettext("All levels")
          else                       gettextf("Above %s%%", format(min_contrib))
  named <- function(s) {
    if (is.null(min_contrib))  gettextf("%s: above mean ctr", s)
    else if (min_contrib <= 0) gettextf("%s: all levels", s)
    else                       gettextf("%s: above %s%%", s, format(min_contrib))
  }
  tot_label <- if (nlevels(kept$set) > 1L) named else function(s) rep(bare, length(s))

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
    dplyr::mutate(group = tot_label(as.character(.data$set)),
                  is_tot = TRUE, block = .Machine$integer.max,
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
gda_poles_tab <- function(packed, axis_label, group_name, n_ind, eig_tab,
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
  # The words are set even under `color = FALSE`: a table can be coloured afterwards with
  # `set_color()`, and a legend the builder never named is one nothing brings back.
  gda_summary(tabs, footer = eig_tab, words = gda_contrib_words(), var_names = "rows",
              glossary = gda_poles_glossary(contrib, complete, color))
}


# === SECTION: the footer ==========================================================================

# The colour legend is TABXPLOR'S, saying ggfacto's nouns: `set_legend_words()` re-states the words
# and nothing else, so the swatches, the ladder, both registers, the publication palettes and the
# five media keep working -- the console included, which no exporter argument can reach.
# DESIGN: the ONE thing a word must carry that the old plain-text line did not -- the sign of the
#   ladder is the POLE of the axis, not an over- or under-contribution. `gda_poles_tab()` negates
#   `ctr` on the negative side, so `.m1`-`.m4` mark the negative pole; the leads say so.
# WARNING: `ref` is what the TERSE form brackets and what a prose lead points at, from this one
#   field. It is accepted only where the baseline is a CONCEPT, which the mean contribution is.
#' @keywords internal
#' @noRd
gda_contrib_words <- function() list(contrib = list(
  word       = gettext("contribution to the variance of the axis"),
  subject    = gettext("a level"),
  lead_over  = gettext("%1$s on the positive side, contributing"),
  lead_under = gettext("%1$s on the negative side, contributing"),
  unit_word  = gettext("the mean contribution"),
  ref        = gettext("the mean contribution")))

# A PCA colours ONE quantity, the coordinate, with the measure `difference` on the standardized
# scale: under `scale.unit` it IS the variable's correlation with the axis, so the 0.1/0.2/0.4/0.8
# ladder reads it end to end. The cos2 beside it is printed and named in the glossary, never graded.
# WARNING: `ref` is REFUSED on `difference`: its baseline is a row of the table, not a concept. The
#   compact form therefore still brackets "(Total)" while the Total row of the column is empty; the
#   prose form, which is what every export prints, does not name it at all. Accepted, not worked
#   around: only tabxplor can widen that.
#' @keywords internal
#' @noRd
gda_pca_words <- function() list(difference = list(
  word_std      = gettext("coordinate on the axis"),
  word_long_std = gettext("coordinate on the axis, i.e. its correlation with it"),
  subject       = gettext("a variable"),
  lead_over     = gettext("%1$s above, by"),
  lead_under    = gettext("%1$s below, by")))

# ONE sentence for the coefficient of variation, shared by pca_interpret() and the deprecated
# mean_sd_tab(): one column, one msgid, one translation.
#' @keywords internal
#' @noRd
gda_cv_line <- function() {
  gettext("sd/mean: coefficient of variation -- the standard deviation as a percentage of the mean, comparable between variables measured in different units")
}

# The glossary: ONE LINE PER STATISTIC THE GENERATED LEGEND DOES NOT COVER, giving its full name and
# nothing else. How to read it lives in the course and in the exploration skill, not under every
# table. The lines name no placeholder, so tabxplor appends them to the template.
# DESIGN: `contrib` and `ctr` are two different columns -- the whole question's contribution, and the
#   level's -- and only the second is coloured. One line each, where a single line used to carry the
#   name of one and the ladder of the other.
#' @keywords internal
#' @noRd
gda_poles_glossary <- function(contrib = TRUE, complete = FALSE, color = TRUE) {
  c(if (!color)   gettext("ctr: the level's contribution to the variance of the axis"),
    if (contrib)  gettext("contrib: the whole question's contribution to the axis"),
    if (complete) c(
      gettext("coord: coordinate on the axis"),
      gettext("cos2: quality of representation"),
      gettext("spread: share of the group's contribution the gap between its two sides accounts for")
    ))
}


# === SECTION: multiple correspondence analysis =====================================================

# The model of an MCA -> one row per (axis, active level). `set` is a single value: every active
# level of an MCA belongs to one population whose contributions sum to 100 % per axis.
#' @keywords internal
#' @noRd
mca_interpret_data <- function(m, axes) {
  if (!inherits(m, "ggfacto_mca_model")) m <- mca_model(m)
  lv <- m$levels[m$levels$kept, ]
  purrr::map_dfr(axes, function(a) tibble::tibble(
    axis  = as.character(a),
    set   = factor("levels"),
    group = lv$vars,
    level = lv$lvs,
    coord = m$var$coord[, a],
    ctr   = m$var$contrib[, a],
    cos2  = m$var$cos2[, a],
    fk    = lv$freq,
    eig   = m$eig[a, 1],
    pct   = round(m$eig[a, 2], 1)
  ))
}


#' Interpret the Axes of an Analysis
#'
#' @description
#' One table to read the axes of a factorial analysis, whatever the analysis:
#'
#' \itemize{
#'   \item a \strong{multiple correspondence analysis}: per axis, the active levels contributing more
#'   than the mean contribution, the positive side facing the negative one, and the spread between
#'   the two sides in percent of each question's contribution (Brigitte Le Roux and Henri Rouanet,
#'   \emph{Geometric data analysis}, Kluwer, 2004; Brigitte Le Roux, \emph{Analyse geometrique des
#'   donnees multidimensionnelles}, Dunod, 2014);
#'   \item a \strong{correspondence analysis}: the same for the row points and the column points,
#'   each margin against its own mean contribution, since each sums to 100 % of the axis over a
#'   different number of points;
#'   \item a \strong{principal component analysis}: each active variable's mean and spread, then,
#'   per axis, its coordinate --- which under `scale.unit` IS its correlation with the axis ---, its
#'   contribution and its cos2.
#' }
#'
#' The eigenvalues of the axes travel under the table, with Benzecri's modified rate for an MCA.
#' `mca_interpret()` and `pca_interpret()` are the same tables, for one analysis each.
#'
#' @param res An analysis made with \code{\link{multiple_correspondence_analysis}},
#' \code{\link{correspondence_analysis}} or \code{\link{principal_component_analysis}} (or with
#' \code{FactoMineR::MCA()}, \code{CA()} or \code{PCA()}, or \code{GDAtools::speMCA()} or
#' \code{csMCA()}).
#' @param ... The arguments below. A correspondence analysis takes one more, `vars`: the two
#' margins' names, as in `vars = c("CSER", "PR2017")`. By default, the names
#' \code{\link{correspondence_analysis}} kept; after a bare \code{FactoMineR::CA()}, which keeps
#' none, the table says \dQuote{Rows} and \dQuote{Columns}.
#' @param res.mca,res.pca The analysis, for `mca_interpret()` and `pca_interpret()`.
#' @param axes The axes to interpret, as an integer vector. By default, the first five of an MCA,
#' two of a CA, three of a PCA.
#' @param complete For an MCA or a CA, set to \code{TRUE} for the fuller summary: each side of the
#' axis gains the point's coordinate and its cos2, and the table gains the spread between the two
#' sides.
#' @param min_contrib For an MCA or a CA, the contribution threshold, in percent. \code{NULL} (the
#' default) is the mean contribution of the point's own set; \code{0} keeps every point.
#' @param color Set to \code{FALSE} to build the table with no colour measure, and no data bar
#' under the eigenvalues.
#' @param eig The eigenvalues travel under the table. Set to \code{FALSE} in a document that already
#' shows them, or that prints the summary several times to comment it column by column.
#' @param n_axes How many axes the eigenvalue table prints. When some are left out, an ellipsis
#' row states how many the cloud has.
#' @param lang \code{NULL} (the session's language), \code{"en"} or \code{"fr"}.
#' @param type Deprecated. The output format is now \code{options(tabxplor.print)}, or an explicit
#' \code{\link[tabxplor]{tab_md}} / \code{\link[tabxplor]{tab_html}} call --- see
#' [ggfacto_summary].
#' @param spread Deprecated. Folded into \code{complete}.
#'
#' @return A \code{tabxplor} table --- see [ggfacto_summary] for how it prints.
#' @export
#' @seealso [ggfacto_summary], [benzecri_mrv()].
#' @examples \donttest{
#' # ONE option decides how every tabxplor table prints, an interpretation table included.
#' # In a script it goes once, at the top, beside the library() calls.
#' options(tabxplor.print = "html")
#'
#' data(tea, package = "FactoMineR")
#' res.mca <- multiple_correspondence_analysis(tea, 1:18)
#' interpret(res.mca)
#' interpret(res.mca, axes = 1:2, complete = TRUE)
#'
#' # a correspondence analysis draws the STRUCTURE of a crosstab's deviations and says nothing of
#' # their size, so the crosstab is asked for beside it, never instead of it:
#' crosstab <- tabxplor::tab(forcats::gss_cat, race, marital)
#' interpret(correspondence_analysis(crosstab))
#' tabxplor::tab(forcats::gss_cat, race, marital, pct = "row", color = "contrib", test = TRUE)
#'
#' cars <- dplyr::rename(mtcars[1:7], weight = wt)
#' interpret(principal_component_analysis(cars, 1:7))
#' }
interpret <- function(res, ...) UseMethod("interpret")

#' @export
#' @noRd
interpret.MCA <- function(res, ...) mca_interpret(res, ...)

#' @export
#' @noRd
interpret.speMCA <- function(res, ...) mca_interpret(res, ...)

#' @export
#' @noRd
interpret.CA <- function(res, ...) ca_interpret(res, ...)

#' @export
#' @noRd
interpret.PCA <- function(res, ...) pca_interpret(res, ...)

#' @export
#' @noRd
interpret.default <- function(res, ...) stop(
  "interpret() reads a multiple correspondence, correspondence or principal component analysis, ",
  "made with multiple_correspondence_analysis(), correspondence_analysis() or ",
  "principal_component_analysis().", call. = FALSE)

#' @rdname interpret
#' @export
mca_interpret <- function(res.mca,
                          axes = 1:5,
                          complete = FALSE,
                          min_contrib = NULL,
                          color = TRUE,
                          eig = TRUE,
                          n_axes = 8L,
                          lang = NULL,
                          type = NULL,
                          spread = NULL) {
  if (!is.null(spread)) complete <- renamed_arg(spread, "spread", "complete", "mca_interpret")
  model <- mca_model(res.mca)
  # an axis the fit did not keep would index past the end of `$var$coord`: it is left out
  axes <- axes[axes <= ncol(model$var$coord)]
  if (!is.null(type))   renamed_arg(type, "type", "options(tabxplor.print)", "mca_interpret")
  with_gda_lang(lang, function(lg) {

  long   <- mca_interpret_data(model, axes)
  packed <- gda_poles(long, min_contrib)

  mrv     <- benzecri_mrv(res.mca)
  eig_tab <- eig_tab_of(res.mca, n_axes = n_axes, color = color, model = model)

  # The axis heading states the raw eigenvalue percentage AND Benzecri's modified rate, which is the
  # number that corrects it -- an MCA's raw percentages understate the first axes badly. An axis
  # below the 1/Q cutoff has no modified rate, and simply does not get the clause.
  m     <- mrv[as.integer(packed$axis)]
  label <- ifelse(is.na(m),
                  gettextf("Axe %s: %s%% of variance", packed$axis, gda_num(packed$pct, lg)),
                  gettextf("Axe %s: %s%% of variance (mod. %s%%)",
                           packed$axis, gda_num(packed$pct, lg), round(m)))

  gda_poles_tab(packed, axis_label = label, group_name = "Question",
                n_ind = model$n,
                eig_tab = if (eig) eig_tab, contrib = TRUE, complete = complete, color = color)
  })
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


# The counterpart of mca_interpret() for a simple correspondence analysis, reached through
# interpret(): per axis, the row points and the column points above their own mean contribution,
# the positive side facing the negative one. The two margins are two populations, each with its own
# threshold and its own summary row.
#' @keywords internal
#' @noRd
ca_interpret <- function(res.ca, axes = 1:2, complete = FALSE, min_contrib = NULL,
                         vars = NULL, color = TRUE, eig = TRUE, n_axes = 8L, lang = NULL) {
  with_gda_lang(lang, function(lg) {
  # WARNING: FactoMineR::CA() drops `names(dimnames())` from every matrix it keeps; only
  #   correspondence_analysis() writes them back on `call$X`. Without either, the two words that are
  #   always true.
  nm <- as.character(if (is.null(vars)) names(dimnames(res.ca$call$X)) else vars)
  if (length(nm) != 2L || !all(nzchar(nm)) || anyNA(nm)) nm <- c(gettext("Rows"), gettext("Columns"))

  axes   <- axes[axes <= nrow(res.ca$eig)]
  long   <- ca_interpret_data(res.ca, axes, nm)
  packed <- gda_poles(long, min_contrib)

  # the ACTIVE table: `Xtot` also holds the supplementary rows and columns
  n_ind   <- round(sum(res.ca$call$X))
  eig_tab <- eig_tab_of(res.ca, n_axes = n_axes, color = color)
  label   <- gettextf("Axe %s: %s%% of variance", packed$axis, gda_num(packed$pct, lg))

  gda_poles_tab(packed, axis_label = label, group_name = "Variable", n_ind = n_ind,
                eig_tab = if (eig) eig_tab,
                contrib = FALSE, complete = complete, color = color)
  })
}


# === SECTION: principal component analysis ========================================================

#' @rdname interpret
#' @export
pca_interpret <- function(res.pca, axes = 1:3, color = TRUE, eig = TRUE, n_axes = 8L,
                          lang = NULL) {
  with_gda_lang(lang, function(lg) {
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
  # first thing the reader meets. ONE fmt record printed three times -- the sd and the coefficient of
  # variation are tokens tabxplor DERIVES from the same variance, so the three columns cannot
  # disagree.
  # ⚠ THE SPREAD IS COMPUTED, NOT READ OFF `call$ecart.type`. That slot is the SCALING divisor, and
  #   under `scale.unit = FALSE` it is 1 for every variable -- a column of standard deviations that
  #   would every one of them read 1. The mean and the weights are FactoMineR's own, so the block
  #   still describes the very cloud the axes were built on.
  act     <- rownames(res.pca$var$coord)
  x_act   <- as.data.frame(res.pca$call$X)[, act, drop = FALSE]
  row_w   <- if (is.null(res.pca$call$row.w)) rep(1, nrow(x_act)) else res.pca$call$row.w
  ctr_all <- res.pca$call$centre
  ctr_pca <- if (!is.null(names(ctr_all)) && all(act %in% names(ctr_all))) ctr_all[act]
             else purrr::map_dbl(x_act, ~ stats::weighted.mean(.x, row_w, na.rm = TRUE))
  var_pca <- purrr::map_dbl(x_act, ~ weighted.var(.x, wt = row_w, na.rm = TRUE))
  univ <- function(display, digits) tabxplor::fmt(
    n = nn, scale = "level_mean", mean = pad(unname(ctr_pca)), var = pad(unname(var_pca)),
    row_kind = kind, col_var = "Variables", color = "no",
    display = display, digits = digits)
  out[["mean_Variables"]]    <- univ("mean", 2L)
  out[["sd_Variables"]]      <- univ("sd"  , 2L)
  out[["sd/mean_Variables"]] <- univ("cv"  , 0L)

  # DESIGN: the column is named `<statistic>_<col_var>`, and tabxplor strips the suffix at export
  #   (tab_col_var_header) -- the same rule that turns "Other_race" into "Other". So the tibble keeps
  #   unique names one can index, and html, markdown and Excel show a bare `coord` under an `Axe 1`
  #   span. The axes are named once, by the span, and never repeated in every header.
  for (a in axes) {
    k  <- sub("^Dim\\.?", "", colnames(res.pca$var$coord)[a])
    cv <- paste("Axe", k)   # a col_var, so never translated: it is part of the column name

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

    # WARNING: NOT coloured either, and the rule is now uniform across the three functions -- a cos2
    #   is never graded. The 50 % rule the course teaches is a rule of thumb about a WHOLE cloud, not
    #   a per-cell threshold: it makes a plausible-looking colour out of a quantity whose only honest
    #   reading is the comparison between the variables displayed. `coord` alone carries the colour.
    out[[paste0("cos2_", cv)]] <- tabxplor::fmt(
      n = nn, scale = "level_pct", pct_type = "row",
      pct = pad(res.pca$var$cos2[, a]),
      row_kind = kind, in_refrow = is_tot, ref = "tot",
      col_var = cv, color = "no")
  }

  eig_tab <- eig_tab_of(res.pca, n_axes = n_axes, color = color)

  # The generated legend names `coord`, the only column it grades; the glossary names the rest.
  glossary <- c(
    gettext("contrib: its contribution to the variance of the axis; an axis sums to 100 %"),
    gettext("cos2: quality of representation"),
    gda_cv_line())

  gda_summary(tabxplor::new_tab(out, meta = list(render_extras = list(n = "no"))),
              footer = if (eig) eig_tab, words = gda_pca_words(), glossary = glossary)
  })
}
