# PURPOSE: The CA entry point and its graph -- correspondence_analysis() (alias CA2()), ggca(), and
#   ca_plot_data(), the CA's builder of the shared plot model (R/plot-model.R).
# ROLE: correspondence_analysis() is the ingress normaliser of a crosstab: it takes the table the
#   student made with tabxplor::tab() and gives FactoMineR the counts it needs. ggca() draws the
#   analysis through the one renderer (R/plot-render.R).
# KEY CONSTRAINTS:
#   - A CA reads COUNTS, whatever the table displays: a tab() in percentages still gives its
#     weighted counts, and its Total rows and column are dropped.
#   - Its supplementary variables ride the table: in a tab() of several row or column variables,
#     the first row variable and the first column variable are the active table, and every other
#     variable gives supplementary rows or columns, passed to FactoMineR as POSITIONS.
#   - Every level is read by POSITION in `call$Xtot`, which holds the whole table in its order:
#     FactoMineR make.unique()s level names. `res$source` records each level's variable and name.
#   - A level's tooltip is its profile against each variable of the other margin, each block against
#     its own Total, computed on the table's cells as the MCA's crosstabs are (R/tooltips.R). Its
#     frequency comes from the margins, never from the stacked blocks, which would count a level
#     once per block.
#   - Clusters are the named factor hierarchical_clust() returns for the levels of one margin. A
#     cluster's label is the mass-weighted barycentre of its levels: the supplementary projection
#     of the merged category.
# See: CLAUDE.md section ggfacto architecture > The FactoMineR contract.

#' Correspondence Analysis of a Crosstab
#'
#' @description Makes the correspondence analysis of a crosstab with
#' \code{FactoMineR::\link[FactoMineR]{CA}}: first make the table with \code{tabxplor::tab()},
#' then analyse it. The analysis reads the (weighted) counts of the table, whatever it displays,
#' without its Total rows and columns. To leave out some rows or columns, filter the table before,
#' with \code{dplyr::filter()} and \code{dplyr::select()}. `CA2()` is a shorter name for the same
#' function.
#'
#' \strong{Supplementary variables} are given in the table itself: in a `tab()` of several row
#' variables, or several column variables, the first row variable and the first column variable
#' make the active table, and the other variables are supplementary, placed on the axes without
#' taking part in them. `tab(data, c(relig, marital), c(partyid, race))` analyses `relig` by
#' `partyid`, and places the levels of `marital` by their profile over `partyid`, and the levels of
#' `race` by their profile over `relig`.
#'
#' @param table A crosstab made with \code{tabxplor::tab()}, with one or several row variables and
#' one or several column variables. A matrix or a \code{table} of counts works too, with its
#' supplementary rows and columns given as in \code{FactoMineR::CA()} (`row.sup`, `col.sup`).
#' @param ncp The number of axes to keep. All of them by default.
#' @param ... Additional arguments to pass to \code{\link[FactoMineR]{CA}}.
#'
#' @return A `CA` object from \pkg{FactoMineR}, which remembers the names of the two active
#' variables, so that \code{\link{interpret}} can print them, and, in `source`, the variable and the
#' name of every row and column of the table.
#' @export
#'
#' @examples
#' gss <- forcats::gss_cat |>
#'   dplyr::filter(!relig %in% c("No answer", "Don't know", "Not applicable"),
#'                 !partyid %in% c("No answer", "Don't know"))
#' tableau <- tabxplor::tab(gss, relig, partyid)
#' res.ca <- correspondence_analysis(tableau)
#' interpret(res.ca)
#'
#' # marital (rows) and race (columns) are supplementary
#' res.ca2 <- tabxplor::tab(gss, c(relig, marital), c(partyid, race)) |>
#'   correspondence_analysis()
#' ggca(res.ca2) |> ggi()
correspondence_analysis <- function(table, ncp = Inf, ...) {
  dots <- list(...)
  tb   <- NULL
  if (inherits(table, "tabxplor_tab")) {
    tb <- ca_table(table)
    X  <- tb$wn
    rs <- union(tb$row_sup, dots$row.sup)
    cs <- union(tb$col_sup, dots$col.sup)
  } else {
    X  <- as.matrix(table)
    rs <- dots$row.sup
    cs <- dots$col.sup
  }
  dots$row.sup <- dots$col.sup <- NULL
  sup_or_null <- function(x) if (length(x) != 0) sort(as.integer(x))
  res <- do.call(ca_fit, c(list(X, ncp, sup_or_null(rs), sup_or_null(cs)), dots))

  # WARNING: FactoMineR::CA() drops `names(dimnames())` from every matrix it keeps; they are written
  #   back on `call$X`, where interpret(), hierarchical_clust() and axis_coord() read them.
  names(dimnames(res$call$X)) <- if (!is.null(tb)) tb$vars else names(dimnames(X))
  if (!is.null(tb)) res$source <- list(rows = tb$rows, cols = tb$cols, counts = tb$n)
  res
}

#' @rdname correspondence_analysis
#' @export
CA2 <- correspondence_analysis

# The one call of FactoMineR::CA(): the matrix travels as a symbol, not inlined into the call.
#' @keywords internal
#' @noRd
ca_fit <- function(X, ncp, row.sup, col.sup, ...) {
  FactoMineR::CA(X, ncp = ncp, row.sup = row.sup, col.sup = col.sup, graph = FALSE, ...)
}

# A tabxplor crosstab as the matrices a CA needs -- weighted and unweighted counts, Totals dropped --
# with the variable and the name of each row and column, and the supplementary positions.
#' @keywords internal
#' @noRd
ca_table <- function(table) {
  s <- tabxplor::tab_structure(table)
  if (length(s$tab_vars) != 0) stop(
    "correspondence_analysis() analyses one table of rows by columns: make it without `tab_vars`.",
    call. = FALSE)
  table <- dplyr::ungroup(table)
  keep  <- !tabxplor::is_totrow(table)
  cols  <- names(table)[purrr::map_lgl(table, tabxplor::is_fmt) & !tabxplor::is_totcol(table)]
  cvar  <- unname(tabxplor::get_col_var(table)[cols])
  if (length(cols) == 0 || any(!nzchar(cvar))) stop(
    "correspondence_analysis() needs a crosstab, with at least one column variable.",
    call. = FALSE)
  if (isTRUE(s$merged)) {
    rvar <- as.character(table$row_var)
    rlv  <- as.character(table$levels)
  } else {
    rvar <- rep(names(table)[1], nrow(table))
    rlv  <- as.character(table[[1]])
  }
  rvar <- rvar[keep]
  rlv  <- rlv[keep]
  # tabxplor names a level two column variables share `<level>_<variable>`
  suffix <- paste0("_", cvar)
  strip  <- endsWith(cols, suffix) & nchar(cols) > nchar(suffix)
  clv    <- ifelse(strip, substr(cols, 1L, nchar(cols) - nchar(suffix)), cols)

  field <- function(f) vapply(cols, function(col) as.numeric(vctrs::field(table[[col]], f)),
                              numeric(nrow(table)))[keep, , drop = FALSE]
  n  <- field("n")
  wn <- field("wn")
  wn[is.na(wn)] <- n[is.na(wn)]
  # a supplementary level named like another one says its variable, as a tooltip line does
  unique_names <- function(lv, var) {
    shared <- lv %in% lv[duplicated(lv)] & var != var[1]
    ifelse(shared, paste0(lv, " (", var, ")"), lv)
  }
  dimnames(wn) <- dimnames(n) <- list(unique_names(rlv, rvar), unique_names(clv, cvar))
  list(wn = wn, n = if (!isTRUE(all.equal(wn, n))) n, vars = c(rvar[1], cvar[1]),
       rows = tibble::tibble(var = rvar, lvs = rlv), cols = tibble::tibble(var = cvar, lvs = clv),
       row_sup = which(rvar != rvar[1]), col_sup = which(cvar != cvar[1]))
}

# The reader of a CA for its graph: each margin's levels by position in `call$Xtot` -- variable,
# name, supplementary or not, coordinates, contributions, counts -- and the active total.
#' @keywords internal
#' @noRd
ca_model <- function(res) {
  Xt  <- as.matrix(res$call$Xtot)
  src <- res$source
  N   <- if (!is.null(src$counts)) src$counts else Xt
  rs  <- as.integer(res$call$row.sup)
  cs  <- as.integer(res$call$col.sup)
  ra  <- setdiff(seq_len(nrow(Xt)), rs)
  ka  <- setdiff(seq_len(ncol(Xt)), cs)
  nm  <- names(dimnames(res$call$X))
  if (length(nm) != 2L || anyNA(nm) || !all(nzchar(nm))) {
    nm <- c(gettext("Rows"), gettext("Columns"))
  }

  margin <- function(side) {
    rows <- side == "row"
    size <- if (rows) nrow(Xt) else ncol(Xt)
    sup  <- if (rows) rs else cs
    act  <- setdiff(seq_len(size), sup)
    lv   <- if (rows) src$rows else src$cols
    var  <- if (!is.null(lv)) lv$var else ifelse(seq_len(size) %in% sup,
      if (rows) gettext("Supplementary rows") else gettext("Supplementary columns"),
      nm[if (rows) 1L else 2L])
    lvs  <- if (!is.null(lv)) lv$lvs else if (rows) rownames(Xt) else colnames(Xt)
    K     <- ncol(as.matrix(res[[side]]$coord))          # a one-axis CA gives vectors
    coord <- contrib <- matrix(NA_real_, size, K)
    coord[act, ]   <- as.matrix(res[[side]]$coord)
    contrib[act, ] <- as.matrix(res[[side]]$contrib)
    if (length(sup) != 0) {
      coord[sup, ] <- as.matrix(res[[paste0(side, ".sup")]]$coord)[, seq_len(K), drop = FALSE]
    }
    wn <- if (rows) rowSums(Xt[, ka, drop = FALSE]) else colSums(Xt[ra, , drop = FALSE])
    n  <- if (rows) rowSums(N[, ka, drop = FALSE])  else colSums(N[ra, , drop = FALSE])
    list(var = var, lvs = lvs, sup = seq_len(size) %in% sup, coord = coord, contrib = contrib,
         n = n, wn = wn, W = stats::ave(wn, var, FUN = sum), vars = unique(var))
  }
  list(Xt = Xt, N = N, rows = margin("row"), cols = margin("col"),
       W = sum(Xt[ra, ka]), n = sum(N[ra, ka]), vars = nm, eig = eig_table(res))
}

# The table's cells as units, one per (row level, column level), each variable a factor NA outside
# its own block: the CA's crosstabs are rowsum()s over them, as the MCA's are over its units.
#' @keywords internal
#' @noRd
ca_units <- function(m, row_lvs, col_lvs) {
  i <- rep(seq_len(nrow(m$Xt)), times = ncol(m$Xt))
  j <- rep(seq_len(ncol(m$Xt)), each  = nrow(m$Xt))
  units <- tibble::tibble(..n = m$N[cbind(i, j)], ..wn = m$Xt[cbind(i, j)])
  add <- function(units, var, lvs, at) {
    for (v in unique(var)) {
      in_v <- var == v
      units[[v]] <- factor(ifelse(in_v[at], lvs[at], NA_character_), unique(lvs[in_v]))
    }
    units
  }
  units <- add(units, m$rows$var, row_lvs, i)
  add(units, m$cols$var, col_lvs, j)
}

# The CA's plot model: both margins' levels (active in their variable's colour, supplementary
# ones too), the clusters of a margin, the central point; no individuals.
#' @keywords internal
#' @noRd
ca_plot_data <- function(res.ca, tooltips, cleannames, color_groups, clust, clust_color_groups,
                         keep_levels, discard_levels, show_sup, uppercase, lang) {
  if (!inherits(res.ca, "CA")) stop(
    "ggca() draws a correspondence analysis, made with correspondence_analysis() or ",
    "FactoMineR::CA().", call. = FALSE)
  clust_name <- if (quo_given(clust)) {
    if (rlang::quo_is_symbol(clust)) rlang::as_name(clust) else "clust"
  }
  clust <- if (quo_given(clust)) rlang::eval_tidy(clust)

  with_gda_lang(lang, function(lg) {
    m    <- ca_model(res.ca)
    rows <- m$rows
    cols <- m$cols
    level <- function(mg, side) {
      tibble::tibble(vars = mg$var, raw = mg$lvs, lvs = clean_levels(mg$lvs, cleannames),
                     role = ifelse(mg$sup, "sup", "active"), side = side, n = mg$n,
                     wcount = mg$wn, W = mg$W)
    }
    lv      <- dplyr::bind_rows(level(rows, "row"), level(cols, "col"))
    coord   <- rbind(rows$coord, cols$coord)
    contrib <- rbind(rows$contrib, cols$contrib)

    # A margin's clusters: its levels take their cluster's colour; the cluster sits at the
    # barycentre of its levels, weighted by their masses.
    cl <- NULL
    if (!is.null(clust)) {
      cl  <- ca_clusters(clust, lv)
      at  <- which(lv$side == cl$side & lv$role == "active")[
        match(cl$lvs, lv$raw[lv$side == cl$side & lv$role == "active"])]
      k   <- clean_factor(factor(cl$clust), cleannames)
      b   <- barycentres(coord[at, , drop = FALSE], lv$wcount[at], k)
      agg <- rowsum(cbind(lv$n[at], lv$wcount[at]), as.integer(k), reorder = TRUE)
      lv  <- dplyr::bind_rows(lv, tibble::tibble(
        vars = clust_name, raw = b$lvs, lvs = b$lvs, role = "clust", side = cl$side,
        n = agg[, 1], wcount = agg[, 2], W = m$W))
      coord   <- rbind(coord, b$coord)
      contrib <- rbind(contrib, matrix(NA_real_, nrow(b$coord), ncol(contrib)))
    }

    vars_data <- vars_rows(lv$vars, lv$lvs, lv$role, wcount = lv$wcount, coord = coord,
                           contrib = contrib)
    vars_data$color_group <- assign_color_groups(
      lv$vars, lv$raw, c(rows$vars, cols$vars, clust_name), color_groups,
      if (!is.null(cl)) clust_name else character(), clust_color_groups)
    sup <- lv$role == "sup"
    vars_data$id <- as.integer(ifelse(sup, cumsum(sup), 1000L + cumsum(!sup)))
    if (!is.null(cl)) {
      is_cl <- lv$role == "clust"
      vars_data$id[is_cl] <- clust_ids(lv$lvs[is_cl], levels(k))
      vars_data$id[at]    <- clust_ids(k, levels(k))
      vars_data$color_group[at] <- vars_data$color_group[is_cl][match(as.character(k),
                                                                      lv$lvs[is_cl])]
    }

    # The tooltips: each level's profile over the other margin, then the central point's.
    units <- ca_units(m, clean_levels(rows$lvs, cleannames), clean_levels(cols$lvs, cleannames))
    if (!is.null(cl)) {
      own <- units[[m$vars[if (cl$side == "row") 1L else 2L]]]
      units[[clust_name]] <- k[match(as.character(own), clean_levels(cl$lvs, cleannames))]
    }
    passes <- list(row = list(crossed = rows$vars, blocks = cols$vars, pop = m$vars[1]),
                   col = list(crossed = cols$vars, blocks = rows$vars, pop = m$vars[2]))
    body <- purrr::imap(passes[intersect(names(passes), tooltips)], function(p, side) {
      crossed <- c(p$crossed, if (!is.null(cl) && cl$side == side) clust_name)
      blocks  <- purrr::map(p$blocks, function(v) tip_block(v, if (v %in% m$vars) {
        gettextf("%s:", v)
      } else {
        gettextf("%s (supplementary):", v)
      }))
      tibble::tibble(
        vars = c(rep(crossed, purrr::map_int(units[crossed], nlevels)), NA_character_),
        lvs  = c(unlist(purrr::map(units[crossed], levels), use.names = FALSE), NA_character_),
        text = tooltip_body(units, crossed, blocks, pop = !is.na(units[[p$pop]])))
    }) |>
      dplyr::bind_rows()

    vars_data$begin_text <- tooltip_header(lv$lvs, lv$vars, lv$n, lv$wcount, lv$W)
    vars_data$interactive_text <- body$text[vctrs::vec_match(
      data.frame(vars = lv$vars, lvs = lv$lvs), data.frame(vars = body$vars, lvs = body$lvs))]
    central <- central_row(m$W, ncol(coord))
    central$begin_text <- tooltip_header(NA, NA, m$n, m$W, m$W)
    central_body <- body$text[is.na(body$vars) & !is.na(body$text)]
    if (length(central_body) != 0) central$interactive_text <- paste(central_body, collapse = "\n")

    keep <- lv$role %in% c("active", "clust") | (show_sup & lv$role == "sup")
    keep <- keep & (lv$role == "clust" | filter_levels(lv$raw, keep_levels, discard_levels))
    up   <- lv$role != "clust" &
      ((lv$side == "col" & "col" %in% uppercase) | (lv$side == "row" & "row" %in% uppercase))
    vars_data$lvs[up] <- toupper(vars_data$lvs[up])

    plot_model(dplyr::bind_rows(vars_data[keep, ], central),
               res = list(eig = m$eig, axes_names = res.ca$axes_names),
               clust = if (!is.null(cl)) clust_name else character(), lang = lg)
  })
}

# The clusters of a CA's margin, from the named factor hierarchical_clust() returns outside mutate():
# which margin its names are the levels of.
#' @keywords internal
#' @noRd
ca_clusters <- function(clust, lv) {
  nm <- names(clust)
  if (is.null(nm) || length(clust) == 0) stop(
    "In a correspondence analysis, `clust` is the clusters of the levels of one margin, named ",
    "after them, as hierarchical_clust() returns them outside mutate(): `ggca(res, clust = ",
    "hierarchical_clust(res, ncp = 2, nb_clust = 4))`.", call. = FALSE)
  for (side in c("row", "col")) {
    if (all(nm %in% lv$raw[lv$side == side & lv$role == "active"])) {
      return(list(side = side, lvs = nm, clust = unname(clust)))
    }
  }
  stop("The names of `clust` are not the levels of one margin of the analysis: ",
       str_c(utils::head(setdiff(nm, lv$raw), 3), collapse = ", "), ".", call. = FALSE)
}


#' Readable and Interactive Graph for Simple Correspondence Analysis
#'
#' @description A readable, complete and beautiful graph of a correspondence analysis. Hovering a
#' level shows its profile --- its distribution over the levels of the other variable, each
#' percentage coloured by its difference from the average profile, as in
#' \code{tabxplor::tab(color = "diff")} --- so the graph is read with the table it draws. The
#' supplementary variables of the table (see \code{\link{correspondence_analysis}}) are drawn in
#' their own colours, and the clusters of one margin, made with \code{\link{hierarchical_clust}},
#' can colour its levels. It is a \pkg{ggplot2} graph, to which elements can be added with `+`;
#' pass it to \code{\link{ggi}} for the interactive version. \code{\link{ggfacto}} is the same
#' graph, for any analysis.
#'
#' @param res.ca An analysis made with \code{\link{correspondence_analysis}} or
#' \code{FactoMineR::\link[FactoMineR]{CA}}.
#' @param axes The axes to print, as a numeric vector of length 2.
#' @param show_sup Set to \code{FALSE} to leave the supplementary rows and columns out.
#' @param xlim,ylim Horizontal and vertical axes limits, as double vectors of length 2.
#' @param out_lims_move When \code{TRUE}, the levels outside \code{xlim} or \code{ylim} are moved
#' to the edges of the graph rather than left out.
#' @param type How the levels are printed: \code{"points"} (coloured points with their names),
#' \code{"text"} (coloured names) or \code{"labels"} (coloured labels).
#' @param text_repel By default, labels are moved so that they do not overlap. Set to
#' \code{FALSE} to print each label exactly at its point.
#' @param uppercase Print the levels of the column variables (\code{"col"}, the default), of the row
#' variables (\code{"row"}), of both or none (\code{NULL}) in uppercase.
#' @param tooltips The tooltips to build: \code{"row"} for the profiles of the row levels,
#' \code{"col"} for those of the column levels, both by default.
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing prefix numbers like
#' \code{"1-"}, and text in parentheses.
#' @param title The title of the graph.
#' @param text_size Size of text.
#' @param dist_labels When \code{type = "points"}, the distance of the names from the points.
#' @param right_margin A margin at the right, in cm.
#' @param size_scale_max The size of the largest point. By default, computed from the spread of the
#' levels' weights.
#' @param use_theme By default, a specific \code{ggplot2} theme is used. Set to \code{FALSE} to
#' customize your own \code{\link[ggplot2:theme]{theme}}.
#' @param clust The clusters of the levels of one margin, as \code{\link{hierarchical_clust}}
#' returns them outside \code{mutate()}: `clust = hierarchical_clust(res.ca, ncp = 2, nb_clust = 4)`.
#' The levels of a cluster take its colour, and the cluster is drawn at their barycentre.
#' @param color_groups One colour per variable by default. A regex matched against each level name
#' makes colour groups within the variables (\code{"^.{1}"}: upon their first character).
#' @param clust_color_groups Color groups for the clusters.
#' @param keep_levels,discard_levels Regexes (or vectors of them) of the levels to keep, or to leave
#' out.
#' @param axes_names Names of all the axes (not just the two selected ones), as a character vector.
#' @param axes_reverse `1` to invert left and right, `2` to invert up and down, `1:2` for both.
#' @param actives_in_bold Set the active levels in bold font.
#' @param sup_in_italic Set the supplementary levels in italics.
#' @param shift_colors Change the colors of the variables.
#' @param colornames_recode A named character vector, in \code{forcats::fct_recode()} style, to
#' rename the colour groups (printed with `options(ggfacto.verbose = TRUE)`).
#' @param scale_color_light,scale_color_dark The colours of the points and of the names.
#' @param get_data Returns the data frames the graph is drawn from, instead of the graph.
#' @param lang \code{NULL} (the session's language), \code{"en"} or \code{"fr"}.
#' @param rowtips_subtitle,coltips_subtitle,rowcolor_numbers,colcolor_numbers,filter Deprecated.
#' A tooltip is headed by its variables' names; \code{color_groups = "^.{2}"} replaces
#' \code{rowcolor_numbers = 2}, and \code{discard_levels} replaces \code{filter}.
#'
#' @return A \code{\link[ggplot2:ggplot]{ggplot}} object, to which elements can be added with
#' \code{+}. Sending it through \code{\link{ggi}} draws the interactive graph.
#' @export
#'
#' @examples
#' \donttest{
#' gss <- forcats::gss_cat |>
#'   dplyr::filter(!relig %in% c("No answer", "Don't know", "Not applicable"),
#'                 !partyid %in% c("No answer", "Don't know"))
#' res.ca <- correspondence_analysis(tabxplor::tab(gss, relig, partyid))
#' ggca(res.ca) |>
#'   ggi()
#'
#' # the clusters of the religions, drawn among them
#' ggca(res.ca, clust = hierarchical_clust(res.ca, ncp = 2, nb_clust = 4))
#' }
ggca <- function(res.ca, axes = c(1,2), show_sup = TRUE, xlim, ylim, out_lims_move = FALSE,
                 type = c("points", "text", "labels"), text_repel = TRUE, uppercase = "col",
                 tooltips = c("row", "col"), rowtips_subtitle, coltips_subtitle,
                 rowcolor_numbers, colcolor_numbers, cleannames = TRUE, filter,
                 title, text_size = 3.5, dist_labels = c("auto", 0.12), right_margin = 0,
                 size_scale_max = NULL, use_theme = TRUE,
                 clust, color_groups = "^.{0}", clust_color_groups = "^.+$",
                 keep_levels, discard_levels, axes_names = NULL, axes_reverse = NULL,
                 actives_in_bold = TRUE, sup_in_italic = TRUE, shift_colors = 0,
                 colornames_recode, scale_color_light = material_colors_light(),
                 scale_color_dark = material_colors_dark(), get_data = FALSE, lang = NULL) {
  # The former arguments keep their places; each says once what replaces it.
  if (!missing(rowtips_subtitle) || !missing(coltips_subtitle)) deprecated_notice(
    "ggca::tips_subtitle", str_c("ggca(rowtips_subtitle =, coltips_subtitle =) are deprecated: ",
                                 "a tooltip is headed by its variables."))
  if (missing(keep_levels))    keep_levels    <- character()
  if (missing(discard_levels)) discard_levels <- character()
  if (!missing(filter)) {
    discard_levels <- c(discard_levels, renamed_arg(filter[nzchar(filter)], "filter",
                                                    "discard_levels", "ggca"))
  }
  if (!missing(rowcolor_numbers) || !missing(colcolor_numbers)) {
    k_row <- if (missing(rowcolor_numbers)) 0L else {
      renamed_arg(rowcolor_numbers, "rowcolor_numbers", "color_groups", "ggca")
    }
    k_col <- if (missing(colcolor_numbers)) 0L else {
      renamed_arg(colcolor_numbers, "colcolor_numbers", "color_groups", "ggca")
    }
    m <- ca_model(res.ca)
    color_groups <- c(rep(paste0("^.{", k_row, "}"), length(m$rows$vars)),
                      rep(paste0("^.{", k_col, "}"), length(m$cols$vars)))
  }
  plot_data <- ca_plot_data(
    res.ca, tooltips = match.arg(tooltips, several.ok = TRUE), cleannames = cleannames,
    color_groups = color_groups, clust = rlang::enquo(clust),
    clust_color_groups = clust_color_groups, keep_levels = keep_levels,
    discard_levels = discard_levels, show_sup = show_sup, uppercase = uppercase, lang = lang)
  ggmca_plot(plot_data, axes = axes, axes_names = axes_names, axes_reverse = axes_reverse,
             type = match.arg(type), text_repel = text_repel, title = title,
             actives_in_bold = actives_in_bold, sup_in_italic = sup_in_italic,
             xlim = xlim, ylim = ylim, out_lims_move = out_lims_move,
             shift_colors = shift_colors, colornames_recode = colornames_recode,
             scale_color_light = scale_color_light, scale_color_dark = scale_color_dark,
             text_size = text_size, size_scale_max = size_scale_max, dist_labels = dist_labels,
             right_margin = right_margin, use_theme = use_theme, get_data = get_data)
}
