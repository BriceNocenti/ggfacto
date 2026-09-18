# PURPOSE: interactive_tooltips() -- the hover text behind every MCA level.
# ROLE: The feature the package exists for. Crosses the `crossed` variables with the active (and
#   tooltip) variables, renders each cell as HTML, and returns one row per (vars, lvs) -- its header
#   and its body -- for ggmca_data() to join onto the coordinates.
# KEY CONSTRAINTS:
#   - The crosstabs are rowsum()s over the UNITS (R/model.R): the answer profiles, crossed with the
#     non-active answers when there are some. Active against active, it is the Burt table the MCA
#     was computed from, built from P profiles rather than n individuals.
#   - Each block (a row variable crossed with a column variable) is compared to its OWN Total, over
#     the individuals who answered both: the difference `tabxplor::tab(comp = "tab", na = "drop")`
#     computes.
#   - The cells are `tabxplor::fmt()` records, so their colour (fmt_get_color_code()) and their text
#     (format()) are tabxplor's. Never reach into tabxplor internals.
#   - The "Frequency" denominator is the population, the central point's weight, never a block's
#     Total row.
#   - Padding relies on the str_pad() shim's exact stringr semantics: tooltip numbers align under a
#     monospace font, so a vector `width` and a non-space fill are both load-bearing.
# See: CLAUDE.md section ggfacto architecture > The tooltip is the package.

#' @keywords internal
interactive_tooltips <- function(units, crossed, counted, active_vars,
                                 tooltip_vars_1lv = character(), tooltip_vars = character()) {
  units[tooltip_vars_1lv] <- purrr::map(units[tooltip_vars_1lv], function(x) {
    if (is.factor(x)) levels(x)[-1] <- "Remove_levels"
    x
  })
  rows <- c(crossed, counted)
  s    <- stack_levels(units, rows)
  lvl  <- rbind(cell_sums(units, s$u, s$g, s$G), c(sum(units$..n), sum(units$..wn)))
  tips <- tibble::tibble(
    vars = c(rep(rows, purrr::map_int(units[rows], nlevels)), "All"),
    lvs  = c(unlist(purrr::map(units[rows], levels), use.names = FALSE), "Central point"),
    n = lvl[, 1], wcount = lvl[, 2], interactive_text = NA_character_
  )
  if (length(crossed) != 0) {
    at <- which(tips$vars %in% crossed | tips$vars == "All")
    tips$interactive_text[at] <- tooltip_body(units, crossed, active_vars, tooltip_vars_1lv,
                                              tooltip_vars)
  }
  tips <- tips[tips$lvs != "Remove_levels", ]
  tips$begin_text <- paste0(
    "<b>", tips$lvs, "</b>",
    dplyr::if_else(tips$lvs != "Central point", paste0("\n", tips$vars), ""),
    "\nFrequency (n=", as.integer(round(tips$n)), "): ",
    format(round(tips$wcount / sum(units$..wn) * 100, 0)), "%"
  )
  tips[c("vars", "lvs", "begin_text", "interactive_text")]
}

# The body of each crossed level, and of the central point (its last element): the tooltip_vars_1lv
# lines, then the active variables under their heading, then each tooltip variable under its own.
tooltip_body <- function(units, crossed, active_vars, tooltip_vars_1lv, tooltip_vars) {
  s      <- stack_levels(units, crossed)
  cols   <- c(tooltip_vars_1lv, active_vars, tooltip_vars)
  labels <- tip_labels(units[cols])
  head   <- c(rep(TRUE, s$G), FALSE)                      # the central point takes no headings
  lines  <- purrr::map(rlang::set_names(cols), function(v) {
    if (is.numeric(units[[v]])) {
      mean <- mean_cells(units, s, v)
      return(matrix(str_c(v, " (mean): ", format(round(mean, 1), trim = TRUE)), ncol = 1))
    }
    shown <- levels(units[[v]]) != "Remove_levels"
    # DESIGN: a two-level active question says everything in its first level: the second is the
    #   first upside down.
    if (v %in% active_vars && sum(shown) == 2) shown[which(shown)[2]] <- FALSE
    j <- which(shown)
    f <- cells_fmt(crosstab_cells(units, s, v), j, v)
    matrix(format_pct(round(vctrs::field(f, "diff") * 100, 0), format(f),
                      rep(labels[[v]][j], each = s$G + 1L), tabxplor::fmt_get_color_code(f)),
           s$G + 1L)
  })
  for (v in tooltip_vars) {
    h <- head & !is.na(lines[[v]][, 1])
    lines[[v]][h, 1] <- paste0("\n<b>Distribution by ", v, ":</b>\n", lines[[v]][h, 1])
  }
  body <- do.call(cbind, c(lines[tooltip_vars_1lv],
                           list(dplyr::if_else(head, "\n<b>Active variables:</b>", NA_character_)),
                           lines[c(active_vars, tooltip_vars)]))
  apply(body, 1, function(l) if (all(is.na(l))) NA_character_ else paste(l[!is.na(l)], collapse = "\n"))
}

# The row variables stacked once: each unit once per variable it answered, `g` its level among the
# G stacked levels, `block` the variable each stacked level belongs to.
stack_levels <- function(units, vars) {
  x   <- purrr::map(units[vars], as.integer)
  nl  <- purrr::map_int(units[vars], nlevels)
  off <- cumsum(c(0L, nl))[seq_along(nl)]
  list(u = unlist(purrr::map(x, function(r) which(!is.na(r))), use.names = FALSE),
       g = unlist(purrr::map2(x, off, function(r, o) r[!is.na(r)] + o), use.names = FALSE),
       block = rep(seq_along(nl), nl), G = sum(nl))
}

# The count and weighted count of units `i`, summed by `cell`, in a `size` x 2 matrix.
cell_sums <- function(units, i, cell, size) {
  out <- matrix(0, size, 2)
  if (length(i) != 0) {
    agg <- rowsum(cbind(units$..n[i], units$..wn[i]), cell, reorder = TRUE)
    out[as.integer(rownames(agg)), ] <- agg
  }
  out
}

# One column variable against every stacked row level, plus the central row (the population): the
# counts, the row percentages over the individuals who answered that column variable, and the
# difference with the row variable's own Total row.
# WARNING: this is tabxplor::tab()'s arithmetic, computed here for speed: it is pinned cell for cell
#   against a direct tab() in tests/testthat/test-tooltips.R (expect_cells_equal_tab()).
crosstab_cells <- function(units, s, col) {
  x   <- as.integer(units[[col]])
  L   <- nlevels(units[[col]])
  ok  <- !is.na(x[s$u])
  cnt <- cell_sums(units, s$u[ok], s$g[ok] + (x[s$u][ok] - 1L) * s$G, s$G * L)
  tot <- cell_sums(units, which(!is.na(x)), x[!is.na(x)], L)
  n   <- rbind(matrix(cnt[, 1], s$G), tot[, 1])
  wn  <- rbind(matrix(cnt[, 2], s$G), tot[, 2])
  pct <- wn / rowSums(wn)
  blk <- rowsum(wn[seq_len(s$G), , drop = FALSE], s$block, reorder = TRUE)
  ref <- rbind((blk / rowSums(blk))[s$block, , drop = FALSE], pct[s$G + 1L, ])
  list(n = n, wn = wn, pct = pct, diff = pct - ref)
}

# The weighted mean of a number for every stacked row level, then for the population.
mean_cells <- function(units, s, col) {
  x   <- units[[col]]
  ok  <- !is.na(x[s$u])
  i   <- s$u[ok]
  agg <- rowsum(cbind(units$..wn[i], units$..wn[i] * x[i]), s$g[ok], reorder = TRUE)
  out <- rep(NA_real_, s$G)
  out[as.integer(rownames(agg))] <- agg[, 2] / agg[, 1]
  all <- !is.na(x)
  c(out, sum(units$..wn[all] * x[all]) / sum(units$..wn[all]))
}

# Columns `j` of the cells as one tabxplor record, every row level then the central row, per column.
cells_fmt <- function(cl, j, col_var) {
  r <- nrow(cl$n)
  tabxplor::fmt(n = as.integer(round(cl$n[, j])), wn = as.vector(cl$wn[, j]),
                pct = as.vector(cl$pct[, j]), diff = as.vector(cl$diff[, j]),
                scale = "level_pct", digits = 0L, display = "pct",
                row_kind = rep(c(rep("data", r - 1L), "total"), length(j)),
                ref = "tot", pct_type = "row", col_var = col_var, color = "difference")
}

# DESIGN: a line is labelled by its level, as the reader named it. Only a level name several of the
#   printed variables share says which variable it belongs to: "no (lunch)".
tip_labels <- function(cols) {
  lv     <- purrr::map(cols, function(x) if (is.factor(x)) levels(x) else character())
  all    <- unlist(lv, use.names = FALSE)
  shared <- unique(all[duplicated(all)])
  purrr::imap(lv, function(l, v) dplyr::if_else(l %in% shared, str_c(l, " (", v, ")"), l))
}

# A cell as "level: (+diff%) pct", the percentage coloured when tabxplor grades it.
format_pct <- function(diff, pct, colname, color_code) {
  pct <- str_pad(pct, 3, pad = "@")
  colored <- !is.na(color_code)
  pct[colored]  <- str_c("<font color=\"", color_code[colored], "\"><b>", pct[colored],
                         "</b></font>")
  pct[!colored] <- paste0(unbrk, pct[!colored])

  dplyr::case_when(
    diff == 0 ~ paste0(colname, ": ", pct),
    diff >  0 ~ paste0(colname, ": ", "(", str_pad(paste0("+", abs(diff)), 3, pad = "@"), "%) ",
                       pct),
    diff <  0 ~ paste0(colname, ": ", "(", str_pad(paste0("-", abs(diff)), 4, pad = "@"), "%) ",
                       pct)
  ) |>
    str_replace_all("@", paste0(unbrk, unbrk, collapse = ""))
}
