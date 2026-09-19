# PURPOSE: The hover text behind every level and point of the three graphs, the feature the package
#   exists for: an MCA level's crosstabs with the active variables, a CA level's profile over the
#   other margin, a PCA supplementary level's means, each cell coloured by tabxplor.
# KEY CONSTRAINTS:
#   - The crosstabs are rowsum()s over UNITS: an MCA's answer profiles crossed with the non-active
#     answers (R/model.R), a CA's table cells, a PCA's distinct individuals. Active against active,
#     an MCA's are the Burt table it was computed from, built from P profiles, not n individuals.
#   - Each block (a row variable crossed with a column variable) is compared to its OWN Total, over
#     the units that answered both: the difference `tabxplor::tab(comp = "tab", na = "drop")`
#     computes. The central row is the population `pop` names, all the units by default.
#   - The cells are `tabxplor::fmt()` records, so their colour (fmt_get_color_code()) and their text
#     (format()) are tabxplor's. Never reach into tabxplor internals.
#   - The "Frequency" denominator is the population, never a block's Total row.
#   - Padding relies on the str_pad() shim's exact stringr semantics: tooltip numbers align under a
#     monospace font, so a vector `width` and a non-space fill are both load-bearing.
#   - Every word goes through gettext (R/i18n.R): the builders run inside with_gda_lang().
# See: CLAUDE.md section ggfacto architecture > The tooltip is the package.

# The header of each level: its name, its variable, its frequency in the population `W`. A row
# whose `vars` is NA is the central point.
#' @keywords internal
#' @noRd
tooltip_header <- function(lvs, vars, n, wn, W) {
  central <- is.na(vars)
  paste0("<b>", ifelse(central, gettext("Central point"), lvs), "</b>",
         ifelse(central, "", paste0("\n", vars)), "\n",
         gettextf("Frequency (n=%s): %s%%", format(as.integer(round(n)), trim = TRUE),
                  format(round(wn / W * 100, 0))))
}

# A block of a tooltip body. `binary`: a two-level question says everything in its first level.
#' @keywords internal
#' @noRd
tip_block <- function(vars, heading = NULL, binary = FALSE, colour_means = FALSE) {
  list(vars = vars, heading = heading, binary = binary, colour_means = colour_means)
}

# The tooltip of every `crossed` and `counted` level, and of the central point: its header, then,
# for the crossed ones and the central point, the body the `blocks` describe. `lines_1lv` show
# their first level alone (a factor) or their mean (a number), at the top.
#' @keywords internal
#' @noRd
interactive_tooltips <- function(units, crossed, counted, blocks = list(),
                                 lines_1lv = character()) {
  units[lines_1lv] <- purrr::map(units[lines_1lv], function(x) {
    if (is.factor(x)) levels(x)[-1] <- "Remove_levels"
    x
  })
  rows <- c(crossed, counted)
  s    <- stack_levels(units, rows)
  lvl  <- rbind(cell_sums(units, s$u, s$g, s$G), c(sum(units$..n), sum(units$..wn)))
  tips <- tibble::tibble(
    vars = c(rep(rows, purrr::map_int(units[rows], nlevels)), NA_character_),
    lvs  = c(unlist(purrr::map(units[rows], levels), use.names = FALSE), NA_character_),
    n = lvl[, 1], wn = lvl[, 2], interactive_text = NA_character_
  )
  if (length(crossed) != 0) {
    at <- which(tips$vars %in% crossed | is.na(tips$vars))
    tips$interactive_text[at] <- tooltip_body(
      units, crossed, c(if (length(lines_1lv)) list(tip_block(lines_1lv)), blocks))
  }
  tips <- tips[!tips$lvs %in% "Remove_levels", ]
  tips$begin_text <- tooltip_header(tips$lvs, tips$vars, tips$n, tips$wn, sum(units$..wn))
  tips[c("vars", "lvs", "begin_text", "interactive_text")]
}

# The body of each crossed level, and of the central point (its last element): each block under its
# heading, one line per level (or mean) of each of its variables. `pop` names the units the central
# row counts; all of them by default.
#' @keywords internal
#' @noRd
tooltip_body <- function(units, crossed, blocks, pop = NULL) {
  s      <- stack_levels(units, crossed)
  cols   <- unique(unlist(purrr::map(blocks, "vars")))
  labels <- tip_labels(units[cols])
  parts  <- purrr::map(blocks, function(b) {
    lines <- purrr::map(b$vars, function(v) {
      if (is.numeric(units[[v]])) {
        if (!b$colour_means) {
          mean <- mean_cells(units, s, v, pop)
          return(matrix(gettextf("%s (mean): %s", v, format(round(mean, 1), trim = TRUE)),
                        ncol = 1))
        }
        f <- mean_lines(units, s, v, pop)
        return(matrix(format_mean(f$diff, f$text, v, f$colour), ncol = 1))
      }
      shown <- levels(units[[v]]) != "Remove_levels"
      if (b$binary && sum(shown) == 2) shown[which(shown)[2]] <- FALSE
      j <- which(shown)
      f <- cells_fmt(crosstab_cells(units, s, v, pop), j, v)
      matrix(format_pct(round(vctrs::field(f, "diff") * 100, 0), format(f),
                        rep(labels[[v]][j], each = s$G + 1L), tabxplor::fmt_get_color_code(f)),
             s$G + 1L)
    })
    lines <- do.call(cbind, lines)
    if (is.null(b$heading)) return(lines)
    # a heading only over a block that has something to say for that level
    cbind(ifelse(rowSums(!is.na(lines)) != 0, paste0("\n<b>", b$heading, "</b>"), NA_character_),
          lines)
  })
  body <- do.call(cbind, parts)
  apply(body, 1, function(l) {
    if (all(is.na(l))) NA_character_ else paste(l[!is.na(l)], collapse = "\n")
  })
}

# The row variables stacked once: each unit once per variable it answered, `g` its level among the
# G stacked levels, `block` the variable each stacked level belongs to.
#' @keywords internal
#' @noRd
stack_levels <- function(units, vars) {
  x   <- purrr::map(units[vars], as.integer)
  nl  <- purrr::map_int(units[vars], nlevels)
  off <- cumsum(c(0L, nl))[seq_along(nl)]
  list(u = unlist(purrr::map(x, function(r) which(!is.na(r))), use.names = FALSE),
       g = unlist(purrr::map2(x, off, function(r, o) r[!is.na(r)] + o), use.names = FALSE),
       block = rep(seq_along(nl), nl), G = sum(nl))
}

#' @keywords internal
#' @noRd
cell_sums <- function(units, i, cell, size) {
  out <- matrix(0, size, 2)
  if (length(i) != 0) {
    agg <- rowsum(cbind(units$..n[i], units$..wn[i]), cell, reorder = TRUE)
    out[as.integer(rownames(agg)), ] <- agg
  }
  out
}

# One column variable against every stacked row level, plus the central row (the population `pop`):
# the counts, the row percentages over the units that answered that column variable, and the
# difference with the row variable's own Total row.
# WARNING: this is tabxplor::tab()'s arithmetic, computed here for speed: it is pinned cell for cell
#   against a direct tab() in tests/testthat/test-tooltips.R and test-ggca.R.
#' @keywords internal
#' @noRd
crosstab_cells <- function(units, s, col, pop = NULL) {
  x   <- as.integer(units[[col]])
  L   <- nlevels(units[[col]])
  ok  <- !is.na(x[s$u])
  cnt <- cell_sums(units, s$u[ok], s$g[ok] + (x[s$u][ok] - 1L) * s$G, s$G * L)
  in_pop <- !is.na(x) & (if (is.null(pop)) TRUE else pop)
  tot <- cell_sums(units, which(in_pop), x[in_pop], L)
  n   <- rbind(matrix(cnt[, 1], s$G), tot[, 1])
  wn  <- rbind(matrix(cnt[, 2], s$G), tot[, 2])
  pct <- wn / rowSums(wn)
  blk <- rowsum(wn[seq_len(s$G), , drop = FALSE], s$block, reorder = TRUE)
  ref <- rbind((blk / rowSums(blk))[s$block, , drop = FALSE], pct[s$G + 1L, ])
  list(n = n, wn = wn, pct = pct, diff = pct - ref)
}

#' @keywords internal
#' @noRd
mean_cells <- function(units, s, col, pop = NULL) {
  x   <- units[[col]]
  ok  <- !is.na(x[s$u])
  i   <- s$u[ok]
  agg <- rowsum(cbind(units$..wn[i], units$..wn[i] * x[i]), s$g[ok], reorder = TRUE)
  out <- rep(NA_real_, s$G)
  out[as.integer(rownames(agg))] <- agg[, 2] / agg[, 1]
  all <- !is.na(x) & (if (is.null(pop)) TRUE else pop)
  c(out, sum(units$..wn[all] * x[all]) / sum(units$..wn[all]))
}

# The mean of a number in every stacked row level against its block's own mean -- the units that
# answered that row variable -- then the population's, coloured by the standardized difference
# (Glass's delta) clust_tab() uses. Returns each row's difference, text and colour code.
# WARNING: one tabxplor record per level, holding it and its block's total: with several total rows
#   in one vector, tabxplor's reference for the colour would be ambiguous (measured).
#' @keywords internal
#' @noRd
mean_lines <- function(units, s, col, pop = NULL) {
  x    <- units[[col]]
  ok   <- !is.na(x[s$u])
  i    <- s$u[ok]
  w    <- units$..wn[i]
  mom  <- function(g, size) {
    M <- matrix(0, size, 4)
    agg <- rowsum(cbind(units$..n[i], w, w * x[i], w * x[i]^2), g, reorder = TRUE)
    M[as.integer(rownames(agg)), ] <- agg
    M
  }
  lv   <- mom(s$g[ok], s$G)
  blk  <- mom(s$block[s$g[ok]], max(s$block))[s$block, , drop = FALSE]
  all  <- !is.na(x) & (if (is.null(pop)) TRUE else pop)
  xa   <- x[all]
  wa   <- units$..wn[all]
  pop_m <- c(sum(units$..n[all]), sum(wa), sum(wa * xa), sum(wa * xa^2))
  moments <- function(M) {
    M <- matrix(M, ncol = 4)
    mean <- M[, 3] / M[, 2]
    list(n = M[, 1], wn = M[, 2], mean = mean, var = M[, 4] / M[, 2] - mean^2)
  }
  a <- moments(lv); b <- moments(blk); p <- moments(pop_m)
  digits <- max(0L, min(3L, 2L - floor(log10(abs(p$mean) + 1e-12))))
  cell <- function(k, ref) tabxplor::fmt(
    n = as.integer(round(c(k$n, ref$n))), wn = c(k$wn, ref$wn), mean = c(k$mean, ref$mean),
    var = c(k$var, ref$var), diff = c(k$mean - ref$mean, 0), scale = "level_mean",
    display = "mean", digits = digits, row_kind = c("data", "total"), ref = "tot",
    col_var = col, color = "difference")
  one <- purrr::map(seq_len(s$G), function(g) {
    f <- cell(purrr::map(a, g), purrr::map(b, g))
    list(diff = a$mean[g] - b$mean[g], text = format(f)[1],
         colour = tabxplor::fmt_get_color_code(f)[1])
  })
  tot <- cell(p, p)
  list(diff   = c(purrr::map_dbl(one, "diff"), 0),
       text   = c(purrr::map_chr(one, "text"), format(tot)[1]),
       colour = c(purrr::map_chr(one, "colour"), NA_character_))
}

# Columns `j` of the cells as one tabxplor record, every row level then the central row, per column.
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
tip_labels <- function(cols) {
  lv     <- purrr::map(cols, function(x) if (is.factor(x)) levels(x) else character())
  all    <- unlist(lv, use.names = FALSE)
  shared <- unique(all[duplicated(all)])
  purrr::imap(lv, function(l, v) dplyr::if_else(l %in% shared, str_c(l, " (", v, ")"), l))
}

#' @keywords internal
#' @noRd
colour_cell <- function(txt, width, color_code) {
  txt <- str_pad(txt, width, pad = "@")
  colored <- !is.na(color_code)
  txt[colored]  <- str_c("<font color=\"", color_code[colored], "\"><b>", txt[colored],
                         "</b></font>")
  txt[!colored] <- paste0(unbrk, txt[!colored])
  txt
}

# A cell as "level: (+diff%) pct", the percentage coloured when tabxplor grades it.
#' @keywords internal
#' @noRd
format_pct <- function(diff, pct, colname, color_code) {
  pct <- colour_cell(pct, 3, color_code)
  dplyr::case_when(
    diff == 0 ~ paste0(colname, ": ", pct),
    diff >  0 ~ paste0(colname, ": ", "(", str_pad(paste0("+", abs(diff)), 3, pad = "@"), "%) ",
                       pct),
    diff <  0 ~ paste0(colname, ": ", "(", str_pad(paste0("-", abs(diff)), 4, pad = "@"), "%) ",
                       pct)
  ) |>
    str_replace_all("@", paste0(unbrk, unbrk, collapse = ""))
}

# A mean as "variable: (+diff) mean", the mean coloured when tabxplor grades it.
#' @keywords internal
#' @noRd
format_mean <- function(diff, mean, colname, color_code) {
  digits <- max(0L, nchar(sub("^[^.,]*[.,]?", "", mean[length(mean)])))
  d <- formatC(abs(diff), format = "f", digits = digits)
  paste0(colname, ": ",
         ifelse(diff == 0 | is.na(diff), "", paste0("(", ifelse(diff > 0, "+", "-"), d, ") ")),
         colour_cell(mean, 0, color_code))
}

# The tooltip of each drawn point: its heading, its counts (the weighted one only when it differs),
# then its lines -- an MCA profile's answers, a PCA individual's values.
#' @keywords internal
#' @noRd
point_tooltips <- function(heading, count, wcount, lines) {
  counts <- list(
    count  = gettextf("n: %s", format(round(count, 0), trim = TRUE, big.mark = " ")),
    wcount = dplyr::if_else(count == wcount, "",
                            paste0(gettextf("weighted n: %s",
                                            format(round(wcount, 0), trim = TRUE,
                                                   big.mark = " ")), "\n"))
  )
  frags <- c(heading, counts, lines)
  names(frags) <- paste0("f", seq_along(frags))
  tidyr::unite(tibble::as_tibble(frags), "text", tidyselect::everything(), sep = "\n",
               na.rm = TRUE)$text
}
