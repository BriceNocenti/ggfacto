# PURPOSE: The plot model the three graphs share -- list(vars_data, ind_data, individuals, res,
#   clust, lang) -- and the helpers the MCA, CA and PCA builders assemble it from.
# KEY CONSTRAINTS:
#   - `role` says what a level row IS (active, sup, clust, central); `color_group` says how it is
#     coloured, and NA means neutral ink -- the MCA's active levels, the CA's supplementary ones,
#     the central point. So no code compares a label to a sentinel string, and a CA can colour its
#     active levels.
#   - Hover ids are one per VARIABLE, so hovering a level lights every level of its variable, and
#     banded: supplementary variables from 1, active ones from 1000, clusters and the points of a
#     cloud from 10000. A cluster's id is matched by its NAME, for its label and its points alike:
#     cleannames re-sorts a factor, so its codes cannot link them.
#   - Every table is flat, with no list-column: a user edits them between the two halves.
#   - The points a graph draws are ranked heaviest first, ties broken by a Weyl sequence on their
#     first row, so a cap on equal weights keeps an evenly spread sample, not the file's first rows.
# See: CLAUDE.md section ggfacto architecture > The plot model.

#' @keywords internal
#' @noRd
vars_rows <- function(vars, lvs, role, color_group = NA_character_, id = NA_integer_,
                      wcount = NA_real_, coord = NULL, contrib = NULL) {
  n   <- length(lvs)
  out <- tibble::tibble(
    vars = as.character(vars), lvs = as.character(lvs), role = rep(role, length.out = n),
    color_group = rep(as.character(color_group), length.out = n),
    id = rep(as.integer(id), length.out = n), wcount = rep(as.numeric(wcount), length.out = n),
    begin_text = NA_character_, interactive_text = NA_character_
  )
  if (!is.null(coord)) {
    coord <- as.matrix(coord)
    colnames(coord) <- paste("Dim", seq_len(ncol(coord)))
    out <- dplyr::bind_cols(out, tibble::as_tibble(coord))
  }
  if (!is.null(contrib)) {
    contrib <- as.matrix(contrib)
    colnames(contrib) <- paste0("contrib", seq_len(ncol(contrib)))
    out <- dplyr::bind_cols(out, tibble::as_tibble(contrib))
  }
  out
}

# The one assembly of a model, so the three builders cannot drift.
#' @keywords internal
#' @noRd
plot_model <- function(vars_data, ind_data = NULL, individuals = NULL, res, clust = character(),
                       lang = NULL) {
  vars_data <- dplyr::bind_rows(vars_data)
  vars_data$lvs         <- forcats::as_factor(vars_data$lvs)
  vars_data$color_group <- forcats::as_factor(vars_data$color_group)
  vars_data <- dplyr::relocate(vars_data, tidyselect::starts_with("Dim "),
                               tidyselect::starts_with("contrib"), .after = dplyr::last_col())
  list(vars_data = vars_data, ind_data = ind_data, individuals = individuals,
       res = list(eig = res$eig, axes_names = res$axes_names), clust = clust,
       lang = gda_resolve_lang(lang))
}

# Its `vars` is not NA, so that `vars_data[vars_data$vars == "x", ]` brings no empty row.
#' @keywords internal
#' @noRd
central_row <- function(W, dims) {
  vars_rows("All", gettext("Central point"), "central", wcount = W, coord = matrix(0, 1, dims))
}

#' @keywords internal
#' @noRd
clean_levels <- function(x, cleannames = TRUE) {
  if (cleannames) str_remove_all(x, cleannames_condition()) else x
}

#' @keywords internal
#' @noRd
clean_factor <- function(x, cleannames = TRUE) {
  if (is.factor(x) && cleannames) levels(x) <- clean_levels(levels(x))
  x
}

# The weighted barycentre of each level of `group`, over the rows of `coord`. A supplementary level
# is that point: in an MCA over the square root of each eigenvalue (`scale`), in a PCA as it is.
#' @keywords internal
#' @noRd
barycentres <- function(coord, w, group, scale = NULL) {
  group <- as.factor(group)
  ok    <- !is.na(group)
  agg   <- rowsum(cbind(w[ok], w[ok] * coord[ok, , drop = FALSE]), as.integer(group)[ok],
                  reorder = TRUE)
  pts   <- agg[, -1L, drop = FALSE] / agg[, 1L]
  if (!is.null(scale)) pts <- t(t(pts) / scale[seq_len(ncol(pts))])
  colnames(pts) <- colnames(coord)
  list(lvs = levels(group)[as.integer(rownames(agg))], wcount = agg[, 1L], coord = pts)
}

# A level's colour group: its variable, then what the variable's regex extracts from its name.
#' @keywords internal
#' @noRd
assign_color_groups <- function(vars, lvs, sup_vars, color_groups, clust = character(),
                                clust_color_groups = "^.+$") {
  others <- setdiff(sup_vars, clust)
  regex  <- stats::setNames(rep(NA_character_, length(sup_vars)), sup_vars)
  regex[others] <- vctrs::vec_recycle(color_groups, length(others))
  if (length(clust) != 0) regex[clust] <- clust_color_groups
  out <- rep(NA_character_, length(vars))
  for (v in intersect(sup_vars, vars)) {
    i <- vars == v
    out[i] <- str_c(v, "_", str_extract(lvs[i], regex[[v]]))
  }
  str_remove(out, "_$")
}

#' @keywords internal
#' @noRd
filter_levels <- function(lvs, keep = character(), discard = character()) {
  ok <- rep(TRUE, length(lvs))
  if (length(keep)    != 0) ok <- ok &  str_detect(lvs, str_c(keep, collapse = "|"))
  if (length(discard) != 0) ok <- ok & !str_detect(lvs, str_c(discard, collapse = "|"))
  ok %in% TRUE
}

#' @keywords internal
#' @noRd
clust_ids <- function(lvs, clust_levels) 10000L + match(as.character(lvs), clust_levels)

# The points of a cloud, aggregated from `group`, ranked by `nb`, `drawn` the first max_profiles.
# DESIGN: heaviest first, the points that stand for the most people; ties are broken by a Weyl
#   sequence on the first row, so on equal weights any prefix is an evenly spread, deterministic
#   systematic sample rather than the first rows of a file that may be sorted.
#' @keywords internal
#' @noRd
cloud_points <- function(group, count, wn, max_profiles) {
  first  <- which(!duplicated(group))
  count  <- as.vector(rowsum(count, group, reorder = TRUE))
  wcount <- as.vector(rowsum(wn, group, reorder = TRUE))
  drawn  <- order(-wcount, (first * 0.618034) %% 1)
  if (length(max_profiles) != 0) drawn <- drawn[seq_len(min(max_profiles, length(drawn)))]
  nb <- rep(NA_integer_, length(first))
  nb[drawn] <- seq_along(drawn)
  list(point = group, first = first, count = count, wcount = wcount, drawn = drawn, nb = nb)
}

# DESIGN: a point's cluster is the weighted plurality of its units, missing ones left out. Clusters
#   made on the analysis are pure within a point (its individuals share it), so the rule only
#   decides for clusters made elsewhere or partly missing, and every point with one clustered
#   individual is coloured.
#' @keywords internal
#' @noRd
point_clusters <- function(unit_point, unit_wn, cl, n_points) {
  by <- tapply(unit_wn, list(factor(unit_point, seq_len(n_points)), cl), sum, default = 0)
  plural <- max.col(by, ties.method = "first")
  plural[rowSums(by) == 0] <- NA
  factor(levels(cl)[plural], levels(cl))
}

#' @keywords internal
#' @noRd
points_table <- function(pts, coord, clusters = NULL, clust_rows = NULL) {
  drawn <- pts$drawn
  out <- tibble::tibble(nb = seq_along(drawn), count = pts$count[drawn],
                        wcount = pts$wcount[drawn])
  if (!is.null(clusters)) {
    out$clust       <- clusters[drawn]
    at              <- match(as.character(out$clust), as.character(clust_rows$lvs))
    out$color_group <- as.character(clust_rows$color_group)[at]
    out$id          <- clust_rows$id[at]
  } else {
    out$color_group <- NA_character_
    out$id          <- 10000L + out$nb
  }
  coord <- as.matrix(coord)[pts$first[drawn], , drop = FALSE]
  colnames(coord) <- paste("Dim", seq_len(ncol(coord)))
  dplyr::bind_cols(out, tibble::as_tibble(coord))
}

# WARNING: one row per INDIVIDUAL, not per point. ggplot2's ellipse sizes its radius on
#   nrow(data) - 1 degrees of freedom, and MASS::cov.trob() tests its convergence on absolute
#   weights: drawn from aggregated points, an ellipse would not be the individuals' one.
#' @keywords internal
#' @noRd
individuals_table <- function(nb, w, coord, extra) {
  coord <- as.matrix(coord)
  colnames(coord) <- paste("Dim", seq_len(ncol(coord)))
  dplyr::bind_cols(tibble::tibble(nb = nb, row.w = w), tibble::as_tibble(coord),
                   tibble::as_tibble(extra))
}

# The supplementary level rows of an MCA or a PCA, at their barycentres over `unit_coord`.
#' @keywords internal
#' @noRd
sup_rows <- function(units, unit_coord, sup_vars, scale, clust, color_groups, clust_color_groups,
                     keep_levels, discard_levels, cleannames) {
  if (length(sup_vars) == 0) return(NULL)
  rows <- purrr::map(sup_vars, function(v) {
    b <- barycentres(unit_coord, units$..wn, units[[v]], scale)
    vars_rows(v, b$lvs, if (v %in% clust) "clust" else "sup", wcount = b$wcount, coord = b$coord)
  }) |>
    dplyr::bind_rows()
  rows$color_group <- assign_color_groups(rows$vars, rows$lvs, sup_vars, color_groups, clust,
                                          clust_color_groups)
  rows <- rows[rows$role == "clust" | filter_levels(rows$lvs, keep_levels, discard_levels), ]
  rows$lvs <- clean_levels(rows$lvs, cleannames)
  rows$id  <- match(rows$vars, unique(rows$vars))
  if (length(clust) != 0) {
    is_clust <- rows$role == "clust"
    rows$id[is_clust] <- clust_ids(rows$lvs[is_clust], clean_levels(levels(units[[clust]]),
                                                                    cleannames))
  }
  rows
}

# The non-active columns a graph reads, as factors, a number kept only where a mean is printed.
#' @keywords internal
#' @noRd
extra_columns <- function(data, vars, numeric_ok = character()) {
  if (length(vars) == 0) return(NULL)
  purrr::imap(data[vars], function(x, v) {
    if (is.numeric(x) && v %in% numeric_ok) x else forcats::fct_drop(as.factor(x))
  }) |>
    tibble::as_tibble()
}
