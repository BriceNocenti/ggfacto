# PURPOSE: The readers of a fitted analysis -- mca_model(), the one reader of an MCA, and
#   pca_model(), a PCA's individuals as a cloud of points -- the units their consumers aggregate
#   over, and what an analysis gives back to the data frame: axis_coord(), and the write-back it
#   shares with hierarchical_clust(). A CA's reader, ca_model(), lives with it in R/ca.R.
# ROLE: Every consumer of an MCA (the plot model, the tooltips, the clusters, the interpretation
#   table, the teaching plots, the alignment in R/ingress.R) reads the model, never the fitted
#   object's slots. Its unit is the ANSWER PROFILE, a distinct combination of active answers, whose
#   individuals share one point; a PCA's graph reads its distinct individuals the same way.
# KEY CONSTRAINTS:
#   - mca_model() is the only code that knows how each engine stores an MCA: a ggfacto fit
#     (FactoMineR fed the profiles, `source$key` mapping the individuals to them), a
#     FactoMineR::MCA() fit on individuals, GDAtools' speMCA() and csMCA() (its subcloud only).
#   - Levels are read by POSITION in the indicator table, never matched by name: FactoMineR renames
#     a level two variables share (`var_lv`) and a y/n level (`var.y`), GDAtools names every level
#     `var.lv`.
#   - Profiles are numbered by first appearance, so `key` maps each fitted individual to a row of
#     the profile tables, in the order the fit saw them.
#   - A unit is a (profile x non-active answers) cell, with its `..n` and `..wn`: every crosstab,
#     supplementary coordinate and cluster plurality is a rowsum() over the units, never over
#     individuals. cloud_units() serves the PCA's model too, whose points are its distinct
#     individuals.
# See: CLAUDE.md section ggfacto architecture > The FactoMineR contract.

is_mca <- function(res) inherits(res, c("MCA", "speMCA"))

# FactoMineR's eigenvalue table (a matrix, or a data frame of five columns under `excl`) and
# GDAtools' list, as the one three-column matrix every reader indexes. GDAtools pads its axes with
# numerical zeros (1e-32), which are not axes of the cloud.
eig_table <- function(res) {
  e <- res$eig
  if (is.list(e) && !is.data.frame(e)) {
    e <- cbind(e$eigen, e$rate, e$cum.rate)[e$eigen > max(e$eigen) * 1e-12, , drop = FALSE]
    rownames(e) <- paste("dim", seq_len(nrow(e)))
  }
  e <- as.matrix(e)[, 1:3, drop = FALSE]
  colnames(e) <- c("eigenvalue", "percentage of variance", "cumulative percentage of variance")
  e
}

#' @keywords internal
#' @noRd
mca_model <- function(res) {
  if (!is_mca(res)) stop(
    "The analysis must be a multiple correspondence analysis, made with ",
    "multiple_correspondence_analysis() (or FactoMineR::MCA(), GDAtools::speMCA(), csMCA()).",
    call. = FALSE)
  if (length(res$call$ind.sup) != 0) stop(
    "An MCA with supplementary individuals (`ind.sup`) is not supported: fit it without them.",
    call. = FALSE)

  X    <- as.data.frame(res$call$X)[res$call$quali]
  rows <- if (inherits(res, "csMCA")) which(res$call$subcloud)
  if (!is.null(rows)) X <- X[rows, , drop = FALSE]
  # FactoMineR prefixes EVERY level of a variable that shares one; undone only then.
  X[] <- purrr::imap(X, function(x, v) {
    x <- as.factor(x)
    l <- levels(x)
    if (all(startsWith(l, str_c(v, "_")))) levels(x) <- str_sub(l, str_length(v) + 2L)
    x
  })

  # DESIGN: a ggfacto fit was made on the profiles, and remembers each individual's; any other fit
  #   was made on individuals, grouped here -- identical rows of the indicator table share a point.
  src <- res$source
  if (!is.null(src$key)) {
    key   <- src$key
    w     <- src$w
    coord <- res$ind$coord
  } else {
    key   <- as.integer(vctrs::vec_group_id(X))
    first <- which(!duplicated(key))
    X     <- X[first, , drop = FALSE]
    coord <- res$ind$coord[first, , drop = FALSE]
    w     <- res$call$row.w
    if (!is.null(rows)) w <- w[rows]
    if (all(w == 1)) w <- NULL
    src   <- list(n = nrow(res$call$X), rows = rows, wt = NULL, name = NULL)
  }
  coord <- as.matrix(coord)
  colnames(coord) <- paste("Dim", seq_len(ncol(coord)))
  rownames(X) <- NULL

  count <- tabulate(key, nrow(X))
  wn    <- if (is.null(w)) as.numeric(count) else as.vector(rowsum(w, key, reorder = TRUE))
  nlev  <- vapply(X, nlevels, integer(1))
  off   <- c(0L, cumsum(nlev)[-length(nlev)])
  codes <- matrix(vapply(seq_along(X), function(q) as.integer(X[[q]]) + off[q],
                         integer(nrow(X))), nrow(X))

  K   <- sum(nlev)
  Q   <- ncol(X)
  agg <- rowsum(cbind(rep(count, Q), rep(wn, Q)), as.vector(codes), reorder = TRUE)
  lv_n <- lv_wn <- numeric(K)
  lv_n[as.integer(rownames(agg))]  <- agg[, 1]
  lv_wn[as.integer(rownames(agg))] <- agg[, 2]
  W <- sum(wn)

  kept <- !seq_len(K) %in% res$call$excl
  lvs  <- unlist(lapply(X, levels), use.names = FALSE)
  # the level rows under the data's names, whatever the engine called them
  axes <- function(M) {
    M <- as.matrix(M)
    dimnames(M) <- list(lvs[kept], paste("Dim", seq_len(ncol(M))))
    M
  }
  structure(list(
    X = X, codes = codes, count = count, wn = wn, coord = coord,
    key = key, w = w, n = length(key), W = W, Q = Q, vars = names(X),
    levels = tibble::tibble(vars = rep(names(X), nlev), lvs = lvs, kept = kept,
                            n = lv_n, wn = lv_wn, freq = lv_wn / (Q * W)),
    var = list(coord = axes(res$var$coord), contrib = axes(res$var$contrib),
               cos2 = axes(res$var$cos2)),
    eig = eig_table(res), vs = res$svd$vs,
    source = src[c("n", "rows", "wt", "name")]
  ), class = c("ggfacto_mca_model", "ggfacto_fit_view"))
}

# The reader of a PCA for its graph: its active individuals as a cloud of distinct points, the way
# mca_model() gives an MCA's -- `key` maps each individual to its point, `count`/`wn` count them --
# with each point's active values, the user's row numbers and, when the data had them, its name.
# WARNING: FactoMineR names the axes `Dim.k`; they are renamed `Dim k`, the model's one naming.
#' @keywords internal
#' @noRd
pca_model <- function(res) {
  vars <- rownames(res$var$coord)
  X    <- as.data.frame(res$call$X)[vars]
  rows <- seq_len(nrow(X))
  sup  <- res$call$ind.sup
  if (length(sup) != 0) {
    X    <- X[-sup, , drop = FALSE]
    rows <- rows[-sup]
  }
  if (!is.null(res$source$rows)) rows <- res$source$rows[rows]
  names <- rownames(X)
  if (is.null(names) || all(grepl("^[0-9]+$", names))) names <- NULL

  key   <- as.integer(vctrs::vec_group_id(X))
  first <- which(!duplicated(key))
  w     <- fit_weights(res)
  if (all(w == w[1])) w <- NULL
  count <- tabulate(key, length(first))
  wn    <- if (is.null(w)) as.numeric(count) else as.vector(rowsum(w, key, reorder = TRUE))
  coord <- as.matrix(res$ind$coord)[first, , drop = FALSE]
  colnames(coord) <- paste("Dim", seq_len(ncol(coord)))
  rownames(X) <- NULL

  structure(list(
    X = X[first, , drop = FALSE], key = key, w = w, count = count, wn = wn, coord = coord,
    n = length(key), W = sum(wn), vars = vars, rows = rows[first], names = names[first],
    eig = eig_table(res)
  ), class = "ggfacto_pca_model")
}

# The distinct (point x non-active answers) cells of the fitted individuals -- an MCA's answer
# profiles, a PCA's distinct individuals -- `data` holding their non-active variables in the fitted
# order, with each cell's count and weighted count. With no such variable, the units are the
# points themselves.
cloud_units <- function(m, data = NULL) {
  if (length(data) == 0) {
    return(tibble::tibble(..profile = seq_along(m$count), ..n = m$count, ..wn = m$wn))
  }
  g     <- vctrs::vec_group_id(vctrs::vec_cbind(tibble::tibble(..p = m$key), data))
  first <- which(!duplicated(g))
  w     <- if (is.null(m$w)) rep(1, m$n) else m$w
  vctrs::vec_cbind(
    tibble::tibble(..profile = m$key[first], ..n = tabulate(g, length(first)),
                   ..wn = as.vector(rowsum(w, g, reorder = TRUE))),
    tibble::as_tibble(data)[first, , drop = FALSE]
  )
}

# The active answers of `profile` (units' or individuals'), under the data's level names, the
# excluded levels merged into one "Remove_levels", which no tooltip or profile prints.
active_factors <- function(m, profile) {
  lv <- m$levels
  purrr::map(rlang::set_names(seq_along(m$vars), m$vars), function(q) {
    k <- which(lv$vars == m$vars[q])
    x <- structure(m$codes[profile, q] - k[1] + 1L, levels = lv$lvs[k], class = "factor")
    levels(x) <- dplyr::if_else(lv$kept[k], lv$lvs[k], "Remove_levels")
    x
  })
}

# === SECTION: what an analysis gives back to the data frame =======================================

# Why this exists: the values an analysis computes for its fitted rows -- clusters, coordinates --
# written on the rows of a data frame: inside mutate() the frame being written, its rows aligned and
# verified (R/ingress.R); outside, the frame the analysis started from, `NA` on rows it did not use.
to_data_rows <- function(res, values, vars, fn) {
  target <- tryCatch(dplyr::pick(tidyselect::any_of(vars)), error = function(e) NULL)
  if (!is.null(target) && ncol(dplyr::cur_group()) != 0) stop(
    fn, "() must be used in an ungrouped mutate(): call dplyr::ungroup() first.", call. = FALSE)
  if (!is.null(target)) {
    n   <- nrow(target)
    idx <- fit_rows(res, target)
  } else {
    src <- fit_view(res)$source
    if (is.null(src$rows)) return(values)
    n   <- src$n
    idx <- src$rows
  }
  vctrs::vec_assign(vctrs::vec_init(values, n), idx, values)
}

# Why this exists: a CA describes LEVELS; in mutate(), each individual takes the value of its level,
# matched by name, `NA` for a level outside the table.
level_values <- function(values, target, var, fn) {
  if (length(var) == 0 || !var %in% names(target)) stop(
    "In mutate(), ", fn, "() gives each individual the value of its level, which needs ",
    if (length(var) == 0) {
      "the name of the variable: make the analysis with correspondence_analysis(), which keeps it."
    } else {
      str_c("the variable `", var, "` in the data frame.")
    },
    call. = FALSE)
  lv <- as.character(target[[var]])
  lv[is.na(lv)] <- "NA"                            # tab() names the missing answers' level "NA"
  idx <- match(lv, vctrs::vec_names(values))
  if (all(is.na(idx))) stop(
    "No level of `", var, "` in the data frame is a level of the correspondence analysis. Was its ",
    "table made with `cleannames = TRUE`?", call. = FALSE)
  out <- vctrs::vec_slice(values, idx)
  if (is.data.frame(out)) rownames(out) <- NULL else names(out) <- NULL
  out
}

#' Coordinates of the Individuals on the Axes of an Analysis
#'
#' @description
#' The coordinates of each individual on the axes of a principal component analysis or of a
#' multiple correspondence analysis, to write into the data frame with \code{dplyr::mutate()}, like
#' \code{\link{hierarchical_clust}}:
#'
#' `data <- data |> mutate(axe1 = axis_coord(res, 1))`
#'
#' An analysis made with \code{\link{multiple_correspondence_analysis}} is computed on the distinct
#' answer profiles, so its `$ind$coord` has one row per profile: `axis_coord()` gives each
#' individual the coordinate of its profile. In a correspondence analysis, each individual takes the
#' coordinate of its level.
#'
#' @param res An analysis made with \code{\link{multiple_correspondence_analysis}},
#' \code{\link{principal_component_analysis}} or \code{\link{correspondence_analysis}} (or with
#' \code{FactoMineR::MCA()}, \code{PCA()} or \code{CA()}, or \code{GDAtools::speMCA()} or
#' \code{csMCA()}).
#' @param axes The axes. Several axes give a data frame, which \code{mutate()} writes as several
#' columns, named after `axes` when it has names (`c(axe1 = 1, axe2 = 2)`), else `axis1`, `axis2`...
#' @param margin For a correspondence analysis, the variable whose levels give the coordinates:
#' `"rows"`, the default, or `"columns"`.
#'
#' @return One value per row of the data frame: inside \code{dplyr::mutate()}, of the data frame
#' being written, with `NA` on the rows the analysis did not use (when it was made on a subset of
#' the population); outside, of the data frame the analysis started from. For a correspondence
#' analysis outside \code{mutate()}, one value per level, named after it.
#' @export
#'
#' @examples
#' data(tea, package = "FactoMineR")
#' res.mca <- multiple_correspondence_analysis(tea, 1:18)
#'
#' tea <- tea |>
#'   dplyr::mutate(axe1 = axis_coord(res.mca, 1),
#'                 axis_coord(res.mca, c(axe2 = 2, axe3 = 3)))
#'
#' # How much of the first axis does age explain?
#' summary(stats::lm(axe1 ~ age_Q, data = tea))$r.squared
axis_coord <- function(res, axes = 1, margin = "rows") {
  if (!(is_mca(res) || inherits(res, c("PCA", "CA")))) stop(
    "axis_coord() reads a principal component, correspondence or multiple correspondence analysis.",
    call. = FALSE)
  nm   <- names(axes)
  axes <- as.integer(axes)

  if (inherits(res, "CA")) {
    rows  <- match.arg(margin, c("rows", "columns")) == "rows"
    coord <- if (rows) res$row$coord else res$col$coord
    vars  <- names(dimnames(res$call$X))[if (rows) 1L else 2L]
    vars  <- vars[!is.na(vars) & nzchar(vars)]
  } else if (is_mca(res)) {
    m     <- mca_model(res)
    coord <- m$coord[m$key, , drop = FALSE]
    vars  <- m$vars
  } else {
    coord <- res$ind$coord
    sup   <- res$call$ind.sup
    if (length(sup) != 0) {
      all_rows <- matrix(NA_real_, nrow(res$call$X), ncol(coord))
      all_rows[-sup, ] <- coord
      all_rows[sup, ]  <- res$ind.sup$coord
      coord <- all_rows
    }
    vars  <- fit_view(res)$vars
  }
  if (any(axes > ncol(coord))) stop(
    "`axes` goes up to ", max(axes), ", and the analysis keeps ", ncol(coord), " axes.",
    call. = FALSE)

  values <- if (length(axes) == 1) {
    stats::setNames(coord[, axes], if (inherits(res, "CA")) rownames(coord))
  } else {
    out <- tibble::as_tibble(coord[, axes, drop = FALSE], .name_repair = "minimal")
    names(out) <- if (is.null(nm)) str_c("axis", axes) else nm
    if (inherits(res, "CA")) out <- as.data.frame(out, row.names = rownames(coord))
    out
  }

  if (inherits(res, "CA")) {
    target <- tryCatch(dplyr::pick(tidyselect::any_of(vars)), error = function(e) NULL)
    if (is.null(target)) return(values)
    out <- level_values(values, target, vars, "axis_coord")
    return(if (is.data.frame(out)) tibble::as_tibble(out) else out)
  }
  to_data_rows(res, if (is.null(dim(values))) unname(values) else values, vars, "axis_coord")
}
