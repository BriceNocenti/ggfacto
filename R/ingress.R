# PURPOSE: What an analysis remembers of its input -- its missing levels, its excluded levels, its
#   weights, the rows of the data frame it was fitted on, and its levels as FactoMineR numbers them.
# ROLE: Shared by the ingress normalisers (multiple_correspondence_analysis(),
#   principal_component_analysis()) and by every function that takes the microdata back afterwards
#   (ggmca_data(), ggmca_3d(), ggmca_initial_dims(), hierarchical_clust(), clust_tab()).
# KEY CONSTRAINTS:
#   - A missing answer becomes a level named `<VAR>.NA`: unique across variables, so FactoMineR never
#     renames it `var_lv`, and it is the name a user writes in `excl` to drop that one alone.
#   - `excl` names levels EXACTLY, never as a regex: level names hold "+", "?" and "(". `NA` (or
#     "NA") stands for every missing level -- `<VAR>.NA`, or a level literally named "NA".
#   - The fit stores `res$source = list(n, rows, wt, name)`: the size of the frame the user named,
#     its rows analysed (`NULL`: all), the names of the weight column and of the frame. Rows are
#     recorded only when PROVED; every alignment re-checks the answers, and refuses an edited frame.
#   - A weight of 0 leaves its row out of the fit (recorded in `source`); a missing or negative one
#     is refused. FactoMineR crashes on both, or gives an infinite coordinate.
#   - FactoMineR renames an MCA's levels in its results: mca_levels() reads them by POSITION, and
#     every consumer goes through it rather than matching names.
# See: CLAUDE.md section ggfacto architecture > The FactoMineR contract, and
#   dev/hierarchical_clustering.md section 8 for subpopulations.

# Why this exists: FactoMineR reads a missing answer its own way; one named level per variable lets
# `excl` and the tooltips speak of it like any other level.
na_levels <- function(data, vars) {
  data |>
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(vars) & where(~ is.factor(.) | is.character(.)),
      ~ forcats::fct_na_value_to_level(as.factor(.), str_c(dplyr::cur_column(), ".NA")) |>
        forcats::fct_drop()
    ))
}

# The one `excl` rule, as a named list: variable -> the levels of it to exclude.
excl_levels <- function(data, vars, excl) {
  if (length(excl) == 0) return(list())
  missing_lv <- any(is.na(excl) | excl %in% "NA")
  named      <- unique(excl[!is.na(excl) & !excl %in% "NA"])

  all_lvs <- unlist(purrr::map(vars, ~ levels(as.factor(data[[.]]))))
  unknown <- setdiff(named, all_lvs)
  if (length(unknown) != 0) warning(
    "`excl`: no active level is named ", str_c("\"", unknown, "\"", collapse = ", "),
    ". Levels are matched exactly.", call. = FALSE
  )

  purrr::map(rlang::set_names(vars), function(v) {
    lv <- levels(as.factor(data[[v]]))
    lv[lv %in% named | (missing_lv & lv %in% c(str_c(v, ".NA"), "NA"))]
  }) |>
    purrr::compact()
}

# WARNING: FactoMineR gets POSITIONS in the disjunctive table, never names: it renames a level shared
#   by two variables `var_lv`, so a name can match the wrong column or none. The table's columns are
#   the active variables' levels, concatenated in order.
excl_index <- function(data, vars, excl) {
  ex  <- excl_levels(data, vars, excl)
  pos <- unlist(purrr::map(vars, ~ levels(data[[.]]) %in% ex[[.]]))
  if (any(pos)) which(pos) else NULL
}

# The rows of the data frame the user NAMED that `data` holds: `data` may be that frame itself, or a
# pipe that starts from it (`pc_AGD |> filter(AGE >= 18)`, `pc_AGD[pc_AGD$AGE >= 18, ]`). The pipe is
# re-run with a hidden row-id column bound to the root. `name` is kept only where `n` counts it.
# WARNING: never record rows that cannot be proved. The re-run must give back exactly `data`, with
#   unique ids -- which drops a `select()` that loses the id, a random `slice_sample()`, a `%>%` pipe
#   (its expression is only `.`). Those fall back to "the fitted rows only", and a later alignment on
#   a longer data frame then stops with an explanation instead of guessing.
source_rows <- function(expr, env, data, wt = NULL) {
  root <- expr
  while (is.call(root) && length(root) >= 2) root <- root[[2]]
  name   <- if (is.symbol(root) && !identical(root, quote(.))) as.character(root)
  fitted <- list(n = nrow(data), rows = NULL, wt = wt, name = if (identical(expr, root)) name)
  if (is.null(name) || identical(expr, root)) return(fitted)

  full <- tryCatch(get(as.character(root), envir = env), error = function(e) NULL)
  if (!is.data.frame(full)) return(fitted)

  id <- "..ggfacto_row"
  full[[id]] <- seq_len(nrow(full))
  mask <- new.env(parent = env)
  assign(as.character(root), full, envir = mask)
  res <- tryCatch(eval(expr, mask), error = function(e) NULL)
  if (!is.data.frame(res) || !id %in% names(res)) return(fitted)

  ids <- res[[id]]
  res[[id]] <- NULL
  proved <- !anyNA(ids) && !anyDuplicated(ids) &&
    isTRUE(all.equal(as.data.frame(res), as.data.frame(data), check.attributes = FALSE))
  if (!proved) return(fitted)

  list(n = nrow(full), rows = if (!identical(ids, seq_len(nrow(full)))) ids, wt = wt, name = name)
}

# Why this exists: FactoMineR crashes on a missing weight, crashes on a zero weight in an MCA and
# gives that row an infinite coordinate in a PCA. A zero weight is an out-of-scope row in a survey:
# it is left out of the fit, and `source` records the rows that remain, so a later alignment on the
# whole data frame still finds them. `keep` protects the rows a PCA projects as supplementary.
usable_weights <- function(data, wt, source, keep = integer()) {
  if (is.null(wt)) return(list(data = data, wt = wt, source = source, kept = seq_len(nrow(data))))
  bad <- is.na(wt) | wt < 0
  bad[keep] <- FALSE
  if (any(bad)) stop(
    "The weights must be positive or zero: ", sum(bad), " row(s) have a missing or negative ",
    "weight. Filter them out before the analysis.", call. = FALSE)
  kept <- which(wt > 0 | seq_along(wt) %in% keep)
  if (length(kept) == nrow(data)) return(list(data = data, wt = wt, source = source, kept = kept))
  message(nrow(data) - length(kept), " row(s) with a weight of 0 are left out of the analysis.")
  source$rows <- (if (is.null(source$rows)) seq_len(nrow(data)) else source$rows)[kept]
  list(data = data[kept, , drop = FALSE], wt = wt[kept], source = source, kept = kept)
}

# Why this exists: FactoMineR renames an MCA's levels in its results -- a level two variables share
# becomes `var_lv` (in `call$X` too), a level named y/n/Y/N becomes `var.y` (in `var` and
# `marge.col` only) -- so its rows are matched by POSITION in the indicator table, never by name.
# One row per active level, in the indicator table's order: its variable, its name in the data
# (`lvs`), in `call$X` (`x`) and in FactoMineR's results (`fm`), its column margin, and whether
# `excl` left it out of the axes.
mca_levels <- function(res) {
  X    <- res$call$X[res$call$quali]
  x    <- lapply(X, function(x) levels(as.factor(x)))
  vars <- rep(names(X), lengths(x))
  # FactoMineR prefixes EVERY level of a variable that shares one; undone only then.
  lvs  <- unlist(purrr::imap(x, function(l, v) {
    if (all(startsWith(l, str_c(v, "_")))) str_sub(l, str_length(v) + 2L) else l
  }), use.names = FALSE)
  tibble::tibble(vars = vars, lvs = lvs, x = unlist(x, use.names = FALSE),
                 fm = names(res$call$marge.col), freq = unname(res$call$marge.col),
                 kept = !seq_along(lvs) %in% res$call$excl)
}

# The one reader of the weights a fit was made with, raw: a PCA keeps them so in `row.w.init` only.
fit_weights <- function(res) {
  if (is.null(res$call$row.w.init)) res$call$row.w else res$call$row.w.init
}

# The names of an analysis's active variables, for an MCA or a PCA.
active_names <- function(res) {
  if (inherits(res, "MCA")) names(res$call$X)[res$call$quali] else rownames(res$var$coord)
}

# Do rows `idx` of `data` hold the answers the analysis was fitted on? Compared as the fit saw them:
# a missing answer as `<VAR>.NA`, a level without FactoMineR's `var_` prefix.
same_answers <- function(res, data, idx) {
  vars <- active_names(res)
  if (!all(vars %in% names(data))) return(FALSE)
  as_fit <- function(x, v) {
    if (is.numeric(x)) return(x)
    x <- dplyr::if_else(is.na(x), str_c(v, ".NA"), as.character(x))
    dplyr::if_else(startsWith(x, str_c(v, "_")), str_sub(x, str_length(v) + 2L), x)
  }
  all(purrr::map_lgl(vars, function(v) {
    new <- as_fit(data[[v]][idx], v)
    fit <- as_fit(res$call$X[[v]], v)
    if (is.numeric(new)) {
      ok <- !is.na(new)          # FactoMineR's PCA imputes a missing value with the column mean
      isTRUE(all.equal(as.numeric(new[ok]), as.numeric(fit[ok])))
    } else {
      identical(new, fit)
    }
  }))
}

# The one gate for microdata handed back after the fit: the positions, in `data`, of the fitted rows.
# `data` is the frame the analysis started from, or the fitted rows; either way, answers must match.
fit_rows <- function(res, data) {
  n_fit <- nrow(res$call$X)
  src   <- res$source
  cand  <- list()
  if (!is.null(src$rows) && nrow(data) == src$n) cand <- c(cand, list(src$rows))
  if (nrow(data) == n_fit)                        cand <- c(cand, list(seq_len(n_fit)))

  if (length(cand) == 0) stop(
    "The analysis was fitted on ", n_fit, " rows",
    if (!is.null(src$name)) str_c(" of `", src$name, "`"), ", and `data` has ", nrow(data), ". ",
    if (is.null(src$rows)) str_c(
      "Pass the same rows as the analysis. To analyse a subset and still use the whole data frame, ",
      "filter it inside the call with the native pipe: ",
      "`data |> dplyr::filter(...) |> multiple_correspondence_analysis(...)`."
    ),
    call. = FALSE
  )

  for (idx in cand) if (same_answers(res, data, idx)) return(idx)

  missing_vars <- setdiff(active_names(res), names(data))
  stop(
    if (length(missing_vars) != 0) {
      str_c("`data` lacks the active variable(s) ", str_c(missing_vars, collapse = ", "), ". ")
    } else {
      "The active variables of `data` do not hold the answers the analysis was fitted on. "
    },
    "Was the data frame reordered or modified after the analysis? Pass the data frame it was ",
    "fitted on.", call. = FALSE
  )
}

# The data frame handed back to a graph, cut down to the fitted rows, in the fitted order.
align_to_fit <- function(res, data) {
  data[fit_rows(res, data), , drop = FALSE]
}

# Why this exists: a function that needs the microdata says so in the words of the course, rather
# than failing on "object 'data' not found" deep inside.
need_data <- function(data_missing, why, fn) {
  if (data_missing) stop(
    fn, "() needs the data frame to find ", why, ": pass it second, as in `tab()`, e.g. `",
    fn, "(res, my_data, ...)`.", call. = FALSE
  )
}
