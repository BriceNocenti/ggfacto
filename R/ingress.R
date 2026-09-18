# PURPOSE: What an analysis remembers of its input -- its missing levels, its excluded levels, and
#   the rows of the data frame it was fitted on.
# ROLE: Shared by the ingress normalisers (multiple_correspondence_analysis(),
#   principal_component_analysis()) and by every function that takes the microdata back afterwards
#   (ggmca_data(), ggmca_3d(), ggmca_initial_dims(), hierarchical_clust(), clust_tab()).
# KEY CONSTRAINTS:
#   - A missing answer becomes a level named `<VAR>.NA`: unique across variables, so FactoMineR never
#     renames it `var_lv`, and it is the name a user writes in `excl` to drop that one alone.
#   - `excl` names levels EXACTLY, never as a regex: level names hold "+", "?" and "(". `NA` (or
#     "NA") stands for every missing level -- `<VAR>.NA`, or a level literally named "NA".
#   - The fit stores `res$source = list(n, rows)`: the row count of the data frame the user named, and
#     which of its rows were analysed (`NULL`: all of them, in order). Rows are recorded only when
#     they are PROVED, and every later alignment re-checks the active answers, so a data frame
#     filtered, reordered or edited after the fit is refused rather than misaligned.
# See: CLAUDE.md section ggfacto architecture > The FactoMineR contract.

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
# re-run with a hidden row-id column bound to the root.
# WARNING: never record rows that cannot be proved. The re-run must give back exactly `data`, with
#   unique ids -- which drops a `select()` that loses the id, a random `slice_sample()`, a `%>%` pipe
#   (its expression is only `.`). Those fall back to "the fitted rows only", and a later alignment on
#   a longer data frame then stops with an explanation instead of guessing.
source_rows <- function(expr, env, data) {
  fitted <- list(n = nrow(data), rows = NULL)
  root   <- expr
  while (is.call(root) && length(root) >= 2) root <- root[[2]]
  if (!is.symbol(root) || identical(expr, root)) return(fitted)

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

  list(n = nrow(full), rows = if (!identical(ids, seq_len(nrow(full)))) ids)
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
# `data` is either the data frame the analysis started from (its rows are then picked out) or the
# fitted rows themselves; either way the active answers must match.
fit_rows <- function(res, data) {
  n_fit <- nrow(res$call$X)
  src   <- res$source
  cand  <- list()
  if (!is.null(src$rows) && nrow(data) == src$n) cand <- c(cand, list(src$rows))
  if (nrow(data) == n_fit)                        cand <- c(cand, list(seq_len(n_fit)))

  if (length(cand) == 0) stop(
    "The analysis was fitted on ", n_fit, " rows, and `data` has ", nrow(data), ". ",
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
