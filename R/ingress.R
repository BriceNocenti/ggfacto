# PURPOSE: What an analysis remembers of its input -- its missing levels, its excluded levels, its
#   weights, the rows of the data frame it was fitted on -- and the one gate through which the
#   microdata comes back.
# ROLE: Shared by the ingress normalisers (multiple_correspondence_analysis(),
#   principal_component_analysis()) and by every function that takes the microdata back afterwards
#   (ggmca_data(), ggmca_3d(), ggpca(), hierarchical_clust(), clust_tab(), axis_coord(),
#   is_in_analysis()).
# KEY CONSTRAINTS:
#   - A missing answer becomes a level named `<VAR>.NA`: unique across variables, so FactoMineR never
#     renames it `var_lv`, and it is the name a user writes in `excl` to drop that one alone.
#   - `excl` names levels EXACTLY, never as a regex: level names hold "+", "?" and "(". `NA` (or
#     "NA") stands for every missing level -- `<VAR>.NA`, or a level literally named "NA".
#   - The fit stores `res$source = list(key, w, wt)`: ONE entry of `key` (and `w`) per row of the
#     REFERENCE frame, the data frame the user named -- its row of `call$X` (for an MCA fitted on
#     the profiles, its answer profile), `NA` if it was left out -- and the name of the weight column.
#   - A row is left out by a proved pipe (reference_rows()), by `filter =`, or by a weight of 0.
#     The fit keeps the order of the data it was given (an `arrange()` in the pipe holds); only
#     the match to the reference frame is recorded.
#   - Every alignment checks the rows it picks: their answers and weights must give back the fit.
#     A frame reordered, shortened or extended after the fit is refused in counts.
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

# The rows of the REFERENCE frame -- the data frame the user named -- that `data` holds, by position.
# `data` may be that frame itself, or a pipe that starts from it (`pc_AGD |> filter(AGE >= 18)`,
# `pc_AGD[pc_AGD$AGE >= 18, ]`), re-run here with a hidden row-id column bound to the root.
# WARNING: never record rows that cannot be proved. The re-run must give back exactly `data`, with
#   unique ids -- which drops a `select()` that loses the id, a random `slice_sample()`, a `%>%` pipe
#   (its expression is only `.`). `data` is then its own reference, and a later alignment on a
#   longer frame stops with an explanation instead of guessing.
reference_rows <- function(expr, env, data) {
  own  <- list(n = nrow(data), ids = seq_len(nrow(data)))
  root <- expr
  while (is.call(root) && length(root) >= 2) root <- root[[2]]
  if (!is.symbol(root) || identical(root, quote(.)) || identical(expr, root)) return(own)

  full <- tryCatch(get(as.character(root), envir = env), error = function(e) NULL)
  if (!is.data.frame(full)) return(own)

  id <- "..ggfacto_row"
  full[[id]] <- seq_len(nrow(full))
  mask <- new.env(parent = env)
  assign(as.character(root), full, envir = mask)
  res <- tryCatch(eval(expr, mask), error = function(e) NULL)
  if (!is.data.frame(res) || !id %in% names(res)) return(own)

  ids <- res[[id]]
  res[[id]] <- NULL
  proved <- !anyNA(ids) && !anyDuplicated(ids) &&
    isTRUE(all.equal(as.data.frame(res), as.data.frame(data), check.attributes = FALSE))
  if (proved) list(n = nrow(full), ids = ids) else own
}

# The one ingress of the rows: which rows of `data` the analysis fits, and where they sit in the
# reference frame. A subset comes from the pipe (reference_rows()), from `filter` (a condition
# evaluated in `data`, NA counting as FALSE, as in dplyr::filter()), and from the weights: a zero
# weight is an out-of-scope row in a survey, left out, since FactoMineR crashes on it in an MCA and
# gives it an infinite coordinate in a PCA. `keep` (a PCA's `ind.sup`) is exempt from both.
# DESIGN: the kept rows stay in the order of `data`, the one the user intended; `rows` places each
#   in the reference frame, and one vector over that frame, `source$key`, records the match.
fitted_rows <- function(expr, env, data, filter = NULL, wt = NULL, keep = integer(),
                        drop = NULL) {
  ref <- reference_rows(expr, env, data)
  sel <- rep(TRUE, nrow(data))
  if (!is.null(filter)) {
    cond <- rlang::eval_tidy(filter, data)
    if (!is.logical(cond) || !length(cond) %in% c(1L, nrow(data))) stop(
      "`filter` must be a condition on the rows of `data`, like in dplyr::filter(): ",
      "`filter = AGE >= 18`.", call. = FALSE)
    sel <- sel & !is.na(cond) & cond
  }
  sel[keep] <- TRUE
  if (!is.null(drop)) {
    out <- sel & drop
    out[keep] <- FALSE
    if (any(out)) message(sum(out), " row(s) with a missing value are left out of the analysis.")
    sel <- sel & !out
  }
  if (!is.null(wt)) {
    bad <- sel & (is.na(wt) | wt < 0)
    bad[keep] <- FALSE
    if (any(bad)) stop(
      "The weights must be positive or zero: ", sum(bad), " row(s) have a missing or negative ",
      "weight. Filter them out before the analysis.", call. = FALSE)
    zero <- sel & !is.na(wt) & wt == 0
    zero[keep] <- FALSE
    if (any(zero)) message(sum(zero), " row(s) with a weight of 0 are left out of the analysis.")
    sel <- sel & !zero
  }
  kept <- which(sel)
  if (length(kept) == 0) stop("No row is left to analyse.", call. = FALSE)
  list(data = data[kept, , drop = FALSE], wt = wt[kept], kept = kept, rows = ref$ids[kept],
       n = ref$n)
}

# `x`, one value per fitted row, spread over the rows of the reference frame, NA elsewhere.
over_reference <- function(x, fr) {
  if (is.null(x)) return(NULL)
  vctrs::vec_assign(vctrs::vec_init(x, fr$n), fr$rows, x)
}

# The fitted rows of an analysis, in the reference frame: `source$key` (a ggfacto fit), else every
# row of the fitted object. `n` is the size of the reference frame. An MCA's rows come in the
# frame's order, its individuals being read through their profiles; a PCA's in the order of
# `call$X`, which its key numbers.
source_positions <- function(res) {
  key <- res$source$key
  if (is.null(key)) {
    n <- nrow(res$call$X)
    return(list(n = n, rows = seq_len(n)))
  }
  rows <- which(!is.na(key))
  if (!is_mca(res)) rows <- rows[order(key[rows])]
  list(n = length(key), rows = rows)
}

# What the alignment needs of a fit: the fitted rows and their active answers. An MCA's come from
# its model, whose profiles `key` expands back to the individuals; a PCA's from its own slots, its
# weights NA on the supplementary individuals it did not weigh.
fit_view <- function(res) {
  if (inherits(res, "ggfacto_fit_view")) return(res)
  if (is_mca(res)) return(mca_model(res))
  vars <- rownames(res$var$coord)
  w    <- rep(NA_real_, nrow(res$call$X))
  sup  <- res$call$ind.sup
  w[if (length(sup) != 0) -sup else seq_along(w)] <- fit_weights(res)
  structure(list(
    n = nrow(res$call$X), vars = vars, X = pca_observed(res), key = NULL, w = w,
    source = c(source_positions(res), list(wt = res$source$wt))
  ), class = "ggfacto_fit_view")
}

# The one reader of the weights of the fitted individuals: an MCA's from its model (`source$w`), a
# PCA's from `row.w.init`, where it keeps them raw.
fit_weights <- function(res) {
  if (is_mca(res) || inherits(res, "ggfacto_mca_model")) {
    v <- fit_view(res)
    return(if (is.null(v$w)) rep(1, v$n) else v$w)
  }
  if (is.null(res$call$row.w.init)) res$call$row.w else res$call$row.w.init
}

# The names of an analysis's active variables, for an MCA or a PCA.
active_names <- function(res) fit_view(res)$vars

# Do rows `idx` of `data` hold the individuals the analysis was fitted on? Their active answers,
# compared as the fit saw them (a missing answer as `<VAR>.NA`), must give back each one's profile,
# and their weights, when `data` has the weight column, each one's weight.
# WARNING: rows swapped with identical answers AND weights go unseen -- they are indistinguishable in
#   every number an analysis computes, so nothing can be misaligned by them.
same_individuals <- function(view, data, idx) {
  if (!all(view$vars %in% names(data))) return(FALSE)
  wt <- view$source$wt
  if (length(wt) == 1 && wt %in% names(data)) {
    w  <- if (is.null(view$w)) rep(1, length(idx)) else view$w
    ok <- !is.na(w)
    if (!isTRUE(all.equal(as.numeric(data[[wt]][idx][ok]), w[ok]))) return(FALSE)
  }
  all(purrr::map_lgl(seq_along(view$vars), function(q) {
    v   <- view$vars[q]
    new <- data[[v]][idx]
    if (is.null(view$key)) {                     # a PCA: its values
      fit <- view$X[[v]]
      ok  <- !is.na(new)          # FactoMineR's PCA imputes a missing value with the column mean
      return(is.numeric(new) && isTRUE(all.equal(as.numeric(new[ok]), as.numeric(fit[ok]))))
    }
    k   <- which(view$levels$vars == v)          # an MCA: each individual's level of its profile
    new <- dplyr::if_else(is.na(new), str_c(v, ".NA"), as.character(new))
    identical(match(new, view$levels$lvs[k]), view$codes[view$key, q] - k[1] + 1L)
  }))
}

# The one gate for microdata handed back after the fit: the positions, in `data`, of the fitted rows.
# `data` is the reference frame the analysis started from, or its fitted rows; either way, checked.
fit_rows <- function(res, data) {
  view  <- fit_view(res)
  src   <- view$source
  n_fit <- length(src$rows)
  # the analysed rows alone are taken in the reference frame's order
  idx   <- if (nrow(data) == src$n) {
    src$rows
  } else if (nrow(data) == n_fit) {
    match(src$rows, sort(src$rows))
  }

  if (is.null(idx)) stop(
    if (n_fit < src$n) {
      str_c("The analysis was fitted on ", n_fit, " of the ", src$n, " rows of its data frame, ",
            "and `data` has ", nrow(data), ": pass that data frame, or its ", n_fit,
            " analysed rows.")
    } else {
      str_c("The analysis was fitted on the ", n_fit, " rows it was given, and `data` has ",
            nrow(data), ". To analyse a subset and still pass the whole data frame, give the ",
            "analysis the whole data frame and `filter = <condition>`, or filter it in the call ",
            "with the native pipe: `data |> dplyr::filter(...) |> multiple_correspondence_analysis(...)`.")
    },
    call. = FALSE)

  if (same_individuals(view, data, idx)) return(idx)
  missing_vars <- setdiff(view$vars, names(data))
  stop(
    if (length(missing_vars) != 0) {
      str_c("`data` lacks the active variable(s) ", str_c(missing_vars, collapse = ", "), ". ")
    } else {
      "The rows of `data` do not hold the answers or weights the analysis was fitted on. "
    },
    "Was the data frame reordered, filtered or modified after the analysis? Pass the data frame ",
    "it was fitted on.", call. = FALSE
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
