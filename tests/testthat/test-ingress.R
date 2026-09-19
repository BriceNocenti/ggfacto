# PURPOSE: Lock the ingress -- the entry points that compute an analysis, and what the analysis
#   remembers of its input: the `<VAR>.NA` levels, the `excl` rule, and the rows it was fitted on.
# ROLE: multiple_correspondence_analysis(), principal_component_analysis() and
#   correspondence_analysis() are the only functions that compute an analysis; R/ingress.R is what
#   every later function uses to take the microdata back (the other half: test-clust.R).
# KEY CONSTRAINTS:
#   - What is asserted is the FITTED OBJECT's slots -- $call$excl, $call$quali, $source -- because
#     the MCA's model (R/model.R) reads them, and a user's own code may too. The profile fit itself
#     is pinned in test-model.R.
#   - The subset matrix is pinned in both directions: every pipe shape that CAN be proved, and
#     `filter =`, record the rows of the reference frame; every shape that cannot makes the subset
#     its own reference -- never a guess. And every way a data frame handed back can differ from
#     the analysed one is refused.
# See: CLAUDE.md section ggfacto architecture > The FactoMineR contract.

# --- the names ----------------------------------------------------------------------------------

test_that("PCA2(), the 0.3.2 name, is the same function as the long one", {
  expect_identical(PCA2, principal_component_analysis)
})

# --- MCA2(): the 0.3.2 fit, on the individuals --------------------------------------------------

test_that("MCA2() fits the individuals, with the long name's arguments", {
  d   <- fx_tea_na()
  d$w <- rep(c(0.5, 1.5, 2), length.out = nrow(d))
  res <- MCA2(d, 1:6, wt = w)
  expect_identical(formals(MCA2), formals(multiple_correspondence_analysis))
  expect_identical(nrow(res$ind$coord), nrow(d))
  expect_identical(res$source$key, seq_len(nrow(d)))
  ref <- mca_ind(d, 1:6, wt = "w")
  expect_equal(res$eig, ref$eig, tolerance = 1e-10)
  expect_equal(abs(res$ind$coord), abs(ref$ind$coord), tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("MCA2() gives the long name's model, hence its graphs, tables and clusters", {
  d   <- fx_tea_na()
  d$w <- rep(c(0.5, 1.5, 2), length.out = nrow(d))
  a <- mca_model(fit_mca(d, 1:6, wt = w))
  b <- mca_model(MCA2(d, 1:6, wt = w))
  for (el in c("X", "codes", "key", "w", "count", "wn", "levels", "source"))
    expect_identical(b[[el]], a[[el]], label = el)
  expect_equal(abs(b$coord), abs(a$coord), tolerance = 1e-10)

  # the fit keeps the piped order: HCPC()$data.clust follows it
  arranged <- d |> dplyr::arrange(age) |> MCA2(1:6)
  expect_identical(arranged$source$key[order(d$age)], seq_len(nrow(d)))
  expect_identical(mca_model(arranged)$key,
                   mca_model(d |> dplyr::arrange(age) |> fit_mca(1:6))$key)
})

test_that("FactoMineR::HCPC() of MCA2() classifies the individuals, as the 0.3.2 course did", {
  d   <- fx_tea_na()
  d$w <- rep(c(0.5, 1.5, 2), length.out = nrow(d))
  acm_2axes <- MCA2(d, 1:6, wt = w, ncp = 2)
  cah <- FactoMineR::HCPC(acm_2axes, nb.clust = 4, graph = FALSE)
  old <- dplyr::mutate(d, cl = cah$data.clust$clust)
  new <- dplyr::mutate(d, cl = hierarchical_clust(fit_mca(d, 1:6, wt = w), ncp = 2, nb_clust = 4,
                                                     tree = FALSE))
  expect_identical(as.integer(old$cl), as.integer(new$cl))
})

test_that("MCA2() records a subset as the long name does", {
  d     <- fx_tea()
  young <- which(d$age < 30)
  piped <- d |> dplyr::filter(age < 30) |> MCA2(1:6)
  expect_identical(nrow(piped$ind$coord), length(young))
  expect_identical(which(!is.na(piped$source$key)), young)
  expect_identical(MCA2(d, 1:6, filter = age < 30)$source, piped$source)
  local_fit <- function(local_data) local_data |> dplyr::filter(age < 30) |> MCA2(1:6)
  expect_identical(local_fit(d)$source, piped$source)
  expect_equal(abs(dplyr::mutate(d, x = axis_coord(piped, 1))$x),
               abs(dplyr::mutate(d, x = axis_coord(fx_mca_young(), 1))$x), tolerance = 1e-10)
})

# --- active_vars accepts what tidyselect accepts ------------------------------------------------

test_that("the MCA ingress takes active_vars by position, by name and by all_of()", {
  # The three spellings the examples and the README use must select the same six columns.
  by_index <- fit_mca(fx_tea(), 1:6, ncp = 2)
  by_name  <- fit_mca(fx_tea(), tidyselect::all_of(fx_active()), ncp = 2)

  expect_identical(colnames(by_index$call$X), colnames(by_name$call$X))
  expect_identical(by_index$call$quali, by_name$call$quali)
  expect_equal(by_index$eig, by_name$eig)
})

test_that("the active variables are read from the fit, in their order", {
  expect_identical(mca_model(fx_mca())$vars, fx_active())
  expect_identical(active_names(fx_mca()), fx_active())
})

test_that("PCA2 takes active_vars by position and by name alike", {
  by_index <- PCA2(mtcars, 1:7)
  by_name  <- PCA2(mtcars, tidyselect::all_of(names(mtcars)[1:7]))
  expect_equal(by_index$eig, by_name$eig)
})

# --- weights ------------------------------------------------------------------------------------

test_that("the MCA keeps the individuals' weights, and gives each profile the sum of its own", {
  # The weights ride one channel: `source$w`, the individuals', from which every tooltip, table and
  # cluster reads them; `call$row.w` is what FactoMineR was fed, one weight per answer profile.
  res <- fx_mca_wt()
  expect_identical(fit_weights(res), fx_tea_wt()$w)
  expect_equal(sum(res$call$row.w), sum(fx_tea_wt()$w))
})

test_that("an unweighted MCA counts each individual once", {
  expect_identical(fit_weights(fx_mca()), rep(1, nrow(fx_tea())))
  expect_equal(sum(fx_mca()$call$row.w), nrow(fx_tea()))
})

test_that("weighting changes the coordinates it is supposed to change", {
  # Guards against wt = being silently dropped, which would look like success everywhere else.
  expect_false(isTRUE(all.equal(unname(fx_mca()$var$coord),
                                unname(fx_mca_wt()$var$coord))))
})

test_that("PCA2 accepts wt and reaches row.w", {
  d <- mtcars[1:7]
  d$w <- rep(c(0.5, 1.5), length.out = nrow(d))
  res <- PCA2(d, active_vars = 1:7, wt = "w")
  expect_length(res$call$row.w, nrow(d))
  expect_equal(res$call$row.w / sum(res$call$row.w), d$w / sum(d$w))

  # wt and ind_name are selected as in tab(): a bare name works like the string
  bare <- PCA2(d, active_vars = 1:7, wt = w)
  expect_equal(bare$call$row.w, res$call$row.w)
  d$car <- rownames(d)
  named <- PCA2(d, active_vars = 1:7, wt = w, ind_name = car)
  expect_identical(rownames(named$ind$coord), rownames(mtcars))
})

test_that("a zero weight leaves its row out, and the whole data frame still aligns", {
  # FactoMineR's MCA() crashes on a zero weight; in a survey it marks an out-of-scope row.
  d <- fx_tea(); d$w <- c(0, 0, rep(1, nrow(d) - 2))
  expect_message(res <- fit_mca(d, 1:6, wt = "w"), "2 row")
  expect_length(res$source$key, nrow(d))
  expect_identical(which(!is.na(res$source$key)), 3:nrow(d))
  clust <- dplyr::mutate(d, cl = hierarchical_clust(res, ncp = 2, nb_clust = 3, tree = FALSE))$cl
  expect_true(all(is.na(clust[1:2])) && !anyNA(clust[-(1:2)]))
  expect_no_error(quietly(ggmca(res, d, sup_vars = "SPC")))
})

test_that("a missing or negative weight is refused in words", {
  d <- fx_tea(); d$w <- c(NA, rep(1, nrow(d) - 1))
  expect_error(fit_mca(d, 1:6, wt = "w"), "positive or zero")
  d$w[1] <- -1
  expect_error(fit_mca(d, 1:6, wt = "w"), "positive or zero")
})

test_that("a PCA leaves a zero-weight row out, and keeps its supplementary rows", {
  # FactoMineR's PCA() gives a zero-weight row infinite coordinates.
  d <- mtcars[1:7]; d$w <- c(0, rep(1, 31))
  res <- suppressMessages(PCA2(d, 1:7, wt = "w", ind.sup = 30:32))
  expect_true(all(is.finite(res$ind$coord)))
  expect_equal(nrow(res$ind$coord), 28)
  expect_identical(rownames(res$ind.sup$coord), rownames(mtcars)[30:32])
})

# --- missing answers and excl: specific MCA -----------------------------------------------------

excluded <- function(res) names(res$call$Xtot)[res$call$excl]

test_that("a missing answer becomes a level named <VAR>.NA, excluded by default", {
  res <- fit_mca(fx_tea_na(), 1:6)
  expect_true("breakfast.NA" %in% levels(res$call$X$breakfast))
  expect_setequal(excluded(res), c("breakfast.NA", "lunch.NA"))
  # `NA` and "NA" are the same request
  expect_identical(fit_mca(fx_tea_na(), 1:6, excl = "NA")$call$excl, res$call$excl)
})

test_that("excl names one variable's missing values, or none at all", {
  expect_identical(excluded(fit_mca(fx_tea_na(), 1:6, excl = "lunch.NA")), "lunch.NA")
  res <- fit_mca(fx_tea_na(), 1:6, excl = NULL)
  expect_length(res$call$excl, 0L)
  expect_true("breakfast.NA" %in% rownames(res$var$coord))
})

test_that("excl matches level names exactly, never as a regex", {
  # "Not." as a regex matched every "Not.*" level; exactly, it matches none, and says so.
  expect_warning(res <- fit_mca(fx_tea(), 1:6, excl = "Not."), "no active level")
  expect_length(res$call$excl, 0L)

  # a level holding regex metacharacters is found, and only it
  d <- fx_tea()
  levels(d$breakfast)[1] <- "65 ans et + (oui?)"
  expect_identical(excluded(fit_mca(d, 1:6, excl = "65 ans et + (oui?)")), "65 ans et + (oui?)")
  expect_setequal(excluded(fit_mca(d, 1:6, excl = c(NA, "Not.lunch", "evening"))),
                  c("Not.lunch", "evening"))
})

test_that("excl reaches FactoMineR as positions, identical to a hand-built specific MCA", {
  # FactoMineR renames a level shared by two variables `var_lv`: positions are the only safe input.
  d   <- ggfacto:::na_levels(as.data.frame(fx_tea_na()[1:6]), fx_active())
  pos <- which(unlist(lapply(d, levels)) %in% c("breakfast.NA", "lunch.NA"))
  ref <- FactoMineR::MCA(d, excl = pos, graph = FALSE, ncp = Inf)
  expect_equal(fit_mca(fx_tea_na(), 1:6)$eig, ref$eig)
})

test_that("excl actually removes the excluded levels from the drawn cloud", {
  # The user-visible consequence: an excluded level must not appear among the plotted points.
  plot_data <- md(fit_mca(fx_tea(), 1:6, excl = "Not.breakfast", ncp = 2))
  expect_false("Not.breakfast" %in% as.character(plot_data$vars_data$lvs))
  expect_true("Not.lunch" %in% as.character(plot_data$vars_data$lvs))
})

test_that("with no missing answer, the default excl leaves $call$excl empty", {
  expect_length(fx_mca()$call$excl, 0L)
})

# --- the rows of the reference frame: recorded or not ------------------------------------------

fitted <- function(res) which(!is.na(res$source$key))

test_that("the source is one key per row of the reference frame", {
  res <- fx_mca_young()
  d   <- fx_tea()
  expect_named(res$source, c("key", "w", "wt"))
  expect_length(res$source$key, nrow(d))
  expect_identical(fitted(res), which(d$age < 30))
  # the key IS the profile of each row, the same integer vector on both sides
  m <- mca_model(res)
  expect_identical(res$source$key[fitted(res)], m$key)
  expect_identical(fitted(fx_mca()), seq_len(nrow(d)))
})

test_that("every provable pipe shape records the rows it kept", {
  d <- fx_tea()
  young <- which(d$age < 30)
  expect_identical(fitted(d |> dplyr::filter(age < 30) |> fit_mca(1:6)), young)
  expect_identical(fitted(dplyr::filter(d, age < 30) |> fit_mca(1:6)), young)
  expect_identical(fitted(fit_mca(dplyr::filter(d, age < 30), 1:6)), young)
  expect_identical(fitted(fit_mca(d[which(d$age < 30), ], 1:6)), young)
  expect_identical(fitted(fit_mca(d[d$age < 30 & !is.na(d$age), ], 1:6)), young)
  expect_identical(fitted(fit_mca(subset(d, age < 30), 1:6)), young)
  expect_identical(fitted(d |> head(100) |> fit_mca(1:6)), 1:100)
  expect_identical(fitted(d |> dplyr::slice(10:50) |> fit_mca(1:6)), 10:50)
  expect_identical(
    fitted(d |> dplyr::mutate(age2 = age * 2) |> dplyr::filter(age2 < 60) |> fit_mca(1:6)), young)
  expect_identical(
    fitted(d |> dplyr::group_by(sex) |> dplyr::filter(age < 30) |> dplyr::ungroup() |> fit_mca(1:6)),
    young)
  expect_identical(fitted(tibble::as_tibble(d) |> dplyr::filter(age < 30) |> fit_mca(1:6)), young)
  limit <- 30
  expect_identical(fitted(d |> dplyr::filter(age < limit) |> fit_mca(1:6)), young)
  f <- function() {
    local_data <- fx_tea()
    local_data |> dplyr::filter(age < 30) |> fit_mca(1:6)
  }
  expect_identical(fitted(f()), young)
})

test_that("a reordering pipe keeps its order, and the reference frame still aligns", {
  d <- fx_tea()
  ord <- order(d$age, method = "radix")
  arranged <- d |> dplyr::arrange(age) |> dplyr::mutate(x = 1) |> fit_mca(1:6)
  expect_identical(fitted(arranged), seq_len(nrow(d)))
  expect_equal(arranged$eig, fx_mca()$eig)
  # the profiles are numbered in the order the analysis saw them
  expect_identical(arranged$source$key[ord][!duplicated(arranged$source$key[ord])],
                   seq_len(nrow(arranged$ind$coord)))
  # to rounding: FactoMineR was fed the same profiles in another order
  expect_equal(md(arranged, d, sup_vars = "SPC")$vars_data,
               md(fx_mca(), d, sup_vars = "SPC")$vars_data)

  # a PCA keeps the pipe's order in $ind, and its key places each individual in the frame
  cars <- fx_cars()
  pca  <- cars |> dplyr::arrange(mpg) |> PCA2(tidyselect::all_of(fx_pca_vars()), wt = w)
  expect_identical(rownames(pca$ind$coord), rownames(cars)[order(cars$mpg)])
  expect_identical(pca$source$key[order(cars$mpg)], seq_len(nrow(cars)))
  plain <- PCA2(cars, tidyselect::all_of(fx_pca_vars()), wt = w)
  expect_equal(abs(dplyr::mutate(cars, a = axis_coord(pca, 1))$a),
               abs(dplyr::mutate(cars, a = axis_coord(plain, 1))$a))
  local_null_device()
  expect_no_error(quietly(ggfacto(pca, cars, sup_vars = gear)))
})

test_that("tidyr::drop_na() records the complete rows", {
  d <- fx_tea_na()
  expect_identical(fitted(d |> tidyr::drop_na(breakfast) |> fit_mca(1:6)), which(!is.na(d$breakfast)))
})

test_that("a pipe that cannot be proved makes the subset its own reference", {
  d <- fx_tea()
  n_young <- sum(d$age < 30)
  own <- function(res, n) {
    expect_length(res$source$key, n)
    expect_identical(fitted(res), seq_len(n))
  }
  # the id column is lost
  own(d |> dplyr::select(1:6, age) |> dplyr::filter(age < 30) |> fit_mca(1:6), n_young)
  # random: the re-run is another sample
  set.seed(1)
  own(d |> dplyr::slice_sample(n = 200) |> fit_mca(1:6), 200L)
  # %>% hides the expression behind `.`
  `%>%` <- magrittr::`%>%`
  own(d %>% dplyr::filter(age < 30) %>% fit_mca(1:6), n_young)
  # the root is not a named data frame
  own(fit_mca(dplyr::filter(fx_tea(), age < 30), 1:6), n_young)
  # a join that duplicates rows: the ids are not unique
  dup <- dplyr::left_join(d, tibble::tibble(sex = c("F", "F"), k = 1:2), by = "sex",
                          relationship = "many-to-many")
  own(d |> dplyr::left_join(tibble::tibble(sex = c("F", "F"), k = 1:2), by = "sex",
                            relationship = "many-to-many") |> fit_mca(1:6), nrow(dup))

  # a base subset on a condition with NA yields rows of NA: no id can be proved for them
  x <- data.frame(a = c(1, NA, 3, 4), b = letters[1:4])
  e <- rlang::current_env()
  expect_identical(reference_rows(quote(x[x$a > 2, ]), e, x[x$a > 2, ])$n, 3L)

  # the subset aligns, the whole frame is refused with the counts and the way out
  res <- d |> dplyr::select(1:6, age, SPC) |> dplyr::filter(age < 30) |> fit_mca(1:6)
  expect_no_error(md(res, dplyr::filter(d, age < 30), sup_vars = "SPC"))
  expect_error(md(res, d, sup_vars = "SPC"), paste0("fitted on the ", n_young, " rows it was given"))
  expect_error(md(res, d, sup_vars = "SPC"), "filter = ")
})

# --- filter = : the subset declared in the call ------------------------------------------------

test_that("filter = records the same rows, and gives the same analysis, as the pipe", {
  d <- fx_tea()
  res <- fit_mca(d, 1:6, filter = age < 30)
  expect_identical(res$source, fx_mca_young()$source)
  expect_equal(res$eig, fx_mca_young()$eig)
  expect_equal(res$var$coord, fx_mca_young()$var$coord)
  expect_identical(md(res, d, sup_vars = "SPC")$vars_data, md(fx_mca_young(), d, sup_vars = "SPC")$vars_data)
  limit <- 30
  expect_identical(fit_mca(d, 1:6, filter = age < limit)$source, res$source)
})

test_that("the fit records the name of its weight column", {
  d <- fx_tea_wt()
  expect_identical(fit_mca(d, 1:6, wt = w)$source$wt, "w")
  expect_identical(fit_mca(d, 1:6, wt = "w")$source$wt, "w")
  expect_identical(PCA2(fx_cars(), tidyselect::all_of(fx_pca_vars()), wt = w)$source$wt, "w")
  expect_null(fx_mca()$source$wt)
})

test_that("filter = drops the rows where the condition is NA, as dplyr::filter()", {
  d <- fx_tea(); d$age[1:5] <- NA
  expect_identical(fitted(fit_mca(d, 1:6, filter = age < 30)), which(d$age < 30))
})

test_that("filter = combines with the pipe, the weights and excl", {
  d <- fx_tea()
  both <- d |> dplyr::filter(sex == "F") |> fit_mca(1:6, filter = age < 30)
  expect_identical(fitted(both), which(d$sex == "F" & d$age < 30))
  expect_length(both$source$key, nrow(d))

  d$w <- c(0, 0, rep(1, nrow(d) - 2))
  d$w[3] <- NA                                     # a missing weight outside the filter is fine
  d$age[3] <- 90
  res <- suppressMessages(fit_mca(d, 1:6, wt = w, filter = age < 30))
  expect_identical(fitted(res), setdiff(which(d$age < 30), 1:2))
  expect_true(all(is.na(res$source$w[-fitted(res)])))

  na <- fx_tea_na()
  spe <- fit_mca(na, 1:6, filter = age < 30, excl = NA)
  expect_identical(fitted(spe), which(na$age < 30))
  expect_true(length(spe$call$excl) > 0)
})

test_that("filter = is refused in words when it selects nothing or is not a condition", {
  d <- fx_tea()
  expect_error(fit_mca(d, 1:6, filter = age > 1000), "No row")
  expect_error(fit_mca(d, 1:6, filter = age), "condition")
  expect_error(fit_mca(d, 1:6, filter = c(TRUE, FALSE)), "condition")
})

test_that("a PCA takes filter = too, and keeps its supplementary individuals", {
  cars <- fx_cars()
  vars <- fx_pca_vars()
  f <- PCA2(cars, tidyselect::all_of(vars), wt = w, filter = mpg < 25)
  p <- cars |> dplyr::filter(mpg < 25) |> PCA2(tidyselect::all_of(vars), wt = w)
  expect_identical(f$source, p$source)
  expect_equal(f$eig, p$eig)
  expect_equal(f$ind$coord, p$ind$coord)
  expect_identical(fitted(f), which(cars$mpg < 25))

  s <- PCA2(cars, tidyselect::all_of(vars), wt = w, filter = mpg < 25, ind.sup = 1:3)
  expect_identical(fitted(s), sort(union(1:3, which(cars$mpg < 25))))
  expect_identical(rownames(s$ind.sup$coord), rownames(cars)[1:3])
  out <- dplyr::mutate(cars, i = is_in_analysis(s), a = axis_coord(s, 1))
  expect_identical(out$i, cars$mpg < 25 & !seq_len(nrow(cars)) %in% 1:3)
  expect_identical(!is.na(out$a), seq_len(nrow(cars)) %in% fitted(s))
})

# --- taking the data back: the one gate -------------------------------------------------------

test_that("ggmca takes the whole data frame back after an analysis of a subset", {
  d     <- fx_tea()
  whole <- md(fx_mca_young(), d, sup_vars = "SPC")
  sub   <- md(fx_mca_young(), d[d$age < 30, ], sup_vars = "SPC")
  expect_identical(whole$vars_data$`Dim 1`, sub$vars_data$`Dim 1`)
  # new columns do not matter
  expect_no_error(md(fx_mca_young(), dplyr::mutate(d, z = 1), sup_vars = "SPC"))
})

test_that("ggmca refuses data that is not the analysed data, and says why", {
  d <- fx_tea()
  expect_error(md(fx_mca(), d[1:100, ], sup_vars = "SPC"), "fitted on the 300 rows")
  expect_error(md(fx_mca_young(), d[1:100, ], sup_vars = "SPC"),
               paste0("fitted on ", sum(d$age < 30), " of the 300 rows"))
  expect_error(md(fx_mca(), dplyr::arrange(d, age), sup_vars = "SPC"), "reordered")
  expect_error(md(fx_mca_young(), dplyr::arrange(d, age), sup_vars = "SPC"), "reordered")
  expect_error(md(fx_mca(), rbind(d, d[1, ]), sup_vars = "SPC"), "fitted on the 300")
  recoded <- d; recoded$breakfast[which(d$age < 30)[1]] <- setdiff(levels(d$breakfast), d$breakfast[which(d$age < 30)[1]])
  expect_error(md(fx_mca_young(), recoded, sup_vars = "SPC"), "reordered")
  # an edit outside the analysed rows is no concern of the analysis
  outside <- d; outside$breakfast[which(d$age >= 30)[1]] <- NA
  expect_no_error(md(fx_mca_young(), outside, sup_vars = "SPC"))
  expect_error(md(fx_mca(), d[-1], sup_vars = "SPC"), "lacks the active variable")
  expect_error(md(fx_mca(), sup_vars = "SPC"), "pass it second")
})

test_that("the weights are checked too: a swap of two rows with the same answers is refused", {
  d   <- fx_tea_wt()
  res <- fx_mca_wt()
  key <- res$source$key
  pair <- which(key == key[which(duplicated(key) & d$w != d$w[match(key, key)])[1]])[1:2]
  pair <- c(match(key[pair[2]], key), pair[2])
  swapped <- d[replace(seq_len(nrow(d)), pair, rev(pair)), ]
  expect_false(d$w[pair[1]] == d$w[pair[2]])
  expect_error(md(res, swapped, sup_vars = "SPC"), "reordered")
  # without the weight column, the fit's own weights are used
  expect_no_error(md(res, d[setdiff(names(d), "w")], sup_vars = "SPC"))
})

test_that("an analysis made by another engine on a subset aligns on that subset", {
  d <- fx_tea()
  raw <- FactoMineR::MCA(d[d$age < 30, 1:6], graph = FALSE)
  expect_no_error(md(raw, d[d$age < 30, ], sup_vars = "SPC"))
  expect_error(md(raw, d, sup_vars = "SPC"), "fitted on the")
  skip_if_not_installed("GDAtools")
  women <- d$sex == "F"
  cs <- GDAtools::csMCA(d[1:6], subcloud = women)
  out <- dplyr::mutate(d, i = is_in_analysis(cs), a = axis_coord(cs, 1))
  expect_identical(out$i, women)
  expect_identical(!is.na(out$a), women)
})

test_that("what an analysis writes back is NA, or FALSE, outside its rows", {
  d   <- fx_tea()
  res <- fx_mca_young()
  young <- d$age < 30
  out <- dplyr::mutate(d, a = axis_coord(res, 1), i = is_in_analysis(res),
                       k = hierarchical_clust(res, ncp = 2, nb_clust = 3, tree = FALSE))
  expect_identical(out$i, young)
  expect_identical(!is.na(out$a), young)
  expect_identical(!is.na(out$k), young)
  expect_identical(is_in_analysis(res), young)
  expect_identical(length(axis_coord(res, 1)), nrow(d))
  expect_identical(nrow(dplyr::filter(d, is_in_analysis(res))), sum(young))
  # on the analysed rows alone
  expect_true(all(dplyr::mutate(d[young, ], i = is_in_analysis(res))$i))
  expect_error(dplyr::mutate(dplyr::group_by(d, sex), i = is_in_analysis(res)), "ungrouped")
  expect_error(is_in_analysis(fx_ca()), "table")
})

test_that("clust_tab and a PCA's biplot describe an analysed subset given the whole data frame", {
  cars <- fx_cars()
  vars <- fx_pca_vars()
  res  <- PCA2(cars, tidyselect::all_of(vars), wt = w, filter = mpg < 25)
  cars <- dplyr::mutate(cars, k = hierarchical_clust(res, ncp = 2, nb_clust = 2, tree = FALSE))
  expect_identical(!is.na(cars$k), cars$mpg < 25)
  local_null_device()
  expect_no_error(quietly(ggfacto(res, cars, sup_vars = gear, clust = k)))
  expect_s3_class(clust_tab(res, cars, k), "tabxplor_tab")
  expect_error(quietly(ggfacto(res, dplyr::arrange(cars, mpg), sup_vars = gear)), "reordered")
})

test_that("ggmca without data draws the active variables alone", {
  local_null_device()
  expect_s3_class(quietly(ggmca(fx_mca())), "ggplot")
})

# --- axes_names: supplied by the user, not by the analysis --------------------------------------

test_that("a fitted object carries no axes_names of its own", {
  # axes_names is NOT something fit_mca()/PCA2() compute: it is the user's argument to ggmca(), which
  # ggmca_plot() writes onto the stripped model. theme_facto() must therefore read it defensively.
  expect_false("axes_names" %in% names(fx_mca()))
  expect_false("axes_names" %in% names(PCA2(mtcars, 1:7)))
})

test_that("axes_names given to ggmca reaches the axis titles", {
  local_null_device()
  p <- quietly(ggmca(fx_mca(), fx_tea(), axes_names = c("first axis", "second axis")))
  labs <- ggplot2::ggplot_build(p)$plot$labels
  expect_true(any(grepl("first axis",  unlist(labs), fixed = TRUE)))
  expect_true(any(grepl("second axis", unlist(labs), fixed = TRUE)))
})

test_that("no axes_names still yields axis titles carrying the eigenvalue percentages", {
  # theme_facto()'s other job: the axis title always states the axis's share of variance.
  local_null_device()
  labs <- ggplot2::ggplot_build(quietly(ggmca(fx_mca(), fx_tea())))$plot$labels
  expect_true(any(grepl("%", unlist(labs), fixed = TRUE)))
})

test_that("ncp is honoured, and the default keeps every axis", {
  # The default is Inf: `res$eig` is how one chooses how many axes to interpret, and FactoMineR
  # truncates it to `ncp`. Clustering takes its own `ncp`, in hierarchical_clust().
  expect_equal(ncol(fit_mca(fx_tea(), 1:6, ncp = 2)$ind$coord), 2L)
  expect_equal(ncol(fx_mca()$ind$coord), nrow(fx_mca()$eig))
  expect_equal(sum(fx_mca()$eig[, 2]), 100, tolerance = 1e-6)
})

# --- correspondence_analysis: a table, then its analysis --------------------------------------

test_that("correspondence_analysis reads a tab's counts and keeps its variables' names", {
  t   <- tabxplor::tab(forcats::gss_cat, race, marital)
  res <- correspondence_analysis(t)
  expect_identical(names(dimnames(res$call$X)), c("race", "marital"))
  expect_equal(res$eig, FactoMineR::CA(as.matrix(t), graph = FALSE, ncp = Inf)$eig)
  # the counts, whatever the table displays
  pct <- tabxplor::tab(forcats::gss_cat, race, marital, pct = "row")
  expect_equal(correspondence_analysis(pct)$eig, res$eig)
})

test_that("ca_interpret names the two margins a correspondence_analysis kept", {
  withr::local_options(tabxplor.print = "console")
  txt <- utils::capture.output(print(ca_interpret(correspondence_analysis(
    tabxplor::tab(forcats::gss_cat, race, marital))), n = Inf, width = Inf))
  expect_true(any(grepl("race", txt, fixed = TRUE)))
  expect_false(any(grepl("Rows", txt, fixed = TRUE)))
})
