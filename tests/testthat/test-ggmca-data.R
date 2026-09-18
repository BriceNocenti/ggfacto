# PURPOSE: Lock the plot model ggmca_data() returns, and cover every argument that opens a code
#   path a plain ggmca_data(res.mca, data) call never reaches.
# ROLE: The package's central contract. ggmca() is pure orchestration of ggmca_data() and
#   ggmca_plot(), and the seam between them is public: a user edits plot_data$vars_data by hand.
# KEY CONSTRAINTS:
#   - ARGUMENT coverage, not function coverage. Phase 1a's migration left 25 stranded `.`
#     placeholders that a default call could not reach, and they failed quietly rather than loudly
#     because of the package-level `. = NULL` binding.
#   - WARNING -- several arguments are NESTED and test vacuously on their own:
#       * keep_levels / discard_levels only act inside `if (length(sup_vars) != 0)`;
#       * tooltip_vars / tooltip_vars_1lv only act when a tooltip is built at all -- by default
#         (active_tables = "active"), or alongside sup_vars -- so with active_tables = NULL alone
#         the result is byte-identical to a plain call;
#       * clust draws the profiles by default, and colours nothing under profiles = FALSE.
#     Every case below therefore supplies the enabling argument too, and the vacuous paths are
#     pinned explicitly so a future reader does not "simplify" them back into nothing.
# See: CLAUDE.md section ggfacto architecture > The plot model.

# --- the plot model -----------------------------------------------------------------------------

test_that("ggmca_data returns the five-element plot model", {
  plot_data <- md(fx_mca(), fx_tea())
  expect_type(plot_data, "list")
  expect_named(plot_data, c("vars_data", "ind_data", "individuals", "res.mca", "clust"))
})

test_that("the plot model is flat: no list-column in any of its tables", {
  # A user edits these tables between the halves; a nested column is what made them unreadable
  # and what every per-profile loop used to walk.
  pd <- md(fx_mca(), fx_tea_clust(), sup_vars = "SPC", clust = "clust", profiles = TRUE,
           active_tables = "active")
  for (tbl in pd[c("vars_data", "ind_data", "individuals")]) {
    expect_false(any(vapply(tbl, is.list, logical(1))))
  }
})

test_that("the model carries a STRIPPED res.mca, not the FactoMineR object", {
  # The rendering half must not be able to recompute anything: it gets eigenvalues and axis names.
  res <- fx_pd_plain()$res.mca
  expect_named(res, c("eig", "axes_names"))
  expect_false("call" %in% names(res))
  expect_false("var"  %in% names(res))
})

test_that("vars_data has one row per level, with coordinates on every extracted axis", {
  vars_data <- fx_pd_plain()$vars_data
  expect_true(all(c("vars", "lvs", "color_group", "id", "begin_text", "interactive_text")
                  %in% names(vars_data)))
  expect_true(all(paste("Dim", 1:5) %in% names(vars_data)))
  expect_true(all(paste0("contrib", 1:5) %in% names(vars_data)))
  # One row per level of the six binary active variables, plus the central point.
  expect_gte(nrow(vars_data), 12L)
})

test_that("the levels FactoMineR renames keep their data's names", {
  # A level named y/n becomes `var.y` in FactoMineR's results, a level two questions share
  # `var_lv`: both are read by position, never matched by name.
  d <- fx_tea()[1:6]
  d$always   <- factor(ifelse(d$always == "always", "y", "n"))
  d$breakfast <- factor(ifelse(d$breakfast == "breakfast", "yes", "no"))
  d$lunch     <- factor(ifelse(d$lunch == "lunch", "yes", "no"))
  res <- MCA2(d, 1:6)
  expect_true(all(c("always.n", "breakfast_no") %in% rownames(res$var$coord)))
  vd <- md(res, d)$vars_data
  expect_setequal(as.character(vd$lvs[vd$vars == "always"]), c("n", "y"))
  expect_setequal(as.character(vd$lvs[vd$vars == "lunch"]), c("no", "yes"))
  expect_false(anyNA(vd$`Dim 1`))
  interp <- mca_interpret_data(res, 1)
  expect_false(anyNA(interp$group))
  expect_setequal(interp$level[interp$group == "always"], c("n", "y"))
})

test_that("the plot model is the same whether FactoMineR saw the profiles or the individuals", {
  d    <- fx_tea_wt()
  prof <- md(fx_mca_wt(), d, sup_vars = SPC, profiles = TRUE)
  ind  <- md(mca_ind(d, 1:6, wt = "w"), d, sup_vars = SPC, profiles = TRUE)
  expect_equal(prof$vars_data, ind$vars_data, tolerance = 1e-10)
  expect_equal(prof$ind_data, ind$ind_data, tolerance = 1e-10)
  expect_equal(prof$individuals, ind$individuals, tolerance = 1e-10)
})

test_that("a GDAtools speMCA() is drawn like the equivalent specific MCA", {
  skip_if_not_installed("GDAtools")
  spe <- md(GDAtools::speMCA(fx_tea()[1:6], excl = 3), fx_tea(), sup_vars = SPC)$vars_data
  gg  <- md(MCA2(fx_tea(), 1:6, excl = "Not.tea time"), fx_tea(), sup_vars = SPC)$vars_data
  expect_identical(spe[c("vars", "lvs", "wcount", "begin_text")],
                   gg[c("vars", "lvs", "wcount", "begin_text")])
  expect_equal(abs(spe$`Dim 1`), abs(gg$`Dim 1`), tolerance = 1e-10)
})

test_that("lvs stays a FACTOR", {
  # It is the label a user renames or reorders between the halves, the forcats way.
  expect_s3_class(fx_pd_plain()$vars_data$lvs, "factor")
})

test_that("ind_data is NULL unless profiles are asked for", {
  expect_null(fx_pd_plain()$ind_data)
  expect_s3_class(fx_pd_profiles()$ind_data, "data.frame")
})

test_that("clust draws the answer profiles by default, and profiles = FALSE still wins", {
  pd <- md(fx_mca(), fx_tea_clust(), clust = "clust")
  expect_s3_class(pd$ind_data, "data.frame")
  expect_false(anyNA(pd$ind_data$clust))
  expect_null(md(fx_mca(), fx_tea_clust(), clust = "clust", profiles = FALSE)$ind_data)
})

test_that("clust is character() when unused", {
  expect_identical(fx_pd_plain()$clust, character())
})

# --- sup_vars -----------------------------------------------------------------------------------

test_that("sup_vars adds supplementary rows in their own colour group", {
  plain <- md(fx_mca(), fx_tea())
  sup   <- md(fx_mca(), fx_tea(), sup_vars = c("SPC", "sex"))

  expect_gt(nrow(sup$vars_data), nrow(plain$vars_data))
  expect_true(any(!sup$vars_data$color_group %in% c("active_vars", "Central point")))
  expect_true(all(c("SPC", "sex") %in% as.character(sup$vars_data$vars)))
})

test_that("sup_vars are read from `data`, so a column absent there is an error", {
  expect_error(md(fx_mca(), fx_tea(), sup_vars = "not_a_column"))
  expect_error(md(fx_mca(), fx_tea(), sup_vars = not_a_column))
})

test_that("sup_vars takes bare names, as in tab(), strings, or a vector of names", {
  bare <- md(fx_mca(), fx_tea(), sup_vars = c(SPC, sex))$vars_data
  expect_true(all(c("SPC", "sex") %in% bare$vars))
  expect_identical(md(fx_mca(), fx_tea(), sup_vars = c("SPC", "sex"))$vars_data, bare)
  sup <- c("SPC", "sex")
  expect_identical(md(fx_mca(), fx_tea(), sup_vars = sup)$vars_data, bare)
  expect_identical(md(fx_mca(), fx_tea(), sup_vars = tidyselect::all_of(sup))$vars_data, bare)
  expect_identical(suppressMessages(ggmca(fx_mca(), fx_tea(), sup_vars = c(SPC, sex),
                                         get_data = TRUE))$vars_data$vars,
                   suppressMessages(ggmca(fx_mca(), fx_tea(), sup_vars = sup,
                                          get_data = TRUE))$vars_data$vars)
})

test_that("a supplementary level sits at the barycentre of its individuals, over sqrt(eigenvalue)", {
  d  <- fx_tea_wt()
  vd <- md(fx_mca_wt(), d, sup_vars = SPC)$vars_data
  x  <- axis_coord(fx_mca_wt(), 1)
  bary <- tapply(d$w * x, d$SPC, sum) / tapply(d$w, d$SPC, sum) / fx_mca_wt()$svd$vs[1]
  got  <- vd$`Dim 1`[vd$vars == "SPC"]
  names(got) <- as.character(vd$lvs[vd$vars == "SPC"])
  expect_equal(got[names(bary)], c(bary), tolerance = 1e-10, ignore_attr = TRUE)
})

# --- active_tables: the crosstabs that are the package's whole point ----------------------------

test_that("the crosstabs are in the tooltip body by default, and active_tables = NULL removes them", {
  # This is the bet: the Burt table travels inside the hover of each point, unasked.
  plain  <- md(fx_mca(), fx_tea(), active_tables = NULL)
  active <- fx_pd_active()

  expect_true(all(is.na(plain$vars_data$interactive_text)))
  expect_match(active$vars_data$interactive_text[1], "Active variables")
  expect_identical(md(fx_mca())$vars_data, active$vars_data)
})

test_that("wcount is present on every path, and agrees with the crosstabs where both exist", {
  # wcount used to appear only when a tooltip table had been built, which left type = "points"
  # -- it sizes points by wcount -- failing on a plain call. It is now derived from the column
  # margin whenever the crosstabs do not supply it, and the two must agree exactly.
  plain  <- md(fx_mca(), fx_tea(), active_tables = NULL)
  active <- fx_pd_active()

  expect_true("wcount" %in% names(plain$vars_data))
  expect_true("wcount" %in% names(active$vars_data))

  key <- function(m) stats::setNames(m$vars_data$wcount,
                                     paste(m$vars_data$vars, m$vars_data$lvs))
  common <- intersect(names(key(plain)), names(key(active)))
  expect_gt(length(common), 0L)
  expect_equal(key(plain)[common], key(active)[common])
})

test_that("wcount sums to the population within each variable", {
  vd <- fx_pd_plain()$vars_data
  vd <- vd[vd$color_group == "active_vars", ]
  per_var <- tapply(vd$wcount, as.character(vd$vars), sum)
  expect_true(all(abs(per_var - nrow(fx_tea())) < 1e-6))
})

test_that("active_tables = 'sup' crosses against the supplementary variables instead", {
  sup <- md(fx_mca(), fx_tea(), sup_vars = "SPC", active_tables = "sup")
  spc <- sup$vars_data$interactive_text[sup$vars_data$vars == "SPC"]
  expect_true(all(grepl("<font color", spc, fixed = TRUE)))
  expect_true("wcount" %in% names(sup$vars_data))
})

# --- tooltip_vars / tooltip_vars_1lv: inert on their own ----------------------------------------

test_that("tooltip_vars only acts when a tooltip is built at all", {
  # WARNING: this is the vacuous path. Without sup_vars/active_tables no tooltip is built, so
  # tooltip_vars changes precisely nothing -- a test that omitted active_tables would pass while
  # exercising none of the code it claims to cover.
  plain <- md(fx_mca(), fx_tea(), active_tables = NULL)
  alone <- md(fx_mca(), fx_tea(), active_tables = NULL, tooltip_vars = "SPC")
  expect_identical(plain, alone)

  expect_false(grepl("Distribution by", fx_pd_active()$vars_data$interactive_text[1]))
  widened <- md(fx_mca(), fx_tea(), active_tables = "active", tooltip_vars = "SPC")
  expect_match(widened$vars_data$interactive_text[1], "Distribution by")
})

test_that("tooltip_vars_1lv adds one condensed line per variable", {
  condensed <- md(fx_mca(), fx_tea(), active_tables = "active", tooltip_vars_1lv = "SPC")
  first_lv  <- levels(fx_tea()$SPC)[1]
  body      <- condensed$vars_data$interactive_text[1]

  expect_false(grepl(paste0(first_lv, ":"), fx_pd_active()$vars_data$interactive_text[1]))
  # It is placed before the active-variable block, right after the header.
  expect_lt(regexpr(paste0(first_lv, ":"), body), regexpr("Active variables", body))
})

# --- keep_levels / discard_levels: nested inside sup_vars ---------------------------------------

test_that("discard_levels drops the matching supplementary levels", {
  sup     <- fx_pd_sup()
  dropped <- md(fx_mca(), fx_tea(), sup_vars = "SPC", discard_levels = "employee")

  expect_lt(nrow(dropped$vars_data), nrow(sup$vars_data))
  expect_false(any(grepl("employee", as.character(dropped$vars_data$lvs))))
  expect_true(any(grepl("employee", as.character(sup$vars_data$lvs))))
})

test_that("keep_levels keeps only the matching supplementary levels", {
  sup  <- fx_pd_sup()
  kept <- md(fx_mca(), fx_tea(), sup_vars = "SPC", keep_levels = "employee")

  expect_lt(nrow(kept$vars_data), nrow(sup$vars_data))
  spc_levels <- kept$vars_data$lvs[as.character(kept$vars_data$vars) == "SPC"]
  expect_true(all(grepl("employee", as.character(spc_levels))))
})

test_that("keep_levels without sup_vars does nothing at all", {
  # WARNING: the second vacuous path -- both level filters live inside the sup_vars block.
  expect_identical(md(fx_mca(), fx_tea()),
                   md(fx_mca(), fx_tea(), keep_levels = "employee"))
})

# --- profiles -----------------------------------------------------------------------------------

test_that("profiles builds one row per answer profile, with count and wcount", {
  ind <- fx_pd_profiles()$ind_data
  expect_true(all(c("count", "wcount") %in% names(ind)))
  # A profile is a unique combination of active answers, so there are fewer than individuals.
  expect_lt(nrow(ind), nrow(fx_tea()))
  expect_equal(sum(ind$count), nrow(fx_tea()))
})

test_that("max_profiles caps the profile cloud, keeping the largest", {
  capped <- md(fx_mca(), fx_tea(), profiles = TRUE, max_profiles = 10)$ind_data
  full   <- fx_pd_profiles()$ind_data

  expect_lte(nrow(capped), 10L)
  expect_lt(nrow(capped), nrow(full))
  # Truncation is by weighted count, so the kept profiles are the heaviest ones.
  expect_equal(sort(capped$wcount, decreasing = TRUE),
               sort(full$wcount, decreasing = TRUE)[seq_len(nrow(capped))])
})

# --- clust ----------------------------------------------------------------------------------------

test_that("a cluster's label and its profiles share one hover id, whatever the level order", {
  # Ids are banded on purpose: active variables from 1000, clusters and profiles from 10000, so
  # every point of one cluster shares an id and hovering any of them lights them all. They are
  # matched by NAME: cleannames re-sorts a factor alphabetically, and ids taken from its codes
  # linked a label to another cluster's profiles. Non-alphabetical names are the case that broke.
  d <- fx_tea_clust()
  d$clust <- factor(c("Zeta", "Alpha", "Mu", "Beta")[as.integer(d$clust)],
                    levels = c("Zeta", "Alpha", "Mu", "Beta"))
  plot_data <- md(fx_mca(), d, clust = "clust", profiles = TRUE)
  labels <- plot_data$vars_data[plot_data$vars_data$vars == "clust", ]
  expect_true(all(labels$id >= 10000L))
  for (i in seq_len(nrow(labels))) {
    profile_ids <- plot_data$ind_data$id[as.character(plot_data$ind_data$clust) ==
                                           as.character(labels$lvs[i])]
    expect_true(length(profile_ids) > 0)
    expect_true(all(profile_ids == labels$id[i]))
  }
  expect_identical(plot_data$clust, "clust")
})

test_that("a missing cluster moves no profile, and every profile keeps a colour", {
  # A profile takes the weighted plurality of its individuals' clusters, missing ones left out:
  # the clusters made on the analysis are pure within a profile, so blanking a few individuals
  # must change nothing.
  local_null_device()
  d <- fx_tea_clust()
  d$clust[c(2, 30, 31, 150, 299)] <- NA
  full    <- fx_pd_clust()$ind_data
  blanked <- md(fx_mca(), d, clust = "clust", profiles = TRUE)$ind_data
  expect_identical(as.character(blanked$clust), as.character(full$clust))

  built <- ggplot2::ggplot_build(quietly(ggmca_plot(md(fx_mca(), d, clust = "clust",
                                                       profiles = TRUE))))
  profile_layer <- built$data[[which(vapply(built$data, nrow, 1L) == nrow(blanked))[1]]]
  expect_false(anyNA(profile_layer$colour))
})

test_that("a profile split between clusters takes the one weighing most", {
  # Clusters made elsewhere need not follow the profiles: the heavier side wins, weights counted.
  d <- fx_tea_wt()
  key  <- interaction(d[fx_active()], drop = TRUE)
  rows <- which(key == names(which(table(key) >= 3))[1])
  d$split <- factor(ifelse(seq_len(nrow(d)) %in% rows[-1], "B", "A"), levels = c("A", "B"))
  heavier <- names(which.max(tapply(d$w[rows], d$split[rows], sum)))

  ind  <- md(fx_mca_wt(), d, clust = "split", profiles = TRUE)$ind_data
  coord <- axis_coord(fx_mca_wt(), 1:2)[rows[1], ]
  here <- abs(ind$`Dim 1` - coord$axis1) < 1e-9 & abs(ind$`Dim 2` - coord$axis2) < 1e-9
  expect_equal(sum(here), 1L)
  expect_equal(as.character(ind$clust[here]), heavier)
})

test_that("individuals holds one row per fitted individual, with the rank of its profile", {
  plot_data <- md(fx_mca(), fx_tea(), sup_vars = "SPC", max_profiles = 5)
  ind <- plot_data$individuals
  expect_null(plot_data$ind_data)
  expect_equal(nrow(ind), nrow(fx_tea()))
  expect_true(all(c("nb", "row.w", "Dim 1", "SPC") %in% names(ind)))
  # nb is missing exactly for the individuals whose profile max_profiles left out.
  expect_setequal(stats::na.omit(unique(ind$nb)), 1:5)
  expect_true(anyNA(ind$nb))
  expect_null(fx_pd_plain()$individuals)
})

test_that("an excluded answer is not listed in a profile's tooltip", {
  # excl = NA (the default) excludes the `<VAR>.NA` levels: a missing answer has no line of its own.
  res <- MCA2(fx_tea_na(), 1:6)
  txt <- md(res, fx_tea_na(), profiles = TRUE)$ind_data$interactive_text
  expect_false(any(grepl("Remove_levels", txt, fixed = TRUE)))
  expect_false(any(grepl("\\.NA", txt)))
})

test_that("clust is added to the supplementary variables automatically", {
  plot_data <- md(fx_mca(), fx_tea_clust(), clust = "clust")
  expect_true("clust" %in% as.character(plot_data$vars_data$vars))
})

test_that("clust takes a bare name, a string or the clusters, and refuses anything else", {
  d <- fx_tea_clust()
  by_name <- md(fx_mca(), d, clust = clust)$vars_data
  expect_identical(md(fx_mca(), d, clust = "clust")$vars_data$`Dim 1`, by_name$`Dim 1`)
  expect_identical(md(fx_mca(), d, clust = d$clust)$vars_data$`Dim 1`, by_name$`Dim 1`)
  expect_error(md(fx_mca(), d, clust = c("clust", "SPC")), "column of `data` that holds")
})

# --- cleannames ---------------------------------------------------------------------------------

test_that("cleannames strips prefix numbers and parenthesised text from level names", {
  # tea's own levels are already clean, so the rule needs a level that actually carries the two
  # things cleannames_condition() removes: a leading "1-" and a parenthesised aside.
  dirty <- fx_tea()
  dirty$dirty_var <- factor(rep(c("1-Yes (a lot)", "2-No (never)"), length.out = nrow(dirty)))

  clean <- md(fx_mca(), dirty, sup_vars = "dirty_var", cleannames = TRUE)$vars_data
  raw   <- md(fx_mca(), dirty, sup_vars = "dirty_var", cleannames = FALSE)$vars_data

  expect_true(any(grepl("^1-Yes \\(a lot\\)$", as.character(raw$lvs))))
  expect_true(any(as.character(clean$lvs) == "Yes"))
  expect_false(any(grepl("\\(a lot\\)", as.character(clean$lvs))))
})

# --- weights ride one channel -------------------------------------------------------------------

test_that("weights are read from the fitted object, not from the data passed in", {
  # A tooltip must describe the population the analysis was fitted on, even when the user hands a
  # different `data`. So a weighted fit yields weighted counts against unweighted microdata.
  ind <- md(fx_mca_wt(), fx_tea_wt(), profiles = TRUE)$ind_data
  expect_true(all(c("count", "wcount") %in% names(ind)))
  expect_false(isTRUE(all.equal(ind$count, ind$wcount)))
  expect_equal(sum(ind$count), nrow(fx_tea_wt()))
})

test_that("an unweighted fit has count equal to wcount", {
  ind <- fx_pd_profiles()$ind_data
  expect_equal(ind$count, ind$wcount)
})

# --- the public seam between the two halves -----------------------------------------------------

test_that("a user can edit the plot model between the halves", {
  # This is documented behaviour: call ggmca_data(), drop or rename a level in vars_data, and hand
  # the result to ggmca_plot().
  local_null_device()
  plot_data <- fx_pd_sup()
  plot_data$vars_data <- plot_data$vars_data[as.character(plot_data$vars_data$vars) != "SPC", ]

  p <- quietly(ggmca_plot(plot_data))
  expect_s3_class(p, "ggplot")
  drawn <- unlist(lapply(ggplot2::ggplot_build(p)$data, function(d) as.character(d$label)))
  expect_false(any(grepl("employee", drawn)))
})

test_that("ggmca is exactly ggmca_data piped into ggmca_plot", {
  # ggmca()'s body is pure orchestration; if it ever grows logic of its own, this fails.
  local_null_device()
  direct   <- quietly(ggmca(fx_mca(), fx_tea(), sup_vars = "SPC"))
  in_halves <- quietly(ggmca_plot(fx_pd_sup()))
  expect_equal(lapply(ggplot2::ggplot_build(direct)$data, dim),
               lapply(ggplot2::ggplot_build(in_halves)$data, dim))
})
