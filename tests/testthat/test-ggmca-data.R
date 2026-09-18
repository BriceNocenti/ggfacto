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
#       * tooltip_vars / tooltip_vars_1lv only act when a tooltip is built at all, i.e. alongside
#         sup_vars or active_tables -- on their own the result is byte-identical to a plain call;
#       * clust only colours anything alongside profiles = TRUE.
#     Every case below therefore supplies the enabling argument too, and the vacuous paths are
#     pinned explicitly so a future reader does not "simplify" them back into nothing.
# See: CLAUDE.md section ggfacto architecture > The plot model.

# --- the plot model -----------------------------------------------------------------------------

test_that("ggmca_data returns the four-element plot model", {
  plot_data <- md(fx_mca(), fx_tea())
  expect_type(plot_data, "list")
  expect_named(plot_data, c("vars_data", "ind_data", "res.mca", "clust"))
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
  expect_true(all(c("vars", "lvs", "color_group", "id", "contribs", "interactive_text")
                  %in% names(vars_data)))
  expect_true(all(paste("Dim", 1:5) %in% names(vars_data)))
  # One row per level of the six binary active variables, plus the central point.
  expect_gte(nrow(vars_data), 12L)
})

test_that("lvs stays a FACTOR, which the tooltip join depends on", {
  # interactive_tooltips() finds its crosstab columns by is.character(); if lvs were character it
  # would be swept into them and the join would break.
  expect_s3_class(fx_pd_plain()$vars_data$lvs, "factor")
})

test_that("ind_data is NULL unless profiles are asked for", {
  expect_null(fx_pd_plain()$ind_data)
  expect_s3_class(fx_pd_profiles()$ind_data, "data.frame")
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
})

# --- active_tables: the crosstabs that are the package's whole point ----------------------------

test_that("active_tables widens the nested tooltip", {
  # This is the bet: the Burt table travels inside the hover of each point.
  plain  <- md(fx_mca(), fx_tea())
  active <- fx_pd_active()

  expect_equal(ncol(plain$vars_data$interactive_text[[1]]), 1L)
  expect_gt(ncol(active$vars_data$interactive_text[[1]]), 1L)
})

test_that("wcount is present on every path, and agrees with the crosstabs where both exist", {
  # wcount used to appear only when a tooltip table had been built, which left type = "points"
  # -- it sizes points by wcount -- failing on a plain call. It is now derived from the column
  # margin whenever the crosstabs do not supply it, and the two must agree exactly.
  plain  <- md(fx_mca(), fx_tea())
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
  expect_gt(ncol(sup$vars_data$interactive_text[[1]]), 1L)
  expect_true("wcount" %in% names(sup$vars_data))
})

# --- tooltip_vars / tooltip_vars_1lv: inert on their own ----------------------------------------

test_that("tooltip_vars only acts when a tooltip is built at all", {
  # WARNING: this is the vacuous path. Without sup_vars/active_tables no tooltip is built, so
  # tooltip_vars changes precisely nothing -- a test that omitted active_tables would pass while
  # exercising none of the code it claims to cover.
  plain <- md(fx_mca(), fx_tea())
  alone <- md(fx_mca(), fx_tea(), tooltip_vars = "SPC")
  expect_identical(plain, alone)

  with_tables <- fx_pd_active()
  widened     <- md(fx_mca(), fx_tea(), active_tables = "active", tooltip_vars = "SPC")
  expect_gt(ncol(widened$vars_data$interactive_text[[1]]),
            ncol(with_tables$vars_data$interactive_text[[1]]))
})

test_that("tooltip_vars_1lv adds one condensed line per variable", {
  with_tables <- fx_pd_active()
  condensed   <- md(fx_mca(), fx_tea(), active_tables = "active", tooltip_vars_1lv = "SPC")

  expect_gt(ncol(condensed$vars_data$interactive_text[[1]]),
            ncol(with_tables$vars_data$interactive_text[[1]]))
  # It is placed before the active-variable block, right after the header.
  expect_equal(names(condensed$vars_data$interactive_text[[1]])[1], "begin_text")
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

test_that("clust adds a clust_id offset into its own band, so hover links a whole cluster", {
  # Ids are banded on purpose: active variables from 1000, clusters and profiles from 10000, so
  # every point of one cluster shares an id and hovering any of them lights them all.
  plot_data <- fx_pd_clust()
  expect_true("clust_id" %in% names(plot_data$vars_data))
  ids <- plot_data$vars_data$clust_id[!is.na(plot_data$vars_data$clust_id)]
  expect_true(length(ids) > 0)
  expect_true(all(ids >= 10000L))
  expect_identical(plot_data$clust, "clust")
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
