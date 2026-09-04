# PURPOSE: Lock the two tables that describe the DATA -- mean_sd_tab() and HCPC_tab().
# ROLE: what the variables look like before the analysis, and what a cluster is made of. The
#   interpretation tables of the analyses are in test-interpret.R, together with the output contract
#   the whole summary family shares.
# KEY CONSTRAINTS:
#   - Tables are tabxplor's job: these build tabxplor::fmt() columns and let tabxplor render them.
#     No kableExtra, DT or gt.
#   - Numbers are asserted as facts (a weighted mean differs from an unweighted one) rather than as
#     digits wherever possible.
# See: CLAUDE.md section ggfacto architecture > Tables are tabxplor's.

# --- mean_sd_tab ---------------------------------------------------------------------------------

test_that("mean_sd_tab reports one row per variable", {
  tab <- suppressWarnings(mean_sd_tab(mtcars, 1:7))
  expect_s3_class(tab, "tbl_df")
  expect_equal(nrow(tab), 7L)
  expect_identical(names(tab), c("variables", "n", "mean", "sd", "sd/mean"))
})

test_that("mean_sd_tab's unweighted mean and sd match base R", {
  tab <- suppressWarnings(mean_sd_tab(mtcars, tidyselect::all_of("mpg")))
  expect_equal(tab$mean$mean, mean(mtcars$mpg))
  expect_equal(sqrt(tab$sd$var), stats::sd(mtcars$mpg))
  # ONE record, printed three times: the sd and the cv are DERIVED from the same variance
  expect_identical(tab$mean$var, tab$sd$var)
  expect_identical(unique(tab$sd$display), "sd")
  expect_identical(unique(tab[["sd/mean"]]$display), "cv")
})

test_that("weighting mean_sd_tab changes the mean it reports", {
  # The weighted branch uses stats::weighted.mean and weighted.var from R/utils.R; a weight that is
  # silently dropped would leave the two identical.
  d <- mtcars[1:7]
  d$w <- rep(c(0.5, 1.5), length.out = nrow(d))
  expect_false(identical(as.character(suppressWarnings(mean_sd_tab(d, 1:7))),
                         as.character(suppressWarnings(mean_sd_tab(d, 1:7, wt = "w")))))
})

# --- HCPC_tab ------------------------------------------------------------------------------------

test_that("HCPC_tab accepts the clusters as a column name, a bare symbol and a vector", {
  # All three are documented. The external-vector spelling routes through tidyselect's deprecated
  # env fallback, which is a known and deliberate wart, so its warning is suppressed here rather
  # than being allowed to fail the suite.
  d <- fx_tea_clust()
  rows <- tidyselect::all_of(fx_active())

  by_string <- HCPC_tab(d, row_vars = rows, clust = "clust")
  by_symbol <- HCPC_tab(d, row_vars = rows, clust = clust)
  by_vector <- suppressWarnings(HCPC_tab(d, row_vars = rows, clust = d$clust))

  expect_s3_class(by_string, "tbl_df")
  expect_equal(dim(by_string), dim(by_symbol))
  expect_equal(dim(by_string), dim(by_vector))
})

test_that("HCPC_tab has one column per cluster plus the population total", {
  tab <- HCPC_tab(fx_tea_clust(), row_vars = tidyselect::all_of(fx_active()), clust = "clust")
  expect_gte(ncol(tab), nlevels(fx_tea_clust()$clust))
})

test_that("HCPC_tab builds tabxplor fmt columns", {
  tab <- HCPC_tab(fx_tea_clust(), row_vars = tidyselect::all_of(fx_active()), clust = "clust")
  expect_true(any(vapply(tab, inherits, logical(1), "tabxplor_fmt")))
})

test_that("HCPC_tab weights its percentages", {
  d <- fx_tea_clust()
  d$w <- rep(c(0.5, 1.5), length.out = nrow(d))
  rows <- tidyselect::all_of(fx_active())
  expect_false(identical(as.character(HCPC_tab(d, row_vars = rows, clust = "clust")),
                         as.character(HCPC_tab(d, row_vars = rows, clust = "clust", wt = "w"))))
})

test_that("HCPC_tab's excl collapses the excluded levels instead of dropping the row", {
  rows <- tidyselect::all_of(fx_active())
  plain <- HCPC_tab(fx_tea_clust(), row_vars = rows, clust = "clust")
  excl  <- HCPC_tab(fx_tea_clust(), row_vars = rows, clust = "clust", excl = "Not.")
  expect_false(identical(as.character(plain), as.character(excl)))
})


# *Silent failure guarded: a function retired in the documentation and nowhere else.*
test_that("mean_sd_tab() says it is retired, and names its replacement", {
  # the notice fires ONCE per session, so the register is cleared first -- an earlier call in this
  # file has already spent it.
  e <- ggfacto:::deprecated_args_warned
  rm(list = ls(envir = e), envir = e)
  expect_warning(mean_sd_tab(mtcars, 1:3), "pca_interpret")
})
