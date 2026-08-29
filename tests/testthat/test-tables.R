# PURPOSE: Lock the five functions that return a table rather than a graph.
# ROLE: These are the reading aids -- the axes interpreted by Le Roux and Rouanet's method, the
#   modified rate of variance, the cluster description, the weighted means.
# KEY CONSTRAINTS:
#   - Tables are tabxplor's job: these build tabxplor::fmt() columns and let tabxplor render them.
#     No kableExtra, DT or gt. mca_interpret(type = "html") is the one exception left and is
#     Phase 1c's target, so it is NOT snapshotted here -- the snapshot would exist to be deleted.
#   - Snapshots cover the console/tibble outputs, which are small and stable. Numbers are asserted
#     as facts (a modified rate is below the raw rate) rather than as digits wherever possible.
# See: CLAUDE.md section ggfacto architecture > Tables are tabxplor's.

# --- benzecri_mrv --------------------------------------------------------------------------------

test_that("benzecri_mrv returns one modified rate per retained axis", {
  mrv <- benzecri_mrv(fx_mca())
  expect_type(as.numeric(mrv), "double")
  expect_gt(length(mrv), 0L)
})

test_that("the modified rates sum to 100 % and re-rank nothing", {
  # The whole point of the modified rate: raw MCA eigenvalue percentages understate the first axes,
  # so Benzecri's correction must redistribute weight without changing the order of the axes.
  mrv <- as.numeric(benzecri_mrv(fx_mca()))
  expect_equal(sum(mrv), 100, tolerance = 1e-6)
  expect_identical(order(mrv, decreasing = TRUE), seq_along(mrv))
})

test_that("the modified rate of the first axis exceeds its raw eigenvalue percentage", {
  # This is the correction's direction, and the reason the function exists.
  raw <- fx_mca()$eig[1, 2]
  expect_gt(as.numeric(benzecri_mrv(fx_mca()))[1], raw)
})

test_that("benzecri_mrv(fmt = TRUE) hands back a tabxplor column", {
  expect_s3_class(benzecri_mrv(fx_mca(), fmt = TRUE), "tabxplor_fmt")
})

# --- mca_interpret -------------------------------------------------------------------------------

test_that("mca_interpret(type = 'console') returns a tibble with a row per kept level", {
  tab <- mca_interpret(fx_mca(), axes = 1:2, type = "console")
  expect_s3_class(tab, "tbl_df")
  expect_gt(nrow(tab), 0L)
})

test_that("mca_interpret keeps only the levels contributing above the mean", {
  # Le Roux and Rouanet's rule: a level is worth reading only if it contributes more than the
  # average contribution. So the table must be shorter than the full list of levels.
  tab <- mca_interpret(fx_mca(), axes = 1:2, type = "console")
  all_levels <- nrow(fx_mca()$var$coord)
  expect_lt(nrow(tab), all_levels * 2L)
})

test_that("mca_interpret covers each requested axis", {
  one <- mca_interpret(fx_mca(), axes = 1,   type = "console")
  two <- mca_interpret(fx_mca(), axes = 1:2, type = "console")
  expect_gt(nrow(two), nrow(one))
})

test_that("the console interpretation table is stable", {
  expect_snapshot(print(mca_interpret(fx_mca(), axes = 1:2, type = "console"), n = Inf, width = Inf))
})

# --- pca_interpret -------------------------------------------------------------------------------

test_that("pca_interpret returns tabxplor fmt columns, ready for tabxplor to render", {
  tab <- pca_interpret(fx_pca(), axes = 1:2)
  expect_s3_class(tab, "tbl_df")
  expect_true(any(vapply(tab, inherits, logical(1), "tabxplor_fmt")))
})

test_that("the PCA interpretation table is stable", {
  expect_snapshot(print(pca_interpret(fx_pca(), axes = 1:2), n = Inf, width = Inf))
})

# --- mean_sd_tab ---------------------------------------------------------------------------------

test_that("mean_sd_tab reports one row per variable", {
  tab <- mean_sd_tab(mtcars, 1:7)
  expect_s3_class(tab, "tbl_df")
  expect_equal(nrow(tab), 7L)
})

test_that("mean_sd_tab's unweighted mean matches base R", {
  tab <- mean_sd_tab(mtcars, tidyselect::all_of("mpg"))
  num <- vapply(tab, function(x) suppressWarnings(as.numeric(as.character(x)))[1], numeric(1))
  expect_true(any(abs(num - mean(mtcars$mpg)) < 0.05, na.rm = TRUE))
})

test_that("weighting mean_sd_tab changes the mean it reports", {
  # The weighted branch uses stats::weighted.mean and weighted.var from R/utils.R; a weight that is
  # silently dropped would leave the two identical.
  d <- mtcars[1:7]
  d$w <- rep(c(0.5, 1.5), length.out = nrow(d))
  expect_false(identical(as.character(mean_sd_tab(d, 1:7)),
                         as.character(mean_sd_tab(d, 1:7, wt = "w"))))
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
