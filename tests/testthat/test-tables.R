# PURPOSE: Lock the five functions that return a table rather than a graph.
# ROLE: These are the reading aids -- the axes interpreted by Le Roux and Rouanet's method, the
#   modified rate of variance, the cluster description, the weighted means.
# KEY CONSTRAINTS:
#   - Tables are tabxplor's job: these build tabxplor::fmt() columns and let tabxplor render them.
#     No kableExtra, DT or gt.
#   - Snapshots cover the tibble outputs, which are small and stable -- never the rendered html,
#     which is 7 kB of inlined stylesheet. Numbers are asserted as facts (a modified rate is below
#     the raw rate) rather than as digits wherever possible.
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

# --- mca_interpret, the tabxplor table -----------------------------------------------------------

itab <- function(res, spread = FALSE, axes = 1:2) {
  mca_interpret_tab(mca_interpret(res, axes = axes, type = "console"),
                    mean_ctr = mean(res$var$contrib[, axes]),
                    mrv      = benzecri_mrv(res),
                    n_ind    = nrow(res$call$X),
                    spread   = spread)
}
fx_itab <- function(spread = FALSE) itab(fx_mca(), spread)

# tea[1:6] is a battery of binary items, so every question packs into exactly one row. The blanking
# of a repeated question figure only has anything to hide once a question keeps several levels on
# one side of the axis, which needs variables with more than two levels and a third axis.
fx_mca_multi <- function() fx("mca_multi", function() {
  MCA2(fx_tea(), tidyselect::all_of(c("Tea", "How", "how", "where", "price")), ncp = 5)
})

test_that("mca_interpret_tab builds fmt cells under declared tabxplor index columns", {
  # Both halves are needed: tabxplor styles nothing without fmt columns, and cannot find the row
  # variable without a declared tabxplor_lvl column.
  tab <- fx_itab()
  expect_true(any(vapply(tab, tabxplor::is_fmt, logical(1))))
  expect_true(tabxplor::is_lvl(tab$Axe))
  expect_true(tabxplor::is_lvl(tab$Question))
})

test_that("tabxplor accepts the table rather than degrading it to plain html", {
  # The one predicate that decides whether the table styles at all; it also proves the character
  # level-name columns sitting between fmt columns do not break the row model.
  expect_false(tabxplor:::tab_render_vars(fx_itab())$degrade)
})

test_that("a question's two sides share one row, and an unmatched level takes its own", {
  # The packing: a level is read against the level facing it, so a question with one positive and
  # one negative level is one line, not two.
  tab  <- fx_itab()
  long <- mca_interpret(fx_mca(), axes = 1:2, type = "console")
  expect_lt(nrow(tab), nrow(long))
  both <- !is.na(tabxplor::get_num(tab[["  "]])) & !is.na(tabxplor::get_num(tab[["   "]]))
  expect_true(any(both))
})

test_that("the total row displays the sum but is coloured against the mean contribution", {
  # The pair that makes the colour say what it claims: `pct` prints, `ctr` grades. If ctr held the
  # sum, every level would read as a fraction of the total instead of a multiple of the mean.
  tab <- fx_itab()
  tot <- which(tabxplor::get_row_kind(tab[["  "]]) == "total")
  expect_gt(length(tot), 0L)
  expect_equal(unique(round(tab[["  "]][tot]$ctr, 10)),
               round(mean(fx_mca()$var$contrib[, 1:2]) / 100, 10))
  expect_gt(tab[["  "]][tot[1]]$pct, tab[["  "]][tot[1]]$ctr)
})

test_that("a negative-side contribution is signed, so it colours on the under-represented half", {
  # The sign rides `ctr` alone: it must never reach `pct`, which is what prints.
  tab <- fx_itab()
  neg <- tab[["   "]]
  dat <- tabxplor::get_row_kind(neg) == "data" & !is.na(neg$pct)
  expect_true(all(neg$ctr[dat] < 0))
  expect_true(all(neg$pct[dat] > 0))
})

test_that("a question's own figures are carried in every cell and displayed once", {
  # `display = "blank"` hides the repeat without dropping the value, which is what lets a reader
  # sort or export the column. Needs a question that keeps two levels on one side -- see fx_mca_multi.
  ctr <- itab(fx_mca_multi(), axes = 1:3)$contrib
  expect_false(any(is.na(ctr$pct)))                    # the field is written everywhere ...
  expect_true(any(is.na(tabxplor::get_num(ctr))))      # ... and get_num() follows the display
  expect_true(any(ctr$display == "blank"))
  expect_true(any(ctr$display != "blank"))
})

test_that("a battery of binary items packs to exactly one row per question", {
  # The packing's best case, and the ordinary MCA input: two levels facing each other are one line.
  tab <- fx_itab()
  expect_equal(nrow(tab), sum(tab$Question != ""))
})

test_that("spread is opt-in on the html table and always present in the console tibble", {
  expect_false("spread" %in% names(fx_itab()))
  expect_true("spread" %in% names(fx_itab(spread = TRUE)))
  expect_true("spread" %in% names(mca_interpret(fx_mca(), axes = 1:2, type = "console")))
})

test_that("mca_interpret(type = 'html') returns a rendered tabxplor table", {
  html <- mca_interpret(fx_mca(), axes = 1:2)
  expect_s3_class(html, "tabxplor_kable")
  expect_match(paste(html, collapse = ""), '<table class="tabxplor-tab"', fixed = TRUE)
  # a character NA would reach the page as the literal string "NA"
  expect_false(grepl(">NA<", paste(html, collapse = ""), fixed = TRUE))
})

test_that("the interpretation table is stable", {
  # n = Inf on purpose: pillar formats only the rows it shows, and a slice with no total row makes
  # color = "contrib" warn that it has no mean contribution to read.
  expect_snapshot(print(fx_itab(spread = TRUE), n = Inf, width = Inf))
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
