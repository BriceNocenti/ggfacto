# PURPOSE: Lock the interpretation tables -- benzecri_mrv(), mca_interpret(), ca_interpret(),
#   pca_interpret() -- and the output contract the whole summary family shares.
# ROLE: These read a factorial analysis WITHOUT the cloud, so what they say must be what the axes
#   say: the mean-contribution threshold, the side a point falls on, and the numbers that decide how
#   many axes to interpret.
# KEY CONSTRAINTS:
#   - ONE table, never a list: the eigenvalues ride meta$footer_tabs, and the format is a print-time
#     decision -- tabxplor's own, options(tabxplor.print). Both are asserted.
#   - Snapshots cover the console tibble, which is small and stable -- never the rendered html, which
#     is 7 kB of inlined stylesheet. Numbers are asserted as facts wherever possible.
#   - tea[1:6] is a battery of binary items, so every question packs to one row. Anything about
#     several levels facing each other on one side needs fx_mca_multi().
# See: CLAUDE.md section ggfacto architecture > Tables are tabxplor's.

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())

# tea[1:6] is all binary, so mca_interpret()'s row packing collapses every question to one line and
# its display blanking has nothing to hide -- that needs multi-level variables and a third axis.
fx_mca_multi <- function() fx("mca_multi", function() {
  MCA2(fx_tea(), tidyselect::all_of(c("Tea", "How", "how", "where", "price")))
})

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

# --- the shared contract -------------------------------------------------------------------------

test_that("every summary is one tagged tabxplor table, never a list", {
  # The contract: a table one can pipe, filter and export. A list would be none of those.
  for (x in list(mca_interpret(fx_mca(), axes = 1:2),
                 ca_interpret(fx_ca()),
                 pca_interpret(fx_pca(), axes = 1:2),
                 suppressWarnings(mean_sd_tab(mtcars, 1:7)),
                 HCPC_tab(fx_tea_clust(), row_vars = tidyselect::all_of(fx_active()),
                          clust = "clust"))) {
    expect_s3_class(x, "ggfacto_summary")
    expect_true(tabxplor::is_tab(x))
    expect_true(any(vapply(x, tabxplor::is_fmt, logical(1))))
  }
})

test_that("an axes summary carries the eigenvalues as a subordinate table", {
  # The eigenvalues are what the rule for choosing how many axes to interpret is read on, so they
  # travel with the summary rather than being a second call the reader must remember.
  for (x in list(mca_interpret(fx_mca(), axes = 1:2),
                 ca_interpret(fx_ca()),
                 pca_interpret(fx_pca(), axes = 1:2))) {
    eig <- tabxplor::get_footer_tabs(x)
    expect_length(eig, 1L)
    expect_true(all(c("Axe", "eigenvalue", "% variance", "cumul.") %in% names(eig[[1]])))
  }
  # ... and only an MCA has a modified rate to put beside them
  expect_true("Benzecri's modified rate" %in% names(tabxplor::get_footer_tabs(mca_interpret(fx_mca()))[[1]]))
  expect_false("Benzecri's modified rate" %in% names(tabxplor::get_footer_tabs(ca_interpret(fx_ca()))[[1]]))
  # a cluster description describes no axes
  expect_null(tabxplor::get_footer_tabs(
    HCPC_tab(fx_tea_clust(), row_vars = tidyselect::all_of(fx_active()), clust = "clust")))
})

test_that("eig = FALSE leaves the eigenvalues out", {
  # A document that shows them already, or prints the summary several times to comment it column by
  # column, wants one copy and not five.
  expect_null(tabxplor::get_footer_tabs(mca_interpret(fx_mca(), axes = 1, eig = FALSE)))
  expect_null(tabxplor::get_footer_tabs(ca_interpret(fx_ca(), eig = FALSE)))
  expect_null(tabxplor::get_footer_tabs(pca_interpret(fx_pca(), axes = 1, eig = FALSE)))

})

test_that("options(tabxplor.print) decides the medium, for a summary as for a crosstab", {
  # There is no ggfacto option: one option governs both, so a script sets it once. A summary that
  # obeyed its own would print differently from the `tab()` two lines above it.
  x <- mca_interpret(fx_mca(), axes = 1)

  withr::with_options(list(tabxplor.print = "html"),
                      expect_s3_class(suppressWarnings(print(x)), "tabxplor_kable"))
  withr::with_options(list(tabxplor.print = "kable"),   # the pre-2.0.0 synonym still works
                      expect_s3_class(suppressWarnings(print(x)), "tabxplor_kable"))

  txt <- utils::capture.output(withr::with_options(list(tabxplor.print = "console"), print(x)))
  expect_true(any(grepl("A tabxplor tab", txt)))
  expect_true(any(grepl("eigenvalue", txt)))        # the subordinate table renders under it

  # markdown is an explicit pipe, not an option: `tabxplor.print` has no "md".
  md <- tabxplor::tab_md(mca_interpret(fx_mca(), axes = 1), css = FALSE, print = FALSE)
  expect_true(any(grepl("^\\|", strsplit(md, "\n")[[1]])))
  expect_true(grepl("Positive_levels", md))
  expect_false(grepl("<style>", md, fixed = TRUE))
})

test_that("the colour legend is tabxplor's, saying ggfacto's nouns", {
  x   <- mca_interpret(fx_mca(), axes = 1, complete = TRUE)
  tpl <- tabxplor::get_subtext(x)

  # the template OWNS the layout, and the legend is generated -- not written here
  expect_identical(tpl[1:2], c("<legend>", "<stars>"))
  expect_false(any(grepl("variance of the axis", tpl)))

  foot <- tabxplor::tab_footer_text(x)
  expect_match(foot[[1]], "[Cc]ontribution to the variance of the axis")
  expect_false(any(grepl("Chi2", foot)))   # a factorial axis has no chi-squared

  # the ladder is BUILT by tabxplor from the plan the cells are painted with, never pasted here
  for (b in tabxplor::get_color_breaks()[["contrib"]])
    expect_match(foot[[1]], paste0("\u00d7", b), fixed = TRUE)
  for (b in tabxplor::get_color_breaks()[["contrib"]])
    expect_false(any(grepl(paste0("\u00d7", b), tpl, fixed = TRUE)))

  # and it is coloured, in a medium the old plain-text line could not reach
  expect_match(tabxplor::tab_footer_text(x, medium = "md")[[1]], "{.p1}", fixed = TRUE)
})

test_that("the glossary names each statistic the colours do NOT grade, and only those", {
  gloss <- function(...) {
    tpl <- tabxplor::get_subtext(mca_interpret(fx_mca(), axes = 1, ...))
    tpl[!startsWith(tpl, "<")]
  }
  # the whole question's contribution is a column of its own, and uncoloured
  expect_identical(length(gloss()), 1L)
  expect_true(startsWith(gloss()[[1]], "contrib:"))

  for (w in c("contrib:", "coord:", "cos2:", "spread:"))
    expect_true(any(startsWith(gloss(complete = TRUE), w)))

  # with no colour measure there is no generated legend, so `ctr` must be named here instead
  expect_true(any(startsWith(gloss(color = FALSE), "ctr:")))
  expect_false(any(startsWith(gloss(), "ctr:")))

  # plain text, always: `subtext` reaches a markdown file and an Excel cell as written
  expect_false(any(grepl("[<{]", gloss(complete = TRUE))))
})

test_that("a PCA legend names the ONE column it grades, and no empty Total row", {
  # The coordinate is the only graded quantity: under `scale.unit` it IS a correlation, so the
  # 0.1/0.2/0.4/0.8 ladder reads it end to end. The cos2 prints beside it and goes to the glossary.
  foot <- tabxplor::tab_footer_text(pca_interpret(fx_pca(), axes = 1:2))
  expect_match(foot[[1]], "[Cc]oordinate on the axis")
  expect_false(grepl("[Qq]uality of representation", foot[[1]]))
  expect_true(any(startsWith(foot, "cos2:")))      # named, not graded
  # the prose form names no reference: the Total row of that column is empty
  expect_false(grepl("Total", foot[[1]]))

  # `set_legend_words()` is keyed on the measure's full name; an acronym is refused
  expect_error(tabxplor::set_legend_words(pca_interpret(fx_pca()), diff = "x"),
               "not a colour measure")
})

# --- mca_interpret -------------------------------------------------------------------------------

test_that("mca_interpret keeps only the levels contributing above the mean", {
  # Le Roux and Rouanet's rule: a level is worth reading only if it contributes more than the
  # average contribution. So the table must be shorter than the full list of levels.
  tab <- mca_interpret(fx_mca(), axes = 1:2)
  expect_lt(nrow(tab), nrow(fx_mca()$var$coord) * 2L)
  expect_gt(nrow(tab), 0L)
})

test_that("mca_interpret covers each requested axis", {
  expect_gt(nrow(mca_interpret(fx_mca(), axes = 1:2)), nrow(mca_interpret(fx_mca(), axes = 1)))
  expect_length(unique(as.character(mca_interpret(fx_mca(), axes = 1:2)$Axe)), 2L)
})

test_that("a question's two sides share one row, and an unmatched level takes its own", {
  # The packing: a level is read against the level facing it, so a question with one positive and
  # one negative level is one line, not two.
  tab  <- mca_interpret(fx_mca(), axes = 1:2)
  both <- !is.na(tabxplor::get_num(tab[["  "]])) & !is.na(tabxplor::get_num(tab[["   "]]))
  expect_true(any(both))
  expect_true(any(!both))
})

test_that("a battery of binary items packs to exactly one row per question", {
  # The packing's best case, and the ordinary MCA input: two levels facing each other are one line.
  tab <- mca_interpret(fx_mca(), axes = 1:2)
  expect_equal(nrow(tab), sum(tab$Question != ""))
})

test_that("the total row displays the sums but is coloured against the mean contribution", {
  # The pair that makes the colour say what it claims: `pct` prints, `ctr` grades. `ctr` holds the
  # MULTIPLE of the mean and the total row holds exactly 1, so a table with one total row per axis
  # cannot grade against another axis's mean.
  tab <- mca_interpret(fx_mca(), axes = 1:2)
  tot <- which(tabxplor::get_row_kind(tab[["  "]]) == "total")
  expect_length(tot, 2L)
  expect_equal(unique(tab[["  "]][tot]$ctr), 1)
  expect_equal(unique(tab[["   "]][tot]$ctr), 1)
  # and the printed figure is the SUM of the side above it, which the colour reference is not
  dat <- tabxplor::get_row_kind(tab[["  "]]) == "data" &
    as.character(tab$Axe) == as.character(tab$Axe)[1]
  expect_equal(unname(tab[["  "]][tot[1]]$pct),
               unname(sum(tab[["  "]][dat]$pct, na.rm = TRUE)), tolerance = 1e-8)
})

test_that("a negative-side contribution is signed, so it colours on the under-represented half", {
  # The sign rides `ctr` alone: it must never reach `pct`, which is what prints.
  neg <- mca_interpret(fx_mca(), axes = 1:2)[["   "]]
  dat <- tabxplor::get_row_kind(neg) == "data" & !is.na(neg$pct)
  expect_true(all(neg$ctr[dat] < 0))
  expect_true(all(neg$pct[dat] > 0))
})

test_that("a question's own figures are carried in every cell and displayed once", {
  # `display = "blank"` hides the repeat without dropping the value, which is what lets a reader
  # sort or export the column. Needs a question that keeps two levels on one side.
  ctr <- mca_interpret(fx_mca_multi(), axes = 1:3)$contrib
  expect_false(any(is.na(ctr$pct)))                    # the field is written everywhere ...
  expect_true(any(is.na(tabxplor::get_num(ctr))))      # ... and get_num() follows the display
  expect_true(any(ctr$display == "blank"))
  expect_true(any(ctr$display != "blank"))
})

test_that("complete = TRUE adds the coordinate, the cos2 and the spread, and nothing else does", {
  concise  <- mca_interpret(fx_mca(), axes = 1:2)
  complete <- mca_interpret(fx_mca(), axes = 1:2, complete = TRUE)

  expect_false(any(c("coord", "cos2", "spread") %in% names(concise)))
  expect_true(all(c("coord", "cos2", "spread", "coord ", "cos2 ") %in% names(complete)))
  expect_identical(nrow(concise), nrow(complete))

  # the coordinate's SIGN is the side it is printed on
  cd  <- complete$coord
  dat <- !is.na(cd$mean)
  expect_true(all(cd$mean[dat] > 0))
  expect_true(all(complete[["coord "]]$mean[!is.na(complete[["coord "]]$mean)] < 0))

  # cos2 sums to 100 % over the AXES -- along a row of the analysis's own cos2 matrix
  expect_identical(unique(tabxplor::get_pct_type(complete$cos2)), "row")
  expect_true(all(complete$cos2$pct[!is.na(complete$cos2$pct)] <= 1))

  # NEITHER is coloured: no absolute threshold fits them here, and a ladder that does not fit is a
  # signal that is plausible and false. Only the contribution carries Le Roux and Rouanet's.
  for (nm in c("coord", "cos2", "coord ", "cos2 "))
    expect_true(unique(tabxplor::get_color(complete[[nm]])) %in% c("no", ""))
  expect_identical(unique(tabxplor::get_color(complete$ctr)), "contrib")

  # A PCA colours the COORDINATE, which there is a correlation and has a ladder that fits. The cos2
  # is never graded on any of the three: its 50 % rule is about a whole cloud, not about a cell.
  pca <- pca_interpret(fx_pca(), axes = 1)
  expect_identical(unique(tabxplor::get_color(pca[["coord_Axe 1"]])), "difference")
  expect_true(unique(tabxplor::get_color(pca[["cos2_Axe 1"]])) %in% c("no", ""))
  expect_true(unique(tabxplor::get_color(pca[["contrib_Axe 1"]])) %in% c("no", ""))
})

test_that("the spread is NA for a question with only one side", {
  # A group with no negative level has no gap to measure, and 0 would read as "no spread".
  tab <- mca_interpret(fx_mca(), axes = 1:2, complete = TRUE)
  one <- is.na(tabxplor::get_num(tab[["ctr "]])) & tab$Question != ""
  expect_true(any(one))
  expect_true(all(is.na(tab$spread$pct[one])))
})

test_that("the retired arguments warn and route", {
  expect_warning(a <- mca_interpret(fx_mca(), axes = 1, spread = TRUE), "deprecated")
  expect_true("spread" %in% names(a))
  expect_warning(mca_interpret(fx_mca(), axes = 1, type = "console"), "deprecated")
})

test_that("several levels of one question on one side are all kept, on continuation rows", {
  # The block is as tall as its longer side (`k <- max(nrow(p), nrow(n), 1L)`) and the shorter one is
  # padded, so nothing is dropped when a question keeps two levels at the same pole. The question's
  # name and its `contrib` are blanked on a continuation row; the LEVEL itself never is.
  tab <- mca_interpret(fx_mca_multi(), axes = 1:3)
  q   <- as.character(tab$Question)
  pos <- as.character(tab$Positive_levels)
  neg <- as.character(tab$Negative_levels)

  cont <- which(q == "")
  expect_gt(length(cont), 0L)                             # there ARE continuation rows
  expect_true(all(nzchar(pos[cont]) | nzchar(neg[cont]))) # each carries a level, never both empty

  # and nothing is lost: on each axis, one side's displayed contributions sum to its summary row.
  # ⚠ on `pct`, the DISPLAYED share -- `ctr` is the multiple of the mean contribution, and every
  # summary row holds exactly 1 by construction, so summing it would compare nothing.
  ctr  <- tab[[which(names(tab) == "Positive_levels") + 1L]]
  kind <- tabxplor::get_row_kind(ctr)
  for (a in unique(as.character(tab$Axe))) {
    rows <- as.character(tab$Axe) == a
    expect_equal(sum(ctr$pct[rows & kind == "data"], na.rm = TRUE),
                 sum(ctr$pct[rows & kind != "data"], na.rm = TRUE), tolerance = 1e-6)
  }
})

test_that("the MCA interpretation table is stable", {
  # n = Inf on purpose: pillar formats only the rows it shows, and a slice with no total row makes
  # color = "contrib" warn that it has no mean contribution to read.
  withr::local_options(tabxplor.print = "console")
  expect_snapshot(print(mca_interpret(fx_mca(), axes = 1:2), n = Inf, width = Inf))
  expect_snapshot(print(mca_interpret(fx_mca(), axes = 1:2, complete = TRUE), n = Inf, width = Inf))
})

# --- ca_interpret --------------------------------------------------------------------------------

test_that("ca_interpret reads both margins, each against its own threshold and under its own name", {
  # A CA's rows and its columns each sum to 100 % of the axis over a DIFFERENT number of points, so
  # one pooled mean contribution would keep too many of one and too few of the other. And each
  # summary row NAMES its margin: an axis carries two of them, and the bare label said which of the
  # two contributions it totals -- twice the same word.
  tab  <- ca_interpret(fx_ca())
  vars <- as.character(tab$Variable)
  tot  <- tabxplor::get_row_kind(tab[["  "]]) == "total"
  expect_equal(sum(tot), 2L * nrow(fx_ca()$eig))

  marges <- setdiff(unique(vars[!tot]), "")
  expect_length(marges, 2L)
  expect_setequal(unique(vars[tot]), paste0(marges, ": above mean ctr"))

  # the threshold still drives the wording, margin name and all
  z <- as.character(ca_interpret(fx_ca(), min_contrib = 0)$Variable)
  zt <- tabxplor::get_row_kind(ca_interpret(fx_ca(), min_contrib = 0)[["  "]]) == "total"
  expect_setequal(unique(z[zt]), paste0(marges, ": all levels"))

  # an MCA has ONE set, so it keeps the bare label the courses and the skill quote
  expect_true("Above mean ctr" %in% as.character(mca_interpret(fx_mca(), axes = 1)$Question))
})

test_that("ca_interpret has no contribution column, because it would always be 100 %", {
  expect_false("contrib" %in% names(ca_interpret(fx_ca())))
  expect_equal(unname(colSums(fx_ca()$row$contrib)), rep(100, ncol(fx_ca()$row$contrib)),
               tolerance = 1e-6)
})

test_that("ca_interpret names the margins from the matrix, or says Rows and Columns", {
  # unnamed dimnames: the two words that are always true
  expect_true(all(c("Rows", "Columns") %in% as.character(ca_interpret(fx_ca())$Variable)))
  # ⚠ FactoMineR::CA() drops names(dimnames()) from every matrix it keeps, so they cannot be
  # recovered from the result: `vars =` is the only way to name the two margins.
  named <- ca_interpret(fx_ca(), vars = c("race", "marital"))
  expect_true(all(c("race", "marital") %in% as.character(named$Variable)))
  # ONE table: the crosstab is asked for separately, not carried along
  expect_length(tabxplor::get_footer_tabs(ca_interpret(fx_ca())), 1L)
  expect_false("crosstab" %in% names(formals(ca_interpret)))
})

test_that("the CA interpretation table is stable", {
  withr::local_options(tabxplor.print = "console")
  expect_snapshot(print(ca_interpret(fx_ca(), complete = TRUE), n = Inf, width = Inf))
})

# --- pca_interpret -------------------------------------------------------------------------------

test_that("pca_interpret works on a single axis", {
  # One axis drops the matrix to a named vector, and the reshape then selects on a column that is no
  # longer called "Dim.1" -- an error, at the last step, on the most ordinary call there is.
  tab <- pca_interpret(fx_pca(), axes = 1)
  expect_true(all(c("coord_Axe 1", "contrib_Axe 1", "cos2_Axe 1") %in% names(tab)))
})

test_that("pca_interpret gives one block of three columns per axis, named as the reader names it", {
  tab <- pca_interpret(fx_pca(), axes = 1:2)
  expect_true(all(c("coord_Axe 1", "contrib_Axe 1", "cos2_Axe 1", "coord_Axe 2", "contrib_Axe 2", "cos2_Axe 2") %in% names(tab)))
  expect_identical(unique(tabxplor::get_col_var(tab[["coord_Axe 1"]])), "Axe 1")
  expect_identical(unique(tabxplor::get_col_var(tab[["cos2_Axe 2"]])), "Axe 2")
  # the contributions of an axis sum to 100 %, and the Total row states the MEAN
  dat <- tabxplor::get_row_kind(tab[["contrib_Axe 1"]]) == "data"
  expect_equal(sum(tab[["contrib_Axe 1"]]$pct[dat]) * 100, 100, tolerance = 1e-6)
})

test_that("the PCA interpretation table is stable", {
  withr::local_options(tabxplor.print = "console")
  expect_snapshot(print(pca_interpret(fx_pca(), axes = 1:2), n = Inf, width = Inf))
})

# --- the eigenvalue block ------------------------------------------------------------------------


test_that("the eigenvalue table ends on an ellipsis STATING how many axes the cloud has", {
  # A reader must know how many axes there are, whether the analysis has nine or forty. The count is
  # what answers that, so the ellipsis carries it -- and the last axis is not shown, its numbers
  # having nothing above them to be read against.
  eig <- function(...) tabxplor::get_footer_tabs(mca_interpret(fx_mca(), axes = 1, ...))[[1]]

  full <- eig(n_axes = 8L)                       # tea[1:6] has 6 axes: nothing is missing
  expect_identical(as.character(full$Axe), c(paste("Axe", 1:6), "Total"))
  expect_false(any(grepl("\\.\\.\\.", as.character(full$Axe))))   # no ellipsis over nothing

  cut <- eig(n_axes = 2L)
  expect_identical(as.character(cut$Axe), c("Axe 1", "Axe 2", "... of 6", "Total"))
  # the ellipsis row is a display device, and says so
  expect_identical(tabxplor::get_row_kind(cut[["% variance"]])[3], "blank")
  expect_true(is.na(cut[["% variance"]]$pct[3]))
})

test_that("the ellipsis counts the CLOUD's axes, not the ones the fit kept", {
  # `ncp` truncates `res$eig`, so the count cannot be read off it: an MCA has (levels - questions)
  # axes, whatever the fit kept. Without this, a table showing 5 of 27 axes claimed to show them all.
  cut <- MCA2(fx_tea(), 1:6, ncp = 2)
  eig <- tabxplor::get_footer_tabs(mca_interpret(cut, axes = 1))[[1]]
  expect_identical(as.character(eig$Axe), c("Axe 1", "Axe 2", "... of 6", "Total"))

  # and the Total still covers the whole cloud, the ellipsis standing for the axes not kept
  tot <- which(tabxplor::get_row_kind(eig[["% variance"]]) == "total")
  expect_equal(unname(eig[["% variance"]]$pct[tot]), 1)
  expect_equal(unname(eig$eigenvalue$var[tot]), sum(fx_mca()$eig[, 1]))
})


test_that("the eigenvalue table totals the whole cloud, truncation included", {
  # The Total is the axes shown plus those the ellipsis counts: 100 % and the total inertia, which
  # is read off one axis because `ncp` truncates `res$eig` (FactoMineR's default keeps 5).
  eig <- tabxplor::get_footer_tabs(mca_interpret(fx_mca(), axes = 1))[[1]]
  tot <- which(tabxplor::get_row_kind(eig[["% variance"]]) == "total")
  expect_length(tot, 1L)
  expect_equal(unname(eig[["% variance"]]$pct[tot]), 1)          # MCA2() keeps every axis
  expect_equal(unname(eig$eigenvalue$var[tot]), sum(fx_mca()$eig[, 1]))

  cut  <- MCA2(fx_tea(), 1:6, ncp = 3)
  eig2 <- tabxplor::get_footer_tabs(mca_interpret(cut, axes = 1))[[1]]
  tot2 <- which(tabxplor::get_row_kind(eig2[["% variance"]]) == "total")
  expect_equal(unname(eig2[["% variance"]]$pct[tot2]), 1)
  expect_equal(unname(eig2$eigenvalue$var[tot2]), sum(fx_mca()$eig[, 1]))

  # a raw FactoMineR fit, default `ncp = 5` on 7 scaled variables: 100 % of an inertia of 7
  pca  <- FactoMineR::PCA(datasets::mtcars[1:7], graph = FALSE)
  eig3 <- tabxplor::get_footer_tabs(pca_interpret(pca))[[1]]
  tot3 <- which(tabxplor::get_row_kind(eig3[["% variance"]]) == "total")
  expect_equal(unname(eig3[["% variance"]]$pct[tot3]), 1)
  expect_equal(unname(eig3$eigenvalue$var[tot3]), 7)

  # `% variance` carries a data bar, scaled on the column's own largest axis: NA is `set_bars()`'s
  # word for "no ceiling stated". A ceiling of 100 % would flatten every MCA scree into stubs.
  expect_identical(tabxplor::get_bars(eig), c("% variance" = NA_real_))
  expect_null(tabxplor::get_bars(
    tabxplor::get_footer_tabs(mca_interpret(fx_mca(), axes = 1, color = FALSE))[[1]]))
})

test_that("the data bar reaches the RENDERED html, on the data rows alone", {
  # A VALUE test, not a shape one: the bar was once set, documented and tested, and displayed never --
  # `% variance` has a space in its name, and anything keyed by column name goes stale silently. Only
  # reading the widths out of the html says whether a bar was drawn, and where.
  h <- paste(as.character(tabxplor::tab_html(mca_interpret(fx_mca(), axes = 1, n_axes = 4))),
             collapse = "")
  rows <- regmatches(h, gregexpr("<tr[^>]*>.*?</tr>", h))[[1]]
  wid  <- function(x) as.numeric(sub("%.*", "", sub(".*--tx-bar:", "", x)))
  bars <- unlist(regmatches(h, gregexpr("--tx-bar:[0-9.]+%", h)))

  expect_length(bars, 4L)                       # the four axes shown, and nothing else
  expect_equal(wid(bars[1]), 100)               # the largest fills its cell: that IS the ceiling
  expect_true(all(diff(wid(bars)) < 0))         # and the decline is the shape one reads

  # neither the ellipsis nor the Total is on the axes' scale, so neither carries a bar
  expect_false(any(grepl("tx-bar", rows[grepl("\\.\\.\\. of", rows)])))
  expect_false(any(grepl("tx-bar", rows[grepl(">Total<", rows)])))
})

test_that("MCA2() keeps every axis, so the modified rate is the cloud's", {
  # `ncp` truncates `res$eig`, and benzecri_mrv() renormalises over the axes it finds: a truncated
  # fit gives the SAME axis a different modified rate. The default must therefore keep them all.
  expect_identical(formals(MCA2)$ncp, Inf)
  expect_identical(formals(PCA2)$ncp, Inf)

  full <- MCA2(fx_tea(), 1:6)
  cut  <- MCA2(fx_tea(), 1:6, ncp = 2)
  expect_gt(nrow(full$eig), nrow(cut$eig))
  expect_equal(sum(full$eig[, 2]), 100, tolerance = 1e-6)
  # tea[1:6] has THREE axes above 1/Q, so cutting at two renormalises the rate over two of them:
  # the same axis then reports a different modified rate, which is the defect the default closes.
  expect_false(isTRUE(all.equal(benzecri_mrv(full)[[1]], benzecri_mrv(cut)[[1]])))

  # and an axis the fit did not keep is dropped, not indexed past the end
  expect_identical(as.character(mca_interpret(cut, axes = 1:8, eig = FALSE)$Axe),
                   as.character(mca_interpret(cut, axes = 1:2, eig = FALSE)$Axe))
})


test_that("min_contrib moves the threshold, and the summary row's label follows it", {
  # A label that says "above the mean" over a row that totals something else is the one thing this
  # table must never do.
  lab <- function(...) setdiff(as.character(mca_interpret(fx_mca(), axes = 1, ...)$Question), "")
  expect_true("Above mean ctr" %in% lab())
  expect_true("All levels"     %in% lab(min_contrib = 0))
  expect_true("Above 5%"       %in% lab(min_contrib = 5))
  # ... and 0 really keeps everything
  expect_gt(nrow(mca_interpret(fx_mca(), axes = 1, min_contrib = 0)),
            nrow(mca_interpret(fx_mca(), axes = 1)))
})


test_that("color = FALSE builds the table without a colour measure at all", {
  for (x in list(mca_interpret(fx_mca(), axes = 1, color = FALSE),
                 ca_interpret(fx_ca(), color = FALSE))) {
    fmts <- Filter(tabxplor::is_fmt, as.list(x))
    expect_true(all(vapply(fmts, function(c) all(tabxplor::get_color(c) %in% c("no", "")),
                           logical(1))))
  }
  # a PCA keeps its COORDINATE coloured, and neither its cos2 nor its contribution
  pca <- pca_interpret(fx_pca(), axes = 1)
  expect_identical(unique(tabxplor::get_color(pca[["coord_Axe 1"]])), "difference")
  expect_true(unique(tabxplor::get_color(pca[["cos2_Axe 1"]]))    %in% c("no", ""))
  expect_true(unique(tabxplor::get_color(pca[["contrib_Axe 1"]])) %in% c("no", ""))
})


test_that("pca_interpret opens with what the variables look like, in one block", {
  # mean_sd_tab()'s three figures, taken from the analysis itself so the two cannot disagree.
  tab <- pca_interpret(fx_pca(), axes = 1)
  expect_true(all(c("mean_Variables", "sd_Variables", "sd/mean_Variables") %in% names(tab)))
  expect_false("n" %in% names(tab))
  expect_identical(unique(tabxplor::get_col_var(tab$mean_Variables)), "Variables")
  expect_equal(unname(tab$mean_Variables$mean[1]), unname(fx_pca()$call$centre[1]))
  # ONE record printed three times: the sd and the cv are derived from the same variance
  expect_identical(tab$mean_Variables$var, tab$sd_Variables$var)
  expect_identical(unique(tab[["sd/mean_Variables"]]$display), "cv")

  # ⚠ ASSERT THE VALUE, NOT ONLY THE SHAPE. The three columns were structurally perfect and
  # ENTIRELY BLANK -- `call$ecart.red` does not exist (the slot is `ecart.type`) -- and both the
  # snapshot and an identical-`var` check passed on NA == NA.
  x   <- as.data.frame(fx_pca()$call$X)[, rownames(fx_pca()$var$coord), drop = FALSE]
  ref <- vapply(x, function(v) mean((v - mean(v))^2), numeric(1))
  expect_equal(unname(tab$sd_Variables$var[seq_along(ref)]), unname(ref))
  expect_false(any(is.na(tab$sd_Variables$var[seq_along(ref)])))
  expect_true(all(nzchar(trimws(format(tab$sd_Variables)[seq_along(ref)]))))

  # and it is the variables' OWN spread, so `scale.unit` cannot move it: `call$ecart.type` is the
  # scaling divisor and is 1 for every variable when the analysis does not scale.
  unscaled <- pca_interpret(
    FactoMineR::PCA(as.data.frame(fx_pca()$call$X), graph = FALSE, scale.unit = FALSE), axes = 1)
  expect_equal(unscaled$sd_Variables$var, tab$sd_Variables$var)
})


test_that("an export shows the bare column name under its axis span", {
  # The suffix tabxplor strips (tab_col_var_header) is what lets the tibble keep unique names while
  # html, markdown and Excel show "coord" under an "Axe 1" span.
  h <- as.character(ggfacto:::gda_render(pca_interpret(fx_pca(), axes = 1:2), "html"))
  # tabxplor sets a span header's spaces as narrow no-break spaces in html
  expect_true(grepl(">Axe[ \u202f]1</th>", h))
  expect_true(grepl(">coord</th>", h, fixed = TRUE))
  expect_false(grepl("coord_Axe", h, fixed = TRUE))
  expect_true(grepl(">sd/mean</th>", h, fixed = TRUE))
})


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

# *Silent failure guarded: a function retired in the documentation and nowhere else.*
test_that("mean_sd_tab() says it is retired, and names its replacement", {
  # the notice fires ONCE per session, so the register is cleared first -- an earlier call in this
  # file has already spent it.
  e <- ggfacto:::deprecated_args_warned
  rm(list = ls(envir = e), envir = e)
  expect_warning(mean_sd_tab(mtcars, 1:3), "pca_interpret")
})
