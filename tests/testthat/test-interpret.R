# PURPOSE: Lock the interpretation tables -- benzecri_mrv(), mca_interpret(), ca_interpret(),
#   pca_interpret() -- and the output contract the whole summary family shares.
# ROLE: These read a factorial analysis WITHOUT the cloud, so what they say must be what the axes
#   say: the mean-contribution threshold, the side a point falls on, and the numbers that decide how
#   many axes to interpret.
# KEY CONSTRAINTS:
#   - ONE table, never a list: the eigenvalues ride meta$footer_tabs, and the format is a print-time
#     decision (options(ggfacto.print)). Both are asserted, in the three media.
#   - Snapshots cover the console tibble, which is small and stable -- never the rendered html, which
#     is 7 kB of inlined stylesheet. Numbers are asserted as facts wherever possible.
#   - tea[1:6] is a battery of binary items, so every question packs to one row. Anything about
#     several levels facing each other on one side needs fx_mca_multi().
# See: CLAUDE.md section ggfacto architecture > Tables are tabxplor's.

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())

# tea[1:6] is all binary, so mca_interpret()'s row packing collapses every question to one line and
# its display blanking has nothing to hide -- that needs multi-level variables and a third axis.
fx_mca_multi <- function() fx("mca_multi", function() {
  MCA2(fx_tea(), tidyselect::all_of(c("Tea", "How", "how", "where", "price")), ncp = 5)
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

test_that("options(ggfacto.print) decides the medium, and html is the default", {
  x <- mca_interpret(fx_mca(), axes = 1)

  withr::with_options(list(ggfacto.print = NULL), {
    out <- suppressWarnings(print(x))
    expect_s3_class(out, "tabxplor_kable")          # unset means html, whatever tabxplor asks
  })
  withr::with_options(list(ggfacto.print = "html", tabxplor.print = "console"),
                      expect_s3_class(suppressWarnings(print(x)), "tabxplor_kable"))

  md <- utils::capture.output(withr::with_options(list(ggfacto.print = "md"), print(x)))
  expect_true(any(grepl("^\\|", md)))               # a markdown pipe table
  expect_true(any(grepl("Positive_levels", md)))
  expect_true(any(grepl("eigenvalue", md)))         # the subordinate table renders under it
  expect_false(any(grepl("<style>", md, fixed = TRUE)))   # css = FALSE, always

  txt <- utils::capture.output(
    withr::with_options(list(ggfacto.print = "console", tabxplor.print = "html"), print(x)))
  expect_true(any(grepl("A tabxplor tab", txt)))    # console means console, whatever tabxplor asks
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

test_that("a PCA gets one legend line per scale, and no line about the empty Total row", {
  # ONE measure (`difference`) on TWO scales: `coord` in axis SD, `cos2` in percentage points.
  foot <- tabxplor::tab_footer_text(pca_interpret(fx_pca(), axes = 1:2))
  expect_match(foot[[1]], "[Cc]oordinate on the axis")
  expect_match(foot[[2]], "[Qq]uality of representation")
  # the prose form names no reference: the Total row of these two columns is empty
  expect_false(any(grepl("Total", foot[1:2])))

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
  # ... whereas a PCA colours both, because there the two quantities DO have a ladder that fits
  pca <- pca_interpret(fx_pca(), axes = 1)
  expect_identical(unique(tabxplor::get_color(pca[["coord_Axe 1"]])), "difference")
  expect_identical(unique(tabxplor::get_color(pca[["cos2_Axe 1"]])), "difference")
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

test_that("the MCA interpretation table is stable", {
  # n = Inf on purpose: pillar formats only the rows it shows, and a slice with no total row makes
  # color = "contrib" warn that it has no mean contribution to read.
  withr::local_options(ggfacto.print = "console")
  expect_snapshot(print(mca_interpret(fx_mca(), axes = 1:2), n = Inf, width = Inf))
  expect_snapshot(print(mca_interpret(fx_mca(), axes = 1:2, complete = TRUE), n = Inf, width = Inf))
})

# --- ca_interpret --------------------------------------------------------------------------------

test_that("ca_interpret reads both margins, each against its own threshold", {
  # A CA's rows and its columns each sum to 100 % of the axis over a DIFFERENT number of points, so
  # one pooled mean contribution would keep too many of one and too few of the other.
  tab <- ca_interpret(fx_ca())
  vars <- as.character(tab$Variable)
  expect_length(setdiff(unique(vars), c("", "Above mean ctr")), 2L)
  # one summary row per (axis, margin), under the block it sums
  tot <- tabxplor::get_row_kind(tab[["  "]]) == "total"
  expect_equal(sum(tot), 2L * nrow(fx_ca()$eig))
  expect_true(all(vars[tot] == "Above mean ctr"))
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
  withr::local_options(ggfacto.print = "console")
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
  withr::local_options(ggfacto.print = "console")
  expect_snapshot(print(pca_interpret(fx_pca(), axes = 1:2), n = Inf, width = Inf))
})

# --- the eigenvalue block ------------------------------------------------------------------------


test_that("the eigenvalue table shows n_axes rows, then an ellipsis and the last one", {
  # A reader must know how many axes there are, whether the analysis has nine or forty -- so the LAST
  # row is shown whatever `n_axes` says, and an ellipsis row states that something was skipped.
  eig <- function(...) tabxplor::get_footer_tabs(mca_interpret(fx_mca(), axes = 1, ...))[[1]]
  full <- eig(n_axes = 8L)                       # tea[1:6] has 5 axes: nothing to skip
  expect_identical(as.character(full$Axe), c(paste("Axe", 1:5), "Total"))
  expect_false(any(as.character(full$Axe) == "..."))

  cut <- eig(n_axes = 2L)
  expect_identical(as.character(cut$Axe), c("Axe 1", "Axe 2", "...", "Axe 5", "Total"))
  # the ellipsis row is a display device, and says so
  expect_identical(tabxplor::get_row_kind(cut[["% variance"]])[3], "blank")
  expect_true(is.na(cut[["% variance"]]$pct[3]))
})


test_that("the eigenvalue table totals what the axes add up to, and carries a data bar", {
  eig <- tabxplor::get_footer_tabs(mca_interpret(fx_mca(), axes = 1))[[1]]
  tot <- which(tabxplor::get_row_kind(eig[["% variance"]]) == "total")
  expect_length(tot, 1L)
  expect_equal(unname(eig[["% variance"]]$pct[tot]), 1)
  expect_equal(unname(eig$eigenvalue$var[tot]), sum(fx_mca()$eig[, 1]))
  # `% variance` IS the screeplot: a bar chart inside the table (tabxplor::set_bars())
  expect_identical(tabxplor::get_bars(eig), "% variance")
  expect_null(tabxplor::get_bars(
    tabxplor::get_footer_tabs(mca_interpret(fx_mca(), axes = 1, color = FALSE))[[1]]))
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
  # a PCA keeps its coordinate and its cos2 coloured, and never its contribution
  pca <- pca_interpret(fx_pca(), axes = 1)
  expect_identical(unique(tabxplor::get_color(pca[["coord_Axe 1"]])), "difference")
  expect_identical(unique(tabxplor::get_color(pca[["cos2_Axe 1"]])),  "difference")
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
  expect_true(grepl(">Axe 1</th>", h, fixed = TRUE))
  expect_true(grepl(">coord</th>", h, fixed = TRUE))
  expect_false(grepl("coord_Axe", h, fixed = TRUE))
  expect_true(grepl(">sd/mean</th>", h, fixed = TRUE))
})
