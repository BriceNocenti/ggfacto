# PURPOSE: Lock the crosstabs that travel inside each point's hover tooltip.
# ROLE: This is the package's one bet -- "not mainly visual but statistical". The tooltip carries the
#   Burt table the MCA was computed from, so the reader re-derives the geometry from the data.
#   interactive_tooltips() is internal and is reached here through ggmca_data(); its cells are
#   compared, one by one, with a direct tabxplor::tab() on the individuals.
# KEY CONSTRAINTS:
#   - The cells are computed on the answer profiles (R/tooltips.R), with tabxplor::tab()'s own
#     arithmetic: expect_cells_equal_tab() pins counts, percentages, differences, colours and text
#     against tab(), on weights that are not exact in binary.
#   - Tooltips are byte-identical across runs (no sampling, no hashing, no locale-dependent sort),
#     which is what makes expect_snapshot() safe here. If one ever becomes unstable, delete the
#     snapshot rather than loosening it.
#   - The frequency denominator is the POPULATION, computed once before any table is bound. Using
#     the last row of the bound tables instead once gave levels a frequency above 100 %.
#   - Numbers are aligned with str_pad() under a monospace font, so the shim's vector `width` is
#     load-bearing. See tests/testthat/test-str-shim.R.
# See: CLAUDE.md section ggfacto architecture > The tooltip is the package.

# The tooltip as the reader sees it: ggmca_plot() joins the header, the contributions and the body.
tips <- function(plot_data) {
  vd <- suppressMessages(ggmca_plot(plot_data, get_data = TRUE))
  vd <- dplyr::bind_rows(vd$vars_data, vd$mean_point_data)
  stats::setNames(vd$interactive_text, paste(vd$vars, vd$lvs))
}

# --- the header every tooltip opens with --------------------------------------------------------

test_that("each tooltip names its level, its variable and its frequency", {
  plot_data <- fx_pd_active()
  first <- plot_data$vars_data$begin_text[1]

  expect_match(first, "<b>.+</b>")
  expect_match(first, "Frequency")
  expect_match(first, "n=[0-9]+")
})

test_that("no level is more frequent than the whole population", {
  # WARNING: this is the exact bug the denominator rule exists to prevent. The population is
  # computed once, before any table is bound; taking the last row of the bound tables instead --
  # whatever the last variable's last level happens to be -- put some levels above 100 %.
  plot_data <- fx_pd_active()
  pct <- as.numeric(sub(".*Frequency[^:]*:\\s*([0-9.]+)\\s*%.*", "\\1",
                        plot_data$vars_data$begin_text))
  pct <- pct[!is.na(pct)]
  expect_gt(length(pct), 0L)
  expect_true(all(pct <= 100))
})

test_that("the frequencies of one variable's levels sum to about 100 %", {
  plot_data <- fx_pd_active()
  vd  <- plot_data$vars_data
  pct <- as.numeric(sub(".*Frequency[^:]*:\\s*([0-9.]+)\\s*%.*", "\\1", vd$begin_text))
  one <- pct[as.character(vd$vars) == fx_active()[1] & !is.na(pct)]
  expect_equal(sum(one), 100, tolerance = 2)
})

# --- the crosstabs ------------------------------------------------------------------------------

test_that("active_tables puts one crosstab line per active variable in the tooltip", {
  body <- fx_pd_active()$vars_data$interactive_text[1]

  # The "Active variables:" heading, then one percentage line per active variable.
  expect_match(body, "Active variables:", fixed = TRUE)
  lines <- strsplit(sub(".*Active variables:</b>", "", body), "\n")[[1]]
  expect_equal(sum(grepl("%", lines)), length(fx_active()))
})

test_that("the crosstab cells are coloured HTML, which is how deviation from the mean is shown", {
  # Each percentage is coloured by its spread from the mean, blue over- and red under-represented.
  # The colour is read through tabxplor's public accessor, never from its internals.
  all_text <- tips(fx_pd_active())
  expect_true(any(grepl("<font color", all_text, fixed = TRUE)))
})

test_that("a level's own crosstab cell against its own variable is 100 %", {
  # A sanity check on orientation: crossed with itself, a level is entirely itself.
  plot_data <- fx_pd_active()
  vd <- plot_data$vars_data
  i  <- which(as.character(vd$vars) == fx_active()[1])[1]
  expect_match(vd$interactive_text[i], "100%")
})

test_that("tooltip level names carry no tabxplor disambiguation suffix", {
  # tea's "breakfast" is both a variable and a level, which tabxplor would rename "breakfast_lv".
  # The crosstab's rows are mapped back to their levels by code, so no such name reaches lvs.
  plot_data <- fx_pd_active()
  expect_s3_class(plot_data$vars_data$lvs, "factor")
  expect_false(any(grepl("_lv$", as.character(plot_data$vars_data$lvs))))
})

# --- weights ride one channel, all the way into the tooltip -------------------------------------

test_that("a weighted PROFILE prints a weighted n, and an unweighted one does not", {
  # The weighted n is printed only when it differs from the unweighted one, so a weighted analysis
  # is never described by unweighted numbers and an unweighted one is not cluttered with a second
  # identical figure. It lives on the profile tooltips, which are the ones counting individuals.
  weighted   <- md(fx_mca_wt(), fx_tea_wt(), profiles = TRUE)$ind_data
  unweighted <- md(fx_mca(),    fx_tea(),    profiles = TRUE)$ind_data

  expect_true(any(grepl("weighted n", weighted$interactive_text, fixed = TRUE)))
  expect_false(any(grepl("weighted n", unweighted$interactive_text, fixed = TRUE)))
})

test_that("the tooltip describes the population the analysis was FITTED on", {
  # Weights are recovered from the fit (`source$w`), never from the data argument, so handing a
  # different `data` cannot change the counts the tooltip reports.
  a <- tips(md(fx_mca_wt(), fx_tea_wt(), active_tables = "active"))
  b <- tips(md(fx_mca_wt(), fx_tea(),    active_tables = "active"))
  expect_identical(a, b)
})

test_that("a crossed variable with missing values is compared to its own total", {
  # Each variable keeps its own Total as the reference of its differences, as one tab() per
  # variable would. A variable with missing values is where one shared Total would go wrong.
  d <- fx_tea()
  d$SPC[1:40] <- NA
  vd <- md(fx_mca(), d, sup_vars = "SPC", active_tables = "sup")$vars_data
  direct <- tabxplor::tab(d, SPC, lunch, na = "drop", pct = "row", color = "difference")
  cell <- direct[[which(vapply(direct, tabxplor::is_fmt, logical(1)))[1]]]
  lvs  <- as.character(direct$SPC)

  for (lv in setdiff(lvs, "Total")) {
    body <- vd$interactive_text[vd$vars == "SPC" & as.character(vd$lvs) == lv]
    line <- regmatches(body, regexpr("\nlunch(_lv)?: [^\n]*", body))
    x <- cell[lvs == lv]
    diff <- round(vctrs::field(x, "diff") * 100)
    nums <- as.numeric(regmatches(line, gregexpr("[0-9]+(?=%)", line, perl = TRUE))[[1]])
    expect_equal(nums[length(nums)], round(vctrs::field(x, "pct") * 100))
    if (diff != 0) expect_equal(nums[1] * sign(diff), diff)
    colour <- tabxplor::fmt_get_color_code(x)
    expect_identical(grepl("<font color", line), !is.na(colour))
  }
})

# --- the cells are tabxplor's, computed on the profiles -----------------------------------------

# The tooltip cells of `rows` x `cols`, built on the units, against a direct tabxplor::tab() on the
# individuals: counts exactly, weighted counts, percentages and differences to 1e-12, and colour
# codes and formatted text identical.
expect_cells_equal_tab <- function(res, data, rows, cols) {
  m     <- mca_model(res)
  data  <- align_to_fit(m, data)
  extra <- setdiff(c(rows, cols), m$vars)
  xtra  <- tibble::as_tibble(purrr::map(data[extra], \(x) forcats::fct_drop(as.factor(x))))
  units <- mca_units(m, if (length(extra) != 0) xtra)
  units[m$vars] <- active_factors(m, units$..profile)
  ind   <- tibble::as_tibble(active_factors(m, m$key))
  ind[extra] <- xtra
  ind$.w <- if (is.null(m$w)) rep(1, m$n) else m$w
  s <- stack_levels(units, rows)
  for (v in cols) {
    cells <- crosstab_cells(units, s, v)
    for (b in seq_along(rows)) {
      dd <- ind
      dd$.R <- dd[[rows[b]]]
      direct <- tabxplor::tab(dd, ".R", v, wt = ".w", na = "drop", pct = "row",
                              color = "difference")
      direct <- direct[as.character(direct$.R) != "Total", ]
      fmts   <- names(direct)[vapply(direct, tabxplor::is_fmt, logical(1))]
      in_b   <- which(s$block == b)[seq_len(nrow(direct))]
      for (j in seq_len(nlevels(units[[v]]))) {
        new <- cells_fmt(cells, j, v)[in_b]
        old <- direct[[fmts[j]]]
        expect_identical(vctrs::field(new, "n"), as.integer(vctrs::field(old, "n")))
        for (f in c("wn", "pct", "diff")) {
          expect_equal(vctrs::field(new, f), vctrs::field(old, f), tolerance = 1e-12)
        }
        expect_identical(tabxplor::fmt_get_color_code(new), tabxplor::fmt_get_color_code(old))
        expect_identical(format(new), format(old))
      }
    }
  }
}

fx_tea_rw <- function() fx("tea_rw", function() {
  d <- fx_tea()
  d$SPC[1:26] <- NA
  d$sex[40:61] <- NA
  d$w <- withr::with_seed(1, round(stats::runif(nrow(d), 0.2, 3), 3))
  d
})

test_that("the crosstab cells are tab()'s: active x active, weighted", {
  expect_cells_equal_tab(MCA2(fx_tea_rw(), 1:6, wt = "w"), fx_tea_rw(), fx_active(), fx_active())
})

test_that("the crosstab cells are tab()'s: a supplementary variable with missing values", {
  expect_cells_equal_tab(MCA2(fx_tea_rw(), 1:6, wt = "w"), fx_tea_rw(), "SPC", fx_active())
})

test_that("the crosstab cells are tab()'s: a tooltip variable with missing values", {
  expect_cells_equal_tab(MCA2(fx_tea_rw(), 1:6, wt = "w"), fx_tea_rw(), fx_active(), "sex")
})

test_that("the crosstab cells are tab()'s: excluded levels", {
  expect_cells_equal_tab(MCA2(fx_tea_na(), 1:6), fx_tea_na(), fx_active(), fx_active())
})

# --- what the tooltips no longer get wrong ------------------------------------------------------

header_n <- function(vd, lv) {
  as.numeric(sub(".*\\(n=([0-9]+)\\).*", "\\1", vd$begin_text[as.character(vd$lvs) == lv][1]))
}

test_that("a level's count is its own, whatever the tooltip variables leave out", {
  d <- fx_tea()
  d$sex[1:30] <- NA
  with_na <- md(fx_mca(), d, tooltip_vars = sex)$vars_data
  expect_identical(header_n(with_na, "breakfast"), header_n(fx_pd_active()$vars_data, "breakfast"))
})

test_that("the central point is the whole population, even with a supplementary variable missing", {
  d <- fx_tea()
  d$SPC[1:40] <- NA
  vd <- md(fx_mca(), d, sup_vars = SPC, active_tables = c("active", "sup"))$vars_data
  expect_match(vd$begin_text[vd$lvs == "Central point"], "n=300): 100%", fixed = TRUE)
})

test_that("the central point keeps every level of the tooltip variables", {
  vd <- md(fx_mca(), fx_tea(), tooltip_vars = sex)$vars_data
  central <- vd$interactive_text[vd$lvs == "Central point"]
  for (lv in levels(fx_tea()$sex)) expect_match(central, paste0(lv, ": "), fixed = TRUE)
})

test_that("no tooltip prints an excluded or a merged level", {
  res <- MCA2(fx_tea_na(), 1:6)
  txt <- md(res, fx_tea_na(), tooltip_vars_1lv = SPC)$vars_data$interactive_text
  expect_false(any(grepl("Remove_levels", txt, fixed = TRUE)))
})

test_that("a yes/no battery shows one line per question, and a level named n does not break", {
  d <- fx_tea()[1:6]
  for (v in names(d)) d[[v]] <- factor(ifelse(as.integer(d[[v]]) == 1, "y", "n"))
  vd <- md(MCA2(d, 1:6), d)$vars_data
  body  <- sub(".*Active variables:</b>\n", "", vd$interactive_text[1])
  lines <- strsplit(body, "\n")[[1]]
  expect_length(lines, 6L)
  expect_match(lines[1], "(breakfast)", fixed = TRUE)
})

test_that("a count prints in full, never in scientific notation", {
  units <- tibble::tibble(..profile = 1:2, ..n = c(1e5, 1), ..wn = c(1e5, 1),
                          a = factor(c("x", "y")))
  tips <- interactive_tooltips(units, character(), "a", character())
  expect_match(tips$begin_text[1], "n=100000", fixed = TRUE)
})

test_that("a number in tooltip_vars_1lv prints its weighted mean", {
  vd <- md(fx_mca_wt(), fx_tea_wt(), tooltip_vars_1lv = age)$vars_data
  central <- vd$interactive_text[vd$lvs == "Central point"]
  mean <- stats::weighted.mean(fx_tea_wt()$age, fx_tea_wt()$w)
  expect_match(central, paste0("age (mean): ", format(round(mean, 1))), fixed = TRUE)
})

# --- profile tooltips ---------------------------------------------------------------------------

test_that("a profile tooltip lists the answers the profile is made of", {
  ind <- fx_pd_profiles()$ind_data
  expect_match(ind$interactive_text[1], "^<b>Answer profile n.1</b>\nn: [0-9]+\n")
  # One line per answer, those matching profiles_tooltip_discard left out: tea[1:6] answers are
  # "Not.x" or "x", so every profile shows between none and six of them.
  answers <- strsplit(sub(".*\n\n", "", ind$interactive_text), "\n")
  expect_true(all(lengths(answers) <= length(fx_active())))
  expect_true(any(lengths(answers) == length(fx_active())))
})

test_that("a clust profile tooltip names its cluster and its rank within it", {
  ind <- fx_pd_clust()$ind_data
  expect_match(ind$interactive_text[1], "^<b>Cluster: .+</b>\n<b>Answer profile n.1/[0-9]+</b>")
})

# --- golden ------------------------------------------------------------------------------------

test_that("the rendered tooltip text is stable", {
  # Golden, because the tooltip IS the package: any change to the crosstab, the colouring or the
  # padding is a user-visible change and should have to be accepted deliberately.
  txt <- tips(fx_pd_active())
  expect_snapshot(
    for (lv in c("breakfast breakfast", "breakfast Not.breakfast")) cat(lv, "\n", txt[[lv]],
                                                                         "\n\n", sep = "")
  )
})
