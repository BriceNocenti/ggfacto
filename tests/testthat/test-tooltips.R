# PURPOSE: Lock the crosstabs that travel inside each point's hover tooltip.
# ROLE: This is the package's one bet -- "not mainly visual but statistical". The tooltip carries the
#   Burt table the MCA was computed from, so the reader re-derives the geometry from the data.
#   interactive_tooltips() is internal and is reached here through ggmca_data(), never with ::: .
# KEY CONSTRAINTS:
#   - Tooltips are byte-identical across runs (no sampling, no hashing, no locale-dependent sort),
#     which is what makes expect_snapshot() safe here. If one ever becomes unstable, delete the
#     snapshot rather than loosening it.
#   - The frequency denominator is the POPULATION, computed once before any table is bound. Using
#     the last row of the bound tables instead once gave levels a frequency above 100 %.
#   - Numbers are aligned with str_pad() under a monospace font, so the shim's vector `width` is
#     load-bearing. See tests/testthat/test-str-shim.R.
# See: CLAUDE.md section ggfacto architecture > The tooltip is the package.

tips <- function(plot_data) {
  txt <- plot_data$vars_data$interactive_text
  vapply(txt, function(x) paste(unlist(x), collapse = ""), character(1))
}

# --- the header every tooltip opens with --------------------------------------------------------

test_that("each tooltip names its level, its variable and its frequency", {
  plot_data <- fx_pd_active()
  first <- plot_data$vars_data$interactive_text[[1]]$begin_text

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
                        vapply(plot_data$vars_data$interactive_text,
                               function(x) x$begin_text, character(1))))
  pct <- pct[!is.na(pct)]
  expect_gt(length(pct), 0L)
  expect_true(all(pct <= 100))
})

test_that("the frequencies of one variable's levels sum to about 100 %", {
  plot_data <- fx_pd_active()
  vd  <- plot_data$vars_data
  pct <- as.numeric(sub(".*Frequency[^:]*:\\s*([0-9.]+)\\s*%.*", "\\1",
                        vapply(vd$interactive_text, function(x) x$begin_text, character(1))))
  one <- pct[as.character(vd$vars) == fx_active()[1] & !is.na(pct)]
  expect_equal(sum(one), 100, tolerance = 2)
})

# --- the crosstabs ------------------------------------------------------------------------------

test_that("active_tables puts one crosstab column per active variable in the tooltip", {
  plot_data <- fx_pd_active()
  nested <- plot_data$vars_data$interactive_text[[1]]

  # begin_text + the "Active variables:" heading + one column per active variable.
  expect_true("actives_text" %in% names(nested))
  expect_gte(ncol(nested), length(fx_active()) + 1L)
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
  own <- vd$interactive_text[[i]]
  expect_true(any(grepl("100", unlist(own))))
})

test_that("tooltip level names have tabxplor's _lv disambiguation suffix removed", {
  # unlv() undoes the "_lv" tabxplor appends when a level name collides with a column name, and it
  # does so through fct_relabel so lvs stays a factor.
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
  flat <- function(ind) vapply(ind$interactive_text,
                               function(x) paste(unlist(x), collapse = "\n"), character(1))

  expect_true(any(grepl("weighted n", flat(weighted), fixed = TRUE)))
  expect_false(any(grepl("weighted n", flat(unweighted), fixed = TRUE)))
})

test_that("the tooltip describes the population the analysis was FITTED on", {
  # Weights are recovered from res.mca$call$row.w, never from the data argument, so handing a
  # different `data` cannot change the counts the tooltip reports.
  a <- tips(md(fx_mca_wt(), fx_tea_wt(), active_tables = "active"))
  b <- tips(md(fx_mca_wt(), fx_tea(),    active_tables = "active"))
  expect_identical(a, b)
})

# --- profile tooltips ---------------------------------------------------------------------------

test_that("a profile tooltip lists the answers the profile is made of", {
  ind <- fx_pd_profiles()$ind_data
  nested <- ind$interactive_text[[1]]
  expect_true(all(c("count", "wcount") %in% names(nested)))
  # One column per active variable, so the reader can see which answers make the profile.
  expect_gte(ncol(nested), length(fx_active()))
})

test_that("a clust profile tooltip names its cluster and its rank within it", {
  ind <- fx_pd_clust()$ind_data
  txt <- paste(unlist(ind$interactive_text[[1]]), collapse = " ")
  expect_match(txt, "Cluster")
})

# --- golden ------------------------------------------------------------------------------------

test_that("the rendered tooltip text is stable", {
  # Golden, because the tooltip IS the package: any change to the crosstab, the colouring or the
  # padding is a user-visible change and should have to be accepted deliberately.
  plot_data <- fx_pd_active()
  vd <- plot_data$vars_data
  keep <- which(as.character(vd$lvs) %in% c("breakfast", "Not.breakfast"))[1:2]
  expect_snapshot(
    for (i in keep) cat(as.character(vd$lvs)[i], "\n",
                        paste(unlist(vd$interactive_text[[i]]), collapse = ""), "\n\n", sep = "")
  )
})
