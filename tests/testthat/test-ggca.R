# PURPOSE: Lock the correspondence analysis of a table -- its supplementary variables, read from a
#   multi-variable tab() -- and its graph, ggca(), on the shared plot model.
# ROLE: A CA's data is its table, so its supplementary variables ride it; its tooltips are each
#   level's profile over the other margin, computed on the table's cells with tab()'s arithmetic.
# KEY CONSTRAINTS:
#   - Supplementary variables never move the analysis: the eigenvalues and the active coordinates
#     are those of the active table alone.
#   - The tooltip cells are tab()'s, cell for cell (expect_ca_cells_equal_tab()), in both passes.
#   - The former ggca() arguments keep their places, each saying once what replaced it.
# See: CLAUDE.md section ggfacto architecture > The tooltip is the package.

# --- the analysis -------------------------------------------------------------------------------

test_that("supplementary variables ride the table, and do not move the analysis", {
  ac     <- fx_ca_multi()
  single <- correspondence_analysis(tabxplor::tab(fx_gss_wt(), relig, partyid, wt = w))
  expect_equal(ac$eig, single$eig)
  expect_equal(ac$row$coord, single$row$coord)
  expect_equal(ac$col$coord, single$col$coord)
  expect_identical(names(dimnames(ac$call$X)), c("relig", "partyid"))
  expect_identical(unique(ac$source$rows$var), c("relig", "marital"))
  expect_identical(unique(ac$source$cols$var), c("partyid", "race"))
  expect_identical(CA2, correspondence_analysis)
})

test_that("a supplementary level sits where FactoMineR puts its profile", {
  d  <- fx_gss_wt()
  A  <- as.matrix(tabxplor::tab(d, relig, partyid, wt = w))
  M  <- as.matrix(tabxplor::tab(d, marital, partyid, wt = w))
  R  <- as.matrix(tabxplor::tab(d, relig, race, wt = w))
  fm_rows <- FactoMineR::CA(rbind(A, M), row.sup = nrow(A) + seq_len(nrow(M)), ncp = Inf,
                            graph = FALSE)
  fm_cols <- FactoMineR::CA(cbind(A, R), col.sup = ncol(A) + seq_len(ncol(R)), ncp = Inf,
                            graph = FALSE)
  expect_equal(unname(fx_ca_multi()$row.sup$coord), unname(fm_rows$row.sup$coord),
               tolerance = 1e-10)
  expect_equal(unname(fx_ca_multi()$col.sup$coord), unname(fm_cols$col.sup$coord),
               tolerance = 1e-10)
})

test_that("a table with a third variable is refused, and a raw fit's supplements are drawn", {
  t3 <- tabxplor::tab(fx_gss_wt(), relig, partyid, tab_vars = race)
  expect_error(correspondence_analysis(t3), "without `tab_vars`")
  X   <- as.matrix(tabxplor::tab(fx_gss_wt(), relig, partyid))
  raw <- FactoMineR::CA(X, row.sup = 1, col.sup = 2, graph = FALSE)
  vd  <- ggca(raw, get_data = TRUE)$vars_data
  expect_identical(sum(vd$role == "sup"), 2L)
  expect_true(all(c("Supplementary rows", "Supplementary columns") %in% vd$vars))
})

# --- the tooltips are tab()'s -------------------------------------------------------------------

# The profile of each level of `rows` over `cols`, built on the table's cells, against a direct
# tab(rows, cols, pct = "row"): counts exactly, weighted counts, percentages and differences to
# 1e-12, colours and formatted text identical.
expect_ca_cells_equal_tab <- function(ac, data, rows, cols) {
  m     <- ca_model(ac)
  units <- ca_units(m, m$rows$lvs, m$cols$lvs)
  s     <- stack_levels(units, rows)
  cells <- crosstab_cells(units, s, cols)
  direct <- tabxplor::tab(data, !!rlang::sym(rows), !!rlang::sym(cols), wt = w, pct = "row",
                          color = "difference")
  direct <- direct[as.character(direct[[1]]) != "Total", ]
  fmts   <- names(direct)[vapply(direct, tabxplor::is_fmt, logical(1))]
  for (j in seq_len(nlevels(units[[cols]]))) {
    new <- cells_fmt(cells, j, cols)[seq_len(s$G)]
    old <- direct[[fmts[j]]]
    expect_identical(vctrs::field(new, "n"), as.integer(vctrs::field(old, "n")))
    for (f in c("wn", "pct", "diff")) {
      expect_equal(vctrs::field(new, f), vctrs::field(old, f), tolerance = 1e-12)
    }
    expect_identical(tabxplor::fmt_get_color_code(new), tabxplor::fmt_get_color_code(old))
    expect_identical(format(new), format(old))
  }
}

test_that("a row level's profile is tab()'s, over the active and the supplementary columns", {
  expect_ca_cells_equal_tab(fx_ca_multi(), fx_gss_wt(), "relig", "partyid")
  expect_ca_cells_equal_tab(fx_ca_multi(), fx_gss_wt(), "relig", "race")
  expect_ca_cells_equal_tab(fx_ca_multi(), fx_gss_wt(), "marital", "partyid")
})

test_that("a column level's profile is tab()'s, its variable in rows", {
  expect_ca_cells_equal_tab(fx_ca_multi(), fx_gss_wt(), "partyid", "relig")
  expect_ca_cells_equal_tab(fx_ca_multi(), fx_gss_wt(), "race", "relig")
})

test_that("a level's frequency is its share of its own variable, never counted twice", {
  vd  <- ggca(fx_ca_multi(), get_data = TRUE)$vars_data
  hit <- regmatches(vd$interactive_text, regexpr("Frequency \\(n=[0-9]+\\): *[0-9]+%",
                                                  vd$interactive_text))
  pct <- as.numeric(sub(".*: *([0-9]+)%$", "\\1", hit))
  for (v in c("relig", "partyid", "marital", "race")) {
    expect_equal(sum(pct[vd$vars == v]), 100, tolerance = 3)
  }
})

test_that("a CA's tooltip is stable", {
  vd <- ggca(fx_ca_multi(), get_data = TRUE, lang = "en")
  expect_snapshot(cat(vd$vars_data$interactive_text[vd$vars_data$lvs == "Catholic"], "\n\n",
                      vd$mean_point_data$interactive_text, sep = ""))
})

# --- clusters ----------------------------------------------------------------------------------

test_that("a margin's clusters colour its levels, and sit at the projection of their merger", {
  ac <- fx_ca_multi()
  cl <- hierarchical_clust(ac, ncp = 2, nb_clust = 3, tree = FALSE)
  vd <- ggca(ac, clust = cl, get_data = TRUE)$vars_data
  labels <- vd[vd$role == "clust", ]
  expect_identical(nrow(labels), 3L)
  for (k in seq_len(nrow(labels))) {
    members <- vd$vars == "relig" &
      vd$lvs %in% names(cl)[as.character(cl) == as.character(labels$lvs[k])]
    expect_true(all(vd$id[members] == labels$id[k]))
    expect_true(all(vd$color_group[members] == labels$color_group[k]))
  }
  A <- as.matrix(tabxplor::tab(fx_gss_wt(), relig, partyid, wt = w))
  merged <- rowsum(A, as.character(cl[rownames(A)]))
  fm <- FactoMineR::CA(rbind(A, merged), row.sup = nrow(A) + seq_len(nrow(merged)), graph = FALSE)
  got <- as.matrix(labels[, c("Dim 1", "Dim 2")])
  expect_equal(unname(got), unname(fm$row.sup$coord[as.character(labels$lvs), 1:2]),
               tolerance = 1e-10)
})

test_that("the clusters of the columns work too, and an unnamed factor is refused", {
  ac <- fx_ca_multi()
  cl <- hierarchical_clust(ac, ncp = 2, nb_clust = 3, margin = "columns", tree = FALSE)
  vd <- ggca(ac, clust = cl, get_data = TRUE)$vars_data
  expect_identical(sum(vd$role == "clust"), 3L)
  expect_error(ggca(ac, clust = unname(cl)), "named")
})

# --- the graph ---------------------------------------------------------------------------------

test_that("every type builds, repelled labels keep their tooltips, and a typo is explained", {
  local_null_device()
  for (type in c("points", "text", "labels")) {
    b <- ggplot2::ggplot_build(ggca(fx_ca_multi(), type = type))
    expect_true(any(vapply(b$plot$layers, function(l) inherits(l$geom, "GeomInteractiveText") ||
                             inherits(l$geom, "GeomInteractiveLabel") ||
                             inherits(l$geom, "GeomInteractiveTextRepel") ||
                             inherits(l$geom, "GeomInteractiveLabelRepel"), logical(1))))
    expect_false(any(vapply(b$plot$layers, function(l) {
      class(l$geom)[1] %in% c("GeomTextRepel", "GeomLabelRepel")
    }, logical(1))))
  }
  expect_error(ggca(fx_ca_multi(), type = "typo"), "should be one of")
})

test_that("show_sup, uppercase and tooltips do what they say", {
  no_sup <- ggca(fx_ca_multi(), show_sup = FALSE, get_data = TRUE)$vars_data
  expect_false(any(no_sup$role == "sup"))
  vd <- ggca(fx_ca_multi(), get_data = TRUE)$vars_data
  expect_true("STRONG DEMOCRAT" %in% vd$lvs)
  expect_true("Protestant" %in% vd$lvs)
  rows_only <- ggca(fx_ca_multi(), tooltips = "row", get_data = TRUE)$vars_data
  expect_false(any(grepl("relig:", rows_only$interactive_text[rows_only$vars == "partyid"])))
})

test_that("the former arguments keep their places, each saying once what replaced it", {
  e <- ggfacto:::deprecated_args_warned
  rm(list = ls(envir = e), envir = e)
  local_null_device()
  p <- ggca(fx_ca_multi(), c(1, 3))
  expect_match(p$labels$y, "^Axe 3")
  expect_warning(ggca(fx_ca_multi(), rowcolor_numbers = 0), "color_groups")
  expect_warning(vd <- ggca(fx_ca_multi(), filter = "Protestant", get_data = TRUE)$vars_data,
                 "discard_levels")
  expect_false("Protestant" %in% vd$lvs)
  expect_warning(ggca(fx_ca_multi(), rowtips_subtitle = "x"), "deprecated")
  expect_no_warning(ggca(fx_ca_multi(), rowtips_subtitle = "x"))
})
