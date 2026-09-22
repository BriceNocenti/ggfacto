# Changelog

## ggfacto 0.4.0

One workflow for the three analyses: the analysis,
[`interpret()`](https://bricenocenti.github.io/ggfacto/reference/interpret.md)
for its axes,
[`ggfacto()`](https://bricenocenti.github.io/ggfacto/reference/ggfacto.md)
for its graph,
[`hierarchical_clust()`](https://bricenocenti.github.io/ggfacto/reference/hierarchical_clust.md)
for its clusters. See the guide:
<https://bricenocenti.github.io/ggfacto/articles/ggfacto.html>.

### New functions

- [`multiple_correspondence_analysis()`](https://bricenocenti.github.io/ggfacto/reference/multiple_correspondence_analysis.md),
  [`principal_component_analysis()`](https://bricenocenti.github.io/ggfacto/reference/principal_component_analysis.md),
  [`correspondence_analysis()`](https://bricenocenti.github.io/ggfacto/reference/correspondence_analysis.md)
  (from a
  [`tabxplor::tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.html)):
  the analyses, weights and variables selected as in `tab()`.
- [`interpret()`](https://bricenocenti.github.io/ggfacto/reference/interpret.md):
  the table of an analysis’ axes, with its eigenvalues below;
  [`eigenvalues()`](https://bricenocenti.github.io/ggfacto/reference/eigenvalues.md)
  alone.
- [`ggfacto()`](https://bricenocenti.github.io/ggfacto/reference/ggfacto.md):
  the graph of any of the three analyses;
  [`ggpca()`](https://bricenocenti.github.io/ggfacto/reference/ggpca.md),
  the individuals of a PCA and its biplot.
- [`hierarchical_clust()`](https://bricenocenti.github.io/ggfacto/reference/hierarchical_clust.md):
  `HCPC()`’s clusters, in linear memory, written into the data with
  `mutate()`;
  [`clust_tab()`](https://bricenocenti.github.io/ggfacto/reference/clust_tab.md)
  describes them.
- [`axis_coord()`](https://bricenocenti.github.io/ggfacto/reference/axis_coord.md),
  [`is_in_analysis()`](https://bricenocenti.github.io/ggfacto/reference/is_in_analysis.md):
  an analysis’ coordinates, and its population, back into the data.
  [`name_axes()`](https://bricenocenti.github.io/ggfacto/reference/name_axes.md)
  names the axes on graphs and tables.

### New arguments and changes

- `excl =` takes exact level names; missing answers become `<VAR>.NA`
  levels, excluded by default (specific MCA).
- `filter =` analyses a subset; the whole data frame is then passed to
  every later function.
- `clust =`, `names =`, `interactive =`, `lang =`;
  `options(ggfacto.widget_dir)` writes widgets to their own file in
  documents.
- `profiles = TRUE` and `active_tables = "active"` are the defaults: the
  crosstables of the Burt table are in the tooltips.
- The MCA is fitted on the distinct answer profiles (same results, much
  faster);
  [`MCA2()`](https://bricenocenti.github.io/ggfacto/reference/multiple_correspondence_analysis.md)
  keeps the 0.3.2 fit on individuals.
- A missing value of a PCA sits at its variable’s weighted mean.
- Requires R \>= 4.3, ggplot2 \>= 4.0 and tabxplor \>= 2.0.1.

### Deprecations and removals

- [`HCPC_tab()`](https://bricenocenti.github.io/ggfacto/reference/HCPC_tab.md)
  (use
  [`clust_tab()`](https://bricenocenti.github.io/ggfacto/reference/clust_tab.md)),
  the `cah` and `dat` arguments (use `clust` and `data`), and `%>%` are
  soft-deprecated.
- `pers_or_plot()` is removed; `ggi(iframe =, pixel_width =)` are
  ignored.

### Bug corrections

- The `Frequency` line of a tooltip could exceed 100 %.
- A question with levels `y`/`n` disappeared from the graphs and tables.
- The ellipses of a weighted analysis ignored the weights.

## ggfacto 0.3.2

CRAN release: 2024-10-02

### Bug correction

- Corrected to work with dependency package `tabxplor::` version 1.2.1

## ggfacto 0.3.1

CRAN release: 2024-08-30

### Bug correction

- [`HCPC_tab()`](https://bricenocenti.github.io/ggfacto/reference/HCPC_tab.md)
  was not working well with numeric variables.

## ggfacto 0.3.0

CRAN release: 2024-03-08

### Added :

- New fonction :
  [`HCPC_tab()`](https://bricenocenti.github.io/ggfacto/reference/HCPC_tab.md)  
- New fonction :
  [`ggmca_initial_dims()`](https://bricenocenti.github.io/ggfacto/reference/ggmca_initial_dims.md)
- New fonction :
  [`ggmca_with_base_ref()`](https://bricenocenti.github.io/ggfacto/reference/ggmca_with_base_ref.md)
- New fonction :
  [`ggmca_3d()`](https://bricenocenti.github.io/ggfacto/reference/ggmca_3d.md)  
- New fonction :
  [`PCA2()`](https://bricenocenti.github.io/ggfacto/reference/principal_component_analysis.md)  
- New fonction :
  [`pca_interpret()`](https://bricenocenti.github.io/ggfacto/reference/interpret.md)  
- New fonction :
  [`ggpca_cor_circle()`](https://bricenocenti.github.io/ggfacto/reference/ggpca_cor_circle.md)  
- New fonction :
  [`ggpca_3d()`](https://bricenocenti.github.io/ggfacto/reference/ggpca_3d.md)  
- New fonction :
  [`mean_sd_tab()`](https://bricenocenti.github.io/ggfacto/reference/mean_sd_tab.md)

### Bug correction

- [`ggmca()`](https://bricenocenti.github.io/ggfacto/reference/ggmca.md)
  can now properly color profiles of answer with `cah` (HCPC clusters)
  levels.

## ggfacto 0.2.3

CRAN release: 2024-01-25

### Added :

- New argument `reverse_axes` in
  [`ggmca()`](https://bricenocenti.github.io/ggfacto/reference/ggmca.md)
  to invert left and right or up and down.

### Bug correction

- `mca_interpret` was not working with `MCA(excl = )`
- in `ggmca`, profiles of answers were plain wrong (because of the
  unanticipated results of a change in code)

## ggfacto 0.2.2

CRAN release: 2021-10-22

- First public version of ggfacto.
