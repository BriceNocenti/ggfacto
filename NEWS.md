# ggfacto (development version)

# ggfacto 0.4.0

One workflow for the three analyses: the analysis, `interpret()` for its axes, `ggfacto()` for its graph, `hierarchical_clust()` for its clusters. See the guide: <https://bricenocenti.github.io/ggfacto/articles/ggfacto.html>.

## New functions

* `multiple_correspondence_analysis()`, `principal_component_analysis()`, `correspondence_analysis()` (from a `tabxplor::tab()`): the analyses, weights and variables selected as in `tab()`.
* `interpret()`: the table of an analysis' axes, with its eigenvalues below; `eigenvalues()` alone.
* `ggfacto()`: the graph of any of the three analyses; `ggpca()`, the individuals of a PCA and its biplot.
* `hierarchical_clust()`: `HCPC()`'s clusters, in linear memory, written into the data with `mutate()`; `clust_tab()` describes them.
* `axis_coord()`, `is_in_analysis()`: an analysis' coordinates, and its population, back into the data. `name_axes()` names the axes on graphs and tables.

## New arguments and changes

* `excl =` takes exact level names; missing answers become `<VAR>.NA` levels, excluded by default (specific MCA).
* `filter =` analyses a subset; the whole data frame is then passed to every later function.
* `clust =`, `names =`, `interactive =`, `lang =`; `options(ggfacto.widget_dir)` writes widgets to their own file in documents.
* `profiles = TRUE` and `active_tables = "active"` are the defaults: the crosstables of the Burt table are in the tooltips.
* The MCA is fitted on the distinct answer profiles (same results, much faster); `MCA2()` keeps the 0.3.2 fit on individuals.
* A missing value of a PCA sits at its variable's weighted mean.
* Requires R >= 4.3, ggplot2 >= 4.0 and tabxplor >= 2.0.1.

## Deprecations and removals

* `HCPC_tab()` (use `clust_tab()`), the `cah` and `dat` arguments (use `clust` and `data`), and `%>%` are soft-deprecated.
* `pers_or_plot()` is removed; `ggi(iframe =, pixel_width =)` are ignored.

## Bug corrections

* The `Frequency` line of a tooltip could exceed 100 %.
* A question with levels `y`/`n` disappeared from the graphs and tables.
* The ellipses of a weighted analysis ignored the weights.

# ggfacto 0.3.2

## Bug correction
* Corrected to work with dependency package `tabxplor::` version 1.2.1


# ggfacto 0.3.1

## Bug correction
* `HCPC_tab()` was not working well with numeric variables.


# ggfacto 0.3.0

## Added : 
* New fonction : `HCPC_tab()`           
* New fonction : `ggmca_initial_dims()` 
* New fonction : `ggmca_with_base_ref()`
* New fonction : `ggmca_3d()`           
* New fonction : `PCA2()`      
* New fonction : `pca_interpret()`      
* New fonction : `ggpca_cor_circle()`   
* New fonction : `ggpca_3d()`           
* New fonction : `mean_sd_tab()`

## Bug correction
* `ggmca()` can now properly color profiles of answer with `cah` (HCPC clusters) levels.


# ggfacto 0.2.3

## Added : 
* New argument `reverse_axes` in `ggmca()` to invert left and right or up and down. 

## Bug correction
* `mca_interpret` was not working with `MCA(excl = )`
* in `ggmca`, profiles of answers were plain wrong (because of the unanticipated results of a change in code)


# ggfacto 0.2.2

* First public version of ggfacto. 
