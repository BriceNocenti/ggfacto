# ggfacto 0.4.0 (development version)

## Interactive graphs can be written to their own file

A `bookdown` book merges every chapter into one markdown file and hands it to a single `pandoc`
call. A widget embedded inline is a raw HTML block of several megabytes on one line, which is the
case that reader handles worst: one course book measured 30 GB of resident memory before being
killed. Setting

``` r
options(ggfacto.widget_dir = "auto")
```

makes every interactive graph write itself to `widget_<chunk label>.html` and put an `<iframe>` in
the document instead. `"auto"` follows the chunk's `fig.path`, so the files travel with the
document exactly like its figures do. See `?ggfacto_widget`.

* The chunk needs a label, since the label names the file. Unset (the default), the option changes
  nothing at all and the widget is embedded as before.
* The frame's aspect ratio is the graph's own --- the ratio of the analysis' axes, computed in R,
  not negotiated in JavaScript --- so an interactive plot keeps the scale it is interpreted with.
  For the 3D graphs, which have no such ratio, the chunk's `fig.width` and `fig.height` decide.
* `ggi()`, `ggpca_cor_circle()`, `ggmca_3d()` and `ggpca_3d()` all return widgets carrying the new
  `ggfacto_widget` class. Printing them interactively is unchanged.

## `ggi(savewidget = TRUE)` writes one standalone file

* It used to write two --- `Plot.html` plus a `Plot_widget/index.html` that had to travel beside
  it, framed by `widgetframe` and pym.js. It now writes a single self-contained page: a file that
  can simply be sent to someone.
* **Breaking:** `ggi(iframe = )` and `ggi(pixel_width = )` are removed, along with the
  `widgetframe` dependency. They existed only to build that frame, and their own documentation
  warned they produced a blank graph under `rmarkdown`. Use `ggfacto.widget_dir` for documents.

## Requires tabxplor 2.0.0

Every table is now built with `tabxplor::tab()` and the 2.0.0 vocabulary.

* `HCPC_tab()` is built the way round it is read --- the variables' levels down the page, the
  clusters across it --- instead of being built the other way round and transposed by hand. Numeric
  variables still become mean rows. The duplicated `n` row is gone, and the summary rows (`%
  of population`, `n`) are declared as display rows, so they are no longer coloured or counted as
  data.
* `HCPC_tab(color = )` takes the tabxplor 2.0.0 measure names and defaults to `"difference"`. The
  mean rows of a table that also holds percentages stay uncoloured, a difference of means and a
  difference of percentages having no ladder in common; `color = "ratio"` colours every row.
* `pca_interpret()` colours a coordinate by its SIZE (the standardized-difference ladder) instead of
  by its sign alone.
* `ggca()` and the README now build the correspondence-analysis input with
  `as.matrix(tabxplor::tab(data, row_var, col_var))`, which drops the totals for you.

## A lighter dependency tree

Installing ggfacto with its `Suggests` now pulls **133 packages instead of 164, 204 MB instead of
244** --- 31 fewer packages, four fewer of them compiled from source. Nothing about the API
changes.

* `finalfit` and `gridExtra` are gone. They were only ever used by `pers_or_plot()`, which has been
  removed --- regression tables belong to tabxplor now. This is the bulk of the saving: 27 packages.
* `ggforce` is gone. It drew one circle, the PCA correlation circle, which is now a `geom_path()`.
  As a side effect that circle is drawn once instead of once per row of the plot's data.
* `stringr` and `stringi` are gone, replaced by base-R helpers that keep stringr's semantics
  (`NA` propagation, padding width and fill) rather than base R's.
* `scales`, `stats` and `grDevices` move from `Suggests` to `Imports`, where their use always
  belonged; `grid` was used but never declared. `ggplot2` now requires 3.4.0 or later, and R 4.1.0
  or later --- the package already used the base pipe, which R 4.0 does not have.

## Deprecations

* Piping with `%>%` is deprecated. ggfacto uses the base pipe `|>` throughout, and re-exports
  `%>%` only for backward compatibility; the re-export, and the magrittr dependency with it, will
  be removed in a future release. Use `|>`.

## Bug corrections

* `ggmca(active_tables = )` / `ggmca(sup_vars = )` gave every level an empty tooltip.
* A level whose name is also a variable's name (`breakfast` of `breakfast`) lost its tooltip.
* The `Frequency` line of a tooltip could read above 100 %: it was divided by the last row of the
  last table instead of by the population.

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
