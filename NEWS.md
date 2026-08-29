# ggfacto 0.4.0 (development version)

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
