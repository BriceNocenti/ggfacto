# Describe Clusters with One Table: the Former Form

Deprecated. \`HCPC_tab(data, row_vars, clust, wt)\`, and \`clust_tab()\`
given a data frame first, are the former form of
[`clust_tab`](https://bricenocenti.github.io/ggfacto/reference/clust_tab.md),
which now takes the analysis first and reads the active variables, the
weights and the rows from it: \`clust_tab(res.mca, data, clust)\`.

## Usage

``` r
HCPC_tab(
  data,
  row_vars = character(),
  clust,
  wt,
  excl = NA,
  color = "difference",
  pct = "col",
  row_tot = "% of population",
  cleannames = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame.

- row_vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  The variables to describe the clusters with. Numeric ones become mean
  rows.

- clust:

  The variable with the clusters, as a bare name or a string, or the
  clusters themselves. Rows without a cluster are left out.

- wt:

  The weight variable. Leave empty for unweighted results.

- excl:

  The levels not to show, matched exactly by name; their individuals
  still count in the percentages. \`NA\`, the default, hides the missing
  values (and the levels named \`\<VAR\>.NA\`); \`excl = NULL\` shows
  every level.

- color:

  The colour measure, see
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.html).
  With \`"difference"\` (the default), percentages are coloured by their
  difference with the whole population, and means by their difference in
  standard deviations — but not both in one table, which a single ladder
  cannot grade: there, means stay uncoloured, and \`"ratio"\` colours
  every row.

- pct:

  \`"col"\` (default) reads each cluster as a distribution: of the
  people in this cluster, what percentage are in this level. \`"row"\`
  reads each level as a distribution across clusters.

- row_tot:

  The name of the row giving each cluster's share of the population.

- cleannames:

  Set to `FALSE` to keep the level and cluster names as they are, prefix
  numbers like `"1-"` and text in parentheses included.

- ...:

  Additional arguments to pass to
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.html).

## Value

A `tabxplor` table — see \[ggfacto_summary\] for how it prints.
