# Coordinates of the Individuals on the Axes of an Analysis

The coordinates of each individual on the axes of a principal component
analysis or of a multiple correspondence analysis, to write into the
data frame with
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
like
[`hierarchical_clust`](https://bricenocenti.github.io/ggfacto/reference/hierarchical_clust.md):

\`data \<- data \|\> mutate(axe1 = axis_coord(res, 1))\`

An analysis made with
[`multiple_correspondence_analysis`](https://bricenocenti.github.io/ggfacto/reference/multiple_correspondence_analysis.md)
is computed on the distinct answer profiles, so its \`\$ind\$coord\` has
one row per profile: \`axis_coord()\` gives each individual the
coordinate of its profile. In a correspondence analysis, each individual
takes the coordinate of its level.

## Usage

``` r
axis_coord(res, axes = 1, margin = "rows")
```

## Arguments

- res:

  An analysis made with
  [`multiple_correspondence_analysis`](https://bricenocenti.github.io/ggfacto/reference/multiple_correspondence_analysis.md),
  [`principal_component_analysis`](https://bricenocenti.github.io/ggfacto/reference/principal_component_analysis.md)
  or
  [`correspondence_analysis`](https://bricenocenti.github.io/ggfacto/reference/correspondence_analysis.md)
  (or with
  [`FactoMineR::MCA()`](https://rdrr.io/pkg/FactoMineR/man/MCA.html),
  `PCA()` or `CA()`, or
  [`GDAtools::speMCA()`](https://nicolas-robette.frama.io/GDAtools/reference/speMCA.html)
  or `csMCA()`).

- axes:

  The axes. Several axes give a data frame, which `mutate()` writes as
  several columns, named after \`axes\` when it has names (\`c(axe1 = 1,
  axe2 = 2)\`), else \`axis1\`, \`axis2\`...

- margin:

  For a correspondence analysis, the variable whose levels give the
  coordinates: \`"rows"\`, the default, or \`"columns"\`.

## Value

One value per row of the data frame: inside
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
of the data frame being written, with \`NA\` on the rows the analysis
did not use (when it was made on a subset of the population); outside,
of the data frame the analysis started from. For a correspondence
analysis outside `mutate()`, one value per level, named after it.

## Examples

``` r
data(tea, package = "FactoMineR")
res.mca <- multiple_correspondence_analysis(tea, 1:18)

tea <- tea |>
  dplyr::mutate(axe1 = axis_coord(res.mca, 1),
                axis_coord(res.mca, c(axe2 = 2, axe3 = 3)))

# How much of the first axis does age explain?
summary(stats::lm(axe1 ~ age_Q, data = tea))$r.squared
#> [1] 0.008006629
```
