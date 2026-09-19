# ggfacto

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/ggfacto)](https://CRAN.R-project.org/package=ggfacto)
[![R-CMD-check](https://github.com/BriceNocenti/ggfacto/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BriceNocenti/ggfacto/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`ggfacto` draws readable, interactive graphs of the principal component, correspondence and multiple correspondence analyses of [FactoMineR](http://factominer.free.fr/). Hover over a point, and the graph shows the crosstables behind it, each deviation from the mean coloured: the geometry is always read against the data it summarises. The graphs are `ggplot` objects, to be extended with `+`.

The three analyses share one workflow: the analysis, `interpret()` for its axes, `ggfacto()` for its graph, and `hierarchical_clust()` for its clusters.

```r
install.packages("ggfacto")

library(ggfacto)
data(tea, package = "FactoMineR")
mca <- multiple_correspondence_analysis(tea, 1:18)
interpret(mca)
ggfacto(mca, tea, sup_vars = c(sex, SPC), interactive = TRUE)
```

See the website, with the interactive graphs: **<https://bricenocenti.github.io/ggfacto/>**, and its guide, [in English](https://bricenocenti.github.io/ggfacto/articles/ggfacto.html) or [en français](https://bricenocenti.github.io/ggfacto/articles/ggfacto-fr.html).
