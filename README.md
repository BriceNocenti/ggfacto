
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggfacto

<!-- badges: start -->

[![R-CMD-check](https://github.com/BriceNocenti/ggfacto/workflows/R-CMD-check/badge.svg)](https://github.com/BriceNocenti/ggfacto/actions)
<!-- badges: end -->

Readable, complete and pretty graphs for correspondence analysis made
with [FactoMineR](http://factominer.free.fr/). Many can be rendered as
interactive html plots, showing useful informations at mouse hover. The
interest is not mainly visual but statistical : it helps the reader to
keep in mind the data contained in the crosstab or Burt table while
reading correspondance analysis, thus preventing overinterpretation.
Graphs are made with [ggplot2](https://ggplot2.tidyverse.org/), which
means that you can use the `+` syntax to manually add as many graphical
pieces you want, or change theme elements.

## Installation

You can install ggfacto from CRAN:

``` r
install.packages("ggfacto")
```

Or install the development version from github:

``` r
# install.packages("devtools")
devtools::install_github("BriceNocenti/ggfacto")
```

## Interactive plot from multiple correspondence analysis (MCA)

Make the MCA with FactoMineR :

``` r
library(ggfacto)

data(tea, package = "FactoMineR")
res.mca <- FactoMineR::MCA(tea, quanti.sup = 19, quali.sup = c(20:36), graph = FALSE)
```

Make the plot (as a ggplot2 object) :

``` r
graph_mca <- ggmca(res.mca, sup_vars = c("SPC", "age_Q")) %>%
```

Turn the plot interactive :

``` r
ggi(graph_mca)
```

![](readme_plot.png) If you don’t want interactive usage, you can use
`text_repel = TRUE` to avoid overlapping of text and obtain a more
readable image (be careful that, if the plot is overloaded, labels can
be far away from their original location) :

``` r
ggmca(res.mca, sup_vars = c("SPC", "age_Q"), ylim = c(NA, 1.2), text_repel = TRUE)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

### Concentration ellipses for each levels of a supplementary variable

``` r
ggmca(res.mca, sup_vars = "SPC", ylim = c(NA, 1.2), ellipses = 0.95, text_repel = TRUE, size_scale_max = 4)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

### Graph of profiles of answer for each levels of a supplementary variable (with median ellipses containing half the population)

``` r
ggmca(res.mca, sup_vars = "SPC", ylim = c(NA, 1.2), type = "facets", ellipses = 0.5, size_scale_max = 4)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

<!-- ## Table to help to interpret MCA -->
<!-- ``` {r} -->
<!-- mca_interpret(res.mca, 1:3) -->
<!-- ``` -->

## Simple correspondence analysis (CA)

Make the correspondence analysis :

``` r
tabs <- tabxplor::tab_plain(forcats::gss_cat, race, marital, df = TRUE)
res.ca <- FactoMineR::CA(tabs, graph = FALSE)
```

Interactive plot :

``` r
graph.ca <- ggca(res.ca,
                 title = "Race by marical : correspondence analysis",
                 tooltips = c("row", "col"))
ggi(graph.ca)
```

Image plot (with `text_repel` to avoid overlapping of labels) :

``` r
ggca(res.ca,
     title = "Race by marical : correspondence analysis",
     text_repel = TRUE, dist_labels = 0.02)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

## Personnalize plots

Step-by-step functions can be used to create a database with all the
necessary data, modify it, then use it to draw the plot :

``` r
library(dplyr)
library(ggplot2)

plot_data <- ggmca_data(res.mca, sup_vars = "SPC")

plot_data$sup_vars_coord <- plot_data$sup_vars_coord %>% 
  filter(!lvs %in% c("other worker", "non-worker"))

ggmca_plot(plot_data, ylim = c(NA, 1.2), text_repel = TRUE, size_scale_max = 4)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

The plot can always be modified using the ggplot2 `+` operator :

``` r
ggmca_plot(plot_data, ylim = c(NA, 1.2), size_scale_max = 4) +
  labs(title = "Multiple correspondence analysis") +
  theme(axis.line = element_line(linetype = "solid") )
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" /> You
can then pass to plot to ggi() to make it interactive

You can also set `use_theme = FALSE` to use you own ggplot2 theme :

``` r
ggmca_plot(plot_data, ylim = c(NA, 1.2), use_theme = FALSE) +
  theme_classic()
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />
