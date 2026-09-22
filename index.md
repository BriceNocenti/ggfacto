# ggfacto

`ggfacto` draws readable, interactive graphs of the principal component,
correspondence and multiple correspondence analyses of
[FactoMineR](http://factominer.free.fr/). Its point is less visual than
statistical: hover over a point, and the graph shows the crosstables
behind it, each deviation from the mean coloured, so that the geometry
is always read against the data it summarises. The graphs are `ggplot`
objects, to be extended with `+`.

`ggfacto` is, for the most part, a wrapper around `FactoMineR`, which
computes the analyses — our thanks to its authors, François Husson,
Julie Josse, Sébastien Lê and Jérémy Mazet. Its clustering reproduces
[`FactoMineR::HCPC()`](https://rdrr.io/pkg/FactoMineR/man/HCPC.html),
and its tables are built with
[tabxplor](https://bricenocenti.github.io/tabxplor/).

## Installation

``` r

install.packages("ggfacto")
```

## A multiple correspondence analysis

`tea`, a survey shipped with `FactoMineR`, asked 300 people 18 questions
about how they drink tea. The analysis, then the interpretation of its
first two axes, the eigenvalues underneath:

``` r

data(tea, package = "FactoMineR")

mca <- multiple_correspondence_analysis(tea, 1:18)
interpret(mca, axes = 1:2)
```

[TABLE]

[TABLE]

The clusters, added to the data frame, then the interactive graph. Hover
over a category to see its crosstables with the other questions, and
over a grey point to see a response pattern:

``` r

tea <- tea |> dplyr::mutate(clusters = hierarchical_clust(mca, ncp = 3))

ggfacto(mca, tea, clust = clusters, interactive = TRUE)
```

## Learn more

The [guide](https://bricenocenti.github.io/ggfacto/articles/ggfacto.md)
(*[en
français](https://bricenocenti.github.io/ggfacto/articles/ggfacto-fr.md)*)
walks through the three analyses, their tables and graphs, and
clustering.
