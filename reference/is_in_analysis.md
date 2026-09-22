# Which Rows an Analysis Was Made On

\`TRUE\` for each row of the data frame the analysis was made on,
\`FALSE\` for the others: the rows filtered out (with the pipe or with
\`filter\`), those with a weight of 0, and the supplementary individuals
of a principal component analysis. Use it to describe the analysed
population:

\`data \|\> dplyr::filter(is_in_analysis(res)) \|\> tabxplor::tab(SEXE,
AGE)\`

## Usage

``` r
is_in_analysis(res)
```

## Arguments

- res:

  An analysis made with
  [`multiple_correspondence_analysis`](https://bricenocenti.github.io/ggfacto/reference/multiple_correspondence_analysis.md)
  or
  [`principal_component_analysis`](https://bricenocenti.github.io/ggfacto/reference/principal_component_analysis.md)
  (or with
  [`FactoMineR::MCA()`](https://rdrr.io/pkg/FactoMineR/man/MCA.html) or
  `PCA()`, or
  [`GDAtools::speMCA()`](https://nicolas-robette.frama.io/GDAtools/reference/speMCA.html)
  or `csMCA()`).

## Value

A logical vector, one value per row: inside
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
or
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
of the data frame being read; outside, of the data frame the analysis
started from.

## Examples

``` r
data(tea, package = "FactoMineR")
res.mca <- multiple_correspondence_analysis(tea, 1:18, filter = age < 30)

tea |>
  dplyr::filter(is_in_analysis(res.mca)) |>
  tabxplor::tab(sex, SPC)
#> <div class="tx-scrollbox"><table class="tabxplor-tab" data-quarto-disable-processing="true"><thead><tr><th class="tx-span" colspan="1"></th><th class="tx-span" colspan="7">SPC</th><th class="tx-span" colspan="1"></th></tr><tr><th class="tx-l tx-br tx-bl tx-rv" rowspan="2">sex</th><th class="tx-r tx-num">employee</th><th class="tx-r tx-num">middle</th><th class="tx-r tx-num">non-worker</th><th class="tx-r tx-num">other worker</th><th class="tx-r tx-num">senior</th><th class="tx-r tx-num">student</th><th class="tx-r tx-num">workman</th><th class="tx-r tx-num tx-br tx-bl tx-tot">Total</th></tr><tr><th class="tx-r tx-num tx-unit">&lt;n&gt;</th><th class="tx-r tx-num tx-unit"></th><th class="tx-r tx-num tx-unit"></th><th class="tx-r tx-num tx-unit"></th><th class="tx-r tx-num tx-unit"></th><th class="tx-r tx-num tx-unit"></th><th class="tx-r tx-num tx-unit"></th><th class="tx-r tx-num tx-br tx-bl tx-tot tx-unit">&lt;n&gt;</th></tr></thead><tbody><tr><td class="tx-l tx-br tx-bl tx-rv">F</td><td class="tx-r tx-num g2">16</td><td class="tx-r tx-num g2">1</td><td class="tx-r tx-num g2">6</td><td class="tx-r tx-num g2">1</td><td class="tx-r tx-num g2">2</td><td class="tx-r tx-num g2">51</td><td class="tx-r tx-num g2">2</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">79</td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">M</td><td class="tx-r tx-num g2">17</td><td class="tx-r tx-num g2">5</td><td class="tx-r tx-num g2">7</td><td class="tx-r tx-num g2">2</td><td class="tx-r tx-num g2">4</td><td class="tx-r tx-num g2">18</td><td class="tx-r tx-num g2">4</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">57</td></tr>
#> <tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total</td><td class="tx-r tx-num tx-b">33</td><td class="tx-r tx-num tx-b">6</td><td class="tx-r tx-num tx-b">13</td><td class="tx-r tx-num tx-b">3</td><td class="tx-r tx-num tx-b">6</td><td class="tx-r tx-num tx-b">69</td><td class="tx-r tx-num tx-b">6</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b">136</td></tr></tbody></table></div>
```
