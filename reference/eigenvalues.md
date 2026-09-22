# The Eigenvalues of an Analysis

The table of the eigenvalues of the axes, the one
[`interpret`](https://bricenocenti.github.io/ggfacto/reference/interpret.md)
prints under its table: the variance of each axis, its percentage and
the cumulated percentage, and for a multiple correspondence analysis
Benzecri's modified rate, which corrects the raw percentages. It is read
to choose how many axes to interpret.

## Usage

``` r
eigenvalues(res, n_axes = 8L, color = TRUE, lang = NULL)
```

## Arguments

- res:

  An analysis made with
  [`multiple_correspondence_analysis`](https://bricenocenti.github.io/ggfacto/reference/multiple_correspondence_analysis.md),
  [`correspondence_analysis`](https://bricenocenti.github.io/ggfacto/reference/correspondence_analysis.md)
  or
  [`principal_component_analysis`](https://bricenocenti.github.io/ggfacto/reference/principal_component_analysis.md)
  (or with
  [`FactoMineR::MCA()`](https://rdrr.io/pkg/FactoMineR/man/MCA.html),
  `CA()` or `PCA()`, or
  [`GDAtools::speMCA()`](https://nicolas-robette.frama.io/GDAtools/reference/speMCA.html)
  or `csMCA()`).

- n_axes:

  How many axes to print. When some are left out, a last row states how
  many the cloud has.

- color:

  Set to `FALSE` to draw no data bar behind the percentages.

- lang:

  `NULL` (the session's language), `"en"` or `"fr"`.

## Value

A tabxplor table, printed as `options(tabxplor.print)` says.

## See also

\[interpret()\], \[benzecri_mrv()\].

## Examples

``` r
data(tea, package = "FactoMineR")
res.mca <- multiple_correspondence_analysis(tea, 1:18)
eigenvalues(res.mca)
#> <div class="tx-scrollbox"><table class="tabxplor-tab" data-quarto-disable-processing="true"><thead><tr><th class="tx-span" colspan="1"></th><th class="tx-span" colspan="3">Variance</th><th class="tx-span" colspan="2">Benzecri</th></tr><tr><th class="tx-l tx-br tx-bl tx-rv" rowspan="2">Axe</th><th class="tx-r tx-num">eigenvalue</th><th class="tx-r tx-num">% variance</th><th class="tx-r tx-num tx-br">cumul.</th><th class="tx-r tx-num">Benzecri's<br>modified rate</th><th class="tx-r tx-num tx-br">cumul. mod.</th></tr><tr><th class="tx-r tx-num tx-unit">&lt;var&gt;</th><th class="tx-r tx-num tx-unit">&lt;col%&gt;</th><th class="tx-r tx-num tx-br tx-unit"></th><th class="tx-r tx-num tx-unit">&lt;col%&gt;</th><th class="tx-r tx-num tx-br tx-unit"></th></tr></thead><tbody><tr><td class="tx-l tx-br tx-bl tx-rv">Axe 1</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="sd: 0.385 ; n: 300">0.148</td><td class="tx-r tx-num g2 tx-bar tx-bar-on" style="--tx-bar:100%" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">9.9%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">9.9%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">55.4%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">55.4%</td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Axe 2</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="sd: 0.349 ; n: 300">0.122</td><td class="tx-r tx-num g2 tx-bar tx-bar-on" style="--tx-bar:82%" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">8.1%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">18.0%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">28.1%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">83.4%</td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Axe 3</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="sd: 0.300 ; n: 300">0.090</td><td class="tx-r tx-num g2 tx-bar tx-bar-on" style="--tx-bar:60.7%" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">6.0%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">24.0%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">7.6%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">91.1%</td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Axe 4</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="sd: 0.279 ; n: 300">0.078</td><td class="tx-r tx-num g2 tx-bar tx-bar-on" style="--tx-bar:52.6%" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">5.2%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">29.2%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">3.3%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">94.3%</td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Axe 5</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="sd: 0.272 ; n: 300">0.074</td><td class="tx-r tx-num g2 tx-bar tx-bar-on" style="--tx-bar:49.7%" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">4.9%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">34.1%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">2.1%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">96.5%</td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Axe 6</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="sd: 0.267 ; n: 300">0.071</td><td class="tx-r tx-num g2 tx-bar tx-bar-on" style="--tx-bar:48.1%" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">4.8%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">38.9%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">1.6%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">98.1%</td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Axe 7</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="sd: 0.260 ; n: 300">0.068</td><td class="tx-r tx-num g2 tx-bar tx-bar-on" style="--tx-bar:45.7%" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">4.5%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">43.4%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">1.0%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">99.1%</td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">Axe 8</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="sd: 0.256 ; n: 300">0.065</td><td class="tx-r tx-num g2 tx-bar tx-bar-on" style="--tx-bar:44.1%" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">4.4%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">47.7%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">0.6%</td><td class="tx-r tx-num tx-br g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">99.7%</td></tr>
#> <tr class="tx-bt"><td class="tx-l tx-br tx-bl tx-rv">... of 27</td><td class="tx-r tx-num">...</td><td class="tx-r tx-num">...</td><td class="tx-r tx-num tx-br">...</td><td class="tx-r tx-num">...</td><td class="tx-r tx-num tx-br">...</td></tr>
#> <tr class="tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="sd: 1.225 ; n: 300">1.500</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">100%</td><td class="tx-r tx-num tx-br tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300"></td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300">100%</td><td class="tx-r tx-num tx-br tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 300"></td></tr></tbody></table></div>
```
