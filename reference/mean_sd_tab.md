# Simple Mean and SD Summary (deprecated)

One row per numeric variable: its base, its mean, its standard
deviation, and its coefficient of variation — the standard deviation as
a percentage of the mean, which is what lets two variables measured in
different units be compared for how dispersed they are.

**Deprecated**:
[`interpret`](https://bricenocenti.github.io/ggfacto/reference/interpret.md)
now opens with the same three figures, taken from the analysis itself,
so the description and the interpretation are one table and cannot
disagree. Use it instead; this function still works and will be removed
in a future release.

## Usage

``` r
mean_sd_tab(data, vars, wt)
```

## Arguments

- data:

  A data.frame.

- vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  The names of the numeric variables to compute means and sds with.

- wt:

  The name of the weight variable, if needed.

## Value

A `tabxplor` table — see \[ggfacto_summary\] for how it prints.

## See also

\[ggfacto_summary\], \[interpret()\].

## Examples

``` r
mean_sd_tab(mtcars, 1:7)
#> Warning: mean_sd_tab() is deprecated: use interpret(). See https://bricenocenti.github.io/ggfacto/articles/ggfacto.html
#> <div class="tx-scrollbox"><table class="tabxplor-tab" data-quarto-disable-processing="true"><thead><tr><th class="tx-l tx-br tx-bl tx-rv" rowspan="2">variables</th><th class="tx-r tx-num">n</th><th class="tx-r tx-num">mean</th><th class="tx-r tx-num">sd</th><th class="tx-r tx-num tx-br">sd/mean</th></tr><tr><th class="tx-r tx-num tx-unit">&lt;n&gt;</th><th class="tx-r tx-num tx-unit">&lt;mean&gt;</th><th class="tx-r tx-num tx-unit">&lt;sd&gt;</th><th class="tx-r tx-num tx-br tx-unit">&lt;cv&gt;</th></tr></thead><tbody><tr><td class="tx-l tx-br tx-bl tx-rv">mpg</td><td class="tx-r tx-num g2">32</td><td class="tx-r tx-num g2">20.09</td><td class="tx-r tx-num g2">6.03</td><td class="tx-r tx-num tx-br g2">30%</td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">cyl</td><td class="tx-r tx-num g2">32</td><td class="tx-r tx-num g2">6.19</td><td class="tx-r tx-num g2">1.79</td><td class="tx-r tx-num tx-br g2">29%</td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">disp</td><td class="tx-r tx-num g2">32</td><td class="tx-r tx-num g2">230.72</td><td class="tx-r tx-num g2">123.94</td><td class="tx-r tx-num tx-br g2">54%</td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">hp</td><td class="tx-r tx-num g2">32</td><td class="tx-r tx-num g2">146.69</td><td class="tx-r tx-num g2">68.56</td><td class="tx-r tx-num tx-br g2">47%</td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">drat</td><td class="tx-r tx-num g2">32</td><td class="tx-r tx-num g2">3.60</td><td class="tx-r tx-num g2">0.53</td><td class="tx-r tx-num tx-br g2">15%</td></tr>
#> <tr><td class="tx-l tx-br tx-bl tx-rv">wt</td><td class="tx-r tx-num g2">32</td><td class="tx-r tx-num g2">3.22</td><td class="tx-r tx-num g2">0.98</td><td class="tx-r tx-num tx-br g2">30%</td></tr>
#> <tr class="tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">qsec</td><td class="tx-r tx-num g2">32</td><td class="tx-r tx-num g2">17.85</td><td class="tx-r tx-num g2">1.79</td><td class="tx-r tx-num tx-br g2">10%</td></tr></tbody><tfoot><tr><td colspan="5"><div class="tx-foot">sd/mean: coefficient of variation -- the standard deviation as a percentage of the mean, comparable between variables measured in different units</div></td></tr></tfoot></table></div>
```
