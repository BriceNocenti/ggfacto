# Benzecri's modified rate of variance

Benzecri's modified rate of variance

## Usage

``` r
benzecri_mrv(res.mca, fmt = FALSE)
```

## Arguments

- res.mca:

  A multiple correspondence analysis, made with
  [`multiple_correspondence_analysis`](https://bricenocenti.github.io/ggfacto/reference/multiple_correspondence_analysis.md)
  (or
  [`FactoMineR::MCA()`](https://rdrr.io/pkg/FactoMineR/man/MCA.html),
  [`GDAtools::speMCA()`](https://nicolas-robette.frama.io/GDAtools/reference/speMCA.html)
  or `csMCA()`).

- fmt:

  By default, the result is given as a numeric vector. Set to \`TRUE\`
  to have a tabxplor `link[tabxplor]{fmt}` vector instead.

## Value

A numeric vector (or fmt vector with \`fmt = TRUE\`).

## Examples

``` r
data(tea, package = "FactoMineR")
res.mca <- multiple_correspondence_analysis(tea, 1:18)
benzecri_mrv(res.mca)
#>       Dim 1       Dim 2       Dim 3       Dim 4       Dim 5       Dim 6 
#> 55.38062748 28.05390900  7.64716710  3.26093633  2.13224567  1.61325415 
#>       Dim 7       Dim 8       Dim 9      Dim 10      Dim 11 
#>  0.97041810  0.61503425  0.25455746  0.05692444  0.01492601 
```
