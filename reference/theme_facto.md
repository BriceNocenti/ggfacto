# A ggplot2 Theme for Geometrical Data Analysis

A ggplot2 Theme for Geometrical Data Analysis

## Usage

``` r
theme_facto(
  res,
  axes = c(1, 2),
  legend.position = c("none", "left", "right", "bottom", "top"),
  no_color_scale = FALSE,
  size_scale_max = 8,
  xlim,
  ylim
)
```

## Arguments

- res:

  An analysis, made with
  [`multiple_correspondence_analysis`](https://bricenocenti.github.io/ggfacto/reference/multiple_correspondence_analysis.md),
  [`correspondence_analysis`](https://bricenocenti.github.io/ggfacto/reference/correspondence_analysis.md),
  [`principal_component_analysis`](https://bricenocenti.github.io/ggfacto/reference/principal_component_analysis.md),
  FactoMineR or GDAtools.

- axes:

  The axes to print, as a numeric vector of length 2.

- legend.position:

  One of `c("none", "left", "right", "bottom", "top")`.

- no_color_scale:

  When TRUE, you can provide color_scale next without warning.

- size_scale_max:

  Maximum size of the points.

- xlim:

  Horizontal axe limits.

- ylim:

  Vertical axe limits.

## Value

A list of ggplot2 objects.
