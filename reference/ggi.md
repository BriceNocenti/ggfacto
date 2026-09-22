# Make a graph interactive

Make a graph interactive

## Usage

``` r
ggi(
  plot = ggplot2::last_plot(),
  width = NULL,
  height = NULL,
  keep_ratio = TRUE,
  savewidget = FALSE,
  dir = NULL,
  name = "Plot",
  replace = FALSE,
  open = rlang::is_interactive(),
  ...,
  iframe,
  pixel_width
)
```

## Arguments

- plot:

  The graph, made with
  [`ggfacto`](https://bricenocenti.github.io/ggfacto/reference/ggfacto.md)
  (or
  [`ggmca`](https://bricenocenti.github.io/ggfacto/reference/ggmca.md),
  [`ggca`](https://bricenocenti.github.io/ggfacto/reference/ggca.md),
  [`ggpca`](https://bricenocenti.github.io/ggfacto/reference/ggpca.md),
  [`ggpca_cor_circle`](https://bricenocenti.github.io/ggfacto/reference/ggpca_cor_circle.md)),
  with or without ggplot2 elements added with `+`. A graph that is
  already interactive is returned as it is.

- width:

  The width in centimeters. Default to printing device's size.

- height:

  The height in centimeters. Default to printing device's size.

- keep_ratio:

  By default, the height is forced based of the relative size of the
  MCA's axes. Set to `FALSE` to avoid this behavior.

- savewidget:

  Should the html widget be saved on disk ? The file is standalone: one
  single `.html` carrying its own JavaScript, ready to be sent to
  someone, or shown in an `<iframe>`: the graph fills the window or the
  frame it is opened in.

- dir:

  If saved as file, the directory in which to save the html widget.
  Default to temporary directory. Set global option
  `"ggfacto.export_dir"` with `link[base:options](options)` to change
  default directory.

- name:

  The name of the file to save.

- replace:

  Replace file ? By default, number added to find a new name.

- open:

  Should the resulting file be opened at once ?

- ...:

  Additional arguments to pass to
  [`girafe`](https://davidgohel.github.io/ggiraph/reference/girafe.html)
  and
  [`dsvg`](https://davidgohel.github.io/ggiraph/reference/dsvg.html).
  The widget embeds the Liberation Sans font alone; `font_set` (see
  [`gdtools::font_set()`](https://davidgohel.github.io/gdtools/reference/font_set.html))
  embeds others.

- iframe, pixel_width:

  Deprecated and ignored: the widget sizes itself.

## Value

An html plot, of class `ggfacto_widget`. In a knitr document, setting
option `"ggfacto.widget_dir"` writes it to its own file and embeds an
`<iframe>` instead of the widget itself: see
[ggfacto_widget](https://bricenocenti.github.io/ggfacto/reference/ggfacto_widget.md).
