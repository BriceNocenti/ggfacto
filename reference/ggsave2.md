# Save a plot as image

Save a plot as image

## Usage

``` r
ggsave2(
  plot = ggplot2::last_plot(),
  dir = NULL,
  name = "Plot",
  xt = "png",
  dpi = 600,
  width = 21,
  height,
  scale = 1,
  replace = FALSE,
  open = rlang::is_interactive()
)
```

## Arguments

- plot:

  The plot, created with ggplot2.

- dir:

  If saved as file, the directory in which to save the html widget.
  Default to temporary directory. Set global option
  `"ggfacto.export_dir"` with `link[base:options]{options}` to change
  default directory.

- name:

  The name of the file to save.

- xt:

  The extension name, when saving as image (interactive graph will
  always be .html).

- dpi:

  The resolution.

- width:

  The width in centimeters.

- height:

  The height in centimeters. By default, `width/1.41`.

- scale:

  Fixed ratio between horizontal and vertical axes.

- replace:

  Replace file ? By default, number added to find a new name.

- open:

  Should the resulting file be opened at once ?

## Value

Creates a file, and opens it in \`RStudio\` viewer, as a side effect.
