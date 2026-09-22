# Widgets written to their own file

Every interactive graph ggfacto returns –
[ggi](https://bricenocenti.github.io/ggfacto/reference/ggi.md) (and so
`ggfacto(interactive = TRUE)`),
[ggmca_3d](https://bricenocenti.github.io/ggfacto/reference/ggmca_3d.md),
[ggpca_3d](https://bricenocenti.github.io/ggfacto/reference/ggpca_3d.md)
– carries the class `ggfacto_widget`. In a knitr document, setting

## Details

`options(ggfacto.widget_dir = "auto")`

makes such a widget write itself to `widget_<chunk label>.html` and put
an `<iframe>` in the document instead of several megabytes of inline
JSON. Use `"auto"` to follow the chunk's `fig.path`, so the files travel
with the document exactly like its figures do, or give a directory path
of your own.

This matters for books: bookdown merges every chapter into one markdown
file and hands it to a single pandoc call, and a multi-megabyte raw HTML
block on one line is what that reader handles worst.

The chunk must have a label, since the label names the file. Widgets in
a chunk with `results = "hide"` are left alone: nothing would reference
the file.

The frames share one copy of the JavaScript, which the document itself
ships. Option `"ggfacto.widget_lib_dir"` says where, relative to the
document: the default `"libs"` is what a bookdown book uses, while a
plain
[`rmarkdown::html_document`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html)
with `self_contained = FALSE` puts them in `"<name>_files"`. If it is
wrong the frames say so rather than coming out blank.

Unset (the default), the option changes nothing and the widget is
embedded inline.
