# Graphs knitted at their own aspect ratio

Every graph ggfacto draws with ggplot2
([ggfacto](https://bricenocenti.github.io/ggfacto/reference/ggfacto.md),
[ggmca](https://bricenocenti.github.io/ggfacto/reference/ggmca.md),
[ggca](https://bricenocenti.github.io/ggfacto/reference/ggca.md),
[ggpca](https://bricenocenti.github.io/ggfacto/reference/ggpca.md),
[ggpca_cor_circle](https://bricenocenti.github.io/ggfacto/reference/ggpca_cor_circle.md)...)
carries the class `ggfacto_plot`, and knows the ratio of its axes: an
axis twice as long as the other is drawn twice as long, so the cloud
keeps the scale it is interpreted with. In a knitr or Quarto document,
such a graph is drawn at the chunk's `fig.width` and at the height this
ratio gives: no `fig.height` to compute by hand. Captions (`fig.cap`),
alignment, `out.width` and cross-references work as for any figure.

## Details

A chunk that sets its own `fig.height` or `fig.asp` (different from the
document's default) keeps it. The class changes nothing else: the object
is still a ggplot, to which ggplot2 elements can be added with `+`, and
[ggi](https://bricenocenti.github.io/ggfacto/reference/ggi.md) still
makes it interactive.
