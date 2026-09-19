# ggfacto — AI Assistant Guide

## What ggfacto is, and why

`ggfacto` is a public R package: **v0.3.2 on CRAN, 0.4.0 in development here**. It builds readable, complete and pretty graphs for correspondence analysis made with 'FactoMineR'. They can be rendered as interactive 'HTML' plots, showing useful informations at mouse hover. The interest is not mainly visual but statistical: it helps the reader to keep in mind the data contained in the cross-table or Burt table while reading the correspondence analysis, thus preventing over-interpretation. Most graphs are made with 'ggplot2', which means that you can use the + syntax to  manually add as many graphical pieces you want, or change theme elements. 3D  graphs are made with 'plotly'.

The target users are : 1. a "literary" social sciences student, not good at math, learning to read geometrical data analysis ; 2. a serious quantitative analyst — survey researcher, sociologist — often working with weighted survey data.

### The one bet: the crosstable travels with the point

"Not mainly visual but statistical" is one feature and three that support it. `ggmca()` crosses every active variable with every other one (`active_tables = "active"`, the default) and prints those crosstables **inside the hover tooltip of each point** — that set of crosstables is the Burt table the MCA was computed from. Each percentage is coloured by its deviation from the mean, blue over-represented and red under, so a level at the edge of the cloud shows many colours and one near the centre shows few: the reader re-derives the geometry from the data instead of inventing a story for it. `active_tables = "sup"` does the same against the supplementary variables, and `profiles = TRUE` draws the cloud of individuals as distinct **answer profiles**, each hovering to show the answers it is made of. A graph you can interrogate is a graph you can be wrong about out loud.

### What follows from the two audiences

For the analyst, **survey weights ride one channel end to end** — `multiple_correspondence_analysis(wt =)` through the fit and back out into every tooltip crosstab — so a weighted analysis is never described by unweighted numbers; its `excl =` gives specific MCA, `hierarchical_clust()` clusters on the first axes, and `clust_tab()` describes the clusters as a coloured table. The student writes one workflow, the same for every analysis: the data frame first as in `tab()`, the analysis, its `interpret()` table, its `ggfacto()` graph, and clusters (or coordinates, `axis_coord()`) written into the data frame with `mutate()`. For the student, `ggmca_initial_dims()` and `ggmca_with_base_ref()` exist only to *teach*: one draws the active variables in their initial reference frame, the other draws that frame inside the space the analysis built. `interpret()` reads an MCA's axes by Le Roux and Rouanet's method, and `benzecri_mrv()` gives the modified rate of variance, because raw MCA eigenvalue percentages mislead.

### Why not the neighbours

Nothing here replaces FactoMineR — ggfacto never computes an analysis it was not handed, beyond the `multiple_correspondence_analysis()` / `principal_component_analysis()` / `correspondence_analysis()` wrappers and the clustering `hierarchical_clust()` computes — `HCPC()`'s own, rebuilt so that its tree needs no n × n matrix. Against the rendering packages: `factoextra` draws the same clouds, prettily, but a point stays a point; `explor` puts the tables *beside* the graph in a Shiny app rather than inside it, and hands back no ggplot to extend; `Factoshiny` and `FactoInvestigate` automate the reading instead of showing the data behind it. `GDAtools` is the closest neighbour — the statistical toolbox of the same French school, whose `speMCA()` and `csMCA()` fits ggfacto reads as they are — but it leaves rendering to others. ggfacto is the reading layer: it pins the graph back to the crosstable.

For anything related to crosstables, it relies heavily on `~/github/tabxplor/`.

---

## Repository Map

Nineteen files in `R/`, four groups. Every file carries a `# PURPOSE / # ROLE / # KEY CONSTRAINTS` header with fuller design detail: read it before the code.

**The analyses and their graphs** — each analysis has an entry point, a reader and a builder of the shared plot model.

- `mca-data.R` — `multiple_correspondence_analysis()` (alias `MCA2()`), fitted on the answer profiles, `ggmca()`, `ggmca_data()` and the MCA's builder.
- `ca.R` — `correspondence_analysis()` (alias `CA2()`), which reads a `tab()` and its supplementary variables, `ca_model()`, and `ggca()`.
- `pca.R` — `principal_component_analysis()` (alias `PCA2()`), `ggpca_cor_circle()`, `ggpca()`, the shared projector `PCA_ind.sup_coord()`, and the deprecated `mean_sd_tab()`.
- `ggfacto.R` — `ggfacto()`, the one graph verb, dispatching on the analysis.
- `clust.R` — `hierarchical_clust()` and `clust_tab()`: the clusters, and the table that describes them (`HCPC_tab()`, its deprecated former form).
- `mca-teach.R` — `ggmca_initial_dims()`, `ggmca_with_base_ref()`: the two pedagogical plots.

**The plot pipeline**, shared by the three graphs.

- `model.R` — `mca_model()`, the one reader of an MCA whatever engine made it, `pca_model()`, the units every consumer aggregates over, and `axis_coord()`.
- `plot-model.R` — the plot model and the helpers the three builders assemble it from.
- `plot-render.R` — `ggmca_plot()`, the one renderer. ⚠ must keep sorting after `mca-data.R` (see its header).
- `tooltips.R` — the hover text: crosstabs and means as tabxplor `fmt()` cells, headers, the points' tooltips.

**3D** (plotly, `Suggests`-guarded): `mca-3d.R` — `ggmca_3d()` · `pca-3d.R` — `ggpca_3d()`.

**Tables, rendering and plumbing.**

- `interpret.R` — the interpretation tables of a factorial analysis: `interpret()` and its three builders (`mca_interpret()`, `ca_interpret()`, `pca_interpret()`), `eigenvalues()`, `benzecri_mrv()`, and the output contract the summary family shares (`?ggfacto_summary`, its two print methods).
- `ingress.R` — what an analysis remembers of its input: the `<VAR>.NA` levels, the one `excl` rule, the weight rule, the rows it was fitted on (`source_rows()`, `align_to_fit()`).
- `render.R` — `as_ggfacto_plot()`, `theme_facto()`, the material palettes, `ggi()`, `ggsave2()`, `plot_path()`.
- `utils.R` — factor helpers, the base-R string shim that replaced stringr, `weighted.var()`, vendored `where()`, and the two soft-deprecation notices (`renamed_arg()`, `deprecated_fn()`).
- `i18n.R` — the gettext plumbing: the `R-ggfacto` text domain, its own cache flush, the language resolver, and `with_gda_lang()`, which makes `lang =` an argument rather than an accident of the session.
- `knit.R` — the knitr seams: a widget written to its own file with an `<iframe>` in its place when `options(ggfacto.widget_dir)` asks, and a graph drawn at its own aspect ratio.
- `ggfacto-package.R` — imports, global bindings, `.onLoad()`, the deprecated `%>%` re-export.

**Other directories:** `man/` (roxygen-generated, never edit) · `tests/testthat/` (the package's contract: the exported entry points, the argument matrix, the tooltip and table goldens) · `po/` (the message catalogues, `R-ggfacto.pot` and `R-fr.po`) · `inst/po/fr/LC_MESSAGES/` (the compiled `.mo`, committed, since `R CMD build` does not compile it) · `dev/` (`.Rbuildignore`'d; holds `dependency-audit.md`, `hierarchical_clustering.md`, `analysis_engine.md` with its prototype `analysis_engine.R`, `update_translations.R`, `hclust_chunked.R` and `stacked_crosstab.R`).

---

## ggfacto architecture

### How a graph is built

```text
                    ggfacto(res, data, ...)  dispatches on the analysis
                               │
  MCA: mca_model() ─► ggmca_data()     ┐     plot model                     ggplot2 +
  CA:  ca_model()  ─► ca_plot_data()   ┼──► list(vars_data, ind_data, ──► ggmca_plot() ─┬─► ggi()          → girafe widget
  PCA: pca_model() ─► pca_plot_data()  ┘     individuals, res, …)                       ├─► print()        → Plots pane
                                                                                         └─► knit_print()   → a figure at its own ratio
```

**One verb, one model, one renderer.** `ggfacto()` is an S3 generic, as `interpret()` is, dispatching to `ggmca()`, `ggca()` or `ggpca()`: its signature holds the arguments the analyses share, so an editor completes them, and the rest goes through `...` to the analysis's own function; an argument an analysis does not take is refused in words. Each of the three builds the same plot model from its own reader, and `ggmca_plot()` draws it. `ggmca()` has a body of pure orchestration: its signature is exactly `ggmca_data()`'s plus `ggmca_plot()`'s, with no overlap. **The MCA's seam is public on purpose** — a user calls `ggmca_data()`, edits `plot_data$vars_data` (dropping a level, renaming one), and passes it to `ggmca_plot()`; the CA's and PCA's builders stay internal. Interactivity is a separate last step, so everything before `ggi()` is an ordinary ggplot you can `+` into; `ggfacto(interactive = TRUE)` takes that step at once.

**What stays each analysis's own.** An MCA's tooltips cross the Burt table over its answer profiles, its points group the profiles by their *kept* answers, and a supplementary level is divided by √λ. A CA has no individuals: its supplementary levels are FactoMineR's `row.sup`/`col.sup`, its tooltips run twice (the rows' profiles, then the columns'), its frequencies come from the margins, and its clusters are a named factor over one margin. A PCA's active variables are **vectors** (`plot_data$vectors`): `ggfacto(acp)` alone draws the circle of correlations; asking for individuals, supplementary levels or clusters draws the **biplot**, the same arrows rescaled onto the cloud of individuals — the circle holding its 90 % closest points. The two clouds live in different units, so each keeps its own scale on the graph: the axes' values are the individuals', in their grey, and the circle carries the correlations' −1 / 1 graduations, bold, inside it with inward ticks, in the arrows' ink. A variable's tooltip opens on its mean (cv), and the central point gives n and every variable's mean (cv). Its supplementary level is the plain barycentre, and its tooltips print means, not crosstabs. A CA's supplementary levels are black text, never coloured points like the table's own. **Italics mark a supplementary level in every graph**, colour meaning something different in each analysis; clusters stay upright.

### The plot model

Every builder returns a plain `list`, not a class, of **flat** tables — no list-column, so a user can read and edit them:

- **`vars_data`** — one row per level: its `role` (`active`, `sup`, `clust`, `central`), its coordinates and contributions on every axis (`Dim k`, `contrib<k>`, NA outside active levels), its weight, its colour group, its `id`, and its tooltip as two strings, `begin_text` (the header) and `interactive_text` (the body). **`role` says what a row is; `color_group` says how it is coloured, and NA is neutral ink** — an MCA's active levels and the central point, drawn black through the scale's `na.value` — so no code compares a label to a sentinel string, and a CA colours its active levels by variable.
- **`ind_data`** — one row per drawn **point of the cloud**: an MCA's answer profile (profiles whose answers differ only in an excluded level share one), a PCA's distinct individual. `count` / `wcount`, its cluster and that cluster's colour key, its `id`, its tooltip string; `NULL` when `profiles = FALSE`, and always in a CA. A point's cluster is the **weighted plurality** of its individuals, missing ones left out: clusters made on the analysis are pure within a point, so the rule only decides for clusters made elsewhere. Points are drawn heaviest first, ties broken by a Weyl sequence, so a cap on equal weights keeps an evenly spread sample.
- **`individuals`** — one row per fitted individual: the rank `nb` of its point (`NA` beyond `max_profiles`), its weight, its coordinates, its supplementary answers; `NULL` without `sup_vars`. Ellipses and facets group it, so an ellipse covers every individual of its level. ⚠ It stays per individual while everything else is aggregated: ggplot2's ellipse sizes its radius on `nrow(data) - 1` degrees of freedom and `MASS::cov.trob()` tests convergence on absolute weights, so an ellipse drawn from aggregated units would not be the individuals'.
- **`res`** — a stripped `list(eig, axes_names)`, deliberately not the fitted object: the rendering half must not be able to recompute anything.
- **`clust`** — the cluster variable's name, or `character()`. **`lang`** — the language the model was built in, which the renderer writes in too. A PCA's model adds **`vectors`**: its variables' correlations, and the cloud they are rescaled onto in a biplot.

Hover ids are **one per variable** — hovering a level lights every level of its variable — and banded: supplementary variables from 1, active ones (and a PCA's vectors) from 1000, clusters and the points of the cloud from 10000, a cluster's id matched **by its name** for its label, its points and a CA's member levels. `ggmca_plot()` inserts the contribution lines of the two axes it draws between a level's header and body — the one part of a tooltip the data half cannot know. The largest point is sized from the drawn points' weights (`auto_size_max()`: the median profile stays visible, 14 on pc_AGD where the course chose 12 by hand), an explicit `size_scale_max` winning.

### The tooltip is the package

`interactive_tooltips()` computes its crosstabs on the **units** — the answer profiles, crossed with the non-active answers when some are shown — one `rowsum()` per column variable over the stacked row variables. Active against active, it is the Burt table the MCA was computed from, built from P profiles instead of n individuals: 0.1 s on pc_AGD where one stacked `tab()` took 0.8 s (that `stacked_crosstab()` is kept in `dev/`, a lead for speeding up tabxplor itself). Each cell is a `tabxplor::fmt()` record — its counts, its row percentage and its difference with the block's Total computed here, `tab()`'s arithmetic pinned cell for cell by `expect_cells_equal_tab()` — so `format_pct()` renders it with tabxplor's own colour (`fmt_get_color_code()`) and text (`format()`), never through tabxplor internals. A line is labelled by its level; only a level name several printed variables share says its variable, `no (lunch)`. A number in `tooltip_vars_1lv` prints its weighted mean. The body is a list of **blocks** (`tip_block()`), each a heading over its variables, so one builder serves the three analyses:

- a **CA**'s units are its table's cells, each variable NA outside its own block; a level's tooltip is its profile over each variable of the other margin, built in two passes (rows, then columns) and pinned against `tab(pct = "row")` in both. ⚠ Its header reads the **margins**: stacking the blocks would count a level once per block.
- a **PCA**'s units are its distinct individuals; a supplementary level's body is the mean of each active variable, coloured by its standardized difference from its block's mean (Glass's Δ, as `clust_tab()` colours a PCA's clusters), one `fmt` record per level against its own total — with several total rows in one vector, tabxplor's reference would be ambiguous. An individual's tooltip lists its values, coloured the same way.

- **WARNING** — an element shown only while another is hovered (a PCA vector's dashed projections) carries **no hover id**: ggiraph lets any element with one catch the pointer. It is drawn transparent with the id `reveal-<id>`, which `ggi()` turns into a plain attribute, adding a stylesheet that shows it while its vector is hovered (css `:has()`). A text's hover colour is its `fill` (dark gold on a label's light box), an arrow's its `stroke`. A point's tooltip gives its coordinates on the two axes drawn, which only the renderer knows.
- **WARNING** — ggiraph turns `\n` into `<br/>` only when a tooltip does not both start and end with an html tag; such a text it takes for raw html, and its lines collapse into one. `ggiraph_text()` gives every tooltip ending on a tag a trailing space. The tooltip css is `white-space:nowrap`, so near the right edge a tooltip keeps its width and ggiraph flips it to the left instead of squeezing it.
- **WARNING** — each block is compared to its **own** Total, over the individuals who answered both variables (`tab(comp = "tab", na = "drop")`). One stacked Total would measure a variable with missing values against the wrong reference.
- **WARNING** — the "Frequency" denominator is the population, the central point's weight, never a block's Total row, which gave some levels a frequency above 100 %.
- **WARNING** — the numbers are aligned with `str_pad()` under a monospace font, so the shim's exact stringr semantics (a *vector* `width`, a non-space fill) are load-bearing, not stylistic.

### The FactoMineR contract

The package computes no analysis of its own, the clustering apart (below). `multiple_correspondence_analysis()`, `principal_component_analysis()` and `correspondence_analysis()` are the **ingress normalisers** (short names `MCA2()`, `PCA2()`, `CA2()`, not deprecated) — tidyselect for `active_vars`/`wt`, a zero weight leaving its row out (FactoMineR crashes on it) and a missing one refused, a missing answer turned into a level `<VAR>.NA`, and one `excl` rule, **exact level names**, `NA` (the default) standing for every missing level, sent to FactoMineR as positions because it renames a level two variables share — while `correspondence_analysis()` reads a `tab()`'s weighted counts, whatever it displays. **A CA's supplementary variables ride its table**, since a CA's data is its table: in a `tab()` of several row or column variables, the first of each is the active table and every other variable gives supplementary rows or columns, passed to FactoMineR as positions; a matrix still takes FactoMineR's own `row.sup` / `col.sup`. An MCA's and a PCA's supplementary variables are added at the graph, from the microdata, as `sup_vars`.

**FactoMineR is fed the answer profiles.** `multiple_correspondence_analysis()` hands `FactoMineR::MCA()` the distinct combinations of active answers, each weighted by the sum of its individuals: identical rows of the indicator table leave X′X unchanged, so the eigenvalues and every result on the levels are the individuals' (to 1e-13, `dev/analysis_engine.md` section 4), 9 times faster at a million rows. The object keeps FactoMineR's class and slots, which the ecosystem reads as usual, with one row of `$ind` per profile. ⚠ So `FactoMineR::HCPC(acm)` would cluster profiles and `print()` counts them: the individuals come back through `axis_coord()` and `hierarchical_clust()`, in `mutate()`.

**One reader, `mca_model()` (`R/model.R`), is the only code that knows how an MCA is stored** — a ggfacto fit, a `FactoMineR::MCA()` on individuals, GDAtools' `speMCA()` and `csMCA()` (its subcloud) — and every consumer reads it: the plot model, the tooltips, the clusters, `interpret()`, the teaching plots, the alignment. It holds the profiles and their level codes, each individual's profile (`key`) and weight, the levels by position with their counts, the eigenvalues as one three-column table, and the fitted rows. A supplementary level is the weighted barycentre of its individuals over √λ, summed over the units. ggfacto adds two slots of its own: **`res$axes_names`**, the user's, read defensively and used by `theme_facto()` for the axis titles; and **`res$source`**, the ingress's, `list(n, rows, wt, name, key, w)` — which rows of the data frame the user named were analysed, under which weight column, that frame's name for a refusal to cite, and for an MCA each fitted individual's profile and weight. It is read off the call itself: `pc_AGD |> filter(...) |> multiple_correspondence_analysis(...)` is re-run with a hidden row id, and kept only if it gives back exactly the analysed data (a `%>%`, a `select()` or a `slice_sample()` record nothing). Every function that takes the microdata back — `ggmca()`, `ggmca_3d()`, `hierarchical_clust()` inside `mutate()` — goes through `align_to_fit()`, which picks those rows and **re-checks the active answers**, so a subset needs no second filter and a reordered data frame is refused rather than misaligned. `correspondence_analysis()` writes the margin names back on `call$X`, which `FactoMineR::CA()` strips, and records in `res$source` each level's variable and name, by position in `call$Xtot`, which `ca_model()` (`R/ca.R`) reads — FactoMineR `make.unique()`s level names. A PCA's graph reads `pca_model()`, its distinct individuals as a cloud of points. FactoMineR renames an MCA's levels in its results (`var_lv` for a level two variables share, `var.y` for a level named y/n) and GDAtools names them `var.lv`, so **their rows are read by position**, through the model alone, never matched by name.

**The clusters are `HCPC()`'s, computed here.** `FactoMineR::HCPC()` builds its Ward tree from a `dist()` and an n × n `outer()` of weights — +3.3 GB for the course's 9 234 rows, so a 32 GB machine runs out near 30 000. Ward merges identical points first, at no cost, so `hierarchical_clust()` builds the same tree on the **distinct points** of the cloud (an MCA's answer profiles, read from its model, weighted by the sum of their individuals) with `fastcluster::hclust.vector()`, which keeps no dissimilarity matrix: memory linear in the points, time still quadratic (~25 s at 40 000 distinct points). The rest is `HCPC()`'s and must stay so: the points sorted along axis 1 (it decides the ties and where the k-means starts), the cut rule, the **unweighted** k-means consolidation, the clusters numbered along axis 1. `test-clust.R` pins the equality for the three analyses, an MCA against `HCPC()` on its individuals. `consol = "weighted"` opts in to a Lloyd k-means counting each individual by its weight, run on the distinct points; by default the clusters stay FactoMineR's, weighted analysis or not. A CA clusters the levels of one `margin`, weighted by their counts, and in `mutate()` each individual takes its level's cluster, matched by name. **The tree is built once per session**: it is memoised on the content it is built from, so another `nb_clust` or `names` costs only the cut — the design, the options weighed and the course workflow are in `dev/hierarchical_clustering.md`. A tree built by parts, for larger clouds, was measured and kept out of `R/`: `dev/hclust_chunked.R`.

### Weights ride one channel

The user's weight column → `res$source$w`, kept beside FactoMineR's `row.w` (the profiles' sums) → read back through the model, never from `data` → every unit's weighted count, hence every tooltip cell, profile and supplementary level. A PCA's weights are read from `row.w.init` (`fit_weights()`), a CA's from its table, whose unweighted counts travel in `res$source$counts`. So a tooltip always describes the population the analysis was fitted on, even when the user passes the whole data frame after analysing a subset of it; `clust_tab(res, data, clust)` weights its table the same way, under the name of the user's column (`res$source$wt`). Consequently `count` (unweighted `n`) and `wcount` (`sum(row.w)`) travel as a pair everywhere: `wcount` is the point-size aesthetic and the sort key for `max_profiles` truncation, and a tooltip prints the weighted `n` only when it differs from the unweighted one. The ellipses weight each individual by `row.w` too (`stat_ellipse()`'s `weight`, ggplot2 ≥ 4.0.0).

### The plot-object seam

`theme_facto()` returns a **list** of ggplot objects, not a theme — axis titles carrying the eigenvalue percentages, scales, `coord_fixed()` — so it is `+`-ed as a whole; `ggmca_plot()` always calls it with `no_color_scale = TRUE` so the manual palette wins.

Every 2D graph leaves through `as_ggfacto_plot()` (`R/render.R`): it **prepends the S3 class `ggfacto_plot`** and carries its render hints as **attributes** — `height_width_ratio`, `css_hover` — which `ggi()` and `ggsave2()` read back; both must tolerate their absence, since a plain ggplot has none. Class and attributes survive `+`, `ggplot_build()`, `ggsave()` and `girafe()`. The class is what `knit_print.ggfacto_plot()` dispatches on: in a knitted document a graph writes its own figure at the chunk's `fig.width` and its own height, through `include_graphics()` so captions and cross-references work — a chunk that sets its own `fig.height` or `fig.asp` keeps it, detected against the document's default, and so does `results = "hide"`, under which knitr drops an image but keeps a plot. `ggi()` passes the ratio on to the widget as `ggfacto_ratio`, which is the only geometry a knitted `<iframe>` has to go on, and passes an existing widget through. Attributes rather than list slots because the object must stay a real ggplot: `append()` flattens the S7 object into a plain list wearing `c("gg", "ggplot")`, and then `ggplot_build()`, `grid.draw()` and so `ggsave2()` stop dispatching. The hover ids are the plot model's bands (above).

### Tables are tabxplor's

Every table here is built out of `tabxplor::fmt()` columns — scale, `col_var`, `row_kind`, colour, `ref` — and rendered by tabxplor; `benzecri_mrv(fmt = TRUE)` does the same for one vector.

**`interpret()`, `clust_tab()` and `mean_sd_tab()`: one output contract** (`?ggfacto_summary`, `R/interpret.R`). `interpret()` is the one verb the course teaches; its methods are one-line calls to the three builders, `mca_interpret()` and `pca_interpret()` (exported, as in 0.3.2) and `ca_interpret()` (internal). Each returns **one** `tabxplor` table, tagged with the subclass `new_tab(class =)` provides, so it can be piped, filtered and exported like any other; the format is a print-time decision, and it is **tabxplor's own**, `options(tabxplor.print)`, read by `print.` and `knit_print.`. There is no ggfacto option: a summary and the `tab()` two lines above it obey the same one, so a script sets it once. Markdown is not among its values — it is an explicit `|> tab_md()`. `gda_render()` is the one html render, so print and knit_print cannot drift on the option only ggfacto knows: the tooltip. ⚠ `gda_print_html()` reads it through `isTRUE()`: ggfacto reaches tabxplor by `tabxplor::` alone, so `library(ggfacto)` never loads its namespace and leaves the option **unset**, on which a bare `%in%` yields `logical(0)`. For the same reason the console branch **states** its medium for the delegated call — tabxplor's own print stops on an unset option (its roadmap, phase 9). An **axis summary** carries none — every figure it would reveal already has a column of its own — while `clust_tab()` asks for them, being an ordinary crosstab of percentages whose counts are worth hovering for. There is nothing to suppress in the footer, so a `tab_md()` written by hand needs no argument of its own. ⚠ dplyr carries a table's tabxplor attributes but not its class, so a summary that has been through `mutate()` prints as an ordinary table — which is why the render options ride a plain attribute rather than `meta`: they die with the methods that read them.

**The eigenvalues travel under the table**, as a subordinate table (`tabxplor::set_footer_tabs()`), so every medium renders them below it and the rule for choosing how many axes to interpret is never a second call to remember; `eigenvalues(res)` prints that same table alone (one builder, `eig_tab_of()`), for a document that shows it before the axes — a pipe table in console, a `<table>` in html, a sheet in Excel. It is a table and not a barplot, because the rule for choosing axes is a cumulated percentage — Benzecri's modified rate for an MCA, 80 % for a CA, an eigenvalue above 1 for a PCA — and no bar can be read that finely. `% variance` and `cumul.` sit under a `Variance` `col_var`, the modified rate and its cumul under `Benzecri`, so each group is framed as a block, and a `Total` row states what the axes add up to. `n_axes` bounds what is printed **independently of `axes =`**, and when axes are left out an ellipsis row **states how many the cloud has** (`... of 27`) and prints its ellipsis in every column, a row of blanks reading as missing data where the point is that axes are missing — a `"...{tok}"` display template whose token is NA there, so the literal alone survives and the unit line is unmoved (`fmt_display_label()` polls data and total rows alone) — the count being the only question a reader has, where the last axis alone gave a row with nothing above it to be read against. ⚠ The count is **passed in**, not read off `eig`, which `ncp` truncates: an MCA has `levels - questions` axes, a CA `min(dim) - 1`, a PCA `min(vars, n - 1)`. So the row appears in the two cases that differ — `n_axes` cut the display, or `ncp` cut the analysis — and **not at all** when every axis is shown, an ellipsis over nothing being a lie about the tail. The `Total` row is the **whole cloud** — 100 % and the total inertia, the ellipsis standing for the axes not shown — the inertia being read off one axis (`eig[1, 1] / eig[1, 2]`), which `ncp` cannot truncate: FactoMineR's default `ncp = 5` otherwise gave a PCA of seven variables `6.875 · 98.2 %`. **`% variance` carries a data bar** (`tabxplor::set_bars()`), the barplot inside the table: it says the SHAPE of the decline, the elbow being what a reader looks for, and the numbers beside it stay the rule. ⚠ Its ceiling is the column's own largest axis, `set_bars()`'s default. A ceiling of 100 % was measured and refused — an MCA's raw rates are diluted by construction (`tea`: 9.9, 8.1, 6.0, 5.2 %), so every scree would flatten into stubs. Total and ellipsis rows take none, tabxplor barring `row_kind == "data"` alone, and `color = FALSE` draws none at all.

⚠ **A correspondence analysis draws the STRUCTURE of a crosstab's deviations and says nothing of their size**, so the crosstab is asked for beside it — `tab(..., pct = "row", color = "contrib")`, percentages coloured by contribution, never `display = "ctr"`. `interpret()` of a CA does not carry it: a reader who wants both asks for both, and `vars =` is what names the two margins, defaulting to the names `correspondence_analysis()` wrote back, since `FactoMineR::CA()` destroys `names(dimnames())`.

**The ingress keeps every axis** (`ncp = Inf`). `res$eig` is how one chooses how many axes to interpret, and FactoMineR truncates it to `ncp`; worse, `benzecri_mrv()` renormalises over the axes it finds, so a truncated fit gives the SAME axis a different modified rate — 57.4 % against 55.4 % on `tea`, measured. Clustering takes its own `ncp`: `hierarchical_clust()` reads the first `ncp` axes of the fit it is given, which do not depend on how many axes a fit keeps, so there is never a second, truncated analysis in the user's script. Measured cost of the default on a 9 234 × 15 MCA: +0.03 s and +5 MB.

`mca_interpret()` and `ca_interpret()` share one builder, `gda_poles()`. Its statistics are Le Roux and Rouanet's: only a point contributing more than the mean contribution **of its own set** is kept, and the spread between a group's positive and negative points is stated in percent of that group's own contribution. ⚠ The set matters: an MCA has one (the active levels, summing to 100 % over K points), a CA has two (its rows and its columns, summing to 100 % over different numbers of points), so one pooled mean would keep too many of one and too few of the other. The table is **the axes as blocks**: the axis is the row *variable* (so tabxplor writes its heading once per block, wrapped, with one thick rule per axis) and the group is its level; a group's positive and negative points face each other on one row, its own figure is carried in every cell and `display`ed once, and one summary row per (axis, set) gives the two sides' summed contributions — the pair that says whether an axis opposes two poles or one specific group to the average.

**The threshold is an argument, and its label follows it.** `min_contrib = NULL` keeps Le Roux and Rouanet's mean, `0` keeps every level, a number keeps what contributes at least that much **on the displayed scale, in percent**. The summary row is then `Above mean ctr`, `All levels` or `Above 5%` — computed where the filter is applied, never written beside it, because a label naming a set it does not total is the one thing this table must not do. ⚠ **In a CA the margin's name leads it** (`Rows: above mean ctr`, or the name `vars` gave): an axis there carries two summary rows, and the bare label was the same word twice with nothing saying which contributions each totalled.

**A number is normalised before tabxplor grades it.** `pct` prints and `ctr` colours, which is what lets the sign of the coordinate ride the colour without reaching the page — but `ctr` holds the **multiple of the mean contribution**, negated on the negative side, and every summary row holds exactly 1. So a table carrying several summary rows (one per axis, or one per set) cannot grade against the wrong one, and `color = "contrib"`'s ×1/×2/×5/×10 ladder IS Le Roux and Rouanet's threshold.

**Only what has a ladder is coloured**, and the families differ because the quantities do. The contribution has that threshold everywhere. A **coordinate** has one only in a **PCA**, where under `scale.unit` it IS a correlation, so the 0.1/0.2/0.4/0.8 steps read it end to end; in an MCA or a CA, `complete = TRUE` prints it and does not colour it, a coordinate in axis standard deviations having no conventional cut-off. ⚠ **A cos2 is never coloured, on any of the three.** Its 50 % rule judges a whole axis, not a cell: an MCA cloud has dozens of axes, so every cos2 is structurally small — measured on nine binary questions, every retained level fell between 10 % and 48 %, i.e. entirely below the threshold and entirely red. A ladder that does not fit the quantity is a signal that is plausible and false.

**The colour legend is tabxplor's, saying ggfacto's nouns.** `set_legend_words()` re-states what the ladder grades — *contribution to the variance of the axis*, not to a chi² an axis has none of — and changes nothing else, so the swatches, the ladder, both registers, the publication palettes and the five media keep working, the console included, which no exporter argument can reach. It is built at **render**, hence coloured, and in the palette and language of the call that prints it. Two vocabularies, in `R/interpret.R`: `gda_contrib_words()` for the MCA/CA poles — whose leads say the thing the old plain-text line could not, that the sign of the ladder is the POLE of the axis — and `gda_pca_words()`, one measure (`difference`) on two scales, `word_std` naming `coord` and `word` naming `cos2`. ⚠ `ref` is **refused** on `difference` (its baseline is a row of the table, not a concept), so the leads carry the meaning instead and the compact form still brackets an empty `(Total)`; the prose form, which every export prints, does not. See `~/github/tabxplor/dev/legend_and_side_tables.md`.

**Under it, `gda_poles_glossary()` names each statistic the colours do NOT grade** — `contrib` (the whole question's), `coord`, `cos2`, `spread`, and `ctr` itself when `color = FALSE` leaves no legend to name it. One line each, full name and nothing more: *how to read it* belongs to the course and to `formations_stat`'s `agd.md`. They name no placeholder, so tabxplor appends them to the template. ⚠ **Plain text, always**: `subtext` is frozen at build, so an html span written there reaches a markdown file and an Excel cell as raw markup — `<breaks>` is the supported way to get a swatch. And a word set at build is frozen in the language of the build: `lang =` still has to exist here, since the axis headings and the summary-row labels are *factor levels*.

**What is translated is prose, never a name** (`R/i18n.R`, domain `R-ggfacto`): the legend, the axis heading, the summary row's label, `Rows` / `Columns` / `Total`. A column name and a `col_var` become the tibble's own name — `coord_Axe 1` — and one that changed with the language could not be indexed; it is tabxplor's rule too (`dev/french_glossary.md`). A `lang =` argument on the three functions makes the choice explicit, and the cache flush **re-binds our own domain**, since glibc keys on `(domain, msgid)` and tabxplor's flush would leave `R-ggfacto` cached.

**A column is named `<statistic>_<col_var>`, and the export strips the suffix.** `coord_Axe 1`, `contrib_Axe 1`, `cos2_Axe 1` keep the tibble's names unique and indexable; `tabxplor::tab_col_var_header()` shows a bare `coord` under an `Axe 1` span in html, markdown and Excel. `pca_interpret()`'s opening block — `mean`, `sd`, `sd/mean` under one `col_var`, absorbed from the now-deprecated `mean_sd_tab()` — works the same way.

### Cross-cutting invariants

- **All string work goes through the `utils.R` shim**, never stringr and never bare `paste0`/`sub`: the shim keeps stringr's `NA` and padding semantics, which several call sites use as guards.
- **`.` is a lambda pronoun, never a magrittr placeholder.** The package-level `. = NULL` binding turns a stranded placeholder into a wrong answer instead of an error.
- **A tooltip denominator is the population**, computed once, before any binding.
- **The plot object is a ggplot with a prepended class and render hints as attributes**, set by `as_ggfacto_plot()`. Never move the hints into list slots, and never rebuild the object with `append()`: either turns it from a ggplot into a list and silently breaks `ggsave2()`.
- **A graph speaks the language it is built in.** Every word of a tooltip or an axis title goes through gettext (`R-ggfacto`), the builders and the renderer inside `with_gda_lang()`; the model carries its `lang`. Graphs print nothing, but the colour groups under `options(ggfacto.verbose = TRUE)`.
- **Tables are tabxplor's job** — no kableExtra, DT or gt.
- **One name per thing across the three graphs.** The microdata is `data` everywhere, second as in `tab()`; the plot model is `plot_data`; the clusters are `clust`, a bare column name (a named factor of levels, for a CA); the cloud's points are `profiles`, whatever they are. The old `dat`, `cah`, `cah_color_groups` (and `ggmca_plot(data =)`) are soft-deprecated aliases routed through `renamed_arg()`, and `HCPC_tab(data, row_vars, clust, wt)` is the former form of `clust_tab(res, data, clust)`: each warns once per session, in one line pointing to the guide.
- **An answer profile is aggregated, never nested.** The fit's `key` maps each individual to its profile, and every number of a profile, a unit, a tooltip cell or a supplementary level is one `rowsum()` over them — no tibble per profile, no `purrr::map()` over profiles.
- **Variables are selected as in `tab()`.** `active_vars`, `wt` (and a PCA's `ind_name`), `sup_vars`, `tooltip_vars`, `tooltip_vars_1lv`, `row_vars` take tidyselect; `select_vars()` reads a symbol holding a character vector as `all_of()`, silently, so the former string form keeps working.
- **plotly and widgetframe are `Suggests`** and every entry point guards with `requireNamespace()`.

---

### Documentation ecosystem

The docs form one hierarchy, general to specific. **Each fact is stated at exactly one layer, referenced (never duplicated) across the others, and always written present-tense** — the current design is the reference point, never how it got there. The one place dev history is allowed is the roadmap "DONE" summaries. In R scripts, **the comments/code ratio should stay under 0.2**.

- **`## ggfacto architecture`** (this file) — the cross-subsystem big picture: goals, each subsystem's role and its meaningful "why", etc.. Rewritten only when the maintainer asks, by targeted cuts and replacements rather than accretion.
- **`## Repository Map`** (this file) — the file index: one role line per R file. *Cut, don’t accrete.*
- **R file-header comments** — per-file subsystem design: current architecture, key constraints, a pointer up to this file.
- **Inline `# DESIGN:` / `# WARNING:` tags** — the non-obvious "why" at the exact line, caveats to avoid, etc.
- **Vignettes** — usage and teaching, for users. ⚠ **None exist yet**: there is no `vignettes/`, no `VignetteBuilder` and no `knitr` in `Suggests`. Until there is, a `@param` that needs more than a sentence carries it itself; do not link to a vignette that is not there.
- **Roxygen man pages** (`?ggmca`, `?ggca`) — user-facing reference: *usage* and the main use cases, never build/internals/history. A `@param` states what the argument is, its values, and at most one sentence of when to change it; the rest is a link to the vignette that owns it. ⚠ The manual is LaTeX, so an Rd file is ASCII but for the few glyphs it can set (`— … × ÷`); `test-non-ascii.R` locks it.
- **`dev/*.md`** (`.Rbuildignore`'d) — transversal or expert technical guides only; there are three, `dependency-audit.md` (what each dependency costs to install, and the ruling on each), `hierarchical_clustering.md` (the clustering workflow: its design, the options weighed, the course code, the jamovi seam) and `analysis_engine.md` (what computes the analyses: FactoMineR's cost, the answer profile as the unit, parity, upstream and the community, the ruling). Each holds what an `R/` header is too short to derive — a foreign system, a cross-file policy, a statistical derivation — and the header that needs it points at it by section.
- **Roadmap "DONE" summaries are appended to the section below** (this file) — the **ONLY** place dev history lives. The maintainer then manualy moves them to `dev/ggfacto_roadmap_DONE_PHASES.md` for archiving.

---

## Deprecation and retro-compatibility

### For main user-facing functions and arguments
- This package have a small but existing users base : **soft deprecate main user-facing functions and arguments carefully** to ensure retro-compatibility.
- **It’s always possible to modify the main API for user-friendliness and integration** by **routing old arguments to new ones** and do *ad hoc* back-compat *after* having found a new more user-friendly API.

### The deprecation rule
- **Old code keeps working.** A call that ran on the last CRAN release (or in the course material) still runs and gives the same result — same table, same clusters, same graph — unless it was a bug.
- **New names carry the new API; old names keep the old one.** A redesigned API goes to a new function name or to the current one. The old name (`HCPC_tab()`), the old argument (`cah`, `dat`) or the old calling form (`clust_tab(data, ...)`) keeps its exact former signature, so every former call shape binds as before: positional, named, piped.
- **The old is a thin wrapper around the new, never a second implementation.** It resolves its arguments and calls the same internal builder the new API calls (a former calling form is re-dispatched to the old signature with `sys.call()`). Parity with the former output is pinned by a test.
- **Soft deprecation, one line, once per session.** Every notice goes through `deprecated_fn()` / `renamed_arg()` (`R/utils.R`): short, naming what to write instead, and pointing to the guide (`GGFACTO_GUIDE`, the pkgdown vignette) for the new usage. Never an error, and never a notice on every call.
- **Renamed arguments sit last** in the signature, so no positional call can reach them, and only the new name is documented as an argument: the old one is one `@param` line saying it is deprecated. A deprecated function keeps its own man page, `@keywords internal`.
- **Short names that are not deprecated are true aliases** (`MCA2 <- multiple_correspondence_analysis`), not wrappers.

### For internal code and internal functions
- **Do not hesitate to propose radical redesign of internal code and internal workflows** for quality, simplicity, structure, performance and future-proofing, specially when they are too convoluted or have grown organically.
- **Always try to simplify, integrate and create smart shared subfunctions** instead of adding a new layer of confusion and ad-hoc solutions inside the code: your main aim is to simplify, to remove traces of old implementations altogether when they have become useless, to clarify, to help me make relevant architectural choices instead of piling up ad-hoc solutions, to integrate the new features in the current code seamlessly.

---

## Testing

`tests/testthat/` is the package's **contract**: it must fail when a user-visible fact changes, must not fail when an internal is redesigned, and must stay fast enough to run on every edit. Sixteen files: `helper-fixtures.R` (the cached analyses and plot models, and `mca_ind()`, FactoMineR on the individuals, the reference every profile fit must equal), `helper-i18n.R` (`skip_if_no_gettext()`), `test-model.R` (the profile fit against the individuals', the model of every engine, `axis_coord()`), `test-ingress.R` (the ingress normalisers, the `excl` rule, and the source-row contract in both directions — every provable pipe shape recorded, every unprovable one refused), `test-clust.R` (`hierarchical_clust()` equal to `FactoMineR::HCPC()` for the three analyses, one tree for every cut, `names`, its alignment by context, and `clust_tab()` with its former form), `test-ggmca-data.R` (the plot model and the argument matrix), `test-tooltips.R` (the crosstabs behind the hover, cell for cell against `tab()`), `test-ggca.R` (the CA's supplementary variables against FactoMineR, its tooltips cell for cell against `tab()` in both passes, its clusters, its former arguments), `test-ggpca.R` (the PCA's individuals, its supplementary levels against `quali.sup`, its mean cells against `tab()`, the circle), `test-ggfacto.R` (the one verb: its dispatch, its refusals, `interactive`), `test-plots.R` (that every graph builds, the class and render-hint seam, the size rule), `test-knit.R` (the widget seam, and a graph knitted at its own ratio), `test-interpret.R` (the interpretation tables and the output contract the whole summary family shares, `mean_sd_tab()` included), `test-i18n.R` (the French catalogue), plus `test-str-shim.R` and `test-non-ascii.R` from 1a.

**Argument coverage is the point, not function coverage.** Several arguments are inert alone and only act alongside an enabling one — `keep_levels`/`discard_levels` need `sup_vars`, `tooltip_vars`/`tooltip_vars_1lv` need a table to be built at all, and `clust` colours no profile under `profiles = FALSE`. A test that omits the enabler passes while exercising nothing; the vacuous paths are pinned deliberately so nobody "simplifies" them back into nothing.

**Fixtures are `tea[1:6]`.** Six binary questions reach every code path and keep the goldens short; the tooltips no longer cost what made the rule (0.04 s on six questions, 0.08 s on eighteen). GDAtools fits are tested under `skip_if_not_installed("GDAtools")`. The models the suite reuses are memoised in `helper-fixtures.R`, among them `fx_tea_na()` (missing answers for the `excl` rule), `fx_mca_young()` (an analysis of a piped subset, which records its rows), `fx_ca_multi()` (a CA of `tab(gss, c(relig, marital), c(partyid, race))`, weighted) and `fx_pca2()` (a weighted PCA of `mtcars`, with factors to project). The one exception is `fx_mca_multi()`, local to `test-interpret.R`: `tea[1:6]` is all binary, so `mca_interpret()`'s row packing collapses every question to one line there and its display blanking has nothing to hide — that needs multi-level variables and a third axis.

The suite is **small and serial**: 2 354 assertions (most of them the tooltip cells, one by one), about 60 s, no `Config/testthat/parallel`, no `setup.R`. ⚠ **A green local suite does not mean a green CI**: this box is `fr_FR.UTF-8`, while `R CMD check` forces `LANGUAGE=en` with a C message locale, where gettext cannot translate at all. Every French assertion is therefore guarded by `skip_if_no_gettext()`, and each translated feature is pinned **twice** — an unguarded English block plus a guarded French twin. ⚠ Do not turn parallelism on for it, and do not import tabxplor's worker, orphan and gettext conventions — see `~/github/tabxplor/CLAUDE.md` "## Testing" only if the suite ever grows enough to need them.

**Golden tests use `expect_snapshot()`** (`_snaps/*.md`), and only where the output is genuinely stable and worth the churn: the rendered tooltip text of each graph (an MCA level, a CA level and the central point, a PCA supplementary level) and the four interpretation tables (MCA concise and complete, CA, PCA). ⚠ The *rendered html* is never snapshotted — it is 7 kB of inlined stylesheet; an interpretation table's snapshot is the console print, taken with `n = Inf` under `options(tabxplor.print = "console")`, since pillar formats only the rows it shows and a slice without a summary row makes `color = "contrib"` warn.

```bash
#In a temp .R file (outside tests/), then: OMP_NUM_THREADS=1 Rscript that_file.R
#   Sys.setenv(NOT_CRAN = "true"); devtools::test("~/github/ggfacto")
#   devtools::test("~/github/ggfacto", filter = "<name>")   # one/few files while iterating
#   devtools::check("~/github/ggfacto", document = FALSE)   # the release gate, ~1 min, must be 0/0/0
```

⚠ **`devtools::document()` needs `dangerouslyDisableSandbox`** — bwrap `--ro-bind`s `NAMESPACE` and `man/`.

⚠ **`Rscript` writes `Rplots.pdf` into the working directory** when a plot prints without an open device; it turns `R CMD check` into a NOTE. Open a `pdf(tempfile())` in any harness that draws.

---










## ggfacto v 0.4.0 roadmap and "DONE" summaries


### Phase 1 — clean start

#### Phase 1j — la barre de données revient sous l'éboulis (DONE)

**Une ligne restaurée, et l'échelle est enfin une décision plutôt qu'un défaut.** Suite verte :
**422** (416 avant), `check` 0/0/0. `% variance` porte de nouveau sa barre : la phase 1i l'avait
retirée faute de pouvoir énoncer un plafond, et `tabxplor::set_bars(x, cols, max = NULL)` (sa phase
9) le rend énonçable.

**Le plafond reste le plus grand axe, et c'est mesuré.** Un plafond à 100 % a été essayé et refusé :
les taux bruts d'une ACM sont dilués par construction (`tea` : 9,9 / 8,1 / 6,0 / 5,2 %), et tout
éboulis s'y aplatirait en moignons indiscernables. La barre porte donc la **forme de la décroissance**
— c'est `barplot(res$eig[, 2])` à l'intérieur du tableau, et le coude est ce qu'on y cherche — pendant
que les nombres à côté restent la règle du choix des axes, qu'aucune barre ne peut dire.

**`color = FALSE` n'en dessine aucune.** Une barre bleue sous un tableau demandé sans couleur serait
une surprise ; `gda_eig_tab()` reprend donc son paramètre `color`, en argument nommé — sa place
positionnelle d'avant 1i est occupée par `n_total`.

**Rien à faire pour l'aspect.** La barre est un `::before` arrondi, bordure `1px` à pleine force et
fond au mélange 30 %, encre `--tx-bar-ink` = l'`accent` du thème, c'est-à-dire le bleu `.p3` : tout
cela vit dans la feuille de style de `tabxplor`, suit l'interrupteur clair/sombre et les palettes de
publication, et Excel dessine maintenant son `dataBar` aux bornes épinglées.

⚠ **Le test est un test de VALEUR, parce qu'un test de forme a déjà laissé passer cette panne.** En
phase 1f la barre était posée, documentée et testée, et ne s'affichait **jamais** : `% variance` porte
une espace dans son nom, et tout ce qui est clé par nom de colonne se périme en silence. Le nouveau
test lit donc les `--tx-bar:…%` **dans le html rendu** : quatre barres pour quatre axes montrés, la
première à 100 %, une décroissance stricte, et aucune sur la ligne d'ellipse ni sur `Total`.

**La ligne d'ellipse imprime son ellipsis, et pas seulement dans son libellé.** Ses cinq cellules
étaient vides, ce qui se lit comme une donnée manquante là où le propos est que des **axes** manquent.
Chacune reçoit un `display` `"...{tok}"` dont le jeton vaut `NA` sur cette ligne : seul le littéral
survit, et l'étiquette d'unité ne bouge pas — `fmt_display_label()` ne fait voter que les lignes
`"data"` et `"total"`, donc `<var>` et `<col%>` tiennent et rien ne devient `mixed`. Vérifié dans les
cinq médias, Excel compris.

⚠ **Une trace périmée corrigée au passage** : `@param n_axes` annonçait encore « the last one is
always shown besides », faux depuis la phase 1i où l'ellipse a pris la place du dernier axe pour en
porter le compte. Trois blocs roxygen.

**Ce que la phase n'a pas fait.** Aucun graphique, aucun calcul, aucune valeur : seul le balisage
d'une colonne bouge, et les instantanés console ne s'en aperçoivent pas — une *pipe table* n'a nulle
part où mettre une barre. ⚠ Les 19 avertissements de `test-tables.R` et `test-plots.R` préexistent et
n'appartiennent pas à cette phase.


#### Phase 1k — une AGD suit `options(tabxplor.print = "md")` (DONE)

**Le prédicat binaire devient un choix à trois, et c'est tout.** `gda_print_html()` — « l'option
demande-t-elle du html ? » — est remplacé par `gda_medium()`, qui rend le médium au lieu d'un
booléen : `"html"` (avec `"kable"`, son synonyme d'avant 2.0.0), `"md"`, ou la console. Les deux
méthodes cessent de forcer `"console"` dans leur appel délégué et **passent le médium demandé**.
Suite : **420** passants, `FAIL 2` **inchangés avant et après** — vérifié en rejouant la suite sur
le `R/interpret.R` de `HEAD`. ⚠ Ces deux-là sont les instantanés de `test-interpret.R`, et ils
tombent à cause de **`tabxplor` 2.0.1** : une ligne `Total` de *pipe table* n'y est plus en gras
(`|**Total** | **6.875**|` devient `| Total | 6.875 |`, marge de droite comprise). À trancher côté
`tabxplor` — voulu, et l'instantané s'accepte ; non voulu, et c'est une régression de son
`bold_references`. Ni l'un ni l'autre n'appartient à cette phase.

**Pourquoi maintenant.** `tabxplor` vient d'ajouter la valeur `"md"` à `options(tabxplor.print)`
(sa phase 12), pour les carnets d'exploration de `formations_stat` — un `.qmd` knité en `.md`. Sans
cette phase, un `mca_interpret()` posé au milieu d'un carnet retombait **en console**, entre des
`tab()` rendus en markdown, sans un mot. Mesuré avant correctif : le tableau des valeurs propres
sortait en markdown (il passe par `tabxplor`) et celui des axes en tibble (il passe par ici) — deux
médias dans un même appel.

**Le html reste la seule branche, parce qu'il est le seul à avoir des arguments à lui.**
`gda_render()` lit `attr(x, "ggfacto_render")` — `var_names`, `tooltips` — que `tabxplor` ne connaît
pas. La console et le markdown, eux, n'ont rien à ajouter à `gda_plain(x)` : ils délèguent tous deux
au `print()` / `knit_print()` de `tabxplor`, qui sait déjà router les trois. Un médium de plus
n'ajoutera donc pas de branche ici, seulement un bras au `switch()`.

⚠ **Le médium reste ÉNONCÉ dans l'appel délégué, et la raison n'a pas changé** : `library(ggfacto)`
seul laisse le *namespace* de `tabxplor` non chargé, donc l'option non posée. Le `%||% "console"` du
`switch()` remplace l'`isTRUE()` d'avant — sans lui, `switch(NULL, …)` s'arrête. Une valeur inconnue
retombe sur la console, ce que fait aussi le routeur de `tabxplor` : un résumé et le `tab()` deux
lignes au-dessus ne doivent pas être en désaccord sur une faute de frappe.

**Il n'y a toujours aucune option `ggfacto`.** Un résumé et un tableau croisé obéissent à la même,
et c'était déjà la décision qui avait supprimé `options(ggfacto.print)` — cette phase la tient, elle
ne la défait pas : c'est la valeur `"md"` qui revient, du côté où elle a un sens.


#### Phase 1l — le Total de l'éboulis vaut 100 %

**Le `Total` du tableau des valeurs propres est le nuage entier.** `pca_interpret()` et `ca_interpret()` affichaient `98,2 %` / `98,7 %` sur un ajustement `FactoMineR` brut : son `ncp = 5` par défaut tronque `res$eig`, et `gda_eig_tab()` sommait ce qu'il tenait. `mca_interpret()` n'y échappait que parce que `MCA2()` garde tous les axes. Depuis que la ligne d'ellipse énonce le vrai nombre d'axes (`... of 7`), elle peut représenter ceux qui manquent : le `Total` vaut donc 100 %, et sa valeur propre l'**inertie totale**, lue sur un seul axe (`eig[1, 1] * 100 / eig[1, 2]`, que la troncature n'atteint pas ; repli sur la somme pour un nuage dégénéré). Mesuré : `7,000 · 100 %` pour l'ACP de `mtcars[1:7]`. Cela défait la règle « lu, jamais supposé » de la phase 1i, devenue fausse dès que l'ellipse a porté le compte.

Tests : les deux cas tronqués (`ncp = 2`, `ncp = 3`) attendent 100 % et l'inertie de l'ajustement complet, et un `FactoMineR::PCA()` brut s'y ajoute. L'instantané ACP est corrigé **à la main**, gras compris. ⚠ Les deux échecs de `test-interpret.R` préexistent et sont identiques sur `HEAD` : la ligne `Total` d'une *pipe table* qui perd son gras (`tabxplor` 2.0.1, voir 1k), et le test d'export dont l'en-tête `Axe 1` n'est plus trouvé par `>Axe 1</th>`. Ni l'un ni l'autre n'appartient à cette phase.



#### Phase 1m — un seul geste pour les étudiant·es : la base en second, les classes dans `mutate()` (DONE)

**Le cours enseignait un code qu'un·e débutant·e ne peut pas écrire seul·e**, et la phase le remplace par un seul geste, le même pour les trois analyses. Relevé dans le livre du M2S1, ses examens et les carnets : une seconde ACM `ncp = 3` pour nourrir `HCPC()`, `mutate(x = cah_res$data.clust$clust)` — positionnel, donc faux sans un mot dès qu'une ligne a été filtrée ou réordonnée —, `dat =` qui avertissait déjà à chaque appel, `cah = "nom"` en chaîne là où `HCPC_tab(clust = nom)` prenait un nom nu, `excl` en expression régulière (`"NA"` attrapait `"NATIONAL"`, et `"65 ans et +"` ou `"Compliqué? D'accord"` sont des motifs cassés, deux niveaux réels de `FES2017`), `tab() |> as.matrix() |> CA(graph = FALSE)` puis `ca_interpret(vars =)`, et `acm$eig` / `barplot()` / `benzecri_mrv() |> cumsum()` pour choisir les axes alors que le pied de `mca_interpret()` porte déjà tout cela.

**Cinq noms longs, enseignés ; les courts restent, non dépréciés.** `multiple_correspondence_analysis()` (= `MCA2()`), `principal_component_analysis()` (= `PCA2()`), `clust_tab()` (= `HCPC_tab()`), et deux nouvelles sans jumelle courte : `correspondence_analysis(tab)`, qui prend le tableau lui-même — l'étudiant·e comprend qu'une AC est celle d'un tableau — et en lit les **effectifs pondérés quel que soit ce qu'il affiche** (un `tab(pct = "row")` donne la même AC, mesuré), sans ligne ni colonne Total, puis réécrit sur `call$X` les noms des deux variables que `FactoMineR::CA()` efface, ce qui rend `ca_interpret(vars =)` inutile ; et `hierarchical_clust(res, ncp, nb.clust = -1)`, qui **réajuste l'analyse sur ses `ncp` premiers axes depuis son propre appel** (données, poids, exclusions) et y lance `HCPC()` — identique au geste manuel, épinglé par un test — et rend les classes en facteur, invisiblement : un appel nu dessine l'arbre (toujours quand `nb.clust = -1`) sans imprimer 9 234 valeurs. `description = FALSE` : les tests du χ² dont `HCPC()` décrit ses classes ne servent à rien ici, et leurs avertissements tombaient dans le `mutate()` de l'étudiant·e.

**Un sous-ensemble ne se filtre qu'une fois.** `pc_AGD |> filter(AGE >= 18) |> multiple_correspondence_analysis(...)` enregistre dans `res$source` quelles lignes de `pc_AGD` ont été analysées : l'appel est relu (`rlang::enexpr()`), sa racine rejouée avec une colonne d'identifiants cachée, et les lignes ne sont gardées **que si le rejeu redonne exactement les données analysées** — ce qui écarte d'office `select()` (l'identifiant est perdu), `slice_sample()` (un autre tirage) et `%>%` (l'expression n'y est que `.`). Coût mesuré sur `pc_AGD` (9 234 × 41) : 26 ms. Ensuite `pc_AGD |> mutate(cah = hierarchical_clust(acm, ...))` écrit les classes sur les bonnes lignes et `NA` ailleurs, et `ggmca(acm, pc_AGD, ...)` reprend la base entière : les deux passent par `align_to_fit()`, qui **revérifie les réponses actives**, si bien qu'une base réordonnée ou modifiée après l'analyse est refusée avec son explication au lieu d'être mal alignée — le risque que portait déjà `cah_res$data.clust$clust`. ⚠ Le `rlang::caller_env()` doit être capturé avant tout forçage de promesse : passé en argument, il nommait le mauvais cadre et rien n'était jamais enregistré. ⚠ Écarté et mesuré : `data$cah[lignes] <- facteur` sur une colonne neuve donne des **codes entiers** (`NA 1 2`), raison pour laquelle le cours enseigne `mutate()` seul.

**`excl` nomme des niveaux, exactement.** Chaque réponse manquante d'une variable active devient un niveau `<VAR>.NA`, unique d'une variable à l'autre (FactoMineR ne le renomme donc jamais `var_lv`), et `excl = NA` — le défaut, `"NA"` accepté — les exclut tous, ainsi que les niveaux `DIPLOME.NA` que `FES2017` porte déjà : les données du cours n'ont pas eu à changer. `excl = "DIPLOME.NA"` n'exclut que ceux-là, `excl = NULL` garde tout, un nom inconnu avertit. FactoMineR reçoit des **positions**, jamais des noms. `clust_tab()` suit la même règle, et laisse de côté les lignes sans classe (hors de la population analysée). ⚠ Changement silencieux pour un·e utilisateur·rice de `MCA2()` qui avait des valeurs manquantes : son ACM devient spécifique ; le carnet `hdv03_retraite` recalculé redonne pourtant les mêmes classes, individu par individu.

**`clust` remplace `cah` partout** (`ggmca`, `ggmca_data`, `ggmca_3d`, `clust_color_groups`, la case `plot_data$clust`, la bulle « Cluster: »), nom nu, chaîne ou vecteur, lu par un seul `resolve_clust()` ; `cah` et `cah_color_groups` passent par `renamed_arg()`. **`text_repel = TRUE` par défaut** dans `ggmca()` et `ggca()`, que le cours écrivait à chaque appel. **La base est le second argument**, comme dans `tab()`, et une fonction qui en a besoin sans l'avoir le dit dans ces mots-là.

**Fichiers.** `R/ingress.R` (neuf : `na_levels()`, la règle `excl`, `source_rows()`, `align_to_fit()`) et `R/clust.R` (neuf : `hierarchical_clust()`, `clust_tab()` venu de `tables.R`, `resolve_clust()`) ; `tables.R` disparaît, `mean_sd_tab()` rejoint `pca.R` ; `levels_to_na()` (l'expression régulière) disparaît de `utils.R`.

**Les anciens échecs et avertissements.** L'instantané ACP perdait le gras de son `Total` : régression de `tabxplor` phase 11 (une ligne `blank` rendait « discriminante » une colonne de moyennes), corrigée dans `tabxplor` — le gras se décide sur les seules lignes graduées — avec son test et sa ligne de roadmap. L'en-tête `Axe 1` d'un export s'écrit avec une espace fine insécable (`tabxplor` phase 10, voulu) : le test l'accepte. Les 19 avertissements ont disparu : `linewidth` sur les ellipses, un `first_case()` à la place des `case_when()` à condition scalaire des vues 3D, l'échelle manuelle de couleur qui n'est plus posée quand rien ne s'y rapporte (`type = "points"` / `"labels"` sans variable supplémentaire), et `test-tables.R` qui appelait `all_of()` hors sélection. Trouvés au passage : `ggmca_3d()` comparait à `"variables_actives"` (toutes ses étiquettes en italique), `ggmca(type =)` ignorait `"active_vars_only"`. Et `clust_tab()` — déjà `HCPC_tab()` — nettoyait les noms de classes dans ses lignes de modalités et pas dans ses lignes de population : des classes nommées `1-Petit écran`, comme le cours les nomme, donnaient deux jeux de colonnes ; elles sont nettoyées une fois, avant les deux `tab()`, et `cleannames` devient un argument.

**Suite** : 488 assertions (377 avant), 0 échec, 0 avertissement, ~40 s ; `check` 0/0/0. **Le cours** : le livre du M2S1, ses examens et 13 carnets d'exploration réécrits dans ce geste unique, puis rendus un à un.

#### Phase 1n — un profil est un groupe, un tableau croisé est un appel (DONE)

**Deux clés, et plus aucun `nest()` / `map()` / `unnest()` dans le chemin de l'ACM.** Mesuré sur `pc_AGD` (9 234 × 41, 15 variables actives, 5 312 profils) :

| Appel                                         | Avant       | Après       |
|-----------------------------------------------|-------------|-------------|
| `ggmca_data(profiles = TRUE)`                 | 1,1 s       | 0,17 s      |
| `ggmca_data(clust =, profiles = TRUE)`        | 2,5 s       | 0,35 s      |
| `ggmca(ellipses = 0.5)` / `type = "facets"`   | 5,4 / 5,7 s | 0,8 / 0,6 s |
| `ggmca_data(active_tables = "active")`        | 7,7 s       | 1,3 s       |

**Un profil de réponse est un groupe d'individus.** Son appartenance est un vecteur d'entiers (`answer_profiles()`, `vctrs::vec_group_id()` sur les réponses que la bulle affiche), et chaque nombre d'un profil — effectifs, classe, coordonnées — est une agrégation vectorisée sur ce vecteur, là où 5 312 tibbles imbriqués étaient parcourus un à un. Le modèle devient **plat** : `vars_data` porte sa bulle en deux chaînes (`begin_text`, `interactive_text`) et ses contributions en colonnes `contrib<k>`, `ind_data` une chaîne et un `id`, et un cinquième élément, **`individuals`** (une ligne par individu : rang de son profil, poids, coordonnées, variables supplémentaires), nourrit les ellipses et les facettes, que `ggmca_plot()` dépliait jusqu'ici profil par profil (4,2 s). Mesuré : les coordonnées sont identiques au sein d'un tel groupe même sous `excl` (écart 1,6e-15). `data.table` quitte les `Imports` avec `complete_clust()`, et `fct_detect_replace()` disparaît.

**`tabxplor` coûte ~30 ms par paire (variable en ligne × variable en colonne), quelle que soit la taille** — 300 ou 9 234 lignes, mesuré — et la bulle `active_tables = "active"` en faisait 225. Un seul `tab()` sur les variables **empilées** (chaque individu une fois par variable, chaque modalité un code rendu par une table de correspondance) en fait 15, et `tab_vars` rend à chaque bloc son **propre** Total : les 585 colonnes (`pct`, `diff`, `n`, `wn`, couleur, `format()`) sont identiques à un `tab()` par variable, avec des NA en ligne et en colonne. ⚠ Un Total commun avait été mesuré : il décalait `diff` pour une variable à valeurs manquantes. Les variables sans tableau croisé, qui ne servent qu'à l'en-tête, sont comptées directement (`level_counts()`), et `unlv()` disparaît. Rien n'est à exporter de `tabxplor`. `options(tabxplor.parallel =)` traverse ggfacto telle quelle, et le résultat est identique, `clust_tab()` compris ; le `tab()` empilé est une seule unité, que `tabxplor` exécute en série, et un pool (1–2 s au démarrage) n'y gagnerait rien.

**Cinq bogues trouvés et corrigés en chemin.**

- **Les ids de survol reliaient la mauvaise classe.** Ils valaient `as.integer(lvs)`, sur un facteur que `cleannames` retrie par ordre alphabétique : avec 11 classes de `hierarchical_clust()`, 10 étiquettes allumaient les profils d'une autre. Ils sont désormais appariés par le nom, et `clust_id` disparaît au profit du seul `id`.
- **`complete_clust()` supposait contiguës les lignes d'un profil** : 50 classes manquantes injectées déplaçaient 7 profils. Une classe manquante est remplacée par la **pluralité pondérée** des individus du profil, et tout profil ayant un individu classé est coloré.
- **Les réponses exclues s'imprimaient** en `Remove_levels` dans les bulles de profils : 11 sur 46 sur `tea` avec des NA.
- **Les ellipses étaient ignorées sans `profiles = TRUE`**, et ne couvraient que les individus des `max_profiles` profils les plus lourds (8 921 des 9 234 sur `pc_AGD`). Elles couvrent désormais tous les individus, sans `profiles`. Les facettes n'ont plus besoin de `profiles` non plus, s'arrêtent sans variable supplémentaire, et les ellipses sans variable supplémentaire avertissent.
- **`axes_reverse = 1:2`, documenté, échouait** : `if()` sur une condition de longueur 2 (R ≥ 4.2).

⚠ **Un bogue de `tabxplor` 2.0.1, contourné ici, à corriger là-bas** : `tab_vars` à un seul niveau avec `totaltab = "no"` échoue (« Join columns in `x` must be present in the data »). ggfacto ne passe pas de `tab_vars` pour un bloc unique.

**Équivalence.** Un harnais compare `HEAD` et la phase sur 27 appels (`tea` à 18 variables et `pc_AGD` pondéré) : bulles des modalités et des profils, effectifs, classes, et chaque colonne de chaque couche construite. Tout est identique octet pour octet, hors les changements voulus (ids des classes, lignes `Remove_levels`, points des ellipses, et la colonne interne `group` de ggplot, renumérotée par l'ordre des niveaux de classe, couleurs inchangées).

**Tests** : le contrat porte sur le **contenu** et non plus sur la forme imbriquée. L'instantané de la bulle est repris sur le texte final, celui que `ggmca_plot()` assemble : mêmes cellules, désormais séparées par des sauts de ligne, lignes de contribution comprises. Nouveaux tests : ids par le nom (noms non alphabétiques), classes manquantes, pluralité pondérée, `individuals`, réponses exclues, ellipses sans profils, facettes, `axes_reverse = 1:2`, et une cellule empilée égale à la même cellule d'un `tab()` direct. **542 assertions** (488), 0 échec, 0 avertissement, ~34 s ; `check` 0/0/0.

**Ce que la phase n'a pas fait.** `hierarchical_clust()` reste à 7,4 s et en mémoire quadratique (phase 1o). `ggi()` reste à ~1 s, avec un widget de 3,6 Mo pour 5 000 profils. Le suffixe `_lv` de `tabxplor` apparaît encore côté colonnes dans les bulles (`breakfast_lv:`), et ce n'est pas nouveau.

#### Phase 1o — les classes de `HCPC()` en mémoire linéaire (DONE)

**Les mêmes classes, dix-sept fois plus vite, cent fois moins de mémoire.** Mesuré sur `pc_AGD` (9 234 individus pondérés, 15 variables actives, 5 312 profils distincts, `ncp = 3`, 6 classes), chaque variante dans son propre processus :

| Variante                               | Temps  | RAM au-delà de R |
|----------------------------------------|--------|------------------|
| `HCPC()` (avant)                       | 7,4 s  | +3,3 Go          |
| profils distincts + `flashClust`       | 2,2 s  | +0,65 Go         |
| profils distincts + `fastcluster`      | 0,43 s | ~+30 Mo          |

La feuille de route annonçait 1 Go : c'était 3,3 Go, le `dist()` et surtout l'`outer()` n × n des poids de Ward, et une machine de 32 Go tombait vers 30 000 lignes, pas 40 000.

**La clé : l'arbre de Ward sur les points distincts, sans matrice.** Des individus aux mêmes réponses partagent un point, que Ward fusionne d'abord, à coût nul : l'arbre au-dessus est le même sur les points distincts pondérés par la somme de leurs poids. `fastcluster::hclust.vector(members =)` le construit sans matrice de dissimilarités — mémoire linéaire, mesurée plate de 10 000 à 160 000 points (138 → 169 Mo de processus, R compris) —, et seul le temps reste quadratique : 1,5 / 5,8 / 23 / 93 / 379 s à 10 000 / 20 000 / 40 000 / 80 000 / 160 000 points distincts. Plus aucune taille de base ne fait tomber la machine ; une très grosse base est lente, c'est tout. Le reste est celui de `HCPC()`, repris avec crédit à ses auteur·ices : les points triés le long de l'axe 1 (l'ordre départage les ex æquo et fixe le départ des k-means), la règle de coupure automatique, la coupe à mi-hauteur entre deux nœuds, la consolidation par k-means **non pondérée** sur les individus, les classes numérotées le long de l'axe 1. L'analyse n'est plus réajustée : les `ncp` premiers axes de l'ajustement complet donnent les mêmes classes.

**Identique à `HCPC()` partout où on l'a mesuré.** `tea` (6 et 18 variables, `ncp` 2/3/5, `nb_clust` −1 et 2 à 9, avec et sans consolidation), `tea` pondéré, `pc_AGD` pondéré, les ACP de `mtcars` et de 3 000 lignes d'`ee_sal19`, les AC `PPP3 × CSPP` et `relig × partyid` sur leurs deux marges. Puis le cours : les 13 documents de `formations_stat` qui exécutent `hierarchical_clust()` (livre du M2S1, corrections, carnets d'exploration) ont été rejoués jusqu'à leur dernière classification, l'ancien chemin (réajustement + `HCPC()`) à côté du nouveau — **15 appels, 15 identiques**, dont deux ACP (10 272 lignes : 2,1 s contre 10,9 s) et un `consol = FALSE`.

**L'AC rejoint l'ACP et l'ACM.** `hierarchical_clust(ac, ncp, nb_clust, margin = "rows")` classe les modalités d'une marge, pondérées par leurs effectifs, comme `HCPC(cluster.CA =)`. Dans `mutate()`, chaque individu reçoit la classe de sa modalité, appariée par le nom (`NA` hors du tableau) ; hors de `mutate()`, une valeur par modalité, nommée. L'arbre d'une AC porte les noms de ses modalités.

**L'API.** `nb.clust` devient `nb_clust` — la fonction n'a jamais été publiée, donc sans alias —, dans le paquet comme dans tout `formations_stat` (cours, examens, carnets et leurs `.md`, la référence `agd.md` du skill d'exploration). `...` disparaît : il ne nourrissait que `HCPC()`, et `kk` n'a plus d'objet. `consol` et `margin` arrivent. L'arbre est redessiné comme `plot.HCPC(choice = "tree")` — gains d'inertie en haut à droite, un rectangle coloré par classe —, ses feuilles sont les profils, et le trait noir des 9 234 étiquettes superposées disparaît. `06-CAH.qmd` perd sa note sur `kk` et la RAM, et ses quatre `# long`.

**L'arbre par morceaux : mesuré, gardé hors de `R/`, dans `dev/hclust_chunked.R`.** Des tranches de points le long de l'axe 1, un arbre de Ward par tranche coupé en micro-classes, un arbre sur leurs centres pondérés, puis la consolidation — la voie de CURE ou de BIRCH. La qualité de Ward est la même (R² inter-classes égal ; même part d'individus plus proches d'un autre centre que du leur, ~1,5 % après consolidation, ~17 % sans, arbre exact compris : ces « isolés » tiennent à Ward, pas au découpage), et c'est quatre fois plus rapide à 20 000 points. Mais 0 à 26 % des individus changent de classe par rapport à l'arbre exact — autant que l'arbre exact lui-même quand on retire 1 % des individus au hasard (0,7 à 48 %). On ne peut donc le juger que sur la qualité, jamais sur l'égalité à `HCPC()`, et l'arbre exact en mémoire linéaire n'en aurait besoin que bien au-delà de 100 000 points distincts.

**Deux bogues trouvés.** Une ACM ajustée avec `ind.sup` voyait ses individus supplémentaires classés comme actifs — le réajustement perdait `ind.sup` — : elle est refusée, comme l'ACP. Et `HCPC()` rend les **lignes** d'une AC dans son propre ordre, trié le long de l'axe 1 (il teste `cluster.CA == "row"`, jamais vrai), ses colonnes dans le leur : les tests comparent par le nom.

**La consolidation pondérée, en option.** La consolidation de `HCPC()` ignore les poids de sondage : ses k-means comptent chaque individu une fois, même quand l'arbre a été pondéré. `consol = "weighted"` compte chacun selon son poids (poids de sondage, ou effectif d'une modalité d'AC) ; `TRUE` reste le défaut, donc les classes restent celles de FactoMineR. `stats::kmeans()` ne prend pas de poids : c'est un Lloyd à centres pondérés, parti des classes de l'arbre et exécuté sur les points distincts (des individus identiques partagent toujours leur centre le plus proche). Épinglé par deux faits : un poids de 2 vaut deux copies de l'individu, et chaque individu finit dans la classe du centre pondéré le plus proche. Sur `pc_AGD`, pondéré par `POND` : 13 à 15 % des individus changent de classe ; sans poids, 0,9 à 1,6 % seulement — l'écart tient donc aux poids, pas à l'algorithme. ⚠ Lloyd est un optimum local : l'inertie intra pondérée baisse à 6 classes (0,13167 contre 0,13238) mais monte un peu à 9 (0,09588 contre 0,09561), Hartigan-Wong trouvant parfois mieux même au critère pondéré.

**Au passage, R ≥ 4.1 redevient vrai.** `R/interpret.R` utilisait l'opérateur `%||%` de R de base, qui n'existe que depuis R 4.4, alors que `DESCRIPTION` annonce `R (>= 4.1.0)`. Les cinq usages sont réécrits sans lui : un défaut à `getOption()`, `$` sur `NULL`, `max()` sur un vecteur vide, deux `if`.

**Tests** : **560 assertions** (542), 0 échec, 0 avertissement, ~36 s ; `check` 0/0/0. **Dépendances** : `fastcluster` (+1 paquet, +0,3 Mo, aucune dépendance), `graphics` (paquet de base).

#### Phase 1p — un arbre pour toutes les coupes, un tableau qui lit l'analyse (DONE)

**Le geste reste une ligne dans `mutate()`, et ce qu'il faisait redonner disparaît.** Relevé dans le livre du M2S1, ses examens, 13 carnets d'exploration, `agd.md`, `ctall.R` et `pts_analysis.R` : l'arbre construit trois fois dans `06-CAH.qmd` pour une seule classification, les poids et les variables actives redonnés à chaque `clust_tab()` — le seul endroit où « les poids passent par un seul canal » cédait —, un bloc `fct_recode()` + `fct_relevel(sort)` pour nommer, `profiles = TRUE` écrit à chaque `ggmca(clust =)`, des barres de l'arbre qu'il fallait compter. Le document de conception, `dev/hierarchical_clustering.md`, porte le relevé, les mesures, les dix mécanismes pesés, le code de cours proposé et le branchement jamovi.

**L'arbre est construit une fois, coupé autant qu'on veut.** Mesuré sur `pc_AGD` : l'arbre 0,43 s, la coupe et la consolidation 0,05 s. La construction (`ward_tree()`) et la coupe (`cut_ward_tree()`) se séparent, et l'arbre est gardé pour la session sous une clé `rlang::hash()` de ce dont il est fait — coordonnées sur les `ncp` axes, poids, réponses : un arbre gardé ne peut pas être périmé, et une analyse refaite à l'identique le retrouve. 20 arbres par défaut, `options(ggfacto.clust_cache =)` pour en garder plus, `0` pour aucun. Les trois appels de 06-CAH passent d'environ 1,4 s à 0,66 s ; trois partitions dans un seul `mutate()` coûtent 0,67 s. Écartés, avec leurs raisons au document : l'arbre en attribut de la colonne (perdu par `[`, `factor()`, `case_when()`, périmé après un `filter()` — mesuré), une sous-classe vctrs (`if_else()` échoue contre un facteur ordinaire), un verbe qui recoupe une colonne existante, un `nb_clust` vectorisé, un cache disque. L'objet arbre explicite reste en réserve pour les expert·es (`clust_tree(res, ncp)` rendant un vrai `hclust`), et le même découpage construction/coupe est la couture d'un futur module jamovi, l'arbre dans le `$state` d'une Image.

**Nommer dans le même appel.** `hierarchical_clust(acm, ncp = 3, names = c("Petit écran" = 1, ...))`, dans le sens de `fct_recode()` (l'inverse, `"1" = "Petit écran"`, est accepté quand une seule lecture est possible), ou un vecteur sans noms dans l'ordre des classes. L'ordre du vecteur est celui des niveaux — plus de préfixes numérotés ni de `fct_relevel(sort)` — et `nb_clust` vaut le nombre de noms quand il n'est pas donné.

**Un arbre qui se lit.** Chaque barre des gains d'inertie porte le nombre de classes qu'elle fait, et le titre ce que garde la coupe : « 6 classes : la variance inter représente 68,6 % de la variance des axes 1 à 3 », calculée sur les individus, centres pondérés, après consolidation, et traduite avec les mots du cours (`clust_tree_caption()`).

**`clust_tab(res, data, clust)` lit l'analyse.** Même ordre que `ggmca()` : variables actives par défaut (`row_vars =` pour d'autres), poids de l'ajustement sous le nom de la colonne de l'utilisateur·rice (`res$source$wt` : la légende dit toujours « Weighted by POND. »), lignes analysées par `align_to_fit()` — une analyse d'un sous-ensemble se décrit avec la base entière. Une ACP donne des moyennes (cv) colorées en écarts-types (le Δ de Glass de tabxplor), la population passant sous le tableau : liée dedans, ses pourcentages rendaient chaque colonne `mixed`, et aucune moyenne n'était colorée. `shape =` (celui de `tab()`) coupe les nombres en modalités, `"sd_bands"` ou `"quintiles"`. Une AC renvoie au `tab()` à écrire. `gda_summary(footer =)` devient le seul poseur de pied de tableau.

**L'ancienne forme marche toujours.** `HCPC_tab(data, row_vars, clust, wt)` devient sa propre fonction dépréciée, et un `clust_tab()` qui reçoit d'abord un data frame y est ré-aiguillé par `sys.call()`, si bien que toutes les formes d'appel se lient comme avant ; la parité est épinglée. Chaque avis de dépréciation — ceux de `renamed_arg()` compris — tient désormais en une ligne et renvoie au guide, `https://bricenocenti.github.io/ggfacto/articles/ggfacto.html`. ⚠ C'est l'adresse que la vignette de la phase 1r doit publier.

**Sous-populations : le cadre existait, il manquait qu'on s'en serve.** La règle — filtrer dans le pipe de l'analyse, puis toujours donner la base entière — est celle de la phase 1m ; aucun document du cours ne l'employait. `res$source` garde désormais le nom de la base, et le refus le cite (« fitted on the 3 246 rows of `sub` »). Les pièges des carnets (`drop_na()` devant une ACM spécifique, `select()` qui efface l'identifiant, variables créées sur le sous-ensemble) sont au document ; l'extension des classes aux individus hors sous-population, par le centre pondéré le plus proche, y est proposée, pas implémentée.

**`profiles` s'allume avec `clust`.** `ggmca(acm, data, clust = cah)` dessine les profils colorés par classe ; `profiles = FALSE` l'emporte toujours.

**Deux bogues trouvés.** `pct = "row"` ne gardait que la première modalité d'une variable binaire, ce que seule la lecture en colonne permet. Et un `clust_tab()` à une seule variable-facteur échouait : tabxplor nomme alors sa colonne d'après la variable, pas `row_var`.

**Tests** : **609 assertions** (560), 0 échec, 0 avertissement, ~40 s ; `check` 0/0/0. **Dépendances** : `rlang (>= 0.4.10)`, pour `hash()`. Le cours (`formations_stat`) n'est pas touché : ses appels restent valides, avec l'avis de dépréciation.



#### Phase 1q — garder FactoMineR, mais le nourrir de profils de réponses (DONE)

**Recherche seule : rien dans `R/`, les tests, `man/`, `DESCRIPTION` ni `NEWS.md`.** Deux fichiers dans `dev/` : `analysis_engine.md`, la référence, et `analysis_engine.R`, le prototype — moteurs, harnais de parité, mesures —, une section par processus froid sous plafond mémoire, pour que chaque chiffre du document se rejoue d'une commande (son annexe B).

**Le verdict : ne pas vendoriser FactoMineR maintenant.** Passer au profil de réponses comme unité de l'ACM, FactoMineR restant le moteur ; demander d'abord en amont ce qui allégerait la dépendance ; garder prêt dans `dev/` un moteur natif d'environ 130 lignes, seul capable des très grands nuages. Critères GO/NO-GO du moteur natif dans le document (section 11) : deux tenus, un pas encore (à 10 questions, FactoMineR nourri de profils suffit), deux en attente — la réponse de l'amont, et le contact avec son mainteneur.

| Enquête Emploi, actifs occupés, EXTRI    | FactoMineR, individus          | FactoMineR, profils | moteur « lean » |
|------------------------------------------|--------------------------------|---------------------|-----------------|
| 1 million de lignes, 10 questions        | 36 s · 5,5 Go · objet 2,4 Go   | 4,0 s · 0,75 Go     | 0,25 s · 0,2 Go |
| 1,87 million, 15 questions (608 089 profils) | ~106 s · ~18 Go (extrapolé) | 32 s · 6,1 Go      | 2,1 s · 0,8 Go  |

**Ce que coûte la dépendance.** 49 paquets / 67,3 Mo (25 compilés) de l'arbre de ggfacto, et 1,0 s au premier appel d'une session — `library(ggfacto)` ne le charge pas. `car` seul en apporte 27 / 42,4, pour des fonctions que ggfacto n'appelle jamais ; un FactoMineR réduit à son cœur n'en coûterait que 2 / 4,4, mais garderait 0,5 s de chargement par `irlba` → `Matrix`. ⚠ Passer FactoMineR en `Suggests` ne sauve rien : le cours installe avec `dependencies = TRUE`.

**Le profil est exact.** Des individus aux mêmes réponses ont la même ligne du tableau disjonctif : fusionnés, ils laissent X′X inchangé. Mesuré contre `FactoMineR::MCA()` passé par l'ingress de ggfacto, sur `tea`, `pc_AGD` pondéré et l'Enquête Emploi, avec et sans `excl` : 1e-13 au pire, 1,1e-11 par la voie de Burt ; seules divergences, les signes instables de FactoMineR lui-même. Sur l'Enquête Emploi, les profils font 3 % des lignes à 8 questions sur dix ans, 8 % à 10, 32 % à 15 (65 % une fois retirés les passages répétés du panel). Le lien individu → profil coûte 0,1 s à un million de lignes. **La clé manquante à grande échelle est un découplage** : ggfacto demande tous les axes pour le tableau des valeurs propres, et FactoMineR calcule alors trois matrices n × (K − Q) que personne ne lit. Le tableau de Burt (K × K) donne toutes les valeurs propres, et les coordonnées des seuls axes demandés.

**Les bulles se calculent sur le tableau de Burt** (idée du mainteneur, en cours de phase) : les tableaux croisés de `active_tables = "active"` en sont les blocs. Construits depuis les tableaux de Burt pondéré et non pondéré des profils, puis confiés à `tabxplor::fmt()` pour la couleur et le format, ils reproduisent les 1 482 cellules de `pc_AGD` à l'identique — effectifs, pourcentages, écarts, codes couleur et texte — dix fois plus vite (0,81 → 0,08 s), 4,4 fois à un million de lignes. C'est le plus gros gain à portée d'un·e étudiant·e : le moteur, lui, ne pèse que 0,13 s d'une session de cours d'environ 6 s. data.table, même sur 12 fils, n'accélère rien : ces briques sont déjà en C.

**La compatibilité est concrète.** Un objet à la forme de FactoMineR passe factoextra, explor et GDAtools sans FactoMineR chargé, pourvu que `"MCA"` reste la première classe (GDAtools teste `class(x)[1]`). Sans FactoMineR installé, `print()` du même objet déroule 11 767 lignes, et les deux `plot()` du cours échouent. ⚠ ggfacto refuse aujourd'hui un `GDAtools::speMCA()` (`ggmca()`, `mca_interpret()`, `benzecri_mrv()`) : les branches GDAtools de `varsup()` ne servent à rien.

**Neuf bogues de FactoMineR 2.16 reproduits**, rédigés en brouillons d'issues à déposer par le mainteneur (annexe A) : `level.ventil` bouclant sans fin sur un facteur ordonné, l'avertissement des NA jamais émis, `na.method = "Average"` cassé, le repli `eigen()` défectueux, le signe laissé à LAPACK à un seul axe, les `v.test` gonflés par l'échelle des poids, `irlba` dépendant de la graine, `CA()` sur une ligne nulle avec `col.sup`, et un poids nul qui fait planter `MCA()` — cas ordinaire en enquête (EXTRI = 0), donc `MCA2(wt =)` aussi. L'amont répond vite : l'issue #41 a été corrigée le lendemain, et la 2.17 abandonne `ggtext`.

**Trois bogues latents de ggfacto trouvés, non corrigés** : `ggmca_3d(axes = 1:2)` échoue toujours (`select(-"Dim.3")` sur des colonnes `"Dim 3"`) ; une question aux modalités `y`/`n`, que FactoMineR renomme `var.y`, disparaît du modèle de `ggmca_data()` ; les ellipses d'une analyse pondérée ignorent les poids (`stat_ellipse(type = "t")`).

**Un manque d'attribution, quel que soit le verdict.** `varsup()` est copié de GDAtools et `R/clust.R` dérive de `HCPC()`, mais `Authors@R` ne nomme que le mainteneur, et il n'y a pas d'`inst/CITATION`. La politique du CRAN demande des rôles `ctb` (et `cph`) : à régler avant la 0.4.0, avec une citation qui renvoie à FactoMineR (JSS 2008), à Le Roux et Rouanet et à Benzécri.

**Au passage**, les lignes FactoMineR de `dev/dependency-audit.md` (39 paquets / 46,8 Mo, et « laissé tel quel : il est dans le titre ») renvoient désormais au nouveau document, avec les chiffres de l'arbre d'aujourd'hui.

**Ce que la phase n'a pas fait.** Aucune ligne de code du paquet, donc aucune suite relancée (609 assertions, inchangées). La phase suivante naturelle est un lecteur unique de l'analyse, centré sur les profils, avec FactoMineR pour moteur (section 11 du document).



#### Phase 1q-ii — les crédits, et quatre bogues latents (DONE)

**Le premier point du verdict de 1q, appliqué** : les crédits, les bogues latents de ggfacto, les textes pour l'amont. Suite : **632 assertions** (609), 0 échec, 0 avertissement, ~39 s ; `check` 0/0/0. **Le mainteneur a tranché** : FactoMineR reste le moteur, nourri des profils de réponses, et le moteur natif n'est pas poursuivi. La section 11 du document le dit désormais ; ses critères GO/NO-GO ont disparu avec la question.

**Les crédits.** `Authors@R` nomme en `ctb` et `cph` Nicolas Robette, pour `varsup()`, copié de GDAtools, et François Husson, Guillaume Le Ray et Quentin Molto, auteurs du code de `HCPC()` dont `R/clust.R` dérive la règle de coupe et le dessin de l'arbre. Le commentaire de `R/clust.R` nommait à tort les auteur·ices de la méthode. `inst/CITATION` cite ggfacto, puis FactoMineR (JSS 2008, sous son vrai titre : celui du `bibentry` de FactoMineR est fautif), Le Roux et Rouanet (2004, 2010), Benzécri (1979), Husson, Josse et Pagès (2010) et GDAtools. La règle du CRAN est citée dans le document, section 9.

**`mca_levels()` : les modalités d'une ACM lues par leur position** (`R/ingress.R`). FactoMineR renomme une modalité partagée par deux variables en `var_lv` (jusque dans `call$X`) et une modalité `y`/`n`/`Y`/`N` en `var.y` (dans `var` et `marge.col` seulement). Toutes les jointures par nom perdaient donc une question y/n :

- dans `ggmca()`, où elle disparaissait (G2) ;
- dans `mca_interpret()`, où son groupe valait `NA` ;
- dans `ggmca_initial_dims()`, qui échouait ;
- dans `ggmca_with_base_ref()`, qui perdait aussi la fréquence des modalités partagées.

Un seul lecteur positionnel sert désormais les quatre, et `mca_interpret()` affiche le nom de la donnée plutôt que celui de FactoMineR.

**Les poids nuls ou manquants.** FactoMineR plante sur un poids nul en ACM (B9), donne une coordonnée infinie en ACP et plante sur un poids manquant. `usable_weights()` laisse hors de l'ajustement une ligne de poids nul, avec un message, et `res$source` enregistre les lignes restantes : `mutate(hierarchical_clust())` met donc `NA` sur elles, et `ggmca()` reprend la base entière. Un poids manquant ou négatif est refusé en clair. Les individus supplémentaires d'une ACP sont renumérotés.

**`ggmca_3d(axes = 1:2)` échouait toujours** (G1) : `select(-"Dim.3")` portait sur des colonnes nommées `"Dim 3"`. La ligne était morte, puisque le mode 2D n'utilise pas ce tableau, et elle disparaît ; `scene =` n'est plus passé aux traces 2D, qui avertissaient. Le mode 2D n'avait jamais fonctionné au-delà de cette erreur.

**Les ellipses d'une analyse pondérée ignoraient les poids** (G3). Les deux couches portent désormais `weight = row.w` : `stat_ellipse()` pondère `cov.trob()` depuis ggplot2 4.0.0, minimum relevé en conséquence (ggiraph 0.9.6 l'exige déjà). Sans poids, l'ellipse est identique, et c'est épinglé.

**R ≥ 4.3 déclaré**, le plancher de FactoMineR 2.16 : l'ancien `R (>= 4.1.0)` n'était vrai qu'avec un FactoMineR plus ancien.

**L'amont.** Trois textes prêts à déposer par le mainteneur, dans `~/Documents/ggfacto_upstream/` : les neuf bogues de FactoMineR en une issue, deux propositions (`Suggests` ; profils et valeurs propres complètes) à déposer après la réponse, et une note à Nicolas Robette. Rien n'est déposé.

**La recherche pour la phase suivante**, versée au document (sections 5.1 et 7.1) :

- `FactoMineR::svd.triplet()`, exporté, décompose le tableau de Burt de ggfacto à 9e-14 de `MCA()`, en 3,5 s et 0,7 Go à 608 089 profils : c'est la voie légitime pour garder FactoMineR comme moteur à toute échelle.
- FactoMineR nourri des profils égale son ajustement sur les individus à 5e-13.
- Un objet ajusté sur les profils passe tout l'écosystème, sauf les fonctions de GDAtools qui reçoivent une variable par individu. `call$X` et `call$Xtot` restent, puisqu'explor les lit. Le seul amaigrissement simple garde `ind` et `svd$U` sur les premiers axes, comme le fait FactoMineR pour `excl` + `ncp`.
- ⚠ Chargement : sans `car`, `emmeans` ni `multcompView`, les imports de FactoMineR prennent encore 0,96 s (`showtext`, `sysfonts`, `irlba` → `Matrix`). Le tableau de la section 8 du document, qui annonçait 0,6 s de gain, est corrigé.


#### Phase 1q-iii — le profil de réponses comme unité : un seul lecteur, FactoMineR pour moteur (DONE)

**FactoMineR est nourri des profils de réponses, et une ACM n'est plus lue qu'à un seul endroit.** `multiple_correspondence_analysis()` passe à `FactoMineR::MCA()` les combinaisons distinctes de réponses actives, pondérées par la somme de leurs individus, et range dans `res$source` la clé individu → profil (`key`) et les poids individuels (`w`). L'objet reste celui de FactoMineR, classe comprise, avec une ligne de `$ind` par profil. Un seul lecteur, `mca_model()` (`R/model.R`, neuf), sait comment chaque moteur range une ACM — ajustement ggfacto, `FactoMineR::MCA()` sur individus, `GDAtools::speMCA()` et `csMCA()` —, et tous les consommateurs le lisent : modèle du graphique, bulles, classes, tableau d'interprétation, graphiques pédagogiques, alignement. Les 13 lectures de `call$X` et les 7 de `call$quali` n'existent plus qu'en lui, et trois dérivations des variables actives qui divergeaient sur un ajustement FactoMineR brut ont disparu. `varsup()` (95 lignes), `mca_levels()`, `answer_profiles()`, `level_counts()` et `level_clusters()` disparaissent ; `stacked_crosstab()` rejoint `dev/stacked_crosstab.R`, piste pour accélérer tabxplor lui-même.

| pc_AGD (9 234 individus, 5 312 profils)          | avant  | après       |
|--------------------------------------------------|--------|-------------|
| `MCA2()`                                         | 0,13 s | 0,10 s      |
| `ggmca_data(active_tables = "active")`           | 1,05 s | 0,17–0,31 s |
| `ggmca()` (bulles Burt désormais par défaut)     | 1,01 s | 0,22 s      |

| Enquête Emploi, 1 million d'actifs, 10 questions | avant (recherche) | après   |
|--------------------------------------------------|-------------------|---------|
| `MCA2()`                                         | 36 s, 5,5 Go      | 4,7 s, 1,2 Go |
| objet ajusté                                     | 2,4 Go            | 327 Mo  |
| bulles de Burt                                   | 3,8 s (le `tab()` empilé seul) | 0,87 s (`ggmca_data()` entier) |

**Exact.** Sur `tea[1:6]` pondéré, avec valeurs manquantes et `excl` : valeurs propres, coordonnées, contributions, cos2, v.test et η² des modalités, coordonnées individuelles, tous à moins de 1e-10 de FactoMineR sur les individus (3e-16 à 1e-14 mesurés). Le modèle d'un ajustement sur profils égale celui d'un ajustement sur individus, et le modèle du graphique aussi. Les classes restent celles de `HCPC()` — la référence des tests est désormais `HCPC()` sur un ajustement individuel (`mca_ind()`), puisque `HCPC()` sur l'objet classerait les profils.

**Les bulles se calculent sur les profils, cellules `tabxplor::fmt()`** (décision du mainteneur) : un `rowsum()` par variable en colonne sur les « unités » (profils × réponses non actives), les effectifs, pourcentages en ligne et écarts au Total du bloc calculés ici, couleur et texte laissés à tabxplor. `tabxplor::tab_counts()` existe mais, sur les blocs de Burt, ne va pas plus vite qu'un `tab()` empilé (0,5 s sur `tea[1:18]`). La parité est épinglée cellule par cellule contre `tab()` (`expect_cells_equal_tab()`, poids non dyadiques) : effectifs exacts, le reste à 2e-16, couleurs et texte identiques. **Sept bogues des bulles disparaissent** : l'effectif d'en-tête venait de la dernière variable en colonne ; le point central perdait les individus manquants de la dernière variable supplémentaire (« n=274 : 91 % » au lieu de 100 %) et la première modalité de chaque variable d'infobulle ; des lignes « Remove_levels » s'imprimaient ; la coupe des questions à deux modalités ne prenait jamais sur une batterie oui/non ; une modalité nommée `n` faisait planter les bulles ; `n=1e+05`. **Une ligne est étiquetée par sa modalité** : l'artefact `breakfast_lv` (un nom de colonne de tibble) disparaît, une modalité partagée par plusieurs variables se lit `no (lunch)` — seul changement de l'instantané. Un nombre dans `tooltip_vars_1lv` imprime enfin sa moyenne pondérée, comme la documentation le promettait (il échouait).

**Les modalités supplémentaires** sont le barycentre pondéré de leurs individus sur √λ, sommé sur les unités : la formule de `varsup()`, non arrondie (5e-7 d'écart). **`individuals` reste à une ligne par individu** : `calculate_ellipse()` de ggplot2 dimensionne son rayon sur `nrow - 1` degrés de liberté et `MASS::cov.trob()` teste sa convergence sur des poids absolus — l'affirmation contraire de `dev/analysis_engine.md` (4.5) était fausse, corrigée. Les points dessinés sont les profils distincts par leurs réponses **conservées** : identique sur tous les jeux d'essai, et `cleannames` ne fusionne plus deux points distincts.

**L'API.**
- `interpret(res)` : un verbe pour les trois analyses, méthodes d'une ligne vers `mca_interpret()` et `pca_interpret()` (exportées, alias muets, publiques en 0.3.2) et `ca_interpret()` (interne, jamais publiée) ; une seule page `?interpret`.
- `axis_coord(res, axes)` dans `mutate()`, comme `hierarchical_clust()` : ACM (coordonnée du profil de chacun), ACP (individus supplémentaires compris), AC (coordonnée de sa modalité) ; plusieurs axes donnent plusieurs colonnes, nommées par `axes`. Les deux partagent l'écriture sur les lignes, `to_data_rows()`.
- `active_tables = "active"` par défaut : le pari du paquet devient le comportement par défaut (`NULL` pour aucune bulle).
- `sup_vars`, `tooltip_vars`, `tooltip_vars_1lv` prennent des noms nus, comme dans `tab()` ; chaînes et vecteurs de noms restent acceptés sans avis (`select_vars()`).
- L'ingress refuse une seule variable active (FactoMineR échoue sur deux profils) et `ind.sup`, `quali.sup`, `quanti.sup`, `tab.disj` dans `...`.

**GDAtools.** `speMCA()` et `csMCA()` sont acceptés partout où une ACM l'est (GDAtools en `Suggests`, tests seulement). GDAtools oriente autrement certains axes, arrondit ses coordonnées et contributions à six décimales, et complète ses valeurs propres de zéros numériques (1e-32), que le modèle écarte. Un `csMCA()` est lu sur son sous-nuage, ses classes écrites sur ses lignes, `NA` ailleurs. `speMCA()` et `MCA2(excl =)` donnent le même modèle au signe des axes près.

**Crédits.** Plus aucun code copié de GDAtools : l'entrée de Nicolas Robette quitte `Authors@R` (décision du mainteneur), l'en-tête GDAtools d'`inst/CITATION` dit que ggfacto lit ses ajustements, et la note `~/Documents/ggfacto_upstream/03_GDAtools_note.md` est réécrite.

**Trouvés au passage.** `ggmca_with_base_ref(axes =)` titrait toujours les axes 1 et 2. `mca_interpret()` bornait les axes sur `eig` et non sur les colonnes de `$var$coord`.

**Hors du paquet.** Le carnet `pc08_gouts-musicaux-et-cinema.qmd` passe à `axis_coord(acm, axe)[ok]`, rendu à nouveau : η² identiques (33,8 / 10,1 / 26,5 sur l'axe 1). ⚠ Cassent et restent au mainteneur, dans `socio_public_services` : `HCPCprofiles()` et `mca_var_table()` (`Demarrage.R` et leurs copies), qui lisent `call$X`, `row.w` et `ind$coord` par individu, et les `FactoMineR::HCPC(res.mca)` réécrits par position (`ctall.R`), qui échoueront sur la longueur — à passer à `axis_coord()` et à `mutate(hierarchical_clust())`.

**Tests** : **1 787 assertions** (632), surtout les cellules des bulles une à une, 0 échec, 0 avertissement, ~47 s ; `test-model.R` neuf ; `check` 0/0/0. **Ce que la phase n'a pas fait** : le moteur B (`svd.triplet()` sur Burt) reste dans `dev/` ; l'objet n'est pas amaigri (`ind` sur les premiers axes) ; une variable supplémentaire à un million de lignes coûte encore 3 s, l'alignement et le groupement sur les individus.

#### Phase 1q-iv — un verbe graphique et un modèle de graphique pour les trois analyses (DONE)

**La clé manquante était du côté des graphiques.** Seule l'ACM avait un modèle de graphique et un moteur de rendu. `ggca()` était une fonction monolithique de 550 lignes à bulles maison, et une ACP n'avait aucune carte des individus : le cours passait par `plot(acp, choix = "ind")` + `gridExtra`. Désormais les trois analyses bâtissent le même modèle, que dessine un seul moteur. Un seul verbe, `ggfacto()`, s'enseigne à côté d'`interpret()` :

```r
ggfacto(acm, pc_AGD, sup_vars = c(SEXE, AGE), clust = cah, interactive = TRUE)
ggfacto(correspondence_analysis(tab(gss, c(relig, marital), c(partyid, race))) )
ggfacto(acp, ee, sup_vars = c(SEXE, CSTOTR), clust = cah)
```

**`ggfacto()`** est un générique S3, comme `interpret()`, qui aiguille vers `ggmca()`, `ggca()` ou `ggpca()`.
- Sa signature porte les arguments partagés, pour l'autocomplétion : `data`, `sup_vars`, `clust`, `axes`, `axes_reverse`, `type`, `profiles`, `active_tables`, `ellipses`, `title`, `xlim`, `ylim`, `text_size`, `size_scale_max`, `lang` et `interactive`. Le reste passe par `...` vers la fonction de l'analyse, qui le documente.
- Un argument qu'une analyse ne prend pas est refusé en clair, jamais ignoré. Pour une AC, ce sont `data`, `sup_vars`, `profiles`, `active_tables` et `ellipses` ; pour une ACP, `active_tables`.
- `interactive = TRUE` rend le widget. Par défaut, c'est le ggplot, que l'expert complète avec `+` avant `ggi()`.
- Pas de méthode `plot()` : FactoMineR possède `plot.MCA/CA/PCA`, et la redéfinir casserait `plot(acp, choix = "var")`. `autoplot()` a été écarté, au profit d'un verbe au nom du paquet.

**Le modèle partagé** (`R/plot-model.R`) :
- une colonne `role` (`active`, `sup`, `clust`, `central`) dit ce qu'est une ligne, et `color_group` NA vaut encre neutre (`na.value = "black"`) : les sentinelles `"active_vars"` et `"Central point"` disparaissent du code ;
- des aides partagées, dont les barycentres (÷√λ en ACM, non en ACP), les groupes de couleur, les points du nuage (triés du plus lourd au plus léger, à égalité par une suite de Weyl, pour qu'un plafond sur des poids égaux donne un échantillon régulier), les classes à la pluralité pondérée et la table des individus des ellipses ;
- **un moteur**, `ggmca_plot()` (déplacé dans `R/plot-render.R`) ;
- **trois lecteurs** : `mca_model()`, `pca_model()`, `ca_model()`.

Les bulles sont des blocs (`tip_block()`) sur un même constructeur (`R/tooltips.R`).

**L'AC.**
- **Ses variables supplémentaires voyagent dans son tableau** : dans un `tab()` à plusieurs variables en ligne ou en colonne, la première de chaque est active et les autres passent en `row.sup`/`col.sup` par position. Une matrice garde les `row.sup`/`col.sup` de FactoMineR, et un ajustement FactoMineR brut voit ses suppléments dessinés.
- `res$source` note, par position dans `call$Xtot`, la variable et le nom de chaque modalité : FactoMineR `make.unique()` les noms.
- Les bulles montrent le profil de chaque modalité sur chaque variable de l'autre marge, en deux passes. Elles sont épinglées cellule par cellule contre `tab(pct = "row")`. ⚠ Les fréquences viennent des marges, car empiler les blocs compterait deux fois.
- Les classes d'une marge (`clust = hierarchical_clust(ac, …)`) colorent ses modalités. Leur étiquette est au barycentre pondéré par les masses, qui est la projection supplémentaire de la catégorie fusionnée (vérifié contre FactoMineR).
- `CA2()` est un vrai alias.
- Les anciens arguments de `ggca()` gardent leur place :
  - `rowcolor_numbers` et `colcolor_numbers` deviennent `color_groups` ;
  - `filter`, qui n'était jamais lu, devient `discard_levels` ;
  - les sous-titres sont inertes, avec un avis.
- Un appel ancien tourne, mais ne ressemble plus au même graphique : couleurs par variable, suppléments dessinés, nouvelles bulles, plus de formes par marge.

**L'ACP.**
- `ggpca()` dessine le nuage des individus.
- Les modalités supplémentaires sont au barycentre, égales à `quali.sup` à 1e-10.
- Les bulles donnent la moyenne de chaque variable active, colorée par l'écart standardisé (Δ de Glass, comme `clust_tab()`), avec un `fmt` par modalité contre le total de son bloc, et s'accordent avec `tab()`.
- Les individus sont montrés avec leurs valeurs colorées et leur nom si les données en ont un.
- `ggpca_cor_circle()` rend un ggplot ; `interactive = TRUE` rend le widget.
- `wt` et `ind_name` de `PCA2()` passent par tidyselect.

**Le reste.**
- **Chaque graphique 2D porte la classe S3 `ggfacto_plot`**, qui survit à `+`, `ggplot_build()`, `ggsave()` et `girafe()`. `knit_print.ggfacto_plot()` le dessine à la largeur du chunk et à sa propre hauteur, par `include_graphics()` : les légendes et les renvois Quarto fonctionnent, vérifié en rmarkdown et en Quarto. Un `fig.height` ou un `fig.asp` explicite l'emporte, détecté contre le défaut du document ; `results = "hide"` retombe sur le tracé de knitr, qui y garde un graphique mais y jette une image. Les `fig.height = 8*1.141925` du cours peuvent partir.
- **La taille des points est calculée** : `sqrt(max/médiane)` des poids dessinés, bornée entre 4 et 20, 1,5 à poids égaux. Mesuré : tea 4 (inchangé), pc_AGD 14,1, là où le cours avait choisi 12 à la main.
- **Les graphiques parlent la langue de la session** (`lang =`, gettext `R-ggfacto`, 51 messages ; « Point moyen », le mot du cours). Le modèle porte sa langue.
- **Les graphiques se taisent**, sauf les groupes de couleur sous `options(ggfacto.verbose = TRUE)`.
- **`eigenvalues(res)`** donne le tableau des valeurs propres seul, par le même `eig_tab_of()` que les trois `interpret` ; l'instantané d'`interpret` n'a pas bougé d'un octet.
- `ggi()` laisse passer un widget.

**Bogues trouvés et corrigés (tous reproduits).**
- `type = "active_vars_only"` échouait.
- `out_lims_move = TRUE` ne déplaçait rien : il pose désormais les modalités sur les bords.
- `keep_levels` recyclait un vecteur de regex.
- `need_data()` nommait `ggmca` dans `ggmca_data()`.
- `ggmca_with_base_ref(axes = c(1, 3))` échouait.
- Dans `ggca()`, trois bogues : les étiquettes repoussées perdaient leur bulle, `filter` n'était jamais lu, et une faute de frappe dans `type` échouait sans rien dire.
- Le cercle ne lisait pas l'axe 10.
- `ggi(widget)` échouait.
- `ca_interpret()` comptait les suppléments dans son `n`.

**Tests** : 2 354 assertions (1 787), 0 échec, 0 avertissement, environ 60 s. Nouveaux fichiers : `test-ggca.R`, `test-ggpca.R` et `test-ggfacto.R`, plus les tests du knit, de la taille et de la langue. L'instantané des bulles d'ACM ne change que par « Contrib axe 1: » (le deux-points perd son espace) ; ceux de l'AC et de l'ACP sont nouveaux. NEWS.md n'a pas été touché.

**Ce que la phase n'a pas fait.**
- Une échelle de taille unique : `type = "points"` avec des profils les laisse petits.
- Les individus supplémentaires d'une ACP et le `quali.sup` d'une ACP FactoMineR brute ne sont pas dessinés.
- La mise en page de bookdown n'est pas vérifiée.
- **Phase 1r** : `?ggfacto` est la page du générique ; une page `_PACKAGE` ne prendra que `@aliases ggfacto-package`.
- **Phase 1s** : le cours passe à `ggfacto()`, `eigenvalues()`, la hauteur automatique des figures et `clust_tab(res, data, clust)`. Les `ca_interpret()` de `04-AC.qmd` deviennent `interpret()`.

**Après relecture du mainteneur.**
- **L'ACP se lit sur son cercle.** `ggfacto(acp)` seul dessine le cercle des corrélations. Demander des individus, des modalités supplémentaires ou des classes dessine le **biplot** : les mêmes flèches, remises à l'échelle du nuage (le cercle en tient les 90 % les plus proches), avec une légende qui dit de n'en lire que les directions. Les deux cercles sortent du même `variable_vectors()` et du moteur partagé : même encre sobre (`#34515e`) pour le cercle et les flèches, projections au survol. Dans le biplot, chaque nuage garde son échelle : les valeurs des axes, en gris, sont celles des individus, et le cercle porte ses graduations −1 / 1 dans l'encre des flèches. La légende « lire les directions » a disparu, les graduations suffisent. La bulle d'une variable s'ouvre sur sa moyenne (cv), et le point moyen donne n puis la moyenne (cv) de chaque variable. Une flèche s'allume avec son nom (au survol, un texte est coloré par `fill`, en or foncé sur la boîte jaune clair, là où il s'y noyait) ; les projections, tiretées comme les axes, ne portent plus d'id de survol, si bien qu'aucune ne capte le pointeur : dessinées transparentes, elles s'affichent en or par une feuille de style que `ggi()` ajoute (`:has()`), le temps que leur flèche est survolée. Les graduations −1 / 1 sont en gras, à l'intérieur du cercle, avec des tics vers l'intérieur. La bulle d'un individu (ou d'un profil) donne ses coordonnées sur les deux axes dessinés. `ggpca(variables = FALSE)` retire les flèches. `ggpca_cor_circle()` passe par le moteur, et prend désormais `axes_reverse`, `title` et les limites.
- **Les modalités supplémentaires d'une AC** s'écrivent en noir et en italique, sans point. **Et l'italique marque une modalité supplémentaire dans tous les graphiques** (`sup_in_italic = TRUE` par défaut, étiquettes comprises), puisque la couleur ne dit pas la même chose d'une analyse à l'autre ; les classes restent droites.
- **Survoler une modalité allume toutes celles de sa variable**, supplémentaire comme active : un id par variable.
- **Deux bogues de bulles venaient de ggiraph.**
  - Une bulle commençant et finissant par une balise est prise pour du html brut, et ses lignes fusionnent ; c'était le cas des individus d'une ACP dont la dernière valeur était colorée.
  - Près du bord droit, une bulle se tassait et se repliait.
  - Corrigés par une espace finale (`ggiraph_text()`) et par `white-space:nowrap`.
- ggrepel 0.9.8 nomme la bordure d'une étiquette `linewidth` ; `label.size` n'y fait plus rien.

#### Phase 1r — rework filters and subsets 

The current framework is : "**Sous-populations : le cadre existait, il manquait qu'on s'en serve.** La règle — filtrer dans le pipe de l'analyse, puis toujours donner la base entière — est celle de la phase 1m ; aucun document du cours ne l'employait. `res$source` garde désormais le nom de la base, et le refus le cite (« fitted on the 3 246 rows of `sub` »). Les pièges des carnets (`drop_na()` devant une ACM spécifique, `select()` qui efface l'identifiant, variables créées sur le sous-ensemble) sont au document ; l'extension des classes aux individus hors sous-population, par le centre pondéré le plus proche, y est proposée, pas implémentée."

Je n’aime pas trop stocker le nom de la source de données, ce n’est pas fiable, c’est une sorte de hack. Maintenant que nous avons un framework qui est answer-profiles-centric, les lignes d’individus de la base de données sont de toute façon sans arrêt à matcher avec les lignes de profiles de réponse de l’ACM : je voudrais que tu intègres le `filter()` / rows subset dans cette logique de manière fiable, robuste et lisible. L’idée est que la base de données de départ reste la réference, et si possible que le même integer vector permette de matcher les lignes de l’ACM et les lignes de la base de données, de manière fiable, en permanence (avec des tests performants et peu couteux pour si l’ordre des lignes change ou s’il en manque etc.). 

Pour cela :
- Garder la possibilité de détecter les `filter()` et les `data[...,]` and the like  passés à l’input (argument data, native pipe |>, support for %>% if it’s simple and reliable only which I doubt), créer le vecteur qui permet de matcher le base de données totale à la base de données filtrée ?
- Ajouter un argument `filter =` comme dans `tabxplor::tab`. J’aime moins la syntaxe,j’apprendrais la première solution, mais il faut que ce soit possible car c’est plus fiable dans certainses situations.

Ensuite, pour ggfacto() etc., on passe la base de données initiale pour les varsup, et c’est un vecteur est utilisé pour matcher les deux ?

Make many small tests with edge cases to see where it work and where it breaks.



#### Phase 1s — teach the new usage in `formations_stat`

The new aliases and API for the three analyses, the new `interpret()` and `ggfacto()` workflows.
`clust_tab()` : look at `dev/hierarchical_clustering.md`
Sous-populations des ACP AC ACM avec `filter()`, mais on travaillant passant toujours le data.frame de départ (pour les variables sup sur les graphiques, pour ajouter des variables coordonnées et clusters dans la base de données, etc.).




#### Phase 1t — vignette and pkgdown site

Look at `/home/dev1/github/tabxplor/` vignettes and pkgdown site : I want the same kind of pkgdown site for ggfacto, except it will be **much more concise**.

Vignette : only one vignette, with an english and a french versions (start with the french one), and it shall be shorter than tabxplor vignettes, straight to the point, showing the three analyses (PCA/CA/MCA) workflow briefly. 
- Use the same polished workflows that in my AGD course at `/home/dev1/github/formations_stat/cours/M2S1/livre/` to teach the three main principal_component_analysis, correspondence_analysis, multiple_correspondence_analysis with clustering workflows, but with the data on ggfacto’s functions current examples. 
- Show the specificity of the package, staying close to the data : having means and coefficient of variations in PCA ; looking at the crosstables in CA (`tabxplor::tab(color = "contrib")` + interactive tooltips) ; in MCA, using interactive tables to look at the profiles and at the burt table with colored deviations, then doing HCPC with HCPC table and colored clusters (this second one not interactive, because too much interactive tables will make the webpage too big).
- For the French version, look at what vocabulary and style I use in `/home/dev1/github/formations_stat/cours/M2S1/livre/`, except it’s not a course so you shall be straight-to-the point. Then, translate to english, ensuring you use widespread geometrical data analysis vocabulary, and idiomatic english, never word-to-word translation from French.
- Note : the CA example should have enough row levels and enough col levels, not marital × race, more relig × partyid (excluding missings, "No answer", etc. to get a consistent enough result) ?

README and pkgdown site index should be very quick :
- for pkgdown site, only the tea MCA example with the interpretation table for axes 1 and 2 and the interactive plot with HCPC clusters colored. Precise it’s mostly just a wrapper around `FactoMineR::` and thank their authors (and state concisely, in passing, that tea data is FactoMineR’s).
- Github readme can’t have custom html elements and detailed formatting : make it very short, with a bit of text, and point to the pkgdown site.

In the pkgdown site, organise the functions in the "Reference" page in a user-friendly way, adapted to usage, with main functions at the top ordered by kind of analysis (everything about PCA in the same group, analysis, *_interpret, plots ; them everything about CA ; them everything about MCA, including HCPC and HCPC table), then main stuff (ggi, etc.), then secondary stuff (like pedagogical plots), helpers and details.




#### Phase 1u — 0.4.0 release

Help me do the new CRAN release, so I don’t have to check everything myself : I want you to plan for everything, and only let me accept the pull request on github.com and do the `devtools::submit_cran()` myself. You can commit (but you do not sign the commits), you can push : I’ll have a harness permission asked, that’s all.

Look at `/home/dev1/github/tabxplor/dev/release_checklist.md` and create a release checklist for ggfacto : remove everything useless here because it is only useful in tabxplor (next time, simply giving a prompt like "help me release v 0.x.x" should be enough).

On `dev/` branch, we’ll do R CMD CHECK, then github actions and rhub (use the same rhub platforms than tabxplor ?), and everything else needed for the new release. (No reverse dependency exists.)

Look at dev history, and write a very extremely concise NEWS.md, presenting new functions and arguments very shortly ; only the two or three more important bug corrections and detail changed, nobody cares really.

Then, release branch, pull request, new github actions.

For my message to CRAN, reuse `/home/dev1/github/tabxplor/cran-comments.md` and modify it, and change the rhub and github actions links once you have them.

At the end, when everything is ready, I’ll I’ll add win-builder link in CRAN comments and submit to CRAN myself.

github release when CRAN have accepted.



---

## The last step of every implementation: update the documentation

**You should always start updating docs and writing the "DONE" summary while the final test suite runs**, since it’s now quite long (if a fix follows a failure, correct the affected docs once it passes).
- Keep everything **present-tense and concise**. Never clutter the docs with dev history (the ONLY place dev history is allowed is the "DONE" summary).
- Always respect the **documentation ecosystem** hierarchy (top of this file).
- Edit the files yourself (never hand the maintainer lines to paste).
- If you use a plan, do a real **documentation planning work** : define what goes where, avoid duplication, state what level of details and what focus the lines written in each document should have.

1. **File-header + inline comments** of every module you touched — make them state the CURRENT design, caveats and "why", never how it got there; add or adjust `# DESIGN:` / `# WARNING:` tags next to changed logic. *Cut, don't accrete.*
2. **Phase "DONE" summary** — under its own `### Phase <x> — <title>` header in the roadmap above. CLAUDE.md is the ONLY place it goes. This is the ONE place dev-history detail belongs.
3. (**Repository Map** in this file — refresh a file's role line only if you added, removed or repurposed a file; keep it absolutely and utterly brief, *never* add clutter here, *cut, don’t accrete*; otherwise skip.)
4. (**`NEWS.md`** — user-facing / CRAN-facing only, new or changed functions/arguments, deprecations, important user-facing fixes; radically minimalistic, usually skipped.)
5. (**`README.Rmd`** — only before a CRAN release.)
