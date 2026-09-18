# ggfacto — AI Assistant Guide

## What ggfacto is, and why

`ggfacto` is a public R package: **v0.3.2 on CRAN, 0.4.0 in development here**. It builds readable, complete and pretty graphs for correspondence analysis made with 'FactoMineR'. They can be rendered as interactive 'HTML' plots, showing useful informations at mouse hover. The interest is not mainly visual but statistical: it helps the reader to keep in mind the data contained in the cross-table or Burt table while reading the correspondence analysis, thus preventing over-interpretation. Most graphs are made with 'ggplot2', which means that you can use the + syntax to  manually add as many graphical pieces you want, or change theme elements. 3D  graphs are made with 'plotly'.

The target users are : 1. a "literary" social sciences student, not good at math, learning to read geometrical data analysis ; 2. a serious quantitative analyst — survey researcher, sociologist — often working with weighted survey data.

### The one bet: the crosstable travels with the point

"Not mainly visual but statistical" is one feature and three that support it. `ggmca(active_tables = "active")` crosses every active variable with every other one and prints those crosstables **inside the hover tooltip of each point** — that set of crosstables is the Burt table the MCA was computed from. Each percentage is coloured by its deviation from the mean, blue over-represented and red under, so a level at the edge of the cloud shows many colours and one near the centre shows few: the reader re-derives the geometry from the data instead of inventing a story for it. `active_tables = "sup"` does the same against the supplementary variables, and `profiles = TRUE` draws the cloud of individuals as distinct **answer profiles**, each hovering to show the answers it is made of. A graph you can interrogate is a graph you can be wrong about out loud.

### What follows from the two audiences

For the analyst, **survey weights ride one channel end to end** — `multiple_correspondence_analysis(wt =)` through FactoMineR's `row.w` and back out into every tooltip crosstab — so a weighted analysis is never described by unweighted numbers; its `excl =` gives specific MCA, `hierarchical_clust()` clusters on the first axes, and `clust_tab()` describes the clusters as a coloured table. The student writes one workflow, the same for every analysis: the data frame first as in `tab()`, the analysis, its `*_interpret()` table, its graph, and clusters written into the data frame with `mutate()`. For the student, `ggmca_initial_dims()` and `ggmca_with_base_ref()` exist only to *teach*: one draws the active variables in their initial reference frame, the other draws that frame inside the space the analysis built. `mca_interpret()` reads the axes by Le Roux and Rouanet's method, and `benzecri_mrv()` gives the modified rate of variance, because raw MCA eigenvalue percentages mislead.

### Why not the neighbours

Nothing here replaces FactoMineR — ggfacto never computes an analysis it was not handed, beyond the `multiple_correspondence_analysis()` / `principal_component_analysis()` / `correspondence_analysis()` wrappers and the clustering `hierarchical_clust()` computes — `HCPC()`'s own, rebuilt so that its tree needs no n × n matrix. Against the rendering packages: `factoextra` draws the same clouds, prettily, but a point stays a point; `explor` puts the tables *beside* the graph in a Shiny app rather than inside it, and hands back no ggplot to extend; `Factoshiny` and `FactoInvestigate` automate the reading instead of showing the data behind it. `GDAtools` is the closest neighbour — the statistical toolbox of the same French school, and its `varsup()` is vendored here with credit — but it leaves rendering to others. ggfacto is the reading layer: it pins the graph back to the crosstable.

For anything related to crosstables, it relies heavily on `~/github/tabxplor/`.

---

## Repository Map

Fifteen files in `R/`, four groups. Every file carries a `# PURPOSE / # ROLE / # KEY CONSTRAINTS` header with fuller design detail: read it before the code.

**The MCA pipeline** — the package's main path, and the only one that is staged.

- `mca-data.R` — `multiple_correspondence_analysis()` (alias `MCA2()`), `ggmca()`, `ggmca_data()`: the entry points and the data half; plus `answer_profiles()` and the vendored `varsup()`.
- `mca-plot.R` — `ggmca_plot()`: the rendering half. ⚠ must keep sorting after `mca-data.R` (see its header).
- `tooltips.R` — `interactive_tooltips()`: the crosstabs behind the hover, built with tabxplor.

**The other analyses.**

- `ca.R` — `correspondence_analysis()`, which takes a `tab()`, and `ggca()`: one monolithic function.
- `pca.R` — `principal_component_analysis()` (alias `PCA2()`), `ggpca_cor_circle()`, the shared projector `PCA_ind.sup_coord()`, and the deprecated `mean_sd_tab()`.
- `clust.R` — `hierarchical_clust()` and `clust_tab()` (alias `HCPC_tab()`): the clusters, and the table that describes them.
- `mca-teach.R` — `ggmca_initial_dims()`, `ggmca_with_base_ref()`: the two pedagogical plots.

**3D** (plotly, `Suggests`-guarded): `mca-3d.R` — `ggmca_3d()` · `pca-3d.R` — `ggpca_3d()`.

**Tables, rendering and plumbing.**

- `interpret.R` — the interpretation tables of a factorial analysis: `mca_interpret()`, `ca_interpret()`, `pca_interpret()`, `benzecri_mrv()`, and the output contract the summary family shares (`?ggfacto_summary`, its two print methods).
- `ingress.R` — what an analysis remembers of its input: the `<VAR>.NA` levels, the one `excl` rule, and the rows it was fitted on (`source_rows()`, `align_to_fit()`).
- `render.R` — `theme_facto()`, the material palettes, `ggi()`, `ggsave2()`, `plot_path()`, `outlims()`.
- `utils.R` — factor helpers, the base-R string shim that replaced stringr, `weighted.var()`, vendored `where()`, and the two soft-deprecation notices (`renamed_arg()`, `deprecated_fn()`).
- `i18n.R` — the gettext plumbing: the `R-ggfacto` text domain, its own cache flush, the language resolver, and `with_gda_lang()`, which makes `lang =` an argument rather than an accident of the session.
- `knit.R` — the knitr seam: tags every widget the package returns, and writes it to its own file with an `<iframe>` in its place when `options(ggfacto.widget_dir)` asks.
- `ggfacto-package.R` — imports, global bindings, `.onLoad()`, the deprecated `%>%` re-export.

**Other directories:** `man/` (roxygen-generated, never edit) · `tests/testthat/` (the package's contract: the exported entry points, the argument matrix, the tooltip and table goldens) · `po/` (the message catalogues, `R-ggfacto.pot` and `R-fr.po`) · `inst/po/fr/LC_MESSAGES/` (the compiled `.mo`, committed, since `R CMD build` does not compile it) · `dev/` (`.Rbuildignore`'d; holds `dependency-audit.md`, `update_translations.R` and `hclust_chunked.R`).

---

## ggfacto architecture

### How a graph is built

```text
  FactoMineR result  ┐                     plot model                ggplot2 +
  + the microdata    ┼──► ggmca_data() ──► list(vars_data,   ──► ggmca_plot() ──┬──► ggi()    → girafe widget
  + survey weights   ┘                          ind_data, …)                    └──► print()  → Plots pane
```

`ggmca()` has a 25-line body of pure orchestration: its signature is exactly `ggmca_data()`'s plus `ggmca_plot()`'s, with no overlap, and it just routes each argument to its half. **The seam between the halves is public on purpose** — a user calls `ggmca_data()`, edits `plot_data$vars_data` (dropping a level, renaming one), and passes it to `ggmca_plot()`. Interactivity is a separate last step, so everything before `ggi()` is an ordinary ggplot you can `+` into.

⚠ **Only MCA is on this pipeline.** `ggca()` is one function that rebuilds coordinates, tooltips and its colour vector inline; the PCA functions are independent leaves. Bringing CA onto the pipeline — a `ggca_data()`/`ggca_plot()` split — is the stated direction of travel, and it would reuse `theme_facto()`, the material palettes and the colour-group logic as they are; only the tooltip builder would need widening, since `interactive_tooltips()` is shaped for the MCA's Burt-table crosstabs and CA's tooltips are the row and column percentages of one table.

### The plot model

`ggmca_data()` returns a plain `list`, not a class, of **flat** tables — no list-column, so a user can read and edit them:

- **`vars_data`** — one row per level of every active and supplementary variable: its coordinates and contributions on every axis (`Dim k`, `contrib<k>`), its frequency, its colour group, its `id`, and its tooltip as two strings, `begin_text` (the header) and `interactive_text` (the body).
- **`ind_data`** — one row per drawn **answer profile**: `count` / `wcount`, its cluster, its `id`, its tooltip string; `NULL` when `profiles = FALSE`. A profile's cluster is the **weighted plurality** of its individuals, missing ones left out: clusters made on the analysis are pure within a profile, whose individuals share one point, so the rule only decides for clusters made elsewhere.
- **`individuals`** — one row per fitted individual: the rank `nb` of its profile (`NA` beyond `max_profiles`), its weight, its coordinates, its supplementary answers; `NULL` without `sup_vars`. Ellipses and facets group it, so an ellipse covers every individual of its level, with or without `profiles = TRUE`.
- **`res.mca`** — a stripped `list(eig, axes_names)`, deliberately not the FactoMineR object: the rendering half must not be able to recompute anything.
- **`clust`** — the cluster variable's name, or `character()`.

`ggmca_plot()` inserts the contribution lines of the two axes it draws between a level's header and body — the one part of a tooltip the data half cannot know — into the single `interactive_text` column every geom's `tooltip =` aesthetic reads.

### The tooltip is the package

`interactive_tooltips()` crosses the `active_tables` variables with the active and tooltip variables in **one** `tabxplor::tab()` (`wt = "row.w"`, `na = "drop"`, `pct = "row"`, `color = "difference"`), then `format_pct()` renders each cell as `<font color>` HTML, reading the colour through `tabxplor::fmt_get_color_code()`, never through tabxplor internals. tabxplor's cost is per (row variable × column variable) pair, ~30 ms whatever the rows, so `stacked_crosstab()` **stacks** the crossed variables into one row variable — each individual once per variable, each level a code mapped back by lookup, never a label parsed — and `tab_vars` splits the table back into one block per variable: 15 pairs where one `tab()` per variable made 225, for byte-identical cells. A variable without crosstab feeds only its header line, so `level_counts()` counts it instead of tabulating it. `options(tabxplor.parallel =)` reaches every `tab()` ggfacto makes; the stacked call is a single unit, which tabxplor runs serially.

- **WARNING** — `tab_vars` is what keeps the stack exact: each block is compared to its **own** Total (`comp = "tab"`). One stacked Total would measure a variable with missing values against the wrong reference.
- **WARNING** — the "Frequency" denominator is `pop_wcount`, the population, never a table's Total row, which gave some levels a frequency above 100 %.
- **WARNING** — the numbers are aligned with `str_pad()` under a monospace font, so the shim's exact stringr semantics (a *vector* `width`, a non-space fill) are load-bearing, not stylistic.

### The FactoMineR contract

The package computes no analysis of its own, the clustering apart (below). `multiple_correspondence_analysis()`, `principal_component_analysis()` and `correspondence_analysis()` are the **ingress normalisers** (short names `MCA2()`, `PCA2()`, not deprecated) — tidyselect for `active_vars`/`wt`, a missing answer turned into a level `<VAR>.NA`, and one `excl` rule, **exact level names**, `NA` (the default) standing for every missing level, sent to FactoMineR as positions because it renames a level two variables share — while `correspondence_analysis()` reads a `tab()`'s weighted counts, whatever it displays. Everything downstream reads the fitted object directly: `$call$X` / `$quali` / `$Xtot` / `$excl` / `$marge.col` / `$row.w`, `$var$coord` / `$contrib` / `$cos2`, `$ind$coord`, `$eig`, `$svd$V` and `$vs`.

`varsup()`, vendored from GDAtools 1.7.2, is the only extractor that dispatches on the object's class (`MCA` / `speMCA` / `csMCA` / `stMCA` / `multiMCA`) and it covers supplementary variables only. ggfacto adds two slots of its own: **`res$axes_names`**, the user's, read defensively and used by `theme_facto()` for the axis titles; and **`res$source`**, the ingress's, `list(n, rows)` — which rows of the data frame the user named were analysed. It is read off the call itself: `pc_AGD |> filter(...) |> multiple_correspondence_analysis(...)` is re-run with a hidden row id, and kept only if it gives back exactly the analysed data (a `%>%`, a `select()` or a `slice_sample()` record nothing). Every function that takes the microdata back — `ggmca()`, `ggmca_3d()`, `hierarchical_clust()` inside `mutate()` — goes through `align_to_fit()`, which picks those rows and **re-checks the active answers**, so a subset needs no second filter and a reordered data frame is refused rather than misaligned. `correspondence_analysis()` writes the margin names back on `call$X`, which `FactoMineR::CA()` strips. ⚠ There is no shared extractor for the *active* side: `active_vars` is re-derived from `res.mca$call$X` and `$call$quali` in `mca-data.R`, twice in `mca-teach.R`, and by `active_names()` in `ingress.R`.

**The clusters are `HCPC()`'s, computed here.** `FactoMineR::HCPC()` builds its Ward tree from a `dist()` and an n × n `outer()` of weights — +3.3 GB for the course's 9 234 rows, so a 32 GB machine runs out near 30 000. Ward merges identical points first, at no cost, so `hierarchical_clust()` builds the same tree on the **distinct points** of the cloud (an MCA's answer profiles, weighted by the sum of their individuals) with `fastcluster::hclust.vector()`, which keeps no dissimilarity matrix: memory linear in the points, time still quadratic (~25 s at 40 000 distinct points). The rest is `HCPC()`'s and must stay so: the points sorted along axis 1 (it decides the ties and where the k-means starts), the cut rule, the **unweighted** k-means consolidation, the clusters numbered along axis 1. `test-clust.R` pins the equality for the three analyses. A CA clusters the levels of one `margin`, weighted by their counts, and in `mutate()` each individual takes its level's cluster, matched by name. A tree built by parts, for larger clouds, was measured and kept out of `R/`: `dev/hclust_chunked.R`.

### Weights ride one channel

The user's weight column → `FactoMineR`'s `row.w` → recovered from the **fitted object** (`res.mca$call$row.w`), never from `data`, and re-attached as a `row.w` column → `tabxplor::tab(wt = "row.w")`, beside the rows of `data` that `align_to_fit()` picked. So a tooltip always describes the population the analysis was fitted on, even when the user passes the whole data frame after analysing a subset of it. Consequently `count` (unweighted `n`) and `wcount` (`sum(row.w)`) travel as a pair everywhere: `wcount` is the point-size aesthetic and the sort key for `max_profiles` truncation, and a tooltip prints the weighted `n` only when it differs from the unweighted one.

### The plot-object seam

`theme_facto()` returns a **list** of ggplot objects, not a theme — axis titles carrying the eigenvalue percentages, scales, `coord_fixed()` — so it is `+`-ed as a whole; `ggmca_plot()` always calls it with `no_color_scale = TRUE` so the manual palette wins.

`ggmca_plot()` and `ggca()` then carry render hints on the returned object as **attributes** — `css_hover`, `css_tooltip`, `height_width_ratio` — which `ggi()` and `ggsave2()` read back; both must tolerate their absence, since a plain ggplot has none. `ggi()` passes the ratio on to the widget as `ggfacto_ratio`, which is the only geometry a knitted `<iframe>` has to go on (see `R/knit.R`). Attributes rather than list slots because the object must stay a real ggplot: `append()` flattens the S7 object into a plain list wearing `c("gg", "ggplot")`, and then `ggplot_build()`, `grid.draw()` and so `ggsave2()` stop dispatching, while `print()` draws only as a side effect of `print.default()` recursing into the nested plot. The hints survive `+`, so a user can keep extending the graph. Hover linking rests on a `data_id` convention: the ids are offset into disjoint bands — active variables from `1000`, HCPC clusters and answer profiles from `10000` — so that every point of one cluster shares an id and hovering any of them lights them all. A cluster's id is matched **by its name**, for its label and its profiles alike: `cleannames` re-sorts a factor alphabetically, so its codes cannot link them.

### Tables are tabxplor's

Every table here is built out of `tabxplor::fmt()` columns — scale, `col_var`, `row_kind`, colour, `ref` — and rendered by tabxplor; `benzecri_mrv(fmt = TRUE)` does the same for one vector.

**Five functions, one output contract** (`?ggfacto_summary`, `R/interpret.R`). Each returns **one** `tabxplor` table, tagged with the subclass `new_tab(class =)` provides, so it can be piped, filtered and exported like any other; the format is a print-time decision, and it is **tabxplor's own**, `options(tabxplor.print)`, read by `print.` and `knit_print.`. There is no ggfacto option: a summary and the `tab()` two lines above it obey the same one, so a script sets it once. Markdown is not among its values — it is an explicit `|> tab_md()`. `gda_render()` is the one html render, so print and knit_print cannot drift on the option only ggfacto knows: the tooltip. ⚠ `gda_print_html()` reads it through `isTRUE()`: ggfacto reaches tabxplor by `tabxplor::` alone, so `library(ggfacto)` never loads its namespace and leaves the option **unset**, on which a bare `%in%` yields `logical(0)`. For the same reason the console branch **states** its medium for the delegated call — tabxplor's own print stops on an unset option (its roadmap, phase 9). An **axis summary** carries none — every figure it would reveal already has a column of its own — while `clust_tab()` asks for them, being an ordinary crosstab of percentages whose counts are worth hovering for. There is nothing to suppress in the footer, so a `tab_md()` written by hand needs no argument of its own. ⚠ dplyr carries a table's tabxplor attributes but not its class, so a summary that has been through `mutate()` prints as an ordinary table — which is why the render options ride a plain attribute rather than `meta`: they die with the methods that read them.

**The eigenvalues travel under the table**, as a subordinate table (`tabxplor::set_footer_tabs()`), so every medium renders them below it and the rule for choosing how many axes to interpret is never a second call to remember — a pipe table in console, a `<table>` in html, a sheet in Excel. It is a table and not a barplot, because the rule for choosing axes is a cumulated percentage — Benzecri's modified rate for an MCA, 80 % for a CA, an eigenvalue above 1 for a PCA — and no bar can be read that finely. `% variance` and `cumul.` sit under a `Variance` `col_var`, the modified rate and its cumul under `Benzecri`, so each group is framed as a block, and a `Total` row states what the axes add up to. `n_axes` bounds what is printed **independently of `axes =`**, and when axes are left out an ellipsis row **states how many the cloud has** (`... of 27`) and prints its ellipsis in every column, a row of blanks reading as missing data where the point is that axes are missing — a `"...{tok}"` display template whose token is NA there, so the literal alone survives and the unit line is unmoved (`fmt_display_label()` polls data and total rows alone) — the count being the only question a reader has, where the last axis alone gave a row with nothing above it to be read against. ⚠ The count is **passed in**, not read off `eig`, which `ncp` truncates: an MCA has `levels - questions` axes, a CA `min(dim) - 1`, a PCA `min(vars, n - 1)`. So the row appears in the two cases that differ — `n_axes` cut the display, or `ncp` cut the analysis — and **not at all** when every axis is shown, an ellipsis over nothing being a lie about the tail. The `Total` row is the **whole cloud** — 100 % and the total inertia, the ellipsis standing for the axes not shown — the inertia being read off one axis (`eig[1, 1] / eig[1, 2]`), which `ncp` cannot truncate: FactoMineR's default `ncp = 5` otherwise gave a PCA of seven variables `6.875 · 98.2 %`. **`% variance` carries a data bar** (`tabxplor::set_bars()`), the barplot inside the table: it says the SHAPE of the decline, the elbow being what a reader looks for, and the numbers beside it stay the rule. ⚠ Its ceiling is the column's own largest axis, `set_bars()`'s default. A ceiling of 100 % was measured and refused — an MCA's raw rates are diluted by construction (`tea`: 9.9, 8.1, 6.0, 5.2 %), so every scree would flatten into stubs. Total and ellipsis rows take none, tabxplor barring `row_kind == "data"` alone, and `color = FALSE` draws none at all.

⚠ **A correspondence analysis draws the STRUCTURE of a crosstab's deviations and says nothing of their size**, so the crosstab is asked for beside it — `tab(..., pct = "row", color = "contrib")`, percentages coloured by contribution, never `display = "ctr"`. `ca_interpret()` does not carry it: a reader who wants both asks for both, and `vars =` is what names the two margins, defaulting to the names `correspondence_analysis()` wrote back, since `FactoMineR::CA()` destroys `names(dimnames())`.

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
- **The plot object carries render hints as attributes.** Never move them back into list slots, and never rebuild the object with `append()`: either turns it from a ggplot into a list and silently breaks `ggsave2()`.
- **Tables are tabxplor's job** — no kableExtra, DT or gt.
- **One name per thing across the MCA family.** The microdata is `data` everywhere, second as in `tab()`; the plot model is `plot_data`; the clusters are `clust`, a bare column name. The old `dat`, `cah`, `cah_color_groups` (and `ggmca_plot(data =)`) are soft-deprecated aliases routed through `renamed_arg()`, warning once per session.
- **An answer profile is aggregated, never nested.** Its membership is one integer vector (`answer_profiles()`, `vctrs::vec_group_id()` over the answers its tooltip shows), and every per-profile number is one vectorised aggregation over it — no tibble per profile, no `purrr::map()` over profiles.
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
- **`dev/*.md`** (`.Rbuildignore`'d) — transversal or expert technical guides only; there is one, `dependency-audit.md` (what each dependency costs to install, and the ruling on each). Each holds what an `R/` header is too short to derive — a foreign system, a cross-file policy, a statistical derivation — and the header that needs it points at it by section.
- **Roadmap "DONE" summaries are appended to the section below** (this file) — the **ONLY** place dev history lives. The maintainer then manualy moves them to `dev/ggfacto_roadmap_DONE_PHASES.md` for archiving.

---

## Deprecation and retro-compatibility

### For main user-facing functions and arguments
- This package have a small but existing users base : **soft deprecate main user-facing functions and arguments carefully** to ensure retro-compatibility.
- **It’s always possible to modify the main API for user-friendliness and integration** by **routing old arguments to new ones** and do *ad hoc* back-compat *after* having found a new more user-friendly API.

### For internal code and internal functions
- **Do not hesitate to propose radical redesign of internal code and internal workflows** for quality, simplicity, structure, performance and future-proofing, specially when they are too convoluted or have grown organically.
- **Always try to simplify, integrate and create smart shared subfunctions** instead of adding a new layer of confusion and ad-hoc solutions inside the code: your main aim is to simplify, to remove traces of old implementations altogether when they have become useless, to clarify, to help me make relevant architectural choices instead of piling up ad-hoc solutions, to integrate the new features in the current code seamlessly.

---

## Testing

`tests/testthat/` is the package's **contract**: it must fail when a user-visible fact changes, must not fail when an internal is redesigned, and must stay fast enough to run on every edit. Twelve files: `helper-fixtures.R` (the cached analyses and plot models), `helper-i18n.R` (`skip_if_no_gettext()`), `test-ingress.R` (the ingress normalisers, the `excl` rule, and the source-row contract in both directions — every provable pipe shape recorded, every unprovable one refused), `test-clust.R` (`hierarchical_clust()` equal to `FactoMineR::HCPC()` for the three analyses, its alignment by context, and `clust_tab()`), `test-ggmca-data.R` (the plot model and the argument matrix), `test-tooltips.R` (the crosstabs behind the hover), `test-plots.R` (that every graph builds, and the render-hint seam), `test-knit.R` (the widget seam), `test-interpret.R` (the interpretation tables and the output contract the whole summary family shares, `mean_sd_tab()` included), `test-i18n.R` (the French catalogue), plus `test-str-shim.R` and `test-non-ascii.R` from 1a.

**Argument coverage is the point, not function coverage.** Several arguments are inert alone and only act alongside an enabling one — `keep_levels`/`discard_levels` need `sup_vars`, `tooltip_vars`/`tooltip_vars_1lv` need a table to be built at all, `clust` needs `profiles`. A test that omits the enabler passes while exercising nothing; the vacuous paths are pinned deliberately so nobody "simplifies" them back into nothing.

**Fixtures are `tea[1:6]`, never `tea[1:18]`.** Tooltip crosstabs are quadratic in the number of active variables: `active_tables = "active"` costs 1.2 s on six and 10.4 s on eighteen. Six reaches every code path. The models the suite reuses are memoised in `helper-fixtures.R`, among them `fx_tea_na()` (missing answers for the `excl` rule) and `fx_mca_young()` (an analysis of a piped subset, which records its rows). The one exception is `fx_mca_multi()`, local to `test-interpret.R`: `tea[1:6]` is all binary, so `mca_interpret()`'s row packing collapses every question to one line there and its display blanking has nothing to hide — that needs multi-level variables and a third axis.

The suite is **small and serial**: 557 assertions, about 36 s, no `Config/testthat/parallel`, no `setup.R`. ⚠ **A green local suite does not mean a green CI**: this box is `fr_FR.UTF-8`, while `R CMD check` forces `LANGUAGE=en` with a C message locale, where gettext cannot translate at all. Every French assertion is therefore guarded by `skip_if_no_gettext()`, and each translated feature is pinned **twice** — an unguarded English block plus a guarded French twin. ⚠ Do not turn parallelism on for it, and do not import tabxplor's worker, orphan and gettext conventions — see `~/github/tabxplor/CLAUDE.md` "## Testing" only if the suite ever grows enough to need them.

**Golden tests use `expect_snapshot()`** (`_snaps/*.md`), and only where the output is genuinely stable and worth the churn: the rendered tooltip text (as `ggmca_plot()` joins it) and the four interpretation tables (MCA concise and complete, CA, PCA). ⚠ The *rendered html* is never snapshotted — it is 7 kB of inlined stylesheet; an interpretation table's snapshot is the console print, taken with `n = Inf` under `options(tabxplor.print = "console")`, since pillar formats only the rows it shows and a slice without a summary row makes `color = "contrib"` warn.

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

**Deux bogues trouvés.** Une ACM ajustée avec `ind.sup` voyait ses individus supplémentaires classés comme actifs — le réajustement perdait `ind.sup` — : elle est refusée, comme l'ACP. Et `HCPC()` rend les **lignes** d'une AC dans son propre ordre, trié le long de l'axe 1 (il teste `cluster.CA == "row"`, jamais vrai), ses colonnes dans le leur : les tests comparent par le nom. ⚠ Gardé tel quel, à trancher plus tard : la consolidation de `HCPC()` ignore les poids de sondage.

**Tests** : **557 assertions** (542), 0 échec, 0 avertissement, ~36 s ; `check` 0/0/0. **Dépendances** : `fastcluster` (+1 paquet, +0,3 Mo, aucune dépendance), `graphics` (paquet de base).

#### Phase 1p —

Now that ggfacto fully own the hierarchical clustering function, I wonder about the further simplifications of usage and quality-of-life features we could add, for the most straight-to-the-point workflows for the user ?

One point is that it’s useless to do the same hierarchical clustering twice on the same 



Est-ce cela aurait un sens de faire marcher `clust_tab` aussi avec une ACP, et est-ce que cela à un sens ? Utiliser des moyennes avec coefficient de variation, ou utiliser des `sd_bands` ?
Est-ce que `tab()` de tabxplor 2.0.0 


#### Phase 1q — vignette and pkgdown site

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



#### Phase 1r — 0.4.0 release

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
