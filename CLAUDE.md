# ggfacto — AI Assistant Guide

## What ggfacto is, and why

`ggfacto` is a public R package: **v0.3.2 on CRAN, 0.4.0 in development here**. It builds readable, complete and pretty graphs for correspondence analysis made with 'FactoMineR'. They can be rendered as interactive 'HTML' plots, showing useful informations at mouse hover. The interest is not mainly visual but statistical: it helps the reader to keep in mind the data contained in the cross-table or Burt table while reading the correspondence analysis, thus preventing over-interpretation. Most graphs are made with 'ggplot2', which means that you can use the + syntax to  manually add as many graphical pieces you want, or change theme elements. 3D  graphs are made with 'plotly'.

The target users are : 1. a "literary" social sciences student, not good at math, learning to read geometrical data analysis ; 2. a serious quantitative analyst — survey researcher, sociologist — often working with weighted survey data.

### The one bet: the crosstable travels with the point

"Not mainly visual but statistical" is one feature and three that support it. `ggmca(active_tables = "active")` crosses every active variable with every other one and prints those crosstables **inside the hover tooltip of each point** — that set of crosstables is the Burt table the MCA was computed from. Each percentage is coloured by its deviation from the mean, blue over-represented and red under, so a level at the edge of the cloud shows many colours and one near the centre shows few: the reader re-derives the geometry from the data instead of inventing a story for it. `active_tables = "sup"` does the same against the supplementary variables, and `profiles = TRUE` draws the cloud of individuals as distinct **answer profiles**, each hovering to show the answers it is made of. A graph you can interrogate is a graph you can be wrong about out loud.

### What follows from the two audiences

For the analyst, **survey weights ride one channel end to end** — `MCA2(wt =)` through FactoMineR's `row.w` and back out into every tooltip crosstab — so a weighted analysis is never described by unweighted numbers; `MCA2(excl =)` gives specific MCA, and `HCPC_tab()` describes clusters as a coloured table. For the student, `ggmca_initial_dims()` and `ggmca_with_base_ref()` exist only to *teach*: one draws the active variables in their initial reference frame, the other draws that frame inside the space the analysis built. `mca_interpret()` reads the axes by Le Roux and Rouanet's method, and `benzecri_mrv()` gives the modified rate of variance, because raw MCA eigenvalue percentages mislead.

### Why not the neighbours

Nothing here replaces FactoMineR — ggfacto never computes an analysis it was not handed, beyond the `MCA2()`/`PCA2()` wrappers. Against the rendering packages: `factoextra` draws the same clouds, prettily, but a point stays a point; `explor` puts the tables *beside* the graph in a Shiny app rather than inside it, and hands back no ggplot to extend; `Factoshiny` and `FactoInvestigate` automate the reading instead of showing the data behind it. `GDAtools` is the closest neighbour — the statistical toolbox of the same French school, and its `varsup()` is vendored here with credit — but it leaves rendering to others. ggfacto is the reading layer: it pins the graph back to the crosstable.

For anything related to crosstables, it relies heavily on `~/github/tabxplor/`.

---

## Repository Map

Fourteen files in `R/`, four groups. Every file carries a `# PURPOSE / # ROLE / # KEY CONSTRAINTS` header with fuller design detail: read it before the code.

**The MCA pipeline** — the package's main path, and the only one that is staged.

- `mca-data.R` — `MCA2()`, `ggmca()`, `ggmca_data()`: the entry points and the data half; plus `complete_cah()` and the vendored `varsup()`.
- `mca-plot.R` — `ggmca_plot()`: the rendering half. ⚠ must keep sorting after `mca-data.R` (see its header).
- `tooltips.R` — `interactive_tooltips()`: the crosstabs behind the hover, built with tabxplor.

**The other analyses.**

- `ca.R` — `ggca()`: simple correspondence analysis, one monolithic function.
- `pca.R` — `PCA2()`, `ggpca_cor_circle()`, and the shared projector `PCA_ind.sup_coord()`.
- `mca-teach.R` — `ggmca_initial_dims()`, `ggmca_with_base_ref()`: the two pedagogical plots.

**3D** (plotly, `Suggests`-guarded): `mca-3d.R` — `ggmca_3d()` · `pca-3d.R` — `ggpca_3d()`.

**Tables, rendering and plumbing.**

- `interpret.R` — the interpretation tables of a factorial analysis: `mca_interpret()`, `ca_interpret()`, `pca_interpret()`, `benzecri_mrv()`, and the output contract the summary family shares (`?ggfacto_summary`, its two print methods).
- `tables.R` — the two tables that describe the DATA rather than an axis: `mean_sd_tab()`, `HCPC_tab()`.
- `render.R` — `theme_facto()`, the material palettes, `ggi()`, `ggsave2()`, `plot_path()`, `outlims()`.
- `utils.R` — factor helpers, the base-R string shim that replaced stringr, `weighted.var()`, vendored `where()`, and the two soft-deprecation notices (`renamed_arg()`, `deprecated_fn()`).
- `i18n.R` — the gettext plumbing: the `R-ggfacto` text domain, its own cache flush, the language resolver, and `with_gda_lang()`, which makes `lang =` an argument rather than an accident of the session.
- `knit.R` — the knitr seam: tags every widget the package returns, and writes it to its own file with an `<iframe>` in its place when `options(ggfacto.widget_dir)` asks.
- `ggfacto-package.R` — imports, global bindings, `.onLoad()`, the deprecated `%>%` re-export.

**Other directories:** `man/` (roxygen-generated, never edit) · `tests/testthat/` (the package's contract: the exported entry points, the argument matrix, the tooltip and table goldens) · `po/` (the message catalogues, `R-ggfacto.pot` and `R-fr.po`) · `inst/po/fr/LC_MESSAGES/` (the compiled `.mo`, committed, since `R CMD build` does not compile it) · `dev/` (`.Rbuildignore`'d; holds `dependency-audit.md` and `update_translations.R`).

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

`ggmca_data()` returns a plain `list`, not a class:

- **`vars_data`** — one row per level of every active and supplementary variable: its coordinates on every axis, its frequency, its colour group, its `data_id`, and a nested `interactive_text` tibble of tooltip fragments.
- **`ind_data`** — one row per **answer profile**, with `count` / `wcount` and its own nested tooltip; `NULL` when `profiles = FALSE`.
- **`res.mca`** — a stripped `list(eig, axes_names)`, deliberately not the FactoMineR object: the rendering half must not be able to recompute anything.
- **`cah`** — the HCPC cluster variable's name, or `character()`.

`ggmca_plot()` appends the per-axis contributions to the nested tooltip, then `tidyr::unite()`s it into the single `interactive_text` character column that every geom's `tooltip =` aesthetic reads.

### The tooltip is the package

`interactive_tooltips()` builds **one `tabxplor::tab()` per variable** (`output_list = TRUE`, `wt = "row.w"`, `na = "drop"`; `pct = "row"` with `color = "difference"` for the `active_tables` variables, `pct = "col"` for the rest), then `format_pct()` renders each cell as `<font color>` HTML, reading the colour through `tabxplor::fmt_get_color_code()` — never through tabxplor internals, as an earlier version did.

- **WARNING** — the "Frequency" denominator is `pop_wcount`, the population. It is *not* the last row of the bound tables, which is whatever the last variable's last level happens to be and gave some levels a frequency above 100 %.
- **WARNING** — the numbers are aligned with `str_pad()` under a monospace font, so the shim's exact stringr semantics (a *vector* `width`, a non-space fill) are load-bearing, not stylistic.
- `unlv()` undoes tabxplor's `"_lv"` suffix on a level whose name collides with a column name, via `fct_relabel` so `lvs` stays a factor.

### The FactoMineR contract

The package computes no analysis of its own. `MCA2()` and `PCA2()` are the **ingress normalisers** — tidyselect for `active_vars`/`wt`, and a regex `excl` that promotes `NA` to a level and then excludes it, which is specific MCA — and everything downstream reads the fitted object directly: `$call$X` / `$quali` / `$Xtot` / `$excl` / `$marge.col` / `$row.w`, `$var$coord` / `$contrib` / `$cos2`, `$ind$coord`, `$eig`, `$svd$V` and `$vs`.

`varsup()`, vendored from GDAtools 1.7.2, is the only extractor that dispatches on the object's class (`MCA` / `speMCA` / `csMCA` / `stMCA` / `multiMCA`) and it covers supplementary variables only. ggfacto adds one slot of its own, **`res$axes_names`**, read defensively and used by `theme_facto()` for the axis titles. ⚠ There is no shared extractor for the *active* side: `active_vars` is re-derived from `res.mca$call$X` and `$call$quali` independently in four places (`mca-data.R`, twice in `mca-teach.R`, and in a different shape in `tables.R`).

### Weights ride one channel

The user's weight column → `FactoMineR`'s `row.w` → recovered from the **fitted object** (`res.mca$call$row.w`), never from `dat`, and re-attached as a `row.w` column → `tabxplor::tab(wt = "row.w")`. So a tooltip always describes the population the analysis was fitted on, even when the user passes a different `dat`. Consequently `count` (unweighted `n`) and `wcount` (`sum(row.w)`) travel as a pair everywhere: `wcount` is the point-size aesthetic and the sort key for `max_profiles` truncation, and a tooltip prints the weighted `n` only when it differs from the unweighted one.

### The plot-object seam

`theme_facto()` returns a **list** of ggplot objects, not a theme — axis titles carrying the eigenvalue percentages, scales, `coord_fixed()` — so it is `+`-ed as a whole; `ggmca_plot()` always calls it with `no_color_scale = TRUE` so the manual palette wins.

`ggmca_plot()` and `ggca()` then carry render hints on the returned object as **attributes** — `css_hover`, `css_tooltip`, `height_width_ratio` — which `ggi()` and `ggsave2()` read back; both must tolerate their absence, since a plain ggplot has none. `ggi()` passes the ratio on to the widget as `ggfacto_ratio`, which is the only geometry a knitted `<iframe>` has to go on (see `R/knit.R`). Attributes rather than list slots because the object must stay a real ggplot: `append()` flattens the S7 object into a plain list wearing `c("gg", "ggplot")`, and then `ggplot_build()`, `grid.draw()` and so `ggsave2()` stop dispatching, while `print()` draws only as a side effect of `print.default()` recursing into the nested plot. The hints survive `+`, so a user can keep extending the graph. Hover linking rests on a `data_id` convention: the ids are offset into disjoint bands — active variables from `1000`, HCPC clusters and answer profiles from `10000` — so that every point of one cluster shares an id and hovering any of them lights them all.

### Tables are tabxplor's

Every table here is built out of `tabxplor::fmt()` columns — scale, `col_var`, `row_kind`, colour, `ref` — and rendered by tabxplor; `benzecri_mrv(fmt = TRUE)` does the same for one vector.

**Five functions, one output contract** (`?ggfacto_summary`, `R/interpret.R`). Each returns **one** `tabxplor` table, tagged with the subclass `new_tab(class =)` provides, so it can be piped, filtered and exported like any other; the format is a print-time decision, and it is **tabxplor's own**, `options(tabxplor.print)`, read by `print.` and `knit_print.`. There is no ggfacto option: a summary and the `tab()` two lines above it obey the same one, so a script sets it once. Markdown is not among its values — it is an explicit `|> tab_md()`. `gda_render()` is the one html render, so print and knit_print cannot drift on the option only ggfacto knows: the tooltip. ⚠ `gda_print_html()` reads it through `isTRUE()`: ggfacto reaches tabxplor by `tabxplor::` alone, so `library(ggfacto)` never loads its namespace and leaves the option **unset**, on which a bare `%in%` yields `logical(0)`. For the same reason the console branch **states** its medium for the delegated call — tabxplor's own print stops on an unset option (its roadmap, phase 9). An **axis summary** carries none — every figure it would reveal already has a column of its own — while `HCPC_tab()` asks for them, being an ordinary crosstab of percentages whose counts are worth hovering for. There is nothing to suppress in the footer, so a `tab_md()` written by hand needs no argument of its own. ⚠ dplyr carries a table's tabxplor attributes but not its class, so a summary that has been through `mutate()` prints as an ordinary table — which is why the render options ride a plain attribute rather than `meta`: they die with the methods that read them.

**The eigenvalues travel under the table**, as a subordinate table (`tabxplor::set_footer_tabs()`), so every medium renders them below it and the rule for choosing how many axes to interpret is never a second call to remember — a pipe table in console, a `<table>` in html, a sheet in Excel. It is a table and not a barplot, because the rule for choosing axes is a cumulated percentage — Benzecri's modified rate for an MCA, 80 % for a CA, an eigenvalue above 1 for a PCA — and no bar can be read that finely. `% variance` and `cumul.` sit under a `Variance` `col_var`, the modified rate and its cumul under `Benzecri`, so each group is framed as a block, and a `Total` row states what the axes add up to. `n_axes` bounds what is printed **independently of `axes =`**, and when axes are left out an ellipsis row **states how many the cloud has** (`... of 27`) and prints its ellipsis in every column, a row of blanks reading as missing data where the point is that axes are missing — a `"...{tok}"` display template whose token is NA there, so the literal alone survives and the unit line is unmoved (`fmt_display_label()` polls data and total rows alone) — the count being the only question a reader has, where the last axis alone gave a row with nothing above it to be read against. ⚠ The count is **passed in**, not read off `eig`, which `ncp` truncates: an MCA has `levels - questions` axes, a CA `min(dim) - 1`, a PCA `min(vars, n - 1)`. So the row appears in the two cases that differ — `n_axes` cut the display, or `ncp` cut the analysis — and **not at all** when every axis is shown, an ellipsis over nothing being a lie about the tail. The `Total` row is likewise **read off `eig`**: a truncated fit says the share it actually holds, not 100 %. **`% variance` carries a data bar** (`tabxplor::set_bars()`), the barplot inside the table: it says the SHAPE of the decline, the elbow being what a reader looks for, and the numbers beside it stay the rule. ⚠ Its ceiling is the column's own largest axis, `set_bars()`'s default. A ceiling of 100 % was measured and refused — an MCA's raw rates are diluted by construction (`tea`: 9.9, 8.1, 6.0, 5.2 %), so every scree would flatten into stubs. Total and ellipsis rows take none, tabxplor barring `row_kind == "data"` alone, and `color = FALSE` draws none at all.

⚠ **A correspondence analysis draws the STRUCTURE of a crosstab's deviations and says nothing of their size**, so the crosstab is asked for beside it — `tab(..., pct = "row", color = "contrib")`, percentages coloured by contribution, never `display = "ctr"`. `ca_interpret()` does not carry it: a reader who wants both asks for both, and `vars =` is what names the two margins, because `FactoMineR::CA()` destroys `names(dimnames())` of `call$X` and `call$Xtot` even when the input was a named `as.table()`.

**`MCA2()` and `PCA2()` keep every axis** (`ncp = Inf`). `res$eig` is how one chooses how many axes to interpret, and FactoMineR truncates it to `ncp`; worse, `benzecri_mrv()` renormalises over the axes it finds, so a truncated fit gives the SAME axis a different modified rate — 57.4 % against 55.4 % on `tea`, measured. One lowers `ncp` only to feed `FactoMineR::HCPC()`, which clusters on the axes kept. Measured cost of the default on a 9 234 × 15 MCA: +0.03 s and +5 MB.

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
- **One name per thing across the MCA family.** The microdata is `data` everywhere; the plot model is `plot_data`. The old `dat` (and `ggmca_plot(data =)`) are soft-deprecated aliases routed through `renamed_arg()`, warning once per session.
- **`data.table` is used in exactly one function**, `complete_cah()`, where a grouped `.N` over many columns runs on the full individual-level data. The benchmark comment beside the adjacent `tidyr::nest()` records that data.table was *slower* there; do not generalise it.
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

`tests/testthat/` is the package's **contract**: it must fail when a user-visible fact changes, must not fail when an internal is redesigned, and must stay fast enough to run on every edit. Eleven files: `helper-fixtures.R` (the cached analyses and plot models), `helper-i18n.R` (`skip_if_no_gettext()`), `test-mca2-pca2.R` (the ingress normalisers), `test-ggmca-data.R` (the plot model and the argument matrix), `test-tooltips.R` (the crosstabs behind the hover), `test-plots.R` (that every graph builds, and the render-hint seam), `test-knit.R` (the widget seam), `test-interpret.R` (the interpretation tables and the output contract the whole summary family shares), `test-tables.R` (the two tables that describe the data), `test-i18n.R` (the French catalogue), plus `test-str-shim.R` and `test-non-ascii.R` from 1a.

**Argument coverage is the point, not function coverage.** Several arguments are inert alone and only act alongside an enabling one — `keep_levels`/`discard_levels` need `sup_vars`, `tooltip_vars`/`tooltip_vars_1lv` need a table to be built at all, `cah` needs `profiles`. A test that omits the enabler passes while exercising nothing; the vacuous paths are pinned deliberately so nobody "simplifies" them back into nothing.

**Fixtures are `tea[1:6]`, never `tea[1:18]`.** Tooltip crosstabs are quadratic in the number of active variables: `active_tables = "active"` costs 1.2 s on six and 10.4 s on eighteen. Six reaches every code path. The models the suite reuses are memoised in `helper-fixtures.R`; the whole suite runs in about 30 s. The one exception is `fx_mca_multi()`, local to `test-interpret.R`: `tea[1:6]` is all binary, so `mca_interpret()`'s row packing collapses every question to one line there and its display blanking has nothing to hide — that needs multi-level variables and a third axis.

The suite is **small and serial**: 377 assertions, no `Config/testthat/parallel`, no `setup.R`. ⚠ **A green local suite does not mean a green CI**: this box is `fr_FR.UTF-8`, while `R CMD check` forces `LANGUAGE=en` with a C message locale, where gettext cannot translate at all. Every French assertion is therefore guarded by `skip_if_no_gettext()`, and each translated feature is pinned **twice** — an unguarded English block plus a guarded French twin. ⚠ Do not turn parallelism on for it, and do not import tabxplor's worker, orphan and gettext conventions — see `~/github/tabxplor/CLAUDE.md` "## Testing" only if the suite ever grows enough to need them.

**Golden tests use `expect_snapshot()`** (`_snaps/*.md`), and only where the output is genuinely stable and worth the churn: the rendered tooltip text and the four interpretation tables (MCA concise and complete, CA, PCA). ⚠ The *rendered html* is never snapshotted — it is 7 kB of inlined stylesheet; an interpretation table's snapshot is the console print, taken with `n = Inf` under `options(tabxplor.print = "console")`, since pillar formats only the rows it shows and a slice without a summary row makes `color = "contrib"` warn.

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

#### Phase 1a — clean dependency tree

Installing ggfacto with its `Suggests` went from **164 packages / 243.7 MB to 133 / 203.7 MB** (-31 packages, -40.0 MB, four fewer compiled from source), `R CMD check` clean at 0/0/0. `finalfit` and `gridExtra` had no call sites left once `pers_or_plot()` went, and were the bulk of it at 27 packages. `ggforce` drew exactly one thing, the PCA correlation circle, now a `geom_path()` — which also fixed a latent waste: `geom_circle()` evaluated its aes once per row of the plot data, so that circle was drawn eleven times over. `stringr` and `stringi` went behind a vendored base-R shim in `R/utils.R`. Undeclared `grid` became `ggplot2::arrow` (a re-export of the same function); `scales`, `stats` and `grDevices` moved from `Suggests` to `Imports`, where their unguarded use always belonged; `ggplot2` rose to `>= 3.4.0`, and **R to `>= 4.1.0` — the declared floor was 4.0.1, but the package already used `|>`, so it could never have installed there.**

A shim rather than a hand conversion because ~20 of the 188 stringr sites diverge in base R, measured not assumed: `cleannames_condition()`'s lookaround makes `gsub()` *error* without `perl = TRUE`; four sites use `str_c()`'s `NA` as a guard that `paste0()`'s `"NA"` would defeat; `regmatches()` returns a vector shorter than its input on a non-match; `formatC()` errors on a vector `width` and cannot pad with anything but space or zero. The shim keeps stringr's semantics exactly, which turned all 188 sites into a namespace strip, and `tests/testthat/test-str-shim.R` pins it with 42 assertions. Padding has its own block: it aligns tooltip numbers under a monospace font.

⚠ **The `%>%` to `|>` migration was the trap, not the strings.** It left **25** magrittr `.` placeholders in `purrr::keep`/`discard`/`set_names`/`map_if`, not the three a pre-flight regex predicted, and a second parse-tree scan missed them too (sibling `~` lambdas cleared the ancestor check). They fail *quietly*: the package-level `. = NULL` binding turns a stranded placeholder into a wrong answer instead of an error. `R CMD check`'s examples caught the first, a full unfiltered parse-level scan the rest; they are now `(\(x) ...)()` mid-chain and plain lambdas as predicates. `magrittr` itself stays one deprecation cycle — every internal use is `|>`, but `%>%` is still re-exported, and it costs 0 MB.

Verification was a golden snapshot against a pristine `HEAD` extracted with `git archive`: 26 entries over `tea`, `mtcars` and `gss_cat`, deliberately including the `sup_vars` / `tooltip_vars` / `keep_levels` / `discard_levels` / `active_tables` / `cah` argument paths, since the plain calls reach none of the placeholders. **25 of 26 came back byte-identical**; the one diff, `pca_cor_circle`, is the intended ggforce change and was checked geometrically — both trace the unit circle through the same 361 unique points at radius exactly 1.0. `test-non-ascii.R`, copied from tabxplor, earned itself immediately by catching a literal U+202F typed into a test. Two pre-existing deprecations were left alone as out of scope: the `size` aesthetic for lines, and `select(cah)` on an external vector.

#### Phase 1a2 — one file per subsystem

`R/geometrical_data_analysis.R` was 7057 lines holding every user-facing function, which made a "Repository Map" of two files document nothing. It is now **eleven files plus `utils.R`**, cut along the section markers the file already carried, each with a `# PURPOSE / # ROLE / # KEY CONSTRAINTS` header. Pure code motion: signatures, defaults and bodies are untouched.

**Verification was a function-body digest, not a golden snapshot** — exhaustive where a snapshot samples. `HEAD` was extracted with `git archive`, both trees `sys.source`d into separate environments, and every object `deparse()`d and diffed. Result: **44 of 48 shared functions byte-identical**, and the four that differ differ only by the three intended edits. Upstream of that, the split itself was proved by partitioning the file into ranges that provably cover `1..7057` exactly once, then checking that every non-blank line reappears, in order, in the right destination — 5614 of 5614. A 31-call runtime sheet over `tea`/`mtcars`/`gss_cat` covered the argument paths a plain call cannot reach (`sup_vars`, `active_tables` both values, `tooltip_vars*`, `keep`/`discard_levels`, `cah`, the `xlim`/`ylim` no-repel branch, `ggmca_data()` → edit → `ggmca_plot()`), all green; `devtools::test()` 54/54; `R CMD check` 0/0/0.

⚠ **The one trap the split sprang**: `@describeIn ggmca` names the merged help topic after whichever block roxygen reads first, and roxygen reads files in C-locale order — so `mca-plot.R` sorting before `mca.R` silently renamed the topic to `ggmca_plot` and reordered its usage section. The fix is the file name: `mca.R` became **`mca-data.R`**, which is also the truer name, and `mca-plot.R`'s header states the constraint.

Carried in the same pass, all verified against the pre-existing `man/` byte-for-byte: two `\link[stringr]{}` cross-references that had shipped unresolvable since stringr left in 1a; `ggsave2()`'s `exists("plot$heigth_width_ratio")`, which tests for a variable of that literal name and is therefore always `FALSE`, so the aspect-ratio branch never fired — fixed, and the misspelt field renamed `height_width_ratio` at all six sites; `man/tabxplor-data.table.Rd`, copy-pasted from the sibling package, now `ggfacto-data.table`; and the two `"Title Scale color…"` roxygen titles left by the RStudio skeleton. `outlims()`, defined twice byte-identically inside `ggmca_plot()` and `ggca()`, is one internal in `render.R`.

**About 850 lines of dead code and dev scratch went** (`R/` is 7704 lines down to 6981, and that is with 117 lines of new file headers added), each checked for call sites first: `PCA_princ_coord_in_base()` (its Le Roux notation key survives, duplicated verbatim, in `pca-3d.R`), `fct_clean()`, and `utils.R`'s whole colour block — `material_colors_lighter()`, which only comments referenced, `rgb2hsl()`/`hsl2rgb()`, which only it called, and 185 commented-out lines of vendored `plotwidgets`. `utils.R` went 647 → 247 lines. The commented-out scratch calls between functions (`ggmca_cah()`, `theme_ac()`, the `# axes = c(1,2)` debugging preambles, chains naming datasets that no longer exist) went with them. The README stopped teaching `%>%` while NEWS deprecates it, and lost the `marical` typo. The comments/code ratio fell from **0.36 to 0.27** — still over the 0.2 target the documentation ecosystem sets, and what remains is real commented-out alternatives inside `ggpca_3d()` and `ggmca_3d()`, which need reading rather than a sweep.

**Left standing deliberately**, as design decisions rather than slips:

- ⚠ **`ggsave2()` is broken under ggplot2 4.0** — and was already broken at `HEAD`, verified by running the same sheet against the pristine tree. `append()` turns the S7 ggplot into a plain list wearing `c("gg", "ggplot")`; `print()`, `+` and `ggi()` still work on that, `grid.draw()` does not. The real fix is to stop smuggling render hints through `append()`.
- **Four exports default an argument to itself** — `res.mca = res.mca` on `ggmca_initial_dims()`, `ggmca_with_base_ref()` and `mca_interpret()`, `res.ca = res.ca` on `ggca()`. Harmless when supplied, but omitting the argument gives "promise already under evaluation" instead of "argument is missing".
- **`ggpca_3d()` is 1227 lines in one function**, and `pca-3d.R` is only that function. Splitting a function is not a file-organisation task.
- **`vignettes/` still does not exist**, though the documentation ecosystem names it as a layer.



#### Phase 1b — a real test suite

`tests/testthat/` went from 2 files / 54 assertions guarding only the string shim and the ASCII rules, to **7 files / 213 assertions** covering every exported entry point, running in about 30 s. Fixtures are memoised in `helper-fixtures.R` and built on **`tea[1:6]`, never `tea[1:18]`**: the tooltip crosstabs are quadratic in the number of active variables, measured at 1.2 s against 10.4 s, and six active variables reach every code path eighteen do.

**The roadmap's premise for this phase was half stale, and checking it first changed the work.** A full AST scan of `R/` — walking every parse tree, discounting `~` and `\(.)` scopes — found **exactly one** bare `.`, the `. = NULL` declaration itself: 1a's 25 stranded placeholders are all gone, so the argument matrix is regression prevention rather than bug-hunting. Of the three "broken call shapes", two (`ggmca_data(cah =)`, `HCPC_tab(clust =)`) already worked and had simply been called wrongly by 1a's harness; only `ggmca_with_base_ref()` failed, because its signature was `(res.mca, axes, keep)` and a sibling-shaped call bound a 300-row data frame to `axes`.

**Writing the tests turned up five defects the roadmap did not list**, each fixed rather than pinned, since a test that locks in a bug is worse than no test:

- **`ggmca_initial_dims(keep =)` was a hard error** — `keep()` unqualified at a site where `purrr` is not imported, so a documented argument died with "could not find function". Shipping since 0.3.2.
- **`ggmca_initial_dims()` could not render an all-binary MCA at all** — the `x*` columns are one per level of a variable *group*, so a battery of yes/no items (the ordinary MCA input) never yields an `x2`, and both branches plot `x2` against `x1`. The guard existed in the single-variable branch only; it is now hoisted above the branch.
- **The weighted `n` was printed even when it equalled the unweighted one.** Inside one `mutate()`, `count` had already become the string `"n: 36"` before `if_else(count == wcount, ...)` compared it to a number, so the condition was always `FALSE`. Both branches now compare the `*_base` columns, and the documented invariant — a tooltip states the weighted n only when it differs — is finally true.
- **Four exports defaulted an argument to itself** (`res.mca = res.mca` ×3, `res.ca = res.ca`), so omitting it gave "promise already under evaluation" instead of "argument is missing".
- **`cah`'s roxygen invited the wrong call.** It reads as "pass the clusters"; passing them hit `cah %in% sup_vars` and died on "the condition has length > 1". `cah` is now documented as a column *name* and guarded with a message that says so.

**The plot-object seam is fixed, not deferred.** `ggmca_plot()`/`ggca()` built their result with `append()`, which flattens the S7 ggplot into a plain list wearing `c("gg", "ggplot")`: `ggplot_build()`, `grid.draw()` and therefore `ggsave2()` stopped dispatching, and `print()` drew only as a side effect of `print.default()` recursing into the nested plot — while dumping the whole list to stdout. The three hints now ride as **attributes** (4 write sites, 4 read sites), the object stays a real ggplot, all four generics work again, `print()` is silent, and the hints survive `+`.

**One name per thing across the MCA family.** The microdata is `data` everywhere (it was already the majority: `MCA2`, `PCA2`, `HCPC_tab`, `mean_sd_tab`, `ggmca_initial_dims`) and the plot model is `plot_data`. That pairing was forced: `ggmca_plot()`'s first argument was itself called `data`, and since `ggmca()`'s signature is `ggmca_data()`'s plus `ggmca_plot()`'s with no overlap, renaming `dat` alone would have given `ggmca()` two `data` arguments. `ggmca_with_base_ref()` gained `data` in second position, where callers expect it. All four old spellings are **soft-deprecated, not removed**: routed through one shared `renamed_arg()` helper in `utils.R` that warns once per session, the way `lifecycle::deprecate_soft()` does without taking the dependency. Old names sit last in each signature so no positional call can reach them, and a bare numeric in `ggmca_with_base_ref()`'s new `data` slot is routed back to `axes` — a data frame is never an `axes` and a bare numeric is never microdata, so the two are told apart safely.

**Verification was a 41-entry behaviour digest** over `tea`/`mtcars`/`gss_cat`, captured from the pristine tree before any edit and diffed after: the argument matrix, the fitted-object slots, all five table functions, and the *built* layer data of ten graphs (built, not the object, so the comparison survives the seam change). **40 of 41 came back byte-identical**; the one change is `ggmca_initial_dims()` going from error to a working plot. Two vacuous paths were caught by the digest itself — `tooltip_vars` and `tooltip_vars_1lv` are byte-identical to a plain call without `active_tables`, which is exactly the trap the argument matrix exists to document.

**Three more defects the plot tests turned up, all fixed.** `type = "points"` sized by `wcount`, which existed only once a tooltip table had been built, so a plain call failed; `wcount` is now derived from the column margin whenever the crosstabs do not supply it — `marge.col * (n active variables) * population`, verified to reproduce the crosstab figure exactly — and the central point takes the population as its weight. `type = "numbers"` mapped `label = .data$numbers`, a column nothing in the package ever creates, and so failed in **every** configuration; it never worked, nothing can depend on it, and it is removed from the `type` vocabulary rather than left as a trap. And `.data$x` inside a tidyselect context (`select`, `pull`, `relocate`, `nest`, `unnest`, `pivot_longer`, `dplyr::vars`) is deprecated by tidyselect 1.2.0: **59 sites** across six files, found with a paren-aware scanner rather than a line grep, because many calls span lines and several wrap the selection in `c(-x, -y)`. Data-masking `.data$x` — inside `mutate()`, `filter()`, `aes()` — is correct and untouched. ⚠ The distinction that matters: `dplyr::vars()` is a selection, **`ggplot2::vars()` is data-masking**, so `facet_wrap(vars(.data$lvs))` must keep its `.data$` or it facets by a constant string. The scanner conflated the two and was caught by reading its diff, which is the only reason this note exists. The suite's warning count fell from 330 to 38, the remainder being unrelated ggplot2 scale warnings.

**Verification of those three was a tolerant object-level diff against a `git archive` of pristine `HEAD`**, not a hash: 20 cases loaded into separate sessions and compared column by column with `all.equal`. Exact hashing turned out to be the wrong instrument — the *same* pristine tree run twice in two processes differs bitwise by ~7e-15 in the MCA coordinates, which is FactoMineR/BLAS non-determinism, not a change. Under tolerance the only differences are the intended two: `vars_data$wcount` newly present or newly filled (15 cases), and the profile `interactive_text` losing its redundant weighted-n line (3 cases). All five table functions and every coordinate came back identical.

#### Phase 1c — drop kableExtra, render html tables with tabxplor

`mca_interpret(type = "html")` was the package's last kableExtra consumer and the last place "tables are tabxplor's job" was broken. The ten `kableExtra::` calls and the `new_group` / `last_row` / `totrows` / `questions` index arithmetic that fed them are gone, replaced by two internals in `R/tables.R` — `mca_interpret_tab()`, which builds a `tabxplor` table, and `mca_interpret_legend()` — plus one `tab_html()` call. **-4 packages / -5.4 MB, measured: `kableExtra`, `rstudioapi`, `svglite`, `textshaping`**, taking the tree to 129 packages / 198.3 MB. `R CMD check` 0/0/0.

⚠ **The phase's stated premise was wrong, and checking it first changed the work.** The roadmap and `dev/dependency-audit.md` both said `tab_html()` "silently degrades to an unstyled table" on anything but a pure `fmt` tibble, so the html branch would need a shape it could not keep. It does not: `tab_render_vars()` asks only for one `fmt` column and one factor column, and **character columns sitting between `fmt` columns are carried through and styled normally**. The first probe that suggested otherwise was missing a `role = "level"` index column, not tripping over the character columns. So no tabxplor patch was needed, and the level-name columns stayed exactly where they were.

**Three kableExtra hacks became native semantics.** The two-line `Axe 1: 18.4%` / `of variance` label cell is a real `rowspan` cell on a declared index column (`tabxplor::new_lvl()`); the blanked repeated `Question` and the rules around it come from the row model; and `row_spec(totrows, bold)` is `row_kind = "total"` **plus `ref = "tot"`** — `row_kind` alone draws the rules but leaves the row greyed, since the bold-black anchor is decided by `get_reference()`, which reads `ref`.

**The table was rebuilt around the axis, not around the question.** Making `Axe` the row *variable* (`role = "var"`) and the question its level buys two things at once, both from tabxplor's own rules: a row variable's name is written once per block in a column of its own, and only the innermost label column draws the thick rule — so the table has **one rule per axis instead of one per question** (4 instead of 44 on `tea`'s eighteen variables), and the heading carries the axis's raw percentage and Benzecri's modified rate (`Axe 1: 9.9% of variance (mod. 57%)`), which gives `benzecri_mrv()` its first internal caller. ⚠ That heading is **wrapped, not rotated**: `tab_vname_plan()` turns a name only when it fits the block's vertical capacity, ~1.75 characters per row less one, so a 34-character heading would need a 20-row block and never gets one. Wrapping bounds the column at 13 characters (`TX_VNAME_MAX`) anyway, which is what the width worry was about. A question's positive and negative levels now face each other on **one row**, padded to the longer side when a question keeps several levels on one: 44 rows to 28 on `tea`, axes 1:3, with nothing lost. ⚠ The packer keys on the two contribution columns, never on the level names — the `All levels` row carries both figures and no name, and keying on names emptied it.

**`pct` prints and `ctr` colours, and that separation carries two facts the old table could not state.** The colour is `color = "contrib"`, whose declaration carries `break_under` as well as `break_over` and whose own doc reads *"signed contribution to the chi-squared"* — so **negating `ctr` on the negative side puts that level on the red half of the same ×1/×2/×5/×10 ladder while the displayed percentage stays positive**. And the total row's `ctr` is the **mean** contribution, not the sum it displays: `color = "contrib"` grades every cell against the total row, so the ladder now states Le Roux and Rouanet's own threshold — the rule that decided which levels are in the table at all. A question's own contribution (and, with `spread = TRUE`, its spread) is written into every cell and shown once, through the per-cell `display` field and its `"blank"` token, so the figure is still there to sort or export. ⚠ `get_num()` follows the display, so a hidden repeat reads `NA` there; the value lives in `$pct`.

**New argument: `spread = FALSE`.** Opt-in, and it governs the html table only — the `type = "console"` tibble always carries every column, that path being unchanged by contract.

**The legend is ggfacto's own**, built by `mca_interpret_legend()` with `color_legend = FALSE`: tabxplor's generated sentence describes a crosstab's over- and under-representation *against independence*, which an axis has no notion of. The swatch classes and the break values are read from tabxplor at call time (`get_color_breaks()`), so the words cannot drift from the palette. Hover tooltips are off: every figure already has a cell of its own.

⚠ **Two tabxplor findings, neither worked around badly.** A `tab_var` column whose **name contains a space** silently loses its `rowspan` and repeats down every row, and an empty name errors inside `tab_label_runs()` — the axis column is therefore named `Axe`, not `" "` as the kableExtra table had it. And a hand-built table needs `meta = list(render_extras = list(n = "no"))`, or tabxplor materialises a synthetic empty base-count column at render time. `<ctr>` as the column's unit tag was investigated and **deliberately not implemented**: the tag follows the displayed token, so it needs `display = "ctr"`, which prints the signed value (`-20%`); `pct_type = "none"` gives `<%>` but costs the total row its colour reference.

**Verification.** The console path was proved unchanged by a six-cell digest (`tea[1:6]` and `tea[1:18]` × `axes` 1, 1:2, 1:3, 1:5) captured from a `git archive` of pristine `HEAD` and re-run after the edit. Bitwise comparison is the wrong instrument here, as Phase 1b already found: **pristine `HEAD` compared with itself across two processes is 0/6 bit-identical**, and 5/6 equal within 1e-10 — and `HEAD` vs the new tree has exactly the same profile, the one loose entry (`tea[1:18]`, axes 1:5) being unstable at `HEAD` too, at the same magnitude. The suite went from 213 to 241 assertions, the ten new ones covering the packing, the two `pct`/`ctr` invariants, the display blanking, the `spread` argument, and `tab_render_vars()` not degrading — the single predicate that decides whether the table styles at all. ⚠ The first blanking test was **vacuous**: `tea[1:6]` is a battery of binary items, so every question packs to exactly one row and nothing is ever a repeat. It needs multi-level variables and a third axis, which is what `fx_mca_multi()` is for; the binary case is now pinned as its own fact.

#### Phase 1d — a knitted widget writes itself to its own file

An interactive graph embedded inline is a raw HTML block of several megabytes **on one line**, which is what pandoc's markdown reader handles worst: a bookdown book merges every chapter into one file, and `formations_stat`'s M2 book reached 30 GB of resident memory and killed the machine. Setting `options(ggfacto.widget_dir = "auto")` now makes every widget the package returns write itself to `widget_<chunk label>.html` under the chunk's `fig.path` and put an `<iframe>` in the document instead. That book's merged markdown fell from megabyte-long lines to under 4 000 characters, and its peak memory to **4.2 GB**.

**One new file, `R/knit.R`**, holding the seam: `as_ggfacto_widget()` tags what `ggi()`, `ggmca_3d()` and `ggpca_3d()` return, and `knit_print.ggfacto_widget()` (registered in `.onLoad()`, so knitr stays a `Suggests`) writes the page. Unset, the option changes nothing and the widget is embedded as before. Interactive display is untouched.

**The geometry is resolved in R, once.** The frame gets `width:100%` and an `aspect-ratio` taken from the plot's own `height_width_ratio` — the attribute of the plot-object seam, which is what keeps an analysis' axes isotropic. Without one (the 3D plots) the chunk's `fig.width`/`fig.height` decide; `out.width` counts only as a percentage. No JavaScript negotiates anything.

⚠ **Three silent traps, all pinned by `test-knit.R`.** The tag goes **after** the widget's name class and **before** `htmlwidget`: prepending breaks `htmlwidgets`, which reads `class(x)[1]` to find the JavaScript binding, and the page comes out blank; appending lets `knit_print.htmlwidget` win the dispatch. `resolveSizing()` reads the **top level** of `sizingPolicy` for a standalone page and the `browser` scope only for the resizing script, so both must be set or the page keeps a 960×500 box. And every dependency is **renamed with a `ggfacto-` prefix**, because htmltools resolves a name clash by keeping the highest version: plotly asks for jQuery 3.5.1 while a gitbook template ships 3.6.0, and the page pointed at a directory that never existed.

**The page is assembled from htmltools primitives, not `saveWidget()`.** `saveWidget()` insists on copying the libraries into a directory *below* the page — 23 MB beside each graph, or 8.3 MB per graph self-contained, pandoc base64-encoding the fonts. Rewriting each dependency's `src` as an `href` renders the same tags with no copying; shipping the files is the document's job, done by handing them to knitr as chunk metadata. Where they land is `options(ggfacto.widget_lib_dir)`, relative to the document, default `"libs"` (bookdown). Get it wrong and the page **says so** instead of showing an empty frame.

**Removed:** `ggi(iframe = )`, `ggi(pixel_width = )` and the `widgetframe` dependency — a pym.js frame whose height was negotiated in JavaScript, on an unmaintained package, and whose own documentation warned it produced a blank graph under rmarkdown. `ggi(savewidget = TRUE)` now writes **one** standalone file instead of an inseparable pair.

---

#### Phase 1e — une famille de tableaux-résumés, lisibles par une IA

Demandé par `formations_stat` (phase 1u) : l'exploration d'enquête par IA s'arrêtait à la porte de
l'analyse géométrique, faute d'une sortie qu'une machine puisse lire — un plan factoriel se juge sur
une figure, là où `tab_md()` rend un tableau en texte. Suites vertes : `ggfacto` **333** (241 avant),
`R CMD check` 0/0/0 ; `tabxplor` **4 677**, `check` 0/0/0.

**Trois contrats pour une même famille en sont devenus un.** `mca_interpret()` était la seule fonction
qui *rendait*, avec un `type = c("html","console")` qui ne ressemblait à rien d'autre ; `pca_interpret()`
et `HCPC_tab()` rendaient un tableau et laissaient l'appelant l'imprimer. Les cinq rendent désormais
**un** tableau `tabxplor`, marqué de la sous-classe que `new_tab(class =)` prévoit, et le format est une
décision de *print* : `options(ggfacto.print = "html" | "md" | "console")`. Ce qui se pipe, se filtre et
s'exporte reste donc un tableau — les cinq `pca_interpret(acp) |> tab_html()` d'un cours continuent de
marcher — et un `.md` s'obtient par le geste habituel, `|> tab_md(css = FALSE, print = FALSE)`.

**`ca_interpret()` est neuve, et elle enseigne ce que l'AC n'enseignait pas.** Un chapitre d'AC lisait
jusqu'ici deux tableaux de contributions faits à la main, **sans cos² ni signe des coordonnées** : le
côté de l'axe se lisait sur le graphique seulement. La fonction est le miroir exact de `mca_interpret()`
et partage son constructeur, `gda_poles()`. ⚠ **Le seuil est une propriété de l'ENSEMBLE, pas du
tableau** : les lignes et les colonnes d'une AC somment chacune à 100 % de la variance de l'axe, sur un
nombre de points différent, donc une moyenne commune garderait trop de l'un et trop peu de l'autre.
Chaque marge a son seuil et sa ligne de résumé.

**`complete = TRUE` ouvre la coordonnée et le cos², une colonne par nombre.** Une cellule composée
`ctr (coord ; cos²)` a été examinée et écartée : `fmt` est un enregistrement **typé** sur un
pourcentage, une moyenne ou un effectif, et aucun de ses champs n'a pour unité une coordonnée
factorielle — chacun rendrait `0.83` en « 83 % » ou avec les décimales du contrib, et un `fmt` ne porte
qu'un `digits`. Une colonne par nombre garde à chacun son unité, ses décimales et son échelle, et
n'ajoute aucun *token* à `tabxplor`.

⚠ **Un nombre est normalisé AVANT que tabxplor le gradue.** `ctr` porte le **multiple de la contribution
moyenne** — négatif du côté négatif — et toute ligne de résumé porte exactement 1. Les couleurs sont
identiques à celles d'avant (`ctr/moyenne` dans les deux cas), mais un tableau portant **plusieurs**
lignes de résumé — une par axe, ou une par marge — ne peut plus se graduer contre la mauvaise, ce qui
était une ambiguïté silencieuse dès que deux axes étaient demandés.

⚠ **Seul ce qui a une échelle est coloré, et l'épreuve sur du vrai matériel l'a tranché.** Sur une ACM
de neuf questions binaires, la coordonnée graduée sur l'échelle de l'écart-type (0,1 / 0,2 / 0,4 / 0,8)
sortait **entièrement** au cran maximal — une coordonnée d'ACM vaut couramment 1 à 3 écarts-types — et
le cos² coloré contre les **50 %** de la règle d'ACP sortait **entièrement rouge**, les modalités
retenues allant de 10 % à 48 %. Un nuage d'ACM a des dizaines d'axes : le seuil ne s'y transporte pas.
Les deux colonnes s'impriment donc **sans couleur** en ACM et en AC, et se lisent par comparaison entre
les points affichés ; l'ACP les garde toutes les deux, parce que la coordonnée y **est** une corrélation
et que le seuil y a un sens. Le seul seuil absolu qu'ait un axe factoriel est celui de la contribution.

**Les valeurs propres voyagent sous le tableau**, en tableau subordonné (`meta$footer_tabs`, la phase 5
de `tabxplor`) : % expliqué, % cumulé, taux modifié de Benzécri et son cumul, plus la règle du choix des
axes écrite en toutes lettres. Un tableau, pas un barplot — un pourcentage cumulé ne se lit pas sur une
barre, et c'est lui que les trois règles enseignent. `benzecri_mrv(fmt = TRUE)`, exporté sans appelant
depuis toujours, en a enfin un. ⚠ `res$eig` s'arrête à `ncp` : un taux modifié calculé sur un ensemble
tronqué cumule à 100 % du mauvais total, et le pied le **dit** au lieu de le taire.

**Un défaut préexistant trouvé au passage : `pca_interpret(axes = 1)` échouait.** Un seul axe ramène une
matrice à un vecteur nommé, la colonne s'appelle alors `value`, et le remodelage qui suit ne trouve
rien — une erreur, au dernier pas, sur l'appel le plus ordinaire qui soit. Le triplet est désormais
**construit** par axe au lieu d'être remodelé : les noms ne dépendent plus des données.

**`eig = FALSE` est né du cours.** Un chapitre réimprime le même résumé dans quatre ou cinq sections
successives pour le commenter colonne par colonne : le bloc des valeurs propres serait apparu autant de
fois. L'argument est sur les trois fonctions.

**La légende perd ses pastilles de couleur, et c'est un correctif.** `mca_interpret_legend()` écrivait
`<span class="p1">×1</span>` dans `subtext` — du html brut qui fuitait tel quel dans un `.md` et dans un
`.xlsx`, `subtext` étant figé à la construction, avant que le médium soit connu. La phrase est désormais
sans balise et juste dans les trois médias ; les seuils continuent d'être lus par `get_color_breaks()`.

**`mean_sd_tab()` entre dans le dispositif.** Elle rendait un tibble de nombres bruts, sans `fmt` : elle
rend `variables | n | mean | sd | sd/mean` en colonnes `tabxplor`, **un seul enregistrement imprimé
trois fois** — l'écart-type et le coefficient de variation sont des jetons que tabxplor DÉRIVE de la même
variance, donc rien n'est stocké deux fois et les trois colonnes ne peuvent pas se contredire.

**`R/interpret.R` est neuf** (la famille et ses internes partagés) ; `R/tables.R` garde les deux tables
qui décrivent les données. `.onLoad()` cesse de faire `options("x" = NULL)`, qui *retire* une option au
lieu d'en poser une : chaque lecteur énonce son propre défaut, ce qui rend « non posée » distinguable et
permet à `ggfacto.print` d'être html sans dépendre de `tabxplor.print`.

**Ce que la phase n'a pas fait.** Pas de barplot d'éboulis, et pas d'image dans un `.xlsx` : `tab_xl()`
n'a aucune couture pour insérer un graphique et `openxlsx` n'est pas une dépendance. ⚠ Un `tab_md()`
écrit à la main a besoin de `color_legend = FALSE` — la légende engendrée par tabxplor parle de
contribution au chi², ce qu'un axe factoriel ignore ; les méthodes de ggfacto la coupent d'elles-mêmes,
et la console, elle, la garde (elle est toujours terse et n'a pas d'interrupteur).

#### Phase 1f — affiner les tableaux-résumés, et les traduire

Relecture du *maintainer* sur la famille livrée en 1e : des étiquettes de type pillar qui disaient
faux, des légendes trop bavardes, un seuil de contribution non paramétrable, des noms de colonnes
opaques, et aucune version française. Suites vertes : `ggfacto` **377** (333 avant), `R CMD check`
0/0/0 ; `tabxplor` **4 704** (4 677 avant), `check` 0/0/0.

**Chaque étiquette de type dit désormais ce que la colonne somme.** `cos2` s'écrit `<row%>`, parce
qu'un cos² somme à 100 % **sur les axes**, c'est-à-dire le long d'une ligne de `$var$cos2` ; les deux
pourcentages de l'éboulis s'écrivent `<col%>`, parce qu'ils somment sur les axes empilés ; et
`eigenvalue` s'écrit `<var>`. ⚠ Cette dernière a demandé un correctif **dans `tabxplor`** : le jeton
`var` n'avait pas `geometry = "level"`, alors que son propre `sd` l'a, donc `display = "var"` sur une
colonne `level_mean` sortait `<mean-var>` — « la variance de la moyenne ». Une variance est un niveau,
pas une comparaison.

**Le seuil de contribution est un argument, et son libellé le suit.** `min_contrib = NULL` garde la
moyenne (Le Roux et Rouanet), `0` garde tout, un nombre garde ce qui contribue au moins autant, **sur
l'échelle affichée, en pourcentages**. La ligne de résumé s'appelle alors `Above mean ctr`,
`All levels` ou `Above 5%`. ⚠ **Un libellé qui nomme un ensemble qu'il ne totalise pas est la seule
chose que ce tableau ne doit jamais faire** : les trois formes sont donc calculées à l'endroit où le
filtre est appliqué, jamais écrites à côté.

**La légende passe de deux pavés à trois lignes, une par statistique.** Chacune donne le nom complet
et rien d'autre ; celles qui sont colorées ajoutent leur échelle en forme terse. Le « comment lire » —
quel pôle, quel signe, quelle taille — disparaît : il vit dans le cours et dans la référence `agd.md`
du skill, pas sous chaque tableau. `subtext` étant un vecteur, une ligne par élément suffit.

**Le tableau des valeurs propres devient lisible d'un coup d'œil.** `% variance` et `cumul.` sous un
`col_var` `Variance`, le taux modifié et son cumul sous `Benzecri` — deux blocs encadrés là où il n'y
avait aucun `col_var`, donc aucun filet. Une ligne `Total` dit ce que les axes totalisent. `n_axes = 8`
borne l'impression **indépendamment de `axes =`**, et au-delà la **dernière** ligne est montrée quand
même, précédée d'une ligne d'ellipse (`row_kind = "blank"`, le vocabulaire que `ROW_KINDS` a déjà) : le
lecteur sait toujours combien d'axes le nuage porte. La légende de pied disparaît — la règle du choix
des axes est un enseignement, pas une note de tableau.

**Une barre de données derrière `% variance`, et c'est une vraie barre html.** `tabxplor` gagne
`set_bars()` / `get_bars()` et une classe `.tx-bar` ; ce qui est inline sur le `<td>` est une
**longueur**, jamais une couleur, donc `theme = "auto"` reste entier et la règle du header de
`tab-css.R` est réécrite pour l'énoncer au lieu d'être contredite en silence.

**`pca_interpret()` absorbe `mean_sd_tab()`, qui est dépréciée.** `mean`, `sd` et `sd/mean` sont les
premières colonnes `fmt`, sous un `col_var` commun, **sans colonne `n`** : ce qui décrit les variables
et ce qui interprète les axes se lisent dans le même tableau, ce qui était la raison d'être de la
fonction. Et le `contrib` d'une ACP n'est plus coloré — la coordonnée à côté dit déjà quelles variables
bâtissent l'axe, et graduer un même fait deux fois est deux canaux saturés pour une seule lecture.

**Une colonne s'appelle `<statistique>_<col_var>`, et l'export retire le suffixe.** `coord_Axe 1`,
`contrib_Axe 1`, `cos2_Axe 1` dans le tibble — des noms uniques qu'on peut indexer — et un `coord` nu
sous un en-tête `Axe 1` en html, en markdown et en Excel. C'est `tab_col_var_header()` qui strippe,
exactement comme pour `Other_race`. ⚠ **Un défaut y a été trouvé et corrigé dans `tabxplor`** : la
comparaison portait sur le nom **enveloppé** (espaces fines insécables, `<br>` aux coupures) face à un
`col_var` brut, donc tout `col_var` contenant une espace laissait fuiter son suffixe dans l'en-tête.

**`crosstab =` disparaît de `ca_interpret()`, et `vars =` le remplace.** Le lecteur — humain ou IA —
demande les deux tableaux séparément, et la page de manuel dit lequel : `tab(..., pct = "row",
color = "contrib")`, des pourcentages colorés par la contribution, jamais `display = "ctr"`. ⚠ **Il
fallait quand même une entrée pour les noms des deux marges** : `FactoMineR::CA()` **détruit**
`names(dimnames())` de `call$X` comme de `call$Xtot`, même quand l'entrée était une `as.table()` aux
dimnames nommés. Sans `vars =`, les deux marges s'appelleraient `Rows` et `Columns` pour toujours.

**La traduction française est le dispositif de `tabxplor`, transposé au domaine `R-ggfacto`.**
`R/i18n.R` neuf, `po/R-ggfacto.pot`, `po/R-fr.po` (17 messages, tous traduits), le `.mo` committé,
`Config/potools/style: explicit`, `dev/update_translations.R`, et un `lang =` sur les trois fonctions.
⚠ **Le vidage de cache doit re-lier NOTRE domaine** : glibc indexe sur `(domaine, msgid)`, donc celui
de `tabxplor` laisse `R-ggfacto` en cache et la seconde bascule de langue d'une session ne fait rien.
⚠ **Ce qui est traduit est de la prose, jamais un nom** : la légende, l'en-tête d'axe, le libellé de la
ligne de résumé, `Rows`/`Columns`/`Total`. Un nom de colonne qui changerait avec la langue ne pourrait
plus être indexé — c'est aussi la règle de `tabxplor` (`dev/french_glossary.md`). ⚠ Un `%` littéral
dans un `msgstr` de `gettextf()` est lu comme une spécification de conversion : les trois chaînes de
format le doublent (`%%`), les `gettext()` gardent le leur simple. `test-i18n.R` éprouve chaque trait
**deux fois** — un bloc anglais non gardé, son jumeau français gardé par `skip_if_no_gettext()` —,
parce que `R CMD check` force une locale C où gettext ne traduit pas du tout.

**Un tableau subordonné s'imprime en *pipe table* en console.** `tabxplor` exporte `tab_pipe()` et
`print.tabxplor_tab()` s'en sert pour ses `meta$footer_tabs` : l'éboulis était un second
`tabxplor_tab` avec sa propre grille pillar, il est désormais une note, comme les tables de forme et
d'hypothèses le sont déjà.

⚠ **Deux défauts silencieux trouvés en relisant la page d'exemples à l'écran, et non en lisant le code.**
(1) Les trois colonnes qui ouvrent `pca_interpret()` étaient **entièrement vides** : le nom de champ
lu était `call$ecart.red`, qui n'existe pas — celui de `FactoMineR` est `call$ecart.type` —, et le
repli sur `NA` ne dit rien. L'instantané montrait les blancs et l'assertion « une seule variance pour
les trois colonnes » passait sur `NA == NA` : **un test de forme ne remplace pas un test de valeur**,
et la garde en éprouve maintenant une. L'écart-type est désormais **calculé** depuis `call$X` et
`call$row.w` plutôt que lu : `ecart.type` est le diviseur de normalisation, il vaut 1 pour toutes les
variables sous `scale.unit = FALSE`, et une colonne d'écarts-types qui n'affiche que des 1 est pire
qu'une colonne vide. (2) La barre de données ne s'affichait **jamais** — un défaut de `tabxplor`,
corrigé là-bas : `tab_wrap_text()` renomme les colonnes avant le rendu html, et la liste `bars`
gardait les noms bruts, donc `% variance` ne correspondait plus à rien. Son test de la phase 6
employait `"Married"`, un seul mot ; le nom du seul appelant réel en contient une espace.

⚠ **La crainte du *maintainer* sur les modalités multiples d'un même côté d'axe est infondée, et c'est
mesuré.** `gda_poles()` complète le côté court de `NA` (`k = max(nrow(pos), nrow(neg), 1)`), donc rien
n'est coupé : le livre rendu porte **trois** lignes de continuation — `JV` sur l'Axe 2, `RADIO` sur
l'Axe 3, `MUSIQUE` sur l'Axe 4 —, et sur l'Axe 1 les huit contributions positives affichées somment
exactement à la ligne de résumé. Seul défaut, cosmétique : rien ne relie visuellement une ligne de
continuation à la ligne du dessus — voir la phase 1g.

**Ce que la phase n'a pas fait, et pourquoi.** Le **cadre des légendes de `tabxplor`** n'est pas
construit : le *maintainer* l'a demandé **esquissé**, et il l'est dans
`~/github/tabxplor/dev/legend_and_side_tables.md`, qui dit en tête qu'il est un brouillon. Tant qu'il
n'existe pas, `ggfacto` coupe la légende engendrée (`color_legend = FALSE`, parce qu'elle parle de
contribution au chi², ce qu'un axe factoriel ignore) et écrit la sienne en texte simple : **elle n'est
donc ni colorée, ni traduite au rendu** — elle l'est à la construction, avant que le médium et la
langue de lecture soient connus. C'est le prix à payer, et il est nommé. La barre de données en Excel
et l'étiquette `<row%-ctr>` d'une colonne de contributions partent en phase 7 de `tabxplor`.

#### Phase 1g — lier une ligne de continuation à sa question

Quand une question garde plusieurs modalités du **même** côté d'un axe, `gda_poles()` les empile sur
des lignes successives dont les cellules `Question` et `contrib` sont blanchies (le jeton `blank`).
Rien ne dit visuellement que ces lignes appartiennent à la question du dessus : ni retrait, ni
*rowspan*. Un retrait sur le libellé de modalité est la piste la moins coûteuse ; un `rowspan` sur la
cellule de question serait plus juste en html mais n'a d'équivalent ni en markdown ni en console, et
c'est `tabxplor` qui rend les trois. À éprouver sur `05-ACM.qmd`, où les trois cas du corpus vivent.

#### Phase 1h — le pied de tableau vient de tabxplor

**`ggfacto` n'écrit plus de légende : il en re-nomme une.** Suite verte : **397** (377 avant),
`R CMD check` **0/0/0**. Les mots sont dits par `set_legend_words()`, la phrase et l'échelle sont
engendrées au rendu, et `gda_poles_legend()` disparaît avec les quatre choses qu'elle coûtait —
l'échelle recopiée, un seul registre pour cinq médias, une phrase figée dans la langue de la
construction, et, en console, la ligne de tabxplor imprimée **au-dessus** de la sienne, là où
`color_legend = FALSE` ne portait pas.

**Ce que la console imprimait, et qui résume la phase :**

```text
# contribution to Chi2 (vs the mean): x10 x5 x2 x1 x1 x2 x5 x10        <- tabxplor, et faux ici
# contrib: contribution to the variance of the axis, vs the mean contribution: x1 x2 x5 x10
```

**Les mots disent maintenant ce que l'ancienne ligne taisait : le signe de l'échelle est le PÔLE de
l'axe.** `gda_poles_tab()` nie `ctr` du côté négatif, donc `.m1`-`.m4` marquent le pôle négatif et
non une sous-contribution — un lecteur ne pouvait pas le déduire de « vs la contribution moyenne ».
`lead_over` / `lead_under` le disent en toutes lettres.

**Le glossaire ne garde que ce que la couleur ne gradue pas**, et il gagne une justesse au passage :
la ligne unique d'avant portait le nom d'une colonne (`contrib`, la contribution de la question) et
l'échelle d'une autre (`ctr`, celle de la modalité). Une ligne chacune. Sous `color = FALSE`, où
aucune légende n'est engendrée, `ctr` est nommée là.

⚠ **L'ACP colore deux quantités avec UNE mesure.** `difference` sur deux échelles : `word_std` va à
`coord` (écarts-types), `word` à `cos2` (points de %). `lead_over` / `lead_under` sont **partagés**
par les deux lignes, donc neutres et vrais des deux. Et `ref` est **refusé** sur `difference` — sa
référence est une ligne du tableau, pas un concept —, si bien que la forme compacte bracketise
encore « (Total) » alors que la ligne Total de ces deux colonnes est vide ; la forme longue, celle
de tous les exports, ne la nomme pas. Assumé, pas contourné.

**Une correction dans `tabxplor` était nécessaire, et elle est petite** (sa phase 7c) :
une légende qui nomme ses colonnes imprimait le nom BRUT du tibble, `coord_Axe 1, coord_Axe 2`,
alors que l'en-tête montre `coord` sous un bandeau `Axe 1`. La convention `<statistic>_<col_var>`
d'ici rendait donc le pied illisible dès que la légende engendrée revenait. Elle nomme maintenant
ce que l'en-tête nomme.

⚠ **Un contournement périmé était devenu porteur d'autre chose.** Les dix
`mutate(across(where(is_fmt), ~ set_color(., "diff"))) # sinon bug (no color)` des cours de M2 sont
un no-op mesuré (html **identique au bit près**) — mais ils font tomber la classe `ggfacto_summary`,
donc le tableau passait par `tab_html()` et gardait ses info-bulles, que `gda_render()` coupait.
`tooltips` devient donc une option par table : aucune pour un résumé d'axes, où chaque chiffre caché
a déjà sa colonne ; **toutes** pour `HCPC_tab()`, qui est un tableau croisé de pourcentages dont
l'effectif vaut d'être survolé. Les deux chemins de rendu produisent alors le même html, et les dix
lignes partent des cours.

**`HCPC_tab()` gagne une légende là où elle n'en avait aucune.** `gda_render()` la coupait et la
fonction n'en écrivait pas : ses exports html et md montraient un tableau coloré sans un mot sur ses
couleurs. Celle de tabxplor est juste telle quelle — c'est une différence de points de pourcentage.
`mean_sd_tab()`, dépréciée, cesse d'être la seule ligne non traduite du paquet : elle partage
désormais le msgid `sd/mean` de `pca_interpret()`.

**Le catalogue passe de 17 à 27 messages**, tous traduits, `tools::checkPoFile()` propre. ⚠ Un `%`
suivi d'une lettre dans un msgid est lu comme une spécification de format : « the 50 % mark » est
refusé, « of 50 % » passe. Le signe ferme donc la phrase.

**Les tests changent d'objet.** Ils lisaient `attr(x, "subtext")`, c'est-à-dire une phrase ; ils
lisent `tab_footer_text()`, c'est-à-dire ce qui s'imprime — plus un garde qui refuse toute valeur de
`get_color_breaks()` recopiée dans le gabarit, et un autre sur les deux lignes de l'ACP.
⚠ Les quatre instantanés de console bougent aussi pour une raison **étrangère** à la phase : depuis
la phase 7 de tabxplor un tableau subordonné s'imprime **au-dessus** en console (« la dernière chose
imprimée est l'objet qu'on peut piper »), et les instantanés dataient d'avant. Vérifié sur un
`tab()` nu, hors de ggfacto.

**Ce que la phase n'a pas fait.** Aucun graphique, aucun calcul, aucune valeur : le balisage du pied
seul bouge. `mca_interpret()`, `ca_interpret()`, `pca_interpret()`, `HCPC_tab()` et `mean_sd_tab()`
gardent leurs arguments. La phase 1g reste ouverte.

#### Phase 1i — un éboulis honnête, une seule option d'impression

**Cinq retours d'usage, dont un qui était une crainte infondée et deux qui étaient des défauts de
correction.** Suite verte : **412** (397 avant), `R CMD check` **0/0/0**.

**`MCA2()` et `PCA2()` calculent tous les axes (`ncp = Inf`).** `ncp` tronque `res$eig`, et
`benzecri_mrv()` renormalise sur les axes qu'il y trouve : le **même axe** recevait donc un taux de
Benzécri différent selon `ncp` — **57,4 % contre 55,4 %** sur `tea[1:18]`, six axes au-dessus de 1/Q
manquant à l'appel. Accessoirement `n_axes = 8` était **inerte par défaut** (`gap <- k < n` est faux
quand `nrow(eig)` vaut 5), donc l'ellipse et le dernier axe que la documentation promet n'ont jamais
été rendus sur un appel ordinaire. Coût mesuré du nouveau défaut sur une ACM de 9 234 × 15 :
**+0,03 s et +5 Mo**. On abaisse `ncp` uniquement pour nourrir `HCPC()`, qui classe sur les axes
retenus, et `FactoMineR` normalise `Inf` en un entier réel, donc le `1:call$ncp` de `varsup()` reste
sûr. `mca_interpret()` reçoit au passage le garde que ses deux sœurs avaient déjà : un `axes =`
au-delà du dernier axe indexait hors des bornes en silence.

**La ligne `Total` de l'éboulis est lue, plus supposée.** Elle écrivait `pct = 1` en dur : sur un
ajustement gardant cinq axes d'un nuage qui en porte 27, elle annonçait **100 %** là où les cinq
axes font **34,1 %**. Elle dit maintenant la part réellement présente. ⚠ Sur le taux de Benzécri
elle garde 1, et c'est exact : un taux modifié est normalisé sur les axes au-dessus de 1/Q que `eig`
**contient**, donc sa colonne somme à 1 quelle que soit la troncature — ce que la troncature déplace,
c'est chaque taux, pas leur total. Un ternaire mort (`if (n < 1L) "Total" else "Total"`) part avec.

**La barre de données quitte l'éboulis.** `tabxplor` cale une barre sur le **plus grand de la
colonne**, donc l'axe 1 d'une ACM à 9,9 % de l'inertie recevait `--tx-bar:100%` — mesuré, avec l'axe
2 à 82 %. Une barre pleine sous 9,9 % se lit « cet axe, c'est tout ». Elle revient quand `set_bars()`
saura fixer son échelle : le cas d'usage et deux pistes de cadre souple sont écrits dans la feuille
de route de `tabxplor` (phase 9).

**Le cos² n'est plus coloré nulle part.** L'ACP était la dernière exception : la règle des 50 % juge
**un axe entier**, pas une case, et la graduer était le même signal plausible et faux que la phase 1f
avait retiré de l'ACM et de l'AC. Seule la **coordonnée** reste graduée en ACP, où elle EST une
corrélation. `gda_pca_words()` perd donc sa seconde échelle et le glossaire gagne `cos2`, avec le
msgid que la famille employait déjà — une seule formulation, une seule traduction.

**`options(ggfacto.print)` disparaît : une AGD obéit à `options(tabxplor.print)`.** Un résumé et le
`tab()` deux lignes au-dessus s'affichaient d'après deux options différentes, alors qu'un script n'a
aucune raison d'en poser deux. La valeur `"md"` part avec — `tabxplor.print` n'en a pas, et la
recette d'un carnet est un `|> tab_md()` explicite, qui ne passait déjà pas par `print()`.

⚠ **Ce que la suppression a fait apparaître, et qui est un défaut de `tabxplor`.** `tx_print_html()`
écrit `getOption("tabxplor.print") %in% c(...)` : sur une option **non posée** cela vaut
`logical(0)`, et `if` s'arrête. Le cas n'est pas théorique — `.onLoad()` sème l'option, mais
`ggfacto` n'atteint `tabxplor` que par `tabxplor::`, donc **`library(ggfacto)` ne charge pas son
*namespace* et laisse l'option absente** (mesuré : `loadNamespace("tabxplor")` la sème,
`library(ggfacto)` non). `gda_print_html()` lit donc par `isTRUE()`, et la branche console **énonce**
son médium pour l'appel délégué. Sans cela un `mca_interpret()` imprimé dans une session fraîche
s'arrêtait. Signalé dans la feuille de route de `tabxplor` (phase 9), où `isTRUE()` suffit.

**L'ellipse dit désormais combien d'axes le nuage porte, et n'apparaît que s'il en manque.** Trois
défauts en un. (1) `n_axes = 4` sur un ajustement à cinq axes insérait une ellipse **entre l'axe 4 et
l'axe 5**, alors qu'aucun ne manquait entre les deux. (2) Le dernier axe montré sous l'ellipse donnait
une ligne dont les nombres n'avaient rien au-dessus d'eux à quoi se comparer ; le **compte** répond à
la seule question que le lecteur se pose, et il remplace cette ligne : `... of 27`, `... sur 27` en
français. (3) Cinq axes sur 27 s'affichaient **sans aucune ellipse**, donc sans rien dire qu'il en
manquait 22. ⚠ Le compte est **passé par l'appelant**, jamais lu sur `eig` que `ncp` tronque : une ACM
a `modalités - questions` axes (les **lignes** de `$var$coord`, dont seules les colonnes sont
tronquées), une AC `min(dim) - 1`, une ACP `min(variables, individus - 1)`. Vérifié aussi sous
`excl =`, où le compte reste juste.

**Chaque ligne de résumé d'une AC nomme sa marge.** Un axe en porte deux — une pour les lignes, une
pour les colonnes — et toutes deux affichaient `Above mean ctr` : le même mot deux fois, sans rien
dire lequel des deux jeux de contributions chacune totalisait. Elles disent maintenant
`race: above mean ctr` / `marital: above mean ctr`, et `Rows` / `Columns` faute de `vars =`. Le seuil
continue de commander la formulation (`: all levels`, `: above 5%`). ⚠ L'ACM **garde le libellé nu** :
elle n'a qu'un jeu, et c'est ce libellé que les cours et le skill `exploration-donnees` citent. Trois
msgid neufs, traduits avec l'**espace fine insécable** (U+202F) devant `:` et `%` que le catalogue
emploie déjà — une espace ordinaire ne s'y voit pas et ne se cherche pas.

**La crainte du *maintainer* sur les modalités multiples est infondée, et c'est mesuré.** Rien ne
déduplique ni ne tronque : `k <- max(nrow(p), nrow(n), 1L)` rend le bloc aussi haut que son côté le
plus long et `pad()` comble l'autre. Vérifié sur `tea` (la question `How` garde `lemon` face à `milk`
**et** `other`) et sur l'ACM du M2 (`JV`, deux modalités négatives sur l'axe 2). Un test le verrouille
désormais, et il compare les contributions **affichées** (`pct`) à la ligne de résumé — pas `ctr`,
qui porte le multiple de la contribution moyenne et vaut exactement 1 sur toute ligne de résumé.

**Les fixtures exercent le défaut livré.** `fx_mca()` et `fx_mca_multi()` passaient `ncp = 5` : le
`tea[1:6]` de référence porte six axes, la suite en testait cinq. ⚠ `tea[1:6]` a exactement **trois**
axes au-dessus de 1/Q, donc `ncp = 3` n'y renormalise rien — le test de divergence coupe à 2.

**Ce que la phase n'a pas fait.** Aucun fichier de `tabxplor` touché : les barres et le `isTRUE()`
sont écrits dans sa feuille de route et attendent leur session. Les exemples roxygen posent
`options(tabxplor.print = "html")` comme un script le fait, sans le restaurer — ⚠ `options(op)`
**retire** une option qui n'était pas posée, ce qui rallumait précisément le défaut ci-dessus au
milieu de `R CMD check`.


#### Phase 1j — la barre de données revient sous l'éboulis

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
