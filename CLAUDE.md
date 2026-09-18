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


#### Phase 1k — une AGD suit `options(tabxplor.print = "md")`

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
