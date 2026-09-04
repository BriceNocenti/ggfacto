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

Thirteen files in `R/`, four groups. Every file carries a `# PURPOSE / # ROLE / # KEY CONSTRAINTS` header with fuller design detail: read it before the code.

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
- `utils.R` — factor helpers, the base-R string shim that replaced stringr, `weighted.var()`, vendored `where()`.
- `knit.R` — the knitr seam: tags every widget the package returns, and writes it to its own file with an `<iframe>` in its place when `options(ggfacto.widget_dir)` asks.
- `ggfacto-package.R` — imports, global bindings, `.onLoad()`, the deprecated `%>%` re-export.

**Other directories:** `man/` (roxygen-generated, never edit) · `tests/testthat/` (the package's contract: the exported entry points, the argument matrix, the tooltip and table goldens) · `dev/` (`.Rbuildignore`'d; holds `dependency-audit.md`).

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

**Five functions, one output contract** (`?ggfacto_summary`, `R/interpret.R`). Each returns **one** `tabxplor` table, tagged with the subclass `new_tab(class =)` provides, so it can be piped, filtered and exported like any other; the format is a print-time decision, `options(ggfacto.print = "html" | "md" | "console")`, read by `print.` and `knit_print.` — html by default, and the option is deliberately NOT seeded, so ggfacto's default does not depend on what `tabxplor.print` happens to be. `gda_render()` is the one call to the renderers, so html and md cannot drift on the two things only ggfacto knows: no tooltip and no generated legend. ⚠ A `tab_md()` written by HAND therefore needs `color_legend = FALSE`. ⚠ dplyr carries a table's tabxplor attributes but not its class, so a summary that has been through `mutate()` prints as an ordinary table — which is why the render options ride a plain attribute rather than `meta`: they die with the methods that read them.

**The eigenvalues travel under the table**, as a subordinate table (`tabxplor::set_footer_tabs()`), so every medium renders them below it and the rule for choosing how many axes to interpret is never a second call to remember. It is a table and not a barplot: a cumulated percentage — Benzecri's modified rate for an MCA, 80 % for a CA, an eigenvalue above 1 for a PCA — cannot be read off a bar, and `gda_eig_rule()` states the rule in words underneath. `ca_interpret(crosstab =)` hangs the source crosstab there too: a correspondence analysis draws the STRUCTURE of a crosstab's deviations and says nothing of their size.

`mca_interpret()` and `ca_interpret()` share one builder, `gda_poles()`. Its statistics are Le Roux and Rouanet's: only a point contributing more than the mean contribution **of its own set** is kept, and the spread between a group's positive and negative points is stated in percent of that group's own contribution. ⚠ The set matters: an MCA has one (the active levels, summing to 100 % over K points), a CA has two (its rows and its columns, summing to 100 % over different numbers of points), so one pooled mean would keep too many of one and too few of the other. The table is **the axes as blocks**: the axis is the row *variable* (so tabxplor writes its heading once per block, wrapped, with one thick rule per axis) and the group is its level; a group's positive and negative points face each other on one row, its own figure is carried in every cell and `display`ed once, and one summary row per (axis, set) gives the two sides' summed contributions — the pair that says whether an axis opposes two poles or one specific group to the average.

**A number is normalised before tabxplor grades it.** `pct` prints and `ctr` colours, which is what lets the sign of the coordinate ride the colour without reaching the page — but `ctr` holds the **multiple of the mean contribution**, negated on the negative side, and every summary row holds exactly 1. So a table carrying several summary rows (one per axis, or one per set) cannot grade against the wrong one, and `color = "contrib"`'s ×1/×2/×5/×10 ladder IS Le Roux and Rouanet's threshold.

**Only what has a ladder is coloured**, and the two families differ because the quantities do. The contribution has that threshold everywhere. A coordinate and a cos2 have one only in a **PCA**, where under `scale.unit` the coordinate IS a correlation — so the 0.1/0.2/0.4/0.8 steps read it end to end — and where the few axes make the course's 50 % / 75 % cos2 rule meaningful. In an MCA or a CA, `complete = TRUE` prints both and colours neither: a coordinate in axis standard deviations has no conventional cut-off, and an MCA cloud has dozens of axes, so every cos2 is structurally small — measured on nine binary questions, every retained level fell between 10 % and 48 %, i.e. entirely below the PCA threshold and entirely red. A ladder that does not fit the quantity is a signal that is plausible and false.

**The legend is plain text, and deliberately.** `subtext` is fixed when the table is built, before the medium is known, so an html span written there would reach a markdown file and an Excel cell as raw markup. The break values are read from `tabxplor::get_color_breaks()` at call time, so the words cannot drift from the palette.

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

`tests/testthat/` is the package's **contract**: it must fail when a user-visible fact changes, must not fail when an internal is redesigned, and must stay fast enough to run on every edit. Nine files: `helper-fixtures.R` (the cached analyses and plot models), `test-mca2-pca2.R` (the ingress normalisers), `test-ggmca-data.R` (the plot model and the argument matrix), `test-tooltips.R` (the crosstabs behind the hover), `test-plots.R` (that every graph builds, and the render-hint seam), `test-knit.R` (the widget seam), `test-interpret.R` (the interpretation tables and the output contract the whole summary family shares), `test-tables.R` (the two tables that describe the data), plus `test-str-shim.R` and `test-non-ascii.R` from 1a.

**Argument coverage is the point, not function coverage.** Several arguments are inert alone and only act alongside an enabling one — `keep_levels`/`discard_levels` need `sup_vars`, `tooltip_vars`/`tooltip_vars_1lv` need a table to be built at all, `cah` needs `profiles`. A test that omits the enabler passes while exercising nothing; the vacuous paths are pinned deliberately so nobody "simplifies" them back into nothing.

**Fixtures are `tea[1:6]`, never `tea[1:18]`.** Tooltip crosstabs are quadratic in the number of active variables: `active_tables = "active"` costs 1.2 s on six and 10.4 s on eighteen. Six reaches every code path. The models the suite reuses are memoised in `helper-fixtures.R`; the whole suite runs in about 30 s. The one exception is `fx_mca_multi()`, local to `test-interpret.R`: `tea[1:6]` is all binary, so `mca_interpret()`'s row packing collapses every question to one line there and its display blanking has nothing to hide — that needs multi-level variables and a third axis.

The suite is **small and serial**: 322 assertions, no `Config/testthat/parallel`, no `setup.R`, no i18n. ⚠ Do not turn parallelism on for it, and do not import tabxplor's worker, orphan and gettext conventions — see `~/github/tabxplor/CLAUDE.md` "## Testing" only if the suite ever grows enough to need them.

**Golden tests use `expect_snapshot()`** (`_snaps/*.md`), and only where the output is genuinely stable and worth the churn: the rendered tooltip text and the four interpretation tables (MCA concise and complete, CA, PCA). ⚠ The *rendered html* is never snapshotted — it is 7 kB of inlined stylesheet; an interpretation table's snapshot is the console print, taken with `n = Inf` under `options(ggfacto.print = "console")`, since pillar formats only the rows it shows and a slice without a summary row makes `color = "contrib"` warn.

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
