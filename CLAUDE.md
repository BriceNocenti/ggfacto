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

Twelve files in `R/`, four groups. Every file carries a `# PURPOSE / # ROLE / # KEY CONSTRAINTS` header with fuller design detail: read it before the code.

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

- `tables.R` — everything returning a table: `benzecri_mrv()`, `mca_interpret()`, `pca_interpret()`, `mean_sd_tab()`, `HCPC_tab()`.
- `render.R` — `theme_facto()`, the material palettes, `ggi()`, `ggsave2()`, `plot_path()`, `outlims()`.
- `utils.R` — factor helpers, the base-R string shim that replaced stringr, `weighted.var()`, vendored `where()`.
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

`ggmca_plot()` and `ggca()` then carry render hints on the returned object as **attributes** — `css_hover`, `css_tooltip`, `height_width_ratio` — which `ggi()` and `ggsave2()` read back; both must tolerate their absence, since a plain ggplot has none. Attributes rather than list slots because the object must stay a real ggplot: `append()` flattens the S7 object into a plain list wearing `c("gg", "ggplot")`, and then `ggplot_build()`, `grid.draw()` and so `ggsave2()` stop dispatching, while `print()` draws only as a side effect of `print.default()` recursing into the nested plot. The hints survive `+`, so a user can keep extending the graph. Hover linking rests on a `data_id` convention: the ids are offset into disjoint bands — active variables from `1000`, HCPC clusters and answer profiles from `10000` — so that every point of one cluster shares an id and hovering any of them lights them all.

### Tables are tabxplor's

Every table here is built out of `tabxplor::fmt()` columns — scale, `col_var`, `row_kind`, colour, `ref` — and rendered by tabxplor; `benzecri_mrv(fmt = TRUE)` does the same for one vector. `pca_interpret()` and `HCPC_tab()` hand the table back and let the caller print it; **`mca_interpret()` is the only one that also renders**, because its `type = "html"` contract is a finished table — the internal `mca_interpret_tab()` builds it and `tab_html()` draws it.

`mca_interpret()`'s statistics are Le Roux and Rouanet's: only levels contributing above the mean are kept, and the spread between a variable's positive and negative levels is stated in percent of that variable's own variance. That table is **the axes as blocks**: the axis is the row *variable* (so tabxplor writes its heading once per block, in a column it wraps to 13 characters, and draws one thick rule per axis) and the question is its level; the heading states the raw eigenvalue percentage **and** Benzecri's modified rate beside it, which is the number that corrects it — `benzecri_mrv()`'s one internal caller; a question's positive and negative levels face each other on one row, and its own contribution is carried in every cell but `display`ed once. **`pct` prints and `ctr` colours**, which is what lets the sign of the coordinate ride the colour without reaching the page: `ctr` is negated on the negative side, so a level colours on the under-represented half of the same ×1/×2/×5/×10 ladder, and the total row's `ctr` is the *mean* contribution — Le Roux's threshold — rather than the sum it displays.

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

`tests/testthat/` is the package's **contract**: it must fail when a user-visible fact changes, must not fail when an internal is redesigned, and must stay fast enough to run on every edit. Seven files: `helper-fixtures.R` (the cached analyses and plot models), `test-mca2-pca2.R` (the ingress normalisers), `test-ggmca-data.R` (the plot model and the argument matrix), `test-tooltips.R` (the crosstabs behind the hover), `test-plots.R` (that every graph builds, and the render-hint seam), `test-tables.R` (the five table functions), plus `test-str-shim.R` and `test-non-ascii.R` from 1a.

**Argument coverage is the point, not function coverage.** Several arguments are inert alone and only act alongside an enabling one — `keep_levels`/`discard_levels` need `sup_vars`, `tooltip_vars`/`tooltip_vars_1lv` need a table to be built at all, `cah` needs `profiles`. A test that omits the enabler passes while exercising nothing; the vacuous paths are pinned deliberately so nobody "simplifies" them back into nothing.

**Fixtures are `tea[1:6]`, never `tea[1:18]`.** Tooltip crosstabs are quadratic in the number of active variables: `active_tables = "active"` costs 1.2 s on six and 10.4 s on eighteen. Six reaches every code path. The models the suite reuses are memoised in `helper-fixtures.R`; the whole suite runs in about 30 s. The one exception is `fx_mca_multi()`, local to `test-tables.R`: `tea[1:6]` is all binary, so `mca_interpret()`'s row packing collapses every question to one line there and its display blanking has nothing to hide — that needs multi-level variables and a third axis.

The suite is **small and serial**: 241 assertions, no `Config/testthat/parallel`, no `setup.R`, no i18n. ⚠ Do not turn parallelism on for it, and do not import tabxplor's worker, orphan and gettext conventions — see `~/github/tabxplor/CLAUDE.md` "## Testing" only if the suite ever grows enough to need them.

**Golden tests use `expect_snapshot()`** (`_snaps/*.md`), and only where the output is genuinely stable and worth the churn: the rendered tooltip text and the three interpretation tables. ⚠ The *rendered html* is never snapshotted — it is 7 kB of inlined stylesheet; `mca_interpret()`'s snapshot is the `tabxplor` table behind it, printed with `n = Inf`, since pillar formats only the rows it shows and a slice without the total row makes `color = "contrib"` warn.

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
- **`benzecri_mrv()` is exported with no internal caller.** Axis labels and `mca_interpret()` still print the raw eigenvalue percentages — which is the very thing a modified rate exists to correct.
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
