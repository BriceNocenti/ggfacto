# Dependency audit

## Why this document exists

ggfacto is installed on university computers over bad networks. The goal for 0.4.0 is a faster install **with both `Imports` and `Suggests` present**, which has one blunt consequence that shapes everything below: *moving a package between `Imports` and `Suggests` saves nothing*. Only outright removals count. Every figure here is therefore a removal figure.

This file is the transversal reference for what each dependency costs and why it is kept or dropped. It is not a changelog: it states the current tree and the decisions taken on it.

## Method

Measured on the maintainer's machine, R 4.6.1, 531 packages installed. A package's cost is its recursive `Depends + Imports + LinkingTo` closure, minus packages with a non-`NA` `Priority` (base and recommended ship with R, so they are free), sized with `du -sb` on the installed library folders.

```r
# In a temp .R file, then Rscript it.
ip <- installed.packages()
db <- ip[, c("Package", "Depends", "Imports", "LinkingTo", "Suggests")]
rec <- function(p) setdiff(unique(c(p, unlist(tools::package_dependencies(
  p, db = db, which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)))), "R")
# then du -sb the find.package() paths, dropping any package with a non-NA Priority
```

Two totals anchor the rest:

- `Imports` tree: **127 packages / 191.5 MB**
- `Imports` and `Suggests` together: **164 packages / 243.7 MB**

## What each dependency actually costs

Figures are **ordered cumulative removals**, each row showing the tree after that drop. Order matters: `kableExtra` imports `stringr`, so measuring `stringr` first would understate it. A marginal-over-baseline metric would badly overstate `kableExtra` and `widgetframe`, whose tails arrive through `ggiraph` and `FactoMineR` anyway.

| after dropping | packages |    MB | this drop saves     |
|----------------|---------:|------:|---------------------|
| *(start)*      |      164 | 243.7 | ---                 |
| finalfit       |      137 | 208.0 | -27 pkgs / -35.7 MB |
| gridExtra      |      136 | 207.3 | -1 / -0.7           |
| kableExtra     |      132 | 201.9 | -4 / -5.4           |
| ggforce        |      129 | 198.4 | -3 / -3.5           |
| widgetframe    |      128 | 198.2 | -1 / -0.2           |
| stringi        |      128 | 198.2 | -0 / -0.0           |
| stringr        |      128 | 198.2 | -0 / -0.0           |
| magrittr       |      128 | 198.2 | -0 / -0.0           |
| withr          |      128 | 198.2 | -0 / -0.0           |

For reference, outside the plan: dropping `plotly` would save 5 packages / 10.9 MB, `FactoMineR` 39 packages / 46.8 MB.

---

## Tier 1 --- free removals, no code to write

### finalfit --- drop

Zero call sites. Its only consumer was `pers_or_plot()`, already deleted from `R/geometrical_data_analysis.R` (it lived at line 6459 in `ba401e5`). Regressions belong to tabxplor now.

**-27 packages / -35.7 MB, the largest single win available.** The saving is real rather than shared: its tail (`lme4`, `mice`, `glmnet`, `GGally`, `readr`, plus the `RcppEigen` `LinkingTo` source build) is disjoint from every other branch of ggfacto's tree.

⚠ `NAMESPACE:24` still carries `export(pers_or_plot)` and `man/pers_or_plot.Rd` still exists. **The package does not load in this state.** Removing both is a prerequisite, not an optional tidy-up.

### gridExtra --- drop

Zero call sites. Its one use was `pers_or_plot()`'s `grid.arrange()`. Remove from `Imports`. -1 package / -0.7 MB.

### widgetframe --- keep

Three guarded sites (`:6334`, `:6335`, `:6354`). Removal saves 0.2 MB, so there is no size case.

⚠ It is **still on CRAN (0.3.1)**, verified against `available.packages()` --- contrary to a widespread belief that it was archived. The only argument for dropping it is that `frameWidget()`/`frameOptions()` wrap what `htmlwidgets::saveWidget()` already does; that is a code-hygiene call, not a dependency one.

## Tier 2 --- small, self-contained replacements

### ggforce --- replace with one geom_path

One call site, `R/geometrical_data_analysis.R:3718`, drawing the PCA correlation circle:

```r
ggforce::geom_circle(ggplot2::aes(x0 = 0, y0 = 0, r = 1), color = "#d32f2f", linewidth = 1)
```

Replaced by a `ggplot2::geom_path()` over `t <- seq(0, 2 * pi, length.out = 200)`.

**-3 packages / -3.5 MB** (`ggforce`, `tweenr`, `polyclip`), all three compiled, so this also removes three source builds. Best effort-to-gain ratio in the audit.

### kableExtra --- replace with tabxplor's own HTML renderer

Done in 0.4.0. Ten sites, all inside `mca_interpret(type = "html")`. **-4 packages / -5.4 MB** --- measured: `kableExtra`, `rstudioapi`, `svglite`, `textshaping`.

The stronger argument is architectural rather than numeric: tables are tabxplor's job here, and tabxplor 2.0.0 already renders HTML itself: `tab_html()` (and its alias `tab_kable()`) emit raw HTML and return a `c("tabxplor_kable", "knitr_kable")` character object, and `tabxplor/R/tab-render-html.R:554` records that it deliberately uses none of kableExtra's themes. (`kable_tabxplor_style()` is **defunct** in 2.0.0 --- it calls `lifecycle::deprecate_stop()` and always errors.)

**What `tab_html()` actually requires**, since an earlier reading of this got it wrong: `tab_render_vars()` (`tabxplor/R/tab.R:2212`) asks for one `tabxplor_fmt` column **and** one factor column, and degrades with a notice short of that. **Character columns sitting between `fmt` columns are carried through and styled normally** --- the level-name columns of `mca_interpret()`'s table are exactly that. What must be declared is the row index: `tabxplor::new_lvl()` stamps a column `"var"` / `"level"` / `"tab_var"`, and without a `"level"` column the row model is not found. Borders, bold and the block rules are then derived from tabxplor's own semantics (`col_var`, `row_kind`, `ref`) rather than from a `row_spec()`/`column_spec()` equivalent, which does not exist. `pca_interpret()` is the in-file template.

⚠ Two traps found by rendering. A `tab_var` column whose **name contains a space** silently loses its `rowspan` and repeats down every row (an empty name errors outright in `tab_label_runs()`) --- a tabxplor bug, worked around here by naming the column `Axe`. And `render_extras = list(n = "no")` is needed on a hand-built table, or tabxplor materialises a synthetic empty base-count column at render time.

## Tier 3 --- base-R migrations

**These buy 0 MB. They are done for consistency with tabxplor and to shed declared dependencies, not for install speed.** The accounting, so the payoff is never misread:

- `tidyr` imports `stringr`, which imports `stringi`.
- `magrittr` arrives via dplyr, forcats, purrr, tibble and tidyr.
- `withr` arrives via ggplot2, tidyselect and ggrepel.

All three stay installed whatever ggfacto declares --- even if FactoMineR were dropped too. tabxplor has already made this migration: zero `stringr::`/`stringi::` calls in its `R/`, and one remaining `%>%`.

### stringi --- replace with literals

Four live sites, every one an unescape of a literal: `R/utils.R:136` (`"\u202f"`, the `unbrk` top-level constant), `:857` and `:881` (`"\u00b0"`), `:1039` (`"\u202f"` inside a `str_replace()` replacement). Each becomes the plain escape literal --- which is *already* the ASCII source form the project mandates. tabxplor does exactly this at `tabxplor/R/utils.R:713`, `unbrk <- "\u202f"`.

⚠ This also fixes a real defect: `R/utils.R:136` calls a `Suggests` package **unguarded at namespace build time**.

### stringr --- replace with base R

188 live sites, 12 functions. The mapping, with the traps that cause silent breakage:

| stringr                           |  n | base R                         | trap                                                              |
|-----------------------------------|---:|--------------------------------|-------------------------------------------------------------------|
| `str_c`                           | 86 | `paste0`                       | `str_c` returns `NA` if any input is `NA`; `paste0` yields `"NA"` |
| `str_detect`                      | 21 | `grepl`                        | argument order reverses                                           |
| `str_replace` / `str_replace_all` | 23 | `sub` / `gsub`                 | pattern comes first                                               |
| `str_remove` / `str_remove_all`   | 27 | `sub` / `gsub` with `""`       | ---                                                               |
| `str_pad`                         | 10 | `formatC(width =, flag =)`     | negative width pads on the right                                  |
| `str_length`                      |  7 | `nchar(type = "chars")`        | ---                                                               |
| `str_extract`                     |  6 | `regmatches(x, regexpr(p, x))` | drops non-matches instead of returning `NA`                       |
| `str_sub`                         |  4 | `substr`                       | negative indices behave differently                               |
| `str_squish`                      |  2 | `trimws(gsub("\\s+", " ", x))` | ---                                                               |
| `str_to_upper`                    |  2 | `toupper`                      | locale-sensitive                                                  |

⚠ The one that will actually bite: stringr uses ICU regex, base R defaults to TRE. `cleannames_condition()` at `R/utils.R:44` contains a lookbehind, `(?<![[:lower:]])`, which base R accepts only with **`perl = TRUE`**. Every base call built from that pattern needs the flag.

⚠ `str_extract` wants a small internal helper rather than a bare `regmatches()`, so that a non-match yields `NA` instead of shortening the vector.

### magrittr --- migrate to the native pipe

364 live `%>%` plus four `magrittr::` calls. The file already uses `|>` 354 times, so this finishes a migration in progress.

Measured, the risk is much smaller than a raw count suggests --- **three sites need hand-editing**:

- `:5535` --- `res.ca$call$Xtot %>% t %>% ...`, a bare non-call right-hand side; `|>` requires `t()`.
- `R/utils.R:79` and `:82` --- `magrittr::set_names(purrr::map(., ...))`, the only genuine `.` placeholders. Bind the levels to a name first, then call `purrr::set_names(lvl, purrr::map(lvl, ...))`.

The other ~40 lines that match a lone `.` are `~ .` purrr/rlang lambdas, which the native pipe does not touch. The remaining `magrittr::` calls are trivial: `set_names` twice becomes `purrr::set_names` (already used 43 times in the same file), and `set_colnames`/`set_rownames` at `:4657-4658` become one-line assignments.

Finish by removing `export("%>%")` and `importFrom(magrittr, "%>%")`, deleting `man/pipe.Rd`, and dropping the then-dead `. = NULL` binding at `R/utils.R:23`.

⚠ Un-exporting `%>%` is **user-facing breakage** for anyone relying on `library(ggfacto)` to supply the pipe. The soft path is to keep the re-export one cycle with a `NEWS.md` deprecation note while the internal uses go.

## Tier 4 --- correctness fixes worth zero bytes

- **`stats` and `grDevices` sit in `Suggests` but are used unguarded** (12 and 10 live sites). They are base R packages: move them to `Imports`. Free.
- **`scales` sits in `Suggests` but is used unguarded** --- `:2573`, `:2657`, `:4024`. It is a hard ggplot2 dependency, so promoting it to `Imports` costs nothing.
- **`grid` is undeclared.** `:3739` is the only `grid::` call; every other arrow in the file uses `ggplot2::arrow`, a re-export of the same function. Changing it removes the undeclared dependency outright.
- **`ggfacto::theme_facto` at `:2292`** is self-qualified; the other five call sites are bare.
- **Stale minimums.** `DESCRIPTION` asks for `ggplot2 (>= 3.0.0)`, but the code uses `linewidth` at six sites, which is ggplot2 3.4.0. Raising a minimum costs no bytes and converts a confusing runtime failure into a clear install-time one.

## Tier 5 --- what looks prunable and is not

Recorded so the question is not reopened.

- **data.table** --- 0 MB, tabxplor imports it. The whole surface is one internal function, `complete_cah()` at `:6893-6935`. The only change worth considering is narrowing `import(data.table)` to targeted `importFrom` to cut masking risk, and even that is awkward: the `:=`/`.N` idiom wants the full namespace.
- **withr** --- 0 MB. Two live sites, both `with_options(list(tabxplor.output_kable = FALSE), ...)` at `:6637` and `:6663`. That option is still live in tabxplor 2.0.0 (`tab-options.R:314`, `tab.R:380`), so the calls are current. Keep.
- **vctrs, ggrepel, htmlwidgets, scales** --- 0 MB each, all reachable through ggplot2, FactoMineR or ggiraph.
- **plotly** --- 5 packages / 10.9 MB, but 33 live sites implementing the 3D graphs the Description advertises. Keep; already guarded by `requireNamespace()`.
- **ggiraph** --- the expensive `Import` that cannot go: it *is* the interactive-graph feature. Note for the record that roughly 41 MB of its tail is `htmlwidgets` pulling `knitr` and `rmarkdown` through `Imports` rather than `Suggests`. That is upstream's decision and outside ggfacto's control.
- **FactoMineR** --- 39 packages / 46.8 MB, from its own `DT`, `car`/`emmeans`, `ggtext` and `showtext` imports, none of which ggfacto touches. Left alone by decision: it is in the package title and Description. The number still matters, because it means **about two thirds of what remains after Tiers 1 and 2 is FactoMineR's unused statistical tail**, and no ggfacto-side work will move it.

---

## Recommended end state

Applied in 0.4.0:

- `Imports` --- dropped `gridExtra`, `ggforce`, `stringr`; added `stats`, `grDevices`, `scales`;
  `ggplot2` raised to `>= 3.4.0` and `R` to `>= 4.1.0` (the package already used `|>`, which 4.0
  does not have).
- `Suggests` --- dropped `finalfit`, `stringi`, `kableExtra`; added `testthat`. `plotly`,
  `htmlwidgets` and `widgetframe` stay for good.
- `magrittr` stays one deprecation cycle: every internal use is now `|>`, but `%>%` is still
  re-exported for users. It costs 0 MB, so there is no hurry.

Measured after the change: **164 packages / 243.7 MB to 129 / 198.3 MB** --- 35 fewer packages,
45.4 MB less, about 19 %. Retiring the `%>%` re-export would remove the last declared dependency
that no code uses.

The honest ceiling: with FactoMineR and ggiraph both untouchable, ~198 MB is ggfacto's floor, and
~47 MB of that is FactoMineR's unused statistical tail.
