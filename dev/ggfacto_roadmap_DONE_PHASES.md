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