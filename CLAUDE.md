# ggfacto — AI Assistant Guide

## What ggfacto is, and why

`ggfacto` is a public CRAN R package (v0.3.2). It builds readable, complete and pretty graphs for correspondence analysis made with 'FactoMineR'. They can be rendered as interactive 'HTML' plots, showing useful informations at mouse hover. The interest is not mainly visual but statistical: it helps the reader to keep in mind the data contained in the cross-table or Burt table while reading the correspondence analysis, thus preventing over-interpretation. Most graphs are made with 'ggplot2', which means that you can use the + syntax to  manually add as many graphical pieces you want, or change theme elements. 3D  graphs are made with 'plotly'.

The target users are : 1. a "literary" social sciences student, not good at math, learning to read geometrical data analysis ; 2. a serious quantitative analyst — survey researcher, sociologist — often working with weighted survey data.

For anything related to crosstables, it relies heavily on `~/github/tabxplor/`.

---

## Repository Map

To be written. See `~/github/tabxplor/CLAUDE.md` "## Repository Map" for an example.

## ggfacto architecture

To be written. See `~/github/tabxplor/CLAUDE.md` "## tabxplor architecture" for an example.

### Documentation ecosystem

The docs form one hierarchy, general to specific. **Each fact is stated at exactly one layer, referenced (never duplicated) across the others, and always written present-tense** — the current design is the reference point, never how it got there. The one place dev history is allowed is the roadmap "DONE" summaries. In R scripts, **the comments/code ratio should stay under 0.2**.

- **`## ggfacto architecture`** (this file) — the cross-subsystem big picture: goals, data-flow, the declarative pattern, the type system, each subsystem's role and its meaningful "why". Rewritten only when the maintainer asks, by targeted cuts and replacements rather than accretion.
- **`## Repository Map`** (this file) — the file index: one role line per R file. *Cut, don’t accrete.*
- **R file-header comments** — per-file subsystem design: current architecture, key constraints, a pointer up to this file.
- **Inline `# DESIGN:` / `# WARNING:` tags** — the non-obvious "why" at the exact line, caveats to avoid, etc.
- **Vignettes** (`vignette("ggfacto")`) — usage and teaching, for users.
- **Roxygen man pages** (`?ggmca`, `?ggca`) — user-facing reference: *usage* and the main use cases, never build/internals/history. A `@param` states what the argument is, its values, and at most one sentence of when to change it; the rest is a link to the vignette that owns it. ⚠ The manual is LaTeX, so an Rd file is ASCII but for the few glyphs it can set (`— … × ÷`); copy `test-non-ascii.R` from `~/github/tabxplor` here to lock it.
- **`dev/*.md`** (`.Rbuildignore`'d) — transversal or expert technical guides only. Each holds what an `R/` header is too short to derive — a foreign system, a cross-file policy, a statistical derivation — and the header that needs it points at it by section.
- **Roadmap "DONE" summaries → `dev/ggfacto_roadmap_DONE_PHASES.md`** — the ONLY place dev history lives. *Only the maintainer* moves the DONE summaries there.

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

`tests/testthat/` are to be implemented in the future (currently, only examples work as tests).

It will be the package's **contract**: it must fail when a user-visible fact changes, must not fail when an internal is redesigned, and must stay fast enough to run on every edit.

```bash
#In a temp .R file (outside tests/), then: OMP_NUM_THREADS=1 Rscript that_file.R
#   Sys.setenv(TESTTHAT_CPUS = "6", NOT_CRAN = "true"); devtools::test("~/github/ggfacto")
#   devtools::test("~/github/ggfactor", filter = "<name>")   # one/few files while iterating
```

### Threads and workers

Copies from `~/github/tabxplor/CLAUDE.md`: to adapt to ggfacto when needed.

✅ The suite **self-pins**: `tests/testthat/setup.R` pins data.table and BLAS/OpenMP per worker, and `tests/testthat.R` sets `OMP_NUM_THREADS=1` before they spawn. Keep the `OMP_NUM_THREADS=1` prefix anyway (grandchild processes, RhpcBLASctl-less setups).

⚠ **The trap this guards against**: `Config/testthat/parallel: true` runs each file in its own PROCESS, and each then multi-threads on its own — measured, 8 workers × (data.table ~6 + OpenBLAS ~10) = 165 threads on 12 logical cores, and a ~1 min suite ran >26 min.

⚠ **`detectCores()` counts SMT siblings.** This CPU reports 12 and has **6** real cores (`/sys/devices/system/cpu/cpu*/topology/thread_siblings_list` shows two siblings each); `parallelly::availableCores(logical = FALSE)` cannot tell either under WSL2. 8 workers beat 6 by ~6 % while oversubscribing a shared machine, so `tests/testthat.R` sizes the pool from the kernel topology. ⚠ **`devtools::test()` does not read `tests/testthat.R`** — set `TESTTHAT_CPUS` yourself there, which is what the recipe above does.

⚠ **`setDTthreads()` must never be called in a per-file loop**: it tears down and rebuilds data.table's OpenMP pool, which inflated a per-file timing harness ~2×.

**Never run anything else while the suite runs.** Before blaming the code for a slow run, check whether YOU are the cause: another R of yours running, then `ps -eLo pid,args | grep -c "[-]-no-readline --slave"` (thread count ≫ cores?), then orphans.

### Locale, sandbox, orphans

⚠ **A green local suite does NOT mean a green CI — this box is `fr_FR.UTF-8`.** GNU gettext ignores `LANGUAGE` when `LC_MESSAGES` is `C`/`POSIX`, which is the state under `R CMD check` on Linux and on the CRAN farm. So every French assertion passes here and fails there. French output is guarded by `skip_if_no_gettext()` (`tests/testthat/helper-i18n.R`), and each i18n feature is tested twice — an UNGUARDED English block plus a GUARDED French one. **Never simulate CI unless the maintainer asks**: `LC_ALL=C.UTF-8 LANGUAGE=en OMP_NUM_THREADS=1 Rscript <runner>.R` (use `C.UTF-8`, not `C`, which is harsher than any real runner).

⚠ **Two steps need `dangerouslyDisableSandbox`** — bwrap runs `--unshare-net` and `--ro-bind`s `NAMESPACE`/`man/`: `dev/tests/testthat/test-tab-parallel.R` (mirai's dispatcher needs sockets) and `devtools::document()`.

⛔ **NEVER kill a test run by killing its parent — you orphan the workers, and they do NOT stop.** Measured: two killed suites left 6 R processes alive for 52 minutes at ~860 % CPU, silently starving every later run.

- **Diagnose AND kill unsandboxed** — bwrap runs `--unshare-pid`, so each Bash call gets its own PID namespace: `ps aux` cannot see the orphans, and a sandboxed `kill <host-pid>` would hit the wrong process. Identify yours by the parent's `--file=/tmp/claude-…/<session-id>/scratchpad/…`, never by name alone (Positron runs its own R).
- **Never `pkill -f <pattern>`** — measured, `pkill -f testthat` killed the calling shell, and `pkill -f t9.R` is what orphaned the workers. Read `ps` first, then `kill` explicit PIDs.
- **Never pipe a long run through `tail`/`head`** — they buffer until EOF, so the log looks empty and the run looks hung. Write to a file and read that.
- ⚠ Killing PIDs needs the maintainer: surface the `ps` evidence and hand over the exact `kill -9 <pids>`.

## ggfacto v 0.4.0 roadmap and "DONE" summaries

### Phase 1 — clean start

#### Phase 1a — clean dependency tree

Installing ggfacto with its `Suggests` went from **164 packages / 243.7 MB to 133 / 203.7 MB** (-31 packages, -40.0 MB, four fewer compiled from source), `R CMD check` clean at 0/0/0. `finalfit` and `gridExtra` had no call sites left once `pers_or_plot()` went, and were the bulk of it at 27 packages. `ggforce` drew exactly one thing, the PCA correlation circle, now a `geom_path()` — which also fixed a latent waste: `geom_circle()` evaluated its aes once per row of the plot data, so that circle was drawn eleven times over. `stringr` and `stringi` went behind a vendored base-R shim in `R/utils.R`. Undeclared `grid` became `ggplot2::arrow` (a re-export of the same function); `scales`, `stats` and `grDevices` moved from `Suggests` to `Imports`, where their unguarded use always belonged; `ggplot2` rose to `>= 3.4.0`, and **R to `>= 4.1.0` — the declared floor was 4.0.1, but the package already used `|>`, so it could never have installed there.**

A shim rather than a hand conversion because ~20 of the 188 stringr sites diverge in base R, measured not assumed: `cleannames_condition()`'s lookaround makes `gsub()` *error* without `perl = TRUE`; four sites use `str_c()`'s `NA` as a guard that `paste0()`'s `"NA"` would defeat; `regmatches()` returns a vector shorter than its input on a non-match; `formatC()` errors on a vector `width` and cannot pad with anything but space or zero. The shim keeps stringr's semantics exactly, which turned all 188 sites into a namespace strip, and `tests/testthat/test-str-shim.R` pins it with 42 assertions. Padding has its own block: it aligns tooltip numbers under a monospace font.

⚠ **The `%>%` to `|>` migration was the trap, not the strings.** It left **25** magrittr `.` placeholders in `purrr::keep`/`discard`/`set_names`/`map_if`, not the three a pre-flight regex predicted, and a second parse-tree scan missed them too (sibling `~` lambdas cleared the ancestor check). They fail *quietly*: the package-level `. = NULL` binding turns a stranded placeholder into a wrong answer instead of an error. `R CMD check`'s examples caught the first, a full unfiltered parse-level scan the rest; they are now `(\(x) ...)()` mid-chain and plain lambdas as predicates. `magrittr` itself stays one deprecation cycle — every internal use is `|>`, but `%>%` is still re-exported, and it costs 0 MB.

Verification was a golden snapshot against a pristine `HEAD` extracted with `git archive`: 26 entries over `tea`, `mtcars` and `gss_cat`, deliberately including the `sup_vars` / `tooltip_vars` / `keep_levels` / `discard_levels` / `active_tables` / `cah` argument paths, since the plain calls reach none of the placeholders. **25 of 26 came back byte-identical**; the one diff, `pca_cor_circle`, is the intended ggforce change and was checked geometrically — both trace the unit circle through the same 361 unique points at radius exactly 1.0. `test-non-ascii.R`, copied from tabxplor, earned itself immediately by catching a literal U+202F typed into a test. Two pre-existing deprecations were left alone as out of scope: the `size` aesthetic for lines, and `select(cah)` on an external vector.

#### Phase 1b — a real test suite

`tests/` exists as of 1a but only guards the string helpers (`test-str-shim.R`, 42 parity assertions) and the ASCII/Rd rules (`test-non-ascii.R`, copied from tabxplor). Phase 1b turns that into the package's contract: it must fail when a user-visible fact changes, must not fail when an internal is redesigned, and must stay fast enough to run on every edit.

Cover the exported entry points — `MCA2`/`PCA2`, `ggmca_data` tooltips, `ggmca`, `ggca`, `HCPC_tab`, `mca_interpret`/`pca_interpret`, `mean_sd_tab`, `benzecri_mrv` — on the fixtures the roxygen examples already use (`tea`, `mtcars`, `gss_cat`). Golden tests only where the output is genuinely stable and the value beats the churn: tooltip strings and the interpret tables qualify, ggplot internals, and most internals in general, largely do not.

⚠ **Argument coverage is the point, not function coverage.** 1a's migration left 25 magrittr `.` placeholders that a plain `ggmca_data(res.mca)` call cannot reach: every one lives on a `sup_vars` / `tooltip_vars` / `tooltip_vars_1lv` / `keep_levels` / `discard_levels` / `active_tables` / `cah` path, and they failed quietly rather than loudly. Port that argument matrix into `tests/testthat/`.

Three calls in 1a's snapshot harness fail on argument shape rather than on behaviour — `ggmca_data(cah = )`, `HCPC_tab(clust = )` and `ggmca_with_base_ref()`. Resolve them while writing the tests: either the call is wrong or the roxygen is, and either way a user hits it first.

Follow the locale, threads and orphan conventions in the Testing section above. The suite is small and runs serially; do not turn on `Config/testthat/parallel` for it.

#### Phase 1c — drop kableExtra, render html tables with tabxplor

`mca_interpret(type = "html")` (`R/geometrical_data_analysis.R:3431-3451`) is the package's only kableExtra consumer, and the last hard rule still broken: tables are tabxplor's job. Removing it takes the tree from 133 packages / 203.7 MB to **129 / 198.3 MB**.

⚠ **This is a rewrite of the html branch, not a rewiring.** `tabxplor::tab_html()` accepts a plain tibble but *silently degrades to an unstyled table*: `tab_render_vars()` requires `tabxplor_fmt` columns plus a factor row-variable, and there is no public `row_spec()`/`column_spec()` equivalent — borders, bold and block rules are all derived from tabxplor's own semantics. What `mca_interpret` currently hands over is a grouped tibble of eight pre-rendered character columns, which fails both tests and would render bare.

`pca_interpret()` (`:5157`) is the in-file template: it already builds `tabxplor::fmt()` columns with `scale`, `col_var`, `row_kind`, `color` and `ref`. Do the same here — keep the contribution and spread columns as `fmt`, mark each axis's "All levels" row `row_kind = "total"`, use `col_var` to separate the positive and negative blocks so the side borders land, and make `Axe`/`Question` real factors so the row variable and the block boundaries are found. Then delete the manual `new_group` / `last_row` / `totrows` / `questions` index arithmetic at `:3393-3400` outright: tabxplor derives all four itself, and that arithmetic exists only to feed kableExtra.

Anything the class vocabulary cannot express — the two-line `Axe 1: 18.4%` / `of variance` label cell, the thin rule above each question — is a few user CSS rules appended after `tab_css()`, which is explicitly supported. The `type = "console"` path shares the computation and must not change. ⚠ Do not reach for `kable_tabxplor_style()`: it is defunct in tabxplor 2.0.0 and always errors.

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
