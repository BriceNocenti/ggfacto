# The analysis engine: FactoMineR, upstream, or our own

This is the transversal reference for what computes ggfacto's analyses: what FactoMineR costs the package, how an MCA can be computed on answer profiles instead of individuals, how parity with FactoMineR is kept, how to do it as a good citizen of the geometrical data analysis community, and the ruling. Every figure comes from one section of `dev/analysis_engine.R` (Appendix B), on R 4.6.1, OpenBLAS 0.3.32 on one thread, FactoMineR 2.16, GDAtools 2.3, factoextra 2.1.0 and explor 0.3.11. The large-scale figures use the Enquête Emploi 2009--2018 (INSEE, 3.9 million rows) read locally, never copied.

**The ruling, in one paragraph** (section 11): FactoMineR stays the engine, and ggfacto does not vendor it. The MCA goes profile-centric, fed the distinct answer profiles, which is exact and 9 times faster at a million rows. At the largest scales, FactoMineR's own exported `svd.triplet()` decomposes ggfacto's Burt table (section 5.1). The dependency weight is asked of upstream first. The native engine stays in `dev/` as a measured prototype, not a plan.

---

## 1. What ggfacto needs from an engine

ggfacto calls exactly three FactoMineR functions: `MCA()` (`R/mca-data.R`), `PCA()` (`R/pca.R`) and `CA()` (`R/ca.R`), always with `ncp = Inf` and `graph = FALSE`. `HCPC()` is already rebuilt in `R/clust.R`. Everything else is reading slots of the fitted object, about 90 reads across 11 files:

- **MCA** --- `eig`; `var$coord`, `var$contrib` and `var$cos2`; `ind$coord`; `svd$vs`; and in `call`: `X`, `quali`, `Xtot`, `excl`, `marge.col`, `row.w`, `row.w.init`, `ncp` and `ind.sup`.
- **CA** --- `eig`; `row` and `col`, each with `coord`, `contrib` and `cos2`; `row.sup$coord` and `col.sup$coord`; and in `call`: `X`, `Xtot`, `marge.row` and `marge.col`.
- **PCA** --- `eig`; `var` and `ind`, each with `coord`, `contrib` and `cos2`; `svd$V` and `svd$vs`; and in `call`: `X`, `centre`, `ecart.type`, `col.w`, `row.w`, `quali.sup` and `quanti.sup`.

It never reads `var$eta2`, `var$v.test`, the fitted supplementary elements, `svd$U`, or an MCA's individual contributions and cos2. Supplementary variables are projected at plot time, as barycentres over the answer profiles.

Two FactoMineR conventions leak into the code:

- **Axis names differ between analyses.** They are `"Dim k"` in an MCA or a CA and `"Dim.k"` in a PCA: 25 and 36 sites.
- **Levels are renamed.** A level shared by two variables becomes `var_lv`, and a level named `y`, `n`, `Y` or `N` becomes `var.y`.

`mca_levels()` (`R/ingress.R`) is the one reader of an MCA's levels, by position, so both renames are harmless. ⚠ **There is no single reader of the rest of the fitted object.** The active variables are derived again in `R/mca-data.R`, `R/mca-teach.R` and `active_names()`, with 40-odd reads of `call$X` and `call$quali`.

---

## 2. What FactoMineR costs

### 2.1 Dependencies

The method is `dev/dependency-audit.md`'s: recursive `Depends + Imports + LinkingTo`, with recommended packages counted as free. ggfacto's tree with `Imports` and `Suggests` is **142 packages / 218.6 MB**. Without FactoMineR it is 93 / 151.3. The scenarios below edit FactoMineR's own `Imports`; nothing is installed.

| FactoMineR's Imports                            | exclusive packages |   MB | compiled |
|-------------------------------------------------|-------------------:|-----:|---------:|
| 2.16 (installed)                                |                 49 | 67.3 |       25 |
| 2.17, on GitHub (drops `ggtext`)                |                 41 | 63.6 |       20 |
| 2.17 with `car` in Suggests                     |                 14 | 21.2 |        7 |
| ... and `emmeans`, `multcompView` in Suggests   |                 10 | 15.2 |        6 |
| core only (`irlba`, `ggplot2`, `ggrepel`, base) |                  2 |  4.4 |        2 |

`car` alone brings 27 packages / 42.4 MB, through `lme4`, `pbkrtest`, `quantreg` and `forecast`. `RcppEigen`, a `LinkingTo` of `lme4`, is one more source build for Linux users and is not even counted here. FactoMineR uses `car` only in `AovSum()` and `LinearModel()`, `emmeans` and `multcompView` in `meansComp()`, `showtext`, `sysfonts` and `ggtext` in `theme_factominer()`, and `DT` in `plot.catdes()`. ggfacto touches none of them. The audit's former 39 packages / 46.8 MB were measured on the tree of that time, before its own removals; these are the figures to use.

⚠ **Moving FactoMineR to ggfacto's `Suggests` would save nothing** for `install.packages(dependencies = TRUE)`, which is what the course prescribes (`formations_stat/cours/M2S1/livre/index.qmd:96`). Only an outright removal, or a lighter FactoMineR, counts.

jamovi 2.7.27 already bundles `car`, `lme4`, `pbkrtest`, `quantreg` and `emmeans` in its base library, so a jamovi module would gain almost nothing from either.

### 2.2 Load time

`library(ggfacto)` never loads FactoMineR, since every call is `::`. The **first analysis of a session** does. It costs **1.01 s and 28 namespaces**: the median of 10 cold runs, measured after the 20 namespaces a session loads anyway. The same analysis then takes 0.13 s. By import:

| import       | alone after the same baseline | namespaces |
|--------------|------------------------------:|-----------:|
| `emmeans`    |                        0.62 s |         14 |
| `irlba`      |                        0.53 s | 3 (`Matrix`) |
| `showtext`   |                        0.38 s |          3 |
| `sysfonts`   |                        0.35 s |          1 |
| the 14 others |                     < 0.02 s each |    1--4 |

`car` costs nothing at load: FactoMineR calls it by `::`, so an MCA never loads it. The imports overlap, so their times do not add up: with `car`, `emmeans` and `multcompView` gone, FactoMineR's imports still take 0.96 s; with its core only, 0.57 s. ⚠ **Even a core-only FactoMineR keeps about 0.5 s**, through `irlba` → `Matrix`.

### 2.3 Churn

FactoMineR changes what ggfacto receives:

- **2.15** truncates `eig` to `ncp` rows (unless `excl` or Burt), switches to `irlba` when `ncp < min(n, K) / 2`, and changes `HCPC()`'s suggested cut.
- **2.16** makes `theme_factominer()` the plot default and adds `svd$sumvp`; the current release requires R ≥ 4.3.
- **2.17** drops `ggtext`.

The wrappers' `ncp = Inf` shields ggfacto's own fits. A raw `FactoMineR::MCA()` fit a user passes in still carries 5 eigenvalues and seed-dependent coordinates (B7). ggfacto declares R ≥ 4.3, FactoMineR 2.16's own floor.

### 2.4 Speed and memory

See section 5. At the course's size the engine is irrelevant: in the course's MCA session on pc_AGD (`section_session`), the fit takes 0.13 s, the other steps 5.3 s, and FactoMineR's first load 1.0 s:

| step                                    |    time |
|-----------------------------------------|--------:|
| `ggmca(active_tables = "active") \|> ggi()` |  1.6 s |
| `ggmca(clust =) \|> ggi()`                 | 1.4--2.0 s |
| `clust_tab()`                           |   0.9 s |
| `mca_interpret()`                       |  0.27 s |
| `hierarchical_clust()`                  | 0.11 s, 0.46 s the first time |

---

## 3. How FactoMineR computes

An MCA is the CA of the indicator table Z (n × K levels, Q questions) with row weights w, summing to W:

- `r_i = w_i / W`
- `c_j = W_j / (Q W)`, where `W_j` is the weighted count of level j
- `Tc_ij = z_ij / (Q c_j) − 1`

`svd.triplet()` decomposes `D_r^½ Tc D_c*^½`, where `c* = c` except that an **excluded level gets a mass of 1e-15** during the SVD, then 0. Its contribution is therefore 0, and the percentages of variance are taken over `Σ_{j∉excl} (1/Q − c_j)`. With `excl`, FactoMineR returns K − Q axes, some of them ghost axes of order 1e-15, and `eig` becomes a five-column data frame whose modified rates are **rounded to 2 decimals** (`modif.rate()`).

What a native engine must reproduce, and what it must not:

- **The sign rule.** Each axis is signed so that its unit column vector sums positive. ⚠ The rule is skipped at `ncp == 1` when the table has more rows than columns (B5). And when that sum is zero in exact arithmetic, as with a question that mirrors another, rounding decides the sign: the synthetic case P6 flips one axis between individuals and profiles.
- **`v.test = coord · √(W_j (W − 1) / (W − W_j))`** uses the raw sum of the weights, so survey weights inflate it (B6). ggfacto never reads it.
- **Closed forms, where nothing is n × K:**
  - `dist²_j = 1/(Q c_j) − 1`
  - `contrib_j = 100 c_j g_j² / λ`
  - `cos²_j = g_j² / dist²_j`
  - `η²_q = Q λ Σ_{j∈q} contrib_j / 100`
  - `dist²_i = Σ_{j∉excl} c_j − 2 m_i / Q + Σ_{j∈i, ∉excl} 1/(Q² c_j)`, where `m_i` counts i's kept levels
- **The Burt route.** It builds `B = Z′WZ` (K × K) and `S = D_c*^½ (B / (Q² W c c′) − 1) D_c*^½`, which is exactly X′X. `eigen(S)` gives λ and the column side. Each individual's coordinate is the transition formula, `f_i = (1/Q) Σ_{j∈i} (c*_j / c_j) V_j − Σ_j c*_j V_j`; the last term is 0 without `excl`.

The same `Burt` table is what the tooltip crosstabs show (section 4.5).

"Specific MCA" gives the same numbers in FactoMineR (`excl`) and in GDAtools (`speMCA()`), to the six digits compared, up to the axis signs, since GDAtools applies no sign rule. `ca::mjca(subsetcat =)` has no row weights, so it cannot carry survey weights.

---

## 4. The answer profile as the unit

### 4.1 Why it is exact

Individuals with the same answers share one row of Z, hence one row of `Tc`. Merged into one row of weight `r_i + r_i′`, they leave X′X unchanged, and so every eigenvalue and every quantity on the level side: coordinates, contributions, cos2, η² and v.test, since `W` and `W_j` are kept. Each individual keeps its profile's coordinate and cos2, and its contribution is the profile's times `w_i / w_p`. This holds under `excl`.

The only thing a merge can change is FactoMineR's arbitrary sign at `ncp == 1` (B5), which depends on the row count. A weight of 0, on an individual or a profile, crashes FactoMineR either way (B9): ggfacto's ingress leaves such rows out.

### 4.2 Measured

Each case compares `FactoMineR::MCA()` on the individuals, called through ggfacto's own ingress (`na_levels()` then `excl_index()`), with three fits on the profiles, their individual slots expanded back: FactoMineR itself (*FM prof*), and the prototype's two engines. The table gives the largest relative difference over every slot ggfacto reads, plus η², v.test and individual contributions and cos2:

| case                                     |       n |      P |    K | FM prof   | dense     | Burt      |
|------------------------------------------|--------:|-------:|-----:|-----------|-----------|-----------|
| P1 tea[1:18]                             |     300 |    290 |   45 | 5.3e-14   | 5.3e-14   | 4.8e-14   |
| P2 tea, NA levels, `excl = NA`           |     300 |    291 |   47 | 6.0e-14   | 6.0e-14   | 5.3e-14   |
| P3 tea, random weights, 2 levels excl.   |     300 |    290 |   45 | 5.1e-14   | 5.1e-14   | 3.0e-14   |
| P4 pc_AGD, POND                          |   9 234 |  5 312 |   38 | 3.9e-14   | 3.9e-14   | 6.5e-14   |
| P5 pc_AGD, POND, rarest level excl.      |   9 234 |  5 312 |   38 | 1.0e-13   | 1.0e-13   | 1.1e-13   |
| P7 n < K; all-binary; two questions      | 10--300 | 4--35  | 4--35 | ≤ 1.4e-14 | ≤ 1.4e-14 | ≤ 2.4e-14 |
| P13 Enquête Emploi 2018, EXTRI, NA excl. | 100 000 | 23 649 |   66 | 2.3e-13   | 2.3e-13   | 1.1e-11   |

In every case the level names are identical and no sign flips. The exceptions are FactoMineR's own:

- **P6, a question mirroring another:** one sign-degenerate axis, identical up to its sign.
- **P8b, one binary question:** FactoMineR's `ncp == 1` sign (B5); fed its two profiles, FactoMineR fails outright ("argument of length 0").

A level every individual chose has an undefined cos2: rounding noise in FactoMineR, Inf in closed form. The Burt route loses a little precision by squaring the matrix, 1e-11 at worst, far inside any tolerance a test would set (1e-8).

### 4.3 How many profiles real data has

The Enquête Emploi's questions come in nested sets of 5, 8, 10 and 15 (`section_compression`). The table gives P, the number of distinct profiles, with P/n in brackets, among the employed:

| rows                          |         n |    Q5 |               Q8 |                Q10 |                Q15 |
|-------------------------------|----------:|------:|-----------------:|-------------------:|-------------------:|
| 10 000 (2018)                 |    10 000 |   689 |   5 172 (0.52) |     7 668 (0.77) |     9 546 (0.95) |
| 100 000 (2018)                |   100 000 | 1 046 |  16 941 (0.17) |    34 594 (0.35) |    65 773 (0.66) |
| 2018                          |   180 494 | 1 107 |  19 923 (0.11) |    43 244 (0.24) |    90 043 (0.50) |
| 2016--2018                    |   547 748 | 1 224 |  29 919 (0.05) |    76 330 (0.14) |   208 573 (0.38) |
| 2009--2018                    | 1 871 836 | 1 350 |  47 753 (0.03) |   158 913 (0.08) |   608 089 (0.32) |
| 2009--2018, 1 row per person-year | 816 253 | 1 335 | 44 806 (0.05) |  144 849 (0.18) |   533 094 (0.65) |

P grows as n^0.12, n^0.41, n^0.56 and n^0.77 for Q5, Q8, Q10 and Q15. The whole population, missing answers included, compresses about twice as much. pc_AGD, with 15 questions, has P/n = 0.58.

⚠ The rotating panel repeats people across quarters, which inflates compression. The honest line is the last one.

So the profile is a large gain up to about 10 questions, and a modest one at 15, where each profile has one or two individuals. The design must not depend on compression for its correctness, only for its speed.

### 4.4 Keeping individuals and profiles aligned

The fit stores `key`, one integer per fitted row (4n bytes), plus the P × Q profile table and its summed weights and counts. Every later call re-derives the map with `vctrs::vec_match(answers, profiles)`. That is exact where `vec_group_id()`, which numbers groups by first appearance, only gives a partition.

`section_map` times it at 1 million rows:

| questions | profiles | group | match | `rlang::hash()` |
|-----------|---------:|------:|------:|----------------:|
| 10        |  135 523 | 0.07 s | 0.06 s | 0.002 s |
| 15        |  475 373 | 0.10 s | 0.14 s | 0.003 s |

Preparing the factors (`na_levels()`, 0.4--0.6 s) costs more than the map.

It composes with what exists:

- `res$source` and `fit_rows()` keep picking the rows.
- `same_answers()` becomes that one match against the stored profiles, instead of a comparison with `call$X`, which is no longer stored.
- A row that matches no profile is refused, as today.

### 4.5 What reads the profile model

- **Supplementary levels.** A supplementary level is a weighted barycentre, and it follows from its crosstab with the active levels: `g_l = [(W_l· / W_l) A − Σ c* V] / √λ`, with `A = diag(c*/c) V`. On pc_AGD it equals the barycentre to 1e-15, FactoMineR's `quali.sup` to 1e-14, and `varsup()` to 5e-7, since `varsup()` rounds to 6 digits. At 1 million rows it takes 0.17 s per variable, against 0.58 s by gathering individual coordinates. Changing `sup_vars` stays cheap.
- **Tooltips.** The `active_tables = "active"` crosstabs **are** the Burt table, in row percentages, each block against its own Total. Built from the weighted and unweighted Burt tables of the profiles and handed to `tabxplor::fmt()` for colour and formatting only (`section_tooltips`), they reproduce every cell of pc_AGD's 1 482: n, weighted n, pct and diff all differ by 0, with **identical colour codes and identical formatted text**. The cost:

  | data                        | stacked `tab()` | Burt route |
  |-----------------------------|----------------:|-----------:|
  | pc_AGD                      |          0.81 s |     0.08 s |
  | Enquête Emploi, 100 000 rows |          0.92 s |     0.23 s |
  | Enquête Emploi, 1 million rows |        3.78 s |     0.86 s |

  This needs no new engine, and is the largest single saving available to a student (section 2.4).
- **Ellipses.** They are `stat_ellipse(type = "t")`, ggplot2's robust ellipse (`MASS::cov.trob()`), weighted by `row.w` since ggplot2 4.0.0. ⚠ They cannot be drawn from the (profile × level) weights: ggplot2 sizes the radius on `nrow(data) - 1` degrees of freedom (`calculate_ellipse()`), and `cov.trob()` tests its convergence on absolute weights. The plot model keeps one row per individual for them.
- **Clustering.** The Ward tree already works on the distinct points (`R/clust.R`), which are the profiles. Its time is quadratic in P, so at 15 questions over several years (P ≈ 600 000) the tree, not the MCA, is the limit.

### 4.6 What stays n-sized

At 1 million rows, 10 questions (`section_bench`):

- **FactoMineR's object weighs 2.4 GB.** It holds `call$Xtot` (the n × K indicator table as a data frame), `call$X`, three n × (K − Q) `ind` matrices and `svd$U`.
- **A profile model needs about 15 MB:** `key`, the weights, the profiles and five axes of their coordinates.

Individual coordinates are only ever needed on the 2 to 5 axes a graph or a clustering uses, and can be computed on the fly.

---

## 5. The scale benchmark

MCA of the Enquête Emploi's employed, weighted by EXTRI, with missing answers as excluded levels. Each cell is its own cold process: fit time and peak R heap, preparation excluded. The engines:

- **FM ind** --- FactoMineR on individuals, ggfacto today.
- **FM prof** --- FactoMineR fed the profiles.
- **dense** and **Burt** --- the prototype's two engines, with every axis for every profile.
- **lean** --- every eigenvalue, but coordinates for 5 axes only.

⚠ The FM ind figures marked *extrap.* are extrapolated linearly from the two sizes below them, and were deliberately not run: they would exceed the machine.

| n, questions     |       P | FM ind          | FM prof       | dense         | Burt           | lean          |
|------------------|--------:|-----------------|---------------|---------------|----------------|---------------|
| pc_AGD, 15       |   5 312 | 0.09 s, 40 MB   | 0.05 s, 39 MB | 0.02 s, 30 MB | 0.04 s, 38 MB  | 0.02 s, 16 MB |
| 100 000, 10      |  44 844 | 2.7 s, 573 MB   | 0.8 s, 236 MB | 0.2 s, 187 MB | 0.2 s, 307 MB  | 0.06 s, 90 MB |
| 300 000, 10      |  83 948 | 9.6 s, 1.5 GB   | 2.3 s, 539 MB | 0.9 s, 391 MB | 1.1 s, 549 MB  | ---           |
| 1 000 000, 10    | 135 523 | 36 s, 5.5 GB    | 4.0 s, 752 MB | 1.9 s, 575 MB | 2.1 s, 981 MB  | 0.25 s, 205 MB |
| 1 871 836, 10    | 158 913 | *extrap.* 70 s, 10 GB | 4.6 s, 904 MB | 2.3 s, 745 MB | 2.6 s, 1.0 GB | 0.42 s, 328 MB |
| 300 000, 15      | 226 582 | 16 s, 2.8 GB    | ---           | ---           | ---            | ---           |
| 1 871 836, 15    | 608 089 | *extrap.* 106 s, 18 GB | 32 s, 6.1 GB | 17 s, 4.3 GB | 20 s, 10.3 GB | 2.1 s, 806 MB |

The object sizes at 1 million rows, 10 questions, are 2.4 GB for FM ind, 334 MB for FM prof, 247 MB for dense and Burt, and **9.5 MB** for lean. With `ncp = 5`, FactoMineR switches to `irlba`: 1.6 s against 2.7 s at 100 000 rows, but with 5 eigenvalues and seed-dependent coordinates (B7).

Three readings:

1. **The profile is most of the gain, and it needs no engine of our own.** Fed the profiles, FactoMineR is exact (section 4.2), 9 times faster and 7 times lighter at a million rows.
2. **The missing key at scale is decoupling.** ggfacto asks FactoMineR for every axis because the eigenvalue table needs them all, and FactoMineR then computes three n × (K − Q) matrices nobody reads. The Burt route computes every eigenvalue from a K × K matrix and the coordinates of only the axes asked for. That is the only engine that handles 15 questions over 10 years, **2 s and 0.8 GB where FactoMineR fed the profiles needs 32 s and 6 GB**, and FactoMineR on individuals would not run at all.
3. **The Burt table's own cost is small.** With `Matrix`, 0.07 s at 10 questions and 0.6 s at 15; in base R with `rowsum()`, 0.23 s and 2.2 s. `Matrix` is free to install but costs 0.5 s to load.

### 5.1 FactoMineR's own SVD, fed the Burt table

FactoMineR exports `svd.triplet()`, the weighted SVD behind `MCA()`, `CA()` and `PCA()`. Given the K × K matrix of the Burt route, `S = D_c*^½ (B / (Q² W c c′) − 1) D_c*^½`, whose singular values are the MCA's eigenvalues, it gives every eigenvalue and the levels' side with FactoMineR's own numerics and sign rule (`section_fm_burt`). ggfacto adds the Burt table, the transition formula for the profiles' coordinates on the axes asked for, and one correction: `svd.triplet()` normalises its row weights to sum 1, which under `excl` scales the singular values by `1/√Σc*`.

| case                                     | largest relative difference to `MCA()` on individuals |
|------------------------------------------|--------------------------------------------------------|
| tea[1:18]; tea weighted, 2 levels excl.  | 5.9e-14; 4.3e-14                                       |
| pc_AGD, POND; with the NA rule           | 8.7e-14                                                |

On the Enquête Emploi's 1.87 million employed, with coordinates on 5 axes:

| questions | P       | time and peak | stored |
|-----------|--------:|---------------|-------:|
| 10        | 158 913 | 0.3 s, 0.4 GB | 14 MB  |
| 15        | 608 089 | 3.5--4.1 s, 0.7 GB | 32 MB |

This is the way to keep FactoMineR the engine at every scale. What ggfacto writes is the Burt table, the transition formula and the object's slots in their closed forms (section 3), about 60 lines, pinned by the parity above.

### 5.2 data.table

data.table, even on 12 threads, groups profiles no faster than `vctrs` + `rowsum()` (0.10 s against 0.10 s), and its self-join Burt table is the slowest (3.0 s at 15 questions). ⚠ **It brings nothing here:** these building blocks are already C-level, and the remaining costs are formatting (tabxplor) and preparation (`na_levels()`).

---

## 6. The prototype and its parity

`dev/analysis_engine.R` holds two things:

- **The engines** --- the profile table, `svd.triplet()`'s branches and sign rule, the dense and Burt MCA, the closed-form slots, `modif.rate()`, a CA and a PCA. **About 190 lines**, base R plus optionally `Matrix`, against FactoMineR's 1 100 deparsed lines for `MCA()`, `CA()`, `PCA()`, `svd.triplet()` and `tab.disjonctif()`, plus its C code.
- **The parity harness** --- `section_parity`.

Parity:

- **MCA** --- section 4.2.
- **CA** (gss_cat religion × party, a two-column table, a two-row table, a zero row dropped, `row.sup` and `col.sup`) --- within 7e-15.
- **PCA** (mtcars[1:7], the 16 combinations of `scale.unit`, `row.w`, `col.w` and `ind.sup`, a missing value imputed, a constant column) --- within 1.3e-16.

For a CA or a PCA the tables are small: an engine of our own gains no speed there, only the dependency.

What a native engine would still let ggfacto delete: the `"Dim k"` / `"Dim.k"` duality, across 61 sites. The rest — `varsup()`, the derivations of the active side, the reads of `call$X` and `call$Xtot` — went with the profile-centric reader, FactoMineR staying the engine (`R/model.R`).

---

## 7. The object and its methods

The same fitted MCA was run under three class orders through factoextra, explor, GDAtools and `print()` (`section_object`). O3, a tidy object of ggfacto's own, would fail every one of them by construction.

| class                              | factoextra | explor | GDAtools                            |
|------------------------------------|:----------:|:------:|:-----------------------------------:|
| O1 `c("MCA", "list")`              | ok         | ok     | ok                                  |
| O2 `c("ggfacto_mca", "MCA", ...)`  | ok         | ok     | **fails**: it tests `class(x)[1]`   |
| O2′ `c("MCA", "ggfacto_mca", ...)` | ok         | ok     | ok                                  |

factoextra was run through `fviz_mca_var()` and `fviz_mca_ind()`, explor through `prepare_results()`, and GDAtools through `supvar()`, `dimdescr()` and `ggcloud_variables()`.

A FactoMineR-shaped object works in all three **without FactoMineR loaded**, since they dispatch on the class name. But in a library where FactoMineR is absent (`section_nofm`):

- `print()` of the same MCA dumps **11 767 lines** instead of 19.
- The course's `plot(acp, choix = "var")` into `grid.arrange()` (`03-ACP.qmd:533`) and `plot(resultat_ac)` (`04-AC.qmd:297`) both fail.

explor keeps working.

ggfacto reads GDAtools' `speMCA()` and `csMCA()` fits through the same model as its own (`R/model.R`): GDAtools orients some axes the other way and rounds its coordinates and contributions to six decimals, both harmless.

### 7.1 A profile-fitted object, and what can be slimmed

Fed the profiles, FactoMineR returns its usual object with one row per profile: `call$X` is the P profiles (with FactoMineR's renamed levels), `call$Xtot` their P × K indicator table (integers already), `call$row.w` their summed weights, and `ind` and `svd$U` one row per profile. Every slot shrinks by the compression ratio (section 4.3), with nothing to decide.

What the ecosystem reads, by a static scan of every function (`section_slots`):

| package          | `call$Xtot`                    | `call$X`        | `ind$...`        |
|------------------|--------------------------------|-----------------|------------------|
| FactoMineR       | `dimdesc()`, `HCPC()`, `plot.CA()` | 24 functions | 29 functions    |
| explor           | `prepare_results()`            | 4 functions     | 6 functions      |
| GDAtools         | none                           | 18 functions    | 31 functions     |
| factoextra       | none                           | 6 functions     | none             |
| FactoInvestigate | 2 functions                    | 2 functions     | 5 functions      |

And through FactoMineR's methods (`print`, `summary`, `plot`, `dimdesc`), factoextra, explor and GDAtools (`section_slim`):

| object                                         | what breaks                                        |
|------------------------------------------------|----------------------------------------------------|
| fitted on profiles                             | GDAtools `supvar()`, `ggadd_ellipses()` (1)        |
| ... `call$Xtot` or `call$X` dropped            | the same, and explor                               |
| ... `ind` and `svd$U` kept on the first 5 axes | the same as fitted on profiles: nothing more       |

(1) Given a variable with one value per individual.

⚠ **The contract with a profile-fitted object** is that the ecosystem reads profiles as individuals: a function given one value per individual stops on a length mismatch, and needs one value per profile.

**What is worth slimming, and what is not.** `call$X` and `call$Xtot` stay, since explor needs them, and on profiles they are small. The one slim that is simple and breaks nothing more is to keep `ind` and `svd$U` on the first few axes, while `eig`, `var` and `svd$V` keep them all. It is FactoMineR's own shape for `MCA(excl =, ncp = k)`, and ggfacto recomputes any other axis of a profile from `svd$V` by the transition formula.

At a million rows and 10 questions, the object is 2.4 GB on individuals, 334 MB on profiles, and 113 MB with profiles on 5 axes. At 15 questions over ten years it is 619 MB, where `call$Xtot` weighs most. It is worth doing only as far as it stays that simple.

Consequences for the object ggfacto returns:

- **Keep `"MCA"` / `"PCA"` / `"CA"` first in the class.** A ggfacto class second (O2′) gets print, summary and plot methods that act only when FactoMineR's are absent, with no "S3 method overwritten" and nothing broken downstream.
- **Individual coordinates become an accessor used in `mutate()`**, like `hierarchical_clust()`, rather than an n-row slot.

---

## 8. Upstream first

FactoMineR's maintainer answers quickly. Issue #41 (`showtext` shrinking every later plot) was opened on 2026-09-06 and fixed on GitHub the next day, and 2.17 dropped `ggtext` in the same move. Most of section 2's weight is in imports that serve functions ggfacto never calls, so the cheapest route to a light ggfacto runs through FactoMineR itself.

What each outcome gives ggfacto:

| upstream moves to Suggests               | ggfacto's gain, packages / MB / load |
|------------------------------------------|--------------------------------------|
| `car` only                               | 35 / 46 / nothing at load            |
| `car`, `emmeans`, `multcompView`         | 39 / 52 / 0.05 s                     |
| every non-core import                    | 47 / 63 / 0.44 s (`irlba` keeps `Matrix`) |

The drafts in Appendix A carry:

- **the Suggests proposal**, with these measurements;
- **nine bug reports**, B1--B9, each with a minimal reproduction;
- **two proposals that would help every FactoMineR user:**
  - the exact **profile aggregation** of `MCA()` (section 4.1);
  - **every eigenvalue whatever `ncp`**, since an MCA's K × K eigen-decomposition is cheap and 2.15's truncation is what forces `ncp = Inf` onto ggfacto.

---

## 9. Community practice

ggfacto sits in a community with its ways: FactoMineR (Husson, Josse, Lê, Mazet), from Pagès's Rennes school; Le Roux and Rouanet's line through GDAtools (Robette); explor (Barnier); factoextra (Kassambara). The aim is to be read as someone bringing something new and compatible.

**The compatibility contract**, whatever engine computes:

- The object keeps FactoMineR's class first and FactoMineR's slot names, so factoextra, explor, GDAtools, Factoshiny and FactoInvestigate keep working (section 7).
- No FactoMineR method is ever overwritten.
- ggfacto accepts FactoMineR fits and GDAtools' `speMCA()` and `csMCA()`.
- A parity test pins the latest FactoMineR release, from `Suggests`, in CI. A divergence is reported upstream, never silently kept.

**Attribution.** The CRAN Repository Policy:

> Where code is copied (or derived) from the work of others (including from R itself), care must be taken that any copyright/license statements are preserved and authorship is not misrepresented. Preferably, an 'Authors@R' field would be used with 'ctb' roles for the authors of such code. [...] Where copyrights are held by an entity other than the package authors, this should preferably be indicated via 'cph' roles in the 'Authors@R' field.

`Authors@R` credits, as `ctb` and `cph`, **François Husson, Guillaume Le Ray and Quentin Molto**, the authors of `HCPC()`, for the cut rule and the tree plot `R/clust.R` derives from it. No code copied from GDAtools remains (`varsup()` gave way to the barycentre over the answer profiles).

`inst/CITATION` cites ggfacto, then the software and methods it rests on:

- Lê, Josse and Husson (2008), *FactoMineR: An R Package for Multivariate Analysis*, JSS 25(1);
- Le Roux and Rouanet (2004), *Geometric Data Analysis*, and (2010), *Multiple Correspondence Analysis*;
- Benzécri (1979), for the modified rates;
- Husson, Josse and Pagès (2010), for the hierarchical clustering on principal components;
- GDAtools.

ggfacto is GPL ≥ 3, compatible with both.

**Wording.** The Description says "made with 'FactoMineR'", which stays true.

**Talk before code.**

- Open a FactoMineR issue with section 2's measurements and the profile result before any vendoring.
- Tell Robette that ggfacto reads `speMCA()` and `csMCA()` fits.
- Ask Barnier whether explor would read a lighter object.

Where the community meets:

- the CARME conferences (correspondence analysis and related methods);
- the Rencontres R;
- the SFdS Journées de Statistique;
- the GDA sociology network around Le Roux and Lebaron;
- for the software itself, JOSS or the R Journal.

**Vocabulary.** Keep the community's words: specific MCA, modified rates, class-specific MCA. Le Roux and Rouanet's English says *response pattern* where ggfacto says *answer profile*; the English documentation should name both once.

---

## 10. Packaging

- **The R version.** ggfacto declares R ≥ 4.3, FactoMineR 2.16's own floor, so the two cannot disagree.
- **`tea`.** Nine examples, the test fixtures and the pkgdown site planned for phase 1r read `data(tea, package = "FactoMineR")`, which is right while FactoMineR is imported.
- **The course.** Two `plot()` calls and about ten `library(FactoMineR)` lines depend on FactoMineR's methods, and six passages of prose say that FactoMineR computes. They stay true.

---

## 11. Verdict

**Is it possible?** Yes. Parity holds to 1e-13 on every realistic case and is only limited by FactoMineR's own sign instabilities. The engine is about 130 lines for the MCA, plus about 50 for CA and PCA, in base R.

**Is it useful?** Not for the reasons first expected.

| axis                  | dropping FactoMineR gives                  | available without dropping it             |
|-----------------------|--------------------------------------------|-------------------------------------------|
| dependency weight     | 49 packages, 67 MB, 1.0 s at first use     | most of it, if upstream moves (section 8) |
| speed, course size    | nothing: the fit is 0.1 s of a 6 s session | Burt tooltips: -0.7 s per `ggmca()`       |
| scale, 500k--2M rows  | lean engine: 2 s, 0.8 GB at 15 questions   | FactoMineR's SVD on Burt: 3.5 s, 0.7 GB   |
| profile-centric model | native, profiles all the way down          | the same model, FactoMineR as its engine  |
| readability           | one reader, one axis naming, no `Xtot`     | one reader in front of FactoMineR         |
| lone-wolf risk        | real, mitigated only by section 9          | none                                      |

**The ruling.** FactoMineR stays the engine, fed the answer profiles, and ggfacto reads every MCA through one profile-centric reader (`R/model.R`):

- one model per fit: the profiles, the individual-to-profile map, the weights, the source rows;
- FactoMineR's `MCA()` fed the profiles; its `svd.triplet()` fed the Burt table (`mca_fm_burt()`, above) stays here, for a cloud of several hundred thousand profiles;
- every consumer reading that model: the tooltips by the Burt route, the supplementary levels as barycentres over the profiles, the clustering on the profiles;
- FactoMineR's object shape and class kept: the ecosystem reads the profiles as individuals.

The upstream proposals (section 8) and the credits (section 9) come first. The native engine is not pursued: FactoMineR's own SVD reaches the same scale.

---

## 12. Set aside, and open

**Set aside, with their reasons above:**

- `ca` and `ade4` as lighter engines. `mjca()` has no row weights; `dudi.acm()` has no specific MCA and compiles C++ (section 3).
- data.table (section 5).
- RSpectra, irlba and compiled code: K is small, and `eigen()` on K × K is instantaneous.
- A synthetic generator of survey answers: the Enquête Emploi measures compression directly.
- Tooltips counted on profiles through `tab()`: tabxplor's unweighted `n` would count profiles instead of individuals. The Burt route computes both counts instead.

**Open:**

- **The sign convention.** FactoMineR's rule is undefined for a mirrored question and skipped at one axis (B5). Feeding profiles can flip the sign in those two cases only; they are documented as known limits.

---

## Appendix A. Upstream drafts

For the maintainer to file on <https://github.com/husson/FactoMineR/issues>, after a first message introducing ggfacto and this measurement. Each reproduction runs on FactoMineR 2.16 (`section_bugs`).

```r
library(FactoMineR); data(tea)
# B1 level.ventil never returns on an ordered factor (tabl is not recomputed in the while loop).
d <- data.frame(a = factor(c(rep("low", 97), "mid", "high", "high"), c("low", "mid", "high"),
                           ordered = TRUE), b = factor(rep(c("x", "y"), 50)))
MCA(d, level.ventil = 0.05, graph = FALSE)                      # no return
# B2 the NA warning is never shown: MCA() calls warnings() instead of warning().
na <- tea[1:6]; na$breakfast[1:20] <- NA; MCA(na, graph = FALSE) # silent
# B3 na.method = "Average" fails without ind.sup (newRowW[-ind.sup] with ind.sup = NULL).
MCA(na, na.method = "Average", graph = FALSE)                   # invalid argument to unary operator
# B4 svd.triplet()'s eigen() fallback never sets d from bb$values (by inspection).
# B5 at ncp == 1 the sign rule is skipped when nrow > ncol: the same data aggregated flips sign.
MCA(tea["breakfast"], graph = FALSE)$var$coord[1]               # -1.04
MCA(data.frame(breakfast = factor(levels(tea$breakfast))), row.w = c(144, 156),
    graph = FALSE)$var$coord[1]                                 # +1.04
# B6 v.test scales with the survey weights (N = sum(row.w) is not normalised).
MCA(tea[1:6], row.w = rep(1000, 300), graph = FALSE)$var$v.test # 31.7 times the unweighted one
# B7 with irlba (ncp < min(n, K) / 2), coordinates depend on the seed: up to 1.7e-5 apart.
# B8 CA() with a zero-sum row and col.sup: non-conformable arguments (the row leaves X, not Xtot).
X <- rbind(unclass(table(forcats::gss_cat$race, forcats::gss_cat$marital)), zero = 0)
CA(X, col.sup = 2, graph = FALSE)
# B9 one zero row weight crashes MCA(): 0/0 in Tc (on survey data, through the fallback of B4).
MCA(tea[1:6], row.w = c(0, rep(1, 299)), graph = FALSE)
```

**Proposals:**

- **Move to `Suggests`** the imports that serve `AovSum()`, `LinearModel()`, `meansComp()`, `plot.catdes()`, `plot.HCPC()`, `RegBest()` and `theme_factominer()`, with `requireNamespace()` guards (section 2.1's table).
- **Aggregate identical rows before the SVD of an MCA** when there is no `ind.sup` or `quanti.sup`: exact, 9 times faster at a million rows.
- **Return every eigenvalue whatever `ncp`**, the K × K eigen-decomposition being cheap.

---

## Appendix B. How to rerun

From the package root, one section per cold, memory-capped process. The outputs go to `$ENGINE_OUT`. `extract` must run first, since the Enquête Emploi sections read its cache.

```bash
export ENGINE_OUT=/path/to/scratch
run() { ( ulimit -v 12582912; OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript dev/analysis_engine.R "$@" ); }
run env; run deps; run load 10                       # sections 2.1-2.2
run extract; run compression; run map; run sup       # section 4
run parity                                           # sections 4.2, 6
run bench fm_ind ee 1e6; run bench lean ee all 15    # section 5, one cell each (engines in the file)
run burt; run tooltips; run datatable 0              # sections 4.5, 5, 5.2
run fm_burt                                          # section 5.1
run object; R_LIBS_USER=/lib/without/FactoMineR R_LIBS= run nofm   # section 7
run slots; run slim                                  # section 7.1
run bugs; run session                                # Appendix A, section 2.4
```
