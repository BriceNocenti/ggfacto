# Hierarchical clustering: the design of the workflow

This is the transversal reference for how ggfacto clusters an analysis, why the workflow has the shape it has, and what was weighed and set aside. `R/clust.R` states the code's own constraints and points here; the course material in `~/github/formations_stat` is the main user of what follows.

---

## 1. What the workflow must do

The package speaks to a "literary" social-science student and to a survey analyst. For clustering, that gives six requirements:

- **One gesture per step**, the same for the three analyses: the analysis, its tree, the clusters written into the data frame with `mutate()`, their table, their graph.
- **Nothing given twice.** The weights, the active variables and the rows analysed belong to the analysis; no later call asks for them again.
- **A subset analysed, the whole data frame kept.** Supplementary variables, clusters and later tables all use the one data frame the user loaded.
- **Changing the number of clusters is free.** The tree is the expensive part and depends neither on the number of clusters nor on their names.
- **The clusters are `FactoMineR::HCPC()`'s**, so a result is the one the literature and FactoMineR give (pinned by `tests/testthat/test-clust.R`).
- **The design carries over to a jamovi module**, where every option change re-runs the analysis.

---

## 2. What the survey found

The course book (`cours/M2S1/livre/`), its exams, the exploration notebooks (`dev/explorations/*.qmd`), the skill reference `agd.md`, the maintainer's research scripts (`socio_public_services/current_private/ctall.R` and its `Demarrage.R` helpers) and a sequence analysis (`~/Data/CASD SIASP FGE/Scripts/pts_analysis.R`) were read end to end.

### 2.1 The course and its exams

The taught chain is the one of the exam corrections:

```r
acm <- multiple_correspondence_analysis(FES2017, all_of(variables_actives), wt = w5,
                                        excl = exclure_categories)
mca_interpret(acm, axes = 1:2)
FES2017 <- FES2017 |> mutate(cah_elections = hierarchical_clust(acm, ncp = 2, nb_clust = 6))
ggmca(acm, FES2017, clust = cah_elections, profiles = TRUE)
clust_tab(FES2017, all_of(variables_actives), cah_elections, wt = w5)
FES2017 <- FES2017 |> mutate(cah_elections = fct_recode(cah_elections, "1-Nom" = "1", ...) |>
                               fct_relevel(sort))
clust_tab(FES2017, all_of(variables_sup), cah_elections, pct = "row", wt = w5)
ggmca(acm, FES2017, clust = cah_elections, profiles = TRUE)
```

What it costs a student:

- **The tree is built three times** in `06-CAH.qmd`: once to look at it, once cut at 6 to show the cut, once in `mutate()`. The taught gesture is "look at the tree, then compute again with `nb_clust`".
- **The weights and the active variables are given twice**, in the analysis and in every `clust_tab()`. It is the one place the package's "weights ride one channel" breaks: a forgotten `wt =` describes a weighted analysis with unweighted numbers.
- **The renaming block** is six `"k-Name" = "k"` lines plus `fct_relevel(sort)`, and 06-CAH swaps two number prefixes to reorder clusters.
- **`profiles = TRUE`** is written on every `ggmca(clust =)`.
- **Row against column percentages** confuse students (06-CAH's own TODO); and `pct = "row"` on a table of supplementary variables silently kept only the first level of each binary variable, which only a column reading allows.
- **The tree's barplot** must be read by counting bars to know how many clusters a cut makes.
- Clusters are never computed on a PCA or a CA in the course; the exams impose `ncp = 2, nb_clust = 6`.

### 2.2 The exploration notebooks

Every notebook that clusters builds a **separate analysis data frame** and runs everything on it:

```r
sorties <- pc18 |>
  select(all_of(c(actives, "POND", "AGE_5", "DIPLOM_4"))) |>
  drop_na(all_of(actives))
acm <- multiple_correspondence_analysis(sorties, all_of(actives), wt = POND)
sorties <- sorties |> mutate(classe = hierarchical_clust(acm, ncp = 3, nb_clust = 9))
```

- `drop_na()` rests on a belief the package does not share: with `excl = NA` (the default) a missing answer is a passive level and the individual stays in the cloud (the course's own "ACM spécifique", `08-Explo.qmd`). One notebook calls `drop_na()` with no columns and loses individuals missing only a supplementary variable.
- `select()` carries the supplementary variables into the subset. It is also what stops the analysis from recording which rows of `pc18` it used (section 8).
- Two notebooks cluster a PCA of scores and describe the clusters with `tab()` on the original categorical items, clusters in rows. Several compare numbers of clusters in prose only, because comparing meant computing again.

### 2.3 The old research scripts

**Good ideas, kept or proposed here:**

- One tree, many cuts (`HCPC_cut_tree()`, `HCPC_recut()`), with the old-to-new mapping printed when recutting.
- Clustering the distinct answer profiles with summed weights (`HCPCprofiles()`), now the core of `ward_tree()`.
- Comparing partitions by crossing them (`tab(clust5, clust3)`), and all partitions as supplementary variables on one map.
- Joining clusters back by an identifier rather than by position.
- Extending clusters to rows outside the analysed population (kNN on the MCA coordinates).
- Explicit `stop()` on a length or level mismatch.

**Bad ideas, removed by construction:**

- Positional write-back under a repeated logical filter (`ct[filter, ]$x <- cah$data.clust$clust`), which breaks as soon as the filter differs from the analysis's one; the scripts do exactly that twice.
- Refitting the MCA to change `ncp`, and re-running `HCPC()` to change `nb.clust`.
- One R object per partition, named after its settings (`cah_orga_STARK_19axes_7cl`), with references to deleted objects.
- Hand-written number-to-name maps per run, different for the consolidated and the raw cut of the same tree.
- `HCPC(kk =)` pre-clustering, which made the tree depend on a k-means and broke recutting.

### 2.4 Sequence analysis

`pts_analysis.R` builds one Ward tree per field (`fastcluster::hclust()` on an optimal-matching distance) and cuts it at every level from 1 to 20 at once (`all_clust()`). The result is a nesting table: one row per final cluster, its parent at each level, the height and drop at which it appeared, its share. It then picks a level by a stated rule (the last drop above the mean drop, with a minimum cluster size). The partition is joined back to the individuals by their identifier. For large fields it clusters a weighted sample of distinct sequences and assigns the rest to the nearest cluster.

What carries over: nested partitions are only nested **without** consolidation, and comparing them is a crosstab of two columns (section 5); the join by identifier is what `res$source` does for a filtered analysis (section 8).

### 2.5 The former `HCPC_tab()`

`HCPC_tab()` (0.3.0) took a data frame and a cluster column, never an HCPC object, and returned one tabxplor table. That shape was right and is kept; what changes is where the variables, the weights and the rows come from (section 9).

---

## 3. What it costs

Measured on the course's `pc_AGD` (9,234 weighted individuals, 15 active variables, 5,312 distinct answer profiles, `ncp = 3`):

| Step                                                         | Cost    |
|--------------------------------------------------------------|---------|
| Ward's tree on the distinct points (`ward_tree()`)           | 0.43 s  |
| Cut and k-means consolidation (`cut_ward_tree()`)            | 0.05 s  |
| The cache key, `rlang::hash()` of the inputs                 | 1 ms    |
| The tree object kept                                         | 0.44 MB |
| 06-CAH's three calls (tree, cut at 6, `mutate()`), one tree  | 0.66 s  |
| Three partitions in one `mutate()`, one tree                 | 0.67 s  |

The tree's time is quadratic in the distinct points (~25 s at 40,000), its memory linear (phase 1o); a kept tree takes about 2.5 MB at 40,000 distinct points among 200,000 individuals.

---

## 4. Cutting the tree again without building it

### 4.1 The mechanisms weighed

Each was judged on the beginner's code, what an expert can customise, the risk of a stale result, persistence across sessions, memory, the one-line `mutate()` gesture, jamovi, and the former calls.

| Mechanism                                        | Verdict                                                  |
|--------------------------------------------------|----------------------------------------------------------|
| Content-addressed session cache                  | **chosen**: no new code to learn, never stale            |
| Explicit tree object (`hclust()` + `cutree()`)   | a second object and verb for students; later, for experts |
| Cache in an environment slot of the analysis     | breaks `identical()` on fits; misses raw FactoMineR fits |
| Tree carried as an attribute of the cluster column | lost by `[`, `factor()`, `case_when()`; stale after `filter()` |
| vctrs factor subclass holding the partitions     | `if_else()` and `case_when()` fail against plain factors |
| Data-first verb re-cutting an existing column    | behaviour hidden in the state of the data                |
| Vectorised `nb_clust` (all partitions at once)   | clutters the data; unneeded once a cut is free           |
| Disk cache (`tools::R_user_dir()`)               | stale files to manage, a CRAN burden, nothing gained     |
| knitr chunk cache                                | the user's business, and fragile                         |
| A faster tree                                    | exact Ward is already linear in memory (phase 1o)        |

Three of them deserve their reasons in full:

- **The attribute and the subclass** were probed: an attribute survives `dplyr::filter()`, `arrange()`, `forcats::fct_recode()` and `saveRDS()`, but not base `[`, `c()`, `factor()` or a parquet round trip, and after a `filter()` the tree no longer matches the rows it would re-cut. A feature that silently disappears on half the idioms a student writes is worse than none.
- **The explicit object** is the classic R pattern and FactoMineR's own (`HCPC()` then `$call$t$tree`). It costs the student a second object whose name collides with the column's (the 2024 course warned about exactly that), and it is not needed for speed once the cache exists. It stays open for experts as `clust_tree(res, ncp)` returning a real `hclust`, for `dendextend` and the like.
- **The disk cache** would survive a knitted render, but a course document builds one tree per render in under a second.

### 4.2 The design

`hierarchical_clust()` keeps its signature and its one line in `mutate()`. Internally:

- `ward_tree(coord, w, answers)` builds the tree and everything that depends only on the points: HCPC()'s point order, the distinct leaves, their weights. It is a plain list, so it can be stored anywhere.
- `cut_ward_tree(t, coord, w, nb_clust, consol)` cuts it, consolidates, numbers the clusters along the first axis and measures the share of inertia they keep.
- `cached_ward_tree()` keeps the last trees for the session, keyed on `rlang::hash(list(coord, w, answers))`: the coordinates on the `ncp` axes, the weights and the answers, dimnames included (a CA's leaves carry its row names). A kept tree cannot be stale, since a change to anything it was built from changes the key; a refit of the same analysis finds it.
- `options(ggfacto.clust_cache = n)` sets how many trees are kept, 20 by default, first in first out; `0` keeps none.

The consequences for the user:

- The tree, the cut shown, the `mutate()`, a renaming with `names`, and several partitions in one `mutate()` all build one tree.
- A new session builds it once again.
- A hash is not stable across rlang versions: that only costs a rebuild here, and a stored state (section 11) must treat a mismatch as "rebuild".

---

## 5. Choosing the number of clusters

`hierarchical_clust(res, ncp = 3)` draws HCPC()'s tree plot: the dendrogram, one rectangle per cluster of the automatic cut, and top right the inertia gained at each split. Two things make it readable without counting:

- **Each bar carries the number of clusters it makes** (2, 3, 4, ...): the black bars end at the number of clusters of the cut drawn.
- **The title states what the cut keeps**: "6 clusters: the between-cluster inertia is 68.6% of the inertia of axes 1 to 3", computed on the individuals with weighted centres, after consolidation. In French: "6 classes : la variance inter représente 68,6 % de la variance des axes 1 à 3", the course's words.

The automatic cut is HCPC()'s rule (the largest relative drop, between 3 and 10 clusters). The course's own rule stays the sociological one: several cuts, kept for the one that reads best, often 4 to 8 clusters.

**Comparing two cuts is a crosstab.** With the tree kept, the partitions are cheap:

```r
pc_AGD <- pc_AGD |> mutate(
  cah_6 = hierarchical_clust(acm, ncp = 3, nb_clust = 6),
  cah_7 = hierarchical_clust(acm, ncp = 3, nb_clust = 7)
)
tab(pc_AGD, cah_7, cah_6)
```

Each row of that table says which clusters of 6 a cluster of 7 comes from. The partitions are exactly nested only with `consol = FALSE`: the k-means consolidation moves individuals across the cut, and on `pc_AGD` several clusters of the 7-cluster partition straddle two of the 6-cluster one. To read how the tree splits, compare the raw cuts; to describe the clusters, the consolidated ones.

---

## 6. Several partitions

No vectorised argument is needed:

```r
# several columns, one tree
data <- data |> mutate(
  cah_5 = hierarchical_clust(acm, ncp = 3, nb_clust = 5),
  cah_6 = hierarchical_clust(acm, ncp = 3, nb_clust = 6),
  cah_7 = hierarchical_clust(acm, ncp = 3, nb_clust = 7)
)

# one table per partition, into one workbook
purrr::map(c("cah_5", "cah_6", "cah_7"), \(cl) clust_tab(acm, data, !!rlang::sym(cl))) |>
  tabxplor::tab_xl(sheets = "tabs")

# every partition on the map
ggfacto(acm, data, sup_vars = c(cah_5, cah_6, cah_7))
```

A vectorised `nb_clust = 5:7` was set aside: it would return a data frame, which `mutate()` packs into one column under a name, or splices only when unnamed, and it breaks "one factor per call".

---

## 7. Naming the clusters

The clusters are numbered along the first axis. `names =` names them in the same call, in the order the levels should take:

```r
pc_AGD <- pc_AGD |> mutate(cah_culture = hierarchical_clust(acm, ncp = 3, names = c(
  "Petit écran"          = 1,
  "Bain audiovisuel"     = 2,
  "Tout numérique"       = 3,
  "Culture patrimoniale" = 4,
  "Éclectisme classique" = 6,
  "Éclectisme augmenté"  = 5
)))
```

- The named form reads as `fct_recode()`, `"new name" = old number`, which the course already teaches; `c("1" = "Petit écran", ...)` is accepted too when only that reading fits, and an unnamed vector names clusters 1, 2, ... in order.
- The order of the vector is the order of the levels: no number prefixes and no `fct_relevel(sort)`.
- `nb_clust` is the number of names when it is not given; a mismatch, a duplicate or an unmapped cluster is refused with the `fct_recode()`-style example.
- The risk is `fct_recode()`'s, no more: names belong to one cut of one tree, and a change of `ncp` or `nb_clust` renumbers the clusters. With the tree kept, the named call costs only the cut.

---

## 8. Subpopulations

### 8.1 The rule

**Declare the subset in the analysis, then always hand the whole data frame:**

```r
acm <- pc_AGD |>
  filter(CRITAGE %in% c("1-15 à 29 ans", "2-30 à 44 ans")) |>
  multiple_correspondence_analysis(all_of(variables_actives), wt = POND)
# or, the same analysis
acm <- multiple_correspondence_analysis(pc_AGD, all_of(variables_actives), wt = POND,
                                        filter = CRITAGE %in% c("1-15 à 29 ans", "2-30 à 44 ans"))

pc_AGD <- pc_AGD |> mutate(cah_jeunes = hierarchical_clust(acm, ncp = 2, nb_clust = 4))
ggfacto(acm, pc_AGD, sup_vars = all_of(variables_sup), clust = cah_jeunes)
clust_tab(acm, pc_AGD, cah_jeunes)
pc_AGD |> filter(is_in_analysis(acm)) |> tab(SEXE, CRITAGE)
```

The analysis records in `res$source$key` one entry per row of `pc_AGD`: its answer profile, or `NA` for the rows it left out (5,988 of 9,234 here). Every function that takes the microdata back goes through `align_to_fit()`, which picks the analysed rows and checks their answers and weights again: the clusters are written on the right rows with `NA` elsewhere, the graph and the table describe the analysed population, and a data frame reordered, shortened or extended since the fit is refused with the counts instead of misaligned.

### 8.2 What is recorded, what is refused

| The analysis is given                                   | Rows recorded                              |
|---------------------------------------------------------|--------------------------------------------|
| `data`                                                  | all of them                                |
| `data, filter = cond`                                   | the rows where `cond` is TRUE              |
| `data \|> filter() \|> mutate() \|> drop_na(vars)`      | the rows kept, proved by re-running        |
| `data[cond, ]`, `subset(data, cond)`, `arrange()`       | the rows kept, fitted in the pipe's order  |
| a `select()` in the pipe                                | none: the hidden row id is lost            |
| `slice_sample()`                                        | none: a re-run draws another sample        |
| `data %>% filter()`                                     | none: the expression is only `.`           |
| `sub <- filter(data, ...)`, then `sub`                  | the rows of `sub`                          |

"None" makes the subset its own reference: it still works everywhere, and the whole data frame is refused with a message giving both counts and the `filter =` to write instead. A zero weight leaves its row out in every case.

### 8.3 Pitfalls the notebooks fell into

- **`drop_na()` before a specific MCA.** `excl = NA`, the default, makes a missing answer passive; dropping the individual changes the population and biases it (`08-Explo.qmd` says so).
- **`select()` before the analysis.** Nothing needs it: the analysis takes `active_vars`, and supplementary variables are read from the whole data frame later.
- **Variables created on the subset.** A variable made in the analysis's pipe exists only there, so the whole data frame lacks it afterwards. Create variables on the whole data frame, filter in the pipe.

### 8.4 Later: clusters for the rows outside

The old research scripts extended clusters to a later survey by kNN on the MCA coordinates. The principled version is the k-means rule the consolidation already uses: project the other rows as supplementary individuals (the barycentric formula on the fitted levels, excluded levels left out) and give each the cluster of the nearest weighted centre on the `ncp` axes. It would be an argument of `hierarchical_clust()` (e.g. `extend = TRUE`), writing clusters on every row rather than `NA` outside the subset. It needs care with levels unseen in the fit and with specific MCA, and is not implemented.

A class-specific MCA (`GDAtools::csMCA()`, the analysis of a subpopulation within the space of the whole one) is a different question, answered by a different analysis; ggfacto reads a `csMCA()` fit through its model, and `hierarchical_clust()` clusters its subcloud.

---

## 9. Describing the clusters

### 9.1 `clust_tab(res, data, clust)`

The table takes the analysis first, in the order of `ggfacto(res, data, clust = )`:

- **The variables** default to the active ones; `row_vars =` describes the clusters by others.
- **The weights** are the analysis's (`res$call$row.w`, or `row.w.init` for a PCA), under the name of their column (`res$source$wt`), so the caption still reads "Weighted by POND."; `wt =` is refused in this form.
- **The rows** are the analysed ones (`align_to_fit()`); rows without a cluster are left out.
- **`pct = "col"`**, the default, reads each cluster as a distribution; only there does a binary variable say everything in one row. **`pct = "row"`** reads each level across the clusters and keeps both levels.
- **`excl = NA`** hides the missing answers while counting them in the percentages.

A correspondence analysis clusters the levels of one variable, and is described by crossing the clusters with the other one (section 9.4).

### 9.2 Numbers, and a principal component analysis

A numeric variable enters as a **mean row, with its coefficient of variation**, coloured by its difference from the whole population **in standard deviations** (tabxplor's Glass's delta, ladder 0.1 / 0.2 / 0.4 / 0.8). For a PCA on standardised variables this is the analysis's own metric, where a ratio of means or a cv would say nothing (means near 0).

`shape =`, passed to `tabxplor::tab()`, cuts the numbers into levels instead: `shape = "sd_bands"` (below and above the mean, beyond one standard deviation), `"quintiles"`, or `c(AGE = "quintiles")` for one variable. Means and bands are the two readings of a number, a centre and a distribution; the default is the centre, the bands one argument away.

**Where the population goes.** In a table of levels, the "% of population" and "n" rows sit at the bottom. In a table where every row is a mean (a PCA's active variables), they go under it as a footer table: bound into it, their percentages would make each column "mixed", which an additive ladder cannot grade, and every mean would lose its colour. A table mixing level rows and mean rows (an MCA with a count variable beside its active ones) still leaves its means uncoloured: grading a mixed column per cell is tabxplor's TODO for 2.1.0, and `color = "ratio"` grades every row meanwhile.

### 9.3 Could `tab()` replace it?

For an MCA, the closest plain call is:

```r
pc_AGD |>
  filter(!is.na(cah_culture)) |>
  tab(all_of(variables_actives), cah_culture, wt = POND, pct = "col", color = "diff", tot = "col")
```

What it loses against `clust_tab(acm, pc_AGD, cah_culture)`:

- the weights, the variables and the rows read from the analysis (the student gives them again);
- one row per binary variable (38 rows against 31 on `pc_AGD`);
- the "% of population" and "n" rows;
- missing answers hidden but counted (`na = "keep"` shows them, `na = "drop"` changes the denominators);
- `cleannames` on the levels and the clusters by default, and the summary class with its hover counts.

For a PCA, the plain call is already the right table, clusters down and variables across:

```r
tab(data, cah, all_of(active_vars), wt = w, color = "diff")
```

The verdict: the course teaches `clust_tab()` for the one table the exam needs, and `tab()` wherever it is the same thing, which includes crossing the clusters with anything else.

### 9.4 A correspondence analysis

Its clusters group the levels of one variable; each individual gets the cluster of its level in `mutate()`. They are described by the other variable of the table:

```r
ac  <- tab(gss, relig, partyid) |> correspondence_analysis()
gss <- gss |> mutate(relig_clust = hierarchical_clust(ac, ncp = 2, nb_clust = 4))
tab(gss, relig_clust, partyid, pct = "row", color = "contrib")   # the clusters, described
tab(gss, relig, relig_clust)                                      # which levels each holds
```

### 9.5 The former form

`HCPC_tab(data, row_vars, clust, wt)`, and `clust_tab()` given a data frame first, keep working as they did, with one notice per session pointing to the package's guide (`https://bricenocenti.github.io/ggfacto/articles/ggfacto.html`, which the vignette of phase 1r publishes). A data frame first is re-dispatched to `HCPC_tab()`'s own signature, so every former call shape binds as before.

---

## 10. The course code

The MCA, as the course teaches it (`06-CAH.qmd`):

```r
acm <- multiple_correspondence_analysis(pc_AGD, all_of(variables_actives), wt = POND)
interpret(acm, axes = 1:3)

hierarchical_clust(acm, ncp = 3)                   # the tree, to choose the number of clusters

pc_AGD <- pc_AGD |>
  mutate(cah_culture = hierarchical_clust(acm, ncp = 3, nb_clust = 6))
clust_tab(acm, pc_AGD, cah_culture)
ggfacto(acm, pc_AGD, clust = cah_culture, interactive = TRUE)

pc_AGD <- pc_AGD |>                                # the names, once the clusters are read
  mutate(cah_culture = hierarchical_clust(acm, ncp = 3, names = c(
    "Petit écran" = 1, "Bain audiovisuel" = 2, "Tout numérique" = 3,
    "Culture patrimoniale" = 4, "Éclectisme classique" = 6, "Éclectisme augmenté" = 5
  )))
clust_tab(acm, pc_AGD, cah_culture, row_vars = all_of(variables_sup), pct = "row")
ggfacto(acm, pc_AGD, clust = cah_culture)
```

On a subpopulation, only the analysis's line changes (section 8.1). A PCA:

```r
acp <- principal_component_analysis(ee_sal19, all_of(variables_actives), wt = EXTRID)
interpret(acp)
hierarchical_clust(acp, ncp = 2)
ee_sal19 <- ee_sal19 |> mutate(cah = hierarchical_clust(acp, ncp = 2, nb_clust = 4))
clust_tab(acp, ee_sal19, cah)                        # means, coloured in standard deviations
```

A CA is in section 9.4.

---

## 11. jamovi

A jamovi analysis re-runs from its options at every change, and only an Image element's `$state` survives the engine's reset (see `~/github/tabxplor/dev/jamovi_module.md` §4.3 and §10.1). The clustering maps onto it as follows:

- **The seam.** The build and the cut are two functions: the module calls `ward_tree()` when its key misses, keeps the tree (a plain list) in the `$state` of a hidden Image declared `clearWith: []`, and calls `cut_ward_tree()` on every run. The key is the same `rlang::hash()` of the coordinates, weights and answers; a key that differs, including after an rlang upgrade, means "rebuild".
- **The options.** `ncp`, the variables, the weights and `excl` change the key; `nClust`, `consol` and `names` only the cut. A large cloud (tens of thousands of distinct points) would make the tree a staged step behind a Run button, as tabxplor's regression comparison already is.
- **The clusters** are an `Output` column (`measureType: nominal`), written with `setRowNums()` and `setValues()`, as the `snowCluster` module does.
- **Subpopulations** are jamovi's own filters: the analysis receives the filtered rows, and `setRowNums(rownames(self$data))` writes the clusters back on them, which is what `res$source$key` does in R.
- **Weights** are jamovi's data-level weights, which reach the analysis as a column: the same channel as `wt =`.

---

## 12. Set aside, and open

**Set aside, with their reasons in the sections above:** the column attribute and the vctrs subclass (4.1), the data-first verb (4.1), vectorised `nb_clust` (6), a disk cache (4.1).

**Open:**

- `extend =`, clusters for the rows outside an analysed subset (8.4).
- An exported `clust_tree(res, ncp)` returning a real `hclust`, for expert dendrogram tooling (4.1).
- Paragons, the individuals nearest each centre, for a PCA of named individuals (`HCPC()$desc.ind`).
- Coloured means beside coloured percentages in one table, which waits on tabxplor 2.1.0 (9.2).
