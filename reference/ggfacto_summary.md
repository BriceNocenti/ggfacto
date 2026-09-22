# The interpretation tables, and how they print

[`interpret`](https://bricenocenti.github.io/ggfacto/reference/interpret.md)
(for the three analyses),
[`clust_tab`](https://bricenocenti.github.io/ggfacto/reference/clust_tab.md)
and
[`mean_sd_tab`](https://bricenocenti.github.io/ggfacto/reference/mean_sd_tab.md)
all return **one** `tabxplor` table, so it can be piped, filtered and
exported like any other. What differs is only how it is *shown*:

`options(tabxplor.print = "html")` draws it with
[`tab_html`](https://bricenocenti.github.io/tabxplor/reference/tab_html.html):
the Viewer pane in RStudio/Positron, a real html table when knitted. The
default, `"console"`, prints the plain `tabxplor` grid. It is the same
option that governs an ordinary crosstab — one to set, once, at the top
of a script — and it is read at print time, so it can be set after the
table is built. For a text file or a language model, pipe the table into
[`tab_md`](https://bricenocenti.github.io/tabxplor/reference/tab_md.html)
explicitly. An html summary of *axes* carries no hover tooltip — every
figure one would reveal already has a column of its own — while
[`clust_tab`](https://bricenocenti.github.io/ggfacto/reference/clust_tab.md),
being a crosstab of percentages, keeps them: the count behind each one
is worth hovering for.

An analysis-of-axes summary carries the **eigenvalues** as a subordinate
table
([`set_footer_tabs`](https://bricenocenti.github.io/tabxplor/reference/set_footer_tabs.html)),
which every medium renders under it: the percentage of variance of each
axis, its cumulated percentage, and for an MCA Benzecri's modified rate
— the numbers the rule for choosing how many axes to interpret is read
on.

`eig = FALSE` leaves them out, for a document that shows them already or
prints the summary several times to comment it column by column;
`n_axes` says how many of them to print. When some axes are left out —
by `n_axes`, or because `ncp` truncated the analysis — a final row
states how many the cloud has (`... of 27`). A table showing every axis
carries no such row. The `Total` row is always the whole cloud: 100 %
and the total inertia.

`min_contrib` moves the threshold: `NULL` (the default) keeps the points
contributing more than the mean — Le Roux and Rouanet's rule — `0` keeps
them all, and a number keeps what contributes at least that many
percent. The summary row's label follows it, so it can never name a set
it does not total — and in a correspondence analysis, where each axis
carries two such rows, it leads with the margin's own name
(`Rows: above mean ctr`, or the name `vars` gave it). `color = FALSE`
builds the table with no colour measure at all.

`lang` is `NULL` (the session's language), `"en"` or `"fr"`: it
translates what a reader reads as prose — the axis heading, the summary
row's label, the words the colour legend uses and the glossary lines
under it. **Column names are never translated**: they are the tibble's
own names, and a name that changed with the language could not be
indexed. These words are fixed when the table is *built*, so an export
asking for the other language (`tab_md(lang = )`) gets tabxplor's
grammar translated and ggfacto's nouns as they were written: **build the
table in the language you will print it in**.

`complete = TRUE` widens an MCA or CA summary: each side of the axis
gains the point's **coordinate** (its sign says which pole, its size how
far out) and its **cos2** (the share of the point's own variance the
axis holds), plus the *spread* between the two sides. Neither is
coloured there: a coordinate in axis standard deviations has no
conventional cut-off, and an MCA cloud has so many axes that every cos2
is small — the 50 % / 75 % rule a
[`interpret`](https://bricenocenti.github.io/ggfacto/reference/interpret.md)
table of a PCA reads does not transfer. Both are read by comparing the
points shown; only the contribution carries an absolute threshold.

## Usage

``` r
# S3 method for class 'ggfacto_summary'
print(x, ...)
```

## Arguments

- x:

  A table returned by one of the functions of \[ggfacto_summary\].

- ...:

  Passed to
  [`tab_html`](https://bricenocenti.github.io/tabxplor/reference/tab_html.html),
  or to the console print method.

## Value

`x` invisibly (or the rendered object, for html).

## The footer

The **colour legend is tabxplor's, saying ggfacto's nouns** — a
factorial axis has no chi-squared, so
[`set_legend_words`](https://bricenocenti.github.io/tabxplor/reference/set_legend_words.html)
re-states what the ladder grades and nothing else. It is therefore built
at render, in the language and the palette of the call that prints it,
with its coloured swatches, in all five media. **Nothing to suppress**:
a call written by hand is just
`interpret(res.mca) |> tab_md(css = FALSE, print = FALSE)`. Under it,
one plain line names each statistic the colours do *not* grade.

## After a dplyr verb

The subclass is not carried by dplyr (only a table's `tabxplor`
attributes are), so a summary that has been through `mutate()` prints as
an ordinary `tabxplor` table — the eigenvalues still render under it,
and the format is the same `options(tabxplor.print)` either way. What is
lost is only the hover policy and the margin names.

## See also

\[interpret()\], \[clust_tab()\], \[mean_sd_tab()\].
