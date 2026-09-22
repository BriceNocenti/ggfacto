# Release checklist — the release-branch pattern

A prompt like "help me release v0.x.y" should be enough: Claude runs every step below itself (commit and push behind a permission prompt) and pauses only for the maintainer's steps, marked **[maintainer]**.

The permanent branches:

- **`dev`** — the everything-branch: full history + `dev/`, `CLAUDE.md`, `.claude/`. All development and future bug fixes happen here.
- **`main`** — strictly user-facing: what a visitor or CRAN sees. Never commit to it directly; it only moves by merging release branches. It is the default branch, so `workflow_dispatch` workflows (rhub) run from it.
- **`gh-pages`** — the built pkgdown site, written by CI (`.github/workflows/pkgdown.yaml`) on every push to main. `docs/` is git-ignored; never commit a built site. GitHub Pages serves this branch (Settings → Pages → `gh-pages`, `/root`).

## Per release

```bash
git checkout dev && git pull

# 1. Pre-flight on dev, IN THIS ORDER.
#    a) Version bumped in DESCRIPTION (x.y.z), NEWS.md section finalized: extremely concise --
#       new functions and arguments in one line each, only the two or three bug fixes that matter.
#       cran-comments.md rewritten for x.y.z, its links left as <FILL>.
#    b) devtools::document(), then the full test suite green (CLAUDE.md § Testing recipe).
#    c) devtools::check(manual = TRUE, remote = TRUE, incoming = TRUE) green (~3 min). NONE of the
#       three arguments is optional:
#         `manual = TRUE`   -- the only step that catches a glyph LaTeX cannot set in an Rd.
#                              Needs HTML Tidy (apt install tidy), or the HTML manual check SKIPS.
#         `incoming = TRUE` -- devtools defaults it to FALSE: no URL check, no CRAN-incoming check.
#       A URL NOTE on bricenocenti.github.io/ggfacto is expected only if the site has never been
#       deployed; it is cleared at step 5.
#    d) The nosuggests rehearsal, which rhub cannot give (see the notes):
#         withr::with_envvar(c("_R_CHECK_DEPENDS_ONLY_" = "true"),
#                            devtools::check(manual = TRUE))
#       Anything a test or an example takes from Suggests (GDAtools, plotly, htmlwidgets, knitr,
#       rmarkdown) must be guarded.
#    e) Rscript -e 'pkgdown::check_pkgdown()' clean, and the site builds: Rscript dev/build_site.R
#    Commit and push dev: R-CMD-check runs on 5 platforms from dev. Fix until green.

# 2. Branch + strip development-only files
git checkout -b release/x.y.z
git rm -r -q dev CLAUDE.md          # plus .claude/ if it is ever tracked
git commit -m "release x.y.z: strip development-only files"

# 2b. Prove the release tree IS dev minus the strip list. BOTH must print nothing.
#     `refs/heads/dev`, not `dev`: the name is a revision AND the directory just stripped.
git diff --name-only refs/heads/dev HEAD | grep -vE '^(dev/|\.claude/|CLAUDE\.md$)'
git ls-files -- dev .claude CLAUDE.md

# 3. PR
git push -u origin release/x.y.z
gh pr create --base main --title "ggfacto x.y.z" --body "<NEWS summary>"
#    Wait for R-CMD-check (5 platforms) and the pkgdown build (no deploy on a PR) green.
#    A fix goes on dev, then `git merge dev` into the release branch (and re-strip if needed).

# 4. [maintainer] Merge the PR on github.com.
#    ALWAYS a merge commit, NEVER squash/rebase: squash breaks the merge-base, so the next release
#    merge would re-conflict on every dev-only file.
git checkout dev
git push origin --delete release/x.y.z
#    [maintainer] git branch -D release/x.y.z   (denied in Claude sessions)

# 5. The URL gate, once pkgdown has deployed from main (gh run watch):
#    every bricenocenti.github.io link in DESCRIPTION, README.md, inst/CITATION and the Rd 404s
#    until then, and a 404 or a 301 is a NOTE from CRAN's incoming check.
Rscript -e 'urlchecker::url_check(".")'

# 6. rhub (from main: its workflow_dispatch needs the default branch) and win-builder.
#    Both URL-check, so both run AFTER step 5.
GITHUB_PAT=$(gh auth token) Rscript -e 'rhub::rhub_check(platforms = c("nold", "atlas", "mkl",
  "donttest", "ubuntu-next", "ubuntu-release"), branch = "main")'
Rscript -e 'devtools::check_win_devel()'   # the result is mailed to the maintainer

# 7. CRAN
#    - Fill cran-comments.md's links (commit on dev, push): the R-CMD-check run of the PR merge on
#      main, and the rhub run.
#    - [maintainer] paste the win-builder link, then devtools::submit_cran() from dev (the
#      .Rbuildignore is the same on both branches, so the tarball is the release one).

# 8. After CRAN acceptance
git tag vx.y.z <merge-commit-sha> && git push origin vx.y.z
gh release create vx.y.z --title "ggfacto x.y.z" --notes "<NEWS section>"
#    then on dev: Version x.y.z.9000, a "# ggfacto (development version)" NEWS heading,
#    and delete CRAN-SUBMISSION.
```

## Notes

- The strip list (step 2) is the single source of truth for "not on main": `dev/`, `.claude/`, `CLAUDE.md`. Everything else stays (`po/`, `vignettes/articles/`, `pkgdown/`, `_pkgdown.yml`, `.github/`, `cran-comments.md`, `.Rbuildignore`). If a new dev-only path appears, add it to step 2 and to `.Rbuildignore`.
- `.Rbuildignore` stays identical on both branches, so the CRAN tarball built from `dev` is the release one, and dev-green means release-green.
- **rhub: only the runtime platforms.** ggfacto has no `src/`, so the compiler containers (`clang*`, `gcc*`, `*-asan`, `valgrind`, `rchk`) exercise a toolchain the package never uses. The six of step 6 are the whole list.
- ⛔ **Never ask rhub for `nosuggests`: that container does not finish** (6 h and killed, measured on tabxplor). Its mechanism is `_R_CHECK_DEPENDS_ONLY_=true`, which `R CMD check` alone reads, so step 1d reproduces it locally.
- The GitHub Actions link in cran-comments.md is the R-CMD-check run of the merge commit on main; the PR run is the same tree.
- Hotfix only if CRAN demands one: fix on `dev`, then run this same checklist for x.y.z+1.
