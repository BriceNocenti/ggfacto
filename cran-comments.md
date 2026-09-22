## New version 0.4.0

Major update: new functions for the three analyses (principal component, correspondence and multiple correspondence analysis), their interpretation tables and their clustering. The former API keeps working, through soft deprecation.

## Test environments
* local WSL2 Ubuntu install, R 4.6.1

* github Actions (PR merge) : [https://github.com/BriceNocenti/ggfacto/actions/runs/35751383626](https://github.com/BriceNocenti/ggfacto/actions/runs/35751383626)
  - macOS, R-release
  - Microsoft Windows Server, R-release
  - Ubuntu Linux LTS, R-devel
  - Ubuntu Linux LTS, R-release
  - Ubuntu Linux LTS, R-oldrel-1

* win-builder (R-devel): [https://win-builder.r-project.org/pYO235CgUV85/](https://win-builder.r-project.org/pYO235CgUV85/)

* rhub: [https://github.com/BriceNocenti/ggfacto/actions/runs/35752659051](https://github.com/BriceNocenti/ggfacto/actions/runs/35752659051)
  Platforms run: nold, atlas, mkl, donttest, ubuntu-next, ubuntu-release.

## R CMD check results
* local (devtools::check(manual = TRUE, remote = TRUE, incoming = TRUE)):
  - No Error, no Warning, no Note.

* local, with `_R_CHECK_DEPENDS_ONLY_=true` (the `nosuggests` check):
  - No Error, no Warning, no Note.

* github Actions :
  - No Error, no Warning, no Note.

* win-builder
  - No Error, no Warning, no Note.

* rhub
  - No Error, no Warning, no Note.

## Downstream dependencies
There are currently no downstream dependencies for this package.
