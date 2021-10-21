## Third submission 
Thanks to Julia Haider, I did enhance the following points : 
- Package names and API in DESCRIPTION where single quoted (or put into \pkg{} in documentation).
- Ensured that functions do not write by default or in examples/vignettes/tests in the user's home filespace. I thought I had corrected that one, and I am not sure what the problem is, so sorry if I miss it again. Saving functions, `ggsave2` and `ggi`, already write to `tempdir()` by default (using `plot_path` internal function, with options to save in a directory chosen by the user). Examples don't create files I can detect. Readme creates images : it seems ok, but is that in fact the problematic point ? I also removed forgotten readme files which where not used anymore (directory "docs").

## Second submission
Message was : "package ggfacto_0.2.2.tar.gz does not pass the incoming checks automatically, please see the following pre-tests" (2 NOTEs on Windows and Debian). Since one note is "New submission", the real one is "Examples with CPU (user + system) or elapsed time > 5s". To make it disappear, I passed most of examples to "don't test" (since they need to calculate correspondence analysis with the package FactoMineR, which is always too long). 

## Test environments
* local Windows 10 install, R 4.1.1 and R devel (4.2.0)
* Ubuntu 20.04.3 (on github actions), R 4.1.1

* https://builder.r-hub.io :
   - Windows Server 2008 R2 SP1, R-devel 
   (Build ID: ggfacto_0.2.2.tar.gz-99c0c4e43d684e498c8fd33e832898bf)
   - Ubuntu Linux 20.04.1 LTS, R-release, GCC, 
   (Build ID: ggfacto_0.2.2.tar.gz-c5bb951f43e844b48c7eec405ad08d5d)
   - Fedora Linux, R-devel, clang, gfortran, 
   (Build ID: ggfacto_0.2.2.tar.gz-de4ef4dde6ae466cb46a90d0e7b1d09)

* https://win-builder.r-project.org/Z01F91j2eUIh : 
   - Windows x86_64-w64-mingw32 (64-bit), R-devel

## R CMD check results
* local Windows 10 and Ubuntu 20.04 (on github actions) :
    There were no ERRORs, WARNINGs or NOTEs. 

* https://builder.r-hub.io : 
    No ERRORs, no WARNINGs, 1 NOTE : 
        New submission

* https://win-builder.r-project.org : 
    No ERRORs, no WARNINGs, 1 NOTE : 
        New submission

## Downstream dependencies
There are currently no downstream dependencies for this package.
