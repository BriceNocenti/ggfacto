## First submission
This is a first submission.

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
    No ERRORs, no WARNINGs, 2 NOTE : 
        New submission
        Examples with CPU (user + system) or elapsed time > 10s

* https://win-builder.r-project.org : 
    No ERRORs, no WARNINGs, 2 NOTE : 
        New submission
        Examples with CPU (user + system) or elapsed time > 5s

The examples which takes time are the miminal examples for the user to understand the usage of the main functions, and can't really be reduced : in order to draw the plots, the correspondence analysis must first be calculated using the package FactoMineR.

## Downstream dependencies
There are currently no downstream dependencies for this package.
