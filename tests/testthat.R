library(testthat)
library(ggfacto)

# Pin BLAS/OpenMP before anything spawns: an OpenBLAS-pthread build fixes its thread count from the
# environment at process startup. The suite is small and runs serially, so no worker pool is sized
# here; see CLAUDE.md section Testing for the trap that guards against.
if (Sys.getenv("OMP_NUM_THREADS") == "") Sys.setenv(OMP_NUM_THREADS = "1")

test_check("ggfacto")
