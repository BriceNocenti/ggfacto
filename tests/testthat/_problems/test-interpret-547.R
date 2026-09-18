# Extracted from test-interpret.R:547

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "ggfacto", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())
fx_mca_multi <- function() fx("mca_multi", function() {
  MCA2(fx_tea(), tidyselect::all_of(c("Tea", "How", "how", "where", "price")))
})

# test -------------------------------------------------------------------------
h <- as.character(ggfacto:::gda_render(pca_interpret(fx_pca(), axes = 1:2), "html"))
expect_true(grepl(">Axe 1</th>", h, fixed = TRUE))
