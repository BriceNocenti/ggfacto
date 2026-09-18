# PURPOSE: The analyses every test file builds on, computed once and cached.
# ROLE: ggfacto computes no analysis of its own, so every test needs a fitted FactoMineR object
#   first. Building one per test_that() would dominate the suite's runtime.
# KEY CONSTRAINTS:
#   - MCA fixtures use tea[1:6]: six binary questions exercise every code path the eighteen do, and
#     keep the goldens short.
#   - mca_ind() is FactoMineR on the individuals: the reference a fit on the answer profiles must
#     equal, since multiple_correspondence_analysis() feeds FactoMineR the profiles.
#   - The fixtures mirror the datasets the roxygen examples use (tea, mtcars, gss_cat), so a test
#     failure points at a call a user can actually reproduce from the manual.
#   - testthat sources helpers once per worker, so the cache is shared across files.

fx_cache <- new.env(parent = emptyenv())

fx <- function(name, compute) {
  if (is.null(fx_cache[[name]])) assign(name, compute(), envir = fx_cache)
  fx_cache[[name]]
}

# --- the data ----------------------------------------------------------------------------------

fx_tea <- function() fx("tea", function() {
  e <- new.env(); utils::data("tea", package = "FactoMineR", envir = e); e$tea
})

# tea[1:6] are all binary ("Not.breakfast" / "breakfast"). That is deliberate: a battery of yes/no
# items is the ordinary MCA input, and it is the shape that used to break ggmca_initial_dims().
fx_active <- function() names(fx_tea())[1:6]

# tea plus an HCPC cluster column, which is what `clust =` names. Clusters are a column of the data,
# never the HCPC object itself.
fx_tea_clust <- function() fx("tea_clust", function() {
  d <- fx_tea()
  d$clust <- hierarchical_clust(fx_mca(), ncp = 3, nb_clust = 4, tree = FALSE)
  d
})

# A deterministic, non-constant weight, so a weighted result is provably different from an
# unweighted one rather than accidentally equal to it.
fx_tea_wt <- function() fx("tea_wt", function() {
  d <- fx_tea()
  d$w <- rep(c(0.5, 1.5), length.out = nrow(d))
  d
})

# tea with missing answers in two active variables, so the `<VAR>.NA` levels and `excl = NA` have
# something to act on.
fx_tea_na <- function() fx("tea_na", function() {
  d <- fx_tea()
  d$breakfast[1:20] <- NA
  d$lunch[10:40]    <- NA
  d
})

# --- the analyses ------------------------------------------------------------------------------

fx_mca    <- function() fx("mca",    function() MCA2(fx_tea(), 1:6))
fx_mca_wt <- function() fx("mca_wt", function() MCA2(fx_tea_wt(), 1:6, wt = "w"))

# An MCA of a SUBSET, piped from a named data frame: it records which of its rows it analysed.
fx_mca_young <- function() fx("mca_young", function() {
  d <- fx_tea()
  d |> dplyr::filter(age < 30) |> multiple_correspondence_analysis(1:6)
})

# FactoMineR::MCA() on the INDIVIDUALS, through ggfacto's own ingress (the `<VAR>.NA` levels, the
# `excl` rule): the reference an analysis fitted on the answer profiles must equal.
mca_ind <- function(data, vars, wt = NULL, excl = NA, ncp = Inf) {
  X <- na_levels(as.data.frame(data[vars]), names(data[vars]))
  FactoMineR::MCA(X, ncp = ncp, row.w = if (!is.null(wt)) data[[wt]], graph = FALSE,
                  excl = excl_index(X, names(X), excl))
}

fx_pca <- function() fx("pca", function() {
  d <- mtcars[1:7]; names(d)[names(d) == "wt"] <- "weight"
  FactoMineR::PCA(d, graph = FALSE)
})

fx_ca <- function() fx("ca", function() {
  FactoMineR::CA(as.matrix(tabxplor::tab(forcats::gss_cat, race, marital)), graph = FALSE)
})

# --- plot models -------------------------------------------------------------------------------

# ggmca_data() messages the colour groups it found on every call; that is not what is under test.
md <- function(...) suppressMessages(ggmca_data(...))

# The handful of ggmca_data() calls the suite makes over and over, cached rather than rebuilt.
# A caller may edit what it gets back: R copies on modify, so the cache cannot be corrupted.
fx_pd <- function(name, ...) {
  args <- list(...)
  fx(paste0("pd_", name), function() suppressMessages(do.call(ggmca_data, args)))
}

fx_pd_plain    <- function() fx_pd("plain",    fx_mca(), fx_tea())
fx_pd_active   <- function() fx_pd("active",   fx_mca(), fx_tea(), active_tables = "active")
fx_pd_sup      <- function() fx_pd("sup",      fx_mca(), fx_tea(), sup_vars = "SPC")
fx_pd_profiles <- function() fx_pd("profiles", fx_mca(), fx_tea(), profiles = TRUE)
fx_pd_clust    <- function() fx_pd("clust",    fx_mca(), fx_tea_clust(),
                                 clust = "clust", profiles = TRUE)

# --- drawing -----------------------------------------------------------------------------------

# Rscript writes Rplots.pdf into the working directory whenever a plot prints with no device open,
# which turns R CMD check into a NOTE. Every test that draws opens this first.
local_null_device <- function(.env = parent.frame()) {
  grDevices::pdf(tempfile(fileext = ".pdf"))
  withr::defer(grDevices::dev.off(), envir = .env)
}

# ggmca_plot() and ggca() print the saved file path with writeLines(), and ggmca_data() messages the
# colour groups it found. Neither is under test; this keeps the reporter readable.
quietly <- function(expr) {
  utils::capture.output(out <- suppressMessages(force(expr)))
  out
}
