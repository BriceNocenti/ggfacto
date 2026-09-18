# PURPOSE: What should compute ggfacto's analyses -- FactoMineR, FactoMineR fed the answer profiles,
#   or an engine of our own -- measured: dependencies, load time, parity, speed, memory, scale.
# ROLE: The research prototype behind dev/analysis_engine.md, kept out of R/: every figure of the
#   document comes from one section of this file. The engines replicate FactoMineR 2.16's algebra --
#   an MCA is the CA of the indicator table, svd.triplet()'s branches and sign rule, `excl` as a
#   1e-15 column mass during the SVD -- on the DISTINCT answer profiles, weighted by the summed
#   weights of their individuals.
# KEY CONSTRAINTS:
#   - Each benchmark cell is its own cold process under a memory cap: an out-of-memory error kills
#     the whole WSL distro. FactoMineR is never run where its predicted peak exceeds 6 GB.
#   - Only svd.triplet()'s full-SVD branches are replicated: ggfacto's ncp = Inf never reaches its
#     seed-dependent irlba branch (taken when ncp < min(n, K) / 2).
#   - Outputs go to $ENGINE_OUT (default: tempdir()); nothing is written under the package.
# Run from the package root, one section per process:
#   ( ulimit -v 12582912; OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 \
#     Rscript dev/analysis_engine.R <section> [args] )
# Sections: env deps load parity extract compression map sup bench burt tooltips datatable fm_burt
#   object nofm slots slim bugs session (extract first: the Enquete Emploi sections read its cache).
# See: dev/analysis_engine.md, Appendix B.

out_dir <- Sys.getenv("ENGINE_OUT", tempdir())
ee_path <- file.path(out_dir, "ee_extract.rds")
course  <- path.expand("~/github/formations_stat/etudiants/donnees")
pc_vars <- c("MUSIQUE", "TELE", "RADIO", "LIVRES", "CINEMA", "JV", "VIDEOS", "RESEAUX", "DANSE",
             "THEATRE", "CLASSIQUE", "POP_ROCK_JAZZ", "CIRQUE", "MUSEE_EXPO", "MONUMENT")
# Nested sets of the Enquete Emploi's questions, from 5 to 15.
ee_q5  <- c("SEXE", "MATRI", "DIPLOME", "PPP1", "TEMPS")
ee_q8  <- c(ee_q5, "CONTR", "EMP_PUB", "ANCIENNETE")
ee_q10 <- c(ee_q8, "TAILLE_ETAB", "TT")
ee_q15 <- c(ee_q10, "FONCTION", "REG", "AGE5", "CSER", "STAT2")

load_pkg <- function() suppressMessages(devtools::load_all(".", quiet = TRUE, export_all = TRUE))

say <- function(...) cat(sprintf(...), "\n", sep = "")

# Elapsed seconds of `expr`, best of `times`, warm (the first call pays the namespace loads).
timed <- function(expr, times = 3) {
  expr <- substitute(expr)
  env  <- parent.frame()
  eval(expr, env)
  min(vapply(seq_len(times), function(i) system.time(eval(expr, env))[["elapsed"]], numeric(1)))
}

# Peak memory R allocated for `expr` (Vcells + Ncells high-water mark, MB), and its value.
peak_mb <- function(expr) {
  gc(reset = TRUE)
  before <- sum(gc()[, 2])
  value  <- expr
  list(mb = sum(gc()[, 6]) - before, value = value)
}

# === SECTION: env -- the machine =============================================================

section_env <- function() {
  si <- utils::sessionInfo()
  say("%s | %s | BLAS %s | LAPACK %s", si$R.version$version.string, si$running,
      basename(si$BLAS), basename(si$LAPACK))
  say("locale: %s", Sys.getlocale("LC_COLLATE"))
  for (p in c("FactoMineR", "GDAtools", "factoextra", "explor", "vctrs", "Matrix", "fastcluster"))
    say("%-11s %s", p, tryCatch(as.character(utils::packageVersion(p)), error = function(e) "-"))
}

# === SECTION: deps -- what FactoMineR weighs in ggfacto's install ===========================

# dev/dependency-audit.md's method: recursive Depends + Imports + LinkingTo, Priority packages free,
# du -sb. FactoMineR's scenarios edit its Imports cell; nothing is installed.
section_deps <- function() {
  ip <- utils::installed.packages()
  ip <- ip[!duplicated(ip[, "Package"]), ]
  db <- ip[, c("Package", "Depends", "Imports", "LinkingTo", "Suggests")]
  gg <- read.dcf("DESCRIPTION", fields = c("Imports", "Suggests"))
  direct <- sub("\\s*\\(.*", "", trimws(unlist(strsplit(paste(gg[1, ], collapse = ","), ","))))
  direct <- setdiff(direct[nzchar(direct)], "R")
  free   <- ip[!is.na(ip[, "Priority"]), "Package"]

  tree <- function(db, pkgs) {
    deps <- tools::package_dependencies(pkgs, db = db, which = c("Depends", "Imports", "LinkingTo"),
                                        recursive = TRUE)
    setdiff(unique(c(pkgs, unlist(deps))), c("R", free, rownames(installed.packages(priority = "base"))))
  }
  size <- function(p) {
    p <- p[p %in% ip[, "Package"]]
    sum(vapply(p, function(x) as.numeric(strsplit(system2("du", c("-sb", shQuote(
      find.package(x))), stdout = TRUE), "\t")[[1]][1]), numeric(1))) / 1e6
  }
  compiled <- function(p) {
    p <- p[p %in% ip[, "Package"]]
    sum(vapply(p, function(x) identical(utils::packageDescription(x)$NeedsCompilation, "yes"),
               logical(1)))
  }
  fm <- which(db[, "Package"] == "FactoMineR")
  imports_216 <- trimws(strsplit(db[fm, "Imports"], ",")[[1]])
  core  <- c("cluster", "ggplot2", "ggrepel", "graphics", "grDevices", "irlba", "lattice", "MASS",
             "scales", "stats", "utils")
  scenarios <- list(
    "2.16 (installed)"                     = imports_216,
    "2.17 (drops ggtext)"                  = setdiff(imports_216, "ggtext"),
    "2.17, car to Suggests"                = setdiff(imports_216, c("ggtext", "car")),
    "2.17, car emmeans multcompView to S." = setdiff(imports_216, c("ggtext", "car", "emmeans",
                                                                     "multcompView")),
    "core imports only"                    = core
  )
  without <- tree(db, setdiff(direct, "FactoMineR"))
  full    <- tree(db, direct)
  say("ggfacto tree (Imports + Suggests): %d packages, %.1f MB; without FactoMineR: %d, %.1f MB",
      length(full), size(full), length(without), size(without))
  say("%-38s %5s %7s %9s  %s", "FactoMineR scenario", "pkgs", "MB", "compiled", "not installed")
  for (s in names(scenarios)) {
    d <- db
    d[fm, "Imports"] <- paste(scenarios[[s]], collapse = ", ")
    only <- setdiff(tree(d, direct), without)
    say("%-38s %5d %7.1f %9d  %s", s, length(only), size(only), compiled(only),
        paste(setdiff(only, ip[, "Package"]), collapse = " "))
  }
  only <- setdiff(full, without)
  say("\nExclusive packages of 2.16, by the FactoMineR import that brings them:")
  for (x in intersect(imports_216, only)) {
    reach <- intersect(tree(db, x), only)
    say("  %-14s %3d pkgs %6.1f MB", x, length(reach), size(reach))
  }
  saveRDS(list(only = only, without = without), file.path(out_dir, "deps.rds"))
}

# === SECTION: load -- what the first analysis of a session pays ==============================

# Cold processes: the namespaces a session loads anyway (ggfacto's other Imports), then FactoMineR,
# then each of its imports alone after the same baseline.
section_load <- function(runs = 10) {
  runs <- as.integer(runs)
  gg   <- read.dcf("DESCRIPTION", fields = "Imports")
  base <- sub("\\s*\\(.*", "", trimws(strsplit(gg[1, 1], ",")[[1]]))
  base <- setdiff(base[nzchar(base)], "FactoMineR")
  child <- function(target) {
    code <- sprintf(paste0(
      "invisible(lapply(c(%s), loadNamespace)); before <- loadedNamespaces(); ",
      "t <- system.time(loadNamespace('%s'))[['elapsed']]; ",
      "cat(t, length(setdiff(loadedNamespaces(), before)), '\\n')"),
      paste0("'", base, "'", collapse = ", "), target)
    out <- system2("Rscript", c("-e", shQuote(code)), stdout = TRUE, stderr = FALSE)
    as.numeric(strsplit(trimws(out[length(out)]), " ")[[1]])
  }
  ggfacto_alone <- vapply(seq_len(runs), function(i) system.time(system2(
    "Rscript", c("-e", shQuote("invisible(loadNamespace('ggfacto'))")), stdout = FALSE))[["elapsed"]],
    numeric(1))
  say("cold Rscript + loadNamespace('ggfacto'): median %.2f s (process start included)",
      stats::median(ggfacto_alone))
  fm <- t(vapply(seq_len(runs), function(i) child("FactoMineR"), numeric(2)))
  say("FactoMineR after the %d namespaces of a session: median %.2f s, %d more namespaces",
      length(base), stats::median(fm[, 1]), as.integer(stats::median(fm[, 2])))
  imports <- setdiff(trimws(strsplit(utils::packageDescription("FactoMineR")$Imports, ",")[[1]]),
                     c("graphics", "grDevices", "stats", "utils"))
  res <- t(vapply(imports, function(x) {
    r <- t(vapply(1:3, function(i) child(x), numeric(2)))
    c(stats::median(r[, 1]), stats::median(r[, 2]))
  }, numeric(2)))
  res <- res[order(-res[, 1]), , drop = FALSE]
  say("\nEach FactoMineR import alone, after the same baseline (median of 3):")
  for (x in rownames(res)) say("  %-14s %.3f s  %3d namespaces", x, res[x, 1], as.integer(res[x, 2]))
  sets <- list("without car, emmeans, multcompView" = setdiff(imports, c("car", "emmeans",
                                                                         "multcompView", "ggtext")),
               "core only" = c("cluster", "ggplot2", "ggrepel", "irlba", "lattice", "MASS", "scales"))
  for (s in names(sets)) {
    code <- sprintf(paste0("invisible(lapply(c(%s), loadNamespace)); ",
                           "cat(system.time(for (p in c(%s)) loadNamespace(p))[['elapsed']])"),
                    paste0("'", base, "'", collapse = ", "), paste0("'", sets[[s]], "'", collapse = ", "))
    t <- vapply(seq_len(runs), function(i) as.numeric(utils::tail(system2(
      "Rscript", c("-e", shQuote(code)), stdout = TRUE, stderr = FALSE), 1)), numeric(1))
    say("FactoMineR's imports, %s: median %.2f s", s, stats::median(t))
  }
}

# === SECTION: The answer profiles ============================================================

# The distinct answer profiles of the active variables X (factors): the unit every engine below
# computes on. `key` maps each individual to its profile; `codes` are each profile's columns in the
# indicator table (levels in variable order, as FactoMineR's tab.disjonctif() lays them out).
profile_table <- function(X, w = NULL) {
  X   <- as.data.frame(X)
  w   <- if (is.null(w)) rep(1, nrow(X)) else as.numeric(w)
  key <- as.integer(vctrs::vec_group_id(X))
  first <- which(!duplicated(key))
  nlev  <- vapply(X, nlevels, integer(1))
  off   <- c(0L, cumsum(nlev)[-length(nlev)])
  codes <- vapply(seq_along(X), function(q) as.integer(X[[q]][first]) + off[q],
                  integer(length(first)))
  list(key = key, first = first, codes = matrix(codes, length(first)),
       w = as.vector(rowsum(w, key)), count = tabulate(key, length(first)), W = sum(w),
       n = nrow(X), Q = ncol(X), K = sum(nlev), var = rep(names(X), nlev),
       level = unlist(lapply(X, levels), use.names = FALSE))
}

# FactoMineR's level names: a level two variables share becomes `var_lv` (MCA), then y/n/Y/N
# becomes `var.y` (tab.disjonctif(), when there are several variables).
fm_level_names <- function(X) {
  lv  <- lapply(X, levels)
  dup <- anyDuplicated(unlist(lv))
  if (dup) lv <- Map(function(l, v) if (sum(unlist(lv) %in% l) != length(l)) paste(v, l, sep = "_")
                     else l, lv, names(X))
  out <- unlist(lv, use.names = FALSE)
  yn  <- out %in% c("y", "n", "Y", "N") & ncol(X) > 1
  out[yn] <- paste(rep(names(X), lengths(lv))[yn], out[yn], sep = ".")
  out
}

# === SECTION: The engines ====================================================================

# svd.triplet()'s full-SVD branches: the SVD of diag(sqrt(r)) X diag(sqrt(c)), each axis signed so
# that its column singular vector sums positive -- except at ncp == 1 when ncol < nrow, where
# FactoMineR keeps LAPACK's sign.
svd_triplet <- function(X, row.w, col.w, ncp = Inf) {
  ncp   <- min(ncp, nrow(X) - 1, ncol(X))
  row.w <- row.w / sum(row.w)
  X <- t(t(X) * sqrt(col.w)) * sqrt(row.w)
  if (ncol(X) < nrow(X)) {
    s <- svd(X, nu = ncp, nv = ncp); U <- s$u; V <- s$v; signed <- ncp > 1
  } else {
    s <- svd(t(X), nu = ncp, nv = ncp); U <- s$v; V <- s$u; signed <- TRUE
  }
  sign_axes(list(vs = s$d[seq_len(min(ncol(X), nrow(X) - 1, ncp))], U = U, V = V), signed,
            row.w, col.w)
}

sign_axes <- function(s, signed, row.w, col.w) {
  if (signed) {
    m <- sign(colSums(s$V)); m[m == 0] <- 1
    s$U <- t(t(s$U) * m); s$V <- t(t(s$V) * m)
  }
  s$U <- s$U / sqrt(row.w); s$V <- s$V / sqrt(col.w)
  tiny <- which(s$vs < 1e-15)
  if (length(tiny)) {
    s$U[, tiny] <- t(t(s$U[, tiny, drop = FALSE]) * s$vs[tiny])
    s$V[, tiny] <- t(t(s$V[, tiny, drop = FALSE]) * s$vs[tiny])
  }
  s
}

# The MCA as FactoMineR computes it -- the CA of the indicator table -- on the profile table.
mca_dense <- function(pt, excl = integer()) {
  P <- nrow(pt$codes)
  Z <- matrix(0, P, pt$K)
  Z[cbind(rep(seq_len(P), pt$Q), as.vector(pt$codes))] <- 1
  F  <- Z * (pt$w / (pt$Q * pt$W))
  cm <- colSums(F); rm <- rowSums(F)
  Tc <- F / outer(rm, cm) - 1
  cs <- cm; cs[excl] <- 1e-15
  s  <- svd_triplet(Tc, rm, cs, min(pt$K - pt$Q, P - 1, pt$K - 1))
  mca_slots(pt, cm, excl, s, coord_row = t(t(s$U) * s$vs))
}

# B = Z'WZ, the weighted Burt table (K x K), from the profiles' level codes: never an n x K matrix.
burt_table <- function(pt) {
  i  <- rep(seq_len(nrow(pt$codes)), pt$Q)
  Zs <- Matrix::sparseMatrix(i = i, j = as.vector(pt$codes), x = 1, dims = c(nrow(pt$codes), pt$K))
  as.matrix(Matrix::crossprod(Zs, Zs * pt$w))
}

# The same table in base R, one rowsum() per variable: no Matrix namespace to load (0.5 s).
burt_table_base <- function(pt) {
  K <- pt$K; B <- numeric(K * K)
  wq <- rep(pt$w, pt$Q)
  for (a in seq_len(pt$Q)) {
    agg <- rowsum(wq, (as.vector(pt$codes) - 1L) * K + pt$codes[, a])
    B[as.integer(rownames(agg))] <- agg
  }
  matrix(B, K, K)
}

# The same MCA from the Burt table: its eigenvectors are the column side of the SVD, and each
# profile's coordinate is the transition formula -- the mean of its levels' rescaled standard
# coordinates, recentred when `excl` moves the centre.
mca_burt <- function(pt, excl = integer(), B = burt_table(pt), axes = Inf) {
  Q <- pt$Q; K <- pt$K; P <- nrow(pt$codes)
  cm <- diag(B) / (Q * pt$W)
  cs <- cm; cs[excl] <- 1e-15
  S  <- (B / (Q^2 * pt$W * outer(cm, cm)) - 1) * outer(sqrt(cs), sqrt(cs))
  ncp <- min(K - Q, P - 1, K - 1)
  e  <- eigen(S, symmetric = TRUE)
  s  <- sign_axes(list(vs = sqrt(pmax(e$values[seq_len(ncp)], 0)), U = matrix(0, 0, ncp),
                       V = e$vectors[, seq_len(ncp), drop = FALSE]),
                  signed = !(ncp == 1 && K < P), row.w = 1, col.w = cs)
  a  <- seq_len(min(axes, ncp))
  A  <- s$V[, a, drop = FALSE] * (cs / cm)
  coord_row <- Reduce(`+`, lapply(seq_len(Q), function(q) A[pt$codes[, q], , drop = FALSE])) / Q
  coord_row <- sweep(coord_row, 2, colSums(cs * s$V[, a, drop = FALSE]))
  if (is.finite(axes)) return(list(eig = s$vs^2, V = s$V, cm = cm, cs = cs, coord = coord_row))
  s$U <- t(t(coord_row) / s$vs)
  mca_slots(pt, cm, excl, s, coord_row)
}

# FactoMineR's MCA slots from the column masses and the SVD, in closed form: nothing n x K. The
# rows are the profiles; expand_ind() gives back the individuals.
mca_slots <- function(pt, cm, excl, s, coord_row) {
  Q <- pt$Q; W <- pt$W
  eig <- s$vs^2
  d2c <- 1 / (Q * cm) - 1
  c0  <- cm; c0[excl] <- 0
  coord   <- t(t(s$V) * s$vs)
  contrib <- t(t(coord^2 * c0) / eig) * 100
  Nj  <- cm * Q * W
  eta2 <- rowsum(contrib / 100, factor(pt$var, unique(pt$var)))
  eta2 <- if (Q > 1) t(t(eta2) * eig) * Q else eta2 * 0 + 1
  keep <- !seq_len(pt$K) %in% excl
  inv  <- ifelse(keep, 1 / cm, 0)
  m    <- rowSums(matrix(keep[pt$codes], nrow(pt$codes)))
  d2r  <- sum(c0) - 2 * m / Q + rowSums(matrix(inv[pt$codes], nrow(pt$codes))) / Q^2
  r    <- pt$w / W
  pct  <- eig / sum(c0 * d2c) * 100
  n_eig <- if (Q > 1) min(length(eig), pt$n - 1, pt$K - Q) else pt$K - 1
  eigt <- cbind(eigenvalue = eig, "percentage of variance" = pct,
                "cumulative percentage of variance" = cumsum(pct))[seq_len(n_eig), , drop = FALSE]
  var <- list(coord = coord, contrib = contrib, cos2 = coord^2 / d2c,
              v.test = coord * (if (W > 1) sqrt(Nj * (W - 1) / (W - Nj)) else sqrt(Nj)), eta2 = eta2)
  if (length(excl)) {
    eigt <- modif_rate(eigt, Q)
    var  <- c(lapply(var[1:4], function(M) M[keep, , drop = FALSE]), var[5])
  }
  list(eig = eigt, var = var,
       ind = list(coord = coord_row, contrib = t(t(coord_row^2 * r) / eig) * 100,
                  cos2 = coord_row^2 / d2r),
       svd = s, cm = cm, cs = replace(cm, excl, 1e-15), excl = excl)
}

# FactoMineR's modif.rate(): Benzecri's rates, over the eigenvalues above 1/Q, ROUNDED to 2 digits.
modif_rate <- function(eig, Q) {
  tab <- cbind.data.frame(eig, 0, 100)
  big <- eig[eig[, 1] >= 1 / Q, 1]
  pseudo <- (Q / (Q - 1) * (big - 1 / Q))^2
  tab[seq_along(big), 4] <- round(pseudo / sum(pseudo) * 100, 2)
  tab[, 5] <- cumsum(tab[, 4])
  colnames(tab)[4:5] <- c("modified rates", "cumulative modified rates")
  tab
}

# A profile's individuals share its coordinate and cos2; a contribution is split by weight.
expand_ind <- function(fit, pt, w = rep(1, pt$n)) {
  i <- pt$key
  list(coord = fit$ind$coord[i, , drop = FALSE], cos2 = fit$ind$cos2[i, , drop = FALSE],
       contrib = fit$ind$contrib[i, , drop = FALSE] * (w / pt$w[i]))
}

# FactoMineR's CA, with supplementary rows and columns: the reference replication, not a design.
ca_native <- function(X, row.sup = NULL, col.sup = NULL) {
  Xa <- X[setdiff(seq_len(nrow(X)), row.sup), setdiff(seq_len(ncol(X)), col.sup), drop = FALSE]
  Xa <- Xa[rowSums(Xa) != 0, colSums(Xa) != 0, drop = FALSE]
  F  <- Xa / sum(Xa)
  cm <- colSums(F); rm <- rowSums(F)
  Tc <- F / outer(rm, cm) - 1
  s  <- svd_triplet(Tc, rm, cm, min(nrow(Xa) - 1, ncol(Xa) - 1))
  eig <- s$vs^2
  coord_c <- t(t(s$V) * s$vs); coord_r <- t(t(s$U) * s$vs)
  out <- list(eig = eig, pct = eig / sum(rm * Tc^2 %*% cm) * 100,
              col = list(coord = coord_c, contrib = t(t(coord_c^2 * cm) / eig) * 100,
                         cos2 = coord_c^2 / colSums(Tc^2 * rm)),
              row = list(coord = coord_r, contrib = t(t(coord_r^2 * rm) / eig) * 100,
                         cos2 = coord_r^2 / drop(Tc^2 %*% cm)))
  if (length(row.sup)) {
    Xs <- X[row.sup, setdiff(seq_len(ncol(X)), col.sup), drop = FALSE]
    out$row.sup <- list(coord = (Xs / rowSums(Xs)) %*% s$V)
  }
  if (length(col.sup)) {
    Xs <- X[setdiff(seq_len(nrow(X)), row.sup), col.sup, drop = FALSE]
    out$col.sup <- list(coord = t(t(Xs) / colSums(Xs)) |> crossprod(s$U))
  }
  out
}

# FactoMineR's PCA: weighted centring, weighted population sd, column weights in the metric.
pca_native <- function(X, scale.unit = TRUE, row.w = NULL, col.w = NULL, ind.sup = NULL) {
  X <- as.matrix(X)
  for (j in which(colSums(is.na(X)) > 0)) X[is.na(X[, j]), j] <- mean(X[, j], na.rm = TRUE)
  Xs <- X[ind.sup, , drop = FALSE]
  if (length(ind.sup)) X <- X[-ind.sup, , drop = FALSE]
  r  <- if (is.null(row.w)) rep(1, nrow(X)) else row.w
  r  <- r / sum(r)
  cw <- if (is.null(col.w)) rep(1, ncol(X)) else col.w
  centre <- drop(crossprod(r, X))
  X  <- t(t(X) - centre)
  sd <- if (scale.unit) drop(sqrt(crossprod(r, X^2))) else rep(1, ncol(X))
  sd[sd <= 1e-16] <- 1
  X  <- t(t(X) / sd)
  s  <- svd_triplet(X, r, cw, min(nrow(X) - 1, ncol(X)))
  eig <- s$vs^2
  coord_v <- t(t(s$V) * s$vs); coord_i <- t(t(s$U) * s$vs)
  out <- list(eig = eig, pct = eig / drop(r %*% (X^2 %*% cw)) * 100,
              var = list(coord = coord_v, contrib = sweep(coord_v^2 * cw, 2, eig, "/") * 100,
                         cos2 = coord_v^2 / colSums(X * (r * X))),
              ind = list(coord = coord_i, contrib = t(t(coord_i^2 * r) / eig) * 100,
                         cos2 = coord_i^2 / drop(X^2 %*% cw)),
              centre = centre, ecart.type = sd, V = s$V)
  if (length(ind.sup)) out$ind.sup <- list(coord = (t(t(Xs) - centre) / rep(sd, each = nrow(Xs)))
                                           %*% (s$V * cw))
  out
}

# === SECTION: parity -- the engines against FactoMineR =======================================

# Largest difference of `b` from the reference `a`, relative to a's largest absolute value, on the
# axes that are neither null (ghost axes of `excl`) nor tied with a neighbour.
# A level every individual chose sits at the centre: its cos2 and v.test are undefined, rounding
# noise in FactoMineR and Inf/NaN in closed form. Such cells are left out.
cmp <- function(a, b, axes, free = integer()) {
  a <- as.matrix(a)[, axes, drop = FALSE]; b <- as.matrix(b)[, axes, drop = FALSE]
  ok <- is.finite(a) & is.finite(b) & abs(a) < 1e12
  if (!any(ok)) return(NA_real_)
  flip <- axes %in% free & colSums(a * b * ok, na.rm = TRUE) < 0
  b[, flip] <- -b[, flip]
  max(abs(a - b)[ok]) / max(abs(a[ok]), 1e-300)
}

sound_axes <- function(eig) {
  ok  <- eig > 1e-12 * eig[1]
  gap <- pmin(abs(diff(c(Inf, eig))), abs(diff(c(eig, -Inf))))
  which(ok & gap > 1e-8 * eig[1])
}

parity_row <- function(id, fm, nat, pt, w, what = "dense") {
  eig_fm <- as.matrix(fm$eig)
  ax <- sound_axes(eig_fm[, 1])
  ax <- ax[ax <= ncol(fm$var$coord)]
  ind <- expand_ind(nat, pt, w)
  # WARNING: the sign rule reads the sum of an axis' unit column vector; when that sum is zero in
  #   exact arithmetic (a variable mirroring another), rounding decides it, so such axes are
  #   compared up to their sign.
  free <- which(abs(colSums(nat$svd$V * sqrt(nat$cs))) < 1e-8)
  d <- c(eig = max(abs(eig_fm[, 1:3] - as.matrix(nat$eig)[, 1:3])),
         var_coord = cmp(fm$var$coord, nat$var$coord, ax, free),
         var_contrib = cmp(fm$var$contrib, nat$var$contrib, ax),
         var_cos2 = cmp(fm$var$cos2, nat$var$cos2, ax),
         v.test = cmp(fm$var$v.test, nat$var$v.test, ax, free),
         eta2 = cmp(fm$var$eta2, nat$var$eta2, ax),
         ind_coord = cmp(fm$ind$coord, ind$coord, ax, free),
         ind_contrib = cmp(fm$ind$contrib, ind$contrib, ax),
         ind_cos2 = cmp(fm$ind$cos2, ind$cos2, ax),
         vs = max(abs(fm$svd$vs[ax] - nat$svd$vs[ax])))
  if (ncol(eig_fm) == 5) d["mrate"] <- max(abs(eig_fm[, 4:5] - as.matrix(nat$eig)[, 4:5]))
  flips <- sum(colSums(fm$var$coord[, ax, drop = FALSE] * nat$var$coord[, ax, drop = FALSE]) < 0)
  say("%-34s %-5s n %6d P %6d K %3d axes %2d/%2d  max rel %.1e  eig %.1e  flips %d (sign-free %d)",
      id, what, pt$n, nrow(pt$codes), pt$K, length(ax), ncol(fm$var$coord),
      max(d[-1], na.rm = TRUE), d[["eig"]], flips, sum(free %in% ax))
  invisible(d)
}

# One MCA case: ggfacto's own ingress (na_levels(), excl_index(), FactoMineR::MCA) against the
# dense and Burt engines on the profiles.
mca_case <- function(id, data, vars, wt = NULL, excl = NA) {
  X  <- na_levels(as.data.frame(data[vars]), vars)
  w  <- if (is.null(wt)) NULL else data[[wt]]
  ex <- excl_index(X, vars, excl)
  fm <- FactoMineR::MCA(X, ncp = Inf, row.w = w, graph = FALSE, excl = ex)
  pt <- profile_table(X, w)
  wi <- if (is.null(w)) rep(1, nrow(X)) else w
  names_ok <- identical(rownames(fm$var$coord), fm_level_names(X)[setdiff(seq_len(pt$K), ex)])
  a <- parity_row(id, fm, mca_dense(pt, ex), pt, wi, "dense")
  b <- parity_row(id, fm, mca_burt(pt, ex), pt, wi, "burt")
  fp <- FactoMineR::MCA(X[pt$first, ], ncp = Inf, row.w = pt$w, graph = FALSE, excl = ex)
  fp <- c(fp[c("eig", "var", "ind", "svd")],
          list(cs = replace(unname(fp$call$marge.col), ex, 1e-15)))
  parity_row(id, fm, fp, pt, wi, "FMpro")
  say("%-34s names %s", "", if (names_ok) "identical" else "DIFFER")
  invisible(list(dense = a, burt = b))
}

parity_data <- function() {
  e <- new.env(); utils::data("tea", package = "FactoMineR", envir = e); tea <- e$tea
  tea_na <- tea; tea_na$breakfast[1:20] <- NA; tea_na$lunch[10:40] <- NA
  tea_w  <- tea; tea_w$w <- withr::with_seed(1, stats::runif(nrow(tea), 0.2, 3))
  syn <- data.frame(a = factor(c("yes", "no", "yes", "no", "yes", "no")),
                    b = factor(c("yes", "yes", "no", "no", "yes", "no")),
                    c = factor(c("y", "n", "y", "y", "n", "n")),
                    d = factor(c("x", "z", "z", "x", "z", "x")))
  syn <- syn[rep(1:6, 20), ]; syn$d[c(3, 50, 77)] <- "x"
  list(tea = tea, tea_na = tea_na, tea_w = tea_w, syn = syn,
       pc = if (dir.exists(course)) readRDS(file.path(course, "M2S1_pc_AGD.rds")))
}

section_parity <- function() {
  load_pkg()
  d <- parity_data()
  v18 <- names(d$tea)[1:18]
  say("== MCA: FactoMineR::MCA through ggfacto's ingress, against the engines on profiles")
  mca_case("P1 tea[1:18]", d$tea, v18)
  mca_case("P2 tea[1:18] NA, excl = NA", d$tea_na, v18)
  mca_case("P3 tea weighted, 2 levels excl", d$tea_w, v18, "w", c("breakfast", "green"))
  if (!is.null(d$pc)) {
    mca_case("P4 pc_AGD POND", d$pc, pc_vars, "POND", excl = NULL)
    X <- na_levels(as.data.frame(d$pc[pc_vars]), pc_vars)
    rare <- names(which.min(unlist(lapply(X, function(x) table(x)))))
    mca_case("P5 pc_AGD POND, rarest excl", d$pc, pc_vars, "POND", sub("^[^.]+\\.", "", rare))
  }
  mca_case("P6 shared level + y/n", d$syn, names(d$syn))
  mca_case("P7a tea[1:10, 1:18] (n < K)", d$tea[1:10, ], v18)
  mca_case("P7b all-binary tea[1:6]", d$tea, names(d$tea)[1:6])
  mca_case("P8a 2 binary variables", d$tea, names(d$tea)[1:2])
  tryCatch(mca_case("P8b 1 binary variable", d$tea, names(d$tea)[1]),
           error = function(e) say("P8b 1 binary variable: %s", conditionMessage(e)))
  if (file.exists(ee_path)) {
    ee <- ee_data(employed = FALSE)
    ee <- ee[ee$ANNEE == 2018, ]
    for (n in c(1e4, 1e5)) {
      s <- ee[withr::with_seed(1, sample(nrow(ee), n)), ]
      mca_case(sprintf("P13 EE 2018 n=%g, excl = NA", n), s, ee_q10, "EXTRI")
    }
  }
  section_parity_ca_pca()
}

section_parity_ca_pca <- function() {
  say("\n== CA: FactoMineR::CA against ca_native()")
  gss <- forcats::gss_cat |>
    dplyr::filter(!relig %in% c("No answer", "Don't know", "Not applicable"),
                  !partyid %in% c("No answer", "Don't know", "Other party")) |>
    droplevels()
  tabs <- list("P10 gss relig x partyid" = unclass(table(gss$relig, gss$partyid)),
               "P11 2 columns, 5 rows" = unclass(table(gss$relig, gss$year > 2006))[1:5, ],
               "P11 control: 2 rows" = unclass(table(gss$year > 2006, gss$partyid)))
  for (id in names(tabs)) {
    X  <- tabs[[id]]
    fm <- FactoMineR::CA(X, ncp = Inf, graph = FALSE)
    nt <- ca_native(X)
    ax <- sound_axes(fm$eig[, 1])
    say("%-30s eig %.1e  row %.1e  col %.1e  contrib %.1e  cos2 %.1e", id,
        max(abs(fm$eig[, 1] - nt$eig)), cmp(fm$row$coord, nt$row$coord, ax),
        cmp(fm$col$coord, nt$col$coord, ax), cmp(fm$col$contrib, nt$col$contrib, ax),
        cmp(fm$row$cos2, nt$row$cos2, ax))
  }
  X  <- tabs[[1]]; X0 <- rbind(X, zero = 0)
  fm <- suppressWarnings(FactoMineR::CA(X0, ncp = Inf, graph = FALSE))
  nt <- ca_native(X0)
  say("%-30s eig %.1e  row %.1e", "P12a a zero row, dropped", max(abs(fm$eig[, 1] - nt$eig)),
      cmp(fm$row$coord, nt$row$coord, sound_axes(fm$eig[, 1])))
  fm <- FactoMineR::CA(X, ncp = Inf, graph = FALSE, row.sup = 2, col.sup = 3)
  nt <- ca_native(X, row.sup = 2, col.sup = 3)
  ax <- sound_axes(fm$eig[, 1])
  say("%-30s eig %.1e  row.sup %.1e  col.sup %.1e", "P12b row.sup, col.sup",
      max(abs(fm$eig[, 1] - nt$eig)), cmp(fm$row.sup$coord, nt$row.sup$coord, ax),
      cmp(fm$col.sup$coord, nt$col.sup$coord, ax))
  say("%-30s FactoMineR: %s", "P12c a zero row and col.sup", tryCatch({
    suppressWarnings(FactoMineR::CA(X0, graph = FALSE, col.sup = 3)); "ok"
  }, error = function(e) conditionMessage(e)))

  say("\n== PCA: FactoMineR::PCA against pca_native(), mtcars[1:7], 16 combinations + NA + constant")
  d <- mtcars[1:7]
  worst <- 0
  for (su in c(TRUE, FALSE)) for (rw in c(FALSE, TRUE)) for (cw in c(FALSE, TRUE))
    for (is in c(FALSE, TRUE)) {
      r  <- if (rw) seq(0.5, 2, length.out = if (is) 29 else 32)
      c  <- if (cw) c(2, 1, 1, 0.5, 1, 1, 3)
      i  <- if (is) 30:32
      fm <- FactoMineR::PCA(d, scale.unit = su, ncp = Inf, row.w = r, col.w = c, ind.sup = i,
                            graph = FALSE)
      nt <- pca_native(d, su, r, c, i)
      ax <- sound_axes(fm$eig[, 1])
      dd <- c(max(abs(fm$eig[, 1] - nt$eig)), cmp(fm$var$coord, nt$var$coord, ax),
              cmp(fm$ind$coord, nt$ind$coord, ax), cmp(fm$var$contrib, nt$var$contrib, ax),
              cmp(fm$ind$cos2, nt$ind$cos2, ax),
              if (is) cmp(fm$ind.sup$coord, nt$ind.sup$coord, ax) else 0)
      worst <- max(worst, dd)
    }
  say("worst over the 16: %.1e", worst)
  dn <- d; dn$mpg[c(2, 9)] <- NA; dn$const <- 1
  fm <- suppressWarnings(FactoMineR::PCA(dn, ncp = Inf, graph = FALSE))
  nt <- pca_native(dn)
  ax <- sound_axes(fm$eig[, 1])
  say("NA imputed + constant column:  eig %.1e  var %.1e  ind %.1e",
      max(abs(fm$eig[, 1] - nt$eig)), cmp(fm$var$coord, nt$var$coord, ax),
      cmp(fm$ind$coord, nt$ind$coord, ax))
}

# === SECTION: extract -- the Enquete Emploi, read once ======================================

# 2009-2018, the columns the experiments use, factors with explicit levels; the rotating panel's
# person id is kept as an integer. Benchmark processes read this .rds and never load arrow.
section_extract <- function() {
  Sys.setenv(ARROW_DEFAULT_MEMORY_POOL = "system")
  cols <- c("ANNEE", "IDENT", "NOI", "EXTRI", "ACTEU", "AGE", setdiff(ee_q15, "AGE5"))
  ee <- arrow::open_dataset("~/Data/Enquête Emploi Parquet") |>
    dplyr::filter(ANNEE >= 2009, ANNEE <= 2018) |>
    dplyr::select(tidyselect::all_of(cols)) |>
    dplyr::collect()
  ee$person <- as.integer(vctrs::vec_group_id(ee[c("IDENT", "NOI")]))
  ee$IDENT <- ee$NOI <- NULL
  ee$AGE5 <- cut(as.numeric(as.character(ee$AGE)), c(-Inf, 24, 34, 44, 54, Inf),
                 c("15-24", "25-34", "35-44", "45-54", "55+"))
  fac <- setdiff(names(ee), c("ANNEE", "EXTRI", "AGE", "person"))
  ee[fac] <- lapply(ee[fac], function(x) factor(as.character(x), sort(unique(as.character(x)))))
  ee$EXTRI[is.na(ee$EXTRI)] <- 0
  ee <- as.data.frame(ee)
  saveRDS(ee, ee_path)
  say("%d rows, %d persons, years %s; employed %d", nrow(ee), max(ee$person),
      paste(range(ee$ANNEE), collapse = "-"), sum(ee$ACTEU == "1-Actif occupe", na.rm = TRUE))
  na <- vapply(ee[ee_q15], function(x) mean(is.na(x)), numeric(1))
  say("share missing: %s", paste(sprintf("%s %.0f%%", names(na), 100 * na), collapse = ", "))
}

ee_data <- function(employed = TRUE) {
  ee <- readRDS(ee_path)
  if (employed) ee <- ee[ee$ACTEU %in% "1-Actif occupe", ]
  ee[ee$EXTRI > 0, ]
}

# === SECTION: compression -- how many distinct answer profiles real data has ================

section_compression <- function() {
  load_pkg()
  ee_all <- ee_data(employed = FALSE)
  sets <- list(Q5 = ee_q5, Q8 = ee_q8, Q10 = ee_q10, Q15 = ee_q15)
  count_profiles <- function(d, vars) {
    X <- na_levels(d[vars], vars)
    length(unique(vctrs::vec_group_id(X)))
  }
  for (pop in c("everyone", "employed")) {
    ee <- if (pop == "employed") ee_all[ee_all$ACTEU %in% "1-Actif occupe", ] else ee_all
    y18 <- ee[ee$ANNEE == 2018, ]
    samples <- list(
      "1e4 (2018)" = y18[withr::with_seed(1, sample(nrow(y18), 1e4)), ],
      "3e4 (2018)" = y18[withr::with_seed(1, sample(nrow(y18), 3e4)), ],
      "1e5 (2018)" = y18[withr::with_seed(1, sample(nrow(y18), 1e5)), ],
      "2018" = y18, "2016-2018" = ee[ee$ANNEE >= 2016, ], "2009-2018" = ee)
    persons <- ee[!duplicated(ee[c("person", "ANNEE")]), ]
    samples[["2009-2018, 1 row/person-year"]] <- persons
    say("\n== %s: distinct profiles P (P/n)", pop)
    say("%-30s %9s %s", "rows", "n", paste(sprintf("%16s", names(sets)), collapse = ""))
    res <- t(vapply(samples, function(d) vapply(sets, function(v) count_profiles(d, v),
                                                numeric(1)), numeric(length(sets))))
    for (s in names(samples)) {
      n <- nrow(samples[[s]])
      say("%-30s %9d %s", s, n, paste(sprintf("%8d (%5.3f)", res[s, ], res[s, ] / n), collapse = ""))
    }
    ns <- vapply(samples[1:6], nrow, numeric(1))
    slope <- vapply(seq_along(sets), function(j) stats::coef(stats::lm(log(res[1:6, j]) ~ log(ns)))[2],
                    numeric(1))
    say("log-log slope of P on n (1e4 to 10 years): %s",
        paste(sprintf("%s %.2f", names(sets), slope), collapse = ", "))
  }
}

# === SECTION: map -- keeping individuals and profiles aligned ================================

# The individual -> profile map at scale: grouping (fit time), matching the data frame against the
# stored profiles (every later call), and hashing (a cheap stale-data check).
section_map <- function() {
  load_pkg()
  ee <- ee_data()
  for (vars in list(ee_q10, ee_q15)) for (n in c(1e5, 5e5, 1e6)) {
    d <- ee[withr::with_seed(1, sample(nrow(ee), n)), ]
    X <- na_levels(d[vars], vars)
    t_group <- timed(key <- vctrs::vec_group_id(X))
    prof <- X[!duplicated(key), ]
    t_match <- timed(m <- vctrs::vec_match(X, prof))
    t_hash  <- timed(rlang::hash(X))
    t_prep  <- timed(na_levels(d[vars], vars), times = 1)
    stopifnot(identical(as.integer(key), m))
    say("Q%-2d n %7d  P %7d  group %.3f s  match %.3f s  hash %.3f s  (na_levels %.2f s)",
        length(vars), n, nrow(prof), t_group, t_match, t_hash, t_prep)
  }
}

# === SECTION: sup -- a supplementary level from the profile model ===========================

# A supplementary level is the weighted mean of its individuals' coordinates over sqrt(eigenvalue):
# the profile model needs each individual's profile and weight, nothing n x K.
sup_coord <- function(fit, pt, s, w) {
  f <- fit$ind$coord[pt$key, , drop = FALSE]
  t(t(rowsum(w * f, s) / as.vector(rowsum(w, s))) / fit$svd$vs)
}

# The same from the crosstab of the supplementary levels with the active ones -- the table the
# tooltips show: O(nQ) once, then K-sized, and no individual coordinate is ever gathered.
sup_coord_xtab <- function(fit, pt, s, w) {
  L  <- nlevels(s); K <- pt$K
  Wl <- matrix(0, L, K)
  for (q in seq_len(pt$Q)) {
    cell <- (as.integer(s) - 1L) * K + pt$codes[pt$key, q]
    agg  <- rowsum(w, cell)
    Wl[as.integer(rownames(agg))] <- Wl[as.integer(rownames(agg))] + agg
  }
  Wl <- t(matrix(Wl, K, L, byrow = FALSE))
  A  <- fit$svd$V * (fit$cs / fit$cm)
  g  <- (Wl / rowSums(Wl)) %*% A
  g  <- sweep(g, 2, colSums(fit$cs * fit$svd$V))
  structure(t(t(g) / fit$svd$vs), dimnames = list(levels(s), NULL))
}

section_sup <- function() {
  load_pkg()
  pc <- readRDS(file.path(course, "M2S1_pc_AGD.rds"))
  sup <- c("SEXE", "CRITAGE", "DIPLOM", "CSTOTR")
  X  <- na_levels(as.data.frame(pc[pc_vars]), pc_vars)
  res <- multiple_correspondence_analysis(pc, tidyselect::all_of(pc_vars), wt = POND)
  pt  <- profile_table(X, pc$POND)
  nat <- mca_burt(pt)
  Xs  <- na_levels(as.data.frame(pc[c(pc_vars, sup)]), c(pc_vars, sup))
  fm  <- FactoMineR::MCA(Xs, ncp = Inf, row.w = pc$POND, graph = FALSE,
                         quali.sup = length(pc_vars) + seq_along(sup))
  for (v in sup) {
    a <- varsup(res, pc[[v]])$coord
    b <- sup_coord(nat, pt, Xs[[v]], pc$POND)
    c <- fm$quali.sup$coord[paste0(if (anyDuplicated(unlist(lapply(Xs, levels)))) paste0(v, "_"),
                                   levels(Xs[[v]])), , drop = FALSE]
    x <- sup_coord_xtab(nat, pt, Xs[[v]], pc$POND)
    say("%-9s levels %2d  native vs varsup() %.1e  vs MCA(quali.sup) %.1e  crosstab route %.1e",
        v, nrow(b), max(abs(a - b[rownames(a), seq_len(ncol(a))])),
        max(abs(unname(c) - unname(b[, seq_len(ncol(c))]))), max(abs(x - b)))
  }
  ee <- ee_data()
  d  <- ee[withr::with_seed(1, sample(nrow(ee), 1e6)), ]
  X  <- na_levels(d[ee_q10], ee_q10)
  pt <- profile_table(X, d$EXTRI)
  nat <- mca_burt(pt)
  s  <- na_levels(d["AGE5"], "AGE5")$AGE5
  say("EE 1e6 rows, Q10, %d axes: a %d-level supplementary variable by gathering coordinates %.3f s,
by the crosstab %.3f s", length(nat$svd$vs), nlevels(s), timed(sup_coord(nat, pt, s, d$EXTRI)),
      timed(sup_coord_xtab(nat, pt, s, d$EXTRI)))
}

# === SECTION: bench -- one engine, one size, one process ====================================

# engine: fm_ind (ggfacto today), fm_ind5 (ncp = 5: irlba), fm_prof (FactoMineR fed the
# profiles), fm_prof5 (the same, its profiles kept on 5 axes), dense, burt, lean (burt: every
# eigenvalue, the profiles' coordinates on 5 axes only), fm_svd (lean, with FactoMineR's own
# svd.triplet()), none (the baseline: data prepared, nothing fitted). data: pc, or ee (the
# employed of 2009-2018, q = 10 or 15 questions, EXTRI), sampled to n rows ("all": 1.87 million).
section_bench <- function(engine, data = "ee", n = "1e5", q = "10") {
  load_pkg()
  if (data == "pc") {
    d <- readRDS(file.path(course, "M2S1_pc_AGD.rds")); vars <- pc_vars; wt <- "POND"
  } else {
    d <- ee_data(); vars <- get(paste0("ee_q", q)); wt <- "EXTRI"
    if (n != "all") d <- d[withr::with_seed(1, sample(nrow(d), as.numeric(n))), ]
  }
  prep <- function(d) {
    X <- na_levels(as.data.frame(d[vars]), vars)
    list(X = X, w = d[[wt]], ex = excl_index(X, vars, NA))
  }
  fit <- function(p) with(p, switch(engine,
    none    = NULL,
    fm_ind  = FactoMineR::MCA(X, ncp = Inf, row.w = w, excl = ex, graph = FALSE),
    fm_ind5 = FactoMineR::MCA(X, ncp = 5, row.w = w, graph = FALSE),
    fm_prof = {
      pt <- profile_table(X, w)
      r  <- FactoMineR::MCA(X[pt$first, ], ncp = Inf, row.w = pt$w, excl = ex, graph = FALSE)
      list(fit = r, key = pt$key)
    },
    dense = { pt <- profile_table(X, w); list(fit = mca_dense(pt, ex), key = pt$key) },
    burt  = { pt <- profile_table(X, w); list(fit = mca_burt(pt, ex), key = pt$key) },
    lean  = { pt <- profile_table(X, w); list(fit = mca_burt(pt, ex, axes = 5), key = pt$key) },
    fm_svd = { pt <- profile_table(X, w); list(fit = mca_fm_burt(pt, ex, axes = 5), key = pt$key) },
    fm_prof5 = {
      pt <- profile_table(X, w)
      r  <- FactoMineR::MCA(X[pt$first, ], ncp = Inf, row.w = pt$w, excl = ex, graph = FALSE)
      list(fit = axes_kept(r, 5, var = FALSE), key = pt$key)
    }))
  invisible(fit(prep(d[seq_len(min(1000, nrow(d))), ])))
  t_prep <- system.time(p <- prep(d))[["elapsed"]]
  rm(d); gc()
  el <- system.time(v <- fit(p))[["elapsed"]]
  rm(v)
  pk <- peak_mb(fit(p))
  say("BENCH engine=%s data=%s n=%d P=%d K=%d prep=%.2f fit=%.2f heap_peak_mb=%.0f object_mb=%.1f",
      engine, data, nrow(p$X), length(unique(vctrs::vec_group_id(p$X))),
      sum(vapply(p$X, nlevels, integer(1))), t_prep, el, pk$mb,
      as.numeric(utils::object.size(pk$value)) / 1e6)
}

# === SECTION: object -- what the ecosystem reads =============================================

# The same fitted MCA under three class orders, through factoextra, explor, GDAtools and print().
# A tidy object of ggfacto's own (O3) would fail every one of them by construction.
section_object <- function() {
  load_pkg()
  grDevices::pdf(tempfile())
  e <- new.env(); utils::data("tea", package = "FactoMineR", envir = e); tea <- e$tea
  res <- multiple_correspondence_analysis(tea, 1:18)
  try_it <- function(f) tryCatch({ force(f); "ok" }, error = function(e)
    paste("ERROR:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 60)))
  variants <- list("O1 c(MCA, list)" = c("MCA", "list"),
                   "O2 c(ggfacto_mca, MCA, list)" = c("ggfacto_mca", "MCA", "list"),
                   "O2' c(MCA, ggfacto_mca, list)" = c("MCA", "ggfacto_mca", "list"))
  for (v in names(variants)) {
    x <- res; class(x) <- variants[[v]]
    say("\n%s", v)
    say("  factoextra::fviz_mca_var  %s", try_it(print(factoextra::fviz_mca_var(x))))
    say("  factoextra::fviz_mca_ind  %s", try_it(print(factoextra::fviz_mca_ind(x))))
    say("  explor prepare_results    %s", try_it(explor:::prepare_results(x)))
    say("  GDAtools::supvar          %s", try_it(GDAtools::supvar(x, tea$SPC)))
    say("  GDAtools::ggcloud_variables %s", try_it(print(GDAtools::ggcloud_variables(x))))
    say("  GDAtools::dimdescr        %s", try_it(GDAtools::dimdescr(x, tea["SPC"])))
    say("  print(): %d lines", length(utils::capture.output(print(x))))
  }
  say("\nggfacto on a GDAtools::speMCA() fit:")
  spe <- GDAtools::speMCA(tea[1:18])
  say("  ggmca()         %s", try_it(print(ggmca(spe, tea, sup_vars = "SPC", text_repel = FALSE))))
  say("  mca_interpret() %s", try_it(print(mca_interpret(spe))))
  say("  benzecri_mrv()  %s", try_it(benzecri_mrv(spe)))
  pca <- principal_component_analysis(mtcars, 1:7)
  ca  <- correspondence_analysis(tabxplor::tab(forcats::gss_cat, relig, partyid))
  saveRDS(list(mca = res, pca = pca, ca = ca), file.path(out_dir, "fits.rds"))
}

# The course's print() and plot() calls, and explor, in a library where FactoMineR is absent: the
# objects were fitted beforehand by section_object().
section_nofm <- function() {
  f <- readRDS(file.path(out_dir, "fits.rds"))
  grDevices::pdf(tempfile())
  say("FactoMineR installed: %s", requireNamespace("FactoMineR", quietly = TRUE))
  try_it <- function(f) tryCatch({ force(f); "ok" }, error = function(e)
    paste("ERROR:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 70)))
  say("print(mca): %d lines", length(utils::capture.output(print(f$mca))))
  say("plot(acp, choix = \"var\") into grid.arrange: %s",
      try_it(gridExtra::grid.arrange(plot(f$pca, choix = "var"), plot(f$pca, choix = "ind"))))
  say("plot(resultat_ac): %s", try_it(plot(f$ca)))
  say("explor prepare_results(mca): %s", try_it(explor:::prepare_results(f$mca)))
}

# === SECTION: bugs -- minimal reproductions, for the issues drafted in Appendix A ============

section_bugs <- function() {
  load_pkg()
  grDevices::pdf(tempfile())
  e <- new.env(); utils::data("tea", package = "FactoMineR", envir = e); tea <- e$tea
  mca <- function(...) FactoMineR::MCA(..., graph = FALSE)
  out <- function(id, x) say("%-4s %s", id, x)

  ord <- data.frame(a = factor(c(rep("low", 97), "mid", "high", "high"), c("low", "mid", "high"),
                               ordered = TRUE), b = factor(rep(c("x", "y"), 50)))
  code <- sprintf("d <- %s; FactoMineR::MCA(d, level.ventil = 0.05, graph = FALSE); cat('returned')",
                  paste(deparse(ord), collapse = ""))
  r <- suppressWarnings(system2("timeout", c("10", "Rscript", "-e", shQuote(code)), stdout = TRUE,
                                stderr = FALSE))
  out("B1", sprintf("level.ventil on an ordered factor: %s",
                    if (length(r) && grepl("returned", r[length(r)])) "returned" else "no return in 10 s"))

  na <- tea[1:6]; na$breakfast[1:20] <- NA
  w <- 0L
  withCallingHandlers(mca(na), warning = function(x) { w <<- w + 1L; invokeRestart("muffleWarning") })
  out("B2", sprintf("MCA() with missing values: %d warning (the code means to give one)", w))

  out("B3", sprintf("na.method = \"Average\": %s", tryCatch({ mca(na, na.method = "Average"); "ok" },
                    error = function(x) conditionMessage(x))))

  one <- tea["breakfast"]
  a <- mca(one)$var$coord[1, 1]
  b <- mca(data.frame(breakfast = factor(levels(one$breakfast))),
           row.w = as.vector(table(one$breakfast)))$var$coord[1, 1]
  out("B5", sprintf("one binary variable: coordinate of \"breakfast\" %.4f on 300 rows, %.4f on the 2
     aggregated rows of the same data", a, b))

  x1 <- mca(tea[1:6])$var
  x2 <- mca(tea[1:6], row.w = rep(1000, 300))$var
  out("B6", sprintf("row.w x 1000: coordinates differ by %.1e, v.test multiplied by %.1f",
                    max(abs(x1$coord - x2$coord)), mean(x2$v.test / x1$v.test)))

  c1 <- withr::with_seed(1, mca(tea[1:18], ncp = 5)$ind$coord)
  c2 <- withr::with_seed(2, mca(tea[1:18], ncp = 5)$ind$coord)
  out("B7", sprintf("ncp = 5 (irlba), seeds 1 and 2: individual coordinates differ by up to %.1e",
                    max(abs(abs(c1) - abs(c2)))))

  w0 <- rep(1, 300); w0[1] <- 0
  out("B9", sprintf("one zero row weight: %s", tryCatch({ mca(tea[1:6], row.w = w0); "ok" },
                    error = function(x) gsub("\\s+", " ", conditionMessage(x)))))

  X <- unclass(table(forcats::gss_cat$race, forcats::gss_cat$marital)); X <- rbind(X, zero = 0)
  out("B8", sprintf("CA() with a zero row and col.sup: %s", tryCatch({
    suppressWarnings(FactoMineR::CA(X, col.sup = 2, graph = FALSE)); "ok"
  }, error = function(x) conditionMessage(x))))

  say("\nggfacto:")
  res <- multiple_correspondence_analysis(tea, 1:6)
  out("G1", sprintf("ggmca_3d(axes = 1:2): %s", tryCatch({ ggmca_3d(res, tea, axes = 1:2); "ok" },
                    error = function(x) substr(gsub("\\s+", " ", conditionMessage(x)), 1, 80))))
  yn <- tea[1:6]
  yn$always <- factor(ifelse(yn$always == "always", "y", "n"))
  r2 <- multiple_correspondence_analysis(yn, 1:6)
  vd <- suppressMessages(ggmca_data(r2, yn))$vars_data
  out("G2", sprintf("a y/n variable: levels of `always` in the plot model: %s (FactoMineR names them %s)",
                    paste(vd$lvs[vd$vars == "always"], collapse = ", "),
                    paste(grep("always", rownames(r2$var$coord), value = TRUE), collapse = ", ")))
}

# === SECTION: tooltips -- the hover crosstabs from the Burt table ============================

# The `active_tables = "active"` crosstabs ARE the Burt table: each block (row variable a, column
# variable b) is B[a, b] in row percentages, compared to its own Total row. Built from the weighted
# and unweighted Burt tables of the profiles, handed to tabxplor::fmt() for the colours and the
# formatting only. Assumes complete answers (missing ones are levels, `<VAR>.NA`).
burt_crosstab <- function(dat, vars, template) {
  pt <- profile_table(dat[vars], dat$row.w)
  Bw <- burt_table_base(pt)
  pt$w <- pt$count
  Bn <- burt_table_base(pt)
  lv <- split(seq_len(pt$K), factor(pt$var, unique(pt$var)))
  last <- lv[[length(lv)]]
  cols <- lapply(seq_len(pt$K), function(k) {
    b   <- lv[[pt$var[k]]]
    ref <- vapply(lv, function(a) sum(Bw[a, k]) / sum(Bw[a, b]), numeric(1))[pt$var]
    pct <- c(Bw[, k] / diag(Bw), sum(Bw[last, k]) / sum(Bw[last, b]))
    tpl <- template[[k + 2]]
    a   <- attributes(tpl)
    tabxplor::fmt(n = c(Bn[, k], sum(Bn[last, k])), wn = c(Bw[, k], sum(Bw[last, k])), pct = pct,
                  diff = pct - c(ref, pct[length(pct)]), scale = a$scale, digits = 0L,
                  display = "pct", row_kind = c(rep("data", pt$K), "total"),
                  in_refrow = c(rep(FALSE, pt$K), TRUE), ref = a$ref, pct_type = a$pct_type,
                  col_var = a$col_var, color = a$color)
  })
  names(cols) <- names(template)[seq_len(pt$K) + 2]
  cols
}

section_tooltips <- function() {
  load_pkg()
  pc  <- readRDS(file.path(course, "M2S1_pc_AGD.rds"))
  dat <- na_levels(as.data.frame(pc[pc_vars]), pc_vars)
  dat$row.w <- pc$POND
  t_tab <- timed(tabs <- stacked_crosstab(dat, pc_vars, pc_vars), 2)
  t_brt <- timed(brt <- burt_crosstab(dat, pc_vars, tabs), 2)
  f <- c("n", "wn", "pct", "diff")
  worst <- vapply(f, function(x) max(vapply(names(brt), function(k) max(abs(
    as.numeric(vctrs::field(tabs[[k]], x)) - as.numeric(vctrs::field(brt[[k]], x))), na.rm = TRUE),
    numeric(1))), numeric(1))
  same_col <- all(vapply(names(brt), function(k) identical(
    tabxplor::fmt_get_color_code(tabs[[k]]), tabxplor::fmt_get_color_code(brt[[k]])), logical(1)))
  same_txt <- all(vapply(names(brt), function(k) identical(format(tabs[[k]]), format(brt[[k]])),
                         logical(1)))
  say("pc_AGD, 15 x 15 active crosstabs (%d cells): stacked tab() %.2f s, Burt route %.2f s",
      nrow(tabs) * length(brt), t_tab, t_brt)
  say("max field difference: %s; colour codes identical: %s; formatted text identical: %s",
      paste(sprintf("%s %.1e", f, worst), collapse = ", "), same_col, same_txt)

  ee  <- ee_data()
  for (n in c(1e5, 1e6)) {
    d   <- ee[withr::with_seed(1, sample(nrow(ee), n)), ]
    dat <- na_levels(as.data.frame(d[ee_q10]), ee_q10)
    dat$row.w <- d$EXTRI
    t_tab <- timed(tabs <- stacked_crosstab(dat, ee_q10, ee_q10), 1)
    t_brt <- timed(brt <- burt_crosstab(dat, ee_q10, tabs), 1)
    say("EE n = %g, Q10 (%d x %d cells): stacked tab() %.2f s, Burt route %.2f s", n, nrow(tabs),
        length(brt), t_tab, t_brt)
  }
}

# data.table against the base and vctrs building blocks: profile grouping and the Burt table.
section_datatable <- function(threads = "0") {
  load_pkg()
  data.table::setDTthreads(as.integer(threads))
  ee <- ee_data()
  for (q in c("10", "15")) {
    vars <- get(paste0("ee_q", q))
    d  <- ee[withr::with_seed(1, sample(nrow(ee), 1e6)), ]
    X  <- na_levels(as.data.frame(d[vars]), vars)
    X$w <- d$EXTRI
    t_vc <- timed({ k <- vctrs::vec_group_id(X[vars]); rowsum(X$w, k) })
    dt <- data.table::as.data.table(X)
    t_dt <- timed(dt[, list(w = sum(w), n = .N), by = vars])
    pt <- profile_table(X[vars], X$w)
    long <- data.table::data.table(p = rep(seq_len(nrow(pt$codes)), pt$Q),
                                   j = as.vector(pt$codes), w = rep(pt$w, pt$Q))
    t_bdt <- timed(long[long, on = "p", allow.cartesian = TRUE][, list(w = sum(w)), by = c("j", "i.j")], 1)
    say("%d threads, Q%s n 1e6 P %d: profiles vctrs+rowsum %.3f s, data.table by= %.3f s | Burt table: Matrix %.2f s, base rowsum %.2f s, data.table self-join %.2f s",
        data.table::getDTthreads(), q, nrow(pt$codes), t_vc, t_dt, timed(burt_table(pt), 1), timed(burt_table_base(pt), 1), t_bdt)
  }
}

# === SECTION: fm_burt -- FactoMineR's own SVD, fed our Burt table ===========================

# FactoMineR::svd.triplet() (exported) on the K x K matrix of the Burt route: FactoMineR's numerics
# and sign rule, ggfacto's Burt table. That matrix is X'X, so its singular values are the MCA's
# eigenvalues; svd.triplet() normalises its row weights to sum 1, which scales them by
# 1/sqrt(sum(c*)) under `excl`, undone here.
mca_fm_burt <- function(pt, excl = integer(), B = burt_table_base(pt), axes = Inf) {
  Q <- pt$Q; K <- pt$K
  cm <- diag(B) / (Q * pt$W)
  cs <- cm; cs[excl] <- 1e-15
  s  <- FactoMineR::svd.triplet(B / (Q^2 * pt$W * outer(cm, cm)) - 1, row.w = cs, col.w = cs,
                                ncp = min(K - Q, nrow(pt$codes) - 1))
  s$vs <- sqrt(s$vs * sqrt(sum(cs)))
  a  <- seq_len(min(axes, length(s$vs)))
  A  <- s$V[, a, drop = FALSE] * (cs / cm)
  coord_row <- Reduce(`+`, lapply(seq_len(Q), function(q) A[pt$codes[, q], , drop = FALSE])) / Q
  coord_row <- sweep(coord_row, 2, colSums(cs * s$V[, a, drop = FALSE]))
  if (is.finite(axes)) return(list(eig = s$vs^2, V = s$V, cm = cm, cs = cs, coord = coord_row))
  s$U <- t(t(coord_row) / s$vs)
  mca_slots(pt, cm, excl, s, coord_row)
}

section_fm_burt <- function() {
  load_pkg()
  d <- parity_data()
  cases <- list(list("P1 tea[1:18]", d$tea, names(d$tea)[1:18], NULL, NA),
                list("P3 tea weighted, 2 levels excl", d$tea_w, names(d$tea)[1:18], "w",
                     c("breakfast", "green")),
                list("P4 pc_AGD POND", d$pc, pc_vars, "POND", NULL),
                list("P5 pc_AGD POND, NA excl", d$pc, pc_vars, "POND", NA))
  for (k in cases) {
    X  <- na_levels(as.data.frame(k[[2]][k[[3]]]), k[[3]])
    w  <- if (is.null(k[[4]])) NULL else k[[2]][[k[[4]]]]
    ex <- excl_index(X, k[[3]], k[[5]])
    fm <- FactoMineR::MCA(X, ncp = Inf, row.w = w, graph = FALSE, excl = ex)
    pt <- profile_table(X, w)
    parity_row(k[[1]], fm, mca_fm_burt(pt, ex), pt, if (is.null(w)) rep(1, nrow(X)) else w,
               "fmSVD")
  }
  ee <- ee_data()
  for (q in c("10", "15")) {
    vars <- get(paste0("ee_q", q))
    X  <- na_levels(as.data.frame(ee[vars]), vars)
    ex <- excl_index(X, vars, NA)
    pt <- profile_table(X, ee$EXTRI)
    pk <- peak_mb(mca_fm_burt(pt, ex, axes = 5))
    say("EE 1.87 million, Q%s, P %d: FactoMineR::svd.triplet() on the Burt table, 5 axes of coordinates: %.2f s, %.0f MB",
        q, nrow(pt$codes), timed(mca_fm_burt(pt, ex, axes = 5), 1), pk$mb)
  }
}

# === SECTION: slots -- which package reads which slot of an MCA ==============================

# A static scan of every function of the packages that consume an MCA, for the slots a lighter
# object would drop or shrink.
section_slots <- function() {
  pats <- c("call$Xtot" = "call\\$Xtot|\\$Xtot", "call$X" = "call\\$X\\b|call\\$X\\[",
            "ind$..." = "\\$ind\\$", "svd$U" = "svd\\$U", "call$row.w" = "call\\$row\\.w")
  for (p in c("FactoMineR", "factoextra", "explor", "GDAtools", "Factoshiny", "FactoInvestigate")) {
    if (!requireNamespace(p, quietly = TRUE)) next
    ns <- asNamespace(p)
    fns <- Filter(function(f) is.function(get(f, ns)), ls(ns, all.names = TRUE))
    src <- vapply(fns, function(f) paste(deparse(get(f, ns)), collapse = "\n"), character(1))
    say("\n%s (%d functions)", p, length(fns))
    for (k in names(pats)) {
      hit <- fns[grepl(pats[[k]], src)]
      say("  %-11s %3d: %s", k, length(hit), paste(utils::head(hit, 8), collapse = " "))
    }
  }
}

# === SECTION: slim -- the ecosystem on a profile-fitted, slimmed MCA ========================

# FactoMineR fed the profiles, then slimmed: `call$Xtot` dropped or made integer, `call$X` dropped.
# Each shape through FactoMineR's own methods, factoextra, explor and GDAtools.
# FactoMineR's own shape for `MCA(excl =, ncp = k)`: every eigenvalue, k axes elsewhere.
axes_kept <- function(res, k, var = TRUE) {
  cut <- function(M) if (is.matrix(M) || is.data.frame(M)) M[, seq_len(k), drop = FALSE] else M
  res$ind <- lapply(res$ind, cut)
  res$svd$U <- cut(res$svd$U)
  if (var) { res$var <- lapply(res$var, cut); res$svd$V <- cut(res$svd$V) }
  res$call$ncp <- k
  res
}

section_slim <- function() {
  load_pkg()
  grDevices::pdf(tempfile())
  e <- new.env(); utils::data("tea", package = "FactoMineR", envir = e); tea <- e$tea
  X  <- na_levels(as.data.frame(tea[1:18]), names(tea)[1:18])
  pt <- profile_table(X)
  ind  <- FactoMineR::MCA(X, ncp = Inf, graph = FALSE)
  prof <- FactoMineR::MCA(X[pt$first, ], ncp = Inf, row.w = pt$w, graph = FALSE)
  shapes <- list(
    "individuals (today)"     = ind,
    "profiles"                = prof,
    "profiles, Xtot integer"  = within(prof, call$Xtot[] <- lapply(call$Xtot, as.integer)),
    "profiles, Xtot NULL"     = within(prof, call$Xtot <- NULL),
    "profiles, call$X NULL"   = within(prof, call$X <- NULL),
    "profiles, ind and var on 5 axes, every eigenvalue" = axes_kept(prof, 5, var = TRUE),
    "profiles, ind on 5 axes, var on every axis"        = axes_kept(prof, 5, var = FALSE))
  try_it <- function(f) tryCatch({ force(f); "ok" }, error = function(e)
    paste("ERROR", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 45)))
  checks <- list(
    "print"            = function(x) utils::capture.output(print(x)),
    "summary"          = function(x) utils::capture.output(summary(x)),
    "plot.MCA"         = function(x) print(plot(x)),
    "dimdesc"          = function(x) FactoMineR::dimdesc(x),
    "fviz_mca_var"     = function(x) print(factoextra::fviz_mca_var(x)),
    "fviz_mca_ind"     = function(x) print(factoextra::fviz_mca_ind(x)),
    "explor"           = function(x) explor:::prepare_results(x),
    "supvar (n rows)"  = function(x) GDAtools::supvar(x, tea$SPC),
    "ggcloud_indiv"    = function(x) print(GDAtools::ggcloud_indiv(x)),
    "ggadd_ellipses"   = function(x) print(GDAtools::ggadd_ellipses(GDAtools::ggcloud_indiv(x), x, tea$SPC)))
  say("call$Xtot is stored as %s", typeof(prof$call$Xtot[[1]]))
  for (s in names(shapes)) {
    x <- shapes[[s]]
    say("\n%s: object %.2f MB", s, as.numeric(utils::object.size(x)) / 1e6)
    for (k in names(checks)) say("  %-16s %s", k, try_it(checks[[k]](x)))
  }
}

# === SECTION: session -- where a course session's time goes =================================

# The course's MCA chain on pc_AGD, step by step, the first time (namespaces loading, as a student
# lives it) and again.
section_session <- function() {
  load_pkg()
  grDevices::pdf(tempfile())
  pc <- readRDS(file.path(course, "M2S1_pc_AGD.rds"))
  steps <- list(
    "multiple_correspondence_analysis()" = quote(acm <- multiple_correspondence_analysis(
      pc, tidyselect::all_of(pc_vars), wt = POND)),
    "mca_interpret()" = quote(print(mca_interpret(acm, axes = 1:2))),
    "ggmca(active_tables = \"active\") |> ggi()" = quote(w <- ggi(ggmca(acm, pc,
      active_tables = "active"))),
    "hierarchical_clust() in mutate()" = quote(pc <- dplyr::mutate(pc, cah = hierarchical_clust(
      acm, ncp = 3, nb_clust = 6, tree = FALSE))),
    "ggmca(clust = cah, profiles) |> ggi()" = quote(w <- ggi(ggmca(acm, pc, clust = cah))),
    "clust_tab()" = quote(print(clust_tab(acm, pc, cah))))
  for (round in c("first", "again")) {
    say("\n%s run", round)
    for (s in names(steps)) say("  %-42s %.2f s", s, system.time(utils::capture.output(
      eval(steps[[s]], environment())))[["elapsed"]])
  }
}

section_burt <- function() {
  load_pkg()
  ee <- ee_data()
  for (q in c("10", "15")) {
    vars <- get(paste0("ee_q", q))
    X  <- na_levels(as.data.frame(ee[vars]), vars)
    pt <- profile_table(X, ee$EXTRI)
    t_m <- timed(B1 <- burt_table(pt), 1)
    t_b <- timed(B2 <- burt_table_base(pt), 1)
    say("Q%s P %d K %d: Burt table with Matrix %.2f s, base rowsum() %.2f s, max diff %.1e", q,
        nrow(pt$codes), pt$K, t_m, t_b, max(abs(B1 - B2)))
  }
}

# === SECTION: dispatch =======================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 0) do.call(paste0("section_", args[1]), as.list(args[-1]))
