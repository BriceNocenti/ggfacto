# PURPOSE: The chunked Ward tree (CURE-like) measured against the exact one -- a prototype, kept out
#   of R/.
# ROLE: Answers "can the tree be built by parts, for clouds too large for the exact one?". Slabs of
#   distinct points along the first axis each get their own Ward tree, cut into micro-clusters; one
#   Ward tree on their weighted centroids gives the clusters; HCPC()'s k-means consolidates them.
# KEY CONSTRAINTS:
#   - Not in R/: it keeps Ward's quality (same R2, same share of individuals closer to another
#     centre) but reclassifies up to a third of the individuals -- as much as the exact tree itself
#     moves when 1 % of them are dropped. It can be judged on quality, never on equality to HCPC().
#   - The exact tree, ggfacto:::ward_clusters(), has memory linear in the distinct points and time
#     quadratic in them (about 25 s at 40 000): this only matters well beyond 100 000.
# Run from the package root: OMP_NUM_THREADS=1 Rscript dev/hclust_chunked.R

suppressMessages(devtools::load_all(".", quiet = TRUE))

# === SECTION: The chunked tree ==========================================================

# ward_clusters() with its tree built by parts: `chunk` leaves per slab, one micro-cluster per
# `ratio` leaves.
chunked_clusters <- function(coord, w, answers, nb_clust, chunk = 5000, ratio = 5, consol = TRUE) {
  n   <- nrow(coord)
  ord <- order(coord[, 1])
  X   <- coord[ord, , drop = FALSE]
  leaf  <- if (is.null(answers)) seq_len(n) else vctrs::vec_group_id(answers[ord, , drop = FALSE])
  first <- which(!duplicated(leaf))
  Xl <- X[first, , drop = FALSE]
  wl <- as.vector(rowsum(w[ord], leaf))

  slab  <- ceiling(seq_along(first) / chunk)
  micro <- integer(length(first))
  for (s in unique(slab)) {
    i  <- which(slab == s)
    k  <- max(1L, ceiling(length(i) / ratio))
    cl <- if (length(i) <= k) seq_along(i) else stats::cutree(
      fastcluster::hclust.vector(Xl[i, , drop = FALSE], method = "ward", members = wl[i]), k = k)
    micro[i] <- cl + max(micro)
  }
  wm   <- as.vector(rowsum(wl, micro))
  tree <- fastcluster::hclust.vector(rowsum(Xl * wl, micro) / wm, method = "ward", members = wm)

  cl <- stats::cutree(tree, k = nb_clust)[micro][leaf]
  centers <- rowsum(X, cl) / tabulate(cl)
  if (consol) {
    km <- stats::kmeans(X, centers = centers, iter.max = 10)
    cl <- km$cluster
    centers <- km$centers
  }
  out <- integer(n)
  out[ord] <- order(order(centers[, 1]))[cl]
  list(clust = factor(out), micro = length(wm))
}

# === SECTION: Measures ==================================================================

ari <- function(a, b) {
  pairs <- function(x) sum(x * (x - 1) / 2)
  t <- table(a, b)
  ra <- pairs(rowSums(t)); rb <- pairs(colSums(t)); all <- pairs(sum(t))
  (pairs(t) - ra * rb / all) / ((ra + rb) / 2 - ra * rb / all)
}

# Weighted share of individuals in another cluster, after the best one-to-one matching of labels.
reclassified <- function(a, b, w) {
  t <- as.matrix(stats::xtabs(w ~ a + b))
  kept <- 0
  while (length(t) && max(t) > 0) {
    ij <- which(t == max(t), arr.ind = TRUE)[1, ]
    kept <- kept + t[ij[1], ij[2]]
    t <- t[-ij[1], -ij[2], drop = FALSE]
  }
  1 - kept / sum(w)
}

wmeans <- function(X, g, w) rowsum(X * w, g) / as.vector(rowsum(w, g))

# The share of the inertia between the clusters: what Ward's method maximises.
r2 <- function(X, cl, w) {
  g <- colSums(X * w) / sum(w)
  between <- sum(as.vector(rowsum(w, cl)) * rowSums(sweep(wmeans(X, cl, w), 2, g)^2))
  between / sum(w * rowSums(sweep(X, 2, g)^2))
}

# Weighted share of individuals closer to another cluster's centre than to their own.
lost <- function(X, cl, w) {
  C <- wmeans(X, cl, w)
  d <- vapply(seq_len(nrow(C)), function(k) rowSums(sweep(X, 2, C[k, ])^2), numeric(nrow(X)))
  sum(w[max.col(-d, ties.method = "first") != as.integer(cl)]) / sum(w)
}

# === SECTION: The clouds ================================================================

course <- path.expand("~/github/formations_stat/etudiants/donnees")
clouds <- if (dir.exists(course)) {
  pc <- readRDS(file.path(course, "M2S1_pc_AGD.rds"))
  mca <- multiple_correspondence_analysis(pc, c(
    "MUSIQUE", "TELE", "RADIO", "LIVRES", "CINEMA", "JV", "VIDEOS", "RESEAUX", "DANSE", "THEATRE",
    "CLASSIQUE", "POP_ROCK_JAZZ", "CIRQUE", "MUSEE_EXPO", "MONUMENT"), wt = POND)
  ee   <- readRDS(file.path(course, "M2S1_ee_sal19.rds"))
  num  <- c("AGE", "SALAIRE", "HHC", "ANCIENNETE_A", "ADFE", "NBENFIND")
  ee   <- as.data.frame(ee[stats::complete.cases(ee[num]), c(num, "EXTRIDF")])
  pca  <- principal_component_analysis(ee, tidyselect::all_of(num), wt = EXTRIDF)
  list("pc_AGD MCA" = list(res = mca, chunk = 1000), "ee_sal19 PCA" = list(res = pca, chunk = 5000))
} else {
  data(tea, package = "FactoMineR")
  list("tea MCA" = list(res = multiple_correspondence_analysis(tea, 1:18), chunk = 100))
}

# === SECTION: Exact against chunked, and against itself ================================

for (name in names(clouds)) {
  res  <- clouds[[name]]$res
  pts  <- clust_points(res, ncp = 3, margin = "rows")
  X    <- pts$coord
  w    <- pts$w
  for (k in c(6, 9)) {
    t0    <- proc.time()[[3]]
    exact <- ward_clusters(X, w, pts$answers, k, consol = TRUE)$clust
    cat(sprintf("\n== %s, ncp = 3, %d clusters: exact %.2f s, R2 %.4f, lost %.2f %%\n", name, k,
                proc.time()[[3]] - t0, r2(X, exact, w), 100 * lost(X, exact, w)))
    for (ratio in c(5, 20)) {
      t0 <- proc.time()[[3]]
      ch <- chunked_clusters(X, w, pts$answers, k, chunk = clouds[[name]]$chunk, ratio = ratio)
      cat(sprintf(str_c("chunked, 1 micro-cluster per %2d points (%5d): %.2f s, ARI %.3f, ",
                        "reclassified %5.2f %%, R2 %.4f, lost %.2f %%\n"),
                  ratio, ch$micro, proc.time()[[3]] - t0, ari(exact, ch$clust),
                  100 * reclassified(exact, ch$clust, w), r2(X, ch$clust, w),
                  100 * lost(X, ch$clust, w)))
    }
    for (seed in 1:3) {
      keep <- withr::with_seed(seed, sort(sample(nrow(X), round(0.99 * nrow(X)))))
      sub  <- ward_clusters(X[keep, , drop = FALSE], w[keep],
                            pts$answers[keep, , drop = FALSE], k, consol = TRUE)$clust
      cat(sprintf("exact, 1 %% of individuals dropped (seed %d): ARI %.3f, reclassified %5.2f %%\n",
                  seed, ari(exact[keep], sub), 100 * reclassified(exact[keep], sub, w[keep])))
    }
  }
}
