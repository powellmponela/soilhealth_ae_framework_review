# ============================================================
# Figure 2 heatmap (z-scores) with:
#   - hierarchical clustering (explicit distance + linkage)
#   - data-driven clustering via Dynamic Tree Cut
#   - merged to EXACTLY 5 framework design domains (rows)
#   - principles shown + merged to k_col groups (columns)
#   - framework domain legend labels + ORDER (4,1,2,3,5)
#   - manual framework-domain colors
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
  library(dynamicTreeCut)
  library(pheatmap)
  library(RColorBrewer)
})

# ---------------------------
# INPUT expected:
# principle_matrix_merged = data.frame with:
#   - author_abbreviation (framework ID; row key)
#   - numeric columns = agroecological principles (counts or scores)
# ---------------------------

# 1) Clean + keep only valid frameworks
df <- principle_matrix %>%
  mutate(author_abbreviation = str_to_lower(str_squish(author_abbreviation))) %>%
  filter(!is.na(author_abbreviation), author_abbreviation != "") %>%
  relocate(author_abbreviation, .before = 1)

# 2) Build matrix (principles = columns; frameworks = rows)
mat_raw <- df %>%
  select(author_abbreviation, where(is.numeric)) %>%
  column_to_rownames("author_abbreviation") %>%
  as.matrix()

# 3) Z-score normalize by principle (column-wise)
mat_z <- scale(mat_raw) |> as.matrix()

# (Optional) cap extreme z-scores to keep legend interpretable
mat_plot <- pmax(pmin(mat_z, 3), -3)

# ---------------------------
# Clustering settings (report these in caption)
# ---------------------------
dist_metric <- "euclidean"
link_method <- "complete"   # alternative: "ward.D2" (choose one and keep consistent)

row_hc <- hclust(dist(mat_plot, method = dist_metric), method = link_method)
col_hc <- hclust(dist(t(mat_plot), method = dist_metric), method = link_method)

# ---------------------------
# Dynamic Tree Cut (data-driven; no fixed cut height)
# ---------------------------
row_cl0 <- cutreeDynamic(
  dendro = row_hc,
  distM  = as.matrix(dist(mat_plot, method = dist_metric)),
  method = "hybrid",
  deepSplit = 2,
  minClusterSize = 3
)

col_cl0 <- cutreeDynamic(
  dendro = col_hc,
  distM  = as.matrix(dist(t(mat_plot), method = dist_metric)),
  method = "hybrid",
  deepSplit = 2,
  minClusterSize = 2
)

# Remove "0" (unassigned) so it doesn't create extra legend levels
row_cl0[row_cl0 == 0] <- NA
col_cl0[col_cl0 == 0] <- NA

# ---------------------------
# Merge dynamic clusters to EXACTLY k domains/groups by centroid similarity
# (still data-driven; avoids arbitrary cut heights)
# ---------------------------
merge_to_k <- function(mat, cl, k, dist_metric = "euclidean", link_method = "complete") {
  cl_ids <- sort(unique(na.omit(cl)))
  if (length(cl_ids) <= k) return(as.integer(factor(cl, levels = cl_ids)))
  
  centroids <- do.call(rbind, lapply(cl_ids, function(g) {
    colMeans(mat[cl == g, , drop = FALSE], na.rm = TRUE)
  }))
  rownames(centroids) <- cl_ids
  
  hc_c <- hclust(dist(centroids, method = dist_metric), method = link_method)
  map_old_to_new <- cutree(hc_c, k = k)
  as.integer(map_old_to_new[as.character(cl)])
}

# ---- Rows: EXACTLY 5 framework domains ----
k_row <- 5
framework_domain5_code <- merge_to_k(mat_plot, row_cl0, k = k_row, dist_metric, link_method)
framework_domain5_code <- factor(framework_domain5_code, levels = 1:k_row, labels = as.character(1:k_row))

# ---- Columns: set your desired number of principle groups (edit k_col if needed) ----
k_col <- 3
principle_group_code <- merge_to_k(t(mat_plot), col_cl0, k = k_col, dist_metric, link_method)
principle_group <- factor(principle_group_code, levels = 1:k_col, labels = as.character(1:k_col))

# ---------------------------
# Framework-domain legend labels + ORDER (4,1,2,3,5)
# 4= Soil steward
# 1= Soil health assess
# 2= AE-ecosystem
# 3= Landscape-livelihood
# 5= Policy-outcome
# ---------------------------
fw_order_codes <- c("4","1","2","5","3")
fw_order_names <- c("Soil steward",
                    "Soil health assess",
                    "AE-ecosystem",
                    "Policy-outcome", 
                    "Landscape-livelihood")

framework_domain5 <- factor(
  as.character(framework_domain5_code),
  levels = fw_order_codes,
  labels = fw_order_names
)

# ---------------------------
# Annotations
# ---------------------------
ann_row <- data.frame(Framework_domain = framework_domain5)
rownames(ann_row) <- rownames(mat_plot)

ann_col <- data.frame(Principle_group = principle_group)
rownames(ann_col) <- colnames(mat_plot)

# ---------------------------
# Separators (robust to factor ordering)
# ---------------------------
dom_chr <- as.character(ann_row$Framework_domain)[row_hc$order]
gaps_row <- cumsum(rle(dom_chr)$lengths)
gaps_row <- gaps_row[-length(gaps_row)]

grp_chr <- as.character(ann_col$Principle_group)[col_hc$order]
gaps_col <- cumsum(rle(grp_chr)$lengths)
gaps_col <- gaps_col[-length(gaps_col)]

# ---------------------------
# Manual colors for framework domains (mapped to NEW labels)
# (Domain 4=orange, 1=green, 2=red, 3=purple, 5=blue)
# ---------------------------
fw_cols <- c(
  "Soil steward"         = "orange",
  "AE-ecosystem"         = "red",
  "Landscape-livelihood" = "purple",
  "Soil health assess"   = "green",
  "Policy-outcome"       = "blue"
)

# Principle-group colors (distinct)
if (k_col <= 8) {
  pr_cols <- setNames(brewer.pal(max(3, k_col), "Set2")[1:k_col], levels(principle_group))
} else {
  pr_cols <- setNames(hcl.colors(k_col, "Dark 3"), levels(principle_group))
}

ann_colors <- list(
  Framework_domain = fw_cols,
  Principle_group  = pr_cols
)

# ---------------------------
# Plot
# ---------------------------
jpeg("principle_heatmap_zscore.jpeg", width = 2600, height = 3000, res = 300)
pheatmap(
  mat = mat_plot,
  cluster_rows = row_hc,
  cluster_cols = col_hc,
  annotation_row = ann_row,
  annotation_col = ann_col,
  annotation_colors = ann_colors,
  gaps_row = gaps_row,
  gaps_col = gaps_col,
  color = colorRampPalette(rev(brewer.pal(11, "RdBu")))(100),
  breaks = seq(-3, 3, length.out = 101),
  legend_breaks = c(-3, -2, -1, 0, 1, 2, 3),
  legend_labels = c("-3 SD", "-2 SD", "-1 SD", "Mean (0)", "+1 SD", "+2 SD", "+3 SD"),
  fontsize_row = 8,
  fontsize_col = 9,
  angle_col = 90
)
dev.off()

# Quick audit
cat("Framework domain levels (should be 5):", paste(levels(framework_domain5), collapse = " | "), "\n")
cat("Principle groups:", length(levels(principle_group)), "\n")
cat("Frameworks plotted:", nrow(mat_plot), " | Principles plotted:", ncol(mat_plot), "\n")