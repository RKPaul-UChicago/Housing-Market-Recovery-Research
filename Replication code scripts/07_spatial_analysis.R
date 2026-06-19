# =============================================================================
# 07_spatial_analysis.R
# Spatial autocorrelation analysis: global Moran's I, LISA clusters, and
# k-means recovery typology. Saves all results for paper tables.
# Outputs:
#   processed/results_moran_lisa.csv     -- global Moran's I and LISA counts
#   processed/results_kmeans_clusters.csv -- k-means cluster profiles (Table 4)
# =============================================================================

source("00_setup.R")

cat("\n--- Loading PUMA geometry ---\n")
shp_files <- list.files(SHP_ROOT, pattern = "_puma10[.]shp$",
                         recursive = TRUE, full.names = TRUE)
pumas <- do.call(rbind, lapply(shp_files, function(f) {
  s <- st_read(f, quiet = TRUE)
  s[, c("STATEFP10", "PUMACE10", "GEOID10", "NAMELSAD10")]
}))

cat("--- Loading analysis dataset ---\n")
analysis_df <- read_csv(processed_path("analysis_dataset.csv"), show_col_types = FALSE)

# Attach geometry
analysis_sf <- pumas %>%
  inner_join(analysis_df, by = c("GEOID10" = "puma_geoid")) %>%
  rename(puma_geoid = GEOID10)
cat("Spatial dataset:", nrow(analysis_sf), "PUMAs\n")

cat("--- Restricting to CONUS, projecting to Albers ---\n")
conus <- analysis_sf %>%
  filter(!as.integer(STATEFP10) %in% c(2L, 15L, 72L)) %>%
  filter(!st_is_empty(geometry)) %>%
  st_transform(5070)
cat("CONUS PUMAs:", nrow(conus), "\n")

cat("--- Queen contiguity weights ---\n")
nb <- poly2nb(conus, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

cat("--- Global Moran's I ---\n")
gmi <- moran.test(conus$recovery_index, lw, zero.policy = TRUE)
cat("Moran's I:", round(gmi$estimate["Moran I statistic"], 4), "\n")
cat("Expected I:", round(gmi$estimate["Expectation"], 6), "\n")
cat("p-value:", format(gmi$p.value, scientific = TRUE, digits = 3), "\n")

cat("--- LISA ---\n")
lmi <- localmoran(conus$recovery_index, lw, zero.policy = TRUE)
conus$lisa_p <- lmi[, "Pr(z != E(Ii))"]
ri_mean <- mean(conus$recovery_index, na.rm = TRUE)
z_ri    <- (conus$recovery_index - ri_mean) / sd(conus$recovery_index, na.rm = TRUE)
lag_ri  <- lag.listw(lw, conus$recovery_index, zero.policy = TRUE)
conus$lisa_type <- case_when(
  conus$lisa_p > 0.05          ~ "Not Significant",
  z_ri > 0 & lag_ri > ri_mean  ~ "High-High",
  z_ri < 0 & lag_ri < ri_mean  ~ "Low-Low (Unrecovered Core)",
  TRUE                          ~ "Spatial Outlier"
)
cat("LISA counts:\n"); print(table(conus$lisa_type))

# Save global spatial results
moran_results <- tibble(
  statistic        = "Global Moran's I",
  estimate         = round(gmi$estimate["Moran I statistic"], 4),
  expected         = round(gmi$estimate["Expectation"], 6),
  variance         = round(gmi$estimate["Variance"], 6),
  p_value          = gmi$p.value,
  n_pumas_conus    = nrow(conus),
  n_hh_count       = NA,
  weight_style     = "Queen contiguity, row-standardized (W)",
  lisa_high_high   = sum(conus$lisa_type == "High-High"),
  lisa_low_low     = sum(conus$lisa_type == "Low-Low (Unrecovered Core)"),
  lisa_outlier     = sum(conus$lisa_type == "Spatial Outlier"),
  lisa_ns          = sum(conus$lisa_type == "Not Significant")
)
write_csv(moran_results, processed_path("results_moran_lisa.csv"))
cat("Saved: results_moran_lisa.csv\n")

cat("--- K-means typology (k=4) ---\n")
set.seed(2026)
clust_features <- c("recovery_index", "med_value_2021", "real_value_2012",
                    "med_inc_2021", "risk_score", "sovi_score")
clust_df <- analysis_df %>%
  select(puma_geoid, all_of(clust_features)) %>%
  drop_na()
cat("PUMAs entering clustering:", nrow(clust_df), "\n")
X  <- scale(clust_df[, clust_features])
km <- kmeans(X, centers = 4, nstart = 25)
clust_df$cluster <- km$cluster

co <- clust_df %>%
  group_by(cluster) %>%
  summarise(mean_ri = mean(recovery_index), .groups = "drop") %>%
  arrange(mean_ri) %>%
  mutate(cluster_label = c(
    "Persistently Depressed",
    "Below-Average Recovery",
    "Above-Average Recovery",
    "Boom Markets"
  ))
clust_df <- clust_df %>%
  left_join(co[, c("cluster", "cluster_label")], by = "cluster")

cluster_profiles <- clust_df %>%
  group_by(cluster_label) %>%
  summarise(
    n_pumas          = n(),
    mean_ri          = round(mean(recovery_index, na.rm = TRUE), 3),
    mean_value_2021  = round(mean(med_value_2021, na.rm = TRUE)),
    mean_value_2012r = round(mean(real_value_2012, na.rm = TRUE)),
    mean_hh_income   = round(mean(med_inc_2021, na.rm = TRUE)),
    mean_risk_score  = round(mean(risk_score, na.rm = TRUE), 1),
    mean_sovi_score  = round(mean(sovi_score, na.rm = TRUE), 1),
    sd_ri            = round(sd(recovery_index, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(mean_ri)
write_csv(cluster_profiles, processed_path("results_kmeans_clusters.csv"))
cat("Saved: results_kmeans_clusters.csv\n")
print(cluster_profiles, width = 120)

# Save cluster assignments for joining to spatial data
clust_assign <- clust_df %>% select(puma_geoid, cluster, cluster_label)
write_csv(clust_assign, processed_path("kmeans_cluster_assignments.csv"))
cat("Saved: kmeans_cluster_assignments.csv  (", nrow(clust_assign), "rows)\n")

# Save LISA assignments from CONUS run
lisa_out <- conus %>%
  st_drop_geometry() %>%
  select(puma_geoid, lisa_p, lisa_type)
write_csv(lisa_out, processed_path("lisa_cluster_assignments.csv"))
cat("Saved: lisa_cluster_assignments.csv  (", nrow(lisa_out), "rows)\n")

cat("\n07_spatial_analysis.R complete.\n")
