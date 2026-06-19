# =============================================================================
# generate_figures.R
# Produces all figures saved to data and Replication codes/figures/ using only data from:
#   data and Replication codes/raw/       -- TIGER shapefiles
#   data and Replication codes/processed/ -- pre-computed analysis results
#
# Map projection: Albers Equal Area Conic (EPSG:5070) enforced on all maps.
# State outlines are derived by dissolving PUMA polygons -- no raster tiles
# are used, which ensures a clean, distortion-free projected map.
# No internet access required.
#
# HOW TO SET THE PROJECT ROOT
# ---------------------------
# By default the project root is auto-detected from the working directory.
# If auto-detection fails, set OVERRIDE_ROOT below to the full path of the
# folder that contains the "data and Replication codes/" directory.
# Example:  OVERRIDE_ROOT <- "/home/yourname/Paul_2026_Housing_Recovery"
# =============================================================================

OVERRIDE_ROOT <- NULL   # <-- set this only if auto-detection fails

# Auto-detect root (same logic as 00_setup.R)
.sentinel <- function(p)
  dir.exists(file.path(p, "data and Replication codes", "raw"))

if (!is.null(OVERRIDE_ROOT)) {
  REPO_ROOT <- normalizePath(OVERRIDE_ROOT, mustWork = FALSE)
  if (!.sentinel(REPO_ROOT))
    stop("OVERRIDE_ROOT does not contain the expected subfolders. Check the path.")
} else {
  wd <- normalizePath(getwd(), mustWork = FALSE)
  parts <- strsplit(wd, .Platform$file.sep)[[1]]
  REPO_ROOT <- NULL
  for (depth in seq_len(min(6L, length(parts)))) {
    candidate <- paste(parts[seq_len(length(parts) - depth + 1L)],
                       collapse = .Platform$file.sep)
    if (nchar(candidate) < 2) break
    if (.sentinel(candidate)) { REPO_ROOT <- normalizePath(candidate); break }
  }
  if (is.null(REPO_ROOT))
    stop("Cannot auto-detect project root. Set OVERRIDE_ROOT in generate_figures.R.")
  message("Project root auto-detected: ", REPO_ROOT)
}

# PROJ/GDAL on macOS
if (Sys.info()[["sysname"]] == "Darwin") {
  sf_proj <- system.file("proj", package = "sf")
  sf_gdal <- system.file("gdal", package = "sf")
  if (nchar(sf_proj) > 0L) Sys.setenv(PROJ_LIB = sf_proj, GDAL_DATA = sf_gdal)
}

suppressMessages({
  library(tidyverse)
  library(sf)
  library(scales)
})
options(scipen = 999)
theme_set(theme_minimal(base_size = 12))

# ---------------------------------------------------------------------------
# Paths (all derived from REPO_ROOT -- never hardcoded)
# ---------------------------------------------------------------------------
RAW_DIR   <- file.path(REPO_ROOT, "data and Replication codes", "raw")
PROC_DIR  <- file.path(REPO_ROOT, "data and Replication codes", "processed")
FIG_DIR   <- file.path(REPO_ROOT, "data and Replication codes", "figures")

raw_path  <- function(...) file.path(RAW_DIR, ...)
proc_path <- function(...) file.path(PROC_DIR, ...)

# ---------------------------------------------------------------------------
# Load processed analysis files
# ---------------------------------------------------------------------------
cat("--- Reading processed files ---\n")
trends    <- read_csv(proc_path("national_value_trends.csv"),      show_col_types = FALSE)
puma_rec  <- read_csv(proc_path("puma_recession_recovery.csv"),    show_col_types = FALSE)
analysis  <- read_csv(proc_path("analysis_dataset.csv"),           show_col_types = FALSE)
state_inc <- read_csv(proc_path("state_income_analysis.csv"),      show_col_types = FALSE)
moran_res <- read_csv(proc_path("results_moran_lisa.csv"),         show_col_types = FALSE)
clust     <- read_csv(proc_path("kmeans_cluster_assignments.csv"), show_col_types = FALSE)
lisa      <- read_csv(proc_path("lisa_cluster_assignments.csv"),   show_col_types = FALSE)

analysis <- analysis %>%
  left_join(clust, by = "puma_geoid") %>%
  left_join(lisa,  by = "puma_geoid")

cat("Analysis dataset:", nrow(analysis), "PUMAs\n")

# ---------------------------------------------------------------------------
# Load PUMA geometry, project to Albers Equal Area (EPSG:5070)
# ---------------------------------------------------------------------------
cat("--- Loading PUMA shapefiles ---\n")
shp_files <- list.files(raw_path("census_geometry"),
                         pattern = "_puma10[.]shp$",
                         recursive = TRUE, full.names = TRUE)
cat("Shapefiles found:", length(shp_files), "\n")

pumas <- do.call(rbind, lapply(shp_files, function(f) {
  s <- st_read(f, quiet = TRUE)
  s[, c("STATEFP10", "PUMACE10", "GEOID10", "NAMELSAD10")]
}))

sf_data <- pumas %>%
  inner_join(analysis, by = c("GEOID10" = "puma_geoid")) %>%
  rename(puma_geoid = GEOID10)

# CONUS only, Albers Equal Area
map_df <- sf_data %>%
  filter(!as.integer(STATEFP10) %in% c(2L, 15L, 72L)) %>%
  st_transform(5070)

conus  <- map_df %>% filter(!is.na(recovery_index))
cat("CONUS PUMAs for maps:", nrow(conus), "\n")

# ---------------------------------------------------------------------------
# Build state outline background by dissolving PUMA polygons to state level.
# This gives a clean, properly projected background without raster tiles.
# ---------------------------------------------------------------------------
cat("--- Building state background (dissolve PUMAs by state) ---\n")
states_bg <- map_df %>%
  group_by(STATEFP10) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

states_conus <- conus %>%
  group_by(STATEFP10) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

cat("State outlines built:", nrow(states_bg), "states\n")

# ---------------------------------------------------------------------------
# Shared map theme: void background, Albers projection enforced
# ---------------------------------------------------------------------------
map_theme <- function(legend.position = "right", ...) {
  list(
    coord_sf(crs = 5070, datum = NA),
    theme_void(base_size = 11),
    theme(
      plot.title      = element_text(face = "bold", size = 13),
      plot.subtitle   = element_text(size = 9, color = "grey30"),
      plot.caption    = element_text(size = 7, color = "grey50", hjust = 0,
                                     margin = margin(t = 6)),
      legend.position = legend.position,
      plot.margin     = margin(6, 6, 6, 6),
      ...
    )
  )
}

MAP_CAPTION <- paste0(
  "Data: data and Replication codes/raw/census_geometry TIGER/Line shapefiles ",
  "(2010-vintage PUMAs). Projection: Albers Equal Area Conic (EPSG:5070). ",
  "Alaska, Hawaii, and Puerto Rico excluded."
)

save_fig <- function(plot, name, w = 10, h = 6.5) {
  path <- file.path(FIG_DIR, name)
  ggsave(path, plot, width = w, height = h, dpi = 300, bg = "white")
  cat("Saved:", name, "\n")
}

# ===========================================================================
# Figure 1: National median and mean home values 2007-2021
# (not a map -- no projection needed)
# ===========================================================================
cat("--- Figure 1: National value trends ---\n")

trend_long <- trends %>%
  pivot_longer(cols = c(median_value, mean_value),
               names_to = "stat", values_to = "value") %>%
  mutate(
    label   = if_else(stat == "median_value", "Median", "Mean"),
    value_k = value / 1000
  )

fig1 <- ggplot(trend_long, aes(x = year, y = value_k,
                                color = label, linetype = label, shape = label)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  annotate("rect", xmin = 2007, xmax = 2009, ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "firebrick") +
  annotate("rect", xmin = 2019, xmax = 2021, ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "steelblue") +
  annotate("text", x = 2008, y = 430, label = "Recession\nWindow",
           size = 3, color = "firebrick", fontface = "italic") +
  annotate("text", x = 2020, y = 430, label = "COVID\nBoom",
           size = 3, color = "steelblue", fontface = "italic") +
  scale_x_continuous(breaks = c(2007, 2009, 2019, 2021)) +
  scale_y_continuous(labels = function(x) paste0("$", x, "K")) +
  scale_color_manual(values = c(Mean = "#2c7bb6", Median = "#d7191c")) +
  scale_linetype_manual(values = c(Mean = "dashed", Median = "solid")) +
  scale_shape_manual(values = c(Mean = 17, Median = 16)) +
  labs(
    x       = NULL,
    y       = "Home Value (nominal USD, thousands)",
    color   = NULL, linetype = NULL, shape = NULL,
    caption = paste0(
      "Source: processed/national_value_trends.csv (derived from IPUMS USA ACS 1-year samples). ",
      "Owner-occupied households only. N by year: ",
      paste(paste0(trends$year, " = ", comma(trends$n_hh)), collapse = "; "), "."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 7, color = "grey50", hjust = 0)
  )
save_fig(fig1, "fig1_value_trends.png", w = 7, h = 5)

# ===========================================================================
# Figure 2: Real recovery index choropleth -- Albers Equal Area
# ===========================================================================
cat("--- Figure 2: Recovery index map ---\n")

ri_lim <- quantile(map_df$recovery_index, c(0.01, 0.99), na.rm = TRUE)

fig2 <- ggplot() +
  # State background
  geom_sf(data  = states_bg,
          fill  = "#f0ede8", color = "white", linewidth = 0.4) +
  # PUMA choropleth
  geom_sf(data  = map_df,
          aes(fill = recovery_index), color = NA) +
  # Thin state border overlay for readability
  geom_sf(data  = states_bg,
          fill  = NA, color = "white", linewidth = 0.25) +
  scale_fill_viridis_c(
    option   = "magma", direction = -1,
    name     = "Recovery\nIndex",
    limits   = ri_lim, oob = squish,
    labels   = function(x) round(x, 2),
    na.value = "grey85"
  ) +
  labs(
    title    = "Real Housing-Wealth Recovery Index by PUMA, 2012--2021",
    subtitle = paste0(
      "Recovery Index = 2021 median value / CPI-adj. 2012 median value. ",
      "Index > 1.0 = real appreciation. Mean = ",
      round(mean(map_df$recovery_index, na.rm = TRUE), 3), "; Median = ",
      round(median(map_df$recovery_index, na.rm = TRUE), 3), "."
    ),
    caption = paste0(
      "Data: processed/analysis_dataset.csv; ", MAP_CAPTION
    )
  ) +
  map_theme()
save_fig(fig2, "fig2_recovery_map.png")

# ===========================================================================
# Figure 3: PUMA scatter -- recession drop vs COVID gain (no map)
# ===========================================================================
cat("--- Figure 3: Recession vs COVID scatter ---\n")

fig3 <- ggplot(puma_rec, aes(x = pct_chg_0709, y = pct_chg_1921, color = decimated)) +
  geom_point(alpha = 0.25, size = 0.9, stroke = 0) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.1, alpha = 0.18) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  scale_color_manual(
    values = c("FALSE" = "#636e72", "TRUE" = "#c0392b"),
    labels = c("FALSE" = "Other PUMAs (upper 75%)",
               "TRUE"  = "Hardest-Hit (worst 25% recession decline)")
  ) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    x       = "Great Recession change in home value, 2007 to 2009 (%)",
    y       = "COVID-era change in home value, 2019 to 2021 (%)",
    color   = NULL,
    caption = paste0(
      "Data: processed/puma_recession_recovery.csv. ",
      "Decimation cutoff: -9.67% (25th percentile of 2007-09 change). ",
      "N = ", comma(nrow(puma_rec)), " PUMAs. OLS fit with 95% CI."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 7.5, color = "grey50", hjust = 0)
  )
save_fig(fig3, "fig3_puma_scatter.png", w = 8, h = 6)

# ===========================================================================
# Figure 4: LISA cluster map -- Albers Equal Area
# ===========================================================================
cat("--- Figure 4: LISA cluster map ---\n")

moran_i   <- round(moran_res$estimate[1], 4)
moran_lbl <- sprintf("Global Moran's I = %.4f  (p < 0.001, queen contiguity, EPSG:5070)", moran_i)

# Assign "Not Significant" for any PUMA not in the LISA results
conus_lisa <- conus %>%
  mutate(lisa_type = if_else(is.na(lisa_type), "Not Significant", lisa_type))

fig4 <- ggplot() +
  geom_sf(data  = states_conus,
          fill  = "#f0ede8", color = "white", linewidth = 0.4) +
  geom_sf(data  = conus_lisa,
          aes(fill = lisa_type), color = NA) +
  geom_sf(data  = states_conus,
          fill  = NA, color = "white", linewidth = 0.25) +
  scale_fill_manual(
    values = c(
      "High-High"                  = "#c0392b",
      "Low-Low (Unrecovered Core)" = "#2980b9",
      "Spatial Outlier"            = "#f39c12",
      "Not Significant"            = "grey88"
    ),
    name = "LISA Cluster"
  ) +
  labs(
    title    = "Local Spatial Clusters of Housing Recovery (LISA)",
    subtitle = moran_lbl,
    caption  = paste0(
      "Data: processed/results_moran_lisa.csv; processed/lisa_cluster_assignments.csv. ",
      "LISA significance at 5 percent (conditional randomization, n = 999). ",
      MAP_CAPTION
    )
  ) +
  map_theme(legend.position = "right")
save_fig(fig4, "fig4_lisa_map.png")

# ===========================================================================
# Figure 5: K-means cluster typology map -- Albers Equal Area
# ===========================================================================
cat("--- Figure 5: Cluster typology map ---\n")

map_clust <- map_df %>%
  filter(!is.na(cluster_label)) %>%
  mutate(cluster_label = factor(cluster_label,
    levels = c("Persistently Depressed", "Below-Average Recovery",
               "Above-Average Recovery", "Boom Markets")))

fig5 <- ggplot() +
  geom_sf(data  = states_bg,
          fill  = "#f0ede8", color = "white", linewidth = 0.4) +
  geom_sf(data  = map_clust,
          aes(fill = cluster_label), color = NA) +
  geom_sf(data  = states_bg,
          fill  = NA, color = "white", linewidth = 0.25) +
  scale_fill_brewer(palette = "RdYlGn", name = "Recovery\nCluster") +
  labs(
    title    = "Housing Recovery Typology by PUMA, 2012--2021",
    subtitle = "K-means clusters (k = 4, seed = 2026) on six standardized economic and risk features",
    caption  = paste0(
      "Data: processed/kmeans_cluster_assignments.csv. ",
      MAP_CAPTION
    )
  ) +
  map_theme(legend.position = "right",
            legend.text = element_text(size = 9))
save_fig(fig5, "fig5_cluster_map.png")

# ===========================================================================
# Figure 6: Compound disadvantage scatter (no map)
# ===========================================================================
cat("--- Figure 6: Compound disadvantage scatter ---\n")

compound_df <- analysis %>%
  filter(!is.na(cluster_label), !is.na(sovi_score), !is.na(risk_score)) %>%
  mutate(cluster_label = factor(cluster_label,
    levels = c("Persistently Depressed", "Below-Average Recovery",
               "Above-Average Recovery", "Boom Markets")))

fig6 <- ggplot(compound_df, aes(x = sovi_score, y = recovery_index, color = cluster_label)) +
  geom_point(alpha = 0.30, size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.2, span = 0.8) +
  scale_color_brewer(palette = "RdYlGn", name = "Recovery Cluster", direction = -1) +
  labs(
    x       = "FEMA Social Vulnerability Score (0--100; higher = more vulnerable)",
    y       = "Recovery Index (2021 / CPI-adj. 2012)",
    caption = paste0(
      "Data: processed/analysis_dataset.csv; processed/kmeans_cluster_assignments.csv. ",
      "SOVI from FEMA NRI v1.20 (raw/fema_nri/NRI_Table_CensusTracts.csv), ",
      "aggregated via processed/nri_puma.csv. N = ", comma(nrow(compound_df)), " PUMAs."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 7, color = "grey50", hjust = 0)
  ) +
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 3, alpha = 0.9)))
save_fig(fig6, "fig6_compound_disadvantage.png", w = 8, h = 6)

# ===========================================================================
# Appendix Figure A1: Income group recovery bar chart (no map)
# ===========================================================================
cat("--- Appendix Figure A1: Income group recovery ---\n")

rec_sum <- state_inc %>%
  group_by(income_group) %>%
  summarise(share = mean(recovered_07), se = sd(recovered_07) / sqrt(n()),
            .groups = "drop") %>%
  mutate(income_group = factor(income_group,
    levels = c("Low", "Middle", "High"),
    labels = c("Low Income\n(Bottom Quintile)",
               "Middle Income\n(Quintiles 2-4)",
               "High Income\n(Top Quintile)")))

figa1 <- ggplot(rec_sum, aes(x = income_group, y = share, fill = income_group)) +
  geom_col(width = 0.55, color = "white") +
  geom_errorbar(aes(ymin = share - 1.96 * se, ymax = share + 1.96 * se),
                width = 0.2, linewidth = 0.7, color = "grey30") +
  scale_fill_manual(values = c("#c0392b", "#3498db", "#27ae60"), guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1.05), expand = c(0, 0)) +
  labs(
    x = NULL,
    y = "Share of state-income cells nominally recovered",
    caption = paste0(
      "Data: processed/state_income_analysis.csv. A cell is recovered if the 2021 ",
      "HHWT-weighted mean home value >= 2007 value (nominal). ",
      "Income groups: state-specific, year-specific quintile thresholds. ",
      "Error bars: 95% CI. N = 51 per group."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(size = 7.5, color = "grey50", hjust = 0)
  )
save_fig(figa1, "figa1_income_recovery.png", w = 6.5, h = 5)

cat("\n=== All figures saved to", FIG_DIR, "===\n")
cat("Map projection: Albers Equal Area Conic (EPSG:5070)\n")
cat("State backgrounds: dissolved from PUMA polygons (no raster tiles)\n")
