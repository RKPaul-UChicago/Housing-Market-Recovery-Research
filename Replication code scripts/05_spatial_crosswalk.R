# =============================================================================
# 05_spatial_crosswalk.R
# Build the tract-to-PUMA spatial crosswalk using Census Gazetteer interior
# centroids and TIGER/Line PUMA polygons (point-in-polygon join). Aggregate
# FEMA National Risk Index scores from tract to PUMA level using
# population-weighted means.
# Outputs:
#   processed/nri_puma.csv  -- PUMA-level NRI scores with crosswalk
# =============================================================================

source("00_setup.R")

cat("\n--- Loading TIGER/Line PUMA shapefiles ---\n")
shp_files <- list.files(SHP_ROOT, pattern = "_puma10[.]shp$",
                         recursive = TRUE, full.names = TRUE)
cat("Shapefiles found:", length(shp_files), "\n")
pumas <- do.call(rbind, lapply(shp_files, function(f) {
  s <- st_read(f, quiet = TRUE)
  s[, c("STATEFP10", "PUMACE10", "GEOID10", "NAMELSAD10")]
}))
cat("PUMA polygons loaded:", nrow(pumas), "\n")
cat("CRS:", st_crs(pumas)$input, "\n")

cat("--- Loading Census Gazetteer tract centroids ---\n")
gaz <- read_tsv(GAZ_PATH, col_types = cols(.default = col_character()),
                show_col_types = FALSE)
names(gaz) <- trimws(names(gaz))

gaz_sf <- gaz %>%
  transmute(
    tract_geoid = GEOID,
    lon         = as.numeric(INTPTLONG),
    lat         = as.numeric(INTPTLAT)
  ) %>%
  filter(is.finite(lon), is.finite(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(pumas))
cat("Tract centroids:", nrow(gaz_sf), "\n")

cat("--- Spatial join: tract centroid inside PUMA polygon ---\n")
xwalk <- st_join(gaz_sf, pumas[, "GEOID10"], join = st_within) %>%
  st_drop_geometry() %>%
  filter(!is.na(GEOID10)) %>%
  rename(puma_geoid = GEOID10)
cat("Tracts matched to a PUMA:", nrow(xwalk), "of", nrow(gaz_sf), "\n")

cat("--- Loading FEMA NRI ---\n")
nri_raw <- read_csv(
  NRI_PATH,
  col_types      = cols(TRACTFIPS = col_character(), .default = col_double()),
  show_col_types = FALSE
) %>%
  mutate(tract_geoid = stringr::str_pad(TRACTFIPS, 11, pad = "0"))
cat("NRI tract records:", nrow(nri_raw), "\n")

cat("--- Aggregating NRI to PUMA via population-weighted means ---\n")
nri_puma <- nri_raw %>%
  inner_join(xwalk, by = "tract_geoid") %>%
  filter(!is.na(POPULATION), POPULATION > 0) %>%
  group_by(puma_geoid) %>%
  summarise(
    risk_score    = weighted.mean(RISK_SCORE, POPULATION, na.rm = TRUE),
    sovi_score    = weighted.mean(SOVI_SCORE, POPULATION, na.rm = TRUE),
    resl_score    = weighted.mean(RESL_SCORE, POPULATION, na.rm = TRUE),
    nri_pop       = sum(POPULATION, na.rm = TRUE),
    nri_tracts    = n(),
    .groups = "drop"
  )
cat("PUMAs with NRI data:", nrow(nri_puma), "\n")
cat("risk_score range:", round(range(nri_puma$risk_score, na.rm=T), 2), "\n")
cat("sovi_score range:", round(range(nri_puma$sovi_score, na.rm=T), 2), "\n")
cat("resl_score range:", round(range(nri_puma$resl_score, na.rm=T), 2), "\n")

write_csv(nri_puma, processed_path("nri_puma.csv"))
cat("Saved: nri_puma.csv\n")

# Also export pumas as a summary CSV (without geometry) for reference
pumas_df <- pumas %>%
  st_drop_geometry() %>%
  transmute(
    puma_geoid = GEOID10,
    state_fips = STATEFP10,
    puma_code  = PUMACE10,
    puma_name  = NAMELSAD10
  )
write_csv(pumas_df, processed_path("puma_names.csv"))
cat("Saved: puma_names.csv  (", nrow(pumas_df), "PUMAs)\n")

cat("\n05_spatial_crosswalk.R complete.\n")
