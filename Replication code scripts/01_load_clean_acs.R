# =============================================================================
# 01_load_clean_acs.R
# Load and clean both IPUMS ACS extracts. Apply sample restrictions and
# construct PUMA identifiers. Print verification counts.
# Outputs: none (objects used by downstream scripts via source())
# =============================================================================

source("00_setup.R")

cat("\n--- Loading ACS Extract 1 (2007 / 2009 / 2019 / 2021) ---\n")
acs_main_raw <- read_csv(ACS_MAIN, show_col_types = FALSE)
names(acs_main_raw) <- toupper(names(acs_main_raw))

acs_main <- acs_main_raw %>%
  filter(GQ %in% c(1L, 2L)) %>%
  filter(OWNERSHP == 1L) %>%
  filter(HHINCOME != 9999999) %>%
  filter(!VALUEH %in% c(0, 9999998, 9999999)) %>%
  mutate(puma_geoid = sprintf("%02d%05d", STATEFIP, PUMA))

cat("Rows after cleaning:", format(nrow(acs_main), big.mark = ","), "\n")
cat("Years:", paste(sort(unique(acs_main$YEAR)), collapse = ", "), "\n")
cat("States:", length(unique(acs_main$STATEFIP)), "\n")
cat("Unique PUMAs:", length(unique(acs_main$puma_geoid)), "\n")
stopifnot(nrow(acs_main) == 8955228)  # verification checkpoint

cat("\n--- Loading ACS Extract 2 (2012 / 2021) ---\n")
acs_gis_raw <- read_csv(ACS_GIS, show_col_types = FALSE)
names(acs_gis_raw) <- toupper(names(acs_gis_raw))

acs_gis <- acs_gis_raw %>%
  distinct(YEAR, SERIAL, .keep_all = TRUE) %>%
  filter(OWNERSHP == 1L, VALUEH > 0, VALUEH < 9999999) %>%
  mutate(puma_geoid = sprintf("%02d%05d", STATEFIP, PUMA))

cat("Rows after cleaning:", format(nrow(acs_gis), big.mark = ","), "\n")
cat("Years:", paste(sort(unique(acs_gis$YEAR)), collapse = ", "), "\n")
stopifnot(nrow(acs_gis) == 1769702)

cat("\n01_load_clean_acs.R complete.\n")
