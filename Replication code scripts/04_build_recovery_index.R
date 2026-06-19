# =============================================================================
# 04_build_recovery_index.R
# Build the real housing-wealth recovery index from the 2012/2021 ACS data.
# Computes PUMA-level CPI-adjusted recovery index = 2021 median value /
# (2012 median value x CPI factor).
# Outputs:
#   processed/puma_recovery_index.csv  -- PUMA-level recovery index
# =============================================================================

source("00_setup.R")
source("01_load_clean_acs.R")

cat("\n--- PUMA x year weighted medians from GIS extract ---\n")
pv <- acs_gis %>%
  group_by(YEAR, puma_geoid, STATEFIP) %>%
  summarise(
    med_value = wtd_median(VALUEH, HHWT),
    med_inc   = wtd_median(
      ifelse(HHINCOME >= 0 & HHINCOME < 9999999, HHINCOME, NA_real_), HHWT
    ),
    n_hh      = n(),
    .groups   = "drop"
  )

cat("--- Reshaping wide ---\n")
ri_df <- pv %>%
  pivot_wider(
    names_from  = YEAR,
    values_from = c(med_value, med_inc, n_hh)
  ) %>%
  filter(!is.na(med_value_2012), !is.na(med_value_2021)) %>%
  mutate(
    real_value_2012 = med_value_2012 * CPI_FACTOR,
    recovery_index  = med_value_2021 / real_value_2012,
    pct_change_real = 100 * (recovery_index - 1),
    recovery_q      = ntile(recovery_index, 4),
    depressed       = recovery_q == 1L
  )

cat("PUMAs with both years:", nrow(ri_df), "\n")
cat("Recovery index -- mean  :", round(mean(ri_df$recovery_index, na.rm=T), 3), "\n")
cat("Recovery index -- median:", round(median(ri_df$recovery_index, na.rm=T), 3), "\n")
cat("Recovery index -- SD    :", round(sd(ri_df$recovery_index, na.rm=T), 3), "\n")
cat("Recovery index -- min   :", round(min(ri_df$recovery_index, na.rm=T), 3), "\n")
cat("Recovery index -- max   :", round(max(ri_df$recovery_index, na.rm=T), 3), "\n")
cat("Share RI < 1 (real decline):", round(100 * mean(ri_df$recovery_index < 1, na.rm=T), 1), "%\n")
cat("Share RI > 1.5 (strong gain):", round(100 * mean(ri_df$recovery_index > 1.5, na.rm=T), 1), "%\n")
cat("Depressed (bottom quartile):", sum(ri_df$depressed, na.rm=T), "PUMAs\n")

write_csv(ri_df, processed_path("puma_recovery_index.csv"))
cat("Saved: puma_recovery_index.csv\n")

cat("\n04_build_recovery_index.R complete.\n")
