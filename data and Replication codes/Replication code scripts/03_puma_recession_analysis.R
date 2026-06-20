# =============================================================================
# 03_puma_recession_analysis.R
# PUMA-level 2007-2021 nominal recovery analysis. Constructs PUMA-level
# weighted mean home values for 2007, 2009, 2019, 2021; identifies
# "decimated" PUMAs (bottom quartile 2007-2009 decline); and classifies
# whether each PUMA recovered to its pre-crisis nominal value by 2021.
# Outputs:
#   processed/puma_recession_recovery.csv  -- PUMA-level changes and flags
#   processed/results_recovery_by_decimation.csv  -- summary statistics
#   processed/national_value_trends.csv  -- national raw weighted median/mean
#                                           home value by year (Table 2 / Figure 1)
# =============================================================================

source("00_setup.R")
source("01_load_clean_acs.R")

cat("\n--- Winsorizing VALUEH within PUMA x YEAR ---\n")
acs_winsor <- acs_main %>%
  group_by(puma_geoid, YEAR) %>%
  mutate(
    q01      = quantile(VALUEH, 0.01, na.rm = TRUE),
    q99      = quantile(VALUEH, 0.99, na.rm = TRUE),
    VALUEH_w = pmin(pmax(VALUEH, q01), q99)
  ) %>%
  ungroup()

cat("--- PUMA x year weighted means ---\n")
puma_yr <- acs_winsor %>%
  group_by(puma_geoid, YEAR, STATEFIP) %>%
  summarise(
    mean_val = weighted.mean(VALUEH_w, HHWT, na.rm = TRUE),
    med_val  = wtd_median(VALUEH_w, HHWT),
    med_inc  = wtd_median(HHINCOME, HHWT),
    n_hh     = n(),
    .groups = "drop"
  )

cat("--- Reshaping wide ---\n")
puma_wide <- puma_yr %>%
  filter(YEAR %in% c(2007, 2009, 2019, 2021)) %>%
  pivot_wider(
    id_cols     = c(puma_geoid, STATEFIP),
    names_from  = YEAR,
    values_from = c(mean_val, med_val, med_inc, n_hh)
  ) %>%
  drop_na(mean_val_2007, mean_val_2009, mean_val_2019, mean_val_2021) %>%
  mutate(
    pct_chg_0709  = 100 * (mean_val_2009 - mean_val_2007) / mean_val_2007,
    pct_chg_1921  = 100 * (mean_val_2021 - mean_val_2019) / mean_val_2019,
    pct_chg_0721  = 100 * (mean_val_2021 - mean_val_2007) / mean_val_2007,
    abs_gain_0721 = mean_val_2021 - mean_val_2007,
    recovered_07  = as.integer(mean_val_2021 >= mean_val_2007)
  )

decim_cutoff <- quantile(puma_wide$pct_chg_0709, 0.25, na.rm = TRUE)
cat("Decimation cutoff (25th percentile, 2007-09 pct change):", round(decim_cutoff, 2), "%\n")

puma_wide <- puma_wide %>%
  mutate(decimated = pct_chg_0709 <= decim_cutoff)

write_csv(puma_wide, processed_path("puma_recession_recovery.csv"))
cat("Saved: puma_recession_recovery.csv  (", nrow(puma_wide), "PUMAs)\n")

cat("--- Summary by decimation status ---\n")
dec_summary <- puma_wide %>%
  group_by(decimated) %>%
  summarise(
    n                    = n(),
    pct_recovered        = round(100 * mean(recovered_07), 2),
    avg_pct_chg_0709     = round(mean(pct_chg_0709), 2),
    avg_pct_chg_1921     = round(mean(pct_chg_1921), 2),
    avg_pct_chg_0721     = round(mean(pct_chg_0721), 2),
    avg_abs_gain_0721    = round(mean(abs_gain_0721)),
    .groups = "drop"
  ) %>%
  mutate(group_label = if_else(decimated, "Decimated (bottom 25%)", "Other PUMAs"))

write_csv(dec_summary, processed_path("results_recovery_by_decimation.csv"))
cat("Saved: results_recovery_by_decimation.csv\n")
print(dec_summary)

# --- National raw (unwinsorized) weighted value trends by year (Table 2 / Figure 1) ---
cat("--- National value trends (raw, weighted) ---\n")
national_trends <- acs_main %>%
  filter(YEAR %in% c(2007, 2009, 2019, 2021)) %>%
  group_by(year = YEAR) %>%
  summarise(
    median_value = round(wtd_median(VALUEH, HHWT)),
    mean_value   = round(weighted.mean(VALUEH, HHWT, na.rm = TRUE)),
    n_hh         = n(),
    .groups = "drop"
  )
write_csv(national_trends, processed_path("national_value_trends.csv"))
cat("Saved: national_value_trends.csv\n")
print(national_trends)

cat("\n03_puma_recession_analysis.R complete.\n")
