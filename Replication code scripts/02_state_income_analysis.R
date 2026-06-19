# =============================================================================
# 02_state_income_analysis.R
# State-level income group analysis: assign households to state-specific
# quintile groups, compute weighted mean home values, calculate percentage
# changes across recession and COVID periods, and run OLS regressions.
# Outputs:
#   processed/state_income_analysis.csv   -- state x year x income group means
#   processed/results_ols_state.csv       -- OLS regression results (Table 5)
# =============================================================================

source("00_setup.R")
source("01_load_clean_acs.R")

cat("\n--- State income quintile thresholds ---\n")
income_cutoffs <- acs_main %>%
  group_by(STATEFIP, YEAR) %>%
  summarise(
    p20 = as.numeric(wtd.quantile(HHINCOME, weights = HHWT, probs = 0.20, na.rm = TRUE)),
    p80 = as.numeric(wtd.quantile(HHINCOME, weights = HHWT, probs = 0.80, na.rm = TRUE)),
    .groups = "drop"
  )

acs_inc <- acs_main %>%
  left_join(income_cutoffs, by = c("STATEFIP", "YEAR")) %>%
  mutate(
    income_group = case_when(
      HHINCOME <= p20 ~ "Low",
      HHINCOME >= p80 ~ "High",
      TRUE             ~ "Middle"
    )
  )

cat("--- Winsorizing and computing state-year-group means ---\n")
state_means <- acs_inc %>%
  group_by(STATEFIP, YEAR, income_group) %>%
  mutate(
    q01    = quantile(VALUEH, 0.01, na.rm = TRUE),
    q99    = quantile(VALUEH, 0.99, na.rm = TRUE),
    VALUEH_w = pmin(pmax(VALUEH, q01), q99)
  ) %>%
  ungroup() %>%
  group_by(STATEFIP, YEAR, income_group) %>%
  summarise(
    mean_value   = weighted.mean(VALUEH_w, HHWT, na.rm = TRUE),
    median_value = wtd_median(VALUEH_w, HHWT),
    n_hh         = n(),
    .groups = "drop"
  )

cat("--- Reshaping wide and computing changes ---\n")
state_wide <- state_means %>%
  filter(YEAR %in% c(2007, 2009, 2019, 2021)) %>%
  pivot_wider(
    id_cols     = c(STATEFIP, income_group),
    names_from  = YEAR,
    values_from = c(mean_value, median_value, n_hh)
  ) %>%
  drop_na(mean_value_2007, mean_value_2009, mean_value_2019, mean_value_2021) %>%
  mutate(
    pct_chg_0709 = 100 * (mean_value_2009 - mean_value_2007) / mean_value_2007,
    pct_chg_1921 = 100 * (mean_value_2021 - mean_value_2019) / mean_value_2019,
    pct_chg_0721 = 100 * (mean_value_2021 - mean_value_2007) / mean_value_2007,
    abs_gain_0721 = mean_value_2021 - mean_value_2007,
    recovered_07  = as.integer(mean_value_2021 >= mean_value_2007)
  )

write_csv(state_wide, processed_path("state_income_analysis.csv"))
cat("Saved: state_income_analysis.csv  (", nrow(state_wide), "rows )\n")

cat("--- OLS regressions: pct_chg_1921 ~ pct_chg_0709 by income group ---\n")
run_ols <- function(df, grp) {
  m <- lm(pct_chg_1921 ~ pct_chg_0709, data = df)
  s <- summary(m)
  tibble(
    income_group  = grp,
    n             = nrow(df),
    intercept     = coef(m)[1],
    beta          = coef(m)[2],
    se_beta       = s$coefficients[2, 2],
    t_stat        = s$coefficients[2, 3],
    p_value       = s$coefficients[2, 4],
    r_squared     = s$r.squared,
    adj_r_squared = s$adj.r.squared
  )
}

ols_results <- bind_rows(
  run_ols(filter(state_wide, income_group == "Low"),    "Low (bottom quintile)"),
  run_ols(filter(state_wide, income_group == "Middle"), "Middle (quintiles 2-4)"),
  run_ols(filter(state_wide, income_group == "High"),   "High (top quintile)")
)

write_csv(ols_results, processed_path("results_ols_state.csv"))
cat("Saved: results_ols_state.csv\n")

cat("\n=== OLS Results Summary ===\n")
ols_results %>%
  select(income_group, n, beta, se_beta, p_value, r_squared) %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  print()

cat("\n02_state_income_analysis.R complete.\n")
