# =============================================================================
# 08_spatial_lag_model.R
# Estimate the spatial lag model (equation 6 in paper) on the PUMA-level
# recovery index. Compare against OLS to assess residual spatial dependence.
# Outputs:
#   processed/results_spatial_lag.csv  -- spatial lag model coefficients
# =============================================================================

source("00_setup.R")
library(spatialreg)

cat("\n--- Loading PUMA geometry and analysis dataset ---\n")
shp_files <- list.files(SHP_ROOT, pattern = "_puma10[.]shp$",
                         recursive = TRUE, full.names = TRUE)
pumas <- do.call(rbind, lapply(shp_files, function(f) {
  s <- st_read(f, quiet = TRUE)
  s[, c("STATEFP10", "PUMACE10", "GEOID10", "NAMELSAD10")]
}))
analysis_df <- read_csv(processed_path("analysis_dataset.csv"), show_col_types = FALSE)
analysis_sf <- pumas %>%
  inner_join(analysis_df, by = c("GEOID10" = "puma_geoid")) %>%
  rename(puma_geoid = GEOID10)

conus <- analysis_sf %>%
  filter(!as.integer(STATEFP10) %in% c(2L, 15L, 72L),
         !st_is_empty(geometry)) %>%
  filter(!is.na(recovery_index), !is.na(risk_score), !is.na(sovi_score)) %>%
  mutate(log_med_inc = log(pmax(med_inc_2021, 1))) %>%
  st_transform(5070)
cat("CONUS PUMAs for SLM:", nrow(conus), "\n")

nb  <- poly2nb(conus, queen = TRUE)
lw  <- nb2listw(nb, style = "W", zero.policy = TRUE)

cat("--- OLS baseline ---\n")
ols_mod <- lm(recovery_index ~ real_value_2012 + log_med_inc + risk_score + sovi_score,
              data = conus %>% st_drop_geometry())
cat("OLS Moran's I test on residuals:\n")
mi_ols <- moran.test(residuals(ols_mod), lw, zero.policy = TRUE)
print(mi_ols)

cat("--- Spatial lag model ---\n")
slm <- lagsarlm(
  recovery_index ~ real_value_2012 + log_med_inc + risk_score + sovi_score,
  data        = conus %>% st_drop_geometry(),
  listw       = lw,
  zero.policy = TRUE
)
s_slm <- summary(slm)
cat("SLM rho:", round(slm$rho, 4), "  p-value:", round(s_slm$rho.se, 4), "\n")

coef_table <- s_slm$Coef                          # matrix: rows = terms, cols include Estimate, Std. Error
coef_names <- rownames(coef_table)
slm_results <- tibble(
  term             = c("rho (spatial lag)", coef_names),
  estimate         = c(slm$rho, coef_table[, "Estimate"]),
  std_error        = c(s_slm$rho.se, coef_table[, "Std. Error"]),
  z_value          = c(NA_real_,  coef_table[, "z value"]),
  p_value          = c(NA_real_,  coef_table[, "Pr(>|z|)"]),
  n_obs            = nrow(conus),
  ols_moran_i      = round(mi_ols$estimate["Moran I statistic"], 4),
  ols_moran_pvalue = mi_ols$p.value,
  model            = "Spatial Lag (Queen contiguity, row-std. W)"
)
write_csv(slm_results, processed_path("results_spatial_lag.csv"))
cat("Saved: results_spatial_lag.csv\n")

cat("\n08_spatial_lag_model.R complete.\n")
