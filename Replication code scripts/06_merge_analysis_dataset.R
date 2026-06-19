# =============================================================================
# 06_merge_analysis_dataset.R
# Merge all processed components into the main analysis dataset:
#   recovery index + NRI scores + puma names
# Also merges 2007-2021 nominal changes from 03 where puma_geoid overlaps.
# Outputs:
#   processed/analysis_dataset.csv  -- main flat analysis file (all PUMAs)
# =============================================================================

source("00_setup.R")

cat("\n--- Reading processed files ---\n")
ri      <- read_csv(processed_path("puma_recovery_index.csv"),      show_col_types = FALSE)
nri     <- read_csv(processed_path("nri_puma.csv"),                  show_col_types = FALSE)
pnames  <- read_csv(processed_path("puma_names.csv"),                show_col_types = FALSE)
prec    <- read_csv(processed_path("puma_recession_recovery.csv"),   show_col_types = FALSE) %>%
  select(puma_geoid, pct_chg_0709, pct_chg_1921, pct_chg_0721,
         abs_gain_0721, recovered_07, decimated)

cat("Recovery index rows:", nrow(ri), "\n")
cat("NRI PUMA rows:", nrow(nri), "\n")
cat("Recession recovery rows:", nrow(prec), "\n")

analysis <- ri %>%
  left_join(nri,    by = "puma_geoid") %>%
  left_join(pnames, by = "puma_geoid") %>%
  left_join(prec,   by = "puma_geoid")

cat("Final analysis dataset:", nrow(analysis), "PUMAs\n")
cat("PUMAs with NRI data:", sum(!is.na(analysis$risk_score)), "\n")
cat("PUMAs with recession data:", sum(!is.na(analysis$pct_chg_0709)), "\n")

write_csv(analysis, processed_path("analysis_dataset.csv"))
cat("Saved: analysis_dataset.csv\n")

cat("\nColumn list:\n")
cat(paste(names(analysis), collapse = "\n"), "\n")

cat("\n06_merge_analysis_dataset.R complete.\n")
