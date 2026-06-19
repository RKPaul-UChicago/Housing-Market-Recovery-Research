# =============================================================================
# 14_fhfa_outcome.R
# Transaction-based house-price outcome from the FHFA county HPI.
#
# Replaces the self-reported ACS home values with the FHFA All-Transactions
# county House Price Index (repeat-sales / appraisal based). Within a county,
# price CHANGES are independent of the index base, so we use the raw index.
#
#   recession change  = 100 * (HPI_2009 / HPI_2007 - 1)
#   COVID-era change  = 100 * (HPI_2021 / HPI_2019 - 1)
#   trough-to-peak    = 100 * (HPI_2021 / HPI_2012 - 1)
#
# County series are population-weighted to PUMA via the crosswalk.
#
# Input:  raw/fhfa_hpi/hpi_at_county.xlsx ; processed/county_to_puma_xwalk.csv
# Output: processed/fhfa_puma_outcomes.csv
#           puma_geoid, fhfa_rec_0709, fhfa_covid_1921, fhfa_trough_1221
# =============================================================================

source("00_setup.R")
library(readxl)

cat("\n--- Reading FHFA county HPI ---\n")
# Header is on row 6 (5 metadata rows precede it)
fhfa <- read_excel(raw_path("fhfa_hpi", "hpi_at_county.xlsx"), sheet = 1, skip = 5)
names(fhfa) <- c("state", "county", "county_geoid", "year",
                 "annual_chg", "hpi", "hpi90", "hpi00")
fhfa <- fhfa %>%
  mutate(county_geoid = as.character(county_geoid),
         year = as.integer(year),
         hpi  = suppressWarnings(as.numeric(hpi))) %>%
  filter(year %in% c(2007, 2009, 2012, 2019, 2021), !is.na(hpi))

wide <- fhfa %>%
  select(county_geoid, year, hpi) %>%
  tidyr::pivot_wider(names_from = year, values_from = hpi, names_prefix = "h") %>%
  filter(!is.na(h2007), !is.na(h2009), !is.na(h2019), !is.na(h2021)) %>%
  mutate(fhfa_rec_0709   = 100 * (h2009 / h2007 - 1),
         fhfa_covid_1921 = 100 * (h2021 / h2019 - 1),
         fhfa_trough_1221 = ifelse(!is.na(h2012), 100 * (h2021 / h2012 - 1), NA_real_))
cat("Counties with complete 2007-2021 HPI:", nrow(wide), "\n")
cat("Recession change 07-09: median", round(median(wide$fhfa_rec_0709), 1),
    "%, COVID change 19-21: median", round(median(wide$fhfa_covid_1921), 1), "%\n")

# county-level output (used by the county-level causal analysis)
write_csv(wide %>% select(county_geoid, h2007, fhfa_rec_0709,
                          fhfa_covid_1921, fhfa_trough_1221),
          processed_path("fhfa_county_outcomes.csv"))
cat("Saved: fhfa_county_outcomes.csv\n")

# ---------------------------------------------------------------------------
# County -> PUMA (population-weighted average of county changes)
# ---------------------------------------------------------------------------
xw <- read_csv(processed_path("county_to_puma_xwalk.csv"),
               col_types = cols(puma_geoid = "c", county_geoid = "c",
                                w = "d", .default = "d"))

puma_out <- xw %>%
  inner_join(wide, by = "county_geoid") %>%
  group_by(puma_geoid) %>%
  summarise(
    wsum            = sum(w),
    fhfa_rec_0709   = sum(w * fhfa_rec_0709)   / wsum,
    fhfa_covid_1921 = sum(w * fhfa_covid_1921) / wsum,
    fhfa_trough_1221 = sum(w * fhfa_trough_1221) / wsum,
    .groups = "drop") %>%
  filter(wsum > 0.5) %>%                       # PUMA mostly covered by matched counties
  mutate(puma_geoid = as.numeric(puma_geoid)) %>%
  select(puma_geoid, fhfa_rec_0709, fhfa_covid_1921, fhfa_trough_1221)

cat("PUMAs with FHFA outcome:", nrow(puma_out), "\n")
write_csv(puma_out, processed_path("fhfa_puma_outcomes.csv"))
cat("Saved: fhfa_puma_outcomes.csv\n")
