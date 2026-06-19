# =============================================================================
# 11_build_crosswalk.R
# Population-weighted county -> PUMA crosswalk.
#
# County-level series (FHFA county HPI in script 14, HMDA high-cost shares in
# script 13) must be mapped onto the PUMA units used throughout the paper.
# Counties and PUMAs do not nest, so we build a weighted bridge: each
# (PUMA, county) pair gets a weight equal to the share of the PUMA's 2010
# population that falls in that county. A PUMA-level value is then the
# population-weighted average of the county values overlapping it.
#
# Inputs:
#   raw/crosswalks/2010_Census_Tract_to_2010_PUMA.txt  (Census relationship file)
#   2010 Census SF1 tract population P001001            (Census Data API)
#
# The Census API key is read from the CENSUS_API_KEY environment variable or
# from ~/.census_key. A key is free at https://api.census.gov/data/key_signup.html
# (only needed to REBUILD this file; the saved output ships in the archive).
#
# Output:
#   processed/county_to_puma_xwalk.csv
#     puma_geoid (7), county_geoid (5), inter_pop, puma_pop, w  (w sums to 1 per PUMA)
# =============================================================================

source("00_setup.R")
library(jsonlite)

# ---------------------------------------------------------------------------
# 1. Census API key
# ---------------------------------------------------------------------------
census_key <- Sys.getenv("CENSUS_API_KEY")
if (!nzchar(census_key) && file.exists("~/.census_key"))
  census_key <- readLines("~/.census_key", warn = FALSE)[1]
if (!nzchar(census_key))
  stop("Set CENSUS_API_KEY (env var) or ~/.census_key to rebuild the crosswalk.")

# ---------------------------------------------------------------------------
# 2. Tract -> PUMA relationship file
# ---------------------------------------------------------------------------
rel <- read_csv(raw_path("crosswalks", "2010_tract_to_2010_puma.txt"),
                col_types = cols(.default = "c"))
names(rel) <- tolower(names(rel))   # statefp, countyfp, tractce, puma5ce
rel <- rel %>%
  mutate(tract_geoid  = paste0(statefp, countyfp, tractce),
         county_geoid = paste0(statefp, countyfp),
         puma_geoid   = paste0(statefp, puma5ce))
cat("Tract->PUMA rows:", nrow(rel), "\n")

# ---------------------------------------------------------------------------
# 3. 2010 tract population from the Census API (loop over states)
# ---------------------------------------------------------------------------
states <- sort(unique(rel$statefp))
cat("Querying tract population for", length(states), "state FIPS...\n")

get_state_pop <- function(st) {
  url <- sprintf(
    "https://api.census.gov/data/2010/dec/sf1?get=P001001&for=tract:*&in=state:%s&key=%s",
    st, census_key)
  out <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
  if (is.null(out)) { message("  skip state ", st); return(NULL) }
  df <- as.data.frame(out[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(df) <- out[1, ]
  df %>% transmute(
    tract_geoid = paste0(state, county, tract),
    pop = as.numeric(P001001))
}

pop <- purrr::map_dfr(states, get_state_pop)
cat("Tract population records retrieved:", nrow(pop), "\n")

# ---------------------------------------------------------------------------
# 4. Join population and build (PUMA, county) population weights
# ---------------------------------------------------------------------------
xwalk <- rel %>%
  left_join(pop, by = "tract_geoid") %>%
  mutate(pop = ifelse(is.na(pop), 0, pop)) %>%
  group_by(puma_geoid, county_geoid) %>%
  summarise(inter_pop = sum(pop), .groups = "drop") %>%
  group_by(puma_geoid) %>%
  mutate(puma_pop = sum(inter_pop),
         w = ifelse(puma_pop > 0, inter_pop / puma_pop, 0)) %>%
  ungroup() %>%
  filter(inter_pop > 0)

# sanity: weights sum to 1 within each PUMA
chk <- xwalk %>% group_by(puma_geoid) %>% summarise(s = sum(w), .groups = "drop")
cat("PUMAs:", nrow(chk),
    "| weight-sum range:", round(min(chk$s), 4), "to", round(max(chk$s), 4), "\n")
cat("County-PUMA pairs:", nrow(xwalk), "\n")

write_csv(xwalk, processed_path("county_to_puma_xwalk.csv"))
cat("Saved: county_to_puma_xwalk.csv\n")
