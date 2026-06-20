# =============================================================================
# 13_hmda_creditsupply.R
# Pre-crisis credit-supply instrument: high-cost (higher-priced) lending share.
#
# Markets where a large share of pre-crisis mortgage originations were
# higher-priced ("subprime") were more exposed to the credit-supply expansion
# whose reversal drove the 2007-2009 bust (the Mian-Sufi mechanism). This is a
# second instrument for recession severity, independent of industry mix.
#
#   high-cost share_c = (first-lien originations with a reported rate spread)
#                       / (first-lien originations), pooled over 2004-2006,
#   by county c, then population-weighted to PUMA.
#
# HMDA reports the rate-spread (higher-priced) flag from 2004 on. We restrict to
# first-lien, owner-occupied, 1-4 family home-purchase/refinance originations so
# the rate-spread threshold (>= 3 pts for first liens) is applied consistently.
#
# Inputs:  raw/hmda/HMDA_LAR_2004.zip, _2005.zip, _2006.zip (pipe-delimited LAR)
#          processed/county_to_puma_xwalk.csv
# Output:  processed/hmda_highcost_puma.csv  (puma_geoid, hc_share, n_orig)
#
# Pipe-delimited fields used: 5 loan_purpose, 6 occupancy, 8 action_taken,
#   10 state_code, 11 county_code, 21 property_type, 35 rate_spread, 37 lien_status
# =============================================================================

source("00_setup.R")
library(data.table)

years <- 2004:2006

# AWK aggregates each 3.5 GB file to county counts in one streaming pass
# (low memory). Output per line: countyfips,n_orig,n_highcost
awk_prog <- paste0(
  "NR>1 && $8==\"1\" && $37==\"1\" && $6==\"1\" && $21==\"1\" && ($5==\"1\"||$5==\"3\")",
  "{k=$10$11; n[k]++; if($35!=\"\"){h[k]++}} ",
  "END{for(k in n) print k\",\"n[k]\",\"(h[k]+0)}")

read_year <- function(yr) {
  zip   <- raw_path("hmda", sprintf("HMDA_LAR_%d.zip", yr))
  inner <- sprintf("HMDA_LAR_%d.txt", yr)
  cmd   <- sprintf("unzip -p '%s' '%s' | awk -F'|' '%s'", zip, inner, awk_prog)
  cat("  streaming", yr, "...\n")
  dt <- fread(cmd = cmd, header = FALSE,
              col.names = c("county_geoid", "n_orig", "n_hc"),
              colClasses = list(character = 1, integer = 2:3))
  dt[, year := yr]
  dt
}

cat("\n--- Aggregating HMDA 2004-2006 high-cost originations by county ---\n")
hmda <- rbindlist(lapply(years, read_year))

# Pool the three years, then county high-cost share
county_hc <- hmda[, .(n_orig = sum(n_orig), n_hc = sum(n_hc)),
                  by = county_geoid][n_orig >= 50]   # drop tiny-count counties
county_hc[, hc_share := n_hc / n_orig]
cat("Counties (>=50 originations):", nrow(county_hc),
    "| national pooled high-cost share:",
    round(sum(county_hc$n_hc) / sum(county_hc$n_orig), 3), "\n")

# county-level output (used by the county-level causal analysis)
fwrite(county_hc[, .(county_geoid, hc_share, n_orig)],
       processed_path("hmda_highcost_county.csv"))
cat("Saved: hmda_highcost_county.csv\n")

# ---------------------------------------------------------------------------
# Population-weighted county -> PUMA
# ---------------------------------------------------------------------------
xw <- fread(processed_path("county_to_puma_xwalk.csv"),
            colClasses = list(character = c("puma_geoid", "county_geoid")))
m  <- merge(xw, county_hc, by = "county_geoid")
m[, `:=`(w_orig = w * n_orig, w_hc = w * n_hc)]

puma_hc <- m[, .(n_orig = sum(w_orig), n_hc = sum(w_hc)), by = puma_geoid]
puma_hc[, `:=`(hc_share = n_hc / n_orig,
               puma_geoid = as.numeric(puma_geoid))]
puma_hc <- puma_hc[, .(puma_geoid, hc_share, n_orig = round(n_orig))]

cat("PUMAs with HMDA instrument:", nrow(puma_hc),
    "| high-cost share: median", round(median(puma_hc$hc_share), 3),
    "range", round(min(puma_hc$hc_share), 3), "to",
    round(max(puma_hc$hc_share), 3), "\n")

fwrite(puma_hc, processed_path("hmda_highcost_puma.csv"))
cat("Saved: hmda_highcost_puma.csv\n")
