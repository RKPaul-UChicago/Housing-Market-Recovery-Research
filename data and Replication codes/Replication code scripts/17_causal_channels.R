# =============================================================================
# 17_causal_channels.R
# Channel decomposition for the county-level causal analysis (companion to 16).
# Reports each instrument's just-identified 2SLS separately, with the full
# inference battery, to show WHICH channel (industry-mix Bartik vs credit-supply
# HMDA) drives the combined estimate and whether it survives spatial inference.
#
#   Y (recovery) = FHFA county HPI change 2019->2021
#   D (severity) = FHFA county HPI change 2007->2009
#   Z1 = Bartik industry-mix shock ; Z2 = HMDA pre-crisis high-cost lending share
#   Controls = log 2007 HPI + state FE
#
# Output: processed/results_causal_channels.csv
# =============================================================================

source("00_setup.R")
library(data.table); library(fixest)

fhfa <- fread(processed_path("fhfa_county_outcomes.csv"),
              colClasses = list(character = "county_geoid"))
hmda <- fread(processed_path("hmda_highcost_county.csv"),
              colClasses = list(character = "county_geoid"))
bart <- fread(processed_path("bartik_county.csv"),
              colClasses = list(character = "county_geoid"))

d <- Reduce(function(a, b) merge(a, b, by = "county_geoid"),
            list(fhfa, hmda[, .(county_geoid, hc_share)], bart[, .(county_geoid, bartik)]))
d[, state := substr(county_geoid, 1, 2)]
d <- d[!(state %in% c("02", "15", "72")) & h2007 > 0]
d[, `:=`(Y = fhfa_covid_1921, D = fhfa_rec_0709, logbase = log(h2007))]
d <- d[is.finite(Y) & is.finite(D) & is.finite(hc_share) & is.finite(bartik)]

gaz <- fread(GAZ_PATH, sep = "\t", quote = "")
setnames(gaz, trimws(names(gaz)))
gaz[, county_geoid := substr(sprintf("%011s", as.character(GEOID)), 1, 5)]
cent <- gaz[, .(lat = mean(as.numeric(INTPTLAT), na.rm = TRUE),
                lon = mean(as.numeric(INTPTLONG), na.rm = TRUE)), by = county_geoid]
d <- merge(d, cent, by = "county_geoid", all.x = TRUE)

pval <- function(b, s) 2 * pnorm(-abs(b / s))

# just-identified 2SLS for a single excluded instrument, full SE battery
channel <- function(inst, label) {
  f  <- as.formula(sprintf("Y ~ logbase | state | D ~ %s", inst))
  iv <- feols(f, data = d)
  b  <- coef(iv)[["fit_D"]]
  s  <- function(v) sqrt(diag(vcov(iv, vcov = v)))[["fit_D"]]
  s_iid <- s("iid"); s_hc <- s("hetero"); s_cl <- s(~ state)
  s_con <- summary(iv, vcov = conley(cutoff = 200, distance = "spherical"))$se[["fit_D"]]
  # first-stage F on this single excluded instrument
  fs <- feols(as.formula(sprintf("D ~ %s + logbase | state", inst)), data = d)
  F_hc <- wald(fs, keep = inst, vcov = "hetero")$stat
  F_cl <- wald(fs, keep = inst, vcov = ~ state)$stat
  data.table(
    channel = label, n = nrow(d), iv_beta = round(b, 3),
    fs_F_hc = round(F_hc, 1), fs_F_clust = round(F_cl, 1),
    p_iid    = round(pval(b, s_iid), 3),
    p_hc     = round(pval(b, s_hc),  3),
    p_clust  = round(pval(b, s_cl),  3),
    p_conley = round(pval(b, s_con), 3)
  )
}

res <- rbind(
  channel("bartik",   "Bartik industry-mix (only)"),
  channel("hc_share", "HMDA high-cost share (only)")
)
fwrite(res, processed_path("results_causal_channels.csv"))
cat("\n=== CHANNEL DECOMPOSITION (just-identified, single instrument) ===\n")
print(res)
cat("\nSaved: results_causal_channels.csv\n")
