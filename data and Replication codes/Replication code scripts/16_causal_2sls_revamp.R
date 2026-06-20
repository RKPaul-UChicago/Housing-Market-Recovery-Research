# =============================================================================
# 16_causal_2sls_revamp.R
# County-level causal analysis: did Great Recession severity causally affect
# COVID-era housing recovery? Two-instrument 2SLS.
#
#   Y (recovery)   = FHFA county HPI change 2019->2021
#   D (severity)   = FHFA county HPI change 2007->2009  (more negative = harder hit)
#   Instruments    = (1) Bartik industry-mix employment shock (CBP)
#                    (2) pre-crisis high-cost lending share 2004-2006 (HMDA)
#   Controls       = log 2007 HPI level + state fixed effects
#
# Reports: OLS vs 2SLS; first-stage F (robust & clustered); over-identification
# (Sargan/Hansen J); Wu-Hausman; Anderson-Rubin weak-IV-robust test of beta=0;
# and classical / heteroskedasticity-robust / state-clustered / Conley spatial SEs.
#
# Output: processed/results_causal_revamp.csv
# =============================================================================

source("00_setup.R")
library(data.table); library(fixest); library(AER)
library(sandwich); library(lmtest)

# ---------------------------------------------------------------------------
# 1. Assemble county analysis table
# ---------------------------------------------------------------------------
fhfa <- fread(processed_path("fhfa_county_outcomes.csv"),
              colClasses = list(character = "county_geoid"))
hmda <- fread(processed_path("hmda_highcost_county.csv"),
              colClasses = list(character = "county_geoid"))
bart <- fread(processed_path("bartik_county.csv"),
              colClasses = list(character = "county_geoid"))

d <- Reduce(function(a, b) merge(a, b, by = "county_geoid"),
            list(fhfa, hmda[, .(county_geoid, hc_share)], bart[, .(county_geoid, bartik)]))
d[, state := substr(county_geoid, 1, 2)]
d <- d[!(state %in% c("02", "15", "72")) & h2007 > 0]   # contiguous US
d[, `:=`(Y = fhfa_covid_1921, D = fhfa_rec_0709, logbase = log(h2007))]
d <- d[is.finite(Y) & is.finite(D) & is.finite(hc_share) & is.finite(bartik)]
cat("County analysis sample:", nrow(d), "counties,", uniqueN(d$state), "states\n")

# county centroids (mean of tract interior points) for Conley SEs
gaz <- fread(GAZ_PATH, sep = "\t", quote = "")
setnames(gaz, trimws(names(gaz)))
gaz[, county_geoid := substr(sprintf("%011s", as.character(GEOID)), 1, 5)]
cent <- gaz[, .(lat = mean(as.numeric(INTPTLAT), na.rm = TRUE),
                lon = mean(as.numeric(INTPTLONG), na.rm = TRUE)), by = county_geoid]
d <- merge(d, cent, by = "county_geoid", all.x = TRUE)
cat("Counties matched to centroids:", sum(is.finite(d$lat)), "of", nrow(d), "\n")

# ---------------------------------------------------------------------------
# 2. OLS and 2SLS (fixest, with state FE)
# ---------------------------------------------------------------------------
ols  <- feols(Y ~ D + logbase | state, data = d)
iv   <- feols(Y ~ logbase | state | D ~ bartik + hc_share, data = d)

b_ols <- coef(ols)[["D"]]
b_iv  <- coef(iv)[["fit_D"]]

se <- function(m, v) sqrt(diag(vcov(m, vcov = v)))[["fit_D"]]
se_iid  <- se(iv, "iid")
se_hc   <- se(iv, "hetero")
se_cl   <- se(iv, ~ state)
se_con  <- summary(iv, vcov = conley(cutoff = 200, distance = "spherical"))$se[["fit_D"]]

pval <- function(b, s) 2 * pnorm(-abs(b / s))

# ---------------------------------------------------------------------------
# 3. First stage (relevance) — robust & clustered F on excluded instruments
# ---------------------------------------------------------------------------
fs <- feols(D ~ bartik + hc_share + logbase | state, data = d)
F_hc <- fitstat(fs, "wald", vcov = "hetero")$wald$stat   # joint, but includes logbase
# joint test of the two excluded instruments only:
W_hc <- wald(fs, keep = c("bartik", "hc_share"), vcov = "hetero")
W_cl <- wald(fs, keep = c("bartik", "hc_share"), vcov = ~ state)
cat("\nFirst-stage coefficients (HC):\n"); print(coeftable(fs, vcov = "hetero")[c("bartik","hc_share"), ])
cat(sprintf("First-stage joint F (excluded instruments): HC=%.1f  cluster=%.1f\n",
            W_hc$stat, W_cl$stat))

# ---------------------------------------------------------------------------
# 4. Diagnostics via AER: weak-instrument F, Wu-Hausman, Sargan over-id
# ---------------------------------------------------------------------------
ivaer <- ivreg(Y ~ D + logbase + factor(state) |
                 bartik + hc_share + logbase + factor(state), data = d)
diag  <- summary(ivaer, diagnostics = TRUE)$diagnostics
print(round(diag, 4))

# ---------------------------------------------------------------------------
# 5. Anderson-Rubin weak-IV-robust test of H0: beta = 0
#    = joint significance of the instruments in the reduced form Y ~ Z + controls
# ---------------------------------------------------------------------------
rf   <- feols(Y ~ bartik + hc_share + logbase | state, data = d)
AR_hc <- wald(rf, keep = c("bartik", "hc_share"), vcov = "hetero")
cat(sprintf("\nAnderson-Rubin (H0: beta=0) robust F=%.2f  p=%.4f\n",
            AR_hc$stat, AR_hc$p))

# ---------------------------------------------------------------------------
# 6. Assemble results
# ---------------------------------------------------------------------------
res <- data.table(
  n_counties   = nrow(d),
  ols_beta     = round(b_ols, 3),
  iv_beta      = round(b_iv, 3),
  iv_se_iid    = round(se_iid, 3),
  iv_se_hc     = round(se_hc, 3),
  iv_se_clust  = round(se_cl, 3),
  iv_se_conley = round(se_con, 3),
  iv_p_hc      = round(pval(b_iv, se_hc), 4),
  iv_p_clust   = round(pval(b_iv, se_cl), 4),
  iv_p_conley  = round(pval(b_iv, se_con), 4),
  fs_F_hc      = round(W_hc$stat, 1),
  fs_F_clust   = round(W_cl$stat, 1),
  wu_hausman_p = round(diag["Wu-Hausman", "p-value"], 4),
  sargan_p     = round(diag["Sargan", "p-value"], 4),
  AR_p         = round(AR_hc$p, 4)
)
fwrite(res, processed_path("results_causal_revamp.csv"))
cat("\n=== REVAMP CAUSAL RESULTS (county level) ===\n")
print(t(res))
cat("\nSaved: results_causal_revamp.csv\n")
