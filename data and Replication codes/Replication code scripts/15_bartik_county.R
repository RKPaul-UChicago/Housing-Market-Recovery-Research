# =============================================================================
# 15_bartik_county.R
# County-level shift-share ("Bartik") instrument for Great Recession severity,
# built from County Business Patterns (CBP). Used because the causal analysis
# is at the county level (FHFA county HPI outcome), which avoids the 2000/2010
# PUMA-vintage problem entirely.
#
#   bartik_c = sum_k ( share_{c,k,2006} * g_k )
#   share_{c,k,2006} = county c's 2006 employment share in 2-digit NAICS sector k
#   g_k              = national employment growth in sector k, 2007 -> 2009
#
# Suppressed county employment cells (empflag set) are imputed from the
# establishment size-class counts using class midpoints.
#
# Inputs:  raw/cbp/cbp06co.zip  (2006 county)
#          raw/cbp/cbp07us.txt  (2007 national)
#          raw/cbp/cbp09us.zip  (2009 national)
# Output:  processed/bartik_county.csv  (county_geoid, bartik, emp_2006)
# =============================================================================

source("00_setup.R")
library(data.table)

cbp_path <- function(...) raw_path("cbp", ...)
is_sector <- function(naics) grepl("^[0-9]{2}----$", naics)

# establishment size-class midpoints for imputing suppressed employment
midpts <- c(n1_4 = 2, n5_9 = 7, n10_19 = 14.5, n20_49 = 34.5, n50_99 = 74.5,
            n100_249 = 174.5, n250_499 = 374.5, n500_999 = 749.5, n1000 = 1500)

# ---------------------------------------------------------------------------
# 1. County 2006 employment by 2-digit sector (impute suppressed cells)
# ---------------------------------------------------------------------------
co <- fread(cmd = sprintf("unzip -p '%s'", cbp_path("cbp06co.zip")),
            colClasses = list(character = c("fipstate", "fipscty", "naics")))
co <- co[is_sector(naics)]
co[, imp := as.numeric(emp)]
sc <- intersect(names(midpts), names(co))
co[, imp_sc := as.numeric(Reduce(`+`, Map(function(col, m) as.numeric(get(col)) * m,
                                          sc, midpts[sc])))]
co[, emp_use := ifelse(is.finite(imp) & imp > 0, imp, imp_sc)]
co[, county_geoid := paste0(fipstate, fipscty)]

county_share <- co[, .(emp_ck = sum(emp_use, na.rm = TRUE)), by = .(county_geoid, naics)]
county_share[, emp_c := sum(emp_ck), by = county_geoid]
county_share[, share_ck := emp_ck / emp_c]
cat("Counties:", uniqueN(county_share$county_geoid),
    "| sectors:", uniqueN(county_share$naics), "\n")

# ---------------------------------------------------------------------------
# 2. National sector employment growth 2007 -> 2009
# ---------------------------------------------------------------------------
read_us <- function(path_or_cmd, is_cmd = FALSE) {
  dt <- if (is_cmd) fread(cmd = path_or_cmd, colClasses = list(character = "naics"))
        else        fread(path_or_cmd, colClasses = list(character = "naics"))
  # newer US files carry a legal-form-of-organization dimension; keep the total
  if ("lfo" %in% names(dt)) dt <- dt[lfo == "-"]
  dt[is_sector(naics), .(naics, emp = as.numeric(emp))]
}
us07 <- read_us(cbp_path("cbp07us.txt"))
us09 <- read_us(sprintf("unzip -p '%s'", cbp_path("cbp09us.zip")), is_cmd = TRUE)

natg <- merge(us07[, .(naics, emp07 = emp)], us09[, .(naics, emp09 = emp)], by = "naics")
natg[, g_k := (emp09 - emp07) / emp07]
cat("National sectors matched:", nrow(natg),
    "| employment growth 2007-2009: median", round(median(natg$g_k), 3),
    "range", round(min(natg$g_k), 3), "to", round(max(natg$g_k), 3), "\n")

# ---------------------------------------------------------------------------
# 3. Bartik = sum_k share_ck * g_k
# ---------------------------------------------------------------------------
bk <- merge(county_share, natg[, .(naics, g_k)], by = "naics")
bartik <- bk[, .(bartik = sum(share_ck * g_k), emp_2006 = first(emp_c)),
             by = county_geoid]

cat("\nCounties with Bartik:", nrow(bartik),
    "| Bartik: median", round(median(bartik$bartik), 4),
    "range", round(min(bartik$bartik), 3), "to", round(max(bartik$bartik), 3), "\n")

fwrite(bartik, processed_path("bartik_county.csv"))
cat("Saved: bartik_county.csv\n")
