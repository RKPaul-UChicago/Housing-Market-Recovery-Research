# =============================================================================
# 19_iv_diagnostics.R
# Q1 identification diagnostics for the county-level causal design (companion to
# 16/17). Two gating analyses:
#
#   (A) FALSIFICATION / PLACEBO. Does each instrument predict the PRE-pandemic
#       2012->2019 FHFA HPI change, a period in which it should have no effect
#       if its only channel runs through 2007->2009 recession severity? Run both
#       unconditionally and conditional on the 2007-2009 change D. A strong
#       conditional association is evidence of an exclusion-restriction violation.
#
#   (B) ROTEMBERG WEIGHTS for the Bartik instrument (Goldsmith-Pinkham, Sorkin
#       & Swift 2020). Decomposes the Bartik-only just-identified 2SLS into a
#       weighted sum over 2-digit NAICS sectors, sum_k alpha_k * beta_k, to show
#       which industries drive the identifying variation and how high-exposure
#       counties differ.
#
# Inputs : raw/fhfa_hpi/hpi_at_county.xlsx, raw/cbp/*, processed/hmda_highcost_county.csv,
#          processed/bartik_county.csv, Census Gazetteer (county centroids)
# Outputs: processed/results_iv_falsification.csv, processed/results_rotemberg.csv
# =============================================================================

source("00_setup.R")
library(data.table); library(fixest); library(readxl)

cbp_path <- function(...) raw_path("cbp", ...)
pval <- function(b, s) 2 * pnorm(-abs(b / s))

# ---------------------------------------------------------------------------
# 1. County panel: treatment, COVID outcome, and the 2012-2019 placebo outcome
# ---------------------------------------------------------------------------
fhfa <- read_excel(raw_path("fhfa_hpi", "hpi_at_county.xlsx"), sheet = 1, skip = 5)
names(fhfa) <- c("state_nm","county_nm","county_geoid","year","annual_chg","hpi","hpi90","hpi00")
setDT(fhfa)
fhfa[, `:=`(county_geoid = as.character(county_geoid), year = as.integer(year),
            hpi = suppressWarnings(as.numeric(hpi)))]
fhfa <- fhfa[year %in% c(2007,2009,2012,2019,2021) & !is.na(hpi)]
w <- dcast(fhfa, county_geoid ~ year, value.var = "hpi")
setnames(w, c("2007","2009","2012","2019","2021"), c("h2007","h2009","h2012","h2019","h2021"))
w <- w[!is.na(h2007) & !is.na(h2009) & !is.na(h2012) & !is.na(h2019) & !is.na(h2021)]
w[, `:=`(D = 100*(h2009/h2007-1), Y = 100*(h2021/h2019-1),
         pre_1219 = 100*(h2019/h2012-1), logbase = log(h2007))]

hmda <- fread(processed_path("hmda_highcost_county.csv"), colClasses = list(character="county_geoid"))
bart <- fread(processed_path("bartik_county.csv"),        colClasses = list(character="county_geoid"))
d <- Reduce(function(a,b) merge(a,b,by="county_geoid"),
            list(w, hmda[,.(county_geoid,hc_share)], bart[,.(county_geoid,bartik)]))
d[, state := substr(county_geoid,1,2)]
d <- d[!(state %in% c("02","15","72")) & h2007 > 0]
d <- d[is.finite(D)&is.finite(Y)&is.finite(pre_1219)&is.finite(hc_share)&is.finite(bartik)&is.finite(logbase)]

gaz <- fread(GAZ_PATH, sep="\t", quote="")
setnames(gaz, trimws(names(gaz)))
gaz[, county_geoid := substr(sprintf("%011s", as.character(GEOID)),1,5)]
cent <- gaz[, .(lat=mean(as.numeric(INTPTLAT),na.rm=TRUE),
                lon=mean(as.numeric(INTPTLONG),na.rm=TRUE)), by=county_geoid]
d <- merge(d, cent, by="county_geoid", all.x=TRUE)
cat("Falsification sample:", nrow(d), "counties (require all 5 HPI years)\n")

# standardise instruments so coefficients are comparable (per 1 SD)
d[, `:=`(z_hmda = scale(hc_share)[,1], z_bartik = scale(bartik)[,1])]

# ---------------------------------------------------------------------------
# 2. Falsification: instrument -> pre-pandemic (2012-2019) HPI change
#    full SE battery; unconditional and conditional on D
# ---------------------------------------------------------------------------
se_row <- function(m, var, label, outcome) {
  b <- coef(m)[[var]]
  s <- function(v) sqrt(diag(vcov(m, vcov=v)))[[var]]
  s_hc <- s("hetero"); s_cl <- s(~state)
  s_co <- summary(m, vcov=conley(cutoff=200, distance="spherical"))$se[[var]]
  data.table(test=label, outcome=outcome, coef=round(b,3),
             p_hc=round(pval(b,s_hc),3), p_clust=round(pval(b,s_cl),3),
             p_conley=round(pval(b,s_co),3))
}
fals <- rbind(
  # main-period reduced forms (reference: instruments SHOULD load on COVID Y)
  se_row(feols(Y ~ z_bartik + logbase | state, d), "z_bartik", "Bartik  -> COVID 2019-21 (ref)", "Y_1921"),
  se_row(feols(Y ~ z_hmda   + logbase | state, d), "z_hmda",   "HMDA    -> COVID 2019-21 (ref)", "Y_1921"),
  # placebo: instruments should NOT load on pre-pandemic trend if exclusion holds
  se_row(feols(pre_1219 ~ z_bartik + logbase | state, d), "z_bartik", "Bartik  -> pre 2012-19 (uncond.)", "pre_1219"),
  se_row(feols(pre_1219 ~ z_hmda   + logbase | state, d), "z_hmda",   "HMDA    -> pre 2012-19 (uncond.)", "pre_1219"),
  # placebo conditional on recession severity D (the cleaner exclusion test)
  se_row(feols(pre_1219 ~ z_bartik + D + logbase | state, d), "z_bartik", "Bartik  -> pre 2012-19 | D", "pre_1219"),
  se_row(feols(pre_1219 ~ z_hmda   + D + logbase | state, d), "z_hmda",   "HMDA    -> pre 2012-19 | D", "pre_1219")
)
cat("\n=== (A) FALSIFICATION: standardized (per-1-SD) instrument effects ===\n")
print(fals)
fwrite(fals, processed_path("results_iv_falsification.csv"))

# ---------------------------------------------------------------------------
# 3. Rotemberg weights for the Bartik instrument
# ---------------------------------------------------------------------------
# rebuild county 2006 sector shares (s_ik) and national 2007->2009 growth (g_k)
is_sector <- function(naics) grepl("^[0-9]{2}----$", naics)
midpts <- c(n1_4=2,n5_9=7,n10_19=14.5,n20_49=34.5,n50_99=74.5,
            n100_249=174.5,n250_499=374.5,n500_999=749.5,n1000=1500)
co <- fread(cmd=sprintf("unzip -p '%s'", cbp_path("cbp06co.zip")),
            colClasses=list(character=c("fipstate","fipscty","naics")))
co <- co[is_sector(naics)]
co[, imp := as.numeric(emp)]
sc <- intersect(names(midpts), names(co))
co[, imp_sc := as.numeric(Reduce(`+`, Map(function(col,m) as.numeric(get(col))*m, sc, midpts[sc])))]
co[, emp_use := ifelse(is.finite(imp)&imp>0, imp, imp_sc)]
co[, county_geoid := paste0(fipstate, fipscty)]
cs <- co[, .(emp_ck=sum(emp_use,na.rm=TRUE)), by=.(county_geoid,naics)]
cs[, emp_c := sum(emp_ck), by=county_geoid]
cs[, share_ck := emp_ck/emp_c]

read_us <- function(p, is_cmd=FALSE){ dt <- if(is_cmd) fread(cmd=p,colClasses=list(character="naics")) else fread(p,colClasses=list(character="naics")); if("lfo"%in%names(dt)) dt<-dt[lfo=="-"]; dt[is_sector(naics),.(naics,emp=as.numeric(emp))] }
natg <- merge(read_us(cbp_path("cbp07us.txt"))[,.(naics,emp07=emp)],
              read_us(sprintf("unzip -p '%s'",cbp_path("cbp09us.zip")),TRUE)[,.(naics,emp09=emp)], by="naics")
natg[, g_k := (emp09-emp07)/emp07]

# wide share matrix for the falsification sample, aligned to sectors present nationally
secs <- sort(intersect(unique(cs$naics), natg$naics))
S <- dcast(cs[county_geoid %in% d$county_geoid & naics %in% secs],
           county_geoid ~ naics, value.var="share_ck", fill=0)
S <- merge(d[,.(county_geoid, D, Y, logbase, state)], S, by="county_geoid")

# FWL: residualise D, Y and each share on controls (logbase + state FE)
resid_on_W <- function(v) resid(feols(v ~ logbase | state, data=cbind(d[,.(logbase,state)], v=v), notes=FALSE))
Dt <- resid_on_W(S$D); Yt <- resid_on_W(S$Y)
g_k <- setNames(natg$g_k, natg$naics)[secs]
lab2 <- c("11"="Agriculture","21"="Mining/Oil-Gas","22"="Utilities","23"="Construction",
          "31"="Manufacturing","32"="Manufacturing","33"="Manufacturing","42"="Wholesale",
          "44"="Retail","45"="Retail","48"="Transport/Warehouse","49"="Transport/Warehouse",
          "51"="Information","52"="Finance/Insurance","53"="Real estate","54"="Prof/Sci/Tech",
          "55"="Mgmt of companies","56"="Admin/Support/Waste","61"="Education","62"="Health care",
          "71"="Arts/Entertainment","72"="Accommodation/Food","81"="Other services","99"="Unclassified")
rot <- rbindlist(lapply(secs, function(k){
  sk <- resid_on_W(S[[k]])
  num <- sum(sk*Dt); bk <- sum(sk*Yt)/num
  data.table(naics=substr(k,1,2), g_k=g_k[[k]], num=num, beta_k=bk, wraw=g_k[[k]]*num)
}))
rot[, sector := lab2[naics]]
rot <- rot[, .(g_k=g_k[1], beta_k=weighted.mean(beta_k, abs(wraw)),
               wraw=sum(wraw)), by=sector]      # collapse split sectors (mfg/retail/transport)
rot[, alpha := wraw/sum(wraw)]
beta_bartik <- sum(rot$wraw*rot$beta_k)/sum(rot$wraw)
setorder(rot, -alpha)
cat(sprintf("\n=== (B) ROTEMBERG: Bartik-only beta = %.3f = sum_k alpha_k beta_k (check %.3f) ===\n",
            sum(Yt*resid_on_W(rowSums(sapply(secs,function(k) S[[k]]*g_k[[k]]))))/
            sum(Dt*resid_on_W(rowSums(sapply(secs,function(k) S[[k]]*g_k[[k]])))), beta_bartik))
print(rot[, .(sector, g_k=round(g_k,3), alpha=round(alpha,3), beta_k=round(beta_k,3))])
fwrite(rot[, .(sector, g_k=round(g_k,4), rotemberg_alpha=round(alpha,4), beta_k=round(beta_k,3))],
       processed_path("results_rotemberg.csv"))

# how do high-exposure counties (top sector by |alpha|) differ?
topsec <- rot[which.max(abs(alpha)), sector]
topk <- secs[lab2[substr(secs,1,2)] == topsec]
d[, top_share := rowSums(S[match(d$county_geoid, S$county_geoid), ..topk, with=FALSE])]
d[, hi := top_share >= quantile(top_share, 0.75, na.rm=TRUE)]
cmp <- d[, .(n=.N, mean_D=round(mean(D),1), mean_Y=round(mean(Y),1),
             mean_hc=round(mean(hc_share),3), mean_logHPI=round(mean(logbase),2)), by=hi]
cat(sprintf("\nTop-Rotemberg sector = %s. High- vs low-exposure counties (top-quartile share):\n", topsec))
print(cmp)
cat("\n19_iv_diagnostics.R complete.\n")
