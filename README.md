# Research paper: "The Geography of Incomplete Recovery: Spatial Persistence and Compound Vulnerability in United States Housing Wealth, 2007-2021"

**Author:** Rajat Kanti Paul, Harris School of Public Policy, The University of Chicago

---

## Overview

This archive reproduces every table, figure, and statistical result in the manuscript and its appendices. The paper has two halves:

1. **A descriptive and spatial analysis** (PUMA level) that builds a real housing-wealth recovery index for 2,351 PUMAs, documents strong spatial clustering (Moran's I = 0.70), isolates 360 persistent Low-Low recovery cores and a 968-PUMA "Persistently Depressed" typology, and derives a Community Reinvestment Act (CRA) targeting screen. This is the paper's robust backbone.

2. **A causal analysis presented as a cautionary case study** (county level) that asks whether deeper Great Recession losses *caused* differential COVID-era recovery. Using a transaction-based FHFA House Price Index outcome and two strong instruments (a Bartik industry-mix labour-demand shock and the pre-crisis HMDA high-cost lending share), it shows the effect is **not identified** here: a pre-trend falsification test invalidates the Bartik instrument (Rotemberg weights show it is ~87% construction employment, i.e. the housing cycle itself), while the one instrument that passes falsification (HMDA) is too weak for sharp inference under spatially robust standard errors. The apparent OLS "catch-up" is a statistical mirage of mean reversion, but no available instrument is both strong and valid, a cautionary result for the mechanical use of shift-share instruments when housing is the outcome.

All data are public U.S. government sources and are included in the archive. The data, replication code, generated charts, and results tables are all self-contained within the `data and Replication codes/` folder. Scripts are numbered and self-contained.

---

## Quick Start

### Step 1: Required R packages
  "tidyverse", "Hmisc", "sf", "spdep", "spatialreg", "scales",
  #### county causal analysis
  "data.table", "fixest", "AER", "sandwich", "lmtest", "readxl",
  #### figures and graphical abstract
  "cowplot", "png"

### Step 2: Run the descriptive / spatial pipeline

Set the working directory to `data and Replication codes/Replication code scripts/` and source the scripts in order. Each sources `00_setup.R`; scripts 02-08 also source `01_load_clean_acs.R` automatically.
```r
setwd(".../data and Replication codes/Replication code scripts")
source("02_state_income_analysis.R")
source("03_puma_recession_analysis.R")
source("04_build_recovery_index.R")
source("05_spatial_crosswalk.R")
source("06_merge_analysis_dataset.R")
source("07_spatial_analysis.R")
source("08_spatial_lag_model.R")
```

### Step 3: Run the county-level causal pipeline

```r
source("11_build_crosswalk.R")     # county <-> PUMA crosswalk
source("13_hmda_creditsupply.R")   # HMDA pre-crisis high-cost lending share (instrument 2)
source("14_fhfa_outcome.R")        # FHFA county HPI treatment and outcome
source("15_bartik_county.R")       # Bartik industry-mix shock (instrument 1)
source("16_causal_2sls_revamp.R")  # combined two-instrument 2SLS (Tables 6-7)
source("17_causal_channels.R")     # per-instrument channel decomposition (Table 7)
source("19_iv_diagnostics.R")      # falsification + Rotemberg-weight diagnostics (Tables 8-9)
```

### Step 4: Generate charts and the graphical abstract

```r
setwd(".../data and Replication codes")   # folder containing generate_figures.R
source("generate_figures.R")              # Figures 1-6, A1 -> data and Replication codes/figures/

setwd(".../data and Replication codes/Replication code scripts")
source("18_graphical_abstract.R")         # Graphical abstract (PNG + PDF) -> ../figures/
```

---

## Folder Structure

```
Housing Market Recovery Research/
├── README.md                                  <- this file
└── data and Replication codes/
    ├── generate_figures.R                     <- standalone chart generation
    ├── raw/                                   <- all original source data (see Data Sources)
    ├── processed/                             <- analysis-ready datasets + results tables (CSV)
    ├── figures/                               <- generated charts (PNG) + graphical abstract (PNG/PDF)
    └── Replication code scripts/              <- numbered analysis scripts (00-08, 11, 13-19)
```

---

## Data Sources

All data are from publicly available U.S. government sources. No proprietary data are used.

| Source | Level | Years | Archive file(s) | Used by |
|--------|-------|-------|-----------------|---------|
| IPUMS USA ACS (home value, income, weights) | PUMA | 2007, 2009, 2012, 2019, 2021 | `raw/ipums_acs/usa_00010.csv`, `usa_00011.csv` | descriptive 01-08 |
| Census TIGER/Line PUMA polygons | PUMA | 2010 vintage | `raw/census_geometry/tl_2021_XX_puma10/` | 05, 07, 08, figures |
| Census Gazetteer (tract centroids) | tract | 2021 | `raw/census_geometry/2021_Gaz_tracts_national.txt` | 05, 11, 16, 17 |
| FEMA National Risk Index v1.20 | tract | Dec 2025 | `raw/fema_nri/NRI_Table_CensusTracts.csv` | 05, 07 |
| BLS CPI-U annual averages | national | 2007-2021 | `raw/cpi_u/cpi_u_annual.csv` | 04 (CPI factor 1.1802) |
| FHFA All-Transactions House Price Index | county | 2007, 2009, 2019, 2021 | `raw/fhfa_hpi/hpi_at_county.xlsx` | 14 (treatment + outcome) |
| Census County Business Patterns | county / national | 2006, 2007, 2009 | `raw/cbp/cbp06co.zip`, `cbp07us.txt`, `cbp09us.zip` | 15 (Bartik instrument) |
| HMDA Loan Application Register | county | 2004-2006 | `raw/hmda/HMDA_LAR_2004/2005/2006.zip` | 13 (credit-supply instrument) |
| Tract-to-PUMA crosswalk | tract→PUMA | 2010 | `raw/crosswalks/2010_tract_to_2010_puma.txt` | 11 |

The HMDA 2004-2006 extracts are redistributed under a Creative Commons Attribution 4.0 (CC BY 4.0) license via openICPSR (Forrester, Andrew. 2021. *Historical Home Mortgage Disclosure Act (HMDA) Data*. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor]. https://doi.org/10.3886/E151921V1).

---

## Replication Scripts

| Script | Key inputs | Key outputs | Manuscript object |
|--------|-----------|-------------|-------------------|
| `00_setup.R` | -- | path detection, helpers (`raw_path`, `processed_path`, `fig_path`, `wtd_median`), `CPI_FACTOR` | -- |
| `01_load_clean_acs.R` | `raw/ipums_acs/*.csv` | cleaned ACS objects (8,955,228 + 1,769,702 rows) | -- |
| `02_state_income_analysis.R` | ACS main | `state_income_analysis.csv`, `results_ols_state.csv` | Table 5 |
| `03_puma_recession_analysis.R` | ACS main | `puma_recession_recovery.csv`, `results_recovery_by_decimation.csv`, `national_value_trends.csv` | Table 2, Fig 1, text |
| `04_build_recovery_index.R` | ACS gis, CPI | `puma_recovery_index.csv` | Tables 3-4 |
| `05_spatial_crosswalk.R` | TIGER, NRI, Gazetteer | `nri_puma.csv`, `puma_names.csv` | -- |
| `06_merge_analysis_dataset.R` | processed CSVs | `analysis_dataset.csv` | -- |
| `07_spatial_analysis.R` | `analysis_dataset.csv`, TIGER | `results_moran_lisa.csv`, `results_kmeans_clusters.csv`, cluster/LISA assignments | Tables 3-4, Figs 4-5 |
| `08_spatial_lag_model.R` | `analysis_dataset.csv`, TIGER | `results_spatial_lag.csv` | Section 6 |
| `11_build_crosswalk.R` | `raw/crosswalks/`, Gazetteer | `county_to_puma_xwalk.csv` | -- |
| `13_hmda_creditsupply.R` | `raw/hmda/HMDA_LAR_2004-2006.zip`, crosswalk | `hmda_highcost_county.csv` (+ `_puma`) | instrument 2 |
| `14_fhfa_outcome.R` | `raw/fhfa_hpi/hpi_at_county.xlsx`, crosswalk | `fhfa_county_outcomes.csv` (+ `_puma`) | treatment + outcome |
| `15_bartik_county.R` | `raw/cbp/*` | `bartik_county.csv` | instrument 1 |
| `16_causal_2sls_revamp.R` | county outcomes, HMDA, Bartik, Gazetteer | `results_causal_revamp.csv` | **Tables 6-7** |
| `17_causal_channels.R` | county outcomes, HMDA, Bartik, Gazetteer | `results_causal_channels.csv` | **Table 7** (per-instrument rows) |
| `19_iv_diagnostics.R` | county outcomes, HMDA, Bartik, CBP | `results_iv_falsification.csv`, `results_rotemberg.csv` | **Tables 8-9** |
| `18_graphical_abstract.R` | `results_causal_*.csv`, `figures/fig4_lisa_map.png` | `figures/Graphical_Abstract.png` / `.pdf` | graphical abstract |
| `generate_figures.R` (root) | processed CSVs, TIGER | `figures/*.png` | Figures 1-6, A1 |

Headline causal results (`results_causal_revamp.csv` / `results_causal_channels.csv`, 2,709 counties):
OLS β = -0.167; combined 2SLS β = -0.233 (first-stage F = 22.7; HC p = 0.063, clustered p = 0.16, Conley p = 0.21); Wu-Hausman p = 0.54; Sargan over-id p = 0.066; Anderson-Rubin p = 0.072. Channel decomposition: Bartik-only β = -0.015 (p ≈ 0.93 under every SE); HMDA-only β = -0.424 (p = 0.026 robust, but 0.13-0.14 under clustered/Conley).

Instrument validity diagnostics (`results_iv_falsification.csv` / `results_rotemberg.csv`, 2,707 counties) are decisive and motivate the non-identification conclusion: in a pre-trend placebo on the 2012-2019 FHFA HPI change, the **Bartik instrument fails** (it predicts the pre-pandemic trend conditional on recession severity, standardized coefficient -1.07, p < 0.001 under HC, clustered, and Conley SEs), while the **HMDA instrument passes** (p = 0.23 clustered, 0.26 Conley). The Rotemberg-weight decomposition explains the failure: the Bartik shock is ~87% driven by Construction employment (α = 0.871), i.e. the housing cycle itself, a mechanical exclusion violation when house prices are the outcome. No available instrument is therefore both strong and valid.

---

## Generated Charts (`figures/`)

Produced by `generate_figures.R` (Figures 1-6, A1) and `18_graphical_abstract.R` (graphical abstract):

| File | Manuscript figure | Content |
|------|-------------------|---------|
| `figures/fig1_value_trends.png` | Figure 1 | National median and mean owner-occupied home value, 2007-2021 |
| `figures/fig2_recovery_map.png` | Figure 2 | Real housing-wealth recovery index by PUMA, 2012-2021 |
| `figures/fig3_puma_scatter.png` | Figure 3 | Great Recession vs COVID-era value change at the PUMA level |
| `figures/fig4_lisa_map.png` | Figure 4 | LISA recovery cluster map (Low-Low / High-High cores) |
| `figures/fig5_cluster_map.png` | Figure 5 | K-means recovery typology by PUMA |
| `figures/fig6_compound_disadvantage.png` | Figure 6 | Recovery index vs FEMA social vulnerability by cluster |
| `figures/figa1_income_recovery.png` | Figure A1 | Income-group recovery rates (state-income cells) |
| `figures/Graphical_Abstract.png` / `.pdf` | Graphical abstract | Three-panel summary (2SLS forest plot, non-identification note, LISA map) |

---

## Generated Tables (`processed/`)

Each manuscript table is reproduced from a results CSV written to `processed/`:

| Results file | Manuscript table |
|--------------|------------------|
| `national_value_trends.csv` | Table 2 (summary statistics by survey year) |
| `puma_recovery_index.csv`, `results_kmeans_clusters.csv`, `results_moran_lisa.csv` | Tables 3-4 (recovery quartiles and k-means typology) |
| `results_ols_state.csv`, `state_income_analysis.csv` | Table 5 (state-level income-group regression) |
| `results_causal_revamp.csv` | Tables 6-7 (first stage, OLS and combined 2SLS) |
| `results_causal_channels.csv` | Table 7 (per-instrument 2SLS rows) |
| `results_iv_falsification.csv` | Table 8 (pre-trend falsification) |
| `results_rotemberg.csv` | Table 9 (Rotemberg-weight decomposition) |
| `results_spatial_lag.csv` | Section 6 (spatial lag model) |

Supporting analysis-ready datasets in `processed/` (not tables themselves) include `analysis_dataset.csv`, `bartik_county.csv`, `fhfa_county_outcomes.csv`, `hmda_highcost_county.csv`, `county_to_puma_xwalk.csv`, `nri_puma.csv`, and the LISA / k-means cluster-assignment files.

---

## Citation

Paul, Rajat Kanti (2026). "The Geography of Incomplete Recovery: Spatial Persistence and Compound Vulnerability in United States Housing Wealth, 2007-2021."
