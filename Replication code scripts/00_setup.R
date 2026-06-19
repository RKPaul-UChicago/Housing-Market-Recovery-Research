# =============================================================================
# 00_setup.R
# Environment setup: path auto-detection, PROJ configuration, package checks,
# shared path helpers, and the wtd_median() function.
# Source this file at the top of every numbered script (01-09).
#
# HOW TO SET THE PROJECT ROOT (read this if you downloaded from Harvard Dataverse)
# ---------------------------------------------------------------------------------
# By default this script auto-detects the project root by searching up from the
# current working directory.  That works when you:
#   (a) open R and setwd() to any folder inside the archive, OR
#   (b) open the .Rproj file (if one is present), OR
#   (c) run scripts from the command line inside the archive folder.
#
# If auto-detection fails you will see a clear error message.  In that case,
# set OVERRIDE_ROOT below to the full path of the folder that contains the
#   "data and Replication codes/" directory.
#
# Example (macOS/Linux):  OVERRIDE_ROOT <- "/home/yourname/Paul_2026_Housing_Recovery"
# Example (Windows):      OVERRIDE_ROOT <- "C:/Users/yourname/Paul_2026_Housing_Recovery"
# =============================================================================

OVERRIDE_ROOT <- NULL   # <-- set this if auto-detection fails (see above)

# ---------------------------------------------------------------------------
# Auto-detect project root
# The project root is the folder that contains the
#   "data and Replication codes/" directory.
# ---------------------------------------------------------------------------
.find_root <- function(override) {
  sentinel <- function(path) {
    dir.exists(file.path(path, "data and Replication codes", "raw"))
  }
  if (!is.null(override)) {
    root <- normalizePath(override, mustWork = FALSE)
    if (sentinel(root)) return(root)
    stop("OVERRIDE_ROOT '", override, "' does not contain the expected subfolders.\n",
         "Check that 'data and Replication codes/' exists inside it.")
  }
  # Search upward from working directory (up to 6 levels)
  wd <- normalizePath(getwd(), mustWork = FALSE)
  parts <- strsplit(wd, .Platform$file.sep)[[1]]
  for (depth in seq_len(min(6L, length(parts)))) {
    candidate <- paste(parts[seq_len(length(parts) - depth + 1L)],
                       collapse = .Platform$file.sep)
    if (nchar(candidate) < 2) break
    if (sentinel(candidate)) {
      message("Project root auto-detected: ", candidate)
      return(normalizePath(candidate))
    }
  }
  stop(
    "\nCould not auto-detect the project root.\n\n",
    "Please open 00_setup.R and set OVERRIDE_ROOT to the folder that contains\n",
    "the 'data and Replication codes/' directory.\n\n",
    "Example:\n",
    "  OVERRIDE_ROOT <- \"/home/yourname/Paul_2026_Housing_Recovery\"\n"
  )
}

PROJECT_ROOT <- .find_root(OVERRIDE_ROOT)

# ---------------------------------------------------------------------------
# Convenience path builders (all paths in every script flow through these)
# ---------------------------------------------------------------------------
raw_path       <- function(...) file.path(PROJECT_ROOT, "data and Replication codes", "raw", ...)
processed_path <- function(...) file.path(PROJECT_ROOT, "data and Replication codes", "processed", ...)
fig_path       <- function(...) file.path(PROJECT_ROOT, "data and Replication codes", "figures", ...)

# ---------------------------------------------------------------------------
# PROJ/GDAL configuration
# On macOS the sf package bundles its own PROJ database; we point to it
# automatically so the correct version is always used regardless of R version.
# On Windows and Linux, PROJ is found via the system PATH; no action needed.
# ---------------------------------------------------------------------------
if (Sys.info()[["sysname"]] == "Darwin") {
  sf_proj <- system.file("proj", package = "sf")
  sf_gdal <- system.file("gdal", package = "sf")
  if (nchar(sf_proj) > 0L) {
    Sys.setenv(PROJ_LIB = sf_proj, GDAL_DATA = sf_gdal)
  }
}

# ---------------------------------------------------------------------------
# Required packages
# ---------------------------------------------------------------------------
required_pkgs <- c(
  "tidyverse",   # data manipulation and ggplot2
  "Hmisc",       # wtd.quantile() for weighted percentiles
  "sf",          # spatial data and polygon operations
  "spdep",       # Moran's I, LISA, spatial weights
  "spatialreg",  # lagsarlm() spatial lag model
  "AER",         # ivreg() two-stage least squares
  "sandwich",    # vcovHC / vcovCL robust and clustered standard errors
  "lmtest",      # coeftest / waldtest with robust vcov
  "fixest",      # feols() IV with Conley spatial-HAC standard errors
  "scales"       # number formatting in figures
)

missing_pkgs <- required_pkgs[!required_pkgs %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0L) {
  stop(
    "The following R packages are required but not installed:\n",
    "  ", paste(missing_pkgs, collapse = ", "), "\n\n",
    "Install them with:\n",
    "  install.packages(c(\"", paste(missing_pkgs, collapse = "\", \""), "\"))\n"
  )
}
invisible(lapply(required_pkgs, function(p) suppressMessages(library(p, character.only = TRUE))))
options(scipen = 999)

# ---------------------------------------------------------------------------
# Fixed data paths (all relative to PROJECT_ROOT)
# ---------------------------------------------------------------------------
ACS_MAIN  <- raw_path("ipums_acs", "usa_00010.csv")   # 2007/2009/2019/2021
ACS_GIS   <- raw_path("ipums_acs", "usa_00011.csv")   # 2012/2021
NRI_PATH  <- raw_path("fema_nri",  "NRI_Table_CensusTracts.csv")
GAZ_PATH  <- raw_path("census_geometry", "2021_Gaz_tracts_national.txt")
CPI_PATH  <- raw_path("cpi_u",     "cpi_u_annual.csv")
SHP_ROOT  <- raw_path("census_geometry")
FHFA_PATH <- raw_path("fhfa_hpi",  "hpi_at_state.csv")

# CPI-U factor: convert 2012 dollars to 2021 purchasing power
CPI_FACTOR <- 270.970 / 229.594   # = 1.1802

# ---------------------------------------------------------------------------
# Shared helper: household-weight-adjusted median
# ---------------------------------------------------------------------------
wtd_median <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (!length(x)) return(NA_real_)
  o <- order(x); x <- x[o]; w <- w[o]
  x[which(cumsum(w) >= sum(w) / 2)[1L]]
}

cat("00_setup.R: project root =", PROJECT_ROOT, "\n")
