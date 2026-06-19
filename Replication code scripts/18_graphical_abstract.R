# =============================================================================
# 18_graphical_abstract.R
# Builds the journal graphical abstract for the current ("Geography of Incomplete
# Recovery") analysis. Three stacked panels, matching the previous abstract's layout:
#   (1) forest plot of the 2SLS estimate under four standard-error regimes,
#       showing the apparent catch-up vanishing once spatial dependence is honored;
#   (2) a short "why the causal effect is not identified" text block: the strong
#       Bartik instrument fails a pre-trend falsification (it is ~87% construction),
#       and the one instrument that passes (HMDA) is too weak for sharp inference;
#   (3) the LISA recovery-cluster map (reused Figure 4) showing regional persistence.
#
# Inputs : processed/results_causal_revamp.csv, processed/results_causal_channels.csv,
#          figures/fig4_lisa_map.png
# Outputs: figures/Graphical_Abstract.png and .pdf
# =============================================================================

source("00_setup.R")
library(ggplot2); library(cowplot); library(readr); library(grid); library(png)

rev <- read_csv(processed_path("results_causal_revamp.csv"),   show_col_types = FALSE)
ch  <- read_csv(processed_path("results_causal_channels.csv"), show_col_types = FALSE)

sub_out <- function(...) fig_path(...)

# ---- colours -------------------------------------------------------------
head_col <- "#1f4e79"; sig_col <- "#c0392b"; null_col <- "#34495e"
accent   <- "#b9770e"

# ---- Panel 1: forest plot of the combined 2SLS beta under 4 SE regimes ----
b <- rev$iv_beta[1]
se <- data.frame(
  method = c("Classical (i.i.d.)", "Heteroskedasticity-robust",
             "Clustered by state", "Conley spatial (200 km)"),
  se = c(rev$iv_se_iid[1], rev$iv_se_hc[1], rev$iv_se_clust[1], rev$iv_se_conley[1])
)
se$method <- factor(se$method, levels = rev(se$method))   # Classical at top
se$beta <- b
se$lo <- b - 1.96 * se$se
se$hi <- b + 1.96 * se$se
se$sig <- ifelse(se$hi < 0 | se$lo > 0, "Excludes 0", "Includes 0")

p_coef <- ggplot(se, aes(y = method)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_errorbarh(aes(xmin = lo, xmax = hi, colour = sig), height = 0.22, linewidth = 0.9) +
  geom_point(aes(x = beta, colour = sig), size = 2.6) +
  geom_vline(xintercept = rev$ols_beta[1], linetype = "dotted", colour = null_col) +
  annotate("text", x = rev$ols_beta[1], y = 0.55, label = "OLS = -0.167",
           size = 3, colour = null_col, hjust = 1.05) +
  scale_colour_manual(values = c("Excludes 0" = sig_col, "Includes 0" = null_col), name = NULL) +
  scale_x_continuous(limits = c(-0.75, 0.35), breaks = seq(-0.6, 0.3, 0.3)) +
  labs(x = expression("2SLS estimate "*hat(beta)*" (95% CI)"), y = NULL) +
  theme_minimal_vgrid(font_size = 11) +
  theme(legend.position = c(0.82, 0.25), legend.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(2, 14, 2, 2))

# ---- Panel 3: LISA map (reused Figure 4) ---------------------------------
img <- readPNG(fig_path("fig4_lisa_map.png"))
map_grob <- rasterGrob(img, interpolate = TRUE)
p_map <- ggdraw() + draw_grob(map_grob)

# ---- text helpers --------------------------------------------------------
title_block <- ggdraw() +
  draw_label("The Geography of Incomplete Recovery",
             fontface = "bold", size = 22, colour = "#14233a", x = 0.5, y = 0.80) +
  draw_label("Recession severity, the COVID-19 boom, and the limits of market self-correction",
             fontface = "italic", size = 12.5, colour = head_col, x = 0.5, y = 0.52) +
  draw_label(paste0("Did the hardest-hit U.S. housing markets catch up during the 2020-2021 boom,\n",
                    "or does the apparent catch-up dissolve under credible identification?"),
             size = 10.5, colour = "grey25", x = 0.5, y = 0.18, lineheight = 1.1)

h1 <- ggdraw() + draw_label(
  "1.  The headline: an apparent catch-up that vanishes under credible identification",
  fontface = "bold", size = 13, colour = head_col, x = 0.012, hjust = 0)
cap1 <- ggdraw() + draw_label(
  paste0("County 2SLS (2,709 counties), two instruments, first-stage F = 22.7. beta = -0.233 keeps the OLS sign; it is not a reversal.\n",
         "Significant only under naive errors; once the strong spatial dependence (Moran's I = 0.70) is honored it is\n",
         "indistinguishable from zero, and a weak-instrument-robust Anderson-Rubin test cannot reject beta = 0 (p = 0.07)."),
  size = 9, colour = "grey25", x = 0.012, hjust = 0, lineheight = 1.05)

h2 <- ggdraw() + draw_label("2.  Why the apparent catch-up is a mirage that cannot be pinned down",
  fontface = "bold", size = 13, colour = head_col, x = 0.012, hjust = 0)
body2 <- ggdraw() + draw_label(
  paste0(
   "The OLS catch-up (beta = -0.167) is what mean reversion and measurement error produce even with no causal effect.\n",
   "Testing it needs an instrument, and no available one is both strong and valid. The Bartik industry-mix shock is\n",
   "strong (F = 21) but INVALID: it fails a pre-trend placebo (it predicts the 2012-2019 housing trend it should not),\n",
   "because Rotemberg weights show it is ~87% construction employment, which is the housing cycle itself. The HMDA\n",
   "high-cost lending share (F = 28) passes the placebo but is too weak: its estimate loses significance under spatial\n",
   "inference. Between a strong-but-invalid and a valid-but-weak instrument, the causal effect is NOT IDENTIFIED."),
  size = 9.5, colour = "grey15", x = 0.012, hjust = 0, lineheight = 1.12)

h3 <- ggdraw() + draw_label("3.  Recovery failure is regional and persistent",
  fontface = "bold", size = 13, colour = head_col, x = 0.012, hjust = 0)
cap3 <- ggdraw() + draw_label(
  "Local clusters of recovery (LISA): 360 Low-Low cores in the Great Lakes corridor, Appalachia, and the Mississippi Delta.",
  size = 9, colour = "grey25", x = 0.012, hjust = 0)

footer <- ggdraw() +
  draw_label(
    paste0("Because recovery does not self-correct, this spatial inequality is durable, not transitional.\n",
           "Policy use: the Low-Low footprint is a ready, reproducible CRA assessment-area targeting screen."),
    fontface = "bold", size = 10, colour = "#14233a", x = 0.012, y = 0.74, hjust = 0, lineheight = 1.1) +
  draw_label(
    paste0("Recovery index 2012-2021 (2,351 PUMAs); causal design 2,709 counties.\n",
           "Sources: IPUMS ACS, FHFA House Price Index, Census County Business Patterns, HMDA, FEMA National Risk Index, Census TIGER/Line."),
    fontface = "italic", size = 7.8, colour = "grey35", x = 0.012, y = 0.24, hjust = 0, lineheight = 1.15)

# ---- compose -------------------------------------------------------------
ga <- plot_grid(
  title_block,
  h1, p_coef, cap1,
  h2, body2,
  h3, p_map, cap3,
  footer,
  ncol = 1,
  rel_heights = c(1.5, 0.45, 2.7, 1.0, 0.45, 1.9, 0.45, 4.4, 0.5, 1.3)
) + theme(plot.background = element_rect(fill = "white", colour = NA))

ggsave(sub_out("Graphical_Abstract.png"), ga, width = 8.6, height = 12.4, dpi = 300, bg = "white")
ggsave(sub_out("Graphical_Abstract.pdf"), ga, width = 8.6, height = 12.4, bg = "white")
cat("Saved Graphical_Abstract.png and .pdf to data and Replication codes/figures/\n")
