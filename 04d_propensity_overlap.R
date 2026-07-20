# ─────────────────────────────────────────────────────────────────────────────
# 04d_propensity_overlap.R
# Diagnostic: extract and inspect the fitted propensity scores underlying
# 04_dml_did.R's DR-DiD "Adjusted" spec (xformla_adj = lean z-scored covs).
#
# att_gt(est_method = "dr") doesn't expose per-unit propensity scores in its
# return object -- DRDID::drdid_rc() fits/discards them internally for each
# (g,t) cell. This script refits the same logistic model (treated ~ lean
# covariates, survey-weighted by GS_INDCH, matching how weightsname is passed
# through inside att_gt()) on the pooled analysis sample, then splits by
# pre/post period as a proxy for the per-(g,t)-cell comparisons att_gt() runs
# internally. Not a bit-for-bit replication of every (g,t) cell -- a common-
# support/overlap check on the same covariate set and weighting scheme.
#
# Question this answers: since treatment (Scotland) is a deterministic
# function of geography and NOT of the covariates in xformla_adj, does the
# propensity model still produce non-degenerate scores with real overlap
# between Scotland and England, or does it push mass toward 0/1?
#
# Run after 01_hbai_prep.R-equivalent load (script is self-contained, loads
# its own copy of the CSV like 03/04/05 do).
#
# Outputs
#   tables/  table_A4_propensity_overlap.csv   (dist. of p-scores by group)
#   figures/ ps_overlap_pooled.png             (density, treated vs control)
#   figures/ ps_overlap_by_period.png          (pre vs post, small multiples)
#   figures/ ps_overlap_by_year.png            (per-year small multiples)
# ─────────────────────────────────────────────────────────────────────────────

library(data.table)
library(tidyverse)
library(ggplot2)

# ─────────────────────────────────────────────────────────────────────────────
# CONFIG (mirrors 04_dml_did.R)
# ─────────────────────────────────────────────────────────────────────────────
DATA_PATH   <- "/Users/guypigott/python-venv-demo/Dissertation/data/hbai_lca.csv"
TABLES_DIR  <- "/Users/guypigott/Claude/Projects/MSc Dissertation/tables"
FIGURES_DIR <- "/Users/guypigott/Claude/Projects/MSc Dissertation/figures"
dir.create(TABLES_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

SCP_EXPAND_YEAR <- 2023
TRIM_LO <- 0.10   # conventional trimming band -- flag scores outside [0.10, 0.90]
TRIM_HI <- 0.90
TRIM_LO_STRICT <- 0.05
DRDID_TRIM_LEVEL <- 0.995   # DRDID::drdid_rc()'s own default control-group trim
TRIM_HI_STRICT <- 0.95

cat("Loading data...\n")
df <- fread(DATA_PATH)

df[, YEAR    := as.integer(YEAR)]
df[, treated := as.numeric(scotland)]
if ("post" %in% names(df)) {
  cat("  Using `post` already in hbai_lca.csv (may carry 01_hbai_prep.R's exact-interview-date\n")
  cat("  refinement for FY2022/23) rather than recomputing from YEAR.\n")
  df[, post := as.numeric(post)]
} else {
  df[, post := as.numeric(YEAR >= SCP_EXPAND_YEAR)]
}
df <- df[YEAR != 2021]  # dropped throughout the pipeline (COVID survey disruption)

# ─────────────────────────────────────────────────────────────────────────────
# COVARIATES -- identical construction to 04_dml_did.R's xformla_adj
# ─────────────────────────────────────────────────────────────────────────────
LEAN_COVS_RAW <- c("AGE", "NUMBKIDS", "ADULTH", "S_OE_BHC", "S_OE_AHC", "EHCOST")
LEAN_COVS_RAW <- LEAN_COVS_RAW[LEAN_COVS_RAW %in% names(df)]

for (v in LEAN_COVS_RAW) {
  df[[paste0(v, "_z")]] <- as.numeric(scale(df[[v]]))
}
LEAN_COVS_Z <- paste0(LEAN_COVS_RAW, "_z")
if ("DIS" %in% names(df)) LEAN_COVS_Z <- c(LEAN_COVS_Z, "DIS")

cat(sprintf("Propensity covariates: %s\n", paste(LEAN_COVS_Z, collapse = ", ")))

cols_need <- unique(c("treated", "post", "YEAR", "GS_INDCH", LEAN_COVS_Z))
cols_need <- cols_need[cols_need %in% names(df)]
sub <- na.omit(df[, ..cols_need])
cat(sprintf("Complete-case analysis sample: %s rows (%s Scotland, %s England)\n",
            format(nrow(sub), big.mark = ","),
            format(sum(sub$treated == 1), big.mark = ","),
            format(sum(sub$treated == 0), big.mark = ",")))

ps_formula <- as.formula(paste("treated ~", paste(LEAN_COVS_Z, collapse = " + ")))

# ─────────────────────────────────────────────────────────────────────────────
# HELPER: fit weighted logit, return data with fitted p-scores attached
# ─────────────────────────────────────────────────────────────────────────────
# NOTE: parameter deliberately NOT named `data` -- glm()'s NSE evaluation of
# `weights=` resolves symbols against model.frame's own `data` argument
# first, then falls back to environment(ps_formula) -- the GLOBAL env, since
# the formula was built once at top level -- NOT fit_ps()'s local frame.
# A symbol like `wts` or `data` that only exists locally in fit_ps() is
# therefore invisible to that lookup (and `data` resolves to the base
# utils::data() closure in the global env instead). Fix: put the weight
# column INSIDE the data frame passed to `data=` so it resolves from the
# data mask directly, without needing any environment fallback.
# GS_INDCH is a population grossing factor ("SPI'd grossing factor for
# dependant children" per the HBAI variable guide), not a small precision
# weight -- raw values inflate the effective N into the millions, which
# makes glm.fit() numerically unstable (non-convergence, fitted probs stuck
# at 0/1, nonsense AIC) even though the true beta-hat is invariant to a
# uniform rescaling of the weights. DRDID::drdid_rc() (which att_gt(est_
# method="dr") calls internally) explicitly does `i.weights/mean(i.weights)`
# before fitting its own propensity logit -- confirmed by reading its source
# on GitHub (pedrohcgs/DRDID/R/drdid_rc.R). Matching that convention here so
# this diagnostic is numerically well-behaved AND faithful to what att_gt()
# actually estimates internally.
fit_ps <- function(dat, label = "") {
  dat$.glm_weight <- dat$GS_INDCH / mean(dat$GS_INDCH)
  m <- glm(ps_formula, data = dat, family = binomial(), weights = .glm_weight)
  dat$ps <- fitted(m)
  cat(sprintf("  %-12s n=%-7d  AIC=%.1f  ps range=[%.4f, %.4f]\n",
              label, nrow(dat), AIC(m), min(dat$ps), max(dat$ps)))
  dat
}

summarise_ps <- function(dat, label) {
  dat |>
    group_by(treated) |>
    summarise(
      n           = n(),
      mean_ps     = mean(ps),
      p01         = quantile(ps, .01),
      p05         = quantile(ps, .05),
      p10         = quantile(ps, .10),
      p50         = quantile(ps, .50),
      p90         = quantile(ps, .90),
      p95         = quantile(ps, .95),
      p99         = quantile(ps, .99),
      min_ps      = min(ps),
      max_ps      = max(ps),
      pct_outside_10_90 = 100 * mean(ps < TRIM_LO | ps > TRIM_HI),
      pct_outside_05_95 = 100 * mean(ps < TRIM_LO_STRICT | ps > TRIM_HI_STRICT),
      # DRDID::drdid_rc() zeroes out control-group (treated==0) obs with
      # ps > 0.995 by default (trim.level). Only meaningful for treated==0 --
      # shown for treated==1 too for reference, but att_gt() never trims them.
      pct_ps_gt_drdid_trim = 100 * mean(ps > DRDID_TRIM_LEVEL),
      .groups = "drop"
    ) |>
    mutate(sample = label, group = if_else(treated == 1, "Scotland (treated)", "England (control)"))
}

# ─────────────────────────────────────────────────────────────────────────────
# 1) POOLED (full sample, all years)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n== Pooled (all years) ========================================================\n")
sub_pooled <- fit_ps(sub, "Pooled")
tab_pooled <- summarise_ps(sub_pooled, "Pooled (all years)")

# ─────────────────────────────────────────────────────────────────────────────
# 2) PRE vs POST -- proxy for the base-period-vs-t comparisons att_gt() runs
# ─────────────────────────────────────────────────────────────────────────────
cat("\n== Pre vs post period =========================================================\n")
sub_pre  <- fit_ps(sub[sub$post == 0, ], "Pre (<2023)")
sub_post <- fit_ps(sub[sub$post == 1, ], "Post (>=2023)")
tab_period <- bind_rows(
  summarise_ps(sub_pre,  "Pre-period (YEAR<2023)"),
  summarise_ps(sub_post, "Post-period (YEAR>=2023)")
)

# ─────────────────────────────────────────────────────────────────────────────
# 3) PER-YEAR -- most granular check, closest to what a given att_gt() (g,t)
#    cell actually sees (two adjacent survey years' worth of RCS data)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n== Per year ====================================================================\n")
years <- sort(unique(sub$YEAR))
sub_by_year <- lapply(years, function(y) fit_ps(sub[sub$YEAR == y, ], as.character(y))) |> bind_rows()
tab_year <- lapply(years, function(y) {
  summarise_ps(sub_by_year[sub_by_year$YEAR == y, ], as.character(y))
}) |> bind_rows()

# ─────────────────────────────────────────────────────────────────────────────
# COMBINE + WRITE TABLE
# ─────────────────────────────────────────────────────────────────────────────
tab_all <- bind_rows(tab_pooled, tab_period, tab_year) |>
  select(sample, group, n, mean_ps, p01, p05, p10, p50, p90, p95, p99,
         min_ps, max_ps, pct_outside_10_90, pct_outside_05_95, pct_ps_gt_drdid_trim)

write_csv(tab_all, file.path(TABLES_DIR, "table_A4_propensity_overlap.csv"))
cat("\nwrote table_A4_propensity_overlap.csv\n")

cat("\n── Overlap summary (pooled + pre/post) ────────────────────────────────────\n")
print(tab_all |> filter(sample %in% c("Pooled (all years)", "Pre-period (YEAR<2023)", "Post-period (YEAR>=2023)")) |>
        mutate(across(where(is.numeric), \(x) round(x, 4))), width = Inf)

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE 1: pooled density, treated vs control
# ─────────────────────────────────────────────────────────────────────────────
p1 <- ggplot(sub_pooled, aes(x = ps, fill = factor(treated, labels = c("England", "Scotland")))) +
  geom_density(alpha = 0.5, colour = NA) +
  geom_vline(xintercept = c(TRIM_LO, TRIM_HI), linetype = "dotted", colour = "grey40") +
  scale_fill_manual(values = c("England" = "#2c7bb6", "Scotland" = "#d7191c"), name = NULL) +
  labs(title = "Propensity score overlap: Scotland vs England (pooled, all years)",
       subtitle = sprintf("P(Scotland | %s) -- dotted lines mark conventional [%.2f, %.2f] trim band",
                           paste(LEAN_COVS_RAW, collapse = ", "), TRIM_LO, TRIM_HI),
       x = "Fitted propensity score", y = "Density") +
  theme_minimal(base_size = 11)
ggsave(file.path(FIGURES_DIR, "ps_overlap_pooled.png"), p1, width = 9, height = 5.5, dpi = 150)
cat("wrote ps_overlap_pooled.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE 2: pre vs post period
# ─────────────────────────────────────────────────────────────────────────────
sub_period <- bind_rows(
  sub_pre  |> mutate(period = "Pre-period (YEAR<2023)"),
  sub_post |> mutate(period = "Post-period (YEAR>=2023)")
)
p2 <- ggplot(sub_period, aes(x = ps, fill = factor(treated, labels = c("England", "Scotland")))) +
  geom_density(alpha = 0.5, colour = NA) +
  geom_vline(xintercept = c(TRIM_LO, TRIM_HI), linetype = "dotted", colour = "grey40") +
  facet_wrap(~period, ncol = 1) +
  scale_fill_manual(values = c("England" = "#2c7bb6", "Scotland" = "#d7191c"), name = NULL) +
  labs(title = "Propensity score overlap by period",
       x = "Fitted propensity score", y = "Density") +
  theme_minimal(base_size = 11)
ggsave(file.path(FIGURES_DIR, "ps_overlap_by_period.png"), p2, width = 9, height = 7, dpi = 150)
cat("wrote ps_overlap_by_period.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE 3: per-year small multiples
# ─────────────────────────────────────────────────────────────────────────────
p3 <- ggplot(sub_by_year, aes(x = ps, fill = factor(treated, labels = c("England", "Scotland")))) +
  geom_density(alpha = 0.5, colour = NA) +
  geom_vline(xintercept = c(TRIM_LO, TRIM_HI), linetype = "dotted", colour = "grey40") +
  facet_wrap(~YEAR) +
  scale_fill_manual(values = c("England" = "#2c7bb6", "Scotland" = "#d7191c"), name = NULL) +
  labs(title = "Propensity score overlap by survey year",
       x = "Fitted propensity score", y = "Density") +
  theme_minimal(base_size = 10)
ggsave(file.path(FIGURES_DIR, "ps_overlap_by_year.png"), p3, width = 11, height = 8, dpi = 150)
cat("wrote ps_overlap_by_year.png\n")

cat("\nDone. Check table_A4_propensity_overlap.csv's pct_outside_10_90 / pct_outside_05_95\n")
cat("columns -- large shares of either group near the boundary would mean the DR-DiD\n")
cat("estimator is leaning heavily on extrapolation (outcome-regression side) rather than\n")
cat("genuine reweighting (IPW side) for those cells.\n")
