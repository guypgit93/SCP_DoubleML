# ─────────────────────────────────────────────────────────────────────────────
# 11_wild_cluster_bootstrap.R
# Wild-cluster bootstrap inference on the headline Adjusted
# Scotland x Post coefficient (official MDCH flag), as a confirmatory check
# on the household-clustered SEs reported in Stage 1.
#
# Section 2.3 clusters at household level (SERNUM, tens of thousands of
# clusters) rather than region, specifically to avoid a few-clusters problem
# (Scotland would be the only treated cluster among ~10 GB regions; Cameron &
# Miller 2015; MacKinnon & Webb 2018). This script checks that those
# household-clustered CR2 SEs aren't themselves an artefact of cluster-
# size/treatment imbalance, via wild-cluster bootstrap-t inference (Cameron,
# Gelbach & Miller 2008) rather than asymptotic cluster-robust SEs.
#
# This is a hand-rolled wild cluster restricted bootstrap using only
# feols()/coef()/predict(), not fwildclusterboot's boottest(). Fits the
# model with tp restricted to 0, generates B pseudo-outcomes by adding
# Rademacher-reweighted restricted residuals (weight drawn once per household
# cluster, applied to every row in it), refits the unrestricted model on each,
# and sees where the real t-statistic on tp falls in that distribution.
# ─────────────────────────────────────────────────────────────────────────────

library(data.table)
library(fixest)

# ─────────────────────────────────────────────────────────────────────────────
# CONFIG
# ─────────────────────────────────────────────────────────────────────────────
DATA_PATH  <- "/Users/guypigott/python-venv-demo/Dissertation/data/hbai_clean.csv"
TABLES_DIR <- "/Users/guypigott/Claude/Projects/MSc Dissertation/tables"
dir.create(TABLES_DIR, showWarnings = FALSE, recursive = TRUE)

set.seed(20260818)   # reproducibility
B <- 999             # number of bootstrap replications

# ─────────────────────────────────────────────────────────────────────────────
# LOAD + REBUILD
# ─────────────────────────────────────────────────────────────────────────────
cat("Loading data...\n")
df <- fread(DATA_PATH)   # same source CSV as 03_stage1_baseline_did.R; reloaded fresh, not sourced

df[, YEAR    := as.integer(YEAR)]
df[, treated := as.numeric(scotland)]   # 1 = Scotland, 0 = England
if ("post" %in% names(df)) {
  df[, post := as.numeric(post)]
} else {
  df[, post := as.numeric(YEAR >= 2023)]   # fallback if 01_hbai_prep.R's exact-date post column is missing
}
df[, tp     := treated * post]   # the DiD interaction term itself; this is the coefficient being bootstrapped
df[, YEAR_f := factor(YEAR)]     # year fixed effects, matching Stage 1's spec

df[, MDCH := suppressWarnings(as.numeric(MDCH))]   # coerce to numeric; non-numeric codes become NA
df[, MDCH := ifelse(MDCH < 0, NA_real_, MDCH)]      # negative codes are DWP "missing" sentinels

df[, young_head         := as.numeric(AGEHDBAND == 1)]         # head of household under 25
df[, female_head        := as.numeric(SEXHD == 2)]              # 2 = female in HBAI's SEXHD coding
df[, disabled_household := as.numeric(DSCORFAM == 2)]           # 2 = at least one disabled member
df[, lone_parent        := as.numeric(MARITAL_WITHKID == 1)]    # 1 = single parent with children
df[, large_family       := as.numeric(NUMBKIDS == 3)]           # 3 = "3 or more" band in HBAI
df[, ETH_f              := factor(ifelse(ETH == 99, NA, ETH))]  # 99 = not stated, coded to NA

CASE_COVS <- c("young_head", "female_head", "ETH_f",
               "disabled_household", "lone_parent", "large_family")
avail_covs <- CASE_COVS[CASE_COVS %in% names(df)]   # guards against a covariate column missing from this data pull

df_mdch_flag <- df[!is.na(MDCH)]   # complete-case sample for the official MDCH flag

# Pre-filter to the exact complete-case, positive-weight rows: keeps this
# script's own bookkeeping (N, cluster count) unambiguous throughout.
model_vars <- c("MDCH", "treated", "tp", "YEAR_f", avail_covs, "GS_INDCH", "SERNUM")
df_reg <- df_mdch_flag[GS_INDCH > 0]
df_reg <- df_reg[complete.cases(df_reg[, ..model_vars])]
df_reg[, SERNUM := as.character(SERNUM)]

cat(sprintf("  N (MDCH flag, complete cases, positive weight): %s\n",
            format(nrow(df_reg), big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# Unrestricted model and restricted model
# ─────────────────────────────────────────────────────────────────────────────
# Unrestricted: the headline Adjusted spec itself, tp included.
fml_unrestricted <- as.formula(paste(
  "MDCH ~ treated + tp +", paste(avail_covs, collapse = " + "), "| YEAR_f"
))
# Restricted: same spec with tp dropped, imposing the null H0: tp = 0 that
# the bootstrap residuals are generated under.
fml_restricted <- as.formula(paste(
  "MDCH ~ treated +", paste(avail_covs, collapse = " + "), "| YEAR_f"
))

m_adjusted <- feols(fml_unrestricted, data = df_reg, weights = ~GS_INDCH, cluster = ~SERNUM)
stopifnot(nobs(m_adjusted) == nrow(df_reg))   # sanity check: fixest silently drops rows on some errors, this would catch it

cat("\n── Asymptotic (already-reported) result ────────────────────────────────\n")
print(summary(m_adjusted))   # the already-reported household-clustered result this script is checking

t_obs <- unname(coef(m_adjusted)["tp"]) / unname(fixest::se(m_adjusted)["tp"])   # observed t-stat, tested against the bootstrap distribution below

m_restricted <- feols(fml_restricted, data = df_reg, weights = ~GS_INDCH, cluster = ~SERNUM)
df_reg[, fitted_r := predict(m_restricted)]   # fitted values under H0: tp = 0
df_reg[, resid_r  := MDCH - fitted_r]          # residuals to be Rademacher-reweighted each bootstrap rep

# ─────────────────────────────────────────────────────────────────────────────
# WILD-CLUSTER BOOTSTRAP (Rademacher weights, drawn once per household
# cluster and applied to every row in that household)
# ─────────────────────────────────────────────────────────────────────────────
cat(sprintf("\n── Wild-cluster bootstrap (B=%d, Rademacher weights) ──────────────\n", B))

clusters   <- unique(df_reg$SERNUM)
n_clusters <- length(clusters)
t_boot     <- rep(NA_real_, B)   # one bootstrap t-stat per replication; stays NA if that rep fails

# Same formula as fml_unrestricted, but regressing on the pseudo-outcome
# MDCH_star instead of the real MDCH.
fml_boot <- as.formula(paste(
  "MDCH_star ~ treated + tp +", paste(avail_covs, collapse = " + "), "| YEAR_f"
))

for (b in seq_len(B)) {
  # Draw one Rademacher weight (+1/-1) per household cluster, not per row:
  # every row in the same SERNUM gets the same sign this replication.
  v <- sample(c(-1, 1), n_clusters, replace = TRUE)
  names(v) <- clusters
  df_reg[, wild_w    := v[SERNUM]]                        # broadcast cluster weight to every row
  df_reg[, MDCH_star := fitted_r + resid_r * wild_w]       # restricted fit + reweighted residual

  # Refit the unrestricted model on the pseudo-outcome; NULL on convergence failure.
  m_b <- tryCatch(
    feols(fml_boot, data = df_reg, weights = ~GS_INDCH, cluster = ~SERNUM, notes = FALSE),
    error = function(e) NULL
  )

  # Only record a t-stat if the fit succeeded, tp survived (wasn't dropped
  # for collinearity), and its SE is a usable positive number.
  if (!is.null(m_b) && "tp" %in% names(coef(m_b))) {
    se_b <- unname(fixest::se(m_b)["tp"])
    if (!is.na(se_b) && se_b > 0) {
      t_boot[b] <- unname(coef(m_b)["tp"]) / se_b
    }
  }

  if (b %% 200 == 0) cat(sprintf("  ... %d / %d\n", b, B))
}

t_boot     <- t_boot[!is.na(t_boot)]   # drop failed replications before computing the p-value
wildboot_p <- mean(abs(t_boot) >= abs(t_obs))   # share of bootstrap t-stats at least as extreme as observed

# Percentile bootstrap CI on the coefficient itself, as a companion to the
# p-value. Rather than bootstrapping the coefficient directly in a second
# loop, this reuses t_boot's implied scale: the standard WCR shortcut,
# CI = coef +/- t_boot quantiles * SE.
boot_ci <- coef(m_adjusted)["tp"] - rev(quantile(t_boot, c(0.025, 0.975))) * fixest::se(m_adjusted)["tp"]
boot_ci <- unname(sort(boot_ci))   # sort since the reversed quantile subtraction can flip the endpoint order

cat(sprintf(
  "\nHeadline tp coefficient: %.4f\nAsymptotic (household-clustered) SE p-value: %.4f\nWild-cluster bootstrap p-value:            %.4f\nWild-cluster bootstrap 95%% CI: [%.4f, %.4f]\nBootstrap reps used (non-failed): %d / %d\n",
  coef(m_adjusted)["tp"],
  fixest::pvalue(m_adjusted)["tp"],
  wildboot_p,
  boot_ci[1], boot_ci[2],
  length(t_boot), B
))

# ─────────────────────────────────────────────────────────────────────────────
# SAVE
# ─────────────────────────────────────────────────────────────────────────────
out <- data.frame(
  outcome        = "Official MDCH flag (Adjusted)",
  coef           = unname(coef(m_adjusted)["tp"]),        # headline DiD coefficient
  asymp_se       = unname(fixest::se(m_adjusted)["tp"]),   # already-reported household-clustered SE
  asymp_p        = unname(fixest::pvalue(m_adjusted)["tp"]),
  wildboot_p     = wildboot_p,       # this script's own bootstrap p-value, for comparison
  wildboot_ci_lo = boot_ci[1],
  wildboot_ci_hi = boot_ci[2],
  n_obs          = nobs(m_adjusted),
  n_clusters     = n_clusters,
  B_reps_used    = length(t_boot),   # replications that actually succeeded
  B_reps_target  = B                 # replications requested (B_reps_used <= this)
)
write.csv(out, file.path(TABLES_DIR, "table_wildboot_headline.csv"), row.names = FALSE)
cat(sprintf("\n✓ Saved tables/table_wildboot_headline.csv\n"))
cat("\nCopy the printed coef / asymp_p / wildboot_p / wildboot CI numbers above back to Claude to write up.\n")
