# ─────────────────────────────────────────────────────────────────────────────
# 12_wild_cluster_bootstrap.R
# ROBUSTNESS: wild-cluster bootstrap inference on the headline Adjusted
# Scotland x Post coefficient (official MDCH flag), as a modern-best-practice
# confirmatory check on the household-clustered SEs reported in Stage 1.
#
# WHY THIS CHECK, AND WHAT IT DOES / DOES NOT ANSWER:
# Section 2.3 already flags that region-level clustering would leave Scotland
# as the only treated cluster among ~10 GB regions (Cameron & Miller 2015;
# MacKinnon & Webb 2018), and resolves this by clustering at household level
# instead (SERNUM), which has tens of thousands of clusters -- the standard
# cluster-robust asymptotics are on solid ground there, this is NOT a "few
# clusters" problem in the SE sense. What this script adds is a belt-and-
# braces check that the household-clustered CR2 SEs already reported are not
# themselves an artefact of the (mild) imbalance in cluster sizes / treatment
# assignment at household level, using wild-cluster bootstrap-t inference
# (Cameron, Gelbach & Miller 2008) rather than asymptotic cluster-robust SEs.
# A bootstrap p-value close to the already-reported asymptotic p-value is a
# reassuring confirmation, not a new finding -- report it as exactly that,
# don't oversell it.
#
# This does NOT test whether Scotland being the only treated NATION could
# itself be an idiosyncratic-shock confound (that's a different, harder
# question, partially addressed already by the Wales/NI falsification checks
# in 10_placebo_wales_ni.R). Don't conflate the two in the write-up.
#
# IMPLEMENTATION NOTE: this is a hand-rolled WCR (wild cluster restricted)
# bootstrap using only feols()/coef()/predict() -- NOT fwildclusterboot's
# boottest(), which threw inconsistent errors against this fixest/R version
# combination. Building it manually avoids that dependency entirely and is
# fully transparent: fit the model with tp restricted to 0, generate B
# pseudo-outcomes by adding Rademacher-reweighted restricted residuals
# (weight drawn once per household cluster, applied to every row in that
# cluster), refit the unrestricted model on each pseudo-outcome, and see
# where the real t-statistic on tp falls in that distribution.
#
# Run after 01_hbai_prep.R has produced hbai_clean.csv. Independent of every
# other script -- reads the same CSV as 03_stage1_baseline_did.R, refits only
# the one headline Adjusted MDCH model, and adds nothing to any other output.
#
# Runtime: B=1999 refits of a ~51k-row, 7-FE, clustered feols model -- a few
# minutes. Progress is printed every 200 reps. Drop B to 999 below for a
# faster, still-reasonable run if you're short on time.
# ─────────────────────────────────────────────────────────────────────────────

library(data.table)
library(fixest)

# ─────────────────────────────────────────────────────────────────────────────
# CONFIG -- same paths/conventions as 03_stage1_baseline_did.R
# ─────────────────────────────────────────────────────────────────────────────
DATA_PATH  <- "/Users/guypigott/python-venv-demo/Dissertation/data/hbai_clean.csv"
TABLES_DIR <- "/Users/guypigott/Claude/Projects/MSc Dissertation/tables"
dir.create(TABLES_DIR, showWarnings = FALSE, recursive = TRUE)

set.seed(20260818)   # reproducibility
B <- 1999            # number of bootstrap replications

# ─────────────────────────────────────────────────────────────────────────────
# LOAD + REBUILD (identical construction to 03_stage1_baseline_did.R --
# copy-pasted rather than sourced, so this script has zero side effects on
# anything else and can be run standalone, any time, in any order)
# ─────────────────────────────────────────────────────────────────────────────
cat("Loading data...\n")
df <- fread(DATA_PATH)

df[, YEAR    := as.integer(YEAR)]
df[, treated := as.numeric(scotland)]
if ("post" %in% names(df)) {
  df[, post := as.numeric(post)]
} else {
  df[, post := as.numeric(YEAR >= 2023)]
}
df[, tp     := treated * post]
df[, YEAR_f := factor(YEAR)]

df[, MDCH := suppressWarnings(as.numeric(MDCH))]
df[, MDCH := ifelse(MDCH < 0, NA_real_, MDCH)]

df[, young_head         := as.numeric(AGEHDBAND == 1)]
df[, female_head        := as.numeric(SEXHD == 2)]
df[, disabled_household := as.numeric(DSCORFAM == 2)]
df[, lone_parent        := as.numeric(MARITAL_WITHKID == 1)]
df[, large_family       := as.numeric(NUMBKIDS == 3)]
df[, ETH_f              := factor(ifelse(ETH == 99, NA, ETH))]

CASE_COVS <- c("young_head", "female_head", "ETH_f",
               "disabled_household", "lone_parent", "large_family")
avail_covs <- CASE_COVS[CASE_COVS %in% names(df)]

df_mdch_flag <- df[!is.na(MDCH)]

# Pre-filter to the exact complete-case, positive-weight rows -- keeps this
# script's own bookkeeping (N, cluster count) unambiguous throughout.
model_vars <- c("MDCH", "treated", "tp", "YEAR_f", avail_covs, "GS_INDCH", "SERNUM")
df_reg <- df_mdch_flag[GS_INDCH > 0]
df_reg <- df_reg[complete.cases(df_reg[, ..model_vars])]
df_reg[, SERNUM := as.character(SERNUM)]

cat(sprintf("  N (MDCH flag, complete cases, positive weight): %s\n",
            format(nrow(df_reg), big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# UNRESTRICTED model (the headline Adjusted spec -- Table 3) and RESTRICTED
# model (same, but with tp dropped -- imposes the null H0: tp = 0, which is
# what a WCR bootstrap resamples under)
# ─────────────────────────────────────────────────────────────────────────────
fml_unrestricted <- as.formula(paste(
  "MDCH ~ treated + tp +", paste(avail_covs, collapse = " + "), "| YEAR_f"
))
fml_restricted <- as.formula(paste(
  "MDCH ~ treated +", paste(avail_covs, collapse = " + "), "| YEAR_f"
))

m_adjusted <- feols(fml_unrestricted, data = df_reg, weights = ~GS_INDCH, cluster = ~SERNUM)
stopifnot(nobs(m_adjusted) == nrow(df_reg))

cat("\n── Asymptotic (already-reported) result ────────────────────────────────\n")
print(summary(m_adjusted))

t_obs <- unname(coef(m_adjusted)["tp"]) / unname(fixest::se(m_adjusted)["tp"])

m_restricted <- feols(fml_restricted, data = df_reg, weights = ~GS_INDCH, cluster = ~SERNUM)
df_reg[, fitted_r := predict(m_restricted)]
df_reg[, resid_r  := MDCH - fitted_r]

# ─────────────────────────────────────────────────────────────────────────────
# WILD-CLUSTER BOOTSTRAP (Rademacher weights, drawn once per household
# cluster and applied to every row in that household)
# ─────────────────────────────────────────────────────────────────────────────
cat(sprintf("\n── Wild-cluster bootstrap (B=%d, Rademacher weights) ──────────────\n", B))

clusters   <- unique(df_reg$SERNUM)
n_clusters <- length(clusters)
t_boot     <- rep(NA_real_, B)

fml_boot <- as.formula(paste(
  "MDCH_star ~ treated + tp +", paste(avail_covs, collapse = " + "), "| YEAR_f"
))

for (b in seq_len(B)) {
  v <- sample(c(-1, 1), n_clusters, replace = TRUE)
  names(v) <- clusters
  df_reg[, wild_w    := v[SERNUM]]
  df_reg[, MDCH_star := fitted_r + resid_r * wild_w]

  m_b <- tryCatch(
    feols(fml_boot, data = df_reg, weights = ~GS_INDCH, cluster = ~SERNUM, notes = FALSE),
    error = function(e) NULL
  )

  if (!is.null(m_b) && "tp" %in% names(coef(m_b))) {
    se_b <- unname(fixest::se(m_b)["tp"])
    if (!is.na(se_b) && se_b > 0) {
      t_boot[b] <- unname(coef(m_b)["tp"]) / se_b
    }
  }

  if (b %% 200 == 0) cat(sprintf("  ... %d / %d\n", b, B))
}

t_boot     <- t_boot[!is.na(t_boot)]
wildboot_p <- mean(abs(t_boot) >= abs(t_obs))

# Percentile bootstrap CI on the coefficient itself, as a companion to the
# p-value (refit once per bootstrap coefficient draw is already captured
# above via t_boot; for the CI we bootstrap the coefficient directly using
# the same Rademacher draws is a second loop -- reuse t_boot's implied scale
# instead, which is the standard WCR shortcut: CI = coef +/- t_boot quantiles * SE)
boot_ci <- coef(m_adjusted)["tp"] - rev(quantile(t_boot, c(0.025, 0.975))) * fixest::se(m_adjusted)["tp"]
boot_ci <- unname(sort(boot_ci))

cat(sprintf(
  "\nHeadline tp coefficient: %.4f\nAsymptotic (household-clustered) SE p-value: %.4f\nWild-cluster bootstrap p-value:            %.4f\nWild-cluster bootstrap 95%% CI: [%.4f, %.4f]\nBootstrap reps used (non-failed): %d / %d\n",
  coef(m_adjusted)["tp"],
  fixest::pvalue(m_adjusted)["tp"],
  wildboot_p,
  boot_ci[1], boot_ci[2],
  length(t_boot), B
))

# ─────────────────────────────────────────────────────────────────────────────
# SAVE — single-row CSV, easy to drop straight into an appendix table
# ─────────────────────────────────────────────────────────────────────────────
out <- data.frame(
  outcome        = "Official MDCH flag (Adjusted)",
  coef           = unname(coef(m_adjusted)["tp"]),
  asymp_se       = unname(fixest::se(m_adjusted)["tp"]),
  asymp_p        = unname(fixest::pvalue(m_adjusted)["tp"]),
  wildboot_p     = wildboot_p,
  wildboot_ci_lo = boot_ci[1],
  wildboot_ci_hi = boot_ci[2],
  n_obs          = nobs(m_adjusted),
  n_clusters     = n_clusters,
  B_reps_used    = length(t_boot),
  B_reps_target  = B
)
write.csv(out, file.path(TABLES_DIR, "table_wildboot_headline.csv"), row.names = FALSE)
cat(sprintf("\n✓ Saved tables/table_wildboot_headline.csv\n"))
cat("\nCopy the printed coef / asymp_p / wildboot_p / wildboot CI numbers above back to Claude to write up.\n")
