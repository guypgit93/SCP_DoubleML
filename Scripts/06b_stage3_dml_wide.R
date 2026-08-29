# ─────────────────────────────────────────────────────────────────────────────
# 06b_stage3_dml_wide.R
# Stage 3, WIDE covariate arm: same causalweight::didDML() setup as
# 06_stage3_dml_lean.R (identical estimator, outcome, ML methods), but a much
# richer ~40-variable covariate set instead of the lean six CASE controls --
# the setting where DML's high-dimensional tolerance should show up (unlike
# the archived att_gt() approach, archive/04_dml_did.R, whose per-cell models
# went singular/rank-deficient on a rich covariate set).
#
# Covariate set curated from 01_hbai_prep.R's ~65-variable DML block, typed
# against the HBAI variable guide (determines z-scoring/0-1/one-hot below):
#
# MLmethod restricted to lasso + randomforest -- no ensemble (too slow even
# on the lean set; see 06's header).
# ─────────────────────────────────────────────────────────────────────────────

library(causalweight)
library(data.table)
library(tidyverse)
library(ggplot2)

# ─────────────────────────────────────────────────────────────────────────────
# CONFIG
# ─────────────────────────────────────────────────────────────────────────────
DATA_PATH   <- "/Users/guypigott/python-venv-demo/Dissertation/data/hbai_clean.csv"
TABLES_DIR  <- "/Users/guypigott/Claude/Projects/MSc Dissertation/tables"
FIGURES_DIR <- "/Users/guypigott/Claude/Projects/MSc Dissertation/figures"
dir.create(TABLES_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

ALPHA      <- 0.05
CLUSTERVAR <- "SERNUM"
K_FOLDS    <- 3
TRIM       <- 0.05
ML_METHODS <- c("lasso", "randomforest")

cat("Loading data...\n")
df <- fread(DATA_PATH)
df[, YEAR := as.integer(YEAR)]

if (!"post" %in% names(df)) {
  stop("`post` column not found in hbai_clean.csv -- rerun 01_hbai_prep.R first.")
}
df[, treated := as.numeric(scotland)]
df[, post    := as.numeric(post)]

df$MDCH <- suppressWarnings(as.numeric(df$MDCH))
df$MDCH <- ifelse(df$MDCH < 0, NA_real_, df$MDCH)   # negative codes are DWP "missing" sentinels
df_mdch <- df[mdch_observed == 1]

# ─────────────────────────────────────────────────────────────────────────────
# WIDE COVARIATE SET -- see header for curation rationale
# ─────────────────────────────────────────────────────────────────────────────
CONTINUOUS_VARS <- c(
  "AGE", "AGEHD",                                              # Years old
  "S_OE_BHC", "S_OE_AHC", "S_OE_GRO",                          # £ equivalised income
  "S_OE_GRO_PROP_EARN", "S_OE_GRO_PROP_BEN", "S_OE_GRO_PROP_INV", # Value (income shares)
  "EHCOST", "ES_HCOST",                                        # £ housing costs
  "CHBENBU", "ESBENIBU", "INCHILBU", "EGRINCBU", "WINPAYBU"
  # WFTCBU dropped (2026-07-17 run: 100% NA) -- Working Families' Tax Credit
  # was abolished in 2003; this dataset covers 2017-2024, so the column is
  # pure legacy and empty throughout.
)

# "Category"-typed but count-like -- treated as continuous (ordinal-as-
# continuous), same convention already used for NUMBKIDS/ADULTH in the lean
# set (06_stage3_dml_lean.R).
COUNT_VARS <- c("NUMBKIDS", "ADULTH")

CATEGORICAL_VARS <- c(
  "DIS", "DIS_TYPE", "DSCORFAM", "BENBU_DISBEN",
  "TENHBAI", "ETHGRPHHPUB",
  "NEWFAMBU_KID", "MARITAL_KID",
  "BENBU_FSM", "BENBU_IS", "BENBU_JSA", "BENBU_ESA",
  "BENBU_HB", "BENBU_CTC", "BENBU_WTC", "BENBU_PC",
  "SEX", "SEXHD"
)

CONTINUOUS_VARS <- CONTINUOUS_VARS[CONTINUOUS_VARS %in% names(df_mdch)]
COUNT_VARS      <- COUNT_VARS[COUNT_VARS %in% names(df_mdch)]
CATEGORICAL_VARS <- CATEGORICAL_VARS[CATEGORICAL_VARS %in% names(df_mdch)]
cat(sprintf("  Continuous (n=%d): %s\n", length(CONTINUOUS_VARS), paste(CONTINUOUS_VARS, collapse=", ")))
cat(sprintf("  Count-as-continuous (n=%d): %s\n", length(COUNT_VARS), paste(COUNT_VARS, collapse=", ")))
cat(sprintf("  Categorical, to be auto-detected binary/multi-level (n=%d): %s\n",
            length(CATEGORICAL_VARS), paste(CATEGORICAL_VARS, collapse=", ")))

cols_need <- unique(c("MDCH", "treated", "post", CLUSTERVAR,
                       CONTINUOUS_VARS, COUNT_VARS, CATEGORICAL_VARS))
raw_sub <- df_mdch[, ..cols_need]
for (v in c(CONTINUOUS_VARS, COUNT_VARS, CATEGORICAL_VARS)) {
  raw_sub[[v]] <- suppressWarnings(as.numeric(raw_sub[[v]]))
  raw_sub[[v]] <- ifelse(raw_sub[[v]] < 0, NA_real_, raw_sub[[v]])  # HBAI sentinel negatives -> NA
}

# ── Per-variable missingness diagnostic, run BEFORE na.omit() so a single
# structurally-conditional variable is visible immediately
# instead of silently collapsing the joint complete-case sample to 0 rows. ──
na_report <- sapply(c(CONTINUOUS_VARS, COUNT_VARS, CATEGORICAL_VARS),
                     function(v) round(100 * mean(is.na(raw_sub[[v]])), 1))
na_report <- sort(na_report, decreasing = TRUE)
cat("\n  Per-variable missingness (%), highest first:\n")
print(na_report[na_report > 0])
if (any(na_report > 30)) {
  cat("\n  ⚠ Variable(s) above have >30% missingness -- check they're not structurally\n")
  cat("  conditional (like AGESP was) before proceeding. na.omit() below is joint across\n")
  cat("  ALL variables, so high-missingness ones compound fast.\n")
}

sub <- na.omit(raw_sub)
cat(sprintf("\n  Complete-case sample: %s rows (lean-set comparison was 46,197)\n",
            format(nrow(sub), big.mark = ",")))
if (nrow(sub) == 0) {
  stop("Complete-case sample is 0 rows -- check the missingness report above for the ",
       "culprit variable(s) before rerunning.")
}

# ── Build design matrix: z-score continuous/count vars, one-hot categoricals ──
for (v in c(CONTINUOUS_VARS, COUNT_VARS)) {
  sub[[paste0(v, "_z")]] <- as.numeric(scale(sub[[v]]))
}
scaled_vars <- paste0(c(CONTINUOUS_VARS, COUNT_VARS), "_z")

binary_vars <- c()
multilevel_vars <- c()
for (v in CATEGORICAL_VARS) {
  nlvl <- length(unique(sub[[v]]))
  if (nlvl <= 2) binary_vars <- c(binary_vars, v) else multilevel_vars <- c(multilevel_vars, v)
}
cat(sprintf("  Binary (kept 0/1): %s\n", paste(binary_vars, collapse=", ")))
cat(sprintf("  Multi-level (one-hot encoded): %s\n", paste(multilevel_vars, collapse=", ")))

# Recode binary vars to clean 0/1 (some HBAI flags use 1/2 rather than 0/1)
for (v in binary_vars) {
  lvls <- sort(unique(sub[[v]]))
  sub[[v]] <- as.numeric(sub[[v]] == lvls[length(lvls)])
}

dummy_mat <- NULL
if (length(multilevel_vars) > 0) {
  fml <- as.formula(paste("~", paste(multilevel_vars, collapse = " + "), "- 1"))
  dummy_mat <- model.matrix(fml, data = as.data.frame(lapply(sub[, ..multilevel_vars], factor)))
  cat(sprintf("  Multi-level vars expanded to %d dummy columns\n", ncol(dummy_mat)))
}

x_mat <- as.data.frame(sub[, ..scaled_vars])
if (!is.null(dummy_mat)) x_mat <- cbind(x_mat, as.data.frame(dummy_mat))
if (length(binary_vars) > 0) x_mat <- cbind(x_mat, as.data.frame(sub[, ..binary_vars]))
cat(sprintf("  Final design matrix: %d columns, %s rows\n", ncol(x_mat), format(nrow(x_mat), big.mark=",")))

# ─────────────────────────────────────────────────────────────────────────────
# RUN didDML() FOR EACH ML METHOD
# ─────────────────────────────────────────────────────────────────────────────
results <- list()
for (ml in ML_METHODS) {
  cat(sprintf("\n== didDML (WIDE covariates): MLmethod='%s' (est='dr', k=%d folds) ====\n",
              ml, K_FOLDS))
  cat(sprintf("  NOTE: %d covariate columns vs 10 in the lean set -- expect meaningfully\n", ncol(x_mat)))
  cat("  longer runtime than 06_stage3_dml_lean.R's equivalent run.\n")
  t0 <- Sys.time()
  fit <- tryCatch(
    didDML(
      y = sub$MDCH, d = sub$treated, t = sub$post, x = x_mat,
      MLmethod = ml, est = "dr", trim = TRIM,
      cluster = sub[[CLUSTERVAR]], k = K_FOLDS
    ),
    error = function(e) { cat(sprintf("  ✗ MLmethod='%s' failed: %s\n", ml, e$message)); NULL }
  )
  elapsed <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
  if (is.null(fit)) next

  crit <- qnorm(1 - ALPHA / 2)
  results[[ml]] <- data.frame(
    MLmethod = ml, covset = "wide", n_covariates = ncol(x_mat),
    ATET = fit$ATET, se = fit$se, pval = fit$pval,
    ci_lo = fit$ATET - crit * fit$se, ci_hi = fit$ATET + crit * fit$se,
    n_trimmed = fit$ntrimmed, n_used = nrow(sub) - fit$ntrimmed, runtime_s = elapsed
  )
  sig <- ifelse(fit$pval < .01, "***", ifelse(fit$pval < .05, "**", ifelse(fit$pval < .1, "*", "")))
  cat(sprintf("  ATET=%8.4f  SE=%8.4f  p=%6.3f  %s  (trimmed %d/%d, %.1fs)\n",
              fit$ATET, fit$se, fit$pval, sig, fit$ntrimmed, nrow(sub), elapsed))
}

# ─────────────────────────────────────────────────────────────────────────────
# SAVE + COMPARE AGAINST LEAN-SET RESULTS
# ─────────────────────────────────────────────────────────────────────────────
if (length(results) > 0) {
  results_df <- bind_rows(results)
  # Outcome-specific filename
  write.csv(results_df, file.path(TABLES_DIR, "dml_did_wide_covariates_MDCH.csv"), row.names = FALSE)
  cat("\n── Wide-covariate results (official MDCH flag) ─────────────────────────\n")
  print(results_df)
  cat(sprintf("\nwrote %s\n", file.path(TABLES_DIR, "dml_did_wide_covariates_MDCH.csv")))

  cat("\nCompare against:\n")
  cat("  - 03_stage1_baseline_did.R's OLS baseline (Stage 2, CASE/Andersen et al.\n")
  cat("    replication) -- is the baseline sensitive to covariate adjustment/functional\n")
  cat("    form. If the wide-set estimates move meaningfully (especially lasso, which\n")
  cat("    does its own variable selection via regularisation), that's evidence the\n")
  cat("    richer covariate set is adding real information -- exactly the setting where\n")
  cat("    DML's high-dimensional tolerance (vs. archive/04_dml_did.R's singular-matrix\n")
  cat("    failure with the same rich set) pays off.\n")
  cat("  - 06_stage3_dml_lean.R's lean-covariate estimates -- same estimator, same\n")
  cat("    outcome, only the covariate set differs.\n")
} else {
  cat("\n✗ No MLmethod succeeded.\n")
}
