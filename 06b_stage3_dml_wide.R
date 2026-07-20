# ─────────────────────────────────────────────────────────────────────────────
# 06b_stage3_dml_wide.R
# Stage 3 (Guy's 2026-07-17 narrative: "is this sensitive to covariate
# adjustment/functional form"). Same causalweight::didDML() setup as
# 06_stage3_dml_lean.R, but with a much richer covariate set instead of
# the 7-variable lean set -- deliberately the setting where DML's tolerance
# for high-dimensional X should show up, unlike att_gt() (04_dml_did.R),
# which needed the lean set because a rich one-hot covariate set made its
# per-(g,t)-cell models singular/rank-deficient. NOTE: att_gt()/DR-DiD (04)
# is NOT part of the final write-up per Guy's decision -- this comment stays
# only as the technical reason DML tolerates a wide X where att_gt() didn't.
#
# PRIMARY OUTCOME = official DWP MDCH flag ("MDCH"), matching Stage 2's
# baseline (CASE/Stewart et al. replication). mdch_any results already
# obtained (lasso -0.0955***, randomforest -0.0626 n.s.) stay valid as a
# secondary/robustness outcome, just not primary going forward. See
# 06_stage3_dml_lean.R's header for the interesting lasso-vs-RF reversal
# under mdch_any (RF beat lasso on the lean set, lost to it on the wide set --
# likely default `mtry` diluting across many sparse one-hot dummy covariates,
# something lasso's regularization handles more gracefully).
#
# Covariate set built from 01_hbai_prep.R's KEEP_VARS "DML covariates" block
# (~65 raw variables originally kept for this purpose but never used, since
# 04_dml_did.R had to fall back to the lean set). Types confirmed against
# the HBAI variable guide (5828_hbai_2425_harmonised_dataset_variables_guide.xlsx)
# rather than guessed from variable names -- every variable there is typed
# "Category" / "£ Amount" / "Years old" / "Value", which determines whether
# it's z-scored, kept as 0/1, or one-hot encoded below.
#
# Curation applied (not a raw dump of all 65 variables):
#   - Dropped *BAND variants (AGEBAND_CH, AGEHDBAND, AGESPBAND, ADULTHBAND,
#     AGEHDBAND_KID) -- strictly coarser re-encodings of continuous variables
#     already included (AGE, AGEHD, AGESP, ADULTH); no new information, just
#     redundant dummy columns.
#   - Dropped ETH / ETHGRPHH in favour of ETHGRPHHPUB, and PTENTYP2 in favour
#     of TENHBAI -- the variable guide itself recommends these over the
#     alternatives.
#   - Kept one representative each of family-type (NEWFAMBU_KID) and marital
#     status (MARITAL_KID) rather than all ~8 overlapping family-composition
#     variables (COUPLE_KID, NEWFAMBU_SINGLE/WITH/WITH_WA/WITH_PN/WITH_PN_TOT,
#     MARITAL_WITHKID) and the age-specific KID0_1..KID16PLUS counts, which
#     mostly reduce to combinations of NUMBKIDS + AGE already in the model.
#   - Kept DIS (existing) + DIS_TYPE + DSCORFAM + BENBU_DISBEN as the
#     disability set, skipping the remaining near-duplicate disability flags
#     (DISCORKID, DISCORABFLG, DSCORFAM_WORK, DSCORANDBEN, BENBU_DLA,
#     BENBU_PIP -- these overlap heavily with BENBU_DISBEN's definition).
#   - Kept all benefit-receipt flags (UC, FSM, IS, JSA, ESA, HB, CTC, WTC, PC)
#     as-is -- these are genuinely distinct programmes, not near-duplicates.
#
# Binary vs multi-level categoricals are detected automatically at runtime
# (by unique-value count) rather than hardcoded, since exact coding isn't
# knowable without the live data. Binary ones are used as 0/1 directly;
# multi-level ones are one-hot encoded via model.matrix() with one reference
# level dropped.
#
# MLmethod restricted to lasso + randomforest (no ensemble -- see
# 06_stage3_dml_lean.R's header for why: 75min/outcome, not viable).
#
# Requires: same packages as 06_stage3_dml_lean.R.
# ─────────────────────────────────────────────────────────────────────────────

library(causalweight)
library(data.table)
library(tidyverse)
library(ggplot2)

# ─────────────────────────────────────────────────────────────────────────────
# CONFIG
# ─────────────────────────────────────────────────────────────────────────────
DATA_PATH   <- "/Users/guypigott/python-venv-demo/Dissertation/data/hbai_lca.csv"
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
  stop("`post` column not found in hbai_lca.csv -- rerun 01_hbai_prep.R first.")
}
df[, treated := as.numeric(scotland)]
df[, post    := as.numeric(post)]

df$MDCH <- suppressWarnings(as.numeric(df$MDCH))
df$MDCH <- ifelse(df$MDCH < 0, NA_real_, df$MDCH)
df_mdch <- df[mdch_observed == 1]

# ─────────────────────────────────────────────────────────────────────────────
# WIDE COVARIATE SET -- see header for curation rationale
# ─────────────────────────────────────────────────────────────────────────────
CONTINUOUS_VARS <- c(
  "AGE", "AGEHD",                                              # Years old
  # AGESP (age of spouse) deliberately excluded -- structurally NA for
  # single-parent/single-person families (no spouse to have an age), not
  # randomly missing. Combined with ~40 other variables in one na.omit(),
  # this alone was enough to collapse the complete-case sample to 0 rows.
  # Family/partnership structure is already captured via MARITAL_KID /
  # NEWFAMBU_KID below without needing spouse-specific age.
  "S_OE_BHC", "S_OE_AHC", "S_OE_GRO",                          # £ equivalised income
  "S_OE_GRO_PROP_EARN", "S_OE_GRO_PROP_BEN", "S_OE_GRO_PROP_INV", # Value (income shares)
  "EHCOST", "ES_HCOST",                                        # £ housing costs
  # ERENTBU dropped (2026-07-17 run: 28.3% NA) -- structurally NA for
  # owner-occupiers (no rent component), same pattern as AGESP. TENHBAI
  # (tenure type, below) already captures renter-vs-owner, so this was
  # redundant on top of being a missingness risk.
  "CHBENBU", "ESBENIBU", "INCHILBU", "EGRINCBU", "WINPAYBU"
  # WFTCBU dropped (2026-07-17 run: 100% NA) -- Working Families' Tax Credit
  # was abolished in 2003; this dataset covers 2017-2024, so the column is
  # pure legacy and empty throughout.
)

# "Category"-typed but count-like -- treated as continuous (ordinal-as-
# continuous), same convention already used for NUMBKIDS/ADULTH in the lean
# set (06_stage3_dml_lean.R).
COUNT_VARS <- c("NUMBKIDS", "ADULTH")

# Everything else from the KEEP_VARS DML block that survived curation --
# auto-detected below as binary (kept 0/1) vs multi-level (one-hot encoded).
# EMPSTATI dropped (2026-07-17 run: 100% NA despite being a valid column --
# likely a harmonisation gap across the 3 stacked HBAI vintage files in
# 01_hbai_prep.R; worth investigating separately, not blocking on it here).
# BENBU_UC dropped (2026-07-17 run: 30.7% NA) -- likely reflects UC's
# gradual rollout/legacy-benefit migration overlapping this sample period;
# genuinely informative, not just noise, but too much missingness to include
# safely alongside ~35 other variables in one na.omit(). Worth reintroducing
# later as a 3-level factor (received / not received / not-yet-eligible-era)
# rather than dropping permanently.
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

# ── Per-variable missingness diagnostic -- run BEFORE na.omit() so a single
# structurally-conditional variable (like AGESP was) is visible immediately
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
  cat(sprintf("  NOTE: %d covariate columns vs 7 in the lean set -- expect meaningfully\n", ncol(x_mat)))
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
  # Outcome-specific filename -- the earlier mdch_any run's output
  # (dml_did_wide_covariates.csv: lasso -0.0955***, randomforest -0.0626 n.s.)
  # is intentionally NOT overwritten; it stays as the secondary-outcome table.
  write.csv(results_df, file.path(TABLES_DIR, "dml_did_wide_covariates_MDCH.csv"), row.names = FALSE)
  cat("\n── Wide-covariate results (official MDCH flag) ─────────────────────────\n")
  print(results_df)
  cat(sprintf("\nwrote %s\n", file.path(TABLES_DIR, "dml_did_wide_covariates_MDCH.csv")))

  cat("\nCompare against 03_stage1_baseline_did.R's OLS baseline for the official MDCH flag\n")
  cat("(Stage 2 -- the CASE/Stewart et al. replication). That comparison is what Stage 3\n")
  cat("is actually testing: is the baseline sensitive to covariate adjustment/functional form.\n")
  cat("If the wide-set estimates move meaningfully (especially lasso, which does its own\n")
  cat("variable selection via regularization), that's evidence the richer covariate set is\n")
  cat("adding real information -- exactly the setting where DML's high-dimensional tolerance\n")
  cat("(vs att_gt()'s singular-matrix failure with the same rich set) pays off.\n")
} else {
  cat("\n✗ No MLmethod succeeded.\n")
}
