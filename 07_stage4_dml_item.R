# ─────────────────────────────────────────────────────────────────────────────
# 07_stage4_dml_item.R
# Stage 4 (Guy's 2026-07-17 narrative: "which items are affected by SCP" --
# single-outcome DiD AND DML DiD for each of the 10 MDCH items).
#
# Uses the SAME wide, curated covariate set as 06b_stage3_dml_wide.R
# (not the lean 7-variable set) -- Guy's explicit decision: "best to just use
# the wide covariate set, as this is the main benefit of using ML/
# regularisation." See 06b's header for full curation rationale (which raw
# KEEP_VARS were dropped and why) and the missingness fixes (WFTCBU/EMPSTATI/
# ERENTBU/BENBU_UC excluded -- see that script's comments).
#
# MLmethod defaults to lasso ONLY. Wide-set lasso took 1377s (~23min) for one
# outcome in 06b -- looping that over 10 items is ~230min (~4hrs), long but
# tractable overnight/unattended. Wide-set randomforest's runtime is untested
# but likely much longer (lean-set RF was already 602s vs lasso's 89s, a 6.8x
# multiple; a similar multiple on wide-lasso's 1377s would put wide-RF per
# item well past an hour, i.e. 10+ hours for the full item loop). NOT viable
# to loop blindly -- add "randomforest" to ML_METHODS only for specific items
# you want to follow up on individually once lasso results are in.
#
# Baseline (non-DML) item-level DiD already exists separately in
# 04_stage2_item_did.R (OLS, joint household-clustered model) -- that
# satisfies the "single outcome DID" half of Stage 4's brief. This script is
# the "Double ML DiD for each outcome" half.
#
# BH-FDR correction applied within each MLmethod across the 10 items, same
# convention as 04_dml_did.R Stage 3 and 05_parallel_trends.R.
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
DATA_PATH   <- "/Users/guypigott/python-venv-demo/Dissertation/data/hbai_clean.csv"
TABLES_DIR  <- "/Users/guypigott/Claude/Projects/MSc Dissertation/tables"
FIGURES_DIR <- "/Users/guypigott/Claude/Projects/MSc Dissertation/figures"
dir.create(TABLES_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

ALPHA      <- 0.05
CLUSTERVAR <- "SERNUM"
K_FOLDS    <- 3
TRIM       <- 0.05
ML_METHODS <- c("lasso")   # see header -- add "randomforest" selectively, not by default

MDCH_ITEMS <- c("MDCH_BED", "MDCH_CEL", "MDCH_COAT", "MDCH_EQP", "MDCH_HOL",
                "MDCH_PLAY", "MDCH_PLY", "MDCH_TEA", "MDCH_TRP", "MDCH_VEG")

MDCH_LABELS <- c(
  MDCH_BED  = "Bed / bedroom",
  MDCH_CEL  = "Celebrations",
  MDCH_COAT = "Warm coat",
  MDCH_EQP  = "School equipment",
  MDCH_HOL  = "Holiday away",
  MDCH_PLAY = "Indoor play / games",
  MDCH_PLY  = "Outdoor play area",
  MDCH_TEA  = "Fresh fruit / veg",
  MDCH_TRP  = "Trips / outings",
  MDCH_VEG  = "Vegetables"
)

cat("Loading data...\n")
df <- fread(DATA_PATH)
df[, YEAR := as.integer(YEAR)]
df[, .rowid := .I]   # stable row index -- used to realign item columns after na.omit() below

if (!"post" %in% names(df)) {
  stop("`post` column not found in hbai_clean.csv -- rerun 01_hbai_prep.R first.")
}
df[, treated := as.numeric(scotland)]
df[, post    := as.numeric(post)]

for (v in MDCH_ITEMS) {
  df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
  df[[v]] <- ifelse(df[[v]] < 0, NA_real_, df[[v]])
}

# ─────────────────────────────────────────────────────────────────────────────
# WIDE COVARIATE SET -- identical curation to 06b_stage3_dml_wide.R
# ─────────────────────────────────────────────────────────────────────────────
CONTINUOUS_VARS <- c(
  "AGE", "AGEHD",                          # AGESP excluded -- structurally NA for non-couples
  "S_OE_BHC", "S_OE_AHC", "S_OE_GRO",
  "S_OE_GRO_PROP_EARN", "S_OE_GRO_PROP_BEN", "S_OE_GRO_PROP_INV",
  "EHCOST", "ES_HCOST",                    # ERENTBU excluded -- structurally NA for owners
  "CHBENBU", "ESBENIBU", "INCHILBU", "EGRINCBU", "WINPAYBU"  # WFTCBU excluded -- 100% NA, obsolete
)
COUNT_VARS <- c("NUMBKIDS", "ADULTH")
CATEGORICAL_VARS <- c(
  "DIS", "DIS_TYPE", "DSCORFAM", "BENBU_DISBEN",
  "TENHBAI", "ETHGRPHHPUB",                # EMPSTATI excluded -- 100% NA in this extract
  "NEWFAMBU_KID", "MARITAL_KID",
  "BENBU_FSM", "BENBU_IS", "BENBU_JSA", "BENBU_ESA",  # BENBU_UC excluded -- 30.7% NA
  "BENBU_HB", "BENBU_CTC", "BENBU_WTC", "BENBU_PC",
  "SEX", "SEXHD"
)

CONTINUOUS_VARS  <- CONTINUOUS_VARS[CONTINUOUS_VARS %in% names(df)]
COUNT_VARS       <- COUNT_VARS[COUNT_VARS %in% names(df)]
CATEGORICAL_VARS <- CATEGORICAL_VARS[CATEGORICAL_VARS %in% names(df)]
cov_cols <- c(CONTINUOUS_VARS, COUNT_VARS, CATEGORICAL_VARS)
cat(sprintf("  Continuous (n=%d) + count (n=%d) + categorical (n=%d) = %d raw covariates\n",
            length(CONTINUOUS_VARS), length(COUNT_VARS), length(CATEGORICAL_VARS), length(cov_cols)))

for (v in cov_cols) {
  df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
  df[[v]] <- ifelse(df[[v]] < 0, NA_real_, df[[v]])
}

# ── Complete-case on covariates alone (independent of which item we predict) ──
cc_covs <- na.omit(df[, .SD, .SDcols = c(".rowid", "treated", "post", CLUSTERVAR, cov_cols)])
cat(sprintf("  Complete-case sample on covariates alone: %s rows\n",
            format(nrow(cc_covs), big.mark = ",")))
if (nrow(cc_covs) == 0) stop("Covariate complete-case sample is 0 rows -- check missingness before proceeding.")

# ── Build design matrix once: z-score continuous/count, one-hot categoricals ──
for (v in c(CONTINUOUS_VARS, COUNT_VARS)) {
  cc_covs[[paste0(v, "_z")]] <- as.numeric(scale(cc_covs[[v]]))
}
scaled_vars <- paste0(c(CONTINUOUS_VARS, COUNT_VARS), "_z")

binary_vars <- c(); multilevel_vars <- c()
for (v in CATEGORICAL_VARS) {
  nlvl <- length(unique(cc_covs[[v]]))
  if (nlvl <= 2) binary_vars <- c(binary_vars, v) else multilevel_vars <- c(multilevel_vars, v)
}
for (v in binary_vars) {
  lvls <- sort(unique(cc_covs[[v]]))
  cc_covs[[v]] <- as.numeric(cc_covs[[v]] == lvls[length(lvls)])
}
dummy_mat <- NULL
if (length(multilevel_vars) > 0) {
  fml <- as.formula(paste("~", paste(multilevel_vars, collapse = " + "), "- 1"))
  dummy_mat <- model.matrix(fml, data = as.data.frame(lapply(cc_covs[, ..multilevel_vars], factor)))
}
x_mat_full <- as.data.frame(cc_covs[, ..scaled_vars])
if (!is.null(dummy_mat)) x_mat_full <- cbind(x_mat_full, as.data.frame(dummy_mat))
if (length(binary_vars) > 0) x_mat_full <- cbind(x_mat_full, as.data.frame(cc_covs[, ..binary_vars]))
cat(sprintf("  Design matrix: %d columns, %s rows (before per-item outcome filtering)\n",
            ncol(x_mat_full), format(nrow(x_mat_full), big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# RUN didDML() FOR EACH ITEM x ML METHOD
# cc_covs$.rowid maps each row of x_mat_full back to the original df row, so
# each item's own missingness can be applied on top without rebuilding the
# whole covariate matrix per item.
# ─────────────────────────────────────────────────────────────────────────────
results <- list()
run_i <- 0
total_runs <- length(MDCH_ITEMS) * length(ML_METHODS)
est_min_per_lasso_run <- 1377 / 60   # from 06b's wide-lasso timing on this same covariate set
cat(sprintf("\nEstimated runtime if all lasso: ~%.0f min total (%d items x ~%.0f min each)\n",
            length(MDCH_ITEMS) * est_min_per_lasso_run, length(MDCH_ITEMS), est_min_per_lasso_run))

item_vals_by_rowid <- df[cc_covs$.rowid, ..MDCH_ITEMS]  # aligned to cc_covs row order

for (item in MDCH_ITEMS) {
  keep <- !is.na(item_vals_by_rowid[[item]])
  n_item_na <- sum(!keep)
  x_mat   <- x_mat_full[keep, , drop = FALSE]
  y_vec   <- item_vals_by_rowid[[item]][keep]
  d_vec   <- cc_covs$treated[keep]
  t_vec   <- cc_covs$post[keep]
  cl_vec  <- cc_covs[[CLUSTERVAR]][keep]

  for (ml in ML_METHODS) {
    run_i <- run_i + 1
    cat(sprintf("\n== [%d/%d] item=%s (%s)  MLmethod='%s'  (n=%s, %d dropped for item NA) ====\n",
                run_i, total_runs, item, MDCH_LABELS[item], ml,
                format(length(y_vec), big.mark = ","), n_item_na))
    t0 <- Sys.time()
    fit <- tryCatch(
      didDML(y = y_vec, d = d_vec, t = t_vec, x = x_mat,
             MLmethod = ml, est = "dr", trim = TRIM, cluster = cl_vec, k = K_FOLDS),
      error = function(e) { cat(sprintf("  ✗ failed: %s\n", e$message)); NULL }
    )
    elapsed <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
    if (is.null(fit)) next

    crit <- qnorm(1 - ALPHA / 2)
    results[[paste(item, ml)]] <- data.frame(
      item = item, label = unname(MDCH_LABELS[item]), MLmethod = ml, covset = "wide",
      ATET = fit$ATET, se = fit$se, pval = fit$pval,
      ci_lo = fit$ATET - crit * fit$se, ci_hi = fit$ATET + crit * fit$se,
      n_trimmed = fit$ntrimmed, n_used = length(y_vec) - fit$ntrimmed, runtime_s = elapsed
    )
    sig <- ifelse(fit$pval < .01, "***", ifelse(fit$pval < .05, "**", ifelse(fit$pval < .1, "*", "")))
    cat(sprintf("  ATET=%8.4f  SE=%8.4f  p=%6.3f  %s  (%.1fs)\n",
                fit$ATET, fit$se, fit$pval, sig, elapsed))
  }
}

# ─────────────────────────────────────────────────────────────────────────────
# BH-FDR CORRECTION (within each MLmethod, across the 10 items) + SAVE
# ─────────────────────────────────────────────────────────────────────────────
if (length(results) > 0) {
  results_df <- bind_rows(results) |>
    group_by(MLmethod) |>
    mutate(pval_bh = p.adjust(pval, method = "BH"), sig_bh = pval_bh < ALPHA) |>
    ungroup()

  write.csv(results_df, file.path(TABLES_DIR, "dml_did_item_level_wide.csv"), row.names = FALSE)
  cat("\n── Item-level DML DiD, wide covariates (BH-corrected within MLmethod) ──\n")
  print(results_df |> select(label, MLmethod, ATET, se, pval, pval_bh, sig_bh) |>
          mutate(across(where(is.numeric), \(x) round(x, 4))), n = Inf)
  cat(sprintf("\nwrote %s\n", file.path(TABLES_DIR, "dml_did_item_level_wide.csv")))

  plot_df <- results_df |> mutate(label = factor(label, levels = rev(unique(label))))
  p <- ggplot(plot_df, aes(x = ATET, y = label, xmin = ci_lo, xmax = ci_hi, colour = sig_bh)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_errorbarh(height = 0.3) +
    geom_point(size = 3) +
    facet_wrap(~MLmethod) +
    scale_colour_manual(values = c("TRUE" = "#d7191c", "FALSE" = "#2c7bb6"),
                         labels = c("TRUE" = "BH-significant", "FALSE" = "n.s."), name = NULL) +
    labs(title = "Item-level DML DiD (causalweight::didDML), wide covariate set",
         subtitle = "Doubly robust (est='dr') | 95% CI | BH-FDR corrected within method",
         x = "ATET", y = NULL) +
    theme_minimal(base_size = 11)
  ggsave(file.path(FIGURES_DIR, "dml_did_item_level_wide.png"), p, width = 9, height = 6, dpi = 150)
  cat(sprintf("wrote %s\n", file.path(FIGURES_DIR, "dml_did_item_level_wide.png")))

  cat("\nCompare against 04_stage2_item_did.R's OLS item-interacted table (baseline single-\n")
  cat("outcome DiD per item) -- do items that are null under OLS become significant under\n")
  cat("wide-covariate DML, or vice versa?\n")
} else {
  cat("\n✗ No item/method combination succeeded.\n")
}
