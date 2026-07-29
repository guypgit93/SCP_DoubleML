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
# MLmethod: lasso-only run completed 2026-07-21 (results already in
# dml_did_item_level_wide.csv -- see below, NOT rerun by this version).
# ZERO of 10 items were BH-significant under lasso (vs. 2 under 03's OLS
# item-level regressions) -- see results_narrative.docx Section 8.
#
# 2026-07-21: added "randomforest" as a second method, per Guy's request, to
# see (1) actual wide-set RF-per-item runtime (the "10+ hours" estimate below
# was written before 06b's composite-outcome finding that wide-RF (2657s) was
# actually FASTER than wide-lasso (4111s) on the full sample -- so that
# estimate may be too pessimistic) and (2) whether RF changes any
# significance conclusions. Also worth testing as a side effect: two items
# (Warm coat, Outdoor play area) took ~3hrs each under lasso despite similar
# sample sizes to other items that took 22-35min -- a plausible cause is
# quasi-separation in one of glmnet's propensity sub-models, which has no
# real analogue for randomForest's greedy tree-splitting (no MLE/convergence
# step to fail). If RF does NOT reproduce that slowdown on the same two
# items, that's supporting evidence for the quasi-separation theory; if it
# DOES, that points to something harder in those items' data specifically.
#
# This run-mode change (ML_METHODS below) only runs the methods NOT already
# present in the saved CSV for a given item, and merges with what's already
# there -- lasso's confirmed numbers are never re-fit or overwritten.
#
# SAFETY NETS added 2026-07-21 (the lasso run had no checkpointing -- a hang
# on a later item would have lost every earlier-completed item's result):
#   - TIMEOUT_SECS: each item x method fit is capped via setTimeLimit(); a
#     fit that exceeds this is treated as a failure (skipped, not blocking).
#   - After every item completes, the full accumulated results-so-far are
#     rewritten to a checkpoint CSV, so an interruption only costs whatever
#     is currently in flight, never anything already finished.
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
ML_METHODS <- c("lasso", "randomforest")   # lasso already confirmed 2026-07-21; RF added per Guy's request
TIMEOUT_SECS <- 9000   # 2.5hr cap per item x method fit (generous above composite-level wide-RF's 2657s);
                        # a fit exceeding this is treated as failed/skipped, not left to block indefinitely.
                        # CAVEAT: setTimeLimit() can only interrupt at points R checks for it -- a single
                        # long-running call into compiled C/Fortran code (e.g. deep inside glmnet or
                        # randomForest's internals) may not yield control back until it finishes regardless
                        # of this limit. It should catch most pathological cases (like Warm coat/Outdoor
                        # play's ~3hr lasso runs) but is not a guaranteed hard kill -- if a fit still runs
                        # past TIMEOUT_SECS with no sign of stopping, you may still need to kill the R
                        # process manually, same as before.

MAIN_OUT_CSV       <- file.path(TABLES_DIR, "dml_did_item_level_wide.csv")
CHECKPOINT_OUT_CSV <- file.path(TABLES_DIR, "dml_did_item_level_wide_CHECKPOINT.csv")

MDCH_ITEMS <- c("MDCH_BED", "MDCH_CEL", "MDCH_COAT", "MDCH_EQP", "MDCH_HOL",
                "MDCH_PLAY", "MDCH_PLY", "MDCH_TEA", "MDCH_TRP", "MDCH_VEG")

MDCH_LABELS <- c(
  # Labels corrected 2026-07-28 against the official HBAI item wording (see
  # 03_stage1_baseline_did.R's header for the full rationale) -- MDCH_TEA,
  # MDCH_EQP and MDCH_PLAY were factually wrong, not just imprecise.
  MDCH_BED  = "Bed / bedroom",
  MDCH_CEL  = "Celebrations",
  MDCH_COAT = "Warm coat",
  MDCH_EQP  = "Leisure/sports equipment",
  MDCH_HOL  = "Holiday away",
  MDCH_PLAY = "Playgroup attendance",
  MDCH_PLY  = "Outdoor play area",
  MDCH_TEA  = "Friends round for tea/snack",
  MDCH_TRP  = "School trip",
  MDCH_VEG  = "Fresh fruit/veg daily"
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
#
# Resume/skip logic: if MAIN_OUT_CSV already has a row for a given
# item x MLmethod combination (e.g. lasso, confirmed 2026-07-21), that
# combination is NOT refit -- its saved row is carried forward as-is. This
# means adding "randomforest" to ML_METHODS above only runs the new cells.
# ─────────────────────────────────────────────────────────────────────────────
results <- list()
if (file.exists(MAIN_OUT_CSV)) {
  prior <- read.csv(MAIN_OUT_CSV, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(prior))) {
    key <- paste(prior$item[i], prior$MLmethod[i])
    results[[key]] <- prior[i, setdiff(names(prior), c("pval_bh", "sig_bh")), drop = FALSE]
  }
  cat(sprintf("Loaded %d already-confirmed item x method result(s) from %s -- these will NOT be refit.\n",
              length(results), MAIN_OUT_CSV))
}

run_i <- 0
already_done <- length(results)
to_run <- sum(!sapply(MDCH_ITEMS, function(it) sapply(ML_METHODS, function(ml) paste(it, ml) %in% names(results))))
cat(sprintf("Cells to run this session: %d (already have %d)\n", to_run, already_done))

save_checkpoint <- function() {
  if (length(results) == 0) return(invisible())
  ck <- bind_rows(results) |>
    group_by(MLmethod) |>
    mutate(pval_bh = p.adjust(pval, method = "BH"), sig_bh = pval_bh < ALPHA) |>
    ungroup()
  write.csv(ck, CHECKPOINT_OUT_CSV, row.names = FALSE)
}

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
    key <- paste(item, ml)
    if (key %in% names(results)) {
      cat(sprintf("\n== [%d] item=%s  MLmethod='%s'  -- already confirmed, skipping ====\n",
                  run_i, item, ml))
      next
    }
    cat(sprintf("\n== [%d] item=%s (%s)  MLmethod='%s'  (n=%s, %d dropped for item NA, timeout=%ds) ====\n",
                run_i, item, MDCH_LABELS[item], ml,
                format(length(y_vec), big.mark = ","), n_item_na, TIMEOUT_SECS))
    t0 <- Sys.time()
    fit <- tryCatch({
      setTimeLimit(elapsed = TIMEOUT_SECS, transient = TRUE)
      res <- didDML(y = y_vec, d = d_vec, t = t_vec, x = x_mat,
                     MLmethod = ml, est = "dr", trim = TRIM, cluster = cl_vec, k = K_FOLDS)
      setTimeLimit(elapsed = Inf, transient = TRUE)
      res
    }, error = function(e) {
      setTimeLimit(elapsed = Inf, transient = TRUE)
      cat(sprintf("  ✗ failed or timed out after %ds: %s\n", TIMEOUT_SECS, e$message))
      NULL
    })
    elapsed <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
    if (is.null(fit)) next

    crit <- qnorm(1 - ALPHA / 2)
    results[[key]] <- data.frame(
      item = item, label = unname(MDCH_LABELS[item]), MLmethod = ml, covset = "wide",
      ATET = fit$ATET, se = fit$se, pval = fit$pval,
      ci_lo = fit$ATET - crit * fit$se, ci_hi = fit$ATET + crit * fit$se,
      n_trimmed = fit$ntrimmed, n_used = length(y_vec) - fit$ntrimmed, runtime_s = elapsed
    )
    sig <- ifelse(fit$pval < .01, "***", ifelse(fit$pval < .05, "**", ifelse(fit$pval < .1, "*", "")))
    cat(sprintf("  ATET=%8.4f  SE=%8.4f  p=%6.3f  %s  (%.1fs)\n",
                fit$ATET, fit$se, fit$pval, sig, elapsed))

    save_checkpoint()   # rewrite full accumulated results-so-far after EVERY completed fit
    cat(sprintf("  checkpoint written (%s)\n", CHECKPOINT_OUT_CSV))
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

  write.csv(results_df, MAIN_OUT_CSV, row.names = FALSE)
  cat("\n── Item-level DML DiD, wide covariates (BH-corrected within MLmethod) ──\n")
  print(results_df |> select(label, MLmethod, ATET, se, pval, pval_bh, sig_bh, runtime_s) |>
          mutate(across(where(is.numeric), \(x) round(x, 4))), n = Inf)
  cat(sprintf("\nwrote %s (final, includes both lasso and randomforest)\n", MAIN_OUT_CSV))
  cat(sprintf("Compare randomforest rows against lasso rows above to answer Guy's two questions:\n"))
  cat(sprintf("  1) runtime -- see runtime_s column per method\n"))
  cat(sprintf("  2) significance -- compare pval_bh and sig_bh columns across MLmethod\n"))

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
