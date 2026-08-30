# ─────────────────────────────────────────────────────────────────────────────
# 06_stage3_dml_lean.R
# Stage 3, LEAN covariate arm: Double/debiased ML DiD for the official DWP
# MDCH flag, using the same six CASE covariates as the OLS baseline (03/04).
# Paired with 06b_stage3_dml_wide.R (identical didDML() setup, much
# richer covariate set) to test sensitivity to covariate breadth as well as
# functional form.
#
# Uses causalweight::didDML() (CRAN), Zimmert (2020)'s efficient-score DiD
# estimator. For each of the 4 treatment x time cells it fits both an
# outcome-regression model and a propensity/cell-membership model via
# cross-fitted ML, combined into a doubly robust score (est="dr", default)
# consistent if either nuisance model is right, not both.
# ─────────────────────────────────────────────────────────────────────────────

library(causalweight)
library(data.table)
library(tidyverse)
library(ggplot2)

# ─────────────────────────────────────────────────────────────────────────────
# CONFIG
# ─────────────────────────────────────────────────────────────────────────────
library(here)
source(here("Scripts", "00_config.R"))
DATA_PATH <- file.path(DATA_DIR, "hbai_clean.csv")

ALPHA      <- 0.05
CLUSTERVAR <- "SERNUM"
K_FOLDS    <- 3      # package default
TRIM       <- 0.05   # package default propensity trim

# lasso and randomforest only. "ensemble"
# (SuperLearner combination of lasso/randomforest/xgboost/svm, Zimmert 2020's
# own preferred nuisance-model choice) is too slow and was already run once on 
# the secondary mdch_any outcome (see dml_did_causalweight_comparison.csv); add 
# "ensemble" back to this vector when want to rerun that for MDCH too.
ML_METHODS <- c("lasso", "randomforest")

cat("Loading data...\n")
df <- fread(DATA_PATH)
df[, YEAR := as.integer(YEAR)]

if (!"post" %in% names(df)) {
  stop("`post` column not found in hbai_clean.csv -- rerun 01_hbai_prep.R first ",
       "(it builds `post` with the exact-interview-date refinement for FY2022/23).")
}
df[, treated := as.numeric(scotland)]
df[, post    := as.numeric(post)]   # use as-is from the CSV, do not recompute

df$MDCH <- suppressWarnings(as.numeric(df$MDCH))
df$MDCH <- ifelse(df$MDCH < 0, NA_real_, df$MDCH)   # negative codes are DWP "missing" sentinels
df_mdch <- df[mdch_observed == 1]

cat(sprintf("  %s rows after mdch_observed filter | years: %s\n",
            format(nrow(df_mdch), big.mark = ","),
            paste(sort(unique(df_mdch$YEAR)), collapse = ", ")))
cat(sprintf("  treated: Scotland=%s  England=%s | post: pre=%s  post=%s\n",
            sum(df_mdch$treated == 1), sum(df_mdch$treated == 0),
            sum(df_mdch$post == 0), sum(df_mdch$post == 1)))

# ─────────────────────────────────────────────────────────────────────────────
# COVARIATES -- CASE (Andersen et al. 2025) paper's six controls
# ─────────────────────────────────────────────────────────────────────────────
df_mdch[, young_head          := as.numeric(AGEHDBAND == 1)]
df_mdch[, female_head         := as.numeric(SEXHD == 2)]
df_mdch[, disabled_household  := as.numeric(DSCORFAM == 2)]
df_mdch[, lone_parent         := as.numeric(MARITAL_WITHKID == 1)]
df_mdch[, large_family        := as.numeric(NUMBKIDS == 3)]
df_mdch[, ETH_clean           := ifelse(ETH == 99, NA_real_, ETH)]   # 99 = "not declared" -> NA

CASE_BINARY_COVS <- c("young_head", "female_head", "disabled_household",
                       "lone_parent", "large_family")
CASE_BINARY_COVS <- CASE_BINARY_COVS[CASE_BINARY_COVS %in% names(df_mdch)]
cat(sprintf("  Covariates (CASE 2025 controls): %s + ETH (5-category, one-hot)\n",
            paste(CASE_BINARY_COVS, collapse = ", ")))

cols_need <- unique(c("MDCH", "treated", "post", CLUSTERVAR, CASE_BINARY_COVS, "ETH_clean"))
sub <- na.omit(df_mdch[, ..cols_need])
cat(sprintf("  Complete-case sample: %s rows\n", format(nrow(sub), big.mark = ",")))

# Ethnicity is the one multi-level control -- one-hot encode into 5 dummy
# columns
eth_dummies <- model.matrix(~ factor(ETH_clean) - 1, data = as.data.frame(sub))
colnames(eth_dummies) <- sub("factor\\(ETH_clean\\)", "ETH_", colnames(eth_dummies))

x_mat <- cbind(as.data.frame(sub[, ..CASE_BINARY_COVS]), as.data.frame(eth_dummies))

# ─────────────────────────────────────────────────────────────────────────────
# RUN didDML() FOR EACH ML METHOD
# ─────────────────────────────────────────────────────────────────────────────
results <- list()
for (ml in ML_METHODS) {   # one full didDML() fit per method in ML_METHODS (e.g. lasso, randomforest)
  cat(sprintf("\n== didDML: MLmethod='%s' (est='dr', k=%d folds) ==================\n",
              ml, K_FOLDS))
  t0 <- Sys.time()   # Timer
  fit <- tryCatch(
    didDML(
      y       = sub$MDCH,       # outcome: official MDCH flag (0/1)
      d       = sub$treated,    # treatment group: 1 = Scotland, 0 = England
      t       = sub$post,       # time period: 1 = post-SCP, 0 = pre-SCP
      x       = x_mat,          # covariates for the nuisance (outcome + propensity) models
      MLmethod = ml,            # which ML learner fits those nuisance models this iteration
      est     = "dr",           # doubly robust: consistent if EITHER nuisance model is correctly specified
      trim    = TRIM,           # drop observations with extreme propensity scores (overlap violations)
      cluster = sub[[CLUSTERVAR]],  # household-level clustering for standard errors
      k       = K_FOLDS         # number of cross-fitting folds
    ),
    error = function(e) { cat(sprintf("  ✗ MLmethod='%s' failed: %s\n", ml, e$message)); NULL }
  )
  elapsed <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)

  if (is.null(fit)) next   # this method failed -- skip to the next one rather than halting the whole loop

  crit <- qnorm(1 - ALPHA / 2)   # z-critical value for a 95% CI (ALPHA = 0.05)
  results[[ml]] <- data.frame(
    MLmethod = ml,
    ATET     = fit$ATET,                    # the DiD treatment effect estimate itself
    se       = fit$se,
    pval     = fit$pval,
    ci_lo    = fit$ATET - crit * fit$se,     # manual 95% CI, since didDML() doesn't return one directly
    ci_hi    = fit$ATET + crit * fit$se,
    n_trimmed = fit$ntrimmed,                # how many rows the `trim` step above dropped
    n_used    = nrow(sub) - fit$ntrimmed,
    runtime_s = elapsed
  )
  sig <- ifelse(fit$pval < .01, "***", ifelse(fit$pval < .05, "**", ifelse(fit$pval < .1, "*", "")))
  cat(sprintf("  ATET=%8.4f  SE=%8.4f  p=%6.3f  %s  (trimmed %d/%d, %.1fs)\n",
              fit$ATET, fit$se, fit$pval, sig, fit$ntrimmed, nrow(sub), elapsed))

  # Propensity-score range per method -- checks whether RF/ensemble found more
  # discriminatory structure than a linear logit would (that comparison lived
  # in archive/04d_propensity_overlap.R, ~0.3 ceiling for the linear case).
  # fit$pscores has one column per of the 4 treatment x time cells; min/max
  # close to 0 or 1 would signal a propensity-overlap problem.
  cat(sprintf("  Propensity score ranges (4 cells): %s\n",
              paste(sprintf("[%.3f,%.3f]", apply(fit$pscores, 2, min), apply(fit$pscores, 2, max)),
                    collapse = "  ")))
}

# ─────────────────────────────────────────────────────────────────────────────
# SAVE COMPARISON TABLE
# ─────────────────────────────────────────────────────────────────────────────
if (length(results) > 0) {
  results_df <- bind_rows(results)
  out_path <- file.path(TABLES_DIR, "dml_did_causalweight_comparison_MDCH.csv")

  # Merge with whatever's already saved rather than overwrite wholesale -- this
  # table gets built up across separate runs of ML_METHODS subsets (e.g. a
  # slow ensemble-only run, backfilled later by a fast lasso+RF run).
  # Overwriting wholesale would silently drop any method not in *this*
  # execution's `results`, even if it succeeded in an earlier run.
  if (file.exists(out_path)) {
    prev_df <- tryCatch(read.csv(out_path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(prev_df) && "MLmethod" %in% names(prev_df)) {
      prev_df <- prev_df[!(prev_df$MLmethod %in% results_df$MLmethod), ]  # this run's methods win
      results_df <- bind_rows(prev_df, results_df)
    }
  }
  results_df <- results_df[match(unique(results_df$MLmethod), results_df$MLmethod), ]  # de-dupe, keep order stable
  write.csv(results_df, out_path, row.names = FALSE)
  cat("\n── MLmethod comparison (official MDCH flag) -- merged with any prior save ──\n")
  print(results_df)
  cat(sprintf("\nwrote %s\n", out_path))

  # ── Coefficient plot: lasso (linear benchmark) vs RF/ensemble (flexible) ──
  # Levels drawn from results_df itself (not just this run's ML_METHODS) so a
  # method merged in from a previous run (e.g. ensemble) still appears.
  method_order <- c("lasso", "randomforest", "xgboost", "svm", "ensemble", "parametric")
  method_order <- c(intersect(method_order, results_df$MLmethod),
                     setdiff(results_df$MLmethod, method_order))
  plot_df <- results_df |> mutate(MLmethod = factor(MLmethod, levels = rev(method_order)))
  p <- ggplot(plot_df, aes(x = ATET, y = MLmethod, xmin = ci_lo, xmax = ci_hi)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_errorbarh(height = 0.2, colour = "#2c7bb6") +
    geom_point(size = 3, colour = "#2c7bb6") +
    labs(title = "DML DiD (causalweight::didDML) -- official MDCH flag, by ML method",
         subtitle = "Doubly robust (est='dr') | 95% CI | household-clustered SEs",
         x = "ATET", y = NULL,
         caption = "lasso = linear nuisance models (benchmark); randomforest/ensemble allow nonlinearities & interactions") +
    theme_minimal(base_size = 11)
  ggsave(file.path(FIGURES_DIR, "dml_did_causalweight_comparison_MDCH.png"), p, width = 9, height = 5, dpi = 150)
  cat(sprintf("wrote %s\n", file.path(FIGURES_DIR, "dml_did_causalweight_comparison_MDCH.png")))

  cat("\nCompare these ATET estimates against:\n")
  cat("  - OLS baseline, official MDCH flag (03_stage1_baseline_did.R) -- this is Stage 2's\n")
  cat("    replication of the CASE/Andersen et al. paper. Stage 3's job is testing whether\n")
  cat("    THAT number is sensitive to covariate adjustment/functional form.\n")
  cat("  - 06b_stage3_dml_wide.R's wide-covariate estimates -- same estimator, richer X,\n")
  cat("    testing sensitivity to covariate BREADTH as well as functional form.\n")
  cat("If randomforest/ensemble diverge meaningfully from lasso, that divergence is your\n")
  cat("evidence that covariate nonlinearity/interaction matters -- the thing your supervisor\n")
  cat("asked you to go find.\n")
} else {
  cat("\n✗ No MLmethod succeeded -- check errors above before proceeding to Stage 4 (item-level).\n")
}
