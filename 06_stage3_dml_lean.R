# ─────────────────────────────────────────────────────────────────────────────
# 06_dml_did_causalweight.R
# Stage 3 (Guy's 2026-07-17 narrative): Double/debiased ML DiD for repeated
# cross-sections, single outcome. PRIMARY OUTCOME = official DWP MDCH flag
# ("MDCH"), matching Stage 2's baseline (replicates the CASE/Stewart et al.
# paper using the official flag) so Stage 3 is testing sensitivity to
# covariate adjustment/functional form on the SAME outcome, not a different
# one. mdch_any (the earlier target) stays available as a secondary/
# robustness outcome -- results already obtained under mdch_any (lean lasso
# -0.0648*, lean RF -0.0824**, lean ensemble -0.0891**, wide lasso -0.0955***,
# wide RF -0.0626 n.s.) remain valid, just no longer primary.
#
# Caveat carried over from 04_dml_did.R: the official MDCH flag's methodology
# changed at FYE2024 (old 21-item vs new 11+11-item design). This dataset's
# `post` period spans YEAR 2023 (pre-transition) and YEAR 2024 (the mixed/
# transition year, ~76% new-methodology / ~24% old-methodology households).
# So the post period pools two different "deprived" definitions for THIS
# outcome specifically -- worth a sentence in the methodology/limitations
# section regardless of estimation method used.
#
# Uses causalweight::didDML() (CRAN) -- a REAL package for exactly this
# setting, found after the meeting. Built on Zimmert (2020)'s efficient-score
# DiD estimator, NOT Chang (2020) -- i.e. a different construction from the
# one that broke in 04b_chang_dmldid_trial.R (-0.35 vs raw DiD -0.02,
# propensity collapse). Structurally doubly robust (est="dr", the default):
# for each of the 4 treatment x time cells it fits BOTH an outcome-regression
# model and a propensity/cell-membership model via cross-fitted ML, and
# combines them into an augmented ("efficient") score -- consistent if either
# nuisance model is right, not both. See chat discussion for the full
# derivation; source read directly from
# https://raw.githubusercontent.com/cran/causalweight/master/R/didDML.R
#
# IMPORTANT: MLmethod="lasso" is the package default but is a LINEAR method --
# it will NOT pick up the nonlinearities/interactions among covariates Guy's
# supervisor specifically asked for (and Guy proposed in his presentation).
# This script therefore compares lasso (linear benchmark) against
# randomforest and ensemble (which includes tree-based learners via
# SuperLearner) -- if RF/ensemble differ meaningfully from lasso, that
# difference IS the evidence that covariate nonlinearity/interaction matters
# here, which is a presentable finding in itself.
#
# Requires: install.packages(c("causalweight", "SuperLearner", "randomForest",
#                               "xgboost", "glmnet", "e1071", "sandwich"))
#
# Uses the existing `post` column from hbai_lca.csv as-is (do NOT recompute
# from YEAR>=2023 -- see 01_hbai_prep.R's exact-interview-date refinement for
# FY2022/23 and the 2026-07-17 fix propagating it through 03/03b/04/04b/04d).
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
K_FOLDS    <- 3      # package default; raise to 5 later as a robustness check
TRIM       <- 0.05   # package default propensity trim

# Run lasso FIRST when testing -- it's fast and validates the pipeline works
# end-to-end before committing to the much slower tree/ensemble methods.
ML_METHODS <- c("lasso", "randomforest", "ensemble")

cat("Loading data...\n")
df <- fread(DATA_PATH)
df[, YEAR := as.integer(YEAR)]

if (!"post" %in% names(df)) {
  stop("`post` column not found in hbai_lca.csv -- rerun 01_hbai_prep.R first ",
       "(it builds `post` with the exact-interview-date refinement for FY2022/23).")
}
df[, treated := as.numeric(scotland)]
df[, post    := as.numeric(post)]   # use as-is from the CSV, do not recompute

df$MDCH <- suppressWarnings(as.numeric(df$MDCH))
df$MDCH <- ifelse(df$MDCH < 0, NA_real_, df$MDCH)
df_mdch <- df[mdch_observed == 1]

cat(sprintf("  %s rows after mdch_observed filter | years: %s\n",
            format(nrow(df_mdch), big.mark = ","),
            paste(sort(unique(df_mdch$YEAR)), collapse = ", ")))
cat(sprintf("  treated: Scotland=%s  England=%s | post: pre=%s  post=%s\n",
            sum(df_mdch$treated == 1), sum(df_mdch$treated == 0),
            sum(df_mdch$post == 0), sum(df_mdch$post == 1)))

# ─────────────────────────────────────────────────────────────────────────────
# COVARIATES -- same lean set as 04_dml_did.R's "Adjusted" spec, for direct
# comparability against the DR-DiD headline. (DML's cross-fitting is designed
# to tolerate a richer X than att_gt() could handle -- see 04_dml_did.R's
# comment on singular per-(g,t)-cell models -- so a richer-X robustness pass
# is a natural extension once this baseline comparison is validated.)
# ─────────────────────────────────────────────────────────────────────────────
LEAN_COVS_RAW <- c("AGE", "NUMBKIDS", "ADULTH", "S_OE_BHC", "S_OE_AHC", "EHCOST")
LEAN_COVS_RAW <- LEAN_COVS_RAW[LEAN_COVS_RAW %in% names(df_mdch)]
for (v in LEAN_COVS_RAW) {
  df_mdch[[paste0(v, "_z")]] <- as.numeric(scale(df_mdch[[v]]))
}
LEAN_COVS_Z <- paste0(LEAN_COVS_RAW, "_z")
if ("DIS" %in% names(df_mdch)) LEAN_COVS_Z <- c(LEAN_COVS_Z, "DIS")
cat(sprintf("  Covariates: %s\n", paste(LEAN_COVS_Z, collapse = ", ")))

cols_need <- unique(c("MDCH", "treated", "post", CLUSTERVAR, LEAN_COVS_Z))
sub <- na.omit(df_mdch[, ..cols_need])
cat(sprintf("  Complete-case sample: %s rows\n", format(nrow(sub), big.mark = ",")))

x_mat <- as.data.frame(sub[, ..LEAN_COVS_Z])

# ─────────────────────────────────────────────────────────────────────────────
# RUN didDML() FOR EACH ML METHOD
# ─────────────────────────────────────────────────────────────────────────────
results <- list()
for (ml in ML_METHODS) {
  cat(sprintf("\n== didDML: MLmethod='%s' (est='dr', k=%d folds) ==================\n",
              ml, K_FOLDS))
  t0 <- Sys.time()
  fit <- tryCatch(
    didDML(
      y       = sub$MDCH,
      d       = sub$treated,
      t       = sub$post,
      x       = x_mat,
      MLmethod = ml,
      est     = "dr",
      trim    = TRIM,
      cluster = sub[[CLUSTERVAR]],
      k       = K_FOLDS
    ),
    error = function(e) { cat(sprintf("  ✗ MLmethod='%s' failed: %s\n", ml, e$message)); NULL }
  )
  elapsed <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)

  if (is.null(fit)) next

  crit <- qnorm(1 - ALPHA / 2)
  results[[ml]] <- data.frame(
    MLmethod = ml,
    ATET     = fit$ATET,
    se       = fit$se,
    pval     = fit$pval,
    ci_lo    = fit$ATET - crit * fit$se,
    ci_hi    = fit$ATET + crit * fit$se,
    n_trimmed = fit$ntrimmed,
    n_used    = nrow(sub) - fit$ntrimmed,
    runtime_s = elapsed
  )
  sig <- ifelse(fit$pval < .01, "***", ifelse(fit$pval < .05, "**", ifelse(fit$pval < .1, "*", "")))
  cat(sprintf("  ATET=%8.4f  SE=%8.4f  p=%6.3f  %s  (trimmed %d/%d, %.1fs)\n",
              fit$ATET, fit$se, fit$pval, sig, fit$ntrimmed, nrow(sub), elapsed))

  # Propensity-score range per method -- directly checks whether RF/ensemble
  # actually found more discriminatory structure than the ~0.3 ceiling found
  # for the linear logit in 04d_propensity_overlap.R.
  cat(sprintf("  Propensity score ranges (4 cells): %s\n",
              paste(sprintf("[%.3f,%.3f]", apply(fit$pscores, 2, min), apply(fit$pscores, 2, max)),
                    collapse = "  ")))
}

# ─────────────────────────────────────────────────────────────────────────────
# SAVE COMPARISON TABLE
# ─────────────────────────────────────────────────────────────────────────────
if (length(results) > 0) {
  results_df <- bind_rows(results)
  # Outcome-specific filename -- the earlier mdch_any run's output
  # (dml_did_causalweight_comparison.csv: lasso -0.0648*, RF -0.0824**,
  # ensemble -0.0891**) is intentionally NOT overwritten; kept as the
  # secondary-outcome table.
  write.csv(results_df, file.path(TABLES_DIR, "dml_did_causalweight_comparison_MDCH.csv"), row.names = FALSE)
  cat("\n── MLmethod comparison (official MDCH flag) ────────────────────────────\n")
  print(results_df)
  cat(sprintf("\nwrote %s\n", file.path(TABLES_DIR, "dml_did_causalweight_comparison_MDCH.csv")))

  # ── Coefficient plot: lasso (linear benchmark) vs RF/ensemble (flexible) ──
  plot_df <- results_df |> mutate(MLmethod = factor(MLmethod, levels = rev(ML_METHODS)))
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
  cat("  - OLS baseline, official MDCH flag (03_did_replication.R) -- this is Stage 2's\n")
  cat("    replication of the CASE/Stewart et al. paper. Stage 3's job is testing whether\n")
  cat("    THAT number is sensitive to covariate adjustment/functional form, so compare\n")
  cat("    against it directly, not against 04_dml_did.R's att_gt()-based numbers (not\n")
  cat("    part of the write-up per Guy's 2026-07-17 decision).\n")
  cat("If randomforest/ensemble diverge meaningfully from lasso, that divergence is your\n")
  cat("evidence that covariate nonlinearity/interaction matters -- the thing your supervisor\n")
  cat("asked you to go find.\n")
} else {
  cat("\n✗ No MLmethod succeeded -- check errors above before proceeding to Stage 4 (item-level).\n")
}
