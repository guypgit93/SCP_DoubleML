# ─────────────────────────────────────────────────────────────────────────────
# 12_dml_stability_check.R
# Finite-sample stability of the headline doubly robust DML
# estimate.
# Its nuisance models are cross-fit on a modest post-treatment Scottish
# sample (~800 obs), so this refits the same 06b_stage3_dml_wide.R spec B
# times, varying only the random seed (fold split + random forest draws),
# to see how much the coefficient and significance move across refits.
#
# causalweight::didDML() hard-codes set.seed(1) on its first
# line, silently overriding any seed set by the caller, so a first run of
# this script came back 20/20 identical. `didDML_seeded()` below
# is a copy of didDML()'s source with that one line changed to take
# seed as a real parameter.
#
# Slow (~20-25 min/rep)
# ─────────────────────────────────────────────────────────────────────────────

library(causalweight)
library(sandwich)
library(data.table)
library(tidyverse)

# ─────────────────────────────────────────────────────────────────────────────
# didDML_seeded(): copy of causalweight::didDML() (fetched from the CRAN GitHub mirror), 
# with `set.seed(1)` replaced by set.seed(seed)` and `seed` exposed as a parameter. This is the only
# change. MLfunct() is an internal helper in the causalweight
# package, referenced here via `causalweight:::MLfunct` since it isn't
# available unqualified outside the package namespace.
# ─────────────────────────────────────────────────────────────────────────────
didDML_seeded <- function(y, d, t, x, MLmethod = "lasso", est = "dr",
                           trim = 0.05, cluster = NULL, k = 3, seed = 1) {
  ybin <- 1 * (length(unique(y)) == 2 & min(y) == 0 & max(y) == 1)   # is y binary 0/1?
  controls <- data.frame(x)
  stepsize <- ceiling((1 / k) * length(d))   # rows per fold
  set.seed(seed); idx <- sample(length(d), replace = FALSE)   # <-- the fix: seed is a parameter, not hard-coded
  param <- c()   # accumulates each fold's held-out propensity/outcome predictions
  for (i in 1:k) {
    # Contiguous block of the shuffled idx is this fold's test set; the rest trains.
    tesample <- idx[((i - 1) * stepsize + 1):(min((i) * stepsize, length(d)))]
    trsample <- idx[!(idx %in% tesample)]
    ytr <- y[trsample]; dtr <- d[trsample]; ttr <- t[trsample]
    controlstr <- data.frame(1, controls)[trsample, ]   # prepend intercept column for MLfunct

    # Outcome regressions: one per (d,t) cell (excluding d1t1, the ATET's own
    # cell), trained on this fold's training rows, predicted onto the test rows.
    mud0t1 <- causalweight:::MLfunct(y = ytr, x = controlstr, d1 = 1 * (dtr == 0 & ttr == 1), MLmethod = MLmethod, ybin = ybin)
    mud1t0 <- causalweight:::MLfunct(y = ytr, x = controlstr, d1 = 1 * (dtr == 1 & ttr == 0), MLmethod = MLmethod, ybin = ybin)
    mud0t0 <- causalweight:::MLfunct(y = ytr, x = controlstr, d1 = 1 * (dtr == 0 & ttr == 0), MLmethod = MLmethod, ybin = ybin)
    controlstest <- data.frame(1, controls)[tesample, ]
    mud0t1 <- predict(mud0t1, controlstest, onlySL = TRUE)$pred
    mud1t0 <- predict(mud1t0, controlstest, onlySL = TRUE)$pred
    mud0t0 <- predict(mud0t0, controlstest, onlySL = TRUE)$pred

    # Propensity models: P(cell membership = 1 | x), one per (d,t) cell,
    # again trained on the training rows and predicted onto the test rows.
    rhod1t1 <- causalweight:::MLfunct(y = 1 * (dtr == 1 & ttr == 1), x = controlstr, MLmethod = MLmethod, ybin = 1)
    rhod1t1 <- predict(rhod1t1, controlstest, onlySL = TRUE)$pred
    rhod1t0 <- causalweight:::MLfunct(y = 1 * (dtr == 1 & ttr == 0), x = controlstr, MLmethod = MLmethod, ybin = 1)
    rhod1t0 <- predict(rhod1t0, controlstest, onlySL = TRUE)$pred
    rhod0t1 <- causalweight:::MLfunct(y = 1 * (dtr == 0 & ttr == 1), x = controlstr, MLmethod = MLmethod, ybin = 1)
    rhod0t1 <- predict(rhod0t1, controlstest, onlySL = TRUE)$pred
    rhod0t0 <- causalweight:::MLfunct(y = 1 * (dtr == 0 & ttr == 0), x = controlstr, MLmethod = MLmethod, ybin = 1)
    rhod0t0 <- predict(rhod0t0, controlstest, onlySL = TRUE)$pred
    param <- rbind(param, cbind(rhod1t1, rhod1t0, rhod0t1, rhod0t0, mud1t0, mud0t1, mud0t0))   # stack this fold's test-row predictions
  }
  param <- param[order(idx), ]   # restore original row order across all folds
  # Bundle d, 1-d, t, y alongside the fold predictions; column order matters
  # for the index arithmetic below (col 1=d, 2=1-d, 3=t, 4=y, 5-8=rho's, 9-11=mu's).
  param <- cbind(d, 1 - d, t, y, param)
  # Flag rows for trimming: d1t1 propensity too close to 1, or one of the
  # other three cell propensities too close to 0 (scaled by the row's own d/t).
  trimmed <- 1 * ((param[, 5] > (1 - trim)) | (param[, 6] < trim * d * (1 - t)) |
                   (param[, 7] < trim * (1 - d) * t) | (param[, 8] < trim * (1 - d) * (1 - t)))
  param <- param[trimmed == 0, ]   # drop trimmed rows
  # Guard against exact-zero propensities among surviving rows, since columns
  # 6-8 are denominators in the reweighting terms below.
  param[, 6] <- ifelse(param[, 6] == 0, .Machine$double.eps, param[, 6])
  param[, 7] <- ifelse(param[, 7] == 0, .Machine$double.eps, param[, 7])
  param[, 8] <- ifelse(param[, 8] == 0, .Machine$double.eps, param[, 8])
  # Four IPW reweighting terms, one per (d,t) cell, each normalised to sum to 1.
  resd1t1 <- (param[, 1] * param[, 3]) / sum(param[, 1] * param[, 3])   # d1t1 needs no reweighting: it's the ATET's own cell
  resd1t0 <- (param[, 1] * (1 - param[, 3]) * param[, 5] / param[, 6]) / sum(param[, 1] * (1 - param[, 3]) * param[, 5] / param[, 6])
  resd0t1 <- (param[, 2] * param[, 3] * param[, 5] / param[, 7]) / sum(param[, 2] * param[, 3] * param[, 5] / param[, 7])
  resd0t0 <- (param[, 2] * (1 - param[, 3]) * param[, 5] / param[, 8]) / sum(param[, 2] * (1 - param[, 3]) * param[, 5] / param[, 8])
  reg <- resd1t1 * (param[, 4] - param[, 9] - param[, 10] + param[, 11])   # outcome-regression (doubly robust) correction term
  if (est == "dr") {
    # Centre each IPW term on its own cell's outcome-regression prediction:
    # the AIPW residual-correction structure.
    resd1t0 <- resd1t0 * (param[, 4] - param[, 9])
    resd0t1 <- resd0t1 * (param[, 4] - param[, 10])
    resd0t0 <- resd0t0 * (param[, 4] - param[, 11])
    # DiD contrast of the four terms, rescaled by the untrimmed sample size
    # so mean(score) below is the ATET, not a trimmed-sample-only average.
    score <- sum(1 - trimmed) * (reg - resd1t0 - resd0t1 + resd0t0)
  }
  if (est == "reg") score <- sum(1 - trimmed) * (reg)   # outcome-regression-only estimator; unused here (this script always calls with est="dr")
  if (est == "ipw") score <- sum(1 - trimmed) * (resd1t1 * param[, 4] - resd1t0 * param[, 4] - resd0t1 * param[, 4] + resd0t0 * param[, 4])   # pure IPW estimator; likewise unused here
  ATET <- mean(score)
  if (is.null(cluster)) {
    se <- summary(lm(score ~ 1))$coefficients[, "Std. Error"]   # unclustered SE; unused here, cluster is always supplied below
  } else {
    se <- sqrt(vcovCL(lm(score ~ 1), cluster = cluster[trimmed == 0]))[1, 1]   # cluster-robust SE via a one-column OLS trick
  }
  pval <- 2 * pnorm((-1) * abs(ATET / se))
  list(ATET = ATET, se = se, pval = pval, ntrimmed = sum(trimmed),
       pscores = param[, 5:8], outcomepred = param[, 9:11], treat = param[, 1], time = param[, 3])
}

DATA_PATH  <- "/Users/guypigott/python-venv-demo/Dissertation/data/hbai_clean.csv"
TABLES_DIR <- "/Users/guypigott/Claude/Projects/MSc Dissertation/tables"
dir.create(TABLES_DIR, showWarnings = FALSE, recursive = TRUE)

OUT_PATH    <- file.path(TABLES_DIR, "dml_stability_wide_rf_v2.csv")   # new file: v1 was invalid
MASTER_SEED <- 20260818
B           <- 20
CLUSTERVAR  <- "SERNUM"
K_FOLDS     <- 3
TRIM        <- 0.05

# ─────────────────────────────────────────────────────────────────────────────
# DATA PREP: from 06b_stage3_dml_wide.R
# ─────────────────────────────────────────────────────────────────────────────
cat("Loading data...\n")
df <- fread(DATA_PATH)
df[, YEAR := as.integer(YEAR)]

if (!"post" %in% names(df)) {
  stop("`post` column not found in hbai_clean.csv -- rerun 01_hbai_prep.R first.")
}
df[, treated := as.numeric(scotland)]
df[, post    := as.numeric(post)]

df$MDCH <- suppressWarnings(as.numeric(df$MDCH))
df$MDCH <- ifelse(df$MDCH < 0, NA_real_, df$MDCH)
df_mdch <- df[mdch_observed == 1]

CONTINUOUS_VARS <- c(
  "AGE", "AGEHD",
  "S_OE_BHC", "S_OE_AHC", "S_OE_GRO",
  "S_OE_GRO_PROP_EARN", "S_OE_GRO_PROP_BEN", "S_OE_GRO_PROP_INV",
  "EHCOST", "ES_HCOST",
  "CHBENBU", "ESBENIBU", "INCHILBU", "EGRINCBU", "WINPAYBU"
)
COUNT_VARS <- c("NUMBKIDS", "ADULTH")
CATEGORICAL_VARS <- c(
  "DIS", "DIS_TYPE", "DSCORFAM", "BENBU_DISBEN",
  "TENHBAI", "ETHGRPHHPUB",
  "NEWFAMBU_KID", "MARITAL_KID",
  "BENBU_FSM", "BENBU_IS", "BENBU_JSA", "BENBU_ESA",
  "BENBU_HB", "BENBU_CTC", "BENBU_WTC", "BENBU_PC",
  "SEX", "SEXHD"
)

CONTINUOUS_VARS  <- CONTINUOUS_VARS[CONTINUOUS_VARS %in% names(df_mdch)]
COUNT_VARS       <- COUNT_VARS[COUNT_VARS %in% names(df_mdch)]
CATEGORICAL_VARS <- CATEGORICAL_VARS[CATEGORICAL_VARS %in% names(df_mdch)]

cols_need <- unique(c("MDCH", "treated", "post", CLUSTERVAR,
                       CONTINUOUS_VARS, COUNT_VARS, CATEGORICAL_VARS))
raw_sub <- df_mdch[, ..cols_need]
for (v in c(CONTINUOUS_VARS, COUNT_VARS, CATEGORICAL_VARS)) {
  raw_sub[[v]] <- suppressWarnings(as.numeric(raw_sub[[v]]))
  raw_sub[[v]] <- ifelse(raw_sub[[v]] < 0, NA_real_, raw_sub[[v]])
}

sub <- na.omit(raw_sub)
cat(sprintf("  Complete-case sample: %s rows\n", format(nrow(sub), big.mark = ",")))
if (nrow(sub) == 0) stop("Complete-case sample is 0 rows -- data prep drifted from 06b, check it.")

for (v in c(CONTINUOUS_VARS, COUNT_VARS)) {
  sub[[paste0(v, "_z")]] <- as.numeric(scale(sub[[v]]))
}
scaled_vars <- paste0(c(CONTINUOUS_VARS, COUNT_VARS), "_z")

binary_vars <- c(); multilevel_vars <- c()
for (v in CATEGORICAL_VARS) {
  nlvl <- length(unique(sub[[v]]))
  if (nlvl <= 2) binary_vars <- c(binary_vars, v) else multilevel_vars <- c(multilevel_vars, v)
}
for (v in binary_vars) {
  lvls <- sort(unique(sub[[v]]))
  sub[[v]] <- as.numeric(sub[[v]] == lvls[length(lvls)])
}

dummy_mat <- NULL
if (length(multilevel_vars) > 0) {
  fml <- as.formula(paste("~", paste(multilevel_vars, collapse = " + "), "- 1"))
  dummy_mat <- model.matrix(fml, data = as.data.frame(lapply(sub[, ..multilevel_vars], factor)))
}

x_mat <- as.data.frame(sub[, ..scaled_vars])
if (!is.null(dummy_mat)) x_mat <- cbind(x_mat, as.data.frame(dummy_mat))
if (length(binary_vars) > 0) x_mat <- cbind(x_mat, as.data.frame(sub[, ..binary_vars]))
cat(sprintf("  Design matrix: %d columns, %s rows (should match 06b's original run)\n",
            ncol(x_mat), format(nrow(x_mat), big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# STABILITY LOOP: varies the seed via didDML_seeded()
# ─────────────────────────────────────────────────────────────────────────────
# Appends one row immediately (rather than batching to write at the end), so a
# killed process or a sleeping laptop only loses the in-progress rep, not the run.
append_row <- function(row_df) {
  write.table(row_df, OUT_PATH, sep = ",", row.names = FALSE,
              col.names = !file.exists(OUT_PATH), append = file.exists(OUT_PATH))
}

seeds <- MASTER_SEED + seq_len(B)   # one distinct seed per target replication
completed_seeds <- integer(0)
if (file.exists(OUT_PATH)) {
  # Resume support: if this script was already run (fully or partially),
  # don't refit seeds whose result is already on disk.
  prev <- tryCatch(read.csv(OUT_PATH), error = function(e) NULL)
  if (!is.null(prev) && "seed" %in% names(prev)) completed_seeds <- prev$seed
}
seeds_to_run <- setdiff(seeds, completed_seeds)
cat(sprintf("\n%d / %d reps already completed in a previous run; running remaining %d.\n",
            length(completed_seeds), B, length(seeds_to_run)))

for (s in seeds_to_run) {
  cat(sprintf("\n== seed=%d (%d of %d target reps) ==================================\n",
              s, which(seeds == s), B))
  t0 <- Sys.time()
  fit <- tryCatch(
    didDML_seeded(
      y = sub$MDCH, d = sub$treated, t = sub$post, x = x_mat,
      MLmethod = "randomforest", est = "dr", trim = TRIM,
      cluster = sub[[CLUSTERVAR]], k = K_FOLDS, seed = s
    ),
    error = function(e) { cat(sprintf("  ✗ seed=%d failed: %s\n", s, e$message)); NULL }
  )
  elapsed <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)

  if (is.null(fit)) {
    append_row(data.frame(seed = s, ATET = NA, se = NA, pval = NA,
                           n_trimmed = NA, runtime_s = elapsed, status = "failed"))
    next
  }

  row_df <- data.frame(
    seed = s, ATET = fit$ATET, se = fit$se, pval = fit$pval,
    n_trimmed = fit$ntrimmed, runtime_s = elapsed, status = "ok"
  )
  append_row(row_df)

  sig <- ifelse(fit$pval < .01, "***", ifelse(fit$pval < .05, "**", ifelse(fit$pval < .1, "*", "")))
  cat(sprintf("  ATET=%8.4f  SE=%8.4f  p=%6.3f  %s  (%.1fs)\n",
              fit$ATET, fit$se, fit$pval, sig, elapsed))
}

final <- read.csv(OUT_PATH)
final_ok <- final[final$status == "ok", ]

cat("\n\n══ STABILITY SUMMARY ═══════════════════════════════════════════════════\n")
cat(sprintf("Completed reps: %d / %d target (%d failed)\n",
            nrow(final_ok), B, sum(final$status == "failed")))
cat(sprintf("Original single-run estimate (06b, seed=1 -- now known to be the package's hard-coded default): -0.0451\n"))
cat(sprintf("Across %d reruns -- ATET: mean=%.4f  sd=%.4f  min=%.4f  max=%.4f  range=%.4f\n",
            nrow(final_ok), mean(final_ok$ATET), sd(final_ok$ATET),
            min(final_ok$ATET), max(final_ok$ATET), max(final_ok$ATET) - min(final_ok$ATET)))
cat(sprintf("Significant at 5%%: %d / %d reps (%.0f%%)\n",
            sum(final_ok$pval < 0.05), nrow(final_ok),
            100 * mean(final_ok$pval < 0.05)))
cat(sprintf("Significant at 10%%: %d / %d reps (%.0f%%)\n",
            sum(final_ok$pval < 0.10), nrow(final_ok),
            100 * mean(final_ok$pval < 0.10)))
cat(sprintf("\n✓ Full per-rep results in %s\n", OUT_PATH))
cat("\nPaste this summary block (and the CSV if easy) back to Claude to write up.\n")
