# ─────────────────────────────────────────────────────────────────────────────
# 08_stage5_stacked_ml_did.R
# Stage 5: stacked ML DiD using the vector of item outcomes when predicting.
# This script hand-codes a doubly robust DiD estimator,
# reusing causalweight's own machinery where possible rather than
# reinventing it wholesale.
#
# Stage 4 called causalweight::didDML() once per item,
# ten fully independent fits, each with its own propensity models and its
# own outcome-regression models. This script instead:
#   (a) fits the propensity models once per cross-fitting fold and reuses
#       them across all items, which is valid because propensity (P(d,t|x))
#       does not depend on which item's y I'm predicting, and because
#       this script uses one shared sample across items,
#       so d/t/x really are identical across items here.
#   (b) fits one joint multivariate outcome-regression model per (d,t) cell
#       per fold, predicting the full item-outcome vector at once via
#       glmnet(family="mgaussian"), instead of 8 independent univariate
#       models. This is the actual "stacking": correlated items share
#       information that helps the outcome model, which a per-item
#       independent fit can't exploit.
#
# A joint multivariate model needs one consistent row set
# across every item, since it can't have a different missingness pattern
# per output column. Bed/bedroom (MDCH_BED, ~6,874 non-missing of ~50k) and
# Playgroup attendance (MDCH_PLAY, ~14,889) are much thinner than the other
# 8 items, so including them would shrink the joint sample to whatever the
# thinnest item allows. Dropped both. Stage 5 runs on the remaining 8, so
# it isn't directly comparable to Stage 4 for those two items. Flag this
# in the write-up rather than silently comparing all 10.
#
# didDML()'s own source calls an internal helper `MLfunct(y, x, d1=NULL,
# MLmethod, ybin)` that wraps SuperLearner::SuperLearner() (confirmed by
# didDML() itself calling predict(fit, newdata, onlySL=TRUE)$pred on its
# output). MLfunct's own source couldn't be located to verify its call
# signature independently; it's inferred entirely from how didDML()
# invokes it. `:::` calls an unexported function, not part of the package's
# public API, and could break silently across causalweight versions.
#
# Since this script re-implements didDML()'s own scoring formula by hand, 
# it first reconstructs a single-outcome fit for Celebrations using this 
# script's own code and checks it reproduces Stage 4's confirmed number 
# If it doesn't match closely, the script stops rather than silently proceeding
# on an unverified reimplementation.
# ─────────────────────────────────────────────────────────────────────────────

library(causalweight)
library(glmnet)
library(data.table)
library(tidyverse)
library(ggplot2)
library(sandwich)

# ─────────────────────────────────────────────────────────────────────────────
# CONFIG
# ─────────────────────────────────────────────────────────────────────────────
library(here)
source(here("Scripts", "00_config.R"))
DATA_PATH <- file.path(DATA_DIR, "hbai_clean.csv")

ALPHA        <- 0.05
CLUSTERVAR   <- "SERNUM"
K_FOLDS      <- 3
TRIM         <- 0.05
ML_METHOD    <- "lasso"   # glmnet(family="mgaussian") for the outcome step, MLfunct's own
                          # lasso dispatch for propensity. randomforest/randomForestSRC not
                          # implemented in this version.
SEED         <- 1         # matches didDML()'s own hardcoded set.seed(1), needed for the
                          # validation step to reproduce Stage 4's exact fold assignment

STAGE4_CSV <- file.path(TABLES_DIR, "dml_did_item_level_wide.csv")
OUT_CSV    <- file.path(TABLES_DIR, "stage5_stacked_item_did.csv")

ALL_MDCH_ITEMS <- c("MDCH_BED", "MDCH_CEL", "MDCH_COAT", "MDCH_EQP", "MDCH_HOL",
                     "MDCH_PLAY", "MDCH_PLY", "MDCH_TEA", "MDCH_TRP", "MDCH_VEG")
DROPPED_ITEMS  <- c("MDCH_BED", "MDCH_PLAY")   # too thin for a joint complete-case sample
ITEMS          <- setdiff(ALL_MDCH_ITEMS, DROPPED_ITEMS)

MDCH_LABELS <- c(
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

cat(sprintf("Stage 5 item subset: %s (dropped: %s)\n",
            paste(ITEMS, collapse = ", "), paste(DROPPED_ITEMS, collapse = ", ")))

# ─────────────────────────────────────────────────────────────────────────────
# LOAD DATA + WIDE COVARIATE SET. Identical curation to 06b/07, for direct
# comparability; do not change this independently of those scripts.
# ─────────────────────────────────────────────────────────────────────────────
cat("Loading data...\n")
df <- fread(DATA_PATH)
df[, YEAR := as.integer(YEAR)]
df[, .rowid := .I]   # stable row index, used later to realign rows after filtering
if (!"post" %in% names(df)) stop("`post` column not found -- rerun 01_hbai_prep.R first.")
df[, treated := as.numeric(scotland)]   # 1 = Scotland, 0 = England
df[, post    := as.numeric(post)]       # 1 = post-SCP, 0 = pre-SCP

for (v in ALL_MDCH_ITEMS) {
  df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
  df[[v]] <- ifelse(df[[v]] < 0, NA_real_, df[[v]])   # negative codes are DWP "missing" sentinels
}

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
CONTINUOUS_VARS  <- CONTINUOUS_VARS[CONTINUOUS_VARS %in% names(df)]
COUNT_VARS       <- COUNT_VARS[COUNT_VARS %in% names(df)]
CATEGORICAL_VARS <- CATEGORICAL_VARS[CATEGORICAL_VARS %in% names(df)]
cov_cols <- c(CONTINUOUS_VARS, COUNT_VARS, CATEGORICAL_VARS)

for (v in cov_cols) {
  df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
  df[[v]] <- ifelse(df[[v]] < 0, NA_real_, df[[v]])
}

cc_covs <- na.omit(df[, .SD, .SDcols = c(".rowid", "treated", "post", CLUSTERVAR, cov_cols)])
cat(sprintf("  Complete-case on covariates alone: %s rows\n", format(nrow(cc_covs), big.mark = ",")))

for (v in c(CONTINUOUS_VARS, COUNT_VARS)) cc_covs[[paste0(v, "_z")]] <- as.numeric(scale(cc_covs[[v]]))
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
cat(sprintf("  Design matrix: %d columns, %s rows (before item-level joint filtering)\n",
            ncol(x_mat_full), format(nrow(x_mat_full), big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# JOINT COMPLETE-CASE SAMPLE ACROSS THE 8 RETAINED ITEMS
# ─────────────────────────────────────────────────────────────────────────────
item_vals_by_rowid <- df[cc_covs$.rowid, ..ITEMS]
joint_keep <- Reduce(`&`, lapply(ITEMS, function(it) !is.na(item_vals_by_rowid[[it]])))
cat(sprintf("  Joint complete-case sample across %d items: %s rows (of %s covariate-complete)\n",
            length(ITEMS), format(sum(joint_keep), big.mark = ","), format(nrow(cc_covs), big.mark = ",")))
if (sum(joint_keep) < 1000) stop("Joint complete-case sample looks implausibly small -- check item missingness before proceeding.")

x_mat_joint <- x_mat_full[joint_keep, , drop = FALSE]
Y_mat_joint <- as.matrix(item_vals_by_rowid[joint_keep, ..ITEMS])
d_joint     <- cc_covs$treated[joint_keep]
t_joint     <- cc_covs$post[joint_keep]
cl_joint    <- cc_covs[[CLUSTERVAR]][joint_keep]
n_joint     <- nrow(x_mat_joint)

# ─────────────────────────────────────────────────────────────────────────────
# HELPERS. Faithful transcription of didDML()'s own scoring formula (source
# read from
# https://raw.githubusercontent.com/cran/causalweight/master/R/didDML.R),
# ─────────────────────────────────────────────────────────────────────────────

# One propensity model (fit once per fold, reused across all items)
fit_propensity <- function(y01, x_with_intercept) {
  causalweight:::MLfunct(y = y01, x = x_with_intercept, MLmethod = ML_METHOD, ybin = 1)
}

# One item's outcome-regression model within a (d,t) cell
fit_outcome_univariate <- function(y, x_with_intercept, ybin) {
  causalweight:::MLfunct(y = y, x = x_with_intercept, MLmethod = ML_METHOD, ybin = ybin)
}

predict_nuisance <- function(fit, newx_with_intercept) {
  # as.vector() is load-bearing here: SuperLearner's predict(...)$pred returns a
  # one-column matrix, not a plain vector. cbind() only auto-names bare vector
  # arguments from their deparsed variable name; for a matrix argument it keeps
  # the matrix's own (empty) colname instead, so every cbind(idx=..., rhod1t1, ...)
  # call below would silently produce unnamed columns without this coercion
  as.vector(predict(fit, newx_with_intercept, onlySL = TRUE)$pred)
}

# Joint multivariate outcome-regression model across all retained items,
# fit on one (d,t) cell's training subset. Returns predictions on newx as an
# n_new x length(ITEMS) matrix. glmnet(family="mgaussian") only; no
# randomforest/randomForestSRC path in this version.
fit_multivariate_outcome <- function(Y_cell, x_cell_notintercept, newx_notintercept) {
  if (ML_METHOD != "lasso") stop("fit_multivariate_outcome(): only ML_METHOD='lasso' (glmnet mgaussian) is implemented.")
  cvfit <- cv.glmnet(x = as.matrix(x_cell_notintercept), y = Y_cell, family = "mgaussian", alpha = 1)
  pred  <- predict(cvfit, newx = as.matrix(newx_notintercept), s = "lambda.min")
  # predict.cv.glmnet with family="mgaussian" returns an n x nvars x 1 array; drop to a plain matrix
  matrix(pred[, , 1], nrow = nrow(newx_notintercept), dimnames = list(NULL, colnames(Y_cell)))
}

# Faithful reimplementation of didDML()'s trimming + AIPW score construction
# (est="dr" branch only). `param` columns, in original row order, exactly
# mirroring didDML.R: d, 1-d, t, y, rhod1t1, rhod1t0, rhod0t1, rhod0t0,
# mud1t0, mud0t1, mud0t0.
compute_dr_score <- function(d, t, y, rhod1t1, rhod1t0, rhod0t1, rhod0t0,
                              mud1t0, mud0t1, mud0t0, cluster, trim = TRIM, alpha = ALPHA) {
  # Bundle everything into one matrix, columns in didDML.R's own order, so the
  # column-index arithmetic below lines up with the original source exactly.
  param <- cbind(d, 1 - d, t, y, rhod1t1, rhod1t0, rhod0t1, rhod0t0, mud1t0, mud0t1, mud0t0)

  # Flag rows for trimming: either the d1t1 propensity is too close to 1, or
  # one of the other three cell propensities is too close to 0 (scaled by
  # the row's own d/t so only the relevant cell's propensity is checked).
  trimmed <- 1 * ((param[, 5] > (1 - trim)) |
                   (param[, 6] < trim * d * (1 - t)) |
                   (param[, 7] < trim * (1 - d) * t) |
                   (param[, 8] < trim * (1 - d) * (1 - t)))
  param   <- param[trimmed == 0, ]   # drop trimmed rows from here on
  cluster <- cluster[trimmed == 0]   # keep cluster IDs aligned with param's rows
  # Guard against exact-zero propensities among the surviving rows,
  # since columns 6-8 appear as denominators in the reweighting terms below.
  param[, 6] <- ifelse(param[, 6] == 0, .Machine$double.eps, param[, 6])
  param[, 7] <- ifelse(param[, 7] == 0, .Machine$double.eps, param[, 7])
  param[, 8] <- ifelse(param[, 8] == 0, .Machine$double.eps, param[, 8])

  # Four IPW reweighting terms, one per (d,t) cell, each normalised to sum to
  # 1 over the sample so they act as sampling weights within their cell.
  # d1t1 (the treated post-period cell) needs no propensity reweighting since
  # it's the cell the ATET is defined on.
  resd1t1 <- (param[, 1] * param[, 3]) / sum(param[, 1] * param[, 3])
  # d1t0 (treated pre-period): reweighted by rhod1t1/rhod1t0, the odds of
  # being in the treated-post cell rather than the treated-pre cell.
  resd1t0 <- (param[, 1] * (1 - param[, 3]) * param[, 5] / param[, 6]) /
              sum(param[, 1] * (1 - param[, 3]) * param[, 5] / param[, 6])
  # d0t1 (untreated post-period): reweighted by rhod1t1/rhod0t1.
  resd0t1 <- (param[, 2] * param[, 3] * param[, 5] / param[, 7]) /
              sum(param[, 2] * param[, 3] * param[, 5] / param[, 7])
  # d0t0 (untreated pre-period): reweighted by rhod1t1/rhod0t0.
  resd0t0 <- (param[, 2] * (1 - param[, 3]) * param[, 5] / param[, 8]) /
              sum(param[, 2] * (1 - param[, 3]) * param[, 5] / param[, 8])

  # Outcome-regression (doubly robust) correction term, the plug-in DiD of
  # the four cell-mean predictions, weighted by the d1t1 cell's own weights.
  reg     <- resd1t1 * (param[, 4] - param[, 9] - param[, 10] + param[, 11])
  # Each IPW term is then centred on its own cell's outcome-regression
  # prediction (param columns 9-11), giving the AIPW residual-correction
  # structure IPW term plus an outcome-model correction, per cell.
  resd1t0 <- resd1t0 * (param[, 4] - param[, 9])
  resd0t1 <- resd0t1 * (param[, 4] - param[, 10])
  resd0t0 <- resd0t0 * (param[, 4] - param[, 11])
  # Final per-row score: reg term combines with the three IPW-corrected terms
  # in a DiD contrast (treated-post minus treated-pre minus untreated-post
  # plus untreated-pre); rescaled by the untrimmed sample size so the mean
  # of `score` below is the ATET, not a trimmed-sample-only average.
  score   <- sum(1 - trimmed) * (reg - resd1t0 - resd0t1 + resd0t0)

  ATET <- mean(score)
  # Cluster-robust variance of the score's mean, via a one-column OLS trick
  # (regressing score on an intercept) so vcovCL() can be reused as-is.
  se   <- sqrt(vcovCL(lm(score ~ 1), cluster = cluster))[1, 1]
  pval <- 2 * pnorm(-abs(ATET / se))
  list(ATET = ATET, se = se, pval = pval, n_trimmed = sum(trimmed), n_used = sum(1 - trimmed))
}

# ─────────────────────────────────────────────────────────────────────────────
# VALIDATION STEP. Reproduces Stage 4's Celebrations lasso number using
# this script's own custom code (MLfunct calls + compute_dr_score()), on
# Celebrations' own complete-case sample (not the joint 8-item one), with
# the same seed/fold logic didDML() itself uses.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n== VALIDATION: reproducing Stage 4's Celebrations (lasso) result with this script's own code ====\n")

val_item <- "MDCH_CEL"
val_keep <- !is.na(item_vals_by_rowid[[val_item]])
x_val <- x_mat_full[val_keep, , drop = FALSE]
y_val <- item_vals_by_rowid[[val_item]][val_keep]
d_val <- cc_covs$treated[val_keep]
t_val <- cc_covs$post[val_keep]
cl_val <- cc_covs[[CLUSTERVAR]][val_keep]
n_val <- length(y_val)
ybin_val <- 1 * (length(unique(y_val)) == 2 && min(y_val) == 0 && max(y_val) == 1)

set.seed(SEED)   # must match didDML()'s own set.seed(1) for the fold split to line up
idx <- sample(n_val, replace = FALSE)   # random row order, split into K_FOLDS contiguous blocks below
stepsize <- ceiling((1 / K_FOLDS) * n_val)   # rows per fold
val_param <- c()   # accumulates each fold's held-out predictions across the loop
for (i in 1:K_FOLDS) {
  # This fold's test block is one contiguous chunk of the shuffled idx;
  # everything else in idx is this fold's training set.
  tesample <- idx[((i - 1) * stepsize + 1):(min(i * stepsize, n_val))]
  trsample <- idx[!(idx %in% tesample)]
  # data.frame(1, x_val) prepends an intercept column, since MLfunct expects
  # a design matrix that already includes one.
  xtr <- data.frame(1, x_val)[trsample, ]; xte <- data.frame(1, x_val)[tesample, ]
  ytr <- y_val[trsample]; dtr <- d_val[trsample]; ttr <- t_val[trsample]

  # Outcome regressions: one univariate model per (d,t) cell, trained on this
  # fold's training rows within that cell, predicted onto the held-out test rows.
  # d1t1 (treated, post) needs no outcome model: it's the ATET's own cell.
  mud0t1 <- predict_nuisance(fit_outcome_univariate(ytr[dtr == 0 & ttr == 1], xtr[dtr == 0 & ttr == 1, ], ybin_val), xte)
  mud1t0 <- predict_nuisance(fit_outcome_univariate(ytr[dtr == 1 & ttr == 0], xtr[dtr == 1 & ttr == 0, ], ybin_val), xte)
  mud0t0 <- predict_nuisance(fit_outcome_univariate(ytr[dtr == 0 & ttr == 0], xtr[dtr == 0 & ttr == 0, ], ybin_val), xte)
  # Propensity models: P(cell membership = 1 | x), one per (d,t) cell,
  # again trained on this fold's training rows and predicted onto the test rows.
  rhod1t1 <- predict_nuisance(fit_propensity(1 * (dtr == 1 & ttr == 1), xtr), xte)
  rhod1t0 <- predict_nuisance(fit_propensity(1 * (dtr == 1 & ttr == 0), xtr), xte)
  rhod0t1 <- predict_nuisance(fit_propensity(1 * (dtr == 0 & ttr == 1), xtr), xte)
  rhod0t0 <- predict_nuisance(fit_propensity(1 * (dtr == 0 & ttr == 0), xtr), xte)

  # Stack this fold's test-row predictions, tagged with their original row
  # index (idx column) so the folds can be reassembled in original row order.
  val_param <- rbind(val_param, cbind(idx = tesample, rhod1t1, rhod1t0, rhod0t1, rhod0t0, mud1t0, mud0t1, mud0t0))
}
val_param <- val_param[order(val_param[, "idx"]), ]   # restore original row order across all folds

val_fit <- compute_dr_score(
  d = d_val, t = t_val, y = y_val,
  rhod1t1 = val_param[, "rhod1t1"], rhod1t0 = val_param[, "rhod1t0"],
  rhod0t1 = val_param[, "rhod0t1"], rhod0t0 = val_param[, "rhod0t0"],
  mud1t0 = val_param[, "mud1t0"], mud0t1 = val_param[, "mud0t1"], mud0t0 = val_param[, "mud0t0"],
  cluster = cl_val
)

cat(sprintf("  This script's custom reimplementation:  ATET=%.4f  SE=%.4f  p=%.3f\n",
            val_fit$ATET, val_fit$se, val_fit$pval))

if (file.exists(STAGE4_CSV)) {
  stage4 <- read.csv(STAGE4_CSV, stringsAsFactors = FALSE)
  stage4_cel <- stage4[stage4$item == val_item & stage4$MLmethod == "lasso", ]
  if (nrow(stage4_cel) == 1) {
    cat(sprintf("  Stage 4's confirmed result (07_stage4_dml_item.R):  ATET=%.4f  SE=%.4f  p=%.3f\n",
                stage4_cel$ATET, stage4_cel$se, stage4_cel$pval))
    atet_diff <- abs(val_fit$ATET - stage4_cel$ATET)
    if (atet_diff > 0.01) {
      stop(sprintf(
        "VALIDATION FAILED: custom reimplementation's ATET differs from Stage 4's confirmed value by %.4f (>0.01). ",
        atet_diff), "Do NOT trust the joint multivariate results below until this discrepancy is understood -- ",
        "check the MLfunct call signature and the compute_dr_score() transcription against didDML.R before proceeding.")
    } else {
      cat(sprintf("  VALIDATION PASSED (difference %.4f, within tolerance). Proceeding to the joint model.\n", atet_diff))
    }
  } else {
    cat("  ! Could not find a unique Stage 4 lasso row for MDCH_CEL to validate against -- proceeding without this check. Interpret results with extra caution.\n")
  }
} else {
  cat(sprintf("  ! %s not found -- cannot validate against Stage 4. Proceeding without this check. Interpret results with extra caution.\n", STAGE4_CSV))
}

# ─────────────────────────────────────────────────────────────────────────────
# MAIN RUN. Joint 8-item stacked model. Propensity fit once per fold and
# reused across items (valid here: d/t/x are shared across items by
# construction, unlike Stage 4). Outcome-regression fit once per (d,t) cell
# per fold as a joint multivariate model over the 8-item matrix.
# ─────────────────────────────────────────────────────────────────────────────
cat(sprintf("\n== STAGE 5: joint stacked DML DiD, %d items, n=%s ====\n",
            length(ITEMS), format(n_joint, big.mark = ",")))

set.seed(SEED)   # same seed as the validation step and didDML() itself
idx <- sample(n_joint, replace = FALSE)   # random row order for this joint sample
stepsize <- ceiling((1 / K_FOLDS) * n_joint)
joint_param_rho <- c()   # accumulates propensity predictions across folds, one row per obs
joint_param_mu  <- vector("list", K_FOLDS)   # each element: n_test x length(ITEMS) x 3 (mud1t0/mud0t1/mud0t0)

for (i in 1:K_FOLDS) {
  cat(sprintf("  fold %d/%d...\n", i, K_FOLDS))
  # Same contiguous-block train/test split logic as the validation loop above.
  tesample <- idx[((i - 1) * stepsize + 1):(min(i * stepsize, n_joint))]
  trsample <- idx[!(idx %in% tesample)]

  # Keep both the intercept-free (glmnet, which fits its own intercept) and
  # intercept-prepended (MLfunct, which expects one already in x) versions,
  # since the propensity and outcome-regression steps need different inputs.
  xtr_noint <- x_mat_joint[trsample, , drop = FALSE]; xte_noint <- x_mat_joint[tesample, , drop = FALSE]
  xtr <- data.frame(1, xtr_noint); xte <- data.frame(1, xte_noint)
  Ytr <- Y_mat_joint[trsample, , drop = FALSE]   # training rows, all 8 item columns at once
  dtr <- d_joint[trsample]; ttr <- t_joint[trsample]

  # Propensity: once per fold, shared across all 8 items
  rhod1t1 <- predict_nuisance(fit_propensity(1 * (dtr == 1 & ttr == 1), xtr), xte)
  rhod1t0 <- predict_nuisance(fit_propensity(1 * (dtr == 1 & ttr == 0), xtr), xte)
  rhod0t1 <- predict_nuisance(fit_propensity(1 * (dtr == 0 & ttr == 1), xtr), xte)
  rhod0t0 <- predict_nuisance(fit_propensity(1 * (dtr == 0 & ttr == 0), xtr), xte)
  # Tag with this fold's test-row indices so folds can be reassembled below.
  joint_param_rho <- rbind(joint_param_rho, cbind(idx = tesample, rhod1t1, rhod1t0, rhod0t1, rhod0t0))

  # Outcome regression: joint multivariate model per (d,t) cell. d1t1 (treated,
  # post) is skipped, same as the validation loop, since it's the ATET's own cell.
  cell_d0t1 <- dtr == 0 & ttr == 1
  cell_d1t0 <- dtr == 1 & ttr == 0
  cell_d0t0 <- dtr == 0 & ttr == 0
  # Each call fits one glmnet(family="mgaussian") model on this cell's training
  # rows across all 8 item outcomes jointly, then predicts onto the full test set.
  mud0t1 <- fit_multivariate_outcome(Ytr[cell_d0t1, , drop = FALSE], xtr_noint[cell_d0t1, , drop = FALSE], xte_noint)
  mud1t0 <- fit_multivariate_outcome(Ytr[cell_d1t0, , drop = FALSE], xtr_noint[cell_d1t0, , drop = FALSE], xte_noint)
  mud0t0 <- fit_multivariate_outcome(Ytr[cell_d0t0, , drop = FALSE], xtr_noint[cell_d0t0, , drop = FALSE], xte_noint)
  # Store as a list keyed by fold index; each element carries its own test-row
  # indices since mu predictions (n_test x 8 items) can't be rbind-stacked with
  # a single idx column the way the propensity vectors above can.
  joint_param_mu[[i]] <- list(idx = tesample, mud1t0 = mud1t0, mud0t1 = mud0t1, mud0t0 = mud0t0)
}

joint_param_rho <- joint_param_rho[order(joint_param_rho[, "idx"]), ]   # restore original row order

# Reassemble each item's mu matrices into original row order. Needed because
# joint_param_mu is stored fold-by-fold above rather than already stacked.
assemble_mu <- function(which_mu) {
  out <- matrix(NA_real_, nrow = n_joint, ncol = length(ITEMS), dimnames = list(NULL, ITEMS))
  for (i in 1:K_FOLDS) {
    fold <- joint_param_mu[[i]]
    out[fold$idx, ] <- fold[[which_mu]]   # place this fold's rows back at their original positions
  }
  out
}
mu_mud1t0 <- assemble_mu("mud1t0")
mu_mud0t1 <- assemble_mu("mud0t1")
mu_mud0t0 <- assemble_mu("mud0t0")

# ─────────────────────────────────────────────────────────────────────────────
# PER-ITEM SCORES (shared rho's, item-specific mu's) + BH-FDR + SAVE
# ─────────────────────────────────────────────────────────────────────────────
results <- list()
for (item in ITEMS) {
  # Same rho's (propensity) for every item, since they were fit on shared
  # d/t/x; only the y and mu (outcome-regression) inputs vary by item.
  fit <- compute_dr_score(
    d = d_joint, t = t_joint, y = Y_mat_joint[, item],
    rhod1t1 = joint_param_rho[, "rhod1t1"], rhod1t0 = joint_param_rho[, "rhod1t0"],
    rhod0t1 = joint_param_rho[, "rhod0t1"], rhod0t0 = joint_param_rho[, "rhod0t0"],
    mud1t0 = mu_mud1t0[, item], mud0t1 = mu_mud0t1[, item], mud0t0 = mu_mud0t0[, item],
    cluster = cl_joint
  )
  sig <- ifelse(fit$pval < .01, "***", ifelse(fit$pval < .05, "**", ifelse(fit$pval < .1, "*", "")))
  cat(sprintf("  %-20s ATET=%8.4f  SE=%8.4f  p=%6.3f  %s\n",
              MDCH_LABELS[item], fit$ATET, fit$se, fit$pval, sig))
  results[[item]] <- data.frame(
    item = item, label = unname(MDCH_LABELS[item]), stage = "stage5_stacked",
    ATET = fit$ATET, se = fit$se, pval = fit$pval,
    n_trimmed = fit$n_trimmed, n_used = fit$n_used
  )
}

results_df <- bind_rows(results) |>
  mutate(pval_bh = p.adjust(pval, method = "BH"), sig_bh = pval_bh < ALPHA)

write.csv(results_df, OUT_CSV, row.names = FALSE)
cat(sprintf("\nwrote %s\n", OUT_CSV))

# ── Compare against Stage 4 (lasso, same items) ──
if (file.exists(STAGE4_CSV)) {
  stage4 <- read.csv(STAGE4_CSV, stringsAsFactors = FALSE) |>
    filter(MLmethod == "lasso", item %in% ITEMS) |>
    select(item, stage4_ATET = ATET, stage4_se = se, stage4_pval_bh = pval_bh)
  comparison <- results_df |> select(item, label, ATET, se, pval_bh, sig_bh) |>
    left_join(stage4, by = "item")

  # Save to CSV before printing: print() has previously crashed here on
  # tibble's na.print handling, and the save must not depend on that succeeding.
  write.csv(comparison, file.path(TABLES_DIR, "stage5_vs_stage4_comparison.csv"), row.names = FALSE)
  cat(sprintf("wrote %s\n", file.path(TABLES_DIR, "stage5_vs_stage4_comparison.csv")))

  # plain data.frame print (not the tibble/pillar path) to avoid the na.print crash
  comparison_rounded <- as.data.frame(comparison)
  for (col in names(comparison_rounded)) {
    if (is.numeric(comparison_rounded[[col]])) comparison_rounded[[col]] <- round(comparison_rounded[[col]], 4)
  }
  print(comparison_rounded, row.names = FALSE)
}

# ── Figure ──
plot_df <- results_df |> mutate(label = factor(label, levels = rev(unique(label))),
                                  ci_lo = ATET - qnorm(0.975) * se, ci_hi = ATET + qnorm(0.975) * se)
p <- ggplot(plot_df, aes(x = ATET, y = label, xmin = ci_lo, xmax = ci_hi, colour = sig_bh)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(height = 0.3) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("TRUE" = "#d7191c", "FALSE" = "#2c7bb6"),
                       labels = c("TRUE" = "BH-significant", "FALSE" = "n.s."), name = NULL) +
  labs(title = "Stage 5: stacked joint-item DML DiD (8 items, shared propensity + multivariate outcome model)",
       subtitle = "Doubly robust | 95% CI | BH-FDR corrected across 8 items | lasso/mgaussian only",
       x = "ATET", y = NULL) +
  theme_minimal(base_size = 11)
ggsave(file.path(FIGURES_DIR, "stage5_stacked_item_did.png"), p, width = 9, height = 5.5, dpi = 150)
cat(sprintf("wrote %s\n", file.path(FIGURES_DIR, "stage5_stacked_item_did.png")))

cat("\nDone. Compare stage5_vs_stage4_comparison.csv: does joint/stacked outcome modelling\n")
cat("recover significance that Stage 4's independent per-item models missed, or not?\n")
