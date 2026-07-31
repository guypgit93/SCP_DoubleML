# ─────────────────────────────────────────────────────────────────────────────
# 03_stage1_baseline_did.R
# Stage 1: OLS DiD Replication of Stewart (2025) / CASE Paper
# Scottish Child Payment → Child Material Deprivation
# ─────────────────────────────────────────────────────────────────────────────
#
# Design
#   Treated  : Scotland
#   Control  : England
#   Pre      : FYE 2017–2022 (excl. FYE 2021 COVID)
#   Post     : FYE 2023–2024 (£25/wk SCP for all under-16s from Nov 2022)
#
#   FYE 2024 is NOT excluded: it's on the same MDCH definition as every
#   earlier year (the MDCH redesign only affects 2024/25, which isn't loaded)
#   and is valid post-treatment data.
#
# Outputs
#   tables/  — modelsummary regression tables (.tex and .docx)
#   figures/ — event study plots (.png)
#
# Run after 01_hbai_prep.R has produced hbai_clean.csv
# ─────────────────────────────────────────────────────────────────────────────

library(fixest)
library(tidyverse)
library(modelsummary)
library(data.table)
library(patchwork)

# etable() is built into fixest — no extra install needed.
# setFixest_dict() maps internal variable names to display labels.
setFixest_dict(c(
  tp      = "DiD (Scotland x Post)",
  treated = "Scotland",
  # Label describes intent, not a guarantee -- `post` is built in
  # 01_hbai_prep.R as YEAR>=2023 for every year EXCEPT FYE2023, which is
  # overwritten using real FRS household interview dates against the exact
  # 14-Nov-2022 SCP full-rollout cutoff (see that script's "FY2022/23 rows
  # reclassified" console line to confirm the merge succeeded when the CSV
  # was last built -- if it silently fell back to FY-only, this label would
  # be wrong and `post` would just mean YEAR>=2023 with no sub-year precision).
  post    = "Post (14-Nov-2022 cutoff)"
))

# ─────────────────────────────────────────────────────────────────────────────
# CONFIG
# ─────────────────────────────────────────────────────────────────────────────
DATA_PATH   <- "/Users/guypigott/python-venv-demo/Dissertation/data/hbai_clean.csv"
FIGURES_DIR <- "/Users/guypigott/Claude/Projects/MSc Dissertation/figures"
TABLES_DIR  <- "/Users/guypigott/Claude/Projects/MSc Dissertation/tables"

dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TABLES_DIR,  showWarnings = FALSE, recursive = TRUE)

REF_YEAR   <- 2022   # last pre-treatment year; omitted in event study
ALPHA      <- 0.05

MDCH_ITEMS <- c("MDCH_BED", "MDCH_CEL", "MDCH_COAT", "MDCH_EQP", "MDCH_HOL",
                "MDCH_PLAY", "MDCH_PLY",  "MDCH_TEA",  "MDCH_TRP", "MDCH_VEG")

# Labels corrected 2026-07-28 against the official HBAI item wording in
# 5828_hbai_2425_harmonised_dataset_variables_guide.xlsx -- three were wrong,
# not just imprecise: MDCH_TEA is "Have friends round for tea or a snack once
# a fortnight" (a social/hospitality item), NOT fresh fruit/veg -- that's
# actually MDCH_VEG ("Eat fresh fruit and/or vegetables every day"), which had
# been labelled just "Vegetables". MDCH_EQP is "Leisure equipment such as
# sports equipment or a bicycle", NOT school equipment. MDCH_PLAY is "Go to a
# playgroup at least once a week" (an early-years attendance item), NOT
# generic indoor play. Do not revert to the old labels.
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

# ─────────────────────────────────────────────────────────────────────────────
# LOAD DATA
# ─────────────────────────────────────────────────────────────────────────────
cat("Loading data...\n")
df <- fread(DATA_PATH)
cat(sprintf("  Raw: %s rows, %s cols\n", format(nrow(df), big.mark=","), ncol(df)))

# Coerce key variables
df[, YEAR      := as.integer(YEAR)]
df[, treated   := as.numeric(scotland)]
if ("post" %in% names(df)) {
  cat("  Using `post` already in hbai_clean.csv -- may carry 01_hbai_prep.R's exact 14-Nov-2022\n")
  cat("  FRS-interview-date refinement for FY2022/23, not just the FY>=2023 rule. See that\n")
  cat("  script's console output for which branch ran when the CSV was last built.\n")
  df[, post := as.numeric(post)]
} else {
  df[, post := as.numeric(YEAR >= 2023)]
}
df[, tp        := treated * post]              # DiD term — explicit product, no formula ambiguity
df[, YEAR_f    := factor(YEAR)]                # factor for FE / event study
df[, GVTREGN_f := factor(GVTREGN)]            # descriptive/robustness use only; SEs
                                               # clustered at SERNUM (household), not region --
                                               # region-level clustering is anti-conservative
                                               # here (see 05b_stage1_pretrend_diagnostics.R)

# Diagnostic: check treatment/post variation and tp coefficient name
cat(sprintf("  treated: Scotland=%s  England=%s\n",
            sum(df$treated == 1, na.rm=TRUE), sum(df$treated == 0, na.rm=TRUE)))
cat(sprintf("  post:    pre=%s  post=%s\n",
            sum(df$post == 0, na.rm=TRUE), sum(df$post == 1, na.rm=TRUE)))
cat(sprintf("  tp:      0=%s  1=%s\n",
            sum(df$tp == 0, na.rm=TRUE), sum(df$tp == 1, na.rm=TRUE)))

df[, MDCH      := suppressWarnings(as.numeric(MDCH))]
df[, MDCH      := ifelse(MDCH < 0, NA_real_, MDCH)]  # sentinels → NA

# Numeric coerce for all MDCH items and outcomes
for (v in c(MDCH_ITEMS, "mdch_any", "mdch_count", "mdch_severe",
            "food_insecure", "very_low_food_sec")) {
  if (v %in% names(df))
    df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
}

cat(sprintf("  Sample: %s rows, years: %s\n",
            format(nrow(df), big.mark=","),
            paste(sort(unique(df$YEAR)), collapse=", ")))

# ─────────────────────────────────────────────────────────────────────────────
# COVARIATES
# CASE (Stewart et al. 2025) paper footnote (iii): "Controls include: a dummy
# variable equal to one if the child resides in a household with a head aged
# under 25 years; female head of household; ethnicity (five categories);
# disability status of the household; lone parent; and large family, defined
# as three or more dependent children in the household." These are the exact
# six controls used below -- NOT income, benefit-receipt, or housing tenure,
# which the CASE paper does not control for and which carry a mediator/bad-
# control risk here (income and, pre-Nov-2022, benefit-receipt respond
# mechanically to SCP eligibility/receipt itself).
#
# Same six variables already used in 02_summary_stats.R's "cf. CASE Table 2"
# background-characteristics table, so the balance table and this regression
# are now built on a consistent operationalisation of Stewart's controls.
#
# IMPORTANT: these must be added to `df` BEFORE the df_mdch/df_mdch_flag
# subsamples are sliced off below -- data.table subsets are snapshots, not
# live views, so columns added to `df` afterward would silently not appear
# in df_mdch/df_mdch_flag (this broke the first run: "not in the data set").
# ─────────────────────────────────────────────────────────────────────────────
df[, young_head          := as.numeric(AGEHDBAND == 1)]   # head aged 16-24
df[, female_head         := as.numeric(SEXHD == 2)]
df[, disabled_household  := as.numeric(DSCORFAM == 2)]
df[, lone_parent         := as.numeric(MARITAL_WITHKID == 1)]
df[, large_family        := as.numeric(NUMBKIDS == 3)]     # 3+ dependent children
df[, ETH_f               := factor(ifelse(ETH == 99, NA, ETH))]  # 99 = not declared -> NA

# ── Subsamples (created after tp AND the covariates above are added to df) ───
df_mdch      <- df[mdch_observed == 1]      # MDCH module observed
df_mdch_flag <- df[!is.na(MDCH)]           # official DWP flag, 2017-2024
# Food insecurity is a separate USDA-derived module (WhoFood questions) with
# its own availability window/non-response pattern -- deliberately NOT
# restricted to df_mdch's "MDCH module observed" criterion, which describes
# completion of the (unrelated) deprivation-item battery. Own subsample,
# same pattern as df_mdch_flag above.
df_food      <- df[!is.na(food_insecure)]  # food security module observed

cat(sprintf("  MDCH items subsample:     %s rows\n", format(nrow(df_mdch),      big.mark=",")))
cat(sprintf("  MDCH official flag:       %s rows\n", format(nrow(df_mdch_flag), big.mark=",")))
cat(sprintf("  Food insecurity module:   %s rows\n", format(nrow(df_food),      big.mark=",")))

# CASE (2025) replication spec -- exactly the paper's six controls.
CASE_COVS <- c("young_head", "female_head", "ETH_f",
               "disabled_household", "lone_parent", "large_family")

# Extended spec: CASE controls + child's own AGE. NOT part of the replication
# claim -- reported as a separate robustness variant (Stage 1b'). Age is
# plausibly pre-determined w.r.t. treatment (not caused by receiving SCP) and
# substantively relevant here because several MDCH items (school equipment,
# indoor/outdoor play, holidays) are age-differentiated in what they capture.
CASE_COVS_EXT <- c(CASE_COVS, "AGE")

# Retained for reference only -- NOT part of the CASE replication spec above.
# Income/benefits are mediator/bad-control risks; none of these match
# Stewart's actual published controls.
DEMO_COVS    <- c("AGE", "SEX", "ADULTH", "NUMBKIDS")
INCOME_COVS  <- c("S_OE_BHC", "BENBU_UC_OR_EQUIV", "BENBU_CTC", "BENBU_IS", "BENBU_DLA")
HOUSING_COVS <- c("TENHBAI")

# All OLS covariates (used in the headline covariate-adjusted spec)
OLS_COVS <- CASE_COVS

# DiD coefficient name — tp is a pre-computed numeric column (treated * post).
# Using an explicit column avoids all formula-interaction naming ambiguity in fixest.
DID_TERM <- "tp"

# Helper: build fixest DiD formula
# y ~ treated + tp [+ covs] | YEAR_f
# treated: baseline Scotland-England gap; YEAR_f: common time trends; tp: DiD estimator.
make_did_fml <- function(outcome, covs = NULL) {
  base <- paste(outcome, "~ treated + tp")
  if (!is.null(covs) && length(covs) > 0)
    base <- paste(base, "+", paste(covs, collapse = " + "))
  as.formula(paste(base, "| YEAR_f"))
}

# ─────────────────────────────────────────────────────────────────────────────
# STAGE 1A: SIMPLE 2×2 DiD (no covariates, no FE beyond treated + post)
# Establishes the raw treatment effect before any adjustment
# ─────────────────────────────────────────────────────────────────────────────
cat("\n── Stage 1a: Simple 2×2 DiD ────────────────────────────────────────────\n")

simple_outcomes <- list(
  "Any deprivation (mdch_any)"    = list(data = df_mdch,      y = "mdch_any"),
  "Deprivation count (mdch_count)"= list(data = df_mdch,      y = "mdch_count"),
  "Severe deprivation"            = list(data = df_mdch,      y = "mdch_severe"),
  "Official MDCH flag"            = list(data = df_mdch_flag, y = "MDCH"),
  # Baseline-only, for comparability with Stewart et al. (CASE, 2025), who
  # include food insecurity alongside material deprivation in their headline
  # table. NOT carried into Stage 2+ (item-level/DML decomposition): food
  # insecurity is not one of the ten MDCH items, so it has no place in an
  # item-level breakdown of material deprivation specifically -- see
  # Methodology 4.x for the explicit scope note.
  "Food insecurity"               = list(data = df_food,      y = "food_insecure")
)

simple_models <- lapply(simple_outcomes, function(spec) {
  feols(
    make_did_fml(spec$y),
    data    = spec$data,
    weights = ~GS_INDCH,
    cluster = ~SERNUM,
    notes   = FALSE
  )
})

modelsummary(
  simple_models,
  stars    = c("*" = .1, "**" = .05, "***" = .01),
  coef_map = c("tp" = "DiD (Scotland x Post)"),
  gof_map  = c("nobs", "r.squared"),
  title    = "Table: Simple 2x2 DiD estimates",
  output   = file.path(TABLES_DIR, "table_simple_did.tex")
)
cat("  ✓ Simple DiD table saved (LaTeX)\n\n")

# Console display
cat("── Table 1a: Simple 2x2 DiD ─────────────────────────────────────────────\n")
etable(simple_models,
       keep_raw  = "^tp$",
       se.below  = TRUE,
       signif.code = c("***"=.01, "**"=.05, "*"=.1),
       headers   = list("mdch_any", "mdch_count", "mdch_severe", "MDCH flag", "food_insecure"))

# Print to console
cat("\nSimple DiD estimates:\n")
for (nm in names(simple_models)) {
  cf  <- coef(simple_models[[nm]])
  sv  <- fixest::se(simple_models[[nm]])
  pv  <- fixest::pvalue(simple_models[[nm]])
  if (!DID_TERM %in% names(cf)) { cat(sprintf("  %-40s  [%s dropped]\n", nm, DID_TERM)); next }
  sig <- ifelse(pv[DID_TERM] < .01, "***", ifelse(pv[DID_TERM] < .05, "**",
               ifelse(pv[DID_TERM] < .1, "*", "")))
  cat(sprintf("  %-40s  coef=%6.4f  SE=%6.4f  %s\n", nm, cf[DID_TERM], sv[DID_TERM], sig))
}

# CSV companion to table_simple_did.tex (same numbers, easier to scan/reuse)
simple_did_csv <- bind_rows(lapply(names(simple_models), function(nm) {
  m <- simple_models[[nm]]
  cf <- coef(m); sv <- fixest::se(m); pv <- fixest::pvalue(m); ci <- confint(m)
  if (!DID_TERM %in% names(cf)) return(NULL)
  data.frame(outcome = nm, coef = cf[DID_TERM], se = sv[DID_TERM], pval = pv[DID_TERM],
             ci_lo = ci[DID_TERM, 1], ci_hi = ci[DID_TERM, 2], n_obs = nobs(m), r2 = r2(m, "r2"))
}))
write.csv(simple_did_csv, file.path(TABLES_DIR, "table_simple_did.csv"), row.names = FALSE)
cat("  ✓ Simple DiD table saved (CSV)\n")

# ─────────────────────────────────────────────────────────────────────────────
# STAGE 1B: COVARIATE-ADJUSTED DiD (replication of Stewart 2025)
# Adds demographic, income, and housing controls + year FEs
# ─────────────────────────────────────────────────────────────────────────────
cat("\n── Stage 1b: Covariate-adjusted DiD (Stewart replication) ─────────────\n")

# Available covariates (drop any not in dataset)
avail_covs <- OLS_COVS[OLS_COVS %in% names(df)]
cov_str    <- paste(avail_covs, collapse = " + ")

adj_models <- lapply(simple_outcomes, function(spec) {
  feols(
    make_did_fml(spec$y, avail_covs),
    data    = spec$data,
    weights = ~GS_INDCH,
    cluster = ~SERNUM,
    notes   = FALSE
  )
})

# Diagnostic: show which models have tp and which dropped it
cat("  Coefficient check for adj_models:\n")
for (nm in names(adj_models)) {
  cf_names <- names(coef(adj_models[[nm]]))
  if ("tp" %in% cf_names) {
    cat(sprintf("    %-42s  ✓ tp found\n", nm))
  } else {
    cat(sprintf("    %-42s  ✗ tp DROPPED — coefs: %s\n", nm,
                paste(cf_names, collapse = ", ")))
  }
}

adj_models_ok <- Filter(function(m) "tp" %in% names(coef(m)), adj_models)
if (length(adj_models_ok) > 0) {
  modelsummary(
    adj_models_ok,
    stars    = c("*" = .1, "**" = .05, "***" = .01),
    coef_map = c("tp" = "DiD (Scotland x Post)"),
    gof_map  = c("nobs", "r.squared"),
    title    = "Table: Covariate-adjusted DiD (Stewart 2025 replication)",
    output   = file.path(TABLES_DIR, "table_adj_did.tex")
  )
  cat("  ✓ Adjusted DiD table saved (LaTeX)\n\n")

  cat("── Table 1b: Covariate-adjusted DiD ────────────────────────────────────\n")
  etable(adj_models_ok,
         keep_raw    = "^tp$",
         se.below    = TRUE,
         signif.code = c("***"=.01, "**"=.05, "*"=.1))
} else {
  cat("  ✗ tp dropped from ALL adj_models — table not written; check collinearity above\n")
}

cat("\nAdjusted DiD estimates:\n")
for (nm in names(adj_models)) {
  cf  <- coef(adj_models[[nm]])
  sv  <- fixest::se(adj_models[[nm]])
  pv  <- fixest::pvalue(adj_models[[nm]])
  if (!DID_TERM %in% names(cf)) { cat(sprintf("  %-40s  [%s dropped]\n", nm, DID_TERM)); next }
  sig <- ifelse(pv[DID_TERM] < .01, "***", ifelse(pv[DID_TERM] < .05, "**",
               ifelse(pv[DID_TERM] < .1, "*", "")))
  cat(sprintf("  %-40s  coef=%6.4f  SE=%6.4f  %s\n", nm, cf[DID_TERM], sv[DID_TERM], sig))
}

# CSV companion to table_adj_did.tex (same numbers, easier to scan/reuse)
adj_did_csv <- bind_rows(lapply(names(adj_models), function(nm) {
  m <- adj_models[[nm]]
  cf <- coef(m); sv <- fixest::se(m); pv <- fixest::pvalue(m); ci <- confint(m)
  if (!DID_TERM %in% names(cf)) return(NULL)
  data.frame(outcome = nm, coef = cf[DID_TERM], se = sv[DID_TERM], pval = pv[DID_TERM],
             ci_lo = ci[DID_TERM, 1], ci_hi = ci[DID_TERM, 2], n_obs = nobs(m), r2 = r2(m, "r2"))
}))
write.csv(adj_did_csv, file.path(TABLES_DIR, "table_adj_did.csv"), row.names = FALSE)
cat("  ✓ Adjusted DiD table saved (CSV)\n")

# ─────────────────────────────────────────────────────────────────────────────
# STAGE 1B': CASE CONTROLS + CHILD AGE (extended spec, NOT part of the
# replication claim -- reported separately as a robustness check on whether
# the CASE (2025) spec is sensitive to adding the child's own age)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n── Stage 1b': CASE controls + child age (extended, not a replication) ──\n")

avail_covs_ext <- CASE_COVS_EXT[CASE_COVS_EXT %in% names(df)]

ext_models <- lapply(simple_outcomes, function(spec) {
  feols(
    make_did_fml(spec$y, avail_covs_ext),
    data    = spec$data,
    weights = ~GS_INDCH,
    cluster = ~SERNUM,
    notes   = FALSE
  )
})

ext_models_ok <- Filter(function(m) "tp" %in% names(coef(m)), ext_models)
if (length(ext_models_ok) > 0) {
  modelsummary(
    ext_models_ok,
    stars    = c("*" = .1, "**" = .05, "***" = .01),
    coef_map = c("tp" = "DiD (Scotland x Post)"),
    gof_map  = c("nobs", "r.squared"),
    title    = "Table: CASE controls + child age (extended, robustness only)",
    output   = file.path(TABLES_DIR, "table_adj_ext_did.tex")
  )
  cat("  ✓ Extended (CASE + age) DiD table saved (LaTeX)\n\n")

  cat("── Table 1b': CASE controls + child age ────────────────────────────────\n")
  etable(ext_models_ok,
         keep_raw    = "^tp$",
         se.below    = TRUE,
         signif.code = c("***"=.01, "**"=.05, "*"=.1))
} else {
  cat("  ✗ tp dropped from ALL ext_models — table not written\n")
}

cat("\nExtended (CASE + age) DiD estimates:\n")
for (nm in names(ext_models)) {
  cf  <- coef(ext_models[[nm]])
  sv  <- fixest::se(ext_models[[nm]])
  pv  <- fixest::pvalue(ext_models[[nm]])
  if (!DID_TERM %in% names(cf)) { cat(sprintf("  %-40s  [%s dropped]\n", nm, DID_TERM)); next }
  sig <- ifelse(pv[DID_TERM] < .01, "***", ifelse(pv[DID_TERM] < .05, "**",
               ifelse(pv[DID_TERM] < .1, "*", "")))
  cat(sprintf("  %-40s  coef=%6.4f  SE=%6.4f  %s\n", nm, cf[DID_TERM], sv[DID_TERM], sig))
}

ext_did_csv <- bind_rows(lapply(names(ext_models), function(nm) {
  m <- ext_models[[nm]]
  cf <- coef(m); sv <- fixest::se(m); pv <- fixest::pvalue(m); ci <- confint(m)
  if (!DID_TERM %in% names(cf)) return(NULL)
  data.frame(outcome = nm, coef = cf[DID_TERM], se = sv[DID_TERM], pval = pv[DID_TERM],
             ci_lo = ci[DID_TERM, 1], ci_hi = ci[DID_TERM, 2], n_obs = nobs(m), r2 = r2(m, "r2"))
}))
write.csv(ext_did_csv, file.path(TABLES_DIR, "table_adj_ext_did.csv"), row.names = FALSE)
cat("  ✓ Extended (CASE + age) DiD table saved (CSV)\n")

# ─────────────────────────────────────────────────────────────────────────────
# STAGE 1B'': CASE EXACT-REPLICATION SPEC (diagnostic add-on, NOT a replacement
# for Simple/Adjusted/Extended above)
#
# Stewart et al. (2025) equation (1): Y = a + l1*Scot + l2*Post + d*(Scot*Post)
# + b*X + e, with year FE added in their "controls" column. Our Adjusted spec
# above matches their controls+year-FE column on N, controls, and treatment
# timing exactly, but omits a standalone `post` regressor -- we only ever
# enter it via `tp` (treated*post), relying on YEAR_f to soak up everything
# else. That's not fully redundant: YEAR_f is constant across an entire survey
# year, but `post` has real sub-year variation within FYE2023 (the exact
# 14-Nov-2022 cutoff), so CASE's model can separately estimate a common
# (non-Scotland-specific) within-FYE2023 shift that ours currently cannot.
#
# This spec adds `post` explicitly -- y ~ treated + post + tp + X | YEAR_f --
# nesting CASE's equation while keeping our extra year-FE flexibility, as a
# check on how much of the CASE magnitude gap (-0.076 vs -0.085 on the MDCH
# flag; -0.066 vs -0.088 on food insecurity, per their Table 3) that one
# omission explains. Kept as a separate diagnostic table for now; fold into
# (or swap for) the main Adjusted/Extended spec later once reviewed.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n── Stage 1b'': CASE exact-replication spec (explicit Post term) ───────\n")

make_case_fml <- function(outcome, covs = NULL) {
  base <- paste(outcome, "~ treated + post + tp")
  if (!is.null(covs) && length(covs) > 0)
    base <- paste(base, "+", paste(covs, collapse = " + "))
  as.formula(paste(base, "| YEAR_f"))
}

case_models <- lapply(simple_outcomes, function(spec) {
  feols(
    make_case_fml(spec$y, avail_covs),
    data    = spec$data,
    weights = ~GS_INDCH,
    cluster = ~SERNUM,
    notes   = FALSE
  )
})

case_models_ok <- Filter(function(m) "tp" %in% names(coef(m)), case_models)
if (length(case_models_ok) > 0) {
  # No coef_map here, deliberately -- we want every coefficient printed
  # (treated, post, tp, and all six controls), matching CASE's Table 3 layout,
  # not just the DiD term as in the Simple/Adjusted/Extended tables above.
  modelsummary(
    case_models_ok,
    stars    = c("*" = .1, "**" = .05, "***" = .01),
    gof_map  = c("nobs", "r.squared"),
    title    = "Table: CASE exact-replication spec (explicit Post term, all coefficients)",
    output   = file.path(TABLES_DIR, "table_case_exact_did.tex")
  )
  cat("  ✓ CASE exact-replication table saved (LaTeX, full coefficients)\n\n")

  cat("── Table 1b'': CASE exact-replication spec (full coefficients) ────────\n")
  etable(case_models_ok,
         se.below    = TRUE,
         signif.code = c("***"=.01, "**"=.05, "*"=.1))
} else {
  cat("  ✗ tp dropped from ALL case_models — table not written\n")
}

cat("\nCASE exact-replication DiD estimates (tp only, for quick comparison):\n")
for (nm in names(case_models)) {
  cf  <- coef(case_models[[nm]])
  sv  <- fixest::se(case_models[[nm]])
  pv  <- fixest::pvalue(case_models[[nm]])
  if (!DID_TERM %in% names(cf)) { cat(sprintf("  %-40s  [%s dropped]\n", nm, DID_TERM)); next }
  sig <- ifelse(pv[DID_TERM] < .01, "***", ifelse(pv[DID_TERM] < .05, "**",
               ifelse(pv[DID_TERM] < .1, "*", "")))
  cat(sprintf("  %-40s  coef=%6.4f  SE=%6.4f  %s\n", nm, cf[DID_TERM], sv[DID_TERM], sig))
}

# Full-coefficient CSV (every term, not just tp) -- this is what the LaTeX
# table-generation script (11_make_latex_tables.R) will read to reproduce a
# CASE-Table-3-style layout with all covariates shown.
case_exact_csv <- bind_rows(lapply(names(case_models_ok), function(nm) {
  m  <- case_models_ok[[nm]]
  cf <- coef(m); sv <- fixest::se(m); pv <- fixest::pvalue(m); ci <- confint(m)
  data.frame(outcome = nm, term = names(cf), coef = as.numeric(cf),
             se = as.numeric(sv), pval = as.numeric(pv),
             ci_lo = ci[, 1], ci_hi = ci[, 2],
             n_obs = nobs(m), r2 = r2(m, "r2"), row.names = NULL)
}))
write.csv(case_exact_csv, file.path(TABLES_DIR, "table_case_exact_did.csv"), row.names = FALSE)
cat("  ✓ CASE exact-replication table saved (CSV, full coefficients)\n")

# ─────────────────────────────────────────────────────────────────────────────
# STAGE 1C: ITEM-LEVEL OLS DiD (one regression per MDCH item)
# Supervisor note: "looking at each outcome individually gives more information"
# ─────────────────────────────────────────────────────────────────────────────
cat("\n── Stage 1c: Item-level OLS DiD ────────────────────────────────────────\n")

avail_items <- MDCH_ITEMS[MDCH_ITEMS %in% names(df_mdch)]

item_results <- lapply(avail_items, function(item) {
  fit <- tryCatch(
    feols(make_did_fml(item, avail_covs), data = df_mdch, weights = ~GS_INDCH,
          cluster = ~SERNUM, notes = FALSE),
    error = function(e) NULL
  )
  if (is.null(fit) || !DID_TERM %in% names(coef(fit))) return(NULL)
  data.frame(
    item      = item,
    label     = MDCH_LABELS[item],
    coef      = coef(fit)[DID_TERM],
    se        = fixest::se(fit)[DID_TERM],
    pval      = fixest::pvalue(fit)[DID_TERM],
    n         = nobs(fit)
  )
}) |> bind_rows()

# Benjamini-Hochberg FDR correction across items
item_results$pval_bh <- p.adjust(item_results$pval, method = "BH")
item_results$sig_raw <- ifelse(item_results$pval    < .01, "***",
                        ifelse(item_results$pval    < .05, "**",
                        ifelse(item_results$pval    < .1,  "*",  "")))
item_results$sig_bh  <- ifelse(item_results$pval_bh < ALPHA, "✓", "")

cat("\nItem-level DiD (BH-corrected):\n")
cat(sprintf("  %-30s  %8s  %8s  %8s  %8s  %4s\n",
            "Item", "Coef", "SE", "p-raw", "p-BH", "FDR"))
for (i in seq_len(nrow(item_results))) {
  r <- item_results[i, ]
  cat(sprintf("  %-30s  %8.4f  %8.4f  %8.3f  %8.3f  %4s\n",
              r$label, r$coef, r$se, r$pval, r$pval_bh, r$sig_bh))
}

write.csv(item_results,
          file.path(TABLES_DIR, "table_item_did.csv"),
          row.names = FALSE)
cat("  ✓ Item-level results saved\n")

# ─────────────────────────────────────────────────────────────────────────────
# ROBUSTNESS: item-level sample composition & sensitivity to FYE2024
# MDCH_ITEMS are only populated for FYE2024 respondents on the OLD question
# wording (~25-30% of that year -- see 05_stage1_parallel_trends.R), so FYE2024's
# item-level N is thin, especially for Scotland. Check that here, then re-run
# Stage 1c excluding FYE2024 to see if item-level results depend on it.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n── Robustness: item-level sample composition by year x country ─────────\n")
item_n_by_year <- df_mdch[, .(n = .N), by = .(YEAR, treated)][order(YEAR, treated)]
print(item_n_by_year)
cat("  (treated: 0 = England, 1 = Scotland -- note how small FYE2024 x Scotland is\n")
cat("   relative to every other year, since only the old-questions arm has items.)\n\n")

item_results_ex2024 <- lapply(avail_items, function(item) {
  fit <- tryCatch(
    feols(make_did_fml(item, avail_covs), data = df_mdch[YEAR != 2024],
          weights = ~GS_INDCH, cluster = ~SERNUM, notes = FALSE),
    error = function(e) NULL
  )
  if (is.null(fit) || !DID_TERM %in% names(coef(fit))) return(NULL)
  data.frame(
    item = item, label = MDCH_LABELS[item],
    coef_ex2024 = coef(fit)[DID_TERM],
    se_ex2024   = fixest::se(fit)[DID_TERM],
    pval_ex2024 = fixest::pvalue(fit)[DID_TERM],
    n_ex2024    = nobs(fit)
  )
}) |> bind_rows()

item_compare <- item_results |>
  select(item, label, coef, se, pval, n) |>
  left_join(item_results_ex2024, by = c("item", "label"))

cat("Item-level DiD: full sample (incl. FYE2024) vs. excluding FYE2024:\n")
cat(sprintf("  %-20s  %10s  %10s  %8s  %10s  %10s  %8s\n",
            "Item", "coef(all)", "coef(ex24)", "n(all)", "n(ex24)", "", ""))
for (i in seq_len(nrow(item_compare))) {
  r <- item_compare[i, ]
  cat(sprintf("  %-20s  %10.4f  %10.4f  %8d  %10d\n",
              r$label, r$coef, r$coef_ex2024, r$n, r$n_ex2024))
}
write.csv(item_compare, file.path(TABLES_DIR, "table_item_did_fye2024_sensitivity.csv"),
          row.names = FALSE)
cat("  ✓ Item-level FYE2024-sensitivity comparison saved\n")

# ─────────────────────────────────────────────────────────────────────────────
# STAGE 1D: EVENT STUDY
# Year-specific treatment effects relative to FYE 2022 (last pre-period year)
# Tests parallel pre-trends assumption visually and via Wald test
# ─────────────────────────────────────────────────────────────────────────────
cat("\n── Stage 1d: Event study ───────────────────────────────────────────────\n")

# Helper: run event study and return tidy coefficient frame
# i(YEAR_f, treated, ref) creates year-specific Scotland coefficients.
# Include treated as a baseline regressor; year FE absorbed by i() interactions.
run_event_study <- function(data, outcome, ref = REF_YEAR) {
  fml <- as.formula(paste0(
    outcome, " ~ treated + i(YEAR_f, treated, ref = '", ref, "') | YEAR_f"
  ))
  fit <- tryCatch(
    feols(fml, data = data, weights = ~GS_INDCH,
          cluster = ~SERNUM, notes = FALSE),
    error = function(e) { message("Event study failed for ", outcome, ": ", e$message); NULL }
  )
  if (is.null(fit)) return(NULL)

  # Extract i() coefficients — named "YEAR_f::YYYY:treated" by fixest
  td <- broom::tidy(fit, conf.int = TRUE) |>
    filter(str_detect(term, "YEAR_f::")) |>
    mutate(
      year = as.integer(str_extract(term, "[0-9]{4}")),
      post = year >= 2023
    )
  attr(td, "fit") <- fit
  td
}

# Pre-trend Wald test helper
pretrend_wald <- function(fit, ref = REF_YEAR) {
  pre_years <- setdiff(as.character(sort(unique(df$YEAR[df$YEAR < 2023]))), as.character(ref))
  pre_terms <- paste0("YEAR_f::", pre_years, ":treated")
  pre_terms <- pre_terms[pre_terms %in% names(coef(fit))]
  if (length(pre_terms) == 0) return(NA_real_)
  wt <- wald(fit, keep = pre_terms)
  wt$p
}

# Run event study for composite outcomes
es_composite <- list(
  mdch_any   = run_event_study(df_mdch,      "mdch_any"),
  mdch_count = run_event_study(df_mdch,      "mdch_count"),
  MDCH_flag  = run_event_study(df_mdch_flag, "MDCH")
)

# Plot helper
plot_event_study <- function(td, title, ylab = "DiD coefficient") {
  ggplot(td, aes(x = year, y = estimate,
                 ymin = conf.low, ymax = conf.high,
                 colour = post, fill = post)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_vline(xintercept = 2022.5, linetype = "dashed",
               colour = "firebrick", linewidth = 0.7) +
    geom_ribbon(alpha = 0.15, colour = NA) +
    geom_point(size = 2.5) +
    geom_errorbar(width = 0.2) +
    annotate("point", x = REF_YEAR, y = 0, size = 3, shape = 1, colour = "grey40") +
    scale_colour_manual(values = c("FALSE" = "#2c7bb6", "TRUE" = "#d7191c"),
                        labels = c("Pre-SCP", "Post-SCP"), name = "") +
    scale_fill_manual(  values = c("FALSE" = "#2c7bb6", "TRUE" = "#d7191c"),
                        labels = c("Pre-SCP", "Post-SCP"), name = "") +
    scale_x_continuous(breaks = sort(unique(td$year))) +
    labs(title = title,
         subtitle = paste0("Reference year: FYE ", REF_YEAR,
                           " | Dashed line = SCP expansion (Nov 2022)"),
         x = "Financial year ending", y = ylab,
         caption = "SEs clustered at household (SERNUM) level. Weights: GS_INDCH.") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"))
}

# Save composite event study plots
es_specs <- list(
  list(key = "mdch_any",   title = "Event Study: Any material deprivation (mdch_any)",
       ylab = "DiD estimate (proportion)"),
  list(key = "mdch_count", title = "Event Study: Mean items lacking (mdch_count)",
       ylab = "DiD estimate (no. items)"),
  list(key = "MDCH_flag",  title = "Event Study: Official DWP MDCH flag",
       ylab = "DiD estimate (proportion)")
)

for (spec in es_specs) {
  td <- es_composite[[spec$key]]
  if (is.null(td)) next
  p  <- plot_event_study(td, spec$title, spec$ylab)
  fp <- file.path(FIGURES_DIR, paste0("es_", spec$key, ".png"))
  ggsave(fp, p, width = 9, height = 5.5, dpi = 150)
  cat(sprintf("  ✓ %s\n", fp))

  # Wald pre-trend test
  fit <- attr(td, "fit")
  if (!is.null(fit)) {
    pw <- tryCatch(pretrend_wald(fit), error = function(e) NA_real_)
    cat(sprintf("    Pre-trend Wald p = %.3f %s\n", pw,
                ifelse(!is.na(pw) & pw < .05, "⚠  (possible pre-trend)", "✓")))
  }
}

# Event study for all 10 items — small multiples
cat("\n  Running item-level event studies...\n")
item_es <- lapply(avail_items, function(item) {
  td <- run_event_study(df_mdch, item)
  if (!is.null(td)) td$item <- item
  td
}) |> bind_rows()

if (nrow(item_es) > 0) {
  item_es <- item_es |>
    mutate(label = MDCH_LABELS[item])

  p_items <- ggplot(item_es,
                    aes(x = year, y = estimate,
                        ymin = conf.low, ymax = conf.high,
                        colour = post)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = 2022.5, linetype = "dashed",
               colour = "firebrick", linewidth = 0.5, alpha = 0.7) +
    geom_ribbon(aes(fill = post), alpha = 0.1, colour = NA) +
    geom_point(size = 1.8) +
    geom_errorbar(width = 0.3) +
    scale_colour_manual(values = c("FALSE" = "#2c7bb6", "TRUE" = "#d7191c"), guide = "none") +
    scale_fill_manual(  values = c("FALSE" = "#2c7bb6", "TRUE" = "#d7191c"), guide = "none") +
    scale_x_continuous(breaks = c(2017, 2019, 2022, 2023, 2024)) +
    facet_wrap(~label, ncol = 3, scales = "free_y") +
    labs(title    = "Event Studies: Individual MDCH Items",
         subtitle = paste0("Reference year: FYE ", REF_YEAR),
         x = "Financial year ending", y = "DiD estimate") +
    theme_minimal(base_size = 9) +
    theme(panel.grid.minor  = element_blank(),
          strip.text        = element_text(face = "bold", size = 8),
          plot.title        = element_text(face = "bold"))

  fp_items <- file.path(FIGURES_DIR, "es_items_grid.png")
  ggsave(fp_items, p_items, width = 12, height = 10, dpi = 150)
  cat(sprintf("  ✓ %s\n", fp_items))
}

# ─────────────────────────────────────────────────────────────────────────────
# ROBUSTNESS: exclude FYE 2022 from pre-period
# FYE 2022 (FY 2021/22): partial SCP rollout + admin data series break
# ─────────────────────────────────────────────────────────────────────────────
cat("\n── Robustness: excluding FYE 2022 from pre-period ──────────────────────\n")

df_rob      <- df_mdch[YEAR != 2022]
rob_models  <- lapply(c("mdch_any", "mdch_count", "mdch_severe"), function(y) {
  feols(make_did_fml(y, avail_covs), data = df_rob, weights = ~GS_INDCH,
        cluster = ~SERNUM, notes = FALSE)
})
names(rob_models) <- c("mdch_any", "mdch_count", "mdch_severe")

cat("Robustness estimates (FYE 2022 excluded from pre-period):\n")
for (nm in names(rob_models)) {
  cf <- coef(rob_models[[nm]]); sv <- fixest::se(rob_models[[nm]]); pv <- fixest::pvalue(rob_models[[nm]])
  sig <- ifelse(pv[DID_TERM] < .01, "***", ifelse(pv[DID_TERM] < .05, "**",
                ifelse(pv[DID_TERM] < .1, "*", "")))
  cat(sprintf("  %-20s  coef=%6.4f  SE=%6.4f  %s\n", nm, cf[DID_TERM], sv[DID_TERM], sig))
}

# ─────────────────────────────────────────────────────────────────────────────
# ROBUSTNESS: income/benefit covariates -- NOTE
# The CASE (2025) replication spec (Stage 1b, above) never included income or
# benefit-receipt covariates in the first place (see the COVARIATES section
# header for the mediator/bad-control rationale), so there's no "drop income"
# check left to run here. The sensitivity check that matters now is CASE
# controls vs. CASE + child age -- see Stage 1b' above.
# ─────────────────────────────────────────────────────────────────────────────

# ─────────────────────────────────────────────────────────────────────────────
# SAVE FULL RESULTS SUMMARY
# ─────────────────────────────────────────────────────────────────────────────
all_models <- c(
  setNames(simple_models, paste0("simple_", names(simple_models))),
  setNames(adj_models,    paste0("adj_",    names(adj_models)))
)

all_models_ok <- Filter(function(m) "tp" %in% names(coef(m)), all_models)
modelsummary(
  all_models_ok,
  stars    = c("*" = .1, "**" = .05, "***" = .01),
  coef_map = c("tp" = "DiD (Scotland x Post)"),
  gof_map  = c("nobs", "r.squared"),
  title    = "OLS DiD Results - Simple and Covariate-Adjusted",
  output   = file.path(TABLES_DIR, "table_did_full.tex")
)

# Console summary: simple vs adjusted, side by side for the two primary outcomes
cat("\n── Table: Simple vs Adjusted DiD — primary outcomes ────────────────────\n")
etable(
  simple_models[["Any deprivation (mdch_any)"]],
  adj_models_ok[["Any deprivation (mdch_any)"]],
  simple_models[["Deprivation count (mdch_count)"]],
  adj_models_ok[["Deprivation count (mdch_count)"]],
  keep_raw    = "^tp$",
  se.below    = TRUE,
  signif.code = c("***"=.01, "**"=.05, "*"=.1),
  headers     = list("mdch_any\n(simple)", "mdch_any\n(adjusted)",
                     "mdch_count\n(simple)", "mdch_count\n(adjusted)")
)

cat("\n✓ All outputs saved to:\n")
cat(sprintf("  Tables:  %s\n", TABLES_DIR))
cat(sprintf("  Figures: %s\n", FIGURES_DIR))
cat("\nNext: run 04_dml_did.R for Double ML estimates\n")
