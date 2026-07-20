# ─────────────────────────────────────────────────────────────────────────────
# 03_did_replication.R
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
# Run after 01_hbai_prep.R has produced hbai_lca.csv
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
  post    = "Post (FYE 2023)"
))

# ─────────────────────────────────────────────────────────────────────────────
# CONFIG
# ─────────────────────────────────────────────────────────────────────────────
DATA_PATH   <- "/Users/guypigott/python-venv-demo/Dissertation/data/hbai_lca.csv"
FIGURES_DIR <- "/Users/guypigott/Claude/Projects/MSc Dissertation/figures"
TABLES_DIR  <- "/Users/guypigott/Claude/Projects/MSc Dissertation/tables"

dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TABLES_DIR,  showWarnings = FALSE, recursive = TRUE)

REF_YEAR   <- 2022   # last pre-treatment year; omitted in event study
ALPHA      <- 0.05

MDCH_ITEMS <- c("MDCH_BED", "MDCH_CEL", "MDCH_COAT", "MDCH_EQP", "MDCH_HOL",
                "MDCH_PLAY", "MDCH_PLY",  "MDCH_TEA",  "MDCH_TRP", "MDCH_VEG")

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
  cat("  Using `post` already in hbai_lca.csv -- may carry 01_hbai_prep.R's exact 14-Nov-2022\n")
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
                                               # here (see 05b_pretrend_diagnostics.R)

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

# ── Subsamples (created after tp is added to df) ─────────────────────────────
df_mdch      <- df[mdch_observed == 1]      # MDCH module observed
df_mdch_flag <- df[!is.na(MDCH)]           # official DWP flag, 2017-2024

cat(sprintf("  MDCH items subsample:     %s rows\n", format(nrow(df_mdch),      big.mark=",")))
cat(sprintf("  MDCH official flag:       %s rows\n", format(nrow(df_mdch_flag), big.mark=",")))

# ─────────────────────────────────────────────────────────────────────────────
# COVARIATES
# Parsimonious set for OLS replication (close to Stewart 2025)
# DML script uses full kitchen-sink set with regularisation
# ─────────────────────────────────────────────────────────────────────────────
# Core demographic controls
DEMO_COVS <- c("AGE", "SEX", "ADULTH", "NUMBKIDS")

# Benefit / income controls
# BENBU_HB excluded: near-collinear with tp (UC rollout timing differed by country).
# S_OE_BHC used over AHC (avoids SCP-income mediation). BENBU_UC_OR_EQUIV used
# instead of BENBU_UC, which is structurally NA before UC existed (pre FYE2018/19).
INCOME_COVS <- c("S_OE_BHC", "BENBU_UC_OR_EQUIV", "BENBU_CTC", "BENBU_IS", "BENBU_DLA")

# Housing
HOUSING_COVS <- c("TENHBAI")

# All OLS covariates (used in covariate-adjusted specs)
OLS_COVS <- c(DEMO_COVS, INCOME_COVS, HOUSING_COVS)

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
  "Official MDCH flag"            = list(data = df_mdch_flag, y = "MDCH")
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
       headers   = list("mdch_any", "mdch_count", "mdch_severe", "MDCH flag"))

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
# wording (~25-30% of that year -- see 05_parallel_trends.R), so FYE2024's
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
# ROBUSTNESS: without income covariates (mediator concern)
# S_OE_AHC / S_OE_BHC may partially absorb the SCP income effect
# ─────────────────────────────────────────────────────────────────────────────
cat("\n── Robustness: without income covariates ───────────────────────────────\n")

non_income_covs <- DEMO_COVS[DEMO_COVS %in% names(df)]
non_income_str  <- paste(non_income_covs, collapse = " + ")

noinc_models <- lapply(c("mdch_any", "mdch_count"), function(y) {
  feols(make_did_fml(y, non_income_covs), data = df_mdch, weights = ~GS_INDCH,
        cluster = ~SERNUM, notes = FALSE)
})
names(noinc_models) <- c("mdch_any", "mdch_count")

cat("Without income covariates:\n")
for (nm in names(noinc_models)) {
  cf <- coef(noinc_models[[nm]]); sv <- fixest::se(noinc_models[[nm]]); pv <- fixest::pvalue(noinc_models[[nm]])
  sig <- ifelse(pv[DID_TERM] < .01, "***", ifelse(pv[DID_TERM] < .05, "**",
                ifelse(pv[DID_TERM] < .1, "*", "")))
  cat(sprintf("  %-20s  coef=%6.4f  SE=%6.4f  %s\n", nm, cf[DID_TERM], sv[DID_TERM], sig))
}

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
