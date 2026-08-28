# ─────────────────────────────────────────────────────────────────────────────
# 10_placebo_wales_ni.R
# FALSIFICATION/PLACEBO CHECK (feedback point 2): Wales and NI never received
# SCP -- so if we run the SAME DiD spec Stage 1 uses for Scotland-vs-England,
# but with Wales (or NI) standing in as the "treated" nation against England,
# using the same post-period cutoff (SCP's real rollout timing, 14 Nov
# 2022 refinement / YEAR>=2023), we should find nothing.
# ─────────────────────────────────────────────────────────────────────────────

library(data.table)
library(fixest)
library(tidyverse)
library(ggplot2)

# ─────────────────────────────────────────────────────────────────────────────
# CONFIG
# ─────────────────────────────────────────────────────────────────────────────
DATA_PATH   <- "/Users/guypigott/python-venv-demo/Dissertation/data/hbai_clean_placebo.csv"
TABLES_DIR  <- "/Users/guypigott/Claude/Projects/MSc Dissertation/tables"
FIGURES_DIR <- "/Users/guypigott/Claude/Projects/MSc Dissertation/figures"
dir.create(TABLES_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

ALPHA <- 0.05
MDCH_ITEMS <- c("MDCH_BED", "MDCH_CEL", "MDCH_COAT", "MDCH_EQP", "MDCH_HOL",
                "MDCH_PLAY", "MDCH_PLY", "MDCH_TEA", "MDCH_TRP", "MDCH_VEG")

cat("Loading data...\n")
df <- fread(DATA_PATH)
if (!"post" %in% names(df)) stop("`post` column not found -- rerun 01_hbai_prep.R first.")
df[, YEAR := as.integer(YEAR)]
df[, post := as.numeric(post)]   # SCP's REAL rollout timing -- exact 14-Nov-2022 refinement where
                                  # available. This is what makes it a meaningful falsification test:
                                  # Wales/NI are being asked "did anything happen to you at the exact
                                  # moment SCP happened to Scotland", not some arbitrary date.
df[, YEAR_f := factor(YEAR)]

df[, MDCH := suppressWarnings(as.numeric(MDCH))]
df[, MDCH := ifelse(MDCH < 0, NA_real_, MDCH)]
for (v in c("mdch_any", "mdch_count", "mdch_severe")) {
  if (v %in% names(df)) df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
}

cat(sprintf("  %s rows | countries: %s | years: %s\n",
            format(nrow(df), big.mark = ","),
            paste(sort(unique(df$country_f)), collapse = ", "),
            paste(sort(unique(df$YEAR)), collapse = ", ")))

# ── CASE (Andersen et al. 2025) covariates -- identical construction to ────
# 03_stage1_baseline_did.R, so this placebo comparison varies only the
# pseudo-treated country, not the spec.
df[, young_head          := as.numeric(AGEHDBAND == 1)]
df[, female_head         := as.numeric(SEXHD == 2)]
df[, disabled_household  := as.numeric(DSCORFAM == 2)]
df[, lone_parent         := as.numeric(MARITAL_WITHKID == 1)]
df[, large_family        := as.numeric(NUMBKIDS == 3)]
df[, ETH_f               := factor(ifelse(ETH == 99, NA, ETH))]

CASE_COVS <- c("young_head", "female_head", "ETH_f",
               "disabled_household", "lone_parent", "large_family")

df_mdch      <- df[mdch_observed == 1]
df_mdch_flag <- df[!is.na(MDCH)]

make_did_fml <- function(outcome, covs = NULL) {
  base <- paste(outcome, "~ treated + tp")
  if (!is.null(covs) && length(covs) > 0) base <- paste(base, "+", paste(covs, collapse = " + "))
  as.formula(paste(base, "| YEAR_f"))
}

outcomes <- list(
  "Official MDCH flag"             = list(data = df_mdch_flag, y = "MDCH"),
  "Any deprivation (mdch_any)"     = list(data = df_mdch,      y = "mdch_any"),
  "Severe deprivation (mdch_severe)"= list(data = df_mdch,     y = "mdch_severe")
)

# ENGLAND is the control in every leg here (COUNTRY==1). Scotland (3) is
# deliberately excluded from this script entirely.
pseudo_treated <- list(
  "Wales" = 2,
  "NI"    = 4
)
CONTROL_CODE <- 1  # England

# ─────────────────────────────────────────────────────────────────────────────
# RUN: {Wales, NI} vs England, each of the 3 outcomes, Simple + Adjusted,
# using SCP's actual `post` timing as the pseudo-treatment date.
# ─────────────────────────────────────────────────────────────────────────────
extract_row <- function(fit, outcome_label, pseudo_name, spec, n_treat, n_ctrl) {
  ct <- coeftable(fit)
  if (!("tp" %in% rownames(ct))) return(NULL)
  est  <- ct["tp", "Estimate"]; se <- ct["tp", "Std. Error"]; pval <- ct["tp", "Pr(>|t|)"]
  crit <- qnorm(1 - ALPHA / 2)
  data.frame(outcome = outcome_label, pseudo_treated = pseudo_name, spec = spec,
             n_treated = n_treat, n_control = n_ctrl,
             coef = est, se = se, pval = pval,
             ci_lo = est - crit * se, ci_hi = est + crit * se,
             r2 = r2(fit, "r2"))
}

all_results <- list()
for (outcome_label in names(outcomes)) {
  spec_data <- outcomes[[outcome_label]]$data
  y <- outcomes[[outcome_label]]$y

  for (pseudo_name in names(pseudo_treated)) {
    pseudo_code <- pseudo_treated[[pseudo_name]]
    sub <- spec_data[COUNTRY %in% c(CONTROL_CODE, pseudo_code)]
    sub[, treated := as.numeric(COUNTRY == pseudo_code)]
    sub[, tp      := treated * post]
    n_treat <- sum(sub$treated == 1)
    n_ctrl  <- sum(sub$treated == 0)

    if (n_treat < 50 || n_ctrl < 50) {
      cat(sprintf("  ⚠ %s: %s vs England -- too few obs (%s=%d, England=%d), skipping\n",
                  outcome_label, pseudo_name, pseudo_name, n_treat, n_ctrl))
      next
    }

    fit_simple <- tryCatch(
      feols(make_did_fml(y), data = sub, weights = ~GS_INDCH, cluster = ~SERNUM, notes = FALSE),
      error = function(e) { cat(sprintf("  ✗ %s: %s vs England (Simple): %s\n", outcome_label, pseudo_name, e$message)); NULL })
    fit_adj <- tryCatch(
      feols(make_did_fml(y, CASE_COVS), data = sub, weights = ~GS_INDCH, cluster = ~SERNUM, notes = FALSE),
      error = function(e) { cat(sprintf("  ✗ %s: %s vs England (Adjusted): %s\n", outcome_label, pseudo_name, e$message)); NULL })

    if (!is.null(fit_simple)) all_results[[length(all_results) + 1]] <-
      extract_row(fit_simple, outcome_label, pseudo_name, "Simple", n_treat, n_ctrl)
    if (!is.null(fit_adj)) all_results[[length(all_results) + 1]] <-
      extract_row(fit_adj, outcome_label, pseudo_name, "Adjusted (CASE)", n_treat, n_ctrl)
  }
}

results_df <- bind_rows(all_results)

cat("\n── Falsification check: {Wales, NI} vs England at SCP's actual timing ──\n")
cat("   (expect coefficients close to zero and NOT significant -- that's the good outcome)\n")
cat(sprintf("  %-33s  %-9s  %-16s  %8s  %8s  %8s  %s\n",
            "Outcome", "Pseudo-tr", "Spec", "Coef", "SE", "p", ""))
for (i in seq_len(nrow(results_df))) {
  r <- results_df[i, ]
  sig <- ifelse(r$pval < .01, "***", ifelse(r$pval < .05, "**", ifelse(r$pval < .1, "*", "")))
  cat(sprintf("  %-33s  %-9s  %-16s  %8.4f  %8.4f  %8.3f  %s\n",
              r$outcome, r$pseudo_treated, r$spec, r$coef, r$se, r$pval, sig))
}

# ─────────────────────────────────────────────────────────────────────────────
# COEFFICIENT PLOT — headline outcomes (MDCH flag, mdch_any), Adjusted spec
# ─────────────────────────────────────────────────────────────────────────────
if (nrow(results_df) > 0) {
  plot_df <- results_df |>
    filter(spec == "Adjusted (CASE)",
           outcome %in% c("Official MDCH flag", "Any deprivation (mdch_any)")) |>
    mutate(pseudo_treated = factor(pseudo_treated, levels = c("NI", "Wales")))

  p <- ggplot(plot_df, aes(x = coef, y = pseudo_treated, xmin = ci_lo, xmax = ci_hi)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_errorbarh(height = 0.2, colour = "#d7191c") +
    geom_point(size = 3, colour = "#d7191c") +
    facet_wrap(~outcome, ncol = 1, scales = "free_x") +
    labs(title = "Falsification Check: Wales/NI vs England at SCP's Actual Rollout Timing",
         subtitle = "Neither received SCP -- expect null (CI crossing zero) | CASE-adjusted | 95% CI | household-clustered SEs",
         x = "DiD coefficient (pseudo-treatment)", y = NULL,
         caption = "Reference: Scotland vs England (the real effect, Section 4) is -0.0759*** (MDCH flag, Adjusted).\nNI's estimate should be read alongside the FYE2024 old/new question-design asymmetry (NI: 100% new design).") +
    theme_minimal(base_size = 11) +
    theme(strip.text = element_text(face = "bold"), plot.title = element_text(face = "bold"))
  ggsave(file.path(FIGURES_DIR, "placebo_wales_ni_coefplot.png"), p, width = 8, height = 6, dpi = 150)
  cat("  ✓ Falsification coefficient plot saved\n")
}

write.csv(results_df, file.path(TABLES_DIR, "placebo_wales_ni.csv"), row.names = FALSE)
cat(sprintf("\n✓ Saved: %s\n", file.path(TABLES_DIR, "placebo_wales_ni.csv")))
cat("\nHow to read this: coefficients near zero and NOT significant for both Wales and NI,\n")
cat("across outcomes, is the reassuring result -- it means nothing resembling SCP's effect\n")
cat("shows up in nations that never received it, at the exact time SCP rolled out in Scotland.\n")
cat("That supports attributing Scotland's own estimate (Section 4) to SCP specifically, not a\n")
cat("UK-wide shock. A significant coefficient here would be a warning sign worth investigating.\n")
