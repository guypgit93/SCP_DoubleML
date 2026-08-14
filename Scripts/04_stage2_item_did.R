# ─────────────────────────────────────────────────────────────────────────────
# 04_stage2_item_did.R
# Stacked item-level DiD: pools the 10 MDCH items into ONE regression
# (long format, item + year FE, clustered at household) instead of 10
# separate ones, so SEs correctly account for within-child correlation
# across items (Moulton problem). Same 2023-cutoff treatment definition as
# 03_stage1_baseline_did.R -- this is a clustering fix, not a timing fix (that's
# 04c_agecohort_staggered_did.R).
#
# Two specs:
#   (a) Pooled:          item_value ~ treated + tp | YEAR_f + item_f
#       one average DiD effect across all 10 items.
#   (b) Item-interacted: item_value ~ treated + tp:item_f | YEAR_f + item_f
#       one DiD coefficient per item in a single joint model, plus a joint
#       Wald test of whether all 10 are zero.
# Both reported Simple (no covariates) and Adjusted (CASE 2025 controls,
# see COVARIATE SET section below -- same six variables as 03).
# ─────────────────────────────────────────────────────────────────────────────

library(data.table)
library(fixest)
library(tidyverse)
library(ggplot2)

# ─────────────────────────────────────────────────────────────────────────────
# CONFIG
# ─────────────────────────────────────────────────────────────────────────────
DATA_PATH       <- "/Users/guypigott/python-venv-demo/Dissertation/data/hbai_clean.csv"
TABLES_DIR      <- "/Users/guypigott/Claude/Projects/MSc Dissertation/tables"
FIGURES_DIR     <- "/Users/guypigott/Claude/Projects/MSc Dissertation/figures"
dir.create(TABLES_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

ALPHA           <- 0.05
SCP_EXPAND_YEAR <- 2023
CLUSTERVAR      <- "SERNUM"   # household -- consistent with 03/04/05 scripts

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

# ─────────────────────────────────────────────────────────────────────────────
# LOAD & PREPARE DATA
# ─────────────────────────────────────────────────────────────────────────────
cat("Loading data...\n")
df <- fread(DATA_PATH)
df[, YEAR    := as.integer(YEAR)]
df[, treated := as.numeric(scotland)]
if ("post" %in% names(df)) {
  cat("  Using `post` already in hbai_clean.csv (may carry 01_hbai_prep.R's exact-interview-date\n")
  cat("  refinement for FY2022/23) rather than recomputing from YEAR.\n")
  df[, post := as.numeric(post)]
} else {
  df[, post := as.numeric(YEAR >= SCP_EXPAND_YEAR)]
}
df[, tp      := treated * post]
df[, YEAR_f  := factor(YEAR)]

for (v in MDCH_ITEMS) {
  df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
  df[[v]] <- ifelse(df[[v]] < 0, NA_real_, df[[v]])
}

cat(sprintf("  %s rows | years: %s\n",
            format(nrow(df), big.mark = ","),
            paste(sort(unique(df$YEAR)), collapse = ", ")))

# ─────────────────────────────────────────────────────────────────────────────
# COVARIATE SET (Adjusted spec) -- CASE (Stewart et al. 2025) paper's actual
# six controls (footnote iii): head aged under 25, female head, ethnicity
# (five categories), disabled household, lone parent, large family (3+ kids).
# Same construction as 03_stage1_baseline_did.R's CASE_COVS, kept identical
# so the composite (03) and item-stacked (04) OLS results are directly
# comparable on covariates, not just on outcome. NOT income/benefit-receipt/
# housing -- see 03's COVARIATES section header for the mediator/bad-control
# rationale for excluding those.
# ─────────────────────────────────────────────────────────────────────────────
df[, young_head          := as.numeric(AGEHDBAND == 1)]
df[, female_head         := as.numeric(SEXHD == 2)]
df[, disabled_household  := as.numeric(DSCORFAM == 2)]
df[, lone_parent         := as.numeric(MARITAL_WITHKID == 1)]
df[, large_family        := as.numeric(NUMBKIDS == 3)]
df[, ETH_f               := factor(ifelse(ETH == 99, NA, ETH))]

CASE_COVS <- c("young_head", "female_head", "ETH_f",
               "disabled_household", "lone_parent", "large_family")
CASE_COVS <- CASE_COVS[CASE_COVS %in% names(df)]

cat(sprintf("  Adjusted-spec covariates (CASE 2025 controls): %s\n", paste(CASE_COVS, collapse = ", ")))

# ─────────────────────────────────────────────────────────────────────────────
# RESHAPE WIDE -> LONG (one row per child-year-item)
# Subset to only the columns actually needed BEFORE reshaping -- reshaping
# with all ~90 pipeline columns as id.vars would needlessly 10x the memory
# footprint for no benefit here.
# ─────────────────────────────────────────────────────────────────────────────
id_cols  <- c("SERNUM", "YEAR", "YEAR_f", "treated", "post", "tp", "GS_INDCH", CASE_COVS)
id_cols  <- id_cols[id_cols %in% names(df)]
keep_cols <- unique(c(id_cols, MDCH_ITEMS))
sub <- df[, ..keep_cols]

long <- melt(sub, id.vars = id_cols, measure.vars = MDCH_ITEMS,
             variable.name = "item", value.name = "item_value")
long <- long[!is.na(item_value)]
long[, item_f := factor(item)]

cat(sprintf("\nLong-format sample: %s child-item-year rows (%s unique children x up to 10 items)\n",
            format(nrow(long), big.mark = ","), format(uniqueN(long$SERNUM), big.mark = ",")))
cat("Rows per item:\n")
print(long[, .N, by = item])

# ─────────────────────────────────────────────────────────────────────────────
# (a) POOLED SPEC — one average DiD effect across all 10 items
# ─────────────────────────────────────────────────────────────────────────────
cat("\n══ Pooled spec: item_value ~ treated + tp | YEAR_f + item_f ═══════════\n")

fit_pooled_simple <- feols(item_value ~ treated + tp | YEAR_f + item_f,
                            data = long, weights = ~GS_INDCH,
                            cluster = ~SERNUM, notes = FALSE)

fml_pooled_adj <- as.formula(paste(
  "item_value ~ treated + tp +", paste(CASE_COVS, collapse = " + "),
  "| YEAR_f + item_f"
))
fit_pooled_adj <- feols(fml_pooled_adj, data = long, weights = ~GS_INDCH,
                         cluster = ~SERNUM, notes = FALSE)

extract_row <- function(fit, coefname, label, spec) {
  ct <- coeftable(fit)
  if (!(coefname %in% rownames(ct))) return(NULL)
  est  <- ct[coefname, "Estimate"]
  se   <- ct[coefname, "Std. Error"]
  pval <- ct[coefname, "Pr(>|t|)"]
  crit <- qnorm(1 - ALPHA / 2)
  data.frame(model = "Pooled", item = NA_character_, label = label, spec = spec,
             coef = est, se = se, pval = pval,
             ci_lo = est - crit * se, ci_hi = est + crit * se,
             r2 = r2(fit, "r2"))
}

pooled_results <- bind_rows(
  extract_row(fit_pooled_simple, "tp", "Pooled (all 10 items)", "Simple"),
  extract_row(fit_pooled_adj,    "tp", "Pooled (all 10 items)", "Adjusted")
)

cat("\n── Pooled results ──────────────────────────────────────────────────────\n")
for (i in seq_len(nrow(pooled_results))) {
  r <- pooled_results[i, ]
  sig <- ifelse(r$pval < .01, "***", ifelse(r$pval < .05, "**", ifelse(r$pval < .1, "*", "")))
  cat(sprintf("  %-14s  coef=%8.4f  se=%8.4f  p=%6.3f  %s\n", r$spec, r$coef, r$se, r$pval, sig))
}

# ─────────────────────────────────────────────────────────────────────────────
# (b) ITEM-INTERACTED SPEC — one DiD coefficient PER item, one joint model
# ─────────────────────────────────────────────────────────────────────────────
cat("\n══ Item-interacted spec: item_value ~ treated + tp:item_f | YEAR_f + item_f ══\n")

fit_interact_simple <- feols(item_value ~ treated + tp:item_f | YEAR_f + item_f,
                              data = long, weights = ~GS_INDCH,
                              cluster = ~SERNUM, notes = FALSE)

fml_interact_adj <- as.formula(paste(
  "item_value ~ treated + tp:item_f +", paste(CASE_COVS, collapse = " + "),
  "| YEAR_f + item_f"
))
fit_interact_adj <- feols(fml_interact_adj, data = long, weights = ~GS_INDCH,
                           cluster = ~SERNUM, notes = FALSE)

extract_item_rows <- function(fit, spec) {
  ct <- coeftable(fit)
  rn <- rownames(ct)
  item_rows <- grep("^tp:item_f", rn, value = TRUE)
  if (length(item_rows) == 0) {
    cat(sprintf("  ⚠ (%s): no 'tp:item_f...' coefficients found -- check coeftable rownames:\n", spec))
    print(rn)
    return(NULL)
  }
  crit  <- qnorm(1 - ALPHA / 2)
  fit_r2 <- r2(fit, "r2")  # one value for the whole joint model -- same for every item row in this spec
  out <- lapply(item_rows, function(rname) {
    item_code <- sub("^tp:item_f", "", rname)
    est  <- ct[rname, "Estimate"]
    se   <- ct[rname, "Std. Error"]
    pval <- ct[rname, "Pr(>|t|)"]
    data.frame(model = "Item-interacted", item = item_code,
               label = unname(MDCH_LABELS[item_code]), spec = spec,
               coef = est, se = se, pval = pval,
               ci_lo = est - crit * se, ci_hi = est + crit * se,
               r2 = fit_r2)
  })
  bind_rows(out)
}

interact_simple_rows <- extract_item_rows(fit_interact_simple, "Simple")
interact_adj_rows    <- extract_item_rows(fit_interact_adj,    "Adjusted")
interact_results <- bind_rows(interact_simple_rows, interact_adj_rows)

if (nrow(interact_results) > 0) {
  cat("\n── Item-interacted results ─────────────────────────────────────────────\n")
  for (i in seq_len(nrow(interact_results))) {
    r <- interact_results[i, ]
    sig <- ifelse(r$pval < .01, "***", ifelse(r$pval < .05, "**", ifelse(r$pval < .1, "*", "")))
    cat(sprintf("  %-25s  %-10s  coef=%8.4f  se=%8.4f  p=%6.3f  %s\n",
                r$label, r$spec, r$coef, r$se, r$pval, sig))
  }
}

# Joint Wald test: are all 10 item-specific tp effects jointly zero?
cat("\n── Joint Wald test: are all 10 item-specific DiD effects jointly zero? ──\n")
wald_simple <- tryCatch(wald(fit_interact_simple, "tp:item_f"),
                         error = function(e) { cat(sprintf("  ✗ Simple: wald() error — %s\n", e$message)); NULL })
wald_adj    <- tryCatch(wald(fit_interact_adj, "tp:item_f"),
                         error = function(e) { cat(sprintf("  ✗ Adjusted: wald() error — %s\n", e$message)); NULL })
if (!is.null(wald_simple)) { cat("Simple spec:\n");   print(wald_simple) }
if (!is.null(wald_adj))    { cat("Adjusted spec:\n"); print(wald_adj) }

# ─────────────────────────────────────────────────────────────────────────────
# COEFFICIENT PLOT — item-interacted (Simple spec), stacked model
# ─────────────────────────────────────────────────────────────────────────────
if (nrow(interact_results) > 0 && any(interact_results$spec == "Simple")) {
  plot_df <- interact_results |>
    filter(spec == "Simple") |>
    mutate(label = factor(label, levels = rev(label)))

  p_stacked <- ggplot(plot_df, aes(x = coef, y = label, xmin = ci_lo, xmax = ci_hi)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_errorbarh(height = 0.3, colour = "#2c7bb6") +
    geom_point(size = 3, colour = "#2c7bb6") +
    labs(
      title    = "Stacked Item-Level DiD: One Joint Model, Household-Clustered",
      subtitle = "item_value ~ treated + tp:item_f | YEAR_f + item_f | Simple spec | 95% CI",
      x = "DiD coefficient (percentage points)", y = NULL,
      caption = "Positive = more deprivation; Negative = SCP reduced deprivation. Cluster: SERNUM (household)."
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))

  ggsave(file.path(FIGURES_DIR, "stacked_item_coefplot.png"),
         p_stacked, width = 9, height = 6, dpi = 150)
  cat("  ✓ Stacked item coefficient plot saved\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# SAVE RESULTS
# ─────────────────────────────────────────────────────────────────────────────
all_results <- bind_rows(pooled_results, interact_results)
write.csv(all_results, file.path(TABLES_DIR, "stacked_item_did.csv"), row.names = FALSE)

cat("\n✓ Stacked item-level DiD outputs saved to:\n")
cat(sprintf("  Table:  %s\n", file.path(TABLES_DIR, "stacked_item_did.csv")))
cat(sprintf("  Figure: %s\n", file.path(FIGURES_DIR, "stacked_item_coefplot.png")))
cat("\nCompare pooled coefficient here against Stage 1's mdch_any and mdch_severe\n")
cat("(03_stage1_baseline_did.R), and compare item-interacted coefficients here\n")
cat("against Stage 3's 10-separate-regression DR-DiD item table (04_dml_did.R)\n")
cat("as a triangulation check.\n")
