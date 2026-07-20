# ─────────────────────────────────────────────────────────────────────────────
# 05_parallel_trends.R
# Parallel trends diagnostics, replicating CASE paper Table A2 (parallel
# covariate trends: Scot x Year interactions per household characteristic,
# no controls) and Table A3 (parallel outcome trends / event study: Scot x
# Year interactions for MDCH, its components, and food insecurity).
#
# Runs both tables at annual (FYE) granularity throughout, not the paper's
# ~13-week quarters -- HBAI's harmonised extract doesn't carry interview
# date for most years (see 01_hbai_prep.R).
#
# Table A2 covariate definitions match 02_summary_stats.R (confirmed against
# CASE Table 2 / A1), not the archived version which used different raw
# variables that turned out degenerate for this data pull.
#
# Outputs
#   tables/  table_A2_covariate_trends.{csv,tex}, table_A2_pretrend_wald.csv
#            table_A3_composite.csv, table_A3_composite_{un,}adjusted.tex
#            table_A3_items.csv, table_A3_pretrend_wald.csv
#   figures/ pt_covtrend_<covariate>.png (x6), pt_covtrend_grid.png
#            pt_es_<outcome>.png (composite x5, items x10)
#            pt_es_composite_grid.png, pt_es_items_grid.png
#
# Run after 01_hbai_prep.R has produced hbai_lca.csv. Loads its own copy of
# the data, so it can be run independently of 03/04.
# ─────────────────────────────────────────────────────────────────────────────

library(fixest)
library(tidyverse)
library(modelsummary)
library(data.table)
library(patchwork)

setFixest_dict(c(treated = "Scotland"))

# ─────────────────────────────────────────────────────────────────────────────
# CONFIG
# ─────────────────────────────────────────────────────────────────────────────
DATA_PATH   <- "/Users/guypigott/python-venv-demo/Dissertation/data/hbai_lca.csv"
FIGURES_DIR <- "/Users/guypigott/Claude/Projects/MSc Dissertation/figures"
TABLES_DIR  <- "/Users/guypigott/Claude/Projects/MSc Dissertation/tables"

dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TABLES_DIR,  showWarnings = FALSE, recursive = TRUE)

SCP_EXPAND_YEAR <- 2023   # FY 2022/23: SCP expanded to all under-16s, £25/week
ALPHA           <- 0.05

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

# ─────────────────────────────────────────────────────────────────────────────
# LOAD DATA
# ─────────────────────────────────────────────────────────────────────────────
cat("Loading data...\n")
df <- fread(DATA_PATH)
cat(sprintf("  Raw: %s rows, years: %s\n", format(nrow(df), big.mark = ","),
            paste(sort(unique(df$YEAR)), collapse = ", ")))

df[, YEAR      := as.integer(YEAR)]
df[, treated   := as.numeric(scotland)]
df[, YEAR_f    := factor(YEAR)]
df[, GVTREGN_f := factor(GVTREGN)]

REF_YEAR <- min(df$YEAR, na.rm = TRUE)   # earliest observed FYE (matches paper's ref category)
cat(sprintf("  Reference year for all trend regressions: FYE %s\n", REF_YEAR))

# Numeric coercion + sentinel cleanup for outcomes
df[, MDCH := suppressWarnings(as.numeric(MDCH))]
df[, MDCH := ifelse(MDCH < 0, NA_real_, MDCH)]
for (v in c(MDCH_ITEMS, "mdch_any", "mdch_count", "mdch_severe",
            "food_insecure", "very_low_food_sec")) {
  if (v %in% names(df)) df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
}

# ── Subsamples (FYE 2024 kept everywhere -- same MDCH definition as earlier years) ──
df_a2        <- df[!is.na(MDCH) & !is.na(GS_INDCH) & GS_INDCH > 0]
df_mdch      <- df[mdch_observed == 1 & !is.na(GS_INDCH) & GS_INDCH > 0]
df_mdch_flag <- df[!is.na(MDCH)      & !is.na(GS_INDCH) & GS_INDCH > 0]
df_food      <- df[!is.na(food_insecure) & !is.na(GS_INDCH) & GS_INDCH > 0]

cat(sprintf("  Table A2 sample:        %s rows (years %s-%s)\n",
            format(nrow(df_a2), big.mark = ","), min(df_a2$YEAR), max(df_a2$YEAR)))
cat(sprintf("  MDCH items subsample:   %s rows\n", format(nrow(df_mdch), big.mark = ",")))
cat(sprintf("  MDCH official flag:     %s rows\n", format(nrow(df_mdch_flag), big.mark = ",")))
cat(sprintf("  Food insecurity sample: %s rows\n", format(nrow(df_food), big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# MDCH QUESTION-REVISION CHECK: DWP split FYE2024 between old/revised MDCH
# questions. Checks this split isn't differential across countries (an
# assumption the CASE paper relies on). "Got old questions" proxy = ANY of
# the 10 old-style items non-missing (a single-item check badly undercounts).
# ─────────────────────────────────────────────────────────────────────────────
cat("\n== MDCH question-revision check (FYE2024 old vs. new questions) ============\n")

d24 <- df_mdch[YEAR == max(df$YEAR, na.rm = TRUE)]
d24[, got_old_questions := as.numeric(rowSums(!is.na(d24[, ..MDCH_ITEMS])) > 0)]

revision_tab   <- table(scotland = d24$treated, old_questions = d24$got_old_questions)
revision_props <- prop.table(revision_tab, 1)
revision_test  <- chisq.test(revision_tab)

cat("  Counts (rows = country, cols = 0/1 got old questions):\n")
print(revision_tab)
cat("  Row proportions (share of each country given old vs. new questions):\n")
print(round(revision_props, 4))
cat(sprintf("  Chi-square test of independence: X2 = %.3f, df = %d, p = %.4f\n",
            unname(revision_test$statistic), unname(revision_test$parameter), revision_test$p.value))
cat(sprintf("  -> Old-questions rate: England %.1f%%, Scotland %.1f%% -- %s\n",
            100 * revision_props["0", "1"], 100 * revision_props["1", "1"],
            ifelse(revision_test$p.value < ALPHA,
                   "SIGNIFICANTLY DIFFERENT: revision may be differential -- flag as a threat to identification.",
                   "not significantly different -- supports the CASE paper's assumption that the revision isn't differential across countries.")))

revision_summary <- tibble(
  country       = c("England", "Scotland"),
  n_total       = as.integer(rowSums(revision_tab)),
  n_old_qs      = as.integer(revision_tab[, "1"]),
  pct_old_qs    = round(100 * revision_props[, "1"], 2),
  chisq_p_value = revision_test$p.value
)
write_csv(revision_summary, file.path(TABLES_DIR, "table_mdch_revision_check.csv"))
cat("  wrote table_mdch_revision_check.csv\n")

avail_items <- MDCH_ITEMS[MDCH_ITEMS %in% names(df_mdch)]

# ─────────────────────────────────────────────────────────────────────────────
# TABLE A2 COVARIATES -- definitions match 02_summary_stats.R (confirmed
# against CASE Table 2 / A1):
#   NUMBKIDS         == 3  -> larger family (3+ children)
#   MARITAL_WITHKID  == 1  -> lone parent
#   AGEHDBAND        == 1  -> young head (16-24)
#   SEXHD            == 2  -> female head
#   DSCORFAM         == 2  -> disabled household
#   ETH              == 1  -> white household (99 = "not declared" -> NA first)
# ─────────────────────────────────────────────────────────────────────────────
df[, ETH := ifelse(ETH == 99, NA_real_, ETH)]

df[, larger_fam  := as.numeric(NUMBKIDS == 3)]
df[, lone_parent := as.numeric(MARITAL_WITHKID == 1)]
df[, young_head  := as.numeric(AGEHDBAND == 1)]
df[, female_head := as.numeric(SEXHD == 2)]
df[, disabled_hh := as.numeric(DSCORFAM == 2)]
df[, white_hh    := as.numeric(ETH == 1)]

# Re-run df_a2 now that the covariates exist on df
df_a2 <- df[!is.na(MDCH) & !is.na(GS_INDCH) & GS_INDCH > 0]

cat("\n--- Table A2 covariate diagnostics (sanity-check before trusting results) ---\n")
for (v in c("larger_fam", "lone_parent", "young_head", "female_head", "disabled_hh", "white_hh")) {
  cat(sprintf("  %-14s  mean = %.3f  (n non-missing = %s)\n", v,
              mean(df_a2[[v]], na.rm = TRUE), format(sum(!is.na(df_a2[[v]])), big.mark = ",")))
}
# Degenerate check: mean exactly 0 or 1 would signal a miscoded variable
cat(sprintf("\n%s\n", if (mean(df_a2$larger_fam, na.rm=TRUE) %in% c(0,1) ||
                          mean(df_a2$lone_parent, na.rm=TRUE) %in% c(0,1) ||
                          mean(df_a2$young_head,  na.rm=TRUE) %in% c(0,1) ||
                          mean(df_a2$female_head, na.rm=TRUE) %in% c(0,1) ||
                          mean(df_a2$disabled_hh, na.rm=TRUE) %in% c(0,1) ||
                          mean(df_a2$white_hh,    na.rm=TRUE) %in% c(0,1))
    "WARNING: at least one covariate above is degenerate -- check 02_summary_stats.R's raw value counts."
    else "All six covariates look non-degenerate."))

TABLE_A2_VARS <- c(
  larger_fam  = "Larger families",
  lone_parent = "Lone parent households",
  young_head  = "Young head of household",
  female_head = "Female head of household",
  disabled_hh = "Disabled household",
  white_hh    = "White households"
)

# Adjusted-column controls for Table A3 (same parsimonious set as 03_did_replication.R)
DEMO_COVS    <- c("AGE", "SEX", "ADULTH", "NUMBKIDS")
INCOME_COVS  <- c("S_OE_BHC", "BENBU_UC_OR_EQUIV", "BENBU_CTC", "BENBU_IS", "BENBU_DLA")
HOUSING_COVS <- c("TENHBAI")
OLS_COVS     <- c(DEMO_COVS, INCOME_COVS, HOUSING_COVS)
avail_covs   <- OLS_COVS[OLS_COVS %in% names(df)]

# ─────────────────────────────────────────────────────────────────────────────
# HELPERS
# outcome ~ treated + i(YEAR_f, treated, ref) [+ covs] | YEAR_f
# ─────────────────────────────────────────────────────────────────────────────
run_trend_model <- function(data, outcome, covs = NULL, ref = REF_YEAR, weight = "GS_INDCH") {
  base <- paste0(outcome, " ~ treated + i(YEAR_f, treated, ref = '", ref, "')")
  if (!is.null(covs) && length(covs) > 0) base <- paste(base, "+", paste(covs, collapse = " + "))
  fml  <- as.formula(paste(base, "| YEAR_f"))
  # Clustered at household (SERNUM), not region -- region clustering is
  # anti-conservative here (see 05b_pretrend_diagnostics.R)
  tryCatch(
    feols(fml, data = data, weights = as.formula(paste0("~", weight)),
          cluster = ~SERNUM, notes = FALSE),
    error = function(e) { message("  ! model failed for ", outcome, ": ", e$message); NULL }
  )
}

tidy_trend <- function(fit) {
  if (is.null(fit)) return(NULL)
  broom::tidy(fit, conf.int = TRUE) |>
    filter(str_detect(term, "^YEAR_f::")) |>
    mutate(year = as.integer(str_extract(term, "[0-9]{4}")),
           post = year >= SCP_EXPAND_YEAR)
}

pretrend_wald <- function(fit, all_years, ref = REF_YEAR) {
  if (is.null(fit)) return(NA_real_)
  pre_years <- setdiff(as.character(sort(all_years[all_years < SCP_EXPAND_YEAR])), as.character(ref))
  pre_terms <- paste0("YEAR_f::", pre_years, ":treated")
  pre_terms <- pre_terms[pre_terms %in% names(coef(fit))]
  if (length(pre_terms) < 2) return(NA_real_)
  wt <- tryCatch(wald(fit, keep = pre_terms), error = function(e) NULL)
  if (is.null(wt)) return(NA_real_)
  wt$p
}

fmt_cell <- function(est, se, p) {
  # Vectorised -- est/se/p are whole columns when called from mutate()
  stars <- dplyr::case_when(p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ "")
  out <- sprintf("%.3f%s\n(%.3f)", est, stars, se)
  ifelse(is.na(est), "-", out)
}

plot_trend <- function(td, title, ylab, ref = REF_YEAR) {
  ggplot(td, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high,
                 colour = post, fill = post)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_vline(xintercept = SCP_EXPAND_YEAR - 0.5, linetype = "dashed",
               colour = "firebrick", linewidth = 0.6) +
    geom_ribbon(alpha = .15, colour = NA) +
    geom_point(size = 2.3) +
    geom_errorbar(width = 0.2) +
    annotate("point", x = ref, y = 0, size = 3, shape = 1, colour = "grey40") +
    scale_colour_manual(values = c("FALSE" = "#2c7bb6", "TRUE" = "#d7191c"),
                        labels = c("Pre-SCP", "Post-SCP"), name = "") +
    scale_fill_manual(  values = c("FALSE" = "#2c7bb6", "TRUE" = "#d7191c"),
                        labels = c("Pre-SCP", "Post-SCP"), name = "") +
    scale_x_continuous(breaks = sort(unique(td$year))) +
    labs(title = title, subtitle = paste0("Reference year: FYE ", ref, " | Annual FYE dummies (no interview-date field for quarterly)"),
         x = "Financial year ending", y = ylab,
         caption = "SEs clustered at household (SERNUM). Weights: GS_INDCH.") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", size = 10))
}

plot_outcome_trend <- function(td_unadj, td_adj, title, ylab, ref = REF_YEAR) {
  du <- if (!is.null(td_unadj)) td_unadj |> mutate(spec = "Unadjusted") else NULL
  da <- if (!is.null(td_adj))   td_adj   |> mutate(spec = "Adjusted")   else NULL
  td <- bind_rows(du, da)
  if (nrow(td) == 0) return(NULL)
  ggplot(td, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high,
                 colour = spec, shape = spec)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_vline(xintercept = SCP_EXPAND_YEAR - 0.5, linetype = "dashed",
               colour = "firebrick", linewidth = 0.6) +
    geom_point(position = position_dodge(width = 0.3), size = 2.3) +
    geom_errorbar(position = position_dodge(width = 0.3), width = 0.2) +
    annotate("point", x = ref, y = 0, size = 3, shape = 1, colour = "grey40") +
    scale_colour_manual(values = c("Unadjusted" = "#2c7bb6", "Adjusted" = "#e66101"), name = "") +
    scale_shape_manual(values  = c("Unadjusted" = 16, "Adjusted" = 17), name = "") +
    scale_x_continuous(breaks = sort(unique(td$year))) +
    labs(title = title,
         subtitle = paste0("Reference year: FYE ", ref, " | Dashed red = SCP expansion (Nov 2022)"),
         x = "Financial year ending", y = ylab,
         caption = "SEs clustered at household (SERNUM). Weights: GS_INDCH.") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", size = 10))
}

# ═════════════════════════════════════════════════════════════════════════════
# TABLE A2: PARALLEL COVARIATE TRENDS TEST
# ═════════════════════════════════════════════════════════════════════════════
cat("\n== TABLE A2: Parallel covariate trends test ================================\n")

a2_fits <- lapply(names(TABLE_A2_VARS), function(v) run_trend_model(df_a2, v))
names(a2_fits) <- names(TABLE_A2_VARS)
a2_fits_ok <- Filter(Negate(is.null), a2_fits)

if (length(a2_fits_ok) == 0) {
  cat("  ! ALL Table A2 models failed to fit -- skipping Table A2 outputs for this run.\n")
}

a2_tidy <- imap_dfr(a2_fits, function(fit, v) {
  td <- tidy_trend(fit)
  if (is.null(td)) return(NULL)
  td$covariate <- TABLE_A2_VARS[[v]]
  td
})

# Wide CSV: rows = Scot x Year, columns = covariates -- mirrors the paper's layout
if (nrow(a2_tidy) > 0) {
  a2_wide <- a2_tidy |>
    mutate(cell = fmt_cell(estimate, std.error, p.value)) |>
    select(year, covariate, cell) |>
    pivot_wider(names_from = covariate, values_from = cell) |>
    arrange(year)
  write_csv(a2_wide, file.path(TABLES_DIR, "table_A2_covariate_trends.csv"))
  cat("  wrote table_A2_covariate_trends.csv\n")
}

if (length(a2_fits_ok) > 0) {
  years_nonref <- setdiff(sort(unique(df_a2$YEAR)), REF_YEAR)
  year_coef_map_a2 <- setNames(paste0("YEAR_f::", years_nonref, ":treated"),
                                paste0("Scot x FYE ", years_nonref))
  # LaTeX table is presentation-only (real numbers already in the .csv above),
  # so a coef_map mismatch here shouldn't crash the rest of the script.
  tryCatch({
    modelsummary(
      a2_fits_ok,
      stars    = c("*" = .1, "**" = .05, "***" = .01),
      coef_map = year_coef_map_a2,
      gof_map  = c("nobs", "r.squared"),
      title    = "Table A2: Parallel covariate trends test (annual FYE dummies)",
      output   = file.path(TABLES_DIR, "table_A2_covariate_trends.tex")
    )
    cat("  wrote table_A2_covariate_trends.tex\n")
  }, error = function(e) {
    cat("  ! table_A2_covariate_trends.tex failed -- numbers still in the .csv. Error: ",
        conditionMessage(e), "\n")
  })
}

a2_wald <- imap_dfr(a2_fits, function(fit, v) {
  tibble(covariate = TABLE_A2_VARS[[v]], wald_p = pretrend_wald(fit, unique(df_a2$YEAR)))
}) |>
  mutate(flag = if_else(!is.na(wald_p) & wald_p < ALPHA, "possible pre-trend", "OK"))
write_csv(a2_wald, file.path(TABLES_DIR, "table_A2_pretrend_wald.csv"))
cat("  wrote table_A2_pretrend_wald.csv\n")

# Figures: one per covariate + a combined grid
for (v in names(TABLE_A2_VARS)) {
  td <- tidy_trend(a2_fits[[v]])
  if (is.null(td)) next
  p <- plot_trend(td, paste0("Parallel trends: ", TABLE_A2_VARS[[v]]), "Scot x Year coefficient")
  ggsave(file.path(FIGURES_DIR, paste0("pt_covtrend_", v, ".png")), p, width = 7, height = 4.5, dpi = 150)
  cat(sprintf("  wrote pt_covtrend_%s.png\n", v))
}

if (nrow(a2_tidy) > 0) {
  p_a2_grid <- ggplot(a2_tidy, aes(year, estimate, ymin = conf.low, ymax = conf.high, colour = post)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_vline(xintercept = SCP_EXPAND_YEAR - 0.5, linetype = "dashed", colour = "firebrick", alpha = .7) +
    geom_point(size = 1.6) + geom_errorbar(width = .3) +
    scale_colour_manual(values = c("FALSE" = "#2c7bb6", "TRUE" = "#d7191c"), guide = "none") +
    facet_wrap(~covariate, ncol = 3, scales = "free_y") +
    labs(title = "Table A2 replication: parallel covariate trends",
         x = "Financial year ending", y = "Scot x Year coefficient") +
    theme_minimal(base_size = 9) + theme(strip.text = element_text(face = "bold"))
  ggsave(file.path(FIGURES_DIR, "pt_covtrend_grid.png"), p_a2_grid, width = 11, height = 7, dpi = 150)
  cat("  wrote pt_covtrend_grid.png\n")
}

# ═════════════════════════════════════════════════════════════════════════════
# TABLE A3: PARALLEL OUTCOME TRENDS TEST (MDCH + components + food insecurity)
# ═════════════════════════════════════════════════════════════════════════════
cat("\n== TABLE A3: Parallel outcome trends test (event study) ====================\n")

outcome_specs <- list(
  list(key = "mdch_any",      label = "MDCH: Any deprivation",   data = df_mdch,      y = "mdch_any"),
  list(key = "mdch_count",    label = "MDCH: Count of items",    data = df_mdch,      y = "mdch_count"),
  list(key = "mdch_severe",   label = "MDCH: Severe (3+ items)", data = df_mdch,      y = "mdch_severe"),
  list(key = "MDCH_flag",     label = "MDCH: Official DWP flag", data = df_mdch_flag, y = "MDCH"),
  list(key = "food_insecure", label = "Food insecurity",         data = df_food,      y = "food_insecure")
)
for (item in avail_items) {
  outcome_specs[[length(outcome_specs) + 1]] <- list(
    key = item, label = unname(MDCH_LABELS[item]), data = df_mdch, y = item
  )
}
composite_keys <- c("mdch_any", "mdch_count", "mdch_severe", "MDCH_flag", "food_insecure")

a3_results <- map(outcome_specs, function(spec) {
  # Reference year must exist in this outcome's own subsample (food_insecure
  # starts later than the global REF_YEAR)
  spec_ref <- min(spec$data$YEAR, na.rm = TRUE)
  cat(sprintf("  fitting: %s (ref FYE %s)\n", spec$label, spec_ref))
  fit_unadj <- run_trend_model(spec$data, spec$y, covs = NULL,        ref = spec_ref)
  fit_adj   <- run_trend_model(spec$data, spec$y, covs = avail_covs,  ref = spec_ref)
  list(
    key = spec$key, label = spec$label, ref = spec_ref,
    unadj = fit_unadj, adj = fit_adj,
    td_unadj = tidy_trend(fit_unadj), td_adj = tidy_trend(fit_adj),
    wald_unadj = pretrend_wald(fit_unadj, unique(spec$data$YEAR), ref = spec_ref),
    wald_adj   = pretrend_wald(fit_adj,   unique(spec$data$YEAR), ref = spec_ref)
  )
})
names(a3_results) <- map_chr(outcome_specs, "key")

# ── Pre-trend Wald summary (BH-corrected across all outcomes+items) ─────────
a3_wald <- map_dfr(a3_results, function(r) {
  tibble(outcome = r$label, wald_p_unadj = r$wald_unadj, wald_p_adj = r$wald_adj)
}) |>
  mutate(
    wald_p_unadj_bh = p.adjust(wald_p_unadj, method = "BH"),
    wald_p_adj_bh   = p.adjust(wald_p_adj,   method = "BH"),
    flag_unadj = if_else(!is.na(wald_p_unadj_bh) & wald_p_unadj_bh < ALPHA, "possible pre-trend", "OK"),
    flag_adj   = if_else(!is.na(wald_p_adj_bh)   & wald_p_adj_bh   < ALPHA, "possible pre-trend", "OK")
  )
write_csv(a3_wald, file.path(TABLES_DIR, "table_A3_pretrend_wald.csv"))
cat("  wrote table_A3_pretrend_wald.csv\n")

# ── Wide CSVs (paper-style: unadjusted + adjusted columns per outcome) ──────
build_wide_table <- function(keys) {
  long_df <- map_dfr(keys, function(k) {
    r <- a3_results[[k]]
    tdu <- r$td_unadj; tda <- r$td_adj
    all_years <- sort(unique(c(tdu$year, tda$year)))
    map_dfr(all_years, function(yr) {
      u <- if (!is.null(tdu)) tdu[tdu$year == yr, ] else tdu
      a <- if (!is.null(tda)) tda[tda$year == yr, ] else tda
      tibble(
        year       = yr,
        outcome    = r$label,
        unadjusted = if (!is.null(u) && nrow(u) == 1) fmt_cell(u$estimate, u$std.error, u$p.value) else "-",
        adjusted   = if (!is.null(a) && nrow(a) == 1) fmt_cell(a$estimate, a$std.error, a$p.value) else "-"
      )
    })
  })
  if (nrow(long_df) == 0) {
    cat("  ! No models fit for this key set -- skipping wide table.\n")
    return(tibble())
  }
  long_df |>
    pivot_wider(id_cols = year, names_from = outcome, values_from = c(unadjusted, adjusted)) |>
    arrange(year)
}

a3_composite_wide <- build_wide_table(composite_keys)
if (nrow(a3_composite_wide) > 0) {
  write_csv(a3_composite_wide, file.path(TABLES_DIR, "table_A3_composite.csv"))
  cat("  wrote table_A3_composite.csv\n")
}

a3_items_wide <- build_wide_table(avail_items)
if (nrow(a3_items_wide) > 0) {
  write_csv(a3_items_wide, file.path(TABLES_DIR, "table_A3_items.csv"))
  cat("  wrote table_A3_items.csv\n")
}

# ── Publication tex tables for the 5 composite outcomes ─────────────────────
composite_labels <- vapply(a3_results[composite_keys], `[[`, character(1), "label")

unadj_list  <- map(a3_results[composite_keys], "unadj")
unadj_ok    <- !vapply(unadj_list, is.null, logical(1))
composite_unadj_fits <- unadj_list[unadj_ok]
names(composite_unadj_fits) <- composite_labels[unadj_ok]

adj_list    <- map(a3_results[composite_keys], "adj")
adj_ok      <- !vapply(adj_list, is.null, logical(1))
composite_adj_fits <- adj_list[adj_ok]
names(composite_adj_fits) <- composite_labels[adj_ok]

years_nonref_a3 <- setdiff(sort(unique(df$YEAR)), REF_YEAR)
year_coef_map_a3 <- setNames(paste0("YEAR_f::", years_nonref_a3, ":treated"),
                              paste0("Scot x FYE ", years_nonref_a3))

if (length(composite_unadj_fits) > 0) {
  tryCatch({
    modelsummary(
      composite_unadj_fits,
      stars    = c("*" = .1, "**" = .05, "***" = .01),
      coef_map = year_coef_map_a3,
      gof_map  = c("nobs", "r.squared"),
      title    = "Table A3 (unadjusted): parallel outcome trends test",
      output   = file.path(TABLES_DIR, "table_A3_composite_unadjusted.tex")
    )
    cat("  wrote table_A3_composite_unadjusted.tex\n")
  }, error = function(e) {
    cat("  ! table_A3_composite_unadjusted.tex failed -- numbers still in the .csv. Error: ",
        conditionMessage(e), "\n")
  })
}
if (length(composite_adj_fits) > 0) {
  tryCatch({
    modelsummary(
      composite_adj_fits,
      stars    = c("*" = .1, "**" = .05, "***" = .01),
      coef_map = year_coef_map_a3,
      gof_map  = c("nobs", "r.squared"),
      title    = "Table A3 (adjusted): parallel outcome trends test",
      output   = file.path(TABLES_DIR, "table_A3_composite_adjusted.tex")
    )
    cat("  wrote table_A3_composite_adjusted.tex\n")
  }, error = function(e) {
    cat("  ! table_A3_composite_adjusted.tex failed -- numbers still in the .csv. Error: ",
        conditionMessage(e), "\n")
  })
}

# ── Figures: one per outcome (unadjusted + adjusted overlaid) ───────────────
composite_grid_rows <- list()
items_grid_rows     <- list()

for (r in a3_results) {
  p <- plot_outcome_trend(r$td_unadj, r$td_adj, paste0("Parallel trends: ", r$label),
                          "DiD-style coefficient (Scot x Year)")
  if (!is.null(p)) {
    ggsave(file.path(FIGURES_DIR, paste0("pt_es_", r$key, ".png")), p, width = 8, height = 5, dpi = 150)
    cat(sprintf("  wrote pt_es_%s.png\n", r$key))
  }
  if (r$key %in% composite_keys) {
    if (!is.null(r$td_unadj)) composite_grid_rows[[r$key]] <- r$td_unadj |> mutate(outcome = r$label)
  } else {
    if (!is.null(r$td_unadj)) items_grid_rows[[r$key]] <- r$td_unadj |> mutate(outcome = r$label)
  }
}

if (length(composite_grid_rows) > 0) {
  gdat <- bind_rows(composite_grid_rows)
  p_grid <- ggplot(gdat, aes(year, estimate, ymin = conf.low, ymax = conf.high, colour = post)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_vline(xintercept = SCP_EXPAND_YEAR - 0.5, linetype = "dashed", colour = "firebrick", alpha = .7) +
    geom_point(size = 1.8) + geom_errorbar(width = .3) +
    scale_colour_manual(values = c("FALSE" = "#2c7bb6", "TRUE" = "#d7191c"), guide = "none") +
    facet_wrap(~outcome, ncol = 3, scales = "free_y") +
    labs(title = "Table A3 replication: composite MDCH & food insecurity trends (unadjusted)",
         x = "Financial year ending", y = "Scot x Year coefficient") +
    theme_minimal(base_size = 9) + theme(strip.text = element_text(face = "bold"))
  ggsave(file.path(FIGURES_DIR, "pt_es_composite_grid.png"), p_grid, width = 11, height = 7, dpi = 150)
  cat("  wrote pt_es_composite_grid.png\n")
}

if (length(items_grid_rows) > 0) {
  gdat <- bind_rows(items_grid_rows)
  p_grid <- ggplot(gdat, aes(year, estimate, ymin = conf.low, ymax = conf.high, colour = post)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_vline(xintercept = SCP_EXPAND_YEAR - 0.5, linetype = "dashed", colour = "firebrick", alpha = .7) +
    geom_point(size = 1.6) + geom_errorbar(width = .3) +
    scale_colour_manual(values = c("FALSE" = "#2c7bb6", "TRUE" = "#d7191c"), guide = "none") +
    facet_wrap(~outcome, ncol = 3, scales = "free_y") +
    labs(title = "Table A3 replication: individual MDCH item trends (unadjusted)",
         x = "Financial year ending", y = "Scot x Year coefficient") +
    theme_minimal(base_size = 9) + theme(strip.text = element_text(face = "bold"))
  ggsave(file.path(FIGURES_DIR, "pt_es_items_grid.png"), p_grid, width = 12, height = 10, dpi = 150)
  cat("  wrote pt_es_items_grid.png\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# SUMMARY
# ─────────────────────────────────────────────────────────────────────────────
cat("\n== SUMMARY: pre-trend Wald tests ============================================\n")
cat("\nTable A2 (covariates, joint test of all pre-SCP Scot x Year terms):\n")
for (i in seq_len(nrow(a2_wald))) {
  r <- a2_wald[i, ]
  cat(sprintf("  %-28s  Wald p = %s  [%s]\n", r$covariate,
              ifelse(is.na(r$wald_p), "NA", sprintf("%.3f", r$wald_p)), r$flag))
}

cat("\nTable A3 (outcomes + items, BH-corrected across all rows):\n")
cat(sprintf("  %-28s  %10s  %10s\n", "Outcome", "unadj (BH)", "adj (BH)"))
for (i in seq_len(nrow(a3_wald))) {
  r <- a3_wald[i, ]
  cat(sprintf("  %-28s  p=%s [%s]   p=%s [%s]\n", r$outcome,
              ifelse(is.na(r$wald_p_unadj_bh), "NA", sprintf("%.3f", r$wald_p_unadj_bh)), r$flag_unadj,
              ifelse(is.na(r$wald_p_adj_bh),   "NA", sprintf("%.3f", r$wald_p_adj_bh)),   r$flag_adj))
}

cat("\n✓ All outputs saved to:\n")
cat(sprintf("  Tables:  %s\n", TABLES_DIR))
cat(sprintf("  Figures: %s\n", FIGURES_DIR))
