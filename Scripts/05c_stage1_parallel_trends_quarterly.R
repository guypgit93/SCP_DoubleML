# =============================================================================
# 05c_stage1_parallel_trends_quarterly.R
#
# Replicates CASE (Andersen, Nesom, Patrick, Pinter, Stewart & Tominey 2025,
# CASE paper 238) Table A3 EXACTLY at quarterly granularity: event-study
# models with Scot x quarter dummies, using the paper's own rolling 13-week
# windows (anchored 14 Feb / 14 May / 14 Aug / 14 Nov each year), instead of
# 05_stage1_parallel_trends.R's annual FYE dummies.
#
# NOTE: the CASE paper itself only runs its COVARIATE balance table (Table
# A2) annually -- only the OUTCOME event-study (Table A3) is quarterly. So
# this script only rebuilds a Table A3 equivalent; Table A2 in
# 05_stage1_parallel_trends.R is unchanged and still correct.
#
# quarter_label/quarter_start/quarter_index come straight from hbai_clean.csv
# -- 01_hbai_prep.R builds them itself (looping over every FRS year's raw
# household file for INTDATE), so this script needs no separate merge step.
# Just re-run 01_hbai_prep.R after any change to that logic, then this script.
#
# COVERAGE CAVEAT: hbai_clean.csv's earliest year (FYE2017, i.e. FY2016/17)
# has no FRS raw download in this project, so it has no interview date and
# drops out of this quarterly sample entirely -- the quarterly event-study
# here starts one year later than the annual one in 05_stage1_parallel_trends.R.
#
# Outputs
#   tables/  table_A3_quarterly_composite.csv, table_A3_quarterly_items.csv,
#            table_A3_quarterly_pretrend_wald.csv,
#            table_A3_quarterly_composite_{un,}adjusted.tex
#   figures/ pt_es_quarterly_<outcome>.png (composite x5, items x10)
#            pt_es_quarterly_composite_grid.png, pt_es_quarterly_items_grid.png
# =============================================================================

library(fixest)
library(tidyverse)
library(modelsummary)
library(data.table)
library(patchwork)

setFixest_dict(c(treated = "Scotland"))

# ─────────────────────────────────────────────────────────────────────────────
# CONFIG
# ─────────────────────────────────────────────────────────────────────────────
DATA_PATH    <- "/Users/guypigott/python-venv-demo/Dissertation/data/hbai_clean.csv"
FIGURES_DIR  <- "/Users/guypigott/Claude/Projects/MSc Dissertation/figures"
TABLES_DIR   <- "/Users/guypigott/Claude/Projects/MSc Dissertation/tables"

dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TABLES_DIR,  showWarnings = FALSE, recursive = TRUE)

SCP_EXPAND_DATE <- as.Date("2022-11-14")   # under-16s expansion -- CASE's Table A3 cutoff
ALPHA           <- 0.05

MDCH_ITEMS <- c("MDCH_BED", "MDCH_CEL", "MDCH_COAT", "MDCH_EQP", "MDCH_HOL",
                "MDCH_PLAY", "MDCH_PLY", "MDCH_TEA", "MDCH_TRP", "MDCH_VEG")

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
# LOAD
# ─────────────────────────────────────────────────────────────────────────────
cat("Loading HBAI data...\n")
df <- fread(DATA_PATH)
cat(sprintf("  Raw: %s rows, years: %s\n", format(nrow(df), big.mark = ","),
            paste(sort(unique(df$YEAR)), collapse = ", ")))

if (!all(c("quarter_label", "quarter_start", "quarter_index") %in% names(df))) {
  stop("quarter_label/quarter_start/quarter_index not found in hbai_clean.csv -- ",
       "re-run 01_hbai_prep.R (it builds these from the raw FRS household files).")
}
df[, YEAR := as.integer(YEAR)]
df[, quarter_start := as.Date(quarter_start)]

n_total <- nrow(df)
n_matched <- sum(!is.na(df$quarter_label))
cat(sprintf("  %s of %s HBAI rows (%.1f%%) have a quarter assignment.\n",
            format(n_matched, big.mark = ","), format(n_total, big.mark = ","),
            100 * n_matched / n_total))
cat("  Match rate by year (years with 0%% have no FRS raw download in this project):\n")
print(df[, .(matched = mean(!is.na(quarter_label))), by = YEAR][order(YEAR)])

df <- df[!is.na(quarter_label)]
cat(sprintf("  Retained %s rows with a valid quarter assignment.\n", format(nrow(df), big.mark = ",")))
if (nrow(df) == 0) stop("No rows matched a quarter -- check the merge before going further.")

df[, treated   := as.numeric(scotland)]
df[, quarter_f := factor(quarter_label, levels = unique(quarter_label[order(quarter_start)]))]

# Numeric coercion + sentinel cleanup for outcomes
df[, MDCH := suppressWarnings(as.numeric(MDCH))]
df[, MDCH := ifelse(MDCH < 0, NA_real_, MDCH)]
for (v in c(MDCH_ITEMS, "mdch_any", "mdch_count", "mdch_severe",
            "food_insecure", "very_low_food_sec")) {
  if (v %in% names(df)) df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
}

# ─────────────────────────────────────────────────────────────────────────────
# COVARIATES -- identical construction to 05_stage1_parallel_trends.R /
# 03_stage1_baseline_did.R's CASE_COVS, rebuilt here since this script loads
# its own copy of the data.
# ─────────────────────────────────────────────────────────────────────────────
df[, ETH := ifelse(ETH == 99, NA_real_, ETH)]
df[, larger_fam  := as.numeric(NUMBKIDS == 3)]
df[, lone_parent := as.numeric(MARITAL_WITHKID == 1)]
df[, young_head  := as.numeric(AGEHDBAND == 1)]
df[, female_head := as.numeric(SEXHD == 2)]
df[, disabled_hh := as.numeric(DSCORFAM == 2)]
df[, ETH_f       := factor(ETH)]
CASE_COVS  <- c("young_head", "female_head", "ETH_f", "disabled_hh", "lone_parent", "larger_fam")
avail_covs <- CASE_COVS[CASE_COVS %in% names(df)]

df_mdch      <- df[mdch_observed == 1 & !is.na(GS_INDCH) & GS_INDCH > 0]
df_mdch_flag <- df[!is.na(MDCH)      & !is.na(GS_INDCH) & GS_INDCH > 0]
df_food      <- df[!is.na(food_insecure) & !is.na(GS_INDCH) & GS_INDCH > 0]

avail_items <- MDCH_ITEMS[MDCH_ITEMS %in% names(df_mdch)]

# ─────────────────────────────────────────────────────────────────────────────
# HELPERS -- same shape as 05_stage1_parallel_trends.R, swapping YEAR_f for
# quarter_f and the year-based pretrend cutoff for SCP_EXPAND_DATE.
# ─────────────────────────────────────────────────────────────────────────────
run_trend_model <- function(data, outcome, covs = NULL, ref, weight = "GS_INDCH") {
  base <- paste0(outcome, " ~ treated + i(quarter_f, treated, ref = '", ref, "')")
  if (!is.null(covs) && length(covs) > 0) base <- paste(base, "+", paste(covs, collapse = " + "))
  fml <- as.formula(paste(base, "| quarter_f"))
  tryCatch(
    feols(fml, data = data, weights = as.formula(paste0("~", weight)),
          cluster = ~SERNUM, notes = FALSE),
    error = function(e) { message("  ! model failed for ", outcome, ": ", e$message); NULL }
  )
}

# quarter_label -> quarter_start lookup for tidy_trend / plotting
qmap <- unique(df[, .(quarter_label, quarter_start)])
setkey(qmap, quarter_label)

tidy_trend <- function(fit) {
  if (is.null(fit)) return(NULL)
  broom::tidy(fit, conf.int = TRUE) |>
    filter(str_detect(term, "^quarter_f::")) |>
    mutate(quarter_label = str_remove(str_extract(term, "^quarter_f::[^:]+"), "^quarter_f::")) |>
    left_join(as.data.frame(qmap), by = "quarter_label") |>
    mutate(post = quarter_start >= SCP_EXPAND_DATE)
}

pretrend_wald <- function(fit, all_quarters, ref) {
  if (is.null(fit)) return(NA_real_)
  pre_labels <- qmap[quarter_start < SCP_EXPAND_DATE & quarter_label %in% all_quarters & quarter_label != ref, quarter_label]
  pre_terms <- paste0("quarter_f::", pre_labels, ":treated")
  pre_terms <- pre_terms[pre_terms %in% names(coef(fit))]
  if (length(pre_terms) < 2) return(NA_real_)
  wt <- tryCatch(wald(fit, keep = pre_terms), error = function(e) NULL)
  if (is.null(wt)) return(NA_real_)
  wt$p
}

fmt_cell <- function(est, se, p) {
  stars <- dplyr::case_when(p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ "")
  out <- sprintf("%.3f%s\n(%.3f)", est, stars, se)
  ifelse(is.na(est), "-", out)
}

plot_outcome_trend <- function(td_unadj, td_adj, title, ylab) {
  du <- if (!is.null(td_unadj)) td_unadj |> mutate(spec = "Unadjusted") else NULL
  da <- if (!is.null(td_adj))   td_adj   |> mutate(spec = "Adjusted")   else NULL
  td <- bind_rows(du, da)
  if (nrow(td) == 0) return(NULL)
  ggplot(td, aes(x = quarter_start, y = estimate, ymin = conf.low, ymax = conf.high,
                 colour = spec, shape = spec)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_vline(xintercept = SCP_EXPAND_DATE, linetype = "dashed", colour = "firebrick", linewidth = 0.6) +
    geom_point(position = position_dodge(width = 20), size = 2.1) +
    geom_errorbar(position = position_dodge(width = 20), width = 0) +
    scale_colour_manual(values = c("Unadjusted" = "#2c7bb6", "Adjusted" = "#e66101"), name = "") +
    scale_shape_manual(values  = c("Unadjusted" = 16, "Adjusted" = 17), name = "") +
    labs(title = title,
         subtitle = "Rolling ~13-week quarters (CASE 2025 Table A3 windows) | Dashed red = SCP expansion (14 Nov 2022)",
         x = "Quarter start date", y = ylab,
         caption = "SEs clustered at household (SERNUM). Weights: GS_INDCH.") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", size = 10))
}

# ═════════════════════════════════════════════════════════════════════════════
# TABLE A3 (QUARTERLY): PARALLEL OUTCOME TRENDS TEST
# ═════════════════════════════════════════════════════════════════════════════
cat("\n== TABLE A3 (QUARTERLY): Parallel outcome trends test (event study) =======\n")

outcome_specs <- list(
  list(key = "mdch_any",      label = "MDCH: Any deprivation",   data = df_mdch,      y = "mdch_any"),
  list(key = "mdch_severe",   label = "MDCH: Severe (>5 items)", data = df_mdch,      y = "mdch_severe"),
  list(key = "MDCH_flag",     label = "MDCH: Official DWP flag", data = df_mdch_flag, y = "MDCH"),
  list(key = "food_insecure", label = "Food insecurity",         data = df_food,      y = "food_insecure")
)
for (item in avail_items) {
  outcome_specs[[length(outcome_specs) + 1]] <- list(
    key = item, label = unname(MDCH_LABELS[item]), data = df_mdch, y = item
  )
}
composite_keys <- c("mdch_any", "mdch_severe", "MDCH_flag", "food_insecure")

a3_results <- map(outcome_specs, function(spec) {
  spec_quarters <- sort(unique(spec$data$quarter_label[!is.na(spec$data$quarter_label)]))
  spec_ref <- qmap[quarter_label %in% spec_quarters][order(quarter_start)][1, quarter_label]
  cat(sprintf("  fitting: %-28s (ref quarter %s, n quarters = %d)\n", spec$label, spec_ref, length(spec_quarters)))
  fit_unadj <- run_trend_model(spec$data, spec$y, covs = NULL,       ref = spec_ref)
  fit_adj   <- run_trend_model(spec$data, spec$y, covs = avail_covs, ref = spec_ref)
  list(
    key = spec$key, label = spec$label, ref = spec_ref,
    unadj = fit_unadj, adj = fit_adj,
    td_unadj = tidy_trend(fit_unadj), td_adj = tidy_trend(fit_adj),
    wald_unadj = pretrend_wald(fit_unadj, spec_quarters, ref = spec_ref),
    wald_adj   = pretrend_wald(fit_adj,   spec_quarters, ref = spec_ref)
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
write_csv(a3_wald, file.path(TABLES_DIR, "table_A3_quarterly_pretrend_wald.csv"))
cat("  wrote table_A3_quarterly_pretrend_wald.csv\n")

# ── Wide CSVs (paper-style: unadjusted + adjusted columns per outcome) ──────
build_wide_table <- function(keys) {
  long_df <- map_dfr(keys, function(k) {
    r <- a3_results[[k]]
    tdu <- r$td_unadj; tda <- r$td_adj
    all_q <- sort(unique(c(tdu$quarter_label, tda$quarter_label)))
    map_dfr(all_q, function(ql) {
      u <- if (!is.null(tdu)) tdu[tdu$quarter_label == ql, ] else tdu
      a <- if (!is.null(tda)) tda[tda$quarter_label == ql, ] else tda
      tibble(
        quarter    = ql,
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
    pivot_wider(id_cols = quarter, names_from = outcome, values_from = c(unadjusted, adjusted)) |>
    arrange(quarter)
}

a3_composite_wide <- build_wide_table(composite_keys)
if (nrow(a3_composite_wide) > 0) {
  write_csv(a3_composite_wide, file.path(TABLES_DIR, "table_A3_quarterly_composite.csv"))
  cat("  wrote table_A3_quarterly_composite.csv\n")
}

a3_items_wide <- build_wide_table(avail_items)
if (nrow(a3_items_wide) > 0) {
  write_csv(a3_items_wide, file.path(TABLES_DIR, "table_A3_quarterly_items.csv"))
  cat("  wrote table_A3_quarterly_items.csv\n")
}

# ── Publication tex tables for the 5 composite outcomes ─────────────────────
composite_labels <- vapply(a3_results[composite_keys], `[[`, character(1), "label")

unadj_list <- map(a3_results[composite_keys], "unadj")
unadj_ok   <- !vapply(unadj_list, is.null, logical(1))
composite_unadj_fits <- unadj_list[unadj_ok]
names(composite_unadj_fits) <- composite_labels[unadj_ok]

adj_list  <- map(a3_results[composite_keys], "adj")
adj_ok    <- !vapply(adj_list, is.null, logical(1))
composite_adj_fits <- adj_list[adj_ok]
names(composite_adj_fits) <- composite_labels[adj_ok]

if (length(composite_unadj_fits) > 0) {
  tryCatch({
    modelsummary(
      composite_unadj_fits,
      stars   = c("*" = .1, "**" = .05, "***" = .01),
      gof_map = c("nobs", "r.squared"),
      title   = "Table A3 (quarterly, unadjusted): parallel outcome trends test",
      output  = file.path(TABLES_DIR, "table_A3_quarterly_composite_unadjusted.tex")
    )
    cat("  wrote table_A3_quarterly_composite_unadjusted.tex\n")
  }, error = function(e) {
    cat("  ! table_A3_quarterly_composite_unadjusted.tex failed -- numbers still in the .csv. Error: ",
        conditionMessage(e), "\n")
  })
}
if (length(composite_adj_fits) > 0) {
  tryCatch({
    modelsummary(
      composite_adj_fits,
      stars   = c("*" = .1, "**" = .05, "***" = .01),
      gof_map = c("nobs", "r.squared"),
      title   = "Table A3 (quarterly, adjusted): parallel outcome trends test",
      output  = file.path(TABLES_DIR, "table_A3_quarterly_composite_adjusted.tex")
    )
    cat("  wrote table_A3_quarterly_composite_adjusted.tex\n")
  }, error = function(e) {
    cat("  ! table_A3_quarterly_composite_adjusted.tex failed -- numbers still in the .csv. Error: ",
        conditionMessage(e), "\n")
  })
}

# ── Figures: one per outcome (unadjusted + adjusted overlaid) ───────────────
composite_grid_rows <- list()
items_grid_rows     <- list()

for (r in a3_results) {
  p <- plot_outcome_trend(r$td_unadj, r$td_adj, paste0("Parallel trends (quarterly): ", r$label),
                          "DiD-style coefficient (Scot x quarter)")
  if (!is.null(p)) {
    ggsave(file.path(FIGURES_DIR, paste0("pt_es_quarterly_", r$key, ".png")), p, width = 9, height = 5, dpi = 150)
    cat(sprintf("  wrote pt_es_quarterly_%s.png\n", r$key))
  }
  if (r$key %in% composite_keys) {
    if (!is.null(r$td_unadj)) composite_grid_rows[[r$key]] <- r$td_unadj |> mutate(outcome = r$label)
  } else {
    if (!is.null(r$td_unadj)) items_grid_rows[[r$key]] <- r$td_unadj |> mutate(outcome = r$label)
  }
}

if (length(composite_grid_rows) > 0) {
  gdat <- bind_rows(composite_grid_rows)
  p_grid <- ggplot(gdat, aes(quarter_start, estimate, ymin = conf.low, ymax = conf.high, colour = post)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_vline(xintercept = SCP_EXPAND_DATE, linetype = "dashed", colour = "firebrick", alpha = .7) +
    geom_point(size = 1.4) + geom_errorbar(width = 0) +
    scale_colour_manual(values = c("FALSE" = "#2c7bb6", "TRUE" = "#d7191c"), guide = "none") +
    facet_wrap(~outcome, ncol = 3, scales = "free_y") +
    labs(title = "Table A3 replication (quarterly): composite MDCH & food insecurity trends (unadjusted)",
         x = "Quarter start date", y = "Scot x quarter coefficient") +
    theme_minimal(base_size = 9) + theme(strip.text = element_text(face = "bold"),
                                          axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file.path(FIGURES_DIR, "pt_es_quarterly_composite_grid.png"), p_grid, width = 12, height = 7, dpi = 150)
  cat("  wrote pt_es_quarterly_composite_grid.png\n")
}

if (length(items_grid_rows) > 0) {
  gdat <- bind_rows(items_grid_rows)
  p_grid <- ggplot(gdat, aes(quarter_start, estimate, ymin = conf.low, ymax = conf.high, colour = post)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_vline(xintercept = SCP_EXPAND_DATE, linetype = "dashed", colour = "firebrick", alpha = .7) +
    geom_point(size = 1.2) + geom_errorbar(width = 0) +
    scale_colour_manual(values = c("FALSE" = "#2c7bb6", "TRUE" = "#d7191c"), guide = "none") +
    facet_wrap(~outcome, ncol = 3, scales = "free_y") +
    labs(title = "Table A3 replication (quarterly): individual MDCH item trends (unadjusted)",
         x = "Quarter start date", y = "Scot x quarter coefficient") +
    theme_minimal(base_size = 9) + theme(strip.text = element_text(face = "bold"),
                                          axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file.path(FIGURES_DIR, "pt_es_quarterly_items_grid.png"), p_grid, width = 13, height = 10, dpi = 150)
  cat("  wrote pt_es_quarterly_items_grid.png\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# SUMMARY
# ─────────────────────────────────────────────────────────────────────────────
cat("\n== SUMMARY: pre-trend Wald tests (quarterly, BH-corrected) =================\n")
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
