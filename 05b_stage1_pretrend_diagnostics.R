# ─────────────────────────────────────────────────────────────────────────────
# 05b_pretrend_diagnostics.R
# Diagnoses why 05_parallel_trends.R's Wald tests flag almost every covariate
# and outcome as "possible pre-trend", including demographic variables with
# no plausible policy-related trend.
#
# Leading hypothesis: a clustering artifact, not genuine divergence.
# run_trend_model() clusters at GVTREGN_f (~10 regions, Scotland the only
# treated one) -- cluster-robust inference is unreliable with this few
# clusters and a single treated cluster (Cameron & Miller 2015; MacKinnon &
# Webb). The CASE paper this replicates never clusters at region level.
#
# Requires 05_parallel_trends.R already run in the same session (reuses
# df_a2, df_mdch, df_mdch_flag, run_trend_model(), pretrend_wald(),
# TABLE_A2_VARS, REF_YEAR, TABLES_DIR, FIGURES_DIR, etc.)
#
# Outputs
#   tables/  table_pretrend_cluster_structure.csv
#            table_pretrend_alt_vcov_comparison.csv
#            table_pretrend_placebo_regions.csv
#   figures/ pt_diag_placebo_distribution.png
# ─────────────────────────────────────────────────────────────────────────────

stopifnot(
  "Run 05_parallel_trends.R first (df_a2 not found)"        = exists("df_a2"),
  "Run 05_parallel_trends.R first (df_mdch not found)"      = exists("df_mdch"),
  "Run 05_parallel_trends.R first (df_mdch_flag not found)" = exists("df_mdch_flag"),
  "Run 05_parallel_trends.R first (run_trend_model missing)" = exists("run_trend_model"),
  "Run 05_parallel_trends.R first (pretrend_wald missing)"   = exists("pretrend_wald")
)

library(fixest)
library(tidyverse)
library(data.table)

cat("\n═══════════════════════════════════════════════════════════════════════════\n")
cat("PRE-TREND DIAGNOSTICS: why is almost everything flagged?\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")

# ─────────────────────────────────────────────────────────────────────────────
# DIAGNOSTIC 1: cluster structure
# ─────────────────────────────────────────────────────────────────────────────
cat("\n== DIAGNOSTIC 1: Cluster structure (GVTREGN) ================================\n")

cluster_tab <- df_a2[, .(n_obs = .N), by = .(GVTREGN, treated)][order(treated, GVTREGN)]
print(cluster_tab)

n_clusters         <- length(unique(df_a2$GVTREGN))
n_treated_clusters <- length(unique(df_a2[treated == 1]$GVTREGN))
n_control_clusters <- length(unique(df_a2[treated == 0]$GVTREGN))

cat(sprintf(
  "\nTotal GVTREGN clusters: %d | Treated (Scotland) clusters: %d | Control clusters: %d\n",
  n_clusters, n_treated_clusters, n_control_clusters
))

if (n_treated_clusters == 1) {
  cat("!! Scotland is a SINGLE treated cluster out of", n_clusters, "total --\n")
  cat("   cluster-robust Wald tests here are likely anti-conservative.\n")
}

write_csv(cluster_tab, file.path(TABLES_DIR, "table_pretrend_cluster_structure.csv"))
cat("wrote table_pretrend_cluster_structure.csv\n")

# ─────────────────────────────────────────────────────────────────────────────
# DIAGNOSTIC 2: does the flag survive under alternative SE specs? Refit a
# representative set of covariates/outcomes region-clustered, hetero-robust,
# and household-clustered (SERNUM -- far more clusters).
# ─────────────────────────────────────────────────────────────────────────────
cat("\n== DIAGNOSTIC 2: Pre-trend Wald flag under alternative SE specs ============\n")

run_trend_model_vcov <- function(data, outcome, vcov_spec, covs = NULL,
                                  ref = REF_YEAR, weight = "GS_INDCH") {
  base <- paste0(outcome, " ~ treated + i(YEAR_f, treated, ref = '", ref, "')")
  if (!is.null(covs) && length(covs) > 0) base <- paste(base, "+", paste(covs, collapse = " + "))
  fml  <- as.formula(paste(base, "| YEAR_f"))
  tryCatch(
    feols(fml, data = data, weights = as.formula(paste0("~", weight)),
          vcov = vcov_spec, notes = FALSE),
    error = function(e) { message("  ! model failed for ", outcome, " (", deparse(substitute(vcov_spec)), "): ", e$message); NULL }
  )
}

# Representative set, kept small so the script runs quickly
diag_covariates <- c(white_hh = "White households", female_head = "Female head of household")
diag_outcomes <- list(
  list(key = "MDCH_flag", label = "MDCH: Official DWP flag", data = df_mdch_flag, y = "MDCH"),
  list(key = "mdch_any",  label = "MDCH: Any deprivation",   data = df_mdch,      y = "mdch_any"),
  list(key = "MDCH_COAT", label = "Warm coat",                data = df_mdch,      y = "MDCH_COAT")
)

has_sernum <- "SERNUM" %in% names(df_a2)
if (!has_sernum) {
  cat("  ! SERNUM not found in data -- skipping household-clustered comparison column.\n")
}

alt_vcov_rows <- list()

for (cov_var in names(diag_covariates)) {
  d <- df_a2
  fit_region <- run_trend_model_vcov(d, cov_var, ~GVTREGN_f)
  fit_hetero <- run_trend_model_vcov(d, cov_var, "hetero")
  fit_hh     <- if (has_sernum) run_trend_model_vcov(d, cov_var, ~SERNUM) else NULL
  alt_vcov_rows[[length(alt_vcov_rows) + 1]] <- tibble(
    outcome           = diag_covariates[[cov_var]],
    type              = "covariate",
    wald_p_region_clu = pretrend_wald(fit_region, unique(d$YEAR)),
    wald_p_hetero      = pretrend_wald(fit_hetero, unique(d$YEAR)),
    wald_p_hh_clu      = if (!is.null(fit_hh)) pretrend_wald(fit_hh, unique(d$YEAR)) else NA_real_
  )
}

for (spec in diag_outcomes) {
  spec_ref <- min(spec$data$YEAR, na.rm = TRUE)
  fit_region <- run_trend_model_vcov(spec$data, spec$y, ~GVTREGN_f, ref = spec_ref)
  fit_hetero <- run_trend_model_vcov(spec$data, spec$y, "hetero",   ref = spec_ref)
  fit_hh     <- if (has_sernum) run_trend_model_vcov(spec$data, spec$y, ~SERNUM, ref = spec_ref) else NULL
  alt_vcov_rows[[length(alt_vcov_rows) + 1]] <- tibble(
    outcome           = spec$label,
    type              = "outcome",
    wald_p_region_clu = pretrend_wald(fit_region, unique(spec$data$YEAR), ref = spec_ref),
    wald_p_hetero      = pretrend_wald(fit_hetero, unique(spec$data$YEAR), ref = spec_ref),
    wald_p_hh_clu      = if (!is.null(fit_hh)) pretrend_wald(fit_hh, unique(spec$data$YEAR), ref = spec_ref) else NA_real_
  )
}

alt_vcov_tab <- bind_rows(alt_vcov_rows) |>
  mutate(across(starts_with("wald_p"), ~ round(.x, 4)),
         flag_region = if_else(!is.na(wald_p_region_clu) & wald_p_region_clu < ALPHA, "possible pre-trend", "OK"),
         flag_hetero  = if_else(!is.na(wald_p_hetero)     & wald_p_hetero     < ALPHA, "possible pre-trend", "OK"),
         flag_hh      = if_else(!is.na(wald_p_hh_clu)     & wald_p_hh_clu     < ALPHA, "possible pre-trend", "OK"))

print(alt_vcov_tab)
write_csv(alt_vcov_tab, file.path(TABLES_DIR, "table_pretrend_alt_vcov_comparison.csv"))
cat("wrote table_pretrend_alt_vcov_comparison.csv\n")

# ─────────────────────────────────────────────────────────────────────────────
# DIAGNOSTIC 3: placebo test -- relabel each English region as fake-treated
# and re-run the same joint pre-trend Wald test. If Scotland's real p-value
# isn't more extreme than the placebo draws, the test fails structurally
# (few clusters), not because Scotland specifically diverges.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n== DIAGNOSTIC 3: Placebo test (each English region as fake-treated) ========\n")

placebo_outcomes <- list(
  list(key = "MDCH_flag", label = "MDCH: Official DWP flag", data = df_mdch_flag, y = "MDCH"),
  list(key = "mdch_any",  label = "MDCH: Any deprivation",   data = df_mdch,      y = "mdch_any"),
  list(key = "MDCH_COAT", label = "Warm coat",                data = df_mdch,      y = "MDCH_COAT"),
  list(key = "white_hh",  label = "White households",         data = df_a2,        y = "white_hh")
)

england_regions <- sort(unique(df_a2[treated == 0]$GVTREGN))
cat(sprintf("  %d English regions found: %s\n", length(england_regions), paste(england_regions, collapse = ", ")))

placebo_rows <- list()

for (spec in placebo_outcomes) {
  d_full <- as.data.table(spec$data)[treated == 0]  # England only, drop real Scotland
  spec_ref <- min(d_full$YEAR, na.rm = TRUE)

  # Real Scotland result for comparison (region-clustered, from original data)
  real_fit <- run_trend_model_vcov(spec$data, spec$y, ~GVTREGN_f, ref = spec_ref)
  real_p   <- pretrend_wald(real_fit, unique(spec$data$YEAR), ref = spec_ref)

  for (r in england_regions) {
    d_placebo <- copy(d_full)
    d_placebo[, treated := as.numeric(GVTREGN == r)]
    fit <- run_trend_model_vcov(d_placebo, spec$y, ~GVTREGN_f, ref = spec_ref)
    p   <- pretrend_wald(fit, unique(d_placebo$YEAR), ref = spec_ref)
    placebo_rows[[length(placebo_rows) + 1]] <- tibble(
      outcome = spec$label, placebo_region = r, is_real_scotland = FALSE, wald_p = p
    )
  }
  placebo_rows[[length(placebo_rows) + 1]] <- tibble(
    outcome = spec$label, placebo_region = NA_real_, is_real_scotland = TRUE, wald_p = real_p
  )
}

placebo_tab <- bind_rows(placebo_rows)
write_csv(placebo_tab, file.path(TABLES_DIR, "table_pretrend_placebo_regions.csv"))
cat("wrote table_pretrend_placebo_regions.csv\n")

placebo_summary <- placebo_tab |>
  group_by(outcome) |>
  summarise(
    n_placebo           = sum(!is_real_scotland, na.rm = TRUE),
    n_placebo_flagged   = sum(!is_real_scotland & !is.na(wald_p) & wald_p < ALPHA, na.rm = TRUE),
    pct_placebo_flagged = round(100 * n_placebo_flagged / n_placebo, 1),
    scotland_wald_p     = wald_p[is_real_scotland],
    scotland_rank_pctile = round(100 * mean(wald_p[!is_real_scotland] <= scotland_wald_p, na.rm = TRUE), 1)
  )
print(placebo_summary)
# scotland_rank_pctile near 0/100 = Scotland's result is unusual vs placebo draws;
# near 50 = looks like a typical single-region draw

p_placebo <- ggplot(placebo_tab, aes(x = wald_p, fill = is_real_scotland)) +
  geom_histogram(data = filter(placebo_tab, !is_real_scotland), bins = 15, alpha = 0.7) +
  geom_vline(data = filter(placebo_tab, is_real_scotland),
             aes(xintercept = wald_p), colour = "firebrick", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = ALPHA, colour = "grey40", linetype = "dotted") +
  facet_wrap(~outcome, scales = "free_y") +
  scale_fill_manual(values = c("FALSE" = "#2c7bb6"), guide = "none") +
  labs(title = "Placebo test: joint pre-trend Wald p-value by fake-treated region",
       subtitle = "Blue bars = each English region as fake-treated | Red dashed = actual Scotland | Grey dotted = alpha 0.05",
       x = "Wald p-value", y = "Count of placebo regions") +
  theme_minimal(base_size = 10)
ggsave(file.path(FIGURES_DIR, "pt_diag_placebo_distribution.png"), p_placebo, width = 10, height = 7, dpi = 150)
cat("wrote pt_diag_placebo_distribution.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# DIAGNOSTIC 4: Wild cluster bootstrap (region-level) joint pre-trend test
#
# Caveat: MacKinnon & Webb (2018) show that with a single treated cluster
# (Scotland, under GVTREGN clustering), the wild cluster bootstrap typically
# never rejects at any conventional level -- so this is expected to look
# conservative regardless of the true pre-trends, and does NOT fix the
# single-treated-cluster problem the way household clustering does. Shown
# alongside household clustering for comparison, not as a replacement.
#
# Requires: install.packages("fwildclusterboot")
# ─────────────────────────────────────────────────────────────────────────────
cat("\n== DIAGNOSTIC 4: Wild cluster bootstrap (region-level, joint test) =========\n")

wild_boot_available <- requireNamespace("fwildclusterboot", quietly = TRUE)
if (!wild_boot_available) {
  cat("  ! fwildclusterboot not installed. Run install.packages('fwildclusterboot')\n")
  cat("    then re-run this section to get the wild-bootstrap comparison column.\n")
}

# Joint test of H0: all pre-period Scot x Year coefficients = 0, via
# fwildclusterboot::mboottest(). engine = "R" avoids the Julia backend
# (not installed). type = "webb": few clusters means rademacher weights
# give too few unique draws; Webb (2013) weights are the package's own
# recommendation here.
mboot_joint_pretrend <- function(fit, all_years, ref, clustid = "GVTREGN_f",
                                  B = 9999, type = "webb", engine = "R") {
  if (is.null(fit) || !wild_boot_available) return(NA_real_)
  pre_years <- setdiff(as.character(sort(all_years[all_years < SCP_EXPAND_YEAR])), as.character(ref))
  pre_terms <- paste0("YEAR_f::", pre_years, ":treated")
  cn <- names(coef(fit))
  pre_terms <- pre_terms[pre_terms %in% cn]
  if (length(pre_terms) < 2) return(NA_real_)
  R <- matrix(0, nrow = length(pre_terms), ncol = length(cn))
  for (i in seq_along(pre_terms)) R[i, which(cn == pre_terms[i])] <- 1
  r <- rep(0, length(pre_terms))
  res <- tryCatch(
    fwildclusterboot::mboottest(object = fit, clustid = clustid, B = B, R = R, r = r,
                                 type = type, engine = engine),
    error = function(e) { message("  ! mboottest failed: ", e$message); NULL }
  )
  if (is.null(res)) return(NA_real_)
  # Field name for the bootstrap p-value has varied across package versions --
  # try the common ones defensively rather than hard-coding a single path.
  p_val <- tryCatch(res$p_val, error = function(e) NULL)
  if (is.null(p_val) || length(p_val) == 0) p_val <- tryCatch(summary(res)$p_val, error = function(e) NA_real_)
  as.numeric(p_val)
}

wild_boot_rows <- list()

if (wild_boot_available) {
  for (cov_var in names(diag_covariates)) {
    fit_region <- run_trend_model_vcov(df_a2, cov_var, ~GVTREGN_f)
    wp <- mboot_joint_pretrend(fit_region, unique(df_a2$YEAR), ref = REF_YEAR)
    wild_boot_rows[[length(wild_boot_rows) + 1]] <-
      tibble(outcome = diag_covariates[[cov_var]], wald_p_wildboot_region = wp)
  }
  for (spec in diag_outcomes) {
    spec_ref <- min(spec$data$YEAR, na.rm = TRUE)
    fit_region <- run_trend_model_vcov(spec$data, spec$y, ~GVTREGN_f, ref = spec_ref)
    wp <- mboot_joint_pretrend(fit_region, unique(spec$data$YEAR), ref = spec_ref)
    wild_boot_rows[[length(wild_boot_rows) + 1]] <-
      tibble(outcome = spec$label, wald_p_wildboot_region = wp)
  }
  wild_boot_tab <- bind_rows(wild_boot_rows) |> mutate(wald_p_wildboot_region = round(wald_p_wildboot_region, 4))
} else {
  wild_boot_tab <- tibble(
    outcome = c(unname(diag_covariates), map_chr(diag_outcomes, "label")),
    wald_p_wildboot_region = NA_real_
  )
}

print(wild_boot_tab)

# ── Combined comparison table: all four SE/inference approaches side by side ──
full_comparison <- alt_vcov_tab |>
  select(outcome, type, wald_p_region_clu, wald_p_hetero, wald_p_hh_clu,
         flag_region, flag_hetero, flag_hh) |>
  left_join(wild_boot_tab, by = "outcome") |>
  mutate(flag_wildboot = case_when(
    is.na(wald_p_wildboot_region)        ~ "not run",
    wald_p_wildboot_region < ALPHA        ~ "possible pre-trend",
    TRUE                                  ~ "OK"
  ))

print(full_comparison)
write_csv(full_comparison, file.path(TABLES_DIR, "table_pretrend_full_comparison.csv"))
cat("wrote table_pretrend_full_comparison.csv\n")

cat("\n═══════════════════════════════════════════════════════════════════════════\n")
cat("DONE. See table_pretrend_full_comparison.csv and table_pretrend_placebo_regions.csv.\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
