# =============================================================================
# 02_summary_stats.R
# Descriptive summary stats comparing Scotland vs England samples, following
# the sample definition and weighting of Table 2 in Stewart et al. (CASE 2025).
# No regressions -- descriptive tables only.
#
# Reads:  hbai_clean.csv        (produced by 01_hbai_prep.R)
# Writes: figures/summary_table.csv
#         figures/sample_composition.csv
#         figures/background_characteristics.csv   (cf. CASE Table 2)
#         figures/table_a1_by_period.csv            (cf. CASE Table A1)
#         figures/summary_plot.png
#
# Packages required: tidyverse, scales
# Install once with: install.packages(c("tidyverse", "scales"))
# =============================================================================

library(tidyverse)
library(scales)

# ── Paths ────────────────────────────────────────────────────────────────────
DATA_ROOT <- "/Users/guypigott/python-venv-demo/Dissertation"
HBAI_CSV  <- file.path(DATA_ROOT, "data", "hbai_clean.csv")
OUT_DIR   <- "figures"
dir.create(OUT_DIR, showWarnings = FALSE)

SCP_EXPAND_YEAR <- 2023   # FY 2022/23: SCP extended to all under-16s, £25/week

# =============================================================================
# LOAD
# =============================================================================
df <- read_csv(HBAI_CSV, show_col_types = FALSE)

cat(sprintf("Loaded %s rows, years %d-%d\n",
            format(nrow(df), big.mark = ","), min(df$YEAR), max(df$YEAR)))

df <- df |>
  mutate(
    group  = if_else(scotland == 1, "Scotland", "England"),
    period = if_else(post == 1, "Post-SCP", "Pre-SCP")
  )

# =============================================================================
# 1. DATA QUALITY CHECK -- completeness of key variables after cleaning
# =============================================================================
key_vars <- c("MDCH", "food_insecure", "AGE")
key_vars <- key_vars[key_vars %in% names(df)]

quality <- tibble(variable = key_vars) |>
  rowwise() |>
  mutate(
    n_non_missing = sum(!is.na(df[[variable]])),
    pct_missing   = round(mean(is.na(df[[variable]])) * 100, 1)
  ) |>
  ungroup()

cat("\n--- Data quality check ---\n")
print(quality)

# =============================================================================
# 2. SAMPLE COMPOSITION BY YEAR AND GROUP (full cleaned sample; sections 3-5
# below use a smaller analytic subsample -- see next section)
# =============================================================================
sample_composition <- df |>
  count(YEAR, group) |>
  pivot_wider(names_from = group, values_from = n, values_fill = 0) |>
  mutate(Total = England + Scotland) |>
  rename(`Year (FY ending)` = YEAR)

cat("\n--- Sample composition by year and group ---\n")
print(sample_composition)
write_csv(sample_composition, file.path(OUT_DIR, "sample_composition.csv"))

# =============================================================================
# ANALYTIC SAMPLE for sections 3-5
# Restricted to children with a valid MDCH flag and weighted by GS_INDCH
# (dependent child weight), following Stewart et al. (CASE 2025) Table 2 note
# (ii). food_insecure only starts 2021/22, so it's missing more than MDCH.
# =============================================================================
df_mdch <- df |>
  filter(!is.na(MDCH), !is.na(GS_INDCH)) |>
  mutate(ETH = if_else(ETH == 99, NA_real_, ETH))   # 99 = "not declared" -> missing

cat(sprintf("\nFull cleaned sample: %s  |  Valid MDCH flag: %s\n",
            format(nrow(df), big.mark = ","), format(nrow(df_mdch), big.mark = ",")))

# =============================================================================
# RAW VALUE DIAGNOSTIC -- sanity-check the category codes used in section 3
# =============================================================================
cat("\n--- RAW VALUE COUNTS ---\n")
for (v in c("NUMBKIDS", "DISCORABFLG", "DISCORKID", "SEXHD",
            "MARITAL_WITHKID", "AGEHDBAND", "ETH")) {
  cat(sprintf("\n%s:\n", v))
  print(table(df_mdch[[v]], useNA = "ifany"))
}
cat("\nfood_insecure (built from FOODSEC_STATUS_CAT, sanity check):\n")
print(table(df_mdch$food_insecure, useNA = "ifany"))

# =============================================================================
# 3. BACKGROUND CHARACTERISTICS BY GROUP (balance table)
# Reproduces Table 2 in Stewart et al. (CASE 2025): compares Scotland vs
# England on observables, pooled across all years.
#   MDCH             1 = "In child material deprivation" (official DWP flag)
#   NUMBKIDS         ==3 = "three or more children"           -> larger family
#   MARITAL_WITHKID  1 = "Lone Parent"                         -> lone parent
#   AGEHDBAND        1 = "16-24 adult"                         -> young head
#   SEXHD            2 = "Female"                              -> female head
#   DSCORFAM         2 = "Family where someone is disabled"    -> disabled household
#   ETH              1 = "White"
# =============================================================================
balance_row <- function(data, var, label, is_yes) {
  data |>
    mutate(flag = as.numeric(is_yes(.data[[var]]))) |>
    filter(!is.na(flag)) |>
    group_by(group) |>
    summarise(stat = round(weighted.mean(flag, GS_INDCH), 3), .groups = "drop") |>
    mutate(Characteristic = label)
}

food_insecure_hh <- df_mdch |>
  filter(!is.na(food_insecure)) |>
  group_by(group) |>
  summarise(stat = round(weighted.mean(food_insecure, GS_INDCH), 3), .groups = "drop") |>
  mutate(Characteristic = "Food insecure households")

background_table <- bind_rows(
  balance_row(df_mdch, "MDCH",            "Child material deprivation",              \(x) x == 1),
  food_insecure_hh,
  balance_row(df_mdch, "NUMBKIDS",        "Larger families (3+ children)", \(x) x == 3),
  balance_row(df_mdch, "MARITAL_WITHKID", "Lone parent households",        \(x) x == 1),
  balance_row(df_mdch, "AGEHDBAND",       "Young head of household",       \(x) x == 1),
  balance_row(df_mdch, "SEXHD",           "Female head of household",      \(x) x == 2),
  balance_row(df_mdch, "DSCORFAM",        "Disabled household",            \(x) x == 2),
  balance_row(df_mdch, "ETH",             "White households",              \(x) x == 1)
) |>
  pivot_wider(names_from = group, values_from = stat) |>
  select(Characteristic, England, Scotland)

# N row, matching the bottom row of the CASE table (raw count, not weighted)
n_row <- df_mdch |> count(group) |> pivot_wider(names_from = group, values_from = n) |>
  mutate(Characteristic = "N", .before = 1)

background_table <- bind_rows(background_table, n_row)

cat("\n--- Background characteristics by group (cf. CASE Table 2) ---\n")
print(background_table)
write_csv(background_table, file.path(OUT_DIR, "background_characteristics.csv"))

# =============================================================================
# 3b. TABLE A1: same characteristics, split by group x period (All/Pre/Post)
# Reproduces Table A1 in Stewart et al. (CASE 2025). Same variables/codes as
# section 3 above.
# =============================================================================
table_a1_row <- function(data, var, label, is_yes) {
  d <- data |> mutate(flag = as.numeric(is_yes(.data[[var]]))) |> filter(!is.na(flag))
  all_rows    <- d |> group_by(group) |>
    summarise(stat = weighted.mean(flag, GS_INDCH), .groups = "drop") |> mutate(period = "All")
  period_rows <- d |> group_by(group, period) |>
    summarise(stat = weighted.mean(flag, GS_INDCH), .groups = "drop")
  bind_rows(all_rows, period_rows) |> mutate(stat = round(stat, 3), Characteristic = label)
}

food_insecure_a1 <- {
  d <- df_mdch |> filter(!is.na(food_insecure))
  all_rows    <- d |> group_by(group) |>
    summarise(stat = weighted.mean(food_insecure, GS_INDCH), .groups = "drop") |> mutate(period = "All")
  period_rows <- d |> group_by(group, period) |>
    summarise(stat = weighted.mean(food_insecure, GS_INDCH), .groups = "drop")
  bind_rows(all_rows, period_rows) |> mutate(stat = round(stat, 3), Characteristic = "Food insecure households")
}

col_order <- c("Scotland All", "Scotland Pre-SCP", "Scotland Post-SCP",
               "England All", "England Pre-SCP", "England Post-SCP")

table_a1 <- bind_rows(
  table_a1_row(df_mdch, "MDCH",            "Child material deprivation",    \(x) x == 1),
  food_insecure_a1,
  table_a1_row(df_mdch, "NUMBKIDS",        "Larger families (3+ children)", \(x) x == 3),
  table_a1_row(df_mdch, "MARITAL_WITHKID", "Lone parent households",        \(x) x == 1),
  table_a1_row(df_mdch, "AGEHDBAND",       "Young head of household",       \(x) x == 1),
  table_a1_row(df_mdch, "SEXHD",           "Female head of household",      \(x) x == 2),
  table_a1_row(df_mdch, "DSCORFAM",        "Disabled household",            \(x) x == 2),
  table_a1_row(df_mdch, "ETH",             "White households",              \(x) x == 1)
) |>
  mutate(col = paste(group, period)) |>
  select(Characteristic, col, stat) |>
  pivot_wider(names_from = col, values_from = stat) |>
  select(Characteristic, all_of(col_order))

# N row (raw count, not weighted), same All/Pre/Post x group breakdown
n_a1 <- bind_rows(
  df_mdch |> count(group) |> mutate(period = "All"),
  df_mdch |> count(group, period)
) |>
  mutate(col = paste(group, period)) |>
  select(col, n) |>
  pivot_wider(names_from = col, values_from = n) |>
  mutate(Characteristic = "N", .before = 1) |>
  select(Characteristic, all_of(col_order))

table_a1 <- bind_rows(table_a1, n_a1)

cat("\n--- Table A1: characteristics by group x period (cf. CASE Table A1) ---\n")
print(table_a1)
write_csv(table_a1, file.path(OUT_DIR, "table_a1_by_period.csv"))

# =============================================================================
# 4. SUMMARY STATISTICS TABLE
# Key outcomes by group x period, same weighted sample as section 3.
# "Severe deprivation" (>=3 of 10 MDCH_* items) is an approximate measure, not
# an official DWP statistic -- MDCHDMP (official) only exists from 2023/24.
# =============================================================================
summary_table <- df_mdch |>
  group_by(group, period) |>
  summarise(
    N                    = n(),
    `Any deprivation`    = round(weighted.mean(MDCH, GS_INDCH, na.rm = TRUE), 3),
    `Severe deprivation` = round(weighted.mean(mdch_severe, GS_INDCH, na.rm = TRUE), 3),
    `Food insecure`      = round(weighted.mean(food_insecure, GS_INDCH, na.rm = TRUE), 3),
    `Mean age`           = round(weighted.mean(AGE, GS_INDCH, na.rm = TRUE), 1),
    .groups = "drop"
  )

cat("\n--- Summary statistics table ---\n")
print(summary_table)
write_csv(summary_table, file.path(OUT_DIR, "summary_table.csv"))

# =============================================================================
# 5. PLOT
# Trend in the main outcome (official material deprivation flag, MDCH),
# Scotland vs England
# =============================================================================
annual <- df_mdch |>
  filter(!is.na(MDCH)) |>
  group_by(YEAR, group) |>
  summarise(mean_val = weighted.mean(MDCH, GS_INDCH), .groups = "drop")

p <- ggplot(annual, aes(x = YEAR, y = mean_val, colour = group)) +
  geom_vline(xintercept = SCP_EXPAND_YEAR - 0.5,
             linetype = "dashed", colour = "grey40") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = sort(unique(annual$YEAR))) +
  scale_colour_manual(values = c("Scotland" = "#3B0064", "England" = "#9333C8")) +
  labs(x = "Financial year (ending)", y = "Share with any material deprivation",
       colour = NULL, title = "Child material deprivation, Scotland vs England") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(OUT_DIR, "summary_plot.png"), p, width = 7, height = 4.5, dpi = 200)

cat("\nDone. Outputs written to figures/:\n")
cat("  summary_table.csv, sample_composition.csv, background_characteristics.csv,\n")
cat("  table_a1_by_period.csv, summary_plot.png\n")
