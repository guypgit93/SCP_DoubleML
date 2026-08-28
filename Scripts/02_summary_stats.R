# =============================================================================
# 02_summary_stats.R
# Descriptive summary stats comparing Scotland vs England samples, following
# the sample definition and weighting of Table 2 in Andersen et al. (CASE 2025).
# No regressions -- descriptive tables only.
#
# Reads:  hbai_clean.csv        (produced by 01_hbai_prep.R)
# Writes: tables/summary_table.csv
#         tables/sample_composition.csv
#         tables/background_characteristics.csv   (cf. CASE Table 2)
#         tables/table_a1_by_period.csv            (cf. CASE Table A1)
#         figures/summary_plot.png
# =============================================================================

library(tidyverse)
library(scales)

# ── Paths ────────────────────────────────────────────────────────────────────
DATA_ROOT   <- "/Users/guypigott/python-venv-demo/Dissertation"
HBAI_CSV    <- file.path(DATA_ROOT, "data", "hbai_clean.csv")
TABLES_DIR  <- "/Users/guypigott/Claude/Projects/MSc Dissertation/tables"
FIGURES_DIR <- "/Users/guypigott/Claude/Projects/MSc Dissertation/figures"
dir.create(TABLES_DIR, showWarnings = FALSE, recursive = TRUE)   # no-op if it already exists
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

SCP_EXPAND_YEAR <- 2023   # FY 2022/23: SCP extended to all under-16s, £25/week

# =============================================================================
# LOAD
# =============================================================================
df <- read_csv(HBAI_CSV, show_col_types = FALSE)

cat(sprintf("Loaded %s rows, years %d-%d\n",
            format(nrow(df), big.mark = ","), min(df$YEAR), max(df$YEAR)))

# Human-readable group/period labels, used as the grouping columns in every
# table below instead of the raw 0/1 scotland/post flags.
df <- df |>
  mutate(
    group  = if_else(scotland == 1, "Scotland", "England"),
    period = if_else(post == 1, "Post-SCP", "Pre-SCP")
  )

# =============================================================================
# DATA QUALITY CHECK 
# =============================================================================
key_vars <- c("MDCH", "food_insecure", "AGE")
key_vars <- key_vars[key_vars %in% names(df)]   # drop any that aren't actually in this build of the CSV

quality <- tibble(variable = key_vars) |>
  rowwise() |>                                  # so n_non_missing/pct_missing are computed one variable at a time
  mutate(
    n_non_missing = sum(!is.na(df[[variable]])),
    pct_missing   = round(mean(is.na(df[[variable]])) * 100, 1)
  ) |>
  ungroup()

cat("\n--- Data quality check ---\n")
print(quality)

# =============================================================================
# SAMPLE COMPOSITION BY YEAR AND GROUP (full cleaned sample; sections 3-5
# below use a smaller analytic subsample -- see next section)
# =============================================================================
sample_composition <- df |>
  count(YEAR, group) |>                                             # rows per YEAR x group
  pivot_wider(names_from = group, values_from = n, values_fill = 0) |>  # -> one column per group
  mutate(Total = England + Scotland) |>
  rename(`Year (FY ending)` = YEAR)

cat("\n--- Sample composition by year and group ---\n")
print(sample_composition)
write_csv(sample_composition, file.path(TABLES_DIR, "sample_composition.csv"))

# =============================================================================
# ANALYTIC SAMPLE for sections 3-5
# Restricted to children with a valid MDCH flag and weighted by GS_INDCH
# (dependent child weight), following Andersen et al. (CASE 2025) Table 2 note
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
  print(table(df_mdch[[v]], useNA = "ifany"))   # eyeball that the codes used below (e.g. AGEHDBAND==1) still mean what I think
}
cat("\nfood_insecure (built from FOODSEC_STATUS_CAT, sanity check):\n")
print(table(df_mdch$food_insecure, useNA = "ifany"))

# =============================================================================
# BACKGROUND CHARACTERISTICS BY GROUP (balance table)
# Reproduces Table 2 in Andersen et al. (CASE 2025): compares Scotland vs
# England on observables, pooled across all years.
#   MDCH             1 = "In child material deprivation" (official DWP flag)
#   NUMBKIDS         ==3 = "three or more children"           -> larger family
#   MARITAL_WITHKID  1 = "Lone Parent"                         -> lone parent
#   AGEHDBAND        1 = "16-24 adult"                         -> young head
#   SEXHD            2 = "Female"                              -> female head
#   DSCORFAM         2 = "Family where someone is disabled"    -> disabled household
#   ETH              1 = "White"
# =============================================================================

# One row = one characteristic. `is_yes` turns the raw variable into a 0/1
# flag (e.g. \(x) x == 3 for "3+ kids"), then takes the GS_INDCH-weighted
# share of that flag, split by Scotland/England.
balance_row <- function(data, var, label, is_yes) {
  data |>
    mutate(flag = as.numeric(is_yes(.data[[var]]))) |>
    filter(!is.na(flag)) |>
    group_by(group) |>
    summarise(stat = round(weighted.mean(flag, GS_INDCH), 3), .groups = "drop") |>
    mutate(Characteristic = label)
}

# food_insecure is already 0/1, so it doesn't need balance_row's is_yes step --
# built the same way otherwise.
food_insecure_hh <- df_mdch |>
  filter(!is.na(food_insecure)) |>
  group_by(group) |>
  summarise(stat = round(weighted.mean(food_insecure, GS_INDCH), 3), .groups = "drop") |>
  mutate(Characteristic = "Food insecure households")

# Stack one row per characteristic, then pivot so Scotland/England become columns.
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
  mutate(Characteristic = "N", .before = 1)   # .before = 1 puts N as the last row when bound below, not sorted alphabetically

background_table <- bind_rows(background_table, n_row)

cat("\n--- Background characteristics by group (cf. CASE Table 2) ---\n")
print(background_table)
write_csv(background_table, file.path(TABLES_DIR, "background_characteristics.csv"))

# =============================================================================
# TABLE A1: same characteristics, split by group x period (All/Pre/Post)
# Reproduces Table A1 in Andersen et al. (CASE 2025). Same variables/codes as
# section 3 above.
# =============================================================================

# Same idea as balance_row above, but computes both an "All years" row and
# separate Pre-SCP/Post-SCP rows for each characteristic, then stacks them.
table_a1_row <- function(data, var, label, is_yes) {
  d <- data |> mutate(flag = as.numeric(is_yes(.data[[var]]))) |> filter(!is.na(flag))
  all_rows    <- d |> group_by(group) |>
    summarise(stat = weighted.mean(flag, GS_INDCH), .groups = "drop") |> mutate(period = "All")
  period_rows <- d |> group_by(group, period) |>
    summarise(stat = weighted.mean(flag, GS_INDCH), .groups = "drop")
  bind_rows(all_rows, period_rows) |> mutate(stat = round(stat, 3), Characteristic = label)
}

# food_insecure again needs its own version since it's already 0/1 (no is_yes step).
food_insecure_a1 <- {
  d <- df_mdch |> filter(!is.na(food_insecure))
  all_rows    <- d |> group_by(group) |>
    summarise(stat = weighted.mean(food_insecure, GS_INDCH), .groups = "drop") |> mutate(period = "All")
  period_rows <- d |> group_by(group, period) |>
    summarise(stat = weighted.mean(food_insecure, GS_INDCH), .groups = "drop")
  bind_rows(all_rows, period_rows) |> mutate(stat = round(stat, 3), Characteristic = "Food insecure households")
}

# Fixes the column order in the final table -- pivot_wider alone would sort
# these alphabetically, which scrambles the natural Scotland-then-England,
# All-then-Pre-then-Post reading order.
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
  mutate(col = paste(group, period)) |>          # e.g. "Scotland Pre-SCP", becomes a column name below
  select(Characteristic, col, stat) |>
  pivot_wider(names_from = col, values_from = stat) |>
  select(Characteristic, all_of(col_order))       # reorder into col_order

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
write_csv(table_a1, file.path(TABLES_DIR, "table_a1_by_period.csv"))

# =============================================================================
# SUMMARY STATISTICS TABLE
# Key outcomes by group x period, same weighted sample as section 3. 
# =============================================================================
summary_table <- df_mdch |>
  group_by(group, period) |>
  summarise(
    N                     = n(),                                                      # raw row count, not weighted
    `Official MDCH flag`  = round(weighted.mean(MDCH, GS_INDCH, na.rm = TRUE), 3),
    `Any deprivation`     = round(weighted.mean(mdch_any, GS_INDCH, na.rm = TRUE), 3),
    `Severe deprivation`  = round(weighted.mean(mdch_severe, GS_INDCH, na.rm = TRUE), 3),
    `Food insecure`       = round(weighted.mean(food_insecure, GS_INDCH, na.rm = TRUE), 3),
    .groups = "drop"
  )

cat("\n--- Summary statistics table ---\n")
print(summary_table)
write_csv(summary_table, file.path(TABLES_DIR, "summary_table.csv"))

# =============================================================================
# PLOT
# Trend in the main outcome (official material deprivation flag, MDCH),
# Scotland vs England
# =============================================================================
annual <- df_mdch |>
  filter(!is.na(MDCH)) |>
  group_by(YEAR, group) |>
  summarise(mean_val = weighted.mean(MDCH, GS_INDCH), .groups = "drop")   # one weighted MDCH rate per YEAR x group

p <- ggplot(annual, aes(x = YEAR, y = mean_val, colour = group)) +
  geom_vline(xintercept = SCP_EXPAND_YEAR - 0.5,          # marks the SCP expansion, just before the 2023 point
             linetype = "dashed", colour = "grey40") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = sort(unique(annual$YEAR))) +  # one tick per year actually in the data (2021 is skipped, not interpolated)
  scale_colour_manual(values = c("Scotland" = "#3B0064", "England" = "#9333C8")) +
  labs(x = "Financial year (ending)", y = "Official MDCH flag (share)",
       colour = NULL, title = "Child material deprivation, Scotland vs England") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(FIGURES_DIR, "summary_plot.png"), p, width = 7, height = 4.5, dpi = 200)

cat("\nDone. Tables written to tables/:\n")
cat("  summary_table.csv, sample_composition.csv, background_characteristics.csv,\n")
cat("  table_a1_by_period.csv\n")
cat("Plot written to figures/: summary_plot.png\n")
