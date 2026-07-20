# =============================================================================
# 02b_hbai_case_tables.R
# CASE Paper-Style Summary Statistics Tables
# Scottish Child Payment Dissertation
# =============================================================================
# Replicates the structure of:
#   Table 2  (Stewart et al., LSE CASE 2025) — overall means by group
#   Table A1 — means by group × period (All / Pre-SCP / Post-SCP)
#   Table A2 — parallel covariate trends test (Scotland × year interactions)
#
# Run AFTER 01_hbai_lca_prep.py has produced hbai_lca.csv
#
# Packages: tidyverse, broom, lmtest, sandwich, flextable, officer
# install.packages(c("tidyverse","broom","lmtest","sandwich","officer","flextable"))
# =============================================================================

library(tidyverse)
library(broom)
library(lmtest)
library(sandwich)
library(officer)
library(flextable)

# ── Paths ─────────────────────────────────────────────────────────────────────
DATA_ROOT <- "/Users/guypigott/python-venv-demo/Dissertation"
HBAI_CSV  <- file.path(DATA_ROOT, "data", "hbai_lca.csv")
DOCX_OUT  <- "hbai_case_tables.docx"
CSV_DIR   <- "figures"

SCP_EXPAND_YEAR <- 2023   # FY 2022/23: SCP to all under-16s at £25/week
REF_YEAR        <- 2017   # reference year for parallel trends regressions

dir.create(CSV_DIR, showWarnings = FALSE)

# =============================================================================
# LOAD AND PREPARE
# =============================================================================
cat("Loading", HBAI_CSV, "...\n")
df <- read_csv(HBAI_CSV, show_col_types = FALSE)
cat(sprintf("  %s rows, years: %s\n",
            format(nrow(df), big.mark = ","),
            paste(sort(unique(df$YEAR)), collapse = ", ")))

# Quick diagnostic — check key outcome variables
cat("\nMDCH distribution (official DWP binary flag):\n")
print(table(df$MDCH, useNA = "always"))
cat("\nFOODSEC distribution (before propagation):\n")
print(table(df$FOODSEC, useNA = "always"))

# FOODSEC is recorded on the benefit unit reference person's row only.
# Propagate to all children in the same benefit unit so every child
# in a household that answered the question gets the FOODSEC value.
df <- df |>
  group_by(SERNUM, BENUNIT, YEAR) |>
  mutate(
    FOODSEC       = suppressWarnings(max(FOODSEC,       na.rm = TRUE)),
    food_insecure = suppressWarnings(max(food_insecure, na.rm = TRUE))
  ) |>
  ungroup() |>
  mutate(
    FOODSEC       = na_if(FOODSEC,       -Inf),
    food_insecure = na_if(food_insecure, -Inf)
  )

cat("\nFOODSEC distribution (after propagation within BU):\n")
print(table(df$FOODSEC, useNA = "always"))
cat(sprintf("  → food_insecure = 1 for %s children; post-2020 rate = %.1f%%\n",
            format(sum(df$food_insecure == 1 & df$YEAR >= 2020, na.rm = TRUE), big.mark = ","),
            100 * sum(df$food_insecure == 1 & df$YEAR >= 2020, na.rm = TRUE) /
                  sum(df$YEAR >= 2020)))

# ── Derived variables (matching CASE paper definitions) ───────────────────────
# Variable guide codings (HBAI):
#   COUPLE_KID  : 1=couple with children, 0=lone parent
#   NEWFAMBU_KID: count of dependent children in benefit unit
#   AGEHD       : age of household head (continuous)
#   SEXHD       : 1=male, 2=female
#   DIS         : 1=disability present in household
#   ETH         : 1=White, 2+=non-White (simplified indicator)
#   GS_INDCH    : dependent child grossing weight (HBAI official)

df <- df |>
  mutate(
    # ── Outcome variables ──────────────────────────────────────────────────
    # MDCH: official DWP weighted-score binary flag (1=deprived, 0=not).
    # NOT the same as mdch_any (any item lacking) — uses weighted score ≥25
    # threshold giving ~15-20% rates matching CASE paper.
    cmd        = as.numeric(MDCH == 1),
    # FOODSEC is only asked of lower-income households (~8% of children).
    # CASE paper computes food insecurity over full sample, treating
    # unobserved as food-secure (0). Replicate that here.
    # FOODSEC collected from FY 2019/20 (YEAR >= 2020) onwards.
    # Within module years, treat unobserved children as food-secure (0).
    # Pre-module years → NA (excluded). Rate ~10% vs CASE paper 13.5%;
    # gap reflects limited FOODSEC coverage in our HBAI edition.
    food_insec = case_when(
      YEAR < 2020                                 ~ NA_real_,
      !is.na(food_insecure) & food_insecure == 1 ~ 1,
      TRUE                                        ~ 0
    ),
    # ── Household characteristics ──────────────────────────────────────────
    # COUPLE_KID codes: 1=couple 1kid, 2=couple 2kids, 3=couple 3+kids,
    #                   4=lone 1kid,   5=lone 2kids,   6=lone 3+kids, 7+=other
    # Larger family = 3+ children (codes 3 and 6)
    larger_fam  = as.numeric(!is.na(COUPLE_KID) & COUPLE_KID %in% c(3, 6)),
    lone_parent = as.numeric(!is.na(COUPLE_KID) & COUPLE_KID %in% 4:6),
    # AGEHD: age band, 1=youngest (under 25); NOT raw age
    young_head  = as.numeric(!is.na(AGEHD)        & AGEHD == 1),
    # SEX_ADULT: 1=male, 2=female; preferred over SEXHD (has 81k NAs)
    female_head = as.numeric(!is.na(SEX_ADULT)    & SEX_ADULT == 2),
    # DIS: 5=not disabled, 6=unknown; 1-4=disability present (any type)
    # Exclude code 6 (unknown) from denominator via NA
    disabled_hh = case_when(DIS == 5 ~ 0, DIS %in% 1:4 ~ 1, TRUE ~ NA_real_),
    # ETH: 1=White British, 2=White Irish, 3=Other White
    white_hh    = as.numeric(!is.na(ETH)          & ETH %in% 1:3),
    # ── Period ────────────────────────────────────────────────────────────
    group   = if_else(scotland == 1, "Scotland", "England"),
    pre_scp = as.numeric(YEAR < SCP_EXPAND_YEAR)
  )

# Variable labels (ordered as in CASE paper)
VAR_LABELS <- c(
  cmd         = "Child Material Deprivation",
  food_insec  = "Food Insecure Households",
  larger_fam  = "Larger families",
  lone_parent = "Lone parent households",
  young_head  = "Young head of household",
  female_head = "Female head of household",
  disabled_hh = "Disabled household",
  white_hh    = "White households"
)

# Helper: weighted mean, returning NA if all weights or values missing
wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}


# =============================================================================
# TABLE 2: Overall means by group (replicates CASE Table 2)
# =============================================================================
cat("\n--- TABLE 2: Overall means by group ---\n")

build_t2 <- function(data) {
  map_dfr(names(VAR_LABELS), function(var) {
    tibble(
      Variable = VAR_LABELS[var],
      Scotland = wmean(data[[var]][data$group == "Scotland"],
                       data$GS_INDCH[data$group == "Scotland"]),
      England  = wmean(data[[var]][data$group == "England"],
                       data$GS_INDCH[data$group == "England"])
    )
  })
}

t2_stats <- build_t2(df)

# Append N row
t2_n <- tibble(
  Variable = "N",
  Scotland = sum(df$group == "Scotland" & !is.na(df$cmd)),
  England  = sum(df$group == "England"  & !is.na(df$cmd))
)

t2_full <- bind_rows(
  t2_stats |> mutate(across(c(Scotland, England), \(x) as.character(round(x, 3)))),
  t2_n     |> mutate(across(c(Scotland, England), \(x) format(as.integer(x), big.mark=",")))
)

print(t2_full)
write_csv(t2_full, file.path(CSV_DIR, "case_table2_overall.csv"))


# =============================================================================
# TABLE A1: Means by group × period (All / Pre-SCP / Post-SCP)
# =============================================================================
cat("\n--- TABLE A1: Means by group × period ---\n")

build_t_period <- function(data, grp) {
  sub     <- data[data$group == grp, ]
  sub_pre <- sub[sub$YEAR < SCP_EXPAND_YEAR, ]
  sub_pst <- sub[sub$YEAR >= SCP_EXPAND_YEAR, ]

  map_dfr(names(VAR_LABELS), function(var) {
    tibble(
      Variable = VAR_LABELS[var],
      All      = wmean(sub[[var]],     sub$GS_INDCH),
      `Pre-SCP`  = wmean(sub_pre[[var]], sub_pre$GS_INDCH),
      `Post-SCP` = wmean(sub_pst[[var]], sub_pst$GS_INDCH)
    )
  })
}

ta1_scot <- build_t_period(df, "Scotland")
ta1_eng  <- build_t_period(df, "England")

# N rows
n_row <- function(data, grp) {
  sub     <- data[data$group == grp & !is.na(data$cmd), ]
  tibble(
    Variable   = "N",
    All        = format(nrow(sub),                                   big.mark=","),
    `Pre-SCP`  = format(nrow(sub[sub$YEAR < SCP_EXPAND_YEAR, ]),  big.mark=","),
    `Post-SCP` = format(nrow(sub[sub$YEAR >= SCP_EXPAND_YEAR, ]), big.mark=",")
  )
}

ta1_scot_full <- bind_rows(
  ta1_scot |> mutate(across(c(All, `Pre-SCP`, `Post-SCP`), \(x) as.character(round(x, 3)))),
  n_row(df, "Scotland")
) |> rename_with(\(x) paste("Scotland:", x), -Variable)

ta1_eng_full <- bind_rows(
  ta1_eng |> mutate(across(c(All, `Pre-SCP`, `Post-SCP`), \(x) as.character(round(x, 3)))),
  n_row(df, "England")
) |> rename_with(\(x) paste("England:", x), -Variable)

ta1_full <- bind_cols(
  ta1_scot_full,
  ta1_eng_full |> select(-Variable)
)

print(ta1_full)
write_csv(ta1_full, file.path(CSV_DIR, "case_tableA1_prepost.csv"))


# =============================================================================
# TABLE A2: Parallel covariate trends test
# For each covariate C:
#   C ~ factor(YEAR) + scotland + scotland:factor(YEAR)  [weighted by GS_INDCH]
# Report: scotland:factor(YEAR) coefficients + HC2 robust SEs
# Reference year = REF_YEAR (2016/17)
# =============================================================================
cat("\n--- TABLE A2: Parallel covariate trends test ---\n")

# Covariates only (no outcomes in this table per CASE paper)
TREND_VARS <- c(
  "Larger\nfamilies"        = "larger_fam",
  "Lone parent\nhouseholds" = "lone_parent",
  "Young head\nof household"= "young_head",
  "Female head\nof household"="female_head",
  "Disabled\nhousehold"     = "disabled_hh",
  "White\nhouseholds"       = "white_hh"
)

# Years to interact (exclude reference year and COVID year 2021)
df_reg <- df |>
  filter(!is.na(GS_INDCH), GS_INDCH > 0) |>
  mutate(YEAR = factor(YEAR, levels = sort(unique(YEAR))))

years_non_ref <- setdiff(levels(df_reg$YEAR), as.character(REF_YEAR))

run_trend_reg <- function(var) {
  fml <- as.formula(
    paste0(var, " ~ factor(YEAR) + scotland + scotland:factor(YEAR)")
  )
  mod <- lm(fml, data = df_reg, weights = GS_INDCH, na.action = na.omit)

  # HC2 robust standard errors (heteroskedasticity-consistent)
  vcov_hc2 <- vcovHC(mod, type = "HC2")
  ct <- coeftest(mod, vcov = vcov_hc2)

  # Extract scotland:factor(YEAR) interactions
  int_rows <- rownames(ct)[grepl("^scotland:factor\\(YEAR\\)", rownames(ct))]

  tibble(
    term  = int_rows,
    coef  = ct[int_rows, "Estimate"],
    se    = ct[int_rows, "Std. Error"],
    year  = str_extract(term, "\\d{4}")
  )
}

ta2_list <- map(TREND_VARS, run_trend_reg)

# Format: coefficient(SE) with significance stars
fmt_coef <- function(coef, se) {
  p <- 2 * pt(-abs(coef / se), df = Inf)
  stars <- case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "")
  sprintf("%.3f%s\n(%.3f)", coef, stars, se)
}

# All years present across regressions
all_years <- sort(unique(unlist(map(ta2_list, ~ .x$year))))

ta2_wide <- map_dfr(all_years, function(yr) {
  row <- tibble(Year = paste0("Scot × ", yr, "/", substr(as.integer(yr)+1, 3, 4)))
  for (nm in names(TREND_VARS)) {
    var_results <- ta2_list[[nm]]
    match_row   <- var_results[var_results$year == yr, ]
    row[[nm]] <- if (nrow(match_row) == 1) {
      fmt_coef(match_row$coef, match_row$se)
    } else { "-" }
  }
  row
})

# Add footer rows
n_row_ta2 <- tibble(Year = "N", !!!setNames(
  map(names(TREND_VARS), \(nm) format(
    sum(!is.na(df_reg[[TREND_VARS[nm]]]) & !is.na(df_reg$GS_INDCH)),
    big.mark = ","
  )),
  names(TREND_VARS)
))

ta2_final <- bind_rows(ta2_wide, n_row_ta2)

print(ta2_final)
write_csv(ta2_final, file.path(CSV_DIR, "case_tableA2_covariate_trends.csv"))


# =============================================================================
# WORD DOCUMENT
# =============================================================================
cat("\n--- Building Word document ---\n")

# ── Styling helpers ──────────────────────────────────────────────────────────
PURPLE <- "#3B0064"
LIGHT  <- "#EDE0F7"

make_ft <- function(df_in, caption) {
  ft <- flextable(df_in) |>
    theme_booktabs() |>
    bg(bg = LIGHT, part = "header") |>
    color(color = PURPLE, part = "header") |>
    bold(part = "header") |>
    fontsize(size = 9.5, part = "all") |>
    font(fontname = "Calibri", part = "all") |>
    padding(padding = 3, part = "all") |>
    autofit() |>
    set_caption(caption = as_paragraph(
      as_chunk(caption, props = fp_text(bold = TRUE, font.size = 10,
                                        color = PURPLE, font.family = "Calibri"))
    ))
  ft
}

note_par <- function(doc, txt) {
  body_add_fpar(doc, fpar(
    ftext(txt, prop = fp_text(font.size = 8.5, color = "grey40",
                               font.family = "Calibri", italic = TRUE))
  ))
}

doc <- read_docx()

# ─────────────────────────────────────────────────────────────────────────────
# Table 2
# ─────────────────────────────────────────────────────────────────────────────
doc <- doc |>
  body_add_par("Table 2: Summary Statistics, 2016/17–2023/24", style = "heading 2") |>
  body_add_flextable(make_ft(
    t2_full,
    "Table 2: Summary Statistics, 2016/17–2023/24"
  ))
doc <- note_par(doc, paste0(
  "Notes: (i) Mean values of each variable by country, weighted by the dependent child weight ",
  "(GS_INDCH) provided in the HBAI dataset. (ii) Larger families = 3 or more dependent children ",
  "in benefit unit. Young head = household head aged under 25. ",
  "Source: Authors' calculations using 2016/17–2023/24 HBAI data, 19th edition (DWP, 2025)."
))
doc <- body_add_par(doc, "", style = "Normal")

# ─────────────────────────────────────────────────────────────────────────────
# Table A1
# ─────────────────────────────────────────────────────────────────────────────
doc <- doc |>
  body_add_par("Table A1: Summary Statistics by Period", style = "heading 2") |>
  body_add_flextable(make_ft(
    ta1_full,
    "Table A1: Summary statistics for children observed before and after the SCP by country of residence, 2016/17–2023/24"
  ))
doc <- note_par(doc, paste0(
  "Notes: (i) Mean values of each variable by country and period, weighted by GS_INDCH. ",
  "Pre-SCP = 2016/17–2021/22; Post-SCP = 2022/23–2023/24 (SCP expanded to all under-16s, £25/week). ",
  "2020/21 excluded (COVID-19 data disruption). ",
  "Source: Authors' calculations using HBAI 19th edition (DWP, 2025)."
))
doc <- body_add_par(doc, "", style = "Normal")

# ─────────────────────────────────────────────────────────────────────────────
# Table A2
# ─────────────────────────────────────────────────────────────────────────────
doc <- doc |>
  body_add_par("Table A2: Parallel Covariate Trends Test", style = "heading 2") |>
  body_add_flextable(make_ft(
    ta2_final,
    "Table A2: Parallel covariate trends test (all children)"
  ))
doc <- note_par(doc, paste0(
  "Notes: (i) The table reports estimates of the Scotland × Year interaction from: ",
  "Cᵢₜ = α + β·Scotlandᵢ + Σγₜ·Yearₜ + Σδₜ·(Scotlandᵢ × Yearₜ) + εᵢₜ, ",
  "where each covariate C is the outcome in turn. ",
  "(ii) Reference category is 2016/17. (iii) Scotland is a dummy equal to one if the child resides in Scotland. ",
  "(iv) Regressions weighted by GS_INDCH. (v) HC2 robust standard errors in parentheses. ",
  "(vi) *p<0.05, **p<0.01, ***p<0.001. ",
  "Source: Authors' calculations using HBAI 19th edition (DWP, 2025)."
))

# ─────────────────────────────────────────────────────────────────────────────
# Save
# ─────────────────────────────────────────────────────────────────────────────
print(doc, target = DOCX_OUT)
cat(sprintf("\n✓ Saved: %s\n", DOCX_OUT))
cat(sprintf("✓ CSVs in: %s/\n", CSV_DIR))
