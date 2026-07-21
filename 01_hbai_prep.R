# =============================================================================
# 01_hbai_prep.R
# HBAI data prep for the Scottish Child Payment dissertation.
# Loads the harmonised HBAI extracts, restricts to England/Scotland children,
# builds deprivation/food-security outcomes and DiD variables, saves hbai_clean.csv.
#
# Key choices:
#   - Scotland/England defined via COUNTRY (1=Eng,2=Wales,3=Scot,4=NI).
#   - food_insecure uses FOODSEC_STATUS_CAT (official DWP category), HHSHARE==1 only.
#   - Primary deprivation outcome is MDCH (official DWP flag), not a homemade item count.
#   - mdch_observed = !is.na(MDCH).
# Requires: tidyverse
# =============================================================================

library(tidyverse)

# ── Paths ────────────────────────────────────────────────────────────────────
DATA_ROOT <- "/Users/guypigott/python-venv-demo/Dissertation"
HBAI_ROOT <- file.path(DATA_ROOT, "UKDA-5828-tab", "tab", "23-24prices")
HBAI_OUT  <- file.path(DATA_ROOT, "data", "hbai_clean.csv")

HBAI_FILES <- c(
  "i1518e_2324prices.tab",   # 2015/16 - 2017/18
  "i1821e_2324prices.tab",   # 2018/19 - 2020/21
  "i2124e_2324prices.tab"    # 2021/22 - 2023/24
)

SCP_EXPAND_YEAR <- 2023   # FY 2022/23: SCP extended to all under-16s, £25/week

# 2 of 12 MDCH items dropped (MDCH_LES, MDCH_ACT: sparse/uncertain coverage)
MDCH_ITEMS <- c("MDCH_BED", "MDCH_CEL", "MDCH_COAT", "MDCH_EQP", "MDCH_HOL",
                "MDCH_PLAY", "MDCH_PLY", "MDCH_TEA", "MDCH_TRP", "MDCH_VEG")

# Identifiers, geography, outcomes, and DML covariates (used in 04_dml_did.R)
KEEP_VARS <- c(
  "SERNUM", "BENUNIT", "PERSON", "YEAR_CODE",
  "COUNTRY", "GVTREGN",
  "AGE",
  "MDCH", MDCH_ITEMS, "MDCHDMP",
  "LOWINCMDCH", "LOWINCMDCHSEV",
  "FOODSEC", "FOODSEC_STATUS_CAT", "HHSHARE",
  "GS_INDCH",
  # DML covariates
  "SEX", "AGEBAND_CH", "AGEHD", "AGEHDBAND", "AGEHDBAND_KID", "SEXHD",
  "AGESP", "AGESPBAND",
  "COUPLE_KID", "NEWFAMBU_SINGLE", "NEWFAMBU_KID", "NUMBKIDS",
  "NEWFAMBU_WITH", "NEWFAMBU_WITH_WA", "NEWFAMBU_WITH_PN", "NEWFAMBU_WITH_PN_TOT",
  "ADULTB", "ADULTH", "ADULTHBAND", "MARITAL_KID", "MARITAL_WITHKID",
  "KID0_1", "KID2_4", "KID5_7", "KID8_10", "KID11_12", "KID13_15", "KID16PLUS",
  "KIDECOBU", "KIDECOBU_WORK",
  "DISCORKID", "DISCORABFLG", "DIS", "DIS_TYPE", "DSCORFAM", "DSCORFAM_WORK",
  "BENBU_DISBEN", "BENBU_DLA", "BENBU_PIP", "DSCORANDBEN",
  "EMPSTATI", "SEX_ADULT", "S_OE_GRO_PROP_EARN", "WINPAYBU", "WINPAYHD",
  "WINPAYSP", "EGRINCBU",
  "S_OE_AHC", "S_OE_BHC", "S_OE_GRO", "S_OE_GRO_PROP_BEN", "S_OE_GRO_PROP_INV",
  "S_OE_HC", "ESBENIBU", "CHBENBU", "INCHILBU",
  "BENBU_UC", "BENBU_UC_OR_EQUIV", "NEWFAMBU_UC", "BENBU_FSM", "BENBU_IS",
  "BENBU_JSA", "BENBU_ESA", "BENBU_HB", "BENBU_CTC", "BENBU_WTC", "WFTCBU",
  "BENBU_PC",
  "TENHBAI", "PTENTYP2", "ERENTBU", "EHCOST", "ES_HCOST",
  "ETH", "ETHGRPHHPUB", "ETHGRPHH"
)

# GVTREGN region groupings, used for regional FE / clustering (not treatment)
ENGLAND_REGIONS  <- c(1, 2, 4, 5, 6, 7, 8, 9, 10)
SCOTLAND_REGION  <- 12
NORTHERN_ENGLAND <- c(1, 2, 4)

# =============================================================================
# LOAD AND STACK
# =============================================================================

# MDCH item codes: 1=has item -> 0 (not deprived); 2=wants but can't afford -> 1
# (deprived); 0/3/-9/.A = not asked / doesn't want / missing -> NA
recode_mdch_item <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  dplyr::case_when(x == 1 ~ 0, x == 2 ~ 1, TRUE ~ NA_real_)
}

all_dfs <- list()

for (fname in HBAI_FILES) {
  fpath <- file.path(HBAI_ROOT, fname)
  if (!file.exists(fpath)) {
    cat(sprintf("  x NOT FOUND: %s\n", fpath))
    next
  }

  cat(sprintf("\nLoading %s...\n", fname))
  # Read as character first, then coerce -- avoids readr's type-guessing
  # silently NA-ing a column on a bad guess.
  raw <- read_tsv(fpath, show_col_types = FALSE, col_types = cols(.default = col_character()))
  names(raw) <- toupper(trimws(names(raw)))
  cat(sprintf("  Raw shape: %d rows x %d cols\n", nrow(raw), ncol(raw)))

  # Known data quirk: header is missing one column name, so read_tsv folds an
  # extra tab-delimited field into SERNUM (e.g. "0\t1"). Take the last piece.
  if ("SERNUM" %in% names(raw)) {
    raw$SERNUM <- vapply(strsplit(raw$SERNUM, "\t", fixed = TRUE), tail, character(1), n = 1)
  }

  # Raw column is called YEAR and holds a wave code (23=2016/17 ... 31=2024/25).
  # Rename before deriving calendar YEAR below to avoid a name clash.
  if ("YEAR" %in% names(raw)) raw <- raw |> rename(YEAR_CODE = YEAR)

  # GVTREGN/GS_INDWA column-swap check: pick whichever column actually looks
  # like a ~13-level region code rather than trusting either one blindly.
  if (all(c("GVTREGN", "GS_INDWA") %in% names(raw))) {
    gvtregn_vals <- suppressWarnings(as.numeric(raw$GVTREGN))
    gsindwa_vals <- suppressWarnings(as.numeric(raw$GS_INDWA))
    n_distinct_gvtregn <- length(unique(na.omit(gvtregn_vals)))
    n_distinct_gsindwa <- length(unique(na.omit(gsindwa_vals)))
    if (n_distinct_gvtregn <= 20 && max(gvtregn_vals, na.rm = TRUE) <= 20) {
      cat("  GVTREGN looks like clean region codes; no swap.\n")
    } else if (n_distinct_gsindwa <= 20 && max(gsindwa_vals, na.rm = TRUE) <= 20) {
      cat("  GS_INDWA looks like clean region codes; swapping into GVTREGN.\n")
      raw <- raw |> rename(GVTREGN_RAW = GVTREGN, GVTREGN = GS_INDWA)
    } else {
      cat("  ! Neither GVTREGN nor GS_INDWA looks like clean region codes -- no swap applied.\n")
    }
  }

  keep <- intersect(KEEP_VARS, names(raw))
  if ("MDCHDMP" %in% names(raw)) keep <- union(keep, "MDCHDMP")  # 2023/24+ only
  missing_vars <- setdiff(KEEP_VARS, c(keep, "MDCHDMP"))
  if (length(missing_vars) > 0) {
    cat(sprintf("  ! Variables not found in %s: %s\n", fname, paste(missing_vars, collapse = ", ")))
  }

  d <- raw[, keep]
  d <- d |> mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))

  # Calendar YEAR (financial year ending) = YEAR_CODE + 1994
  if (!"YEAR_CODE" %in% names(d)) stop(sprintf("YEAR_CODE not found in %s", fname))
  d <- d |> mutate(YEAR = YEAR_CODE + 1994)
  if (all(is.na(d$YEAR))) stop(sprintf("YEAR is entirely NA after parsing %s", fname))
  cat(sprintf("  Years in file: %s\n", paste(sort(unique(d$YEAR)), collapse = ", ")))

  # Geography filter: England + Scotland only
  d <- d |> filter(COUNTRY %in% c(1, 3))
  d <- d |>
    mutate(
      scotland         = as.numeric(COUNTRY == 3),
      northern_england = as.numeric(scotland == 0 & GVTREGN %in% NORTHERN_ENGLAND)
    )
  cat(sprintf("  After geo filter - Scotland: %s, England: %s\n",
              format(sum(d$scotland == 1), big.mark = ","),
              format(sum(d$scotland == 0), big.mark = ",")))

  # Sentinel cleanup: negative codes -> NA (valid category codes are always >= 0)
  d <- d |> mutate(across(where(is.numeric), ~ if_else(.x < 0, NA_real_, .x)))

  all_dfs[[fname]] <- d
  cat(sprintf("  -> %s rows retained\n", format(nrow(d), big.mark = ",")))
}

if (length(all_dfs) == 0) stop(sprintf("No HBAI files loaded. Check HBAI_ROOT: %s", HBAI_ROOT))

df <- bind_rows(all_dfs)
cat(sprintf("\n%s\nCombined: %s rows, years: %s\n", strrep("=", 60),
            format(nrow(df), big.mark = ","), paste(sort(unique(df$YEAR)), collapse = ", ")))

# =============================================================================
# SAMPLE RESTRICTIONS
# =============================================================================
n0 <- nrow(df)
df <- df |> filter(AGE <= 16)
cat(sprintf("\nAge <=16 filter:      %8s -> %s rows\n", format(n0, big.mark = ","), format(nrow(df), big.mark = ",")))

n0 <- nrow(df)
df <- df |> filter(YEAR_CODE >= 23)   # 23 = 2016/17, first full year
cat(sprintf("YEAR_CODE >=23 filter:%8s -> %s rows\n", format(n0, big.mark = ","), format(nrow(df), big.mark = ",")))

n0 <- nrow(df)
df <- df |> filter(YEAR != 2021)   # COVID-19 survey disruption
cat(sprintf("Exclude 2020/21:      %8s -> %s rows\n", format(n0, big.mark = ","), format(nrow(df), big.mark = ",")))

# =============================================================================
# SANITY-CHECK DIAGNOSTICS
# =============================================================================
cat("\n--- COUNTRY vs GVTREGN cross-tab ---\n")
print(table(country = df$COUNTRY, scotland_via_gvtregn = ifelse(df$GVTREGN == SCOTLAND_REGION, "Scotland", "Other"), useNA = "ifany"))

cat("\n--- Raw value counts for key DML covariates ---\n")
for (v in c("NUMBKIDS", "DISCORABFLG", "DISCORKID", "SEXHD", "DSCORFAM",
            "MARITAL_WITHKID", "AGEHDBAND", "ETH", "TENHBAI", "EMPSTATI")) {
  if (v %in% names(df)) {
    cat(sprintf("\n%s:\n", v))
    print(table(df[[v]], useNA = "ifany"))
  }
}
cat("\nFOODSEC (continuous 0-10 score):\n")
print(summary(df$FOODSEC))

# =============================================================================
# RECODE LCA INDICATORS (individual MDCH_* items -> 0/1/NA)
# =============================================================================
available_items <- intersect(MDCH_ITEMS, names(df))
missing_items    <- setdiff(MDCH_ITEMS, names(df))
cat(sprintf("\nMDCH items available: %d/%d\n", length(available_items), length(MDCH_ITEMS)))
if (length(missing_items) > 0) cat(sprintf("  Missing: %s\n", paste(missing_items, collapse = ", ")))

df <- df |> mutate(across(all_of(available_items), recode_mdch_item))

# Approximate item-count severity measure (not the official DWP flag -- see MDCH below)
item_mat <- as.matrix(df[available_items])
n_observed <- rowSums(!is.na(item_mat))
df$mdch_count <- ifelse(n_observed == 0, NA_real_, rowSums(item_mat, na.rm = TRUE))
df <- df |>
  mutate(
    mdch_any    = if_else(!is.na(mdch_count), as.numeric(mdch_count > 0), NA_real_),
    mdch_severe = if_else(!is.na(mdch_count), as.numeric(mdch_count >= 3), NA_real_)
  )
cat(sprintf("Approx mdch_count non-missing: %s (mdch_any prevalence %.3f)\n",
            format(sum(!is.na(df$mdch_count)), big.mark = ","), mean(df$mdch_any, na.rm = TRUE)))

# Official DWP deprivation flag -- primary outcome. Already benefit-unit-level.
df <- df |> mutate(MDCH = if_else(MDCH %in% c(0, 1), MDCH, NA_real_))
df <- df |> mutate(mdch_observed = as.numeric(!is.na(MDCH)))
cat(sprintf("Official MDCH non-missing: %s (prevalence %.3f)\n",
            format(sum(df$mdch_observed), big.mark = ","), mean(df$MDCH, na.rm = TRUE)))

# =============================================================================
# FOOD SECURITY
# food_insecure = FOODSEC_STATUS_CAT==2 (official DWP category), restricted to
# HHSHARE==1 per the variable guide. NAs (pre-2021/22, non-response) left as-is.
# very_low_food_sec isn't derivable from this 2-category variable -> NA.
# =============================================================================
df <- df |>
  mutate(
    food_insecure     = if_else(HHSHARE == 1 & !is.na(FOODSEC_STATUS_CAT),
                                 as.numeric(FOODSEC_STATUS_CAT == 2), NA_real_),
    very_low_food_sec = NA_real_
  )
cat(sprintf("food_insecure non-missing: %s  prevalence: %.3f\n",
            format(sum(!is.na(df$food_insecure)), big.mark = ","), mean(df$food_insecure, na.rm = TRUE)))

# =============================================================================
# DiD VARIABLES
# post/did start on a financial-year basis (all of FY2022/23 = post); the
# block below overwrites `post` for FY2022/23 with the exact 14 Nov 2022 SCP
# cutoff if an FRS household file with interview dates is available.
# =============================================================================
df <- df |>
  mutate(
    post    = as.numeric(YEAR >= SCP_EXPAND_YEAR),
    treated = scotland,
    did     = treated * post
  )

# =============================================================================
# EXACT POST CUTOFF (14 Nov 2022) -- overwrites `post` for FY2022/23 only
# HBAI's harmonised extract has no interview-date variable, so this merges in
# UKDA-9252 (FRS 2022/23) household file if present. No-op if not found.
# =============================================================================
FRS_ROOT <- file.path(DATA_ROOT, "UKDA-9252-tab", "tab")
hhold_pattern <- "^(hhold|househol|hhld|household)(_v[0-9]+)?\\.tab$"
hhold_found <- if (dir.exists(FRS_ROOT)) {
  list.files(FRS_ROOT, pattern = hhold_pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
} else {
  character(0)
}

# Prefer a "v2" (corrected re-release) path if one exists
hhold_path <- if (length(hhold_found) == 0) {
  NA_character_
} else if (any(grepl("v2", hhold_found, ignore.case = TRUE))) {
  hhold_found[grepl("v2", hhold_found, ignore.case = TRUE)][1]
} else {
  hhold_found[1]
}

if (is.na(hhold_path)) {
  cat(sprintf("\nFRS household file for FY2022/23 not found under %s -- `post` stays FY-based.\n", FRS_ROOT))
} else {
  cat(sprintf("\nUsing: %s\n", hhold_path))
  hh <- read_tsv(hhold_path, show_col_types = FALSE, col_types = cols(.default = col_character()))
  names(hh) <- toupper(trimws(names(hh)))

  date_col <- intersect(c("INTDATE", "INTDATM", "INTDAT"), names(hh))
  if (length(date_col) == 0) {
    cat("  No column named INTDATE/INTDATM/INTDAT found -- `post` stays FY-based.\n")
  } else {
    raw_dates <- hh[[date_col[1]]]

    # UKDA has used both MM/DD/YYYY text and a numeric day-count (origin
    # 1960-01-01) across versions of this file -- detect which one this is.
    looks_like_slash_date <- any(grepl("/", raw_dates), na.rm = TRUE)
    if (looks_like_slash_date) {
      hh_dates <- hh |>
        transmute(
          SERNUM  = suppressWarnings(as.numeric(SERNUM)),
          intdate = suppressWarnings(as.Date(raw_dates, format = "%m/%d/%Y"))
        )
    } else {
      hh_dates <- hh |>
        transmute(
          SERNUM  = suppressWarnings(as.numeric(SERNUM)),
          intdate = suppressWarnings(as.Date(as.numeric(raw_dates), origin = "1960-01-01"))
        )
    }
    hh_dates <- hh_dates |> filter(!is.na(SERNUM), !is.na(intdate))
    cat(sprintf("  Parsed %s of %s rows to valid dates.\n", nrow(hh_dates), nrow(hh)))

    df <- df |>
      left_join(hh_dates, by = "SERNUM") |>
      mutate(
        post = if_else(YEAR == 2023 & !is.na(intdate),
                        as.numeric(intdate >= as.Date("2022-11-14")),
                        post),
        did  = treated * post
      )
    cat(sprintf("  FY2022/23 rows reclassified: %s Pre, %s Post (of %s total in that year)\n",
                format(sum(df$YEAR == 2023 & df$post == 0), big.mark = ","),
                format(sum(df$YEAR == 2023 & df$post == 1), big.mark = ","),
                format(sum(df$YEAR == 2023), big.mark = ",")))
  }
}

# =============================================================================
# SAVE
# =============================================================================
cat(sprintf("\n%s\nSAMPLE BY YEAR AND GROUP\n", strrep("=", 60)))
print(df |> count(YEAR, scotland) |> pivot_wider(names_from = scotland, values_from = n, names_prefix = "scotland_"))

dir.create(dirname(HBAI_OUT), showWarnings = FALSE, recursive = TRUE)
write_csv(df, HBAI_OUT)
cat(sprintf("\nSaved to %s\n  Shape: %d rows x %d cols\n", HBAI_OUT, nrow(df), ncol(df)))
