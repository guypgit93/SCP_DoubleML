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
#   - Also pulls household interview date from the RAW FRS household files
#     (across all available FRS years, not just FY2022/23) to (a) set the
#     exact 14 Nov 2022 SCP post cutoff and (b) add quarter_label/
#     quarter_start/quarter_index columns replicating CASE (Andersen, Nesom,
#     Patrick, Pinter, Stewart & Tominey 2025, CASE paper 238) Table A3's
#     rolling 13-week quarters, for use by 05c_stage1_parallel_trends_quarterly.R.
#     HBAI's own harmonised extract carries no interview-date field.
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
# HHSHARE==1 per the variable guide. NAs left as-is (genuine non-response, plus
# FOODSEC_STATUS_CAT is structurally unavailable before FYE2020 -- confirmed via
# table(df$YEAR, !is.na(food_insecure)): FYE2017-2019 are 100% NA, FYE2020 is
# ~100% observed (n=7,653), FYE2021 already excluded pipeline-wide (Covid), and
# FYE2022-2024 are ~100% observed. So the food-insecurity DiD's pre-period is
# effectively FYE2020 only, not FYE2017-2020 as for the other outcomes -- see
# Methodology note on identification window before interpreting this estimate.
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
# INTERVIEW DATE (all years) -- exact 14 Nov 2022 post cutoff + CASE-style
# quarter bins
# HBAI's harmonised extract has no interview-date variable, so this pulls
# INTDATE from each year's RAW FRS household file and merges by SERNUM+YEAR
# (not SERNUM alone -- SERNUM isn't guaranteed unique across FRS years, only
# safe here because each year is parsed and tagged with YEAR before
# stacking). No-op (columns stay NA) for any year whose household file isn't
# found, e.g. FY2016/17 has no FRS raw download in this project.
#
# Used for two things:
#   (a) `post`/`did` get the exact 14 Nov 2022 cutoff for FY2022/23 (as
#       before -- other years keep the FY-based post/did, that choice is
#       unchanged)
#   (b) quarter_label/quarter_start/quarter_index -- rolling 13-week windows
#       anchored on the 14th of Feb/May/Aug/Nov, replicating CASE (Andersen,
#       Nesom, Patrick, Pinter, Stewart & Tominey 2025, CASE paper 238)
#       Table A3 exactly (confirmed against the actual PDF's row labels, e.g.
#       "Scot * 14th November 2022 - 13th February 2023"). Consumed by
#       05c_stage1_parallel_trends_quarterly.R.
# =============================================================================
FRS_YEARS <- c(
  "UKDA-8336" = 2017,   # 2016-17
  "UKDA-8460" = 2018,   # 2017-18
  "UKDA-8633" = 2019,   # 2018-19
  "UKDA-8802" = 2020,   # 2019-20
  "UKDA-8948" = 2021,   # 2020-21 (COVID year -- already excluded from df above, kept here for completeness/no-op)
  "UKDA-9073" = 2022,   # 2021-22 (SCP introduced Feb 2021)
  "UKDA-9252" = 2023,   # 2022-23 (SCP expanded Nov 2022)
  "UKDA-9367" = 2024    # 2023-24
)
hhold_pattern <- "^(hhold|househol|hhld|household)(_v[0-9]+)?\\.tab$"

find_hhold <- function(ukda_folder) {
  tab_root <- file.path(DATA_ROOT, paste0(ukda_folder, "-tab"), "tab")
  if (!dir.exists(tab_root)) return(NA_character_)
  found <- list.files(tab_root, pattern = hhold_pattern, recursive = TRUE,
                      full.names = TRUE, ignore.case = TRUE)
  if (length(found) == 0) return(NA_character_)
  if (any(grepl("v2", found, ignore.case = TRUE))) found[grepl("v2", found, ignore.case = TRUE)][1] else found[1]
}

parse_hhold_dates <- function(path, year_int) {
  cat(sprintf("  Using: %s (FYE %d)\n", path, year_int))
  hh <- read_tsv(path, show_col_types = FALSE, col_types = cols(.default = col_character()))
  names(hh) <- toupper(trimws(names(hh)))

  date_col <- intersect(c("INTDATE", "INTDATM", "INTDAT"), names(hh))
  if (length(date_col) == 0) {
    cat("    No column named INTDATE/INTDATM/INTDAT found -- skipping this year.\n")
    return(NULL)
  }
  raw_dates <- hh[[date_col[1]]]

  # UKDA has used both MM/DD/YYYY text and a numeric day-count (origin
  # 1960-01-01) across versions of this file -- detect which one this is.
  looks_like_slash_date <- any(grepl("/", raw_dates), na.rm = TRUE)
  if (looks_like_slash_date) {
    out <- hh |> transmute(SERNUM  = suppressWarnings(as.numeric(SERNUM)),
                           intdate = suppressWarnings(as.Date(raw_dates, format = "%m/%d/%Y")))
  } else {
    out <- hh |> transmute(SERNUM  = suppressWarnings(as.numeric(SERNUM)),
                           intdate = suppressWarnings(as.Date(as.numeric(raw_dates), origin = "1960-01-01")))
  }
  out <- out |> filter(!is.na(SERNUM), !is.na(intdate))
  out$YEAR <- year_int
  cat(sprintf("    Parsed %s of %s rows to valid dates.\n", nrow(out), nrow(hh)))
  out
}

hh_dates_list <- list()
for (ukda_folder in names(FRS_YEARS)) {
  year_int <- FRS_YEARS[[ukda_folder]]
  path <- find_hhold(ukda_folder)
  if (is.na(path)) {
    cat(sprintf("  ! %s (FYE %d): household file not found under %s-tab/tab/ -- skipping.\n",
                ukda_folder, year_int, ukda_folder))
    next
  }
  parsed <- parse_hhold_dates(path, year_int)
  if (!is.null(parsed) && nrow(parsed) > 0) hh_dates_list[[ukda_folder]] <- parsed
}

if (length(hh_dates_list) == 0) {
  cat("\nNo FRS household files loaded -- `post` stays FY-based, no quarter columns added.\n")
} else {
  hh_dates <- bind_rows(hh_dates_list)
  cat(sprintf("\nCombined interview dates: %s rows across %d FRS years.\n",
              format(nrow(hh_dates), big.mark = ","), length(hh_dates_list)))

  # CASE (2025) Table A3 rolling-quarter bins: 13-week windows anchored on
  # the 14th of Feb/May/Aug/Nov -- NOT calendar quarters.
  anchor_mmdd <- c("02-14", "05-14", "08-14", "11-14")
  assign_quarter <- function(d) {
    if (is.na(d)) return(c(label = NA_character_, start = NA_character_))
    yr <- as.integer(format(d, "%Y"))
    candidates <- sort(as.Date(paste0(rep((yr - 1):(yr + 1), each = 4), "-", anchor_mmdd)))
    start <- max(candidates[candidates <= d])
    end   <- min(candidates[candidates > d]) - 1
    c(label = sprintf("%s_%s", format(start, "%Y-%m-%d"), format(end, "%Y-%m-%d")),
      start = as.character(start))
  }
  q <- t(vapply(hh_dates$intdate, assign_quarter, character(2)))
  hh_dates$quarter_label <- q[, "label"]
  hh_dates$quarter_start <- as.Date(q[, "start"])
  quarter_levels <- sort(unique(hh_dates$quarter_start[!is.na(hh_dates$quarter_start)]))
  hh_dates$quarter_index <- match(hh_dates$quarter_start, quarter_levels)
  cat(sprintf("  %d distinct quarter windows: %s to %s\n",
              length(quarter_levels), min(quarter_levels), max(quarter_levels)))

  dupes <- hh_dates |> count(SERNUM, YEAR) |> filter(n > 1)
  if (nrow(dupes) > 0) {
    cat(sprintf("  *** WARNING: %d SERNUM+YEAR combos appear more than once in the household files",
                nrow(dupes)), " -- merge below will duplicate rows for these. Investigate. ***\n")
  }

  df <- df |>
    left_join(hh_dates, by = c("SERNUM", "YEAR")) |>
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
  cat("  Quarter match rate by year (0%% = no FRS raw download for that year):\n")
  print(df |> group_by(YEAR) |> summarise(matched = mean(!is.na(quarter_label)), .groups = "drop"))
}

# =============================================================================
# SAVE
# =============================================================================
cat(sprintf("\n%s\nSAMPLE BY YEAR AND GROUP\n", strrep("=", 60)))
print(df |> count(YEAR, scotland) |> pivot_wider(names_from = scotland, values_from = n, names_prefix = "scotland_"))

dir.create(dirname(HBAI_OUT), showWarnings = FALSE, recursive = TRUE)
write_csv(df, HBAI_OUT)
cat(sprintf("\nSaved to %s\n  Shape: %d rows x %d cols\n", HBAI_OUT, nrow(df), ncol(df)))
