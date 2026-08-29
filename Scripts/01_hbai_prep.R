# =============================================================================
# 01_hbai_prep.R
# HBAI data prep for the Scottish Child Payment dissertation.
#
# Builds BOTH extracts the rest of the pipeline needs, from one shared load
# of the raw HBAI files:
#   - hbai_clean.csv          England vs Scotland only. Everything downstream
#                             (03, 04, 05, 05b, 05c, 06, 06b, 07, 08, 11,
#                             12) reads this one.
#   - hbai_clean_placebo.csv  all four UK nations. Only 09_placebo_wales_ni.R
#                             reads this
#
# Key choices:
#   - Scotland/England/Wales/NI defined via COUNTRY (1/2/3/4).
#   - food_insecure uses FOODSEC_STATUS_CAT (official DWP category), HHSHARE==1 only.
#   - Primary deprivation outcome is MDCH (official DWP flag), not a homemade item count.
#   - mdch_observed = !is.na(MDCH).
#   - Household interview dates come from the RAW FRS household files (HBAI's
#     harmonised extract has no interview-date field), used to (a) set the
#     exact 14 Nov 2022 SCP post cutoff and (b) add CASE-style rolling
#     13-week quarter bins (Andersen, Nesom, Patrick, Pinter, Stewart &
#     Tominey 2025, CASE paper 238, Table A3) for 05c.
# =============================================================================

library(tidyverse)

# ---- Paths ------------------------------------------------------------------
DATA_ROOT   <- "/Users/guypigott/python-venv-demo/Dissertation"
HBAI_ROOT   <- file.path(DATA_ROOT, "UKDA-5828-tab", "tab", "23-24prices")
OUT_MAIN    <- file.path(DATA_ROOT, "data", "hbai_clean.csv")
OUT_PLACEBO <- file.path(DATA_ROOT, "data", "hbai_clean_placebo.csv")

HBAI_FILES <- c(
  "i1518e_2324prices.tab",   # 2015/16 - 2017/18
  "i1821e_2324prices.tab",   # 2018/19 - 2020/21
  "i2124e_2324prices.tab"    # 2021/22 - 2023/24
)

SCP_EXPAND_YEAR <- 2023   # FY 2022/23: SCP extended to all under-16s, £25/week

# 2 of 12 MDCH items dropped (MDCH_LES, MDCH_ACT: sparse/uncertain coverage)
MDCH_ITEMS <- c("MDCH_BED", "MDCH_CEL", "MDCH_COAT", "MDCH_EQP", "MDCH_HOL",
                "MDCH_PLAY", "MDCH_PLY", "MDCH_TEA", "MDCH_TRP", "MDCH_VEG")

# Identifiers, geography, outcomes, and DML covariates (wide set used in 06b)
KEEP_VARS <- c(
  "SERNUM", "BENUNIT", "PERSON", "YEAR_CODE",
  "COUNTRY", "GVTREGN",
  "AGE",
  "MDCH", MDCH_ITEMS, "MDCHDMP",
  "LOWINCMDCH", "LOWINCMDCHSEV",
  "FOODSEC", "FOODSEC_STATUS_CAT", "HHSHARE",
  "GS_INDCH",
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

# GVTREGN region groupings, used for a diagnostic + the northern_england
# flag (England subgroup, not a treatment definition)
ENGLAND_REGIONS  <- c(1, 2, 4, 5, 6, 7, 8, 9, 10)
SCOTLAND_REGION  <- 12
NORTHERN_ENGLAND <- c(1, 2, 4)

# Raw FRS household files, one per survey year, for the exact interview-date cutoff
FRS_YEARS <- c(
  "UKDA-8336" = 2017,   # 2016-17
  "UKDA-8460" = 2018,   # 2017-18
  "UKDA-8633" = 2019,   # 2018-19
  "UKDA-8802" = 2020,   # 2019-20
  "UKDA-8948" = 2021,   # 2020-21 (COVID year -- already excluded below, kept for completeness/no-op)
  "UKDA-9073" = 2022,   # 2021-22 (SCP introduced Feb 2021)
  "UKDA-9252" = 2023,   # 2022-23 (SCP expanded Nov 2022 -- the year this actually matters for)
  "UKDA-9367" = 2024    # 2023-24
)
HHOLD_PATTERN <- "^(hhold|househol|hhld|household)(_v[0-9]+)?\\.tab$"

# =============================================================================
# HELPERS
# =============================================================================

# MDCH item codes: 1=has item -> 0 (not deprived); 2=wants but can't afford ->
# 1 (deprived); 0/3/-9/.A = not asked / doesn't want / missing -> NA
recode_mdch_item <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  case_when(x == 1 ~ 0, x == 2 ~ 1, TRUE ~ NA_real_)
}

# Finds one year's raw household .tab file (prefers the _v2 revision if there is one)
find_hhold <- function(ukda_folder) {
  tab_root <- file.path(DATA_ROOT, paste0(ukda_folder, "-tab"), "tab")
  if (!dir.exists(tab_root)) return(NA_character_)
  found <- list.files(tab_root, pattern = HHOLD_PATTERN, recursive = TRUE,
                      full.names = TRUE, ignore.case = TRUE)
  if (length(found) == 0) return(NA_character_)
  if (any(grepl("v2", found, ignore.case = TRUE))) found[grepl("v2", found, ignore.case = TRUE)][1] else found[1]
}

# Pulls SERNUM + interview date out of one year's household file. UKDA has
# used both a MM/DD/YYYY text date and a numeric day-count (origin
# 1960-01-01) across versions of this file -- detect which one this is.
parse_hhold_dates <- function(path, year_int) {
  cat(sprintf("  Using: %s (FYE %d)\n", path, year_int))
  hh <- read_tsv(path, show_col_types = FALSE, col_types = cols(.default = col_character()))
  names(hh) <- toupper(trimws(names(hh)))

  date_col <- intersect(c("INTDATE", "INTDATM", "INTDAT"), names(hh))
  if (length(date_col) == 0) {
    cat("    No INTDATE/INTDATM/INTDAT column -- skipping this year.\n")
    return(NULL)
  }
  raw_dates <- hh[[date_col[1]]]
  looks_like_slash_date <- any(grepl("/", raw_dates), na.rm = TRUE)
  out <- if (looks_like_slash_date) {
    hh |> transmute(SERNUM  = suppressWarnings(as.numeric(SERNUM)),
                     intdate = suppressWarnings(as.Date(raw_dates, format = "%m/%d/%Y")))
  } else {
    hh |> transmute(SERNUM  = suppressWarnings(as.numeric(SERNUM)),
                     intdate = suppressWarnings(as.Date(as.numeric(raw_dates), origin = "1960-01-01")))
  }
  out <- out |> filter(!is.na(SERNUM), !is.na(intdate))
  out$YEAR <- year_int
  cat(sprintf("    Parsed %s of %s rows to valid dates.\n", nrow(out), nrow(hh)))
  out
}

# CASE (2025) Table A3 rolling-quarter bins: 13-week windows anchored on the
# 14th of Feb/May/Aug/Nov. Confirmed against the
# actual PDF's row labels (e.g. "Scot * 14th November 2022 - 13th February 2023").
assign_quarter <- function(d) {
  if (is.na(d)) return(c(label = NA_character_, start = NA_character_))
  anchor_mmdd <- c("02-14", "05-14", "08-14", "11-14")
  yr <- as.integer(format(d, "%Y"))
  candidates <- sort(as.Date(paste0(rep((yr - 1):(yr + 1), each = 4), "-", anchor_mmdd)))
  start <- max(candidates[candidates <= d])
  end   <- min(candidates[candidates > d]) - 1
  c(label = sprintf("%s_%s", format(start, "%Y-%m-%d"), format(end, "%Y-%m-%d")),
    start = as.character(start))
}

# Builds a SERNUM+YEAR -> interview date (+ CASE quarter bin) lookup across
# every FRS year we have a raw download for. Joining on SERNUM+YEAR together,
# not SERNUM alone, since household ID numbers are only guaranteed unique
# within one year's file, not across years. Returns NULL if no household
# files were found at all (e.g. FY2016/17 has none in this project).
build_interview_dates <- function() {
  hh_dates_list <- list()
  for (ukda_folder in names(FRS_YEARS)) {
    year_int <- FRS_YEARS[[ukda_folder]]
    path <- find_hhold(ukda_folder)
    if (is.na(path)) {
      cat(sprintf("  ! %s (FYE %d): household file not found -- skipping.\n", ukda_folder, year_int))
      next
    }
    parsed <- parse_hhold_dates(path, year_int)
    if (!is.null(parsed) && nrow(parsed) > 0) hh_dates_list[[ukda_folder]] <- parsed
  }
  if (length(hh_dates_list) == 0) return(NULL)

  hh_dates <- bind_rows(hh_dates_list)
  cat(sprintf("\nCombined interview dates: %s rows across %d FRS years.\n",
              format(nrow(hh_dates), big.mark = ","), length(hh_dates_list)))

  q <- t(vapply(hh_dates$intdate, assign_quarter, character(2)))
  hh_dates$quarter_label <- q[, "label"]
  hh_dates$quarter_start <- as.Date(q[, "start"])
  quarter_levels <- sort(unique(hh_dates$quarter_start[!is.na(hh_dates$quarter_start)]))
  hh_dates$quarter_index <- match(hh_dates$quarter_start, quarter_levels)
  cat(sprintf("  %d distinct quarter windows: %s to %s\n",
              length(quarter_levels), min(quarter_levels), max(quarter_levels)))

  dupes <- hh_dates |> count(SERNUM, YEAR) |> filter(n > 1)
  if (nrow(dupes) > 0) {
    cat(sprintf("  *** WARNING: %d SERNUM+YEAR combos appear more than once -- merge will duplicate rows for these. Investigate. ***\n", nrow(dupes)))
  }
  hh_dates
}

# Refines `post`/`did` to the exact 14-Nov-2022 cutoff for FY2022/23 rows
# (every other year keeps the financial-year-based post) and attaches the
# CASE quarter columns. No-op if hh_dates is NULL.
apply_interview_dates <- function(df, hh_dates) {
  if (is.null(hh_dates)) {
    cat("  No FRS household files loaded -- `post` stays FY-based, no quarter columns added.\n")
    return(df)
  }
  df |>
    left_join(hh_dates, by = c("SERNUM", "YEAR")) |>
    mutate(
      post = if_else(YEAR == 2023 & !is.na(intdate),
                      as.numeric(intdate >= as.Date("2022-11-14")), post),
      did  = treated * post
    )
}

# =============================================================================
# LOAD AND STACK
# =============================================================================

all_dfs <- list()

for (fname in HBAI_FILES) {
  fpath <- file.path(HBAI_ROOT, fname)
  if (!file.exists(fpath)) {
    cat(sprintf("  x NOT FOUND: %s\n", fpath))
    next
  }

  cat(sprintf("\nLoading %s...\n", fname))
  # Read as character first, then coerce.
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
    if (length(unique(na.omit(gvtregn_vals))) <= 20 && max(gvtregn_vals, na.rm = TRUE) <= 20) {
      cat("  GVTREGN looks like clean region codes; no swap.\n")
    } else if (length(unique(na.omit(gsindwa_vals))) <= 20 && max(gsindwa_vals, na.rm = TRUE) <= 20) {
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

  # Country filter.
  d <- d |> filter(COUNTRY %in% c(1, 2, 3, 4))
  cat(sprintf("  After geo filter - England: %s, Wales: %s, Scotland: %s, NI: %s\n",
              format(sum(d$COUNTRY == 1), big.mark = ","), format(sum(d$COUNTRY == 2), big.mark = ","),
              format(sum(d$COUNTRY == 3), big.mark = ","), format(sum(d$COUNTRY == 4), big.mark = ",")))

  # Avoid negative codes -> NA (valid category codes are always >= 0)
  d <- d |> mutate(across(where(is.numeric), ~ if_else(.x < 0, NA_real_, .x)))

  all_dfs[[fname]] <- d
  cat(sprintf("  -> %s rows retained\n", format(nrow(d), big.mark = ",")))
}

if (length(all_dfs) == 0) stop(sprintf("No HBAI files loaded. Check HBAI_ROOT: %s", HBAI_ROOT))

df <- bind_rows(all_dfs)
cat(sprintf("\n%s\nCombined (all 4 nations): %s rows, years: %s\n", strrep("=", 60),
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
# DIAGNOSTICS
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
# RECODE DEPRIVATION ITEMS + FOOD SECURITY
# =============================================================================
available_items <- intersect(MDCH_ITEMS, names(df))
missing_items    <- setdiff(MDCH_ITEMS, names(df))
cat(sprintf("\nMDCH items available: %d/%d\n", length(available_items), length(MDCH_ITEMS)))
if (length(missing_items) > 0) cat(sprintf("  Missing: %s\n", paste(missing_items, collapse = ", ")))

df <- df |> mutate(across(all_of(available_items), recode_mdch_item))

# Approximate item-count severity measure. mdch_severe threshold (>5 of 10) is scaled from Eurostat's
# official severe-deprivation indicator (7 of 13 items, ~54%) onto
# 10-item scale (~5.4 items).
item_mat <- as.matrix(df[available_items])
n_observed <- rowSums(!is.na(item_mat))
df$mdch_count <- ifelse(n_observed == 0, NA_real_, rowSums(item_mat, na.rm = TRUE))
df <- df |>
  mutate(
    mdch_any    = if_else(!is.na(mdch_count), as.numeric(mdch_count > 0), NA_real_),
    mdch_severe = if_else(!is.na(mdch_count), as.numeric(mdch_count > 5), NA_real_)
  )
cat(sprintf("Approx mdch_count non-missing: %s (mdch_any prevalence %.3f)\n",
            format(sum(!is.na(df$mdch_count)), big.mark = ","), mean(df$mdch_any, na.rm = TRUE)))

# Official DWP deprivation flag -- primary outcome. Already benefit-unit-level.
df <- df |> mutate(MDCH = if_else(MDCH %in% c(0, 1), MDCH, NA_real_))
df <- df |> mutate(mdch_observed = as.numeric(!is.na(MDCH)))
cat(sprintf("Official MDCH non-missing: %s (prevalence %.3f)\n",
            format(sum(df$mdch_observed), big.mark = ","), mean(df$MDCH, na.rm = TRUE)))

# food_insecure = FOODSEC_STATUS_CAT==2 (official DWP category), restricted to
# HHSHARE==1 per the variable guide. Structurally unavailable before FYE2020
# (confirmed via table(df$YEAR, !is.na(food_insecure)): FYE2017-19 are 100%
# NA), so its pre-period is effectively FYE2020 only, not FYE2017-2020 like
# the other outcomes -- see the Methodology note on identification window
# before interpreting this estimate. very_low_food_sec isn't derivable from
# this 2-category variable -> always NA.
df <- df |>
  mutate(
    food_insecure     = if_else(HHSHARE == 1 & !is.na(FOODSEC_STATUS_CAT),
                                 as.numeric(FOODSEC_STATUS_CAT == 2), NA_real_),
    very_low_food_sec = NA_real_
  )
cat(sprintf("food_insecure non-missing: %s  prevalence: %.3f\n",
            format(sum(!is.na(df$food_insecure)), big.mark = ","), mean(df$food_insecure, na.rm = TRUE)))

# =============================================================================
# INTERVIEW DATES -- build once, reuse for both extracts below
# =============================================================================
cat("\n--- Building interview-date lookup from raw FRS household files ---\n")
hh_dates <- build_interview_dates()

# =============================================================================
# EXTRACT 1: ENGLAND + SCOTLAND -> hbai_clean.csv
# =============================================================================
cat(sprintf("\n%s\nBUILDING MAIN EXTRACT (England + Scotland)\n%s\n", strrep("=", 60), strrep("=", 60)))

df_main <- df |>
  filter(COUNTRY %in% c(1, 3)) |>
  mutate(
    scotland         = as.numeric(COUNTRY == 3),
    northern_england = as.numeric(scotland == 0 & GVTREGN %in% NORTHERN_ENGLAND),
    post             = as.numeric(YEAR >= SCP_EXPAND_YEAR),
    treated          = scotland,
    did              = treated * post
  )
cat(sprintf("Scotland: %s, England: %s\n",
            format(sum(df_main$scotland == 1), big.mark = ","), format(sum(df_main$scotland == 0), big.mark = ",")))

df_main <- apply_interview_dates(df_main, hh_dates)
cat(sprintf("  FY2022/23 rows reclassified: %s Pre, %s Post (of %s total in that year)\n",
            format(sum(df_main$YEAR == 2023 & df_main$post == 0), big.mark = ","),
            format(sum(df_main$YEAR == 2023 & df_main$post == 1), big.mark = ","),
            format(sum(df_main$YEAR == 2023), big.mark = ",")))
if ("quarter_label" %in% names(df_main)) {
  cat("  Quarter match rate by year (0%% = no FRS raw download for that year):\n")
  print(df_main |> group_by(YEAR) |> summarise(matched = mean(!is.na(quarter_label)), .groups = "drop"))
}

cat(sprintf("\n%s\nSAMPLE BY YEAR AND GROUP (main extract)\n", strrep("=", 60)))
print(df_main |> count(YEAR, scotland) |> pivot_wider(names_from = scotland, values_from = n, names_prefix = "scotland_"))

dir.create(dirname(OUT_MAIN), showWarnings = FALSE, recursive = TRUE)
write_csv(df_main, OUT_MAIN)
cat(sprintf("\nSaved to %s\n  Shape: %d rows x %d cols\n", OUT_MAIN, nrow(df_main), ncol(df_main)))

# =============================================================================
# EXTRACT 2: ALL FOUR NATIONS -> hbai_clean_placebo.csv
# Only 09_placebo_wales_ni.R reads this. treated/post/did below are Scotland-
# specific -- they don't drive the actual Wales/NI checks. That script builds
# its own treated/tp for each comparison (Wales vs England, NI vs England),
# since Wales and NI never got SCP and need their own placebo logic.
# =============================================================================
cat(sprintf("\n%s\nBUILDING PLACEBO EXTRACT (all four nations)\n%s\n", strrep("=", 60), strrep("=", 60)))

df_placebo <- df |>
  mutate(
    scotland  = as.numeric(COUNTRY == 3),
    wales     = as.numeric(COUNTRY == 2),
    ni        = as.numeric(COUNTRY == 4),
    country_f = factor(COUNTRY, levels = c(1, 2, 3, 4), labels = c("England", "Wales", "Scotland", "NI")),
    post      = as.numeric(YEAR >= SCP_EXPAND_YEAR),
    treated   = scotland,
    did       = treated * post
  )

df_placebo <- apply_interview_dates(df_placebo, hh_dates)
cat(sprintf("  FY2022/23 rows reclassified: %s Pre, %s Post (of %s total in that year)\n",
            format(sum(df_placebo$YEAR == 2023 & df_placebo$post == 0), big.mark = ","),
            format(sum(df_placebo$YEAR == 2023 & df_placebo$post == 1), big.mark = ","),
            format(sum(df_placebo$YEAR == 2023), big.mark = ",")))

cat("\nOfficial MDCH prevalence BY COUNTRY (sanity check before the placebo regressions):\n")
print(df_placebo |> filter(mdch_observed == 1) |> group_by(country_f) |>
        summarise(prevalence = mean(MDCH), n = n(), .groups = "drop"))

# FYE2024's old/new MDCH question-design split isn't even across nations --
# NI's harmonised extract is 100% new-design (a NISRA sample-size decision)
# vs ~27-30% old-design for GB nations, so NI's flag is on a different
# vintage of the question than everyone else that one year.
if ("MDCHDMP" %in% names(df_placebo)) {
  cat("\nFYE2024 old/new MDCH question-design split BY COUNTRY (MDCHDMP: 1=old, 2=new):\n")
  print(df_placebo |> filter(YEAR == 2024, !is.na(MDCHDMP)) |> count(country_f, MDCHDMP) |>
          pivot_wider(names_from = MDCHDMP, values_from = n, names_prefix = "MDCHDMP_"))
}

dir.create(dirname(OUT_PLACEBO), showWarnings = FALSE, recursive = TRUE)
write_csv(df_placebo, OUT_PLACEBO)
cat(sprintf("\nSaved to %s\n  Shape: %d rows x %d cols\n", OUT_PLACEBO, nrow(df_placebo), ncol(df_placebo)))
