# =============================================================================
# 01b_hbai_prep_placebo.R
# WALES/NI PLACEBO EXTENSION -- near-identical clone of 01_hbai_prep.R, with
# the geography filter relaxed from England+Scotland only to all four UK
# nations, saved to a SEPARATE output file (hbai_clean_placebo.csv).
#
# WHY A SEPARATE SCRIPT/FILE RATHER THAN EDITING 01_hbai_prep.R IN PLACE:
# every one of the six main pipeline stages (03/04/05/06/06b/07/09) reads
# hbai_clean.csv and is validated against England-only-control results
# already written up in results_narrative.docx. Relaxing the filter in
# 01_hbai_prep.R itself would silently change what every downstream script
# sees on its next run -- a real risk this close to the deadline. This
# script instead builds a SEPARATE, wider file used only by
# 10_placebo_wales_ni.R; hbai_clean.csv and every script that reads it are
# completely untouched.
#
# Everything else (MDCH item recoding, food_insecure, the exact 14-Nov-2022
# `post` cutoff refinement) is IDENTICAL to 01_hbai_prep.R -- see that
# script for the full rationale on each of those steps, not repeated here.
# =============================================================================

library(tidyverse)

# ── Paths ────────────────────────────────────────────────────────────────────
DATA_ROOT <- "/Users/guypigott/python-venv-demo/Dissertation"
HBAI_ROOT <- file.path(DATA_ROOT, "UKDA-5828-tab", "tab", "23-24prices")
HBAI_OUT  <- file.path(DATA_ROOT, "data", "hbai_clean_placebo.csv")   # SEPARATE from hbai_clean.csv

HBAI_FILES <- c(
  "i1518e_2324prices.tab",
  "i1821e_2324prices.tab",
  "i2124e_2324prices.tab"
)

SCP_EXPAND_YEAR <- 2023

MDCH_ITEMS <- c("MDCH_BED", "MDCH_CEL", "MDCH_COAT", "MDCH_EQP", "MDCH_HOL",
                "MDCH_PLAY", "MDCH_PLY", "MDCH_TEA", "MDCH_TRP", "MDCH_VEG")

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

recode_mdch_item <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  dplyr::case_when(x == 1 ~ 0, x == 2 ~ 1, TRUE ~ NA_real_)
}

all_dfs <- list()

for (fname in HBAI_FILES) {
  fpath <- file.path(HBAI_ROOT, fname)
  if (!file.exists(fpath)) { cat(sprintf("  x NOT FOUND: %s\n", fpath)); next }

  cat(sprintf("\nLoading %s...\n", fname))
  raw <- read_tsv(fpath, show_col_types = FALSE, col_types = cols(.default = col_character()))
  names(raw) <- toupper(trimws(names(raw)))

  if ("SERNUM" %in% names(raw)) {
    raw$SERNUM <- vapply(strsplit(raw$SERNUM, "\t", fixed = TRUE), tail, character(1), n = 1)
  }
  if ("YEAR" %in% names(raw)) raw <- raw |> rename(YEAR_CODE = YEAR)

  if (all(c("GVTREGN", "GS_INDWA") %in% names(raw))) {
    gvtregn_vals <- suppressWarnings(as.numeric(raw$GVTREGN))
    gsindwa_vals <- suppressWarnings(as.numeric(raw$GS_INDWA))
    if (length(unique(na.omit(gvtregn_vals))) <= 20 && max(gvtregn_vals, na.rm = TRUE) <= 20) {
      # GVTREGN already clean, no swap
    } else if (length(unique(na.omit(gsindwa_vals))) <= 20 && max(gsindwa_vals, na.rm = TRUE) <= 20) {
      raw <- raw |> rename(GVTREGN_RAW = GVTREGN, GVTREGN = GS_INDWA)
    }
  }

  keep <- intersect(KEEP_VARS, names(raw))
  if ("MDCHDMP" %in% names(raw)) keep <- union(keep, "MDCHDMP")
  d <- raw[, keep]
  d <- d |> mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))

  if (!"YEAR_CODE" %in% names(d)) stop(sprintf("YEAR_CODE not found in %s", fname))
  d <- d |> mutate(YEAR = YEAR_CODE + 1994)
  if (all(is.na(d$YEAR))) stop(sprintf("YEAR is entirely NA after parsing %s", fname))

  # ── GEOGRAPHY FILTER: ALL FOUR UK NATIONS (1=Eng, 2=Wales, 3=Scot, 4=NI) ──
  # This is the ONE substantive difference from 01_hbai_prep.R.
  d <- d |> filter(COUNTRY %in% c(1, 2, 3, 4))
  d <- d |>
    mutate(
      scotland = as.numeric(COUNTRY == 3),
      wales    = as.numeric(COUNTRY == 2),
      ni       = as.numeric(COUNTRY == 4),
      country_f = factor(COUNTRY, levels = c(1, 2, 3, 4),
                          labels = c("England", "Wales", "Scotland", "NI"))
    )
  cat(sprintf("  After geo filter - England: %s, Wales: %s, Scotland: %s, NI: %s\n",
              format(sum(d$COUNTRY == 1), big.mark = ","),
              format(sum(d$COUNTRY == 2), big.mark = ","),
              format(sum(d$COUNTRY == 3), big.mark = ","),
              format(sum(d$COUNTRY == 4), big.mark = ",")))

  d <- d |> mutate(across(where(is.numeric), ~ if_else(.x < 0, NA_real_, .x)))
  all_dfs[[fname]] <- d
  cat(sprintf("  -> %s rows retained\n", format(nrow(d), big.mark = ",")))
}

if (length(all_dfs) == 0) stop(sprintf("No HBAI files loaded. Check HBAI_ROOT: %s", HBAI_ROOT))

df <- bind_rows(all_dfs)
cat(sprintf("\nCombined: %s rows, years: %s\n",
            format(nrow(df), big.mark = ","), paste(sort(unique(df$YEAR)), collapse = ", ")))

df <- df |> filter(AGE <= 16)
df <- df |> filter(YEAR_CODE >= 23)
df <- df |> filter(YEAR != 2021)

available_items <- intersect(MDCH_ITEMS, names(df))
df <- df |> mutate(across(all_of(available_items), recode_mdch_item))

item_mat <- as.matrix(df[available_items])
n_observed <- rowSums(!is.na(item_mat))
df$mdch_count <- ifelse(n_observed == 0, NA_real_, rowSums(item_mat, na.rm = TRUE))
df <- df |>
  mutate(
    mdch_any    = if_else(!is.na(mdch_count), as.numeric(mdch_count > 0), NA_real_),
    mdch_severe = if_else(!is.na(mdch_count), as.numeric(mdch_count > 5), NA_real_)
  )

df <- df |> mutate(MDCH = if_else(MDCH %in% c(0, 1), MDCH, NA_real_))
df <- df |> mutate(mdch_observed = as.numeric(!is.na(MDCH)))
cat(sprintf("Official MDCH non-missing: %s (prevalence %.3f)\n",
            format(sum(df$mdch_observed), big.mark = ","), mean(df$MDCH, na.rm = TRUE)))
cat("\nOfficial MDCH prevalence BY COUNTRY (sanity check before the placebo regressions):\n")
print(df |> filter(mdch_observed == 1) |> group_by(country_f) |> summarise(prevalence = mean(MDCH), n = n()))

df <- df |>
  mutate(
    food_insecure     = if_else(HHSHARE == 1 & !is.na(FOODSEC_STATUS_CAT),
                                 as.numeric(FOODSEC_STATUS_CAT == 2), NA_real_),
    very_low_food_sec = NA_real_
  )

# ── DiD variables. `post`/`did` here are SCOTLAND-SPECIFIC (treated=scotland) ──
# -- the placebo script (10_placebo_wales_ni.R) builds its OWN treated/post
# columns per comparison pair (Scotland-vs-Wales, Scotland-vs-NI), since
# "post" for a Wales or NI comparison isn't a treatment date for THEM (they're
# never-treated placebos) -- it only needs to match Scotland's actual post
# period so the DiD coefficient asks "would this look like an effect if Wales/
# NI had been the control instead of England." Kept here for the
# Scotland-vs-England leg only, exactly as in 01_hbai_prep.R, for a direct
# sanity-check comparison against Stage 1's existing headline numbers.
df <- df |>
  mutate(
    post    = as.numeric(YEAR >= SCP_EXPAND_YEAR),
    treated = scotland,
    did     = treated * post
  )

# Exact 14-Nov-2022 cutoff refinement, identical logic to 01_hbai_prep.R
FRS_ROOT <- file.path(DATA_ROOT, "UKDA-9252-tab", "tab")
hhold_pattern <- "^(hhold|househol|hhld|household)(_v[0-9]+)?\\.tab$"
hhold_found <- if (dir.exists(FRS_ROOT)) {
  list.files(FRS_ROOT, pattern = hhold_pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
} else character(0)
hhold_path <- if (length(hhold_found) == 0) NA_character_ else if (any(grepl("v2", hhold_found, ignore.case = TRUE))) hhold_found[grepl("v2", hhold_found, ignore.case = TRUE)][1] else hhold_found[1]

if (!is.na(hhold_path)) {
  cat(sprintf("\nUsing: %s\n", hhold_path))
  hh <- read_tsv(hhold_path, show_col_types = FALSE, col_types = cols(.default = col_character()))
  names(hh) <- toupper(trimws(names(hh)))
  date_col <- intersect(c("INTDATE", "INTDATM", "INTDAT"), names(hh))
  if (length(date_col) > 0) {
    raw_dates <- hh[[date_col[1]]]
    looks_like_slash_date <- any(grepl("/", raw_dates), na.rm = TRUE)
    hh_dates <- if (looks_like_slash_date) {
      hh |> transmute(SERNUM = suppressWarnings(as.numeric(SERNUM)),
                       intdate = suppressWarnings(as.Date(raw_dates, format = "%m/%d/%Y")))
    } else {
      hh |> transmute(SERNUM = suppressWarnings(as.numeric(SERNUM)),
                       intdate = suppressWarnings(as.Date(as.numeric(raw_dates), origin = "1960-01-01")))
    }
    hh_dates <- hh_dates |> filter(!is.na(SERNUM), !is.na(intdate))
    df <- df |>
      left_join(hh_dates, by = "SERNUM") |>
      mutate(
        post = if_else(YEAR == 2023 & !is.na(intdate),
                        as.numeric(intdate >= as.Date("2022-11-14")), post),
        did  = treated * post
      )
    cat(sprintf("  FY2022/23 rows reclassified: %s Pre, %s Post (of %s total in that year)\n",
                format(sum(df$YEAR == 2023 & df$post == 0), big.mark = ","),
                format(sum(df$YEAR == 2023 & df$post == 1), big.mark = ","),
                format(sum(df$YEAR == 2023), big.mark = ",")))
  }
} else {
  cat(sprintf("\nFRS household file for FY2022/23 not found under %s -- `post` stays FY-based.\n", FRS_ROOT))
}

# ── FYE2024 MDCH measurement-transition check BY COUNTRY (relevant here in a ──
# way it wasn't for the England/Scotland-only file): the old/new MDCH
# question-design split in the transition year is NOT even across the four
# nations -- NI's harmonised extract carries 0% old-design questions (100%
# new, a NISRA sample-size decision), vs ~27-30% old-design for GB nations.
# This means NI's official MDCH flag in FYE2024 is entirely on the NEW
# definition while Scotland/England/Wales mix both -- a genuine measurement
# asymmetry to flag explicitly if NI's placebo result looks different, not
# necessarily a sign the DiD identification itself is broken.
if ("MDCHDMP" %in% names(df)) {
  cat("\nFYE2024 old/new MDCH question-design split BY COUNTRY (MDCHDMP: 1=old, 2=new):\n")
  print(df |> filter(YEAR == 2024, !is.na(MDCHDMP)) |> count(country_f, MDCHDMP) |>
          pivot_wider(names_from = MDCHDMP, values_from = n, names_prefix = "MDCHDMP_"))
}

dir.create(dirname(HBAI_OUT), showWarnings = FALSE, recursive = TRUE)
write_csv(df, HBAI_OUT)
cat(sprintf("\nSaved to %s\n  Shape: %d rows x %d cols\n", HBAI_OUT, nrow(df), ncol(df)))
