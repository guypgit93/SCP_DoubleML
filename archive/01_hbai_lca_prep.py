"""
HBAI Data Preparation — LCA Pipeline
Scottish Child Payment Dissertation
======================================
Loads HBAI individual-level data (2016/17–2023/24) and produces a clean
child-level dataset ready for Latent Class Analysis.

Source files (UKDA-5828, 23-24prices folder):
  i1518e_2324prices.tab  →  2015/16, 2016/17, 2017/18
  i1821e_2324prices.tab  →  2018/19, 2019/20, 2020/21
  i2124e_2324prices.tab  →  2021/22, 2022/23, 2023/24

Sample restrictions applied here:
  • Scotland (GVTREGN == 12) and England regions (1,2,4–10) only
  • Children aged ≤ 16
  • Financial year 2016/17 onwards (first full year)
  • 2020/21 excluded (COVID-19 data disruption)

LCA indicator variables (binary, 1 = deprived):
  MDCH_ACT  — activities (school trips, clubs)
  MDCH_BED  — bedroom / bed of their own
  MDCH_CEL  — celebrations (birthday, festivals)
  MDCH_COAT — warm coat
  MDCH_EQP  — school equipment
  MDCH_HOL  — holiday away from home
  MDCH_LES  — leisure / hobby activities
  MDCH_PLAY — indoor play / games
  MDCH_PLY  — outdoor play area
  MDCH_TEA  — fresh fruit / vegetables
  MDCH_TRP  — trips / outings
  MDCH_VEG  — vegetables (separate item in some waves)

Additional outcomes:
  FOODSEC        — food security status (1=high … 4=very low)
  food_insecure  — derived binary (FOODSEC ∈ {3,4})

DiD variables:
  scotland   — 1 if Scotland, 0 if England
  post       — 1 if YEAR ≥ SCP_EXPAND_YEAR (2023)
  did        — scotland × post

Output: hbai_lca.csv  (one row per child-year observation)
"""

import pandas as pd
import numpy as np
import os
import warnings
warnings.filterwarnings("ignore")

from config import HBAI_ROOT, HBAI_OUT, SCP_INTRO_YEAR, SCP_EXPAND_YEAR

# ─────────────────────────────────────────────────────────────────────────────
# CONFIGURATION
# ─────────────────────────────────────────────────────────────────────────────

HBAI_FILES = [
    "i1518e_2324prices.tab",   # 2015/16 – 2017/18
    "i1821e_2324prices.tab",   # 2018/19 – 2020/21
    "i2124e_2324prices.tab",   # 2021/22 – 2023/24
]

# All 12 binary child material deprivation items — LCA indicators
MDCH_ITEMS = [
    # MDCH_LES excluded: only 538 observations, 0% prevalence — zero-variance, unusable
    # MDCH_ACT excluded: only ~13k observations; check YEAR coverage before re-adding
    #   (if ACT only appears in post-period years it cannot be used in DiD)
    "MDCH_BED",   # bed / bedroom of own
    "MDCH_CEL",   # celebrations (birthday etc.)
    "MDCH_COAT",  # warm coat
    "MDCH_EQP",   # school equipment
    "MDCH_HOL",   # holiday away from home
    "MDCH_PLAY",  # indoor play / games
    "MDCH_PLY",   # outdoor play area
    "MDCH_TEA",   # fresh fruit / vegetables
    "MDCH_TRP",   # trips / outings
    "MDCH_VEG",   # vegetables
]

KEEP_VARS = [
    # Identifiers
    "SERNUM", "BENUNIT", "PERSON", "YEARFIN",
    # Geography — GVTREGN column is renamed from GS_INDWA at load time (column swap)
    "COUNTRY", "GVTREGN",
    # Child demographics
    "AGE",
    # Child material deprivation — composite count (may be blank in TAB; rebuilt below)
    "MDCH",
    # MDCHDMP is optional — picked up after concat if present
    # Child material deprivation — individual LCA indicators
    *MDCH_ITEMS,
    # Composite poverty measures (for validation)
    "LOWINCMDCH",       # low income AND material deprivation
    "LOWINCMDCHSEV",    # low income AND severe material deprivation
    # Food security (3-level in HBAI: 1=secure, 2=low, 3=very low)
    "FOODSEC",
    # Survey weight
    "GS_INDCH",         # official child-level grossing weight
    # ── DML covariates ─────────────────────────────────────────────────────────
    # Philosophy: include all potentially relevant characteristics and let the
    # DML learners (Lasso / Random Forest) handle selection and regularisation.
    # Exclude only: outcome-derived variables (LOWINCMDCH*), survey weights,
    # identifiers, and pure noise (MDPN_TAXI*, SEWERAGE).
    #
    # ── Child demographics ────────────────────────────────────────────────────
    "SEX",                  # child's sex (1=male, 2=female)
    "AGEBAND_CH",           # child age band (categorical complement to AGE)
    # ── Household head characteristics ────────────────────────────────────────
    "AGEHD",                # age of household head
    "AGEHDBAND",            # banded age of head
    "AGEHDBAND_KID",        # age of head (households with children)
    "SEXHD",                # sex of household head
    "AGESP",                # age of spouse / partner
    "AGESPBAND",            # banded age of spouse
    # ── Family structure ──────────────────────────────────────────────────────
    "COUPLE_KID",           # couple with children (1=yes) — key lone-parent proxy
    "NEWFAMBU_SINGLE",      # single-adult benefit unit
    "NEWFAMBU_KID",         # number of dependent children in benefit unit
    "NUMBKIDS",             # number of children in household
    "NEWFAMBU_WITH",        # BU composition variant
    "NEWFAMBU_WITH_WA",     # BU with working-age adults
    "NEWFAMBU_WITH_PN",     # BU with partner/non-dependant
    "NEWFAMBU_WITH_PN_TOT", # total variant
    "ADULTB",               # number of adults in benefit unit
    "ADULTH",               # number of adults in household
    "ADULTHBAND",           # banded number of adults
    "MARITAL_KID",          # marital status (households with children)
    "MARITAL_WITHKID",      # marital status variant (with children)
    # ── Child age composition ─────────────────────────────────────────────────
    "KID0_1",               # child aged 0–1 in household
    "KID2_4",               # child aged 2–4
    "KID5_7",               # child aged 5–7
    "KID8_10",              # child aged 8–10
    "KID11_12",             # child aged 11–12
    "KID13_15",             # child aged 13–15
    "KID16PLUS",            # child aged 16+
    "KIDECOBU",             # children's economic activity in BU
    "KIDECOBU_WORK",        # children's work-related economic activity
    # ── Disability ────────────────────────────────────────────────────────────
    "DISCORKID",            # child has limiting illness/disability
    "DISCORABFLG",          # disability flag (household/adult level)
    "DIS",                  # binary disability indicator
    "DIS_TYPE",             # type of disability
    "DSCORFAM",             # disability in family
    "DSCORFAM_WORK",        # disability affecting work in family
    "BENBU_DISBEN",         # BU receives disability-related benefits
    "BENBU_DLA",            # Disability Living Allowance receipt
    "BENBU_PIP",            # Personal Independence Payment receipt
    "DSCORANDBEN",          # disability score and benefit combined
    # ── Employment ────────────────────────────────────────────────────────────
    "EMPSTATI",             # employment status
    "SEX_ADULT",            # sex of reference adult
    "S_OE_GRO_PROP_EARN",  # share of gross income from earnings (continuous employment proxy)
    "WINPAYBU",             # wage/salary income in benefit unit
    "WINPAYHD",             # wage income of household head
    "WINPAYSP",             # wage income of spouse
    "EGRINCBU",             # total earnings income in BU
    # ── Income ────────────────────────────────────────────────────────────────
    "S_OE_AHC",             # net equivalised income AHC (potential mediator — included, flagged)
    "S_OE_BHC",             # net equivalised income BHC
    "S_OE_GRO",             # gross equivalised income
    "S_OE_GRO_PROP_BEN",   # share of gross income from benefits
    "S_OE_GRO_PROP_INV",   # share from investments
    "S_OE_HC",              # housing cost component
    "ESBENIBU",             # state benefit income in BU
    "CHBENBU",              # Child Benefit in BU
    "INCHILBU",             # child income in BU
    # ── Benefit receipt flags ─────────────────────────────────────────────────
    "BENBU_UC",             # Universal Credit receipt
    "BENBU_UC_OR_EQUIV",    # UC or legacy equivalent (bridges rollout)
    "NEWFAMBU_UC",          # UC receipt (alternative flag)
    "BENBU_FSM",            # Free School Meals eligibility (income proxy)
    "BENBU_IS",             # Income Support
    "BENBU_JSA",            # Jobseeker's Allowance
    "BENBU_ESA",            # Employment and Support Allowance
    "BENBU_HB",             # Housing Benefit
    "BENBU_CTC",            # Child Tax Credit
    "BENBU_WTC",            # Working Tax Credit
    "WFTCBU",               # Working Families Tax Credit
    "BENBU_PC",             # Pension Credit
    # ── Housing ───────────────────────────────────────────────────────────────
    "TENHBAI",              # tenure type (1=owned outright … 4=private rent)
    "PTENTYP2",             # alternative tenure coding
    "ERENTBU",              # rent paid by benefit unit (continuous)
    "EHCOST",               # total housing costs
    "ES_HCOST",             # standardised housing cost
    # ── Ethnicity ─────────────────────────────────────────────────────────────
    "ETH",                  # simplified ethnicity indicator
    "ETHGRPHHPUB",          # ethnic group of household head (public categories)
    "ETHGRPHH",             # ethnic group of household head (detailed)
]

# England government regions
ENGLAND_REGIONS  = [1, 2, 4, 5, 6, 7, 8, 9, 10]
SCOTLAND_REGION  = 12
NORTHERN_ENGLAND = [1, 2, 4]   # NE, NW, Yorkshire — for robustness checks


# ─────────────────────────────────────────────────────────────────────────────
# HELPER: recode MDCH item
# TAB file coding: 1 = child has item (NOT deprived), 2 = child lacks due to cost (DEPRIVED)
# 3 = not applicable, -9 = DK → all map to NaN via sentinel replacement + missing map entry
# ─────────────────────────────────────────────────────────────────────────────
def recode_mdch_item(series: pd.Series) -> pd.Series:
    s = pd.to_numeric(series, errors="coerce")
    return s.map({1: 0, 2: 1})   # 1=has item/not deprived→0; 2=lacks/deprived→1; else NaN


# ─────────────────────────────────────────────────────────────────────────────
# LOAD AND STACK
# ─────────────────────────────────────────────────────────────────────────────
all_dfs = []

for fname in HBAI_FILES:
    fpath = os.path.join(HBAI_ROOT, fname)
    if not os.path.exists(fpath):
        print(f"  ✗ NOT FOUND: {fpath}")
        continue

    print(f"\nLoading {fname}...")
    raw = pd.read_csv(fpath, sep="\t", low_memory=False)
    raw.columns = raw.columns.str.upper().str.strip()
    print(f"  Raw shape: {raw.shape}")

    # The TAB file has GVTREGN and GS_INDWA swapped relative to the variable guide.
    # GS_INDWA column actually contains region codes 1–13 (confirmed by diagnostics).
    # Rename before selecting columns so KEEP_VARS["GVTREGN"] resolves correctly.
    if "GS_INDWA" in raw.columns:
        raw = raw.rename(columns={"GS_INDWA": "GVTREGN",
                                   "GVTREGN":  "GVTREGN_RAW"})

    # Select available columns
    # MDCHDMP is 2023/24+ only (deep material poverty flag); silently skip in earlier files
    year_limited = {"MDCHDMP"}   # variables not expected before 2023/24
    keep = [v for v in KEEP_VARS if v in raw.columns]
    if "MDCHDMP" in raw.columns:
        keep = list(set(keep + ["MDCHDMP"]))
    missing = [v for v in KEEP_VARS if v not in raw.columns and v not in year_limited]
    if missing:
        print(f"  ⚠ Variables not found in {fname}: {missing}")
    year_limited_found = [v for v in year_limited if v in raw.columns]
    if year_limited_found:
        print(f"  ✓ Year-limited vars present: {year_limited_found}")

    df = raw[keep].copy()

    # Coerce all potential numeric columns (errors='coerce' safely handles any strings)
    numeric_cols = [
        "COUNTRY", "GVTREGN", "AGE", "GS_INDCH", "YEARFIN", "MDCH",
        # Child / head demographics
        "SEX", "AGEBAND_CH", "AGEHD", "AGEHDBAND", "AGEHDBAND_KID",
        "SEXHD", "AGESP", "AGESPBAND", "SEX_ADULT",
        # Family structure
        "COUPLE_KID", "NEWFAMBU_SINGLE", "NEWFAMBU_KID", "NUMBKIDS",
        "NEWFAMBU_WITH", "NEWFAMBU_WITH_WA", "NEWFAMBU_WITH_PN", "NEWFAMBU_WITH_PN_TOT",
        "ADULTB", "ADULTH", "ADULTHBAND",
        "MARITAL_KID", "MARITAL_WITHKID",
        "KID0_1", "KID2_4", "KID5_7", "KID8_10", "KID11_12", "KID13_15", "KID16PLUS",
        "KIDECOBU", "KIDECOBU_WORK",
        # Disability
        "DISCORKID", "DISCORABFLG", "DIS", "DIS_TYPE",
        "DSCORFAM", "DSCORFAM_WORK", "BENBU_DISBEN", "BENBU_DLA", "BENBU_PIP",
        "DSCORANDBEN",
        # Employment / income
        "EMPSTATI", "S_OE_GRO_PROP_EARN", "WINPAYBU", "WINPAYHD", "WINPAYSP",
        "EGRINCBU", "S_OE_AHC", "S_OE_BHC", "S_OE_GRO",
        "S_OE_GRO_PROP_BEN", "S_OE_GRO_PROP_INV", "S_OE_HC", "ESBENIBU",
        "CHBENBU", "INCHILBU",
        # Benefits
        "BENBU_UC", "BENBU_UC_OR_EQUIV", "NEWFAMBU_UC",
        "BENBU_FSM", "BENBU_IS", "BENBU_JSA", "BENBU_ESA",
        "BENBU_HB", "BENBU_CTC", "BENBU_WTC", "WFTCBU", "BENBU_PC",
        # Housing
        "TENHBAI", "PTENTYP2", "ERENTBU", "EHCOST", "ES_HCOST",
        # Ethnicity
        "ETH", "ETHGRPHHPUB", "ETHGRPHH",
    ]
    for col in numeric_cols:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors="coerce")

    # Derive YEAR (financial year ending) from YEARFIN (format YYZZ, e.g. 1617 → 2017)
    if "YEARFIN" in df.columns:
        df["YEAR"] = 2000 + (df["YEARFIN"] % 100)
    else:
        raise KeyError(f"YEARFIN not found in {fname} — cannot derive survey year")

    years_found = sorted(df["YEAR"].dropna().unique().astype(int).tolist())
    print(f"  Years in file: {years_found}")

    # Geography filter: Scotland + England only
    df["GVTREGN"] = pd.to_numeric(df["GVTREGN"], errors="coerce")
    df = df[df["GVTREGN"].isin(ENGLAND_REGIONS + [SCOTLAND_REGION])].copy()
    df["scotland"]         = (df["GVTREGN"] == SCOTLAND_REGION).astype(int)
    df["northern_england"] = (df["GVTREGN"].isin(NORTHERN_ENGLAND)).astype(int)

    print(f"  After geo filter — Scotland: {df['scotland'].sum():,}, "
          f"England: {(df['scotland']==0).sum():,}")

    # Replace negative sentinel values with NaN across all numeric columns
    for col in df.select_dtypes(include=[np.number]).columns:
        df[col] = df[col].where(df[col] >= 0, np.nan)

    all_dfs.append(df)
    print(f"  → {len(df):,} rows retained")

if not all_dfs:
    raise FileNotFoundError(
        f"No HBAI files loaded. Check HBAI_ROOT in config.py: {HBAI_ROOT}"
    )

df = pd.concat(all_dfs, ignore_index=True)
print(f"\n{'='*60}")
print(f"Combined: {len(df):,} rows, years: "
      f"{sorted(df['YEAR'].dropna().unique().astype(int).tolist())}")


# ─────────────────────────────────────────────────────────────────────────────
# SAMPLE RESTRICTIONS
# ─────────────────────────────────────────────────────────────────────────────

# 1. Children aged 16 and under (SCP eligibility + MDCH survey scope)
df["AGE"] = pd.to_numeric(df["AGE"], errors="coerce")
before = len(df)
df = df[df["AGE"] <= 16].copy()
print(f"\nAge ≤16 filter:        {before:>8,} → {len(df):,} rows")

# 2. 2015/16 excluded (first file has partial overlap; 2016/17 is first full year)
before = len(df)
df = df[df["YEAR"] >= 2017].copy()
print(f"Year ≥ 2017 filter:    {before:>8,} → {len(df):,} rows")

# 3. 2020/21 excluded (COVID-19 survey disruption)
before = len(df)
df = df[df["YEAR"] != 2021].copy()
print(f"Exclude 2020/21:       {before:>8,} → {len(df):,} rows")


# ─────────────────────────────────────────────────────────────────────────────
# RECODE LCA INDICATORS
# Individual MDCH items: 1=deprived, 2=not deprived, 3=N/A, -9=DK
# → recode to 1/0/NaN
# ─────────────────────────────────────────────────────────────────────────────
available_items = [c for c in MDCH_ITEMS if c in df.columns]
missing_items   = [c for c in MDCH_ITEMS if c not in df.columns]

print(f"\nMDCH items available: {len(available_items)}/{len(MDCH_ITEMS)}")
if missing_items:
    print(f"  Missing: {missing_items}")

for item in available_items:
    df[item] = recode_mdch_item(df[item])

# Rebuild composite MDCH count from individual items (TAB composite often blank)
if available_items:
    df["mdch_count"]  = df[available_items].sum(axis=1, min_count=1)
    df["mdch_any"]    = (df["mdch_count"] > 0).where(df["mdch_count"].notna()).astype(float)
    df["mdch_severe"] = (df["mdch_count"] >= 3).where(df["mdch_count"].notna()).astype(float)
    print(f"Rebuilt mdch_count — non-missing: {df['mdch_count'].notna().sum():,}")
    print(f"  mdch_any prevalence:    {df['mdch_any'].mean():.3f}")
    print(f"  mdch_severe prevalence: {df['mdch_severe'].mean():.3f}")

# MDCH coverage flag — child was asked the deprivation module
df["mdch_observed"] = df["mdch_count"].notna().astype(int)

# Count of non-missing LCA indicators per row (used to filter low-info rows)
df["n_items_observed"] = df[available_items].notna().sum(axis=1)

# ── Propagate MDCH BU-level flag to all children in the same benefit unit ────
# MDCH is only populated on the BU reference person's record (BU-level flag).
# All children in the BU share the same deprivation status, so we fill down
# using the BU head's value across all members of (SERNUM, BENUNIT).
if "MDCH" in df.columns:
    df["MDCH"] = pd.to_numeric(df["MDCH"], errors="coerce")
    df["MDCH"] = df["MDCH"].where(df["MDCH"] >= 0, np.nan)   # sentinels → NaN
    # Forward-fill within BU: take the max (0 or 1) across BU members
    df["MDCH"] = (df.groupby(["SERNUM", "BENUNIT"])["MDCH"]
                    .transform("max"))
    _mdch_cov = df["MDCH"].notna().sum()
    _mdch_prev = df["MDCH"].mean()
    print(f"\nMDCH BU flag propagated — non-missing: {_mdch_cov:,}  "
          f"prevalence: {_mdch_prev:.3f}")


# ─────────────────────────────────────────────────────────────────────────────
# FOOD SECURITY
# HBAI FOODSEC uses a 3-level scale (derived from USDA module in FRS):
#   1 = food secure (high + marginal combined)
#   2 = low food security
#   3 = very low food security
#   0 / negative = not applicable / not asked → NaN
# ─────────────────────────────────────────────────────────────────────────────
if "FOODSEC" in df.columns:
    df["FOODSEC"] = pd.to_numeric(df["FOODSEC"], errors="coerce")
    # Replace 0 (not applicable) and negatives with NaN
    df["FOODSEC"] = df["FOODSEC"].where(df["FOODSEC"] > 0, np.nan)
    print(f"\nFOODSEC raw distribution (after cleaning):")
    print(df["FOODSEC"].value_counts(dropna=False).sort_index().to_string())
    # Derived binary outcomes
    df["food_insecure"]      = df["FOODSEC"].map({1: 0, 2: 1, 3: 1})  # any insecurity
    df["very_low_food_sec"]  = df["FOODSEC"].map({1: 0, 2: 0, 3: 1})  # very low only
    print(f"  food_insecure prevalence:     {df['food_insecure'].mean():.3f}")
    print(f"  very_low_food_sec prevalence: {df['very_low_food_sec'].mean():.3f}")
else:
    df["food_insecure"]     = np.nan
    df["very_low_food_sec"] = np.nan
    print("\n⚠ FOODSEC not found in dataset")


# ─────────────────────────────────────────────────────────────────────────────
# DiD VARIABLES
# ─────────────────────────────────────────────────────────────────────────────
df["post"]    = (df["YEAR"] >= SCP_EXPAND_YEAR).astype(int)
df["treated"] = df["scotland"].copy()
df["did"]     = df["treated"] * df["post"]


# ─────────────────────────────────────────────────────────────────────────────
# COLUMN ORDER
# Fixed sections are listed explicitly; DML covariates are captured dynamically
# so that adding variables to KEEP_VARS above automatically flows through here.
# ─────────────────────────────────────────────────────────────────────────────
id_cols      = ["SERNUM", "BENUNIT", "PERSON", "YEAR", "YEARFIN"]
geo_cols     = ["COUNTRY", "GVTREGN", "scotland", "northern_england"]
demo_cols    = ["AGE"]
weight_cols  = ["GS_INDCH"]
lca_cols     = available_items
composite    = ["mdch_count", "mdch_any", "mdch_severe", "mdch_observed",
                "n_items_observed", "MDCH", "MDCHDMP",
                "LOWINCMDCH", "LOWINCMDCHSEV"]
outcome_cols = ["FOODSEC", "food_insecure", "very_low_food_sec"]
did_cols     = ["treated", "post", "did"]

# Fixed columns — everything else in df is a DML covariate
fixed_cols = set(id_cols + geo_cols + demo_cols + weight_cols
                 + lca_cols + composite + outcome_cols + did_cols)
covariate_cols = [c for c in df.columns if c not in fixed_cols]

col_order = (id_cols + geo_cols + demo_cols + weight_cols
             + covariate_cols + lca_cols + composite + outcome_cols + did_cols)
col_order = [c for c in col_order if c in df.columns]
df = df[col_order]

print(f"\nDML covariate columns retained: {len(covariate_cols)}")
print(f"  {covariate_cols}")


# ─────────────────────────────────────────────────────────────────────────────
# DIAGNOSTICS
# ─────────────────────────────────────────────────────────────────────────────
print(f"\n{'='*60}")
print("SAMPLE BY YEAR AND GROUP")
print(df.groupby(["YEAR", "scotland"]).size().unstack(fill_value=0).to_string())

print(f"\nMDCH ITEM PREVALENCES (% deprived, where observed)")
prev = df[available_items].mean().rename("prevalence").to_frame()
prev["n_obs"] = df[available_items].notna().sum()
print(prev.round(3).to_string())

print(f"\nCOMPOSITE MDCH STATISTICS")
print(df["mdch_count"].describe().round(3))

print(f"\nFOODSEC DISTRIBUTION")
if "FOODSEC" in df.columns:
    print(df["FOODSEC"].value_counts(dropna=False).sort_index())

print(f"\nLCA-READY ROWS (mdch_observed == 1): {df['mdch_observed'].sum():,}")
print(f"Full dataset shape: {df.shape}")


# ─────────────────────────────────────────────────────────────────────────────
# SAVE
# ─────────────────────────────────────────────────────────────────────────────
os.makedirs(os.path.dirname(HBAI_OUT), exist_ok=True)
df.to_csv(HBAI_OUT, index=False)
print(f"\n✓ Saved to {HBAI_OUT}")
print(f"  Shape: {df.shape}")
print(f"  Columns: {list(df.columns)}")
