"""
FRS Data Preparation Script
Scottish Child Payment Dissertation
================================
Loads, cleans, and merges FRS data across years (2017-18 to 2023-24).
Constructs sample restriction, treatment indicator, and outcome variables.

FRS files used:
  - househol.tab  : household-level variables (deprivation, food insecurity, country)
  - benunit.tab   : benefit unit-level (income, benefit receipt, children)
  - child.tab     : child-level (age of children)

Output: data/frs_merged.csv  — one row per benefit unit, all years stacked
"""

import pandas as pd
import numpy as np
import os
import warnings
warnings.filterwarnings("ignore")

from config import DATA_ROOT, DATA_OUT, YEARS, SCP_INTRO_YEAR, SCP_EXPAND_YEAR

# ─────────────────────────────────────────────────────────────────────────────
# HELPER: load a single .tab file case-insensitively
# ─────────────────────────────────────────────────────────────────────────────
def load_tab(ukda_folder, filename):
    # Actual path: DATA_ROOT/UKDA-XXXX-tab/tab/v1/filename.tab
    tab_root = os.path.join(DATA_ROOT, f"{ukda_folder}-tab", "tab")
    name, ext = os.path.splitext(filename)

    # Try in order of preference:
    candidates = [
        os.path.join(tab_root, "v2", f"{name}_v2{ext}"),   # v2 subfolder, _v2 suffix
        os.path.join(tab_root, "v2", filename),              # v2 subfolder, no suffix
        os.path.join(tab_root, filename),                    # directly in tab/
        os.path.join(tab_root, f"{name}_v2{ext}"),           # tab/ with _v2 suffix
    ]

    path = next((p for p in candidates if os.path.exists(p)), None)
    if path is None:
        raise FileNotFoundError(
            f"Cannot find {filename} for {ukda_folder}. Tried:\n" +
            "\n".join(f"  {c}" for c in candidates) +
            "\nCheck that DATA_ROOT is set correctly."
        )
    print(f"    using: {os.path.relpath(path, DATA_ROOT)}")
    df = pd.read_csv(path, sep="\t", low_memory=False)
    df.columns = df.columns.str.upper().str.strip()
    return df


# ─────────────────────────────────────────────────────────────────────────────
# STEP 1: LOAD AND STACK HOUSEHOLD FILE
# Key variables:
#   SERNUM        — household serial number (links to benunit)
#   COUNTRY       — 1=England, 2=Wales, 3=Scotland, 4=NI
#   GVTREGN       — government region (for robustness)
#   Food insecurity (from 2019-20 onwards):
#     FSSEC1–FSSEC10  — individual USDA module items
#     FSMLSSEC        — derived food security status (1=high … 4=very low)
#   Material deprivation (old questions, consistent pre-2023-24):
#     MATDEP          — derived binary deprivation indicator (if present)
#     MDxx variables  — individual deprivation items
#   Financial situation:
#     FINSIT / FINNOW — "how well managing financially" (ordinal, 1–5)
#   Food bank:
#     FOODBANK        — used food bank in last 12 months (binary)
# ─────────────────────────────────────────────────────────────────────────────
HOUSEHOL_VARS = [
    "SERNUM",
    "COUNTRY", "GVTREGN",
    # Food insecurity — derived status and individual items
    "FOODSEC",                                              # derived: 1=high … 4=very low
    "FOODQ1","FOODQ2","FOODQ3","FOODQ4A","FOODQ4B","FOODQ4C",
    "FOODQ5","FOODQ6","FOODQ7","FOODQ8A","FOODQ8B","FOODQ8C",
    # Food bank
    "FOODBANK",   # any use
    "FOODBKYR",   # used in past year
    "FOODBK12",   # used in past 12 months
    "FOODBK30",   # used in past 30 days
    # UC receipt at household level (for eligibility proxy)
    "HHUC",
    # Children
    "DEPCHLDH",
    # Household income
    "HHINC", "HHINCBND",
    # Housing tenure (covariate)
    "TENURE",
    # Index of Multiple Deprivation (covariate)
    "IMD_E", "IMD_S",
]

def load_househol(ukda_folder, year_int):
    hh = load_tab(ukda_folder, "househol.tab")
    if year_int == 2023:   # print columns once for one year to diagnose variable names
        print(f"\n  [DIAG] househol.tab columns ({year_int}):\n  {sorted(hh.columns.tolist())}\n")
    # Keep only variables that exist in this year's file
    keep = [v for v in HOUSEHOL_VARS if v in hh.columns]
    # Also keep any material deprivation items (MD prefix)
    md_cols = [c for c in hh.columns if c.startswith("MD")]
    keep = list(set(keep + md_cols))
    hh = hh[keep].copy()
    hh["YEAR"] = year_int
    return hh


# ─────────────────────────────────────────────────────────────────────────────
# STEP 2: LOAD BENEFIT UNIT FILE
# Key variables:
#   SERNUM        — links to household
#   BENUNIT       — benefit unit number within household
#   BUINC         — benefit unit net income (weekly)
#   HHINCBND      — income band (for robustness)
#   UCINCAMT      — Universal Credit amount (proxy for eligibility)
#   UCLIVE        — UC claimant flag
#   CTCAMT        — Child Tax Credit amount
#   FAMTYPBU      — family type (identifies lone parent, couple with children etc.)
#   NCHLDBU       — number of children in benefit unit
#   DEPCHLDH      — number of dependent children in household
# ─────────────────────────────────────────────────────────────────────────────
BENUNIT_VARS = [
    "SERNUM", "BENUNIT",
    "BUINC", "BURINC",
    "BUUC",                      # UC income at benefit unit level (>0 = UC recipient)
    "BUTXCRED",                  # Tax credits income at benefit unit level (>0 = CTC/WTC recipient)
    "FAMTYPBU", "FAMTYPBS",
    "DEPCHLDB",                  # dependent children in benefit unit
    "KID04", "KID510", "KID1115", "KID1619",
    "ADULTB",
    # Material deprivation items
    "OAHEAT", "OAHOL",  "OAWARM", "OACOAT", "OACOOK",
    "OADAMP", "OAFRND", "OAHAIR", "OAMEAL", "OAOUT",
    "OAPHON", "OAPRE",  "OATAXI", "OAHOME", "OAHOLB",
    # Financial hardship — primary measure
    "ADBTBL",                    # Keep up with bills and regular debt repayments (binary)
    "HOUSHE1",                   # Able to keep accommodation warm enough (fuel poverty)
    # Financial resilience — secondary measure
    "OAHOWPY1","OAHOWPY2","OAHOWPY3","OAHOWPY4","OAHOWPY5","OAHOWPY6",
    # Debt arrears — robustness checks
    "DEBTAR01","DEBTAR02","DEBTAR03","DEBTAR04","DEBTAR05",
    # Child deprivation items
    "CDEPHOL","CDEPBED","CDEPCEL","CDEPEQP","CDEPTEA","CDEPTRP","CDEPVEG",
]

def load_benunit(ukda_folder, year_int):
    bu = load_tab(ukda_folder, "benunit.tab")
    if year_int == 2023:
        print(f"\n  [DIAG] benunit.tab columns ({year_int}):\n  {sorted(bu.columns.tolist())}\n")
    keep = [v for v in BENUNIT_VARS if v in bu.columns]
    bu = bu[keep].copy()
    bu["YEAR"] = year_int
    return bu


# ─────────────────────────────────────────────────────────────────────────────
# STEP 3: LOAD ADULT FILE — to get financial situation variable
# Aggregated to benefit unit level (take HRP or modal response)
# ─────────────────────────────────────────────────────────────────────────────
ADULT_VARS = [
    "SERNUM", "BENUNIT", "HRPID", "PERSON",
    "FINNOW", "FINSIT",           # financial situation (may not exist)
    "HAPPYWB", "LIFESAT",         # subjective wellbeing (1-10 scales)
    "ANXIOUS", "MEANING",         # GHQ-style wellbeing
    "HEALTH1",                    # self-reported health
]

def load_adult(ukda_folder, year_int):
    ad = load_tab(ukda_folder, "adult.tab")
    if year_int == 2023:
        print(f"\n  [DIAG] OAHOWPY in benunit? — check separately")
        # Confirm wellbeing variables exist
        wb_found = [v for v in ["HAPPYWB","LIFESAT","ANXIOUS","MEANING","HEALTH1"] if v in ad.columns]
        print(f"  [DIAG] Wellbeing vars found in adult.tab: {wb_found}")
    keep = [v for v in ADULT_VARS if v in ad.columns]
    ad = ad[keep].copy()
    if not keep:
        return pd.DataFrame(columns=["SERNUM","BENUNIT","YEAR"])
    # Aggregate to benefit unit level: use HRP where possible, else first adult
    value_vars = [v for v in keep if v not in ["SERNUM","BENUNIT","HRPID","PERSON"]]
    if "HRPID" in ad.columns:
        hrp = ad[ad["HRPID"] == 1][["SERNUM","BENUNIT"] + value_vars].copy()
    else:
        hrp = ad.groupby(["SERNUM","BENUNIT"])[value_vars].first().reset_index()
    hrp["YEAR"] = year_int
    return hrp


# ─────────────────────────────────────────────────────────────────────────────
# STEP 4: LOAD CHILD FILE — to get ages of children
# Key variables:
#   SERNUM, BENUNIT — links
#   CHBAGE          — age of child
# ─────────────────────────────────────────────────────────────────────────────
def load_children(ukda_folder, year_int):
    ch = load_tab(ukda_folder, "child.tab")
    # Age variable name varies across FRS years
    age_candidates = ["CHBAGE", "AGE", "CHBAGEM", "CAGE"]
    age_var = next((v for v in age_candidates if v in ch.columns), None)
    if age_var is None:
        print(f"    WARNING: no child age variable found. Available: {list(ch.columns)}")
        # Return empty aggregation so the merge still works
        return pd.DataFrame(columns=["SERNUM","BENUNIT","max_child_age",
                                     "n_children_u16","n_children_u6","YEAR"])
    keep = [v for v in ["SERNUM","BENUNIT", age_var] if v in ch.columns]
    ch = ch[keep].copy()
    ch_agg = ch.groupby(["SERNUM","BENUNIT"]).agg(
        max_child_age  = (age_var, "max"),
        n_children_u16 = (age_var, lambda x: (x < 16).sum()),
        n_children_u6  = (age_var, lambda x: (x < 6).sum()),
    ).reset_index()
    ch_agg["YEAR"] = year_int
    return ch_agg


# ─────────────────────────────────────────────────────────────────────────────
# STEP 4: MERGE AND STACK ALL YEARS
# ─────────────────────────────────────────────────────────────────────────────
all_dfs = []

for ukda_folder, year_int in YEARS.items():
    print(f"Loading {ukda_folder} ({year_int})...")
    try:
        hh  = load_househol(ukda_folder, year_int)
        bu  = load_benunit(ukda_folder, year_int)
        ch  = load_children(ukda_folder, year_int)
        ad  = load_adult(ukda_folder, year_int)

        # Merge benefit unit onto household
        merged = bu.merge(hh.drop(columns=["YEAR"]), on="SERNUM", how="left")
        # Merge child ages onto benefit unit
        merged = merged.merge(ch.drop(columns=["YEAR"]), on=["SERNUM","BENUNIT"], how="left")
        # Merge adult financial situation onto benefit unit
        merged = merged.merge(ad.drop(columns=["YEAR"]), on=["SERNUM","BENUNIT"], how="left")
        merged["YEAR"] = year_int

        all_dfs.append(merged)
        print(f"  → {len(merged):,} benefit units loaded")

    except FileNotFoundError as e:
        print(f"  ✗ SKIPPED: {e}")
        continue

df = pd.concat(all_dfs, ignore_index=True)
print(f"\nTotal rows before sample restriction: {len(df):,}")

# ── DIAGNOSTICS: check key variables before restriction ──────────────────────
# ─────────────────────────────────────────────────────────────────────────────
# STEP 5: CONSTRUCT KEY VARIABLES
# ─────────────────────────────────────────────────────────────────────────────

# --- Fix COUNTRY dtype: coerce to int to handle mixed float/int across years ---
df["COUNTRY"] = pd.to_numeric(df["COUNTRY"], errors="coerce")

# --- Treatment: Scotland vs England (drop Wales and NI) ---
df = df[df["COUNTRY"].isin([1, 3])].copy()
df["scotland"] = (df["COUNTRY"] == 3).astype(int)
print(f"After England/Scotland filter: {len(df):,} rows")
print(f"  Scotland: {df['scotland'].sum():,} | England: {(df['scotland']==0).sum():,}")

# --- Construct helper variables (no restriction applied here) ---
df["BUINC"] = pd.to_numeric(df["BUINC"], errors="coerce")
df["has_children_u16"] = (df["n_children_u16"].fillna(0) > 0).astype(int)
df["has_children_u6"]  = (df["n_children_u6"].fillna(0) > 0).astype(int)

# SCP eligibility proxy: UC OR tax credit recipient with children under 16
# BUUC and BUTXCRED are income amounts — use > 0 to indicate receipt
df["BUUC"]     = pd.to_numeric(df.get("BUUC",     np.nan), errors="coerce")
df["BUTXCRED"] = pd.to_numeric(df.get("BUTXCRED", np.nan), errors="coerce")
df["uc_recipient"]  = (df["BUUC"]     > 0).astype(int)
df["ctc_recipient"] = (df["BUTXCRED"] > 0).astype(int)
df["scp_eligible"]  = ((df["uc_recipient"] == 1) | (df["ctc_recipient"] == 1)).astype(int)
print(f"  UC recipients:  {df['uc_recipient'].sum():,}")
print(f"  CTC recipients: {df['ctc_recipient'].sum():,}")
print(f"  SCP eligible (UC or CTC): {df['scp_eligible'].sum():,}")

# Low-income flag: bottom 40% of BUINC within each year (broader eligibility proxy)
income_threshold = df.groupby("YEAR")["BUINC"].transform(lambda x: x.quantile(0.4))
df["low_income"] = (df["BUINC"] <= income_threshold).astype(int)

print(f"Full sample saved: {len(df):,} rows")
print(f"  Has children u16:          {df['has_children_u16'].sum():,}")
print(f"  Low income (bottom 40%):   {df['low_income'].sum():,}")
print(f"  SCP eligible (UC or CTC):  {df['scp_eligible'].sum():,}")
print(f"  SCP eligible + children:   {((df['scp_eligible']==1)&(df['has_children_u16']==1)).sum():,}")

# --- SCP treatment period ---
# Main analysis: use the 2022-23 expansion to under-16s as the treatment
# Post = FY 2022-23 (year_int == 2023) onwards
df["post"] = (df["YEAR"] >= SCP_EXPAND_YEAR).astype(int)
df["treated"] = df["scotland"].copy()
df["did"] = df["treated"] * df["post"]   # DiD interaction term

# --- Outcome variables ---

# 1. Food insecurity (available from 2019-20 / year_int==2020)
#    FOODSEC: 1=high security, 2=marginal, 3=low, 4=very low
for col in ["FOODSEC"]:
    if col in df.columns:
        df[col] = pd.to_numeric(df[col], errors="coerce")
if "FOODSEC" in df.columns:
    df["food_insecure"]     = df["FOODSEC"].map({1:0, 2:0, 3:1, 4:1})
    df["very_low_food_sec"] = df["FOODSEC"].map({1:0, 2:0, 3:0, 4:1})
else:
    df["food_insecure"]     = np.nan
    df["very_low_food_sec"] = np.nan

# 2. Food bank use
#    FOODBKYR: used food bank in past year (1=yes, 2=no)
fb_var = next((v for v in ["FOODBKYR","FOODBK12","FOODBANK"] if v in df.columns), None)
if fb_var:
    df[fb_var] = pd.to_numeric(df[fb_var], errors="coerce")
    print(f"  Food bank ({fb_var}) raw value counts:\n{df[fb_var].value_counts(dropna=False).head(8)}")
    df["foodbank_use"] = df[fb_var].map({1: 1, 2: 0})
else:
    df["foodbank_use"] = np.nan

# 3. Financial situation (from adult.tab)
#    FINNOW: 1=living comfortably, 2=doing alright, 3=just about getting by,
#            4=finding it quite difficult, 5=finding it very difficult
# 3. Financial hardship — FINNOW not present in this FRS version.
#    Use OAHOWPY1-6 (binary flags per response category to financial management question)
#    Interpretation: OAHOWPY4=1 → "finding it quite difficult", OAHOWPY5=1 → "very difficult"
oahowpy_vars = [f"OAHOWPY{i}" for i in range(1, 7)]
for v in oahowpy_vars:
    if v in df.columns:
        df[v] = pd.to_numeric(df[v], errors="coerce")

# Primary financial hardship measure: ADBTBL
# "Keep up with bills and regular debt repayments"
# Typical coding: 1=keeping up, 2=falling behind, 3=have no bills (check data dictionary)
if "ADBTBL" in df.columns:
    df["ADBTBL"] = pd.to_numeric(df["ADBTBL"], errors="coerce").replace(-1, np.nan)
    print(f"  ADBTBL value counts:\n{df['ADBTBL'].value_counts(dropna=False).head(6)}")
    # Recode: 1=keeping up (0), 2=falling behind (1), 3/−8/−9=NaN
    df["fin_hardship"] = df["ADBTBL"].replace({-9: np.nan, -8: np.nan}).map({1: 0, 2: 1, 3: np.nan})
    print(f"  Financial hardship (ADBTBL): {df['fin_hardship'].notna().sum():,} non-missing")
else:
    df["fin_hardship"] = np.nan
    print("  NOTE: ADBTBL not found")

# Fuel poverty: HOUSHE1 "able to keep accommodation warm enough"
if "HOUSHE1" in df.columns:
    df["HOUSHE1"] = pd.to_numeric(df["HOUSHE1"], errors="coerce").replace(-1, np.nan)
    df["fuel_poverty"] = df["HOUSHE1"].map({1: 0, 2: 1})  # 1=yes warm enough, 2=no
    print(f"  Fuel poverty (HOUSHE1): {df['fuel_poverty'].notna().sum():,} non-missing")
else:
    df["fuel_poverty"] = np.nan

# Secondary: £200 shock resilience (OAHOWPY1 = would cut back on essentials)
if "OAHOWPY1" in df.columns:
    for v in oahowpy_vars:
        if v in df.columns:
            df[v] = df[v].replace(-1, np.nan)
    asked = df["OAHOWPY1"].notna()
    df["fin_cutback"] = np.where(asked, (df["OAHOWPY1"] == 1).astype(float), np.nan)
else:
    df["fin_cutback"] = np.nan

df["fin_score"] = np.nan  # no ordinal equivalent available

# 4b. Subjective wellbeing (from adult.tab — HRP level)
for v in ["HAPPYWB","LIFESAT","ANXIOUS","MEANING","HEALTH1"]:
    if v in df.columns:
        df[v] = pd.to_numeric(df[v], errors="coerce")
        print(f"  Wellbeing var {v}: {df[v].notna().sum():,} non-missing")

# 4. Material deprivation — OA* variables in benunit.tab
#    Coding: 1 = cannot afford (deprived), 2 = can afford or don't want
OA_ITEMS = ["OAHEAT","OAHOL","OAWARM","OACOAT","OACOOK",
            "OADAMP","OAFRND","OAHAIR","OAMEAL","OAOUT",
            "OAPHON","OAPRE","OATAXI","OAHOME"]
md_items = [v for v in OA_ITEMS if v in df.columns]
if md_items:
    # Recode: 1=deprived, 2=not deprived, -1/other=NaN
    md_recoded = df[md_items].apply(
        lambda col: pd.to_numeric(col, errors="coerce").replace(-1, np.nan).map({1:1, 2:0})
    )
    df["matdep_count"]  = md_recoded.sum(axis=1, min_count=1)  # NaN if all items missing
    # Preserve NaN: only compute binary/severe where matdep_count is non-null
    df["matdep_binary"] = np.where(df["matdep_count"].isna(), np.nan, (df["matdep_count"] > 0).astype(float))
    df["matdep_severe"] = np.where(df["matdep_count"].isna(), np.nan, (df["matdep_count"] >= 3).astype(float))
    print(f"  Material deprivation: {len(md_items)} items loaded: {md_items}")
else:
    print("  NOTE: no OA* deprivation variables found")
    df["matdep_count"]  = np.nan
    df["matdep_binary"] = np.nan
    df["matdep_severe"] = np.nan

# 5. Add financial situation diagnostic — check adult.tab next run if still missing
fin_var = next((v for v in ["FINNOW","FINSIT","FINFUT","FINDIFYR","OAHOWPY"] if v in df.columns), None)
if not fin_var:
    print("  NOTE: financial situation variable not in househol or benunit — likely in adult.tab")

# --- Covariates ---
df["log_income"] = np.log(df["BUINC"].clip(lower=1))   # clip at 1 to avoid log(0)
df["lone_parent"] = df["FAMTYPBU"].apply(
    lambda x: 1 if x in [4, 5] else 0   # check FRS codebook for lone parent codes
) if "FAMTYPBU" in df.columns else np.nan


# ─────────────────────────────────────────────────────────────────────────────
# STEP 6: SAVE CLEAN DATASET
# ─────────────────────────────────────────────────────────────────────────────
out_path = DATA_OUT
os.makedirs(os.path.dirname(out_path), exist_ok=True)
df.to_csv(out_path, index=False)

print(f"\n✓ Saved to {out_path}")
print(f"  Shape: {df.shape}")
print(f"\nYear distribution:")
print(df.groupby("YEAR")[["scotland","post","did"]].agg(["sum","count"]))

print(f"\nOutcome variable coverage (non-missing counts):")
outcomes = ["food_insecure","very_low_food_sec","foodbank_use",
            "fin_hardship","fuel_poverty","fin_cutback",
            "matdep_binary","matdep_count",
            "HAPPYWB","LIFESAT","ANXIOUS"]
for v in outcomes:
    n = df[v].notna().sum()
    print(f"  {v:25s}: {n:,} ({100*n/len(df):.1f}%)")
