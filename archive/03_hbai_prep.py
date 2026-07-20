"""
HBAI Data Preparation Script
Scottish Child Payment Dissertation
=====================================
Loads HBAI individual-level data (2016/17-2023/24) and extracts:
  - Child material deprivation (MDCH, MDCHDMP, individual items)
  - Food security (FOODSEC)
  - Geography, weights, and DiD variables

Files used (from 23-24prices folder — all years in consistent 2023-24 prices):
  - i1518e_2324prices.tab  →  2015/16, 2016/17, 2017/18
  - i1821e_2324prices.tab  →  2018/19, 2019/20, 2020/21
  - i2124e_2324prices.tab  →  2021/22, 2022/23, 2023/24

Output: hbai_clean.csv — one row per child, Scotland and England only
"""

import pandas as pd
import numpy as np
import os
import warnings
warnings.filterwarnings("ignore")

from config import DATA_OUT, SCP_INTRO_YEAR, SCP_EXPAND_YEAR

# ─────────────────────────────────────────────────────────────────────────────
# CONFIG
# ─────────────────────────────────────────────────────────────────────────────
# Folder containing the HBAI UKDA-5828-tab download
HBAI_ROOT = "/Users/guypigott/python-venv-demo/Dissertation/UKDA-5828-tab/tab/23-24prices"

HBAI_FILES = [
    "i1518e_2324prices.tab",   # 2015/16 – 2017/18
    "i1821e_2324prices.tab",   # 2018/19 – 2020/21
    "i2124e_2324prices.tab",   # 2021/22 – 2023/24
]

# Output path (alongside FRS merged data)
out_dir = os.path.dirname(DATA_OUT)
HBAI_OUT = os.path.join(out_dir, "hbai_clean.csv")

# Variables to extract
KEEP_VARS = [
    # Identifiers
    "SERNUM", "BENUNIT", "PERSON", "YEAR",
    # Geography (note: GVTREGN is loaded from GS_INDWA column — TAB file column swap)
    "COUNTRY", "GVTREGN", "GS_INDWA",
    # Child age
    "AGE",
    # Child material deprivation — composite
    "MDCH",       # deprivation score (count of items lacking)
    "MDCHDMP",    # binary: any deprivation
    # Child material deprivation — individual items
    "MDCH_ACT",   # activities (school trips etc.)
    "MDCH_BED",   # bed / bedroom
    "MDCH_CEL",   # celebrations (birthday etc.)
    "MDCH_COAT",  # coat
    "MDCH_EQP",   # equipment (school)
    "MDCH_HOL",   # holiday
    "MDCH_LES",   # leisure
    "MDCH_PLAY",  # play
    "MDCH_PLY",   # outdoor play area
    "MDCH_TEA",   # food (fresh fruit/veg)
    "MDCH_TRP",   # trips
    "MDCH_VEG",   # vegetables
    # Combined measure
    "LOWINCMDCH",     # low income AND material deprivation
    "LOWINCMDCHSEV",  # low income AND severe material deprivation
    # Food security
    "FOODSEC",
    "FOODSEC_STATUS",
    # Survey weight
    "GS_INDCH",   # child-level grossing weight (used in official stats)
]


# ─────────────────────────────────────────────────────────────────────────────
# LOAD AND STACK
# ─────────────────────────────────────────────────────────────────────────────
all_dfs = []

for fname in HBAI_FILES:
    fpath = os.path.join(HBAI_ROOT, fname)
    if not os.path.exists(fpath):
        print(f"  ✗ NOT FOUND: {fpath}")
        continue

    print(f"Loading {fname}...")
    raw = pd.read_csv(fpath, sep="\t", low_memory=False)
    raw.columns = raw.columns.str.upper().str.strip()

    # Diagnostics on first file: show year and geography variable values
    if fname == HBAI_FILES[0]:
        print(f"  All columns: {list(raw.columns[:30])}...")
        for yvar in ["YEAR", "YEARFIN", "YEARN"]:
            if yvar in raw.columns:
                print(f"  {yvar} unique values: {sorted(raw[yvar].dropna().unique())[:10]}")
        if "GVTREGN" in raw.columns:
            print(f"  GVTREGN unique values: {sorted(raw['GVTREGN'].dropna().unique())}")
        if "COUNTRY" in raw.columns:
            print(f"  COUNTRY unique values: {sorted(raw['COUNTRY'].dropna().unique())}")
        if "MDCH" in raw.columns:
            print(f"  MDCH sample values: {raw['MDCH'].head(10).tolist()}")
            print(f"  MDCH dtype: {raw['MDCH'].dtype}")

    # The TAB file has GVTREGN and GS_INDWA swapped relative to the variable guide.
    # Diagnostic confirmed GS_INDWA contains region codes 1-13 (matching GVTREGN spec).
    # Rename on load to fix this.
    if "GS_INDWA" in raw.columns:
        raw = raw.rename(columns={"GS_INDWA": "GVTREGN_TRUE",
                                   "GVTREGN":  "GVTREGN_RAW"})

    # Add YEARFIN to KEEP_VARS lookup; also pick up corrected GVTREGN
    keep = [v for v in KEEP_VARS + ["YEARFIN", "GVTREGN_TRUE"] if v in raw.columns]
    missing = [v for v in KEEP_VARS if v not in raw.columns and v != "GVTREGN"]
    if missing:
        print(f"  ⚠ Not found in {fname}: {missing}")

    df = raw[keep].copy()

    # Restore GVTREGN from the correctly identified column
    if "GVTREGN_TRUE" in df.columns:
        df = df.rename(columns={"GVTREGN_TRUE": "GVTREGN"})

    # Coerce numeric columns
    for col in ["YEAR", "YEARFIN", "COUNTRY", "GVTREGN", "AGE", "MDCH", "GS_INDCH"]:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors="coerce")

    # YEARFIN is formatted YYZZ (e.g. 1617 = 2016/17); convert to year-ending integer
    if "YEARFIN" in df.columns:
        df["YEAR"] = 2000 + (df["YEARFIN"] % 100)
        print(f"  ℹ YEAR derived from YEARFIN: {sorted(df['YEAR'].dropna().unique().astype(int).tolist())}")

    # Filter to Scotland and England using GVTREGN (now correctly identified)
    # England regions: 1=NE, 2=NW, 4=Yorks, 5=EM, 6=WM, 7=East, 8=London, 9=SE, 10=SW
    # Scotland: 12  |  Wales: 11  |  NI: 13
    ENGLAND_REGIONS  = [1, 2, 4, 5, 6, 7, 8, 9, 10]
    SCOTLAND_REGION  = 12
    NORTHERN_ENGLAND = [1, 2, 4]   # NE, NW, Yorkshire — for robustness checks

    df = df[df["GVTREGN"].isin(ENGLAND_REGIONS + [SCOTLAND_REGION])]
    df["scotland"]        = (df["GVTREGN"] == SCOTLAND_REGION).astype(int)
    df["northern_england"]= (df["GVTREGN"].isin(NORTHERN_ENGLAND)).astype(int)
    print(f"  GVTREGN check — Scotland: {(df['scotland']==1).sum():,}, "
          f"England: {(df['scotland']==0).sum():,}")

    # Recode negative sentinel values to NaN
    for col in df.select_dtypes(include=[np.number]).columns:
        df[col] = df[col].where(df[col] >= 0, np.nan)

    print(f"  → {len(df):,} rows (Scotland + England)")
    all_dfs.append(df)

if not all_dfs:
    raise FileNotFoundError(
        f"No HBAI files found. Check HBAI_ROOT: {HBAI_ROOT}"
    )

df = pd.concat(all_dfs, ignore_index=True)
print(f"\nCombined: {len(df):,} rows, {df['YEAR'].nunique()} years")

# ─────────────────────────────────────────────────────────────────────────────
# SAMPLE RESTRICTION
# ─────────────────────────────────────────────────────────────────────────────
# Restrict to children aged 16 and under (matches SCP eligibility and the paper)
if "AGE" in df.columns:
    df["AGE"] = pd.to_numeric(df["AGE"], errors="coerce")
    df = df[df["AGE"] <= 16]
    print(f"After restricting to age ≤16: {len(df):,} rows")

# Exclude 2020/21 (COVID disruption year — YEAR == 2021 in our convention)
df = df[df["YEAR"] != 2021]
print(f"After excluding 2020/21: {len(df):,} rows")

# ─────────────────────────────────────────────────────────────────────────────
# CONSTRUCT MDCH FROM INDIVIDUAL ITEMS
# Individual items coded: 1=deprived, 2=not deprived, 3=not applicable, -9=DK
# MDCH composite column in TAB file is blank — build it ourselves.
# ─────────────────────────────────────────────────────────────────────────────
MDCH_ITEMS = ["MDCH_ACT", "MDCH_BED", "MDCH_CEL", "MDCH_COAT", "MDCH_EQP",
              "MDCH_HOL", "MDCH_LES", "MDCH_PLAY", "MDCH_PLY", "MDCH_TEA",
              "MDCH_TRP", "MDCH_VEG"]

available_items = [c for c in MDCH_ITEMS if c in df.columns]
if available_items:
    print(f"\nConstructing MDCH from {len(available_items)} items: {available_items}")
    recoded = pd.DataFrame(index=df.index)
    for item in available_items:
        col = pd.to_numeric(df[item], errors="coerce")
        # 1 = deprived → 1; 2 = not deprived → 0; 3/−9 = N/A → NaN
        recoded[item] = col.map({1: 1, 2: 0}).where(col.isin([1, 2]), np.nan)

    df["mdch_count"]  = recoded.sum(axis=1, min_count=1)   # NaN if all items missing
    df["mdch_any"]    = np.where(df["mdch_count"].isna(), np.nan,
                                  (df["mdch_count"] > 0).astype(float))
    df["mdch_severe"] = np.where(df["mdch_count"].isna(), np.nan,
                                  (df["mdch_count"] >= 3).astype(float))
    print(f"  mdch_count non-missing: {df['mdch_count'].notna().sum():,}")
    print(f"  mdch_any   mean (where asked): {df['mdch_any'].mean():.3f}")
else:
    print("  ⚠ No MDCH individual items found — mdch_count/any/severe not constructed")

# ─────────────────────────────────────────────────────────────────────────────
# DiD VARIABLES
# ─────────────────────────────────────────────────────────────────────────────
df["post"]    = (df["YEAR"] >= SCP_EXPAND_YEAR).astype(int)
df["treated"] = df["scotland"].copy()
df["did"]     = df["treated"] * df["post"]

# ─────────────────────────────────────────────────────────────────────────────
# DIAGNOSTICS
# ─────────────────────────────────────────────────────────────────────────────
print("\n--- Sample by year and group ---")
print(df.groupby(["YEAR", "scotland"]).size().unstack(fill_value=0))

print("\n--- MDCH coverage and distribution ---")
if "MDCH" in df.columns:
    print(df["MDCH"].describe())
    print(f"  Non-missing: {df['MDCH'].notna().sum():,}")

if "MDCHDMP" in df.columns:
    print(f"\nMDCHDMP (binary deprivation):")
    print(df["MDCHDMP"].value_counts(dropna=False))

print("\n--- FOODSEC coverage ---")
if "FOODSEC" in df.columns:
    print(df["FOODSEC"].value_counts(dropna=False))

# ─────────────────────────────────────────────────────────────────────────────
# SAVE
# ─────────────────────────────────────────────────────────────────────────────
os.makedirs(out_dir, exist_ok=True)
df.to_csv(HBAI_OUT, index=False)
print(f"\n✓ Saved to {HBAI_OUT}")
print(f"  Shape: {df.shape}")
