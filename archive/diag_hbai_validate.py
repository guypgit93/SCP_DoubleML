"""
HBAI Variable Validation
Checks each key variable against its expected values from the variable guide.
Run this before trusting any HBAI analysis outputs.
"""
import pandas as pd
import numpy as np

PATH = "/Users/guypigott/python-venv-demo/Dissertation/UKDA-5828-tab/tab/23-24prices/i1518e_2324prices.tab"

print("Reading first 1000 rows...")
df = pd.read_csv(PATH, sep="\t", nrows=1000, low_memory=False)
df.columns = df.columns.str.upper().str.strip()

# ─────────────────────────────────────────────────────────────────────────────
# Expected value ranges from variable guide
# For each variable: (expected_min, expected_max, expected_unique_values_or_None)
# ─────────────────────────────────────────────────────────────────────────────
EXPECTED = {
    # Geography
    "GVTREGN"  : {"desc": "Govt region (1-13)",       "valid": set(range(1, 14))},
    "GS_INDWA" : {"desc": "Working-age adult weight",  "min": 100, "max": 100_000},
    "COUNTRY"  : {"desc": "Country (1=Eng, 2=Scot)",   "valid": {0, 1, 2, 3}},
    # Year
    "YEARFIN"  : {"desc": "Year (YYZZ format)",        "valid": {1516,1617,1718,1819,1920,2021,2122,2223,2324,2425}},
    # Child characteristics
    "AGE"      : {"desc": "Age (0-120)",                "min": 0,   "max": 120},
    "SEX"      : {"desc": "Sex (1=male, 2=female)",     "valid": {1, 2}},
    # Child material deprivation
    "MDCH"     : {"desc": "Child mat dep (0/1 binary)", "valid": {0, 1}},
    "MDCH_HOL" : {"desc": "No holiday (0/1)",           "valid": {0, 1}},
    "MDCH_BED" : {"desc": "No bed (0/1)",               "valid": {0, 1}},
    "MDCH_COAT": {"desc": "No coat (0/1)",              "valid": {0, 1}},
    # Food security
    "FOODSEC"  : {"desc": "Food security (0-3)",        "valid": {0, 1, 2, 3}},
    # Weights
    "GS_INDCH" : {"desc": "Child weight (large int)",   "min": 100, "max": 500_000},
    "GS_INDHH" : {"desc": "Household weight",           "min": 100, "max": 500_000},
    # Income (should be £ amounts, potentially large)
    "BHCPUBDEF": {"desc": "BHC income (£ pw)",          "min": -500, "max": 10_000},
    # Benefit unit
    "BENUNIT"  : {"desc": "BU number (1-5)",            "valid": {1, 2, 3, 4, 5}},
    "SERNUM"   : {"desc": "Serial number (sequential)", "min": 1, "max": 999_999},
}

print(f"\n{'Variable':<15} {'Expected':<35} {'Actual min':>10} {'Actual max':>10} {'N unique':>8}  Status")
print("-" * 95)

for var, spec in EXPECTED.items():
    if var not in df.columns:
        print(f"{var:<15} {spec['desc']:<35} {'NOT IN FILE':>32}")
        continue

    col = pd.to_numeric(df[var], errors="coerce").dropna()
    if len(col) == 0:
        print(f"{var:<15} {spec['desc']:<35} {'ALL NaN / TEXT':>32}")
        continue

    actual_min   = col.min()
    actual_max   = col.max()
    actual_unique = set(col.unique().astype(int)) if col.max() < 10_000 else None
    n_unique     = col.nunique()

    if "valid" in spec:
        overlap = actual_unique & spec["valid"] if actual_unique else set()
        extra   = actual_unique - spec["valid"] if actual_unique else set()
        ok = len(extra) == 0 and len(overlap) > 0
        status = "✓ OK" if ok else f"✗ UNEXPECTED values: {sorted(extra)[:5]}"
    else:
        ok = actual_min >= spec["min"] and actual_max <= spec["max"]
        status = "✓ OK" if ok else f"✗ range [{actual_min:.0f}, {actual_max:.0f}] outside [{spec['min']}, {spec['max']}]"

    print(f"{var:<15} {spec['desc']:<35} {actual_min:>10.0f} {actual_max:>10.0f} {n_unique:>8}  {status}")

print("\n--- Raw sample values for key variables ---")
for var in ["GVTREGN", "GS_INDWA", "GS_INDCH", "MDCH", "FOODSEC", "AGE", "YEARFIN"]:
    if var in df.columns:
        vals = pd.to_numeric(df[var], errors="coerce").dropna()
        sample = sorted(vals.unique().astype(int).tolist())[:15]
        print(f"  {var}: {sample}")
