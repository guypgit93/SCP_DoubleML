"""
Diagnostic: find the correct GVTREGN column in HBAI TAB files.
Run this locally and paste the output.
"""
import pandas as pd

PATH = "/Users/guypigott/python-venv-demo/Dissertation/UKDA-5828-tab/tab/23-24prices/i1518e_2324prices.tab"

df = pd.read_csv(PATH, sep="\t", nrows=500, low_memory=False)
df.columns = df.columns.str.upper().str.strip()

print(f"Total columns: {len(df.columns)}")
print(f"\n--- Columns whose values are a subset of 1-13 (region candidates) ---")
for col in df.columns:
    try:
        vals = pd.to_numeric(df[col], errors="coerce").dropna().unique()
        vals_int = set(int(v) for v in vals)
        if vals_int and vals_int.issubset(set(range(1, 14))) and len(vals_int) >= 3:
            print(f"  {col}: {sorted(vals_int)}")
    except Exception:
        pass

print(f"\n--- GVTREGN / NEWGVTREGN / COUNTRY sample values ---")
for col in ["GVTREGN", "NEWGVTREGN", "COUNTRY"]:
    if col in df.columns:
        vals = pd.to_numeric(df[col], errors="coerce").value_counts().sort_index().head(20)
        print(f"\n  {col}:\n{vals.to_string()}")
    else:
        print(f"\n  {col}: NOT FOUND")

print(f"\n--- SERNUM sample values (for comparison) ---")
if "SERNUM" in df.columns:
    print(df["SERNUM"].head(10).tolist())
