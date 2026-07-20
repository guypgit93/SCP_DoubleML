"""
Project configuration — edit this file to match your local setup.
This file is tracked by git. Do NOT hardcode absolute paths in other scripts.

To replicate:
  1. Download HBAI TAB files from UK Data Service (EUL access required)
  2. Set DATA_ROOT to the folder containing your UKDA-XXXX-tab folders
  3. Run 01_hbai_prep.R, then 02_summary_stats.R

Data prep is now in R (01_hbai_prep.R). Older FRS-based scripts, the
original Python prep script, and other superseded drafts have been moved
to archive/.
"""

import os

# ── Data ──────────────────────────────────────────────────────────────────────
# Folder containing UKDA-XXXX-tab subfolders downloaded from UK Data Service
DATA_ROOT = "/Users/guypigott/python-venv-demo/Dissertation"

# Output CSV (merged, cleaned FRS data) — gitignored, lives outside the repo
DATA_OUT = os.path.join(DATA_ROOT, "data", "frs_merged.csv")

# HBAI individual-level output — one row per child (Scotland + England)
HBAI_ROOT = os.path.join(DATA_ROOT, "UKDA-5828-tab", "tab", "23-24prices")
HBAI_OUT  = os.path.join(DATA_ROOT, "data", "hbai_lca.csv")

# ── Outputs ───────────────────────────────────────────────────────────────────
# Figures directory — inside the repo (PNGs are small, fine to commit)
FIGURES_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "figures")

# ── FRS year mapping ──────────────────────────────────────────────────────────
# UKDA study number → financial year integer (year ending)
YEARS = {
    "UKDA-8460": 2018,   # 2017-18
    "UKDA-8633": 2019,   # 2018-19
    "UKDA-8802": 2020,   # 2019-20  (food insecurity module starts)
    "UKDA-8948": 2021,   # 2020-21
    "UKDA-9073": 2022,   # 2021-22  (SCP introduced Feb 2021)
    "UKDA-9252": 2023,   # 2022-23  (SCP expanded to under-16s Nov 2022)
    "UKDA-9367": 2024,   # 2023-24
    "UKDA-9563": 2025,   # 2024-25
}

# ── SCP rollout ───────────────────────────────────────────────────────────────
SCP_INTRO_YEAR  = 2022   # FY 2021-22: introduced for under-6s at £10/week
SCP_EXPAND_YEAR = 2023   # FY 2022-23: extended to under-16s, doubled to £25/week
