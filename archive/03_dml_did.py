"""
03_dml_did.py
Double/Debiased ML Difference-in-Differences
Scottish Child Payment → Child Material Deprivation
======================================================
References:
  Chang (2020)          "Double/Debiased Machine Learning for DiD"
                        Econometrics Journal 23(2): 177–191
  Sant'Anna & Zhao (2020) "Doubly Robust DiD"
                        Journal of Econometrics 219(1): 101–122

Three stages
────────────
  Stage 1 — Benchmark: linear DiD with covariates (replicates CASE/Stewart 2025)
  Stage 2 — DML-DiD:  composite outcomes (mdch_any, mdch_severe, food_insecure)
  Stage 3 — Item-level DML-DiD across all 12 MDCH binary items
             + Benjamini-Hochberg FDR correction (α = 0.05)

Treatment design
────────────────
  Treated  : Scotland (GVTREGN == 12)
  Control  : England  (GVTREGN ∈ {1,2,4–10})
  Pre      : FY 2016/17 – 2021/22  (excl. 2020/21 COVID year)
  Post     : FY 2022/23 – 2023/24  (SCP expanded to all <16, £25/week, Nov 2022)
  Note     : FY 2021/22 slightly contaminated by initial SCP for <6s — flagged
             in robustness checks but included in main analysis

Run after 01_hbai_lca_prep.py
Install:  pip install doubleml scikit-learn statsmodels --break-system-packages
"""

import pandas as pd
import numpy as np
import os
import warnings
warnings.filterwarnings("ignore")

# ── statsmodels for Stage 1 OLS ────────────────────────────────────────────
import statsmodels.formula.api as smf
from statsmodels.stats.multitest import multipletests

# ── doubleml ───────────────────────────────────────────────────────────────
try:
    from doubleml import DoubleMLDID, DoubleMLData
    HAS_DOUBLEML = True
except ImportError:
    HAS_DOUBLEML = False
    print("⚠  doubleml not installed.")
    print("   pip install doubleml --break-system-packages")

# ── sklearn learners ────────────────────────────────────────────────────────
from sklearn.ensemble import RandomForestRegressor, RandomForestClassifier
from sklearn.linear_model import LassoCV, LogisticRegressionCV
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

from config import HBAI_OUT, FIGURES_DIR, SCP_EXPAND_YEAR

# ─────────────────────────────────────────────────────────────────────────────
# CONFIGURATION
# ─────────────────────────────────────────────────────────────────────────────
ALPHA       = 0.05      # nominal FDR level for BH correction
N_FOLDS     = 5         # cross-fitting folds
N_REP       = 3         # repetitions (averaged to reduce fold-draw variance)
TRIM        = 0.01      # propensity score trimming: clip to [TRIM, 1-TRIM]
RANDOM_SEED = 42

os.makedirs(FIGURES_DIR, exist_ok=True)

# 12 MDCH binary items — each recoded 0=not deprived, 1=deprived in prep script
MDCH_ITEMS = [
    # MDCH_LES dropped: only 538 obs, 0% prevalence — zero variance, unusable
    # MDCH_ACT dropped: only ~13k obs — likely year-limited; re-add if confirmed
    #   to appear in pre-period years (check with year-coverage diagnostic)
    "MDCH_BED",   # bed / bedroom of own
    "MDCH_CEL",   # celebrations (birthday, festivals)
    "MDCH_COAT",  # warm coat
    "MDCH_EQP",   # school equipment (books, uniform)
    "MDCH_HOL",   # holiday away from home
    "MDCH_PLAY",  # indoor play / games / toys
    "MDCH_PLY",   # outdoor play area
    "MDCH_TEA",   # fresh fruit / vegetables (daily)
    "MDCH_TRP",   # trips / outings (day trips)
    "MDCH_VEG",   # vegetables (regular portions)
]

MDCH_LABELS = {
    "MDCH_BED":  "Bed/bedroom",
    "MDCH_CEL":  "Celebrations",
    "MDCH_COAT": "Warm coat",
    "MDCH_EQP":  "School equipment",
    "MDCH_HOL":  "Holiday",
    "MDCH_PLAY": "Indoor play",
    "MDCH_PLY":  "Outdoor play",
    "MDCH_TEA":  "Fresh fruit/veg",
    "MDCH_TRP":  "Trips/outings",
    "MDCH_VEG":  "Vegetables",
}

# Composite outcomes for Stage 2
COMPOSITE_OUTCOMES = {
    "mdch_any"    : "Any deprivation (≥1 item)",
    "mdch_severe" : "Severe deprivation (≥3 items)",
    "food_insecure": "Food insecure (low/very low)",
}

# Continuous / binary covariates used directly
# Excluded: outcome-derived (LOWINCMDCH*), identifiers, weights, pure noise
# S_OE_AHC included but flagged as potential mediator — robustness check drops it
BASE_COVS = [
    # Child / head demographics
    "AGE", "SEX", "AGEHD", "AGEHDBAND", "AGEHDBAND_KID", "SEXHD", "AGESP", "SEX_ADULT",
    # Family structure
    "COUPLE_KID", "NEWFAMBU_SINGLE", "NEWFAMBU_KID", "NUMBKIDS",
    "NEWFAMBU_WITH", "NEWFAMBU_WITH_WA", "NEWFAMBU_WITH_PN", "NEWFAMBU_WITH_PN_TOT",
    "ADULTB", "ADULTH", "ADULTHBAND",
    # Child age composition
    "KID0_1", "KID2_4", "KID5_7", "KID8_10", "KID11_12", "KID13_15", "KID16PLUS",
    "KIDECOBU", "KIDECOBU_WORK",
    # Disability
    "DISCORKID", "DISCORABFLG", "DIS", "DIS_TYPE", "DSCORFAM", "DSCORFAM_WORK",
    "BENBU_DISBEN", "BENBU_DLA", "BENBU_PIP", "DSCORANDBEN",
    # Employment / earnings
    "S_OE_GRO_PROP_EARN", "WINPAYBU", "WINPAYHD", "WINPAYSP", "EGRINCBU",
    # Income
    "S_OE_AHC", "S_OE_BHC", "S_OE_GRO", "S_OE_GRO_PROP_BEN",
    "S_OE_GRO_PROP_INV", "S_OE_HC", "ESBENIBU", "CHBENBU", "INCHILBU",
    # Benefit flags
    "BENBU_UC", "BENBU_UC_OR_EQUIV", "NEWFAMBU_UC",
    "BENBU_FSM", "BENBU_IS", "BENBU_JSA", "BENBU_ESA",
    "BENBU_HB", "BENBU_CTC", "BENBU_WTC", "WFTCBU", "BENBU_PC",
    # Housing costs (continuous)
    "ERENTBU", "EHCOST", "ES_HCOST",
]
# Categorical — one-hot encoded below: TENHBAI, PTENTYP2, EMPSTATI, ETH, ETHGRPHHPUB, ETHGRPHH, GVTREGN


# ─────────────────────────────────────────────────────────────────────────────
# LOAD DATA
# ─────────────────────────────────────────────────────────────────────────────
print(f"Loading {HBAI_OUT}...")
df = pd.read_csv(HBAI_OUT, low_memory=False)
print(f"  Rows: {len(df):,}  |  Years: {sorted(df['YEAR'].unique().astype(int).tolist())}")

# MDCH items present
lca_items = [c for c in MDCH_ITEMS if c in df.columns]
print(f"  MDCH items present: {len(lca_items)}/12")

# DiD variable checks
assert "treated"  in df.columns, "Missing 'treated' column — re-run 01_hbai_lca_prep.py"
assert "post"     in df.columns, "Missing 'post' column"
assert "scotland" in df.columns, "Missing 'scotland' column"
assert "GVTREGN"  in df.columns, "Missing 'GVTREGN' column"


# ─────────────────────────────────────────────────────────────────────────────
# FEATURE ENGINEERING
# Build X matrix: base covariates + one-hot categoricals + year dummies
# Used in both Stage 1 (OLS controls) and Stage 2/3 (DML inputs)
# ─────────────────────────────────────────────────────────────────────────────

# One-hot encode categorical covariates
def ohe(df, col, prefix):
    """One-hot encode col if present; return (df, list_of_new_cols)."""
    if col not in df.columns:
        print(f"⚠  {col} not found — {prefix} dummies omitted")
        return df, []
    df[col] = pd.to_numeric(df[col], errors="coerce")
    dummies = pd.get_dummies(df[col], prefix=prefix, drop_first=True, dtype=float)
    return pd.concat([df, dummies], axis=1), list(dummies.columns)

df, tenhbai_cols  = ohe(df, "TENHBAI",     "tenure")
df, pten_cols     = ohe(df, "PTENTYP2",   "pten")
df, empstat_cols  = ohe(df, "EMPSTATI",   "empst")
df, eth_cols      = ohe(df, "ETH",        "eth")
df, ethpub_cols   = ohe(df, "ETHGRPHHPUB","ethpub")
df, ethdet_cols   = ohe(df, "ETHGRPHH",   "ethdet")
df, marital_cols  = ohe(df, "MARITAL_KID","marital")

# One-hot encode England regions (gives differential region trends)
# Scotland is the treatment so we exclude it; use NE as base England category
region_dummies = pd.get_dummies(
    df.loc[df["scotland"] == 0, "GVTREGN"],
    prefix="region", drop_first=True, dtype=float
)
df = df.join(region_dummies, how="left")
region_cols = list(region_dummies.columns)
df[region_cols] = df[region_cols].fillna(0)   # 0 for Scotland obs (absorbed into treatment)

# Year dummies — capture common time shocks
year_dummies = pd.get_dummies(df["YEAR"], prefix="yr", drop_first=True, dtype=float)
year_cols    = list(year_dummies.columns)
df = pd.concat([df, year_dummies], axis=1)

# Full covariate list for DML
avail_base = [c for c in BASE_COVS if c in df.columns]
X_COLS = (avail_base + tenhbai_cols + pten_cols + empstat_cols
          + eth_cols + ethpub_cols + ethdet_cols + marital_cols
          + region_cols + year_cols)
print(f"\nCovariate matrix: {len(X_COLS)} features")
print(f"  Base continuous/binary: {avail_base}")
print(f"  Tenure dummies:         {len(tenhbai_cols)}")
print(f"  Employment dummies:     {len(empstat_cols)}")
print(f"  Ethnicity dummies:      {len(eth_cols)}")
print(f"  Region dummies:         {len(region_cols)}")
print(f"  Year dummies:           {len(year_cols)}")


# ─────────────────────────────────────────────────────────────────────────────
# ML LEARNER FACTORY
# Primary: Random Forest (non-parametric, handles interactions)
# Robustness: Lasso / Logistic with L1 (linear, high interpretability)
# ─────────────────────────────────────────────────────────────────────────────
def make_rf_learners():
    ml_g = RandomForestRegressor(
        n_estimators=200, max_depth=5, min_samples_leaf=20,
        n_jobs=-1, random_state=RANDOM_SEED
    )
    ml_m = RandomForestClassifier(
        n_estimators=200, max_depth=5, min_samples_leaf=20,
        n_jobs=-1, random_state=RANDOM_SEED
    )
    return ml_g, ml_m


def make_lasso_learners():
    ml_g = Pipeline([
        ("scaler", StandardScaler()),
        ("lasso",  LassoCV(cv=5, max_iter=5000, random_state=RANDOM_SEED))
    ])
    ml_m = Pipeline([
        ("scaler", StandardScaler()),
        ("logit",  LogisticRegressionCV(
            cv=5, penalty="l1", solver="saga",
            max_iter=3000, random_state=RANDOM_SEED
        ))
    ])
    return ml_g, ml_m


# ─────────────────────────────────────────────────────────────────────────────
# HELPER: run one DML-DiD model
# ─────────────────────────────────────────────────────────────────────────────
def run_dml_did(df_sub, outcome_col, x_cols, learners, label=""):
    """
    Fits DoubleMLDID for a single outcome.
    Returns dict with coef, se, pval, ci_lo, ci_hi, n_obs.
    """
    cols_needed = [outcome_col, "treated", "post"] + x_cols
    sub = df_sub[cols_needed].dropna()
    n = len(sub)

    if n < 200:
        print(f"  ⚠  {label}: n={n} — too few obs, skipping")
        return None

    ml_g, ml_m = learners()

    try:
        dml_data = DoubleMLData(
            sub,
            y_col   = outcome_col,
            d_cols  = "treated",    # group indicator (scotland=1)
            x_cols  = x_cols,
            t_col   = "post",       # period indicator (post-SCP=1)
        )

        model = DoubleMLDID(
            obj_dml_data          = dml_data,
            ml_g                  = ml_g,
            ml_m                  = ml_m,
            n_folds               = N_FOLDS,
            n_rep                 = N_REP,
            score                 = "observational",   # models propensity score
            in_sample_normalization = True,
            trimming_threshold    = TRIM,
        )
        model.fit()
        ci = model.confint(level=1 - ALPHA)

        return {
            "n_obs"   : n,
            "coef"    : float(model.coef[0]),
            "se"      : float(model.se[0]),
            "tstat"   : float(model.t_stat[0]),
            "pval"    : float(model.pval[0]),
            "ci_lo"   : float(ci.iloc[0, 0]),
            "ci_hi"   : float(ci.iloc[0, 1]),
        }

    except Exception as e:
        print(f"  ✗  {label}: DML failed — {e}")
        return None


# ─────────────────────────────────────────────────────────────────────────────
# STAGE 1: BENCHMARK LINEAR DiD
# Replicates CASE paper (Stewart et al. 2025) — OLS with standard controls
# Outcome: mdch_any (binary), plus other composites
# ─────────────────────────────────────────────────────────────────────────────
print("\n" + "="*65)
print("STAGE 1 — BENCHMARK LINEAR DiD")
print("="*65)

# Build OLS covariate string (exclude year cols — absorbed by C(YEAR))
ols_base = " + ".join(
    [c for c in avail_base + tenhbai_cols + region_cols
     if c in df.columns]
)
ols_controls = f"C(YEAR) + {ols_base}" if ols_base else "C(YEAR)"

stage1_rows = []
df_mdch = df[df["mdch_observed"] == 1].copy() if "mdch_observed" in df.columns else df.copy()

for var, label in {**COMPOSITE_OUTCOMES,
                   **{item: MDCH_LABELS[item] for item in lca_items}}.items():
    data = df if var == "food_insecure" else df_mdch
    data = data.copy().dropna(subset=[var, "treated", "post"])
    if len(data) < 100:
        continue
    try:
        formula = f"{var} ~ did + treated + post + {ols_controls}"
        res = smf.ols(formula, data=data).fit(cov_type="HC3")
        row = {
            "outcome"   : var,
            "label"     : label,
            "n_obs"     : int(res.nobs),
            "coef"      : res.params.get("did", np.nan),
            "se"        : res.bse.get("did", np.nan),
            "tstat"     : res.tvalues.get("did", np.nan),
            "pval"      : res.pvalues.get("did", np.nan),
            "ci_lo"     : res.conf_int().loc["did", 0] if "did" in res.conf_int().index else np.nan,
            "ci_hi"     : res.conf_int().loc["did", 1] if "did" in res.conf_int().index else np.nan,
        }
        stage1_rows.append(row)
        sig = "**" if row["pval"] < 0.01 else ("*" if row["pval"] < 0.05 else "")
        print(f"  {label:<30}  coef={row['coef']:+.4f}  SE={row['se']:.4f}"
              f"  p={row['pval']:.3f}  n={row['n_obs']:,}{sig}")
    except Exception as e:
        print(f"  ✗  {label}: {e}")

s1_df = pd.DataFrame(stage1_rows)
s1_df.to_csv(f"{FIGURES_DIR}/stage1_linear_did.csv", index=False)
print(f"\n✓ Stage 1 saved → {FIGURES_DIR}/stage1_linear_did.csv")


# ─────────────────────────────────────────────────────────────────────────────
# STAGE 2: DML-DiD — COMPOSITE OUTCOMES
# ─────────────────────────────────────────────────────────────────────────────
if not HAS_DOUBLEML:
    print("\n⚠  doubleml not installed — skipping Stages 2 & 3")
    print("   Install: pip install doubleml --break-system-packages")
else:
    print("\n" + "="*65)
    print("STAGE 2 — DML-DiD: COMPOSITE OUTCOMES")
    print("="*65)
    print(f"  Learner: Random Forest  |  Folds: {N_FOLDS}  |  Reps: {N_REP}")

    stage2_rows = []
    for var, label in COMPOSITE_OUTCOMES.items():
        if var not in df.columns:
            print(f"  ⚠  {label}: column not found")
            continue
        data = df if var == "food_insecure" else df_mdch
        print(f"\n  → {label} (n≈{data[var].notna().sum():,})")

        # Primary: Random Forest
        result_rf = run_dml_did(data, var, X_COLS, make_rf_learners, label)
        if result_rf:
            result_rf.update({"outcome": var, "label": label, "learner": "Random Forest"})
            stage2_rows.append(result_rf)
            sig = "**" if result_rf["pval"] < 0.01 else ("*" if result_rf["pval"] < 0.05 else "")
            print(f"     RF:    coef={result_rf['coef']:+.4f}  SE={result_rf['se']:.4f}"
                  f"  p={result_rf['pval']:.3f}  [{result_rf['ci_lo']:+.4f}, {result_rf['ci_hi']:+.4f}]{sig}")

        # Robustness: Lasso
        result_l = run_dml_did(data, var, X_COLS, make_lasso_learners, label)
        if result_l:
            result_l.update({"outcome": var, "label": label, "learner": "Lasso"})
            stage2_rows.append(result_l)
            sig = "**" if result_l["pval"] < 0.01 else ("*" if result_l["pval"] < 0.05 else "")
            print(f"     Lasso: coef={result_l['coef']:+.4f}  SE={result_l['se']:.4f}"
                  f"  p={result_l['pval']:.3f}  [{result_l['ci_lo']:+.4f}, {result_l['ci_hi']:+.4f}]{sig}")

    s2_df = pd.DataFrame(stage2_rows)
    s2_df.to_csv(f"{FIGURES_DIR}/stage2_dml_composite.csv", index=False)
    print(f"\n✓ Stage 2 saved → {FIGURES_DIR}/stage2_dml_composite.csv")


    # ─────────────────────────────────────────────────────────────────────────
    # STAGE 3: DML-DiD ITEM-LEVEL DECOMPOSITION + BH FDR CORRECTION
    # ─────────────────────────────────────────────────────────────────────────
    print("\n" + "="*65)
    print("STAGE 3 — DML-DiD: ITEM-LEVEL DECOMPOSITION (12 MDCH items)")
    print(f"         + Benjamini-Hochberg FDR correction (α={ALPHA})")
    print("="*65)

    stage3_rows = []
    for item in lca_items:
        label = MDCH_LABELS.get(item, item)
        result = run_dml_did(df_mdch, item, X_COLS, make_rf_learners, label)
        if result is None:
            result = {"n_obs": 0, "coef": np.nan, "se": np.nan,
                      "tstat": np.nan, "pval": np.nan,
                      "ci_lo": np.nan, "ci_hi": np.nan}
        result.update({"outcome": item, "label": label})
        stage3_rows.append(result)
        print(f"  {label:<24}  coef={result['coef']:+.4f}  "
              f"SE={result['se']:.4f}  p={result['pval']:.3f}")

    s3_df = pd.DataFrame(stage3_rows)

    # Benjamini-Hochberg FDR correction across all 12 p-values
    valid_mask = s3_df["pval"].notna()
    if valid_mask.sum() >= 2:
        reject, pvals_adj, _, _ = multipletests(
            s3_df.loc[valid_mask, "pval"],
            alpha=ALPHA,
            method="fdr_bh"
        )
        s3_df.loc[valid_mask, "pval_bh"]  = pvals_adj
        s3_df.loc[valid_mask, "sig_bh"]   = reject
    else:
        s3_df["pval_bh"] = np.nan
        s3_df["sig_bh"]  = False

    # ── Summary table
    print("\n" + "-"*65)
    print(f"  {'Item':<24}  {'Coef':>8}  {'SE':>7}  {'p (raw)':>8}  "
          f"{'p (BH)':>8}  {'Sig?':>6}")
    print("-"*65)
    for _, row in s3_df.sort_values("pval").iterrows():
        sig_bh = "✓ Yes" if row.get("sig_bh", False) else "No"
        print(f"  {row['label']:<24}  {row['coef']:>+8.4f}  "
              f"{row['se']:>7.4f}  {row['pval']:>8.3f}  "
              f"{row.get('pval_bh', np.nan):>8.3f}  {sig_bh:>6}")

    n_sig = s3_df["sig_bh"].sum() if "sig_bh" in s3_df.columns else 0
    print(f"\n  Items significant after BH correction: {int(n_sig)}/12")

    s3_df.to_csv(f"{FIGURES_DIR}/stage3_dml_items_bh.csv", index=False)
    print(f"✓ Stage 3 saved → {FIGURES_DIR}/stage3_dml_items_bh.csv")


# ─────────────────────────────────────────────────────────────────────────────
# SUMMARY
# ─────────────────────────────────────────────────────────────────────────────
print("\n" + "="*65)
print("OUTPUTS")
print("="*65)
print(f"  {FIGURES_DIR}/stage1_linear_did.csv     — benchmark OLS DiD (all outcomes)")
if HAS_DOUBLEML:
    print(f"  {FIGURES_DIR}/stage2_dml_composite.csv  — DML-DiD composite (RF + Lasso)")
    print(f"  {FIGURES_DIR}/stage3_dml_items_bh.csv   — DML-DiD item decomposition + BH")
print("\nDone.")
