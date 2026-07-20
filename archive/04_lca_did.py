"""
LCA + DiD Analysis
Scottish Child Payment Dissertation
=====================================
Implements:
  1. Latent Class Analysis (LCA) across hardship indicators
     → Produces latent class probabilities per family
  2. DiD using LCA-derived latent hardship as outcome
  3. Comparison table of DiD estimates across all measures
  4. Double ML (Chernozhukov et al. 2018) robustness check

Requires:
  pip install stepmix doubleml --break-system-packages

Run AFTER 01_data_prep.py (and 03_hbai_prep.py if using MDCH).
"""

import pandas as pd
import numpy as np
import os
import warnings
warnings.filterwarnings("ignore")

from config import DATA_OUT, FIGURES_DIR, SCP_EXPAND_YEAR

# ─────────────────────────────────────────────────────────────────────────────
# CONFIG
# ─────────────────────────────────────────────────────────────────────────────
os.makedirs(FIGURES_DIR, exist_ok=True)

# Hardship indicators used in LCA
# All must be binary (0/1) with NaN for missing
INDICATORS = [
    "food_insecure",    # FOODSEC ≥ 3 (low/very low food security)
    "fin_hardship",     # ADBTBL == 2 (behind on bills)
    "foodbank_use",     # FOODBKYR == 1
    "fuel_poverty",     # HOUSHE1 == 2 (can't keep home warm)
]

# Covariates for Double ML (add/remove as needed)
COVARIATES = [
    "YEAR",             # year FE (as continuous — dummies added below)
    "DEPCHLDB",         # number of dependent children
    "BURINC",           # benefit unit income
]

N_CLASSES = None   # set to None to auto-select by BIC (tries 2–5)

# ─────────────────────────────────────────────────────────────────────────────
# LOAD DATA AND RESTRICT SAMPLE
# ─────────────────────────────────────────────────────────────────────────────
df = pd.read_csv(DATA_OUT, low_memory=False)

# Same restriction as 02_summary_stats.py: children u16, Scotland & England,
# exclude 2021 (COVID year)
df = df[
    (df["n_children_u16"] > 0) &
    (df["YEAR"] != 2021)
].copy()

print(f"Analysis sample: {len(df):,} rows, {df['YEAR'].nunique()} years")
print(f"Scotland: {df['scotland'].sum():,}  |  England: {(df['scotland']==0).sum():,}")

# ─────────────────────────────────────────────────────────────────────────────
# STEP 1: PREPARE INDICATOR MATRIX
# ─────────────────────────────────────────────────────────────────────────────
available = [v for v in INDICATORS if v in df.columns]
print(f"\nIndicators available for LCA: {available}")

# Only keep rows with at least 2 non-missing indicators
indicator_df = df[available].copy()
n_observed = indicator_df.notna().sum(axis=1)
lca_mask = n_observed >= 2
print(f"Rows with ≥2 non-missing indicators: {lca_mask.sum():,}")

# For LCA, impute remaining NaNs with column mean (simple approach)
# (stepmix handles missing data natively; we flag which are observed)
X_lca = indicator_df[lca_mask].copy()
X_lca_imputed = X_lca.fillna(X_lca.mean())

# ─────────────────────────────────────────────────────────────────────────────
# STEP 2: FIT LCA — SELECT K BY BIC
# ─────────────────────────────────────────────────────────────────────────────
try:
    from stepmix.stepmix import StepMix
    USE_STEPMIX = True
except ImportError:
    print("stepmix not installed. Install with: pip install stepmix --break-system-packages")
    print("Falling back to sklearn GaussianMixture (less appropriate for binary data)")
    from sklearn.mixture import GaussianMixture
    USE_STEPMIX = False

print("\n--- Fitting LCA models (K=2 to 5) ---")
bic_scores = {}

if USE_STEPMIX:
    for k in range(2, 6):
        model = StepMix(
            n_components=k,
            measurement="binary",
            random_state=42,
            n_init=5,
            max_iter=500,
            verbose=0,
        )
        model.fit(X_lca_imputed.values)
        bic = model.bic(X_lca_imputed.values)
        bic_scores[k] = bic
        print(f"  K={k}: BIC={bic:.1f}")

    best_k = min(bic_scores, key=bic_scores.get) if N_CLASSES is None else N_CLASSES
    print(f"\nSelected K={best_k} (lowest BIC)")

    final_model = StepMix(
        n_components=best_k,
        measurement="binary",
        random_state=42,
        n_init=10,
        max_iter=500,
        verbose=0,
    )
    final_model.fit(X_lca_imputed.values)

    # Class probabilities for each observation
    probs = final_model.predict_proba(X_lca_imputed.values)  # shape (n, K)
    class_assign = final_model.predict(X_lca_imputed.values) # hard assignment

    # Print class profiles
    print("\n--- Class profiles (mean probability of each indicator per class) ---")
    params = final_model.get_parameters()
    if "measurement" in params and "pis" in params["measurement"]:
        profiles = pd.DataFrame(
            params["measurement"]["pis"],
            columns=available,
            index=[f"Class {k+1}" for k in range(best_k)]
        )
        print(profiles.round(3).to_string())

else:
    # Fallback: GaussianMixture on binary data (not ideal but functional)
    for k in range(2, 6):
        model = GaussianMixture(n_components=k, random_state=42, n_init=5)
        model.fit(X_lca_imputed.values)
        bic_scores[k] = model.bic(X_lca_imputed.values)
        print(f"  K={k}: BIC={bic_scores[k]:.1f}")

    best_k = min(bic_scores, key=bic_scores.get) if N_CLASSES is None else N_CLASSES
    final_model = GaussianMixture(n_components=best_k, random_state=42, n_init=10)
    final_model.fit(X_lca_imputed.values)
    probs = final_model.predict_proba(X_lca_imputed.values)
    class_assign = final_model.predict(X_lca_imputed.values)

# Identify the "hardship" class: the one with highest mean on indicators
class_means = np.array([
    X_lca_imputed.values[class_assign == k].mean()
    for k in range(best_k)
])
hardship_class = np.argmax(class_means)
print(f"\nHardship class identified as Class {hardship_class + 1} "
      f"(highest mean indicator score: {class_means[hardship_class]:.3f})")

# Attach results to the LCA subsample
lca_results = df[lca_mask].copy()
lca_results["lca_class"]          = class_assign
lca_results["lca_hardship_prob"]  = probs[:, hardship_class]
lca_results["lca_hardship_binary"]= (class_assign == hardship_class).astype(int)

print(f"\nLCA hardship class prevalence: {lca_results['lca_hardship_binary'].mean():.3f}")

# ─────────────────────────────────────────────────────────────────────────────
# STEP 3: DiD — ALL OUTCOMES INCLUDING LCA
# ─────────────────────────────────────────────────────────────────────────────
ALL_OUTCOMES = {
    "food_insecure"     : "Food insecure (FOODSEC)",
    "fin_hardship"      : "Financial hardship (bills)",
    "foodbank_use"      : "Food bank use",
    "fuel_poverty"      : "Fuel poverty",
    "lca_hardship_prob" : "LCA latent hardship probability",
    "lca_hardship_binary": "LCA hardship class (binary)",
}

def simple_did(data, outcome, treat_col="scotland", post_col="post"):
    """Computes naïve 2x2 DiD estimate with standard error via OLS."""
    sub = data[[outcome, treat_col, post_col]].dropna()
    if len(sub) < 50:
        return None
    # DiD = (Y_treat_post - Y_treat_pre) - (Y_ctrl_post - Y_ctrl_pre)
    cells = sub.groupby([treat_col, post_col])[outcome].agg(["mean","sem","count"])
    try:
        did = ((cells.loc[(1,1),"mean"] - cells.loc[(1,0),"mean"]) -
               (cells.loc[(0,1),"mean"] - cells.loc[(0,0),"mean"]))
        # Approximate SE via delta method (sum of SEMs in quadrature)
        se = np.sqrt(sum(cells.loc[idx,"sem"]**2
                         for idx in [(1,1),(1,0),(0,1),(0,0)]
                         if idx in cells.index))
        n = sub[treat_col].sum()
        return {"DiD": did, "SE": se, "t": did/se if se > 0 else np.nan,
                "N_treat": int(n), "N_ctrl": int(len(sub)-n)}
    except KeyError:
        return None

print("\n--- DiD estimates across all outcomes ---")
print(f"{'Outcome':<40} {'DiD':>8} {'SE':>8} {'t-stat':>8} {'N_treat':>8}")
print("-" * 76)

results = {}
for var, label in ALL_OUTCOMES.items():
    # Use lca_results for LCA outcomes, df for others
    data = lca_results if var.startswith("lca") else df
    if var not in data.columns:
        continue
    r = simple_did(data, var)
    if r is None:
        print(f"  {label:<38} {'insufficient data':>30}")
        continue
    results[label] = r
    sig = "*" if abs(r["t"]) > 1.96 else ""
    print(f"  {label:<38} {r['DiD']:>8.4f} {r['SE']:>8.4f} "
          f"{r['t']:>8.2f} {r['N_treat']:>8}{sig}")

# Save results table
results_df = pd.DataFrame(results).T.reset_index()
results_df.columns = ["Outcome"] + list(results_df.columns[1:])
results_df.to_csv(f"{FIGURES_DIR}/table_did_comparison.csv", index=False)
print(f"\n✓ DiD comparison table saved")

# ─────────────────────────────────────────────────────────────────────────────
# STEP 4: DOUBLE ML ROBUSTNESS CHECK
# ─────────────────────────────────────────────────────────────────────────────
print("\n--- Double ML robustness check ---")
try:
    import doubleml as dml
    from sklearn.linear_model import LassoCV
    from sklearn.ensemble import RandomForestRegressor, RandomForestClassifier

    outcomes_for_dml = {
        "food_insecure"     : "Food insecure",
        "fin_hardship"      : "Financial hardship",
        "lca_hardship_prob" : "LCA latent hardship",
    }

    # Build covariate matrix: year dummies + numeric covariates
    dml_data = lca_results.copy()
    year_dummies = pd.get_dummies(dml_data["YEAR"], prefix="yr", drop_first=True)
    dml_data = pd.concat([dml_data, year_dummies], axis=1)
    year_dummy_cols = list(year_dummies.columns)

    numeric_covs = [c for c in COVARIATES if c != "YEAR" and c in dml_data.columns]
    X_cols = numeric_covs + year_dummy_cols

    print(f"  Covariates: {len(X_cols)} ({len(year_dummy_cols)} year dummies + {len(numeric_covs)} numeric)")

    for outcome, label in outcomes_for_dml.items():
        if outcome not in dml_data.columns:
            continue
        sub = dml_data[[outcome, "did", "scotland", "post"] + X_cols].dropna()
        if len(sub) < 100:
            print(f"  {label}: insufficient data (n={len(sub)})")
            continue

        obj = dml.DoubleMLData(
            sub,
            y_col=outcome,
            d_col="did",       # treatment = DiD interaction
            x_cols=X_cols,
        )
        # Use LASSO for outcome model, Random Forest for treatment model
        ml_g = LassoCV(cv=5)
        ml_m = RandomForestClassifier(n_estimators=100, random_state=42)

        dml_model = dml.DoubleMLPLR(obj, ml_g, ml_m, n_folds=5, score="partialling out")
        dml_model.fit()

        coef = dml_model.coef[0]
        se   = dml_model.se[0]
        t    = coef / se
        sig  = "*" if abs(t) > 1.96 else ""
        print(f"  {label:<35} coef={coef:.4f}  SE={se:.4f}  t={t:.2f}{sig}")

except ImportError:
    print("  doubleml not installed. Install with: pip install doubleml --break-system-packages")
    print("  Skipping Double ML step.")

print("\nDone. Key output: figures/table_did_comparison.csv")
