"""
Summary Statistics — HBAI LCA Sample
Scottish Child Payment Dissertation
======================================
Run AFTER 01_hbai_lca_prep.py

Produces:
  1. Table 1  — sample counts by year × group (Scotland / England)
  2. Table 2  — MDCH item prevalences by group and period
  3. Table 3  — composite MDCH statistics by group and period
  4. Table 4  — FOODSEC distribution by group and period
  5. Figure 1 — parallel trends: mdch_any (proportion any deprivation)
  6. Figure 2 — parallel trends: each MDCH item
  7. Figure 3 — MDCH item heatmap (prevalence × year × group)

All tables saved as CSVs; all figures as PNG in figures/
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import os
from scipy import stats as scipy_stats

from config import HBAI_OUT, FIGURES_DIR, SCP_EXPAND_YEAR

os.makedirs(FIGURES_DIR, exist_ok=True)

TREAT_YEAR = SCP_EXPAND_YEAR   # 2023 = FY 2022-23

MDCH_LABELS = {
    "MDCH_ACT":  "Activities (school trips)",
    "MDCH_BED":  "Bed / bedroom",
    "MDCH_CEL":  "Celebrations",
    "MDCH_COAT": "Warm coat",
    "MDCH_EQP":  "School equipment",
    "MDCH_HOL":  "Holiday away",
    "MDCH_LES":  "Leisure / hobby",
    "MDCH_PLAY": "Indoor play / games",
    "MDCH_PLY":  "Outdoor play area",
    "MDCH_TEA":  "Fresh fruit / veg",
    "MDCH_TRP":  "Trips / outings",
    "MDCH_VEG":  "Vegetables",
}


# ─────────────────────────────────────────────────────────────────────────────
# LOAD DATA
# ─────────────────────────────────────────────────────────────────────────────
print(f"Loading {HBAI_OUT}...")
df = pd.read_csv(HBAI_OUT, low_memory=False)
print(f"Loaded: {len(df):,} rows, {df['YEAR'].nunique()} years")

# Identify which MDCH items are present
lca_items = [c for c in MDCH_LABELS if c in df.columns]
print(f"LCA indicator columns present: {len(lca_items)}")

df = df.copy()   # defragment after wide load
df["group"]  = df["scotland"].map({1: "Scotland", 0: "England"})
df["period"] = df["post"].map({1: "Post-SCP (2023–24)", 0: "Pre-SCP (2017–22)"})

# ── Derived indicator columns used in the balance table ──────────────────────

# SEX: 1=male, 2=female
df["female"]       = (pd.to_numeric(df["SEX"], errors="coerce") == 2).astype(float)

# Lone-parent family: ADULTH==1 means only one adult in the household
# (NEWFAMBU_SINGLE is "single benefit unit in family", not lone-parent)
df["lone_parent"]  = (pd.to_numeric(df["ADULTH"], errors="coerce") == 1).astype(float)

# Individual disability: DIS is the person-level flag (1=disabled)
df["child_dis"]    = (pd.to_numeric(df.get("DIS"), errors="coerce") == 1).astype(float)

# TENHBAI in this dataset has only 3 values (confirmed from value counts):
#   1 = owner-occupied (owned outright or buying with mortgage)
#   2 = social rented  (LA or housing association)
#   3 = private rented
_ten = pd.to_numeric(df["TENHBAI"], errors="coerce")
df["owner_occ"]    = (_ten == 1).astype(float)
df["social_rent"]  = (_ten == 2).astype(float)
df["private_rent"] = (_ten == 3).astype(float)

# Employment: WINPAYBU = wage & salary income for BU (>0 means BU has earnings)
# EGRINCBU is total gross income so near-universal — use WINPAYBU instead
df["any_earnings"] = (pd.to_numeric(df.get("WINPAYBU"), errors="coerce") > 0).astype(float)


# ─────────────────────────────────────────────────────────────────────────────
# TABLE 0: Balance / Descriptive Statistics (pre-SCP period, Scotland vs England)
# Format: Variable | Scotland mean (SD) | England mean (SD) | Diff | p-value
# ─────────────────────────────────────────────────────────────────────────────
SUMSTAT_VARS = [
    # label                                          column              subsample
    ("A. Child and family demographics",              None,              None),
    ("  Child age (years)",                           "AGE",             "all"),
    ("  Female (%)",                                  "female",          "all"),
    ("  Lone-parent family (%)",                      "lone_parent",     "all"),
    ("  Child has disability (%)",                    "child_dis",       "all"),
    ("  Adults in household",                         "ADULTH",          "all"),
    ("B. Income and employment",                      None,              None),
    ("  Net equivalised income AHC (£/wk)",           "S_OE_AHC",       "all"),
    ("  Gross equivalised income BHC (£/wk)",         "S_OE_BHC",       "all"),
    ("  Benefit unit has any earnings (%)",           "any_earnings",    "all"),
    ("C. Benefit receipt",                            None,              None),
    ("  Universal Credit (%)",                        "BENBU_UC",        "all"),
    ("  Child Tax Credit (%)",                        "BENBU_CTC",       "all"),
    ("  Income Support (%)",                          "BENBU_IS",        "all"),
    ("  DLA / PIP recipient (%)",                     "BENBU_DLA",       "all"),
    ("D. Housing tenure",                             None,              None),
    ("  Owner-occupied (%)",                          "owner_occ",       "all"),
    ("  Social rented (%)",                           "social_rent",     "all"),
    ("  Private rented (%)",                          "private_rent",    "all"),
    ("E. Material deprivation outcomes",              None,              None),
    ("  Official DWP flag — MDCH (%)",                "MDCH",            "mdch_flag"),
    ("  Any deprivation, ≥1 item (%)",                "mdch_any",        "mdch"),
    ("  Severe deprivation, ≥3 items (%)",            "mdch_severe",     "mdch"),
    ("  Mean no. deprived items",                     "mdch_count",      "mdch"),
    ("F. Food security (FOODSEC obs. only)",           None,             None),
    ("  Food insecure (%)",                           "food_insecure",   "foodsec"),
    ("  Very low food security (%)",                  "very_low_food_sec","foodsec"),
]

pre = df[df["post"] == 0].copy()
pre["MDCH"] = pd.to_numeric(pre["MDCH"], errors="coerce")
pre_mdch      = pre[pre["mdch_observed"] == 1]
pre_mdch_flag = pre[pre["MDCH"].notna()]
pre_foodsec   = pre[pre["FOODSEC"].notna()]

scot_all       = pre[pre["scotland"] == 1]
eng_all        = pre[pre["scotland"] == 0]
scot_mdch      = pre_mdch[pre_mdch["scotland"] == 1]
eng_mdch       = pre_mdch[pre_mdch["scotland"] == 0]
scot_mdch_flag = pre_mdch_flag[pre_mdch_flag["scotland"] == 1]
eng_mdch_flag  = pre_mdch_flag[pre_mdch_flag["scotland"] == 0]
scot_food      = pre_foodsec[pre_foodsec["scotland"] == 1]
eng_food       = pre_foodsec[pre_foodsec["scotland"] == 0]

def group_frames(subsample):
    if subsample == "all":
        return scot_all, eng_all
    elif subsample == "mdch":
        return scot_mdch, eng_mdch
    elif subsample == "mdch_flag":
        return scot_mdch_flag, eng_mdch_flag
    else:
        return scot_food, eng_food

rows = []
for label, col, sub in SUMSTAT_VARS:
    if col is None:
        # Section header
        rows.append({"Variable": label, "Scotland": "", "England": "",
                     "Difference": "", "p-value": ""})
        continue
    if col not in df.columns:
        continue
    sg, eg = group_frames(sub)
    s_vals = pd.to_numeric(sg[col], errors="coerce").dropna()
    e_vals = pd.to_numeric(eg[col], errors="coerce").dropna()
    if len(s_vals) < 2 or len(e_vals) < 2:
        continue
    s_mean, s_sd = s_vals.mean(), s_vals.std()
    e_mean, e_sd = e_vals.mean(), e_vals.std()
    diff = s_mean - e_mean
    _, pval = scipy_stats.ttest_ind(s_vals, e_vals, equal_var=False)

    # Format: show SD for continuous, N in brackets for pct
    is_pct = label.strip().endswith("(%)")
    if is_pct:
        s_str = f"{s_mean*100:.1f}"
        e_str = f"{e_mean*100:.1f}"
        d_str = f"{diff*100:+.1f}"
    else:
        s_str = f"{s_mean:.2f} ({s_sd:.2f})"
        e_str = f"{e_mean:.2f} ({e_sd:.2f})"
        d_str = f"{diff:+.2f}"

    pval_str = f"{pval:.3f}" if pval >= 0.001 else "<0.001"
    rows.append({"Variable": label, "Scotland": s_str, "England": e_str,
                 "Difference": d_str, "p-value": pval_str})

# Add N rows
for sub, label in [("all",       "N (all children)"),
                   ("mdch_flag", "N (official MDCH flag obs.)"),
                   ("mdch",      "N (MDCH items observed)"),
                   ("foodsec",   "N (FOODSEC observed)")]:
    sg, eg = group_frames(sub)
    rows.append({"Variable": label,
                 "Scotland": f"{len(sg):,}", "England": f"{len(eg):,}",
                 "Difference": "", "p-value": ""})

t0 = pd.DataFrame(rows)
print("\n" + "="*80)
print("TABLE 0: Descriptive Statistics — Pre-SCP Period (Scotland vs England)")
print("="*80)
print(t0.to_string(index=False))
t0.to_csv(f"{FIGURES_DIR}/hbai_table0_balance.csv", index=False)
print(f"\n✓ Saved hbai_table0_balance.csv")


# ─────────────────────────────────────────────────────────────────────────────
# TABLE 1: Sample counts by year × group
# ─────────────────────────────────────────────────────────────────────────────
t1 = (df.groupby(["YEAR", "group"])
        .size()
        .unstack("group")
        .fillna(0)
        .astype(int))
t1["Total"] = t1.sum(axis=1)

# Also show how many have the MDCH module observed
t1_mdch = (df[df["mdch_observed"] == 1]
            .groupby(["YEAR", "group"])
            .size()
            .unstack("group")
            .fillna(0)
            .astype(int))
t1_mdch.columns = [f"{c} (MDCH obs.)" for c in t1_mdch.columns]
t1 = t1.join(t1_mdch, how="left")

print("\n" + "="*60)
print("TABLE 1: Sample counts by year and group")
print(t1.to_string())
t1.to_csv(f"{FIGURES_DIR}/hbai_table1_sample_counts.csv")


# ─────────────────────────────────────────────────────────────────────────────
# TABLE 2: MDCH item prevalences by group × period
# ─────────────────────────────────────────────────────────────────────────────
def item_prevalence_table(data, items, labels):
    rows = []
    for item in items:
        if item not in data.columns:
            continue
        for grp in ["Scotland", "England"]:
            for period in ["Pre-SCP (2017–22)", "Post-SCP (2023–24)"]:
                sub = data[(data["group"] == grp) & (data["period"] == period)][item]
                rows.append({
                    "Item"      : labels.get(item, item),
                    "Variable"  : item,
                    "Group"     : grp,
                    "Period"    : period,
                    "Prevalence": sub.mean(),
                    "N_obs"     : sub.notna().sum(),
                })
    return pd.DataFrame(rows)

t2 = item_prevalence_table(df, lca_items, MDCH_LABELS)
t2_pivot = t2.pivot_table(
    index="Item", columns=["Group", "Period"], values="Prevalence"
).round(3)
t2_pivot.columns = [" | ".join(c) for c in t2_pivot.columns]

print("\n" + "="*60)
print("TABLE 2: MDCH item prevalences by group and period")
print(t2_pivot.to_string())
t2.to_csv(f"{FIGURES_DIR}/hbai_table2_item_prevalences.csv", index=False)
t2_pivot.to_csv(f"{FIGURES_DIR}/hbai_table2_item_prevalences_pivot.csv")


# ─────────────────────────────────────────────────────────────────────────────
# TABLE 3: Composite MDCH statistics by group × period
# ─────────────────────────────────────────────────────────────────────────────
composite_vars = {
    "mdch_any"    : "Any deprivation (≥1 item)",
    "mdch_severe" : "Severe deprivation (≥3 items)",
    "mdch_count"  : "Mean number of items lacking",
}

rows_t3 = []
for var, label in composite_vars.items():
    if var not in df.columns:
        continue
    for grp in ["Scotland", "England"]:
        for period in ["Pre-SCP (2017–22)", "Post-SCP (2023–24)"]:
            sub = df[(df["group"] == grp) & (df["period"] == period)][var]
            rows_t3.append({
                "Measure": label,
                "Group"  : grp,
                "Period" : period,
                "Mean"   : sub.mean(),
                "SD"     : sub.std(),
                "N"      : sub.notna().sum(),
            })

t3 = pd.DataFrame(rows_t3)
t3_pivot = t3.pivot_table(
    index="Measure", columns=["Group", "Period"], values="Mean"
).round(3)
t3_pivot.columns = [" | ".join(c) for c in t3_pivot.columns]

print("\n" + "="*60)
print("TABLE 3: Composite MDCH statistics by group and period")
print(t3_pivot.to_string())
t3.to_csv(f"{FIGURES_DIR}/hbai_table3_composite_mdch.csv", index=False)
t3_pivot.to_csv(f"{FIGURES_DIR}/hbai_table3_composite_mdch_pivot.csv")


# ─────────────────────────────────────────────────────────────────────────────
# TABLE 4: FOODSEC distribution by group × period
# ─────────────────────────────────────────────────────────────────────────────
if "FOODSEC" in df.columns and df["FOODSEC"].notna().any():
    # HBAI FOODSEC is a 3-level scale: 1=secure, 2=low, 3=very low
    foodsec_labels = {1: "Food secure", 2: "Low food security", 3: "Very low food security"}
    rows_t4 = []
    for grp in ["Scotland", "England"]:
        for period in ["Pre-SCP (2017–22)", "Post-SCP (2023–24)"]:
            sub = df[(df["group"] == grp) & (df["period"] == period)]["FOODSEC"]
            n_total = sub.notna().sum()
            for cat, lbl in foodsec_labels.items():
                n_cat = (sub == cat).sum()
                rows_t4.append({
                    "Food security status": lbl,
                    "Group"              : grp,
                    "Period"             : period,
                    "N"                  : n_cat,
                    "Pct"                : n_cat / n_total if n_total > 0 else np.nan,
                })
    t4 = pd.DataFrame(rows_t4)
    t4_pivot = t4.pivot_table(
        index="Food security status", columns=["Group", "Period"], values="Pct"
    ).round(3)
    t4_pivot.columns = [" | ".join(c) for c in t4_pivot.columns]
    print("\n" + "="*60)
    print("TABLE 4: FOODSEC distribution by group and period (% of observed)")
    print(t4_pivot.to_string())
    t4.to_csv(f"{FIGURES_DIR}/hbai_table4_foodsec.csv", index=False)
else:
    print("\n⚠ FOODSEC not available — skipping Table 4")


# ─────────────────────────────────────────────────────────────────────────────
# FIGURE 1: Parallel trends — mdch_any (proportion with any deprivation)
# ─────────────────────────────────────────────────────────────────────────────
def parallel_trends_plot(data, var, label, save_path, fmt_pct=True):
    annual = (data.groupby(["YEAR", "scotland"])[var]
                  .mean()
                  .reset_index()
                  .dropna(subset=[var]))
    if annual.empty:
        print(f"  ✗ No data for {var}")
        return

    fig, ax = plt.subplots(figsize=(9, 5))
    styles = [
        (1, "Scotland (treated)", "#1f77b4", "o"),
        (0, "England (control)",  "#ff7f0e", "s"),
    ]
    for scot, lbl, color, marker in styles:
        sub = annual[annual["scotland"] == scot].sort_values("YEAR")
        ax.plot(sub["YEAR"], sub[var], label=lbl,
                color=color, marker=marker, linewidth=2, markersize=7)

    ax.axvline(x=TREAT_YEAR - 0.5, color="red", linestyle="--",
               linewidth=1.5, label="SCP expanded to under-16s (Nov 2022)")
    ymin, ymax = ax.get_ylim()
    ax.axvspan(annual["YEAR"].min(), TREAT_YEAR - 0.5, alpha=0.04, color="grey")

    ax.set_xlabel("Financial year (ending)", fontsize=12)
    ax.set_ylabel(label, fontsize=12)
    ax.set_title(f"Parallel Trends: {label}", fontsize=13, fontweight="bold")
    ax.legend(fontsize=10)
    ax.xaxis.set_major_locator(mticker.MultipleLocator(1))
    if fmt_pct:
        ax.yaxis.set_major_formatter(mticker.PercentFormatter(xmax=1, decimals=1))
    ax.grid(axis="y", alpha=0.3)
    plt.tight_layout()
    plt.savefig(save_path, dpi=150)
    plt.close()
    print(f"  ✓ {save_path}")

print("\nGenerating parallel trends plots...")
parallel_trends_plot(df[df["mdch_observed"] == 1],
                     "mdch_any",
                     "Proportion with any material deprivation",
                     f"{FIGURES_DIR}/hbai_fig1_parallel_trends_mdch_any.png")

parallel_trends_plot(df[df["mdch_observed"] == 1],
                     "mdch_severe",
                     "Proportion with severe material deprivation (≥3 items)",
                     f"{FIGURES_DIR}/hbai_fig1b_parallel_trends_mdch_severe.png")

if "food_insecure" in df.columns:
    parallel_trends_plot(df,
                         "food_insecure",
                         "Proportion food insecure (low or very low)",
                         f"{FIGURES_DIR}/hbai_fig1c_parallel_trends_food_insecure.png")

if "MDCH" in df.columns:
    df["MDCH"] = pd.to_numeric(df["MDCH"], errors="coerce")
    parallel_trends_plot(df[df["MDCH"].notna()],
                         "MDCH",
                         "Proportion materially deprived — official DWP flag (MDCH)",
                         f"{FIGURES_DIR}/hbai_fig1d_parallel_trends_MDCH.png")


# ─────────────────────────────────────────────────────────────────────────────
# FIGURE 2: Parallel trends for each individual MDCH item
# ─────────────────────────────────────────────────────────────────────────────
if lca_items:
    cols_fig = 3
    rows_fig = (len(lca_items) + cols_fig - 1) // cols_fig
    fig, axes = plt.subplots(rows_fig, cols_fig,
                             figsize=(cols_fig * 5, rows_fig * 3.8))
    axes = axes.flatten()

    df_mdch = df[df["mdch_observed"] == 1]
    for i, item in enumerate(lca_items):
        ax = axes[i]
        annual = (df_mdch.groupby(["YEAR", "scotland"])[item]
                         .mean()
                         .reset_index()
                         .dropna(subset=[item]))
        for scot, lbl, color, marker in [
            (1, "Scotland", "#1f77b4", "o"),
            (0, "England",  "#ff7f0e", "s"),
        ]:
            sub = annual[annual["scotland"] == scot].sort_values("YEAR")
            ax.plot(sub["YEAR"], sub[item], label=lbl,
                    color=color, marker=marker, linewidth=1.8, markersize=5)

        ax.axvline(x=TREAT_YEAR - 0.5, color="red", linestyle="--",
                   linewidth=1.0, alpha=0.7)
        ax.set_title(MDCH_LABELS.get(item, item), fontsize=9, fontweight="bold")
        ax.set_xlabel("Year", fontsize=8)
        ax.yaxis.set_major_formatter(mticker.PercentFormatter(xmax=1, decimals=0))
        ax.legend(fontsize=7)
        ax.grid(axis="y", alpha=0.3)
        ax.xaxis.set_major_locator(mticker.MultipleLocator(2))

    for j in range(i + 1, len(axes)):
        axes[j].set_visible(False)

    fig.suptitle("Parallel Trends: Individual MDCH Items\n"
                 "(Scotland vs England, children ≤16)",
                 fontsize=11, fontweight="bold")
    plt.tight_layout()
    path = f"{FIGURES_DIR}/hbai_fig2_parallel_trends_items.png"
    plt.savefig(path, dpi=150, bbox_inches="tight")
    plt.close()
    print(f"  ✓ {path}")


# ─────────────────────────────────────────────────────────────────────────────
# FIGURE 3: Heatmap — MDCH item prevalence by year (Scotland vs England)
# ─────────────────────────────────────────────────────────────────────────────
if lca_items:
    df_mdch = df[df["mdch_observed"] == 1]
    for grp, grp_label in [(1, "Scotland"), (0, "England")]:
        sub = df_mdch[df_mdch["scotland"] == grp]
        heat = (sub.groupby("YEAR")[lca_items]
                   .mean()
                   .rename(columns=MDCH_LABELS)
                   .T)
        heat.columns = heat.columns.astype(int)

        fig, ax = plt.subplots(figsize=(max(8, len(heat.columns) * 0.9),
                                        max(5, len(heat) * 0.55)))
        im = ax.imshow(heat.values, aspect="auto", cmap="YlOrRd", vmin=0, vmax=0.5)
        plt.colorbar(im, ax=ax, label="Proportion deprived", shrink=0.8)

        ax.set_xticks(range(len(heat.columns)))
        ax.set_xticklabels(heat.columns, rotation=45, ha="right", fontsize=9)
        ax.set_yticks(range(len(heat.index)))
        ax.set_yticklabels(heat.index, fontsize=9)

        # Mark treatment year
        post_years = [y for y in heat.columns if y >= TREAT_YEAR]
        if post_years:
            first_post = heat.columns.tolist().index(min(post_years))
            ax.axvline(x=first_post - 0.5, color="white", linewidth=2.5)
            ax.axvline(x=first_post - 0.5, color="black", linewidth=1.2,
                       linestyle="--", alpha=0.6)

        ax.set_title(f"MDCH Item Prevalences by Year — {grp_label}\n"
                     f"(white dashed = SCP expansion to under-16s)",
                     fontsize=11, fontweight="bold")
        plt.tight_layout()
        path = f"{FIGURES_DIR}/hbai_fig3_heatmap_{grp_label.lower()}.png"
        plt.savefig(path, dpi=150, bbox_inches="tight")
        plt.close()
        print(f"  ✓ {path}")


# ─────────────────────────────────────────────────────────────────────────────
# COVERAGE SUMMARY
# ─────────────────────────────────────────────────────────────────────────────
print("\n" + "="*60)
print("VARIABLE COVERAGE SUMMARY")
coverage_vars = lca_items + ["mdch_count", "mdch_any", "mdch_severe",
                              "FOODSEC", "food_insecure", "GS_INDCH"]
for v in coverage_vars:
    if v in df.columns:
        n = df[v].notna().sum()
        pct = 100 * n / len(df)
        print(f"  {v:20s}: {n:>8,} ({pct:5.1f}%)")

print(f"\nDone. Outputs saved to {FIGURES_DIR}/")
