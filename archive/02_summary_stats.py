"""
Summary Statistics and Parallel Trends Plots
Scottish Child Payment Dissertation
=====================================
Produces:
  1. Summary statistics table (treatment vs control, pre vs post)
  2. Parallel trends plots for each outcome variable
  3. Sample size table by year and group

Run AFTER 01_data_prep.py
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import os

from config import DATA_OUT, FIGURES_DIR, SCP_EXPAND_YEAR

DATA_PATH = DATA_OUT
TREAT_YEAR = SCP_EXPAND_YEAR
os.makedirs(FIGURES_DIR, exist_ok=True)

OUTCOMES = {
    "food_insecure"     : "Food insecure (any)",
    "very_low_food_sec" : "Very low food security",
    "foodbank_use"      : "Used food bank",
    "fin_hardship"      : "Financial hardship (behind on bills)",
    "matdep_count"      : "Material deprivation (item count)",
    "matdep_severe"     : "Severe material deprivation (3+ items)",
}


# ─────────────────────────────────────────────────────────────────────────────
# LOAD DATA
# ─────────────────────────────────────────────────────────────────────────────
df_all = pd.read_csv(DATA_PATH, low_memory=False)
print(f"Loaded {len(df_all):,} observations across {df_all['YEAR'].nunique()} years")

# ── Sample restriction ────────────────────────────────────────────────────────
# Restrict to SCP-eligible benefit units: UC or CTC recipients with children u16
# This mirrors the treatment eligibility criteria and is applied to both nations
df = df_all[
    (df_all["scp_eligible"] == 1) &
    (df_all["n_children_u16"] > 0)
].copy()
print(f"Analysis sample (SCP-eligible, children u16): {len(df):,} observations")

df["group"] = df["scotland"].map({1: "Scotland (treated)", 0: "England (control)"})


# ─────────────────────────────────────────────────────────────────────────────
# TABLE 1: Sample size by year and group
# ─────────────────────────────────────────────────────────────────────────────
sample_table = (
    df.groupby(["YEAR","group"])
    .size()
    .unstack("group")
    .fillna(0)
    .astype(int)
)
sample_table["Total"] = sample_table.sum(axis=1)
print("\n--- Sample size by year and group ---")
print(sample_table.to_string())
sample_table.to_csv(f"{FIGURES_DIR}/table_sample_size.csv")


# ─────────────────────────────────────────────────────────────────────────────
# TABLE 2: Summary statistics — pre/post × treatment/control
# ─────────────────────────────────────────────────────────────────────────────
def summary_stats(data, outcomes):
    rows = []
    for var, label in outcomes.items():
        if var not in data.columns:
            continue
        for period in ["Pre-SCP", "Post-SCP"]:
            post_flag = 1 if period == "Post-SCP" else 0
            sub = data[data["post"] == post_flag]
            for grp, grp_label in [(1, "Scotland"), (0, "England")]:
                cell = sub[sub["scotland"] == grp][var]
                rows.append({
                    "Outcome"  : label,
                    "Period"   : period,
                    "Group"    : grp_label,
                    "Mean"     : cell.mean(),
                    "SD"       : cell.std(),
                    "N"        : cell.notna().sum(),
                })
    return pd.DataFrame(rows)

stats_df = summary_stats(df, OUTCOMES)
# Pivot for readability
stats_pivot = stats_df.pivot_table(
    index=["Outcome","Period"],
    columns="Group",
    values=["Mean","N"],
    aggfunc="first"
).round(3)

print("\n--- Summary statistics ---")
print(stats_pivot.to_string())
stats_pivot.to_csv(f"{FIGURES_DIR}/table_summary_stats.csv")


# ─────────────────────────────────────────────────────────────────────────────
# FIGURE 1: Parallel trends plots — one per outcome
# ─────────────────────────────────────────────────────────────────────────────
def parallel_trends_plot(data, outcome_var, outcome_label, save_path):
    annual = (
        data.groupby(["YEAR","scotland"])[outcome_var]
        .mean()
        .reset_index()
    )
    annual = annual[annual[outcome_var].notna()]
    if annual.empty:
        print(f"  ✗ No data for {outcome_var}, skipping plot")
        return

    fig, ax = plt.subplots(figsize=(9, 5))

    for scot, label, color, marker in [
        (1, "Scotland (treated)", "#1f77b4", "o"),
        (0, "England (control)",  "#ff7f0e", "s"),
    ]:
        sub = annual[annual["scotland"] == scot].sort_values("YEAR")
        ax.plot(sub["YEAR"], sub[outcome_var],
                label=label, color=color, marker=marker,
                linewidth=2, markersize=7)

    # Treatment line
    ax.axvline(x=TREAT_YEAR - 0.5, color="red", linestyle="--",
               linewidth=1.5, label="SCP expanded to under-16s (Nov 2022)")

    # Shade pre/post
    ax.axvspan(ax.get_xlim()[0], TREAT_YEAR - 0.5, alpha=0.04, color="grey")

    ax.set_xlabel("Financial Year (ending)", fontsize=12)
    ax.set_ylabel(f"Mean: {outcome_label}", fontsize=12)
    ax.set_title(f"Parallel Trends: {outcome_label}", fontsize=13, fontweight="bold")
    ax.legend(fontsize=10)
    ax.xaxis.set_major_locator(mticker.MultipleLocator(1))
    ax.yaxis.set_major_formatter(mticker.PercentFormatter(xmax=1, decimals=1))
    ax.grid(axis="y", alpha=0.3)

    plt.tight_layout()
    plt.savefig(save_path, dpi=150)
    plt.close()
    print(f"  ✓ Saved {save_path}")

print("\nGenerating parallel trends plots...")
for var, label in OUTCOMES.items():
    save = f"{FIGURES_DIR}/parallel_trends_{var}.png"
    parallel_trends_plot(df, var, label, save)


# ─────────────────────────────────────────────────────────────────────────────
# FIGURE 2: Combined parallel trends — all outcomes in one figure (overview)
# ─────────────────────────────────────────────────────────────────────────────
valid_outcomes = [v for v in OUTCOMES if v in df.columns and df[v].notna().any()]
n = len(valid_outcomes)
if n > 0:
    cols = 2
    rows = (n + 1) // cols
    fig, axes = plt.subplots(rows, cols, figsize=(14, 4 * rows))
    axes = axes.flatten()

    for i, (var, label) in enumerate(OUTCOMES.items()):
        if var not in df.columns:
            continue
        ax = axes[i]
        annual = df.groupby(["YEAR","scotland"])[var].mean().reset_index()
        annual = annual[annual[var].notna()]

        for scot, lbl, color, marker in [
            (1, "Scotland", "#1f77b4", "o"),
            (0, "England",  "#ff7f0e", "s"),
        ]:
            sub = annual[annual["scotland"] == scot].sort_values("YEAR")
            ax.plot(sub["YEAR"], sub[var], label=lbl,
                    color=color, marker=marker, linewidth=2, markersize=5)

        ax.axvline(x=TREAT_YEAR - 0.5, color="red", linestyle="--", linewidth=1.2)
        ax.set_title(label, fontsize=10, fontweight="bold")
        ax.set_xlabel("Year", fontsize=9)
        ax.yaxis.set_major_formatter(mticker.PercentFormatter(xmax=1, decimals=0))
        ax.legend(fontsize=8)
        ax.grid(axis="y", alpha=0.3)

    # Hide unused subplots
    for j in range(i + 1, len(axes)):
        axes[j].set_visible(False)

    fig.suptitle("Parallel Trends: All Hardship Outcomes\n(Scotland vs England, low-income families with children)",
                 fontsize=12, fontweight="bold", y=1.01)
    plt.tight_layout()
    save = f"{FIGURES_DIR}/parallel_trends_combined.png"
    plt.savefig(save, dpi=150, bbox_inches="tight")
    plt.close()
    print(f"\n✓ Combined plot saved to {save}")

print("\nDone. Check the 'figures/' folder for outputs.")
