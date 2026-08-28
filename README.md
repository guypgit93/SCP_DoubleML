# SCP_DoubleML

Statistical analysis of the Scottish Child Payment (SCP) on child material deprivation, using a covariate-adjusted OLS difference-in-differences design and a doubly robust, cross-fitted double/debiased machine learning (DML) DiD estimator (Zimmert, 2020). Replication material for the MSc Economics and Data Science dissertation *"The Scottish Child Payment and Child Material Deprivation: A Doubly Robust Difference-in-Differences Analysis"* (Guy Pigott, University of Manchester, ECON65000).

## Data

All analysis uses the **harmonised Households Below Average Income (HBAI) extract of the Family Resources Survey (FRS)**, UK Data Service study number **5828**, distributed under a UKDS End User Licence via the UK Data Archive. Access requires a UKDS account and acceptance of the licence terms; the raw and processed data files are **not included in this repository** in line with that licence.

- Raw HBAI `.tab` files (FYE2016–FYE2024, 2023/24 prices): obtained from UKDS study 5828.
- Raw FRS household-level files (for exact interview dates, used to refine the SCP post-treatment cutoff to 14 November 2022): obtained from the corresponding FRS UKDS study series.
- `Scripts/01_hbai_prep.R` and `Scripts/01b_hbai_prep_placebo.R` build the two processed extracts (`hbai_clean.csv`, `hbai_clean_placebo.csv`) that every other script reads. These processed CSVs are also excluded from the repository under the same licence terms; anyone re-running this pipeline needs their own UKDS access and must set `DATA_ROOT` in `01_hbai_prep.R` to their own local path.

## Requirements

R (tidyverse, data.table, fixest, modelsummary, patchwork, causalweight, sandwich). No Python is used in this project.

## Pipeline: which script produces what

Run in the order below; scripts 03–09, 12 and 13 each depend only on `hbai_clean.csv` from script 01, not on each other, so they can be run in any order after that. Script 11 must be run last, since it reformats the CSV outputs of 03–10 into the final `.tex` tables used in the dissertation.

| Script | Purpose | Key outputs (`tables/`, `figures/`) |
|---|---|---|
| `01_hbai_prep.R` | Builds the main analytic extract: restricts to England/Scotland children, constructs the MDCH flag, the ten deprivation items, `mdch_any`/`mdch_severe` composites, food insecurity, and the exact 14-Nov-2022 SCP post-treatment cutoff from FRS interview dates. | `hbai_clean.csv` (data, not checked in) |
| `01b_hbai_prep_placebo.R` | Same construction as above but including Wales and Northern Ireland, for the falsification checks. | `hbai_clean_placebo.csv` (data, not checked in) |
| `02_hbai_summary_stats.R` / `02_summary_stats.R` | Descriptive statistics: sample composition by year/country, outcome prevalence by country/period, background characteristics table. | `sample_composition.csv`, `background_characteristics.csv`, `summary_table.csv`, `table_a1_by_period.csv` |
| `03_stage1_baseline_did.R` | Stage 1: baseline OLS DiD (Simple/Adjusted/Extended specifications) for the official MDCH flag, food insecurity, and the two composite outcomes; CASE exact-replication spec; FYE2022-exclusion and age-restricted robustness checks. | `table_simple_did.csv`, `table_adj_did.csv`, `table_adj_ext_did.csv`, `table_case_exact_did.csv`, `table_fye2022_excl_did.csv`, `table_age_restricted_did.csv`, `table_item_did.csv` |
| `04_stage2_item_did.R` | Stage 2: item-stacked DiD (pooled and item-interacted, household-clustered). | `stacked_item_did.csv` |
| `05_stage1_parallel_trends.R` | Annual-level parallel-trends diagnostics (covariate balance + event-study Wald tests), Benjamini–Hochberg corrected. | `table_A2_pretrend_wald.csv`, `table_A3_pretrend_wald.csv`, event-study figures |
| `05b_stage1_pretrend_diagnostics.R` | Supporting diagnostics for the parallel-trends check (alternative variance-covariance estimators, cluster structure, placebo-region distribution). | `table_pretrend_*.csv` |
| `05c_stage1_parallel_trends_quarterly.R` | Quarterly-level parallel-trends check (rolling 13-week windows), replicating the CASE evaluation's own event-study specification. | `table_A3_quarterly_*.csv`, quarterly event-study figures |
| `06_stage3_dml_lean.R` / `06b_stage3_dml_wide.R` | Stage 3: doubly robust cross-fitted DML DiD (`causalweight::didDML()`) for the official MDCH flag, lasso and random forest nuisance models, lean (6-covariate) and wide (~35-covariate) sets. | `dml_did_causalweight_comparison_MDCH.csv`, `dml_did_wide_covariates_MDCH.csv` |
| `07_stage4_dml_item.R` | Stage 4: the same DML estimator repeated separately for each of the ten deprivation items, wide covariate set. | `dml_did_item_level_wide.csv` |
| `08_stage3_summary_MDCH.R` | Assembles the full OLS-to-DML specification ladder for the official MDCH flag. | `table_stage3_spec_ladder_MDCH.csv/.tex` |
| `09_stage5_stacked_ml_did.R` | Stage 5: jointly estimated, vector-outcome DML DiD across all ten items (shared complete-case sample, multivariate outcome-regression learner). | `stage5_stacked_item_did.csv` |
| `10_placebo_wales_ni.R` | Falsification check: re-estimates the headline spec with Wales, and separately Northern Ireland, as the pseudo-treated unit against England, at SCP's real rollout date. | `placebo_wales_ni.csv/.tex` |
| `11_make_latex_tables.R` | Reformats the CSV outputs of scripts 03–10 into the final `.tex` tables `\input` directly into the dissertation (`Sections/Tables/`). Run last. | All `Sections/Tables/*.tex` files |
| `12_wild_cluster_bootstrap.R` | Robustness: hand-rolled wild cluster restricted (WCR) bootstrap-t inference on the headline Adjusted Scotland×Post coefficient (Cameron, Gelbach & Miller, 2008), as a confirmatory check on the household-clustered standard errors. Independent of every other script. | `table_wildboot_headline.csv` |
| `13_dml_stability_check.R` | Robustness: refits the preferred wide-covariate, random-forest DML cell (script 06b) 20 times under different cross-fitting seeds, to test finite-sample stability given the modest post-treatment Scottish sample (~800 observations). Uses `didDML_seeded()`, a local patch of `causalweight::didDML()` exposing its internally hard-coded `set.seed(1)` as a parameter. Checkpointed/resumable — safe to interrupt and rerun. Independent of every other script. | `tables/dml_stability_wide_rf_v2.csv` |

## Dissertation source

`latex/main.tex` compiles the dissertation itself (`pdflatex` → `bibtex` → `pdflatex` ×2). Section text lives in `Sections/*.tex`; tables produced by this pipeline live in `Sections/Tables/*.tex`.
