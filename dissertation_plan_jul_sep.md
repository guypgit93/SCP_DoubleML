# Dissertation Plan — 8 July to 1 September 2026

**Deadline:** 1 Sept 2026 (submit 31 Aug — 1-day buffer)
**Weight:** written dissertation 80%, max 7,000 words (incl. graphs/tables/refs), plus mandatory replication package
**Presentation feedback to incorporate:** (1) deprivation composite must be built identically for Scotland and England, (2) add Wales/NI as a validity/placebo check, (3) item-level analysis via a stacked DiD across the 10 MDCH indicators, not 10 separate regressions

---

## Where things stand

- `01_hbai_prep.R` currently filters to `COUNTRY %in% c(1,3)` (England + Scotland only). `mdch_count`/`mdch_any`/`mdch_severe` are already built from the same `item_mat` code path regardless of country, so your own composite is mechanically consistent — but see the two findings below on the official MDCH flag.
- Baseline DiD (`03_did_replication.R`), DML (`04_dml_did.R`), and parallel trends diagnostics (`05_parallel_trends.R`) all run and produce output in `tables/` and `figures/`.
- Item-level results currently come from per-item regressions, not a stacked specification.
- Literature review is essentially done.

### Finding 1 — the composite doesn't adjust for regional necessity

The old (pre-FYE2024) prevalence-weighted score weights each item by its national prevalence, calculated on the pooled sample, not separately by nation. The new FYE2024+ methodology drops weighting and counts items equally. Either way, an item like "warm coat" is treated identically regardless of climate, so genuine regional differences in necessity (e.g. Scotland vs England) aren't captured by the flag.

This is already visible in your own diagnostics: `table_A3_pretrend_wald.csv` flags "Warm coat" with a Wald p-value of 0.0001 (adjusted: 0.002) — "possible pre-trend" — meaning Scotland and England were already diverging on coat-lacking before the SCP rollout. `table_item_did.csv` shows a significant MDCH_COAT effect (–0.0076, p=0.009) that is now suspect given that pre-trend flag.

### Finding 2 — nearly every item shows a possible pre-trend, not just coat

**`table_A3_pretrend_wald.csv` flags almost every MDCH item — bedroom, celebrations, school equipment, holiday, indoor play, outdoor play, fruit/veg, vegetables — plus all three composite measures and the official MDCH flag itself as "possible pre-trend". Only food insecurity and trips/outings come back clean.** This is a bigger identification threat than the coat-specific story alone and needs resolving before the baseline DiD can be considered credible — it comes before the Wales/NI extension and the stacked-DiD design in priority, since both are built on the same underlying identification strategy.

### Finding 3 — FYE2024 methodology transition

DWP changed the official MDCH methodology from FYE2024 (old: 21 items, prevalence-weighted score, threshold ≥25; new: 11+11 items, simple count, threshold ≥4). In the FYE2024 transition year, ~76% of GB households got the new questions and ~24% got the old ones (bridged via imputation) — except Northern Ireland, which got 0% old questions (100% new), purely for NISRA sample-size reasons. Scotland (27% old) and England (24–30% old by region) are close enough that this doesn't look like a Scotland-vs-England asymmetry, but it is a real GB-vs-NI asymmetry that matters once NI is added as a validity check: NI's FYE2024 old-definition values would be entirely imputation-derived.

---

## Phase 1 — Jul 8–13: Diagnose pre-trends, then lock in the feedback

**Do this in order.** The pre-trend problem needs understanding — ideally resolving — before the Wales/NI extension and the stacked-DiD design are worth finalizing, since both sit on top of the same identification strategy. Your supervisor becomes less available after mid-July, so get a check-in booked this week regardless.

1. **Diagnose the pre-trend problem first.** `table_A3_pretrend_wald.csv` flags nearly every item, all three composite measures, and the official MDCH flag as "possible pre-trend." Check: (a) does covariate adjustment (the existing "adjusted" column) resolve the flags or do they persist; (b) is this driven by genuine compositional divergence already visible in Table 2 (family size, ethnicity differ by country), or by the Wald test's sensitivity to sample size/clustering; (c) if pre-trends survive covariate adjustment, does the baseline DiD need a different identification strategy — e.g. explicitly modelling differential linear pre-trends, restricting the pre-period, or leaning more on the stacked/DML approaches to absorb it.
2. Audit the composite/MDCH flag construction: document the regional-necessity weighting limitation (Finding 1) as a stated limitation in Methodology/Discussion, and confirm which variable the pipeline reads for FYE2024 rows (item battery vs official MDCH/MDCH_OLD2324), checking `MDIMP` for how much is real response vs imputation (Finding 3).
3. Relax the geography filter to `COUNTRY %in% c(1,2,3,4)` in `01_hbai_prep.R`; check Wales/NI sample sizes are large enough to support a placebo test.
4. Supervisor check-in this week — this is now the most important meeting in the whole plan. Cover: the pre-trend diagnosis and whether it changes the baseline identification strategy, the regional-necessity weighting limitation, and the Wales/NI role (pure falsification check vs. extra control group).
5. Design the stacked item-level DiD: long-format panel (one row per person-item), item fixed effects, item × treatment × post interactions, SEs clustered by household/item, with per-item pre-trend diagnostics reported transparently rather than hidden. This replaces the 10-separate-regressions approach.

**Checkpoint:** pre-trend diagnosis complete, and supervisor meeting held, before Phase 2 starts.

## Phase 2 — Jul 14–20: Refine baseline DiD

Scope here depends directly on Phase 1's pre-trend diagnosis — if pre-trends persist after covariate adjustment, this phase needs to implement whatever alternative identification approach was agreed with your supervisor, not just re-run the existing spec on more data.

- Re-run `03_did_replication.R` on the corrected/extended sample, incorporating the Phase 1 pre-trend decision.
- Add a Wales/NI placebo DiD (new script, e.g. `03b_placebo_validity.R`).
- Extend `05_parallel_trends.R` diagnostics to include Wales/NI trends.
- Finalize baseline regression table + event study figure.

## Phase 3 — Jul 21–27: Double ML

- Finalize `04_dml_did.R` on the corrected sample/composite.
- Sensitivity checks: RF vs Lasso learner, propensity trimming, overlap diagnostics.
- Compare DML estimates to baseline OLS — this comparison is a core result.
- If time allows, extend the Wales/NI check into the DML framework; otherwise flag as a limitation/future work.

## Phase 4 — Jul 28–Aug 3: Item-decomposition DiD

- Implement the stacked DiD across the 10 MDCH indicators.
- Apply BH-FDR correction consistently (already used in `04_dml_did.R`).
- Cross-check stacked estimates against per-item DML results.
- Finalize item-decomposition tables/figures.

## Phase 5 — Aug 3–9: Writing I

No separate buffer week — any empirical slippage from Phases 2–4 needs absorbing here, so start with the sections least dependent on final numbers.

- Introduction: research question, motivation, contribution relative to Stewart et al. (CASE 2025).
- Data: FRS/HBAI, sample construction, composite-consistency note, summary stats.
- Methodology: baseline DiD, DML, stacked item DiD, Wales/NI validity check.

## Phase 6 — Aug 10–19: Writing II

**Target: a complete first draft of every section — including Abstract — finished by 19 Aug.** You're away 20–24 Aug, so nothing about the draft can depend on that week.

- Results: baseline, DML, item-decomposition, validity checks.
- Discussion: interpretation, fit with literature, limitations.
- Conclusion.
- Abstract (≤300 words) — write last, once Results/Discussion are stable.

## Away — Aug 20–24

No work planned. This is exactly why Phase 6 targets a complete draft by the 19th — you come back to editing an existing document, not writing one under a shortened deadline.

## Phase 7 — Aug 25–31: Integration and submission

- Full read-through; cut to 7,000-word limit. Push detailed robustness/full regression tables into the Data Appendix (appears not to count against the 7,000-word main body limit per the course guidance).
- Assemble Data Appendix: data sources, variable definitions, full tables.
- Harvard referencing pass.
- Assemble mandatory replication package: all code, data instructions, `README.txt` (which file produces what) — non-negotiable per course rules.
- Formatting: title page, contents, list of figures/tables, declaration, copyright statement.
- Proofread. Submit 31 Aug.

---

## Key risks

- **The pre-trend problem is the biggest open risk in the whole plan.** Nearly every item and the composite itself show a possible violation — if this doesn't resolve with covariate adjustment, the core DiD identification strategy needs revisiting, which could eat into time budgeted for Phases 2–4.
- **Word budget is tight.** Three empirical strands (baseline, DML, item-decomposition) plus a validity check inside 7,000 words means the main text needs to stay lean — lead with headline tables/figures, move detail to the appendix.
- **Supervisor availability drops after mid-July** — get the Wales/NI and stacked-DiD design confirmed in Phase 1, not later.
- **DoubleML runtime/instability** — with the dedicated buffer week removed to make room for the 20–24 Aug trip, this needs absorbing during Phase 5 instead; don't let empirical work bleed into Phase 6.
- **The 20–24 Aug trip removes the old buffer week entirely** — if the draft isn't complete by 19 Aug, Phase 7 (25–31 Aug) has to cover both finishing the writing and all the integration/appendix/replication-package work, which is a real risk given the 1 Sept deadline.
