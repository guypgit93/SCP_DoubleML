# Literature Summary
## Scottish Child Payment & Hardship Measurement — MSc Dissertation
*For supervisor meeting, 22 June 2026*

---

## 1. The Scottish Child Payment: Policy Background

The Scottish Child Payment (SCP) was introduced in February 2021, initially for children under six in families receiving Universal Credit or legacy means-tested benefits. It was the first new social security payment devolved to Scotland under the Scotland Act 2016. From November 2022, it was doubled to £25/week per child and extended to all children under 16 — a major expansion. This staggered rollout creates the variation exploited in our DiD design.

**Key policy documents:**
- Scottish Government (2023). *Scottish Child Payment: Overview*. Explains eligibility, payment amounts, and phased rollout. Useful for constructing eligibility proxies.
- Bate, A. & McInnes, R. (2023). *Scottish Child Payment*, House of Commons Library Briefing CBP-9968. Good summary of eligibility and rollout timeline.

**Early evaluations:**
- Fitzpatrick, S. et al. (2023). *Destitution in the UK 2023*. Joseph Rowntree Foundation. Uses Trussell Trust and voluntary sector data to track hardship trends in Scotland vs. rest of UK — provides important contextual evidence but not quasi-experimental.
- Scottish Government / Social Security Scotland (2023). *Benefit Take-Up: Scottish Child Payment*. Administrative data on uptake; relevant for discussing compliance and selection into treatment.

---

## 2. Causal Identification: Difference-in-Differences for Social Policy

The central methodological framework is DiD with repeated cross-sections, comparing Scotland (treated) to England (control) before and after SCP rollout.

**Canonical DiD references:**
- Angrist, J. & Pischke, J.S. (2009). *Mostly Harmless Econometrics*. Ch. 5 covers DiD. Standard textbook treatment — parallel trends assumption, common threats, and placebo tests.
- Card, D. & Krueger, A.B. (1994). "Minimum Wages and Employment." *AER* 84(4). Canonical example using repeated cross-sections across states — directly analogous to our Scotland/England setup.

**Staggered treatment:**
Because SCP was first introduced in Feb 2021 and expanded in Nov 2022, there are two treatment waves. This matters for estimation:
- Callaway, B. & Sant'Anna, P.H.C. (2021). "Difference-in-Differences with Multiple Time Periods." *Journal of Econometrics* 225(2): 200–230. Provides ATT(g,t) estimator robust to treatment effect heterogeneity across cohorts — recommended over the standard TWFE estimator when there is staggered rollout.
- Goodman-Bacon, A. (2021). "Difference-in-Differences with Variation in Treatment Timing." *Journal of Econometrics* 225(2): 254–277. Shows that TWFE decomposes into a weighted average of all 2×2 DiDs, and the weights can be negative when treatment effects vary — another argument for Callaway-Sant'Anna.
- Baker, A.C., Larcker, D.F. & Wang, C.C.Y. (2022). "How Much Should We Trust Staggered Difference-in-Differences Estimates?" *Journal of Financial Economics* 144(2). Good practical guidance on which estimator to use.

**Parallel trends:**
The key identifying assumption is that, absent SCP, Scotland and England would have had parallel hardship trends. This is testable in the pre-treatment years (2018–2021) and is the main thing to show your supervisor:
- Pre-treatment trend plots for each outcome are the first deliverable.
- Concerns: Scotland and England differ on housing tenure, industrial composition, prior welfare generosity (e.g. Best Start Foods). Conditional parallel trends (conditioning on observable covariates) may be more credible.

---

## 3. Hardship Measurement: The Core Contribution

This is where the dissertation makes its original contribution. The choice of hardship measure is not neutral — different indicators capture different dimensions of deprivation, have different measurement error properties, and may yield different estimated treatment effects.

### 3a. Conceptual frameworks

**Poverty vs. hardship vs. deprivation:**
- Sen, A. (1985). *Commodities and Capabilities*. Foundational argument that poverty is about capability deprivation, not just income — motivates using non-income hardship measures.
- Townsend, P. (1979). *Poverty in the United Kingdom*. Seminal work on relative deprivation; the origin of material deprivation indices.
- Nolan, B. & Whelan, C.T. (1996). *Resources, Deprivation and Poverty*. Dublin: ESRI. Demonstrates empirically that income and material deprivation measures identify different populations — a key precursor to the dissertation question.

**Multi-dimensional poverty:**
- Alkire, S. & Foster, J. (2011). "Counting and Multidimensional Poverty Measurement." *Journal of Public Economics* 95(7–8): 476–487. Provides a framework for combining multiple binary deprivation indicators into composite indices — relevant if we construct a composite hardship measure.

### 3b. Measurement error in hardship indicators

This is the supervisor's area and the dissertation's theoretical anchor.

**Classical measurement error (CML):**
- Bound, J., Brown, C. & Mathiowetz, N. (2001). "Measurement Error in Survey Data." *Handbook of Econometrics* Vol. 5, Ch. 59. Definitive survey. For binary outcomes, classical ME in the *dependent* variable attenuates estimated treatment effects toward zero — the attenuation bias result. This is directly relevant: if our hardship measure contains random error, DiD estimates are biased toward zero.

**Non-classical measurement error:**
- Bound, J. & Krueger, A.B. (1991). "The Extent of Measurement Error in Longitudinal Earnings Data." *Journal of Labor Economics* 9(1). Shows that measurement error in survey responses is often non-classical (correlated with true values). For hardship indicators, response bias may differ between Scotland and England (e.g. if the SCP creates awareness effects that change how people report hardship) — this would violate classical ME assumptions and create differential bias.
- Meyer, B.D., Mok, W.K.C. & Sullivan, J.X. (2015). "Household Surveys in Crisis." *Journal of Economic Perspectives* 29(4): 199–226. Documents systematic underreporting in US benefit surveys. The FRS may have similar issues — people on UC may underreport receipt.

**Differential measurement error as a threat to DiD:**
The key insight for the dissertation: if ME in hardship outcomes differs between Scotland and England (e.g. because SCP recipients *know* they are receiving a payment and this changes their self-assessment of financial wellbeing), then the DiD estimator captures not just the true treatment effect but also the change in measurement error. The choice of outcome matters for whether this bias is severe.

- Kreider, B. et al. (2012). "Identifying the Effects of Food Stamps on Food Insecurity and Obesity." *Review of Economics and Statistics* 94(1). Uses partial identification methods to bound treatment effects under non-classical ME — a methodological reference for robustness checks.

### 3c. Food insecurity measurement

The FRS includes the USDA 30-day food security module from 2019-20 onwards.

- USDA Economic Research Service (2023). *U.S. Household Food Security Survey Module: Three-Stage Design*. The standard reference for the 18-item module — defines thresholds for low and very low food security.
- Loopstra, R. & Tarasuk, V. (2013). "Severity of Household Food Insecurity Is Sensitive to Change in Household Income." *Journal of Nutrition* 143(8). Food insecurity responds rapidly to income shocks — makes it a good outcome for evaluating income transfers like SCP.
- Gundersen, C. & Ziliak, J.P. (2015). "Food Insecurity and Health Outcomes." *Health Affairs* 34(11). Links food insecurity to broader health and wellbeing — contextualises why it matters.

**Limitation:** The food security module is only in the FRS from 2019-20, giving two pre-treatment years (2020, 2021) before the SCP intro and fewer post-treatment observations. This constrains the food insecurity analysis relative to other outcomes.

### 3d. Material deprivation measurement

The FRS material deprivation module (OA* items in benunit.tab) covers 14 adult items.

- Townsend (1979) — as above, the origin.
- Eurostat (2020). *Material and Social Deprivation — Statistics Explained*. The EU-SILC standard for measuring material deprivation — the FRS items are broadly consistent with this approach.
- Main, G. & Bradshaw, J. (2016). "Child Poverty and Social Exclusion." *Social Policy & Administration* 50(7). Uses composite child deprivation indices in the UK — relevant comparator for our child deprivation items (CDEP*).
- Gordon, D. et al. (2000). *Poverty and Social Exclusion in Britain*. PSE Survey. Foundational UK-specific material deprivation measurement — establishes item selection methodology.

**Key measurement issue:** Material deprivation items are binary (have/don't have) but the threshold for what counts as "deprived" is arbitrary — is 1 item sufficient, or 3? The choice affects both prevalence estimates and sensitivity to an income transfer of £25/week.

### 3e. Financial hardship measurement

The primary measure in this dissertation is ADBTBL ("keeping up with bills"), which maps to a binary hardship indicator.

- Kempson, E. (1996). *Life on a Low Income*. York: Joseph Rowntree Foundation. Documents the use of "keeping up with bills" as a hardship indicator in UK survey research.
- Mack, J. & Lansley, S. (1985). *Poor Britain*. Pioneering use of subjective hardship assessment in UK survey data — ancestor of the ADBTBL-type question.

**Subjective vs. objective measures:** ADBTBL is subjective (self-assessed ability to keep up), whereas material deprivation items are more objective (does the household have X). The distinction matters for measurement error: subjective items are more susceptible to framing effects and response scale interpretation differences across groups.

---

## 4. Income Transfers and Hardship: Evidence on Mechanisms

**Cash transfers — general:**
- Haushofer, J. & Shapiro, J. (2016). "The Short-Term Impact of Unconditional Cash Transfers." *Quarterly Journal of Economics* 131(4). RCT evidence on cash transfers in Kenya — food consumption, assets, and psychological wellbeing all improve. Cited for external validity context.
- Forget, E.L. (2011). "The Town with No Poverty." *Canadian Journal of Economics* 44(2). MINCOME guaranteed income experiment — shows reductions in hospitalisation and mental health contacts from income floor. Analogous to universal-type payments.

**Child poverty and income transfers in the UK:**
- Gregg, P., Waldfogel, J. & Washbrook, E. (2006). "Family Expenditures Post-Welfare Reform in the UK." *Journal of Population Economics* 19(4). Documents how low-income families shift spending in response to benefit increases — mechanism for hardship reduction.
- Brewer, M., Goodman, A. & Leicester, A. (2006). *Household Spending in Britain*. IFS Report. Baseline on how low-income UK families allocate income — relevant for understanding whether £25/week is large enough to affect hardship measures.

**SCP-specific / Scottish context:**
- Scottish Government (2022). *Child Poverty Delivery Plan: Best Start, Bright Futures*. Sets out the role of SCP in Scotland's child poverty reduction strategy — official context.
- Poverty Alliance / Child Poverty Action Group (various). Annual reports on child poverty in Scotland provide non-experimental trend evidence to triangulate with FRS estimates.

---

## 5. Key Gaps This Dissertation Addresses

1. **No quasi-experimental evaluation of SCP on material hardship.** Existing SCP evaluations are administrative (uptake) or descriptive (CPAG annual surveys). No published DiD estimate exists.

2. **No study examines how hardship measure choice affects estimated policy impacts.** The measurement error literature is almost entirely focused on income or earnings. The question of whether different hardship outcomes yield systematically different DiD estimates — and why — is novel.

3. **FRS food insecurity module underused.** The 30-day USDA module in the FRS has been used in only a handful of UK academic papers; applying it to SCP evaluation is original.

---

## 6. Suggested Reading Order (Priority for Meeting)

For the supervisor meeting, the most important to have read or skimmed:

1. Callaway & Sant'Anna (2021) — the estimator you will use
2. Bound, Brown & Mathiowetz (2001) — ME framework for the key theoretical contribution
3. Nolan & Whelan (1996) — empirical case that income and deprivation measures diverge
4. Meyer, Mok & Sullivan (2015) — non-classical ME in survey data
5. Scottish Government SCP overview + Bate & McInnes HoC briefing — policy context

---

*Word count: ~1,400. Expand individual sections for the final dissertation literature review chapter.*
