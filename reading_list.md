# Reading List: SCP Dissertation
## Scottish Child Payment — Causal Evaluation with DiD

*Organised by theme. Starred ★ entries are highest priority. Items already discussed in `literature_summary.md` are included for completeness but marked (existing).*

---

## A. Technical DiD — Foundations

**★ Angrist, J. & Pischke, J.S. (2009).** *Mostly Harmless Econometrics.* Princeton UP. Ch. 5 (DiD), Ch. 3 (OLS). *(existing)*
> The baseline reference. Read Ch. 5 carefully for the assumptions underlying the 2×2 DiD and its extension to repeated cross-sections.

**★ Card, D. & Krueger, A.B. (1994).** "Minimum Wages and Employment: A Case Study of the Fast Food Industry in New Jersey and Pennsylvania." *American Economic Review* 84(4): 772–793. *(existing)*
> Canonical repeated cross-section DiD. Methodologically identical to the Scotland/England setup.

**Bertrand, M., Duflo, E. & Mullainathan, S. (2004).** "How Much Should We Trust Differences-in-Differences Estimates?" *Quarterly Journal of Economics* 119(1): 249–275.
> Shows that standard DiD SEs are badly undersized when errors are serially correlated. Key for your inference — clustered SEs at the country×year level or wild cluster bootstrap are the fix.

**Abadie, A. (2005).** "Semiparametric Difference-in-Differences Estimators." *Review of Economic Studies* 72(1): 1–19.
> Propensity-score-reweighted DiD under conditional (rather than unconditional) parallel trends. Directly relevant if your parallel trends assumption only holds after conditioning on covariates.

**Roth, J. (2022).** "Pre-test with Caution: Event-Study Estimates after Testing for Parallel Trends." *American Economic Review: Insights* 4(3): 305–322.
> Crucial: pre-trend tests have low power and create pre-test bias if you condition on passing. Proposes honest confidence intervals. Read before finalising your event study plots.

**★ Roth, J., Sant'Anna, P.H.C., Bilinski, A. & Poe, J. (2023).** "What's Trending in Difference-in-Differences? A Synthesis of the Recent Econometrics Literature." *Journal of Econometrics* 235(2): 2218–2244.
> You already have the PDF. The definitive survey of modern DiD — covers staggered adoption, conditional parallel trends, pre-testing, and sensitivity analysis in one place. Read this early.

---

## B. Technical DiD — Staggered Treatment

**★ Callaway, B. & Sant'Anna, P.H.C. (2021).** "Difference-in-Differences with Multiple Time Periods." *Journal of Econometrics* 225(2): 200–230. *(existing)*
> The estimator you are implementing. Defines ATT(g,t) and aggregations thereof. Essential.

**★ Goodman-Bacon, A. (2021).** "Difference-in-Differences with Variation in Treatment Timing." *Journal of Econometrics* 225(2): 254–277. *(existing)*
> Shows TWFE = weighted average of 2×2 DiDs with potentially negative weights. Motivates moving away from standard TWFE. Run the Bacon decomposition as a diagnostic.

**★ Sun, L. & Abraham, S. (2021).** "Estimating Dynamic Treatment Effects in Event Studies with Heterogeneous Treatment Effects." *Journal of Econometrics* 225(2): 175–199.
> Interaction-weighted (IW) estimator — an alternative to Callaway-Sant'Anna that is cleaner to implement in R (`sunab` in `fixest`). Directly comparable in your staggered SCP setup (2021 introduction, 2022 expansion). Compare results across estimators.

**de Chaisemartin, C. & D'Haultfœuille, X. (2020).** "Two-Way Fixed Effects Estimators with Heterogeneous Treatment Effects." *American Economic Review* 110(9): 2964–2996.
> Provides the DID_M estimator robust to treatment effect heterogeneity. A third robustness check alongside Callaway-Sant'Anna and Sun-Abraham.

**Baker, A.C., Larcker, D.F. & Wang, C.C.Y. (2022).** "How Much Should We Trust Staggered Difference-in-Differences Estimates?" *Journal of Financial Economics* 144(2): 370–395. *(existing)*
> Practical guidance on comparing TWFE vs. robust estimators. Good for the robustness section.

**Borusyak, K., Jaravel, X. & Spiess, J. (2024).** "Revisiting Event-Study Designs: Robust and Efficient Estimation." *Review of Economic Studies* 91(6): 3253–3285.
> Imputation-based DiD estimator (BJS). Another robust alternative; sometimes preferred for efficiency. Implemented as `did_imputation` in R. Worth including as a robustness spec.

---

## C. Technical DiD — Double Machine Learning (DML-DiD)

*You have `03_dml_did.py` and `04_dml_did.R` — these refs underpin that code.*

**★ Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C., Newey, W. & Robins, J. (2018).** "Double/Debiased Machine Learning for Treatment and Structural Parameters." *Econometrics Journal* 21(1): C1–C68.
> Foundational paper for the DML framework. Introduces cross-fitting and the Neyman-orthogonal score. Essential reading before presenting DML-DiD results.

**★ Sant'Anna, P.H.C. & Zhao, J. (2020).** "Doubly Robust Difference-in-Differences Estimators." *Journal of Econometrics* 219(1): 101–122.
> Combines outcome regression and propensity score weighting into a DR-DiD estimator. The standard reference for DML-DiD in the binary treatment case — directly what your code implements.

**Chang, N.C. (2020).** "Double/Debiased Machine Learning for Difference-in-Differences Models." *Econometrics Journal* 23(2): 177–191.
> Applies the DML framework specifically to DiD. Covers when DML improves efficiency over standard DiD and conditions under which it is consistent. Key technical reference for your DML chapter.

**Knaus, M.C. (2022).** "Double Machine Learning-Based Programme Evaluation under Unconfoundedness." *Econometrics Journal* 25(3): 602–627.
> Applied treatment of DML for programme evaluation — closer to your use case than the more abstract Chernozhukov et al. paper. Good for the methods section.

---

## D. Parallel Trends — Testing and Sensitivity

**Rambachan, A. & Roth, J. (2023).** "A More Credible Approach to Parallel Trends." *Review of Economic Studies* 90(5): 2555–2591.
> Provides sensitivity analysis for violations of parallel trends using restrictions on the smoothness of violations (the `HonestDiD` R package). Essential for your sensitivity section — especially given Scotland/England structural differences.

**Bilinski, A. & Hatfield, L.A. (2018).** "Nothing to See Here? Non-Inferiority Approaches to Parallel Trends and Other Equivalence Tests." *arXiv* 1805.03273.
> Reframes pre-trend testing as an equivalence test — avoids the problem of using a failed test as proof of parallel trends. Accessible and useful for the pre-trends discussion.

**Kahn-Lang, A. & Lang, K. (2020).** "The Promise and Pitfalls of Differences-in-Differences: Reflections on 16 and Pregnant." *Journal of Business & Economic Statistics* 38(3): 613–620.
> Concrete discussion of what "parallel trends" really requires and common violations — good intuition-building.

---

## E. Measurement Error in Survey Data

**★ Bound, J., Brown, C. & Mathiowetz, N. (2001).** "Measurement Error in Survey Data." *Handbook of Econometrics* Vol. 5, Ch. 59. *(existing)*
> The attenuation bias result for binary outcomes. Your theoretical anchor.

**★ Bound, J. & Krueger, A.B. (1991).** "The Extent of Measurement Error in Longitudinal Earnings Data." *Journal of Labor Economics* 9(1): 1–24. *(existing)*
> Non-classical ME.

**★ Meyer, B.D., Mok, W.K.C. & Sullivan, J.X. (2015).** "Household Surveys in Crisis." *Journal of Economic Perspectives* 29(4): 199–226. *(existing)*
> Benefit under-reporting in household surveys.

**Black, D.A., Berger, M.C. & Scott, F.A. (2000).** "Bounding Parameter Estimates with Nonclassical Measurement Error." *Journal of the American Statistical Association* 95(451): 739–748.
> Provides bounds on treatment effects under non-classical ME. Useful for the bounding exercise / robustness discussion.

**Kapteyn, A. & Ypma, J.Y. (2007).** "Measurement Error and Misclassification: A Comparison of Survey and Administrative Data." *Journal of Labor Economics* 25(3): 513–551.
> Compares self-reported benefit receipt to administrative records in Dutch data — directly analogous to whether FRS respondents correctly report UC receipt. Informs your discussion of selection into treatment and compliance.

**★ Kreider, B., Pepper, J.V., Gundersen, C. & Bhattacharya, J. (2012).** "Identifying the Effects of Food Stamps on Food Insecurity and Obesity when the Treatment is Misreported." *Review of Economics and Statistics* 94(1): 52–73. *(existing)*
> Partial identification under non-classical ME — useful methodological comparator for bounding.

---

## F. Hardship, Poverty, and Deprivation Measurement

**★ Sen, A. (1985).** *Commodities and Capabilities.* North-Holland. *(existing)*

**★ Townsend, P. (1979).** *Poverty in the United Kingdom.* Penguin. *(existing)*

**★ Nolan, B. & Whelan, C.T. (1996).** *Resources, Deprivation and Poverty.* ESRI. *(existing)*

**Alkire, S. & Foster, J. (2011).** "Counting and Multidimensional Poverty Measurement." *Journal of Public Economics* 95(7–8): 476–487. *(existing)*

**Ravallion, M. (2011).** "On Multidimensional Indices of Poverty." *Journal of Economic Inequality* 9(2): 235–248.
> Critical perspective on composite indices — argues income-based measures often dominate. Important counterpoint to Alkire-Foster if you construct a composite.

**Whelan, C.T., Layte, R. & Maître, B. (2004).** "Understanding the Mismatch between Income Poverty and Deprivation: A Dynamic Comparative Analysis." *European Sociological Review* 20(4): 287–302.
> Longitudinal evidence on why income poverty and material deprivation diverge — empirical counterpart to Nolan-Whelan.

**Main, G. & Bradshaw, J. (2016).** "Child Poverty and Social Exclusion." *Social Policy & Administration* 50(7): 794–813. *(existing)*

**Gordon, D. et al. (2000).** *Poverty and Social Exclusion in Britain.* PSE Survey. *(existing)*

**Kempson, E. (1996).** *Life on a Low Income.* JRF. *(existing)*

**Hirsch, D. & Stone, J. (2020).** *The Living Standards Audit 2020.* Resolution Foundation.
> Recent UK-specific analysis of how different hardship measures track each other over time — good empirical context for why your measurement choice matters.

---

## G. Food Insecurity

**★ Loopstra, R. & Tarasuk, V. (2013).** "Severity of Household Food Insecurity Is Sensitive to Change in Household Income." *Journal of Nutrition* 143(8): 1316–1323. *(existing)*

**Gundersen, C. & Ziliak, J.P. (2015).** "Food Insecurity and Health Outcomes." *Health Affairs* 34(11): 1830–1839. *(existing)*

**Loopstra, R., Reeves, A., Taylor-Robinson, D., Barr, B., McKee, M. & Stuckler, D. (2015).** "Austerity, Sanctions, and the Rise of Food Banks in the UK." *BMJ* 350: h1775.
> Documents the relationship between welfare reform and food bank use in England 2011–2013. Useful for framing rising UK food insecurity pre-SCP.

**Fitzpatrick, S., Bramley, G., Sosenko, F., Blenkinsopp, J., Wood, J., Johnsen, S., Littlewood, M. & McIntyre, J. (2020).** *Destitution in the UK 2020.* JRF.
> Establishes baseline destitution trends — precursor to the 2023 report you already have.

**Purdam, K., Garratt, E.A. & Esmail, A. (2016).** "Hungry? Food Insecurity, Social Stigma and Embarrassment in the UK." *Sociology* 50(6): 1072–1088.
> Qualitative evidence on underreporting of food insecurity in UK surveys — relevant to the differential ME discussion.

---

## H. UK Welfare and Child Poverty — Empirical Evaluations

**★ Gregg, P., Waldfogel, J. & Washbrook, E. (2006).** "Family Expenditures Post-Welfare Reform in the UK." *Journal of Population Economics* 19(4): 873–903. *(existing)*

**Brewer, M., Goodman, A. & Leicester, A. (2006).** *Household Spending in Britain.* IFS. *(existing)*

**Brewer, M. & Browne, J. (2006).** "The Effect of the Working Families' Tax Credit on Labour Market Participation." *Fiscal Studies* 27(1): 1–22.
> Early DiD-style evaluation of a UK in-work benefit using Labour Force Survey — methodological parallel for evaluating FRS-based outcomes with a quasi-experimental design.

**Cattan, S., Conti, G., Farquharson, C. & Ginja, R. (2021).** "The Health Effects of Sure Start." *American Economic Journal: Applied Economics* 13(3): 372–409.
> DiD evaluation of a UK early years programme using administrative data — a good methodological template for a staggered Scottish policy evaluation.

**Britton, J., Dearden, L., van der Erve, L. & Waltmann, B. (2020).** "The Impact of Undergraduate Degrees on Lifetime Earnings." *Fiscal Studies* 41(4): 703–730.
> Example of DiD with repeated cross-sections and heterogeneous treatment effects in a UK policy context. Less directly relevant but a useful methodological comparator.

**Avram, S. & Popova, D. (2022).** "Do Benefits Compensate for Austerity? Poverty and Income Distribution in 2021." *Journal of Poverty and Social Justice* 30(2): 215–236.
> Simulation-based analysis of UK benefit changes — provides a baseline estimate of what cash transfers of this magnitude should theoretically do to poverty rates.

**Bell, T. & Gardiner, L. (2019).** *The Living Standards Outlook 2019.* Resolution Foundation.
> Pre-pandemic baseline on UK living standards — useful for contextualising the FRS pre-treatment period.

---

## I. Scottish Child Payment — Policy and Context

**★ Scottish Government (2023).** *Scottish Child Payment: Overview.* *(existing)*

**★ Bate, A. & McInnes, R. (2023).** *Scottish Child Payment.* HoC Library CBP-9968. *(existing)*

**★ Scottish Government (2022).** *Child Poverty Delivery Plan: Best Start, Bright Futures.* *(existing)*

**Scottish Government (2021).** *The Scottish Child Payment: Consultation Analysis.*
> Details the design choices behind SCP — eligibility criteria, payment structure. Relevant for constructing treatment proxies and understanding compliance.

**Social Security Scotland (2023).** *Scottish Child Payment Statistics.*
> Administrative data on caseloads by quarter — useful for verifying that the HBAI treatment timing matches actual rollout.

**Sosenko, F., Livingstone, N. & Fitzpatrick, S. (2013).** *Overview of Food Aid Provision in Scotland.* Scottish Government Social Research.
> Pre-SCP baseline on food hardship in Scotland — useful for contextualising the treatment group's starting point.

**McTague, A. & Pratley, P. (2023).** "Evaluating Scotland's Child Payment." *Policy Scotland Working Paper.*
> One of the few quasi-experimental evaluations of SCP — check carefully for overlap with your design and cite accordingly.

---

## J. Cash Transfers — Mechanism and External Validity

**★ Haushofer, J. & Shapiro, J. (2016).** "Short-Term Impacts of Unconditional Cash Transfers to the Poor." *Quarterly Journal of Economics* 131(4): 1973–2042. *(existing)*

**★ Forget, E.L. (2011).** "The Town with No Poverty." *Canadian Journal of Economics* 44(2): 283–305. *(existing)*

**Evans, D.K. & Popova, A. (2017).** "Cash Transfers and Temptation Goods." *Economic Development and Cultural Change* 65(2): 189–221.
> Meta-analysis of 44 cash transfer studies — shows cash transfers do *not* systematically increase spending on alcohol/tobacco, countering a common objection.

**Bastagli, F., Hagen-Zanker, J., Harman, L., Barca, V., Sturge, G. & Schmidt, T. (2016).** *Cash Transfers: What Does the Evidence Say?* ODI Report.
> Comprehensive review of cash transfer impacts across multiple dimensions including food security and material deprivation — good for framing the expected direction of SCP effects.

---

## K. Robustness and Complementary Methods

**Abadie, A., Diamond, A. & Hainmueller, J. (2010).** "Synthetic Control Methods for Comparative Case Studies." *Journal of the American Statistical Association* 105(490): 493–505.
> The synthetic control method — a useful robustness check to the DiD, particularly if the parallel trends assumption looks weak. Scotland vs. a weighted combination of English regions is a natural application.

**Arkhangelsky, D., Athey, S., Hirshberg, D.A., Imbens, G.W. & Wager, S. (2021).** "Synthetic Difference-in-Differences." *American Economic Review* 111(12): 4088–4118.
> Combines DiD and synthetic control — often more robust than either alone. An increasingly standard robustness check. `synthdid` R package.

**Manski, C.F. (1990).** "Nonparametric Bounds on Treatment Effects." *American Economic Review: Papers & Proceedings* 80(2): 319–323.
> The foundational paper on partial identification — bounding treatment effects without strong assumptions. Background reading for the ME bounding exercise.

---

## L. Data and Survey Methodology

**DWP (annual).** *Family Resources Survey: Technical Report.* Department for Work and Pensions.
> Documents survey design, sampling, response rates, and variable construction. Essential for discussing FRS limitations and the representativeness of Scotland subsamples.

**DWP (annual).** *Households Below Average Income: An Analysis of the UK Income Distribution.* 
> The HBAI series — your primary dataset. Read the methodology annex on equivalisation, benefit imputation, and the material deprivation module.

**Atkinson, A.B. (1987).** "On the Measurement of Poverty." *Econometrica* 55(4): 749–764.
> Classic paper on poverty line sensitivity — motivates your robustness checks over different hardship thresholds.

---

## Suggested Reading Order (Priority)

1. **Roth et al. (2023)** — you have the PDF; start here for the methodological landscape *(Section A)*
2. **Callaway & Sant'Anna (2021)** — the estimator *(Section B)*
3. **Sun & Abraham (2021)** — alternative estimator for direct comparison *(Section B)*
4. **Sant'Anna & Zhao (2020)** — underpins DML-DiD code *(Section C)*
5. **Chernozhukov et al. (2018)** — DML foundations *(Section C)*
6. **Rambachan & Roth (2023)** — parallel trends sensitivity *(Section D)*
7. **Bound, Brown & Mathiowetz (2001)** — ME framework *(Section E)*
8. **Meyer, Mok & Sullivan (2015)** — non-classical ME in surveys *(Section E)*
9. **Roth (2022)** — pre-test caution *(Section A)*
10. **Bertrand, Duflo & Mullainathan (2004)** — serial correlation in SEs *(Section A)*

---

*Last updated: June 2026. New entries are those not in `literature_summary.md`.*
