# Presentation Week Plan — Monday 6 July 2026
**Weight:** 20% of ECON65000 | **Length:** max 20 min, max ~16 slides
**Key insight:** Completed empirical work is NOT expected. Focus is research, lit review, strategy, data, and delivery.

---

## Rubric priorities (ranked by marks at stake)

| Criterion | What "Very Good" requires |
|-----------|--------------------------|
| Research Question | Concise articulation + economic AND econometric motivation |
| Application of Econometrics | Methods well-justified AND well-explained |
| Data | Summary stats + visualisation — audience grasps data characteristics |
| Clarity & Organisation | Logical flow throughout |
| Slides / Visual Aids | Low text density — slides support speech, not replace it |
| Presentation Skills | Eye contact, confidence, enthusiasm |
| Responses to Questions | Shows deep grasp of material |

---

## Priority 1 — Content gaps to close

### 1. Strengthen the literature review (slide 4)
Your existing slide cites Stewart et al. (CASE 2025) and two deprivation measurement papers. For "Very Good" on Research Question, the econometric motivation needs to be explicit: *why is linear DiD insufficient and why does DML matter here?* Add 1–2 sentences on this directly in your spoken content (doesn't need to be on the slide).

Check `literature_summary.md` — make sure every claim on the slide is backed up by a paper you can speak to if questioned.

### 2. Generate summary statistics and produce a data visualisation slide
This is the biggest gap for the **Data** criterion. Run `02_hbai_summary_stats.py` to get:
- Pre/post treatment group means for key outcomes (mdch_any, mdch_severe, food insecurity)
- A simple parallel trends plot for the pre-period

Add a summary stats table or figure to the slides — this is explicitly what markers are looking for ("summary statistics as well as visualisation").

### 3. Run benchmark DiD (optional but useful)
Not required, but even preliminary estimates give you something concrete to discuss and make the "Next Steps" slide more credible. If the data prep runs cleanly today, worth doing.

---

## Priority 2 — Slide polish (directly hits rubric)

### 4. Cut text on high-density slides
Slides 9 and 10 (DiD spec, DML explanation) are text-heavy. The rubric specifically penalises slides where "presentation mainly consists of reading off the slides." Aim: each slide should have a headline and 3–4 bullet fragments max — the explanation lives in your speech.

### 5. Audit slide count and flow
You currently have 14 slides. At 20 minutes that's ~85 seconds per slide — fine, but check the flow:
- Slides 11–13 (Initial Results / Anticipated Issues / Next Steps) should be consolidated or reframed as "Preliminary Findings & Planned Extensions"
- Slide 14 (Summary) — ensure it answers the research question directly, even if tentatively

### 6. Add visuals where missing
The parallel trends figures are already generated (`hbai_fig1_parallel_trends_mdch_any.png` etc.). Insert them into the slides — they directly score marks under the Data criterion.

---

## Priority 3 — Delivery

### 7. Prepare spoken narrative (not speaker notes to read)
For each slide, write one sentence max summarising what you'll say — a prompt, not a script. Practice until you can speak to each slide without looking at it.

### 8. Rehearse twice with a timer
- First run: get the timing right (target ≤18 mins)
- Second run: focus on eye contact and not reading the slides

### 9. Prepare Q&A answers
The likely hard questions:
- Why DML over linear DiD? (Slide 10 — know this cold)
- Why focus on 2022 expansion not 2021? (Staggered rollout — you need a crisp answer)
- Missing ethnicity covariate — how does this affect your identification? 
- What do you expect to find / what would falsify your hypothesis?
- How does your contribution differ from Stewart et al. beyond the method?

---

## Suggested day-by-day

| Day | Focus |
|-----|-------|
| Mon 29 Jun (today) | Task 1 (lit review check) + Task 2 (summary stats & figures) |
| Tue 30 Jun | Task 3 (benchmark DiD if data is ready) + Task 4 (cut slide text) |
| Wed 1 Jul | Tasks 5–6 (audit flow, insert figures) |
| Thu 2 Jul | Task 7 (spoken narrative for each slide) |
| Fri 3 Jul | Task 8 (first full rehearse, timed) |
| Sat 4 Jul | Task 9 (Q&A prep) + second rehearsal |
| Sun 5 Jul | Rest / light review only |
| Mon 6 Jul | **Presentation** |

---

## Key risk
Your slides currently have a "Next Steps" slide and "Initial Results" as a placeholder. That's fine — but reframe confidently: you have preliminary evidence consistent with the literature and a clear path to full estimation. Markers expect this at this stage.
