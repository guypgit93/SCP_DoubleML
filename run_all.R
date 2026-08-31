# ─────────────────────────────────────────────────────────────────────────────
# run_all.R
# Runs the full pipeline (01-12) in one R session, in the order README.md
# documents. Uses source(), not separate Rscript processes for each script,
# because 05b_stage1_pretrend_diagnostics.R depends on objects (df_a2,
# df_mdch, run_trend_model, ...) left in memory by 05_stage1_parallel_trends.R
# rather than reading them from a file. That only works if 05 and 05b run
# back-to-back in the same session, which sequential source() calls preserve;
# do not try to parallelise this script or run stages as separate processes
# without handling that dependency some other way.
#
# 12_dml_stability_check.R dominates total runtime (B=20 reps at roughly
# 20-25 min each, so several hours); everything else combined is more like
# an hour. Designed to be started once and left overnight.
#
# To run unattended, from Terminal at the repo root:
#   nohup caffeinate -i Rscript "run_all.R" > run_all_log.txt 2>&1 &
#   tail -f run_all_log.txt
# `caffeinate -i` stops macOS sleeping while it runs (does not cover
# lid-close sleep; leave the lid open and the machine plugged in). `nohup`
# keeps it alive if the terminal window closes.
#
# Each script's errors are caught here rather than stopping the whole run,
# so one failure doesn't cost the rest of the night. A per-script status
# table prints at the end and is saved to run_all_summary.csv; check that
# (or just search the log for "FAILED") to see what needs a rerun.
# ─────────────────────────────────────────────────────────────────────────────

library(here)

# Dot-prefixed names throughout this script deliberately: ls() and
# rm(list = ls()) both skip them by default, so nothing here can collide
# with or be wiped out by a variable any of the sourced scripts defines.
.pipeline <- c(
  "Scripts/01_hbai_prep.R",
  "Scripts/02_summary_stats.R",
  "Scripts/03_stage1_baseline_did.R",
  "Scripts/04_stage2_item_did.R",
  "Scripts/05_stage1_parallel_trends.R",
  "Scripts/05b_stage1_pretrend_diagnostics.R",     # must directly follow 05, same session
  "Scripts/05c_stage1_parallel_trends_quarterly.R",
  "Scripts/06_stage3_dml_lean.R",
  "Scripts/06b_stage3_dml_wide.R",
  "Scripts/07_stage4_dml_item.R",
  "Scripts/08_stage5_stacked_ml_did.R",
  "Scripts/09_placebo_wales_ni.R",
  "Scripts/11_wild_cluster_bootstrap.R",
  "Scripts/12_dml_stability_check.R",              # slowest by far
  "Scripts/10_make_latex_tables.R"                 # last: reformats everything above
)

.log <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), sprintf(...)))

.summary <- data.frame(script = .pipeline, status = NA_character_,
                        minutes = NA_real_, stringsAsFactors = FALSE)

for (.i in seq_along(.pipeline)) {
  .script <- .pipeline[.i]
  .log("== START %s ==", .script)
  .t0 <- Sys.time()

  .ok <- tryCatch({
    source(here(.script), echo = TRUE, print.eval = TRUE)
    TRUE
  }, error = function(e) {
    .log("  FAILED: %s", conditionMessage(e))
    FALSE
  })

  .elapsed <- round(as.numeric(Sys.time() - .t0, units = "mins"), 1)
  .summary$status[.i]  <- if (.ok) "ok" else "FAILED"
  .summary$minutes[.i] <- .elapsed
  .log("== END %s (%s, %.1f min) ==\n", .script, .summary$status[.i], .elapsed)
}

.log("PIPELINE COMPLETE")
print(.summary)
write.csv(.summary, here("run_all_summary.csv"), row.names = FALSE)
