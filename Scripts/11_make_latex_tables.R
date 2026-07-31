# ─────────────────────────────────────────────────────────────────────────────
# 11_make_latex_tables.R
# Generates publication-ready LaTeX tables (booktabs style) for Stages 1-5 of
# the results pipeline, for direct \input{} into the Overleaf write-up.
#
# Deliberately reads ONLY from the source CSVs confirmed (2026-07-30, by
# grepping each stage script's write.csv() calls) to be current outputs of
# the post-restructure (07-21) pipeline:
#   Stage 1  <- 03_stage1_baseline_did.R  -> table_simple_did.csv,
#                                             table_adj_did.csv,
#                                             table_adj_ext_did.csv,
#                                             table_item_did.csv
#   Stage 2  <- 04_stage2_item_did.R      -> stacked_item_did.csv
#   Stage 3  <- 06_stage3_dml_lean.R,
#               06b_stage3_dml_wide.R     -> dml_did_causalweight_comparison_MDCH.csv,
#                                             dml_did_wide_covariates_MDCH.csv
#   Stage 4  <- 07_stage4_dml_item.R      -> dml_did_item_level_wide.csv
#   Stage 5  <- 09_stage5_stacked_ml_did.R -> stage5_stacked_item_did.csv
#
# Deliberately does NOT read: summary_all_stages_comparison.csv or
# dml_vs_ols_composite.csv (both reference the pre-restructure att_gt/
# Sant'Anna-Zhao DR-DiD approach and the discarded Chang trial -- stale), nor
# dml_composite_lasso.csv / dml_composite_rf.csv / dml_items_lasso.csv /
# dml_items_rf.csv (all empty -- orphaned files from before the restructure).
#
# No external table-formatting packages required (no kableExtra/knitr) so
# this runs in any R install that already has the base pipeline working.
#
# Output: one self-contained .tex file per table (a full `table` float --
# \centering, booktabs rules, caption, label) written to tables/latex/,
# each ready to \input{} directly into the paper. Captions are drafted but
# meant to be edited to match final section numbering/wording.
# ─────────────────────────────────────────────────────────────────────────────

TABLES_DIR <- "/Users/guypigott/Claude/Projects/MSc Dissertation/tables"
OUT_DIR    <- file.path(TABLES_DIR, "latex")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ── helpers ─────────────────────────────────────────────────────────────────

stars <- function(p) {
  ifelse(is.na(p), "",
  ifelse(p < 0.01, "***",
  ifelse(p < 0.05, "**",
  ifelse(p < 0.10, "*", ""))))
}

fmt <- function(x, d = 3) {
  ifelse(is.na(x), "--", formatC(x, digits = d, format = "f"))
}

# string-concatenation infix, used throughout for readability
`%+%` <- function(a, b) paste0(a, b)

# escapes LaTeX special characters that appear in outcome/item labels
# (outcome names like "Any deprivation (mdch_any)" contain underscores,
# which would otherwise break compilation outside math mode)
# escapes the LaTeX special characters that can plausibly appear in outcome
# labels ("mdch_any" etc. contain underscores). Deliberately doesn't attempt
# to handle backslash/tilde/caret -- none of the source labels contain them,
# and a general-purpose escaper is unnecessary complexity for known data.
esc <- function(x) gsub("([&%$#_{}])", "\\\\\\1", x)

# writes a complete table float. `body_lines` is a character vector of
# already-formatted LaTeX table rows (each ending in "\\\\"), `colspec` is
# the tabular column spec (e.g. "l c c c"). Notes are appended as a plain
# footnotesize paragraph below the tabular -- deliberately not using
# threeparttable/tablenotes so this has zero extra package dependencies
# beyond a standard article class with booktabs.
write_tex_table <- function(file, colspec, header, body_lines, caption, label,
                              notes = NULL, wide = FALSE) {
  tabular_lines <- c(
    paste0("\\begin{tabular}{", colspec, "}"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    body_lines,
    "\\bottomrule",
    "\\end{tabular}"
  )
  # `wide = TRUE` wraps the tabular in \resizebox to shrink it to \textwidth --
  # used for tables with many numeric columns (e.g. six-way country/period
  # splits) that otherwise overflow the page margin under pdflatex. This is
  # the one place this script relies on graphicx beyond booktabs, but the
  # dissertation's own main.tex already loads graphicx for figures, so it's
  # not an extra dependency in practice.
  if (wide) {
    tabular_lines <- c("\\resizebox{\\textwidth}{!}{%", tabular_lines, "}")
  }
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    tabular_lines
  )
  if (!is.null(notes)) {
    # blank line here is load-bearing: without it, \end{tabular} and the
    # footnotesize note run on as the same paragraph, so on any table
    # narrower than \textwidth the note typesets beside the tabular instead
    # of below it (found 2026-07-31 building the LaTeX draft -- Table 1/3
    # in main.tex both showed this before the blank line was added).
    lines <- c(lines, "", "\\vspace{4pt}", paste0("{\\footnotesize ", notes, "}"))
  }
  lines <- c(lines, "\\end{table}")
  writeLines(lines, file)
  cat("  ✓ wrote", file, "\n")
}

sig_note <- "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$. Standard errors clustered at household level in parentheses."

# ── Stage 1a: composite outcomes, Simple / Adjusted / Extended ─────────────

stage1_composite <- function() {
  simple <- read.csv(file.path(TABLES_DIR, "table_simple_did.csv"))
  adj    <- read.csv(file.path(TABLES_DIR, "table_adj_did.csv"))
  ext    <- read.csv(file.path(TABLES_DIR, "table_adj_ext_did.csv"))

  outcomes <- simple$outcome
  header <- "Outcome & Simple & Adjusted & Extended"
  body <- character(0)
  for (i in seq_along(outcomes)) {
    oc <- outcomes[i]
    s <- simple[simple$outcome == oc, ]
    a <- adj[adj$outcome == oc, ]
    e <- ext[ext$outcome == oc, ]
    row1 <- paste(esc(oc),
                   fmt(s$coef) %+% stars(s$pval),
                   fmt(a$coef) %+% stars(a$pval),
                   fmt(e$coef) %+% stars(e$pval),
                   sep = " & ")
    row2 <- paste("",
                   paste0("(", fmt(s$se), ")"),
                   paste0("(", fmt(a$se), ")"),
                   paste0("(", fmt(e$se), ")"),
                   sep = " & ")
    body <- c(body, paste0(row1, " \\\\"), paste0(row2, " \\\\"))
  }
  n_item_row <- function(df) df$n_obs[df$outcome == "Any deprivation (mdch_any)"]
  n_flag_row <- function(df) df$n_obs[df$outcome == "Official MDCH flag"]
  n_food_row <- function(df) df$n_obs[df$outcome == "Food insecurity"]
  footer <- c("\\midrule",
              paste("N (item outcomes)",
                    n_item_row(simple), n_item_row(adj), n_item_row(ext), sep = " & ") %+% " \\\\",
              paste("N (MDCH flag)",
                    n_flag_row(simple), n_flag_row(adj), n_flag_row(ext), sep = " & ") %+% " \\\\")
  # Food insecurity row only appears once table_simple_did.csv etc. have been
  # regenerated with the new "Food insecurity" outcome added to
  # 03_stage1_baseline_did.R's simple_outcomes list -- guarded so this
  # function doesn't break against the older CSVs in the meantime.
  if (length(n_food_row(simple)) > 0) {
    footer <- c(footer,
                paste("N (food insecurity)",
                      n_food_row(simple), n_food_row(adj), n_food_row(ext), sep = " & ") %+% " \\\\")
  }
  body <- c(body, footer)

  write_tex_table(
    file.path(OUT_DIR, "stage1_composite.tex"),
    colspec = "l c c c",
    header = header,
    body_lines = body,
    caption = "Stage 1: baseline difference-in-differences, composite and official outcomes",
    label = "tab:stage1_composite",
    notes = paste(sig_note,
      "Simple: no covariates. Adjusted: six CASE controls (Stewart et al., 2025). Extended: Adjusted plus child age.",
      "Food insecurity is reported here for comparability with Stewart et al. (2025) only; it is not one of the ten MDCH items and is not carried into the item-level or DML decomposition in Stages 2--5.",
      "The food-insecurity module is only observed in FRS from FYE2020 (FYE2017-2019 are structurally unavailable, not merely non-response), so this estimate is identified off a shorter pre-period (FYE2020 only, vs. FYE2017-2020 for the other outcomes) than the rest of the table; the resulting sample size closely matches Stewart et al.'s (2025) own food-insecurity analysis ($n=27{,}511$), consistent with the same underlying data constraint rather than a difference in sample construction. The quarterly pre-trend test for this outcome (Table~\\ref{tab:parallel_trends_quarterly_wald}) does not flag a violation.")
  )
}

# ── Stage 1b: item-level DiD, BH-corrected ──────────────────────────────────

stage1_items <- function() {
  d <- read.csv(file.path(TABLES_DIR, "table_item_did.csv"))
  d <- d[order(d$pval), ]
  header <- "Item & Coef. & N & $p$ (raw) & $p$ (BH) & Sig."
  body <- character(0)
  for (i in seq_len(nrow(d))) {
    r <- d[i, ]
    # plain "Yes" rather than a literal unicode checkmark -- pdflatex (as
    # opposed to xelatex/lualatex) errors on raw unicode glyphs like U+2713
    # unless inputenc/fontenc are specially configured, and this way there's
    # no extra package dependency either
    sigmark <- ifelse(r$sig_bh == "✓", "Yes", "")
    row1 <- paste(esc(r$label),
                   fmt(r$coef) %+% stars(r$pval),
                   format(r$n, big.mark = ","),
                   fmt(r$pval), fmt(r$pval_bh), sigmark,
                   sep = " & ")
    row2 <- paste("", paste0("(", fmt(r$se), ")"), "", "", "", "", sep = " & ")
    body <- c(body, paste0(row1, " \\\\"), paste0(row2, " \\\\"))
  }
  write_tex_table(
    file.path(OUT_DIR, "stage1_items.tex"),
    colspec = "l c c c c c",
    header = header,
    body_lines = body,
    caption = "Stage 1: item-level difference-in-differences (CASE-adjusted), Benjamini-Hochberg corrected",
    label = "tab:stage1_items",
    notes = paste(sig_note, "Rows sorted by raw $p$-value. Sig. column marks BH-significance at the 5\\% FDR level across all ten items.")
  )
}

# ── Stage 2: item-stacked DiD (pooled + item-interacted) ───────────────────

stage2_stacked <- function() {
  d <- read.csv(file.path(TABLES_DIR, "stacked_item_did.csv"))

  # BH-correct the item-interacted rows within each spec (Simple / Adjusted)
  # separately, matching the paper's stated approach (correction applied
  # across the 10 item-level tests) -- not present as a column in the raw
  # CSV, so computed here rather than assumed.
  d$pval_bh <- NA_real_
  for (sp in unique(d$spec)) {
    idx <- which(d$model == "Item-interacted" & d$spec == sp)
    d$pval_bh[idx] <- p.adjust(d$pval[idx], method = "BH")
  }

  simple_items <- d[d$model == "Item-interacted" & d$spec == "Simple", ]
  adj_items    <- d[d$model == "Item-interacted" & d$spec == "Adjusted", ]
  pooled_simple <- d[d$model == "Pooled" & d$spec == "Simple", ]
  pooled_adj    <- d[d$model == "Pooled" & d$spec == "Adjusted", ]

  header <- "Item & Simple & Adjusted"
  body <- character(0)
  for (item_code in unique(simple_items$item)) {
    s <- simple_items[simple_items$item == item_code, ]
    a <- adj_items[adj_items$item == item_code, ]
    bh_mark <- if (!is.na(a$pval_bh) && a$pval_bh < 0.05) "\\textsuperscript{BH}" else ""
    row1 <- paste(esc(s$label),
                   fmt(s$coef) %+% stars(s$pval),
                   fmt(a$coef) %+% stars(a$pval) %+% bh_mark,
                   sep = " & ")
    row2 <- paste("", paste0("(", fmt(s$se), ")"), paste0("(", fmt(a$se), ")"), sep = " & ")
    body <- c(body, paste0(row1, " \\\\"), paste0(row2, " \\\\"))
  }
  body <- c(body, "\\midrule",
            paste("Pooled (all 10 items)",
                  fmt(pooled_simple$coef) %+% stars(pooled_simple$pval),
                  fmt(pooled_adj$coef) %+% stars(pooled_adj$pval),
                  sep = " & ") %+% " \\\\",
            paste("", paste0("(", fmt(pooled_simple$se), ")"),
                  paste0("(", fmt(pooled_adj$se), ")"), sep = " & ") %+% " \\\\")

  write_tex_table(
    file.path(OUT_DIR, "stage2_stacked.tex"),
    colspec = "l c c",
    header = header,
    body_lines = body,
    caption = "Stage 2: item-stacked difference-in-differences, item- and year-fixed-effects model, household-clustered SEs",
    label = "tab:stage2_stacked",
    notes = paste(sig_note, "\\textsuperscript{BH} marks significance at the 5\\% Benjamini-Hochberg FDR threshold within the Adjusted item-interacted spec (computed here; not in the source CSV).")
  )
}

# ── Stage 3: DML composite (lean vs wide covariates x method) ──────────────

stage3_dml_composite <- function() {
  lean <- read.csv(file.path(TABLES_DIR, "dml_did_causalweight_comparison_MDCH.csv"))
  wide <- read.csv(file.path(TABLES_DIR, "dml_did_wide_covariates_MDCH.csv"))

  methods <- c("lasso", "randomforest", "ensemble")
  method_labels <- c(lasso = "Lasso", randomforest = "Random forest", ensemble = "Lasso + RF ensemble")

  header <- "Method & Lean (6 covariates) & Wide ($\\sim$35 covariates)"
  body <- character(0)
  for (m in methods) {
    l <- lean[lean$MLmethod == m, ]
    w <- wide[wide$MLmethod == m, ]
    l_coef <- if (nrow(l) == 1) fmt(l$ATET) %+% stars(l$pval) else "--"
    l_se   <- if (nrow(l) == 1) paste0("(", fmt(l$se), ")") else ""
    w_coef <- if (nrow(w) == 1) fmt(w$ATET) %+% stars(w$pval) else "--"
    w_se   <- if (nrow(w) == 1) paste0("(", fmt(w$se), ")") else ""
    row1 <- paste(method_labels[[m]], l_coef, w_coef, sep = " & ")
    row2 <- paste("", l_se, w_se, sep = " & ")
    body <- c(body, paste0(row1, " \\\\"), paste0(row2, " \\\\"))
  }
  write_tex_table(
    file.path(OUT_DIR, "stage3_dml_composite.tex"),
    colspec = "l c c",
    header = header,
    body_lines = body,
    caption = "Stage 3: doubly robust DML difference-in-differences, official MDCH flag",
    label = "tab:stage3_dml_composite",
    notes = paste(sig_note, "Wide-set ensemble not run (did not complete within a feasible runtime across items -- see Methodology 4.4). ATET = average treatment effect on the treated.")
  )
}

# ── Stage 4: DML item-level (wide covariates, lasso vs RF) ─────────────────

stage4_dml_items <- function() {
  d <- read.csv(file.path(TABLES_DIR, "dml_did_item_level_wide.csv"))
  items <- unique(d$item)

  header <- "Item & Lasso & Random forest"
  body <- character(0)
  for (it in items) {
    l <- d[d$item == it & d$MLmethod == "lasso", ]
    r <- d[d$item == it & d$MLmethod == "randomforest", ]
    lbl <- if (nrow(l) > 0) l$label[1] else r$label[1]
    l_coef <- if (nrow(l) == 1) fmt(l$ATET) %+% stars(l$pval) else "--"
    l_se   <- if (nrow(l) == 1) paste0("(", fmt(l$se), ")") else ""
    r_coef <- if (nrow(r) == 1) fmt(r$ATET) %+% stars(r$pval) else "--"
    r_se   <- if (nrow(r) == 1) paste0("(", fmt(r$se), ")") else ""
    row1 <- paste(esc(lbl), l_coef, r_coef, sep = " & ")
    row2 <- paste("", l_se, r_se, sep = " & ")
    body <- c(body, paste0(row1, " \\\\"), paste0(row2, " \\\\"))
  }
  write_tex_table(
    file.path(OUT_DIR, "stage4_dml_items.tex"),
    colspec = "l c c",
    header = header,
    body_lines = body,
    caption = "Stage 4: doubly robust DML difference-in-differences, per item (wide covariates)",
    label = "tab:stage4_dml_items",
    notes = paste(sig_note, "None of the ten items are significant at the 5\\% BH-corrected threshold under either method.")
  )
}

# ── Stage 5: stacked ML-DiD (joint doubly robust model across items) ───────

stage5_stacked_ml <- function() {
  path <- file.path(TABLES_DIR, "stage5_stacked_item_did.csv")
  if (!file.exists(path)) {
    cat("  ⚠ stage5_stacked_item_did.csv not found -- skipping Stage 5 table.\n")
    return(invisible(NULL))
  }
  d <- read.csv(path)
  d <- d[order(d$pval), ]
  header <- "Item & ATET & $p$ (raw) & $p$ (BH)"
  body <- character(0)
  for (i in seq_len(nrow(d))) {
    r <- d[i, ]
    row1 <- paste(esc(r$label), fmt(r$ATET) %+% stars(r$pval), fmt(r$pval), fmt(r$pval_bh), sep = " & ")
    row2 <- paste("", paste0("(", fmt(r$se), ")"), "", "", sep = " & ")
    body <- c(body, paste0(row1, " \\\\"), paste0(row2, " \\\\"))
  }
  write_tex_table(
    file.path(OUT_DIR, "stage5_stacked_ml.tex"),
    colspec = "l c c c",
    header = header,
    body_lines = body,
    caption = "Stage 5: jointly estimated doubly robust ML-DiD across items",
    label = "tab:stage5_stacked_ml",
    notes = paste(sig_note, "Only items with complete data across the stacked model are shown; see Methodology 4.6.")
  )
}

# ── Parallel trends (quarterly): joint Wald pre-trend test summary ─────────
#
# Source: 05c_stage1_parallel_trends_quarterly.R -> table_A3_quarterly_pretrend_wald.csv
# (confirmed via grep of that script's own write_csv() call). Deliberately
# does NOT table table_A3_quarterly_composite.csv / table_A3_quarterly_items.csv
# -- those are 24-quarter x up-to-21-column coefficient grids, one cell per
# quarter per outcome, far too large for a printed table; that data is what
# the pt_es_quarterly_*.png figures already visualise.

quarterly_pretrend_wald <- function() {
  path <- file.path(TABLES_DIR, "table_A3_quarterly_pretrend_wald.csv")
  if (!file.exists(path)) {
    cat("  ⚠ table_A3_quarterly_pretrend_wald.csv not found -- skipping.\n")
    return(invisible(NULL))
  }
  d <- read.csv(path)

  flagmark <- function(f) if (identical(f, "possible pre-trend")) "\\textbf{flagged}" else "OK"

  header <- "Outcome & $p$ (unadj.) & $p$ (adj.) & $p_{BH}$ (unadj.) & $p_{BH}$ (adj.) & Flag"
  body <- character(0)
  for (i in seq_len(nrow(d))) {
    r <- d[i, ]
    row <- paste(esc(r$outcome),
                  fmt(r$wald_p_unadj), fmt(r$wald_p_adj),
                  fmt(r$wald_p_unadj_bh), fmt(r$wald_p_adj_bh),
                  flagmark(r$flag_adj),
                  sep = " & ")
    body <- c(body, paste0(row, " \\\\"))
  }
  write_tex_table(
    file.path(OUT_DIR, "parallel_trends_quarterly_wald.tex"),
    colspec = "l c c c c c",
    header = header,
    body_lines = body,
    caption = "Parallel trends (quarterly): joint Wald test of pre-treatment trend, by outcome",
    label = "tab:parallel_trends_quarterly_wald",
    notes = "$p$-values from a joint Wald test on quarter-by-treatment interaction terms in the pre-period (unadjusted and CASE-adjusted specifications); $p_{BH}$ is the Benjamini-Hochberg-corrected value across all 15 outcomes. Flag marks outcomes where the BH-corrected adjusted-spec test suggests a possible pre-trend."
  )
}

# ── Summary statistics (02_summary_stats.R output) ─────────────────────────
#
# Source: 02_summary_stats.R, confirmed 2026-07-30 from Guy's console output
# (that script's OUT_DIR is a *relative* "figures" path, so its output landed
# outside the connected project folder -- these functions assume the four
# CSVs below have been copied into TABLES_DIR; see chat).

summary_background <- function() {
  path <- file.path(TABLES_DIR, "background_characteristics.csv")
  if (!file.exists(path)) { cat("  ⚠ background_characteristics.csv not found in tables/ -- skipping.\n"); return(invisible(NULL)) }
  d <- read.csv(path)
  n_row <- d[d$Characteristic == "N", ]
  d <- d[d$Characteristic != "N", ]

  header <- "Characteristic & England & Scotland"
  body <- character(0)
  for (i in seq_len(nrow(d))) {
    r <- d[i, ]
    body <- c(body, paste(esc(r$Characteristic), fmt(r$England), fmt(r$Scotland), sep = " & ") %+% " \\\\")
  }
  body <- c(body, "\\midrule",
            paste("N", format(n_row$England, big.mark = ","), format(n_row$Scotland, big.mark = ","), sep = " & ") %+% " \\\\")

  write_tex_table(
    file.path(OUT_DIR, "summary_background.tex"),
    colspec = "l c c",
    header = header,
    body_lines = body,
    caption = "Sample background characteristics, England vs. Scotland (weighted by dependent-child weight, pooled across all years)",
    label = "tab:summary_background",
    notes = "Weighted proportions (\\texttt{GS\\_INDCH}), replicating Table 2 of Stewart et al. (CASE, 2025). Cf. Table~\\ref{tab:summary_a1_by_period} for the same characteristics split by pre/post period."
  )
}

summary_a1_by_period <- function() {
  path <- file.path(TABLES_DIR, "table_a1_by_period.csv")
  if (!file.exists(path)) { cat("  ⚠ table_a1_by_period.csv not found in tables/ -- skipping.\n"); return(invisible(NULL)) }
  d <- read.csv(path, check.names = FALSE)
  n_row <- d[d$Characteristic == "N", ]
  d <- d[d$Characteristic != "N", ]

  cols <- c("Scotland All", "Scotland Pre-SCP", "Scotland Post-SCP",
            "England All", "England Pre-SCP", "England Post-SCP")
  header <- paste("Characteristic", paste(cols, collapse = " & "), sep = " & ")
  body <- character(0)
  for (i in seq_len(nrow(d))) {
    r <- d[i, ]
    body <- c(body, paste(c(esc(r$Characteristic), fmt(unlist(r[cols]))), collapse = " & ") %+% " \\\\")
  }
  body <- c(body, "\\midrule",
            paste(c("N", format(unlist(n_row[cols]), big.mark = ",")), collapse = " & ") %+% " \\\\")

  write_tex_table(
    file.path(OUT_DIR, "summary_a1_by_period.tex"),
    colspec = "l c c c c c c",
    header = header,
    body_lines = body,
    caption = "Background characteristics by country and period, cf. Table A1 of Stewart et al. (CASE, 2025)",
    label = "tab:summary_a1_by_period",
    notes = "Weighted proportions (\\texttt{GS\\_INDCH}). Pre/Post split at the November 2022 SCP full-rollout cutoff.",
    wide = TRUE
  )
}

summary_sample_composition <- function() {
  path <- file.path(TABLES_DIR, "sample_composition.csv")
  if (!file.exists(path)) { cat("  ⚠ sample_composition.csv not found in tables/ -- skipping.\n"); return(invisible(NULL)) }
  d <- read.csv(path, check.names = FALSE)
  yr_col <- names(d)[1]

  header <- "Year (FYE) & England & Scotland & Total"
  body <- character(0)
  for (i in seq_len(nrow(d))) {
    r <- d[i, ]
    body <- c(body, paste(r[[yr_col]], format(r$England, big.mark = ","),
                           format(r$Scotland, big.mark = ","), format(r$Total, big.mark = ","),
                           sep = " & ") %+% " \\\\")
  }
  write_tex_table(
    file.path(OUT_DIR, "summary_sample_composition.tex"),
    colspec = "l c c c",
    header = header,
    body_lines = body,
    caption = "Sample composition by survey year and country (full cleaned sample, unweighted counts)",
    label = "tab:summary_sample_composition",
    notes = "FYE2021 excluded throughout (Covid-19 fieldwork disruption)."
  )
}

summary_outcomes_table <- function() {
  path <- file.path(TABLES_DIR, "summary_table.csv")
  if (!file.exists(path)) { cat("  ⚠ summary_table.csv not found in tables/ -- skipping.\n"); return(invisible(NULL)) }
  d <- read.csv(path, check.names = FALSE)

  cell <- function(grp, per, var) {
    v <- d[[var]][d$group == grp & d$period == per]
    if (length(v) == 0) return("--")
    fmt(v)
  }
  outcomes <- c("Any deprivation", "Severe deprivation", "Food insecure", "Mean age")
  header <- "Outcome & England (Pre) & England (Post) & Scotland (Pre) & Scotland (Post)"
  body <- character(0)
  for (oc in outcomes) {
    body <- c(body, paste(esc(oc),
                           cell("England", "Pre-SCP", oc), cell("England", "Post-SCP", oc),
                           cell("Scotland", "Pre-SCP", oc), cell("Scotland", "Post-SCP", oc),
                           sep = " & ") %+% " \\\\")
  }
  body <- c(body, "\\midrule",
            paste("N",
                  format(d$N[d$group == "England" & d$period == "Pre-SCP"], big.mark = ","),
                  format(d$N[d$group == "England" & d$period == "Post-SCP"], big.mark = ","),
                  format(d$N[d$group == "Scotland" & d$period == "Pre-SCP"], big.mark = ","),
                  format(d$N[d$group == "Scotland" & d$period == "Post-SCP"], big.mark = ","),
                  sep = " & ") %+% " \\\\")

  write_tex_table(
    file.path(OUT_DIR, "summary_outcomes.tex"),
    colspec = "l c c c c",
    header = header,
    body_lines = body,
    caption = "Summary statistics: key outcomes by country and period (weighted means)",
    label = "tab:summary_outcomes",
    notes = "Weighted by dependent-child weight (\\texttt{GS\\_INDCH}). Severe deprivation ($\\geq$3 of 10 MDCH items) is this dissertation's own approximate measure, not an official DWP statistic; \\texttt{MDCHDMP} (official deep material poverty) only exists from FYE2024."
  )
}

# ── Stage 1b'': CASE exact-replication spec, full coefficients ─────────────
# CASE-Table-3-style layout: two panels (Official MDCH flag / Food insecurity)
# side by side, every coefficient shown (not just the DiD term), reading
# table_case_exact_did.csv (one row per term x outcome, from
# 03_stage1_baseline_did.R's Stage 1b'' block: y ~ treated + post + tp +
# controls | YEAR_f).
#
# Term labels below, esp. the ETH_f2-5 -> Mixed/Asian/Black/Other mapping,
# were NOT taken from a codebook (none was available) -- they were confirmed
# empirically by matching this spec's coefficients against Stewart et al.'s
# (2025) own published Table 3 values, which line up closely for every
# ethnicity category on both outcomes (e.g. ETH_f4 = 0.178 here vs their
# Black = 0.178 on the MDCH flag; 0.086 here vs their 0.087 on food
# insecurity). Re-confirm against the actual FRS ETH codebook if one becomes
# available before this goes in the final write-up.
#
# No Constant/Intercept row: feols() with `| YEAR_f` absorbs the intercept
# into the year fixed effects, unlike CASE's table which reports one --
# not comparable, deliberately omitted rather than shown as a fake blank row.
stage1_case_exact <- function() {
  path <- file.path(TABLES_DIR, "table_case_exact_did.csv")
  if (!file.exists(path)) { cat("  ⚠ table_case_exact_did.csv not found in tables/ -- skipping.\n"); return(invisible(NULL)) }
  d <- read.csv(path)

  term_labels <- c(
    tp                 = "Scotland $\\times$ Post",
    treated             = "Scotland",
    post                = "Post",
    young_head          = "Young household head",
    female_head         = "Female household head",
    ETH_f2              = "\\hspace{1em}Mixed",
    ETH_f3              = "\\hspace{1em}Asian",
    ETH_f4              = "\\hspace{1em}Black",
    ETH_f5              = "\\hspace{1em}Other",
    disabled_household  = "Disability in household",
    lone_parent         = "Lone parent household",
    large_family        = "Large family household"
  )
  term_order <- names(term_labels)

  panels <- c("Official MDCH flag", "Food insecurity")
  panel_labels <- c("A: Child material deprivation", "B: Food insecurity")
  if (!all(panels %in% d$outcome)) {
    cat("  ⚠ table_case_exact_did.csv missing one of:", paste(panels, collapse=", "), "-- skipping.\n")
    return(invisible(NULL))
  }

  header <- paste("", paste(panel_labels, collapse = " & "), sep = " & ")
  body <- character(0)
  # Ethnicity subheading row, matching CASE's layout
  eth_row_after <- "female_head"
  for (term in term_order) {
    a <- d[d$outcome == panels[1] & d$term == term, ]
    b <- d[d$outcome == panels[2] & d$term == term, ]
    if (nrow(a) == 0 && nrow(b) == 0) next
    get1 <- function(df, col) if (nrow(df) == 1) df[[col]][1] else NA_real_
    row1 <- paste(term_labels[[term]],
                   ifelse(is.na(get1(a,"coef")), "--", fmt(get1(a,"coef")) %+% stars(get1(a,"pval"))),
                   ifelse(is.na(get1(b,"coef")), "--", fmt(get1(b,"coef")) %+% stars(get1(b,"pval"))),
                   sep = " & ")
    row2 <- paste("",
                   ifelse(is.na(get1(a,"se")), "", paste0("(", fmt(get1(a,"se")), ")")),
                   ifelse(is.na(get1(b,"se")), "", paste0("(", fmt(get1(b,"se")), ")")),
                   sep = " & ")
    body <- c(body, paste0(row1, " \\\\"), paste0(row2, " \\\\"))
    if (term == eth_row_after) body <- c(body, "Ethnicity (ref.\\ White) & & \\\\")
  }
  n_row <- function(outcome) unique(d$n_obs[d$outcome == outcome])
  body <- c(body, "\\midrule",
            "Controls and year FE & Yes & Yes \\\\",
            paste("N", format(n_row(panels[1]), big.mark=","), format(n_row(panels[2]), big.mark=","), sep = " & ") %+% " \\\\")

  write_tex_table(
    file.path(OUT_DIR, "stage1_case_exact.tex"),
    colspec = "l c c",
    header = header,
    body_lines = body,
    caption = "Stage 1: CASE exact-replication spec (explicit Post term), all coefficients, cf. Table 3 of Stewart et al. (2025)",
    label = "tab:stage1_case_exact",
    notes = paste(sig_note,
      "Diagnostic robustness spec, not the headline Adjusted/Extended results reported elsewhere -- adds an explicit \\texttt{post} term alongside \\texttt{treated}, \\texttt{tp}, the six CASE controls, and year fixed effects, nesting Stewart et al.'s (2025) equation (1). Ethnicity category labels (Mixed/Asian/Black/Other, ref.\\ White) were confirmed empirically against Stewart et al.'s own Table 3 coefficients, not a codebook. Stewart et al.\\ use a stricter significance convention ($^{*}p<0.05$, $^{**}p<0.01$, $^{***}p<0.001$) than the $p<0.10/0.05/0.01$ used throughout this table and elsewhere in this dissertation -- do not compare star counts directly across the two.")
  )
}

# ── run all ──────────────────────────────────────────────────────────────

cat("Generating LaTeX tables into:", OUT_DIR, "\n\n")
cat("Stage 1 (composite)...\n"); stage1_composite()
cat("Stage 1 (CASE exact-replication)...\n"); stage1_case_exact()
cat("Stage 1 (items)...\n");     stage1_items()
cat("Stage 2 (stacked)...\n");   stage2_stacked()
cat("Stage 3 (DML composite)...\n"); stage3_dml_composite()
cat("Stage 4 (DML items)...\n"); stage4_dml_items()
cat("Stage 5 (stacked ML)...\n"); stage5_stacked_ml()
cat("Parallel trends (quarterly Wald)...\n"); quarterly_pretrend_wald()
cat("Summary stats (background)...\n");    summary_background()
cat("Summary stats (Table A1)...\n");      summary_a1_by_period()
cat("Summary stats (sample composition)...\n"); summary_sample_composition()
cat("Summary stats (outcomes)...\n");      summary_outcomes_table()
cat("\nDone. \\input{tables/latex/<name>.tex} each file directly into the Overleaf paper.\n")
