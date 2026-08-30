# ─────────────────────────────────────────────────────────────────────────────
# 00_config.R
# Single source of truth for this project's file paths. Every other script
# sources this at the top. Edit paths here, not per-script.
# ─────────────────────────────────────────────────────────────────────────────
library(here)

DATA_DIR    <- here("data")      # processed CSVs: hbai_clean.csv, hbai_clean_placebo.csv
TABLES_DIR  <- here("tables")
FIGURES_DIR <- here("figures")
dir.create(DATA_DIR,    showWarnings = FALSE, recursive = TRUE)
dir.create(TABLES_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)
RAW_DATA_ROOT <- "/Users/guypigott/python-venv-demo/Dissertation"
