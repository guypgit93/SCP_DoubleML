# =============================================================================
# 02_hbai_summary_stats.R
# Scottish Child Payment Dissertation — HBAI Summary Statistics
# =============================================================================
# Reads:  hbai_lca.csv  (produced by 01_hbai_lca_prep.py)
# Writes: figures/  (PNG plots)
#         hbai_summary_stats.docx  (formatted Word document)
#
# Packages required:
#   tidyverse, scales, ggplot2, patchwork,
#   officer, flextable
#
# Install once with:
#   install.packages(c("tidyverse","scales","patchwork","officer","flextable"))
# =============================================================================

library(tidyverse)
library(scales)
library(patchwork)
library(officer)
library(flextable)

# ── Paths ────────────────────────────────────────────────────────────────────
DATA_ROOT   <- "/Users/guypigott/python-venv-demo/Dissertation"
HBAI_CSV    <- file.path(DATA_ROOT, "data", "hbai_lca.csv")
FIGURES_DIR <- "figures"
DOCX_OUT    <- "hbai_summary_stats.docx"

SCP_EXPAND_YEAR <- 2023   # FY 2022-23: SCP extended to under-16s, £25/week

dir.create(FIGURES_DIR, showWarnings = FALSE)

# ── Colour palette (Manchester purple) ───────────────────────────────────────
COL_SCOT  <- "#3B0064"   # Scotland — deep purple
COL_ENG   <- "#9333C8"   # England  — mid purple
COL_VLINE <- "#F5A623"   # treatment line — gold
COL_LIGHT <- "#EDE0F7"   # table shading

# ── MDCH item labels ─────────────────────────────────────────────────────────
MDCH_LABELS <- c(
  MDCH_ACT  = "Activities (school trips)",
  MDCH_BED  = "Bed / bedroom",
  MDCH_CEL  = "Celebrations",
  MDCH_COAT = "Warm coat",
  MDCH_EQP  = "School equipment",
  MDCH_HOL  = "Holiday away",
  MDCH_LES  = "Leisure / hobby",
  MDCH_PLAY = "Indoor play / games",
  MDCH_PLY  = "Outdoor play area",
  MDCH_TEA  = "Fresh fruit / veg",
  MDCH_TRP  = "Trips / outings",
  MDCH_VEG  = "Vegetables"
)

# =============================================================================
# LOAD DATA
# =============================================================================
cat("Loading", HBAI_CSV, "...\n")
df <- read_csv(HBAI_CSV, show_col_types = FALSE)
cat(sprintf("Loaded: %s rows, %d years\n", format(nrow(df), big.mark=","),
            n_distinct(df$YEAR)))

# Identify MDCH items present in data
lca_items <- names(MDCH_LABELS)[names(MDCH_LABELS) %in% names(df)]
cat(sprintf("MDCH items found: %d / %d\n", length(lca_items), length(MDCH_LABELS)))

# Group and period labels
df <- df |>
  mutate(
    group  = if_else(scotland == 1, "Scotland", "England"),
    period = if_else(post == 1, "Post-SCP\n(2022/23–2023/24)", "Pre-SCP\n(2017/18–2021/22)")
  )

# MDCH-observed subset
df_mdch <- df |> filter(mdch_observed == 1)

cat(sprintf("Full sample: %s  |  MDCH-observed: %s\n",
            format(nrow(df), big.mark=","),
            format(nrow(df_mdch), big.mark=",")))


# =============================================================================
# TABLE 1: Sample counts by year × group
# =============================================================================
cat("\n--- TABLE 1: Sample counts ---\n")

t1_full <- df |>
  count(YEAR, group) |>
  pivot_wider(names_from = group, values_from = n, values_fill = 0) |>
  mutate(Total = England + Scotland)

t1_mdch <- df_mdch |>
  count(YEAR, group) |>
  pivot_wider(names_from = group, values_from = n, values_fill = 0) |>
  rename(`England (MDCH)` = England, `Scotland (MDCH)` = Scotland)

t1 <- left_join(t1_full, t1_mdch, by = "YEAR") |>
  rename(`Year (FY ending)` = YEAR)

print(t1)
write_csv(t1, file.path(FIGURES_DIR, "table1_sample_counts.csv"))


# =============================================================================
# TABLE 2: MDCH item prevalences by group × period
# =============================================================================
cat("\n--- TABLE 2: Item prevalences ---\n")

t2 <- df_mdch |>
  select(group, period, all_of(lca_items)) |>
  pivot_longer(all_of(lca_items), names_to = "variable", values_to = "value") |>
  group_by(variable, group, period) |>
  summarise(
    prevalence = mean(value, na.rm = TRUE),
    n_obs      = sum(!is.na(value)),
    .groups    = "drop"
  ) |>
  mutate(Item = MDCH_LABELS[variable]) |>
  select(Item, variable, group, period, prevalence, n_obs)

t2_pivot <- t2 |>
  select(Item, group, period, prevalence) |>
  mutate(col = paste(group, period, sep = " | ")) |>
  select(-group, -period) |>
  pivot_wider(names_from = col, values_from = prevalence) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

print(t2_pivot)
write_csv(t2,       file.path(FIGURES_DIR, "table2_item_prevalences.csv"))
write_csv(t2_pivot, file.path(FIGURES_DIR, "table2_item_prevalences_pivot.csv"))


# =============================================================================
# TABLE 3: Composite MDCH statistics by group × period
# =============================================================================
cat("\n--- TABLE 3: Composite MDCH statistics ---\n")

t3 <- df_mdch |>
  group_by(group, period) |>
  summarise(
    `Any deprivation (%)` = mean(mdch_any,    na.rm = TRUE),
    `Severe deprivation (≥3 items, %)`  = mean(mdch_severe, na.rm = TRUE),
    `Mean items lacking` = mean(mdch_count, na.rm = TRUE),
    N = sum(!is.na(mdch_any)),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

print(t3)
write_csv(t3, file.path(FIGURES_DIR, "table3_composite_mdch.csv"))


# =============================================================================
# TABLE 4: FOODSEC distribution by group × period
# =============================================================================
cat("\n--- TABLE 4: Food security ---\n")

if ("FOODSEC" %in% names(df) && any(!is.na(df$FOODSEC))) {
  foodsec_labels <- c(`1` = "Food secure",
                      `2` = "Low food security",
                      `3` = "Very low food security")

  t4 <- df |>
    filter(!is.na(FOODSEC)) |>
    mutate(foodsec_label = recode(as.character(FOODSEC), !!!foodsec_labels)) |>
    count(group, period, foodsec_label) |>
    group_by(group, period) |>
    mutate(pct = n / sum(n)) |>
    ungroup() |>
    select(group, period, foodsec_label, pct) |>
    pivot_wider(names_from = c(group, period), values_from = pct,
                names_sep = " | ") |>
    rename(`Food security status` = foodsec_label) |>
    mutate(across(where(is.numeric), \(x) round(x, 3)))

  print(t4)
  write_csv(t4, file.path(FIGURES_DIR, "table4_foodsec.csv"))
} else {
  cat("  FOODSEC not available — skipping Table 4\n")
  t4 <- NULL
}


# =============================================================================
# TABLE 5: Background characteristics (balance table)
# =============================================================================
cat("\n--- TABLE 5: Background characteristics ---\n")

char_vars <- list(
  `Age (mean)`                  = "AGE",
  `UC receipt (%)`              = "NEWFAMBU_UC",
  `Social renter (%)`           = "TENHBAI",
  `Income AHC (£pw, mean)`      = "S_OE_AHC",
  `Children in BU (mean)`       = "NEWFAMBU_KID"
)

t5_rows <- map_dfr(names(char_vars), function(label) {
  var <- char_vars[[label]]
  if (!var %in% names(df)) return(NULL)

  df |>
    filter(!is.na(.data[[var]])) |>
    # for TENHBAI: social rent = code 3
    mutate(val = if (var == "TENHBAI") as.numeric(.data[[var]] == 3)
                 else as.numeric(.data[[var]])) |>
    group_by(group, period) |>
    summarise(stat = mean(val, na.rm = TRUE), .groups = "drop") |>
    mutate(Characteristic = label)
}) |>
  pivot_wider(names_from = c(group, period), values_from = stat,
              names_sep = " | ") |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  select(Characteristic, everything())

print(t5_rows)
write_csv(t5_rows, file.path(FIGURES_DIR, "table5_characteristics.csv"))


# =============================================================================
# FIGURE 1: Parallel trends — composite outcomes
# =============================================================================
cat("\n--- FIGURE 1: Parallel trends ---\n")

plot_parallel <- function(data, var, y_label, title) {
  annual <- data |>
    group_by(YEAR, group) |>
    summarise(mean_val = mean(.data[[var]], na.rm = TRUE),
              .groups = "drop") |>
    filter(!is.na(mean_val))

  ggplot(annual, aes(x = YEAR, y = mean_val, colour = group, shape = group)) +
    geom_vline(xintercept = SCP_EXPAND_YEAR - 0.5,
               colour = COL_VLINE, linewidth = 0.9, linetype = "dashed") +
    annotate("rect",
             xmin = min(annual$YEAR), xmax = SCP_EXPAND_YEAR - 0.5,
             ymin = -Inf, ymax = Inf, alpha = 0.03, fill = "grey50") +
    geom_line(linewidth = 1.1) +
    geom_point(size = 3) +
    scale_colour_manual(values = c("Scotland" = COL_SCOT, "England" = COL_ENG),
                        name = NULL) +
    scale_shape_manual(values = c("Scotland" = 16, "England" = 15),
                       name = NULL) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_x_continuous(breaks = sort(unique(annual$YEAR))) +
    annotate("text", x = SCP_EXPAND_YEAR - 0.4, y = max(annual$mean_val, na.rm=TRUE),
             label = "SCP expansion\n(Nov 2022)",
             hjust = 0, size = 3, colour = COL_VLINE) +
    labs(x = "Financial year (ending)", y = y_label, title = title) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 12),
      axis.text.x   = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

p1a <- plot_parallel(df_mdch, "mdch_any",
                     "Proportion (%)", "Any material deprivation (≥1 item)")
p1b <- plot_parallel(df_mdch, "mdch_severe",
                     "Proportion (%)", "Severe deprivation (≥3 items)")

fig1 <- p1a + p1b +
  plot_annotation(
    title    = "Figure 1: Parallel Trends — Composite Child Material Deprivation",
    subtitle = "Scotland (treated) vs England (control), children aged ≤16",
    caption  = "Source: HBAI individual microdata (UKDA-5828), 2016/17–2023/24. 2020/21 excluded (COVID).",
    theme    = theme(plot.title    = element_text(face = "bold", size = 13),
                     plot.subtitle = element_text(size = 10, colour = "grey40"),
                     plot.caption  = element_text(size = 8,  colour = "grey50"))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path(FIGURES_DIR, "fig1_parallel_trends_composite.png"),
       fig1, width = 12, height = 5.5, dpi = 150)
cat("  ✓ fig1_parallel_trends_composite.png\n")


# =============================================================================
# FIGURE 2: Parallel trends — all 12 MDCH items
# =============================================================================
cat("\n--- FIGURE 2: Item-level parallel trends ---\n")

annual_items <- df_mdch |>
  select(YEAR, group, all_of(lca_items)) |>
  pivot_longer(all_of(lca_items), names_to = "variable", values_to = "value") |>
  group_by(YEAR, group, variable) |>
  summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop") |>
  filter(!is.na(mean_val)) |>
  mutate(label = MDCH_LABELS[variable])

fig2 <- ggplot(annual_items,
               aes(x = YEAR, y = mean_val, colour = group, shape = group)) +
  geom_vline(xintercept = SCP_EXPAND_YEAR - 0.5,
             colour = COL_VLINE, linewidth = 0.7, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("Scotland" = COL_SCOT, "England" = COL_ENG),
                      name = NULL) +
  scale_shape_manual(values = c("Scotland" = 16, "England" = 15),
                     name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = c(2018, 2020, 2022, 2024)) +
  facet_wrap(~ label, ncol = 4, scales = "free_y") +
  labs(
    x       = "Financial year (ending)",
    y       = "Proportion deprived (%)",
    title   = "Figure 2: Parallel Trends — Individual MDCH Items",
    subtitle = "Scotland vs England, children aged ≤16. Dashed gold line = SCP expansion (Nov 2022).",
    caption  = "Source: HBAI individual microdata (UKDA-5828), 2016/17–2023/24."
  ) +
  theme_minimal(base_size = 9.5) +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 9, colour = "grey40"),
    plot.caption    = element_text(size = 8, colour = "grey50"),
    strip.text      = element_text(face = "bold", size = 8.5),
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 7.5),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(FIGURES_DIR, "fig2_parallel_trends_items.png"),
       fig2, width = 14, height = 9, dpi = 150)
cat("  ✓ fig2_parallel_trends_items.png\n")


# =============================================================================
# FIGURE 3: Heatmap — MDCH prevalence by year (Scotland & England side-by-side)
# =============================================================================
cat("\n--- FIGURE 3: Heatmap ---\n")

heat_data <- df_mdch |>
  select(YEAR, group, all_of(lca_items)) |>
  pivot_longer(all_of(lca_items), names_to = "variable", values_to = "value") |>
  group_by(YEAR, group, variable) |>
  summarise(prevalence = mean(value, na.rm = TRUE), .groups = "drop") |>
  mutate(label = factor(MDCH_LABELS[variable], levels = rev(MDCH_LABELS)))

fig3 <- ggplot(heat_data, aes(x = factor(YEAR), y = label, fill = prevalence)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_vline(data = tibble(x = which(sort(unique(heat_data$YEAR)) == SCP_EXPAND_YEAR) - 0.5),
             aes(xintercept = x), colour = COL_VLINE, linewidth = 1.2) +
  scale_fill_gradient(low = "#EDE0F7", high = "#3B0064",
                      labels = percent_format(accuracy = 1),
                      name = "% deprived") +
  facet_wrap(~ group, ncol = 2) +
  labs(
    x       = "Financial year (ending)",
    y       = NULL,
    title   = "Figure 3: Child Material Deprivation by Item and Year",
    subtitle = "Gold line = SCP expanded to all under-16s (Nov 2022)",
    caption  = "Source: HBAI individual microdata (UKDA-5828), 2016/17–2023/24."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 9, colour = "grey40"),
    plot.caption    = element_text(size = 8, colour = "grey50"),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid      = element_blank(),
    strip.text      = element_text(face = "bold")
  )

ggsave(file.path(FIGURES_DIR, "fig3_heatmap.png"),
       fig3, width = 13, height = 6, dpi = 150)
cat("  ✓ fig3_heatmap.png\n")


# =============================================================================
# WORD DOCUMENT (officer + flextable)
# =============================================================================
cat("\n--- Building Word document ---\n")

# ── flextable theme helper ───────────────────────────────────────────────────
make_ft <- function(df_in, caption = NULL) {
  ft <- flextable(df_in) |>
    theme_booktabs() |>
    bg(bg = COL_LIGHT, part = "header") |>
    color(color = "#3B0064", part = "header") |>
    bold(part = "header") |>
    fontsize(size = 9, part = "all") |>
    font(fontname = "Calibri", part = "all") |>
    autofit()
  if (!is.null(caption)) {
    ft <- set_caption(ft, caption = as_paragraph(
      as_chunk(caption, props = fp_text(bold = TRUE, font.size = 10,
                                        color = "#3B0064",
                                        font.family = "Calibri"))
    ))
  }
  ft
}

# ── officer helpers ──────────────────────────────────────────────────────────
heading1 <- function(doc, txt) {
  body_add_par(doc, txt, style = "heading 1")
}
heading2 <- function(doc, txt) {
  body_add_par(doc, txt, style = "heading 2")
}
normal_par <- function(doc, txt) {
  body_add_par(doc, txt, style = "Normal")
}

# ── Build document ───────────────────────────────────────────────────────────
doc <- read_docx()

# Title page — use styles confirmed present in the default template
doc <- doc |>
  body_add_par("Scottish Child Payment: Summary Statistics", style = "heading 1") |>
  body_add_par("MSc Economics Dissertation — University of Manchester", style = "centered") |>
  body_add_par(paste("Generated:", format(Sys.Date(), "%d %B %Y")), style = "Normal") |>
  body_add_break()

# Data notes
doc <- doc |>
  heading1("Data") |>
  normal_par(paste0(
    "Source: Households Below Average Income (HBAI) individual-level microdata, ",
    "UKDA-5828, harmonised files covering 2016/17–2023/24 in constant 2023/24 prices. ",
    "Sample restricted to children aged ≤16 in Scotland and England; 2020/21 excluded due to ",
    "COVID-19 survey disruption. Treatment group: Scotland (Government Office Region 12). ",
    "Control group: all English Government Office Regions. ",
    "Post-treatment period defined as FY 2022/23 onwards, corresponding to the ",
    "November 2022 expansion of the Scottish Child Payment to all children under 16 at £25/week."
  )) |>
  body_add_par("", style = "Normal")

# ── TABLE 1 ──────────────────────────────────────────────────────────────────
doc <- doc |>
  heading2("Table 1: Sample Size by Year and Group") |>
  normal_par(paste0(
    "Full analytic sample (all children aged ≤16 in Scotland/England) and ",
    "the subset with the child material deprivation module observed (MDCH-observed)."
  )) |>
  body_add_flextable(make_ft(
    t1 |> mutate(across(where(is.numeric), \(x) format(x, big.mark=","))),
    caption = "Table 1: Sample size by financial year and treatment group"
  )) |>
  body_add_par("", style = "Normal")

# ── TABLE 2 ──────────────────────────────────────────────────────────────────
t2_doc <- t2_pivot |>
  mutate(across(where(is.numeric), \(x) paste0(round(x * 100, 1), "%")))

doc <- doc |>
  heading2("Table 2: MDCH Item Prevalences by Group and Period") |>
  normal_par(paste0(
    "Proportion of children lacking each item due to cost (deprived = 1), ",
    "by treatment group and pre/post period. ",
    "Items are the 12 binary child material deprivation indicators from the HBAI. ",
    "MDCH-observed subsample only."
  )) |>
  body_add_flextable(make_ft(t2_doc,
    caption = "Table 2: MDCH item deprivation rates (%) by group and period")) |>
  body_add_par("", style = "Normal")

# ── TABLE 3 ──────────────────────────────────────────────────────────────────
t3_doc <- t3 |>
  mutate(
    `Any deprivation (%)` = paste0(round(`Any deprivation (%)` * 100, 1), "%"),
    `Severe deprivation (≥3 items, %)` = paste0(
      round(`Severe deprivation (≥3 items, %)` * 100, 1), "%"),
    N = format(N, big.mark = ",")
  )

doc <- doc |>
  heading2("Table 3: Composite Material Deprivation by Group and Period") |>
  normal_par(paste0(
    "mdch_any = child lacks at least one item; ",
    "mdch_severe = child lacks three or more items; ",
    "mean items = average count of items lacking across all 12."
  )) |>
  body_add_flextable(make_ft(t3_doc,
    caption = "Table 3: Composite MDCH statistics by group and period")) |>
  body_add_par("", style = "Normal")

# ── TABLE 4 ──────────────────────────────────────────────────────────────────
if (!is.null(t4)) {
  t4_doc <- t4 |>
    mutate(across(where(is.numeric), \(x) paste0(round(x * 100, 1), "%")))

  doc <- doc |>
    heading2("Table 4: Food Security Distribution by Group and Period") |>
    normal_par(paste0(
      "FOODSEC is a 3-level scale derived in HBAI from the USDA food security module: ",
      "1 = food secure, 2 = low food security, 3 = very low food security. ",
      "Available from 2019/20 onwards; pre-period coverage is therefore partial."
    )) |>
    body_add_flextable(make_ft(t4_doc,
      caption = "Table 4: Food security status (% of observed) by group and period")) |>
    body_add_par("", style = "Normal")
}

# ── TABLE 5 ──────────────────────────────────────────────────────────────────
if (nrow(t5_rows) > 0) {
  doc <- doc |>
    heading2("Table 5: Background Characteristics by Group and Period") |>
    normal_par(paste0(
      "S_OE_AHC = net equivalised household income after housing costs (£/week, 2023/24 prices). ",
      "UC receipt = NEWFAMBU_UC flag. Social renter = TENHBAI == 3."
    )) |>
    body_add_flextable(make_ft(t5_rows |>
      mutate(across(where(is.numeric), \(x) round(x, 2))),
      caption = "Table 5: Background characteristics by group and period")) |>
    body_add_par("", style = "Normal")
}

# ── FIGURES ──────────────────────────────────────────────────────────────────
doc <- doc |>
  heading1("Figures") |>
  heading2("Figure 1: Parallel Trends — Composite Outcomes")

fig1_path <- file.path(FIGURES_DIR, "fig1_parallel_trends_composite.png")
if (file.exists(fig1_path)) {
  doc <- doc |>
    body_add_img(fig1_path, width = 6.3, height = 3.0) |>
    normal_par(paste0(
      "Note: Annual mean deprivation rates for children aged ≤16 in Scotland and England. ",
      "Dashed gold line = SCP expanded to under-16s at £25/week (November 2022). ",
      "Grey shading = pre-treatment period. 2020/21 excluded (COVID-19 data disruption)."
    ))
}

doc <- doc |>
  heading2("Figure 2: Parallel Trends — Individual MDCH Items")

fig2_path <- file.path(FIGURES_DIR, "fig2_parallel_trends_items.png")
if (file.exists(fig2_path)) {
  doc <- doc |>
    body_add_img(fig2_path, width = 6.3, height = 4.5) |>
    normal_par(paste0(
      "Note: Each panel shows annual mean deprivation rate for one of the 12 binary MDCH items. ",
      "Y-axis scales vary across panels. Gold dashed line = SCP expansion (November 2022)."
    ))
}

doc <- doc |>
  heading2("Figure 3: MDCH Prevalence Heatmap")

fig3_path <- file.path(FIGURES_DIR, "fig3_heatmap.png")
if (file.exists(fig3_path)) {
  doc <- doc |>
    body_add_img(fig3_path, width = 6.3, height = 3.2) |>
    normal_par(paste0(
      "Note: Colour intensity represents the proportion of children deprived on each item. ",
      "Darker purple = higher deprivation. Gold vertical line = SCP expansion."
    ))
}

# ── Save ─────────────────────────────────────────────────────────────────────
print(doc, target = DOCX_OUT)
cat(sprintf("\n✓ Saved Word document: %s\n", DOCX_OUT))
cat(sprintf("✓ CSV tables and PNG figures in: %s/\n", FIGURES_DIR))
