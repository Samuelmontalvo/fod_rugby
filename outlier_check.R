# =============================================================================
# Outlier Diagnostic Script — NSCA Poster Dataset
# Authors: Pablo Tadeo Ríos-Gallardo, PhD & Samuel Montalvo, PhD
#
# Purpose : Identify potential outliers in DSI, CMJ Peak Force, IMTP Peak Force
# Method  : IQR rule (< Q1 - 1.5*IQR  or  > Q3 + 1.5*IQR)
#           Z-score   (|z| > 3)
# Output  : Printed table in terminal + outliers_report.csv (project root)
# NOTE    : Does NOT touch Figures/ or Tables/
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

BASE_DIR <- "/Volumes/ADATA HD710 PRO/fod_rugby"
RAW_DIR  <- file.path(BASE_DIR, "Raw_data")

cat("=== Outlier Diagnostic Report ===\n")
cat("Dataset: Basketball & Rugby (NSCA Poster)\n")
cat(paste0("Run date: ", Sys.time(), "\n\n"))

# =============================================================================
# 1. LOAD DATA — identical to nsca_poster_analysis.R
# =============================================================================
read_utf8 <- function(f) {
  read_csv(file.path(RAW_DIR, f), locale = locale(encoding = "UTF-8"),
           show_col_types = FALSE, name_repair = "minimal")
}
num <- function(x) suppressWarnings(as.numeric(x))

bas_cmj_raw  <- read_utf8("CMJ_Basket - CMJ.csv")
bas_imtp_raw <- read_utf8("IMPT_Basket - Hoja 1.csv")
rug_demo_raw <- read_utf8("CMJ_IMTP_RUGBY - Hoja 1.csv")
rug_cmj_raw  <- read_utf8("CMJ_Rugby - Hoja 1.csv")
rug_imtp_raw <- read_utf8("IMPT_Rugby - Hoja 1.csv")

# =============================================================================
# 2. STANDARDIZE — identical pipeline
# =============================================================================

# --- Basketball CMJ ---
bas_cmj <- bas_cmj_raw |>
  rename(athlete_name = Name, athlete_id = ExternalId, week = Week,
         date_chr = Date, bw_kg = `BW [KG]`,
         jump_height_cm = `Jump Height (Imp-Mom) [cm]`,
         peak_power_w = `Peak Power [W]`, rsi_mod = `RSI-modified [m/s]`,
         cmj_peak_force_n = `Concentric Peak Force [N]`) |>
  select(athlete_id, athlete_name, week, date_chr, bw_kg,
         jump_height_cm, peak_power_w, rsi_mod, cmj_peak_force_n) |>
  mutate(sport = "Basketball", week = as.integer(num(week)),
         date = suppressWarnings(dmy(date_chr)),
         across(c(bw_kg, jump_height_cm, peak_power_w, rsi_mod, cmj_peak_force_n), num),
         jump_height_cm   = ifelse(jump_height_cm < 0 | jump_height_cm > 120, NA, jump_height_cm),
         peak_power_w     = ifelse(peak_power_w < 0, NA, peak_power_w),
         rsi_mod          = ifelse(rsi_mod < 0 | rsi_mod > 5, NA, rsi_mod),
         cmj_peak_force_n = ifelse(cmj_peak_force_n <= 0, NA, cmj_peak_force_n)) |>
  filter(!is.na(athlete_id), !is.na(week))

# --- Basketball IMTP ---
bas_imtp <- bas_imtp_raw |>
  rename(athlete_id = ExternalId, week = Week,
         imtp_peak_force_n = `Peak Vertical Force [N]`) |>
  select(athlete_id, week, imtp_peak_force_n) |>
  mutate(week = as.integer(num(week)),
         imtp_peak_force_n = num(imtp_peak_force_n),
         imtp_peak_force_n = ifelse(imtp_peak_force_n <= 0, NA, imtp_peak_force_n)) |>
  filter(!is.na(athlete_id), !is.na(week))

# --- Rugby Demographics ---
rug_demo <- rug_demo_raw |>
  rename_with(~ trimws(.x)) |>
  rename(athlete_id = externalId, weight_kg = `peso (kg)`,
         height_cm = `altura(cm)`, birth_date = fecha_nacimiento) |>
  mutate(weight_kg = num(weight_kg), height_cm = num(height_cm),
         birth_date = suppressWarnings(dmy(birth_date))) |>
  select(athlete_id, weight_kg, height_cm)

# --- Rugby CMJ ---
rug_cmj <- rug_cmj_raw |>
  rename(athlete_name = Name, athlete_id = ExternalId,
         date_chr = Date, bw_kg = `BW [KG]`,
         jump_height_cm = `Jump Height (Imp-Mom) [cm]`,
         peak_power_w = `Peak Power [W]`, rsi_mod = `RSI-modified [m/s]`,
         cmj_peak_force_n = `Concentric Peak Force [N]`) |>
  select(athlete_id, athlete_name, week, date_chr, bw_kg,
         jump_height_cm, peak_power_w, rsi_mod, cmj_peak_force_n) |>
  mutate(sport = "Rugby", week = as.integer(num(week)),
         date = suppressWarnings(dmy(date_chr)),
         across(c(bw_kg, jump_height_cm, peak_power_w, rsi_mod, cmj_peak_force_n), num),
         jump_height_cm   = ifelse(jump_height_cm < 0 | jump_height_cm > 120, NA, jump_height_cm),
         peak_power_w     = ifelse(peak_power_w < 0, NA, peak_power_w),
         rsi_mod          = ifelse(rsi_mod < 0 | rsi_mod > 5, NA, rsi_mod),
         cmj_peak_force_n = ifelse(cmj_peak_force_n <= 0, NA, cmj_peak_force_n)) |>
  filter(!is.na(athlete_id), !is.na(week))

# --- Rugby IMTP ---
rug_imtp <- rug_imtp_raw |>
  rename(athlete_id = ExternalId,
         imtp_peak_force_n = `Peak Vertical Force [N]`) |>
  select(athlete_id, week, imtp_peak_force_n) |>
  mutate(week = as.integer(num(week)),
         imtp_peak_force_n = num(imtp_peak_force_n),
         imtp_peak_force_n = ifelse(imtp_peak_force_n <= 0, NA, imtp_peak_force_n)) |>
  filter(!is.na(athlete_id), !is.na(week))

# =============================================================================
# 3. AGGREGATE → MERGE → DSI  (identical to main script)
# =============================================================================
agg_cmj <- function(df) {
  df |> group_by(athlete_id, athlete_name, sport, week) |>
    summarise(across(c(bw_kg, jump_height_cm, peak_power_w, rsi_mod, cmj_peak_force_n),
                     ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x)))
}

agg_imtp <- function(df) {
  df |> group_by(athlete_id, week) |>
    summarise(imtp_peak_force_n = max(imtp_peak_force_n, na.rm = TRUE),
              .groups = "drop") |>
    mutate(imtp_peak_force_n = ifelse(is.infinite(imtp_peak_force_n),
                                      NA_real_, imtp_peak_force_n))
}

bas_perf <- left_join(agg_cmj(bas_cmj), agg_imtp(bas_imtp),
                      by = c("athlete_id", "week")) |>
  mutate(dsi = cmj_peak_force_n / imtp_peak_force_n,
         dsi = ifelse(dsi < 0.3 | dsi > 2.0, NA_real_, dsi),
         height_cm = NA_real_, age_years = NA_real_)

rug_perf <- left_join(agg_cmj(rug_cmj), agg_imtp(rug_imtp),
                      by = c("athlete_id", "week")) |>
  left_join(rug_demo, by = "athlete_id") |>
  mutate(bw_kg = coalesce(weight_kg, bw_kg),
         dsi = cmj_peak_force_n / imtp_peak_force_n,
         dsi = ifelse(dsi < 0.3 | dsi > 2.0, NA_real_, dsi)) |>
  select(-weight_kg)

master <- bind_rows(bas_perf, rug_perf) |>
  mutate(sport = factor(sport, levels = c("Basketball", "Rugby")))

cat(sprintf("Master dataset: %d athlete-week rows | %d Basketball | %d Rugby\n\n",
            nrow(master),
            sum(master$sport == "Basketball"),
            sum(master$sport == "Rugby")))

# =============================================================================
# 4. OUTLIER DETECTION
# =============================================================================
TARGET_VARS <- c(
  "DSI"              = "dsi",
  "CMJ Peak Force"   = "cmj_peak_force_n",
  "IMTP Peak Force"  = "imtp_peak_force_n"
)

detect_outliers <- function(data, var_col, var_label) {
  x <- data[[var_col]]
  valid_idx <- !is.na(x)

  # --- IQR method ---
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lo_iqr <- q1 - 1.5 * iqr
  hi_iqr <- q3 + 1.5 * iqr

  # --- Z-score method ---
  mu <- mean(x, na.rm = TRUE)
  sg <- sd(x,   na.rm = TRUE)

  data |>
    filter(!is.na(.data[[var_col]])) |>
    mutate(
      value     = .data[[var_col]],
      z_score   = (value - mu) / sg,
      flag_iqr  = value < lo_iqr | value > hi_iqr,
      flag_z    = abs(z_score) > 3,
      flag_any  = flag_iqr | flag_z,
      variable  = var_label,
      iqr_lo    = lo_iqr,
      iqr_hi    = hi_iqr,
      dist_iqr  = case_when(
        value < lo_iqr ~ sprintf("BELOW  Q1-1.5×IQR  (fence = %.2f)", lo_iqr),
        value > hi_iqr ~ sprintf("ABOVE  Q3+1.5×IQR  (fence = %.2f)", hi_iqr),
        TRUE           ~ "within fence"
      )
    ) |>
    filter(flag_any) |>
    select(sport, athlete_id, athlete_name, week,
           variable, value, z_score, flag_iqr, flag_z, dist_iqr)
}

all_outliers <- imap_dfr(TARGET_VARS, ~ detect_outliers(master, .x, .y))

# =============================================================================
# 5. PRINT REPORT TO TERMINAL
# =============================================================================

sep_line <- paste0(rep("─", 100), collapse = "")

cat("╔══════════════════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                        OUTLIER DETECTION REPORT                                         ║\n")
cat("║         Methods: IQR (< Q1-1.5×IQR or > Q3+1.5×IQR)  |  Z-score (|z| > 3)            ║\n")
cat("╚══════════════════════════════════════════════════════════════════════════════════════════╝\n\n")

if (nrow(all_outliers) == 0) {
  cat("No outliers detected under either criterion.\n")
} else {
  # Summary counts
  cat(sprintf("Total flagged observations: %d\n", nrow(all_outliers)))
  cat(sprintf("  ▸ IQR flagged:     %d\n", sum(all_outliers$flag_iqr)))
  cat(sprintf("  ▸ Z-score flagged: %d\n", sum(all_outliers$flag_z)))
  cat(sprintf("  ▸ Both methods:    %d\n\n",
              sum(all_outliers$flag_iqr & all_outliers$flag_z)))

  # Print by variable
  for (vname in unique(all_outliers$variable)) {
    sub_df <- all_outliers |> filter(variable == vname) |>
      arrange(sport, week)

    cat(sep_line, "\n")
    cat(sprintf("  VARIABLE: %s\n", vname))
    cat(sep_line, "\n")

    # Distribution context for this variable
    raw_vals  <- master[[TARGET_VARS[vname]]]
    cat(sprintf("  Dataset summary  —  N: %d | Mean: %.2f | SD: %.2f | Min: %.2f | Max: %.2f\n",
                sum(!is.na(raw_vals)), mean(raw_vals, na.rm=TRUE),
                sd(raw_vals, na.rm=TRUE), min(raw_vals, na.rm=TRUE),
                max(raw_vals, na.rm=TRUE)))
    q1v <- quantile(raw_vals, 0.25, na.rm=TRUE)
    q3v <- quantile(raw_vals, 0.75, na.rm=TRUE)
    cat(sprintf("  IQR fences       —  Lower: %.2f  |  Upper: %.2f\n\n",
                q1v - 1.5*(q3v - q1v), q3v + 1.5*(q3v - q1v)))

    # Header
    cat(sprintf("  %-12s %-20s %-30s %5s %9s %7s %6s %6s  %-s\n",
                "Sport", "ID", "Athlete Name", "Week", "Value", "Z-score",
                "IQR?", "Z?", "Direction"))
    cat(sprintf("  %s\n", paste(rep("-", 120), collapse="")))

    for (i in seq_len(nrow(sub_df))) {
      r <- sub_df[i, ]
      cat(sprintf("  %-12s %-20s %-30s %5d %9.2f %7.2f %6s %6s  %-s\n",
                  as.character(r$sport),
                  r$athlete_id,
                  substr(r$athlete_name, 1, 29),
                  r$week,
                  r$value,
                  r$z_score,
                  ifelse(r$flag_iqr, "YES", "no"),
                  ifelse(r$flag_z,   "YES", "no"),
                  r$dist_iqr))
    }
    cat("\n")
  }
}

# =============================================================================
# 6. PER-SPORT BREAKDOWN
# =============================================================================
cat(sep_line, "\n")
cat("  PER-SPORT BREAKDOWN\n")
cat(sep_line, "\n")

sport_summary <- all_outliers |>
  group_by(sport, variable) |>
  summarise(n_outliers = n(),
            n_iqr      = sum(flag_iqr),
            n_z        = sum(flag_z),
            athletes   = paste(unique(athlete_id), collapse = ", "),
            .groups    = "drop")

for (i in seq_len(nrow(sport_summary))) {
  r <- sport_summary[i, ]
  cat(sprintf("  %-12s | %-22s | n=%d  (IQR=%d, Z=%d) | Athletes: %s\n",
              as.character(r$sport), r$variable,
              r$n_outliers, r$n_iqr, r$n_z, r$athletes))
}

# =============================================================================
# 7. PER-ATHLETE FREQUENCY (how often is each athlete flagged?)
# =============================================================================
cat("\n")
cat(sep_line, "\n")
cat("  ATHLETE FLAG FREQUENCY (across all variables)\n")
cat(sep_line, "\n")

athlete_freq <- all_outliers |>
  group_by(sport, athlete_id, athlete_name) |>
  summarise(n_flags    = n(),
            variables  = paste(unique(variable), collapse = " + "),
            weeks      = paste(sort(unique(week)), collapse = ", "),
            .groups    = "drop") |>
  arrange(desc(n_flags))

for (i in seq_len(nrow(athlete_freq))) {
  r <- athlete_freq[i, ]
  cat(sprintf("  %-12s | %-20s | %-28s | Flags=%d | Vars: %-s | Weeks: %s\n",
              as.character(r$sport), r$athlete_id,
              substr(r$athlete_name, 1, 27),
              r$n_flags, r$variables, r$weeks))
}

# =============================================================================
# 8. SAVE CSV REPORT
# =============================================================================
report_path <- file.path(BASE_DIR, "outliers_report.csv")

report_csv <- all_outliers |>
  arrange(variable, sport, week) |>
  transmute(
    Sport          = as.character(sport),
    Athlete_ID     = athlete_id,
    Athlete_Name   = athlete_name,
    Week           = week,
    Variable       = variable,
    Value          = round(value, 3),
    Z_score        = round(z_score, 3),
    Flagged_IQR    = ifelse(flag_iqr, "YES", "no"),
    Flagged_Zscore = ifelse(flag_z,   "YES", "no"),
    Direction      = dist_iqr
  )

write_csv(report_csv, report_path)
cat(sprintf("\n\nCSV report saved to: %s\n", report_path))
cat(sprintf("Total rows in CSV: %d\n\n", nrow(report_csv)))
cat("=== Diagnostic Complete — Figures/ and Tables/ were NOT modified ===\n")
