## =========================================================
## 09_combine_daily_periods.R
## Purpose: Combine Garmin period-based outputs into one file
## Output:  data/processed/daily/garmin_daily_all_by_period.rds
## =========================================================

## ---- 1. Load packages ----
r_libs_user <- Sys.getenv("R_LIBS_USER")
if (r_libs_user == "") stop("R_LIBS_USER is not set.")

.libPaths(c(r_libs_user, .libPaths()))

library(dplyr)
library(fs)
library(tidyr)

## ---- 2. Set folders ----
day_folder <- Sys.getenv("DAILYDIR")

## ---- 3. Create empty list for period files ----
period_list <- list()

## ---- 4. Read adherence file if it exists ----
## Only include this if you have made a by-period adherence file
adherence_file <- file.path(day_folder, "garmin_daily_adherence_by_period.rds")

if (file_exists(adherence_file)) {
  daily_adherence <- readRDS(adherence_file)
  print("Loaded adherence by period")
  print(names(daily_adherence))
  period_list[[length(period_list) + 1]] <- daily_adherence
}

## ---- 5. Read HRV by-period file if it exists ----
hrv_file <- file.path(day_folder, "garmin_daily_hrv_by_period.rds")

if (file_exists(hrv_file)) {
  daily_hrv <- readRDS(hrv_file)
  print("Loaded HRV by period")
  print(names(daily_hrv))
  period_list[[length(period_list) + 1]] <- daily_hrv
}

## ---- 6. Read stress by-period file if it exists ----
stress_file <- file.path(day_folder, "garmin_daily_stress_by_period.rds")

if (file_exists(stress_file)) {
  daily_stress <- readRDS(stress_file)
  print("Loaded stress by period")
  print(names(daily_stress))
  period_list[[length(period_list) + 1]] <- daily_stress
}

## ---- 7. Read steps by-period file if it exists ----
steps_file <- file.path(day_folder, "garmin_daily_steps_by_period.rds")

if (file_exists(steps_file)) {
  daily_steps <- readRDS(steps_file)
  print("Loaded steps by period")
  print(names(daily_steps))
  period_list[[length(period_list) + 1]] <- daily_steps
}

## ---- 8. Read heart-rate by-period file if it exists ----
hr_file <- file.path(day_folder, "garmin_daily_hr_by_period.rds")

if (file_exists(hr_file)) {
  daily_hr <- readRDS(hr_file)
  print("Loaded heart rate by period")
  print(names(daily_hr))
  period_list[[length(period_list) + 1]] <- daily_hr
}

## ---- 9. Check that at least one file was loaded ----
if (length(period_list) == 0) {
  stop("No by-period files were found to combine.")
}

## ---- 10. Start with the first file ----
daily_all_by_period <- period_list[[1]]

## ---- 11. Join remaining files one by one ----
if (length(period_list) > 1) {
  for (i in 2:length(period_list)) {
    daily_all_by_period <- full_join(
      daily_all_by_period,
      period_list[[i]],
      by = c("participant_id", "period_date", "period")
    )
  }
}

## ---- 12. Sort rows ----
daily_all_by_period <- daily_all_by_period %>%
  arrange(participant_id, period_date, period)

## ---- 13. Check result ----
print(dim(daily_all_by_period))
print(names(daily_all_by_period))
print(head(daily_all_by_period, 10))

## ---- 14. Check for duplicate rows ----
dup_check <- daily_all_by_period %>%
  count(participant_id, period_date, period, name = "n_rows") %>%
  filter(n_rows > 1)

print(dup_check)

## ---- 15. Save combined file ----
saveRDS(
  daily_all_by_period,
  file.path(day_folder, "garmin_daily_all_by_period.rds")
)

## ---- 16. Save duplicate check ----
saveRDS(
  dup_check,
  file.path(day_folder, "garmin_daily_all_by_period_duplicate_check.rds")
)


## ---- 17. Create wide-format version ----
## One row per participant per date
## Period names are added to variable names

daily_all_wide <- daily_all_by_period %>%
  pivot_wider(
    id_cols = c(participant_id, period_date),
    names_from = period,
    values_from = -c(participant_id, period_date, period),
    names_glue = "{.value}_{period}"
  ) %>%
  arrange(participant_id, period_date)

## ---- 18. Check wide result ----
print(dim(daily_all_wide))
print(names(daily_all_wide))
print(head(daily_all_wide, 10))

## ---- 19. Check for duplicate participant-date rows in wide data ----
wide_dup_check <- daily_all_wide %>%
  count(participant_id, period_date, name = "n_rows") %>%
  filter(n_rows > 1)

print(wide_dup_check)

## ---- 20. Save wide-format file ----
saveRDS(
  daily_all_wide,
  file.path(day_folder, "garmin_daily_all_wide.rds")
)

## ---- 21. Save wide duplicate check ----
saveRDS(
  wide_dup_check,
  file.path(day_folder, "garmin_daily_all_wide_duplicate_check.rds")
)

message("10_Combine.R complete")