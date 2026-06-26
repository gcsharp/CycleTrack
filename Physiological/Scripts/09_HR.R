## ========================================
## 09_HR.R
## Purpose: Derive heart-rate metrics for 24hr, sleep, wake,
##          morning, afternoon, and evening periods
## ========================================

## ---- 1. Load packages ----
r_libs_user <- Sys.getenv("R_LIBS_USER")
if (r_libs_user == "") stop("R_LIBS_USER is not set.")

.libPaths(c(r_libs_user, .libPaths()))

library(dplyr)
library(fs)

## ---- 2. Set folders ----
day_folder   <- Sys.getenv("DAILYDIR")
pro_folder <- Sys.getenv("PRODIR")

## ---- 3. Read labelled Garmin data ----
garmin_all_labelled <- readRDS(file.path(pro_folder, "garmin_lab_labelled.rds"))

## ---- 4. Check dimensions and columns ----
print(dim(garmin_all_labelled))
print(names(garmin_all_labelled))

## ---- 5. Make sure heart rate is numeric ----
hr_data <- garmin_all_labelled %>%
  mutate(
    beatsPerMinute = suppressWarnings(as.numeric(beatsPerMinute))
  )

## ---- 6. Keep only rows with non-missing heart rate ----
hr_data <- hr_data %>%
  filter(!is.na(beatsPerMinute))

## ---- 7. Keep only plausible heart-rate values ----
## Simple first-pass cleaning rule
hr_data <- hr_data %>%
  filter(beatsPerMinute >= 20, beatsPerMinute <= 250)

## ---- 8. Create date variable for 24hr summaries ----
hr_data <- hr_data %>%
  mutate(
    date_local = as.Date(ts_local)
  )


## ---- Helper function: ---- 
# calculate heart-rate summaries within any grouping structure

calc_hr <- function(df, group_vars, suffix) {
  
  out <- df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n_hr = n(),
      hr_mean = mean(beatsPerMinute, na.rm = TRUE),
      hr_min  = min(beatsPerMinute, na.rm = TRUE),
      hr_max  = max(beatsPerMinute, na.rm = TRUE),
      hr_sd   = sd(beatsPerMinute, na.rm = TRUE),
      .groups = "drop"
    )
  
  names(out)[names(out) == "hr_mean"] <- paste0("hr_mean_", suffix)
  names(out)[names(out) == "hr_min"]  <- paste0("hr_min_", suffix)
  names(out)[names(out) == "hr_max"]  <- paste0("hr_max_", suffix)
  names(out)[names(out) == "hr_sd"]   <- paste0("hr_sd_", suffix)
  
  out
}


## ---- 9. 24-hour heart rate ---- 

daily_hr_24hr <- hr_data %>%
  calc_hr(group_vars = c("participant_id", "date_local"), suffix = "24hr") %>%
  arrange(participant_id, date_local)


## ---- 10. Sleep heart rate ---- 

daily_hr_sleep <- hr_data %>%
  filter(state == "sleep") %>%
  calc_hr(group_vars = c("participant_id", "monitoring_date"), suffix = "sleep") %>%
  arrange(participant_id, monitoring_date)


## ---- 11. Wake heart rate ---- 

daily_hr_wake <- hr_data %>%
  filter(state == "wake") %>%
  calc_hr(group_vars = c("participant_id", "monitoring_date"), suffix = "wake") %>%
  arrange(participant_id, monitoring_date)


## ---- 12. Morning heart rate ---- 

daily_hr_morning <- hr_data %>%
  filter(day_period == "morning") %>%
  calc_hr(group_vars = c("participant_id", "monitoring_date"), suffix = "morning") %>%
  arrange(participant_id, monitoring_date)


## ---- 13. Afternoon heart rate ---- 

daily_hr_afternoon <- hr_data %>%
  filter(day_period == "afternoon") %>%
  calc_hr(group_vars = c("participant_id", "monitoring_date"), suffix = "afternoon") %>%
  arrange(participant_id, monitoring_date)


## ---- 14. Evening heart rate ---- 

daily_hr_evening <- hr_data %>%
  filter(day_period == "evening") %>%
  calc_hr(group_vars = c("participant_id", "monitoring_date"), suffix = "evening") %>%
  arrange(participant_id, monitoring_date)


## ---- 15. Combined long-format object ---- 

daily_hr_by_period <- bind_rows(
  daily_hr_24hr %>%
    transmute(
      participant_id,
      period_date = date_local,
      period = "24hr",
      n_hr,
      hr_mean = hr_mean_24hr,
      hr_min  = hr_min_24hr,
      hr_max  = hr_max_24hr,
      hr_sd   = hr_sd_24hr
    ),
  
  daily_hr_sleep %>%
    transmute(
      participant_id,
      period_date = monitoring_date,
      period = "sleep",
      n_hr,
      hr_mean = hr_mean_sleep,
      hr_min  = hr_min_sleep,
      hr_max  = hr_max_sleep,
      hr_sd   = hr_sd_sleep
    ),
  
  daily_hr_wake %>%
    transmute(
      participant_id,
      period_date = monitoring_date,
      period = "wake",
      n_hr,
      hr_mean = hr_mean_wake,
      hr_min  = hr_min_wake,
      hr_max  = hr_max_wake,
      hr_sd   = hr_sd_wake
    ),
  
  daily_hr_morning %>%
    transmute(
      participant_id,
      period_date = monitoring_date,
      period = "morning",
      n_hr,
      hr_mean = hr_mean_morning,
      hr_min  = hr_min_morning,
      hr_max  = hr_max_morning,
      hr_sd   = hr_sd_morning
    ),
  
  daily_hr_afternoon %>%
    transmute(
      participant_id,
      period_date = monitoring_date,
      period = "afternoon",
      n_hr,
      hr_mean = hr_mean_afternoon,
      hr_min  = hr_min_afternoon,
      hr_max  = hr_max_afternoon,
      hr_sd   = hr_sd_afternoon
    ),
  
  daily_hr_evening %>%
    transmute(
      participant_id,
      period_date = monitoring_date,
      period = "evening",
      n_hr,
      hr_mean = hr_mean_evening,
      hr_min  = hr_min_evening,
      hr_max  = hr_max_evening,
      hr_sd   = hr_sd_evening
    )
) %>%
  arrange(participant_id, period_date, period)

## ---- 16. View outputs ----
print(daily_hr_24hr)
print(daily_hr_sleep)
print(daily_hr_wake)
print(daily_hr_morning)
print(daily_hr_afternoon)
print(daily_hr_evening)

summary(daily_hr_by_period)

## ---- 17. Save outputs ----
saveRDS(daily_hr_24hr,      file.path(day_folder, "garmin_daily_hr_24hr.rds"))
saveRDS(daily_hr_sleep,     file.path(day_folder, "garmin_daily_hr_sleep.rds"))
saveRDS(daily_hr_wake,      file.path(day_folder, "garmin_daily_hr_wake.rds"))
saveRDS(daily_hr_morning,   file.path(day_folder, "garmin_daily_hr_morning.rds"))
saveRDS(daily_hr_afternoon, file.path(day_folder, "garmin_daily_hr_afternoon.rds"))
saveRDS(daily_hr_evening,   file.path(day_folder, "garmin_daily_hr_evening.rds"))
saveRDS(daily_hr_by_period, file.path(day_folder, "garmin_daily_hr_by_period.rds"))

message("09_HR.R complete")