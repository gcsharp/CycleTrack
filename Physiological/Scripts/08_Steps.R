## =====================================================
## 08_Steps.R
## Purpose: Derive step metrics for 24hr, sleep, wake,
##          morning, afternoon, and evening periods
## ====================================================

## ---- 1. Load packages ----
r_libs_user <- Sys.getenv("R_LIBS_USER")
if (r_libs_user == "") stop("R_LIBS_USER is not set.")

.libPaths(c(r_libs_user, .libPaths()))

library(dplyr)
library(fs)
library(lubridate)

## ---- 2. Set folders ----
day_folder   <- Sys.getenv("DAILYDIR")
pro_folder <- Sys.getenv("PRODIR")

## ---- 3. Read labelled Garmin data ----
garmin_all_labelled <- readRDS(file.path(pro_folder, "garmin_lab_labelled.rds"))

## ---- 4. Check dimensions and columns ----
print(dim(garmin_all_labelled))
print(names(garmin_all_labelled))

## ---- 5. Make sure key variables are numeric ----
step_data <- garmin_all_labelled %>%
  mutate(
    steps = suppressWarnings(as.numeric(steps)),
    totalSteps = suppressWarnings(as.numeric(totalSteps))
  )

## ---- 6. Keep rows with some step information ----
step_data <- step_data %>%
  filter(!is.na(steps) | !is.na(totalSteps))

## ---- 7. Create date variable for 24hr summaries ----
step_data <- step_data %>%
  mutate(
    date_local = as.Date(ts_local),
    hour_local = floor_date(ts_local, unit = "hour")
  )


## ---- Helper function: ---- 
# calculate step summaries within any grouping structure

calc_steps <- function(df, group_vars, suffix) {
  
  ## ---- A. Hourly summaries within each period ----
  hourly_steps <- df %>%
    group_by(across(all_of(group_vars)), hour_local) %>%
    summarise(
      hourly_steps = sum(steps, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      over_100_flag = ifelse(hourly_steps > 100, 1, 0)
    )
  
  hourly_summary <- hourly_steps %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      steps_minhourlysteps = min(hourly_steps, na.rm = TRUE),
      steps_maxhourlysteps = max(hourly_steps, na.rm = TRUE),
      steps_meanhourlysteps = mean(hourly_steps, na.rm = TRUE),
      steps_sdhourlysteps = sd(hourly_steps, na.rm = TRUE),
      steps_pcstepsover100 = mean(over_100_flag, na.rm = TRUE) * 100,
      n_hours_with_data = n(),
      .groups = "drop"
    )
  
  ## ---- B. Total/summed step summaries within each period ----
  total_summary <- df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      steps_totalsteps = max(totalSteps, na.rm = TRUE),
      steps_sumsteps = sum(steps, na.rm = TRUE),
      n_rows_steps = n(),
      .groups = "drop"
    )
  
  total_summary$steps_totalsteps[
    is.infinite(total_summary$steps_totalsteps)
  ] <- NA
  
  ## ---- C. Join them together ----
  out <- full_join(total_summary, hourly_summary, by = group_vars)
  
  ## ---- D. Add suffixes ----
  names(out)[names(out) == "steps_totalsteps"]      <- paste0("steps_totalsteps_", suffix)
  names(out)[names(out) == "steps_sumsteps"]        <- paste0("steps_sumsteps_", suffix)
  names(out)[names(out) == "steps_minhourlysteps"]  <- paste0("steps_minhourlysteps_", suffix)
  names(out)[names(out) == "steps_maxhourlysteps"]  <- paste0("steps_maxhourlysteps_", suffix)
  names(out)[names(out) == "steps_meanhourlysteps"] <- paste0("steps_meanhourlysteps_", suffix)
  names(out)[names(out) == "steps_sdhourlysteps"]   <- paste0("steps_sdhourlysteps_", suffix)
  names(out)[names(out) == "steps_pcstepsover100"]  <- paste0("steps_pcstepsover100_", suffix)
  
  out
}


## ---- 8. 24-hour steps ---- 

daily_steps_24hr <- step_data %>%
  calc_steps(group_vars = c("participant_id", "date_local"), suffix = "24hr") %>%
  arrange(participant_id, date_local)


## ---- 9. Sleep steps ---- 

daily_steps_sleep <- step_data %>%
  filter(state == "sleep") %>%
  calc_steps(group_vars = c("participant_id", "monitoring_date"), suffix = "sleep") %>%
  arrange(participant_id, monitoring_date)


## ---- 10. Wake steps---- 

daily_steps_wake <- step_data %>%
  filter(state == "wake") %>%
  calc_steps(group_vars = c("participant_id", "monitoring_date"), suffix = "wake") %>%
  arrange(participant_id, monitoring_date)


## ---- 11. Morning steps ---- 

daily_steps_morning <- step_data %>%
  filter(day_period == "morning") %>%
  calc_steps(group_vars = c("participant_id", "monitoring_date"), suffix = "morning") %>%
  arrange(participant_id, monitoring_date)


## ---- 12. Afternoon steps ---- 

daily_steps_afternoon <- step_data %>%
  filter(day_period == "afternoon") %>%
  calc_steps(group_vars = c("participant_id", "monitoring_date"), suffix = "afternoon") %>%
  arrange(participant_id, monitoring_date)


## ---- 13. Evening steps ---- 

daily_steps_evening <- step_data %>%
  filter(day_period == "evening") %>%
  calc_steps(group_vars = c("participant_id", "monitoring_date"), suffix = "evening") %>%
  arrange(participant_id, monitoring_date)


## ---- 14. Combined long-format object ---- 

daily_steps_by_period <- bind_rows(
  daily_steps_24hr %>%
    transmute(
      participant_id,
      period_date = date_local,
      period = "24hr",
      n_rows_steps,
      n_hours_with_data,
      steps_totalsteps = steps_totalsteps_24hr,
      steps_sumsteps = steps_sumsteps_24hr,
      steps_minhourlysteps = steps_minhourlysteps_24hr,
      steps_maxhourlysteps = steps_maxhourlysteps_24hr,
      steps_meanhourlysteps = steps_meanhourlysteps_24hr,
      steps_sdhourlysteps = steps_sdhourlysteps_24hr,
      steps_pcstepsover100 = steps_pcstepsover100_24hr
    ),
  
  daily_steps_sleep %>%
    transmute(
      participant_id,
      period_date = monitoring_date,
      period = "sleep",
      n_rows_steps,
      n_hours_with_data,
      steps_totalsteps = steps_totalsteps_sleep,
      steps_sumsteps = steps_sumsteps_sleep,
      steps_minhourlysteps = steps_minhourlysteps_sleep,
      steps_maxhourlysteps = steps_maxhourlysteps_sleep,
      steps_meanhourlysteps = steps_meanhourlysteps_sleep,
      steps_sdhourlysteps = steps_sdhourlysteps_sleep,
      steps_pcstepsover100 = steps_pcstepsover100_sleep
    ),
  
  daily_steps_wake %>%
    transmute(
      participant_id,
      period_date = monitoring_date,
      period = "wake",
      n_rows_steps,
      n_hours_with_data,
      steps_totalsteps = steps_totalsteps_wake,
      steps_sumsteps = steps_sumsteps_wake,
      steps_minhourlysteps = steps_minhourlysteps_wake,
      steps_maxhourlysteps = steps_maxhourlysteps_wake,
      steps_meanhourlysteps = steps_meanhourlysteps_wake,
      steps_sdhourlysteps = steps_sdhourlysteps_wake,
      steps_pcstepsover100 = steps_pcstepsover100_wake
    ),
  
  daily_steps_morning %>%
    transmute(
      participant_id,
      period_date = monitoring_date,
      period = "morning",
      n_rows_steps,
      n_hours_with_data,
      steps_totalsteps = steps_totalsteps_morning,
      steps_sumsteps = steps_sumsteps_morning,
      steps_minhourlysteps = steps_minhourlysteps_morning,
      steps_maxhourlysteps = steps_maxhourlysteps_morning,
      steps_meanhourlysteps = steps_meanhourlysteps_morning,
      steps_sdhourlysteps = steps_sdhourlysteps_morning,
      steps_pcstepsover100 = steps_pcstepsover100_morning
    ),
  
  daily_steps_afternoon %>%
    transmute(
      participant_id,
      period_date = monitoring_date,
      period = "afternoon",
      n_rows_steps,
      n_hours_with_data,
      steps_totalsteps = steps_totalsteps_afternoon,
      steps_sumsteps = steps_sumsteps_afternoon,
      steps_minhourlysteps = steps_minhourlysteps_afternoon,
      steps_maxhourlysteps = steps_maxhourlysteps_afternoon,
      steps_meanhourlysteps = steps_meanhourlysteps_afternoon,
      steps_sdhourlysteps = steps_sdhourlysteps_afternoon,
      steps_pcstepsover100 = steps_pcstepsover100_afternoon
    ),
  
  daily_steps_evening %>%
    transmute(
      participant_id,
      period_date = monitoring_date,
      period = "evening",
      n_rows_steps,
      n_hours_with_data,
      steps_totalsteps = steps_totalsteps_evening,
      steps_sumsteps = steps_sumsteps_evening,
      steps_minhourlysteps = steps_minhourlysteps_evening,
      steps_maxhourlysteps = steps_maxhourlysteps_evening,
      steps_meanhourlysteps = steps_meanhourlysteps_evening,
      steps_sdhourlysteps = steps_sdhourlysteps_evening,
      steps_pcstepsover100 = steps_pcstepsover100_evening
    )
) %>%
  arrange(participant_id, period_date, period)

## ---- 15. View outputs ----
print(daily_steps_24hr)
print(daily_steps_sleep)
print(daily_steps_wake)
print(daily_steps_morning)
print(daily_steps_afternoon)
print(daily_steps_evening)

summary(daily_steps_by_period)

## ---- 16. Save outputs ----
saveRDS(daily_steps_24hr,      file.path(day_folder, "garmin_daily_steps_24hr.rds"))
saveRDS(daily_steps_sleep,     file.path(day_folder, "garmin_daily_steps_sleep.rds"))
saveRDS(daily_steps_wake,      file.path(day_folder, "garmin_daily_steps_wake.rds"))
saveRDS(daily_steps_morning,   file.path(day_folder, "garmin_daily_steps_morning.rds"))
saveRDS(daily_steps_afternoon, file.path(day_folder, "garmin_daily_steps_afternoon.rds"))
saveRDS(daily_steps_evening,   file.path(day_folder, "garmin_daily_steps_evening.rds"))
saveRDS(daily_steps_by_period, file.path(day_folder, "garmin_daily_steps_by_period.rds"))

message("08_Steps.R complete")