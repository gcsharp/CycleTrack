
## 07_Stress.R
## Purpose: Derive daily stress metrics for 24hr, sleep, wake,
##          morning, afternoon, and evening periods


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

## ---- 5. Make sure stress is numeric ----
stress_data <- garmin_all_labelled%>%
  mutate(
    stressLevel = suppressWarnings(as.numeric(stressLevel))
  )

## ---- 6. Keep only non-missing stress values ----
stress_data <- stress_data %>%
  filter(!is.na(stressLevel))

## ---- 7. Keep only valid Garmin stress values ----
## Remove special codes -1 and -2, and anything outside 1 to 100
stress_data_valid <- stress_data %>%
  filter(stressLevel >= 1, stressLevel <= 100)

## ---- 8. Create 24hr date variable ----
stress_data_valid <- stress_data_valid %>%
  mutate(
    date_local = as.Date(ts_local)
  )

## ---- 9. Create stress category flags ----
stress_data_valid <- stress_data_valid %>%
  mutate(
    stress_rest_flag   = ifelse(stressLevel >= 1  & stressLevel <= 25, 1, 0),
    stress_low_flag    = ifelse(stressLevel >= 26 & stressLevel <= 50, 1, 0),
    stress_medium_flag = ifelse(stressLevel >= 51 & stressLevel <= 75, 1, 0),
    stress_high_flag   = ifelse(stressLevel >= 76 & stressLevel <= 100, 1, 0)
  )


## ---- Helper function: ---- 
# calculate stress summaries within any grouping structure

calc_stress <- function(df, group_vars, suffix) {
  
  out <- df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n_stress = n(),
      
      stress_mean = mean(stressLevel, na.rm = TRUE),
      stress_min  = min(stressLevel, na.rm = TRUE),
      stress_max  = max(stressLevel, na.rm = TRUE),
      stress_sd   = sd(stressLevel, na.rm = TRUE),
      
      stress_rest   = mean(stress_rest_flag, na.rm = TRUE) * 100,
      stress_low    = mean(stress_low_flag, na.rm = TRUE) * 100,
      stress_medium = mean(stress_medium_flag, na.rm = TRUE) * 100,
      stress_high   = mean(stress_high_flag, na.rm = TRUE) * 100,
      
      .groups = "drop"
    )
  
  names(out)[names(out) == "stress_mean"]   <- paste0("stress_mean_", suffix)
  names(out)[names(out) == "stress_min"]    <- paste0("stress_min_", suffix)
  names(out)[names(out) == "stress_max"]    <- paste0("stress_max_", suffix)
  names(out)[names(out) == "stress_sd"]     <- paste0("stress_sd_", suffix)
  names(out)[names(out) == "stress_rest"]   <- paste0("stress_rest_", suffix)
  names(out)[names(out) == "stress_low"]    <- paste0("stress_low_", suffix)
  names(out)[names(out) == "stress_medium"] <- paste0("stress_medium_", suffix)
  names(out)[names(out) == "stress_high"]   <- paste0("stress_high_", suffix)
  
  out
}


## ---- 10. 24-hour stress ---- 

daily_stress_24hr <- stress_data_valid %>%
  calc_stress(group_vars = c("participant_id", "date_local"), suffix = "24hr") %>%
  arrange(participant_id, date_local)


## ---- 11. Sleep stress ---- 

daily_stress_sleep <- stress_data_valid %>%
  filter(state == "sleep") %>%
  calc_stress(group_vars = c("participant_id", "monitoring_date"), suffix = "sleep") %>%
  arrange(participant_id, monitoring_date)


##  ---- 12. Wake stress ---- 

daily_stress_wake <- stress_data_valid %>%
  filter(state == "wake") %>%
  calc_stress(group_vars = c("participant_id", "monitoring_date"), suffix = "wake") %>%
  arrange(participant_id, monitoring_date)


##  ---- 13. Morning stress ---- 

daily_stress_morning <- stress_data_valid %>%
  filter(day_period == "morning") %>%
  calc_stress(group_vars = c("participant_id", "monitoring_date"), suffix = "morning") %>%
  arrange(participant_id, monitoring_date)


##  ---- 14. Afternoon stress ---- 

daily_stress_afternoon <- stress_data_valid %>%
  filter(day_period == "afternoon") %>%
  calc_stress(group_vars = c("participant_id", "monitoring_date"), suffix = "afternoon") %>%
  arrange(participant_id, monitoring_date)


##  ---- 15. Evening stress ---- 

daily_stress_evening <- stress_data_valid %>%
  filter(day_period == "evening") %>%
  calc_stress(group_vars = c("participant_id", "monitoring_date"), suffix = "evening") %>%
  arrange(participant_id, monitoring_date)


##  ---- 16. Combined long-format object ---- 

daily_stress_by_period <- bind_rows(
  daily_stress_24hr %>%
    transmute(
      participant_id,
      period_date = date_local,
      period = "24hr",
      n_stress,
      stress_mean = stress_mean_24hr,
      stress_min  = stress_min_24hr,
      stress_max  = stress_max_24hr,
      stress_sd   = stress_sd_24hr,
      stress_rest = stress_rest_24hr,
      stress_low  = stress_low_24hr,
      stress_medium = stress_medium_24hr,
      stress_high = stress_high_24hr
    ),
  
  daily_stress_sleep %>%
    transmute(
      participant_id,
      period_date = monitoring_date,
      period = "sleep",
      n_stress,
      stress_mean = stress_mean_sleep,
      stress_min  = stress_min_sleep,
      stress_max  = stress_max_sleep,
      stress_sd   = stress_sd_sleep,
      stress_rest = stress_rest_sleep,
      stress_low  = stress_low_sleep,
      stress_medium = stress_medium_sleep,
      stress_high = stress_high_sleep
    ),
  
  daily_stress_wake %>%
    transmute(
      participant_id,
      period_date = monitoring_date,
      period = "wake",
      n_stress,
      stress_mean = stress_mean_wake,
      stress_min  = stress_min_wake,
      stress_max  = stress_max_wake,
      stress_sd   = stress_sd_wake,
      stress_rest = stress_rest_wake,
      stress_low  = stress_low_wake,
      stress_medium = stress_medium_wake,
      stress_high = stress_high_wake
    ),
  
  daily_stress_morning %>%
    transmute(
      participant_id,
      period_date = monitoring_date,
      period = "morning",
      n_stress,
      stress_mean = stress_mean_morning,
      stress_min  = stress_min_morning,
      stress_max  = stress_max_morning,
      stress_sd   = stress_sd_morning,
      stress_rest = stress_rest_morning,
      stress_low  = stress_low_morning,
      stress_medium = stress_medium_morning,
      stress_high = stress_high_morning
    ),
  
  daily_stress_afternoon %>%
    transmute(
      participant_id,
      period_date = monitoring_date,
      period = "afternoon",
      n_stress,
      stress_mean = stress_mean_afternoon,
      stress_min  = stress_min_afternoon,
      stress_max  = stress_max_afternoon,
      stress_sd   = stress_sd_afternoon,
      stress_rest = stress_rest_afternoon,
      stress_low  = stress_low_afternoon,
      stress_medium = stress_medium_afternoon,
      stress_high = stress_high_afternoon
    ),
  
  daily_stress_evening %>%
    transmute(
      participant_id,
      period_date = monitoring_date,
      period = "evening",
      n_stress,
      stress_mean = stress_mean_evening,
      stress_min  = stress_min_evening,
      stress_max  = stress_max_evening,
      stress_sd   = stress_sd_evening,
      stress_rest = stress_rest_evening,
      stress_low  = stress_low_evening,
      stress_medium = stress_medium_evening,
      stress_high = stress_high_evening
    )
) %>%
  arrange(participant_id, period_date, period)

## ---- 17. View outputs ----
print(dim(daily_stress_24hr))
print(dim(daily_stress_sleep))
print(dim(daily_stress_wake))
print(dim(daily_stress_morning))
print(dim(daily_stress_afternoon))
print(dim(daily_stress_evening))

summary(daily_stress_by_period)

## ---- 18. Save outputs ----
saveRDS(daily_stress_24hr,      file.path(day_folder, "garmin_daily_stress_24hr.rds"))
saveRDS(daily_stress_sleep,     file.path(day_folder, "garmin_daily_stress_sleep.rds"))
saveRDS(daily_stress_wake,      file.path(day_folder, "garmin_daily_stress_wake.rds"))
saveRDS(daily_stress_morning,   file.path(day_folder, "garmin_daily_stress_morning.rds"))
saveRDS(daily_stress_afternoon, file.path(day_folder, "garmin_daily_stress_afternoon.rds"))
saveRDS(daily_stress_evening,   file.path(day_folder, "garmin_daily_stress_evening.rds"))
saveRDS(daily_stress_by_period, file.path(day_folder, "garmin_daily_stress_by_period.rds"))

message("07_Stress.R complete")