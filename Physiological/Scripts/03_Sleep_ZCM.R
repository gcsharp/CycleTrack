## =========================================================
## 3.5_Sleep_ZCM
## Purpose: Derive sleep/wake periods using ZCM actigraphy method
## Output: Cleaned full file with sleep/wake/nonwear + sleep metrics
## =========================================================

## ---- 1. Load packages ----
r_libs_user <- Sys.getenv("R_LIBS_USER")
if (r_libs_user == "") stop("R_LIBS_USER is not set.")

.libPaths(c(r_libs_user, .libPaths()))

library(dplyr)
library(lubridate)
library(slider)
library(tidyr)

## ---- 2. Read clean data ----
pro_folder <- Sys.getenv("PRODIR")
garmin_all <- readRDS(file.path(pro_folder, "garmin_all_clean.rds"))

if (nrow(garmin_all) == 0) stop("garmin_all is empty.")

message("garmin_all read.")

names(garmin_all) <- make.names(names(garmin_all))


## ---- 3. Creat time variables ----

x <- garmin_all %>%
  mutate(
    ts_local = as.POSIXct(ts_local),
    minute_local = floor_date(ts_local, "minute"),
    date_local = as.Date(ts_local)
  )

## ---- 4. Minute-level wear detection ----

# Primary wear signal:
# - valid BBI
# - valid HR
# But also:
# - zeroCrossingCount > 0
# - steps > 0
# Movement can suggest wear, but lack of movement does not infer nonwear.

wear_df <- x %>%
  group_by(participant_id, minute_local) %>%
  summarise(
    wear_raw = any(
      (!is.na(bbi) & bbi > 0) |
        (!is.na(beatsPerMinute) & beatsPerMinute > 30) |
        (!is.na(zeroCrossingCount) & zeroCrossingCount > 0) |
        (!is.na(steps) & steps > 0)
    ),
    .groups = "drop"
  ) %>%
  group_by(participant_id) %>%
  complete(
    minute_local = seq(min(minute_local), max(minute_local), by = "1 min"),
    fill = list(wear_raw = FALSE)
  ) %>%
  arrange(minute_local, .by_group = TRUE) %>%
  mutate(
    wear = slide_dbl(
      as.numeric(wear_raw),
      ~ mean(.x, na.rm = TRUE),
      .before = 5,
      .after = 5
    ) >= 0.5
  ) %>%
  ungroup()

## ---- 5. ZCM activity count Az(n) ----
# ZCM uses movement frequency.
# zeroCrossingCount is used as the activity count Az(n).
# Epoch length = 1 minute.

zcm_df <- x %>%
  filter(!is.na(zeroCrossingCount)) %>%
  group_by(participant_id, minute_local) %>%
  summarise(
    Az = sum(zeroCrossingCount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(participant_id) %>%
  complete(
    minute_local = seq(min(minute_local), max(minute_local), by = "1 min"),
    fill = list(Az = 0)
  ) %>%
  arrange(minute_local, .by_group = TRUE) %>%
  mutate(
    Sp_zcm = 0.0033 * (
      1.06 * lag(Az, 4, default = 0) +
        0.54 * lag(Az, 3, default = 0) +
        0.58 * lag(Az, 2, default = 0) +
        0.76 * lag(Az, 1, default = 0) +
        2.30 * Az +
        0.74 * lead(Az, 1, default = 0) +
        0.67 * lead(Az, 2, default = 0)
    ),
    sleep_zcm_raw = Sp_zcm < 1
  ) %>%
  ungroup()

## ---- 5. Minute-level heart rate ----

hr_df <- x %>%
  filter(!is.na(beatsPerMinute), beatsPerMinute > 30) %>%
  group_by(participant_id, minute_local) %>%
  summarise(
    hr = mean(beatsPerMinute, na.rm = TRUE),
    .groups = "drop"
  )

## ---- 6. Minute-level steps ----

steps_df <- x %>%
  filter(!is.na(steps)) %>%
  group_by(participant_id, minute_local) %>%
  summarise(
    steps = sum(steps, na.rm = TRUE),
    .groups = "drop"
  )

## ---- 7. PIM-style activity count and sleep propensity score ----
# This uses Garmin-derived totalEnergy as the activity count Ap(n).
# This is PIM-style rather than raw-acceleration PIM.

pim_df <- x %>%
  group_by(participant_id, minute_local) %>%
  summarise(
    Ap = sum(totalEnergy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(participant_id) %>%
  complete(
    minute_local = seq(min(minute_local), max(minute_local), by = "1 min"),
    fill = list(Ap = 0)
  ) %>%
  arrange(minute_local, .by_group = TRUE) %>%
  mutate(
    Sp_pim = 
      0.04 * lag(Ap, 4, default = 0) +
      0.04 * lag(Ap, 3, default = 0) +
      0.20 * lag(Ap, 2, default = 0) +
      0.20 * lag(Ap, 1, default = 0) +
      2.00 * Ap +
      0.20 * lead(Ap, 1, default = 0) +
      0.20 * lead(Ap, 2, default = 0) +
      0.04 * lead(Ap, 3, default = 0) +
      0.04 * lead(Ap, 4, default = 0),
    
    sleep_pim_raw = Sp_pim < 40
  ) %>%
  ungroup()

## ---- 8. Combine minute-level signals ----
# if time is 20:00 or later, assign to next day's sleep

minute_state <- wear_df %>%
  left_join(zcm_df, by = c("participant_id", "minute_local")) %>%
  left_join(pim_df, by = c("participant_id", "minute_local")) %>%
  left_join(hr_df, by = c("participant_id", "minute_local")) %>%
  left_join(steps_df, by = c("participant_id", "minute_local")) %>%
  mutate(
    hour_local = hour(minute_local),
    date_local = as.Date(minute_local),
    monitoring_date = if_else(
      hour_local >= 20,
      as.Date(minute_local) + 1,
      as.Date(minute_local)
    ),
    in_night = hour_local >= 20 | hour_local < 9,
    sleep_zcm_raw = wear & in_night & coalesce(sleep_zcm_raw, FALSE)
  )

## ---- 9. ZCM vs. PIM agreement ----

minute_state <- minute_state %>%
  mutate(
    sleep_agreement = case_when(
      sleep_zcm_raw & sleep_pim_raw ~ "both_sleep",
      !sleep_zcm_raw & !sleep_pim_raw ~ "both_wake",
      sleep_zcm_raw & !sleep_pim_raw ~ "zcm_sleep_pim_wake",
      !sleep_zcm_raw & sleep_pim_raw ~ "zcm_wake_pim_sleep",
      TRUE ~ NA_character_
    )
  )

## ---- 10. Apply ZCM sleep onset rule ----
# first continuous block of at least 20 minutes sleep allowing no more than 1 minute interruption.
# Bridges 1-minute wake gaps inside sleep runs.

minute_state <- minute_state %>%
  arrange(participant_id, monitoring_date, minute_local) %>%
  group_by(participant_id, monitoring_date) %>%
  mutate(
    sleep_zcm_bridge = {
      y <- coalesce(sleep_zcm_raw, FALSE)
      r <- rle(y)
      
      for (i in seq_along(r$values)) {
        if (
          r$values[i] == FALSE &&
          r$lengths[i] <= 1 &&
          i > 1 &&
          i < length(r$values) &&
          r$values[i - 1] == TRUE &&
          r$values[i + 1] == TRUE
        ) {
          r$values[i] <- TRUE
        }
      }
      
      inverse.rle(r)
    },
    sleep_run_id = cumsum(coalesce(sleep_zcm_bridge != lag(sleep_zcm_bridge), TRUE))
  ) %>%
  group_by(participant_id, monitoring_date, sleep_run_id) %>%
  mutate(
    sleep_run_length = if_else(first(sleep_zcm_bridge), n(), 0L)
  ) %>%
  ungroup()

## ---- 11. Derive sleep onset and offset ----
# sleep_onset_zcm:
# first minute of first bridged sleep run >= 20 minutes

# sleep_offset_zcm:
# last minute scored asleep before getting out of bed

sleep_metrics <- minute_state %>%
  filter(in_night) %>%
  group_by(participant_id, monitoring_date) %>%
  summarise(
    bed_time = min(minute_local, na.rm = TRUE),
    getup_time = max(minute_local, na.rm = TRUE) + minutes(1),
    
    sleep_onset_zcm = {
      idx <- which(sleep_zcm_bridge & sleep_run_length >= 20)
      if (length(idx) == 0) as.POSIXct(NA) else minute_local[min(idx)]
    },
    
    sleep_offset_zcm = {
      idx <- which(coalesce(sleep_zcm_raw, FALSE))
      if (length(idx) == 0) as.POSIXct(NA) else minute_local[max(idx)] + minutes(1)
    },
    
    .groups = "drop"
  )

## ---- 12. Add onset/offset back and create final state ----

minute_state <- minute_state %>%
  left_join(
    sleep_metrics,
    by = c("participant_id", "monitoring_date")
  ) %>%
  mutate(
    in_oo_zcm = !is.na(sleep_onset_zcm) &
      !is.na(sleep_offset_zcm) &
      minute_local >= sleep_onset_zcm &
      minute_local < sleep_offset_zcm,
    
    state = case_when(
      !wear ~ "nonwear",
      in_oo_zcm & sleep_zcm_raw ~ "sleep",
      in_oo_zcm & !sleep_zcm_raw ~ "wake",
      TRUE ~ "wake"
    )
  )

## ---- 12. ZCM PIM percentage agreement ----

pim_zcm_agreement <- minute_state %>%
  filter(in_oo_zcm, wear) %>%
  group_by(participant_id, monitoring_date) %>%
  summarise(
    pct_agreement = mean(sleep_zcm_raw == sleep_pim_raw, na.rm = TRUE),
    pct_both_sleep = mean(sleep_zcm_raw & sleep_pim_raw, na.rm = TRUE),
    pct_zcm_sleep_pim_wake = mean(sleep_zcm_raw & !sleep_pim_raw, na.rm = TRUE),
    pct_zcm_wake_pim_sleep = mean(!sleep_zcm_raw & sleep_pim_raw, na.rm = TRUE),
    .groups = "drop"
  )

## ---- 13. Sleep metrics ----

sleep_metrics_zcm <- minute_state %>%
  filter(in_oo_zcm) %>%
  group_by(participant_id, monitoring_date) %>%
  summarise(
    method = "ZCM",
    
    bed_time = first(bed_time),
    getup_time = first(getup_time),
    sleep_onset = first(sleep_onset_zcm),
    sleep_offset = first(sleep_offset_zcm),
    
    time_in_bed_min = as.numeric(
      difftime(getup_time, bed_time, units = "mins")
    ),
    
    oo_interval_min = as.numeric(
      difftime(sleep_offset, sleep_onset, units = "mins")
    ),
    
    waso_min = sum(state == "wake", na.rm = TRUE),
    
    total_sleep_time_min = oo_interval_min - waso_min,
    
    sleep_onset_latency_min = as.numeric(
      difftime(sleep_onset, bed_time, units = "mins")
    ),
    
    sleep_efficiency = total_sleep_time_min / oo_interval_min,
    
    wake_episodes = sum(
      state == "wake" &
        lag(state, default = "wake") == "sleep",
      na.rm = TRUE
    ),
    
    wear_minutes = sum(wear, na.rm = TRUE),
    mean_Az = mean(Az, na.rm = TRUE),
    mean_Sp_zcm = mean(Sp_zcm, na.rm = TRUE),
    hr_mean = mean(hr, na.rm = TRUE),
    hr_sd = sd(hr, na.rm = TRUE),
    steps_sum = sum(steps, na.rm = TRUE),
    
    .groups = "drop"
  )

## ---- 14. Create contiguous state periods ----

minute_state <- minute_state %>%
  arrange(participant_id, minute_local) %>%
  group_by(participant_id) %>%
  mutate(
    state_change = coalesce(state != lag(state), TRUE),
    period_id = cumsum(state_change)
  ) %>%
  ungroup()

## ---- 15. Period-level summary ----

period_summary <- minute_state %>%
  group_by(participant_id, monitoring_date, period_id, state) %>%
  summarise(
    period_start = min(minute_local),
    period_end = max(minute_local) + minutes(1),
    duration_min = as.numeric(
      difftime(period_end, period_start, units = "mins")
    ),
    n_minutes = n(),
    wear_minutes = sum(wear, na.rm = TRUE),
    hr_mean = mean(hr, na.rm = TRUE),
    hr_sd = sd(hr, na.rm = TRUE),
    Az_mean = mean(Az, na.rm = TRUE),
    Az_sd = sd(Az, na.rm = TRUE),
    Sp_zcm_mean = mean(Sp_zcm, na.rm = TRUE),
    Sp_zcm_sd = sd(Sp_zcm, na.rm = TRUE),
    steps_sum = sum(steps, na.rm = TRUE),
    .groups = "drop"
  )

sleep_wake_period_summary <- period_summary %>%
  filter(state %in% c("sleep", "wake"))

## ---- 16. Add labels back to original long garmin_all ----

garmin_all_labelled <- x %>%
  left_join(
    minute_state %>%
      select(
        participant_id,
        minute_local,
        monitoring_date,
        wear_raw,
        wear,
        Az,
        Sp_zcm,
        sleep_zcm_raw,
        sleep_zcm_bridge,
        sleep_run_length,
        sleep_onset_zcm,
        sleep_offset_zcm,
        in_oo_zcm,
        state,
        period_id
      ),
    by = c("participant_id", "minute_local")
  )

## ---- 17. Summarise original long data within each period ----

period_summary_long <- garmin_all_labelled %>%
  filter(state %in% c("sleep", "wake")) %>%
  group_by(participant_id, monitoring_date, period_id, state) %>%
  summarise(
    period_start = min(ts_local, na.rm = TRUE),
    period_end = max(ts_local, na.rm = TRUE),
    n_rows = n(),
    bbi_mean = mean(bbi, na.rm = TRUE),
    bbi_sd = sd(bbi, na.rm = TRUE),
    hr_mean = mean(beatsPerMinute, na.rm = TRUE),
    hr_sd = sd(beatsPerMinute, na.rm = TRUE),
    stress_mean = mean(stressLevel, na.rm = TRUE),
    stress_sd = sd(stressLevel, na.rm = TRUE),
    steps_sum = sum(steps, na.rm = TRUE),
    zeroCrossing_mean = mean(zeroCrossingCount, na.rm = TRUE),
    Sp_zcm_mean = mean(Sp_zcm, na.rm = TRUE),
    .groups = "drop"
  )

## ---- 18. Daily summary by state ----

daily_period_summary <- minute_state %>%
  filter(state %in% c("sleep", "wake")) %>%
  group_by(participant_id, monitoring_date, state) %>%
  summarise(
    total_minutes = n(),
    wear_minutes = sum(wear, na.rm = TRUE),
    hr_mean = mean(hr, na.rm = TRUE),
    hr_sd = sd(hr, na.rm = TRUE),
    Az_mean = mean(Az, na.rm = TRUE),
    Az_sd = sd(Az, na.rm = TRUE),
    Sp_zcm_mean = mean(Sp_zcm, na.rm = TRUE),
    Sp_zcm_sd = sd(Sp_zcm, na.rm = TRUE),
    steps_sum = sum(steps, na.rm = TRUE),
    .groups = "drop"
  )

## 19. Save RDS files

saveRDS(
  garmin_all_labelled,
  file.path(pro_folder, "garmin_all_labelled_zcm.rds")
)

saveRDS(
  minute_state,
  file.path(pro_folder, "minute_state_zcm.rds")
)

saveRDS(
  sleep_metrics_zcm,
  file.path(pro_folder, "sleep_metrics_zcm.rds")
)

saveRDS(
  period_summary,
  file.path(pro_folder, "period_summary_zcm.rds")
)

saveRDS(
  sleep_wake_period_summary,
  file.path(pro_folder, "sleep_wake_period_summary_zcm.rds")
)

saveRDS(
  period_summary_long,
  file.path(pro_folder, "period_summary_long_zcm.rds")
)

saveRDS(
  daily_period_summary,
  file.path(pro_folder, "daily_period_summary_zcm.rds")
)

saveRDS(
  pim_zcm_agreement,
  file.path(pro_folder, "pim_zcm_agreement.rds")
)

message("03_Sleep_ZCM.R complete")