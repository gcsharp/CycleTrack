## =========================================================
## 05_Adherence.R
## Purpose: Derive daily adherence variables from BBI
## Output:  data/processed/daily/garmin_daily_adherence.rds
## =========================================================

## ---- 1. Load packages ----
r_libs_user <- Sys.getenv("R_LIBS_USER")
if (r_libs_user == "") stop("R_LIBS_USER is not set.")

.libPaths(c(r_libs_user, .libPaths()))

library(dplyr)
library(fs)

## ---- 2. Create output folder ----
type_folder <- Sys.getenv("TYPEDIR")

day_folder <- Sys.getenv("DAILYDIR")

if (day_folder == "") stop("DAILYDIR is not set.")
fs::dir_create(day_folder)

## ---- 3. Read BBI data ----
bbi_data <- readRDS(file.path(type_folder, "garmin_clean_garmin_device_bbi.rds"))

## ---- 4. Check columns ----
print(names(bbi_data))
print(dim(bbi_data))

## ---- 5. Make sure key columns are correct types ----
bbi_data$bbi <- suppressWarnings(as.numeric(bbi_data$bbi))
bbi_data$unixTimestampInMs <- suppressWarnings(as.numeric(bbi_data$unixTimestampInMs))

## ---- 7. Create date variable ----
bbi_data$date <- as.Date(bbi_data$ts_utc)

## ---- 8. Sort data ----
bbi_data <- bbi_data %>%
  arrange(participant_id, ts_utc)

## ---- 9. Create worn indicator ----
## Assumes device was worn if bbi present
bbi_data <- bbi_data %>%
  mutate(worn = !is.na(bbi))

## ---- 10. Calculate time to next row in minutes ----
bbi_data <- bbi_data %>%
  group_by(participant_id) %>%
  mutate(
    next_ts = lead(ts_utc),
    diff_min = as.numeric(difftime(next_ts, ts_utc, units = "mins"))
  ) %>%
  ungroup()

## ---- 11. Remove impossible or unhelpful time gaps ----
## Negative gaps should not happen
bbi_data$diff_min[bbi_data$diff_min < 0] <- NA

## ---- 12. Cap very large gaps
## This avoids one missing period being counted as huge wear time
## Start with a simple cap of 5 minutes
bbi_data$diff_min[bbi_data$diff_min > 5] <- 5

## ---- 13. For the final row in a sequence, diff_min will be NA
## Set these to 0 so they do not add time
bbi_data$diff_min[is.na(bbi_data$diff_min)] <- 0

## ---- 14. Count wear time only where worn
bbi_data <- bbi_data %>%
  mutate(wear_min = ifelse(worn, diff_min, 0))

## ---- 15. Summarise by participant and date
daily_adherence <- bbi_data %>%
  group_by(participant_id, date) %>%
  summarise(
    adherence_weartime_24hr = sum(wear_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(participant_id, date)

## ---- 16. Convert to percentage of a full 24 hours
daily_adherence <- daily_adherence %>%
  mutate(
    adherence_weartimepc_24hr = adherence_weartime_24hr / 1440 * 100
  )

## ---- 17. Keep percentage within 0 to 100
daily_adherence$adherence_weartimepc_24hr[
  daily_adherence$adherence_weartimepc_24hr > 100
] <- 100

daily_adherence$adherence_weartimepc_24hr[
  daily_adherence$adherence_weartimepc_24hr < 0
] <- 0

## ---- 18. View output
print(daily_adherence)
summary(daily_adherence)

## ---- 19. Save output
saveRDS(daily_adherence, file.path(day_folder, "garmin_daily_adherence.rds"))

message("05_Adherence.R complete")
