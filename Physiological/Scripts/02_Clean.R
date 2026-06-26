## =========================================================
## 02_Cleaning
## Purpose: Clean Garmin data, reduce obvious noise, create timestamps,
##          save cleaned full file + separate files by data type + QC files
## Outputs:
##   data/processed/garmin_all_clean.rds
##   data/processed/garmin_qc_coverage.rds
##   data/processed/garmin_qc_cleaning_summary.rds
##   data/processed/garmin_qc_noise_flags.rds
##   data/type-specific/garmin_clean_*.rds
## =========================================================

## ---- 1. Load packages ----
r_libs_user <- Sys.getenv("R_LIBS_USER")
if (r_libs_user == "") stop("R_LIBS_USER is not set.")

.libPaths(c(r_libs_user, .libPaths()))

library(dplyr)
library(fs)
library(stringr)

## ---- 2. Read raw data ----
pro_dir <- Sys.getenv("PRODIR")
if (pro_dir == "") stop("PRODIR is not set.")

input_file <- file.path(pro_dir, "garmin_all_raw.rds")
if (!file.exists(input_file)) stop("Input file does not exist: ", input_file)

garmin_all <- readRDS(input_file)

if (nrow(garmin_all) == 0) stop("garmin_all is empty.")

message("garmin_all read.")

rows_initial <- nrow(garmin_all)

## ---- 3. Clean column names ----
names(garmin_all) <- make.names(names(garmin_all), unique = TRUE)

## ---- 4. Check required columns ----
required_cols <- c("participant_id", "data_type", "unixTimestampInMs")

missing_required <- setdiff(required_cols, names(garmin_all))

if (length(missing_required) > 0) {
  stop("Missing required columns: ", paste(missing_required, collapse = ", "))
}

## ---- 5. Clean key identifiers ----
garmin_all <- garmin_all %>%
  mutate(
    participant_id = as.character(participant_id),
    participant_id = str_trim(participant_id),
    participant_id = na_if(participant_id, ""),
    
    data_type = as.character(data_type),
    data_type = str_trim(data_type),
    data_type = na_if(data_type, "")
  )

## ---- 6. Convert unix timestamp to numeric ----
garmin_all <- garmin_all %>%
  mutate(
    unixTimestampInMs = suppressWarnings(as.numeric(unixTimestampInMs))
  )

if (all(is.na(garmin_all$unixTimestampInMs))) {
  stop("unixTimestampInMs could not be converted to numeric.")
}

## ---- 7. Remove unusable rows ----
rows_before_basic_clean <- nrow(garmin_all)

garmin_all <- garmin_all %>%
  filter(
    !is.na(participant_id),
    !is.na(data_type),
    !is.na(unixTimestampInMs)
  )

rows_after_basic_clean <- nrow(garmin_all)

message("Rows removed due to missing participant_id, data_type, or timestamp: ",
        rows_before_basic_clean - rows_after_basic_clean)

## ---- 8. Create UTC timestamp/date ----
garmin_all <- garmin_all %>%
  mutate(
    ts_utc = as.POSIXct(
      unixTimestampInMs / 1000,
      origin = "1970-01-01",
      tz = "UTC"
    ),
    date_utc = as.Date(ts_utc),
    hour_utc = as.integer(format(ts_utc, "%H"))
  )

## ---- 9. Remove rows where timestamp conversion failed ----
rows_before_ts_clean <- nrow(garmin_all)

garmin_all <- garmin_all %>%
  filter(!is.na(ts_utc))

rows_after_ts_clean <- nrow(garmin_all)

message("Rows removed due to failed timestamp conversion: ",
        rows_before_ts_clean - rows_after_ts_clean)

## ---- 10. Clean timezone column if present ----
if ("timezone" %in% names(garmin_all)) {
  garmin_all <- garmin_all %>%
    mutate(
      timezone = as.character(timezone),
      timezone = str_trim(timezone),
      timezone = na_if(timezone, "")
    )
  message("timezone column found and cleaned.")
} else {
  garmin_all$timezone <- NA_character_
  message("No timezone column found. Local time will default to UTC.")
}

## ---- 11. Create local timestamp/date ----
valid_tz <- unique(stats::na.omit(garmin_all$timezone))
valid_olson <- valid_tz[valid_tz %in% OlsonNames()]

garmin_all <- garmin_all %>%
  mutate(
    ts_local = ts_utc,
    date_local = date_utc,
    hour_local = hour_utc,
    timezone_used = "UTC_fallback"
  )

if (length(valid_olson) > 0) {
  
  for (tz_i in valid_olson) {
    
    idx <- which(!is.na(garmin_all$timezone) & garmin_all$timezone == tz_i)
    
    if (length(idx) > 0) {
      
      local_chr <- format(garmin_all$ts_utc[idx], tz = tz_i, usetz = TRUE)
      local_posix <- as.POSIXct(local_chr, tz = tz_i)
      
      garmin_all$ts_local[idx] <- local_posix
      garmin_all$date_local[idx] <- as.Date(local_posix)
      garmin_all$hour_local[idx] <- as.integer(format(local_posix, "%H"))
      garmin_all$timezone_used[idx] <- tz_i
    }
  }
  
  message("Local timestamp variables created where valid timezone names were available.")
  
} else {
  message("No valid Olson timezone names found. Using UTC as local time.")
}

## ---- 12. Flag suspicious timestamps ----
garmin_all <- garmin_all %>%
  mutate(
    flag_future_timestamp = ts_utc > Sys.time(),
    flag_old_timestamp = ts_utc < as.POSIXct("2025-01-01", tz = "UTC")
  )

## ---- 13. Remove impossible timestamp rows ----
rows_before_timestamp_filter <- nrow(garmin_all)

garmin_all <- garmin_all %>%
  filter(
    !flag_future_timestamp,
    !flag_old_timestamp
  )

rows_after_timestamp_filter <- nrow(garmin_all)

message("Rows removed due to impossible timestamps: ",
        rows_before_timestamp_filter - rows_after_timestamp_filter)

## ---- 14. Remove duplicate timestamps after cleaning ----
rows_before_dedup <- nrow(garmin_all)

garmin_all <- garmin_all %>%
  arrange(participant_id, data_type, ts_utc) %>%
  distinct(participant_id, data_type, ts_utc, .keep_all = TRUE)

rows_after_dedup <- nrow(garmin_all)

message("Rows removed as duplicate participant/data_type/timestamp records: ",
        rows_before_dedup - rows_after_dedup)

## ---- 15. Function for flagging suspicious values ----
flag_range <- function(data, column_name, min_value = -Inf, max_value = Inf) {
  
  if (!column_name %in% names(data)) {
    return(rep(FALSE, nrow(data)))
  }
  
  x <- suppressWarnings(as.numeric(data[[column_name]]))
  
  !is.na(x) & (x < min_value | x > max_value)
}

## ---- 16. Flag suspicious physiological/activity values ----
## Remove rows with values that fall within impossible/improbabl ranges.

garmin_all <- garmin_all %>%
  mutate(
    flag_hr_outlier = flag_range(cur_data_all(), "beatsPerMinute", 30, 220),
    flag_steps_outlier = flag_range(cur_data_all(), "steps", 0, Inf),
    flag_stress_outlier = flag_range(cur_data_all(), "stressLevel", 0, 100),
    flag_respiration_outlier = flag_range(cur_data_all(), "respirationRate", 4, 60),
    flag_spo2_outlier = flag_range(cur_data_all(), "spo2", 50, 100)
  )

## ---- 17. Overall noise flag ----
garmin_all <- garmin_all %>%
  mutate(
    flag_any_noise = flag_hr_outlier |
      flag_steps_outlier |
      flag_stress_outlier |
      flag_respiration_outlier |
      flag_spo2_outlier
  )

## ---- 18. QC cleaning summary ----
qc_cleaning_summary <- tibble(
  step = c(
    "Initial rows after loading",
    "After removing missing participant/data_type/timestamp",
    "After removing failed timestamp conversions",
    "After removing impossible timestamps",
    "After removing duplicate participant/data_type/timestamp rows"
  ),
  n_rows = c(
    rows_initial,
    rows_after_basic_clean,
    rows_after_ts_clean,
    rows_after_timestamp_filter,
    rows_after_dedup
  )
) %>%
  mutate(
    rows_removed_from_previous_step = lag(n_rows) - n_rows,
    rows_removed_from_previous_step = ifelse(
      is.na(rows_removed_from_previous_step),
      0,
      rows_removed_from_previous_step
    )
  )

print(qc_cleaning_summary)

## ---- 19. QC noise flags summary ----
qc_noise_flags <- garmin_all %>%
  group_by(data_type) %>%
  summarise(
    n_rows = n(),
    n_hr_outliers = sum(flag_hr_outlier, na.rm = TRUE),
    n_steps_outliers = sum(flag_steps_outlier, na.rm = TRUE),
    n_stress_outliers = sum(flag_stress_outlier, na.rm = TRUE),
    n_respiration_outliers = sum(flag_respiration_outlier, na.rm = TRUE),
    n_spo2_outliers = sum(flag_spo2_outlier, na.rm = TRUE),
    n_any_noise_flags = sum(flag_any_noise, na.rm = TRUE),
    pct_any_noise_flags = n_any_noise_flags / n_rows,
    .groups = "drop"
  ) %>%
  arrange(data_type)

print(qc_noise_flags)

## ---- 20. Coverage summary by participant and data type ----
qc_coverage <- garmin_all %>%
  group_by(participant_id, data_type) %>%
  summarise(
    n_rows = n(),
    first_ts = min(ts_utc, na.rm = TRUE),
    last_ts = max(ts_utc, na.rm = TRUE),
    n_days_utc = n_distinct(date_utc),
    n_days_local = n_distinct(date_local),
    n_unique_ts = n_distinct(ts_utc),
    n_dup_ts = n_rows - n_unique_ts,
    n_noise_flags = sum(flag_any_noise, na.rm = TRUE),
    pct_noise_flags = n_noise_flags / n_rows,
    .groups = "drop"
  ) %>%
  arrange(participant_id, data_type)

print(qc_coverage)

## ---- 21. Quick check of final rows and columns ----
message("Final cleaned dataset dimensions:")
print(dim(garmin_all))

message("Final cleaned dataset columns:")
print(names(garmin_all))

## ---- 22. Create output folders ----
type_dir <- Sys.getenv("TYPEDIR")
if (type_dir == "") stop("TYPEDIR is not set.")

fs::dir_create(pro_dir)
fs::dir_create(type_dir)

message("Output folders set.")

## ---- 23. Save cleaned full dataset ----
saveRDS(garmin_all, file.path(pro_dir, "garmin_all_clean.rds"))

## ---- 24. Save QC summaries ----
saveRDS(qc_coverage, file.path(pro_dir, "garmin_qc_coverage.rds"))
saveRDS(qc_cleaning_summary, file.path(pro_dir, "garmin_qc_cleaning_summary.rds"))
saveRDS(qc_noise_flags, file.path(pro_dir, "garmin_qc_noise_flags.rds"))

message("QC files saved.")

## ---- 25. Split by data type ----
tables <- split(garmin_all, garmin_all$data_type)

## ---- 26. Save one RDS per data type ----
for (nm in names(tables)) {
  
  safe_name <- gsub("[^A-Za-z0-9]+", "_", nm)
  
  saveRDS(
    tables[[nm]],
    file.path(type_dir, paste0("garmin_clean_", safe_name, ".rds"))
  )
}

type_summary <- garmin_all %>%
  mutate(date = as.Date(ts_local)) %>%
  group_by(data_type) %>%
  summarise(
    n_rows = n(),
    n_days = n_distinct(date),
    first_day = min(date, na.rm = TRUE),
    last_day = max(date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(n_days)

print(type_summary)

message("02_Cleaning.R complete")