## =========================================================
## 04_DaySeries
## Purpose: Derive morning, afternoon, and evening periods
## Output:  Cleaned full file with morning, afternoon and
## evening columns
## =========================================================

## ---- Load packages ----
r_libs_user <- Sys.getenv("R_LIBS_USER")
if (r_libs_user == "") stop("R_LIBS_USER is not set.")

.libPaths(c(r_libs_user, .libPaths()))

library(dplyr)
library(lubridate)

## ---- Read clean data ----
pro_folder <- Sys.getenv("PRODIR")
garmin_lab <- readRDS(file.path(pro_folder, "garmin_all_labelled_zcm.rds"))

if (nrow(garmin_lab) == 0) stop("garmin_lab is empty.")

message("garmin_lab read.")

names(garmin_lab) <- make.names(names(garmin_lab))

garmin_lab_labelled <- garmin_lab %>%
  mutate(
    hour_local = hour(minute_local),
    day_period = case_when(
      state == "wake" & hour_local >= 7  & hour_local < 12 ~ "morning",
      state == "wake" & hour_local >= 12 & hour_local < 17 ~ "afternoon",
      state == "wake" & hour_local >= 17 & hour_local < 22 ~ "evening",
      TRUE ~ NA_character_
    )
  )

##---- Save RDS files ----
saveRDS(garmin_lab_labelled, file.path(pro_folder, "garmin_lab_labelled.rds"))

## ---- Save one RDS per data type ----
tables <- split(garmin_lab_labelled, garmin_lab_labelled$data_type)

type_dir <- Sys.getenv("TYPEDIR")
if (type_dir == "") stop("TYPEDIR is not set.")

fs::dir_create(type_dir)

for (nm in names(tables)) {
  
  safe_name <- gsub("[^A-Za-z0-9]+", "_", nm)
  
  saveRDS(
    tables[[nm]],
    file.path(type_dir, paste0("garmin_clean_", safe_name, ".rds"))
  )
}


message("04_DaySeries.R complete")