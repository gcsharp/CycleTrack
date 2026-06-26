## =========================================================
## 06_HRV_periods.R
## Purpose: Derive daily HRV metrics for 24hr, sleep, wake,
##          morning, afternoon, and evening periods
## =========================================================

## ---- 1. Load packages ----
r_libs_user <- Sys.getenv("R_LIBS_USER")
if (r_libs_user == "") stop("R_LIBS_USER is not set.")

.libPaths(c(r_libs_user, .libPaths()))

library(data.table)
library(future)
library(future.apply)

## ---- 2. Set folders ----
day_folder  <- Sys.getenv("DAILYDIR")
pro_folder <- Sys.getenv("PRODIR")

if (day_folder == "") stop("DAILYDIR is not set.")
if (pro_folder == "") stop("PRODIR is not set.")

dir.create(day_folder, recursive = TRUE, showWarnings = FALSE)

## ---- Parallelise ----
n_workers <- as.integer(Sys.getenv("R_FUTURE_WORKERS", Sys.getenv("SLURM_CPUS_PER_TASK", "1")))
if (is.na(n_workers) || n_workers < 1) n_workers <- 1

max_gap_sec <- 3

message("Requested workers: ", n_workers)

## ---- 3. Read labelled Garmin data ----
garmin_all_labelled <- readRDS(file.path(pro_folder, "garmin_lab_labelled.rds"))
setDT(garmin_all_labelled)

message("Input rows: ", nrow(garmin_all_labelled))
message("Input columns:")
print(names(garmin_all_labelled))

## ---- 4. Check dimensions and columns ----
print(dim(garmin_all_labelled))
print(names(garmin_all_labelled))

## ---- 5. Keep only needed columns ----
cols_needed <- c(
  "participant_id",
  "monitoring_date",
  "ts_local",
  "bbi",
  "state",
  "day_period"
)

missing_cols <- setdiff(cols_needed, names(garmin_all_labelled))

if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

dt <- garmin_all_labelled[
  ,
  ..cols_needed
]

## ---- 6. Keep only plausible BBI values ----
dt[, bbi := suppressWarnings(as.numeric(bbi))]
dt <- dt[!is.na(bbi) & bbi >= 300 & bbi <= 2000]

## ---- 7. Make sure timestamp is ordered correctly ----
dt[, date_local := as.Date(ts_local)]
message("BBI rows after cleaning: ", nrow(dt))

## ---- 8. Create one long period table ----
dt_24hr <- dt[
  ,
  .(
    participant_id,
    period_date = date_local,
    ts_local,
    bbi,
    period = "24hr"
  )
]

dt_sleep <- dt[
  state == "sleep",
  .(
    participant_id,
    period_date = monitoring_date,
    ts_local,
    bbi,
    period = "sleep"
  )
]

dt_wake <- dt[
  state == "wake",
  .(
    participant_id,
    period_date = monitoring_date,
    ts_local,
    bbi,
    period = "wake"
  )
]

dt_dayperiod <- dt[
  day_period %in% c("morning", "afternoon", "evening"),
  .(
    participant_id,
    period_date = monitoring_date,
    ts_local,
    bbi,
    period = day_period
  )
]

hrv_input <- rbindlist(
  list(dt_24hr, dt_sleep, dt_wake, dt_dayperiod),
  use.names = TRUE
)

rm(dt_24hr, dt_sleep, dt_wake, dt_dayperiod, dt, garmin_all_labelled)
gc()

message("Rows entering HRV calculation: ", nrow(hrv_input))

## ---- 7. Helper function ----
calc_chunk <- function(x) {

  x <- data.table::as.data.table(x)

  data.table::setorder(
    x,
    participant_id,
    period_date,
    period,
    ts_local
  )

  ## Time gap between consecutive BBI rows within each HRV period
  x[
    ,
    dt_sec := as.numeric(
      difftime(
        ts_local,
        data.table::shift(ts_local),
        units = "secs"
      )
    ),
    by = .(participant_id, period_date, period)
  ]

  x[
    ,
    bbi_diff := data.table::fifelse(
      !is.na(dt_sec) & dt_sec <= max_gap_sec,
      bbi - data.table::shift(bbi),
      NA_real_
    ),
    by = .(participant_id, period_date, period)
  ]

  x[
    ,
    .(
      n_bbi = .N,
      n_bbi_diff = sum(!is.na(bbi_diff)),
      hrv_sdrr = sd(bbi),
      hrv_rmssd = {
        d <- bbi_diff[!is.na(bbi_diff)]
        if (length(d) == 0) NA_real_ else sqrt(mean(d^2))
      },
      hrv_rr50 = sum(abs(bbi_diff) > 50, na.rm = TRUE),
      hrv_prr50 = {
        n_diff <- sum(!is.na(bbi_diff))
        if (n_diff == 0) NA_real_ else
          sum(abs(bbi_diff) > 50, na.rm = TRUE) / n_diff
      }
    ),
    by = .(participant_id, period_date, period)
  ]
}

## ---- Calculate HRV ----

if (n_workers > 1) {
  message("Using within-R parallelism with ", n_workers, " workers")

  plan(multisession, workers = n_workers)
  message("Future workers available: ", nbrOfWorkers())

  chunks <- split(hrv_input, by = "participant_id", keep.by = TRUE)

  daily_hrv_by_period <- rbindlist(
    future_lapply(
      chunks,
      calc_chunk,
      future.packages = "data.table",
      future.globals = c("calc_chunk", "max_gap_sec")
  ),
    use.names = TRUE
  )

  plan(sequential)

} else {
  message("Using single R worker")
  daily_hrv_by_period <- calc_chunk(hrv_input)
}

setorder(daily_hrv_by_period, participant_id, period_date, period)

## ---- 14. Wide-format files ----

daily_hrv_24hr <- daily_hrv_by_period[period == "24hr"]
daily_hrv_sleep <- daily_hrv_by_period[period == "sleep"]
daily_hrv_wake <- daily_hrv_by_period[period == "wake"]
daily_hrv_morning <- daily_hrv_by_period[period == "morning"]
daily_hrv_afternoon <- daily_hrv_by_period[period == "afternoon"]
daily_hrv_evening <- daily_hrv_by_period[period == "evening"]

## ---- 15. View outputs ----
print(daily_hrv_24hr)
print(daily_hrv_sleep)
print(daily_hrv_wake)
print(daily_hrv_morning)
print(daily_hrv_afternoon)
print(daily_hrv_evening)

summary(daily_hrv_by_period)

## ---- 16. Save outputs ----
saveRDS(daily_hrv_24hr,      file.path(day_folder, "garmin_daily_hrv_24hr.rds"))
saveRDS(daily_hrv_sleep,     file.path(day_folder, "garmin_daily_hrv_sleep.rds"))
saveRDS(daily_hrv_wake,      file.path(day_folder, "garmin_daily_hrv_wake.rds"))
saveRDS(daily_hrv_morning,   file.path(day_folder, "garmin_daily_hrv_morning.rds"))
saveRDS(daily_hrv_afternoon, file.path(day_folder, "garmin_daily_hrv_afternoon.rds"))
saveRDS(daily_hrv_evening,   file.path(day_folder, "garmin_daily_hrv_evening.rds"))
saveRDS(daily_hrv_by_period, file.path(day_folder, "garmin_daily_hrv_by_period.rds"))

message("06_HRV_periods.R complete")