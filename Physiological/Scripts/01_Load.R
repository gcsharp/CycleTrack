## =========================================================
## 01_Loading
## Purpose: Find all participant Garmin CSV files and stack
##          them into one dataset
## Output:  data/processed/garmin_all_raw.rds
## =========================================================

## ---- 1. Load packages ----
r_libs_user <- Sys.getenv("R_LIBS_USER")
if (r_libs_user == "") stop("R_LIBS_USER is not set.")

.libPaths(c(r_libs_user, .libPaths()))

library(fs)
library(readr)
library(dplyr)
library(stringr)

## ---- 2. Set top folder containing participant folders ----
top_folder <- Sys.getenv("RAWDATADIR")

## ---- 3. Check the folder exists ----
if (top_folder == "") stop("RAWDATADIR is not set.")
if (!fs::dir_exists(top_folder)) stop("Directory does not exist: ", top_folder)

## ---- 4. Find participant folders ----
## Participant folders = folder containing one or more folders beginning with "garmin"
all_dirs <- dir_ls(top_folder, recurse = TRUE, type = "directory")

has_garmin_subfolder <- function(x) {
  subdirs <- dir_ls(x, type = "directory", fail = FALSE)
  any(grepl("^garmin-", path_file(subdirs)))
}

participant_folders <- all_dirs[vapply(all_dirs, has_garmin_subfolder, logical(1))]

participant_folders <- sort(unique(participant_folders))

print("Participant folders found:")
print(participant_folders)

## ---- 6. Create an empty list to store all data ----
all_data_list <- list()

## ---- 7. Start a counter for the list ----
k <- 1

## ---- 8. Loop through each participant folder ----
for (participant_folder in participant_folders) {
  
  ## Participant folder name
  participant_folder_name <- path_file(participant_folder)
  
  ## Weekly batch folder name
  relative_participant_path <- path_rel(participant_folder, start = top_folder)

  batch_folder <- path_file(path_dir(participant_folder))
  
  ## For now, use participant folder name as participant ID
  participant_id <- sub("_[^_]+$", "", participant_folder_name)
  
  cat("\n====================\n")
  cat("Batch folder:", batch_folder, "\n")
  cat("Participant folder:", participant_folder_name, "\n")
  
  ## Find subfolders inside participant folder
  subfolders <- dir_ls(participant_folder, type = "directory")
  
  ## Keep only folders that start with "garmin-"
  garmin_folders <- subfolders[grepl("^garmin-", path_file(subfolders))]
  
  ## Skip participant if no Garmin folders found
  if (length(garmin_folders) == 0) next
  
  ## ---- 9. Loop through each Garmin folder ----
  for (garmin_folder in garmin_folders) {
    
    ## Data type is the folder name
    data_type <- path_file(garmin_folder)
    
    cat("Data type:", data_type, "\n")
    
    ## Find files inside this Garmin folder
    files <- dir_ls(garmin_folder, type = "file")
    
    ## Keep only CSV files
    files <- files[tolower(tools::file_ext(files)) == "csv"]
    
    ## Skip if no CSV files found
    if (length(files) == 0) next
    
    ## ---- 10. Loop through each CSV file ----
    for (f in files) {
      
      cat("Reading file:", path_file(f), "\n")
      
      ## Read the first chunk of lines to find the real header row
      lines <- readLines(f, n = 200, warn = FALSE)
      
      ## Look for the true header row
      header_candidates <- which(
        str_detect(lines, "^timezone,unixTimestampInMs,isoDate,deviceType")
      )
      
      ## Skip file if no header row found
      if (length(header_candidates) == 0) {
        message("Skipping file - no header found: ", f)
        next
      }
      
      ## Work out how many rows to skip
      header_line <- header_candidates[1]
      skip_rows <- header_line - 1
      
      ## Read the CSV
      df <- read_csv(f, skip = skip_rows, show_col_types = FALSE)
      
      ## Skip empty files
      if (nrow(df) == 0) next
      
      ## Add identifiers
      df$participant_id <- participant_id
      df$relative_participant_path <- relative_participant_path
      df$participant_folder <- participant_folder_name
      df$batch_folder <- batch_folder
      df$data_type <- data_type
      df$source_file <- path_file(f)
      df$source_path <- f
      
      ## Store in the list
      all_data_list[[k]] <- df
      k <- k + 1
    }
  }
}

## ---- 11. Combine everything into one dataset ----
if (length(all_data_list) == 0) {
  garmin_all <- tibble()
} else {
  garmin_all <- bind_rows(all_data_list)
}

## ---- 12. Quick check before deduplication ----
cat("\nRows before deduplication:", nrow(garmin_all), "\n")

print(dim(garmin_all))
print(names(garmin_all))

## ---- 13. Deduplicate repeated observations across weekly downloads ----
## Keep one row for each participant + data type + timestamp
if (nrow(garmin_all) > 0) {
  garmin_all <- garmin_all %>%
    distinct(participant_id, data_type, unixTimestampInMs, .keep_all = TRUE)
}

## ---- 14. Quick check after deduplication ----
cat("Rows after deduplication:", nrow(garmin_all), "\n")

## ---- 15. QC counts by participant and data type ----
if (nrow(garmin_all) > 0) {
  
  qc_counts <- garmin_all %>%
    count(participant_id, data_type, name = "n_rows") %>%
    arrange(participant_id, data_type)
  
  print(qc_counts)
}

## ---- 16. Get output folder from config and create it ----
pro_dir <- Sys.getenv("PRODIR")

if (pro_dir == "") stop("PRODIR is not set.")
if (!fs::dir_exists(pro_dir)) fs::dir_create(pro_dir)

## ---- 17. Save raw stacked data ----
saveRDS(garmin_all, file.path(pro_dir, "garmin_all_raw.rds"))

## ---- 18. Save QC counts if created ----
if (exists("qc_counts")) {
  saveRDS(qc_counts, file.path(pro_dir, "garmin_qc_counts.rds"))
}

message("01_Loading complete")