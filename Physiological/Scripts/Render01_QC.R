r_libs_user <- Sys.getenv("R_LIBS_USER")
if (r_libs_user == "") stop("R_LIBS_USER is not set.")

.libPaths(c(r_libs_user, .libPaths()))

library(rmarkdown)
library(fs)

if (!rmarkdown::pandoc_available("1.12.3")) {
  stop("Pandoc >= 1.12.3 not available in PATH or RSTUDIO_PANDOC.")
}

message("Library path called and libraries loaded")

rmd_dir <- Sys.getenv("RMDDIR")
scripts_dir <- Sys.getenv("SCRIPTSDIR")

message("Directories set")

if (rmd_dir == "") stop("RMDDIR is not set.")
if (scripts_dir == "") stop("SCRIPTSDIR is not set.")

fs::dir_create(rmd_dir)

message("Created Rmd directory")

input_file <- file.path(scripts_dir, "RMD01_QC.Rmd")
if (!file.exists(input_file)) stop("Rmd file not found: ", input_file)

rmarkdown::render(
  input = input_file,
  output_dir = rmd_dir,
  output_file = "RMD01_QC.html",
  clean = TRUE,
  envir = new.env(parent = globalenv())
)