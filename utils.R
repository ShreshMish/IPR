library(dotenv)
library(fs)

if(file.exists(here::here("configuration.conf"))) {
  load_dot_env(here::here("configuration.conf"))
}

# These functions count the number of missing or zero-value observations in a tibble
na_counter <- function(x) {sum(is.na(x))}
zero_counter <- function(x) {sum(ifelse(x == 0, 1, 0))}

# Path to data file -- either value of PATH_TO_DATA in configuration.conf
# or here::here()
path_to_data_file <- function(data_file) 
{
  path_to_data <- Sys.getenv("PATH_TO_DATA")
  if(path_to_data == "") {
    path_to_data = here::here();
  }

  return(fs::path(path_to_data, data_file))
}

# These functions save base, and dated files in the Interim or Outputs folder for later use
# Dated files are kept for version control purposes
save_dated <- function(data, path, dated = TRUE, csv = FALSE) {
  
  path_for_output <- dirname(path_to_data_file(path))
  path_for_dated_output <- fs::path(path_for_output, "Dated")
  
  # Save main file
  path_to_rds <- fs::path(path_for_output, paste0(basename(path), ".rds"))
  
  # Don't write out unless there have been changes
  saveRDS(data, path_to_rds)
  
  # Save dated file (optional)
  
  day <- format(Sys.time(), "%d")
  month <-format(Sys.time(), "%m")
  year <- substr(format(Sys.time(), "%Y"), 3, 4)
  date_prefix <- paste0(year, month, day, "_")
  
  if(dated == TRUE) {
    saveRDS(data, fs::path(path_for_dated_output, paste0(date_prefix, basename(path), ".rds")))
  }
  # Save dated CSV file (optional)
  if(csv == TRUE) {
    write_csv(data, fs::path(path_for_dated_output, paste0(date_prefix, basename(path), ".csv")))
  }
}
