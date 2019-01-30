##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  24/01/2019
##### Code author:        Shyamal Patel / Mark Westcott
##### Description:        This script sets out useful functions and file paths called by other scripts within the data cleaning folder
##### Dependencies:       N/A

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Load common packages, set paths for save_dated functions within the Data cleaning folder, and define the save dated function

# Load packages (NB - parallel processing packages are not loaded here)
library(tidyverse)
library(magrittr)
library(readxl)
library(stringi)
library(themeVE)
library(ggrepel)
library(digest)
library(purrr)

# These functions save base, and dated files in the Interim or Outputs folder for later use
save_dated <- function(data, filename = NULL, folder = NULL, dated = TRUE, csv = FALSE) {
  
  # Check that a master folder for save files has been specified
  if(!exists("main_save_folder")) {
    print("No save folder specified - check utils file for details")
    stop()
  }
  
  # If no filename specified, default value is dataframe name
  if(is.null(filename)) {
    print("Filename set to df name by default")
    filename_rds <- paste0(deparse(substitute(data)), ".rds")
    filename_csv <- paste0(deparse(substitute(data)), ".csv")
  } else {
    filename_rds <- paste0(filename, ".rds")
    filename_csv <- paste0(filename, ".csv")
  }
  
  # If no folder specified, stop code and request
  if(is.null(folder)) {
    print("Save subfolder not specified - check utils file for details")
    stop()
  }
  
  # Construct main file path (right target folder)
  path_to_output <- fs::path(here::here(main_save_folder), folder)
  path_to_dated_output <- fs::path(here::here(main_save_folder), folder, "Dated")

  ### Save main file
  path_to_rds <- fs::path(path_to_output, filename_rds)
  path_to_checksum <- fs::path(path_to_output, "Checksum", paste0(filename_rds, ".txt"))
  
  # Only save results if new output file is different to the old (digest / checksum approach)
  checksum_test <- if(file.exists(path_to_checksum)) {readChar(path_to_checksum, file.info(path_to_checksum)$size)} else {NA_character_}
  
  if(is.na(checksum_test) | digest(data) != checksum_test) {
    saveRDS(data, path_to_rds)
    writeChar(digest(data), path_to_checksum, nchars = nchar(digest(data), type = "chars"))
    
    # Date format for dated files folder
    day <- format(Sys.time(), "%d")
    month <-format(Sys.time(), "%m")
    year <- substr(format(Sys.time(), "%Y"), 3, 4)
    hour <- substr(format(Sys.time(), "%H"), 1, 2)
    minute <- substr(format(Sys.time(), "%M"), 1, 2)
    date_time_prefix <- paste0(year, month, day, "_", hour, minute, "_")
    
    # Save dated rds file
    if(dated) {
      path_to_dated_rds <- fs::path(path_to_dated_output, paste0(date_time_prefix, filename_rds))
      saveRDS(data, path_to_dated_rds)
    }
    
    # Save dated csv file
    if(dated & csv) {
      path_to_dated_csv <- fs::path(path_to_dated_output, paste0(date_time_prefix, filename_csv))
      write_csv(data, path_to_dated_csv)
    }
  }
}

# Load input data based on save folder name
input_source <- function(filename) {
  fs::path(here::here(main_save_folder), "Input", filename)
}
  
#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Debugging functions

# These functions count the number of missing, zero, negative or unique value observations in a dataframe
na_counter <- function(x) {sum(is.na(x))}
zero_counter <- function(x) {sum(ifelse(x == 0, 1, 0))}
negative_counter <- function(x) {sum(ifelse(x < 0, 1,0))}
unique_counter <- function(x) {length(unique(x))}

# Custom view function (glimpse + View)
view <- function(x) {
  View(x)
  glimpse(x)
}
