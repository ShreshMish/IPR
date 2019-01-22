##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  22/01/2019
##### Code author:        Shyamal Patel
##### Description:        This script refreshes the HSBC model data with new outputs from the Data cleaning folder
##### Dependencies:       Outputs from Data cleaning - ensure all file paths are still up-to-date

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA ----

packages <- c("tidyverse", "magrittr", "readxl", "here", "stringi")
lapply(packages, require, character.only = TRUE)

# Define date for save file names
day <- format(Sys.time(), "%d")
month <- match(format(Sys.time(), "%b"), month.abb)
if(nchar(month) == 1) {month <- paste0("0", month)}
year <- substr(format(Sys.time(), "%Y"), 3, 4)
date <- paste0(year, month, day)

# These functions count the number of missing or zero-value observations in a tibble
na_counter <- function(x) {sum(is.na(x))}
zero_counter <- function(x) {sum(ifelse(x == 0, 1, 0))}

# Assign location of data cleaning folder
data_cleaning <- "Data cleaning/"

# Assign location of models folder
models <- "Models/"

# Define function for copying files based on source and destination paths, dated option
copy_dated <- function(source_file, dest_folder, dest_file, dated = TRUE) {
  
  source_file_path <- paste0(data_cleaning, source_file, ".rds")
  dest_file_path <- paste0(models, dest_folder, "/", dest_file, ".rds")
  dated_dest_file_path <- paste0(models, dest_folder, "/Dated/", date, "_", dest_file, ".rds")
  dated_dest_csvfile_path <- paste0(models, dest_folder, "/Dated/", date, "_", dest_file, ".csv")

  file.copy(from = source_file_path, to = dest_file_path, overwrite = TRUE)
  
  # Save dated file (optional)
  if(dated) {file.copy(from = source_file_path, to = dated_dest_file_path, overwrite = TRUE)}
  
}

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Update fossil fuel demand destruction data files ----

# Define destination folder for fossil fuel demand destruction files
ff_dest_folder <- "1 - Demand destruction/Input"

# Results from alpha-factor adjustment (price_f (firm) = alpha * price_g (global))
copy_dated(source_file = "4 - Fossil fuels/Oil and gas/Output/DD_alpha_results_production_adjusted",
           dest_folder = ff_dest_folder,
           dest_file = "DD_alpha_results_production_adjusted")

# DD compact dataset for oil and gas supermajor calculations
copy_dated(source_file = "4 - Fossil fuels/Oil and gas/Output/DD_model_data_compact",
           dest_folder = ff_dest_folder,
           dest_file = "DD_model_data_compact")

# Oil and gas scenario data
copy_dated(source_file = "0 - Scenarios/Output/Fossil_fuel_production",
           dest_folder = ff_dest_folder,
           dest_file = "Fossil_fuel_production")

# Coal company-level data
copy_dated(source_file = "4 - Fossil fuels/Coal/Output/Company_coal_data_cleaned",
           dest_folder = ff_dest_folder,
           dest_file = "Coal_company_data_cleaned")

# Coal cost curve data
copy_dated(source_file = "4 - Fossil fuels/Coal/Output/Coal_cost_curve_full",
           dest_folder = ff_dest_folder,
           dest_file = "Coal_cost_curve_full")

# Coal scenario data (rename file to avoid confusion with oil and gas files)
copy_dated(source_file = "4 - Fossil fuels/Coal/Output/Scenario_quantities",
           dest_folder = ff_dest_folder,
           dest_file = "Coal_scenario_quantities")

# ICE vehicle scenario data
copy_dated(source_file = "0 - Scenarios/Output/ICE_new_capacity",
           dest_folder = ff_dest_folder,
           dest_file = "ICE_new_capacity")

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Update green upside data files ----

# Define destination folder for green upside files
gu_dest_folder <- "2 - Cleantech markets/Input"

# Results from green upside data cleaning
copy_dated(source_file = "5 - Green upside/Output/Combined_Green_Revenue_data",
           dest_folder = gu_dest_folder,
           dest_file = "Combined_Green_Revenue_data")

# Renewable capacity dataset
copy_dated(source_file = "0 - Scenarios/Output/Renewable_capacity",
           dest_folder = gu_dest_folder,
           dest_file = "Renewable_capacity")

# EV dataset
copy_dated(source_file = "0 - Scenarios/Output/EV_new_capacity",
           dest_folder = gu_dest_folder,
           dest_file = "EV_new_capacity")

# Biofuels dataset
copy_dated(source_file = "0 - Scenarios/Output/Biofuels_production",
           dest_folder = gu_dest_folder,
           dest_file = "Biofuels_production")

# Cleaned financial dataset
copy_dated(source_file = "1 - Financial prelim/Output/TR_cleaned_2016USD_data",
           dest_folder = gu_dest_folder,
           dest_file = "TR_cleaned_2016USD_data")

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Update carbon cost data files ----

# Define destination folder for carbon cost files
cc_dest_folder <- "3 - Cost and competition/Input"

# Cleaned financial and emissions panel dataset
copy_dated(source_file = "11 - Panel setting/Output/Model_panel",
           dest_folder = cc_dest_folder,
           dest_file = "Model_panel")

# Green upside reclassification dataset
copy_dated(source_file = "9 - Fossil and green upside prod/Output/Green_upside_reclassification",
           dest_folder = cc_dest_folder,
           dest_file = "Green_upside_reclassification")
