##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  15/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script refreshes the Net-zero Toolkit model data with new outputs from the Data cleaning folder
##### Dependencies:       Outputs from Data cleaning - ensure all file paths are still up-to-date

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA ----

source("utils.R")

# Define function for copying files based on source and destination paths
copy_dated <- function(source_folder, source_file, dest_folder, dest_file) {
  
  source_file_path <- fs::path(here::here(data_cleaning), source_folder, paste0(source_file, ".rds"))
  dest_file_path <- fs::path(here::here(models), dest_folder, paste0(dest_file, ".rds"))
    
  source_checksum_path <- fs::path(here::here(data_cleaning), source_folder, "Checksum", paste0(source_file, ".rds.txt"))
  dest_checksum_path <- fs::path(here::here(models), dest_folder, "Checksum", paste0(dest_file, ".rds.txt"))
  
  # Only overwrite existing input data if new data cleaning output file is different to the old (digest / checksum approach)
  checksum_source <- readChar(source_checksum_path, file.info(source_checksum_path)$size)
  checksum_dest <- if(file.exists(dest_checksum_path)) {readChar(dest_checksum_path, file.info(dest_checksum_path)$size)} else {NA_character_}

  if(is.na(checksum_dest) | checksum_source != checksum_dest) {
    file.copy(from = source_file_path, to = dest_file_path, overwrite = TRUE)
    writeChar(checksum_source, dest_checksum_path, nchars = nchar(checksum_source, type = "chars"))
    
    # Date format for dated files folder
    day <- format(Sys.time(), "%d")
    month <-format(Sys.time(), "%m")
    year <- substr(format(Sys.time(), "%Y"), 3, 4)
    hour <- substr(format(Sys.time(), "%H"), 1, 2)
    minute <- substr(format(Sys.time(), "%M"), 1, 2)
    date_time_prefix <- paste0(year, month, day, "_", hour, minute, "_")

    dest_dated_file_path <- fs::path(here::here(models), dest_folder, "Dated", paste0(date_time_prefix, dest_file, ".rds"))
    file.copy(from = source_file_path, to = dest_dated_file_path, overwrite = TRUE)
  }
}

# Assign location of data cleaning folder
data_cleaning <- "Data_cleaning_v2"

# Assign location of models folder
models <- "Models_v2"

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Update fossil fuel demand destruction data files ----

# Define destination folder for fossil fuel demand destruction files
dd_dest_folder <- "1_Demand_destruction/Input"

# Oil and gas company-level data
copy_dated(source_folder = "3_ESG/3b_Oil_and_gas/Output", source_file = "Model_prod_and_economic_data",
           dest_folder = dd_dest_folder, dest_file = "Oil_and_gas_company_data")

# Oil and gas company names data
copy_dated(source_folder = "3_ESG/3b_Oil_and_gas/Output", source_file = "Companies_list",
           dest_folder = dd_dest_folder, dest_file = "Oil_and_gas_company_names")

# Oil and gas scenario data
copy_dated(source_folder = "1_Scenarios/Output", source_file = "Fossil_fuel_production",
           dest_folder = dd_dest_folder, dest_file = "Oil_and_gas_scenario_data")

# Coal company-level data
copy_dated(source_folder = "3_ESG/3c_Coal/Output", source_file = "Model_reg_exposure_data",
           dest_folder = dd_dest_folder, dest_file = "Coal_company_data")

# Coal company names data
copy_dated(source_folder = "3_ESG/3c_Coal/Output", source_file = "Companies_list",
           dest_folder = dd_dest_folder, dest_file = "Coal_company_names")

# Coal cost curve data
copy_dated(source_folder = "3_ESG/3c_Coal/Output", source_file = "Seaborne_supply_curve",
           dest_folder = dd_dest_folder, dest_file = "Coal_cost_curve_data")

# Coal scenario data (rename file to avoid confusion with oil and gas files)
copy_dated(source_folder = "1_Scenarios/Output", source_file = "Fossil_fuel_production",
           dest_folder = dd_dest_folder, dest_file = "Coal_scenario_data")

# ICE vehicle scenario data
copy_dated(source_folder = "1_SCenarios/Output", source_file = "ICE_new_capacity",
           dest_folder = dd_dest_folder, dest_file = "ICE_scenario_data")

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Update cleantech markets data files ----

# Define destination folder for green upside files
cm_dest_folder <- "2_Cleantech_markets/Input"

# Results from cleantech markets company data cleaning
copy_dated(source_folder = "3_ESG/3d_Cleantech/Output", source_file = "Cleantech_patent_and_rev_data",
           dest_folder = cm_dest_folder, dest_file = "Cleantech_company_data")

# Results from financial data cleaning (not the full panel)
### TRY TO DELETE THIS DEPENDENCY IF POSSIBLE
copy_dated(source_folder = "2_Financial/2a_Preliminary/Output", source_file = "Companies_2016USD_data",
           dest_folder = cm_dest_folder, dest_file = "Financial_company_data")

# Results from scenarios analysis for renewable capacity
copy_dated(source_folder = "1_Scenarios/Output", source_file = "Renewable_capacity",
           dest_folder = cm_dest_folder, dest_file = "Renewables_scenario_data")

# Results from scenarios analysis for EV capacity
copy_dated(source_folder = "1_Scenarios/Output", source_file = "EV_new_capacity",
           dest_folder = cm_dest_folder, dest_file = "EVs_scenario_data")

# Biofuels dataset
copy_dated(source_folder = "1_Scenarios/Output", source_file = "Biofuels_production",
           dest_folder = cm_dest_folder, dest_file = "Biofuels_scenario_data")

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Update carbon cost data files ----

# Define destination folder for carbon cost files
cc_dest_folder <- "3_Cost_and_competition/Input"

# Cleaned financial and emissions panel dataset
copy_dated(source_folder = "4_Panel/Output", source_file = "Model_panel_final",
           dest_folder = cc_dest_folder, dest_file = "Model_panel")

# Cleaned carbon cost curves dataset
copy_dated(source_folder = "4_Panel/Output", source_file = "Carbon_cost_curves",
           dest_folder = cc_dest_folder, dest_file = "Carbon_cost_curves")