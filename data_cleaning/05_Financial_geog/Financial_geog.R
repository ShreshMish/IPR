##### Date of last edit:  09/08/2018
##### Code author:        Shyamal Patel
##### Description:        This script reads in TR financial data from Excel, and cleans the data before
#####                     further data cleaning takes place
##### Dependencies:       1.  Latest Thomson Reuters cleaned dataset: "1 - Financial prelim/Output/TR_cleaned_2016USD_data.xlsx"
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA

packages <- c("tidyverse", "magrittr", "readxl", "here", "stringi", "doParallel", "foreach")
lapply(packages, require, character.only = TRUE)
source(here::here("utils.R"))

# If enabled, modelling takes place on a reduced set
test_mode <- FALSE

# Read in source files with functions

source(here::here("05_Financial_geog/Financial_geog_data_cleaning.R"))
source(here::here("05_Financial_geog/Financial_geog_exposure_modelling.R"))
source(here::here("05_Financial_geog/Financial_geog_region_ISO_data_matching.R"))

# Read region ISO Code Data
region_iso <- 
  read_excel(path = path_to_data_file("05_Financial_geog/Input/ISO code and regional gdp.xlsx"), 
             sheet = "W3. Region ISO code table", range = "$A$8:$IC$212")

# Read ??
gdp_iso <- 
  read_excel(path = path_to_data_file("05_Financial_geog/Input/ISO code and regional gdp.xlsx"),
            sheet = "W4. GDP ISO code table", range = "$A$7:$C$211")

# Read LCP Regions
lcp_project_regions <- 
  read_excel(path = path_to_data_file("05_Financial_geog/Input/ISO code and regional gdp.xlsx"), 
             sheet = "W5. LCP region mapping",
             range = "$A$7:$F$211") %>%
  select(ISO_code, Country, LCP_Region)

# Read in Thomson Reuters cleaned dataset
financial_data <- readRDS(path_to_data_file("01_Financial_prelim/Output/TR_cleaned_2016USD_data.rds"))

#Read in ??
financial_domestic_rename <- read_csv(path_to_data_file("05_Financial_geog/Input/TR domestic geogs renamed.csv")) %>%
  select(-X1, -region_num, -region_revenue, -revenue)

# Calculate unique geographies and XXXX
clean_geog_data_results <- clean_financial_data(financial_data, financial_domestic_rename)

# Extract results from previous call
unique_geographies_data <- clean_geog_data_results$unique_geographies
tr_geographies_data <- clean_geog_data_results$TR_geographic_exposure_full_data


# Match ???
matched_geog_region_ISO_codes <- 
  match_geog_region_ISO_codes(unique_geographies_data, region_iso, gdp_iso) 

# Test inputs to financial_geog_exposure_modelling to see if they have changed since last run
# If not, don't run, as financial_geog_exposure_modelling is resource intensive

tr_geographies_data_has_changed_since_last_run <- 
  !file.exists(path_to_data_file("05_Financial_geog/Interim/TR_geographic_exposure_full_data.rds")) |
  !identical(tr_geographies_data, readRDS(path_to_data_file("05_Financial_geog/Interim/TR_geographic_exposure_full_data.rds")))

matched_geog_region_iso_code_data_has_changed_since_last_run <-
  !file.exists(path_to_data_file("05_Financial_geog/Interim/TR_mapped_geographies.rds")) |
  !identical(matched_geog_region_ISO_codes, readRDS(path_to_data_file("05_Financial_geog/Interim/TR_mapped_geographies.rds")))

gdp_iso_has_changed_since_last_run <-
  !file.exists(path_to_data_file("05_Financial_geog/Interim/GDP_ISO_code_data.rds")) |
  !identical(gdp_iso, readRDS(path_to_data_file("05_Financial_geog/Interim/GDP_ISO_code_data.rds")))

# Calculate company level exposure results. I cache inputs and see if they have changed

if(tr_geographies_data_has_changed_since_last_run | matched_geog_region_iso_code_data_has_changed_since_last_run |  gdp_iso_has_changed_since_last_run) {
  company_exposure_results <- financial_geog_exposure_modelling(tr_geographies_data, matched_geog_region_ISO_codes, gdp_iso, test_mode)
  
  company_exposure_results_wide <- company_exposure_results %>%
    select(ISIN_code:LCP_Region_revenue) %>%
    mutate(LCP_Region = paste0(LCP_Region, "_revenue")) %>%
    spread(key = "LCP_Region", value = "LCP_Region_revenue")
  
  temp <- company_exposure_results %>%
    select(ISIN_code, LCP_Region, LCP_Region_revenue_share) %>%
    mutate(LCP_Region = paste0(LCP_Region, "_revenue_share")) %>%
    spread(key = "LCP_Region", value = "LCP_Region_revenue_share")
  
  company_exposure_results_wide %<>%
    left_join(temp, by = "ISIN_code")

  if(!test_mode) {
    save_dated(company_exposure_results, "05_Financial_geog/Output/Company_geog_exposure_results", csv = TRUE)
    save_dated(company_exposure_results_wide, "05_Financial_geog/Output/Company_geog_exposure_results_wide", csv = TRUE)
  }
  
} else {
  warning("Not running financial_geog_exposure_modelling() as inputs haven't changed")
  
  #Resave RDS files so that timestamps gets updated.
  saveRDS(readRDS(path_to_data_file("05_Financial_geog/Output/Company_geog_exposure_results.rds")), path_to_data_file("05_Financial_geog/Output/Company_geog_exposure_results.rds"))
  saveRDS(readRDS(path_to_data_file("05_Financial_geog/Output/Company_geog_exposure_results_wide.rds")), path_to_data_file("05_Financial_geog/Output/Company_geog_exposure_results_wide"))
  
}

if(tr_geographies_data_has_changed_since_last_run) {
  save_dated(tr_geographies_data, "05_Financial_geog/Interim/TR_geographic_exposure_full_data")
}

if(matched_geog_region_iso_code_data_has_changed_since_last_run) {
  save_dated(matched_geog_region_ISO_codes, "05_Financial_geog/Interim/TR_mapped_geographies")
}

if(gdp_iso_has_changed_since_last_run) {
  save_dated(gdp_iso, "05_Financial_geog/Interim/GDP_ISO_code_data")
}

