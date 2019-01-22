##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  29/08/2018
##### Code author:        Shyamal Patel
##### Description:        This script calculates market emissions intensities, and allocates emissions to 
#####                     subsidiaries, based on parent company emissions and the relative emissions intensities of
#####                     sectors to which the parent company is exposed
##### Dependencies:       1.  Results from CO2 emissions data cleaning: "2 - CO2 emissions/Output/Trucost_cleaned_emissions_data.rds"
#####                         Older files can be found in the ".../Dated/" folder
#####                     2.  Results from Fossil fuel prod data product exposure analysis: "8 - Fossil and green upside prod/Output/Market_exposure_results_oilgascoalgreen.rds"
##### Notes:              Emissions intensity variation across regions is not accounted for here (results of geographic
#####                     exposure analysis are not used)

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA

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
zero_counter <- function(x) {sum(ifelse(x == 0, 1, 0), na.rm = TRUE)}

# These functions save base, and dated files in the Interim or Outputs folder for later use
# Dated files are kept for version control purposes
save_dated <- function(data, name = " ", folder, dated = "YES", csv = "NO") {
  main_path <- paste0("10 - Emissions intensity/", folder)
  dated_path <- paste0("10 - Emissions intensity/", folder, "/Dated/")
  
  if(name == " ") {name <- gsub("_", " ", deparse(substitute(data)))}
  
  # Save main file
  saveRDS(data, here(main_path, paste0(name, ".rds")))
  # Save dated file (optional)
  if(dated == "YES") {saveRDS(data, here(dated_path, paste0(date, "_", name, ".rds")))}
  # Save dated CSV file (optional)
  if(csv == "YES") {write_csv(data, here(dated_path, paste0(date, "_", name, ".csv")))}
}

# Read in Trucost emissions data
emissions_data <- readRDS("2 - CO2 emissions/Output/Trucost_cleaned_emissions_data.rds")

# Read in Financials product exposure data
product_exposure_data <- readRDS("9 - Fossil and green upside prod/Output/Market_exposure_results_oilgascoalgreen.rds")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean datasets, and merge together

# Select only variables of interest (year = 2017 for all observations)
emissions_data %<>%
  select(company, ISIN_code, co2_emissions_scope_1, co2_emissions_scope_2, co2_emissions_scope_3)

product_exposure_data %<>%
  select(ISIN_code, company, parent_market, revenue, market, product_revenue_share)

emissions_intensity_data <- product_exposure_data %>%
  left_join(emissions_data) %>%
  filter(!(is.na(co2_emissions_scope_1) & is.na(co2_emissions_scope_2) & is.na(co2_emissions_scope_3))) %>%
  mutate_at(.vars = vars(starts_with("co2_emissions_scope_")),
            .funs = funs("intensity" = . / revenue))

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Median emissions intensity at market-level, calculated based on pure players whenever each
#####             market has at least 10 pure player companies (that is, companies which are not conglomerates (product_revenue_share = 1))

one_product_firm_count <- emissions_intensity_data %>%
  filter(product_revenue_share == 1) %>%
  group_by(market) %>% 
  mutate(market_status = case_when(n() >= 10 ~ "One product firms",
                                   TRUE ~ "All firms")) %>%
  select(market, market_status) %>%
  unique()

market_status <- as.tibble(product_exposure_data$market) %>%
  rename(market = value) %>%
  unique() %>%
  arrange(market) %>%
  left_join(one_product_firm_count) %>%
  # Companies which are missing from the one product firm count dataset have no pure player firms at all
  mutate(market_status = case_when(is.na(market_status) ~ "All firms",
                                   TRUE ~ market_status))

save_dated(market_status, "Market emissions intensity status", folder = "Interim", csv = "YES")

# Note that there will be duplicated emissions intensity values when the same company is the median company
# across sectors (both sectors must of course be "All firms" rather than "One product firms")
market_emissions_intensity_data <- emissions_intensity_data %>%
  left_join(market_status) %>%
  filter(market_status == "All firms" | market_status == "One product firms" & product_revenue_share == 1) %>%
  group_by(market) %>%
  mutate_at(.vars = vars(ends_with("intensity")),
            .funs = funs("market" = median(., na.rm = TRUE))) %>%
  select(market, market_status, ends_with("intensity_market")) %>%
  rename_at(.vars = vars(ends_with("intensity_market")),
            .funs = funs(paste0("market_co2_emissions_intensity_scope_", stri_extract_last_regex(., "[0-9]+")))) %>%
  unique()

save_dated(market_emissions_intensity_data, "Market_emissions_intensities", folder = "Output", csv = "YES")

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Fill in emissions data for all firms, in all markets, based on calculated
#####             market-level median CO2 emissions intensities

disaggregated_emissions_data <- product_exposure_data %>%
  left_join(emissions_data) %>%
  left_join(market_emissions_intensity_data) %>%
  mutate(firm_status = case_when(product_revenue_share == 1 ~ "One product firm",
                                 TRUE ~ "Multi-product firm")) %>%
  select(ISIN_code:co2_emissions_scope_3, firm_status, everything())

#### Four cases to consider here:
# 1. One product firm which has emissions data -> Do nothing
# 2. One product firm with no emissions data -> Solve as median emissions intensity * revenue
# 3. Multi-product firm which has emissions data -> Solve as company emissions * (median emissions intensity * revenue) / Sum(median emissions intensity * revenue, over markets)
# 4. Multi-product firm with no emissions data -> Solve as median emissions intensity * revenue

# Define function to apply operation for a given emissions scope (1-3)
emissions_estimation <- function(emissions_scope = NULL) {
  
  # New and existing variable names
  company_co2_var <- rlang::sym(paste0("co2_emissions_scope_", emissions_scope))
  market_co2_intensity_var <- rlang::sym(paste0("market_co2_emissions_intensity_scope_", emissions_scope))
  product_co2_var <- rlang::sym(paste0("product_co2_emissions_scope_", emissions_scope))
  
  disaggregated_emissions_data %<>%
    mutate(temp_var = revenue * product_revenue_share * !!market_co2_intensity_var) %>% #Temporary variable consisting of revenue * market-emissions intensity (emissions-by-market if there was no data)
    group_by(ISIN_code) %>%
    mutate(temp_var = sum(temp_var),
           !!product_co2_var := case_when(firm_status == "One product firm" & !is.na(!!company_co2_var) ~ !!company_co2_var, # Case (1)
                                          firm_status == "Multi-product firm"& !is.na(!!company_co2_var) ~ !!company_co2_var * ((revenue * product_revenue_share * !!market_co2_intensity_var) / temp_var), # Case 3
                                          is.na(!!company_co2_var) ~ revenue * product_revenue_share * !!market_co2_intensity_var)) %>% # Cases (2) and (4)
    select(-temp_var)
  
  return(disaggregated_emissions_data)
}

# Apply over emissions scopes 1 - 3
emissions_scopes <- seq(1, 3, 1)
disaggregated_emissions_estimation_data_list <- map(emissions_scopes, emissions_estimation)
disaggregated_emissions_data <- disaggregated_emissions_estimation_data_list[[1]] %>%
  left_join(disaggregated_emissions_estimation_data_list[[2]]) %>%
  left_join(disaggregated_emissions_estimation_data_list[[3]])

# Get rid of unnecessary variables
disaggregated_emissions_data %<>%
  select(ISIN_code:co2_emissions_scope_3, firm_status, market_status, starts_with("product_co2_emissions_"))

save_dated(disaggregated_emissions_data, "Product_level_emissions_data", folder = "Output", csv = "YES")
