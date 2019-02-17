##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  11/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script calculates market emissions intensities, and allocates emissions to 
#####                     subsidiaries, based on parent company emissions and the relative emissions intensities of
#####                     sectors to which the parent company is exposed
##### Dependencies:       1.  Results from CO2 emissions data cleaning: "3_ESG/3a_CO2_emissions/Output/Emissions_data.rds"
#####                     2.  Results from revised product exposure analysis: "4_Panel/Output/Rev_prod_exposure_results.rds"
#####                         Older files can be found in the ".../Dated/" folder
##### Notes:              Emissions intensity variation across regions is not accounted for here (results of geographic
#####                     exposure analysis are not used)

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "4_Panel"
source("utils.R")

# Read in Trucost emissions data
emissions_data <- readRDS("3_ESG/3a_CO2_emissions/Output/Emissions_data.rds")

# Read in Financials product exposure data
product_exposure_data <- readRDS("4_Panel/Output/Rev_prod_exposure_results.rds")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean datasets and merge together ----

# Select only variables of interest (year = 2017 for all observations)
emissions_data2 <- emissions_data %>%
  filter(year == 2017) %>%
  select(company_id, company, co2_scope_1, co2_scope_2, co2_scope_3)

product_exposure_data2 <- product_exposure_data %>%
  filter(year == 2017) %>%
  select(company_id, company, parent_market, revenue, market, product_revenue_share)

# Keep companies that have some emissions data
emissions_intensity_data <- product_exposure_data2 %>%
  left_join(emissions_data2, by = c("company_id", "company")) %>%
  filter(!(is.na(co2_scope_1) & is.na(co2_scope_1) & is.na(co2_scope_1))) %>%
  mutate_at(.vars = vars(starts_with("co2_scope_")),
            .funs = funs("intensity" = . / revenue))

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Median emissions intensity at market-level, calculated based on pure players whenever each
#####             market has at least 10 pure player companies (firms which are not conglomerates, that is, product_revenue_share = 1)

one_product_firm_count <- emissions_intensity_data %>%
  filter(product_revenue_share == 1) %>%
  group_by(market) %>% 
  mutate(market_status = case_when(n() >= 10 ~ "One product firms",
                                   TRUE ~ "All firms")) %>%
  select(market, market_status) %>%
  unique()

# Add back in markets which have no pure players (dropped out of one product firm count dataset)
market_status <- expand.grid(market = unique(product_exposure_data$market), stringsAsFactors = FALSE) %>%
  arrange(market) %>%
  left_join(one_product_firm_count, by = "market") %>%
  # Markets which are missing from the one product firm count dataset have no pure player firms at all
  mutate(market_status = case_when(is.na(market_status) ~ "All firms",
                                   TRUE ~ market_status))

save_dated(market_status, "Market_emissions_intensity_status", folder = "Interim", csv = TRUE)

# Note that there will be duplicated emissions intensity values when the same company is the median company
# across sectors (both sectors must of course be "All firms" rather than "One product firms")
emissions_intensity_data2 <- emissions_intensity_data %>%
  left_join(market_status, by = "market") %>%
  filter(market_status == "All firms" | market_status == "One product firms" & product_revenue_share == 1) %>%
  group_by(market) %>%
  mutate_at(.vars = vars(ends_with("intensity")),
            .funs = funs("median" = median(., na.rm = TRUE))) %>%
  select(market, market_status, ends_with("median")) %>%
  rename_at(.vars = vars(ends_with("median")),
            .funs = funs(paste0("median_co2_intensity_scope_", stri_extract_last_regex(., "[0-9]+")))) %>%
  unique()

save_dated(emissions_intensity_data2, "Market_emissions_intensities", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Fill in emissions data for all firms, in all markets, based on calculated
#####             market-level median CO2 emissions intensities

disaggregated_emissions_data <- product_exposure_data2 %>%
  left_join(emissions_data2, by = c("company_id", "company")) %>%
  left_join(emissions_intensity_data2, by = "market") %>%
  mutate(firm_status = case_when(product_revenue_share == 1 ~ "One product firm",
                                 TRUE ~ "Multi-product firm")) %>%
  select(company_id:co2_scope_3, firm_status, everything())

#### Four cases to consider here:
# 1. One product firm which has emissions data -> Do nothing
# 2. One product firm with no emissions data -> Solve as median emissions intensity * revenue
# 3. Multi-product firm which has emissions data -> Solve as company emissions * (median emissions intensity * revenue) / Sum(median emissions intensity * revenue, over markets)
# 4. Multi-product firm with no emissions data -> Solve as median emissions intensity * revenue

# Define function to apply operation for a given emissions scope (1-3)
emissions_estimation <- function(emissions_scope = NULL) {
  
  # New and existing variable names
  company_co2_var <- rlang::sym(paste0("co2_scope_", emissions_scope))
  market_co2_intensity_var <- rlang::sym(paste0("median_co2_intensity_scope_", emissions_scope))
  product_co2_var <- rlang::sym(paste0("product_co2_scope_", emissions_scope))
  
  disaggregated_emissions_data %<>%
    mutate(temp_var = revenue * product_revenue_share * !!market_co2_intensity_var) %>% #Temporary variable consisting of revenue * market-emissions intensity (emissions-by-market if there was no data)
    group_by(company_id) %>%
    mutate(temp_var = sum(temp_var),
           !!product_co2_var := case_when(firm_status == "One product firm" & !is.na(!!company_co2_var) ~ !!company_co2_var, # Case (1)
                                          firm_status == "Multi-product firm"& !is.na(!!company_co2_var) ~ !!company_co2_var * ((revenue * product_revenue_share * !!market_co2_intensity_var) / temp_var), # Case 3
                                          is.na(!!company_co2_var) ~ revenue * product_revenue_share * !!market_co2_intensity_var)) %>% # Cases (2) and (4)
    select(-temp_var)
  
  return(disaggregated_emissions_data)
}

# Apply over emissions scopes 1 - 3
emissions_scopes <- seq(1, 3, 1)
disaggregated_emissions_data2 <- map(emissions_scopes, emissions_estimation) %>%
  reduce(.f = left_join)

# Get rid of unnecessary variables
disaggregated_emissions_data3 <- disaggregated_emissions_data2 %>%
  select(company_id:co2_scope_3, firm_status, market_status, starts_with("product_co2_scope_"))

save_dated(disaggregated_emissions_data3, "Product_emissions_results", folder = "Output", csv = TRUE)