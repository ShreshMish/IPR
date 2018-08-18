##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  16/08/2018
##### Code author:        Shyamal Patel
##### Description:        This script collects together all datasets for read in by the run model scripts
#####                     in the carbon cost model folder
##### Dependencies:       1.  Results from CO2 emissions data cleaning: "2 - CO2 emissions/Output/Trucost_cleaned_emissions_data.rds"
#####                         Older files can be found in the ".../Dated/" folder
#####                     2.  Results from Financial data product exposure analysis: "8 - Fossil fuel prod/Output/Market_exposure_results_oilandgas.rds"
##### Notes:              Emissions intensity variation across regions is not accounted for here (results of geographic
#####                     exposure analysis are not used)

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA

packages <- c("tidyverse", "magrittr", "readxl", "here", "stringi")
lapply(packages, require, character.only = TRUE)
source(here::here("utils.R"))

# Read in Financials main data
financial_data <- readRDS(path_to_data_file("01_Financial_prelim/Output/TR_Cleaned_2016USD_data.rds"))

# Read in Financials geographic exposure data
geographic_exposure_data <- readRDS(path_to_data_file("05_Financial_geog/Output/Company_geog_exposure_results.rds"))

# Read in Financials product exposure data
product_exposure_data <- readRDS(path_to_data_file("08_Fossil_fuel_prod/Output/Market_exposure_results_oilandgas.rds"))

# Read in market parameter data
market_parameter_data <- readRDS(path_to_data_file("07_Market_parameters/Output/Market_parameter_data.rds"))

# Read in emissions-by-product data
product_emissions_data <- readRDS(path_to_data_file("09_Emissions_intensity/Output/Product_level_emissions_data.rds"))

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Simplify datasets to minimise risk of join / merge issues

# Select down to minimal set of variables: financial prelim dataset
financial_data %<>%
  select(ticker, ISIN_code, company, country_of_listing, market_cap, revenue, profit, corporation_tax_rate)
  
# Select down to minimal set of variables: financial geographic exposure dataset
geographic_exposure_data %<>%
  select(ISIN_code, company, LCP_Region, LCP_Region_revenue_share) %>%
  rename(region = LCP_Region,
         region_revenue_share = LCP_Region_revenue_share)

# Select down to minimal set of variables: financial product exposure dataset
product_exposure_data %<>%
  select(ISIN_code, company, parent_market, market, product_revenue_share)

# Select down to minimal set of variables: emissions-by-product data
product_emissions_data %<>%
  select(ISIN_code, company, parent_market, market, starts_with("co2_emissions_scope"), starts_with("product_co2_emissions_scope_")) %>%
  rename_at(.vars = vars(starts_with("co2_emissions_scope")),
            .funs = funs(paste0("parent_co2_emissions_scope_", stri_extract_last_regex(., "[0-9]+")))) %>%
  rename_at(.vars = vars(starts_with("product_co2_emissions_scope")),
            .funs = funs(paste0("co2_emissions_scope_", stri_extract_last_regex(., "[0-9]+"))))

# Select down to minimal set of variables: carbon cost data
carbon_cost_data <- market_parameter_data %>% 
  select(scenario, region, market, year, lever, carbon_price, abatement_cost, abatement_potential, mean_abatement_cost, cum_abatement_potential)

# Select down to minimal set of variables: market parameter data
market_parameter_data %<>%
  select(scenario, region, year, market, elasticity, product_differentiation) %>%
  # Elasticity and product differentiation parameters do not vary over time
  filter(year == 2017) %>%
  select(-year) %>%
  # Unique entries (duplicates owing to abatement levers)
  unique()

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Carbon cost calculations

carbon_cost_data %<>%
  # Fill in MAC curve entries for no MAC curve sectoros (lever = abatement_cost = abatement_potential = NA in all years)
  mutate(lever = case_when(is.na(lever) ~ "Not applicable",
                           TRUE ~ lever)) %>%
  mutate_at(.vars = vars(abatement_cost, abatement_potential, mean_abatement_cost, cum_abatement_potential),
            .funs = funs(case_when(is.na(.) ~ 0,
                                   TRUE ~ .)))

# Arrange carbon cost data in descending order of abatemnet cost (for given scenario, region, market, year) - this is an important step as the code is index-based
carbon_cost_data %<>%
  group_by(scenario, region, market, year) %>%
  arrange(scenario, region, market, year, abatement_cost)

# Cap negative cost entries to 0 (do not allow for these)
carbon_cost_data %<>%
  mutate(abatement_cost = case_when(abatement_cost < 0 ~ 0,
                                    TRUE ~ abatement_cost),
         lever_cost_saving = case_when(carbon_price >= abatement_cost ~ 1,
                                       TRUE ~ NA_real_),
         cost_saving_entry_num = cumsum(lever_cost_saving),
         mean_abatement_cost = case_when(cumsum(abatement_potential) != 0 ~ cumsum(abatement_cost * abatement_potential),
                                         TRUE ~ 0),
         actual_abatement = case_when(max(cost_saving_entry_num, na.rm = TRUE) == -Inf ~ 0, # Case where no abatement options are used
                                      cost_saving_entry_num == max(cost_saving_entry_num, na.rm = TRUE) ~ cum_abatement_potential, #Marginal abatement cost entry
                                      TRUE ~ NA_real_),
         actual_abatement_cost = case_when(max(cost_saving_entry_num, na.rm = TRUE) == -Inf ~ 0, # Case where no abatement options are used
                                           cost_saving_entry_num == max(cost_saving_entry_num, na.rm = TRUE) ~ mean_abatement_cost,
                                           TRUE ~ NA_real_),
         carbon_cost = carbon_price * (1 - actual_abatement) + actual_abatement_cost * actual_abatement) %>%
  ungroup() %>%
  # Keep just the marginal abatement cost lever
  filter(!is.na(carbon_cost))

# Save full dataset (levers for cross-checking later - RDS only)
save_dated(carbon_cost_data, "10_Panel_setting/Interim/Carbon_cost_calculations_full", csv = TRUE)

# Select relevant variables for later
carbon_cost_data %<>%
  select(scenario:year, carbon_price, carbon_cost) %>%
  # Necessary as scenarios with zero carbon prices lead to 1's in all rows for the market, year, region, scenario combination
  unique()

# Rearrange to wide format
carbon_price_data <- carbon_cost_data %>%
  select(scenario:year, carbon_price) %>% 
  spread(key = "year", value = "carbon_price") %>%
  rename_at(.vars = vars(`2017`:`2050`), .funs = funs(paste0("carbon_price_", .)))

carbon_cost_data %<>%
  select(scenario:year, carbon_cost) %>%
  spread(key = "year", value = "carbon_cost") %>%
  rename_at(.vars = vars(`2017`:`2050`), .funs = funs(paste0("carbon_cost_", .)))

carbon_cost_data %<>%
  left_join(carbon_price_data) %>%
  select(scenario:market, starts_with("carbon_price_"), everything())

# Save wide dataset for modelling
save_dated(carbon_cost_data, "10_Panel_setting/Output/Carbon_costs", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Merge together datasets and create full model panel

model_panel <- financial_data %>%
  rename_at(.vars = vars(market_cap, revenue, profit), .funs = funs(paste0("parent_", ., "_2017")))

# Product exposure first
model_panel %<>%
  left_join(product_exposure_data) %>%
  # Calculate market cap, revenue and profit at the subsidiary-level using product_revenue_share
  mutate(market_cap_2017 = parent_market_cap_2017 * product_revenue_share,
         revenue_2017 = parent_revenue_2017 * product_revenue_share,
         profit_2017 = parent_profit_2017 * product_revenue_share) %>%
  # Merge in emissions dataset (already calculated at the subsidiary-level)
  left_join(product_emissions_data) %>%
  rename_at(.vars = vars(starts_with("co2_emissions_scope_")), .funs = funs(paste0(., "_2017"))) %>%
  select(ticker:country_of_listing, starts_with("parent"), everything())

# Geographic exposure next (combinations with product exposure / subsidiaries analysis)
model_panel %<>%
  left_join(geographic_exposure_data) %>%
  mutate_at(.vars = vars(market_cap_2017, revenue_2017, profit_2017, starts_with("co2_emissions_scope_")),
            .funs = funs(. * region_revenue_share)) %>%
  select(ticker:parent_co2_emissions_scope_3, corporation_tax_rate, market, product_revenue_share, region,
         region_revenue_share, everything()) %>%
  arrange(ISIN_code, market, region)

# Carbon costs (for the representative ton)
model_panel %<>%
  left_join(carbon_cost_data) %>%
  select(scenario, ticker:co2_emissions_scope_3_2017, everything())

# Elasticity and product differentiation parameters
model_panel %<>%
  left_join(market_parameter_data)

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Fill corporation tax missing values

# Cap corporation tax rates at 39 (highest global rate, excluding UAE which is an outlier at 55%)
# https://taxfoundation.org/corporate-income-tax-rates-around-world-2016/
model_panel %<>%
  mutate(corporation_tax_rate = case_when(corporation_tax_rate >= 39 ~ 39,
                                             TRUE ~ corporation_tax_rate))

# Fill in corporation tax variable using existing approach (average for each market when not available)
# Drop this if variable does not matter based on later analysis
market_corporation_tax_rates <- model_panel %>% 
  select(ISIN_code, market, corporation_tax_rate) %>% 
  # Keep one entry per subsidiary
  unique() %>%
  group_by(market) %>% 
  summarise(corporation_tax_rate = mean(corporation_tax_rate, na.rm = TRUE)) %>%
  ungroup()

vec_market_corporation_tax_rates <- as.vector(market_corporation_tax_rates$corporation_tax_rate)
names(vec_market_corporation_tax_rates) <- market_corporation_tax_rates$market

# Fill over 5 markets missing values
market_corporation_tax_rates %<>%
  mutate(corporation_tax_rate = case_when(market == "Aluminum mining" ~ vec_market_corporation_tax_rates["Aluminum"],
                                          market == "Diamonds & Gemstones" ~ vec_market_corporation_tax_rates["Gold Mining"],
                                          market == "Iron ore mining" ~ vec_market_corporation_tax_rates["Iron & Steel"],
                                          market == "Mortgage REITs" ~ vec_market_corporation_tax_rates["Ind. & Office REITs"],
                                          market == "Plat.& Precious Metal" ~ vec_market_corporation_tax_rates["Gold Mining"],
                                          TRUE ~ corporation_tax_rate)) %>%
  rename(market_corporation_tax_rate = corporation_tax_rate)

# Fill blanks with industry average corporation tax rate
model_panel %<>%
  left_join(market_corporation_tax_rates) %>%
  mutate(corporation_tax_rate = case_when(is.na(corporation_tax_rate) ~ market_corporation_tax_rate,
                                          TRUE ~ corporation_tax_rate)) %>%
  group_by(ISIN_code) %>%
  # Take average over product categories for each ISIN code (company) - each company should have one corporation tax rate
  mutate(corporation_tax_rate = mean(corporation_tax_rate)) %>%
  ungroup()

#--------------------------------------------------------------------------------------------------

##### SECTION 6 - Create empty variables for the direct carbon costs model

# Lengthen and create variables required for the full model
#names_to_lengthen <- c("revenue_BAU_quantity", "sector_revenue_BAU_quantity", "revenue_actual_quantity", "sector_revenue_actual_quantity",
#                       "total_cost_actual_quantity", "sector_total_cost_BAU_quantity", "sector_total_cost_actual_quantity",
#                       "total_cost_BAU_quantity", "co2_emissions_scope_1_BAU_quantity", "sector_co2_emissions_scope_1_BAU_quantity",
#                       "sector_profit_post_closure_pre_tax", "a_term", "b_term", "number_firms", "c_term", "industry_cost_pass_through",
#                       "price", "d_relative_co2_intensity", "rho_cost_pass_through", "e_sales_impact",
#                       "f_margin_impact", "profit_margin", "sector_profit_margin", "quantity_pre_closure", "sector_quantity_pre_closure",
#                       "quantity_reallocated", "sector_quantity_reallocated", "quantity_post_closure", "sector_quantity_post_closure",
#                       "market_share_post_closure", "profit_pre_closure_pre_tax", "profit_post_closure_pre_tax", "profit_npv_pre_tax")

#years <- c(2018:2050)

#for(j in seq_along(names_to_lengthen)) {
#  for(i in years) {
#    varname = rlang::sym(paste0(names_to_lengthen[[j]], "_", i))
#    model_panel %<>%
#      mutate(!!varname := NA)
#  }
#}

#rm(list = c("names_to_lengthen", "varname", "i", "j"))

save_dated(model_panel, "10_Panel_setting/Output/Model_panel", csv = TRUE)