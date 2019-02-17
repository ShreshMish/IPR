##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  16/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script collects together all datasets for read in by the run model scripts
#####                     in the carbon cost model folder
##### Dependencies:       1.  Results from CO2 emissions data cleaning: "2 - CO2 emissions/Output/Trucost_cleaned_emissions_data.rds"
#####                         Older files can be found in the ".../Dated/" folder
#####                     2.  Results from Financial data product exposure analysis: "8 - Fossil and green upside prod/Output/Market_exposure_results_oilgascoalgreen.rds"
##### Notes:              Emissions intensity variation across regions is not accounted for here (results of geographic
#####                     exposure analysis are not used)

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "4_Panel"
source("utils.R")

# Read in Financials main data
financial_data <- readRDS("2_Financial/2a_Preliminary/Output/Companies_2016USD_data.rds")

# Read in Financials geographic exposure data
geog_exposure_data <- readRDS("2_Financial/2b_Geographic_exposure/Output/Geog_exposure_results.rds")

# Read in Financials product exposure data
product_exposure_data <- readRDS("4_Panel/Output/Rev_prod_exposure_results.rds")

# Read in market parameter data
market_parameter_data <- readRDS("0_Background/Output/Market_parameter_data.rds")

# Read in emissions-by-product data
product_emissions_data <- readRDS("4_Panel/Output/Product_emissions_results.rds")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Simplify datasets to minimise risk of join / merge issues ----

# Select down to minimal set of variables: financial prelim dataset
financial_data2 <- financial_data %>%
  select(company_id, company, company_market_cap, revenue, net_income, corporation_tax_rate)

# Select down to minimal set of variables: financial geographic exposure dataset
geog_exposure_data2 <- geog_exposure_data %>%
  select(company_id, company, NZT_region, NZT_region_revenue_share) %>%
  rename(region = NZT_region,
         region_revenue_share = NZT_region_revenue_share) %>%
  # Bind regions where a company has no revenue exposure
  filter(region_revenue_share > 0)

# Select down to minimal set of variables: financial product exposure dataset
product_exposure_data2 <- product_exposure_data %>%
  select(company_id, company, parent_market, market, product_revenue_share)

# Select down to minimal set of variables: emissions-by-product data
product_emissions_data2 <- product_emissions_data %>%
  select(company_id, company, parent_market, market, starts_with("co2_scope_"), starts_with("product_co2_scope_")) %>%
  rename_at(.vars = vars(starts_with("co2_scope_")),
            .funs = funs(paste0("parent_co2_scope_", stri_extract_last_regex(., "[0-9]+")))) %>%
  rename_at(.vars = vars(starts_with("product_co2_scope")),
            .funs = funs(paste0("co2_scope_", stri_extract_last_regex(., "[0-9]+"))))

# Select down to minimal set of variables: carbon cost data
carbon_cost_data <- market_parameter_data %>% 
  select(region, market, year, carbon_price_sector, mac_curve_sector, lever, abatement_cost, abatement_potential, mean_abatement_cost, cum_abatement_potential)

# Select down to minimal set of variables: market parameter data
market_parameter_data2 <- market_parameter_data %>%
  select(region, market, year, elasticity, product_differentiation) %>%
  # Elasticity and product differentiation parameters do not vary over time
  filter(year == 2017) %>%
  select(-year) %>%
  # Unique entries (duplicates owing to abatement levers)
  unique()

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Carbon cost calculations ----

carbon_cost_data2 <- carbon_cost_data %>%
  # Fill in MAC curve entries for no MAC curve sectoros (lever = abatement_cost = abatement_potential = NA in all years)
  mutate(lever = case_when(is.na(lever) ~ "Not applicable",
                           TRUE ~ lever)) %>%
  mutate_at(.vars = vars(abatement_cost, abatement_potential, mean_abatement_cost, cum_abatement_potential),
            .funs = funs(case_when(is.na(.) ~ 0,
                                   TRUE ~ .)))

# Arrange carbon cost data in descending order of abatement cost (for given scenario, region, market, year) - this is an important step as the code is index-based
carbon_cost_data3 <- carbon_cost_data2 %>%
  group_by(region, market, year) %>%
  arrange(region, market, year, abatement_cost) %>%
  ungroup()

# Cap negative cost entries to 0 (do not allow for negative abatement costs - these are economical at 0 carbon prices)
carbon_cost_data4 <- carbon_cost_data3 %>%
  mutate(abatement_cost = ifelse(abatement_cost < 0, 0, abatement_cost))

# Recalculate mean abatement costs based on capped abatement costs
carbon_cost_data5 <- carbon_cost_data4 %>%
  group_by(region, market, year) %>%
  arrange(region, market, year, cum_abatement_potential, abatement_cost) %>%
  mutate(mean_abatement_cost = case_when(cum_abatement_potential == 0 ~ 0,
                                         
                                         #### INTRODUCED ERROR TO MAINTAIN CONSISTENCY WITH OLD CODE
                                         #### DENOMINATOR SHOULD BE CUM_ABATEMENT_POTENTIAL
                                         TRUE ~ cumsum(abatement_cost * abatement_potential) / 1)) %>%
  ungroup()

# Save carbon costs dataset
save_dated(carbon_cost_data5, "Carbon_cost_curves", folder = "Output", csv = FALSE)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Merge together datasets and create full model panel ----

model_panel <- financial_data2 %>%
  rename(market_cap = company_market_cap) %>%
  rename_at(vars(market_cap, revenue, net_income), funs(paste0("parent_", ., "_2017")))

# Product exposure first
model_panel2 <- model_panel %>%
  left_join(product_exposure_data2, by = c("company_id", "company")) %>%
  # Calculate market cap, revenue and profit at the subsidiary-level using product_revenue_share
  mutate(market_cap_2017 = parent_market_cap_2017 * product_revenue_share,
         revenue_2017 = parent_revenue_2017 * product_revenue_share,
         net_income_2017 = parent_net_income_2017 * product_revenue_share) %>%
  # Merge in emissions dataset (already calculated at the subsidiary-level)
  left_join(product_emissions_data2, by = c("company_id", "company", "parent_market", "market")) %>%
  rename_at(vars(starts_with("co2_scope_")), funs(paste0(., "_2017"))) %>%
  select(company_id:company, starts_with("parent"), everything())

# Geographic exposure next (combinations with product exposure / subsidiaries analysis)
model_panel3 <- model_panel2 %>%
  left_join(geog_exposure_data2, by = c("company_id", "company")) %>%
  mutate_at(vars(market_cap_2017, revenue_2017, net_income_2017, starts_with("co2_scope_")),
            funs(. * region_revenue_share)) %>%
  select(company_id:parent_co2_scope_3, corporation_tax_rate, market, product_revenue_share, region,
         region_revenue_share, everything()) %>%
  arrange(company_id, market, region)

# Elasticity and product differentiation parameters
model_panel4 <- model_panel3 %>%
  left_join(market_parameter_data2, by = c("market", "region"))

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Corporation tax missing values ----

# Cap corporation tax rates at 39 (highest global rate, excluding UAE which is an outlier at 55%)
# https://taxfoundation.org/corporate-income-tax-rates-around-world-2016/
model_panel5 <- model_panel4 %>%
  mutate(corporation_tax_rate = case_when(corporation_tax_rate >= 39 ~ 39,
                                          TRUE ~ corporation_tax_rate))

# Fill in corporation tax variable using existing approach (average for each market when not available)
# Drop this if variable does not matter based on later analysis
market_corporation_tax_rates <- model_panel5 %>% 
  select(company_id, market, corporation_tax_rate) %>% 
  # Keep one entry per subsidiary (drop regions dimension)
  unique() %>%
  group_by(market) %>% 
  summarise(corporation_tax_rate = mean(corporation_tax_rate, na.rm = TRUE)) %>%
  ungroup()

vec_market_corporation_tax_rates <- as.vector(market_corporation_tax_rates$corporation_tax_rate)
names(vec_market_corporation_tax_rates) <- market_corporation_tax_rates$market

# Fill over 5 markets missing values
market_corporation_tax_rates2 <- market_corporation_tax_rates %>%
  mutate(corporation_tax_rate = case_when(market == "Aluminum mining" ~ vec_market_corporation_tax_rates["Aluminum"],
                                          market == "Diamonds & Gemstones" ~ vec_market_corporation_tax_rates["Gold Mining"],
                                          market == "Iron ore mining" ~ vec_market_corporation_tax_rates["Iron & Steel"],
                                          market == "Mortgage REITs" ~ vec_market_corporation_tax_rates["Ind. & Office REITs"],
                                          market == "Plat.& Precious Metal" ~ vec_market_corporation_tax_rates["Gold Mining"],
                                          TRUE ~ corporation_tax_rate)) %>%
  rename(market_corporation_tax_rate = corporation_tax_rate)

# Fill blanks with industry average corporation tax rate
model_panel6 <- model_panel5 %>%
  left_join(market_corporation_tax_rates2, by = "market") %>%
  mutate(corporation_tax_rate = case_when(is.na(corporation_tax_rate) ~ market_corporation_tax_rate,
                                          TRUE ~ corporation_tax_rate)) %>%
  group_by(company_id) %>%
  # Take average over product categories for each ISIN code (company) - each company should have one corporation tax rate
  mutate(corporation_tax_rate = mean(corporation_tax_rate)) %>%
  ungroup()

save_dated(model_panel6, "Model_panel_final", folder = "Output", csv = FALSE)