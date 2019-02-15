##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  14/02/2019
##### Code author:        Justine Schafer
##### Edited by:          Shyamal Patel
##### Description:        This script imports cleaned Rystad data and scenario analysis data for demand destruction analysis
##### Dependencies:       1.  Oil and gas company-level cleaned data
#####                     2.  Oil and gas scenario output data
#####                     3.  Rystad company names matched to Vivid unique company IDs / names
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "1_Demand_destruction"
source("utils.R")

# Read in oil & gas company-level data
company_data <- readRDS(input_source("Oil_and_gas_company_data.rds"))

# Read in oil & gas scenario output data
scenario_data <- readRDS(input_source("Oil_and_gas_scenario_data.rds"))

# Read in oil & gas company name matching results (Rystad names to Vivid names)
company_names <- readRDS(input_source("Oil_and_gas_company_names.rds"))

# Discount rate should be 0.0575 (2% inflation rate adjustment does not apply)
discount_rate <- 0.0575

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Preliminary changes to company-level data ----

company_data2 <- company_data %>%
  select(-alpha, -price_f, -alpha_unit_cost_f, -production_f) %>%
  # Replace firm-level price with alpha-adjusted firm-level price, unit cost with alpha adjusted unit cost (no real changes)
  # Replace production f with production F adjusted
  rename(price_f = alpha_price_f,
         production_f = production_f_adj)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Interpolate Vivid scenario data prices, unit costs and profits ----

scenario_data2 <- scenario_data %>%
  rename(product = fuel,
         production_g = production) %>%
  filter(product != "coal") %>%
  mutate(product = case_when(product == "gas" ~ "Gas",
                             product == "oil" ~ "Liquid",
                             TRUE ~ NA_character_)) %>%
  select(product, everything(), -units)

company_data3 <- company_data2 %>%
  mutate(year = as.numeric(year)) %>%
  # Remove observations for which production_f and price_f is zero
  filter(!is.na(production_f) & !is.na(price_f))

company_data4 <- expand.grid(rystad_name = unique(company_data3$rystad_name),
                             product = unique(company_data3$product),
                             scenario = unique(scenario_data2$scenario),
                             year = unique(scenario_data2$year), stringsAsFactors = FALSE) %>%
  left_join(scenario_data2, by = c("product", "scenario", "year")) %>%
  bind_rows(company_data3)

# Screen production g Vivid scenario values based on Rystad min and max values (cannot interpolate outside of the Rystad scenario range)
company_data5 <- company_data4 %>%
  group_by(product, year) %>%
  mutate(scenario_source = case_when(scenario %in% unique(scenario_data2$scenario) ~ "VE",
                                     scenario %in% unique(company_data3$scenario) ~ "Rystad",
                                     TRUE ~ NA_character_),
         rystad_max = max(ifelse(scenario_source == "Rystad", production_g, NA_real_), na.rm = TRUE),
         rystad_min = min(ifelse(scenario_source == "Rystad", production_g, NA_real_), na.rm = TRUE)) %>%
  mutate(production_g = case_when(scenario_source == "VE" & production_g > rystad_max & rystad_max != -Inf ~ rystad_max,
                                  scenario_source == "VE" & production_g < rystad_min & rystad_min != +Inf ~ rystad_min,
                                  TRUE ~ production_g)) %>%
  select(-scenario_source, -rystad_max, -rystad_min) %>%
  ungroup() %>%
  arrange(rystad_name, product, year, scenario)

### Define function for interpolating price_g from production_g, production_f from production_g,
### price_f from price_g, and unit cost f from production f

# Create list of tibbles to lapply/map the interpolation function over
company_data5_list <- company_data5 %>%
  mutate(tibble_id = paste(rystad_name, product, year)) %>%
  group_by(tibble_id) %>%
  nest() %>% 
  select(data) %>%
  unlist(recursive = FALSE)

# Interpolation function
interpolation <- function(data = NULL) {
  
  # Interpolation is only possible if we have >=2 non-missing values to interpolate using
  if(sum(!is.na(data$production_f) & !is.na(data$price_f)) >= 2) {
    
    temp <- data %>%
      mutate(price_g_interpolated = approx(production_g, price_g, xout = production_g)$y,
             production_f_interpolated = approx(production_g, production_f, xout = production_g)$y,
             price_f_interpolated = approx(price_g_interpolated, price_f, xout = price_g_interpolated)$y) %>%
      # Only possible to interpolate unit cost f from production f when there is variation across scenarios
      # return NAs when this is not the case
      # Not sure why this works - that is not what the code below does ...
      mutate(unit_cost_f_interpolated = ifelse(production_f_interpolated != mean(production_f_interpolated, na.rm = TRUE),
                                               approx(production_f_interpolated, unit_cost_f, xout = production_f_interpolated)$y,
                                               NA_real_)) %>%
      # Eliminate possibility of negative profits by capping firm unit costs at the price
      mutate(unit_cost_f_interpolated = ifelse(unit_cost_f_interpolated > price_f_interpolated, 
                                               price_f_interpolated, unit_cost_f_interpolated))
    
  } else {temp <- data}
  
  return(temp)
}

company_data6 <- map(company_data5_list, interpolation) %>%
  bind_rows() %>%
  arrange(rystad_name, product, year, scenario)

save_dated(company_data6, "Oil_and_gas_company_VE_scenarios", "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Calculate profit impacts and summary statistics ----

# Drop Rystad scenarios
company_results <- company_data6 %>%
  filter(!(scenario %in% c("Very_Low", "Low", "Central", "High")))

# Replace key variable values with NA when any of production f, price f and unit cost f are < 0 or missing
company_results2 <- company_results %>% 
  mutate_at(vars(production_f_interpolated, price_f_interpolated, unit_cost_f_interpolated),
            funs(case_when(production_f_interpolated < 0 | price_f_interpolated < 0 | unit_cost_f_interpolated < 0 |
                             is.na(production_f_interpolated) | is.na(price_f_interpolated) | is.na(unit_cost_f_interpolated) ~ NA_real_,
                           TRUE ~ .)))

# Rename and replace variables of interest
company_results3 <- company_results2 %>% 
  select(-production_f, -price_g, -price_f, -unit_cost_f) %>%
  rename(price_g = price_g_interpolated,
         production_f = production_f_interpolated,
         price_f = price_f_interpolated,
         unit_cost_f = unit_cost_f_interpolated)

# Calculate company profits under each scenario
company_results4 <- company_results3 %>%
  mutate(profit_f = (price_f - unit_cost_f) * production_f)

# Calcluate difference in profits compared to BAU scenario, and stranding / margin impacts
company_results5 <- company_results4 %>%
  group_by(rystad_name, product, year) %>% 
  mutate(profit_impact = -(profit_f - profit_f[[which(scenario == "BAU")]])) %>%
  # Reconsider the calculations / assumptions below at a later stage - preserved for now to ensure results
  # align, but the assumptions don't make sense
  mutate(profit_impact_temp = profit_f[[which(scenario == "BAU")]] - (price_f - unit_cost_f[[which(scenario == "BAU")]]) * production_f,
         margin_impact_temp = (price_f[[which(scenario == "BAU")]] - price_f) * production_f) %>%
  mutate(margin_impact = profit_impact * (margin_impact_temp / profit_impact_temp),
         stranding_impact = profit_impact - margin_impact) %>%
  ungroup() %>%
  select(-profit_impact_temp, -margin_impact_temp)

save_dated(company_results5, "Oil_and_gas_dd_full_results", "Interim", csv = TRUE)

# Get rid of 2016 values (no longer required)
company_results6 <- company_results5 %>%
  filter(year != 2016)

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Calculate NPV impacts ----

# Calculate aggregate production, revenue and profit impact by scenario (not discounted)
company_aggregate_results <- company_results6 %>% 
  mutate(revenue_f = production_f * price_f) %>%
  select(rystad_name, product, scenario, year, production_f, revenue_f, profit_f, profit_impact) %>%
  group_by(rystad_name, product, scenario) %>%
  summarise_at(vars(production_f:profit_impact),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup()

company_aggregate_results2 <- expand.grid(rystad_name = unique(company_aggregate_results$rystad_name),
                                          product = c(unique(company_aggregate_results$product), "All"),
                                          scenario = unique(company_aggregate_results$scenario), stringsAsFactors = FALSE) %>%
  left_join(company_aggregate_results, by = c("rystad_name", "product", "scenario")) %>%
  group_by(rystad_name, scenario) %>%
  # Cannot aggregate production because units for oil and gas are different (keep revenue instead)
  mutate_at(vars(revenue_f, profit_f, profit_impact),
            funs(case_when(product == "All" ~ sum(., na.rm = TRUE),
                           TRUE ~ .))) %>%
  ungroup() %>%
  arrange(rystad_name, scenario, product)

save_dated(company_aggregate_results2, "Oil_and_gas_dd_aggregates", "Interim", csv = TRUE)

# Calculate NPV impacts by scenario (discounted)
company_npv_results <- expand.grid(rystad_name = unique(company_results6$rystad_name),
                                   product = c(unique(company_results6$product), "All"),
                                   scenario = unique(company_results6$scenario),
                                   year = unique(company_results6$year), stringsAsFactors = FALSE) %>%
  left_join(company_results6, by = c("rystad_name", "product", "scenario", "year")) %>%
  select(-production_g, -price_g, -production_f, -price_f, -unit_cost_f) %>%
  group_by(rystad_name, scenario, year) %>%
  # Fill in 'All' product values
  mutate_at(vars(profit_f, profit_impact, margin_impact, stranding_impact),
            funs(case_when(product == "All" ~ sum(., na.rm = TRUE),
                           TRUE ~ .))) %>%
  arrange(rystad_name, scenario, year, product)

company_npv_results2 <- company_npv_results %>%
  # Use weights to include profits from 2018 - 50
  # 2020 value covers 2018 - 22.5 so weight is 4.5 / 5 = 0.9; 2050 value covers 2047.5 - 50 so weight is 2.5 / 5 = 0.5
  # All other weights are x - 2.5 - x + 2.5, so weight is 1
  mutate(weight = case_when(year == 2020 ~ 0.9,
                            year == 2050 ~ 0.5,
                            TRUE ~ 1)) %>%
  mutate_at(vars(profit_f, profit_impact, margin_impact, stranding_impact),
            funs("npv" = . / (1 + discount_rate) ^ (year - 2018))) %>%
  group_by(rystad_name, scenario, product) %>%
  mutate_at(vars(ends_with("npv")),
            funs("sum", sum(weight * ., na.rm = TRUE))) %>%
  summarise_at(vars(ends_with("_npv_sum")),
               funs(sum(., na.rm = TRUE))) %>%
  group_by(rystad_name, product) %>%
  mutate(profit_impact_pct = profit_impact_npv_sum / profit_f_npv_sum[[which(scenario == "BAU")]],
         stranding_impact_pct = stranding_impact_npv_sum / profit_f_npv_sum[[which(scenario == "BAU")]],
         margin_impact_pct = margin_impact_npv_sum / profit_f_npv_sum[[which(scenario == "BAU")]]) %>%
  ungroup() %>%
  select(rystad_name, scenario, product, profit_f_npv_sum, ends_with("pct")) %>%
  rename(profit = profit_f_npv_sum)

save_dated(company_npv_results2, "Oil_and_gas_dd_npv_impacts", "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 6 - Map Rystad company names to Vivid IDS and names ----

company_npv_results3 <- company_npv_results2 %>%
  left_join(company_names, by = "rystad_name") %>%
  filter(!is.na(company_id)) %>%
  select(company_id, company, everything(), -rystad_name)

save_dated(company_npv_results3, "Oil_and_gas_dd_npv_impacts", "Output", csv = TRUE)