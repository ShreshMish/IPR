##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  19/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script imports outputs from company-level demand destruction modelling for use in sector-level
#####                     fossil fuel value chain value impairment estimation (downstream sectors, oil & gas services etc.)
##### Dependencies:       1. Results from the oil & gas company-level modelling (generated by the Upstream_oilandgas.R script)
#####                        Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "1_Demand_destruction"
source("utils.R")

# Read in oil & gas demand destruction model results
upstream_results <- readRDS("1_Demand_destruction/Interim/Oil_and_gas_dd_full_results.rds")

# Discount rate should be 0.0575 (2% inflation rate adjustment does not apply)
discount_rate <- 0.0575

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Find quantity contraction in o&g for related industries (mid-/downstream) ----
#                 (refineries, pipelines, service companies etc.)

# Add column with 2016 prices for each fuel (constant across scenarios)
constant_prices_data <- upstream_results %>%
  group_by(product) %>%
  mutate(price_g_2016 = price_g[[which(year == 2016)[[1]]]]) %>%
  ungroup()

# Filter down to one row per fuel - scenario - year combination
constant_prices_data2 <- constant_prices_data %>%
  select(product, scenario, year, production_g, price_g_2016) %>%
  filter(!is.na(production_g)) %>%
  unique()

# Calculate industry-level quantity impacts in NPV terms (weights on oil & gas are 2016 prices)
revenue_impact <- constant_prices_data2 %>%
  mutate(revenue_g = production_g * price_g_2016) %>%
  # Get rid of 2016 quantity values (not needed for interpolation exercise)
  filter(year != 2016) %>%
  # Use same interpolation weights used in the o&g upstream modelling
  mutate(weight = case_when(year == 2020 ~ 0.9,
                            year == 2050 ~ 0.5,
                            TRUE ~ 1),
         revenue_g_npv = revenue_g / ((1 + discount_rate) ^ (year - 2018)))

# Summarise over years
revenue_impact2 <- revenue_impact %>%
  group_by(scenario, product) %>%
  # Summarise over years
  summarise(revenue_g_npv = sum(revenue_g_npv, na.rm = TRUE)) %>%
  ungroup()

# Calculate percentage impact at the firm level
revenue_impact3 <- revenue_impact2 %>%
  group_by(product) %>%
  mutate(profit_impact_pct = revenue_g_npv / revenue_g_npv[[which(scenario == "Paris_NDCs")]] - 1)

# Add % impact for 'All' (oil & gas combined)
revenue_impact4 <- expand.grid(scenario = unique(revenue_impact3$scenario),
                               product = c("All", unique(revenue_impact3$product)), stringsAsFactors = FALSE) %>%
  left_join(revenue_impact3, by = c("scenario", "product")) %>%
  group_by(scenario) %>%
  mutate(revenue_g_npv = case_when(product %in% c("Liquid", "Gas") ~ revenue_g_npv,
                                   TRUE ~ sum(revenue_g_npv, na.rm = TRUE))) %>%
  group_by(product) %>%
  mutate(profit_impact_pct = case_when(product %in% c("Liquid", "Gas") ~ profit_impact_pct,
                                       TRUE ~ revenue_g_npv / revenue_g_npv[[which(scenario == "Paris_NDCs")]] - 1)) %>%
  ungroup()

save_dated(revenue_impact4, "Oil_and_gas_dd_downstream_qimpacts", folder = "Output", csv = TRUE)
