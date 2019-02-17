##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  17/02/2019
##### Code author:        Shyamal Patel
##### Dependencies:       1. Cleaned carbon cost curves dataset
#####                     2. Cleaned carbon price scenario dataset
##### Notes:              None
##### Called by:          None

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "3_Cost_and_competition"
source("utils.R")

# Data: Carbon cost curves
carbon_cost_curves <- readRDS(input_source("Carbon_cost_curves.rds"))

# Data: Carbon prices scenario data
carbon_prices <- readRDS(input_source("Carbon_prices_scenario_data.rds"))

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Calculate carbon costs by climate scenario ----

# Merge together carbon cost curves and scenario-level carbon prices
carbon_prices2 <- carbon_prices %>%
  rename(carbon_price_sector = sector)

carbon_cost_results <- carbon_prices2 %>%
  left_join(carbon_cost_curves, by = c("year", "region", "carbon_price_sector")) %>%
  filter(year >= 2017) %>%
  select(scenario, market, region, year, carbon_price_sector, mac_curve_sector, lever, carbon_price, everything()) %>%
  filter(!is.na(market))

# Find the marginal abatement lever (most expensive, yet still utilised lever)
carbon_cost_results2 <- carbon_cost_results %>%
  group_by(scenario, market, region, year) %>% 
  # Order entries in order of increasing cost
  arrange(scenario, market, region, year, mean_abatement_cost, cum_abatement_potential) %>%
  # Find utilised levers based on the realised carbon price
  mutate(utilised_lever = case_when(abatement_cost <= carbon_price ~ 1,
                                    TRUE ~ NA_real_),
         marginal_entry = cumsum(utilised_lever),
         # Note that marginal entry takes value -Infinity when all levers are unprofitable (abatement cost > carbon price)
         marginal_entry = case_when(marginal_entry == max(marginal_entry, na.rm = TRUE) ~ 1,
                                    TRUE ~ 0))

# Calculate carbon costs based on the carbon price, actual abatement and mean abatement cost
carbon_cost_results3 <- carbon_cost_results2 %>% 
  mutate(actual_abatement = case_when(max(marginal_entry) == 0 ~ 0, # Special case for when no abatement options are used
                                      marginal_entry == 1 ~ cum_abatement_potential, # Take total actual abatement % from the marginal entry row
                                      TRUE ~ NA_real_), # Residual case [non-marginal entries]
         actual_abatement_cost = case_when(max(marginal_entry) == 0 ~ 0, # Special case for when no abatement options are used
                                           marginal_entry == 1 ~ mean_abatement_cost, # Take mean abatement cost from the marginal entry row
                                           TRUE ~ NA_real_), # Residual case [non-marginal entries]
         carbon_cost = carbon_price * (1 - actual_abatement) + actual_abatement_cost * actual_abatement) %>%
  ungroup()

save_dated(carbon_cost_results3, "Carbon_cost_calculations", folder = "Interim", csv = FALSE)

# Drop the lever variable and keep only unique values
carbon_cost_results4 <- carbon_cost_results3 %>%
  filter(!is.na(actual_abatement)) %>%
  select(scenario, market, region, year, carbon_price, carbon_cost) %>%
  unique()

# Reshape carbon prices and carbon costs data before merging into main dataset
reshape_carbon <- function(var) {
  temp <- carbon_cost_results4 %>%
    select(scenario:year, !!var) %>%
    spread(key = year, value = !!var) %>%
    rename_at(vars(`2017`:`2050`), funs(paste0(var, "_", .)))
  return(temp)
}

carbon_cost_results5 <- reshape_carbon("carbon_cost")
carbon_cost_results6 <- reshape_carbon("carbon_price") %>%
  left_join(carbon_cost_results5, by = c("scenario", "market", "region"))

save_dated(carbon_cost_results6, "Carbon_costs", folder = "Interim", csv = FALSE)