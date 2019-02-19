##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  19/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script reads in ICE vehicle scenario and exposure data and models stranding impacts
##### Dependencies:       1.  ICE new capacity data by climate policy scenario
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "1_Demand_destruction"
source("utils.R")

# Read in scenario data
scenario_data <- readRDS(input_source("ICE_scenario_data.rds"))

# Set discount rate
discount_rate <- 0.0575

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean scenario data ----

scenario_data2 <- scenario_data %>%
  gather(key = "year", value = "quantity", `2020`:`2050`) %>%
  mutate(year = as.numeric(year))

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - NPV quantity contraction ----

# NPV weighting approach is the same used in the oil & gas upstream and downstream scripts
revenue_impact_npv <- scenario_data2 %>%
  mutate(weight = case_when(year == 2020 ~ 0.9,
                                year == 2050 ~ 0.5,
                                TRUE ~ 1),
         quantity_npv = quantity / (1 + discount_rate) ^ (year - 2018)) %>%
  group_by(scenario) %>%
  summarise(quantity_npv = sum(quantity_npv)) %>%
  ungroup() %>%
  mutate(profit_impact_pct = quantity_npv / quantity_npv[[which(scenario == "Paris_NDCs")]] - 1)
  
save_dated(revenue_impact_npv, "ICE_vehicle_dd_qimpacts", folder = "Output", csv = TRUE)