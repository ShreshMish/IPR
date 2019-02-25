##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  17/02/2019
##### Model author:       Robert Ritz
##### Code author:        Shyamal Patel
##### Dependencies:       1. Cleaned financial and emissions dataset
#####                     2. Demand destruction model:
#####                         a) Upstream oil & gas results
#####                         b) Upstream coal results
#####                         c) Downstream and oil & gas services results [industry average]
#####                         d) ICE vehicles results [industry average]
#####                     3. Cleantech market model results
#####                     4. Cleaned carbon costs results
##### Notes:              None
##### Called by:          None

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "3_Cost_and_competition"
source("utils.R")

# Data: Cleaned financial and emissions dataset read in
model_panel <- readRDS(input_source("Model_panel.rds"))

### TRY TO REMOVE THIS DEPENDENCY
# Data: Cleantech markets reclassification
cleantech_reclassified <- readRDS(input_source("Green_upside_reclassification.rds"))

# DD: upstream oil and gas results
dd_oilandgas_upstream <- readRDS("1_Demand_destruction/Output/Oil_and_gas_dd_npv_impacts.rds")

# DD: upstream coal results
dd_coal_upstream <- readRDS("1_Demand_destruction/Output/Coal_dd_npv_impacts.rds")

# DD: downstream oil and gas quantity impact results (for sectors related to oil and gas)
dd_fossil_fuel_downstream <- readRDS("1_Demand_destruction/Output/Oil_and_gas_dd_downstream_qimpacts.rds")

# DD: ICE vehicle results
dd_ice_vehicles <- readRDS("1_Demand_destruction/Output/ICE_vehicle_dd_qimpacts.rds")

# CM: cleantech markets results
cleantech_markets <- readRDS("2_Cleantech_markets/Output/Cleantech_npv_impacts.rds")

# CC: Carbon cost results
carbon_cost_results <- readRDS("3_Cost_and_competition/Interim/Carbon_costs.rds")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean demand destruction & cleantech markets results ----

# DD: upstream oil and gas results - keep product 'All' results only and add market column for merge purposes
dd_oilandgas_upstream2 <- dd_oilandgas_upstream %>%
  filter(product == "All") %>%
  mutate(market = "Exploration and production") %>%
  select(scenario, company_id, company, market, profit_impact_pct, stranding_impact_pct, margin_impact_pct) %>%
  arrange(scenario, company)

# DD: upstream coal results - add market merge column
dd_coal_upstream2 <- dd_coal_upstream %>%
  mutate(market = "Coal") %>%
  select(scenario, company_id, company, market, profit_impact_pct, stranding_impact_pct, margin_impact_pct) %>%
  arrange(scenario, company)

# DD: downstream oil and gas results - add in markets that each value applies to
oilandgas_downstream <- c("O&G T&D", "Oil Equip. & Services", "Petrochemicals")
gas_downstream <- c("Gas Distribution", "Gas processing", "Gas retail", "Gas storage")
oil_downstream <- c("Oil retail")

dd_fossil_fuel_downstream2 <- expand.grid(market = c(oilandgas_downstream, gas_downstream, oil_downstream),
                                          scenario = unique(dd_fossil_fuel_downstream$scenario), stringsAsFactors = FALSE) %>%
  mutate(product = case_when(market %in% oilandgas_downstream ~ "All",
                             market %in% gas_downstream ~ "Gas",
                             market %in% oil_downstream ~ "Liquid",
                             TRUE ~ NA_character_)) %>%
  left_join(dd_fossil_fuel_downstream, by = c("scenario", "product")) %>%
  select(scenario, market, profit_impact_pct)

# DD: ICE vehicle results - add market merge column
dd_ice_vehicles2 <- dd_ice_vehicles %>%
  mutate(market = "Automobiles") %>%
  select(scenario, market, profit_impact_pct)

# CM: cleantech markets results
cleantech_markets2 <- cleantech_markets %>%
  rename(gr_product = ve_category) %>%
  left_join(cleantech_reclassified, by = c("gr_product")) %>%
  rename(market = gr_product_renamed) %>%
  select(scenario, company_id, company, market, profit_impact_pct, marketgrowth_impact_pct, marketshare_impact_pct) %>%
  arrange(scenario, company)

# Bind together company-level results from demand destruction & cleantech markets
dd_cm_company_results <- dd_oilandgas_upstream2 %>%
  bind_rows(dd_coal_upstream2) %>%
  bind_rows(cleantech_markets2)

# Bind together industry-level results from demand destruction (N/A for cleantech markets)
dd_cm_industry_results <- dd_fossil_fuel_downstream2 %>%
  bind_rows(dd_ice_vehicles2)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Merge all datasets together ----

model_panel2 <- model_panel %>%
  left_join(carbon_cost_results, by = c("market", "region")) %>%
  select(scenario, everything()) %>%
  # Temporary scenario merge variable for 'Lack of coordination' scenario [same DD/CM results as 'Central' scenario]
  mutate(scenario_temp = ifelse(scenario == "Lack_Of_Coordination", "2DS_Balanced_Transformation", scenario)) %>%
  left_join(dd_cm_company_results, by = c("scenario_temp" = "scenario", "company_id", "company", "market")) %>%
  left_join(dd_cm_industry_results, by = c("scenario_temp" = "scenario", "market")) %>%
  mutate(profit_impact_pct = ifelse(!is.na(profit_impact_pct.x), profit_impact_pct.x, profit_impact_pct.y)) %>%
  select(scenario:market_corporation_tax_rate, ends_with("_pct"), everything(), -scenario_temp,
         -profit_impact_pct.x, -profit_impact_pct.y)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Ad-hoc changes to the data ----

###### ELIMINATE THIS SECTION IN FURTHER CODE UPDATES
model_panel3 <- model_panel2 %>%
  mutate(corporation_tax_rate = corporation_tax_rate / 100) %>% 
  filter(company_id != "CNE000001LJ2 CNE1000003D8") %>% # Remove HUADIAN POWER INTERNATIONAL 'H' from analysis (big outlier and HUADIAN POWER INTL exists separately (same company))
  filter(company_id != "CNE100000HD4") %>% # Remove CHIN.LONGYUAN PWR.GP.'H' from analysis (not a renewable energy equipment company - utility and therefore a big outlier in the renewable market)
  filter(company_id != "KYG3774X1088") # Remove GCL-POLY ENERGY HOLDINGS from analysis (not a renewable energy equipment company - utility and therefore a big outlier in the renewable market)


##### TEST WHETHER THE ABOVE ARE REALLY NECESSARY + HOW BMW RESULTS LOOK - THIS WAS ANOMALOUS (PREF SHARE CLASS) IN THE PREVIOUS SET OF RESULTS

save_dated(model_panel3, "Cleaned_model_panel", folder = "Interim", csv = FALSE)