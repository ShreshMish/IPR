##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  19/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script reads in TIAM fossil fuel production data from Excel and cleans it for use in later calculations and modelling
##### Dependencies:       1.  Latest Imperial TIAM scenarios Excel file
#####                     2.  IEA WEO scenarios data

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "1_Scenarios"
source("utils.R")

# Read in TIAM scenario data from old scenarios (skip 6 empty rows at the top of the sheet) - units are PJ
tiam_fossil_fuel_prod <- read_excel(input_source("Vivid_20190208.xlsx"),
                                    sheet = "AA_PrimaryEnergyProd", skip = 6)

# Read in IEA WEO scenario data - fossil fuel production (NB - must save a xlsx version of the macro-enabled WEO Annex A workbook) - units are in 'Units' column
public_scen_fossil_fuel_prod <- read_excel(input_source("Public_scenarios.xlsx"),
                                           sheet = "W20. Fossil fuel prod", range = "$B$48:$Q$90")

# Read in energy unit conversion factors
energy_conversion_factors <- read_excel(input_source("Energy_conversion_factors.xlsx"),
                                        sheet = "Energy unit conversions", range = "$A$3:$G$8")

# Read in TIAM scenario names
scenario_names <- read_excel(input_source("Vivid_scenario_names.xlsx"),
                             sheet = "R1. Scenario specifications", range = "$A$10:$E$59")

# Adjust factor for ratio of BP historical oil production data to TIAM data
### IMPROVE AUDIT TRAIL HERE - CALCULATIONS IN R
tiam_oil_adj_factor <- 1.1293
tiam_coal_adj_factor <- 0.8147
tiam_gas_adj_factor <- 0.9915

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean and reshape public fossil fuel production scenario data (2017 - 50) ----

# Clean up scenario names and remove unneeded scenarios (ETP17, WEO16, Shell Sky etc.)
scenario_names2 <- scenario_names %>%
  rename(old_scenario_name = `Scenario name`,
         scenario = `Report name`,
         public_scenario = `Public scen Report name`) %>%
  select(old_scenario_name, scenario, public_scenario)

public_scen_fossil_fuel_prod2 <- public_scen_fossil_fuel_prod %>%
  rename(old_scenario_name = `Scenario`,
         fuel = Fuel,
         subcat = Subcat,
         units = Units) %>%
  gather(key = "year", value = "production", `2014`:`2050`) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(scenario_names2, by = c("old_scenario_name")) %>%
  select(public_scenario, fuel, subcat, units, year, production) %>%
  rename(scenario = public_scenario)

# Filter out WEO18 scenario data used for 'No_New_Action' and 'Paris_NDCs' scenarios
weo18_fossil_fuel_prod <- public_scen_fossil_fuel_prod2 %>%
  filter(!is.na(scenario)) %>%
  # Rystad oil & gas data work requires 2016 values instead of 2017
  filter(year >= 2016 & year != 2017)

# Summarise over coal subcategories (thermal and coking)
weo18_fossil_fuel_prod2 <- weo18_fossil_fuel_prod %>%
  group_by(scenario, fuel, year, units) %>%
  summarise(production = sum(production)) %>%
  ungroup()

# Switch oil units from Mb/d to Mb
weo18_fossil_fuel_prod3 <- weo18_fossil_fuel_prod2 %>%
  mutate(fuel = tolower(fuel)) %>%
  mutate(production = case_when(fuel == "oil" ~ production * 365,
                                TRUE ~ production),
         units = case_when(fuel == "oil" ~ "Mb", 
                           TRUE ~ units))

save_dated(weo18_fossil_fuel_prod3, "WEO_fossil_production", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Clean, reshape and change units of TIAM fossil fuel production data (2020 - 50) ----

# Turn energy conversion factors into named vector and add new factors
energy_conversion_factors_data <- as.vector(energy_conversion_factors$Value)
names(energy_conversion_factors_data) <- energy_conversion_factors$Code

# Add new factor required for TIAM oil data - PJ to million barrels
energy_conversion_factors_data["PJ_to_Mb"] <- energy_conversion_factors_data["PJ_to_Mtoe"] / energy_conversion_factors_data["bbl_to_toe"]

# Add new factor required for TIAM gas data - PJ to bcm
energy_conversion_factors_data["PJ_to_bcm"] <- energy_conversion_factors_data["PJ_to_trnBtu"] * energy_conversion_factors_data["trnBtu_to_bcm"]

# Add new factor required for TIAM coal data - PJ to tce
energy_conversion_factors_data["PJ_to_tce"] <- energy_conversion_factors_data["PJ_to_Mtoe"] / energy_conversion_factors_data["tce_to_toe"]

# Clean up dataframe names
tiam_fossil_fuel_prod2 <- tiam_fossil_fuel_prod %>%
  rename(scenario = Scenario,
         product = ProcessSet,
         region = `Region\\Period`) %>%
  gather(key = "year", value = "production", `2005`:`2100`) %>%
  mutate(year = as.numeric(year))

# Merge in report scenario names and bin unrequired scenarios
tiam_fossil_fuel_prod3 <- tiam_fossil_fuel_prod2 %>%
  rename(old_scenario_name = scenario) %>%
  left_join(scenario_names2, by = "old_scenario_name") %>%
  select(-old_scenario_name, -public_scenario) %>%
  filter(!is.na(scenario))

# Filter out primary energy production categories that are not used here
tiam_fossil_fuel_prod4 <- tiam_fossil_fuel_prod3 %>%
  filter(year <= 2050 & year >= 2020) %>%
  mutate(fuel = case_when(product == "MIN - COA" ~ "coal",
                          product == "MIN - GAS" ~ "gas",
                          product == "MIN - OIL" ~ "oil",
                          TRUE ~ NA_character_)) %>%
  select(-product) %>%
  filter(!is.na(fuel)) %>%
  mutate(units = "PJ") %>%
  select(scenario, region, fuel, units, year, production)

# Summarise fossil fuel categories over regions
tiam_fossil_fuel_prod5 <- tiam_fossil_fuel_prod4 %>%
  group_by(scenario, fuel, units, year) %>%
  summarise(production = sum(production, na.rm = TRUE)) %>%
  ungroup()

# Remove 'No_New_action' and 'Paris_NDCs' scenarios from TIAM [using WEO18 for these]
tiam_fossil_fuel_prod6 <- tiam_fossil_fuel_prod5 %>%
  filter(!(scenario %in% c("No_New_Action", "Paris_NDCs")))

# Switch TIAM scenario units [oil -> Mb, coal -> Mtce, gas -> bcm]
tiam_fossil_fuel_prod7 <- tiam_fossil_fuel_prod6 %>%
  mutate(production = case_when(fuel == "coal" ~ production * energy_conversion_factors_data["PJ_to_tce"],
                                fuel == "oil" ~ production * energy_conversion_factors_data["PJ_to_Mb"],
                                fuel == "gas" ~ production * energy_conversion_factors_data["PJ_to_bcm"]),
         units = case_when(fuel == "coal" ~ "Mtce", # Million tonnes of coal equivalent per annum
                           fuel == "oil" ~ "Mb", # Million barrels of oil per annum
                           fuel == "gas" ~ "bcm")) # Billion cubic metres of natural gas per annum

# Adjust TIAM values by ratio of historical BP Stat review to TIAM
tiam_fossil_fuel_prod8 <- tiam_fossil_fuel_prod7 %>%
  mutate(production = case_when(fuel == "coal" ~ production * tiam_coal_adj_factor,
                                fuel == "oil" ~ production * tiam_oil_adj_factor,
                                fuel == "gas" ~ production * tiam_gas_adj_factor))

# Merge together TIAM scenario data and WEO18 scenario data
fossil_fuel_prod <- tiam_fossil_fuel_prod8 %>%
  bind_rows(weo18_fossil_fuel_prod3)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Ad hoc changes to fossil fuel production data - please check when updating scenario inputs ----

# Put scenarios in columns before further data cleaning
fossil_fuel_prod2 <- fossil_fuel_prod %>%
  spread(key = scenario, value = production)

# Fill in 2016 and 2020 WEO Paris values in place of TIAM values, 2030 WEO Paris values in place of TIAM Late_Action scenario values
fossil_fuel_prod3 <- fossil_fuel_prod2 %>%
  group_by(fuel, units) %>%
  mutate_at(vars(unique(tiam_fossil_fuel_prod7$scenario)),
            funs(case_when(year == 2016 ~ Paris_NDCs,
                           year == 2020 ~ Paris_NDCs,
                           TRUE ~ .))) %>%
  mutate(Late_Action = ifelse(year == 2030, Paris_NDCs, Late_Action)) %>%
  ungroup()

# Interpolate missing years (2025, 2035, 2045)
fossil_fuel_prod4 <- fossil_fuel_prod3 %>%
  group_by(fuel, units) %>%
  # Also apply to WEO scenarios for consistency
  mutate_at(vars(unique(weo18_fossil_fuel_prod3$scenario)),
            funs(ifelse(year %in% c(2025, 2035, 2045), NA_real_, .))) %>%
  mutate_at(vars(c(unique(weo18_fossil_fuel_prod3$scenario), unique(tiam_fossil_fuel_prod7$scenario))),
            funs(approx(x = year, y = ., xout = year)$y)) %>%
  ungroup()

# Reshape and change units to match old spreadsheet outputs (preserved to save on editions to the other code)
fossil_fuel_prod5 <- fossil_fuel_prod4 %>%
  gather(key = "scenario", value = "production", -(fuel:year)) %>%
  mutate(production = case_when(fuel == "gas" ~ production * 10^9, # Gas units: bcm -> cm
                                fuel == "oil" ~ production * 10^6, # Oil units: Mbarrels -> barrels
                                fuel == "coal" ~ production * 10^6), # Coal units: Mtonnes -> tonnes
         units = case_when(fuel == "gas" ~ "cm",
                           fuel == "oil" ~ "barrels",
                           fuel == "coal" ~ "tonnes"))

# Reorder variables for clarity
fossil_fuel_prod6 <- fossil_fuel_prod5 %>%
  select(scenario, fuel, units, year, production)

save_dated(fossil_fuel_prod6, "Fossil_fuel_production", folder = "Output", csv = TRUE)