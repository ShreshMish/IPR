##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  19/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script reads in TIAM biofuel production data from Excel and cleans it for use in later calculations and modelling
##### Dependencies:       1.  Latest Imperial TIAM scenarios Excel file
##### Notes:              IEA WEO scenario data is not used for biofuels as values do not match up

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "1_Scenarios"
source("utils.R")

# Read in TIAM scenario data from old scenarios (skip 6 empty rows at the top of the sheet) - units are PJ in snapshot year
tiam_biofuel_use <- read_excel(input_source("Vivid_20190208.xlsx"),
                               sheet = "AA_PrimaryEnergyProd", skip = 6)

# Read in TIAM scenario names
tiam_scenario_names <- read_excel(input_source("Vivid_scenario_names.xlsx"),
                                  sheet = "R1. Scenario specifications", range = "$A$10:$D$59")

# Rename TIAM fuels to match Vivid descriptions (only care about biofuels)
fuel_names <- tibble(ProcessSet = "MIN - BIO",
                     fuel = "biofuels")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean, reshape and create biofuel productiondata (2020 - 50) ----

# Merge in report scenario names and bin unrequired scenarios
tiam_scenario_names2 <- tiam_scenario_names %>%
  rename(old_scenario_name = `Scenario name`,
         scenario = `Report name`) %>%
  select(old_scenario_name, scenario)

# Clean variable names, remove redundant variables etc.
biofuel_prod1 <- tiam_biofuel_use %>%
  # Gather over years
  gather(key = "year", value = "production", -(Scenario:`Region\\Period`)) %>%
  mutate(year = as.numeric(year)) %>%
  # Merge in fuel names and remove fuels other than biofuels
  left_join(fuel_names, by = "ProcessSet") %>%
  filter(!is.na(fuel)) %>%
  # Rename variables and remove redundant variables
  rename(scenario = Scenario,
         region = `Region\\Period`) %>%
  select(-ProcessSet) %>%
  select(scenario, fuel, region, everything())

# Join in report scenario names and filter down to report scenarios
biofuel_prod2 <- biofuel_prod1 %>%
  rename(old_scenario_name = scenario) %>%
  left_join(tiam_scenario_names2, by = "old_scenario_name") %>%
  filter(!is.na(scenario)) %>%
  select(-old_scenario_name)

# Summarise over regions
biofuel_prod3 <- biofuel_prod2 %>%
  group_by(scenario, year) %>% 
  summarise(production = sum(production, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(scenario, year) %>%
  # Remove years outside of modelling horizon (2018 - 50)
  filter(year <= 2050)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Rename variables and scenarios to match spreadsheet names ----

# Reshape data and add a units column (PJ)
biofuel_prod4 <- biofuel_prod3 %>%
  rename(Scenario = scenario) %>%
  mutate(Units = "PJ") %>%
  select(Scenario, Units, everything()) %>%
  spread(key = "year", value = "production")

save_dated(biofuel_prod4, "Biofuels_production", folder = "Output", csv = TRUE)