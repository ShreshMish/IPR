##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  24/01/2019
##### Code author:        Shyamal Patel
##### Description:        This script reads in TIAM biofuel production data from Excel and cleans it for use in later calculations and modelling
##### Dependencies:       1.  Latest Imperial TIAM scenarios Excel file
#####                     2.  IEA WEO scenarios data
#####                     (current scenario data = "Input/Vivid_scenario_runs.xls" & "Input/Vivid_scenario_runs_newdb.xls" (for Weak 2020))

##### TIAM has been updated with new technology costs and constraint changes, which is causing discrepancies between the same 
##### scenario run in different versions - adjust 'weak 2020' scenario from new database to compensate for this

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "1_Scenarios"
source("utils.R")

# Read in TIAM scenario data from old scenarios (skip 6 empty rows at the top of the sheet) - units are PJ in snapshot year
tiam_biofuel_use <- read_excel(input_source("Vivid_scenario_runs.xls"),
                               sheet = "AA_PrimaryEnergyCons", skip = 6)

# Read in TIAM scenario data from new scenarios (skip 6 empty rows at the top of the sheet) - units are PJ in snapshot year
# tiam_w2020_biofuel_use <- read_excel("0 - Scenarios/Input/Vivid_scenario_runs_newdb.xls",
#                                      sheet = "AA_PrimaryEnergyCons", skip = 6)

# Read in IEA WEO scenario data - renewable generation capacity
weo_biofuel_use <- read_excel(input_source("WEO2017_AnnexA_VE.xlsx"),
                                  sheet = "VE Biofuel prod", range = "$A$7:$G$8")

# Read in scenario names (defined by Carbon_prices.R script)
scenario_names <- readRDS("1_Scenarios/Interim/Scenario_name_mapping.rds")

# Rename TIAM fuels to match Vivid descriptions (only care about biofuels)
fuel_names <- tibble(CommoditySet = "PRIM FUELS - BIO",
                     fuel = "biofuels")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean, reshape and create biofuel production data (2020 - 50) ----

# Clean variable names, remove redundant variables etc.
biofuel_prod1 <- tiam_biofuel_use %>%
  # Remove redundant variable
  select(-UserConstraint, -Attribute) %>%
  # Gather over years
  gather(key = "year", value = "production", -(Scenario:`Region\\Period`)) %>%
  mutate(year = as.numeric(year)) %>%
  # Merge in fuel names and remove fuels other than biofuels
  left_join(fuel_names) %>%
  filter(!is.na(fuel)) %>%
  # Rename variables and remove redundant variables
  rename(scenario = Scenario,
         region = `Region\\Period`) %>%
  select(-CommoditySet) %>%
  select(scenario, fuel, region, everything())

# Summarise over regions
biofuel_prod2 <- biofuel_prod1 %>%
  group_by(scenario, year) %>% 
  summarise(production = sum(production, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(scenario, year) %>%
  # Remove years outside of modelling horizon (2018 - 50)
  filter(year <= 2050)

# Data cleaning for weak 2020 scenario (doesn't exist as Adam hasn't provided the corresponding sheet for the
# w2020 scenario runs)
# For now, add a weak 2020 scenario which takes the same values as BHP base
biofuel_prod3 <- biofuel_prod2 %>%
  spread(key = "scenario", value = "production") %>%
  mutate(s01b_BHP_weak2020_12P2 = s01a_BHP_base) %>%
  gather(key = "scenario", value = "production", -(year)) %>%
  select(scenario, production, year)

# Remove surplus scenarios and rename scenarios based on model names (units are PJ)
biofuel_prod4 <- biofuel_prod3 %>%
  filter(!(scenario %in% c("s06a_BHP_cum3dt_Highfeasibility", "s06a_BHP_cum4dt_Highfeasibility",
                           "s06a_BHP_cumb2c300_Highfeasibility"))) %>%
  left_join(scenario_names) %>%
  select(model_scenario, year, production) %>%
  rename(scenario = model_scenario)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Ad hoc changes to renewable capacity data - please check when updating scenario inputs ----

# Replace 2020 value with BAU value for all scenarios
biofuel_prod5 <- biofuel_prod4 %>%
  mutate(production = case_when(year == 2020 ~ production[[which(scenario == "BAU" & year == 2020)]],
                                TRUE ~ production))

# Put scenarios in columns before further data cleaning
biofuel_prod6 <- biofuel_prod5 %>%
  spread(key = scenario, value = production)


# Replace 2030 value with BAU value for delayed scenario only
biofuel_prod7 <- biofuel_prod6 %>%
  mutate(`2DS_delay` = case_when(year == 2030 ~ BAU,
                                 TRUE ~ `2DS_delay`)) %>%
  # Reshape data
  gather(key = "scenario", value = "production", -(year))

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Rename variables and scenarios to match spreadsheet names ----

biofuel_prod8 <- biofuel_prod7 %>%
  rename(Scenario = scenario) %>%
  mutate(Scenario = case_when(Scenario == "2DS_central" ~ "2DS central",
                              Scenario == "2DS_cheap_ccs" ~ "2DS high CCS",
                              Scenario == "2DS_cheap_ren" ~ "2DS high renewables",
                              Scenario == "2DS_cheap_eff" ~ "2DS high efficiency",
                              Scenario == "2DS_delay" ~ "2DS delayed",
                              TRUE ~ Scenario))

# Reshape data and add a units column (PJ)
biofuel_prod9 <- biofuel_prod8 %>%
  mutate(Units = "PJ") %>%
  select(Scenario, Units, everything()) %>%
  spread(key = "year", value = "production")

save_dated(biofuel_prod9, "Biofuels_production", folder = "Output", csv = TRUE)