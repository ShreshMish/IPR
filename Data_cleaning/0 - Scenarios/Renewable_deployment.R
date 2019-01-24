##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  16/10/2018
##### Code author:        Shyamal Patel
##### Description:        This script reads in TIAM renewable deployment data from Excel and cleans it for use in later calculations and modelling
##### Dependencies:       1.  Latest Imperial TIAM scenarios Excel file
#####                     2.  IEA WEO scenarios data
#####                     (current scenario data = "Input/Vivid_scenario_runs.xls" & "Input/Vivid_scenario_runs_newdb.xls" (for Weak 2020))

##### TIAM has been updated with new technology costs and constraint changes, which is causing discrepancies between the same 
##### scenario run in different versions - adjust 'weak 2020' scenario from new database to compensate for this

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA ----

packages <- c("tidyverse", "magrittr", "readxl", "here", "stringi", "zoo")
lapply(packages, require, character.only = TRUE)

# Define date for save file names
day <- format(Sys.time(), "%d")
month <- match(format(Sys.time(), "%b"), month.abb)
if(nchar(month) == 1) {month <- paste0("0", month)}
year <- substr(format(Sys.time(), "%Y"), 3, 4)
date <- paste0(year, month, day)
rm(day, month, year)

# These functions count the number of missing or zero-value observations in a tibble
na_counter <- function(x) {sum(is.na(x))}
zero_counter <- function(x) {sum(ifelse(x == 0, 1, 0))}

# These functions save base, and dated files in the Interim or Output folder for later use
# Dated files are kept for version control purposes
save_dated <- function(data, name, folder, dated = TRUE, csv = FALSE) {
  main_path <- paste0("0 - Scenarios/", folder)
  dated_path <- paste0("0 - Scenarios/", folder, "/Dated/")
  
  # Save main file
  saveRDS(data, here(main_path, paste0(name, ".rds")))
  # Save dated file (optional)
  if(dated == TRUE) {saveRDS(data, here(dated_path, paste0(date, "_", name, ".rds")))}
  # Save dated CSV file (optional)
  if(csv == TRUE) {write_csv(data, here(dated_path, paste0(date, "_", name, ".csv")))}
}

# Read in TIAM scenario data from old scenarios (skip 6 empty rows at the top of the sheet) - units are GW in snapshot year
tiam_renewable_total_capa <- read_excel("0 - Scenarios/Input/Vivid_scenario_runs.xls",
                                        sheet = "AA_PowerCap", skip = 6)

# Read in TIAM scenario data from new scenarios (skip 6 empty rows at the top of the sheet) - units are GW in snapshot year
tiam_w2020_renewable_total_capa <- read_excel("0 - Scenarios/Input/Vivid_scenario_runs_newdb.xls",
                                              sheet = "AA_PowerCap", skip = 6)

# Read in IEA WEO scenario data - renewable generation capacity
weo_raw_renewable_total_capa <- read_excel("0 - Scenarios/Input/WEO2017_AnnexA_VE.xlsx",
                                           sheet = "VE Total renewable capa", range = "$A$7:$H$14")

# Read in scenario names (defined by Carbon_prices.R script)
scenario_names <- readRDS("0 - Scenarios/Interim/Scenario_name_mapping.rds")

# Rename TIAM fuels to match Vivid descriptions
fuel_names <- tibble(ProcessSet = unique(tiam_renewable_total_capa$ProcessSet)) %>%
  mutate(fuel = case_when(ProcessSet %in% c("POWGEN - BIO", "POWGEN - BIO CCS") ~ "Biomass (incl CCS)", # Group biomass CCS and conventional together
                          ProcessSet == "POWGEN - WIN" ~ "Wind",
                          ProcessSet %in% c("POWGEN - SOPV", "POWGEN - SOTH") ~ "Solar",
                          ProcessSet == "POWGEN - HYD" ~ "Hydro",
                          ProcessSet %in% c("POWGEN - GEO", "POWGEN - TID") ~ "Other renewables"),
         units = "GW")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean, reshape and create aggregate renewable generation capacity data (2020 - 50) ----

renewable_capa <- tiam_renewable_total_capa %>%
  gather(key = "year", value = "capacity", `2005`:`2100`) %>%
  mutate(year = as.numeric(year)) %>%
  rename(scenario = Scenario,
         region = `Region\\Period`) %>%
  # Add units column and rename fuels
  left_join(fuel_names, by = "ProcessSet") %>%
  select(scenario, fuel, units, region, year, capacity)
  
# Summarise all renewable capacity
all_renewable_capa <- renewable_capa %>%
  filter(!is.na(fuel)) %>%
  group_by(scenario, region, year, units) %>%
  summarise(capacity = sum(capacity, na.rm = TRUE)) %>%
  mutate(fuel = "Renewables")

# Summarise all generation capacity
all_capa <- renewable_capa %>%
  group_by(scenario, region, year, units) %>%
  summarise(capacity = sum(capacity, na.rm = TRUE)) %>%
  mutate(fuel = "All")

# Summarise over fuels and merge together datasets
renewable_capa %<>%
  # Summarise over fuels
  group_by(scenario, region, year, units, fuel) %>%
  filter(!is.na(fuel) & fuel != "Other renewables") %>%
  summarise(capacity = sum(capacity, na.rm = TRUE)) %>%
  # Merge in other datasets
  bind_rows(all_renewable_capa) %>%
  bind_rows(all_capa) %>%
  arrange(scenario, region, year, fuel)

# Remove surplus scenarios
renewable_capa %<>%
  filter(!scenario %in% c("s06a_BHP_cum3dt_Highfeasibility", "s06a_BHP_cum4dt_Highfeasibility", "s06a_BHP_cumb2c300_Highfeasibility"))

# Repeat for TIAM weak 2020 scenario data
w2020_renewable_capa <- tiam_w2020_renewable_total_capa %>%
  gather(key = "year", value = "capacity", `2005`:`2100`) %>%
  mutate(year = as.numeric(year)) %>%
  rename(scenario = Scenario,
         region = `Region\\Period`) %>%
  # Add units column and rename fuels
  left_join(fuel_names, by = "ProcessSet") %>%
  select(scenario, fuel, units, region, year, capacity)

# Summarise all renewable capacity for TIAM weak 2020
tiam_w2020_all_renewable_capa <- w2020_renewable_capa %>%
  filter(!is.na(fuel)) %>%
  group_by(scenario, region, year, units) %>%
  summarise(capacity = sum(capacity, na.rm = TRUE)) %>%
  mutate(fuel = "Renewables")

# Summarise all generation capacity for TIAM weak 2020
tiam_w2020_all_capa <- w2020_renewable_capa %>%
  group_by(scenario, region, year, units) %>%
  summarise(capacity = sum(capacity, na.rm = TRUE)) %>%
  mutate(fuel = "All")

# Summarise over fuels and merge together datasets for TIAM weak 2020
w2020_renewable_capa %<>%
  # Summarise over fuels
  group_by(scenario, region, year, units, fuel) %>%
  filter(!is.na(fuel) & fuel != "Other renewables") %>%
  summarise(capacity = sum(capacity, na.rm = TRUE)) %>%
  # Merge in other datasets
  bind_rows(tiam_w2020_all_renewable_capa) %>%
  bind_rows(tiam_w2020_all_capa) %>%
  arrange(scenario, region, year, fuel)

# For TIAM weak, keep "P2" scenarios (2020 start), and keep 2dt high feasbility and base for comparability
w2020_renewable_capa %<>%
  filter(!scenario %in% c("s01a_BHP_base", "s01b_BHP_weak2020", "s06a_BHP_cum2dt_Highfeasibility")) %>%
  # Remove 2025 year (blank values)
  filter(year != 2025)

# Join together TIAM datasets
renewable_capa %<>%
  bind_rows(w2020_renewable_capa)

# Sum over regions
renewable_capa %<>%
  group_by(scenario, fuel, units, year) %>%
  summarise(capacity = sum(capacity, na.rm = TRUE)) %>%
  ungroup() %>%
  # Filter extra years
  filter(year <= 2050)

save_dated(renewable_capa, "renewable_capacity", folder = "Interim", csv = TRUE)

# Rename scenarios based on model names
renewable_capa %<>%
  left_join(scenario_names) %>%
  select(model_scenario, year, fuel, units, capacity) %>%
  rename(scenario = model_scenario)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Ad hoc changes to renewable capacity data - please check when updating scenario inputs ----

# Put scenarios in columns before further data cleaning
renewable_capa %<>%
  spread(key = scenario, value = capacity)

# Adjust Paris_INDCs scenario by ratio of old and new database scenarios (2DS_central and Base)
# Value in 2020 should be equal to other scenarios (which follow the old DB Weak 2020 to 2020)
renewable_capa %<>%
  mutate(ratio_2DS_central = (`2DS_central` / `2DS_central_newDB`),
         ratio_base = (BAU / BAU_newDB),
         ratio_adjustment = (ratio_base + ratio_2DS_central) / 2,
         Paris_INDCs = case_when(year == 2020 ~ `2DS_central`,
                                 year != 2020 ~ Paris_INDCs * ratio_adjustment)) %>%
  # Drop ratios and new DB scenarios other than weak 2020
  select(-ratio_base, -ratio_2DS_central, -ratio_adjustment, -BAU_newDB, -`2DS_central_newDB`)

# Clean IEA scenario data and merge in
weo_renewable_total_capa <- weo_raw_renewable_total_capa %>%
  rename(scenario = Scenario,
         fuel = Fuel,
         units = Units) %>%
  mutate(scenario = "WEO BAU") %>%
  # Maintain calculations for 2050 (straight line through 2030, 2040, 2050)
  mutate(`2050` = `2040` + (`2040`- `2030`))

# Reshape IEA scenario data
weo_renewable_total_capa %<>%
  gather(key = "year", value = "capacity", `2015`:`2050`) %>%
  spread(key = "scenario", value = "capacity") %>%
  mutate(year = as.numeric(year),
         fuel = case_when(fuel == "Bioenergy" ~ "Biomass (incl CCS)",
                          TRUE ~ fuel))

# Merge IEA WEO CPS into TIAM scenario data
renewable_capa %<>%
  full_join(weo_renewable_total_capa)

# Fill in wind + solar for TIAM scenarios
renewable_capa %<>%
  gather(key = "scenario", value = "capacity", (`2DS_central`:`WEO BAU`)) %>%
  spread(key = "fuel", value = "capacity") %>%
  mutate(`Wind + Solar` = Wind + Solar) %>%
  gather(key = "fuel", value = "capacity", (All:`Wind + Solar`)) %>%
  spread(key = "scenario", value = "capacity") %>%
  arrange(fuel, year)

# Apply changes in old scenarios spreadsheet
renewable_capa %<>%
  # Interpolate IEA scenario data for 2020
  group_by(fuel) %>%
  mutate(`WEO BAU` = approx(x = year, y = `WEO BAU`, xout = year)$y) %>%
  ungroup() %>%
  select(year:fuel, BAU, everything()) %>%
  # Replace all TIAM scenario values with IEA WEO CPS for years up to 2020
  mutate_at(.vars = vars(`BAU`:`Paris_INDCs`),
            .funs = funs(ifelse(year %in% c(2015, 2016), `WEO BAU`, .))) %>%
  mutate_at(.vars = vars(`2DS_central`:`Paris_INDCs`),
            .funs = funs(ifelse(year == 2020 & fuel != "All", `WEO BAU`, .))) %>%
  # Adjust 2DS and Paris INDC scenario values by difference between WEO BAU and TIAM BAU
  mutate_at(.vars = vars(`2DS_central`:`2DS_delay`),
            .funs = funs(case_when(year > 2020 & fuel != "All" ~ . + (`WEO BAU` - `BAU`),
                                   year >= 2020 & fuel == "All" ~ . + (`WEO BAU` - `BAU`),
                                   TRUE ~ .))) %>%
  mutate(Paris_INDCs = ifelse(year > 2020, Paris_INDCs + (`WEO BAU` - `BAU`), Paris_INDCs)) %>%
  gather(key = "scenario", value = "capacity", `BAU`:`WEO BAU`) %>%
  spread(key = "year", value = "capacity") %>%
  mutate(`Source / status` = case_when(scenario == "WEO BAU" ~ "WEO",
                                       TRUE ~ "TIAM"))

# Rename variables and restructure dataset to match old scenarios spreadsheet (called by JS Green Upside code)
renewable_capa %<>%
  select(`Source / status`, scenario, fuel, units, everything()) %>%
  rename(Scenario = scenario,
         Fuel = fuel,
         Units = units)

# Remap scenarios to match spreadsheet names
renewable_capa %<>%
  mutate(Scenario = case_when(Scenario == "2DS_cheap_ren" ~ "2DS high renewables",
                              Scenario == "2DS_cheap_ccs" ~ "2DS high CCS",
                              Scenario == "2DS_cheap_eff" ~ "2DS high efficiency",
                              Scenario == "2DS_central" ~ "2DS central",
                              Scenario == "2DS_delay" ~ "2DS delayed",
                              TRUE ~ Scenario))

# Ad hoc changes to match spreadsheet
renewable_capa %<>%
  mutate(Fuel = ifelse(Scenario == "WEO BAU" & Fuel == "Biomass (incl CCS)", "Bioenergy", Fuel))

save_dated(renewable_capa, "Renewable_capacity", folder = "Output", csv = TRUE)