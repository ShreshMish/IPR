##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  01/09/2018
##### Code author:        Shyamal Patel
##### Description:        This script reads in TIAM fossil fuel production data from Excel and cleans it for use in later calculations and modelling
##### Dependencies:       1.  Latest Imperial TIAM scenarios Excel file
#####                     2.  IEA WEO scenarios data
#####                     (current scenario data = "Input/Vivid_scenario_runs.xls" & "Input/Vivid_scenario_runs_newdb.xls" (for Weak 2020))

##### TIAM has been updated with new technology costs and constraint changes, which is causing discrepancies between the same 
##### scenario run in different versions - adjust 'weak 2020' scenario from new database to compensate for this

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA ----

packages <- c("tidyverse", "magrittr", "readxl", "here", "stringi", "stringr", "zoo")
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

# Read in TIAM scenario data from old scenarios (skip 6 empty rows at the top of the sheet) - units are PJ
tiam_fossil_fuel_prod <- read_excel("0 - Scenarios/Input/Vivid_scenario_runs.xls",
                                    sheet = "AA_PrimaryEnergyProd", skip = 6)

# Read in TIAM scenario data for weak 2020 scenario (skip 6 empty rows at the top of the sheet) - units are PJ
tiam_w2020_fossil_fuel_prod <- read_excel("0 - Scenarios/Input/Vivid_scenario_runs_newdb.xls",
                                          sheet = "AA_PrimaryEnergyProd", skip = 6)

# Read in IEA WEO scenario data - fossil fuel production (NB - must save a xlsx version of the macro-enabled WEO Annex A workbook) - units are in 'Units' column
weo_raw_fossil_fuel_prod <- read_excel("0 - Scenarios/Input/WEO2017_AnnexA_VE.xlsx",
                                       sheet = "VE Fossil fuel prod", range = "$A$7:$H$10")

# Read in energy unit conversion factors
energy_conversion_factors <- read_excel("0 - Scenarios/Input/Energy_conversion_factors.xlsx",
                                        sheet = "Energy unit conversions", range = "$A$3:$G$8")

# Read in scenario names (defined by Carbon_prices.R script)
scenario_names <- readRDS("0 - Scenarios/Interim/Scenario_name_mapping.rds")

# Rename TIAM fuels to match Vivid descriptions
fuel_names <- tibble(ProcessSet = unique(tiam_fossil_fuel_prod$ProcessSet)) %>%
  mutate(fuel = case_when(ProcessSet == "MIN - COA" ~ "coal",
                          ProcessSet == "MIN - GAS" ~ "gas",
                          ProcessSet == "MIN - OIL" ~ "oil"),
         units = "PJ")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean, reshape and change units of fossil fuel production data (2020 - 50) ----

## Turn energy conversion factors into named vector and add new factors
energy_conversion_factors_data <- as.vector(energy_conversion_factors$Value)
names(energy_conversion_factors_data) <- energy_conversion_factors$Code

# Add new factor required for TIAM oil data - PJ to million barrels
energy_conversion_factors_data["PJ_to_Mb"] <- energy_conversion_factors_data["PJ_to_Mtoe"] / energy_conversion_factors_data["bbl_to_toe"]

# Add new factor required for TIAM gas data - PJ to bcm
energy_conversion_factors_data["PJ_to_bcm"] <- energy_conversion_factors_data["PJ_to_trnBtu"] * energy_conversion_factors_data["trnBtu_to_bcm"]

# Add new factor required for TIAM coal data - PJ to tce
energy_conversion_factors_data["PJ_to_tce"] <- energy_conversion_factors_data["PJ_to_Mtoe"] / energy_conversion_factors_data["tce_to_toe"]

## Gather, clean and rename TIAM scenario data
fossil_fuel_prod <- tiam_fossil_fuel_prod %>%
  gather(key = "year", value = "production", `2005`:`2100`) %>%
  mutate(year = as.numeric(year)) %>%
  rename(scenario = Scenario,
         region = `Region\\Period`) %>%
  # Add units column and rename fuels
  left_join(fuel_names, by = "ProcessSet") %>%
  select(scenario, fuel, units, region, year, production)
  
# Remove surplus scenarios
fossil_fuel_prod %<>%
  filter(!scenario %in% c("s06a_BHP_cum3dt_Highfeasibility", "s06a_BHP_cum4dt_Highfeasibility", "s06a_BHP_cumb2c300_Highfeasibility"))

# Repeat for TIAm weak 2020 scenario data
tiam_w2020_fossil_fuel_prod %<>%
  gather(key = "year", value = "production", `2005`:`2100`) %>%
  mutate(year = as.numeric(year)) %>%
  rename(scenario = Scenario,
         region = `Region\\Period`) %>%
  # Add units column and rename fuels
  left_join(fuel_names, by = "ProcessSet") %>%
  select(scenario, fuel, units, region, year, production)

# For TIAM weak, keep "P2" scenarios (2020 start), and keep 2dt high feasbility and base for comparability
tiam_w2020_fossil_fuel_prod %<>%
  filter(!scenario %in% c("s01a_BHP_base", "s01b_BHP_weak2020", "s06a_BHP_cum2dt_Highfeasibility")) %>%
  # Remove 2025 year (blank values)
  filter(year != 2025)

# Join together TIAM datasets
fossil_fuel_prod %<>%
  bind_rows(tiam_w2020_fossil_fuel_prod)

# Change units to match IEA WEO datasets (starting units are PJ for all fuels)
fossil_fuel_prod %<>% 
  mutate(production = case_when(fuel == "coal" ~ production * energy_conversion_factors_data["PJ_to_tce"],
                                fuel == "oil" ~ production * energy_conversion_factors_data["PJ_to_Mb"],
                                fuel == "gas" ~ production * energy_conversion_factors_data["PJ_to_bcm"]),
         units = case_when(fuel == "coal" ~ "Mtce", # Million tonnes of coal equivalent per annum
                           fuel == "oil" ~ "Mb", # Million barrels of oil per annum
                           fuel == "gas" ~ "bcm")) # Billion cubic metres of natural gas per annum

# Sum over regions
fossil_fuel_prod %<>%
  group_by(scenario, fuel, units, year) %>%
  summarise(production = sum(production, na.rm = TRUE)) %>% 
  ungroup() %>%
  # Filter extra years
  filter(year <= 2050)

save_dated(fossil_fuel_prod, "fossil_fuel_production", folder = "Interim", csv = TRUE)

# Rename scenarios based on model names
fossil_fuel_prod %<>%
  left_join(scenario_names) %>%
  select(model_scenario, year, fuel, units, production) %>%
  rename(scenario = model_scenario)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Ad hoc changes to carbon price data - please check when updating scenario inputs ----

# Put scenarios in columns before further data cleaning
fossil_fuel_prod %<>%
  spread(key = scenario, value = production)

# Adjust Paris_INDCs scenario by ratio of old and new database scenarios (2DS_central and Base)
# Value in 2020 should be equal to other scenarios (which follow the oldDB Weak 2020 to 2020)
fossil_fuel_prod %<>%
  mutate(ratio_2DS_central = (`2DS_central` / `2DS_central_newDB`),
         ratio_base = (BAU / BAU_newDB),
         ratio_adjustment = (ratio_base + ratio_2DS_central) / 2,
         Paris_INDCs = case_when(year == 2020 ~ `2DS_central`,
                                 year != 2020 ~ Paris_INDCs * ratio_adjustment)) %>%
  # Drop ratios and new DB scenarios other than weak 2020
  select(-ratio_base, -ratio_2DS_central, - ratio_adjustment, -BAU_newDB, -`2DS_central_newDB`)

# Clean IEA scenario data and merge in
weo_fossil_fuel_prod <- weo_raw_fossil_fuel_prod %>%
  rename(scenario = Scenario,
         fuel = Product,
         units = Units) %>%
  mutate(scenario = "WEO_CPS") %>%
  # Maintain calculations for 2050 (straight line through 2030, 2040, 2050)
  mutate(`2050` = `2040` + (`2040` - `2030`)) %>%
  # Change fuel names to lower case
  mutate(fuel = str_to_lower(fuel))

# Reshape IEA scenario data
weo_fossil_fuel_prod %<>%
  gather(key = "year", value = "production", `2015`:`2050`) %>%
  spread(key = "scenario", value = "production") %>%
  # Change units of oil (million barrels per day, to million barrels)
  mutate(year = as.numeric(year),
         WEO_CPS = ifelse(fuel == "oil", WEO_CPS * 365, WEO_CPS),
         units = case_when(fuel == "coal" ~ "Mtce",
                           fuel == "oil" ~ "Mb",
                           fuel == "gas" ~ "bcm"))

# Merge IEA WEO CPS into TIAM scenario data
fossil_fuel_prod %<>%
  full_join(weo_fossil_fuel_prod) %>%
  arrange(fuel, year)

# Apply changes made in old scenarios spreadsheet
fossil_fuel_prod %<>%
  filter(year >= 2016) %>%
  # Interpolate IEA scenario data for 2020
  group_by(fuel) %>%
  mutate(WEO_CPS = approx(x = year, y = WEO_CPS, xout = year)$y) %>%
  ungroup() %>%
  # Replace all TIAM scenario values with IEA WEO CPS for years up to 2020
  mutate_at(.vars = vars(`2DS_central`:`Paris_INDCs`),
            .funs = funs(ifelse(year <= 2020, WEO_CPS, .))) %>%
  # Replace 2DS_delay scenario values with IEA WEO CPS for years up to 2030
  mutate(`2DS_delay` = ifelse(year > 2020 & year <= 2030, WEO_CPS, `2DS_delay`)) %>%
  # Replace BAU scenario values with IEA WEO CPS for all years
  mutate(BAU = ifelse(year > 2020, WEO_CPS, BAU))

# Expand grid to cover 2035, 2045
fossil_fuel_prod <- expand.grid(year = c(2016, seq(2020, 2050, by = 5)),
                                fuel = unique(fossil_fuel_prod$fuel)) %>%
  left_join(fossil_fuel_prod) %>%
  # Fill in units
  group_by(fuel) %>%
  fill(units) %>%
  # Interpolate scenario quantities to fill missing years
  mutate_at(.vars = vars(`2DS_central`:`Paris_INDCs`),
            .funs = funs(approx(x = year, y = ., xout = year)$y)) %>%
  ungroup() %>%
  # Cap INDCs scenario at BAU values
  mutate(Paris_INDCs = ifelse(Paris_INDCs > BAU, BAU, Paris_INDCs))

# Drop WEO_CPS and reshape
fossil_fuel_prod %<>%
  select(-WEO_CPS) %>%
  gather(key = "scenario", value = "production", `2DS_central`:`Paris_INDCs`) %>%
  select(scenario, fuel, units, year, production) %>%
  arrange(scenario, fuel, year)
         
# Change units to match spreadsheet outputs
fossil_fuel_prod %<>%
  mutate(production = case_when(fuel == "gas" ~ production * 10^9,
                                fuel == "oil" ~ production * 10^6,
                                fuel == "coal" ~ production * 10^6)) %>%
  mutate(units = case_when(fuel == "gas" ~ "cm",
                           fuel == "oil" ~ "barrels",
                           fuel == "coal" ~ "tonnes"))

save_dated(fossil_fuel_prod, "Fossil_fuel_production", folder = "Output", csv = TRUE)