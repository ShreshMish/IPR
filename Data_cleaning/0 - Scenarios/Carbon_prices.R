##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  02/09/2018
##### Code author:        Shyamal Patel
##### Description:        This script reads in TIAM carbon price data from Excel and cleans it for use in later calculations and modelling
##### Dependencies:       1.  Latest Imperial TIAM scenarios Excel file
#####                     2.  CPI inflation rates
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

# Read in TIAM scenario data from old scenarios (skip 6 empty rows at the top of the sheet)
tiam_co2_prices <- read_excel("0 - Scenarios/Input/Vivid_scenario_runs.xls",
                              sheet = "AA_CarbonPrice", skip = 6)

# Read in TIAM scenario data for weak 2020 scenario (skip 6 empty rows at the top of the sheet)
tiam_w2020_co2_prices <- read_excel("0 - Scenarios/Input/Vivid_scenario_runs_newdb.xls",
                                    sheet = "AA_CarbonPrice", skip = 6)

# Read in sectoral effective carbon price data
sectoral_effective_co2_prices <- read_excel("0 - Scenarios/Input/Effective_carbon_price_weights.xlsx",
                                            sheet = "W2. CO2 price by sector", range = "$A$47:$D$53")

# Read in regional effective carbon price data
regional_effective_co2_prices <- read_excel("0 - Scenarios/Input/Effective_carbon_price_weights.xlsx",
                                       sheet = "W3. CO2 price by region", range = "$C$7:$G$13")

# Inflation data
cpi_inflation_rates <- read_excel("0 - Scenarios/Input/CPI_inflation.xlsx",
                                  sheet = "R1. US CPI inflation", skip = 7)

# Define scenario name mapping between current TIAM scenarios, and model names
scenario_names <- tribble(~scenario, ~model_scenario,
                          "s06a_BHP_cum2dt_Highfeasibility",      "2DS_central",
                          "s02_BHP_cum2dt_highREN_EV",            "2DS_cheap_ren",
                          "s03_BHP_cum2dt_highCCS_EV",            "2DS_cheap_ccs",
                          "s04_BHP_cum2dt_lowDEM_EV",             "2DS_cheap_eff",
                          "s06c_BHP_cum2dt_hf120v2_delayed2030",  "2DS_delay",
                          "s01a_BHP_base",                        "BAU",
                          "s01a_BHP_base_12P2",                   "BAU_newDB",
                          "s01b_BHP_weak2020_12P2",               "Paris_INDCs",
                          "s06a_BHP_cum2dt_Highfeasibility_12P2", "2DS_central_newDB",
                          "N/A",                                  "2DS_regional")

# Save scenario names
save_dated(scenario_names, "Scenario_name_mapping", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean, reshape and interpolate carbon price data (2016-50) ----

# Gather yearly observations
co2_prices <- tiam_co2_prices %>%
  gather(key = "year", value = "co2_price", `2020`:`2100`) %>%
  rename(scenario = Scenario,
         region = `Region\\Period`) %>%
  mutate(year = as.numeric(year))

# Remove surplus scenarios
co2_prices %<>%
  filter(!scenario %in% c("s06a_BHP_cum3dt_Highfeasibility", "s06a_BHP_cum4dt_Highfeasibility", "s06a_BHP_cumb2c300_Highfeasibility"))

# Repeat for TIAM weak 2020 scenario data
tiam_w2020_co2_prices %<>%
  gather(key = "year", value = "co2_price", `2020`:`2100`) %>%
  rename(scenario = Scenario,
         region = `Region\\Period`) %>%
  mutate(year = as.numeric(year))

# For TIAM weak, keep "P2" scenarios (2020 start), and keep 2dt high feasibility for comparability
tiam_w2020_co2_prices %<>%
  filter(!scenario %in% c("s01b_BHP_weak2020", "s06a_BHP_cum2dt_Highfeasibility"))

# Join together TIAM datasets
co2_prices %<>%
  bind_rows(tiam_w2020_co2_prices)

save_dated(co2_prices, "co2_prices_2005USD", folder = "Interim", csv = TRUE)

# Replace 2020 values with 0 if unavailable (consistent with Excel approach) and replace values with 2005US$/tCO2 (instead of negative 2005US$/ktCO2)
co2_prices %<>%
  mutate(co2_price = case_when(year == 2020 & is.na(co2_price) ~ 0,
                               TRUE ~ co2_price)) %>%
  # Adjust units from negative dollar
  mutate(co2_price = co2_price * (-1000))

# Find inflation adjustment for 2005US$ -> 2016US$
cpi_inflation_rates %<>%
  filter(`Country Code` == "USA") %>%
  select(`Country Code`, `1960`:`2016`) %>%
  rename(iso_code = `Country Code`) %>%
  # Reshape data to have years in a single column
  gather(key = "year", value = "cpi_rate", -iso_code) %>%
  mutate(year = as.numeric(year)) %>%
  # Filter down to 2005 onwards
  filter(year >= 2005) %>%
  arrange(year) %>%
  # Replace 2005 CPI rate with 0% inflation (1) for cumulative product calculations (replace rate with 1 + cpi_rate / 100 for calculations)
  mutate(cpi_rate = ifelse(year == 2005, 1, 1 + cpi_rate / 100),
         cum_cpi_rate = cumprod(cpi_rate))

us_2005_to_2016_cpi <- cpi_inflation_rates$cum_cpi_rate[cpi_inflation_rates$year == 2016]

# Find average across all regions, and adjust all values from 2005US$ to 2016US$
co2_prices %<>%
  group_by(scenario, year) %>%
  # Summarise over regions
  summarise(co2_price = mean(co2_price, na.rm = TRUE)) %>%
  # Adjust to 2016US$
  mutate(co2_price = co2_price * us_2005_to_2016_cpi) %>%
  ungroup()

save_dated(co2_prices, "co2_prices_2016USD", folder = "Interim", csv = TRUE)

# Rename scenarios based on model names
co2_prices %<>%
  left_join(scenario_names) %>%
  select(model_scenario, year, co2_price) %>%
  rename(scenario = model_scenario)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Ad hoc changes to carbon price data - please check when updating scenario inputs ----

# Put scenarios in columns before further data cleaning
co2_prices %<>%
  spread(key = scenario, value = co2_price)

# Adjust Paris_INDCs scenario by ratio of 2DS_central to 2DS_central_newDB
# Value in 2020 should be equal to other scenarios (which follow the oldDB Weak 2020 to 2020)
co2_prices %<>%
  mutate(Paris_INDCs = case_when(year == 2020 ~ `2DS_central`,
                                 year != 2020 ~ Paris_INDCs * (`2DS_central` / `2DS_central_newDB`))) %>%
  # Drop new database 2DS scenario now
  select(-`2DS_central_newDB`) %>%
  # Hold Weak_2020 constant after 2030
  mutate(Paris_INDCs_2030_value = mean(ifelse(year == 2030, Paris_INDCs, NA_real_), na.rm = TRUE),
         Paris_INDCs = case_when(year >= 2040 ~ Paris_INDCs_2030_value,
                                 TRUE ~ Paris_INDCs)) %>%
  select(-Paris_INDCs_2030_value)

# Replace 2DS delay scenario carbon prices with 0 up to 2030
# NB - this should really be weak 2020 until 2030
co2_prices %<>%
  mutate(`2DS_delay` = case_when(year <= 2030 ~ 0,
                                 TRUE ~ `2DS_delay`)) %>%
  filter(year != 2025)

# Add in BAU
co2_prices %<>%
  mutate(BAU = 0)

# Gather data again
co2_prices %<>%
  gather(key = scenario, value = co2_price, -year) %>%
  select(scenario, year, co2_price)

# Save wide data
co2_prices_wide <- co2_prices %>%
  spread(key = year, value = co2_price) %>%
  arrange(`2020`, desc(scenario))

save_dated(co2_prices_wide, "co2_prices_2016USD_cleaned", folder = "Interim", csv = TRUE)

# Create scenario merge variable for 2DS_regional scenario
co2_prices %<>%
  rename(co2_price_merge = scenario)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Calculate regional and sectoral convergence factors for carbon prices ----

# Clean regional effective carbon price data
regional_effective_co2_prices %<>%
  rename(region = Region,
         ratio_full_regions = `Ratio full regions`,
         ratio_dev_undev_regions = `Ratio developed and developing regions`) %>%
  select(region, ratio_full_regions, ratio_dev_undev_regions)

# Clean sectoral effective carbon price data
sectoral_effective_co2_prices %<>%
  rename(sector = Sector,
         ratio_full_sectors = `Ratio all sector variation`,
         ratio_nonroad_sectors = `Ratio all except road transport variation`) %>%
  select(sector, ratio_full_sectors, ratio_nonroad_sectors)

# Create grid of factors for all combinations of region and sector variable choices
sector_region_factors <- expand.grid(region = unique(regional_effective_co2_prices$region),
                                     sector = unique(sectoral_effective_co2_prices$sector)) %>%
  # Join in the datasets
  left_join(regional_effective_co2_prices) %>%
  left_join(sectoral_effective_co2_prices) %>%
  # Add dummy variables for region and sector
  mutate(no_regions = 1,
         no_sectors = 1) %>%
  # Rename variables and construct combinations
  rename_at(.vars = vars(contains("ratio")),
            .funs = funs(gsub("ratio_", "", .))) %>%
  # Create combinations
  mutate(full_reg_full_sect = full_regions * full_sectors,
         full_reg_nonrd_sect = full_regions * nonroad_sectors,
         full_reg_no_sect = full_regions * no_sectors,
         dev_undev_reg_full_sect = dev_undev_regions * full_sectors,
         dev_undev_reg_nonrd_sect = dev_undev_regions * nonroad_sectors,
         dev_undev_reg_no_sect = dev_undev_regions * no_sectors,
         no_reg_full_sect = no_regions * full_sectors,
         no_reg_nonrd_sect = no_regions * nonroad_sectors,
         no_reg_no_sect = no_regions * no_sectors)

sector_region_co2_price_weights <- expand.grid(region = unique(regional_effective_co2_prices$region),
                                               sector = unique(sectoral_effective_co2_prices$sector),
                                               year = seq(2020, 2100, by = 1))

# Define function for mapping convergence between regional, sectoral carbon prices based on input year
# NB names must match substrings defined in combinations section above, and year must be below 2100
sector_region_convergence <- function(initial_regional_variation = NULL, initial_sectoral_variation = NULL, 
                                      final_regional_variation = NULL, final_sectoral_variation = NULL, 
                                      convergence_year = NULL) {
  
  initial_variable <- quote(paste0(initial_regional_variation, "_", initial_sectoral_variation))
  final_variable <- quote(paste0(final_regional_variation, "_", final_sectoral_variation))
  
  # Select just the chosen variables
  sector_region_factors %<>%
    select(region, sector, eval(initial_variable), eval(final_variable)) %>%
    rename(co2_price_initial_weight = eval(initial_variable),
           co2_price_final_weight = eval(final_variable))
  
  # Merge into convergence factors dataset and define co2_price_Weight variable
  sector_region_convergence_factors <- sector_region_co2_price_weights %>%
    left_join(sector_region_factors) %>%
    mutate(co2_price_weight = case_when(year >= convergence_year ~ co2_price_final_weight,
                                        year == 2020 ~ co2_price_initial_weight)) %>%
    group_by(region, sector) %>%
    # Linearly interpolate other values
    mutate(co2_price_weight = approx(x = year, y = co2_price_weight, xout = year)$y) %>%
    select(-co2_price_initial_weight, -co2_price_final_weight) %>%
    ungroup() %>%
    arrange(region, sector, year)

  return(sector_region_convergence_factors)

}

# Regional scenario weights - add variable representing merge scenario variable
regional_scenario_weights <- sector_region_convergence(initial_regional_variation = "full_reg",
                                                       initial_sectoral_variation = "full_sect",
                                                       final_regional_variation = "no_reg",
                                                       final_sectoral_variation = "nonrd_sect",
                                                       convergence_year = 2055) %>%
  mutate(scenario_merge = "2DS_regional")

all_other_scenario_weights <- sector_region_convergence(initial_regional_variation = "no_reg",
                                                        initial_sectoral_variation = "full_sect",
                                                        final_regional_variation = "no_reg",
                                                        final_sectoral_variation = "nonrd_sect",
                                                        convergence_year = 2055) %>%
  mutate(scenario_merge = "Other_scenarios")

scenario_weights <- all_other_scenario_weights %<>%
  bind_rows(regional_scenario_weights)

save_dated(scenario_weights, "Sector_region_co2_price_weights", folder = "Interim", csv = TRUE)

# Merge in to produce final carbon price dataset
sector_region_co2_prices <- expand.grid(scenario = c(unique(co2_prices$co2_price_merge), "2DS_regional"),
                                        region = unique(regional_effective_co2_prices$region),
                                        sector = unique(sectoral_effective_co2_prices$sector),
                                        year = seq(2016, 2100, by = 1)) %>%
  # Convert from factors to strings
  mutate(scenario = as.character(scenario),
         region = as.character(region),
         sector = as.character(sector),
         co2_price_merge = ifelse(scenario == "2DS_regional", "2DS_central", scenario)) %>%
  left_join(co2_prices) %>%
  mutate(scenario_merge = case_when(scenario == "2DS_regional" ~ "2DS_regional",
                                    TRUE ~ "Other_scenarios")) %>%
  left_join(scenario_weights) %>%
  mutate(co2_price = co2_price * co2_price_weight) %>%
  select(scenario, region, sector, year, co2_price) %>%
  filter(year <= 2050)

# Add 2016 variable to match previous dataset and complete interpolation
sector_region_co2_prices %<>% 
  mutate(co2_price = ifelse(year == 2016, 0, co2_price)) %>%
  group_by(scenario, region, sector) %>%
  mutate(co2_price = approx(x = year, y = co2_price, xout = year)$y) %>%
  # Replace prices before 2020 with 0
  mutate(co2_price = case_when(year < 2020 ~ 0,
                               TRUE ~ co2_price)) %>%
  ungroup() %>%
  arrange(scenario, region, sector, year) %>%
  rename(carbon_price = co2_price)

save_dated(sector_region_co2_prices, "Model_co2_prices_2016USD", folder = "Output", csv = TRUE)
