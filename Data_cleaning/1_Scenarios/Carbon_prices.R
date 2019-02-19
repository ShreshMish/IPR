##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  19/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script reads in TIAM carbon price data from Excel and cleans it for use in later calculations and modelling
##### Dependencies:       1.  Latest Imperial TIAM scenarios Excel file
#####                     2.  CPI inflation rates

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "1_Scenarios"
source("utils.R")

# Read in TIAM scenario data from old scenarios (skip 6 empty rows at the top of the sheet)
tiam_co2_prices <- read_excel(input_source("Vivid_20190208.xlsx"),
                              sheet = "AA_CarbonPrice", skip = 6)

# Read in TIAM scenario names
tiam_scenario_names <- read_excel(input_source("Vivid_scenario_names.xlsx"),
                                  sheet = "R1. Scenario specifications", range = "$A$10:$D$59")

# Read in sectoral effective carbon price data
sectoral_effective_co2_prices <- read_excel(input_source("Effective_carbon_price_weights.xlsx"),
                                            sheet = "W2. CO2 price by sector", range = "$A$57:$E$63")

# Read in regional effective carbon price data
regional_effective_co2_prices <- read_excel(input_source("Effective_carbon_price_weights.xlsx"),
                                            sheet = "W3. CO2 price by region", range = "$C$7:$G$13")

# Inflation data
cpi_inflation_rates <- read_excel(input_source("CPI_inflation.xlsx"),
                                  sheet = "R1. US CPI inflation", skip = 7)

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean, reshape and interpolate carbon price data (2016-50) ----

# Merge in report scenario names and bin unrequired scenarios
tiam_scenario_names2 <- tiam_scenario_names %>%
  rename(old_scenario_name = `Scenario name`,
         scenario = `Report name`) %>%
  select(old_scenario_name, scenario)

co2_prices <- tiam_co2_prices %>%
  rename(old_scenario_name = Scenario,
         region = `Region\\Period`) %>%
  left_join(tiam_scenario_names2, by = "old_scenario_name") %>%
  filter(!is.na(scenario)) %>%
  filter(region != "GBL") %>% #Exclude Great Britain and Ireland from average carbon price calculations [values often anomalous]
  select(scenario, region, `2020`:`2100`)

# Gather yearly observations
co2_prices2 <- co2_prices %>%
  gather(key = "year", value = "co2_price", `2020`:`2100`) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year <= 2060)

save_dated(co2_prices2, "co2_prices_2005USD_raw", folder = "Interim", csv = TRUE)

#### UPDATE TO MATCH LATEST CODE
# Replace 2020 values with 0 if unavailable (consistent with Excel approach) and replace values with 2005US$/tCO2 (instead of negative 2005US$/ktCO2)
co2_prices3 <- co2_prices2 %>%
  mutate(co2_price = case_when(year == 2020 & is.na(co2_price) ~ 0,
                               TRUE ~ co2_price)) %>%
  # Adjust units from negative dollar
  mutate(co2_price = co2_price * (-1000)) %>%
  # Fill in No_New_Action value carbon prices with 0 when not available
  mutate(co2_price = case_when(scenario == "No_New_Action" & is.na(co2_price) ~ 0,
                               TRUE ~ co2_price))

# Find inflation adjustment for 2005US$ -> 2016US$
cpi_inflation_rates2 <- cpi_inflation_rates %>%
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

us_2005_to_2016_cpi <- cpi_inflation_rates2$cum_cpi_rate[cpi_inflation_rates2$year == 2016]

# Find average across all regions, and adjust all values from 2005US$ to 2016US$
co2_prices4 <- co2_prices3 %>%
  group_by(scenario, year) %>%
  # Summarise over regions
  summarise(co2_price = mean(co2_price, na.rm = TRUE)) %>%
  # Adjust to 2016US$
  mutate(co2_price = co2_price * us_2005_to_2016_cpi) %>%
  ungroup()

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Ad hoc changes to carbon price data - please check when updating scenario inputs ----

# Difference all scenario against 'No_New_Action' so prices are 0 in this scenario and 'relative to BAU' in all other scenarios
co2_prices5 <- co2_prices4 %>% 
  group_by(year) %>%
  mutate(co2_price = co2_price - co2_price[[which(scenario == "Paris_NDCs")]])

# Save wide data
co2_prices_wide <- co2_prices5 %>%
  spread(key = year, value = co2_price) %>%
  arrange(`2020`, desc(scenario))

save_dated(co2_prices_wide, "co2_prices_2016USD_cleaned", folder = "Interim", csv = TRUE)

# Create scenario merge variable for 2DS_regional scenario
co2_prices6 <- co2_prices5 %>%
  rename(co2_price_merge = scenario)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Calculate regional and sectoral convergence factors for carbon prices ----

# Clean regional effective carbon price data
regional_effective_co2_prices2 <- regional_effective_co2_prices %>%
  rename(region = Region,
         ratio_full_regions = `Ratio full regions`,
         ratio_dev_undev_regions = `Ratio developed and developing regions`) %>%
  select(region, ratio_full_regions, ratio_dev_undev_regions)

# Clean sectoral effective carbon price data
sectoral_effective_co2_prices2 <- sectoral_effective_co2_prices %>%
  rename(sector = Sector,
         ratio_full_sectors = `Ratio all sector variation`,
         ratio_road_sectors = `Ratio all except road transport variation`,
         ratio_no_sectors = `Ratio no sector variation`,
         ratio_woroad_sectors = `Ratio exclude road`) %>%
  select(sector, ratio_full_sectors, ratio_road_sectors, ratio_no_sectors, ratio_woroad_sectors)

# Create grid of factors for all combinations of region and sector variable choices
sector_region_factors <- expand.grid(region = unique(regional_effective_co2_prices2$region),
                                     sector = unique(sectoral_effective_co2_prices2$sector), stringsAsFactors = FALSE) %>%
  # Join in the datasets
  left_join(regional_effective_co2_prices2, by = "region") %>%
  left_join(sectoral_effective_co2_prices2, by = "sector") %>%
  # Add dummy variables for region and sector
  mutate(no_regions = 1) %>%
  # Rename variables and construct combinations
  rename_at(.vars = vars(contains("ratio")),
            .funs = funs(gsub("ratio_", "", .))) %>%
  # Create combinations
  mutate(full_reg_full_sect = full_regions * full_sectors,
         full_reg_road_sect = full_regions * road_sectors,
         full_reg_no_sect = full_regions * no_sectors,
         full_reg_woroad_sect = full_regions * woroad_sectors,
         dev_undev_reg_full_sect = dev_undev_regions * full_sectors,
         dev_undev_reg_road_sect = dev_undev_regions * road_sectors,
         dev_undev_reg_no_sect = dev_undev_regions * no_sectors,
         dev_undev_reg_woroad_sect = dev_undev_regions * woroad_sectors,
         no_reg_full_sect = no_regions * full_sectors,
         no_reg_road_sect = no_regions * road_sectors,
         no_reg_no_sect = no_regions * no_sectors,
         no_reg_woroad_sect = no_regions * woroad_sectors)

sector_region_co2_price_weights <- expand.grid(region = unique(regional_effective_co2_prices2$region),
                                               sector = unique(sectoral_effective_co2_prices2$sector),
                                               year = seq(2020, 2100, by = 1), stringsAsFactors = FALSE)

# Define function for mapping convergence between regional, sectoral carbon prices based on input year
# NB names must match substrings defined in combinations section above, and year must be below 2100
sector_region_convergence <- function(initial_regional_variation = NULL, initial_sectoral_variation = NULL, 
                                      final_regional_variation = NULL, final_sectoral_variation = NULL, 
                                      convergence_year = NULL) {
  
  initial_variable <- rlang::sym(paste0(initial_regional_variation, "_", initial_sectoral_variation))
  final_variable <- rlang::sym(paste0(final_regional_variation, "_", final_sectoral_variation))
  
  # Select just the chosen variables
  sector_region_factors2 <- sector_region_factors %>%
    select(region, sector, !!(initial_variable), !!(final_variable)) %>%
    mutate(co2_price_initial_weight = !!(initial_variable),
           co2_price_final_weight = !!(final_variable)) %>%
    select(region, sector, co2_price_initial_weight, co2_price_final_weight)
  
  # Merge into convergence factors dataset and define co2_price_Weight variable
  sector_region_convergence_factors <- sector_region_co2_price_weights %>%
    left_join(sector_region_factors2) %>%
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
                                                       initial_sectoral_variation = "no_sect",
                                                       final_regional_variation = "no_reg",
                                                       final_sectoral_variation = "no_sect",
                                                       convergence_year = 2055) %>%
  mutate(scenario_merge = "Lack_Of_Coordination")

all_other_scenario_weights <- sector_region_convergence(initial_regional_variation = "no_reg",
                                                        initial_sectoral_variation = "no_sect",
                                                        final_regional_variation = "no_reg",
                                                        final_sectoral_variation = "no_sect",
                                                        convergence_year = 2055) %>%
  mutate(scenario_merge = "Other_scenarios")

scenario_weights <- all_other_scenario_weights %>%
  bind_rows(regional_scenario_weights)

save_dated(scenario_weights, "Sector_region_co2_price_weights", folder = "Interim", csv = TRUE)

# Merge in to produce final carbon price dataset
sector_region_co2_prices <- expand.grid(scenario = c(unique(co2_prices6$co2_price_merge), "Lack_Of_Coordination"),
                                        region = unique(regional_effective_co2_prices2$region),
                                        sector = unique(sectoral_effective_co2_prices2$sector),
                                        year = seq(2016, 2100, by = 1)) %>%
  # Convert from factors to strings
  mutate(scenario = as.character(scenario),
         region = as.character(region),
         sector = as.character(sector),
         co2_price_merge = ifelse(scenario == "Lack_Of_Coordination", "2DS_Balanced_Transformation", scenario)) %>%
  left_join(co2_prices6) %>%
  mutate(scenario_merge = case_when(scenario == "Lack_Of_Coordination" ~ "Lack_Of_Coordination",
                                    TRUE ~ "Other_scenarios")) %>%
  left_join(scenario_weights) %>%
  mutate(co2_price = co2_price * co2_price_weight) %>%
  select(scenario, region, sector, year, co2_price) %>%
  filter(sector != "Road") %>%
  filter(year <= 2050)

# Add 2016 variable to match previous dataset and complete interpolation
sector_region_co2_prices2 <- sector_region_co2_prices %>% 
  mutate(co2_price = ifelse(year == 2016, 0, co2_price)) %>%
  group_by(scenario, region, sector) %>%
  mutate(co2_price = approx(x = year, y = co2_price, xout = year)$y) %>%
  # Replace prices before 2020 with 0
  mutate(co2_price = case_when(year < 2020 ~ 0,
                               TRUE ~ co2_price)) %>%
  ungroup() %>%
  arrange(scenario, region, sector, year) %>%
  rename(carbon_price = co2_price)

save_dated(sector_region_co2_prices2, "Model_co2_prices_2016USD", folder = "Output", csv = TRUE)
