##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  16/10/2018
##### Code author:        Shyamal Patel
##### Description:        This script reads in TIAM scenario data, and coal supply curve data from Dexter Lee's analysis
#####                     in preparation for later modelling on fossil fuel demand destruction
##### Dependencies:       1.  Latest Thomson Reuters cleaned dataset: "1 - Financial prelim/Output/TR_cleaned_2016USD_data.xlsx"
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA

packages <- c("tidyverse", "magrittr", "readxl", "here", "stringi")
lapply(packages, require, character.only = TRUE)

# Define date for save file names
day <- format(Sys.time(), "%d")
month <- match(format(Sys.time(), "%b"), month.abb)
if(nchar(month) == 1) {month <- paste0("0", month)}
year <- substr(format(Sys.time(), "%Y"), 3, 4)
date <- paste0(year, month, day)

# These functions count the number of missing or zero-value observations in a tibble
na_counter <- function(x) {sum(is.na(x))}
zero_counter <- function(x) {sum(ifelse(x == 0, 1, 0))}

# These functions save base, and dated files in the Interim or Output folder for later use
# Dated files are kept for version control purposes
save_dated <- function(data, name, folder, dated = "YES", csv = "NO") {
  main_path <- paste0("4 - Fossil fuels/Coal/", folder)
  dated_path <- paste0("4 - Fossil fuels/Coal/", folder, "/Dated/")
  
  # Save main file
  saveRDS(data, here(main_path, paste0(name, ".rds")))
  # Save dated file (optional)
  if(dated == "YES") {saveRDS(data, here(dated_path, paste0(date, "_", name, ".rds")))}
  # Save dated CSV file (optional)
  if(csv == "YES") {write_csv(data, here(dated_path, paste0(date, "_", name, ".csv")))}
}

# Read in coal scenario data (TIAM / TIMES IAM)
scenario_data <- readRDS("0 - Scenarios/Output/Fossil_fuel_production.rds")

# Read in seaborne coal supply curve data (Dexter's research - Macquarie curve)
supply_curve_data <- read_excel("4 - Fossil fuels/Coal/Input/Coal supply curve data.xlsx",
                                     sheet = "R2. Seaborne CC 16", range = "$A$8:$C$34")

# Read in coal mine company-level data (Dexter's research)
company_exposure_data <- read_excel("4 - Fossil fuels/Coal/Input/Coal supply curve data.xlsx",
                                    sheet = "R1. Company coal data", range = "$A$9:$R$100")

# Read in ISIN code matching
company_ISIN_match_data <- read_excel("4 - Fossil fuels/Coal/Input/Coal supply curve data.xlsx",
                                      sheet = "R0. ISIN code matching", range = "$A$8:$C$38")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean scenario and supply curve datasets

# Scenario data
scenario_data %<>%
  rename(production_g = production) %>%
  filter(fuel == "coal") %>%
  select(-units, -fuel) %>%
  mutate(production_g = production_g / 10^6) # Change supply curve production units from tonnes to Mtonnes

save_dated(scenario_data, "Scenario_quantities", folder = "Output", csv = "YES")

# Seaborne supply curve data
supply_curve_data_2016 <- supply_curve_data %>%
  rename(seaborne_regional_production = `Volume (Mt)`,
         cost = `Price ($/t)`, # Coal cost (supply price) - units are 2016US$
         region = `Country/Region`) %>%
  select(region, everything()) %>%
  # Rename Indonesian coal categories and ROW
  mutate(region = case_when(grepl("Indo", region) ~ "Indonesia",
                            region == "Other" ~ "Rest of World",
                            TRUE ~ region)) %>%
  arrange(region, cost, seaborne_regional_production) %>%
  # Create IDs for each mine
  mutate(mine_ID = rank(cost, ties.method = "first"))

# Store BAU scenario quantities
BAU_scenario_quantities <- scenario_data %>%
  filter(scenario == "BAU")

# Add to seaborne supply curve data and scale supply curve to cover BAU quantity
supply_curve_data_2016 %<>%
  mutate(seaborne_production = sum(seaborne_regional_production),
         all_production = BAU_scenario_quantities$production_g[BAU_scenario_quantities$year == 2016], # Change units to Mtonnes
         regional_production = seaborne_regional_production * (all_production / seaborne_production))

save_dated(supply_curve_data_2016, "Seaborne_cost_curve_full_2016", folder = "Interim", csv = "YES")

# Filter out unnecessary variables from seaborne cost curve
supply_curve_data_2016 %<>%
  select(mine_ID, region, cost, regional_production)

# Expand grid to create all years dataset
supply_curve_data_allyrs <- expand.grid(mine_ID = unique(supply_curve_data_2016$mine_ID),
                                        year = BAU_scenario_quantities$year) %>%
  left_join(BAU_scenario_quantities) %>%
  left_join(supply_curve_data_2016)

# Scale production quantities to fit global BAU production values in each year
supply_curve_data_allyrs %<>%
  group_by(year) %>%
  mutate(production_2016 = sum(regional_production),
         regional_production = regional_production * (production_g / production_2016)) %>%
  select(-production_2016) %>%
  arrange(scenario, year, cost) %>%
  mutate(production_cumulative = cumsum(regional_production)) %>%
  ungroup() %>%
  select(-production_g, -scenario)

save_dated(supply_curve_data_allyrs, "Coal_cost_curve_full", folder = "Output", csv = "YES")

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Clean company data

# Match company names to ISIN codes, and drop redundant variables
company_ISIN_match_data %<>%
  rename(Company = `R1 name`,
         company = `TR name`,
         ISIN_code = `ISIN code`)

# Rename variables from company exposure data
company_exposure_data %<>%
  left_join(company_ISIN_match_data) %>%
  select(ISIN_code, company, everything()) %>%
  select(-Company) %>%
  rename(country = Country,
         mine = Mine,
         observation_year = `Data year (latest available if multiple)`,
         perc_ownership = `Ownership (%)`,
         currency_units = `Units: currency`,
         production = `Production`,
         price = Price,
         revenue = `Revenue (millions)`,
         cost = Cost,
         reserve_life = `Resource / reserve life (yrs)`) %>%
  select(ISIN_code, company, country, mine, observation_year, perc_ownership,
         currency_units, production, price, revenue, cost, reserve_life)

# Replace N/A with missing values, and change type of numeric columns
company_exposure_data %<>%
  mutate_at(.vars = vars(everything()),
            .funs = funs(case_when(. == "N/A" ~ NA_character_,
                                   TRUE ~ .))) %>%
  mutate_at(.vars = vars(observation_year, perc_ownership, production,
                         price, revenue, cost, reserve_life),
            .funs = funs(as.numeric(.)))

save_dated(company_exposure_data, "Company_coal_data_raw", folder = "Interim", csv = "YES")

## Ad hoc changes to the data

# Remove ISIN_codes not associated with coal producers
company_exposure_data %<>%
  # Rio Tinto is no longer a coal producer
  filter(ISIN_code != "AU000000RIO1" & ISIN_code != "GB0007188757") %>%
  # Shaanxi Xishan is a coking coal company
  filter(ISIN_code != "CNE0000013Y5") %>%
  # Shenergy - oil and gas exposure clear, coal exposure unclear
  filter(ISIN_code != "CNE0000005Q7") %>%
  # Hubei Energy Group - utility rather than coal company
  filter(ISIN_code != "CNE000000750")

# Remove blank entries in mine column (both Glencore's)
company_exposure_data %<>%
  filter(!(ISIN_code == "JE00B4T3BW64" & is.na(mine)))

# Add missing companies which have exposure, but no asset-level data
company_exposure_data %<>%
  # Astra International (Indonesian conglomerate with exposure to coal mining)
  mutate(production = case_when(ISIN_code == "ID1000122807" ~ 1, TRUE ~ production),
         mine = case_when(ISIN_code == "ID1000122807" ~ "Dummy", TRUE ~ mine)) %>%
  # CEZ Group (Czech conglomerate with exposure to coal mining)
  mutate(production = case_when(ISIN_code == "CZ0005112300" ~ 1, TRUE ~ production),
         mine = case_when(ISIN_code == "CZ0005112300" ~ "Dummy", TRUE ~ mine)) %>%
  # Inversiones Argos (Grupo Argos is a Colombian conglomerate involved in coal mining)
  mutate(production = case_when(ISIN_code == "COT09PA00035" ~ 1, TRUE ~ production),
         mine = case_when(ISIN_code == "COT09PA00035" ~ "Dummy", TRUE ~ mine)) %>%
  # Idemitsu Kosan (Japanese energy conglomerate with coal mining assets in Australia)
  mutate(production = case_when(ISIN_code == "JP3142500002" ~ 1, TRUE ~ production),
         mine = case_when(ISIN_code == "JP3142500002" ~ "Dummy", TRUE ~ mine))

# Fill in missing values with defaults
company_exposure_data %<>%
  mutate(observation_year = case_when(is.na(observation_year) ~ 2017,
                                      TRUE ~ observation_year),
         # Assume 100% ownership if no values are reported
         perc_ownership = case_when(is.na(perc_ownership) ~ 100,
                                    TRUE ~ perc_ownership),
         # Assume reserve life is equal to the industry-average if no values are reported
         reserve_life = case_when(is.na(reserve_life) ~ mean(reserve_life, na.rm = TRUE),
                                  TRUE ~ reserve_life))

# Calculate production shares for each ISIN_code
company_exposure_data %<>%
  group_by(ISIN_code) %>%
  # Calculate each mine's share of production based on existing production
  mutate(production_share = production * perc_ownership / sum(production * perc_ownership, na.rm = TRUE)) %>%
  # Fill in missing observations with the company average
  mutate(production_share = case_when(is.na(production_share) & n() == 1 ~ 1, # NA value company's with just one mine,
                                      is.na(production_share) ~ mean(production_share, na.rm = TRUE), # Companies with >1 mine with production data, and >1 mine with no production data
                                      TRUE ~ production_share)) %>%
  # Rebase each mine's share of production based on new values
  mutate(production_share = production_share / sum(production_share)) %>%
  ungroup() %>%
  # Remove redundant variables
  select(ISIN_code:observation_year, production_share, reserve_life)

# Replace reserve life with 2 if current value is 1
company_exposure_data %<>%
  mutate(reserve_life = case_when(reserve_life < 2 ~ 2,
                                  TRUE ~ reserve_life))

save_dated(company_exposure_data, "Company_coal_data_cleaned", folder = "Output", csv = "YES")
