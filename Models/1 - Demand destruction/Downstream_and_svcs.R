##### Project code:       171211HSB - Low Carbon Portfolio
##### Date of last edit:  14/01/2019
##### Code author:        Shyamal Patel
##### Description:        This script imports outputs from company-level demand destruction modelling for use in sector-level
#####                     fossil fuel value chain value impairment estimation (downstream sectors, oil & gas services etc.)
##### Dependencies:       Results from the EDA_alpha_Price_F_Prod_adj_SP.R script:
#####                       a) DD stranding data compact.rds
#####                       b) DD full raw dataset.rds

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
save_dated <- function(data, name, folder, dated = TRUE, csv = FALSE) {
  main_path <- paste0("1 - Demand destruction/", folder)
  dated_path <- paste0("1 - Demand destruction/", folder, "/Dated/")
  
  # Save main file
  saveRDS(data, here(main_path, paste0(name, ".rds")))
  # Save dated file (optional)
  if(dated == TRUE) {saveRDS(data, here(dated_path, paste0(date, "_", name, ".rds")))}
  # Save dated CSV file (optional)
  if(csv == TRUE) {write_csv(data, here(dated_path, paste0(date, "_", name, ".csv")))}
}

# Discount rate should be 0.0575 (2% inflation rate adjustment does not apply)
discount_rate <- 0.0575

# Read in oil & gas demand destruction model results
fossil_fuel_dd_quantities <- readRDS("1 - Demand destruction/Interim/DD_stranding_data_compact.rds")

# Read in oil & gas demand destruction full dataset (contains 2016 quantities)
fossil_fuel_dd_quantities_raw <- readRDS("1 - Demand destruction/Interim/DD_full_raw_dataset.rds")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Find quantity contraction in oil and gas production for related industries stranding impact
#                 (refineries, pipelines and service companies)

# Clean DD stranding data compact dataset
fossil_fuel_dd_quantities %<>%
  rename(ISIN_code = ISIN_Code,
         company = VE_Name,
         product = Product,
         year = Year,
         scenario = Scenario) %>%
  select(scenario, ISIN_code, company, Rystad_Name, product, year, Production_G) %>%
  filter(product != "All")

# Clean fossil fuel DD quantities dataset containing 2016 data
fossil_fuel_dd_quantities_raw %<>%
  select(Scenario, Company, Product, Year, Production_G, Price_G, Production_F, Price_F) %>%
  rename(Rystad_Name = Company,
         product = Product,
         year = Year,
         scenario = Scenario) %>%
  filter(scenario == "Central") %>%
  mutate(product = ifelse(product == "Gas", "Gas", "Liquid"))

# Create dataset with 2016 prices
fossil_fuel_2016_prices <- fossil_fuel_dd_quantities_raw %>%
  select(scenario, product, year, Price_G) %>% 
  filter(year == 2016) %>%
  select(-year, -scenario) %>%
  unique() %>%
  rename(Price_G_2016 = Price_G)

# Build quantity impact dataset at industry-level (2016 prices used to weight oil and gas)
industry_level_quantity_impact <- fossil_fuel_dd_quantities %>%
  filter(!is.na(Production_G)) %>%
  select(scenario, product, year, Production_G) %>%
  unique() %>%
  arrange(product, scenario, year) %>%
  left_join(fossil_fuel_2016_prices)

# Calculate industry level quantity impacts in NPV terms (weights on oil and gas are 2016 prices)
industry_level_quantity_impact %<>%
  mutate(Revenue_G = Production_G * Price_G_2016) %>%
  group_by(scenario, product, year) %>% 
  summarise(Quantity_hat_G = sum(Revenue_G)) %>%
  ungroup() %>%
  # Flat around reported year value interpolation approach
  mutate(NPV_weight = case_when(year == 2020 ~ 0.9,
                                year == 2050 ~ 0.5,
                                TRUE ~ 1),
         NPV_discount_years = year - 2018,
         NPV_quantity_hat_G = Quantity_hat_G / ((1 + discount_rate) ^ NPV_discount_years))

save_dated(industry_level_quantity_impact, "Oil_and_gas_quantity_impact_full", folder = "Interim", csv = "YES")

# Calculate % NPV impact at the fuel level
fuel_industry_level_quantity_impact <- industry_level_quantity_impact %>%
  group_by(scenario, product) %>%
  summarise(NPV_quantity = sum(NPV_quantity_hat_G)) %>%
  group_by(product) %>%
  mutate(BAU_NPV_quantity = mean(ifelse(scenario == "BAU", NPV_quantity, NA_real_), na.rm = TRUE),
         perc_NPV_quantity_impact = (BAU_NPV_quantity - NPV_quantity) / BAU_NPV_quantity)

# Summarise overall (oil + gas) results and calculate % NPV impact
overall_industry_level_quantity_impact <- industry_level_quantity_impact %>%
  group_by(scenario) %>%
  summarise(NPV_quantity = sum(NPV_quantity_hat_G)) %>%
  ungroup() %>%
  mutate(BAU_NPV_quantity = mean(ifelse(scenario == "BAU", NPV_quantity, NA_real_), na.rm = TRUE),
         perc_NPV_quantity_impact = (BAU_NPV_quantity - NPV_quantity) / BAU_NPV_quantity) %>%
  mutate(product = "All")

# Merge results together and save final file
industry_level_quantity_impact <- fuel_industry_level_quantity_impact %>%
  bind_rows(overall_industry_level_quantity_impact) %>%
  arrange(scenario, product) %>%
  select(scenario, product, perc_NPV_quantity_impact)

save_dated(industry_level_quantity_impact, "Oil_and_gas_quantity_contraction", folder = "Output", csv = "YES")
