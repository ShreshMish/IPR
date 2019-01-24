##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  09/08/2018
##### Code author:        Shyamal Patel
##### Description:        This script generates MAC curve data (units already 2016US$ following Excel work)
##### Dependencies:       1.  Latest Mac curve data Excel file: "3 - MAC curves/Input/MAC curve data.xlsx"
#####                         Older files can be found in the ".../Dated/" folder (if any)

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
  main_path <- paste0("3 - MAC curves/", folder)
  dated_path <- paste0("3 - MAC curves/", folder, "/Dated/")
  
  # Save main file
  saveRDS(data, here(main_path, paste0(name, ".rds")))
  # Save dated file (optional)
  if(dated == "YES") {saveRDS(data, here(dated_path, paste0(date, "_", name, ".rds")))}
  # Save dated CSV file (optional)
  if(csv == "YES") {write_csv(data, here(dated_path, paste0(date, "_", name, ".csv")))}
}

# All abatement cost data is in 2016US$/tCO2, abatement potential given as a % of total emissions
mac_data_raw <- read_excel("3 - MAC curves/Input/MAC curve data.xlsx", sheet = "W5. Panel data for R export",
                           range = "$A$5:$I$4961", col_names = TRUE)

save_dated(mac_data_raw, "MAC_curve_raw_data", folder = "Interim")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Extending dataset to cover all model years and replacing negative values

# Replace negative abatement potential levers with 0 abatement potential
mac_data_cleaned <- mac_data_raw %>%
  mutate(abatement_potential = ifelse(abatement_potential < 0, 0, abatement_potential),
         sector_lever = paste(sector_code, lever, sep = " "))

# Generate dataset of missing year values to combine with existing dataset
years <- seq(2015, 2050, 1)
years <- subset(years, subset = !(years %in% mac_data_cleaned$year))
regions <- unique(mac_data_cleaned$region_code)
sectors <- unique(mac_data_cleaned$sector_lever)

mac_data_new_years <- expand.grid(regions, sectors, years) %>%
  rename(region_code = Var1, sector_lever = Var2, year = Var3)

# Merge into existing values and create identifiers
mac_data_cleaned <- mac_data_cleaned %>%
  bind_rows(mac_data_new_years)

# Fill down categorical variables (all excluding abatement_cost, abatement_potential)
mac_data_cleaned %<>%
  arrange(region_code, sector_lever, year) %>%
  group_by(region_code, sector_lever) %>%
  fill(id) %>%
  fill(region) %>%
  fill(sector) %>%
  fill(sector_code) %>%
  fill(lever)

# Interpolate abatement potential and cost between 2016 and 2030, hold 2030 values constant out to 2050
mac_data_cleaned %<>%
  group_by(region_code, sector_lever) %>%
  mutate(abatement_cost = approx(x = year, y = abatement_cost, xout = year, rule = 2)$y,
         abatement_potential = approx(x = year, y = abatement_potential, xout = year, rule = 2)$y)

# Find total abatement % and average abatement cost
mac_data_cleaned %<>%
  group_by(region_code, sector_code, year) %>%
  arrange(abatement_cost) %>%
  mutate(cum_abatement_potential = cumsum(abatement_potential),
         total_abatement_cost = abatement_cost * abatement_potential,
         mean_abatement_cost = cumsum(total_abatement_cost)/cum_abatement_potential,
         mean_abatement_cost =ifelse(cum_abatement_potential == 0, 0, mean_abatement_cost)) %>%
  ungroup() %>%
  select(-total_abatement_cost) %>%
  select(id, region, region_code, sector, sector_code, year, lever, sector_lever, abatement_cost,
         abatement_potential, cum_abatement_potential, mean_abatement_cost) %>%
  arrange(region_code, sector_code, year, abatement_cost)

save_dated(mac_data_cleaned, "MAC_curve_cleaned_2016USD_data", folder = "Output", csv = "YES")