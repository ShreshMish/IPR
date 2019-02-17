##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  25/01/2019
##### Code author:        Shyamal Patel
##### Description:        This script generates MAC curve data (units already 2016US$ following Excel work)
##### Dependencies:       1.  Latest Mac curve data Excel file: "3 - MAC curves/Input/MAC curve data.xlsx"
#####                         Older files can be found in the ".../Dated/" folder (if any)

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "0_Background"
source("utils.R")

# All abatement cost data is in 2016US$/tCO2, abatement potential given as a % of total emissions
mac_data_raw <- read_excel(input_source("MAC_curve_data.xlsx"),
                           sheet = "W5. Panel data for R export", range = "$A$5:$I$4961", col_names = TRUE)

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
mac_data_cleaned2 <- mac_data_cleaned %>%
  arrange(region_code, sector_lever, year) %>%
  group_by(region_code, sector_lever) %>%
  fill(id) %>%
  fill(region) %>%
  fill(sector) %>%
  fill(sector_code) %>%
  fill(lever)

# Interpolate abatement potential and cost between 2016 and 2030, hold 2030 values constant out to 2050
mac_data_cleaned3 <- mac_data_cleaned2 %>%
  group_by(region_code, sector_lever) %>%
  mutate(abatement_cost = approx(x = year, y = abatement_cost, xout = year, rule = 2)$y,
         abatement_potential = approx(x = year, y = abatement_potential, xout = year, rule = 2)$y)

# Find total abatement % and average abatement cost
mac_data_cleaned4 <- mac_data_cleaned3 %>%
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

save_dated(mac_data_cleaned4, "MAC_curve_cleaned_2016USD_data", folder = "Output", csv = TRUE)