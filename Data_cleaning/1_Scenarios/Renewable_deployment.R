##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  19/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script reads in TIAM renewable deployment data from Excel and cleans it for use in later calculations and modelling
##### Dependencies:       1.  Latest Imperial TIAM scenarios Excel file
#####                     2.  IEA WEO scenarios data

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "1_Scenarios"
source("utils.R")

# Read in TIAM scenario data from old scenarios (skip 6 empty rows at the top of the sheet) - units are GW in snapshot year
tiam_ren_capa <- read_excel(input_source("Vivid_20190208.xlsx"),
                            sheet = "AA_PowerCap", skip = 6)

# TIAM renewable categories mapping (utilised categories only - rest are NA)
tiam_tech_names <- tribble(~subcat, ~category,
                           "POWGEN - WIN", "Wind",
                           "POWGEN - SOPV", "Solar",
                           "POWGEN - SOTH", "Solar",
                           "POWGEN - HYD", "Hydro") %>%
  mutate(units = "GW")

# Read in TIAM scenario names
scenario_names <- read_excel(input_source("Vivid_scenario_names.xlsx"),
                             sheet = "R1. Scenario specifications", range = "$A$10:$E$59")

# Read in public scenarios consolidated data - WEO 18 total renewable capacity deployment by scenario
public_scen_ren_capa <- read_excel(input_source("Public_scenarios.xlsx"),
                                   sheet = "W21. Total generation capacity", range = "$A$9:$Q$90")

# Set lifetimes for solar, wind and hydro capacity (used to calculate replacement rates for the existing stock
# of each of these technologies)
solar_lifetime <- 25
wind_lifetime <- 25
hydro_lifetime <- 50

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean and reshape public renewable capacity scenario data (2017 - 50) ----

# Clean up scenario names and remove unneeded scenarios (ETP17, WEO16, Shell Sky etc.)
scenario_names2 <- scenario_names %>%
  rename(old_scenario_name = `Scenario name`,
         scenario = `Report name`,
         public_scenario = `Public scen Report name`) %>%
  select(old_scenario_name, scenario, public_scenario)

weo18_ren_capa <- public_scen_ren_capa %>%
  rename(old_scenario_name = Scenario,
         category = Fuel,
         units = Units) %>%
  gather(key = "year", value = "capacity", (`2014`:`2060`)) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(scenario_names2, by = "old_scenario_name") %>%
  select(-old_scenario_name, -scenario, -`Scenario description`) %>%
  rename(scenario = public_scenario) %>%
  select(scenario, category, units, year, capacity) %>%
  filter(!is.na(scenario))

# Remove categories that are not considered here (non-renewables and renewables other than wind / solar / hydro)
weo18_ren_capa2 <- weo18_ren_capa %>%
  filter(category %in% tiam_tech_names$category) %>%
  arrange(scenario, category, year) %>%
  filter(year >= 2016 & year <= 2050)

# Change units to GW
weo18_ren_capa3 <- weo18_ren_capa2 %>%
  mutate(capacity = capacity * 10^3,
         units = "GW")

save_dated(weo18_ren_capa3, "WEO_renewable_capacity", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Clean, reshape and change units of TIAM renewable capacity data ----

# Clean up dataframe names
tiam_ren_capa2 <- tiam_ren_capa %>%
  rename(old_scenario_name = Scenario,
         subcat = ProcessSet,
         region = `Region\\Period`) %>%
  gather(key = "year", value = "capacity", (`2005`:`2100`)) %>%
  mutate(year = as.numeric(year))

# Merge in report scenario names and bin unrequired scenarios
tiam_ren_capa3 <- tiam_ren_capa2 %>%
  left_join(scenario_names2, by = "old_scenario_name") %>%
  select(-old_scenario_name, -public_scenario) %>%
  filter(!is.na(scenario))

# Filter out categories that are not included in the analysis
tiam_ren_capa4 <- tiam_ren_capa3 %>%
  left_join(tiam_tech_names, by = "subcat") %>%
  filter(!is.na(category)) %>%
  mutate(units = "GW") %>%
  select(scenario, region, category, units, subcat, year, capacity)

# Summarise renewable capacity over regions and subcategories
tiam_ren_capa5 <- tiam_ren_capa4 %>%
  group_by(scenario, category, units, year) %>%
  summarise(capacity = sum(capacity, na.rm = TRUE)) %>%
  ungroup()

# Remove 'No_New_action' and reame 'Paris_NDCs' scenarios from TIAM [using WEO18 for these but need to adjust all TIAM scenarios by difference
# between Paris NDCs TIAM and Paris NDCS WEO
tiam_ren_capa6 <- tiam_ren_capa5 %>%
  filter(!(scenario %in% c("No_New_Action"))) %>%
  mutate(scenario = ifelse(scenario == "Paris_NDCs", "Paris_NDCs_TIAM", scenario)) %>%
  filter(year >= 2020 & year <= 2050)

# Merge together TIAM scenario data and WEO18 scenario data
ren_capa <- tiam_ren_capa6 %>%
  bind_rows(weo18_ren_capa3)

# Adjust TIAM scenarios by difference between two Paris scenarios - only for 2030 and do not include Late Action in this
ren_capa2 <- ren_capa %>%
  spread(key = scenario, value = capacity) %>%
  mutate_at(vars(`2DS_Balanced_Transformation`, `Below_2DS`, `Efficiency_Boost`, `EVs_Unplugged`,
                 `Renewable_Revolution`, `Room_for_CCS`),
            funs(ifelse(year == 2030, . + Paris_NDCs - Paris_NDCs_TIAM, .))) %>%
  gather(key = scenario, value = capacity, -(category:year)) %>%
  select(scenario, everything()) %>%
  filter(scenario != "Paris_NDCs_TIAM")

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Ad hoc changes to renewable capacity data - please check when updating scenario inputs ----

# Put scenarios in columns before further data cleaning
ren_capa3 <- ren_capa2 %>%
  spread(key = scenario, value = capacity)

# Fill in 2016, 2017 and 2020 WEO Paris values in place of TIAM values, 2030 WEO Paris values in place of TIAM Late-Action scenario values
ren_capa4 <- ren_capa3 %>%
  mutate_at(vars(unique(tiam_ren_capa5$scenario)[!(unique(tiam_ren_capa5$scenario) %in% c("Paris_NDCs", "No_New_Action"))]),
            funs(case_when(year %in% c(2016, 2017, 2020) ~ Paris_NDCs,
                           TRUE ~ .))) %>%
  mutate(Late_Action = ifelse(year == 2030, Paris_NDCs, Late_Action))

# Replace hydro values under TIAM scenarios with Paris NDCs, as values are lower across the board
ren_capa5 <- ren_capa4 %>%
  mutate_at(vars(unique(tiam_ren_capa5$scenario)[!(unique(tiam_ren_capa5$scenario) %in% c("Paris_NDCs", "No_New_Action"))]),
            funs(case_when(category == "Hydro" ~ Paris_NDCs,
                           TRUE ~ .)))

# Remove unneeded years
ren_capa6 <- ren_capa5 %>%
  filter(year %in% c(2016, 2017, 2020, 2030, 2040, 2050)) %>%
  gather(key = "scenario", value = "capacity", -(category:year)) %>%
  select(scenario, everything())

# Save results before sales / capacity addition calculations
save_dated(ren_capa5, "Cleaned_renewable_capacity", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Calculate smoothed renewable capacity sales in each year

new_capacity_calc <- function(new_cap_category, new_cap_lifetime) {
  
  # Filter based on chosen character
  temp <- ren_capa6 %>%
    filter(new_cap_category == category)
  
  # Extrapolate from 2015 - 2050
  temp2 <- expand.grid(scenario = unique(ren_capa6$scenario),
                       year = seq(2015, 2050, 1), stringsAsFactors = FALSE) %>%
    left_join(temp, by = c("scenario", "year")) %>%
    arrange(scenario, year) %>%
    fill(category) %>%
    group_by(scenario) %>%
    mutate(capacity = approx(x = year, y = capacity, xout = year)$y) %>%
    ungroup()
  
  # Calculate change in total capacity from year-to-year
  temp3 <- temp2 %>%
    group_by(scenario) %>%
    # Add new capacity based on change in total capacity
    mutate(delta_capacity = ifelse(capacity - lag(capacity, n = 1) < 0, 0, capacity - lag(capacity, n = 1))) %>%
    # First 11 years get custom replacement capacity - one 11th of 2017 year value
    mutate(replace_capacity = capacity / new_cap_lifetime) %>%
    # Sales = delta capacity + capacity replaced
    rowwise() %>%
    mutate(sales = sum(c(delta_capacity, replace_capacity), na.rm = TRUE)) %>%
    mutate(sales = ifelse(is.na(delta_capacity) & is.na(replace_capacity), NA_real_, sales)) %>%
    ungroup()
  
  # Moving average of vehicle sales
  temp4 <- map(c(1:4),
               function(x) {temp <- temp3 %>%
                 group_by(scenario) %>%
                 mutate(!!rlang::sym(paste0("lag_sales_", x)) := lag(sales, x),
                        !!rlang::sym(paste0("lead_sales_", x)) := lead(sales, x)) %>%
                 ungroup()}) %>%
    reduce(left_join)
  
  temp5 <- temp4 %>%
    group_by(year) %>%
    rowwise() %>%
    mutate(smooth_sales = case_when(year <= 2020 ~ sales,
                                    TRUE ~ mean(c(sales, lag_sales_1, lag_sales_2, lag_sales_3, lag_sales_4,
                                                  lead_sales_1, lead_sales_2, lead_sales_3, lead_sales_4), na.rm = TRUE))) %>%
    select(scenario, year, category, capacity, delta_capacity, replace_capacity, sales, smooth_sales)
  
  # Replace Late_Action smooth sales with Paris_NDCs out to 2030
  temp6 <- temp5 %>%
    select(scenario, year, category, smooth_sales) %>%
    spread(key = scenario, value = smooth_sales) %>% 
    mutate(Late_Action = ifelse(year <= 2030, Paris_NDCs, Late_Action)) %>%
    gather(key = "scenario", value = "smooth_sales", -(year:category))
  
  temp7 <- temp5 %>%
    select(scenario, year, category, capacity, delta_capacity, replace_capacity, sales) %>%
    left_join(temp6)
  
  return(temp7)
}

solar_capa_sales <- new_capacity_calc("Solar", solar_lifetime)
wind_capa_sales <- new_capacity_calc("Wind", wind_lifetime)
hydro_capa_sales <- new_capacity_calc("Hydro", hydro_lifetime)

prepare_results <- function(prepare_data) {
  
  temp <- prepare_data %>%
    filter(!is.na(category)) %>%
    filter(year %in% c(2017, 2020, 2030, 2040, 2050)) %>%
    select(scenario, year, category, smooth_sales) %>%
    spread(key = year, value = smooth_sales)
}

ren_capa7 <- map(list(solar_capa_sales, wind_capa_sales, hydro_capa_sales), prepare_results) %>%
  bind_rows()

# Add a combined wind + solar category due to the strange solar deployment levels under some scenarios
ren_capa8 <- expand.grid(scenario = unique(ren_capa7$scenario),
                         category = c(unique(ren_capa7$category), "Wind + Solar"), stringsAsFactors = FALSE) %>%
  left_join(ren_capa7, by = c("scenario", "category")) %>%
  gather(key = year, value = capacity, `2017`:`2050`) %>%
  spread(key = category, value = capacity) %>%
  mutate(`Wind + Solar` = Wind + Solar) %>%
  gather(key = category, value = capacity, -(scenario:year)) %>%
  spread(key = year, value = capacity)
 
save_dated(ren_capa8, "Renewable_capacity", folder = "Output", csv = TRUE)

plot_fuel_sales <- function(plot_fuel) {
  
  windows()
  ggplot(ren_capa8 %>% filter(category == plot_fuel) %>%
           gather(key = year, value = capacity, -(scenario:category)) %>%
           mutate(year = as.numeric(year)) %>%
           rename(capacity_sales = capacity)) +
    geom_line(aes(x = year, y = capacity_sales, colour = scenario), size = 1.1) +
    theme_vivid() +
    scale_colour_vivid_house2() +
    ggtitle(plot_fuel)

}

# plot_fuel_sales("Solar")
# plot_fuel_sales("Wind")
# plot_fuel_sales("Wind + Solar")
# plot_fuel_sales("Hydro")