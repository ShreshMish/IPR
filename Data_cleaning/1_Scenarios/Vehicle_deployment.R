##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  19/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script reads in TIAM EV deployment data from Excel and calculates EV and ICE-fired vehicle deployment 
#####                     for use in later calculations and modelling
##### Dependencies:       1.  Latest Imperial TIAM scenarios Excel file
#####                     2.  IEA WEO scenarios data

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "1_Scenarios"
source("utils.R")

# Read in TIAM scenario data from old scenarios (skip 6 empty rows at the top of the sheet) - units are GW in snapshot year
tiam_vehicle_stock <- read_excel(input_source("Vivid_20190208.xlsx"),
                               sheet = "AA_TransCapacity", skip = 6)

# TIAM transport fleet categories from Hawkes (01/02/19 email)
tiam_vehicle_categories <- tribble(~ProcessSet,	~Definition, ~category, ~mapping,
                                   "TRA_$AFV",	 "Alternative fuel vehicles (e.g. ethanol, methanol, LPG, gas…", "Alternative fuels", "Alternative fuels",
                                   "TRA_$ELC",	 "Electric vehicles", "Electric", "EVs",
                                   "TRA_$HH2",	 "Hydrogen vehicles", "Hydrogen", "Alternative fuels",
                                   "TRA_$ICE",	 "ICE vehicles", "ICE", "ICEs",
                                   "TRA_$OIL-HYBRID",	"Hybrid vehicles (non plug-in)", "Hybrid (non-plug-in)", "ICEs",
                                   "TRA_$PHEV", "Plug-in hydrids", "Hybrid (plug-in)", "EVs")

# Read in TIAM scenario names
scenario_names <- read_excel(input_source("Vivid_scenario_names.xlsx"),
                             sheet = "R1. Scenario specifications", range = "$A$10:$E$59")

# Read in public scenarios consolidated data - EVO18 vehicle stock historical sheet
evo18_ev_hist_stock <- read_excel(input_source("Public_scenarios.xlsx"),
                                  sheet = "R15. EVO 18 hist stock", range = "$A$9:$N$32")

# Read in public scenarios consolidated data - EVO 17 vehicle stock scenarios sheet
evo17_ev_scen_stock <- read_excel(input_source("Public_scenarios.xlsx"),
                                  sheet = "R14. EVO 17 scenario stock", range = "$A$10:$H$14")

# Adjustment factor for ratio of bn passenger km in TIAM to OICA light duty vehicle fleet in 2012
### IMPROVE AUDIT TRAIL HERE - CALCULATIONS IN R
tiam_ev_units_adj_factor <- 0.048634

# This parameter is used to calculate the replacement rate for the existing vehicle fleet - TN responsible for figure
vehicle_lifetime <- 11

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean and reshape IEA EVO EV fleet data (2015 - 50) ----

# Clean up scenario names and remove unneeded scenarios (ETP17, WEO16, Shell Sky etc.)
scenario_names2 <- scenario_names %>%
  rename(old_scenario_name = `Scenario name`,
         scenario = `Report name`,
         public_scenario = `Public scen Report name`) %>%
  select(old_scenario_name, scenario, public_scenario)

evo18_ev_hist_stock2 <- evo18_ev_hist_stock %>%
  rename(region = X__1) %>%
  filter(region == "Total") %>%
  select(-region) %>%
  gather(key = year, value = stock, `2005`:`2017`) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year >= 2012) %>%
  mutate(units = "mn vehicles",
         stock = stock / 10^3,
         category = "EVs") %>%
  select(category, units, year, stock)

# Apply EVO scenario names
evo17_ev_scen_stock2 <- evo17_ev_scen_stock %>%
  rename(scenario = Scenario) %>% 
  select(scenario, everything()) %>%
  mutate(category = "EVs",
         units = "mn vehicles") %>%
  select(scenario, category, units, everything()) %>%
  gather(key = year, value = stock, `2016`:`2030`) %>%
  mutate(year = as.numeric(year)) %>%
  select(scenario, category, units, year, stock)

evo17_ev_scen_stock3 <- expand.grid(scenario = unique(evo17_ev_scen_stock2$scenario),
                                    year = c(unique(evo17_ev_scen_stock2$year), 2040, 2050), stringsAsFactors = FALSE) %>%
  left_join(evo17_ev_scen_stock2, by = c("scenario", "year")) %>%
  fill(category) %>%
  fill(units) %>%
  spread(key = year, value = stock) %>%
  # Change the below to be (2030 - 2025) * 2 when you have time
  mutate(`2040` = `2030` + (`2030` - `2020`),
         `2050` = `2040` + (`2040` - `2030`)) %>%
  gather(key = year, value = stock, -(scenario:units)) %>%
  arrange(scenario, year) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year >= 2020 & year != 2025)

# Merge together the two dataset
evo17_ev_hist_stock3 <- expand.grid(scenario =  unique(evo17_ev_scen_stock2$scenario),
                                    year = unique(evo18_ev_hist_stock2$year), stringsAsFactors = FALSE) %>%
  left_join(evo18_ev_hist_stock2, by = "year") %>%
  filter(year >= 2015)

evo_scen_stock <- evo17_ev_scen_stock3 %>%
  bind_rows(evo17_ev_hist_stock3) %>%
  arrange(scenario, year)

save_dated(evo_scen_stock, "IEA_EVO_vehicle_fleet", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Clean, reshape and change units of TIAM vehicle fleet data (2012 - 50) ----

tiam_vehicle_stock2 <- tiam_vehicle_stock %>%
  left_join(tiam_vehicle_categories, by = "ProcessSet") %>%
  rename(old_scenario_name = Scenario,
         oldsubcat = ProcessSet,
         subcat = category,
         category = mapping,
         region = `Region\\Period`) %>%
  select(-Definition) %>%
  gather(key = year, value = stock, (`2005`:`2100`)) %>%
  mutate(year = as.numeric(year)) %>%
  select(old_scenario_name, region, category, subcat, year, stock)

# Merge in report scenario names and bin unrequired scnearios
tiam_vehicle_stock3 <- tiam_vehicle_stock2 %>%
  left_join(scenario_names2, by = "old_scenario_name") %>%
  select(-old_scenario_name, -public_scenario) %>%
  filter(!is.na(scenario)) %>%
  select(scenario, category, subcat, everything())

# Summarise over regions and subcategories
tiam_vehicle_stock4 <- tiam_vehicle_stock3 %>%
  group_by(scenario, year, category) %>%
  summarise(stock = sum(stock, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year >= 2012) %>%
  mutate(units = "Bn passenger km") %>%
  select(scenario, category, units, year, stock)

# Switch units from bn passenger km to mn vehicles using the OICA adjustment ratio
tiam_vehicle_stock5 <- tiam_vehicle_stock4 %>%
  mutate(stock = stock * tiam_ev_units_adj_factor,
         units = "mn vehicles")

# Add total vehicle stock category for later use
tiam_vehicle_stock6 <- expand.grid(scenario = unique(tiam_vehicle_stock5$scenario),
                                   category = c(unique(tiam_vehicle_stock5$category), "Total"),
                                   year = unique(tiam_vehicle_stock5$year), stringsAsFactors = FALSE) %>%
  left_join(tiam_vehicle_stock5, by = c("scenario", "category", "year")) %>%
  fill(units) %>%
  group_by(scenario, year) %>%
  mutate(stock = ifelse(category == "Total", sum(stock, na.rm = TRUE), stock)) %>%
  unique %>%
  arrange(scenario, category, year)

# Merge together EVO scenarios and TIAM scenarios are rename EVO scenarios for baselines (Paris NDCs and No New Action)
vehicle_stock <- tiam_vehicle_stock6 %>%
  bind_rows(evo_scen_stock) %>% # EVO scenario names are RTS, Paris NDCs, 2DS and B2DS
  filter(year <= 2050 & year >= 2015) %>%
  select(-units)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Ad-hoc changes to the TIAM scenario data ----

ev_vehicle_stock <- vehicle_stock %>%
  filter(category == "EVs") %>%
  spread(key = "scenario", value = "stock")

# Replace TIAM Baseline scenarios (No_New_Action and Paris_NDCs) with EVO equivalents
ev_vehicle_stock2 <- ev_vehicle_stock %>%
  mutate(Paris_NDCs = `Paris NDCs`,
         No_New_Action = RTS) %>%
  # Drop EVO scenarios
  select(-RTS, -`Paris NDCs`, -`2DS`, -B2DS)

# Fill in 2015 - 20 values using EVO Paris values for all scenarios
ev_vehicle_stock3 <- ev_vehicle_stock2 %>%
  mutate_at(vars(-one_of("category", "year", "No_New_Action", "Paris_NDCs")),
            funs(ifelse(year <= 2020, Paris_NDCs, .))) %>%
  # Replace 2030 delayed action scenario values with Paris NDCs
  mutate(Late_Action = ifelse(year == 2030, Paris_NDCs, Late_Action)) %>%
  gather(key = "scenario", value = "stock", -(category:year))
  
save_dated(ev_vehicle_stock3, "Cleaned_EV_fleet", folder = "Interim", csv = TRUE)

# Merge EV fleet into overall stock dataset (note that we need EVs from 2015 for the current cleantech markets code)
# but we only need ICE vehicles from 2020
vehicle_stock2 <- vehicle_stock %>%
  filter(category != "EVs") %>%
  filter(year >= 2020) %>% 
  bind_rows(ev_vehicle_stock3)

# Save pre-2017 data for binding after ICE value correction below
vehicle_stock3 <- vehicle_stock2 %>%
  filter(year < 2020)

# Replace 'Baseline' (EVO) scenario ICE values with Total - EVs - Alternative fuels, and repeat for policy scenarios where values have been replaced
# all scenarios in 2020, Late-Action scenario in 2030
vehicle_stock4 <- vehicle_stock2 %>%
  filter(year >= 2020) %>%
  group_by(scenario, year) %>%
  mutate(stock = case_when(category == "ICEs" & scenario %in% c("No_New_Action", "Paris_NDCs") & year >= 2020 | # ICEs baseline scenarios case
                             category == "ICEs" & year == 2020 | # Climate policy scenarios 2020 values case
                             category == "ICEs" & year == 2030 & scenario == "Late_Action" ~ # Late Action 2030 values case
                             stock[[which(category == "Total")]] - stock[[which(category == "EVs")]] - stock[[which(category == "Alternative fuels")]],
                           TRUE ~ stock)) %>%
  ungroup() %>%
  bind_rows(vehicle_stock3) %>%
  arrange(scenario, year, category)

# Save results, then get rid of non- EV / ICE categories
save_dated(vehicle_stock4, "Cleaned_vehicle_fleet", folder = "Interim", csv = TRUE)

vehicle_stock5 <- vehicle_stock4 %>%
  filter(category %in% c("EVs", "ICEs"))

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Calculate smoothed EV and ICE sales in each year ----

new_capacity_calc <- function(new_cap_category) {
  
  # Filter based on chosen character
  temp <- vehicle_stock5 %>%
    filter(new_cap_category == category)
  
  # Extrapolate from 2015 - 2050
  temp2 <- expand.grid(scenario = unique(vehicle_stock5$scenario),
                       year = seq(2015, 2050, 1), stringsAsFactors = FALSE) %>%
    left_join(temp, by = c("scenario", "year")) %>%
    arrange(scenario, year) %>%
    fill(category) %>%
    group_by(scenario) %>%
    mutate(stock = approx(x = year, y = stock, xout = year)$y) %>%
    ungroup()
  
  # Calculate change in total stock from year-to-year
  temp3 <- temp2 %>%
    group_by(scenario) %>%
    # Add new stock based on change in total capacity
    mutate(delta_stock = ifelse(stock - lag(stock, n = 1) < 0, 0, stock - lag(stock, n = 1))) %>%
    # First 11 years get custom replacement capacity - one 11th of 2017 year value
    mutate(replace_stock = stock / vehicle_lifetime) %>%
    # Sales = delta stock + stock replaced
    rowwise() %>%
    mutate(sales = sum(c(delta_stock, replace_stock), na.rm = TRUE)) %>%
    mutate(sales = ifelse(is.na(delta_stock) & is.na(replace_stock), NA_real_, sales)) %>%
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
    select(scenario, year, category, stock, delta_stock, replace_stock, sales, smooth_sales)
  
  # Replace Late_Action smooth sales with Paris_NDCs out to 2030
  temp6 <- temp5 %>%
    select(scenario, year, category, smooth_sales) %>%
    spread(key = scenario, value = smooth_sales) %>% 
    mutate(Late_Action = ifelse(year <= 2030, Paris_NDCs, Late_Action)) %>%
    gather(key = "scenario", value = "smooth_sales", -(year:category))
  
  temp7 <- temp5 %>%
    select(scenario, year, category, stock, delta_stock, replace_stock, sales) %>%
    left_join(temp6)
  
  return(temp7)
}

ice_vehicle_sales <- new_capacity_calc("ICEs")
ev_sales <- new_capacity_calc("EVs")

prepare_results <- function(prepare_data) {
  
  temp <- prepare_data %>%
    select(scenario, year, category, smooth_sales) %>%
    spread(key = year, value = smooth_sales) %>%
    filter(!is.na(category)) %>%
    select(-category)
  
  return(temp)
}

ice_results <- prepare_results(ice_vehicle_sales) %>%
  select(scenario, `2020`, `2025`, `2030`, `2040`, `2050`)

save_dated(ice_results, "ICE_new_capacity", folder = "Output", csv = TRUE)

ev_results <- prepare_results(ev_sales) %>%
  select(scenario, `2015`, `2020`, `2025`, `2030`, `2040`, `2050`)

save_dated(ev_results, "EV_new_capacity", folder = "Output", csv = TRUE)