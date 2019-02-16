##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  16/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script reads in TIAM EV deployment data from Excel and calculates EV and ICE-fired vehicle deployment 
#####                     for use in later calculations and modelling
##### Dependencies:       1.  Latest Imperial TIAM scenarios Excel file
#####                     2.  IEA WEO scenarios data
#####                     (current scenario data = "Input/Vivid_scenario_runs.xls" & "Input/Vivid_scenario_runs_newdb.xls" (for Weak 2020))

##### TIAM has been updated with new technology costs and constraint changes, which is causing discrepancies between the same 
##### scenario run in different versions - adjust 'weak 2020' scenario from new database to compensate for this

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "1_Scenarios"
source("utils.R")

# Read in TIAM scenario data from old scenarios (skip 6 empty rows at the top of the sheet) - units are GW in snapshot year
tiam_ev_new_capa <- read_excel(input_source("Vivid_scenario_runs.xls"),
                               sheet = "AA_EVNewCapacity", skip = 6)

# Read in TIAM scenario data from new scenarios (skip 6 empty rows at the top of the sheet) - units are GW in snapshot year
tiam_w2020_renewable_total_capa <- read_excel(input_source("Vivid_scenario_runs_newdb.xls"),
                                              sheet = "AA_EVNewCapacity", skip = 6)

# Read in IEA EV stock scenario data (2017 Global EV Outlook)
evo_raw_ev_total_stock <- read_excel(input_source("Global_EV_Outlook_VE.xlsx"),
                                    sheet = "R1. IEA EVO deployment", range = "$A$9:$C$20")

# Read in IEA EV sales scenario data (2017 Energy Technology Perspectives)
etp_raw_ev_total_sales <- read_excel(input_source("ETP2017_EVsales_VE.xlsx"),
                                     sheet = "R2. IEA ETP sales", range = "$A$9:$D$15")

# Read in BP Energy Outlook 2018 sales and vehicle km scenario data (2018 BP Energy Outlook)
bp_raw_ev_data <- read_excel(input_source("BPOutlook2017_EVdata_VE.xlsx"),
                             sheet = "R3. BP Energy EV factors", range = "$A$11:$M$13")

# Read in scenario names (defined by Carbon_prices.R script - currently inconsistent between scenario data cleaning scripts - hence recasting below)
scenario_names <- tibble(Scenario = unique(tiam_ev_new_capa$Scenario)) %>%
  mutate(scenario = case_when(Scenario == "s01a_BHP_base" ~ "BAU",
                              Scenario == "s02_BHP_cum2dt_highREN_EV" ~ "2DS_cheap_ren",
                              Scenario == "s03_BHP_cum2dt_highCCS_EV" ~ "2DS_cheap_ccs",
                              Scenario == "s04_BHP_cum2dt_lowDEM_EV" ~ "2DS_cheap_eff",
                              Scenario == "s06a_BHP_cum2dt_Highfeasibility" ~ "2DS_central",
                              Scenario == "s06c_BHP_cum2dt_hf120v2_delayed2030" ~ "2DS_delay",
                              Scenario == "s06a_BHP_cumb2c300_Highfeasibility" ~ "B2DS_central"))

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean, reshape and create aggregate EV new capacity data (2020 - 50) ----

ev_new_capa <- tiam_ev_new_capa %>%
  left_join(scenario_names) %>%
  filter(!is.na(scenario)) %>%
  select(scenario, `Region\\Period`, `2030`:`2100`) %>%
  group_by(scenario) %>%
  # Summarise over regions
  summarise_at(.vars = vars(`2030`:`2100`),
               .funs = funs(sum(., na.rm = TRUE)))

# Add zero-value years
ev_new_years <- expand.grid(scenario = unique(ev_new_capa$scenario),
                            year = c(2005, 2007, 2012, 2020)) %>%
  mutate(value = 0) %>%
  spread(key = "year", value = "value")

ev_new_capa2 <- ev_new_capa %>%
  full_join(ev_new_years) %>%
  gather(key = "year", value = "EV_new_capacity_km", -scenario) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(scenario, year) %>%
  filter(year <= 2050)

# Create new vehicles variable based on BP Energy Outlook usage factors (km / vehicle)
usage_factor <- mean(bp_raw_ev_data$`km / BEV`)

# Calculate number of new vehicles required to deliver additional vehicle km from new vehicles (*1000 to adjust units from bn vehicle km to million vehicles)
tiam_ev_sales <- ev_new_capa2 %>%
  mutate(EV_new_vehicles = EV_new_capacity_km / usage_factor * 1000)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Clean, reshape and create aggregate IEA data (2020 - 50) ----

# Clean IEA ETP scenario data
etp_ev_total_sales <- etp_raw_ev_total_sales %>%
  rename(EV_sales = `Sales (mn vehicles)`) %>%
  # Add IEA B2DS values for 2015 (equal to RTS)
  spread(key = "Scenario", value = "EV_sales") %>%
  mutate(`IEA B2DS` = ifelse(Year == 2015, `IEA RTS`, `IEA B2DS`)) %>%
  gather(key = "Scenario", value = "Sales", -(Year:Type)) %>%
  spread(key = "Type", value = "Sales") %>% 
  mutate(Total = EV + Fossil) %>%
  gather(key = "Type", value = "Sales", -(Year:Scenario))

# Clean IEA Global EV Outlook scenario data
evo_ev_total_stock <- evo_raw_ev_total_stock %>%
  filter(Year != 2016) %>%
  rename(Stock = `EV stock (mn vehicles)`) %>%
  # Add IEA 2DS and B2DS values for 2015
  spread(key = "Scenario", value = "Stock") %>%
  mutate_at(.vars = vars(`IEA 2DS`:`IEA B2DS`),
            .funs = funs(ifelse(Year == 2015, `IEA RTS`, .))) %>%
  gather(key = "Scenario", value = "Stock", -Year) %>%
  mutate(Type = "EV")

# Merge together IEA datasets
iea_ev_sales_stock <- etp_ev_total_sales %>%
  full_join(evo_ev_total_stock) %>%
  arrange(Scenario, Type, Year)

# Compare IEA 2DS and B2DS on stock
iea_ev_stock_adj_factor <- expand.grid(Year = c(2015:2060),
                                       Scenario = unique(iea_ev_sales_stock$Scenario), stringsAsFactors = FALSE) %>%
  left_join(iea_ev_sales_stock) %>%
  filter(Type == "EV" | is.na(Type)) %>%
  select(-Type, -Sales) %>%
  spread(key = "Scenario", value = "Stock") %>%
  select(-`IEA RTS`) %>%
  # Interpolate all years (holding values fixed for years after 2030)
  mutate_at(.vars = vars(`IEA 2DS`:`IEA B2DS`),
            .funs = funs(approx(x = Year, y = ., xout = Year, rule = 2)$y)) %>%
  mutate(adj_factor_B2DS_to_2DS = `IEA 2DS` / `IEA B2DS`) %>%
  select(Year, adj_factor_B2DS_to_2DS)

# Interpoate RTS values for all years 2015 - 2060
iea_sales <- expand.grid(Year = c(2015:2060),
                         Scenario = unique(iea_ev_sales_stock$Scenario),
                         Type = unique(iea_ev_sales_stock$Type), stringsAsFactors = FALSE) %>%
  full_join(iea_ev_sales_stock) %>%
  select(-Stock) %>%
  spread(key = "Scenario", value = "Sales") %>%
  arrange(Type, Year) %>%
  # Interpolate scenario quantities for RTS, by type of vehicle
  group_by(Type) %>%
  mutate(`IEA RTS` = approx(x = Year, y = `IEA RTS`, xout = Year, rule = 2)$y) %>%
  ungroup() %>%
  # Join in adjustment factors
  full_join(iea_ev_stock_adj_factor)

# Impute 2DS EV sales based on ratio of 2DS and B2DS vehicle stocks
iea_sales2 <- iea_sales %>%
  group_by(Year) %>%
  mutate(`IEA RTS Total` = mean(ifelse(Type == "Total", `IEA RTS`, NA_real_), na.rm = TRUE),
         `IEA B2DS Total` = mean(ifelse(Type == "Total", `IEA B2DS`, NA_real_), na.rm = TRUE)) %>%
  # Adjust down B2DS values by ratio of total sales to IEA RTS Total sales
  mutate(`IEA B2DS` = `IEA B2DS` * (`IEA RTS Total` / `IEA B2DS Total`)) %>%
  # Interpolate scenario quantities for B2DS, by type of vehicle
  group_by(Type) %>%
  mutate(`IEA B2DS` = approx(x = Year, y = `IEA B2DS`, xout = Year, rule = 2)$y) %>%
  ungroup() %>%
  # Calculate 2DS quantities using adjustment factors (total and EV first)
  mutate(`IEA 2DS` = case_when(Type == "Total" ~ `IEA B2DS`,
                               Type == "EV" ~ `IEA B2DS` * adj_factor_B2DS_to_2DS)) %>%
  select(-starts_with("adj"), -ends_with("Total"))

# IEA 2DS sales
iea_2ds_sales <- iea_sales2 %>% 
  select(Year, Type, `IEA 2DS`) %>%
  spread(key = "Type", value = "IEA 2DS") %>%
  mutate(Fossil = Total - EV) %>%
  gather(key = "Type", value = "IEA 2DS", -Year)

# Merge together results
iea_sales3 <- iea_sales2 %>%
  select(-`IEA 2DS`) %>%
  left_join(iea_2ds_sales)

iea_ev_sales <- iea_sales3 %>%
  filter(Type == "EV") %>%
  select(-Type) %>%
  gather(key = "scenario", value = "EV_sales", -Year) %>%
  rename(year = Year)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Adjust TIAM EV data for differences against the IEA scenarios (tie results to IEA 2DS scenario)

# Merge the two datasets together
tiam_ev_sales2 <- tiam_ev_sales %>%
  select(scenario, year, EV_new_vehicles) %>%
  rename(EV_sales = EV_new_vehicles) %>%
  bind_rows(iea_ev_sales) %>%
  filter(year %in% c(2015, 2020, 2025, 2030, 2040, 2050)) %>%
  spread(key = "year", value = "EV_sales")

# Ad hoc changes to ensure data is exactly the same as the old EV scenario data range
tiam_ev_sales3 <- tiam_ev_sales2 %>%
  filter(!scenario %in% c("IEA B2DS", "BAU")) %>%
  arrange(desc(`2015`)) %>%
  fill(`2015`) %>%
  gather(key = "year", value = "EV_sales", -scenario) %>%
  spread(key = "scenario", value = "EV_sales") %>%
  mutate_at(.vars = vars(`2DS_central`:`B2DS_central`),
            .funs = funs(approx(x = year, y = ., xout = year)$y)) %>%
  gather(key = "Scenario", value = "sales", -year) %>%
  spread(key = "year", value = "sales") %>%
  mutate(`2020` = ifelse(Scenario == "IEA RTS", `2020`, NA_real_)) %>%
  arrange(desc(`2020`)) %>%
  fill(`2020`) %>%
  gather(key = "year", value = "EV_sales", -Scenario) %>%
  spread(key = "Scenario", value = "EV_sales") %>%
  # Store temporary 2DS central old variable
  mutate(`2DS_central_old` = `2DS_central`) %>%
  mutate_at(.vars = vars(`2DS_central`:`B2DS_central`),
            .funs = funs(. * (`IEA 2DS` / `2DS_central_old`))) %>%
  mutate(`2DS_delay` = case_when(year == 2030 ~ `IEA RTS`,
                                   year == 2025 ~ NA_real_,
                                   TRUE ~ `2DS_delay`),
         `2DS_delay` = approx(x = year, y = `2DS_delay`, xout = year)$y) %>%
  select(-`2DS_central_old`) %>%
  gather(key = "Scenario", value = "Sales", -year) %>%
  spread(key = "year", value = "Sales")

save_dated(tiam_ev_sales3, "EV_new_capacity", folder = "Output", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Find fossil-fired capacity as difference between total vehicle sales (constant across scenarios), and EV sales

tiam_fossil_sales <- iea_sales3 %>%
  filter(Type == "Total") %>%
  filter(Year %in% c(2015, 2018, 2020, 2025, 2030, 2040, 2050)) %>%
  select(Year, `IEA RTS`) %>%
  rename(`IEA RTS_Total` = `IEA RTS`,
         year = Year)

tiam_ev_sales4 <- tiam_ev_sales3 %>%
  gather(key = "year", value = "sales", -Scenario) %>%
  spread(key = "Scenario", value = "sales") %>%
  mutate(year = as.numeric(year))

tiam_fossil_sales2 <- tiam_fossil_sales %>%
  left_join(tiam_ev_sales4) %>%
  select(-`IEA 2DS`) %>%
  mutate_at(.vars = vars(`2DS_central`:`IEA RTS`),
            .funs = funs(`IEA RTS_Total` - .)) %>%
  mutate_at(.vars = vars(`2DS_central`:`IEA RTS`),
            .funs = funs(approx(x = year, y = ., xout = year)$y)) %>%
  mutate(`2DS_cheap_eff` = ifelse(year >= 2025, `2DS_central`, `2DS_cheap_eff`)) %>%
  select(-`IEA RTS_Total`) %>%
  gather(key = "Scenario", value = "sales", -year) %>%
  spread(key = "year", value = "sales")

tiam_fossil_sales3 <- tiam_fossil_sales2 %>%
  rename(`IEA / TIAM Scenario` = Scenario) %>%
  mutate(scenario = case_when(`IEA / TIAM Scenario` == "IEA RTS" ~ "BAU",
                               `IEA / TIAM Scenario` == "2DS high renewables" ~ "2DS_cheap_ren",
                               `IEA / TIAM Scenario` == "2DS high CCS" ~ "2DS_cheap_ccs",
                               `IEA / TIAM Scenario` == "2DS high efficiency" ~ "2DS_cheap_eff",
                               `IEA / TIAM Scenario` == "2DS central" ~ "2DS_central",
                               `IEA / TIAM Scenario` == "2DS delayed" ~ "2DS_delay",
                               `IEA / TIAM Scenario` == "B2DS central" ~ "B2DS_central")) %>%
  select(`IEA / TIAM Scenario`, scenario, everything())

save_dated(tiam_fossil_sales3, "ICE_new_capacity", folder = "Output", csv = TRUE)