##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  14/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script reads in coal scenario and exposure data and models stranding impacts
##### Dependencies:       1.  Coal company-level cleaned data
#####                     2.  Coal scenario output data
#####                     3.  Coal cost curve data (regional)
#####                     4.  Coal spreadsheet company names matched to Vivid unique company IDs / names
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "1_Demand_destruction"
source("utils.R")
library(sfsmisc)

# Read in coal company data
company_data <- readRDS(input_source("Coal_company_data.rds"))

# Read in coal scenario data
scenario_data <- readRDS(input_source("Coal_scenario_data.rds"))

# Read in coal company name matching results (Spreadsheet names to Vivid names)
company_names <- readRDS(input_source("Coal_company_names.rds"))

# Read in coal cost curve data
supply_curve_data <- readRDS(input_source("Coal_cost_curve_data.rds"))

# Set discount rate
discount_rate <- 0.0575

# Match coal cost curve regions to company exposure data regions
country_region_matching <- tribble(~country,          ~region,
                                   "Australia",      "Australia",
                                   "Colombia",       "Colombia",
                                   "South Africa",   "South Africa",
                                   "China",          "Rest of World", # China is close enough to ROW based on observed cost data
                                   "ROW",            "Rest of World",
                                   "Philippines",    "Rest of World",
                                   "Indonesia",      "Indonesia",
                                   "Poland",         "Rest of World",
                                   "India",          "Rest of World",
                                   "Madagascar",     "Rest of World",
                                   "Mozambique",     "Rest of World",
                                   "Czech Republic", "Rest of World")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Preliminary data cleaning ----

# Remove redundant scenario data variables
scenario_data2 <- scenario_data %>%
  filter(fuel == "coal") %>%
  # Change units of production from tce to Mtce
  mutate(production_g = production / 10^6) %>%
  select(-fuel, -units, -production)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Calculate mine-level profits based on supply curves and scenario data ----

# Merge together the datasets (note that the supply curve is assumed to be the same in every period)
mine_profit_data <- expand.grid(scenario = unique(scenario_data2$scenario),
                                year = unique(scenario_data2$year),
                                mine_ID = unique(supply_curve_data$mine_ID), stringsAsFactors = FALSE) %>%
  left_join(supply_curve_data, by = "mine_ID") %>%
  left_join(scenario_data2, by = c("year", "scenario")) %>%
  arrange(scenario, year, mine_ID)

# Scale supply curve so that seaborne production = total global production in the BAU case
mine_profit_data2 <- mine_profit_data %>% 
  group_by(year) %>%
  # Mines are ordered based on increasing cost, so cumulative production is maximised at the highest mined ID
  mutate(scaling_factor = production_g[[which(scenario == "BAU" & mine_ID == 1)]] / cumulative_production[[which(scenario == "BAU" & mine_ID == max(mine_ID))]]) %>%
  group_by(scenario, year) %>%
  arrange(scenario, year, unit_cost) %>%
  mutate(regional_production = regional_production * scaling_factor,
         cumulative_production = cumsum(regional_production)) %>%
  ungroup()

save_dated(mine_profit_data2, "Coal_cost_curve_scaled", folder = "Interim", csv = TRUE)

# Add a 'utilisation share' variable (-1 for stranded mines, 1 for fully utilised mines, and share of production for marginal mine)
mine_profit_data3 <- mine_profit_data2 %>%
  select(-scaling_factor) %>% 
  # Round cumulative production variables to 4 DP to avoid floating point errors
  mutate_at(vars(cumulative_production, production_g), funs(round(., digits = 4))) %>%
  mutate(utilisation_share = case_when(cumulative_production < production_g ~ 1, # Mine can be fully utilised without exceeding global output
                                       lag(cumulative_production, n = 1) > production_g ~ -1, # Global output is already satisfied using previous mines
                                       TRUE ~ (regional_production - (cumulative_production - production_g)) / regional_production)) # Share of the marginal mine that can be used

# Market price based on stranding indicator
mine_profit_data4 <- mine_profit_data3 %>%
  group_by(scenario, year) %>%
  mutate(price = unit_cost[[which(cumsum(utilisation_share) == max(cumsum(utilisation_share)))]]) %>%
  ungroup() %>%
  mutate(utilisation_share = case_when(utilisation_share < 0 ~ 0,
                                       TRUE ~ utilisation_share))

# Calculate profit per tonne on each mine (if any)
mine_profit_data5 <- mine_profit_data4 %>%
  mutate(unit_profit = case_when(utilisation_share > 0 ~ price - unit_cost,
                            TRUE ~ 0))

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Supply curve charts in each year, under each scenario ----

# Define chart function
supply_curve_chart <- function(chart_scenario, chart_year) {
  
  temp <- mine_profit_data5 %>%
    mutate(bau_price = mean(ifelse(scenario == "BAU" & year == chart_year, price, NA_real_), na.rm = TRUE),
           bau_quantity = mean(ifelse(scenario == "BAU" & year == chart_year, production_g, NA_real_), na.rm = TRUE),
           scenario_quantity = mean(ifelse(scenario == chart_scenario & year == chart_year,
                                           production_g, NA_real_), na.rm = TRUE)) %>%
    filter(scenario == chart_scenario & year == chart_year) %>%
    mutate(utilised_entry_cost = case_when(utilisation_share > 0 ~ unit_cost,
                                           TRUE ~ NA_real_),
           unutilitised_entry_cost = case_when(is.na(utilised_entry_cost) ~ unit_cost,
                                               TRUE ~ NA_real_))
  
  ggplot(temp) +
    geom_rect(aes(xmin = ifelse(is.na(lag(cumulative_production, 1)), 0, lag(cumulative_production, 1)), xmax = cumulative_production,
                  ymin = 0, ymax = unutilitised_entry_cost, colour = region), fill = "white") + 
    geom_rect(aes(xmin = ifelse(is.na(lag(cumulative_production, 1)), 0, lag(cumulative_production, 1)), xmax = cumulative_production,
                  ymin = 0, ymax = utilised_entry_cost, colour = region, fill = region)) +
    geom_hline(aes(yintercept = price)) + # Scenario price
    geom_hline(aes(yintercept = bau_price)) + # BAU price
    geom_vline(aes(xintercept = scenario_quantity)) + # Scenario quantity
    geom_vline(aes(xintercept = bau_quantity)) + # BAU quantity
    ggtitle(paste0(chart_scenario, " coal cost curve, ", chart_year)) +
    theme_vivid() +
    scale_x_continuous(name = "Production (Mtonnes)", expand = c(0, 0),
                       limits = c(0, ceiling(max(temp$cumulative_production, na.rm = TRUE)) + 1000)) +
    scale_y_continuous(name = "Price (2016US$/tonne)", expand = c(0, 0), limits = c(0, 80)) +
    scale_colour_vivid_house() +
    scale_fill_vivid_house()
  
  path_to_plot <- fs::path(main_save_folder, "Interim", "Supply_curves", paste0(chart_scenario, "_", chart_year, ".png"))
  ggsave(path_to_plot, width = 22, height = 11.5, units = c("cm"))
}

chart_scenario_year_combs <- expand.grid(scenario = unique(mine_profit_data5$scenario),
                                         year = unique(mine_profit_data5$year), stringsAsFactors = FALSE) %>%
  rowwise() %>%
  do(combs = c(.$scenario, .$year))
chart_scenario_year_combs <- as.list(chart_scenario_year_combs$combs)

# Only needs to be run to regenerate plots
# lapply(chart_scenario_year_combs, function(x) {supply_curve_chart(chart_scenario = x[1], chart_year = x[2])})

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - NPV impacts at regional-level based on supply curves ----

# Calculate profit per mine by scenario, by year
mine_profit_results <- mine_profit_data5 %>%
  group_by(year, mine_ID) %>%
  mutate(profit = unit_profit * regional_production * utilisation_share) %>% # Only include production if mine is not stranded (remains economical)
  mutate(profit_impact = profit - profit[[which(scenario == "BAU")]],
         margin_impact = (price - price[[which(scenario == "BAU")]]) * regional_production * utilisation_share,
         stranding_impact = profit_impact - margin_impact)

# Find net present value of each mine's profits over time
# Note that this has no bearing on company-level results
mine_profit_npv_results <- mine_profit_results %>%
  # Match oil & gas approach by leaving out 2016 and applying 0.9 weight to 2020 observation
  filter(year != 2016) %>%
  mutate(weight = case_when(year == 2020 ~ 0.9, # NPV profit weights (2020 spans 2018 - 2022.5)
                            year == 2050 ~ 0.5, # NPV profit weights (2050 spans 2047.5 - 2050)
                            TRUE ~ 1)) %>% # NPV profit weights (all other years span 5 year windows)
  mutate_at(vars(profit, profit_impact, margin_impact, stranding_impact),
            funs("npv" = . / (1 + discount_rate) ^ (year - 2018))) %>%
  group_by(mine_ID, region, scenario) %>%
  summarise_at(vars(profit_npv, profit_impact_npv, margin_impact_npv, stranding_impact_npv),
               funs("sum", sum(weight * ., na.rm = TRUE))) %>%
  ungroup() %>%
  group_by(mine_ID, region) %>%
  mutate(profit_impact_pct = profit_impact_npv_sum / profit_npv_sum[[which(scenario == "BAU")]],
         stranding_impact_pct = stranding_impact_npv_sum / profit_npv_sum[[which(scenario == "BAU")]],
         margin_impact_pct = margin_impact_npv_sum / profit_npv_sum[[which(scenario == "BAU")]])

save_dated(mine_profit_npv_results, "Coal_cost_curve_scenario_npv_profits", folder = "Interim", csv = TRUE)

# Find regional NPV impacts (used for company-level calculations involving regional exposure estimates)
region_profit_npv_results <- mine_profit_npv_results %>%
  group_by(scenario, region) %>% 
  summarise_at(vars(ends_with("npv_sum")), funs(sum(., na.rm = TRUE))) %>%
  group_by(region) %>%
  mutate(profit_impact_pct = profit_impact_npv_sum / profit_npv_sum[[which(scenario == "BAU")]],
         stranding_impact_pct = stranding_impact_npv_sum / profit_npv_sum[[which(scenario == "BAU")]],
         margin_impact_pct = margin_impact_npv_sum / profit_npv_sum[[which(scenario == "BAU")]])


save_dated(region_profit_npv_results, "Coal_regional_NPV_impacts", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Calculate each company's temporal production exposure

# First year production share is equal to historical
company_data2 <- company_data %>%
  mutate(production_share_2018 = production_share)

# Note that we have modelled future production shares using exponential decay to ensure that all companies are still 
# (somewhat) exposed to the distant future (instead of having cliff edge production profiles)
# NB Companies with short reserve lives will experience faster decay and still be less exposed to the future
for(i in 2019:2050) {
  production_share_newyr <- rlang::sym(paste0("production_share_", i))
  production_share_oldyr <- rlang::sym(paste0("production_share_", i - 1))
  
  company_data2 %<>%
    mutate(!!production_share_newyr := !!production_share_oldyr * (1 - 1 / reserve_life))
}

company_data3 <- company_data2 %>%
  left_join(country_region_matching, by = "country") %>%
  select(producer_name, region, country, everything())

company_region_temporal_exposure <- company_data3 %>%
  # Sum over regions to find production relative to 2018 in each year
  group_by(producer_name, region) %>%
  summarise_at(vars(starts_with("production_share_20")), funs(sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  gather(key = "year", value = "production_share", (production_share_2018:production_share_2050)) %>%
  mutate(year = as.numeric(substring(year, nchar(year) - 3, nchar(year)))) %>%
  arrange(producer_name, region, year)

#--------------------------------------------------------------------------------------------------

##### SECTION 6 - Interpolate mine-level profit over all years before combining with company dataset ----

mine_profit_results2 <- expand.grid(scenario = unique(mine_profit_results$scenario),
                                 mine_ID = unique(mine_profit_results$mine_ID),
                                 year = 2016:2050, stringsAsFactors = FALSE) %>%
  left_join(mine_profit_results, by = c("scenario", "year", "mine_ID")) %>%
  arrange(scenario, mine_ID, year) %>%
  group_by(scenario, mine_ID) %>%
  fill(region) %>%
  group_by(scenario, mine_ID, region) %>%
  mutate_at(vars(profit, profit_impact, margin_impact, stranding_impact), 
            funs(approx(x = year, y = ., xout = year)$y)) %>%
  ungroup() %>%
  select(scenario, mine_ID, year, region, profit, profit_impact, margin_impact, stranding_impact) %>%
  filter(year >= 2018)

# Summarise profit over mines
mine_profit_results3 <- mine_profit_results2 %>%
  group_by(scenario, year, region) %>%
  summarise_at(vars(profit, profit_impact, margin_impact, stranding_impact),
               funs(sum(., na.rm = TRUE)))

#--------------------------------------------------------------------------------------------------

##### SECTION 7 - Company NPV profit impacts ----

company_results <- expand.grid(scenario = unique(mine_profit_results3$scenario),
                                   producer_name = unique(company_data3$producer_name),
                                   year = unique(mine_profit_results3$year), stringsAsFactors = FALSE) %>%
  left_join(company_region_temporal_exposure, by = c("producer_name", "year")) %>%
  left_join(mine_profit_results3, by = c("scenario", "region", "year")) %>%
  # Adjust based on company production in each year (reserve life adjustment)
  mutate_at(vars(profit, profit_impact, margin_impact, stranding_impact),
            funs(. * production_share))

# Summarise over regions
company_results2 <- company_results %>%
  group_by(scenario, producer_name, year) %>%
  summarise_at(vars(profit, profit_impact, margin_impact, stranding_impact),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup()

# Calculate NPV profits and summarise over years
company_npv_results <- company_results2 %>%
  mutate_at(vars(profit, profit_impact, margin_impact, stranding_impact),
            funs(. / (1 + discount_rate) ^ (year - 2018))) %>%
  group_by(scenario, producer_name) %>%
  summarise_at(vars(profit, profit_impact, margin_impact, stranding_impact),
               funs(sum(., na.rm = TRUE))) %>%
  group_by(producer_name) %>%
  mutate(profit_impact_pct = profit_impact / profit[[which(scenario == "BAU")]],
          margin_impact_pct = margin_impact / profit[[which(scenario == "BAU")]],
          stranding_impact_pct = stranding_impact / profit[[which(scenario == "BAU")]]) %>%
  ungroup() %>%
  arrange(producer_name, scenario)

save_dated(company_npv_results, "Coal_dd_npv_impacts", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 8 - Map Rystad company names to Vivid IDS and names ----

company_npv_results2 <- company_npv_results %>%
  left_join(company_names, by = "producer_name") %>%
  filter(!is.na(company_id)) %>%
  select(company_id, company, everything(), -producer_name)

save_dated(company_npv_results2, "Coal_dd_npv_impacts", folder = "Output", csv = TRUE)
