##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  14/02/2019
##### Code author:        Justine Schafer
##### Edited by:          Shyamal Patel
##### Description:        This script uses cleaned patent and revenue data for Vivid categories to project future 
#####                     market size and market shares for exposed companies
##### Dependencies:       1.  Cleantech patent and revenue company-level cleaned data
#####                     2.  Financial company-level cleaned data (no segment analysis)
#####                     3.  Renewables total capacity scenario output data
#####                     4.  EVs scenario output data [check for unit changes]
#####                     5.  Biofuels scenario output data

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "2_Cleantech_markets"
source("utils.R")

# Read in cleantech company-level data
company_cleantech_data <- readRDS(input_source("Cleantech_company_data.rds"))

# Read in revenue data
company_financial_data <- readRDS(input_source("Financial_company_data.rds"))

# Read in renewables scenario data
renew_scenario_data <- readRDS(input_source("Renewables_scenario_data.rds"))

# Read in EV scenario data
EV_scenario_data <- readRDS(input_source("EVs_scenario_data.rds"))

# Read in biofuels scenario data
biofuel_scenario_data <- readRDS(input_source("Biofuels_scenario_data.rds"))

# Set discount rate
discount_rate <- 0.0575

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean up company-level datasets ----

# Remove zero green revenue rows, and CCS (not currently included in the model)
company_cleantech_data2 <- company_cleantech_data %>%
  filter(gr_percent != 0 & ve_category != "CCS") %>%
  # Bin irrelevant variables
  select(-Total_ve_cat_rev_share, -Calc_check, -`revenue%`, -non_computables, -no_of_ve_cat,
         -ftse_id, -ftse_company, -SEDOL, -estimate_date, -icb_industry, -icb_supersector,
         -estimate_type, -id, -lithium_adj, -sum_revenue_pct) %>%
  select(-(periodstart:revenuedatasource))

# Keep only revenue and profit from the financial dataset
company_financial_data2 <- company_financial_data %>%
  select(company_id, company, revenue, net_income) %>%
  rename(total_revenue = revenue,
         total_net_income = net_income)

# Merge the two company-level datasets together (keeping only companies which have green revenues)
company_data <- company_cleantech_data2 %>%
  left_join(company_financial_data2, by = c("company", "company_id")) %>%
  select(company_id, company, total_revenue, total_net_income, everything()) %>%
  # Drop observations where ve category revenue share is 0
  filter(ve_category_rev_share != 0)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Clean up and combine scenario datasets ----

# Scenario data categories
category_mapping <- tibble(ve_category = unique(company_data$ve_category)) %>%
  filter(ve_category != "Other") %>%
  # Currently using only hydro, and combined wind + solar category (EV aggregate and biofuels are defined below)
  # No scenario data for lithium
  mutate(scenario_product = case_when(ve_category == "Hydro_power" ~ "Hydro",
                                      ve_category %in% c("Wind_power", "Solar_power") ~ "Wind + Solar",
                                      ve_category == "Minerals_for_batteries" ~ "EV_aggregate",
                                      TRUE ~ ve_category))

save_dated(category_mapping, "Cleantech_product_categories", "Interim", csv = TRUE)

# Renewables
renew_scenario_data2 <- renew_scenario_data %>%
  rename(scenario = Scenario,
         scenario_product = Fuel) %>%
  select(-`Source / status`, -Units) %>%
  # Using WEO BAU instead of TIAM BAU
  filter(scenario != "BAU") %>%
  mutate(scenario = ifelse(scenario == "WEO BAU", "BAU", scenario)) %>%
  # Merge in matched categories from above
  left_join(category_mapping, by = "scenario_product") %>%
  select(-scenario_product) %>%
  # Keep only modelled VE categories
  filter(!is.na(ve_category)) %>%
  gather(key = year, value = "stock", (`2005`:`2050`))

# Electric vehicles
EV_scenario_data2 <- EV_scenario_data %>%
  rename(scenario = Scenario) %>%
  filter(scenario != "IEA 2DS") %>%
  mutate(scenario_product = "EV_aggregate") %>%
  left_join(category_mapping, by = "scenario_product") %>%
  select(-scenario_product) %>%
  mutate(scenario = ifelse(scenario == "IEA RTS", "BAU", scenario)) %>%
  gather(key = year, value = stock, (`2015`:`2050`)) %>%
  # Drop 2025 values to match old code
  mutate(stock = case_when(year == 2025 ~ NA_real_,
                           TRUE ~ stock)) %>%
  # DANGER - CHANGE THIS FOR NEXT SET OF SCENARIOS
  mutate(scenario = ifelse(scenario == "B2DS_central", "Paris_INDCs", scenario))

# Biofuels
biofuel_scenario_data2 <- biofuel_scenario_data %>%
  rename(scenario = Scenario) %>%
  select(-Units) %>%
  mutate(scenario_product = "Biofuels_production") %>%
  left_join(category_mapping, by = "scenario_product") %>%
  select(-scenario_product) %>%
  gather(key = year, value = stock, (`2005`:`2050`))

# Combine all scenarios data
scenario_data <- renew_scenario_data2 %>%
  bind_rows(EV_scenario_data2) %>%
  bind_rows(biofuel_scenario_data2) %>%
  mutate(year = as.numeric(year))

# Interpolate between 2005 - 50, and drop redundant years
scenario_data2 <- expand.grid(scenario = unique(scenario_data$scenario),
                              ve_category = unique(scenario_data$ve_category),
                              year = unique(c(2017, unique(scenario_data$year), seq(2015, 2050, 5))), stringsAsFactors = FALSE) %>%
  left_join(scenario_data, by = c("scenario", "ve_category", "year")) %>%
  # Interpolate (use end point values for LHS extrapolation only)
  group_by(scenario, ve_category) %>%
  mutate(stock = approx(year, stock, xout = year, rule = 2:1)$y) %>%
  ungroup() %>%
  arrange(scenario, ve_category, year) %>%
  filter(year >= 2017)

# Add relative stock variable (relative to 2017 values) [!? why]
scenario_data3 <- scenario_data2 %>% 
  group_by(scenario, ve_category) %>%
  mutate(stock_change = stock / stock[[which(year == 2017)]])

save_dated(scenario_data3, "Combined_cleantech_scenario", "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Calculate current market shares ----

# Calculate current cleantech market revenue share
company_data2 <- company_data %>%
  # Summarise using company, ve category information
  group_by(company_id, company, ve_category) %>%
  summarise(total_revenue = mean(total_revenue),
            total_net_income = mean(total_net_income),
            ve_category_rev_share = sum(ve_category_rev_share, na.rm = TRUE),
            AllIPC= mean(AllIPC, na.rm = TRUE),
            OnlyMainIPC = mean(OnlyMainIPC, na.rm = TRUE),
            Total_AllIPC = mean(Total_AllIPC, na.rm = TRUE),
            Total_OnlyMainIPC = mean(Total_OnlyMainIPC, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ve_category_rev = total_revenue * ve_category_rev_share) %>%
  # Revisit the below to see whether any items are being double counted
  group_by(ve_category) %>%
  mutate(total_ve_category_rev = sum(ve_category_rev, na.rm = TRUE),
         current_rev_market_share = ve_category_rev / total_ve_category_rev) %>%
  ungroup()

# Create separate market share variable so we can exclude / hold constant the market share of companies which do not have IP data
company_data3 <- company_data2 %>% 
  group_by(ve_category) %>%
  mutate(no_IP_ve_category_rev_share = sum(ifelse(is.na(Total_AllIPC), current_rev_market_share, NA_real_), na.rm = TRUE)) %>%
  ungroup()

# Calculate total IP per VE category and cleantech market IP shares
company_data4 <- company_data3 %>%
  group_by(ve_category) %>% 
  mutate(ip_market_share = (AllIPC / sum(AllIPC, na.rm = TRUE)) * (1 - no_IP_ve_category_rev_share),
         ip_market_share = case_when(is.na(ip_market_share) & !is.na(Total_AllIPC) ~ 0,
                                     is.na(ip_market_share) ~ current_rev_market_share,
                                     TRUE ~ ip_market_share)) %>%
  ungroup()

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Analyse market shares to determine approach for market share changes ----

market_share_analysis <- expand.grid(company_id = unique(company_data4$company_id),
                                     year = c(2018, seq(2020, 2050, 5)), stringsAsFactors = FALSE) %>%
  left_join(company_data4, by = "company_id") %>%
  arrange(company_id, year)

### Determine alpha / constraint for market share project

# Calculate future market shares using arbitrarily chosen alpha (weighting between current revenue and IP market shares) and
# constraint (what proportion of the percentage point difference between % current market share and % IP share a company is allowed to travel over)
future_market_share_test <- function(alpha = NULL, constraint = NULL) {
  
  temp <- market_share_analysis %>%
    mutate(constrained_IP_share = current_rev_market_share + constraint * (ip_market_share - current_rev_market_share),
           projected_market_share = case_when(year == 2018 ~ current_rev_market_share,
                                              year <= 2020 + 1 / alpha ~ current_rev_market_share + alpha * (constrained_IP_share - current_rev_market_share) * (year - 2020),
                                              year > 2020 + 1 / alpha ~ constrained_IP_share),
           projected_market_share = case_when(projected_market_share < 0 ~ 0,
                                              projected_market_share > 1 ~ 1,
                                              current_rev_market_share == 0 & ip_market_share == 0 ~ 0,
                                              TRUE ~ projected_market_share),
           constraint_param = constraint,
           alpha_param = alpha)

  return(temp)
}

years_to_convergence <- seq(5, 100, 5)
alpha_values <- 1 / years_to_convergence
constraint_values <- seq(0.05, 1, 0.05)
parameter_values_list <- expand.grid(alpha_value = alpha_values,
                                     constraint_value = constraint_values, stringsAsFactors = FALSE) %>%
  rowwise() %>%
  do(combs = c(.$alpha_value, .$constraint_value))
parameter_values_list <- as.list(parameter_values_list$combs)

market_share_analysis2 <- map(parameter_values_list, function(x) {future_market_share_test(alpha = x[1], constraint = x[2])}) %>%
  bind_rows()

### Graph different parameter values for the same product

# Alpha parameter effects
graph_alpha <- function (product = NULL, constraint = NULL) {
  temp <- market_share_analysis2 %>%
    filter(ve_category == product & constraint_param == constraint) %>%
    arrange(company_id) %>%
    slice(1:160) %>%
    mutate(alpha_param = as.character(alpha_param))
  
  windows()
  ggplot(temp, aes(x = year, y = projected_market_share)) +
    geom_line(aes(colour = alpha_param)) + 
    geom_hline(aes(yintercept = current_rev_market_share)) +
    geom_hline(aes(yintercept = ip_market_share)) +
    theme_vivid() +
    scale_colour_vivid_house2()
}

# Constraint parameter effects
graph_constraint <- function (product = NULL, alpha = NULL) {
  temp <- market_share_analysis2 %>% 
    filter(ve_category == product & alpha_param == alpha) %>%
    arrange(company_id) %>%
    slice(1:160) %>%
    mutate(constraint_param = as.character(constraint_param))
  
  windows()
  ggplot(temp, aes(x = year, y = projected_market_share)) +
    geom_line(aes(colour = constraint_param)) + 
    geom_hline(aes(yintercept = current_rev_market_share)) +
    geom_hline(aes(yintercept = ip_market_share)) +
    theme_vivid() +
    scale_colour_vivid_house2()  
}

#--------------------------------------------------------------------------------------------------

##### SECTION 6 - Calculate market shares based on chosen parameter estimates ----

# Settled on alpha = 1/20 (2040 realisation of whichever is larger, (1 +/- constraint) * Existing or IP)
# Constraint of 20% means you cannot travel more than 20% of the percentage point difference between Current market share and IP market share
convergence_chosen <- 20
alpha_chosen = 1 / convergence_chosen
constraint_chosen <- 0.2

market_share_results <- market_share_analysis %>%
  # 2020 is assumed to be year after which market shares start to diverge - note that constraint is included in this equation so that you converge
  # to the 'constrained point' after 20 years rather than to the 'unconstrained / IP share point'
  mutate(constrained_IP_share = current_rev_market_share + constraint_chosen * (ip_market_share - current_rev_market_share),
         projected_market_share = case_when(year == 2018 ~ current_rev_market_share,
                                            year <= 2020 + convergence_chosen ~ current_rev_market_share + alpha_chosen * (constrained_IP_share - current_rev_market_share) * (year - 2020),
                                            year > 2020 + convergence_chosen ~ constrained_IP_share),
         projected_market_share = case_when(projected_market_share < 0 ~ 0,
                                            projected_market_share > 1 ~ 1,
                                            current_rev_market_share == 0 & ip_market_share == 0 ~ 0,
                                            TRUE ~ projected_market_share)) %>%
  group_by(year, ve_category) %>%
  # Rebalance market shares so sum total is always 1 (not guaranteed due to asymmetric constraints)
  mutate(projected_market_share = projected_market_share / sum(projected_market_share, na.rm = TRUE)) %>%
  ungroup()

#--------------------------------------------------------------------------------------------------

##### SECTION 7 - Combine market share and size projections ----

# Combine market size and share projections
company_results <- market_share_results %>%
  left_join(scenario_data3, by = c("year", "ve_category")) %>%
  # Projected market share * Market revenue * Change in stock relative to 2018 BAU
  mutate(projected_revenue = projected_market_share * total_ve_category_rev * stock_change,
         projected_profit = projected_revenue * (total_net_income / total_revenue)) %>%
  # Current market share * Market revenue * Change in stock relative to 2018 BAU [for impact decomposition]
  mutate(constant_share_revenue = current_rev_market_share * total_ve_category_rev * stock_change,
         constant_share_profit = constant_share_revenue * (total_net_income / total_revenue)) %>%
  filter(year >= 2020 & ve_category != "Other") %>%
  select(scenario, company_id, company, year, ve_category, projected_profit, projected_revenue,
         constant_share_profit, constant_share_revenue, current_rev_market_share, ip_market_share,
         projected_market_share, everything())

#--------------------------------------------------------------------------------------------------

##### SECTION 8 - Calculate NPV impacts ----

company_npv_results <- company_results %>%
  group_by(scenario, company_id, company, ve_category) %>%
  # Use weights to include profits from 2018 - 50
  # 2020 value covers 2018 - 22.5 so weight is 4.5 / 5 = 0.9; 2050 value covers 2047.5 - 50 so weight is 2.5 / 5 = 0.5
  # All other weights are x - 2.5 - x + 2.5, so weight is 1
  mutate(weight = case_when(year == 2020 ~ 0.9,
                            year == 2050 ~ 0.5,
                            TRUE ~ 1),
         npv_profit = projected_profit / (1 + discount_rate) ^ (year - 2018),
         npv_constant_share_profit = constant_share_profit / (1 + discount_rate) ^ (year - 2018)) %>%
  summarise(profit = sum(weight * npv_profit, na.rm = TRUE),
            constant_share_profit = sum(weight * npv_constant_share_profit)) %>%
  group_by(company_id, company, ve_category) %>%
  mutate(profit_impact_pct = profit / profit[[which(scenario == "BAU")]] - 1,
         marketgrowth_impact_pct = constant_share_profit / constant_share_profit[[which(scenario == "BAU")]] - 1,
         marketshare_impact_pct = profit_impact_pct - marketgrowth_impact_pct) %>%
  ungroup() %>%
  select(-constant_share_profit)

save_dated(company_npv_results, "Cleantech_npv_impacts", "Output", csv = TRUE)