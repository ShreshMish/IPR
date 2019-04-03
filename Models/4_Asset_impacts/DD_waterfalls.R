##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  03/04/2019
##### Code author:        Shyamal Patel
##### Dependencies:       1. Oil & gas, and coal modelling company names
#####                     2. Oil & gas, and coal modelling cost profiles and economics data
#####                     3. Oil & gas, and coal modelling results
##### Notes:              None
##### Called by:          N/A

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "4_Asset_impacts"
source("utils.R")

# Oil and gas results
results_oilgas <- readRDS("1_Demand_destruction/Output/Oil_and_gas_dd_npv_impacts.rds")

# Coal results
results_coal <- readRDS("1_Demand_destruction/Output/Coal_dd_npv_impacts.rds")

# Oil and gas company names
names_oilgas <- readRDS("1_Demand_destruction/Input/Oil_and_gas_company_names.rds")

# Coal company names
names_coal <- readRDS("1_Demand_destruction/Input/Coal_company_names.rds")

# Oil and gas production and cost profiles
data_oilgas <- readRDS("1_Demand_destruction/Interim/Oil_and_gas_dd_full_results.rds")

# Coal production and cost profiles
data_coal <- readRDS("1_Demand_destruction/Interim/Coal_dd_full_results.rds")

# Coal mine exposure data (needed for profit margins)
regional_exposure_data_coal <- readRDS("1_Demand_destruction/Interim/Coal_company_region_exposure.rds")

# Coal mine profit data (needed for profit margins)
mine_profit_data_coal <- readRDS("1_Demand_destruction/Interim/Coal_mine_profit_results.rds")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Combine oil & gas, and coal datasets ----

results_coal2 <- results_coal %>%
  mutate(product = "Coal")

results_oilgas2 <- results_oilgas %>%
  filter(product != "All")

results_comb <- results_oilgas2 %>%
  bind_rows(results_coal2) %>%
  rename(market = product)

# Get rid of NA observations before calculating median and other impacts
# and 100% valuation impact companies
results_comb2 <- results_comb %>% 
  filter(!is.na(profit_impact_pct)) %>%
  filter(profit_impact_pct != -1)

# Calculate stranding and margin profit impacts (absolute) for all fuels
results_comb3 <- results_comb2 %>%
  mutate(post_stranding_profit = profit / (1 + profit_impact_pct) * stranding_impact_pct,
         post_margin_profit = profit / (1 + profit_impact_pct) * margin_impact_pct) %>%
  # Add market cap column
  group_by(company_id, company, market) %>%
  mutate(market_cap = sum(ifelse(scenario == "Paris_NDCs", profit, NA_real_), na.rm = TRUE))

# Get rid of irrelevant variables
results_comb4 <- results_comb3 %>%
  select(-stranding_impact_pct, -margin_impact_pct, -profit_impact, -margin_impact,
         -stranding_impact)

# Process oil & gas production and costs dataset
data_oilgas2 <- names_oilgas %>%
  left_join(data_oilgas, by = "rystad_name") %>%
  filter(!is.na(product) & product != "All")

# Calculate summary statistics for all companies based on full data [margins are based on 2020 data only]
data_oilgas3 <- data_oilgas2 %>%
  filter(scenario %in% c("Paris_NDCs")) %>%
  group_by(company_id, company, product) %>%
  mutate(margin = 1 - unit_cost_f / price_f) %>%
  summarise(margin = sum(margin * production_f, na.rm = TRUE) / sum(production_f, na.rm = TRUE),
            unit_cost = sum(unit_cost_f * production_f, na.rm = TRUE) / sum(production_f, na.rm = TRUE),
            prod_before_2030 = sum(ifelse(year <= 2030, production_f, NA_real_), na.rm = TRUE) / sum(production_f, na.rm = TRUE)) %>%
  ungroup()

# Process coal production time profiles
data_coal2 <- names_coal %>%
  left_join(data_coal, by = "producer_name") %>%
  mutate(product = "Coal") %>%
  filter(scenario %in% c("Paris_NDCs")) %>%
  group_by(company_id, company, product) %>%
  summarise(prod_before_2030 = sum(ifelse(year <= 2030, profit, NA_real_), na.rm = TRUE) / sum(profit, na.rm = TRUE)) %>%
  ungroup()

# Calculate region level profit margin estimates based on mine-level data (based on Paris NDCs scenario)
mine_profit_data_coal2 <- mine_profit_data_coal %>%
  filter(scenario == "Paris_NDCs") %>%
  select(year, mine_ID, region, regional_production, unit_profit, unit_cost) %>%
  group_by(region, year) %>% 
  summarise(sumproduct_profit = sum(unit_profit * regional_production),
            sumproduct_cost = sum(unit_cost * regional_production),
            production = sum(regional_production)) %>%
  ungroup() %>%
  mutate(unit_profit = sumproduct_profit / production,
         unit_cost = sumproduct_cost / production) %>%
  select(-sumproduct_profit, -sumproduct_cost)

# Merge together region and producer region exposure datasets
regional_exposure_data_coal2 <- regional_exposure_data_coal %>%
  left_join(mine_profit_data_coal2, by = c("region", "year")) %>%
  filter(year >= 2020) %>%
  group_by(region) %>%
  mutate_at(vars(production, unit_profit, unit_cost),
            funs(approx(year, ., xout = year)$y)) %>%
  ungroup()

# Calculate profit margins for each company
margins_coal <- regional_exposure_data_coal2 %>%
  group_by(producer_name) %>%
  summarise(margin = sum(production_share * production * unit_profit) / sum(production_share  * production),
            unit_cost = sum(production_share * production * unit_cost) / sum(production_share * production)) %>%
  ungroup()

# Combine coal production and margin datasets
data_coal3 <- data_coal2 %>%
  left_join(names_coal, by = c("company_id", "company")) %>%
  left_join(margins_coal, by = "producer_name") %>%
  select(-producer_name)

# Bind together contextual data
data_comb <- data_oilgas3 %>%
  bind_rows(data_coal3)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Group data and prepare for waterfall ----

# Find median
results_comb5 <- results_comb4 %>%
  group_by(scenario, market) %>%
  mutate(median_impact = quantile(profit_impact_pct, probs = 0.5),
         median_test = case_when(profit_impact_pct <= median_impact ~ "BELOW",
                                 TRUE ~ "ABOVE")) %>%
  ungroup()

# Add in contextual data
results_comb6 <- results_comb5 %>%
  left_join(data_comb, by = c("company_id", "company", "market" = "product"))

# Summarise variables over categories and index so initial profits are 1
results_comb7 <- results_comb6 %>%
  group_by(scenario, market, median_test) %>%
  summarise(prod_before_2030 = sum(prod_before_2030 * profit, na.rm = TRUE) / sum(profit, na.rm = TRUE),
            margin = sum(margin * profit, na.rm = TRUE) / sum(profit, na.rm = TRUE),
            unit_cost = sum(unit_cost * profit, na.rm = TRUE) / sum(profit, na.rm = TRUE),
            profit = sum(profit, na.rm = TRUE),
            post_stranding_profit = sum(post_stranding_profit, na.rm = TRUE),
            post_margin_profit = sum(post_margin_profit, na.rm = TRUE),
            market_cap = sum(market_cap, na.rm = TRUE)) %>%
  group_by(market, median_test) %>% 
  ungroup() %>%
  mutate(profit_index = profit / market_cap - 1) %>%
  mutate_at(vars(market_cap, post_stranding_profit, post_margin_profit),
            funs(index = . / market_cap))

save_dated(results_comb7, "Demand_destruction_statistics", folder = "Output", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Create waterfall ----

# Profit plot waterfall
demand_dest_waterfall <- function(plot_scenario, plot_market, plot_group) {
  
  temp <- results_comb7 %>%
    filter(scenario == plot_scenario & market == plot_market & median_test == plot_group) %>%
    select(scenario, market, median_test, contains("_index")) %>%
    rename(profit_base_index = market_cap_index) %>%
    rename(profit_final_index = profit_index) %>%
    mutate(profit_final_index = 0) %>%
    gather(key = category, value = profit, contains("index")) %>%
    mutate(profit = profit * 100) %>%
    mutate(category = case_when(category == "profit_base_index" ~ "Paris NDCs",
                                category == "post_stranding_profit_index" ~ "Asset stranding",
                                category == "post_margin_profit_index" ~ "Margin impact",
                                category == "profit_final_index" ~ "2DS Balanced Transformation",
                                TRUE ~ NA_character_)) %>%
    mutate(category = ordered(category, levels = c("Paris NDCs", "Asset stranding", "Margin impact", "2DS Balanced Transformation"))) %>%
    arrange(category) %>%
    # Adjust all values so profit units are absolute instead of % impact relative to Paris NDCs
    mutate(profit = cumsum(profit)) %>%
    mutate(lagged_profit = lag(profit, n = 1),
           delta_profit = profit - lagged_profit) %>%
    # Calculate waterfall variables
    mutate(base_stack = ifelse(category %in% c("Paris NDCs", "2DS Balanced Transformation"), profit, NA_real_),
           invisible_stack = case_when(delta_profit <= 0 ~ lagged_profit + delta_profit,
                                       delta_profit > 0 ~ lagged_profit,
                                       TRUE ~ NA_real_),
           fall_stack = ifelse(delta_profit < 0, -delta_profit, NA_real_),
           rise_stack = ifelse(delta_profit > 0, delta_profit, NA_real_)) %>%
    select(scenario, market, median_test, category, contains("_stack")) %>%
    gather(key = stack_type, value = stack_value, contains("_stack")) %>%
    mutate(stack_type = ordered(stack_type, levels = c("base_stack", "invisible_stack", "fall_stack", "rise_stack"))) %>%
    mutate(stack_type = fct_rev(stack_type))
  
  ggplot(temp) +
    geom_col(aes(x = category, y = stack_value, fill = stack_type), colour = NA, width = 0.75) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) + 
    scale_y_continuous(name = "NPV profits (normalised)", expand = c(0,0), limits = c(0, 100)) +
    scale_fill_manual(values = plot_colours) + 
    theme_vivid(vivid_size = 1.6) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          aspect.ratio = 9 / 21.16)
  
  ggsave(paste0("4_Asset_impacts/Output/Plots/DD_waterfalls/", plot_market, "_", plot_group, "_", plot_scenario, ".png"), width = 21.16, height = 9)
  
}

plot_colours <- c("base_stack" = rgb(0, 143, 159, max = 255), "fall_stack" = rgb(255, 77, 166, max = 255), "rise_stack" = rgb(0, 196, 103, max = 255),
                  "invisible_stack" = NA)
plot_list <- expand.grid(fuel = c("Coal", "Gas", "Liquid"),
                         group = c("BELOW", "ABOVE"), stringsAsFactors = FALSE)
map2(plot_list$fuel, plot_list$group, demand_dest_waterfall, plot_scenario = "2DS_Balanced_Transformation")