##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  03/04/2019
##### Code author:        Shyamal Patel
##### Dependencies:       1. Add
##### Notes:              None
##### Called by:          N/A

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "4_Asset_impacts"
source("utils.R")

# Model panel for market allocation data
panel <- readRDS("3_Cost_and_competition/Interim/Cleaned_model_panel.rds")

# Cleantech markets model results
results_cm <- readRDS("2_Cleantech_markets/Output/Cleantech_npv_impacts.rds")

# Cleantech markets modelling dataset for patent shares
data_cm <- readRDS("2_Cleantech_markets/Interim/Cleantech_company_impacts_full.rds")

# ICE vehicle demand destruction model results
results_dd <- readRDS("1_Demand_destruction/Output/ICE_vehicle_dd_qimpacts.rds")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Combine datasets ----

panel2 <- panel %>% 
  select(scenario, company_id, company, parent_market, market, region, product_revenue_share, region_revenue_share,
         market_cap_2017) 

# Note that ICE vehicle demand destruction estimates are market-level [company data not available]
results_dd2 <- results_dd %>%
  select(scenario, profit_impact_pct) %>%
  mutate(market = "Automobiles")

results_cm2 <- results_cm %>%
  select(scenario, company_id, company, ve_category, ends_with("pct")) %>%
  rename(market = ve_category) %>%
  # Recast marktes
  mutate(market = case_when(market == "Biofuels_production" ~ "GR_biofuels",
                            market == "EV_aggregate" ~ "GR_EVs",
                            market == "Minerals_for_batteries" ~ "GR_minerals",
                            market == "Hydro_power" ~ "GR_hydro",
                            market == "Solar_power" ~ "GR_solar",
                            market == "Wind_power" ~ "GR_wind"))

results_comb <- panel2 %>%
  left_join(results_dd2, by = c("scenario", "market")) %>%
  # Don't merge on profit_impact_pct to prevent NA values from being retained
  left_join(results_cm2, by = c("scenario", "company_id", "company", "market")) %>%
  mutate(profit_impact_pct = case_when(!is.na(profit_impact_pct.x) ~ profit_impact_pct.x,
                                       !is.na(profit_impact_pct.y) ~ profit_impact_pct.y,
                                       TRUE ~ NA_real_)) %>%
  select(-profit_impact_pct.x, -profit_impact_pct.y)

# Keep results from only green revenue and EV sectors
results_comb2 <- results_comb %>%
  filter(market %in% c("Automobiles", "GR_biofuels", "GR_EVs", "GR_hydro", "GR_minerals", "GR_solar", "GR_wind")) %>%
  filter(scenario != "Lack_Of_Coordination")

# Partition the two datasets so we can further filter down automobile companies
results_auto <- results_comb2 %>%
  filter(parent_market == "Automobiles") %>%
  filter(market %in% c("Automobiles", "GR_EVs"))

# Filter out renewables results [solar only]
results_ren <- results_comb2 %>% 
  filter(market %in% c("GR_solar"))

# Further clean the automobiles results dataset
results_auto2 <- results_auto %>% 
  group_by(scenario, company_id, company, market) %>% 
  mutate_at(vars(marketgrowth_impact_pct, marketshare_impact_pct, profit_impact_pct),
            funs(profit = . * market_cap_2017)) %>%
  rename_at(vars(ends_with("profit")), funs(gsub("_pct_profit", "", .))) %>%
  summarise_at(vars(market_cap_2017, marketgrowth_impact, marketshare_impact, profit_impact),
               funs(sum(., na.rm = TRUE)))

# Reshape the automobiles results dataset
results_auto3 <- results_auto2 %>% 
  gather(key = line, value = value, (market_cap_2017:profit_impact)) %>%
  mutate(id = paste0(market, "_", line)) %>%
  select(-market, -line) %>% 
  spread(key = id, value = value) %>%
  select(-Automobiles_marketgrowth_impact, -Automobiles_marketshare_impact) %>%
  mutate_at(vars(Automobiles_market_cap_2017:GR_EVs_profit_impact),
            funs(ifelse(is.na(.), 0, .)))

# Calculate overall profit impact for median calculation purposes in automobiles results
results_auto4 <- results_auto3 %>%
  mutate(market_cap_2017 = (Automobiles_market_cap_2017 + GR_EVs_market_cap_2017),
         index_impact = (Automobiles_profit_impact + GR_EVs_profit_impact) / (Automobiles_market_cap_2017 + GR_EVs_market_cap_2017))

# Summarise solar data over regions [note key variables are equal over regions]
results_ren2 <- results_ren %>%
  group_by(scenario, company_id, company, parent_market, market) %>%
  summarise(market_cap_2017 = sum(market_cap_2017),
            marketgrowth_impact_pct = mean(marketgrowth_impact_pct),
            marketshare_impact_pct = mean(marketshare_impact_pct),
            profit_impact_pct = mean(profit_impact_pct)) %>%
  ungroup()

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Group data and prepare for renewables waterfall ----

# Find medians for solar
results_ren3 <- results_ren2 %>%
  group_by(scenario) %>%
  mutate(median_impact = quantile(profit_impact_pct, probs = 0.5),
         median_test = case_when(profit_impact_pct <= median_impact ~ "BELOW",
                                 TRUE ~ "ABOVE")) %>% 
  ungroup()

# Add IP market share to the dataset
data_ren <- data_cm %>%
  select(scenario, company_id, company, year, ve_category, ip_market_share) %>%
  filter(scenario == "Paris_NDCs" & year == 2020 & ve_category == "Solar_power") %>%
  select(company_id, company, ip_market_share)

results_ren4 <- results_ren3 %>%
  left_join(data_ren, by = c("company_id", "company"))

# Calculate profit after each impact (company level) [cumulative]
results_ren5 <- results_ren4 %>%
  mutate(profit_marketgrowth = market_cap_2017 * (1 + marketgrowth_impact_pct),
         profit_marketshare = market_cap_2017 * (1 + marketgrowth_impact_pct + marketshare_impact_pct),
         profit_final = market_cap_2017 * (1 + profit_impact_pct))

# Summarise variables over categories and find (market - scenario - group level)
results_ren6 <- results_ren5 %>%
  group_by(scenario, market, median_test) %>%
  summarise(ip_market_share = sum(ip_market_share * market_cap_2017) / sum(market_cap_2017),
            market_cap_2017 = sum(market_cap_2017),
            profit_marketgrowth = sum(profit_marketgrowth),
            profit_marketshare = sum(profit_marketshare),
            profit_final = sum(profit_final)) %>%
  ungroup() %>%
  mutate_at(vars(market_cap_2017, profit_marketgrowth, profit_marketshare, profit_final),
            funs(index = . / market_cap_2017)) %>%
  rename(market_cap_index = market_cap_2017_index)

save_dated(results_ren6, "Cleantech_renewables_statistics", folder = "Output", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Create renewables waterfall ----

cleantech_ren_waterfall <- function(plot_scenario, plot_group) {
  
  temp <- results_ren6 %>%
    filter(scenario == plot_scenario & median_test == plot_group) %>%
    select(scenario, market, median_test, contains("_index")) %>%
    rename(profit_base_index = market_cap_index) %>%
    gather(key = category, value = profit, contains("index")) %>%
    mutate(profit = profit * 100) %>%
    mutate(category = case_when(category == "profit_base_index" ~ "Paris NDCs",
                                category == "profit_marketgrowth_index" ~ "Market growth",
                                category == "profit_marketshare_index" ~ "Market share",
                                category == "profit_final_index" ~ gsub("_", " ", plot_scenario),
                                TRUE ~ NA_character_)) %>%
    mutate(category = ordered(category, levels = c("Paris NDCs", "Market growth", "Market share", gsub("_", " ", plot_scenario)))) %>%
    arrange(category) %>%
    # Adjust all values so profit units are absolute instead of % impact relative to Paris NDCs
    mutate(lagged_profit = lag(profit, n = 1),
           delta_profit = profit - lagged_profit) %>%
    # Calculate waterfall variables
    mutate(base_stack = ifelse(category %in% c("Paris NDCs", gsub("_", " ", plot_scenario)), profit, NA_real_),
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
    scale_y_continuous(name = "NPV profits (normalised)", expand = c(0,0), limits = c(0, 160)) +
    scale_fill_manual(values = plot_colours) + 
    theme_vivid(vivid_size = 1.6) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          aspect.ratio = 9 / 21.16)
  
  ggsave(paste0("4_Asset_impacts/Output/Plots/CM_waterfalls/Solar_", plot_group, "_", plot_scenario, ".png"), width = 21.16, height = 9)
  
}

plot_colours <- c("base_stack" = rgb(0, 143, 159, max = 255), "fall_stack" = rgb(255, 77, 166, max = 255), "rise_stack" = rgb(0, 196, 103, max = 255),
                  "invisible_stack" = NA)
cleantech_ren_waterfall("2DS_Balanced_Transformation", "ABOVE")
cleantech_ren_waterfall("2DS_Balanced_Transformation", "BELOW")

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Group data and prepare for automobiles waterfall ----

results_auto5 <- results_auto4 %>%
  select(scenario, company_id, company, market_cap_2017, Automobiles_profit_impact,
         GR_EVs_marketgrowth_impact, GR_EVs_marketshare_impact) %>%
  mutate(profit_final = market_cap_2017 + Automobiles_profit_impact + GR_EVs_marketgrowth_impact + GR_EVs_marketshare_impact) %>%
  mutate(profit_impact_pct = profit_final / market_cap_2017 - 1)

# Find medians for automobiles
results_auto6 <- results_auto5 %>%
  group_by(scenario) %>% 
  mutate(median_impact = quantile(profit_impact_pct, probs = 0.5),
         median_test = case_when(profit_impact_pct <= median_impact ~ "BELOW",
                                 TRUE ~ "ABOVE")) %>%
  ungroup()

# Add IP market share to the dataset
data_EV <- data_cm %>%
  select(scenario, company_id, company, year, ve_category, ip_market_share) %>%
  filter(scenario == "Paris_NDCs" & year == 2020 & ve_category == "EV_aggregate") %>%
  select(company_id, company, ip_market_share)

results_auto_test <- results_auto6 %>%
  left_join(data_EV, by = c("company_id", "company")) %>%
  select(scenario, company_id, company, ip_market_share)

# Add ICE and EV market cap / revenue share to the dataset for summary statistics
results_auto7 <- results_auto4 %>%
  select(scenario, company_id, company, Automobiles_market_cap_2017, GR_EVs_market_cap_2017) %>%
  gather(key = category, value = share, (Automobiles_market_cap_2017:GR_EVs_market_cap_2017)) %>%
  group_by(scenario, company_id, company) %>%
  mutate(category = case_when(category == "Automobiles_market_cap_2017" ~ "ICE_share",
                              category == "GR_EVs_market_cap_2017" ~ "EV_share",
                              TRUE ~ NA_character_)) %>%
  mutate(share = share / sum(share, na.rm = TRUE)) %>%
  spread(key = category, value = share)

results_auto8 <- results_auto6 %>%
  left_join(results_auto7, by = c("scenario", "company_id", "company")) %>% 
  left_join(results_auto_test, by = c("scenario", "company_id", "company"))

# Summarise variables over categories and find (market - scenario - group level) resultsx
results_auto9 <- results_auto8 %>%
  group_by(scenario, median_test) %>%
  summarise(ICE_share = sum(ICE_share * market_cap_2017, na.rm = TRUE) / sum(market_cap_2017, na.rm = TRUE),
            EV_share = sum(EV_share * market_cap_2017, na.rm = TRUE) / sum(market_cap_2017, na.rm = TRUE),
            ip_market_share = sum(ip_market_share * market_cap_2017, na.rm = TRUE) / sum(market_cap_2017, na.rm = TRUE),
            market_cap_2017 = sum(market_cap_2017),
            Automobiles_profit_impact = sum(Automobiles_profit_impact),
            GR_EVs_marketgrowth_impact = sum(GR_EVs_marketgrowth_impact),
            GR_EVs_marketshare_impact = sum(GR_EVs_marketshare_impact),
            profit_final = sum(profit_final)) %>%
  ungroup() %>%
  rename(ICE_marketcontract = Automobiles_profit_impact,
         EV_marketgrowth = GR_EVs_marketgrowth_impact,
         EV_marketshare = GR_EVs_marketshare_impact) %>%
  mutate_at(vars(market_cap_2017, ICE_marketcontract, EV_marketgrowth, EV_marketshare, profit_final),
            funs(index = . / market_cap_2017)) %>%
  rename(market_cap_index = market_cap_2017_index)

save_dated(results_auto9, "Cleantech_EVs_statistics.rds", folder = "Output", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 6 - Create EVs waterfall ----

cleantech_evs_waterfall <- function(plot_scenario, plot_group) {
  
  temp <- results_auto9 %>%
    filter(scenario == plot_scenario & median_test == plot_group) %>%
    select(scenario, median_test, contains("_index")) %>%
    rename(profit_base_index = market_cap_index) %>%
    mutate(profit_final_index = 0) %>%
    mutate(EV_impact_index = EV_marketgrowth_index + EV_marketshare_index) %>%
    gather(key = category, value = profit, contains("index")) %>%
    mutate(profit = profit * 100) %>%
    mutate(category = case_when(category == "profit_base_index" ~ "Paris NDCs",
                                category == "ICE_marketcontract_index" ~ "ICE market contraction",
                                category == "EV_impact_index" ~ "EV market growth",
                                category == "profit_final_index" ~ gsub("_", " ", plot_scenario),
                                TRUE ~ NA_character_)) %>%
    filter(!is.na(category)) %>%
    mutate(category = ordered(category, levels = c("Paris NDCs", "ICE market contraction", "EV market growth", gsub("_", " ", plot_scenario)))) %>%
    arrange(category) %>%
    # Adjust all values so profit units are absolute instead of % impact relative to Paris NDCs
    mutate(profit = cumsum(profit)) %>%
    mutate(lagged_profit = lag(profit, n = 1),
           delta_profit = profit - lagged_profit) %>%
    # Calculate waterfall variables
    mutate(base_stack = ifelse(category %in% c("Paris NDCs", gsub("_", " ", plot_scenario)), profit, NA_real_),
           invisible_stack = case_when(delta_profit <= 0 ~ lagged_profit + delta_profit,
                                       delta_profit > 0 ~ lagged_profit,
                                       TRUE ~ NA_real_),
           fall_stack = ifelse(delta_profit < 0, -delta_profit, NA_real_),
           rise_stack = ifelse(delta_profit > 0, delta_profit, NA_real_)) %>%
    select(scenario, median_test, category, contains("_stack")) %>%
    gather(key = stack_type, value = stack_value, contains("_stack")) %>%
    mutate(stack_type = ordered(stack_type, levels = c("base_stack", "invisible_stack", "fall_stack", "rise_stack"))) %>%
    mutate(stack_type = fct_rev(stack_type))
  
  ggplot(temp) +
    geom_col(aes(x = category, y = stack_value, fill = stack_type), colour = NA, width = 0.75) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) + 
    scale_y_continuous(name = "NPV profits (normalised)", expand = c(0,0), limits = c(0, 160)) +
    scale_fill_manual(values = plot_colours) + 
    theme_vivid(vivid_size = 1.6) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          aspect.ratio = 9 / 21.16)
  
  ggsave(paste0("4_Asset_impacts/Output/Plots/CM_waterfalls/EVs_", plot_group, "_", plot_scenario, ".png"), width = 21.16, height = 9)
  
}

plot_colours <- c("base_stack" = rgb(0, 143, 159, max = 255), "fall_stack" = rgb(255, 77, 166, max = 255), "rise_stack" = rgb(0, 196, 103, max = 255),
                  "invisible_stack" = NA)
cleantech_evs_waterfall("2DS_Balanced_Transformation", "ABOVE")
cleantech_evs_waterfall("2DS_Balanced_Transformation", "BELOW")