##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  03/04/2019
##### Code author:        Shyamal Patel
##### Dependencies:       1. Cost & competition model results under a variety of parameter assumptions
#####                        See attributes and Section 1 for more details
##### Notes:              None
##### Called by:          N/A

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "4_Asset_impacts"
source("utils.R")

# Recast input source for Interim folder
input_source <- function(filename) {
  fs::path(here::here(main_save_folder), "Interim", filename)
}

### Read in cost & competition model results for each of the 3 value chain element model runs with
### CM and DD models switched on, and verify that attributes are correct
### (the 'right' switches are ON and OFF as we cycle through)

# Carbon cost only results
results_cc <- readRDS("3_Cost_and_competition/Output/Dated/190225_1106_Subsidiary_results.rds")
glimpse(attr(results_cc, "parameters"))

# Abatement on results
results_abt <- readRDS("3_Cost_and_competition/Output/Dated/190225_1107_Subsidiary_results.rds")
glimpse(attr(results_abt, "parameters"))

# Cost pass through on results (final)
results_cpt <- readRDS("3_Cost_and_competition/Output/Dated/190225_1108_Subsidiary_results.rds")
glimpse(attr(results_cpt, "parameters"))

# Model panel for emissions, elasiticity, product differentiation and DD / CM model results
panel <- readRDS("3_Cost_and_competition/Interim/Dated/190220_1508_Cleaned_model_panel.rds")

# Check that results have been properly calibrated - differences should be in abatement potential for (1) and 
# in CPT, sales impact and Q reallocation for (2)
# library(daff)
# render_diff(diff_data(attr(results_cc, "parameters"), attr(results_abt, "parameters")))
# render_diff(diff_data(attr(results_abt, "parameters"), attr(results_cpt, "parameters")))

### Read in cost & competition model results for each of the 23 value chain element model runs with 
### CM and DD models switched off (looking at CC in isolation)

# Cost & competition model only - carbon cost only results
results_cc_only_cc <- readRDS("3_Cost_and_competition/Output/Dated/190222_1702_Subsidiary_results.rds")
glimpse(attr(results_cc_only_cc, "parameters"))

# Cost & competition model only - abatement on results
results_cc_only_abt <- readRDS("3_Cost_and_competition/Output/Dated/190222_1704_Subsidiary_results.rds")
glimpse(attr(results_cc_only_abt, "parameters"))

# Cost & competition model only - cost pass through on results (final)
results_cc_only_cpt <- readRDS("3_Cost_and_competition/Output/Dated/190222_1705_Subsidiary_results.rds")
glimpse(attr(results_cc_only_cpt, "parameters"))

# Check that results are correctly calibrated
# library(daff)
# render_diff(diff_data(attr(results_cc_only_cc, "parameters"), attr(results_cc_only_abt, "parameters")))
# render_diff(diff_data(attr(results_cc_only_abt, "parameters"), attr(results_cc_only_cpt, "parameters")))

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean up DD and CM model results ---- 

# Find stranding results using the profit impact index and market cap from the model panel
results_dd <- panel %>%
  select(scenario, company_id, company, market, region, market_cap_2017, profit_impact_pct) %>%
  mutate(profit_impact_pct = case_when(substring(market, 1, 3) == "GR_" ~ NA_real_,
                                       TRUE ~ profit_impact_pct)) %>%
  mutate(profit_dd = case_when(!is.na(profit_impact_pct) ~ market_cap_2017 * (1 + profit_impact_pct),
                               TRUE ~ market_cap_2017)) %>%
  select(-profit_impact_pct)

# Find cleantech market results using the profit impact index and market cap from the model panel 
# [note that this is cumulative so includes the effects of both DD and CM]
results_cm <- panel %>%
  select(scenario, company_id, company, market, region, market_cap_2017, profit_impact_pct) %>%
  mutate(profit_cm = case_when(!is.na(profit_impact_pct) ~ market_cap_2017 * (1 + profit_impact_pct),
                               TRUE ~ market_cap_2017)) %>%
  select(-profit_impact_pct)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Combine MSCI ACWI waterfall datasets ----

shorten_tibble <- function(shorten_arg) {
  
  data <- shorten_arg[[1]]
  subscript <- shorten_arg[[2]]
  
  temp <- data %>%
    select(scenario, company_id, company, market, region, market_cap_2017, market_cap_model, index, index_cap) %>%
    rename_at(vars(market_cap_model, index, index_cap),
              funs(paste0(., "_", subscript)))
  
  return(temp)
}

list_cc <- list(results_cc, "cc")
list_abt <- list(results_abt, "abt")
list_cpt <- list(results_cpt, "cpt")

results_comb <- map(list(list_cc, list_abt, list_cpt), shorten_tibble) %>%
  reduce(left_join)

# Verify that all the market_cap_model values are the same
results_comb2 <- results_comb %>%
  select(-contains("market_cap_model")) %>%
  select(-starts_with("index_cap")) %>%
  mutate_at(vars(index_cc, index_abt, index_cpt),
            funs(profit = market_cap_2017 * (1 + .))) %>%
  rename_at(vars(ends_with("profit")),
            funs(paste0("profit_", gsub("index_", "", gsub("_profit", "", .))))) %>%
  select(-starts_with("index"))

# Join in demand destruction and cleantech market results
results_comb3 <- results_comb2 %>%
  left_join(results_dd) %>%
  left_join(results_cm)

# Summarise over regions, markets and companies
results_comb4 <- results_comb3 %>%
  group_by(scenario) %>%
  summarise_at(vars(market_cap_2017, profit_dd, profit_cm, profit_cc, profit_abt, profit_cpt),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate_at(vars(market_cap_2017, profit_dd, profit_cm, profit_cc, profit_abt, profit_cpt),
            funs(index = . / market_cap_2017))

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Summarise MSCI ACWI data and create waterfall ----

msci_acwi_waterfall <- function(plot_scenario) {
  
  temp <- results_comb4 %>%
    filter(scenario == plot_scenario) %>%
    select(scenario, contains("_index")) %>%
    # Add a column which takes the value of CPT (rest can be differenced to be effects of the change from previous column)
    mutate(profit_final_index = profit_cpt_index) %>%
    gather(key = category, value = profit, (market_cap_2017_index:profit_final_index)) %>%
    mutate(profit = profit * 100) %>%
    mutate(category = case_when(category == "market_cap_2017_index" ~ "Paris NDCs",
                                category == "profit_dd_index" ~ "Demand destruction",
                                category == "profit_cm_index" ~ "Cleantech markets",
                                category == "profit_cc_index" ~ "Carbon costs",
                                category == "profit_abt_index" ~ "Abatement",
                                category == "profit_cpt_index" ~ "Cost pass through",
                                category == "profit_final_index" ~ gsub("_", " ", plot_scenario),
                                TRUE ~ NA_character_)) %>%
    mutate(category = ordered(category, levels = c("Paris NDCs", "Demand destruction", "Cleantech markets",
                                                   "Carbon costs", "Abatement", "Cost pass through", gsub("_", " ", plot_scenario)))) %>%
    arrange(category) %>%
    mutate(lagged_profit = lag(profit, n = 1),
           delta_profit = profit - lagged_profit) %>%
    # Calculate waterfall variables
    mutate(base_stack = ifelse(category %in% c("Paris NDCs", gsub("_", " ", plot_scenario)), profit, NA_real_),
           invisible_stack = case_when(delta_profit <= 0 ~ lagged_profit + delta_profit,
                                       delta_profit > 0 ~ lagged_profit,
                                       TRUE ~ NA_real_),
           fall_stack = ifelse(delta_profit < 0, -delta_profit, NA_real_),
           rise_stack = ifelse(delta_profit > 0, delta_profit, NA_real_)) %>%
    select(scenario, category, contains("_stack")) %>%
    gather(key = stack_type, value = stack_value, contains("_stack")) %>%
    mutate(stack_type = ordered(stack_type, levels = c("base_stack", "invisible_stack", "fall_stack", "rise_stack"))) %>%
    mutate(stack_type = fct_rev(stack_type))
  
  waterfall <- ggplot(temp) +
    geom_col(aes(x = category, y = stack_value, fill = stack_type), colour = NA, width = 0.75) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
    scale_y_continuous(name = "NPV profits (normalised)", expand = c(0,0)) +
    scale_fill_manual(values = plot_colours) + 
    theme_vivid(vivid_size = 1.6) +
    theme(legend.position = "none",
          axis.title.x = element_blank())
  
  waterfall <- waterfall + coord_cartesian(ylim = c(40, 100))
  
  ggsave(paste0("4_Asset_impacts/Output/Plots/MSCI_waterfalls/MSCI_ACWI_", plot_scenario, ".png"), plot = waterfall, width = 16, height = 9)
  
}

plot_colours <- c("base_stack" = rgb(0, 143, 159, max = 255), "fall_stack" = rgb(255, 77, 166, max = 255), "rise_stack" = rgb(0, 196, 103, max = 255),
                  "invisible_stack" = NA)
map(unique(results_comb4$scenario)[unique(results_comb4$scenario) != "Paris_NDCs"], msci_acwi_waterfall)

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Combine cost & competition model only datasets ----

list_cc_only_cc <- list(results_cc_only_cc, "cc")
list_cc_only_abt <- list(results_cc_only_abt, "abt")
list_cc_only_cpt <- list(results_cc_only_cpt, "cpt")

results_cc_only_comb <- map(list(list_cc_only_cc, list_cc_only_abt, list_cc_only_cpt), shorten_tibble) %>%
  reduce(left_join)

# Verify that all the market_cap_model values are the same
results_cc_only_comb2 <- results_cc_only_comb %>%
  select(-contains("market_cap_model")) %>%
  select(-starts_with("index_cap")) %>%
  mutate_at(vars(index_cc, index_abt, index_cpt),
            funs(profit = market_cap_2017 * (1 + .))) %>%
  rename_at(vars(ends_with("profit")),
            funs(paste0("profit_", gsub("index_", "", gsub("_profit", "", .))))) %>%
  select(-starts_with("index"))

# Summarise over regions
results_cc_only_comb3 <- results_cc_only_comb2 %>%
  group_by(scenario, company_id, company, market) %>%
  summarise_at(vars(market_cap_2017, profit_cc, profit_abt, profit_cpt),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate_at(vars(profit_cc, profit_abt, profit_cpt),
            funs(index = . / market_cap_2017 - 1)) %>%
  rename_at(vars(ends_with("_index")),
            funs(paste0("index_", gsub("profit_", "", gsub("_index", "", .)))))

# Shorten the panel dataset to essential variables only and calculate emissions intensity at the business segment level
# summarise over regions first
panel2 <- panel %>%
  group_by(scenario, company_id, company, market) %>%
  summarise_at(vars(revenue_2017, co2_scope_1_2017, co2_scope_2_2017, co2_scope_3_2017),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup()

#--------------------------------------------------------------------------------------------------

##### SECTION 6 - Group cost & competition model only datasets based on
#####             above / below median and prepare for waterfall ----

# Find median 
results_cc_only_comb4 <- results_cc_only_comb3 %>%
  group_by(scenario, market) %>% 
  mutate(median_impact = quantile(index_cpt, probs = 0.5),
         median_test = case_when(index_cpt <= median_impact ~ "BELOW",
                                 TRUE ~ "ABOVE"))

# Add emissions intensity to the data
results_cc_only_comb5 <- results_cc_only_comb4 %>%
  left_join(panel2, by = c("scenario", "company_id", "company", "market"))

# Summarise variables over categories and index so initial profits are 1
results_cc_only_comb6 <- results_cc_only_comb5 %>%
  group_by(scenario, market, median_test) %>% 
  summarise_at(vars(market_cap_2017, profit_cc, profit_abt, profit_cpt, revenue_2017, co2_scope_1_2017, co2_scope_2_2017, co2_scope_3_2017),
               funs(sum(., na.rm = TRUE))) %>% 
  mutate_at(vars(market_cap_2017, profit_cc, profit_abt, profit_cpt),
            funs(index = . / market_cap_2017)) %>%
  mutate_at(vars(co2_scope_1_2017, co2_scope_2_2017, co2_scope_3_2017),
            funs(intensity = . / revenue_2017)) %>%
  ungroup() %>%
  select(-revenue_2017, -co2_scope_1_2017, -co2_scope_2_2017, -co2_scope_3_2017)

save_dated(results_cc_only_comb6, "Cost_and_comp_statistics", folder = "Output", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 7 - Create cost & competition model only - above / below median waterfalls ----

cost_comp_waterfall <- function(plot_scenario, plot_market, plot_group) {
  
  temp <- results_cc_only_comb6 %>%
    filter(scenario == plot_scenario & market == plot_market & median_test == plot_group) %>%
    select(scenario, market, median_test, contains("_index")) %>%
    # Add a column which takes the value of CPT (rest can be differenced to be effects of the change from previous column)
    mutate(profit_final_index = profit_cpt_index) %>%
    gather(key = category, value = profit, (market_cap_2017_index:profit_final_index)) %>%
    mutate(profit = profit * 100) %>%
    mutate(category = case_when(category == "market_cap_2017_index" ~ "Paris NDCs",
                                category == "profit_cc_index" ~ "Carbon costs",
                                category == "profit_abt_index" ~ "Abatement",
                                category == "profit_cpt_index" ~ "Cost pass through",
                                category == "profit_final_index" ~ "2DS Balanced Transformation",
                                TRUE ~ NA_character_)) %>%
    mutate(category = ordered(category, levels = c("Paris NDCs", "Carbon costs", "Abatement", "Cost pass through", "2DS Balanced Transformation"))) %>%
    arrange(category) %>%
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
    scale_y_continuous(name = "NPV profits (normalised)", expand = c(0,0), limits = c(0, 130)) +
    scale_fill_manual(values = plot_colours) + 
    theme_vivid(vivid_size = 1.6) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          aspect.ratio = 9 / 21.16)
  
  ggsave(paste0("4_Asset_impacts/Output/Plots/CC_waterfalls/", plot_market, "_", plot_group, "_", plot_scenario, ".png"), width = 21.16, height = 9)
  
}

plot_colours <- c("base_stack" = rgb(0, 143, 159, max = 255), "fall_stack" = rgb(255, 77, 166, max = 255), "rise_stack" = rgb(0, 196, 103, max = 255),
                  "invisible_stack" = NA)
cost_comp_waterfall("2DS_Balanced_Transformation", "Concrete and cement", "BELOW")
cost_comp_waterfall("2DS_Balanced_Transformation", "Concrete and cement", "ABOVE")
cost_comp_waterfall("2DS_Balanced_Transformation", "Power generation", "BELOW")
cost_comp_waterfall("2DS_Balanced_Transformation", "Power generation", "ABOVE")