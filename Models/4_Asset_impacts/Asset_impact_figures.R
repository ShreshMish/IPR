##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  19/02/2019
##### Code author:        Shyamal Patel
##### Dependencies:       1. Results from fixed income and equity analysis
##### Notes:              This script sets out functions for plotting asset-level impacts
##### Called by:          N/A

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "4_Asset_impacts"
source("utils.R")

# Read in equity results
equity_level_results <- readRDS(fs::path(main_save_folder, "Output/Equity_level_results.rds"))
equity_parentmarket_results <- readRDS(fs::path(main_save_folder, "Output/Equity_pmarket_results.rds"))

# Read in FI results
# fi_level_results <- readRDS(fs::path(main_save_folder, "Output/FI_instrument_level_results.rds"))
# fi_parentmarket_results <- readRDS(fs::path(main_save_folder, "Output/FI_pmarket_results.rds"))

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Calculate MSCI level impacts ----

equity_msci_results <- equity_parentmarket_results %>%
  mutate(profit_npv_post_tax = market_cap * (1 + index),
         profit_npv_post_tax_cap = market_cap * (1 + index_cap)) %>%
  group_by(scenario) %>% 
  summarise_at(vars(market_cap, profit_npv_post_tax, profit_npv_post_tax_cap),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(index = profit_npv_post_tax / market_cap - 1,
         index_cap = profit_npv_post_tax_cap / market_cap - 1) %>% 
  select(-profit_npv_post_tax, -profit_npv_post_tax_cap) %>%
  mutate(parent_market = "MSCI ACWI") %>%
  select(scenario, parent_market, everything())

equity_parentmarket_results2 <- equity_parentmarket_results %>%
  bind_rows(equity_msci_results)

# fi_msci_results <- fi_level_results %>%
#   group_by(scenario) %>%
#   summarise_at(vars(starts_with("altman_z_")), funs(median(., na.rm = TRUE))) %>%
#   ungroup() %>%
#   mutate(parent_market = "MSCI ACWI")
# 
# fi_parentmarket_results2 <- fi_parentmarket_results %>%
#   bind_rows(fi_msci_results) %>%
#   ungroup()
# 
# fi_level_results %<>% ungroup()

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Define equity plot functions ----

equity_by_markets <- function(plot_scen = NULL, plot_markets = c("")) {
  
  temp1 <- equity_level_results %>%
    filter(scenario == plot_scen) %>%
    filter(parent_market %in% plot_markets)
  
  temp2 <- equity_level_results %>%
    filter(scenario == plot_scen) %>%
    mutate(parent_market = "MSCI ACWI")
  
  temp3 <- temp1 %>%
    bind_rows(temp2) %>%
    mutate(parent_market = fct_rev(ordered(parent_market, levels = c("MSCI ACWI", sort(plot_markets)))))
  
  temp4 <- equity_parentmarket_results2 %>%
    filter(scenario == plot_scen) %>%
    filter(parent_market %in% c("MSCI ACWI", plot_markets)) %>%
    mutate(parent_market = fct_rev(ordered(parent_market, levels = c("MSCI ACWI", sort(plot_markets)))))
  
  ggplot() +
    geom_jitter(aes(x = temp3$index_cap, y = temp3$parent_market), height = 0.15, size = 1, alpha = 0.2) + 
    geom_point(aes(x = temp4$index_cap, y = temp4$parent_market), size = 3, shape = 23) +
    scale_x_continuous(name = "Value impairment (% impact)", breaks = seq(-1, 1.5, 0.5), labels = scales::percent) +
    scale_y_discrete(name = "Sector") +
    ggtitle(paste0(plot_scen, " equity impact results")) +
    theme_vivid(vivid_size = 1.25)
  
  ggsave(paste0("4_Asset_impacts/Output/Equity_impacts_", plot_scen, ".png"), width = 16, height = 9, units = c("in"))
}

equity_by_markets(plot_scen = "2DS_central", plot_markets = c("Power generation", "Coal",
                                                              "Exploration and production", "Concrete and cement",
                                                              "Automobiles", "Iron & Steel"))
scenarios <- unique(equity_level_results$scenario)
lapply(scenarios, function(x) {equity_by_markets(plot_scen = x, plot_markets = c("Power generation", "Coal", "Exploration and production", "Concrete and cement",
                                                                                 "Automobiles", "Iron & Steel")) })

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Define fixed income plot functions ----

# fi_by_markets <- function(plot_scen = NULL, plot_markets = c(""), plot_year) {
#   
#   altman_year <- rlang::sym(paste0("altman_z_", plot_year))
#   
#   temp1 <- fi_level_results %>%
#     filter(scenario == plot_scen) %>%
#     filter(parent_market %in% plot_markets)
#   
#   temp2 <- fi_level_results %>%
#     filter(scenario == plot_scen) %>%
#     mutate(parent_market = "MSCI ACWI")
#   
#   temp3 <- temp1 %>%
#     bind_rows(temp2) %>%
#     mutate(parent_market = fct_rev(ordered(parent_market, levels = c("MSCI ACWI", sort(plot_markets))))) %>%
#     select(company_id, company, parent_market, !!altman_year) %>%
#     rename(altman = !!altman_year)
#   
#   temp4 <- fi_parentmarket_results2 %>%
#     filter(scenario == plot_scen) %>%
#     filter(parent_market %in% c("MSCI ACWI", plot_markets)) %>%
#     mutate(parent_market = fct_rev(ordered(parent_market, levels = c("MSCI ACWI", sort(plot_markets))))) %>%
#     select(parent_market, !!altman_year) %>%
#     rename(altman = !!altman_year)
#   
#   ggplot() +
#     geom_jitter(aes(x = temp3$altman, y = temp3$parent_market), height = 0.15, size = 1, alpha = 0.2) + 
#     geom_point(aes(x = temp4$altman, y = temp4$parent_market), size = 3, shape = 23) +
#     scale_x_continuous(name = "Altman Z-score value", breaks = seq(0, 3, 0.5), limits = c(0, 3)) +
#     scale_y_discrete(name = "Sector") +
#     ggtitle(paste0(plot_scen, " Z-score impact results")) +
#     theme_vivid(vivid_size = 1.25)
#   
#   ggsave(paste0("4_Asset_impacts/Output/FI_impacts_", plot_year, "_", plot_scen, ".png"), width = 16, height = 9, units = c("in"))
# }
# 
# scenarios <- unique(fi_by_markets$scenario)
# 
# # Altman Z-score results by scenario for 2030
# lapply(scenarios, function(x) {fi_by_markets(plot_scen = x,
#                                              plot_year = 2030, 
#                                              plot_markets = c("Power generation", "Coal", "Exploration and production", "Concrete and cement",
#                                                               "Automobiles", "Iron & Steel")) })
# 
# # Altman Z-score results by scenario for 2050
# lapply(scenarios, function(x) {fi_by_markets(plot_scen = x,
#                                              plot_year = 2050, 
#                                              plot_markets = c("Power generation", "Coal", "Exploration and production", "Concrete and cement",
#                                                               "Automobiles", "Iron & Steel")) })
