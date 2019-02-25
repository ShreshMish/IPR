##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  24/02/2019
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
equity_msci_results <- readRDS(fs::path(main_save_folder, "Output", "Equity_msci_results.rds"))

# Read in FI results
# fi_level_results <- readRDS(fs::path(main_save_folder, "Output/FI_instrument_level_results.rds"))
# fi_parentmarket_results <- readRDS(fs::path(main_save_folder, "Output/FI_pmarket_results.rds"))

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Calculate MSCI level and combined markets impact ----

equity_computermarket_results <- equity_level_results %>%
  filter(parent_market %in% c("Computer Services", "Internet", "Software")) %>%
  group_by(scenario) %>%
  summarise(index_p5 = quantile(index, probs = 0.05),
            index_p10 = quantile(index, probs = 0.10),
            index_cap_p5 = quantile(index_cap, probs = 0.05),
            index_cap_p10 = quantile(index_cap, probs = 0.10),
            index_p95 = quantile(index, probs = 0.95),
            index_p90 = quantile(index, probs = 0.90),
            index_cap_p95 = quantile(index_cap, probs = 0.95),
            index_cap_p90 = quantile(index_cap, probs = 0.90),
            index = quantile(index, probs = 0.5),
            index_cap = quantile(index_cap, probs = 0.5)) %>%
  ungroup() %>%
  mutate(parent_market = "Computers & Internet")

equity_parentmarket_results2 <- equity_parentmarket_results %>%
  bind_rows(equity_msci_results) %>%
  bind_rows(equity_computermarket_results)

save_dated(equity_parentmarket_results2, "Equity_reportmarket_results", folder = "Output", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Define equity plot functions ----

equity_by_markets <- function(plot_scen = NULL, plot_markets = c("")) {
  
  temp1 <- equity_level_results %>%
    filter(scenario == plot_scen) %>%
    filter(parent_market %in% plot_markets)
  
  temp2 <- equity_level_results %>%
    filter(scenario == plot_scen) %>%
    filter(parent_market %in% c("Computer Services", "Internet", "Software")) %>%
    mutate(parent_market = "Computers & Internet")
  
  temp3 <- equity_level_results %>%
    filter(scenario == plot_scen) %>%
    mutate(parent_market = "MSCI ACWI")
  
  temp4 <- temp1 %>%
    bind_rows(temp2) %>%
    bind_rows(temp3) %>%
    mutate(parent_market = fct_rev(ordered(parent_market, levels = c("MSCI ACWI", plot_markets))))
  
  temp5 <- equity_parentmarket_results2 %>%
    filter(scenario == plot_scen) %>%
    filter(parent_market %in% c("MSCI ACWI", plot_markets)) %>%
    mutate(parent_market = fct_rev(ordered(parent_market, levels = c("MSCI ACWI", plot_markets))))
  
  ggplot() +
    geom_jitter(aes(x = temp4$index, y = temp4$parent_market), height = 0.15, size = 1, alpha = 0.2,
                shape = 16, colour = rgb(0, 191, 214, max = 255)) + 
    geom_errorbarh(aes(xmin = temp5$index_p10, xmax = temp5$index_p90, y = temp5$parent_market), colour = "black", height = 0.2) +
    geom_text_repel(aes(x = temp5$index, y = temp5$parent_market, label = scales::percent(round(temp5$index, digits = 2))),
                    nudge_x = 0.1, nudge_y = 0.4, segment.color	= "black") +
    geom_point(aes(x = temp5$index, y = temp5$parent_market), size = 3, shape = 23,
               fill = rgb(0, 143, 159, max = 255), colour = "black") +
    scale_x_continuous(name = paste0("Change in valuation under ", gsub("_", " ", plot_scen), " scenario"), breaks = seq(-1, 1, 0.5),
                       limits = c(-1, 1.2), labels = scales::percent, expand = c(0, 0)) +
    scale_y_discrete(name = "Equity parent market") +
    theme_vivid(vivid_size = 1.4) +
    theme(panel.grid.major.x = element_line(linetype = "dashed"))
  
  ggsave(paste0("4_Asset_impacts/Output/Plots/Equity_impacts_", plot_scen, ".png"), width = 16, height = 9, units = c("in"))
}

scenarios <- unique(equity_level_results$scenario)
lapply(scenarios, function(x) {equity_by_markets(plot_scen = x, plot_markets = c("Coal", "Exploration and production", "Concrete and cement",
                                                                                 "Iron & Steel", "Automobiles", "Power generation",
                                                                                 "Computers & Internet", "Renewable Energy Eq.")) })

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Define equity scenario span plot function ----

market_by_scenario <- function(scen_group_1, scen_group_2, plot_markets) {
  
  temp <- equity_parentmarket_results2 %>%
    filter(parent_market %in% c("MSCI ACWI", plot_markets)) %>%
    mutate(parent_market = fct_rev(ordered(parent_market, levels = c("MSCI ACWI", plot_markets))))
  
  temp2 <- temp %>%
    filter(scenario %in% scen_group_1) %>%
    group_by(parent_market) %>%
    filter(index == max(index) | index == min(index)) %>%
    ungroup()
  
  temp3 <- temp %>%
    filter(scenario %in% scen_group_2) %>%
    group_by(parent_market) %>%
    filter(index == max(index) | index == min(index)) %>%
    ungroup()
  
  temp4 <- temp %>%
    filter(scenario == "2DS_Balanced_Transformation")
  
  ggplot() +
    # Policy scenario extreme points
    geom_point(aes(x = temp2$index, y = temp2$parent_market), size = 2, shape = 16, position = position_nudge(y = -0.1),
               colour = rgb(215, 161, 0, max = 255)) +
    geom_line(aes(x = temp2$index, y = temp2$parent_market), size = 1, colour = rgb(215, 161, 0, max = 255),
              position = position_nudge(y = -0.1), linetype = "dashed") + 
    # geom_text_repel(aes(x = temp2$index, y = temp2$parent_market, label = temp2$scenario),
    #                 colour = rgb(215, 161, 0, max = 255), position = position_nudge(y = -0.2)) +
    # Technology scenario extreme points
    geom_point(aes(x = temp3$index, y = temp3$parent_market), size = 2, shape = 15, position = position_nudge(y = 0.1),
               colour = rgb(215, 0, 109, max = 255)) + 
    geom_line(aes(x = temp3$index, y = temp3$parent_market), size = 1, colour = rgb(215, 0, 109, max = 255),
              position = position_nudge(y = 0.1), linetype = "dashed") + 
    # geom_text_repel(aes(x = temp3$index, y = temp3$parent_market, label = temp3$scenario),
    #                 colour = rgb(215, 0, 109, max = 255), position = position_nudge(y = 0.2)) +
    # 2DS Balanced Transformation scenario points
    geom_point(aes(x = temp4$index, y = temp4$parent_market), size = 4, shape = 23,
               fill = rgb(0, 143, 159, max = 255), colour = "black") +
    scale_x_continuous(name = paste0("Change in valuation under ", gsub("_", " ", "all"), " scenarios"), breaks = seq(-0.5, 0.5, 0.25),
                       limits = c(-0.5, 0.6), labels = scales::percent, expand = c(0, 0)) +
    scale_y_discrete(name = "Equity parent market") +
    theme_vivid(vivid_size = 1.4) +
    theme(panel.grid.major.x = element_line(linetype = "dashed"))
  
  ggsave(paste0("4_Asset_impacts/Output/Plots/Equity_impacts_across_scenarios", ".png"), width = 16, height = 9, units = c("in"))
  
}

market_by_scenario(scen_group_1 = c("Below 2DS", "Lack_Of_Coordination", "Late_Action"),
                   scen_group_2 = c("Efficiency_Boost", "EVs_Unplugged", "Renewable_Revolution", "Room_for_CCS"),
                   plot_markets = c("Coal", "Exploration and production", "Concrete and cement",
                                    "Iron & Steel", "Automobiles", "Power generation",
                                    "Computers & Internet", "Renewable Energy Eq."))

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
