##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  08/07/2019
##### Code author:        Shyamal Patel
##### Dependencies:       1 - 3: equity results at the asset-level, sector-level and for the MSCI ACWI as a whole
#####                     4 - 6: FI results at the asset-level, sector-level and for the MSCI ACWI as a whole
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

# Equity analysis results (sector/market level)
equity_market_results <- readRDS(input_source("Equity_pmarket_results.rds"))

# Equity analysis results (MSCI level)
equity_msci_results <- readRDS(input_source("Equity_msci_results.rds"))

# Equity analysis results (equity level)
equity_level_results <- readRDS(input_source("Equity_level_results.rds"))
  
# FI analysis results (sector/market level)
fi_market_timeseries_results <- readRDS(input_source("FI_market_detailed_results.rds"))

# FI analysis results (instrument level 2050)
fi_level_results <- readRDS(input_source("FI_level_credit_rating_results.rds"))

# FI analysis results (sector/market level 2050)
fi_market_results <- readRDS(input_source("FI_2050_credit_rating_results.rds"))

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Equity scatter plots ----

# Construct 'Computers & Internet' market out of computer services, internet and software for plots
equity_computermarket_results <- equity_level_results %>%
  filter(parent_market %in% c("Computer Services", "Internet", "Software")) %>%
  group_by(scenario) %>%
  summarise_at(vars(index, index_cap),
               funs(list(quantile(., probs = c(0.05, 0.1, 0.5, 0.9, 0.95))))) %>%
  ungroup() %>%
  mutate_at(vars(index, index_cap),
            funs(map(., paste, collapse = ", "))) %>%
  separate(index, into = c("index_p5", "index_p10", "index", "index_p90", "index_p95"), sep = ", ") %>%
  separate(index_cap, into = c("index_cap_p5", "index_cap_p10", "index_cap", "index_cap_p90", "index_cap_p95"), sep = ", ") %>%
  mutate_at(vars(contains("index")), funs(as.numeric(.))) %>%
  mutate(parent_market = "Computers & Internet")

equity_market_results2 <- equity_market_results %>%
  bind_rows(equity_msci_results) %>%
  bind_rows(equity_computermarket_results)

save_dated(equity_market_results2, "Equity_reportmarket_results", folder = "Output", csv = TRUE)

# Define equity scatter plot function
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
  
  temp5 <- equity_market_results2 %>%
    filter(scenario == plot_scen) %>%
    filter(parent_market %in% c("MSCI ACWI", plot_markets)) %>%
    mutate(parent_market = fct_rev(ordered(parent_market, levels = c("MSCI ACWI", plot_markets))))
  
  ggplot() +
    geom_jitter(aes(x = temp4$index, y = temp4$parent_market), height = 0.15, size = 1, alpha = 0.2,
                shape = 16, colour = rgb(0, 191, 214, max = 255)) + 
    geom_errorbarh(aes(xmin = temp5$index_p10, xmax = temp5$index_p90, y = temp5$parent_market), colour = "black", height = 0.2) +
    geom_text_repel(aes(x = temp5$index, y = temp5$parent_market, label = scales::percent_format(1)(round(temp5$index, digits = 2))),
                    nudge_x = 0.1, nudge_y = 0.4, segment.color	= "black", family = "Nordique Pro Semibold") +
    geom_point(aes(x = temp5$index, y = temp5$parent_market), size = 3, shape = 23,
               fill = rgb(0, 143, 159, max = 255), colour = "black") +
    scale_x_continuous(name = paste0("Change in valuation under ", gsub("_", " ", plot_scen), " scenario"), breaks = seq(-1, 1, 0.5),
                       limits = c(-1, 1), labels = scales::percent, expand = c(0, 0)) +
    scale_y_discrete(name = "Equity parent market") +
    theme_vivid(vivid_size = 1.4) +
    theme(panel.grid.major.x = element_line(linetype = "dashed"))
  
  ggsave(paste0("4_Asset_impacts/Output/Plots/Equity/Equity_scatter_", plot_scen, ".png"), width = 16, height = 9, units = c("in"))
}

scenarios <- unique(equity_level_results$scenario)
lapply(scenarios, function(x) {equity_by_markets(plot_scen = x, plot_markets = c("Coal", "Exploration and production", "Concrete and cement",
                                                                                 "Iron & Steel", "Automobiles", "Power generation",
                                                                                 "Computers & Internet", "Renewable Energy Eq.")) })

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Equity scenario span plots ----

equity_scenario_span <- function(scen_group_1, scen_group_2, plot_markets) {
  
  temp <- equity_market_results2 %>%
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
  
  unlabelled_plot <- ggplot() +
    # Policy scenario extreme points
    geom_point(aes(x = temp2$index, y = temp2$parent_market), size = 2, shape = 16, position = position_nudge(y = -0.1),
               colour = rgb(215, 161, 0, max = 255)) +
    geom_line(aes(x = temp2$index, y = temp2$parent_market), size = 1, colour = rgb(215, 161, 0, max = 255),
              position = position_nudge(y = -0.1), linetype = "dashed") + 
    # Technology scenario extreme points
    geom_point(aes(x = temp3$index, y = temp3$parent_market), size = 2, shape = 15, position = position_nudge(y = 0.1),
               colour = rgb(215, 0, 109, max = 255)) + 
    geom_line(aes(x = temp3$index, y = temp3$parent_market), size = 1, colour = rgb(215, 0, 109, max = 255),
              position = position_nudge(y = 0.1), linetype = "dashed") + 
    # 2DS Balanced Transformation scenario points
    geom_point(aes(x = temp4$index, y = temp4$parent_market), size = 4, shape = 23,
               fill = rgb(0, 143, 159, max = 255), colour = "black") +
    scale_x_continuous(name = paste0("Change in valuation across ", gsub("_", " ", "all"), " scenarios"), breaks = seq(-0.6, 0.6, 0.3),
                       limits = c(-0.6, 0.7), labels = scales::percent, expand = c(0, 0)) +
    scale_y_discrete(name = "Equity parent market") +
    theme_vivid(vivid_size = 1.4) +
    theme(panel.grid.major.x = element_line(linetype = "dashed"))
  
  ggsave(paste0("4_Asset_impacts/Output/Plots/Equity/Equity_scenarios_span", ".png"), width = 16, height = 9, units = c("in"))
  
  labelled_plot <- unlabelled_plot +
    geom_text_repel(aes(x = temp2$index, y = temp2$parent_market, label = temp2$scenario),
                    colour = rgb(215, 161, 0, max = 255), position = position_nudge(y = -0.2)) +
    geom_text_repel(aes(x = temp3$index, y = temp3$parent_market, label = temp3$scenario),
                    colour = rgb(215, 0, 109, max = 255), position = position_nudge(y = 0.2))
    
  ggsave(paste0("4_Asset_impacts/Output/Plots/Equity/Equity_scenarios_span_labelled", ".png"), width = 16, height = 9, units = c("in"))
  
}

equity_scenario_span(scen_group_1 = c("Below_2DS", "Lack_Of_Coordination", "Late_Action"),
                     scen_group_2 = c("Efficiency_Boost", "EVs_Unplugged", "Renewable_Revolution", "Room_for_CCS"),
                     plot_markets = c("Coal", "Exploration and production", "Concrete and cement",
                                      "Iron & Steel", "Automobiles", "Power generation",
                                      "Computers & Internet", "Renewable Energy Eq."))

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - FI scatter plots ----

# Data cleaning
fi_level_results2 <- fi_level_results %>%
  select(scenario:moody_rating, credit_rating_change_2050)

fi_market_results2 <- fi_market_results %>%
  filter(year == 2050) %>%
  filter(category == "credit_rating_change")

# Recast market names
new_market_names <- tibble(fi_results_market = c("MSCI ACWI", "Exploration and production",
                                                 "Emissions intensive industries", "Autos",
                                                 "Power generation", "Computers & internet", "Other sectors"),
                           fi_results_market_new = c("MSCI ACWI", "Oil & Gas E&P",
                                                     "Emissions-intensives", "Automobiles & auto parts",
                                                     "Power generation", "Computers & internet", "Other sectors"))

fi_level_results3 <- fi_level_results2 %>%
  left_join(new_market_names, by = "fi_results_market") %>%
  mutate(credit_rating_change_2050 = credit_rating_change_2050 * (-1))

fi_market_results3 <- fi_market_results2 %>%
  left_join(new_market_names, by = "fi_results_market") %>%
  mutate_at(vars(contains("quantile_0")),
            funs(. * (-1)))

# Define FI scatter plot function
fi_by_markets <- function(plot_scen = NULL) {
  
  temp1 <- fi_level_results3 %>%
    filter(scenario == plot_scen & fi_results_market_new != "Other sectors")
  
  temp2 <- fi_level_results3 %>%
    filter(scenario == plot_scen & fi_results_market_new != "Other sectors") %>%
    mutate(fi_results_market_new = "MSCI ACWI")
  
  temp3 <- temp1 %>%
    bind_rows(temp2) %>%
    mutate(fi_results_market_new = fct_rev(ordered(fi_results_market_new, levels = c("MSCI ACWI", "Oil & Gas E&P",
                                                                                     "Emissions-intensives", "Automobiles & auto parts",
                                                                                     "Power generation", "Computers & internet"))))
  
  temp4 <- fi_market_results3 %>%
    filter(scenario == plot_scen & fi_results_market_new != "Other sectors") %>%
    mutate(fi_results_market_new = fct_rev(ordered(fi_results_market_new, levels = c("MSCI ACWI", "Oil & Gas E&P",
                                                                                     "Emissions-intensives", "Automobiles & auto parts",
                                                                                     "Power generation", "Computers & internet"))))
  
  ggplot() +
    geom_jitter(aes(x = temp3$credit_rating_change_2050, y = temp3$fi_results_market_new), height = 0.15, size = 1, alpha = 0.2,
                shape = 16, colour = rgb(0, 191, 214, max = 255)) +
    geom_errorbarh(aes(xmin = temp4$quantile_0.9, xmax = temp4$quantile_0.1, y = temp4$fi_results_market_new),
                   colour = "black", height = 0.1) + 
    geom_text_repel(aes(x = temp4$quantile_0.5, y = temp4$fi_results_market_new, label = scales::comma(temp4$quantile_0.5, accuracy = 0.1)),
                    nudge_x = 0.1, nudge_y = 0.4, segment.colour = "black", family = "Nordique Pro Semibold") +
    geom_point(aes(x = temp4$quantile_0.5, y = temp4$fi_results_market_new), size = 3, shape = 23,
               fill = rgb(0, 143, 159, max = 255), colour = "black") +
    scale_x_continuous(name = paste0("Change in credit rating under ", gsub("_", " ", plot_scen), " scenario (notches)"),
                       breaks = seq(-4, 2, 1), limits = c(-4, 2), expand = c(0, 0)) +
    scale_y_discrete(name = "Corporate bonds parent market") +
    theme_vivid(vivid_size = 1.4) + 
    theme(panel.grid.major.x = element_line(linetype = "dashed"))
  
  ggsave(paste0("4_Asset_impacts/Output/Plots/FI/FI_scatter_", plot_scen, ".png"), width = 16, height = 9, units = c("in"))
  
}

scenarios <- unique(fi_market_results3$scenario)
lapply(scenarios, fi_by_markets)

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - FI time series area plot ----

fi_percentiles <- function(plot_scenario, plot_sector) {
  
  temp <- fi_market_timeseries_results %>%
    filter(scenario == plot_scenario) %>%
    filter(fi_results_market == plot_sector)
  
  ggplot(temp %>% mutate(rating_change_q10 = -rating_change_q10,
                         rating_change_q90 = -rating_change_q90,
                         rating_change_q50 = -rating_change_q50)) +
    geom_ribbon(aes(x = year, ymin = rating_change_q10, ymax = rating_change_q90),
                alpha = 0.9, fill = rgb(red = 196, green = 249, blue = 255, max = 255), colour = NA) + 
    geom_line(aes(x = year, y = rating_change_q50), size = 0.8) +
    geom_line(aes(x = year, y = rating_change_q10), size = 0.8, linetype = "dashed") + 
    geom_line(aes(x = year, y = rating_change_q90), size = 0.8, linetype = "dashed") +
    annotate("text", x = 2050.1, y = temp$rating_change_q50[temp$year == 2050] * (-1), label = "Median", colour = "black", hjust = 0, size = 5) +
    annotate("text", x = 2050.1, y = temp$rating_change_q10[temp$year == 2050] * (-1), label = "90th percentile", colour = "black", hjust = 0, size = 5) +
    annotate("text", x = 2050.1, y = temp$rating_change_q90[temp$year == 2050] * (-1), label = "10th percentile", colour = "black", hjust = 0, size = 5) +
    scale_y_continuous(name = "Change in credit rating", limits = c(-3, 1), expand = c(0, 0)) + 
    scale_x_continuous(name = NULL, limits = c(2017, 2054), expand = c(0, 0)) +
    theme_vivid(vivid_size = 1.4)
  
  ggsave(paste0("4_Asset_impacts/Output/Plots/FI/", "FI_percentiles_", plot_sector, "_", plot_scenario, ".png"), width = 16, height = 9, units = "in")
}

fi_percentiles_input <- expand.grid(scenario = unique(fi_market_timeseries_results$scenario),
                                    fi_results_market = unique(fi_market_timeseries_results$fi_results_market),
                                    stringsAsFactors = FALSE)
map2(fi_percentiles_input$scenario, fi_percentiles_input$fi_results_market, fi_percentiles)