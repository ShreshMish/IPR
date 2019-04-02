##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  01/04/2019
##### Code author:        Shyamal Patel
##### Dependencies:       1. Results from fixed income and equity analysis
##### Notes:              This script sets out functions for plotting asset-level impacts
##### Called by:          N/A

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "4_Asset_impacts"
source("utils.R")

# Read in FI results
fi_level_2050_results <- readRDS(fs::path(main_save_folder, "Output/FI_level_credit_rating_results.rds"))
fi_summary_2050_results <- readRDS(fs::path(main_save_folder, "Output/FI_2050_credit_rating_results.rds"))

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Data cleaning ----

fi_level_2050_results2 <- fi_level_2050_results %>%
  select(scenario:moody_rating, credit_rating_change_2050)

fi_summary_2050_results2 <- fi_summary_2050_results %>%
  filter(year == 2050) %>%
  filter(category == "credit_rating_change")

# Recast market names
new_market_names <- tibble(fi_results_market = c("MSCI ACWI", "Exploration and production",
                                                 "Emissions intensive industries", "Autos",
                                                 "Power generation", "Computers & internet", "Other sectors"),
                           fi_results_market_new = c("MSCI ACWI", "Oil & Gas E&P",
                                                     "Emissions-intensives", "Automobiles & auto parts",
                                                     "Power generation", "Computers & internet", "Other sectors"))

fi_level_2050_results3 <- fi_level_2050_results2 %>%
  left_join(new_market_names, by = "fi_results_market") %>%
  mutate(credit_rating_change_2050 = credit_rating_change_2050 * (-1))

fi_summary_2050_results3 <- fi_summary_2050_results2 %>%
  left_join(new_market_names, by = "fi_results_market") %>%
  mutate_at(vars(contains("quantile_0")),
            funs(. * (-1)))

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Define fixed income plot function ----

fi_by_markets <- function(plot_scen = NULL) {
  
  temp1 <- fi_level_2050_results3 %>%
    filter(scenario == plot_scen & fi_results_market_new != "Other sectors")
  
  temp2 <- fi_level_2050_results3 %>%
    filter(scenario == plot_scen & fi_results_market_new != "Other sectors") %>%
    mutate(fi_results_market_new = "MSCI ACWI")
  
  temp3 <- temp1 %>%
    bind_rows(temp2) %>%
    mutate(fi_results_market_new = fct_rev(ordered(fi_results_market_new, levels = c("MSCI ACWI", "Oil & Gas E&P",
                                                                                     "Emissions-intensives", "Automobiles & auto parts",
                                                                                     "Power generation", "Computers & internet"))))
  
  temp4 <- fi_summary_2050_results3 %>%
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
  
  ggsave(paste0("4_Asset_impacts/Output/Plots/FI_impacts_", plot_scen, ".png"), width = 16, height = 9, units = c("in"))

}

scenarios <- unique(fi_summary_2050_results3$scenario)
lapply(scenarios, fi_by_markets)
