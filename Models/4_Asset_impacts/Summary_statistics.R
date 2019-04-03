##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  03/04/2019
##### Code author:        Shyamal Patel
##### Dependencies:       Results from the cost & competition model
##### Notes:              This script generates summary statistics used in the final HSBC report - none of these numbers appear in any figures
#####                     or tables, but are used to contextualise the results in the text itself
##### Called by:          N/A

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "4_Asset_impacts"
source("utils.R")

# Carbon costs only results
results_cc <- readRDS("3_Cost_and_competition/Output/Dated/190225_1106_Subsidiary_results.rds")
glimpse(attr(results_cc, "parameters"))

# Model panel for emissions, elasticity and product differentiation data
panel <- readRDS("3_Cost_and_competition/Interim/Cleaned_model_panel.rds")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Sectoral exposure shares along with indicators for DD and CM models ----

panel2 <- panel %>%
  select(scenario, parent_market, company_id, company, market, region, profit_impact_pct)

sectoral_stats <- results_cc %>%
  select(scenario, company_id, company, market, region, market_cap_2017) %>%
  left_join(panel2) %>%
  mutate(market_indicator = case_when(!is.na(profit_impact_pct) & substring(market, 1, 3) == "GR_" ~ "CM",
                                      !is.na(profit_impact_pct) ~ "DD",
                                      TRUE ~ "CC ONLY"))

sectoral_stats2 <- sectoral_stats %>%
  group_by(scenario, parent_market, market_indicator) %>%
  summarise(market_cap_2017 = sum(market_cap_2017)) %>%
  group_by(scenario) %>%
  mutate(MSCI_share = market_cap_2017 / sum(market_cap_2017)) %>%
  ungroup()

save_dated(sectoral_stats2, "MSCI_market_statistics", folder = "Output", csv = TRUE)