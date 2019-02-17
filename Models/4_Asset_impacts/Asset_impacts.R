##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  17/02/2019
##### Code author:        Shyamal Patel
##### Dependencies:       1. Cost and competition model subsidiary level results: "3_Cost_and_competition/Output/Subsidiary_results.rds"
#####                     2. Equity data: "4_Asset_impacts/Input/Equity_reconciled_2016USD_data.rds"
#####                     3. Fixed-income data: "4_Asset_impacts/Input/FI_reconciled_2016USD_data.rds"
#####                     4. Revised product exposure results: "4_Asset_impacts/Input/Revised_product_exposure_results.rds"
##### Notes:              None
##### Called by:          N/A

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "4_Asset_impacts"
source("utils.R")

# Equity data
equity_data <- readRDS(input_source("Equity_reconciled_2016USD_data.rds"))

# Fixed income data
fi_data <- readRDS(input_source("FI_reconciled_2016USD_data.rds"))

# Company market exposure results - needed for parent_market variable only
market_exposure_results <- readRDS(input_source("Revised_product_exposure_results.rds"))

# Company subsidiary-level results
subsidiary_results <- readRDS("3_Cost_and_competition/Output/Subsidiary_results.rds")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Summarise subsidiary-level results to the company-level ----

company_results <- subsidiary_results %>%
  group_by(scenario, company_id, company) %>%
  # Index cap = capped npv profit / market_cap_2017 - 1: rearrange for the below
  mutate(profit_npv_total_cap = market_cap_2017 * (index_cap + 1)) %>%
  summarise_at(vars(starts_with("profit_npv_post_closure_post_tax"), profit_npv_total_cap),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(profit_npv_total = (rowSums(.[grep("profit_npv_post_closure_post_tax", names(.))]) - profit_npv_post_closure_post_tax_2017)) %>%
  select(scenario, company_id, company, profit_npv_total, profit_npv_total_cap)

equity_data2 <- equity_data %>%
  group_by(company_id, company) %>%
  mutate(company_market_cap = sum(market_cap, na.rm = TRUE)) %>%
  ungroup()

market_exposure_results2 <- market_exposure_results %>%
  select(company_id, company, parent_market) %>%
  unique()

# Compare to company-level market cap to calculate value impairment index
# equity dataset market cap used to ensure results are consistent with the original data [subsidiary-level mcap was present in Subsidiary_results.rds]
equity_results <- equity_data2 %>%
  left_join(market_exposure_results2, by = c("company_id", "company")) %>%
  left_join(company_results, by = c("company_id", "company")) %>%
  ungroup() %>%
  mutate(index = profit_npv_total / company_market_cap - 1,
         index_cap = profit_npv_total_cap / company_market_cap - 1) %>%
  select(scenario, company_id, equity_isin_code:domicile, parent_market, market_cap, company_market_cap, index, index_cap)

# Drop companies which were not modelled (3 removed in the panel model script (GCL Poly, China Longyuan Power, Huadian Power))
# 36 removed on account of profit margins > 1 in the 'cost & competition' model scripts
# Update with a more complete account / treatment of this in the appropriate place
equity_results2 <- equity_results %>%
  filter(!is.na(scenario))

# Summarise over chosen variable(s) - note that this is based on equity market caps, rather than company market caps so there is no double counting
summarise_equity <- function(...) {
  
  summarise_vars <- enquos(...)
  
  temp <- equity_results2 %>%
    group_by(scenario, !!!summarise_vars) %>%
    mutate(profit_npv_total = market_cap * (index + 1),
           profit_npv_total_cap = market_cap * (index_cap + 1)) %>%
    summarise_at(vars(market_cap, profit_npv_total, profit_npv_total_cap),
                 funs(sum(., na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(index = profit_npv_total / market_cap - 1,
           index_cap = profit_npv_total_cap / market_cap - 1) %>%
    select(scenario, !!!summarise_vars, market_cap, index, index_cap)
  
  return(temp)
}

equity_parentmarket_results <- summarise_equity(parent_market)
equity_domicile_results <- summarise_equity(domicile)
equity_parentmarket_domicile_results <- summarise_equity(parent_market, domicile) %>%
  arrange(scenario, parent_market, domicile)

# Save equity results
save_dated(equity_results2, "Equity_level_results", folder = "Output", csv = FALSE)
save_dated(equity_parentmarket_results, "Equity_pmarket_results", folder = "Output", csv = TRUE)
save_dated(equity_domicile_results, "Equity_dom_results", folder = "Output", csv = TRUE)
save_dated(equity_parentmarket_domicile_results, "Equity_pmarket_dom_results", folder = "Output", csv = TRUE)

