##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  18/02/2019
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

##### SECTION 2 - Clean up company-level modelling results ----

# Summarise subsidiary-level results to the company-level
company_results <- subsidiary_results %>%
  group_by(scenario, company_id, company) %>%
  # Index cap = capped npv profit / market_cap_2017 - 1: rearrange for the below
  mutate(profit_npv_total_cap = market_cap_2017 * (index_cap + 1)) %>%
  summarise_at(vars(starts_with("profit_npv_post_closure_post_tax"), profit_npv_total_cap, starts_with("revenue_"),
                    starts_with("profit_post_closure_pre_tax")),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup()

company_results_for_equity <- company_results %>%
  mutate(profit_npv_total = (rowSums(.[grep("profit_npv_post_closure_post_tax", names(.))]) - profit_npv_post_closure_post_tax_2017)) %>%
  select(scenario, company_id, company, profit_npv_total, profit_npv_total_cap)

# Note that the capped value impairment index values cannot be used for fixed income
company_results_for_fi <- company_results %>%
  mutate(profit_npv_total = (rowSums(.[grep("profit_npv_post_closure_post_tax", names(.))]) - profit_npv_post_closure_post_tax_2017)) %>%
  select(scenario, company_id, company, profit_npv_total, profit_npv_total_cap, starts_with("revenue_"),
         starts_with("profit_post_closure_pre_tax")) %>%
  rename_at(vars(starts_with("profit_post_closure_pre_tax")),
            funs(paste0("profit_pre_tax_", stri_extract_all_regex(., "[0-9]+")))) %>%
  select(-revenue_BAU_quantity_2017)

# Clean up parent market categorisation dataset
market_exposure_results2 <- market_exposure_results %>%
  select(company_id, company, parent_market) %>%
  unique()

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Calculate equity impacts ----

equity_data2 <- equity_data %>%
  group_by(company_id, company) %>%
  mutate(company_market_cap = sum(market_cap, na.rm = TRUE)) %>%
  ungroup()

equity_data_for_fi <- equity_data2 %>%
  select(company_id, company, company_market_cap) %>%
  unique()

# Compare to company-level market cap to calculate value impairment index
# equity dataset market cap used to ensure results are consistent with the original data [subsidiary-level mcap was present in Subsidiary_results.rds]
equity_results <- equity_data2 %>%
  left_join(market_exposure_results2, by = c("company_id", "company")) %>%
  left_join(company_results_for_equity, by = c("company_id", "company")) %>%
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

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Calculate fixed income impacts ----

# Merge in company market cap, revenue and profit pre-tax results
fi_data2 <- fi_data %>%
  rename_at(vars(revenue:retained_earnings),
            funs(paste0(., "_2017"))) %>%
  # Revenue 2017 variable already defined in the company-level dataset [values coincide for BAU scenario, differ for climate 
  # scenarios from 2017 for DD / CM impacted companies]
  select(-revenue_2017)

fi_data3 <- fi_data2 %>%
  left_join(company_results_for_fi, by = "company_id") %>%
  left_join(market_exposure_results2, by = c("company_id", "company")) %>%
  left_join(equity_data_for_fi, by = c("company_id", "company")) %>%
  select(scenario, company_id, company, parent_market, fi_instrument_code:moody_rating, company_market_cap,
         everything()) %>%
  rename(company_market_cap_2017 = company_market_cap) %>%
  arrange(scenario, company)

# Filter out assets for which we cannot calculate Altman Z-score changes
fi_data4 <- fi_data3 %>%
  filter(!(is.na(company_market_cap_2017) | is.na(current_assets_2017) | is.na(current_liabilities_2017) 
           | is.na(retained_earnings_2017) | is.na(total_assets_2017) | is.na(total_liabilities_2017) | is.na(revenue_2017)))

fi_results <- fi_data4 %>%
  mutate(working_capital_2017 = current_assets_2017 - current_liabilities_2017,
         x1_2017 = working_capital_2017 / total_assets_2017,
         x2_2017 = retained_earnings_2017 / total_assets_2017,
         x3_2017 = ebit_2017 / total_assets_2017,
         x4_2017 = company_market_cap_2017 / total_liabilities_2017,
         x5_2017 = revenue_2017 / total_assets_2017) %>%
  mutate(altman_z_2017 = 1.2 * x1_2017 + 1.4 * x2_2017 + 3.3 * x3_2017 + 0.6 * x4_2017 + 1 * x5_2017) %>%
  # Calculate EBIT uplift factor for estimating EBIT from 'Profit pre tax' - cannot be company-level as EBIT < 0 is possible in 2017
  # Market-level is a compromise
  group_by(parent_market) %>%
  mutate(ebit_uplift = mean(ebit_2017 / profit_pre_tax_2017))

# Save EBIT uplift factors for further analysis
ebit_uplift_results <- fi_results %>% 
  select(parent_market, ebit_uplift) %>% 
  unique()

save_dated(ebit_uplift_results, "EBIT_profit_uplift_factors", folder = "Interim", csv = TRUE)

# This is a hack, but I'm too tired to find a better way
for(i in seq(2018, 2050, 1)) {

  # Only X3 (EBIT) and X4 (company market cap) are time varying elements of the Z-score
  suffix_variables_yr <- c("ebit_", "profit_pre_tax_", "revenue_", "x3_", "x4_", "altman_z_")
  for (var in suffix_variables_yr) {
    assign(paste0(var, "yr"), rlang::sym(paste0(var, i)))
  }
  
  fi_results %<>%
    mutate(!!ebit_yr := !!profit_pre_tax_yr * ebit_uplift,
           # Noting that revenue_yr * x5_2017 = assets_yr assuming constant 'asset intensity'
           !!x3_yr := !!ebit_yr / !!revenue_yr * x5_2017,
           # Noting use of capped profit [this term is constant over time]
           !!x4_yr := profit_npv_total_cap / total_liabilities_2017) %>%
    mutate(!!altman_z_yr := 1.2 * x1_2017 + 1.4 * x2_2017 + 3.3 * !!x3_yr + 0.6 * !!x4_yr + 1 * x5_2017)
    
}

fi_results2 <- fi_results %>%
  select(scenario, company_id, company, parent_market:moody_rating, starts_with("altman_z_"),
         net_income_2017, ebit_uplift, current_liabilities_2017, current_assets_2017,
         total_liabilities_2017, total_assets_2017, retained_earnings_2017, profit_npv_total, profit_npv_total_cap,
         starts_with("ebit_"), starts_with("revenue_"))

# Summarise over chosen variable(s) - note that this is based on equity market caps, rather than company market caps so there is no double counting
summarise_fi <- function(...) {
  
  summarise_vars <- enquos(...)
  
  temp <- fi_results2 %>%
    group_by(scenario, !!!summarise_vars) %>%
    summarise_at(vars(altman_z_2017:revenue_2050), funs(median(., na.rm = TRUE))) %>%
    ungroup()
    
  return(temp)
}

fi_parentmarket_results <- summarise_fi(parent_market)

# Save equity results
save_dated(fi_results2, "FI_instrument_level_results", folder = "Output", csv = FALSE)
save_dated(fi_parentmarket_results, "FI_pmarket_results", folder = "Output", csv = TRUE)