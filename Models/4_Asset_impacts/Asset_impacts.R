##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  30/03/2019
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

# Credit rating rankings
credit_rating_rankings <- read_excel(input_source("Credit_rating_rankings.xlsx"),
                                     sheet = "R1. Credit rating rankings", range = "$A$8:$D$25")

# Specify credit ratings provider to use
creditratings_provider <- "moody"

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
    group_by(scenario, !!!(summarise_vars)) %>%
    #mutate(profit_npv_total = market_cap * (index + 1),
    #       profit_npv_total_cap = market_cap * (index_cap + 1)) %>%
    #summarise_at(vars(market_cap, profit_npv_total, profit_npv_total_cap),
    #             funs(sum(., na.rm = TRUE))) %>%
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
    #mutate(index = profit_npv_total / market_cap - 1,
    #       index_cap = profit_npv_total_cap / market_cap - 1) %>%
    select(scenario, !!!summarise_vars, contains("index"))
  
  return(temp)
}

equity_parentmarket_results <- summarise_equity(parent_market)
equity_domicile_results <- summarise_equity(domicile)
equity_parentmarket_domicile_results <- summarise_equity(parent_market, domicile) %>%
  arrange(scenario, parent_market, domicile)

equity_msci_results <- equity_results2 %>%
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
  mutate(parent_market = "MSCI ACWI")

# Save equity results
save_dated(equity_results2, "Equity_level_results", folder = "Output", csv = FALSE)
save_dated(equity_parentmarket_results, "Equity_pmarket_results", folder = "Output", csv = TRUE)
save_dated(equity_domicile_results, "Equity_dom_results", folder = "Output", csv = TRUE)
save_dated(equity_parentmarket_domicile_results, "Equity_pmarket_dom_results", folder = "Output", csv = TRUE)
save_dated(equity_msci_results, "Equity_msci_results", folder = "Output", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Calculate fixed income modelling preliminaries ----

# Map parent markets to fixed income results markets
parent_market_mapping <- tibble(parent_market = unique(market_exposure_results2$parent_market)) %>%
  mutate(fi_results_market = case_when(parent_market %in% c("Exploration and production", "Power generation") ~ parent_market,
                                       parent_market %in% c("Auto Parts", "Automobiles") ~ "Autos",
                                       parent_market %in% c("Concrete and cement", "Iron & Steel",
                                                            "Other Chemicals", "Specialty Chemicals") ~ "Emissions intensive industries",
                                       parent_market %in% c("Computer Services", "Internet", "Software") ~ "Computers & internet",
                                       TRUE ~ "Other sectors"))

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
  left_join(parent_market_mapping, by = "parent_market") %>% 
  select(scenario, company_id, company, parent_market, fi_results_market, fi_instrument_code:moody_rating, company_market_cap,
         everything()) %>%
  rename(company_market_cap_2017 = company_market_cap) %>%
  arrange(scenario, company)

# Filter out assets for which we cannot calculate Altman Z-score changes
fi_data4 <- fi_data3 %>%
  filter(!(is.na(company_market_cap_2017) | is.na(current_assets_2017) | is.na(current_liabilities_2017) 
           | is.na(retained_earnings_2017) | is.na(total_assets_2017) | is.na(total_liabilities_2017) | is.na(revenue_2017)))

# Calculate EBIT uplift factor for estimating EBIT from 'Profit pre tax' - cannot be company-level as EBIT < 0 is possible in 2017
# Market-level is a compromise
fi_data5 <- fi_data4 %>%
  group_by(fi_results_market) %>% 
  mutate(ebit_uplift = median(ebit_2017 / profit_pre_tax_2017)) %>%
  ungroup()

ebit_uplift_results <- fi_data5 %>% 
  select(fi_results_market, parent_market, ebit_uplift) %>% 
  unique()

# Save EBIT uplift factors for further analysis
save_dated(ebit_uplift_results, "EBIT_profit_uplift_factors", folder = "Interim", csv = TRUE)

fi_data6 <- fi_data5 %>%
  group_by(fi_results_market) %>%
  mutate(mcap_uplift = median(company_market_cap_2017 / ebit_2017)) %>%
  ungroup() %>%
  select(scenario:retained_earnings_2017, ebit_uplift, mcap_uplift, everything())
  
mcap_uplift_results <- fi_data6 %>%
  select(fi_results_market, parent_market, mcap_uplift) %>%
  unique()

# Save Market cap - EBIT uplift factors for further analysis
save_dated(mcap_uplift_results, "MCap_EBIT_uplift_factors", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Find current Altman Z-scores ----

# Calculate 2017 Altman Z-scores
fi_data7 <- fi_data6 %>%
  mutate(working_capital_2017 = current_assets_2017 - current_liabilities_2017,
         x1_2017 = working_capital_2017 / total_assets_2017,
         x2_2017 = retained_earnings_2017 / total_assets_2017,
         x3_2017 = ebit_2017 / total_assets_2017,
         x4_2017 = company_market_cap_2017 / total_liabilities_2017,
         x5_2017 = revenue_2017 / total_assets_2017) %>%
  mutate(altman_z_2017 = 1.2 * x1_2017 + 1.4 * x2_2017 + 3.3 * x3_2017 + 0.6 * x4_2017 + 1 * x5_2017)

#--------------------------------------------------------------------------------------------------

##### SECTION 6 - Mapping between Altman Z-scores and credit ratings (today) ----

# Map credit ratings to Altman Z-scores
moody_rating_rankings <- credit_rating_rankings %>%
  select(moody_ranking, moody_rating)
sp_rating_rankings <- credit_rating_rankings %>%
  select(sp_ranking, sp_rating)

credit_altman_mapping <- fi_data7 %>%
  filter(scenario == "Paris_NDCs") %>%
  left_join(moody_rating_rankings, by = c("moody_rating")) %>%
  left_join(sp_rating_rankings, by = c("sp_rating"))

# Calculate MSCI ACWI-level median credit rating by ratings agency ranking
credit_rating_altman <- function(agency_rating, agency_rating_rankings, agency_ranking,
                                 agency_formula) {
  agency_rating <- enquo(agency_rating)
  agency_ranking <- enquo(agency_ranking)
  formula <- enexpr(formula)
  
  temp <- credit_altman_mapping %>%
    group_by(!!agency_rating) %>%
    summarise(altman_z_2017 = median(altman_z_2017, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(agency_rating_rankings) %>%
    arrange(!!agency_ranking)
  
  return(temp)
}

sp_results <- credit_rating_altman(sp_rating, sp_rating_rankings, sp_ranking) %>%
  mutate(excluded = ifelse(sp_ranking > 10, "Y", "N"))
moody_results <- credit_rating_altman(moody_rating, moody_rating_rankings, moody_ranking) %>%
  mutate(excluded = ifelse(moody_ranking > 10, "Y", "N"))

sp_regression_results <- lm(sp_ranking ~ altman_z_2017, data = subset(sp_results, excluded != "Y"))
moody_regression_results <- lm(moody_ranking ~ altman_z_2017, data = subset(moody_results, excluded != "Y"))
  
ggplot(sp_results) +
  geom_point(aes(x = altman_z_2017, y = sp_ranking, colour = excluded)) +
  geom_line(aes(x = altman_z_2017, y = sp_regression_results$coefficients[1] + altman_z_2017 * sp_regression_results$coefficients[2])) +
  theme_vivid() +
  scale_colour_manual(values = c("blue", "red"))
ggsave(paste0("4_Asset_impacts/Interim/Plots/SP_rating_regression.png"), width = 16, height = 9, units = "in")

ggplot(moody_results) +
  geom_point(aes(x = altman_z_2017, y = moody_ranking, colour = excluded)) +
  geom_line(aes(x = altman_z_2017, y = moody_regression_results$coefficients[1] + altman_z_2017 * moody_regression_results$coefficients[2])) +
  theme_vivid() +
  scale_colour_manual(values = c("blue", "red"))
ggsave(paste0("4_Asset_impacts/Interim/Plots/Moody_rating_regression.png"), width = 16, height = 9, units = "in")

#--------------------------------------------------------------------------------------------------

##### SECTION 6 - Calculate fixed income impacts ----

fi_results <- fi_data7

# This is a hack, but I'm too tired to find a better way
for(i in seq(2018, 2050, 1)) {

  # Only X3 (EBIT) and X4 (company market cap) are time varying elements of the Z-score
  suffix_variables_yr <- c("ebit_", "profit_pre_tax_", "mcap_ebit_uplift_", "revenue_", "x3_", "x4_", "x4_alt_",
                           "altman_z_", "altman_z_alt_")
  for (var in suffix_variables_yr) {
    assign(paste0(var, "yr"), rlang::sym(paste0(var, i)))
  }
  
  fi_results %<>%
    mutate(!!ebit_yr := !!profit_pre_tax_yr * ebit_uplift,
           # Noting that revenue_yr * x5_2017 = assets_yr assuming constant 'asset intensity'
           !!x3_yr := !!ebit_yr / !!revenue_yr * x5_2017,
           # Noting use of MCAP uplifted EBIT instead of market cap
           !!mcap_ebit_uplift_yr := !!ebit_yr * mcap_uplift,
           !!x4_yr := !!mcap_ebit_uplift_yr / total_liabilities_2017,
           # Alternative approach to x4: company_market_cap instead of MCAP uplifted EBIT
           !!x4_alt_yr := profit_npv_total_cap / total_liabilities_2017) %>%
    mutate(!!altman_z_yr := 1.2 * x1_2017 + 1.4 * x2_2017 + 3.3 * !!x3_yr + 0.6 * !!x4_yr + 1 * x5_2017,
           !!altman_z_alt_yr := 1.2 * x1_2017 + 1.4 * x2_2017 + 3.3 * !!x3_yr + 0.6 * !!x4_alt_yr + 1 * x5_2017)
    
}

fi_results2 <- fi_results %>%
  select(scenario, company_id, company, parent_market:moody_rating, starts_with("altman_z_2"), altman_z_alt_2018,
         net_income_2017, ebit_uplift, mcap_uplift, current_liabilities_2017, current_assets_2017,
         total_liabilities_2017, total_assets_2017, retained_earnings_2017, profit_npv_total, profit_npv_total_cap,
         starts_with("ebit_"), starts_with("revenue_"), starts_with("mcap_ebit_uplift_"), starts_with("altman_z_alt_"))

save_dated(fi_results2, "FI_altman_z_results", folder = "Interim", csv = FALSE)

# Shorten dataset before credit rating calculations
fi_results3 <- fi_results2 %>%
  select(scenario, company_id:moody_rating, starts_with("altman_z_2"), starts_with("altman_z_alt_"))

#--------------------------------------------------------------------------------------------------

##### SECTION 7 - Calculate credit rating change impacts ----

if(!(creditratings_provider %in% c("moody", "sp"))) {stop("Credit ratings provider incorrectly specified")}

# Note that sectors which are exposed to DD / CM models will have credit rating differences in 2017
# as the NPV impact from the DD / CM models are applied from 2017 onwards
fi_results4 <- fi_results3 %>%
  mutate_at(vars(starts_with("altman_z_2"), starts_with("altman_z_alt")),
            funs(credit_rating = ifelse(rep(creditratings_provider == "moody", n()),
                                        moody_regression_results$coefficients[1] + moody_regression_results$coefficients[2] * .,
                                        ifelse(rep(creditratings_provider == "sp", n()),
                                               sp_regression_results$coefficients[1] + sp_regression_results$coefficients[2] * .,
                                               NA_real_)))) %>%
  rename_at(vars(ends_with("credit_rating")),
            funs(paste0(ifelse(grepl("alt_", .), "credit_rating_alt_", "credit_rating_"),
                        stri_extract_all_regex(., "[0-9]+")))) %>%
  group_by(fi_instrument_code, fi_isin_code) %>%
  mutate_at(vars(starts_with("credit_rating_")),
            funs(change = . - .[[which(scenario == "Paris_NDCs")]])) %>%
  rename_at(vars(ends_with("_change")),
            funs(paste0(ifelse(grepl("alt_", .), "credit_rating_change_alt_", "credit_rating_change_"),
                        stri_extract_all_regex(., "[0-9]+")))) %>%
  ungroup()

# Note that NA in credit rating / credit rating change fields means that the company has dropped out of the CC model at this time
save_dated(fi_results4, "FI_credit_rating_results", folder = "Interim", csv = FALSE)

#--------------------------------------------------------------------------------------------------

##### SECTION 8 - Aggregate credit rating changes over chosen market categories ----

# Remove redundant variables
fi_results5 <- fi_results4 %>%
  select(scenario:moody_rating, contains("credit_rating_"))

# Replace NA values in credit ratings with 100 [high rankings are bad in credit ratings]
# Replace NA values in credit rating changes with 100 [high values are bad in credit rating changes]
fi_results6 <- fi_results5 %>%
  mutate_at(vars(contains("credit_rating_2"), contains("credit_rating_alt_")),
            funs(ifelse(is.na(.), 100, .))) %>%
  mutate_at(vars(contains("credit_rating_change_")),
            funs(ifelse(is.na(.), 100, .)))

save_dated(fi_results6, "FI_level_credit_rating_results", folder = "Output", csv = FALSE)

# Summarise over chosen variable(s) - taking 10th, 50th and 90th percentiles
summarise_quantiles <- function(summarise_var, summarise_start_yr) {
  
  summarise_var_yr_first <- rlang::sym(paste0(summarise_var, "_", summarise_start_yr))
  summarise_var_yr_last <- rlang::sym(paste0(summarise_var, "_2050"))
  
  # FI market level results
  temp <- fi_results6 %>%
    select(scenario:moody_rating, (!!summarise_var_yr_first):(!!summarise_var_yr_last)) %>%
    gather(key = "year", value = "value", (!!summarise_var_yr_first):(!!summarise_var_yr_last)) %>%
    mutate(category = substring(year, first = 1, last = stri_locate_first_regex(year, "[0-9]+")[, 1] - 2),
           year = as.numeric(stri_extract_all_regex(year, "[0-9]+"))) %>%
    group_by(scenario, fi_results_market, category, year) %>%
    summarise(quantile_0.1 = quantile(value, probs = 0.1),
              quantile_0.5 = quantile(value, probs = 0.5),
              quantile_0.9 = quantile(value, probs = 0.9)) %>%
    ungroup()
  
  # MSCI level results
  temp2 <- fi_results6 %>%
    select(scenario:moody_rating, (!!summarise_var_yr_first):(!!summarise_var_yr_last)) %>%
    gather(key = "year", value = "value", (!!summarise_var_yr_first):(!!summarise_var_yr_last)) %>%
    mutate(category = substring(year, first = 1, last = stri_locate_first_regex(year, "[0-9]+")[, 1] - 2),
           year = as.numeric(stri_extract_all_regex(year, "[0-9]+"))) %>%
    group_by(scenario, category, year) %>%
    summarise(quantile_0.1 = quantile(value, probs = 0.1),
              quantile_0.5 = quantile(value, probs = 0.5),
              quantile_0.9 = quantile(value, probs = 0.9)) %>%
    ungroup() %>%
    mutate(fi_results_market = "MSCI ACWI")
  
  temp3 <- temp %>%
    bind_rows(temp2)
  
  return(temp3)
}

var_list <- as.list(c("credit_rating_alt", "credit_rating_change", "credit_rating_change_alt"))

fi_results7 <- summarise_quantiles("credit_rating", 2017)
fi_results8 <- map(var_list, function(x) {summarise_quantiles(x, 2018)}) %>%
  bind_rows() %>%
  bind_rows(fi_results7)

save_dated(fi_results8, "FI_summary_credit_rating_results", folder = "Output", csv = TRUE)
           
#--------------------------------------------------------------------------------------------------

##### SECTION 8 - Credit rating change charts ----

### Quantile impacts table
summarise_year <- function(summarise_year) {
  
  temp <- fi_results8 %>%
    filter(year == summarise_year | category == "credit_rating" & year == 2017)
  return(temp)
}

fi_results_2030 <- summarise_year(2030)
fi_results_2050 <- summarise_year(2050)

### Add baseline credit rating data to the dataset
credit_ratings_baseline_percentile <- function(perc) {
  
  moody_rating_name <- rlang::sym(paste0("moody_rating_p", perc * 100))
  
  temp <- fi_results6 %>%
    select(scenario:moody_rating) %>%
    left_join(moody_rating_rankings, by = c("moody_rating")) %>%
    select(-sp_rating) %>%
    # Median MSCI ACWI rating
    mutate(moody_ranking_msci = quantile(moody_ranking, probs = perc, na.rm = TRUE)) %>%
    # Median rating within each sector - removing NAs for the case where Moody's rating is unavailable ('NORATING')
    group_by(fi_results_market) %>%
    mutate(moody_ranking = quantile(moody_ranking, probs = perc, na.rm = TRUE)) %>%
    ungroup() %>%
    select(fi_results_market, moody_ranking, moody_ranking_msci) %>%
    gather(key = "type", value = "moody_ranking", -fi_results_market) %>% 
    mutate(fi_results_market = case_when(type == "moody_ranking_msci" ~ "MSCI ACWI",
                                         TRUE ~ fi_results_market)) %>%
    select(-type) %>%
    unique() %>%
    left_join(moody_rating_rankings, by = c("moody_ranking")) %>%
    select(-moody_ranking) %>%
    rename(!!moody_rating_name := moody_rating)

}

credit_rating_baseline_p10 <- credit_ratings_baseline_percentile(perc = 0.1)
credit_rating_baseline_p50 <- credit_ratings_baseline_percentile(perc = 0.5)
credit_rating_baseline_p90 <- credit_ratings_baseline_percentile(perc = 0.9)

fi_results_2030_2 <- fi_results_2030 %>%
  left_join(credit_rating_baseline_p10, by = "fi_results_market") %>%
  left_join(credit_rating_baseline_p50, by = "fi_results_market") %>%
  left_join(credit_rating_baseline_p90, by = "fi_results_market")

fi_results_2050_2 <- fi_results_2050 %>%
  left_join(credit_rating_baseline_p10, by = "fi_results_market") %>%
  left_join(credit_rating_baseline_p50, by = "fi_results_market") %>%
  left_join(credit_rating_baseline_p90, by = "fi_results_market")

save_dated(fi_results_2030_2, "FI_2030_credit_rating_results", folder = "Output", csv = TRUE)
save_dated(fi_results_2050_2, "FI_2050_credit_rating_results", folder = "Output", csv = TRUE)

# Credit rating histogram for each sector (used for report statistics)
credit_rating_hist <- function(sector) {
  
  temp <- fi_results6 %>%
    select(scenario:moody_rating) %>%
    left_join(moody_rating_rankings, by = c("moody_rating")) %>%
    select(-sp_rating) %>%
    filter(fi_results_market == sector) %>%
    mutate(moody_group = gsub("[0-9]+", "", moody_rating)) %>%
    mutate(moody_rating = factor(moody_rating, levels = unique(moody_rating[order(moody_ranking)]), ordered = TRUE)) %>%
    mutate(moody_group = factor(moody_group, levels = c("Aaa", "Aa", "A", "Baa", "Ba", "NORATING"), ordered = TRUE))
  
  #windows()
  ggplot(temp) +
    geom_bar(aes(x = moody_group, y = (..count..) / sum(..count..))) + 
    theme_vivid() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(name = "Frequency", expand = c(0, 0), labels = scales::percent_format(1)) +
    ggtitle(label = sector) +
    theme(plot.title = element_text(family = "Nordique Pro Semibold"))

}

map(unique(fi_results6$fi_results_market), credit_rating_hist)

### Q10 - Median - Q90 MSCI ACWI-level impacts area chart

# Clean up the MSCI ACWI level data
fi_results9 <- fi_results6 %>%
  select(scenario:moody_rating, credit_rating_2017, starts_with("credit_rating_change_2"),
         credit_rating_change_alt_2018) %>%
  gather(key = "year", value = "credit_rating_change", `credit_rating_change_2017`:`credit_rating_change_2050`) %>%
  mutate(category = "credit_rating_change",
         year = as.numeric(stri_extract_all_regex(year, "[0-9]+"))) %>%
  select(scenario:moody_rating, category, year, credit_rating_change, credit_rating_2017, credit_rating_change_alt_2018)

fi_results10 <- fi_results9 %>%
  group_by(scenario, year, fi_results_market) %>%
  summarise(rating_change_q10 = quantile(credit_rating_change, probs = 0.1),
            rating_change_q50 = quantile(credit_rating_change, probs = 0.5),
            rating_change_q90 = quantile(credit_rating_change, probs = 0.9),
            credit_rating_2017 = median(credit_rating_2017),
            credit_rating_change_alt_2018 = median(credit_rating_change_alt_2018))

# Ad-hoc changes to the data including smoothness assumptions
fi_results11 <- fi_results10 %>%
  # Fix all pre-2020 values to be 0
  mutate(rating_change_q50 = ifelse(year <= 2020, 0, rating_change_q50),
         rating_change_q10 = ifelse(year <= 2020, 0, rating_change_q10),
         rating_change_q90 = ifelse(year <= 2020, 0, rating_change_q90)) %>%
  # Smooth out rating change variable to 2025
  group_by(scenario, fi_results_market) %>%
  mutate_at(vars(rating_change_q10, rating_change_q50, rating_change_q90),
            funs(ifelse(year > 2020 & year < 2025, NA_real_, .))) %>%
  mutate_at(vars(rating_change_q10, rating_change_q50, rating_change_q90),
            funs(approx(x = year, y = ., xout = year)$y)) %>%
  ungroup()

# Generate the area chart itself
plot_area_chart <- function(plot_scenario, plot_sector) {
  
  temp <- fi_results11 %>%
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
    #annotate("point", x = 2020, y = temp$credit_rating_change_alt_2018[temp$year == 2020] * (-1)) +
    #geom_segment(aes(x = 2020, y = temp$credit_rating_change_alt_2018[temp$year == 2020] * (-1), xend = 2020, yend = temp$rating_change_q50[temp$year == 2020]),
    #             linetype = "dashed", size = 0.8) +
    annotate("text", x = 2050.1, y = temp$rating_change_q50[temp$year == 2050] * (-1), label = "Median", colour = "black", hjust = 0, size = 5) +
    annotate("text", x = 2050.1, y = temp$rating_change_q10[temp$year == 2050] * (-1), label = "90th percentile", colour = "black", hjust = 0, size = 5) +
    annotate("text", x = 2050.1, y = temp$rating_change_q90[temp$year == 2050] * (-1), label = "10th percentile", colour = "black", hjust = 0, size = 5) +
    scale_y_continuous(name = "Change in credit rating", limits = c(-3, 1), expand = c(0, 0)) + 
    scale_x_continuous(name = NULL, limits = c(2017, 2054), expand = c(0, 0)) +
    theme_vivid(vivid_size = 1.4)
  
  ggsave(paste0("4_Asset_impacts/Output/Plots/", plot_sector, "_credit_rating_change_plot_", plot_scenario, ".png"), width = 16, height = 9, units = "in")
}

scenarios <- unique(fi_results11$scenario)
#lapply(scenarios, plot_area_chart)
plot_area_chart("2DS_Balanced_Transformation", "Emissions intensive industries")