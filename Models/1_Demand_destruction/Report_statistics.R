##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  21/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script imports results from the upstream oil & gas, and upstream coal demand destruction modelling
#####                     in order to calculate summary statistics for use in the latest HSBC report
##### Dependencies:       1.  Oil and gas company-level cleaned data
#####                     2.  Oil and gas scenario output data
#####                     3.  Rystad company names matched to Vivid unique company IDs / names
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "1_Demand_destruction"
source("utils.R")

# Read in oil & gas detailed company results
oilgas_data <- readRDS(fs::path(main_save_folder, "Interim", "Oil_and_gas_dd_full_results.rds"))

# Read in oil & gas short company results
oilgas_short_data <- readRDS(fs::path(main_save_folder, "Output", "Oil_and_gas_dd_npv_impacts.rds"))

# Read in oil & gas MSCI ACWI names
oilgas_names <- readRDS(input_source("Oil_and_gas_company_names.rds"))

# Read in coal company preliminary data
coal_temporal <- readRDS(input_source("Coal_company_data.rds"))

# Read in coal supply curve for unit cost exposure
coal_supply_curve <- readRDS(input_source("Coal_cost_curve_data.rds"))

# Read in coal company regional exposure
coal_regional <- readRDS(fs::path(main_save_folder, "Interim", "Coal_company_region_results.rds"))

# Read in coal company results
coal_data <- readRDS(fs::path(main_save_folder, "Interim", "Coal_dd_full_results.rds"))

# Reed in coal short company results
coal_short_data <- readRDS(fs::path(main_save_folder, "Output", "Coal_dd_npv_impacts.rds"))

# Read in coal MSCI ACWI names
coal_names <- readRDS(input_source("Coal_company_names.rds"))

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Consolidate oil & gas data and find the 10th, median and 90th percentile companies in the '2DS Balanced Transformation' scenario ----

# Find result percentiles for each report scenario and store as part one of the output
oilgas_pct_results <- oilgas_short_data %>% 
  filter(product != "All" & !is.na(profit_impact_pct)) %>%
  group_by(scenario, product) %>%
  summarise(p10 = quantile(profit_impact_pct, probs = 0.1),
            p50 = quantile(profit_impact_pct, probs = 0.5),
            p90 = quantile(profit_impact_pct, probs = 0.9)) %>%
  mutate(product = ifelse(product == "Liquid", "Oil", product))

# Store identity of percentile companies
oilgas_all_data <- oilgas_names %>%
  left_join(oilgas_data, by = "rystad_name") %>%
  filter(!is.na(product) & product != "All")

# Find the 10th, median and 90th percentile company on profit impact in 2DS_Balanced_Transformation
p_2ds_companies <- oilgas_short_data %>%
  filter(product != "All") %>%
  filter(scenario == "2DS_Balanced_Transformation") %>%
  group_by(product) %>%
  filter(!is.na(profit_impact_pct)) %>%
  mutate(p10_2ds = quantile(profit_impact_pct, probs = 0.1, na.rm = TRUE),
         p50_2ds = quantile(profit_impact_pct, probs = 0.5, na.rm = TRUE),
         p90_2ds = quantile(profit_impact_pct, probs = 0.9, na.rm = TRUE)) %>%
  mutate_at(vars(p10_2ds, p50_2ds, p90_2ds),
            funs(diff = abs(. - profit_impact_pct))) %>%
  mutate_at(vars(ends_with("diff")), funs(company = company[[which(. == min(.))]])) %>%
  ungroup()

# Store these company identities
oil_p10 <- p_2ds_companies$p10_2ds_diff_company[p_2ds_companies$product == "Liquid"][[1]]
oil_p50 <- p_2ds_companies$p50_2ds_diff_company[p_2ds_companies$product == "Liquid"][[1]]
oil_p90 <- p_2ds_companies$p90_2ds_diff_company[p_2ds_companies$product == "Liquid"][[1]]
gas_p10 <- p_2ds_companies$p10_2ds_diff_company[p_2ds_companies$product == "Gas"][[1]]
gas_p50 <- p_2ds_companies$p50_2ds_diff_company[p_2ds_companies$product == "Gas"][[1]]
gas_p90 <- p_2ds_companies$p90_2ds_diff_company[p_2ds_companies$product == "Gas"][[1]]

# Calculate summary statistics for all companies based on full data
oilgas_all_data2 <- oilgas_all_data %>%
  filter(scenario %in% c("Paris_NDCs")) %>%
  group_by(company_id, company, product) %>%
  summarise(margin = mean(1 - unit_cost_f / price_f, na.rm = TRUE),
            prod_before_2030 = sum(ifelse(year <= 2030, production_f, NA_real_), na.rm = TRUE) / sum(production_f, na.rm = TRUE)) %>%
  ungroup()

# Merge in profit impacts
oilgas_all_data3 <- oilgas_all_data2 %>%
  left_join(oilgas_short_data) %>% 
  filter(scenario == "2DS_Balanced_Transformation") 

oilgas_all_data4 <- oilgas_all_data3 %>%
  filter(product == "Liquid")

oilgas_all_data5 <- oilgas_all_data3 %>%
  filter(product == "Gas")

oil_results <- summary(lm(profit_impact_pct ~ prod_before_2030 + margin, data = oilgas_all_data4))
gas_results <- summary(lm(profit_impact_pct ~ prod_before_2030 + margin, data = oilgas_all_data5))



#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Consolidate coal data and find the 10th, median and 90th percentile companies in the '2DS Balanced Transformation' scenario ----

# Find result percentiles for each report scenario and store as part one of the output
coal_pct_results <- coal_short_data %>%
  group_by(scenario) %>%
  summarise(p10 = quantile(profit_impact_pct, probs = 0.1),
            p50 = quantile(profit_impact_pct, probs = 0.5),
            p90 = quantile(profit_impact_pct, probs = 0.9)) %>%
  mutate(product = "Coal")

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Save consolidated outputs ----

all_pct_results <- oilgas_pct_results %>%
  bind_rows(coal_pct_results)

save_dated(all_pct_results, "Upstream_quantile_impacts", folder = "Output", csv = TRUE)



