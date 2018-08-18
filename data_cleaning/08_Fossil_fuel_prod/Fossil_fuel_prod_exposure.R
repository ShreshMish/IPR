##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  15/08/2018
##### Code author:        Shyamal Patel
##### Description:        This script calculates fossil fuel sector market shares for each company, and outputs a 'final' product exposure
#####                     dataset for read in by the "10_Panel_setting/Panel_setting.R" script
##### Dependencies:       1.  2017 results from product exposure analysis: "06_Financial_prod/Market_exposure_results.rds"
#####                     2.  2016 results from product exposure analysis: "06_Financial_prod/Market_exposure_results_2016.rds"
#####                         Older files can be found in the ".../Dated/" folder
#####                     3.  Results from fossil fuel demand destruction analysis:
#####                         a) "Input/DD NPV stranding impacts.rds"
#####                         b) "Input/DD stranding data compact.rds"
#####                         c) "Input/DD full raw dataset.rds"
#####                         (Copied over from "2.1 - Fossil demand destruction/Demand destruction R Project/R data files/...)

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA

packages <- c("tidyverse", "magrittr", "readxl", "here", "stringi")
lapply(packages, require, character.only = TRUE)
source(here::here("utils.R"))

# Read in financial product exposure dataset for 2017
product_exposure_2017_data <- readRDS(path_to_data_file("06_Financial_prod/Output/Market_exposure_results.rds"))

# Read in financial product exposure dataset for 2016
product_exposure_2016_data <- readRDS(path_to_data_file("06_Financial_prod/Output/Market_exposure_results_2016.rds"))

# Read in demand destruction NPV analysis results
fossil_fuel_dd_results <- readRDS(path_to_data_file("08_Fossil_fuel_prod/Input/DD_NPV_strandingimpacts.rds"))

# Read in demand destruction compact dataset
fossil_fuel_dd_quantities <- readRDS(path_to_data_file("08_Fossil_fuel_prod/Input/DD_stranding_data_compact.rds"))

# Read in demand destruction full dataset (contains 2016 quantities)
fossil_fuel_dd_quantities_raw <- readRDS(path_to_data_file("08_Fossil_fuel_prod/Input/DD_full_raw_dataset.rds"))

# Set discount rate for NPV calculations
discount_rate <- 0.0575

# Consolidate product exposure datasets
product_exposure_2016_data %<>%
  rename(revenue_2016 = revenue,
         product_revenue_share_2016 = product_revenue_share) %>%
  select(ISIN_code, company, market, revenue_2016, product_revenue_share_2016)
# Merge in revenue 2016 separately to avoid losing it when product categories do not align between 2016 and 2017
revenue_2016_data <- product_exposure_2016_data %>%
  select(ISIN_code, company, revenue_2016) %>%
  unique()
# Remove revenue 2016 from product exposure 2016 dataset to avoid merge issues
product_exposure_2016_data %<>%
  select(-revenue_2016)
product_exposure_data <- product_exposure_2017_data %>%
  select(ISIN_code:parent_market, market, revenue, product_revenue_share) %>%
  rename(revenue_2017 = revenue,
         product_revenue_share_2017 = product_revenue_share) %>%
  left_join(revenue_2016_data) %>%
  left_join(product_exposure_2016_data) %>%
  # Replace product_revenue_share_2016 with 2017 proportion when this is unavailable
  # Note that this will cause 2016 revenue shares to not sum to 1 (these will only be used for E&P company calculations anyway)
  # Product-market share is based on 2017 data for all other companies
  mutate(product_revenue_share_2016 = case_when(is.na(product_revenue_share_2016) ~ product_revenue_share_2017,
                                                TRUE ~ product_revenue_share_2016))

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Find quantity contraction in oil and gas production for related industries stranding impact
#                 (refineries, pipelines and service companies)

# Clean DD stranding data compact dataset
fossil_fuel_dd_quantities %<>%
  rename(ISIN_code = ISIN_Code,
         company = VE_Name,
         product = Product,
         year = Year,
         scenario = Scenario) %>%
  select(scenario, ISIN_code, company, Rystad_Name, product, year, Production_G) %>%
  filter(product != "All")

# Clean fossil fuel DD quantities dataset containing 2016 data
fossil_fuel_dd_quantities_raw %<>%
  select(Scenario, Company, Product, Year, Production_G, Price_G, Production_F, Price_F) %>%
  rename(Rystad_Name = Company,
         product = Product,
         year = Year,
         scenario = Scenario) %>%
  filter(scenario == "Central") %>%
  mutate(product = ifelse(product == "Gas", "Gas", "Liquid"))

# Create dataset with 2016 prices
fossil_fuel_2016_prices <- fossil_fuel_dd_quantities_raw %>%
  select(scenario, product, year, Price_G) %>% 
  filter(year == 2016) %>%
  select(-year, -scenario) %>%
  unique() %>%
  rename(Price_G_2016 = Price_G)

# Build quantity impact dataset at industry-level (2016 prices used to weight oil and gas)
industry_level_quantity_impact <- fossil_fuel_dd_quantities %>%
  filter(!is.na(Production_G)) %>%
  select(scenario, product, year, Production_G) %>%
  unique() %>%
  arrange(product, scenario, year) %>%
  left_join(fossil_fuel_2016_prices)

# Calculate industry level quantity impacts in NPV terms (weights on oil and gas are 2016 prices)
industry_level_quantity_impact %<>%
  mutate(Revenue_G = Production_G * Price_G_2016) %>%
  group_by(scenario, product, year) %>% 
  summarise(Quantity_hat_G = sum(Revenue_G)) %>%
  ungroup() %>%
  # Flat around reported year value interpolation approach
  mutate(NPV_weight = case_when(year == 2020 ~ 0.9,
                                year == 2050 ~ 0.5,
                                TRUE ~ 1),
         NPV_discount_years = year - 2018,
         NPV_quantity_hat_G = Quantity_hat_G / ((1 + discount_rate) ^ NPV_discount_years))

save_dated(industry_level_quantity_impact, "08_Fossil_fuel_prod/Interim/Oil_and_gas_quantity_impact_full", csv = TRUE)

# Calculate % NPV impact at the fuel level
fuel_industry_level_quantity_impact <- industry_level_quantity_impact %>%
  group_by(scenario, product) %>%
  summarise(NPV_quantity = sum(NPV_quantity_hat_G)) %>%
  group_by(product) %>%
  mutate(BAU_NPV_quantity = mean(ifelse(scenario == "BAU", NPV_quantity, NA_real_), na.rm = TRUE),
         perc_NPV_quantity_impact = (BAU_NPV_quantity - NPV_quantity) / BAU_NPV_quantity)

# Summarise overall (oil + gas) results and calculate % NPV impact
overall_industry_level_quantity_impact <- industry_level_quantity_impact %>%
  group_by(scenario) %>%
  summarise(NPV_quantity = sum(NPV_quantity_hat_G)) %>%
  ungroup() %>%
  mutate(BAU_NPV_quantity = mean(ifelse(scenario == "BAU", NPV_quantity, NA_real_), na.rm = TRUE),
         perc_NPV_quantity_impact = (BAU_NPV_quantity - NPV_quantity) / BAU_NPV_quantity) %>%
  mutate(product = "All")

# Merge results together and save final file
industry_level_quantity_impact <- fuel_industry_level_quantity_impact %>%
  bind_rows(overall_industry_level_quantity_impact) %>%
  arrange(scenario, product) %>%
  select(scenario, product, perc_NPV_quantity_impact)

save_dated(industry_level_quantity_impact, "08_Fossil_fuel_prod/Output/Oil_and_gas_quantity_contraction", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Calculate firm-level revenues from DD compact dataset, and industry-level NPV quantity impact

# Need to match in with dataset containing ISIN codes and Vivid names
fossil_fuel_dd_quantities_full <- fossil_fuel_dd_quantities %>%
  filter(!is.na(ISIN_code)) %>%
  select(ISIN_code, company, Rystad_Name, product, year) %>%
  filter(year == 2020) %>%
  mutate(year = 2016) %>%
  unique() %>%
  left_join(fossil_fuel_dd_quantities_raw) %>%
  select(-ends_with("_G"))

# Calculate firm-level oil and gas revenues in 2016US$
fossil_fuel_dd_quantities_full %<>%
  mutate(revenue = Production_F * Price_F) %>%
  group_by(ISIN_code, company) %>%
  summarise(revenue = sum(revenue, na.rm = TRUE)) %>%
  rename(oilandgas_revenue = revenue)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Consolidated product exposure datasets and separate out E&P companies

# Create exploration and production indicator (companies which have E&P as a product category in TR)
product_exposure_oilandgas_data <- product_exposure_data %>%
  select(ISIN_code, company, parent_market, market, revenue_2016, product_revenue_share_2016) %>%
  group_by(ISIN_code) %>%
  # Exploration and production indicator
  mutate(market_oilandgas = ifelse(market == "Exploration and production", market, NA_character_)) %>%
  arrange(market_oilandgas) %>%
  fill(market_oilandgas) %>%
  filter(!is.na(market_oilandgas)) %>%
  ungroup()

#------------------------------------------------

#### Case 4.1: Companies which are in Rystad, but do not have E&P as product category in TR
TR_missing_oilandgas_data <- product_exposure_data %>%
  select(ISIN_code, company, parent_market, market, revenue_2016, product_revenue_share_2016) %>%
  filter(ISIN_code %in% fossil_fuel_dd_quantities_full$ISIN_code &
           !(ISIN_code %in% product_exposure_oilandgas_data$ISIN_code))

# Merge in 2016 Rystad oil and gas revenue
TR_missing_oilandgas_data %<>%
  left_join(fossil_fuel_dd_quantities_full) %>%
  # Convert oil and gas revenue to 000s 2016US$
  mutate(oilandgas_revenue = oilandgas_revenue / 1000,
         oilandgas_share = oilandgas_revenue / revenue_2016)

# For these companies, carve E&P share out of the product revenue share of all other markets
TR_missing_eandp_rows <- TR_missing_oilandgas_data %>%
  select(ISIN_code:revenue_2016, market, oilandgas_share) %>%
  mutate(market = "Exploration and production",
         product_revenue_share_2016 = NA_real_) %>%
  unique()

TR_missing_oilandgas_results <- TR_missing_oilandgas_data %<>%
  bind_rows(TR_missing_eandp_rows) %>%
  arrange(ISIN_code, market) %>%
  # Preserve E&P share from Rystad, but dilute all other shares so sum of product revenue shares is 1
  mutate(non_eandp_revenue_share_2016 = 1 - oilandgas_share) %>%
  arrange(non_eandp_revenue_share_2016) %>%
  group_by(ISIN_code) %>%
  fill(non_eandp_revenue_share_2016) %>%
  # Final product revenue share variable
  mutate(product_revenue_share = case_when(market == "Exploration and production" ~ oilandgas_share,
                                                TRUE ~ product_revenue_share_2016 * (non_eandp_revenue_share_2016 / sum(product_revenue_share_2016, na.rm = TRUE)))) %>%
  select(ISIN_code, company, market, product_revenue_share)

save_dated(TR_missing_oilandgas_results, "08_Fossil_fuel_prod/Interim/TR_missing_oilandgas_companies_exposure.rds", csv = TRUE)

#------------------------------------------------

#### Case 4.2: Companies which are down as E&P in TR product categores, but not in Rystad (vice versa of above)
Rystad_missing_oilandgas_data <- product_exposure_data %>%
  filter(!ISIN_code %in% fossil_fuel_dd_quantities_full$ISIN_code &
           ISIN_code %in% product_exposure_oilandgas_data$ISIN_code)

# Resolution is to replace E&P product category in exposure dataset
product_exposure_results <- product_exposure_data %>%
  # For these companies, E&P market entry is reallocated to other sectors
  mutate(market = case_when(company == "FRANCO-NEVADA" & market == "Exploration and production" ~ "Gold Mining", #Only one product reported - oil and gas - company-market is Gold Mining
                            company == "PEMBINA PIPELINE" & market == "Exploration and production" ~ "O&G T&D", #Not part of Rystad - reallocate to O&G T&D
                            company == "PERUSAHAAN GAS NEGARA" & market == "Exploration and production" ~ "O&G T&D", #Not part of Rystad - reallocate to O&G T&D
                            company == "KOC HOLDING" & market == "Exploration and production" ~ "Petrochemicals", #Not part of Rystad - reallocate to Petrochemicals
                            company == "HOLLYFRONTIER" & market == "Exploration and production" ~ "Petrochemicals", #Not an upstream oil and gas company
                            TRUE ~ market)) %>%
  # For BANPU and GENTING, drop E&P from exposure list and recalculate market shares
  filter(!(company == "BANPU" & market == "Exploration and production" |
             company == "GENTING" & market == "Exploration and production"))

save_dated(Rystad_missing_oilandgas_data, "08_Fossil_fuel_prod/Interim/Rystad_missing_oilandgas_companies", csv = TRUE)

#------------------------------------------------

#### Case 4.3: Companies which are down as E&P in both datasets

# Filter down to Case 4.3 observations
matched_oilandgas_data <- product_exposure_data %>%
  filter(ISIN_code %in% fossil_fuel_dd_quantities_full$ISIN_code &
           ISIN_code %in% product_exposure_oilandgas_data$ISIN_code) %>%
  left_join(fossil_fuel_dd_quantities_full) %>%
  # Convert oil and gas revenue to 000s 2016US$
  mutate(oilandgas_revenue = oilandgas_revenue / 1000)

# Calculate Rystad oil and gas share
matched_oilandgas_shares <- matched_oilandgas_data %>%
  filter(market == "Exploration and production") %>%
  select(-contains("_2017")) %>%
  # Calculate Rystad E&P share of business, capping values at 100%
  mutate(rystad_revenue_share = ifelse(oilandgas_revenue / revenue_2016 > 1, 1, oilandgas_revenue / revenue_2016),
         indicator_rystad_higher = case_when(rystad_revenue_share >= product_revenue_share_2016 ~ 1,
                                             TRUE ~ 0)) %>%
  arrange(desc(indicator_rystad_higher), industry_level_5, desc(rystad_revenue_share))

save_dated(matched_oilandgas_shares, "08_Fossil_fuel_prod/Interim/Matched_company_oilandgas_shares", csv = TRUE)

# Full dataset for matched companies, with Rystad E&P product-market share everywhere
matched_oilandgas_data %<>%
  left_join(matched_oilandgas_shares) %>%
  select(-contains("_2017"))

save_dated(matched_oilandgas_data, "08_Fossil_fuel_prod/Interim/Matched_company_all_shares", csv = TRUE)

# Solve for final market shares for oil and gas companies
matched_oilandgas_results <- matched_oilandgas_data %>%
  group_by(ISIN_code) %>%
  mutate(product_count = n(),
         # E&P companies with only one product category still have 100% product-market share
         product_revenue_share = case_when(product_count == 1 ~ 1,
                                           TRUE ~ NA_real_))

remaining_companies <- matched_oilandgas_results %>%
  filter(is.na(product_revenue_share))

# Case where Thomson Reuters revenue % is preferred (only chosen when Rystad is anomalous)
reuters_preferred_companies <- c("PETROLEO BRASILEIRO ON", "PETROLEO BRASILEIRO PN", #Petrobras is 58% E&P in Reuters vs. 40% in Rystad
                                 "CANADIAN NATURAL RES.", #Rystad % is 100% (capped), but company is operational in midstream - take Reuters (98.9%)
                                 "IMPERIAL OIL", #Upstream business seems to be substantial - Reuters value of 25% preferred to Rystad value of 6%
                                 "CHINA PTL.& CHM.'A'", "CHINA PTL.& CHM. 'H'", #Sinopec has substantial upstream business so Reuters value of 79% preferred to Rystad value of 7%
                                 "GLENCORE", #Thomson Reuters value preferred (50% vs. 0.3%) based on annual report: http://www.glencore.com/dam/jcr:79d87b60-d53a-4f1a-9dbe-4d523f27de83/GLEN-2016-Annual-Report.pdf
                                 "EQT", #EQT Corporation is involved in NGL processing and midstream activities (Rystad cap applies 100%, so use Reuters 65%)
                                 "OCCIDENTAL PTL." #Oxy (Occidental Petroleum) has substantial midstream operations (Rystad % is 99.2%, and Reuters is 65% so use Reuters)
                                 )

# Finish processing for case 4.3 companies
matched_oilandgas_results %<>%
  mutate(product_revenue_share = case_when(company %in% reuters_preferred_companies & market == "Exploration and production" ~ product_revenue_share_2016,
                                           is.na(product_revenue_share) & market == "Exploration and production" ~ rystad_revenue_share,
                                           !is.na(product_revenue_share) ~ product_revenue_share,
                                           TRUE ~ NA_real_)) %>%
  group_by(ISIN_code) %>%
  # Product revenue share is non-NA for only E&P at this stage
  mutate(oilandgas_share = mean(product_revenue_share, na.rm = TRUE),
         non_eandp_revenue_share_2016 = 1 - oilandgas_share,
         product_revenue_share_2016 = ifelse(market == "Exploration and production", 0, product_revenue_share_2016)) %>%
  mutate(product_revenue_share = case_when(is.na(product_revenue_share) ~ product_revenue_share_2016 * (non_eandp_revenue_share_2016 / sum(product_revenue_share_2016, na.rm = TRUE)),
                                           TRUE ~ product_revenue_share)) %>%
  select(ISIN_code, company, market, product_revenue_share)

#--------------------------------------------------------------------------------------------------

#### Section 5: Consolidate results and correct product revenue shares for companies which have dropped categories (4.2)

product_exposure_results %<>%
  filter(!ISIN_code %in% TR_missing_oilandgas_results$ISIN_code &
           !ISIN_code %in% matched_oilandgas_results$ISIN_code) %>%
  mutate(product_revenue_share = product_revenue_share_2017) %>%
  select(ISIN_code, company, market, product_revenue_share) %>%
  # Merge in Thomson Reuters missing results
  bind_rows(TR_missing_oilandgas_results) %>%
  # Merge in matched results
  bind_rows(matched_oilandgas_results)

# Merge in other categories for final product exposure dataset (match output of 6 - Financial prod/Financial_products file)
product_exposure_2017_data %<>%
  select(ISIN_code:revenue) %>%
  unique()

# Mop up 'oil and gas other' companies (allocate these to actual markets)
oilandgas_other_results <- product_exposure_results %>%
  filter(market == "Oil and gas other") %>%
  arrange(ISIN_code)

oilandgas_other_results %<>%
  mutate(market = ifelse(market == "Oil and gas other",
                         case_when(company == "CALTEX AUSTRALIA" ~ "Petrochemicals",
                                   company == "KEYERA" ~ "O&G T&D",
                                   company == "NESTE" ~ "Petrochemicals",
                                   company == "JXTG HOLDINGS" ~ "Petrochemicals",
                                   company == "IRPC" ~ "Petrochemicals",
                                   company == "KOC HOLDING" ~ "Petrochemicals",
                                   company == "TUPRAS TKI.PEL.RFNE." ~ "Petrochemicals",
                                   company == "FORMOSA PETROCHEMICAL" ~ "Petrochemicals",
                                   company == "ANDEAVOR" ~ "Petrochemicals",
                                   company == "CABOT OIL & GAS 'A'" ~ "Petrochemicals", #An E&P company, but issues with Rystad data - bucket with refineries for now
                                   company == "MARATHON PETROLEUM" ~ "Petrochemicals",
                                   company == "PHILLIPS 66" ~ "Petrochemicals",
                                   company == "TARGA RESOURCES" ~ "O&G T&D",
                                   TRUE ~ NA_character_),
                         market))

# Fill in other oil and gas results and tidy up the dataset
product_exposure_results %<>%
  filter(!ISIN_code %in% oilandgas_other_results$ISIN_code) %>%
  bind_rows(oilandgas_other_results) %>%
  # Sum over duplicate categories (for instance, Hollyfrontier and Petrochemicals)
  group_by(ISIN_code, company, market) %>%
  summarise(product_revenue_share = sum(product_revenue_share)) %>%
  # Correct companies which do not sum to 1 due to category removal (for instance, BANPU)
  group_by(ISIN_code) %>%
  mutate(product_revenue_share = product_revenue_share / sum(product_revenue_share)) %>%
  # Drop any zero revenue product categories
  filter(product_revenue_share != 0) %>%
  ungroup() %>%
  left_join(product_exposure_2017_data) %>%
  select(ISIN_code, company, starts_with("industry_level_"), parent_market, year, revenue, market, product_revenue_share)

save_dated(product_exposure_results, "08_Fossil_fuel_prod/Output/Market_exposure_results_oilandgas", csv = TRUE)
