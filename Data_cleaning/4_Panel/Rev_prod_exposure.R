##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  16/02/2019
##### Code author:        Shyamal Patel
##### Description:        Revised product exposure data based on Rystad oil & gas, and FTSE Russell green revenue data
##### Dependencies:       1.  2017 results from product exposure analysis: "2_Financial/Output/2c_Product_exposure/Market_exposure_results.rds"
#####                     2.  2016 results from product exposure analysis: "2_Financial/Output/2c_Product_exposure/Market_exposure_results_2016.rds"
#####                     3.  Rystad oil & gas cleaned dataset: "3_ESG/3b_Oil_and_gas/Output/Model_prod_and_economic_data.rds"
#####                     4.  Rystad oil & gas companies list: "3_ESG/3b_Oil_and_gas/Output/Companies_list.rds"
#####                     5.  Cleantech patent and revenue dataset: "3_ESG/3d_Cleantech/Output/Cleantech_patent_and_reV_data.rds"
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "4_Panel"
source("utils.R")

# Read in financial product exposure dataset for 2017
prod_exposure_2017_data <- readRDS("2_Financial/2c_Product_exposure/Output/Prod_exposure_results.rds")

# Read in financial product exposure dataset for 2016
prod_exposure_2016_data <- readRDS("2_Financial/2c_Product_exposure/Output/Prod_exposure_results_2016.rds")

# Read in Rystad oil & gas exposure dataset for all years
fossil_fuel_exposure_data <- readRDS("3_ESG/3b_Oil_and_gas/Output/Model_prod_and_economic_data.rds")

# Read in Rystad oil & gas company names
fossil_fuel_names_data <- readRDS("3_ESG/3b_Oil_and_gas/Output/Companies_list.rds")

# Read in green upside exposure full dataset
cleantech_exposure_data <- readRDS("3_ESG/3d_Cleantech/Output/Cleantech_patent_and_rev_data.rds")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean up financial product exposure data ----

# Remove redundant variables from 2016 dataset in preparation of merge
prod_exposure_2016_data2 <- prod_exposure_2016_data %>%
  rename(revenue_2016 = revenue,
         product_revenue_share_2016 = product_revenue_share) %>%
  select(company_id, company, market, revenue_2016, product_revenue_share_2016)

# Save 2016 revenue data separately (to avoid it being loss during merges if market categories 
# do not align between 2016 and 2017)
revenue_2016_data <- prod_exposure_2016_data2 %>% 
  select(company_id, company, revenue_2016) %>%
  unique()

# Remove revenue 2016 from prod exposure 2016 dataset
prod_exposure_2016_data3 <- prod_exposure_2016_data2 %>%
  select(-revenue_2016)

# Merge into 2017 product exposure data
prod_exposure_data <- prod_exposure_2017_data %>%
  rename(revenue_2017 = revenue,
         product_revenue_share_2017 = product_revenue_share) %>%
  left_join(revenue_2016_data, by = c("company", "company_id")) %>%
  left_join(prod_exposure_2016_data3, by = c("company", "company_id", "market")) %>%
  # Replace product_revenue_share_2016 with 2017 proportion when this is unavailable
  # Note that this will cause 2016 revenue shares to not sum to 1 (these will only be used for E&P company calculations anyway)
  # Product-market share is based on 2017 data for all other companies
  mutate(product_revenue_share_2016 = case_when(is.na(product_revenue_share_2016) ~ product_revenue_share_2017,
                                                TRUE ~ product_revenue_share_2016))

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Clean up Rystad oil and gas exposure data ----

# Clean Rystad oil and gas product exposure dataset
fossil_fuel_exposure_data2 <- fossil_fuel_exposure_data %>%
  left_join(fossil_fuel_names_data, by = "rystad_name") %>%
  select(company_id, company, rystad_name, everything())
  
# Calculate oil & gas revenues in 2016 for each company under the central scenario (should be no variation across scenarios in 2016 anyway)
fossil_fuel_exposure_data3 <- fossil_fuel_exposure_data2 %>%
  filter(year == 2016 & scenario == "Central") %>%
  mutate(revenue = production_f * price_f) %>%
  filter(!is.na(company_id)) %>%
  group_by(company_id, company) %>%
  summarise(oilandgas_revenue = sum(revenue, na.rm = TRUE))

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - E&P (included in Rystad) companies product exposure analysis ----

oilandgas_prod_exposure <- prod_exposure_data %>%
  # Rystad oil & gas revenue data is for 2016 only
  select(company_id, company, parent_market, market, revenue_2016, product_revenue_share_2016) %>%
  group_by(company_id) %>%
  # Exploration and production indicator
  mutate(eandp_indicator = sum(ifelse(market == "Exploration and production", 1, 0))) %>%
  filter(eandp_indicator == 1) %>%
  ungroup()

#------------------------------------------------

#### Case 4.1: Companies which are in Rystad, but do not have E&P product category in TR ----
oilandgas_exposure_data1 <- prod_exposure_data %>%
  select(company_id, company, parent_market, market, revenue_2016, product_revenue_share_2016) %>%
  filter(company_id %in% fossil_fuel_exposure_data3$company_id &
           !(company_id %in% oilandgas_prod_exposure$company_id))

# Merge in 2016 Rystad oil and gas revenue
oilandgas_exposure_data1_2 <- oilandgas_exposure_data1 %>%
  left_join(fossil_fuel_exposure_data3, by = c("company_id", "company")) %>%
  # Convert oil and gas revenue to mn 2016US$
  mutate(oilandgas_revenue = oilandgas_revenue / 10^6,
         oilandgas_share = oilandgas_revenue / revenue_2016)

# Add row for E&P share of these companies
oilandgas_exposure_data1_3 <- expand.grid(company_id = unique(oilandgas_exposure_data1_2$company_id),
                                          stringsAsFactors = FALSE) %>%
  mutate(market = "Exploration and production",
         product_revenue_share_2016 = NA_real_) %>%
  bind_rows(oilandgas_exposure_data1_2) %>%
  select(company_id, company, parent_market, market, revenue_2016, everything()) %>%
  arrange(company_id, company, market) %>%
  fill_(c("company", "parent_market", "revenue_2016", "oilandgas_revenue", "oilandgas_share"))

# Preserve E&P share from Rystad, but dilute all other shares so sum of product revenue shares is 1
oilandgas_exposure_data1_4 <- oilandgas_exposure_data1_3 %>%
  group_by(company_id) %>%
  mutate(noneandp_revenue_share_2016 = 1 - oilandgas_share) %>%
  mutate(product_revenue_share = case_when(market == "Exploration and production" ~ oilandgas_share,
                                           TRUE ~ product_revenue_share_2016 * noneandp_revenue_share_2016 / sum(product_revenue_share_2016, na.rm = TRUE))) %>%
  select(company_id, company, market, product_revenue_share)

save_dated(oilandgas_exposure_data1_4, "TR_missing_oilandgas_prod_exposure", folder = "Interim", csv = TRUE)

#------------------------------------------------

#### Case 4.2: Companies which are in TR as E&P, but not in Rystad (vice versa of above) ----
oilandgas_exposure_data2 <- prod_exposure_data %>%
  filter(!company_id %in% fossil_fuel_exposure_data3$company_id &
           company_id %in% oilandgas_prod_exposure$company_id)

# Resolution is to replace E&P product category in exposure dataset using company-specific adjustments
oilandgas_exposure_data2_2 <- prod_exposure_data %>%
  select(company_id, company, parent_market, market, product_revenue_share_2017) %>%
  mutate(market_old = market) %>%
  # For these companies, E&P market entry is reallocated to other sectors
  mutate(market = case_when(company == "FRANCO-NEVADA" & market_old == "Exploration and production" ~ "Gold Mining", #Only one product reported - oil and gas - company-market is Gold Mining
                            company == "PEMBINA PIPELINE" & market_old == "Exploration and production" ~ "O&G T&D", #Not part of Rystad - reallocate to O&G T&D
                            company == "PERUSAHAAN GAS NEGARA" & market_old == "Exploration and production" ~ "O&G T&D", #Not part of Rystad - reallocate to O&G T&D
                            company == "KOC HOLDING" & market_old == "Exploration and production" ~ "Petrochemicals", #Not part of Rystad - reallocate to Petrochemicals
                            company == "HOLLYFRONTIER" & market_old == "Exploration and production" ~ "Petrochemicals", #Not an upstream oil and gas company
                            TRUE ~ market_old)) %>%
  # For BANPU and GENTING, drop E&P from exposure list and recalculate market shares [note that this happens later in the code]
  filter(!(company == "BANPU" & market_old == "Exploration and production" |
             company == "GENTING" & market_old == "Exploration and production"))

save_dated(oilandgas_exposure_data2_2, "Rystad_missing_oilandgas_prod_exposure", folder = "Interim", csv = TRUE)

oilandgas_exposure_data2_3 <- oilandgas_exposure_data2_2 %>%
  select(-market_old)

#------------------------------------------------

#### Case 4.3: Companies which are down as E&P in both datasets ----
oilandgas_exposure_data3 <- prod_exposure_data %>%
  filter(company_id %in% fossil_fuel_exposure_data3$company_id & 
           company_id %in% oilandgas_prod_exposure$company_id)

# Join in Rystad data
oilandgas_exposure_data3_2 <- oilandgas_exposure_data3 %>%
  left_join(fossil_fuel_exposure_data3, by = c("company_id", "company")) %>%
  # Convert oil and gas revenue to mn 2016US$
  mutate(oilandgas_revenue = oilandgas_revenue / 10^6)

# Calculate Rystad oil & gas revenue share (Rystad data is for 2016, so drop 2017 TR data)
oilandgas_exposure_data3_3 <- oilandgas_exposure_data3_2 %>%
  filter(market == "Exploration and production") %>%
  select(-contains("_2017")) %>%
  mutate(rystad_revenue_share = ifelse(oilandgas_revenue / revenue_2016 > 1, 1, oilandgas_revenue / revenue_2016),
         indicator_rystad_higher = ifelse(rystad_revenue_share >= product_revenue_share_2016, 1, 0)) %>%
  arrange(desc(indicator_rystad_higher), industry_level_5, desc(rystad_revenue_share))

oilandgas_exposure_data3_4 <- oilandgas_exposure_data3_2 %>%
  left_join(oilandgas_exposure_data3_3) %>%
  select(-contains("_2017"))

save_dated(oilandgas_exposure_data3_4, "Matched_oilandgas_prod_exposure_shares", folder = "Interim", csv = TRUE)

#------------------------------------------------

#### Consolidation 4.4: Calculate oil & gas company final product exposure shares ----

oilandgas_exposure_results <- oilandgas_exposure_data3_4 %>%
  group_by(company_id) %>%
  mutate(product_count = n(),
         # E&P companies with only one product category still have 100% product market share
         product_revenue_share = ifelse(product_count == 1, 1 , NA_real_))

# Case where Thomson Reuters revenue % is preferred (only chosen when Rystad is anomalous)
reuters_preferred_companies <- c("PETROLEO BRASILEIRO ON", #Petrobras is 58% E&P in Reuters vs. 40% in Rystad
                                 "CANADIAN NATURAL RES.", #Rystad % is 100% (capped), but company is operational in midstream - take Reuters (98.9%)
                                 "IMPERIAL OIL", #Upstream business seems to be substantial - Reuters value of 25% preferred to Rystad value of 6%
                                 "CHINA PTL.& CHM.'A'", #Sinopec has substantial upstream business so Reuters value of 79% preferred to Rystad value of 7%
                                 "GLENCORE", #Thomson Reuters value preferred (50% vs. 0.3%) based on annual report: http://www.glencore.com/dam/jcr:79d87b60-d53a-4f1a-9dbe-4d523f27de83/GLEN-2016-Annual-Report.pdf
                                 "EQT", #EQT Corporation is involved in NGL processing and midstream activities (Rystad cap applies 100%, so use Reuters 65%)
                                 "OCCIDENTAL PTL." #Oxy (Occidental Petroleum) has substantial midstream operations (Rystad % is 99.2%, and Reuters is 65% so use Reuters)
)

# Finish processing all cases
oilandgas_exposure_results2 <- oilandgas_exposure_results %>%
  mutate(product_revenue_share = case_when(company %in% reuters_preferred_companies & market == "Exploration and production" ~ product_revenue_share_2016,
                                           is.na(product_revenue_share) & market == "Exploration and production" ~ rystad_revenue_share,
                                           !is.na(product_revenue_share) ~ product_revenue_share,
                                           TRUE ~ NA_real_)) %>%
  group_by(company_id) %>%
  # Product revenue share is non-NA for only E&P at this stage
  mutate(oilandgas_share = mean(product_revenue_share, na.rm = TRUE),
         non_eandp_revenue_share_2016 = 1 - oilandgas_share,
         product_revenue_share_2016 = ifelse(market == "Exploration and production", 0, product_revenue_share_2016)) %>%
  mutate(product_revenue_share = case_when(is.na(product_revenue_share) ~ product_revenue_share_2016 * (non_eandp_revenue_share_2016 / sum(product_revenue_share_2016, na.rm = TRUE)),
                                           TRUE ~ product_revenue_share)) %>%
  select(company_id, company, market, product_revenue_share) %>% 
  ungroup()

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Coal and oil & gas companies product exposure adjustments ----

# GLENCORE energy business was allocated to oil and gas, but is mostly coal: see annual report here - http://www.glencore.com/investors/reports-results/2017-annual-report
# 2017 coal revenue = 9787 mn, oil revenue = 280
glencore_exposure_coal_data <- oilandgas_exposure_results2 %>%
  filter(company == "GLENCORE" & market == "Exploration and production") %>%
  mutate(market = "Coal",
         product_revenue_share = (9787 / (9787 + 280)) * product_revenue_share)

oilandgas_exposure_results3 <- oilandgas_exposure_results2 %>%
  # Bind in coal production revenue share
  bind_rows(glencore_exposure_coal_data) %>%
  arrange(company_id) %>%
  # Scale down exploration and production revenue share
  mutate(temp = mean(ifelse(company == "GLENCORE" & market == "Coal", product_revenue_share, NA_real_), na.rm = TRUE),
         product_revenue_share = case_when(company == "GLENCORE" & market == "Exploration and production" ~ product_revenue_share - temp,
                                           TRUE ~ product_revenue_share)) %>%
  select(-temp)

#--------------------------------------------------------------------------------------------------

#### Section 6: Consolidate results and correct product revenue shares for companies which have dropped categories (4.2) ----

product_exposure_results <- oilandgas_exposure_data2_3 %>%
  filter(!company_id %in% oilandgas_exposure_data1_4$company_id &
           !company_id %in% oilandgas_exposure_results3$company_id) %>%
  mutate(product_revenue_share = product_revenue_share_2017) %>%
  select(company_id, company, market, product_revenue_share) %>%
  # Merge in Thomson Reuters missing results
  bind_rows(oilandgas_exposure_data1_4) %>%
  # Merge in matched results
  bind_rows(oilandgas_exposure_results3)

# Keep one row per company with just revenue data
prod_exposure_2017_data2 <- prod_exposure_2017_data %>%
  select(company_id:revenue) %>%
  unique()

# Mop up 'oil and gas other' companies (allocate these to actual markets)
oilandgas_other_results <- product_exposure_results %>%
  filter(market == "Oil and gas other") %>%
  arrange(company_id) %>%
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
product_exposure_results2 <- product_exposure_results %>%
  filter(!company_id %in% oilandgas_other_results$company_id) %>%
  bind_rows(oilandgas_other_results) %>%
  # Sum over duplicate categories (for instance, Hollyfrontier and Petrochemicals)
  group_by(company_id, company, market) %>%
  summarise(product_revenue_share = sum(product_revenue_share)) %>%
  # Correct companies which do not sum to 1 due to category removal (for instance, BANPU)
  group_by(company_id) %>%
  mutate(product_revenue_share = product_revenue_share / sum(product_revenue_share)) %>%
  # Drop any zero revenue product categories
  filter(product_revenue_share != 0) %>%
  ungroup() %>%
  left_join(prod_exposure_2017_data2, by = c("company_id", "company")) %>%
  select(company_id, company, starts_with("industry_level_"), parent_market, year, revenue, market, product_revenue_share)

#--------------------------------------------------------------------------------------------------

#### Section 7: Coal companies product exposure analysis ----

# Create coal indicator (companies which have a coal-product category as a product category in TR)
# (screening purposes only)
product_exposure_coal_data <- product_exposure_results2 %>%
  select(company_id, company, parent_market, market) %>%
  group_by(company_id) %>%
  # Coal indicator
  mutate(market_coal = ifelse(market %in% c("Coal"), market, NA_character_)) %>%
  arrange(market_coal) %>%
  fill(market_coal) %>%
  filter(!is.na(market_coal)) %>%
  ungroup()

# Companies which are not thermal coal producers (all are coking coal, except for FANGDA CARBON which produces carbon fibre)
product_exposure_results3 <- product_exposure_results2 %>% 
  mutate(market = case_when(company == "FANGDA CBN.NEW MRA. 'A'" & market == "Coal" ~ "Other Mining",
                            company == "MAGNITOGORSK IOSTL.WORKS" & market == "Coal" ~ "Coking coal",
                            company == "WESFARMERS" & market == "Coal" ~ "Coking coal", #WESFARMERS produce both coking and thermal coal, but primarily coking
                            market == "Other coal-related products" ~ "Coking coal", #Coking coal is the best fit for other coal-related products (introduced category which is primarily coke, coking coal, and gasified coal etc.)
                            TRUE ~ market)) %>%
  group_by(company_id, company, parent_market, year, revenue, market) %>% 
  summarise(product_revenue_share = sum(product_revenue_share))

save_dated(product_exposure_results3, "Prod_exposure_results_oilgascoal", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

#### Section 8: Cleantech markets companies product exposure analysis ----

cleantech_exposure_data2 <- cleantech_exposure_data %>%
  select(company_id, company, year, ve_category, ve_category_rev_share, gr_percent) %>%
  rename(gr_product = ve_category,
         gr_product_revenue_share = ve_category_rev_share,
         total_gr_share = gr_percent)

# Test sum of green revenue categories against the reported total_gr_share
test_cleantech_totals <- cleantech_exposure_data2 %>%
  group_by(company_id, company) %>% 
  summarise(sum_gr_share = sum(gr_product_revenue_share),
            total_gr_share = mean(total_gr_share)) %>%
  ungroup() %>%
  mutate(test = ifelse(round(sum_gr_share, 4) == round(total_gr_share, 4), 0, 1)) %>%
  filter(test == 1)

# Check discrepancies with Justine's cleantech revenue data cleaning  
save_dated(test_cleantech_totals, "Cleantech_totals_comparison", folder = "Interim", csv = TRUE)

# Group over categories and sum
cleantech_exposure_data3 <- cleantech_exposure_data2 %>%
  group_by(company_id, company, year, gr_product) %>%
  summarise(gr_product_revenue_share = sum(gr_product_revenue_share),
            total_gr_share = mean(total_gr_share, na.rm = TRUE)) %>%
  ungroup()

# Remove other category from green upside exposure dataset, and rename other sectors as appropriate
cleantech_exposure_data4 <- cleantech_exposure_data3 %>%
  filter(gr_product != "Other") #Other category is not used in our analysis

green_upside_redefinition <- tribble(~gr_product, ~gr_product_renamed,
                                     "Biofuels_production",     "GR_biofuels",
                                     "CCS",                     "GR_CCS",
                                     "Hydro_power",             "GR_hydro",
                                     "EV_aggregate",            "GR_EVs",
                                     "Minerals_for_batteries",  "GR_minerals",
                                     "Solar_power",             "GR_solar",
                                     "Wind_power",              "GR_wind")

# Merge in new categories
cleantech_exposure_data5 <- cleantech_exposure_data4 %>%
  left_join(green_upside_redefinition, by = "gr_product") %>%
  # Drop CCS (exposure does not matter)
  filter(gr_product != "CCS") %>%
  select(company_id, company, gr_product, gr_product_renamed, gr_product_revenue_share) %>%
  # Filter out zero revenue companies (no exposure)
  filter(gr_product_revenue_share != 0)

save_dated(cleantech_exposure_data5, "Cleantech_prod_exposure_results", folder = "Interim", csv = TRUE)

# Rename GR dataset model variables to match product_exposure_results dataset
cleantech_exposure_data6 <- cleantech_exposure_data5 %>%
  rename(market = gr_product_renamed,
         product_revenue_share = gr_product_revenue_share) %>%
  select(-gr_product)

# Remove ISIN codes which do not exist in the main dataset before binding rows
# (should not change the dataset)
cleantech_exposure_data7 <- cleantech_exposure_data6 %>%
  filter(company_id %in% product_exposure_results3$company_id)

# Bind together datasets
product_exposure_results4 <- product_exposure_results3 %>%
  bind_rows(cleantech_exposure_data7) %>%
  group_by(company_id) %>%
  arrange(company_id, parent_market) %>%
  fill_(fill_cols = c("parent_market", "year", "revenue")) %>%
  ungroup()

# Adjust down product_revenue_share for non_green sectors
product_exposure_results5 <- product_exposure_results4 %>%
  group_by(company_id) %>%
  mutate(GR_status = case_when(grepl("GR", market) ~ "Y", TRUE ~ "N"),
         GR_product_revenue_share = case_when(GR_status == "Y" ~ product_revenue_share,
                                              TRUE ~ 0),
         GR_product_revenue_share = sum(GR_product_revenue_share),
         non_GR_product_revenue_share = 1 - GR_product_revenue_share) %>%
  # Adjust product_revenue_share only when category is NOT green revenues (full confidence in green revenue % exposures)
  mutate(product_revenue_share = case_when(GR_status == "Y" ~ product_revenue_share,
                                           GR_status == "N" & non_GR_product_revenue_share == 0 ~ 0,
                                           GR_status == "N" ~ product_revenue_share * non_GR_product_revenue_share,
                                           TRUE ~ NA_real_)) %>%
  # Remove redundant lines
  filter(product_revenue_share != 0)

save_dated(product_exposure_results5, "Prod_exposure_results_oilgascoalgreen", folder = "Interim", csv = TRUE)

# Remove redundant variables and save final product exposure dataset
product_exposure_results6 <- product_exposure_results5 %>%
  select(company_id:product_revenue_share)

#--------------------------------------------------------------------------------------------------

#### Section 9: Ad hoc changes to company parent market categorisations ----

concrete_parent_markets <- c("AU000000JHX1", "CH0012214059", "CNE0000019V8 CNE1000001W2",
                             "CNE1000002N9", "COD38PA00046", "COT09PA00035",
                             "DE0006047004", "GRS074083007", "ID1000061302",
                             "ID1000106800", "INE047A01021", "INE070A01015",
                             "INE079A01024", "INE481G01011", "JP3449020001",
                             "KYG2113L1068", "MXP225611567", "PK0071501016",
                             "TW0001101004", "TW0001102002")

product_exposure_results7 <- product_exposure_results6 %>%
  mutate(parent_market = case_when(company_id %in% concrete_parent_markets ~ "Concrete and cement",
                                   company == "SHANXI XISHAN C&ELY.PWR. 'A'" ~ "Coking coal",
                                   TRUE ~ parent_market))

save_dated(product_exposure_results7, "Rev_prod_exposure_results", folder = "Output", csv = TRUE)

# Save table of green upside category reclassification
save_dated(green_upside_redefinition, "Green_upside_reclassification", folder = "Output", csv = TRUE)