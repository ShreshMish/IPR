##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  29/08/2018
##### Code author:        Shyamal Patel
##### Description:        This script calculates fossil fuel sector market shares for each company, and outputs a 'final' product exposure
#####                     dataset for read in by the "10 - Pane setting/Panel setting.R" script
##### Dependencies:       1.  2017 results from product exposure analysis: "6 - Financial prod/Market_exposure_results.rds"
#####                     2.  2016 results from product exposure analysis: "6 - Financial prod/Market_exposure_results_2016.rds"
#####                         Older files can be found in the ".../Dated/" folder
#####                     3.  Results from fossil fuel demand destruction analysis:
#####                         a) "Input/DD NPV stranding impacts.rds"
#####                         b) "Input/DD stranding data compact.rds"
#####                         c) "Input/DD full raw dataset.rds"
#####                         d) "Input/Combined Green Revenue data.rds"
#####                         (Copied over from "2.1 - Fossil demand destruction/Demand destruction R Project/R data files/...)
#####                         (Copied over from "0 - Data cleaning/0.5 - Green upside/Cleaned data/Combined Green Revenue data.rds") file

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA

packages <- c("tidyverse", "magrittr", "readxl", "here", "stringi")
lapply(packages, require, character.only = TRUE)

# Define date for save file names
day <- format(Sys.time(), "%d")
month <- match(format(Sys.time(), "%b"), month.abb)
if(nchar(month) == 1) {month <- paste0("0", month)}
year <- substr(format(Sys.time(), "%Y"), 3, 4)
date <- paste0(year, month, day)

# These functions count the number of missing or zero-value observations in a tibble
na_counter <- function(x) {sum(is.na(x))}
zero_counter <- function(x) {sum(ifelse(x == 0, 1, 0), na.rm = TRUE)}

# These functions save base, and dated files in the Interim or Outputs folder for later use
# Dated files are kept for version control purposes
save_dated <- function(data, name = " ", folder, dated = "YES", csv = "NO") {
  main_path <- paste0("9 - Fossil and green upside prod/", folder)
  dated_path <- paste0("9 - Fossil and green upside prod/", folder, "/Dated/")
  
  if(name == " ") {name <- gsub("_", " ", deparse(substitute(data)))}
  
  # Save main file
  saveRDS(data, here(main_path, paste0(name, ".rds")))
  # Save dated file (optional)
  if(dated == "YES") {saveRDS(data, here(dated_path, paste0(date, "_", name, ".rds")))}
  # Save dated CSV file (optional)
  if(csv == "YES") {write_csv(data, here(dated_path, paste0(date, "_", name, ".csv")))}
}

# Read in financial product exposure dataset for 2017
product_exposure_2017_data <- readRDS("7 - Financial prod/Output/Market_exposure_results.rds")

# Read in financial product exposure dataset for 2016
product_exposure_2016_data <- readRDS("7 - Financial prod/Output/Market_exposure_results_2016.rds")

# Read in Rystad oil & gas exposure dataset for all years
fossil_fuel_exposure_data <- readRDS("4 - Fossil Fuels/Oil and gas/Output/DD_model_data_compact.rds")

# Read in Rystad oil & gas company names
company_name_lookup <- read_excel("4 - Fossil Fuels/Oil and gas/Input/Rystad oil and gas.xlsx",
                                  sheet = "Clean lookup table", range = "$A$3:$E$150", col_names = TRUE) %>%
  mutate(Rystad_Name=gsub(" ","_",Rystad_Name))

# Read in green upside exposure full dataset
green_upside_exposure_data <- readRDS("5 - Green upside/Output/Combined_Green_Revenue_data.rds")

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

# Clean Rystad oil and gas product exposure dataset
fossil_fuel_exposure_data %<>%
  mutate(Rystad_Name=Company) %>%
  select(Rystad_Name,everything(),-Company) %>%
  left_join(company_name_lookup) %>%
  select(ISIN_Code:OG_Subsidiary,everything()) %>%
  rename(ISIN_code = ISIN_Code,
         company = VE_Name,
         product = Product,
         year = Year,
         scenario = Scenario) %>%
  select(scenario, ISIN_code, company, Rystad_Name, product, year, everything())

# Calculate oil & gas revenues in 2016 for each company under the central scenario (should be no variation across scenarios in 2016 anyway)
fossil_fuel_exposure_data %<>%
  filter(year == 2016 & scenario == "Central") %>%
  mutate(revenue = Production_F * Price_F) %>%
  filter(!is.na(ISIN_code)) %>%
  group_by(ISIN_code, company) %>%
  summarise(oilandgas_revenue = sum(revenue, na.rm = TRUE))

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Consolidated product exposure datasets and separate out E&P companies

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

#### Case 3.1: Companies which are in Rystad, but do not have E&P as product category in TR
TR_missing_oilandgas_data <- product_exposure_data %>%
  select(ISIN_code, company, parent_market, market, revenue_2016, product_revenue_share_2016) %>%
  filter(ISIN_code %in% fossil_fuel_exposure_data$ISIN_code &
           !(ISIN_code %in% product_exposure_oilandgas_data$ISIN_code))

# Merge in 2016 Rystad oil and gas revenue
TR_missing_oilandgas_data %<>%
  left_join(fossil_fuel_exposure_data) %>%
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

save_dated(TR_missing_oilandgas_results, "TR_missing_oilandgas_companies_exposure", folder = "Interim", csv = "YES")

#------------------------------------------------

#### Case 3.2: Companies which are down as E&P in TR product categores, but not in Rystad (vice versa of above)
Rystad_missing_oilandgas_data <- product_exposure_data %>%
  filter(!ISIN_code %in% fossil_fuel_exposure_data$ISIN_code &
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

save_dated(Rystad_missing_oilandgas_data, "Rystad_missing_oilandgas_companies", folder = "Interim", csv = "YES")

#------------------------------------------------

#### Case 3.3: Companies which are down as E&P in both datasets

# Filter down to Case 3.3 observations
matched_oilandgas_data <- product_exposure_data %>%
  filter(ISIN_code %in% fossil_fuel_exposure_data$ISIN_code &
           ISIN_code %in% product_exposure_oilandgas_data$ISIN_code) %>%
  left_join(fossil_fuel_exposure_data) %>%
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

save_dated(matched_oilandgas_shares, "Matched_company_oilandgas_shares", folder = "Interim", csv = "YES")

# Full dataset for matched companies, with Rystad E&P product-market share everywhere
matched_oilandgas_data %<>%
  left_join(matched_oilandgas_shares) %>%
  select(-contains("_2017"))

save_dated(matched_oilandgas_data, "Matched_company_all_shares", folder = "Interim", csv = "YES")

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

# Finish processing for case 3.3 companies
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
  select(ISIN_code, company, market, product_revenue_share) %>% 
  ungroup()

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Make any adjustments for coal companies which also produce oil and gas

# GLENCORE energy business was allocated to oil and gas, but is mostly coal: see annual report here - http://www.glencore.com/investors/reports-results/2017-annual-report
# 2017 coal revenue = 9787 mn, oil revenue = 280
glencore_exposure_coal_data <- matched_oilandgas_results %>%
  filter(company == "GLENCORE" & market == "Exploration and production") %>%
  mutate(market = "Coal",
         product_revenue_share = (9787 / (9787 + 280)) * product_revenue_share)
matched_oilandgas_results %<>%
  # Bind in coal production revenue share
  bind_rows(glencore_exposure_coal_data) %>%
  arrange(ISIN_code) %>%
  # Scale down exploration and production revenue share
  mutate(temp = mean(ifelse(company == "GLENCORE" & market == "Coal", product_revenue_share, NA_real_), na.rm = TRUE),
         product_revenue_share = case_when(company == "GLENCORE" & market == "Exploration and production" ~ product_revenue_share - temp,
                                           TRUE ~ product_revenue_share)) %>%
  select(-temp)

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

#--------------------------------------------------------------------------------------------------

#### Section 6: Coal adjustments for companies which do not produce oil and gas

# Create coal indicator (companies which have a coal-product category as a product category in TR) - this is for screening purposes only
product_exposure_coal_data <- product_exposure_results %>%
  select(ISIN_code, company, parent_market, market) %>%
  group_by(ISIN_code) %>%
  # Coal indicator
  mutate(market_coal = ifelse(market %in% c("Coal"), market, NA_character_)) %>%
  arrange(market_coal) %>%
  fill(market_coal) %>%
  filter(!is.na(market_coal)) %>%
  ungroup()

# Companies which are not thermal coal producers (all are coking coal, except for FANGDA CARBON which produces carbon fibre)
product_exposure_results %<>% 
  mutate(market = case_when(company == "FANGDA CBN.NEW MRA. 'A'" & market == "Coal" ~ "Other Mining",
                            company == "MAGNITOGORSK IOSTL.WORKS" & market == "Coal" ~ "Coking coal",
                            company == "WESFARMERS" & market == "Coal" ~ "Coking coal", #WESFARMERS produce both coking and thermal coal, but primarily coking
                            market == "Other coal-related products" ~ "Coking coal", #Coking coal is the best fit for other coal-related products (introduced category which is primarily coke, coking coal, and gasified coal etc.)
                            TRUE ~ market)) %>%
  group_by(ISIN_code, company, parent_market, year, revenue, market) %>% 
  summarise(product_revenue_share = sum(product_revenue_share))

save_dated(product_exposure_results, "Market_exposure_results_oilgascoal", folder = "Interim", csv = "YES")

#--------------------------------------------------------------------------------------------------

#### Section 7: Clean green upside data

green_upside_exposure_data %<>%
  select(VE_ISIN, ISIN_code, tr_company, year, VE_category, VE_category_rev_share, GR_percent) %>%
  rename(gr_product = VE_category,
         gr_product_revenue_share = VE_category_rev_share,
         total_gr_share = GR_percent,
         company = tr_company)

# Test sum of green revenue categories against the reported total_gr_share
test_green_upside_totals <- green_upside_exposure_data %>%
  group_by(VE_ISIN, company) %>% 
  summarise(sum_over_products = sum(gr_product_revenue_share),
            total_gr_share = mean(total_gr_share)) %>%
  ungroup() %>%
  mutate(test = ifelse(round(sum_over_products, 4) == round(total_gr_share, 4), 0, 1)) %>%
  filter(test == 1)

# Group over categories and sum
green_upside_exposure_data %<>%
  group_by(VE_ISIN, ISIN_code, company, year, gr_product) %>%
  summarise(gr_product_revenue_share = sum(gr_product_revenue_share),
            total_gr_share = mean(total_gr_share, na.rm = TRUE)) %>%
  ungroup()

# Check differences with JS  
save_dated(test_green_upside_totals, "Green_upside_totals_comparison", folder = "Interim", csv = "YES")

# Remove other category from green upside exposure dataset, and rename other sectors as appropriate
green_upside_exposure_data %<>%
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
green_upside_exposure_data %<>%
  left_join(green_upside_redefinition)

# Rename ISIN code variables to match main dataset and final cleaning of data
green_upside_exposure_data %<>%
  rename(FTSE_ISIN_code = ISIN_code,
         ISIN_code = VE_ISIN) %>%
  # Drop CCS (exposure does not matter)
  filter(gr_product != "CCS") %>%
  select(ISIN_code, company, gr_product, gr_product_renamed, gr_product_revenue_share) %>%
  # Filter out zero revenue companies (no exposure)
  filter(gr_product_revenue_share != 0)

save_dated(green_upside_exposure_data, "Green_upside_exposure_results", folder = "Interim", csv = "YES")

#--------------------------------------------------------------------------------------------------

#### Section 8: Green upside adjustments for all companies which have exposure

# Rename GR dataset model variables to match product_exposure_results dataset
green_upside_exposure_data %<>%
  rename(market = gr_product_renamed,
         product_revenue_share = gr_product_revenue_share) %>%
  select(-gr_product)

# Remove ISIN codes which do not exist in the main dataset before binding rows
green_upside_exposure_data %<>%
  filter(ISIN_code %in% product_exposure_results$ISIN_code)

# Bind together datasets
product_exposure_results %<>%
  bind_rows(green_upside_exposure_data) %>%
  group_by(ISIN_code) %>%
  arrange(ISIN_code, parent_market) %>%
  fill_(fill_cols = c("parent_market", "year", "revenue")) %>%
  ungroup()

# Replace BMW PREF. (XET) (ISIN Code = DE0005190037) value with that for BMW (XET) (ISIN Code = DE0005190003) [former is anomalous]
product_exposure_results %<>%
  mutate(mean_BMW_EVs = mean(ifelse(ISIN_code == "DE0005190003" & market == "GR_EVs", product_revenue_share, NA_real_), na.rm = TRUE),
         product_revenue_share = case_when(ISIN_code == "DE0005190037" & market == "GR_EVs" ~ mean_BMW_EVs,
                                           TRUE ~ product_revenue_share)) %>%
  select(-mean_BMW_EVs)

# Adjust down product_revenue_share for non_green sectors
product_exposure_results %<>%
  group_by(ISIN_code) %>%
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

save_dated(product_exposure_results, "Product_exposure_results_integrating_GR_full", folder = "Interim", csv = "YES")

# Remove redundant variables and save final product exposure dataset
product_exposure_results %<>%
  select(ISIN_code:product_revenue_share)

save_dated(product_exposure_results, "Market_exposure_results_oilgascoalgreen", folder = "Output", csv = "YES")

# Save table of green upside category reclassification
save_dated(green_upside_redefinition, "Green_upside_reclassification", folder = "Output", csv = "YES")
