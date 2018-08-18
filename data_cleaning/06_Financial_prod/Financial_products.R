##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  11/08/2018
##### Code author:        Shyamal Patel
##### Description:        This script reads in TR financial data from Excel, and cleans the data before
#####                     further data cleaning takes place
##### Dependencies:       1.  Latest Thomson Reuters cleaned dataset: "1 - Financial prelim/Output/TR_cleaned_2016USD_data.rds"
#####                     2.  Latest list of oil and gas analysis companies: "4 - Fossil fuels/Oil and gas/Output/DD analysis companies list.rds"
#####                     3.  Ethan's product-market matching exercise results: "6 - Financial prod/Input/Market product classification_EM.xlsx",
#####                     4.  Aaron's company-product-market matching exercise results: "6 - Financial prd/Input/Company product classificaiton_AT.xlsx"
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA

packages <- c("tidyverse", "magrittr", "readxl", "here", "stringi")
lapply(packages, require, character.only = TRUE)
source(here::here("utils.R"))

# Read in Thomson Reuters cleaned dataset (2016US$)
financial_data <- readRDS(path_to_data_file("01_Financial_prelim/Output/TR_cleaned_2016USD_data.rds"))

## Read in market-product allocation exercise findings

# Ethan's initial work, supplemented with Shyamal's sweep through of remaining sectors
product_market_allocation_1 <- read_excel(path_to_data_file("06_Financial_prod/Input/Market product classification_EM.xlsx"),
                                          sheet = "R1. Market product match", range = "$A$9:$D$3565")

# Aaron's PRELIMINARY results through company outliers (full list generated below in Section 3)
# Change range when Aaron does round 2 (w/c 13/08)
product_market_allocation_2 <- read_excel(path_to_data_file("06_Financial_prod/Input/Company product classification_AT.xlsx"),
                                          sheet = "180808 Company product classifi", range = "$A$1:$P$525")

# Rystad analysed oil and gas companies
oil_and_gas_companies <- readRDS(path_to_data_file("04_Fossil_fuels/Oil_and_gas/Output/DD_analysis_companies_list.rds")) %>%
  # Filter out companies not included in Justine's analysis
  filter(!(company %in% c("CABOT OIL & GAS 'A'", "GENTING")))

# CO2 emissions data
co2_emissions_data <- readRDS(path_to_data_file("02_CO2_emissions/Output/Trucost_cleaned_emissions_data.rds"))

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Preliminary cleaning and removing unrequired variables from each dataset

financial_data %<>%
  select(ticker:corporation_tax_rate, revenue_2017, revenue_2016, contains("product_"))

financial_prod_data <- financial_data %>%
  # Define markets based on old definitions (level 5 unless power or O&G)
  mutate(market = case_when(industry_level_4 == "Electricity" ~ "Power generation",
                            industry_level_4 == "Oil & Gas Producers" & company %in% oil_and_gas_companies$company ~ "Exploration and production",
                            industry_level_4 == "Oil & Gas Producers" ~ "Oil and gas other",
                            TRUE ~ industry_level_5))

# Clean Ethan's product-market allocation data
product_market_allocation_1 %<>%
  rename(product = Product,
         market = Market,
         market_new = `Product code 1`,
         comment = `Product code 2`) %>%
  mutate(market_new = case_when(is.na(market_new) ~ market,
                                TRUE ~ market_new))

# Clean CO2 emissions data
co2_emissions_data %<>%
  select(ISIN_code, year, starts_with("co2")) %>%
  select(-contains("intensity")) %>%
  filter(year >= 2016)

# Rearrange Thomson Reuters data to long format
financial_prod_data %<>%
  select(ticker:revenue_2016, market, everything()) %>%
  gather(key = "Category", value = "product", - (ticker:market)) %>%
  left_join(product_market_allocation_1, by = c("market", "product")) %>%
  select(ticker:industry_level_5, market, starts_with("revenue_"), everything()) %>%
  arrange(ISIN_code) %>%
  mutate(year = as.numeric(stri_extract_last_regex(Category, "[0-9]+")),
         temp = gregexpr("_", Category))

financial_prod_data$temp <- sapply(financial_prod_data$temp, function(temp) temp[2])

financial_prod_data %<>%
  mutate(category = substr(Category, start = 1, stop = temp - 1)) %>%
  select(ticker:market, starts_with("revenue"), market, category, year, Category, product, market_new) %>% # Drop comments (from Ethan) and temporary variables
  mutate(category_no = stri_extract_first_regex(Category, "[0-9]+"))

# Company revenue data
financial_company_rev_data <- financial_prod_data %>% 
  select(ISIN_code, revenue, revenue_2016) %>%
  rename(revenue_2017 = revenue) %>%
  unique() %>%
  gather(key = "year", value = "revenue", -ISIN_code) %>%
  mutate(year = as.numeric(gsub("\\D+", "", year))) %>%
  arrange(ISIN_code)

# Product category revenue data
financial_prod_rev_data <- financial_prod_data %>%
  select(ticker:market, category, year, Category, product, market_new, category_no) %>%
  filter(category == "product_revenue") %>%
  select(-Category, -category) %>%
  select(ticker:market, year, category_no, everything()) %>%
  rename(product_sales = product) %>%
  select(ISIN_code, year, category_no, product_sales)

# Product category name data
financial_prod_name_data <- financial_prod_data %>%
  select(ticker:market, category, year, Category, product, market_new, category_no) %>%
  filter(category == "product_name") %>%
  select(-Category, -category) %>%
  select(ticker:market, year, category_no, everything()) %>%
  rename(product_name = product)

# Combining datasets again
financial_prod_data <- financial_prod_name_data %<>%
  left_join(financial_prod_rev_data, by = c("ISIN_code", "year", "category_no")) %>%
  select(ISIN_code, company, starts_with("industry"), market, year, category_no, product_name, product_sales,
         market_new) %>%
  left_join(financial_company_rev_data, by = c("ISIN_code", "year")) %>%
  select(ISIN_code:market, year, revenue, everything()) %>%
  left_join(co2_emissions_data, by = c("ISIN_code", "year")) %>%
  group_by(ISIN_code) %>%
  arrange(co2_emissions_scope_1) %>%
  fill_(fill_cols = c("co2_emissions_scope_1", "co2_emissions_scope_2", "co2_emissions_scope_3")) %>%
  ungroup()

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Generate list of emissions intensity outlier companies for Aaron's analysis

# Generate emissions intensity variable for screening purposes
financial_prod_data %<>%
  # Scope 1 + 2 emissions intensity variable (tCO2 / US$)
  mutate(emissions_intensity = (co2_emissions_scope_1 + co2_emissions_scope_2) / revenue) %>%
  select(-starts_with("co2")) %>%
  select(ISIN_code:revenue, emissions_intensity, everything())

# Calculate market median emissions intensity for all original markets
financial_emissions_intensity_data <- financial_prod_data %>%
  filter(year == 2017) %>%
  select(ISIN_code, company, market, emissions_intensity, product_name, market_new) %>%
  select(-product_name, -market_new) %>%
  unique() %>%
  group_by(market) %>%
  mutate(median_emissions_intensity = median(emissions_intensity, na.rm = TRUE)) %>%
  ungroup() %>%
  # Screen companies based on 75% range around median emissions intensity of revenue
  mutate(emissions_intensity_screen = case_when(emissions_intensity >= 1.75 * median_emissions_intensity ~ 1,
                                                emissions_intensity <= 0.25 * median_emissions_intensity ~ 1,
                                                TRUE ~ 0)) %>%
  arrange(ISIN_code)

# Merge into main dataset
financial_prod_data %<>%
  left_join(financial_emissions_intensity_data) %>%
  group_by(ISIN_code) %>%
  arrange(emissions_intensity_screen) %>%
  fill(emissions_intensity_screen, .direction = "down") %>%
  ungroup()

# Create company-product list for Aaron to review
financial_data_for_aaron <- financial_prod_data %>%
  filter(is.na(market_new)) %>%
  filter(!is.na(product_name)) %>%
  filter(year == 2017) %>%
  filter(emissions_intensity_screen == 1) %>%
  select(ISIN_code:market, revenue, median_emissions_intensity, emissions_intensity, category_no:market_new) %>%
  mutate(priority = abs((emissions_intensity - median_emissions_intensity) / median_emissions_intensity)) %>%
  arrange(desc(priority)) %>%
  select(priority, everything())

save_dated(financial_data_for_aaron, "06_Financial_prod/Interim/Company_product_classification", csv = TRUE)

# Markets list for Aaron's analysis
markets_for_aaron <- as.tibble(financial_prod_data$market) %>%
  rename(market = value)
aaron_markets_data_temp <- as.tibble(financial_prod_data$market_new) %>%
  rename(market = value)

markets_for_aaron %<>%
  bind_rows(aaron_markets_data_temp) %>%
  unique() %>%
  arrange(market) %>%
  filter(!(market %in% c("Iron and steel fabrication", "Iron and steel foundary"))) %>%
  mutate(market = case_when(market %in% c("Broadline Retailers", "Food Retail,Wholesale") ~ "Food and broadline retailers",
                            market %in% c("Clothing & Accessory", "Footwear") ~ "Clothing, footwear and accessories",
                            market %in% c("Recreational Products", "Recreational Services") ~ "Recreational products and services",
                            market == "Hotel & Lodging REITs" ~ "Diversified REITs",
                            TRUE ~ market)) %>%
  filter(!is.na(market)) %>%
  unique()

save_dated(markets_for_aaron, "06_Financial_prod/Interim/Markets_for_classification", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Merge in Aaron's analysis and replace redundant markets with new categories

# Clean Aaron's company-product-market allocation data
product_market_allocation_2 %<>%
  select(ISIN_code, company, product_name, market_new)

# Remove financial dataset variables which are no longer required
financial_prod_data %<>%
  select(ISIN_code:revenue, category_no, product_name, product_sales, market_new) %>%
  rename(product_revenue = product_sales) %>%
  left_join(product_market_allocation_2, by = c("ISIN_code", "company", "product_name")) %>%
  mutate(market_new = case_when(company == "TITAN CEMENT CR" ~ "Concrete and cement", #Exception for TITAN CEMENT (Building Mat.& Fix. category is not to be used and this company has no product)
                                is.na(market_new.x) & is.na(market_new.y) ~ market,
                                !is.na(market_new.x) ~ market_new.x,
                                !is.na(market_new.y) ~ market_new.y,
                                TRUE ~ NA_character_)) %>%
  select(-market_new.x , -market_new.y) %>%
  rename(parent_market = market,
         market = market_new)

# Overwrite redundant market categories
financial_prod_data %<>%
  mutate_at(.vars = c("parent_market", "market"),
            .funs = funs(case_when(. %in% c("Iron and steel fabrication", "Iron and steel foundary") ~ "Iron & Steel",
                                   . %in% c("Broadline Retailers", "Food Retail,Wholesale") ~ "Food and broadline retailers",
                                   . %in% c("Clothing & Accessory", "Footwear") ~ "Clothing, footwear and accessories",
                                   . %in% c("Recreational Products", "Recreational Services") ~ "Recreational products and services",
                                   . %in% c("Transport - Air", "Airlines") ~ "Transport - Air",
                                   . %in% c("Transport - Rail", "Railroads") ~ "Transport - Rail",
                                   . %in% c("Transport - Shipping", "Marine Transportation") ~ "Transport - Shipping",
                                   . %in% c("Transport - Trucking", "Trucking") ~ "Transport - Trucking",
                                   . %in% c("Transport - Delivery", "Delivery Services") ~ "Transport - Delivery",
                                   . %in% c("Consumer Electronics", "Electronic Equipment", "Electronics") ~ "Consumer Electronics",
                                   . %in% c("Life insurance", "Life Insurance") ~ "Life Insurance",
                                   . %in% c("Pipelines", "O&G T&D") ~ "O&G T&D",
                                   . %in% c("Other Chemicals", "Commodity Chemicals") ~ "Other Chemicals",
                                   . %in% c("Other Mining", "General Mining") ~ "Other Mining",
                                   . == "Hotel & Lodging REITs" ~ "Diversified REITs",
                                   TRUE ~ .)))

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Exclude product categories that are unrelated to actual business (for instance, concessions)

excluded_categories <- c("Adminstrative", "ADministrative", "Concessions", "Investment", "Other", "Others", "Other Businesses",
                         "Other segments and activities")

financial_prod_data %<>%
  mutate(product_revenue = case_when(product_name %in% excluded_categories ~ NA_character_,
                                     TRUE ~ product_revenue))

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Find market exposure based on product-based analysis at firm-level

product_exposure_results <- financial_prod_data %>%
  filter(!(is.na(product_revenue))) %>%
  mutate(product_revenue = as.numeric(product_revenue),
         # No negative product sales values are allowed
         product_revenue = case_when(product_revenue <= 0 ~ 0,
                                   TRUE ~ product_revenue)) %>%
  group_by(ISIN_code, year) %>%
  mutate(product_revenue_share = product_revenue / sum(product_revenue)) %>%
  group_by(ISIN_code, year, company, market) %>%
  mutate(product_revenue_share = sum(product_revenue_share),
         # Product revenue share will only be zero when the sum over all categories is 0
         category_count = n(),
         product_revenue_share = case_when(is.na(product_revenue_share) ~ 1 / category_count,
                                           TRUE ~ product_revenue_share)) %>%
  select(-category_count) %>%
  ungroup()

modelled_2017 <- product_exposure_results %>%
  filter(year == 2017)

missing_2017 <- financial_prod_data %>%
  filter(year == 2017) %>%
  filter(!(ISIN_code %in% modelled_2017$ISIN_code))

modelled_2016 <- product_exposure_results %>%
  filter(year == 2016)

missing_2016 <- financial_prod_data %>%
  filter(year == 2016) %>% 
  filter(!(ISIN_code %in% modelled_2016$ISIN_code))

no_product_exposure_data <- missing_2017 %>%
  bind_rows(missing_2016)

# Add back in company-year combinations which have no product revenue data
no_product_exposure_data %<>%
  # This changes nothing (all category number rows will be the same)
  filter(category_no == 1) %>%
  mutate(product_revenue_share = 1)

# Combine datasets
product_exposure_results %<>%
  bind_rows(no_product_exposure_data)

# Keep unique entries only after removing original product categories
# (companies may have >1 product which falls inside the same market for our analysis)
product_exposure_results %<>%
  select(-category_no, -product_name, -product_revenue) %>%
  unique() %>%
  # Filter out zero revenue market share combinations (do not impact analysis)
  filter(product_revenue_share != 0)

# Save final list of markets
final_markets <- product_exposure_results %>%
  select(market) %>%
  unique()

save_dated(final_markets, "06_Financial_prod/Output/Final_markets_list", csv = TRUE)

# Save product exposure data for 2016
product_exposure_results_2016 <- product_exposure_results %>%
  filter(year == 2016)

save_dated(product_exposure_results_2016, "06_Financial_prod/Output/Market_exposure_results_2016", csv = TRUE)

# Save product exposure data for model year (2017)
product_exposure_results %<>%
  filter(year == 2017)

save_dated(product_exposure_results, "06_Financial_prod/Output/Market_exposure_results", csv = TRUE)
