##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  30/01/2019
##### Code author:        Shyamal Patel
##### Description:        This script reads in TR financial data from Excel, and cleans the data before
#####                     further data cleaning takes place
##### Dependencies:       1.  Latest Thomson Reuters cleaned dataset: "2_Financial/2a_Preliminary/Output/Companies_2016USD_data.rds"
#####                     2.  Latest list of oil and gas analysis companies: "Rystad_oil_and_gas.xlsx"
#####                     3.  Ethan's product-market matching exercise results: "Market_product_classification_EM.xlsx",
#####                     4.  Aaron's company-product-market matching exercise results: "Company_product_classificaiton_AT.xlsx"
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "2_Financial/2c_Product_exposure"
source("utils.R")

# Read in cleaned company-level dataset (2016US$)
company_data <- readRDS("2_Financial/2a_Preliminary/Output/Companies_2016USD_data.rds")

# Read in cleaned company list including isin codes
company_list <- readRDS("2_Financial/2a_Preliminary/Output/Companies_list.rds")

## Read in market-product allocation exercise findings

# Ethan's initial work, supplemented with Shyamal's sweep through of remaining sectors
product_market_allocation_1 <- read_excel(input_source("Market_product_classification_EM.xlsx"),
                                          sheet = "R1. Market product match", range = "$A$9:$D$3565")

# Aaron's PRELIMINARY results through company outliers (full list generated below in Section 3)
# Change range when Aaron does round 2 (w/c 13/08)
product_market_allocation_2 <- read_excel(input_source("Company_product_classification_AT.xlsx"),
                                          sheet = "180808 Company product classifi", range = "$A$1:$P$1946")

# Rystad analysed oil and gas companies (exclude those listed but excluded from demand destruction modelling)
oil_and_gas_data <- read_excel(input_source("Rystad_oil_and_gas.xlsx"),
                                                 sheet = "Clean lookup table", range = "$A$3:$E$150")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Preliminary cleaning and removing unrequired variables from each dataset

company_data2 <- company_data %>%
  select(company_id:corporation_tax_rate, revenue_2017, revenue_2016, contains("product_")) %>%
  left_join(company_list, by = c("company_id", "company")) %>%
  select(company_id, starts_with("equity_isin_code_"), everything())

# Create merge indicators for oil & gas dataset (ISINs included in Rystad dataset, rather than unique company ids)@
oil_and_gas_data2 <- oil_and_gas_data %>%
  filter(Type == "Equity") %>%
  filter(!is.na(Rystad_Name)) %>%
  select(-Type, -OG_Subsidiary, -Rystad_Name) %>%
  rename(oil_merge_isin_code = ISIN_Code,
         company = VE_Name) %>%
  filter(!(company %in% c("CABOT OIL & GAS 'A'", "GENTING")))

company_data3 <- company_data2 %>%
  mutate(oil_merge_isin_code = case_when(equity_isin_code_1 %in% oil_and_gas_data2$oil_merge_isin_code ~ equity_isin_code_1,
                                         equity_isin_code_2 %in% oil_and_gas_data2$oil_merge_isin_code ~ equity_isin_code_2,
                                         equity_isin_code_3 %in% oil_and_gas_data2$oil_merge_isin_code ~ equity_isin_code_3,
                                         TRUE ~ NA_character_))

# Generate market definitions based on existing definitions
company_prod_data <- company_data3 %>%
  # Define markets based on old definitions (TRBC level 5 unless power or O&G)
  mutate(market = case_when(industry_level_4 == "Electricity" ~ "Power generation",
                            industry_level_4 == "Oil & Gas Producers" & oil_merge_isin_code %in% oil_and_gas_data2$oil_merge_isin_code ~ "Exploration and production",
                            industry_level_4 == "Oil & Gas Producers" ~ "Oil and gas other",
                            TRUE ~ industry_level_5)) %>%
  select(-oil_merge_isin_code)

# Clean Ethan's product-market allocation data
product_market_allocation_1_2 <- product_market_allocation_1 %>%
  rename(product = Product,
         market = Market,
         market_new = `Product code 1`,
         comment = `Product code 2`) %>%
  mutate(market_new = case_when(is.na(market_new) ~ market,
                                TRUE ~ market_new))

# Rearrange company financial data to long format
company_prod_data2 <- company_prod_data %>%
  select(company_id:revenue_2016, market, everything()) %>%
  gather(key = "category", value = "product", (product_name_1_2017:product_revenue_10_2016)) %>%
  left_join(product_market_allocation_1_2, by = c("market", "product")) %>%
  select(company_id:industry_level_5, market, starts_with("revenue_"), everything()) %>%
  arrange(company_id) %>%
  mutate(year = as.numeric(stri_extract_last_regex(category, "[0-9]+")),
         temp = gregexpr("_", category))

company_prod_data2$temp <- sapply(company_prod_data2$temp, function(temp) temp[2])

company_prod_data3 <- company_prod_data2 %>%
  mutate(category_meta = substr(category, start = 1, stop = temp - 1)) %>%
  select(company_id:market, starts_with("revenue"), market, category_meta, category, year, product, market_new,
         equity_isin_code_1, equity_isin_code_2, equity_isin_code_3) %>% # Drop comments (from Ethan) and temporary variables
  mutate(category_no = stri_extract_first_regex(category, "[0-9]+")) %>%
  select(-category)

# Company revenue data
company_rev_data <- company_prod_data3 %>% 
  select(company_id, revenue, revenue_2016) %>%
  rename(revenue_2017 = revenue) %>%
  unique() %>%
  gather(key = "year", value = "revenue", (revenue_2017:revenue_2016)) %>%
  mutate(year = as.numeric(gsub("\\D+", "", year))) %>%
  arrange(company_id)

# Product category revenue data
prod_rev_data <- company_prod_data3 %>%
  select(company_id:market, category_meta, year, product, market_new, category_no) %>%
  filter(category_meta == "product_revenue") %>%
  select(-category_meta) %>%
  select(company_id:market, year, category_no, everything()) %>%
  rename(product_sales = product) %>%
  select(company_id, year, category_no, product_sales)

# Product category name data
prod_name_data <- company_prod_data3 %>%
  select(company_id:market, category_meta, year, product, market_new, category_no,
         equity_isin_code_1, equity_isin_code_2, equity_isin_code_3) %>%
  filter(category_meta == "product_name") %>%
  select(-category_meta) %>%
  select(company_id:market, year, category_no, everything()) %>%
  rename(product_name = product)

# Combining datasets again to obtain right format for further product exposure analysis
company_prod_data4 <- prod_name_data %<>%
  left_join(prod_rev_data, by = c("company_id", "year", "category_no")) %>%
  select(company_id, company, starts_with("industry"), market, year, category_no, product_name, product_sales,
         market_new, equity_isin_code_1, equity_isin_code_2, equity_isin_code_3) %>%
  left_join(company_rev_data, by = c("company_id", "year")) %>%
  select(company_id:market, year, revenue, everything())

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Merge in Aaron's analysis and replace redundant markets with new categories

# Clean Aaron's company-product-market allocation data
product_market_allocation_2_2 <- product_market_allocation_2 %>%
  select(ISIN_code, company, product_name, market_new) %>%
  rename(product_merge_isin_code = ISIN_code)
  
# Create merge indicator in company product exposure dataset
company_prod_data5 <- company_prod_data4 %>%
  mutate(product_merge_isin_code = case_when(equity_isin_code_1 %in% product_market_allocation_2_2$product_merge_isin_code ~ equity_isin_code_1,
                                             equity_isin_code_2 %in% product_market_allocation_2_2$product_merge_isin_code ~ equity_isin_code_2,
                                             equity_isin_code_3 %in% product_market_allocation_2_2$product_merge_isin_code ~ equity_isin_code_3,
                                             TRUE ~ NA_character_))

# Remove financial dataset variables which are no longer required
company_prod_data6 <- company_prod_data5 %>%
  rename(product_revenue = product_sales) %>%
  left_join(product_market_allocation_2_2, by = c("product_merge_isin_code", "company", "product_name")) %>%
  mutate(market_new = case_when(company == "TITAN CEMENT CR" ~ "Concrete and cement", #Exception for TITAN CEMENT (Building Mat.& Fix. category is not to be used and this company has no product)
                                company == "JOHNSON CONTROLS INTL." ~ "Other building materials", #Exception for JOHSN CONTROLS INTERNATIONAL (Building Mat.& Gix. cateogyr is not to be used and this company has no product)
                                is.na(market_new.x) & is.na(market_new.y) ~ market,
                                !is.na(market_new.x) ~ market_new.x,
                                !is.na(market_new.y) ~ market_new.y,
                                TRUE ~ NA_character_)) %>%
  select(-market_new.x , -market_new.y, -product_merge_isin_code,
         -equity_isin_code_1, -equity_isin_code_2, -equity_isin_code_3) %>%
  rename(parent_market = market,
         market = market_new)

# Overwrite redundant market categories
company_prod_data7 <- company_prod_data6 %>%
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
                                   . %in% c("Coal", "Coal production", "Coal retail") ~ "Coal", # Coal and coal production categories appear to be exactly the same. Coal retail is just one company
                                   . %in% c("Other coal-related products") ~ "Coking coal", # Other coal-related products (introduced category) is coking coal, coke, coal gases etc. (none of which are subject to stranding for now)
                                   . == "Hotel & Lodging REITs" ~ "Diversified REITs",
                                   TRUE ~ .)))

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Exclude product categories that are unrelated to actual business (for instance, concessions)

excluded_categories <- c("Adminstrative", "ADministrative", "Concessions", "Investment", "Other", "Others", "Other Businesses",
                         "Other segments and activities")

company_prod_data8 <- company_prod_data7 %>%
  mutate(product_revenue = case_when(product_name %in% excluded_categories ~ NA_character_,
                                     TRUE ~ product_revenue))

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Find market exposure based on product-based analysis at firm-level

product_exposure_results <- company_prod_data8 %>%
  filter(!(is.na(product_revenue))) %>%
  mutate(product_revenue = as.numeric(product_revenue),
         # No negative product sales values are allowed
         product_revenue = case_when(product_revenue <= 0 ~ 0,
                                     TRUE ~ product_revenue)) %>%
  group_by(company_id, year) %>%
  mutate(product_revenue_share = product_revenue / sum(product_revenue)) %>%
  group_by(company_id, year, company, market) %>%
  mutate(product_revenue_share = sum(product_revenue_share),
         # Product revenue share will only be zero when the sum over all categories is 0
         category_count = n(),
         product_revenue_share = case_when(is.na(product_revenue_share) ~ 1 / category_count,
                                           TRUE ~ product_revenue_share)) %>%
  select(-category_count) %>%
  ungroup()

modelled_2017 <- product_exposure_results %>%
  filter(year == 2017)

missing_2017 <- company_prod_data8 %>%
  filter(year == 2017) %>%
  filter(!(company_id %in% modelled_2017$company_id))

modelled_2016 <- product_exposure_results %>%
  filter(year == 2016)

missing_2016 <- company_prod_data8 %>%
  filter(year == 2016) %>% 
  filter(!(company_id %in% modelled_2016$company_id))

no_product_exposure_data <- missing_2017 %>%
  bind_rows(missing_2016)

# Add back in company-year combinations which have no product revenue data
no_product_exposure_data2 <- no_product_exposure_data %>%
  # This changes nothing (all category number rows will be the same)
  filter(category_no == 1) %>%
  mutate(product_revenue_share = 1)

# Combine datasets
product_exposure_results2 <- product_exposure_results %>%
  bind_rows(no_product_exposure_data2)

# Keep unique entries only after removing original product categories
# (companies may have >1 product which falls inside the same market for our analysis)
product_exposure_results3 <- product_exposure_results2 %>%
  select(-category_no, -product_name, -product_revenue) %>%
  unique() %>%
  # Filter out zero revenue market share combinations (do not impact analysis)
  filter(product_revenue_share != 0)

# Save final list of markets - one missing market added (Coking coal - carved out in section 8 - Fossil fuel prod)
final_markets <- product_exposure_results3 %>%
  select(market) %>%
  unique()

green_markets <- tibble(market = c("GR_biofuels", "GR_CCS", "GR_hydro", "GR_EVs", "GR_minerals", "GR_solar", "GR_wind"))

final_markets %<>%
  bind_rows(green_markets) %>%
  arrange(market)

save_dated(final_markets, "Prod_markets_list", folder = "Output", csv = TRUE)

# Save product exposure data for 2016
product_exposure_results_2016 <- product_exposure_results3 %>%
  filter(year == 2016)

save_dated(product_exposure_results_2016, "Prod_exposure_results_2016", folder = "Output", csv = TRUE)

# Save product exposure data for model year (2017)
product_exposure_results_2017 <- product_exposure_results3 %>%
  filter(year == 2017)

save_dated(product_exposure_results_2017, "Prod_exposure_results", folder = "Output", csv = TRUE)
