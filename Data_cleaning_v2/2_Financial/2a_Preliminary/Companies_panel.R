##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  28/01/2019
##### Code author:        Shyamal Patel
##### Description:        This script reads in TR financial data from Excel, and cleans the data
#####                     Unique companies analysis of MSCI ACWI equitites is also read in from Excel and used to create a company-level dataset 
#####                     for use in later calculations and modelling
##### Dependencies:       1.  Output from Import_and_cleanup.R script (Equity_cleaned_2016USD_data.rds)
#####                     2.  Results from unique companies analysis (by hand analysis based on equal revenues, similarity of company names etc.)
#####                         Hope to improve on this with a more principled / automated approach in next iteration
#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "2_Financial/2a_Preliminary"
source("utils.R")

# Read in results from equity data cleaning
equity_clean_data <- readRDS("2_Financial/2a_Preliminary/Interim/Equity_full_2016USD_data.rds")

# Read in results from FI data cleaning
fi_clean_data <- readRDS("2_Financial/2a_Preliminary/Interim/Credit_rating_full_data.rds")

# Read in results from multi-listed companies analysis
unique_companies_data <- read_excel(input_source("Multi_listed_companies.xlsx"),
                                    skip = 0, guess_max = 5000)

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean unique companies data ----

unique_companies_data2 <- unique_companies_data %>%
  # Company ID = equity ISIN code when company has one listing, and concatenated ISIN codes when company has >1 listing
  rename(company_id = unique_id,
         equity_isin_code = ISIN_code, 
         multi_listing_status = `>1 listing`,
         multi_listing_reason = `Reason for multi listing`,
         comments = Notes)

save_dated(unique_companies_data2, "Unique_companies_clean_data", folder = "Interim", csv = TRUE)

unique_companies_data3 <- unique_companies_data2 %>%
  select(company_id, equity_isin_code, company, multi_listing_status, multi_listing_reason, comments)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Create panel consisting of unique companies ----

# First save full financial dataset containing all equity and FI data
equity_data <- unique_companies_data3 %>%
  left_join(equity_clean_data, by = c("equity_isin_code", "company"))

# Replace company-level market cap with sum of equity ISIN code level values
equity_data2 <- equity_data %>%
  group_by(company_id) %>%
  mutate(company_market_cap = sum(market_cap, na.rm = TRUE))

# Replace company-level fundamental variables with averages across equity IDs
equity_data3 <- equity_data2 %>%
  group_by(company_id) %>%
  mutate_at(vars(revenue, net_income, corporation_tax_rate, contains("revenue_201"),
                 current_liabilities, current_assets, ebit, price_to_book_ratio,
                 total_liabilities, total_assets, retained_earnings),
            funs(mean(., na.rm = TRUE)))

# For regional and product exposure, fill down observations from the first equity ISIN code over company ids (note that this should not
# make any difference as the values should coincide and if they do not, currency differences will drop out of % exposure indices)
equity_data4 <- equity_data3 %>%
  group_by(company_id) %>%
  # Regional exposure
  mutate_at(vars(starts_with("region_revenue_"), starts_with("region_name_")),
            funs(.[[1]])) %>%
  # Product exposure (2016 and 2017)
  mutate_at(vars(starts_with("product_revenue_"), starts_with("product_name_")),
            funs(.[[1]]))

# Keep just one company name
equity_data5 <- equity_data4 %>% 
  group_by(company_id) %>%
  # Company name
  mutate(company = company[[1]]) %>%
  ungroup()

# Regional and product exposure change diagnostics - only affected companies are BHP BILLITON, RIO TINTO, UNILEVER AND RELX
# library(daff)
# differences <- diff_data(equity_data5, equity_data4)
# render_diff(differences)

equity_data6 <- equity_data5 %>%
  select(company_id, equity_isin_code, company, multi_listing_status, multi_listing_reason,
         comments, domicile, market_cap)

# Save equity summary data for use in asset-level impact calculations
save_dated(equity_data6, "Equity_reconciled_2016USD_data", folder = "Output", csv = TRUE)

# Remove redundant variables and keep only unique companies
company_data <- equity_data5 %>%
  select(-market_cap, multi_listing_status, -multi_listing_reason,
         -comments, -domicile, -equity_isin_code) %>%
  unique()

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Add cashflow fundamental variables to FI dataset  ----

# Create ISIN merge code based on reported company ISIN code for multilisted companies
fi_data <- fi_clean_data %>%
  left_join(equity_data5, by = c("equity_isin_code", "company")) %>%
  select(company_id, fi_instrument_code, fi_isin_code, sp_rating, moody_rating,
         revenue, net_income, current_liabilities, current_assets,
         ebit, price_to_book_ratio, total_liabilities, total_assets,
         retained_earnings)

# Save FI summary data for use in asset-level impact calculations
save_dated(fi_data, "FI_reconciled_2016USD_data", folder = "Output", csv = TRUE)
  
# Save unique company-level data for use in data cleaning and modelling work
# keeping only essential variables
company_data2 <- company_data %>%
  select(company_id, company, industry_level_2:industry_level_5, company_market_cap,
         revenue, net_income, corporation_tax_rate, starts_with("region_revenue_"),
         starts_with("region_name_"), starts_with("revenue_201"), starts_with("product_name_"),
         starts_with("product_revenue_"))

save_dated(company_data2, "Companies_2016USD_data", folder = "Output", csv = TRUE)

# Save unique company codes, names and associated ISIN codes list for later use
company_list <- company_data2 %>% 
  select(company_id, company) %>%
  separate(company_id, into = c("equity_isin_code_1", "equity_isin_code_2", "equity_isin_code_3"),
           sep = " ", fill = "right", remove = FALSE)

save_dated(company_list, "Companies_list", folder = "Output", csv = TRUE)
