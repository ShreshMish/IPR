##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  16/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script reads in TR equity and FI financial data from Excel, and cleans the data
##### Dependencies:       1.  Latest Thomson Reuters financial data Excel file: "2_Financial/Input/TR data consolidated.xlsx"
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "2_Financial/2a_Preliminary"
source("utils.R")

# Read in Thomson Reuters main financial spreadsheet
# NB two manual change has been made to the spreadsheet to remove characters R cannot parse
#    company = CNH INDUSTRIAL, ISIN code = NL0010545661, segment 2 --> Other countries (see comment in cell Y916 for details)
#    company = VERTEX PHARMS., ISIN code = US92532F1003, segment 2 --> Europe (see comment in cell Y2620 for details - was Europe`)
equity_data <- read_excel(input_source("TR_data_consolidated.xlsx"),
                          sheet = "W2. Consolidated TR data", skip = 10)

# Read in Bloomberg credit rating spreadsheet
credit_rating_data <- read_excel(input_source("TR_data_consolidated.xlsx"),
                                 sheet = "W4. Credit rating data",  skip = 6)

# Read in OECD CPI inflation data
us_inflation_data <- read_excel(input_source("OECD_CPI_inflation.xlsx"),
                                             sheet = "R1. OECD CPI inflation", skip = 8, guess_max = 10000)

save_dated(equity_data, "Equity_raw_data", folder = "Interim", csv = FALSE)
save_dated(credit_rating_data, "Credit_rating_raw_data", folder = "Interim", csv = FALSE)

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - TR main financial data cleaning ----

# Rename variables and remove irrelevant variables
equity_data2 <- equity_data %>%
  select(-`WEIGHT IN INDEX`, -Type) %>%
  # Basic variables
  rename(company = NAME,
         equity_isin_code = `EQUITY ISIN CODE`,
         domicile = `GEOGRAPHIC DESCR.`,
         market_cap = `MARKET VALUE`,
         revenue = `NET SALES OR REVENUES`,
         net_income = `NET INCOME - BASIC`,
         corporation_tax_rate = `TAX RATE - 5 YR AVG`) %>%
  # Industry classifications
  rename_at(vars(starts_with("INDUSTRY")), funs(paste0("industry_level_", substr(., nchar(.), nchar(.))))) %>%
  # Geographic segment sales variables
  rename_at(vars(starts_with("GEOGRAPHIC SEGMENT")), funs(paste0("region_revenue_", gsub("\\D+", "", .)))) %>%
  # Geographic segment description variables
  rename_at(vars(starts_with("GEOGRPHC SEGMENT")), funs(paste0("region_name_", gsub("\\D+", "", .)))) %>%
  # Revenue by year
  rename_at(vars(starts_with("NET SALES OR REVENUES")), funs(paste0("revenue_", gsub("\\D+", "", .)))) %>%
  # Product segment sales variables
  rename_at(vars(contains("-SALES")), funs(paste0("product_revenue_", stri_extract_first_regex(., "[0-9]+"), "_", stri_extract_last_regex(., "[0-9]+")))) %>%
  #  Product segment description variables
  rename_at(vars(contains("-DESCRIPTION")), funs(paste0("product_name_", stri_extract_first_regex(., "[0-9]+"), "_", stri_extract_last_regex(., "[0-9]+")))) %>%
  # Fixed income variables
  rename(current_liabilities = `Current liabilities (total)`,
         current_assets = `Current assets (total)`,
         ebit = `EBIT (Earnings before interest and taxes) US$`,
         price_to_book_ratio = `Book value of equity or price-to-book ratio`,
         total_liabilities = `Total liabilities`,
         total_assets = `Total Assets`,
         retained_earnings = `Retained earnings`) %>%
  # Net income profit margin variables
  rename_at(vars(starts_with("NET INCOME - BASIC")),
            funs(paste0("net_income_", gsub("\\D+", "", .))))

save_dated(equity_data2, "Equity_renamed_data", folder = "Interim", csv = FALSE)

# Remove observations with insufficient data for modelling purposes
equity_data3 <- equity_data2 %>%
  # Remove ABSA GROUP (no data from pre-13/07 dataasets)
  filter(company != "ABSA GROUP") %>%
  # Remove companies with no reported latest year revenue (revenue_2017 = 0 for these companies)
  filter(!is.na(revenue))

# Change fixed income variable types from character to numeric (may include some variables where this is not required)
equity_data4 <- equity_data3 %>%
  mutate_at(vars(current_liabilities:retained_earnings),
            funs(as.numeric(.)))

save_dated(equity_data4, "Equity_full_nominal_data", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Bloomberg credit data cleaning ----

credit_rating_data2 <- credit_rating_data %>%
  rename(company = NAME,
         equity_isin_code = `EQUITY ISIN CODE`,
         fi_instrument_code = `INSTRUMENT KEY`,
         fi_isin_code = `FI ISIN CODE`,
         sp_rating = `SP RATING`,
         moody_rating = `MOODY RATING`) %>%
  select(-Type)

save_dated(credit_rating_data2, "Credit_rating_full_data", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Clean US inflation data ----

# Initial cleaning and annual inflation rate calculations
us_inflation_data2 <- us_inflation_data %>%
  # Rename variables and filter out US observations from OECD dataset
  rename(iso_code = LOCATION,
         date = TIME,
         inflation_rate = Value) %>%
  select(iso_code, date, inflation_rate) %>%
  filter(iso_code == "USA") %>%
  mutate(year = as.numeric(substr(date, 1, 4))) %>%
  group_by(iso_code, year) %>%
  # Annual average inflation rate [compared against WB data for 2016 - values are close (1.26% vs. 1.25%)]
  summarise(inflation_rate = mean(inflation_rate),
            count = n())

# Calculate adjustment factors (nominal US$ -> 2016US$)
us_inflation_data3 <- us_inflation_data2 %>%
  mutate(inflation_rate = (inflation_rate / 100) + 1) %>%
  arrange(year) %>%
  mutate(cum_inflation_rate = cumprod(inflation_rate)) %>%
  mutate(price_level = 100 * cum_inflation_rate / cum_inflation_rate[[which(year == 2016)]])

save_dated(us_inflation_data3, "US_price_level_data", folder = "Interim", csv = TRUE)

# Create price level adjustment vector
us_price_level <- us_inflation_data3$price_level
names(us_price_level) <- us_inflation_data3$year

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Adjust TR dataset so all monetary values are in 2016US$ ----

# Rebase currency units to 2016US$ (NB no need to adjust 2016 product revenue values)
equity_data5 <- equity_data4 %>%
  # 2017 variables to 2016US$ (no year in variable name aside from revenue_2017 and product_revenue variables)
  mutate_at(vars(market_cap, revenue, net_income, current_liabilities, current_assets, ebit,
                 total_liabilities, total_assets, retained_earnings),
            funs(. * us_price_level["2016"] / us_price_level["2017"])) %>%
  mutate_at(vars(revenue_2017, contains("region_revenue_")), funs(. * us_price_level["2016"] / us_price_level["2017"])) %>%
  mutate_at(vars(product_revenue_1_2017:product_revenue_10_2017), funs(. * us_price_level["2016"] / us_price_level["2017"])) %>%
  # in year revenues and net income variables
  mutate(revenue_2015 = revenue_2015 * us_price_level["2016"] / us_price_level["2015"],
         revenue_2014 = revenue_2014 * us_price_level["2016"] / us_price_level["2014"],
         revenue_2013 = revenue_2013 * us_price_level["2016"] / us_price_level["2013"],
         net_income_2017 = net_income_2017 * us_price_level["2016"] / us_price_level["2017"],
         net_income_2015 = net_income_2015 * us_price_level["2016"] / us_price_level["2015"],
         net_income_2014 = net_income_2014 * us_price_level["2016"] / us_price_level["2014"],
         net_income_2013 = net_income_2013 * us_price_level["2016"] / us_price_level["2013"])

# Adjust all financial values to be in mn US$ (currently in thousands)
# NB for some reason retained earnings is already in millions - other credit data is not
equity_data6 <- equity_data5 %>%
  mutate_at(vars(revenue, net_income, starts_with("region_revenue_"), starts_with("revenue_201"),
                 starts_with("product_revenue_"), starts_with("net_income_201")),
            funs(case_when(!is.na(.) ~ . / 10^3,
                           TRUE ~ .))) %>%
   mutate_at(vars(current_liabilities, current_assets, ebit, total_liabilities, total_assets),
             funs(case_when(!is.na(.) ~ . / 10^3,
                            TRUE ~ .)))

save_dated(equity_data6, "Equity_full_2016USD_data", folder = "Interim", csv = TRUE)