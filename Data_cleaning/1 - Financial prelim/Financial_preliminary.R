##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  09/08/2018
##### Code author:        Shyamal Patel
##### Description:        This script reads in TR financial data from Excel, and cleans the data before
#####                     further data cleaning takes place
##### Dependencies:       1.  Latest Thomson Reuters financial data Excel file: "1 - Financial prelim/Input/TR data consolidated.xlsx"
#####                         Older files can be found in the ".../Dated/" folder

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
zero_counter <- function(x) {sum(ifelse(x == 0, 1, 0))}

# These functions save base, and dated files in the Interim or Output folder for later use
# Dated files are kept for version control purposes
save_dated <- function(data, name, folder, dated = "YES", csv = "NO") {
  main_path <- paste0("1 - Financial prelim/", folder)
  dated_path <- paste0("1 - Financial prelim/", folder, "/Dated/")
  
  # Save main file
  saveRDS(data, here(main_path, paste0(name, ".rds")))
  # Save dated file (optional)
  if(dated == "YES") {saveRDS(data, here(dated_path, paste0(date, "_", name, ".rds")))}
  # Save dated CSV file (optional)
  if(csv == "YES") {write_csv(data, here(dated_path, paste0(date, "_", name, ".csv")))}
}

# Read in Thomson Reuters spreadsheet dataset
# NB two manual change has been made to the spreadsheet to remove characters R cannot parse
#    company = CHN INDUSTRIAL, ISIN code = NL0010545661, segment 2 --> Other countries (see comment in cell Y916 for details)
#    company = VERTEX PHARMS., ISIN code = US92532F1003, segment 2 --> Europe (see comment in cell Y2620 for details - was Europe`)
tr_raw_data <- read_excel(here("1 - Financial prelim/Input/TR data consolidated.xlsx"), sheet = "W2. Consolidated TR data",
                          range = "$A$11:$BZ$2791")

save_dated(tr_raw_data, "TR_raw_data", folder = "Interim", csv = "YES")

# Read in OECD CPI inflation data
us_inflation_data <- read_excel("1 - Financial prelim/Input/OECD CPI inflation.xlsx", sheet = "R1. OECD CPI inflation", range = "$A$9:$G$4408")

save_dated(us_inflation_data, "US_inflation_data", folder = "Interim", csv = "YES")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - TR data cleaning - change variable names

tr_cleaned_data <- tr_raw_data %>%
  # Remove ABSA GROUP (no data from pre-13/07 datasets)
  filter(!(NAME == "ABSA GROUP")) %>%
  # Remove companies with no reported revenue
  filter(!is.na(`NET SALES OR REVENUES`))

# Rename variables and remove irrelevant variables for geographic exposure analysis
tr_cleaned_data %<>%
  select(-`WEIGHT IN INDEX`) %>%
  rename(ticker = Type,
         company = NAME,
         ISIN_code = `ISIN CODE`,
         country_of_listing = `GEOGRAPHIC DESCR.`,
         market_cap = `MARKET VALUE`,
         revenue = `NET SALES OR REVENUES`,
         profit = `NET INCOME - BASIC`,
         corporation_tax_rate = `TAX RATE - 5 YR AVG`) %>%
  rename_at(.vars = vars(starts_with("INDUSTRY")),
            .funs = funs(paste0("industry_level_", stri_extract_last_regex(., "[0-9]+")))) %>%
  # Geographic segment sales variables
  rename_at(.vars = vars(starts_with("GEOGRAPHIC SEGMENT")), .funs = funs(paste0("region_revenue_", gsub("\\D+", "", .)))) %>%
  # Geographic segment description variables
  rename_at(.vars = vars(starts_with("GEOGRPHC SEGMENT")), .funs = funs(paste0("region_name_", gsub("\\D+", "", .)))) %>%
  # Revenue by year
  rename_at(.vars = vars(starts_with("NET SALES OR REVENUES ")),
            .funs = funs(paste0("revenue_", stri_extract_last_regex(., "[0-9]+")))) %>%
  # Revenue by product category, by year
  rename_at(.vars = vars(contains("-DESCRIPTION")),
            .funs = funs(paste0("product_name_", stri_extract_first_regex(., "[0-9]+"), "_", stri_extract_last_regex(., "[0-9]+")))) %>%
  # Revenue by product revenue, by year
  rename_at(.vars = vars(contains("-SALES")),
            .funs = funs(paste0("product_revenue_", stri_extract_first_regex(., "[0-9]+"), "_", stri_extract_last_regex(., "[0-9]+"))))

save_dated(tr_cleaned_data, "TR_cleaned_nominal_data", folder = "Interim", csv = "YES")

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Adjust TR dataset so all monetary values are in 2016US$

# Clean US inflation data
us_inflation_data %<>%
  rename(iso_code = LOCATION,
         date = TIME,
         inflation_rate = Value) %>%
  select(iso_code, date, inflation_rate) %>%
  filter(iso_code == "USA") %>%
  mutate(year = as.numeric(substr(date, 1, 4))) %>%
  group_by(iso_code, year) %>%
  #Annual average inflation rate
  summarise(inflation_rate = mean(inflation_rate),
            count = n())

# 2016 value from the above approach is close to WB data (1.26% vs. 1.25%) so approach is close enough
us_inflation_rates <- us_inflation_data$inflation_rate
names(us_inflation_rates) <- us_inflation_data$year
us_inflation_rates <- us_inflation_rates / 100 + 1

us_price_level <- tibble(year = us_inflation_data$year,
                              # 1 + inflation rate
                              inflation_rate = 1 + us_inflation_data$inflation_rate / 100)

us_price_level %<>%
  mutate(cum_inflation_rate = cumprod(inflation_rate),
         price_level = 100 * cum_inflation_rate / cum_inflation_rate[year == 2016])

save_dated(us_price_level, "US_price_level_data", folder = "Interim", csv = "NO")

us_price_level_vec <- us_price_level$price_level
names(us_price_level_vec) <- us_price_level$year

# Inflation adjust to 2016US$ (only needs to be applied to non-2016 monetary values)
tr_cleaned_2016usd_data <- tr_cleaned_data %>%
  # 2017 variables to 2016US$ (no year in variable name)
  mutate_at(.vars = vars(market_cap, revenue, profit), .funs = funs(. * us_price_level_vec["2016"] / us_price_level_vec["2017"])) %>%
  mutate_at(.vars = vars(revenue_2017, contains("region_revenue_"), product_revenue_1_2017:product_revenue_10_2017),
            .funs = funs(. * us_price_level_vec["2016"] / us_price_level_vec["2017"])) %>%
  mutate(revenue_2015 = revenue_2015 * us_price_level_vec["2016"] / us_price_level_vec["2015"],
         revenue_2014 = revenue_2014 * us_price_level_vec["2016"] / us_price_level_vec["2014"],
         revenue_2013 = revenue_2013 * us_price_level_vec["2016"] / us_price_level_vec["2013"])
  
save_dated(tr_cleaned_2016usd_data, "TR_cleaned_2016USD_data", folder = "Output", csv = "YES")
