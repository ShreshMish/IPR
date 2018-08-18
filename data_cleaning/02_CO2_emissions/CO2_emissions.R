##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  12/08/2018
##### Code author:        Shyamal Patel
##### Description:        This script reads in Trucost CO2 emissions data and matches it against the Thomson Reuters
#####                     companies list, incorporating results from TN and SP's ISIN code matching exercise
##### Dependencies:       1.  Latest Trucost CO2 emissions data Excel file: "2 - CO2 emissions/Input/Carbon Emissions Screen_v1.0.xlsx"
#####                         (Older files can be found in the ".../Dated/" folder)
#####                     2.  Results from Financial data prelim cleaning: "1 - Financial prelim" data cleaning: "1 - Financial prelim/Output/tr cleaned 2016usd data.rds"
#####                     3.  Results from missing companies analysis: "2 - CO2 emissions/Input/TR missing companies analysis.xlsx"

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read inand QA

packages <- c("tidyverse", "magrittr", "readxl", "here", "stringi")
lapply(packages, require, character.only = TRUE)
source(here::here("utils.R"))

# Read in Trucost emissions dataset
emissions_raw_data <- read_excel(path_to_data_file("02_CO2_emissions/Input/Carbon Emissions Screen_v1.0.xlsx"), sheet = "Carbon Emissions",
                                 range = "$A$6:$R$27853")

save_dated(emissions_raw_data, "02_CO2_emissions/Interim/Emissions_raw_data")

# Read in latest Thomson Reuters financial dataset (processed in 1 - Financial prelim section)

financial_data <- readRDS(path_to_data_file("01_Financial_prelim/Output/TR_cleaned_2016USD_data.rds"))

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Trucost data cleaning - change variable names

# Units for emissions data are tonnes CO2e (equivalent), emissions intensity is in tonne CO2e / MUSD, revenue is in MUSD
emissions_clean_data <- emissions_raw_data %<>%
  rename(company = Company,
         ISIN_code = ISIN,
         year = Year,
         GICS_sector_name = `GICS Sector Name`,
         GICS_industry_group_name = `GICS Industry Group Name`,
         GICS_industry_name = `GICS Industry Name`,
         GICS_sub_industry = `GICS Sub Industry`,
         GICS_description = `GICS Description`,
         country = Country) %>%
  rename_at(.vars = vars(starts_with("Carbon-")), .funs = funs(paste0("co2_emissions_scope_", stri_extract_first(., regex = "\\d")))) %>%
  rename_at(.vars = vars(starts_with("Carbon Intensity-")), .funs = funs(paste0("co2_emissions_intensity_scope_", stri_extract_first(., regex = "\\d")))) %>%
  rename(disclosure_type = `Carbon Disclosure`,
         trucost_revenue = `Revenue (USD mn)`)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Emissions intensity calculations: fill in carbon intensity for 2017 based on available data

emissions_intensity_2017 <- emissions_clean_data %>%
  select(company:ISIN_code) %>%
  unique() %>%
  mutate(year = 2017)

emissions_clean_data %<>%
  full_join(emissions_intensity_2017) %>%
  group_by(company, ISIN_code) %>%
  fill(ID) %>%
  fill_(fill_cols = c("GICS_sector_name", "GICS_industry_group_name", "GICS_industry_name",
                      "GICS_sub_industry", "GICS_description", "country"))

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Missing TR companies filled in using company name matching exercise results

# Save list of missing companies for analysis (this analysis has already been completed but save files act as checkpoint)
# NB - this does not need to be run anymore as the analysis is complete

financial_data %<>%
  select(company, ISIN_code, starts_with("industry"), revenue, starts_with("revenue_")) %>%
  select(-revenue_2017) %>%
  rename(revenue_2017 = revenue) %>%
  gather(key = "year", value = "revenue", -(company:industry_level_5)) %>%
  mutate(year = as.numeric(stri_extract_all_regex(year, "[0-9]+"))) %>%
  arrange(ISIN_code, year)

missing_companies <- financial_data %>%
  rename(TR_company = company) %>%
  select(TR_company, ISIN_code) %>%
  left_join(emissions_clean_data) %>%
  filter(is.na(ID)) %>%
  select(TR_company, ISIN_code) %>%
  mutate(trucost_ISIN_code = NA_character_,
         trucost_company = NA_character_,
         Scope_1_2017 = NA_character_,
         Scope_2_2017 = NA_character_,
         Scope_3_2017 = NA_character_) %>%
  unique() %>%
  arrange(TR_company)

save_dated(missing_companies, "02_CO2_emissions/Interim/Trucost_missing_companies", csv = TRUE)

# Read in missing companies analysis and merge with Trucost data based on new set of ISIN codes

missing_companies_analysis <- read_excel(path_to_data_file("02_CO2_emissions/Input/TR missing companies analysis.xlsx"),
                                         sheet = "TR missing companies analysis", range = "$A$1:$G$326")

missing_companies_analysis %<>%
  select(ISIN_code, trucost_company:Scope_3_2017)

emissions_clean_data %<>%
  rename(ISIN_code_match = ISIN_code,
         trucost_company = company)

company_emissions_data <- financial_data %>%
  left_join(missing_companies_analysis, by = "ISIN_code") %>%
  mutate(ISIN_code_match = case_when(!is.na(trucost_ISIN_code) ~ trucost_ISIN_code,
                                     TRUE ~ ISIN_code)) %>%
  select(-trucost_company) %>%
  left_join(emissions_clean_data) %>%
  select(company:revenue, Scope_1_2017:Scope_3_2017, year, starts_with("co2_emissions"),
         trucost_company, trucost_revenue, disclosure_type) %>%
  # Filter out no Trucost company data (2017 emissions cannot be estimated for these companies here)
  filter(!is.na(trucost_company) | !(is.na(Scope_1_2017))) %>%
  select(-trucost_company)

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Fill emissions data for companies in 2017, using 2016 emissions

# NB: In instances where latest year 2016 scope 1 + 2 emissions intensity is +-50% of the median intensity,
#     values are assumed to be erroneous, and older data points are used

company_emissions_data %<>%
  group_by(ISIN_code) %>%
  mutate(co2_emissions_intensity_scope_1_2 = co2_emissions_intensity_scope_1 + co2_emissions_intensity_scope_2,
         median_co2_emissions_intensity_scope_1_2 = median(co2_emissions_intensity_scope_1_2, na.rm = TRUE)) %>%
  arrange(ISIN_code, year) %>%
  
  # Lag emissions variables
  mutate_at(.vars = vars(co2_emissions_scope_1:co2_emissions_scope_3), .funs = funs("lag" = lag(., 1))) %>%
  mutate_at(.vars = vars(co2_emissions_scope_1:co2_emissions_scope_3), .funs = funs("lag2" = lag(., 2))) %>%
  mutate_at(.vars = vars(co2_emissions_scope_1:co2_emissions_scope_3), .funs = funs("lag3" = lag(., 3))) %>%
  
  # Create lagged scope 1 + 2 emissions intensity of revenue variables, and test indicator which compares against the median
  mutate(co2_emissions_intensity_scope_1_2_lag = lag(co2_emissions_intensity_scope_1_2, 1),
         co2_emissions_intensity_scope_1_2_lag2 = lag(co2_emissions_intensity_scope_1_2, 2),
         co2_emissions_intensity_scope_1_2_lag3 = lag(co2_emissions_intensity_scope_1_2, 3),
         co2_intensity_test = co2_emissions_intensity_scope_1_2_lag / median_co2_emissions_intensity_scope_1_2,
         co2_intensity_test_lag = co2_emissions_intensity_scope_1_2_lag2 / median_co2_emissions_intensity_scope_1_2,
         co2_intensity_test_lag2 = co2_emissions_intensity_scope_1_2_lag3 / median_co2_emissions_intensity_scope_1_2) %>%
  
  # Fill in 2017 emissions variables based on test against median emissions intensity of revenue (scope 1 + 2)
  mutate(co2_emissions_scope_1 = case_when(year != 2017 ~ co2_emissions_scope_1,
                                           year == 2017 & (co2_intensity_test > 0.5) & (co2_intensity_test < 1.5) ~ co2_emissions_scope_1_lag,
                                           year == 2017 & (co2_intensity_test_lag > 0.5) & (co2_intensity_test_lag < 1.5) ~ co2_emissions_scope_1_lag2,
                                           year == 2017 & (co2_intensity_test_lag2 > 0.5) & (co2_intensity_test_lag2 < 1.5) ~ co2_emissions_scope_1_lag3,
                                           year == 2017 ~ median(co2_emissions_scope_1, na.rm = TRUE),
                                           TRUE ~ NA_real_)) %>%
  mutate(co2_emissions_scope_2 = case_when(year != 2017 ~ co2_emissions_scope_2,
                                           year == 2017 & (co2_intensity_test > 0.5) & (co2_intensity_test < 1.5) ~ co2_emissions_scope_2_lag,
                                           year == 2017 & (co2_intensity_test_lag > 0.5) & (co2_intensity_test_lag < 1.5) ~ co2_emissions_scope_2_lag2,
                                           year == 2017 & (co2_intensity_test_lag2 > 0.5) & (co2_intensity_test_lag2 < 1.5) ~ co2_emissions_scope_2_lag3,
                                           year == 2017 ~ median(co2_emissions_scope_2, na.rm = TRUE),
                                           TRUE ~ NA_real_)) %>%
  mutate(co2_emissions_scope_3 = case_when(year != 2017 ~ co2_emissions_scope_3,
                                           year == 2017 & (co2_intensity_test > 0.5) & (co2_intensity_test < 1.5) ~ co2_emissions_scope_3_lag,
                                           year == 2017 & (co2_intensity_test_lag > 0.5) & (co2_intensity_test_lag < 1.5) ~ co2_emissions_scope_3_lag2,
                                           year == 2017 & (co2_intensity_test_lag2 > 0.5) & (co2_intensity_test_lag < 1.5) ~ co2_emissions_scope_3_lag3,
                                           year == 2017 ~ median(co2_emissions_scope_3, na.rm = TRUE),
                                           TRUE ~ NA_real_)) %>%
  
  # Alter disclosure type to match new approach
  mutate(disclosure_type = case_when(year != 2017 ~ disclosure_type,
                                     year == 2017 & co2_emissions_scope_1 == co2_emissions_scope_1_lag ~ "Equal to 2016 emissions",
                                     year == 2017 & co2_emissions_scope_1 == co2_emissions_scope_1_lag2 ~ "Equal to 2015 emissions",
                                     year == 2017 & co2_emissions_scope_1 == co2_emissions_scope_1_lag3 ~ "Equal to 2014 emissions",
                                     year == 2017 ~ "Equal to median emissions",
                                     TRUE ~ "NA"))

# Fill in researched emissions data for companies omitted from Trucost and analysed manually (Porsche, Aker BP)
company_emissions_data %<>%
  select(company:disclosure_type) %>%
  mutate(co2_emissions_scope_1 = case_when(is.na(co2_emissions_scope_1) & year == 2017 ~ Scope_1_2017,
                                                 TRUE ~ co2_emissions_scope_1),
         co2_emissions_scope_2 = case_when(is.na(co2_emissions_scope_2)  & year == 2017 ~ Scope_2_2017,
                                                 TRUE ~ co2_emissions_scope_2),
         co2_emissions_scope_3 = case_when(is.na(co2_emissions_scope_3) & year == 2017 ~ Scope_3_2017,
                                                 TRUE ~ co2_emissions_scope_3)) %>%
  select(-Scope_1_2017, -Scope_2_2017, -Scope_3_2017)

save_dated(company_emissions_data, "02_CO2_emissions/Interim/Trucost_emissions_data_full", csv = TRUE)

company_emissions_data %<>%
  filter(year == 2017)

save_dated(company_emissions_data, "02_CO2_emissions/Output/Trucost_cleaned_emissions_data", csv = TRUE)