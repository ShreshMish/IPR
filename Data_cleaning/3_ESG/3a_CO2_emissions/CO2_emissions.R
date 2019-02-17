##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  17/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script reads in Trucost CO2 emissions data and matches it against the Thomson Reuters
#####                     companies list, incorporating results from TN and SP's ISIN code matching exercise
##### Dependencies:       1.  Latest Trucost CO2 emissions data Excel file: "2 - CO2 emissions/Input/Carbon Emissions Screen_v1.0.xlsx"
#####                         (Older files can be found in the ".../Dated/" folder)
#####                     2.  Results from Financial data prelim cleaning: "1 - Financial prelim" data cleaning: "1 - Financial prelim/Output/tr cleaned 2016usd data.rds"
#####                     3.  Results from missing companies analysis: "2 - CO2 emissions/Input/TR missing companies analysis.xlsx"

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "3_ESG/3a_CO2_emissions"
source("utils.R")

# Read in Trucost emissions dataset
emissions_data <- read_excel(input_source("Carbon_Emissions_Screen_v1.0.xlsx"),
                                 sheet = "Carbon Emissions", range = "$A$6:$R$27853")

# Read in latest unique companies list
company_list <- readRDS("2_Financial/2a_Preliminary/Output/Companies_list.rds")

# Read in latest missing companies analysis for Trucost ISIN codes (07/18)
missing_companies_data <- read_excel(input_source("TR_missing_companies_analysis.xlsx"),
                                     sheet = "TR missing companies analysis", range = "$A$1:$G$324")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Trucost data cleaning - change variable names ----

# Units for emissions data are tonnes CO2e (equivalent), emissions intensity is in tonne CO2e / MUSD, revenue is in MUSD
emissions_data2 <- emissions_data %>%
  # Rename variables
  rename(company = Company,
         ISIN_code = ISIN,
         year = Year,
         GICS_sector_name = `GICS Sector Name`,
         GICS_industry_group_name = `GICS Industry Group Name`,
         GICS_industry_name = `GICS Industry Name`,
         GICS_sub_industry = `GICS Sub Industry`,
         GICS_description = `GICS Description`,
         country = Country) %>%
  rename_at(.vars = vars(starts_with("Carbon-")), .funs = funs(paste0("co2_scope_", stri_extract_first(., regex = "\\d")))) %>%
  rename_at(.vars = vars(starts_with("Carbon Intensity-")), .funs = funs(paste0("co2_intensity_scope_", stri_extract_first(., regex = "\\d")))) %>%
  rename(disclosure_type = `Carbon Disclosure`,
         trucost_revenue = `Revenue (USD mn)`) %>%
  # Remove redundant variables
  select(-ID)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Add 2017 (year) row in dataset and fill down names, industry codes and domicile ----

emissions_data3 <- tibble(ISIN_code = unique(emissions_data2$ISIN_code),
                          year = 2017) %>%
  bind_rows(emissions_data2) %>%
  arrange(ISIN_code, year) %>%
  # Fill down categorical variables, grouping by ISIN_code
  group_by(ISIN_code) %>%
  fill_(fill_cols = c("company", "GICS_sector_name", "GICS_industry_group_name", "GICS_industry_name",
                      "GICS_sub_industry", "GICS_description", "country")) %>%
  ungroup()

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Missing companies filled in using company name matching exercise results  ----
#####             Note that this includes some observations that have been filled from public sources
#####             e.g, 'AKER BP'

missing_companies_data2 <- missing_companies_data %>%
  select(ISIN_code:Scope_3_2017) %>%
  rename(missing_merge_ISIN_code = ISIN_code,
         missing_co2_scope_1_2017 = Scope_1_2017,
         missing_co2_scope_2_2017 = Scope_2_2017,
         missing_co2_scope_3_2017 = Scope_3_2017)

emissions_data4 <- emissions_data3 %>%
  rename(co2_merge_ISIN_code = ISIN_code,
         trucost_company = company)

company_emissions_data <- company_list %>%
  mutate(missing_merge_ISIN_code = case_when(equity_isin_code_1 %in% missing_companies_data2$missing_merge_ISIN_code ~ equity_isin_code_1,
                                             equity_isin_code_2 %in% missing_companies_data2$missing_merge_ISIN_code ~ equity_isin_code_2,
                                             equity_isin_code_3 %in% missing_companies_data2$missing_merge_ISIN_code ~ equity_isin_code_3,
                                             TRUE ~ NA_character_)) %>%
  left_join(missing_companies_data2, by = "missing_merge_ISIN_code") %>%
  # Note that when equity_isin_code_1 is not present in the Trucost dataset, the equity would have originally been flagged
  # for missing companies analysis (trucost_ISIN_code would not be NA), so there is no risk from assigning equity_isin_code_1 below
  # provided that emissions are the same for all equities associated with a particular company (which should be the case)
  mutate(co2_merge_ISIN_code = case_when(!is.na(trucost_ISIN_code) ~ trucost_ISIN_code,
                                         TRUE ~ equity_isin_code_1)) %>%
  select(-trucost_company) %>%
  left_join(emissions_data4, by = "co2_merge_ISIN_code")
  
save_dated(company_emissions_data, "Emissions_merge_data", folder = "Interim", csv = TRUE)
  
#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Fill emissions data for companies in 2017, using 2016 emissions ----

# NB: In instances where latest year 2016 scope 1 + 2 emissions intensity is +-50% of the median intensity,
#     values are assumed to be erroneous, and older data points are used
company_emissions_data2 <- company_emissions_data %>%
  select(company_id, company, co2_merge_ISIN_code,  year, co2_scope_1:co2_intensity_scope_3, 
         disclosure_type, missing_co2_scope_1_2017:missing_co2_scope_3_2017) %>%
  rename(trucost_ISIN_code = co2_merge_ISIN_code) %>%
  group_by(company_id) %>%
  mutate(co2_intensity_scope_1_2 = co2_intensity_scope_1 + co2_intensity_scope_2,
         median_co2_intensity_scope_1_2 = median(co2_intensity_scope_1_2, na.rm = TRUE)) %>%
  arrange(company_id, year) %>%
  # Lag emissions variables
  mutate_at(.vars = vars(co2_scope_1:co2_scope_3), .funs = funs("lag" = lag(., 1))) %>%
  mutate_at(.vars = vars(co2_scope_1:co2_scope_3), .funs = funs("lag2" = lag(., 2))) %>%
  mutate_at(.vars = vars(co2_scope_1:co2_scope_3), .funs = funs("lag3" = lag(., 3))) %>%
  
  # Create lagged scope 1 + 2 emissions intensity of revenue variables, and test indicator which compares against the median
  mutate(co2_intensity_scope_1_2_lag = lag(co2_intensity_scope_1_2, 1),
         co2_intensity_scope_1_2_lag2 = lag(co2_intensity_scope_1_2, 2),
         co2_intensity_scope_1_2_lag3 = lag(co2_intensity_scope_1_2, 3),
         co2_intensity_test = co2_intensity_scope_1_2_lag / median_co2_intensity_scope_1_2,
         co2_intensity_test_lag = co2_intensity_scope_1_2_lag2 / median_co2_intensity_scope_1_2,
         co2_intensity_test_lag2 = co2_intensity_scope_1_2_lag3 / median_co2_intensity_scope_1_2) %>%
  
  # Fill in 2017 emissions variables based on test against median emissions intensity of revenue (scope 1 + 2)
  mutate(co2_scope_1 = case_when(year != 2017 ~ co2_scope_1,
                                 year == 2017 & (co2_intensity_test > 0.5) & (co2_intensity_test < 1.5) ~ co2_scope_1_lag,
                                 year == 2017 & (co2_intensity_test_lag > 0.5) & (co2_intensity_test_lag < 1.5) ~ co2_scope_1_lag2,
                                 year == 2017 & (co2_intensity_test_lag2 > 0.5) & (co2_intensity_test_lag2 < 1.5) ~ co2_scope_1_lag3,
                                 year == 2017 ~ median(co2_scope_1, na.rm = TRUE),
                                 TRUE ~ NA_real_)) %>%
  mutate(co2_scope_2 = case_when(year != 2017 ~ co2_scope_2,
                                 year == 2017 & (co2_intensity_test > 0.5) & (co2_intensity_test < 1.5) ~ co2_scope_2_lag,
                                 year == 2017 & (co2_intensity_test_lag > 0.5) & (co2_intensity_test_lag < 1.5) ~ co2_scope_2_lag2,
                                 year == 2017 & (co2_intensity_test_lag2 > 0.5) & (co2_intensity_test_lag2 < 1.5) ~ co2_scope_2_lag3,
                                 year == 2017 ~ median(co2_scope_2, na.rm = TRUE),
                                 TRUE ~ NA_real_)) %>%
  mutate(co2_scope_3 = case_when(year != 2017 ~ co2_scope_3,
                                 year == 2017 & (co2_intensity_test > 0.5) & (co2_intensity_test < 1.5) ~ co2_scope_3_lag,
                                 year == 2017 & (co2_intensity_test_lag > 0.5) & (co2_intensity_test_lag < 1.5) ~ co2_scope_3_lag2,
                                 year == 2017 & (co2_intensity_test_lag2 > 0.5) & (co2_intensity_test_lag < 1.5) ~ co2_scope_3_lag3,
                                 year == 2017 ~ median(co2_scope_3, na.rm = TRUE),
                                 TRUE ~ NA_real_)) %>%
  
  # Alter disclosure type to match new approach
  mutate(disclosure_type = case_when(year != 2017 ~ disclosure_type,
                                     year == 2017 & co2_scope_1 == co2_scope_1_lag ~ "Equal to 2016 emissions",
                                     year == 2017 & co2_scope_1 == co2_scope_1_lag2 ~ "Equal to 2015 emissions",
                                     year == 2017 & co2_scope_1 == co2_scope_1_lag3 ~ "Equal to 2014 emissions",
                                     year == 2017 ~ "Equal to median emissions",
                                     TRUE ~ "NA")) %>%
  ungroup()

# Fill in researched emissions data for companies omitted from Trucost and analysed manually (Porsche, Aker BP)
company_emissions_data3 <- company_emissions_data2 %>%
  mutate(co2_scope_1 = case_when(is.na(co2_scope_1) & year == 2017 ~ missing_co2_scope_1_2017,
                                 TRUE ~ co2_scope_1),
         co2_scope_2 = case_when(is.na(co2_scope_2)  & year == 2017 ~ missing_co2_scope_2_2017,
                                 TRUE ~ co2_scope_2),
         co2_scope_3 = case_when(is.na(co2_scope_3) & year == 2017 ~ missing_co2_scope_3_2017,
                                 TRUE ~ co2_scope_3))

save_dated(company_emissions_data3, "Emissions_data_full", folder = "Interim", csv = TRUE)

company_emissions_data4 <- company_emissions_data3 %>%
  filter(year == 2017) %>%
  select(company_id, company, year, co2_scope_1:co2_scope_3, disclosure_type)

save_dated(company_emissions_data4, "Emissions_data", folder = "Output", csv = TRUE)