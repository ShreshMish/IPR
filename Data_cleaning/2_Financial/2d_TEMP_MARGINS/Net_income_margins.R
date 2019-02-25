##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  19/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script calculates median net income profit margins for each company used in the calculation of 'gamma'
#####                     factors (used to adjust the profile of future 'BAU' cashflows and reduce the impact of low margin companies on results)
##### Dependencies:       1.  Output from Companies_panel.R script (Companies_2016USD_data.rds")
#####                     2.  Spreadsheet of company net income profit margins

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "2_Financial/2d_TEMP_MARGINS"
source("utils.R")

# Read in results from multi-listed companies analysis
unique_companies_data <- readRDS("2_Financial/2a_Preliminary/Output/Companies_2016USD_data.rds")
  
# Read in equity ISIN code - company unique IDs matching
companies_list <- readRDS("2_Financial/2a_Preliminary/Output/Companies_list.rds")

# Read in company-level net income profit margin data
net_income_margin_data <- read_excel(input_source("Profit margin and FI data calculations.xlsx"),
                                     sheet = "Profit margin and FI data", range = "$A$3:$X$2765")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean net income margin data ----

net_income_margin_data2 <- net_income_margin_data %>%
  rename(isin_code = ISIN_code) %>%
  select(isin_code, starts_with("net_income_margin_20"))

net_income_margin_data3 <- companies_list %>%
  left_join(net_income_margin_data2, by = c("equity_isin_code_1" = "isin_code")) %>%
  left_join(net_income_margin_data2, by = c("equity_isin_code_2" = "isin_code")) %>%
  left_join(net_income_margin_data2, by = c("equity_isin_code_3" = "isin_code"))

for(i in 2013:2017) {
  
  margin_var1 <- rlang::sym(paste0("net_income_margin_", i))
  margin_var2 <- rlang::sym(paste0("net_income_margin_", i, ".x"))
  margin_var3 <- rlang::sym(paste0("net_income_margin_", i, ".y"))
  
  net_income_margin_data3 %<>%
    mutate(!!margin_var1 := case_when(!is.na(!!margin_var1) ~ !!margin_var1,
                                      !is.na(!!margin_var2) ~ !!margin_var2,
                                      !is.na(!!margin_var3) ~ !!margin_var3,
                                      TRUE ~ NA_real_))
}

net_income_margin_data4 <- net_income_margin_data3 %>%
  select(company_id:company, net_income_margin_2017:net_income_margin_2013) %>%
  gather(key = "year", value = "net_income_margin", (net_income_margin_2017:net_income_margin_2013)) %>%
  group_by(company_id) %>%
  mutate(year = as.numeric(stri_extract_all_regex(year, "[0-9]+"))) %>%
  arrange(company_id, year) %>%
  mutate(net_income_margin_med = median(net_income_margin, na.rm = TRUE))

save_dated(net_income_margin_data4, "Company_net_income_margins_full", folder = "Interim", csv = TRUE)

net_income_margin_data5 <- net_income_margin_data4 %>%
  select(company_id:company, net_income_margin_med) %>%
  rename(net_income_margin = net_income_margin_med) %>%
  unique()

save_dated(net_income_margin_data5, "Company_net_income_margins", folder = "Output", csv = TRUE)