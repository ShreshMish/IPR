##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  04/04/2019
##### Code author:        Shyamal Patel
##### Description:        This script calculates median net income profit margins for each company used in the calculation of 'gamma'
#####                     factors (used to adjust the profile of future 'BAU' cashflows and reduce the impact of low margin companies on results)
##### Dependencies:       1.  Output from Companies_panel.R script (Companies_2016USD_data.rds")
#####                     2.  Spreadsheet of company net income profit margins

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "2_Financial/2d_Net_income_margins"
source("utils.R")

# Read in results from multi-listed companies analysis
unique_companies_data <- readRDS("2_Financial/2a_Preliminary/Output/Companies_2016USD_data.rds")
  
#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean net income margin data ----

net_income_margin_data <- unique_companies_data %>%
  select(company_id, company, revenue, starts_with("revenue_201"), net_income, starts_with("net_income_201"))

median_calc <- function(year) {
  
  new_var <- rlang::sym(paste0("net_income_margin_", year))
  net_income_var <- rlang::sym(paste0("net_income_", year))
  revenue_var <- rlang::sym(paste0("revenue_", year))
  
  temp <- net_income_margin_data %>%
    mutate(!!new_var := case_when(!!net_income_var == 0 | !!revenue_var == 0 ~ NA_real_,
                                  TRUE ~ !!net_income_var / !!revenue_var)) %>%
    select(company_id, company, !!new_var)
  
  return(temp)
}

net_income_margin_data2 <- map(c(2013:2016), median_calc) %>%
  reduce(.f = left_join)

net_income_margin_data3 <- net_income_margin_data %>%
  mutate(net_income_margin_2017 = case_when(net_income == 0 | revenue == 0 ~ NA_real_,
                                            TRUE ~ net_income / revenue)) %>%
  select(company_id, company, net_income_margin_2017) %>%
  left_join(net_income_margin_data2, by = c("company_id", "company")) %>%
  gather(key = "year", value = "net_income_margin", -(company_id:company)) %>%
  mutate(year = as.numeric(gsub("\\D+", "", year))) %>%
  group_by(company_id, company) %>%
  mutate(net_income_margin_med = median(net_income_margin, na.rm = TRUE))

save_dated(net_income_margin_data3, "Net_income_margin_calcs", folder = "Interim", csv = TRUE)

net_income_margin_data4 <- net_income_margin_data3 %>%
  select(company_id, company, net_income_margin_med) %>%
  unique()

save_dated(net_income_margin_data4, "Company_net_income_margins", folder = "Output", csv = TRUE)