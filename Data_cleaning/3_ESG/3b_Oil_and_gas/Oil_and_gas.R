##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  06/06/2019
##### Code author:        Justine Schafer
##### Edited by:          Shyamal Patel (new file / folder structure x2)
##### Description:        This script reads in Rystad oil and gas production and economics data and cleans it in
#####                     preparation for later modelling on fossil fuel demand destruction
##### Dependencies:       1.  Rystad oil and gas data spreadsheet
#####                     2.  List of model companies from financial data analysis
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "3_ESG/3b_Oil_and_gas"
source("utils.R")

# Read in company gas production data
company_gas_data <- read_excel(input_source("Rystad_oil_and_gas.xlsx"),
                               sheet = "Gas production", range = "$A$1:$J$497")

# Read in global gas production data
global_gas_data <- read_excel(input_source("Rystad_oil_and_gas.xlsx"),
                              sheet = "Global", range = "$A$25:$I$29")

# Read in company liquid production data
company_liquid_data <- read_excel(input_source("Rystad_oil_and_gas.xlsx"),
                                  sheet = "Liquid production", range = "$A$1:$J$501")

# Read in global liquid production data
global_liquid_data <- read_excel(input_source("Rystad_oil_and_gas.xlsx"),
                                 sheet = "Global", range = "$A$18:$I$22")

# Read in economics data
economics_data <- read_excel(input_source("Rystad_oil_and_gas.xlsx"),
                             sheet = "Economics", range = "$A$4:$Z$996")

# Read in Rystad-Thomson Reuters company name matches
oil_and_gas_company_names <- read_excel(input_source("Rystad_oil_and_gas.xlsx"),
                                        sheet = "Clean lookup table", range = "$A$3:$E$150")

# Read in company unique names list
company_names <- readRDS("2_Financial/2a_Preliminary/Output/Companies_list.rds")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Create E&P company names dataframe for later use ----

# Save E&P company names/ISIN codes dataset for product data processing
oil_and_gas_companies <- oil_and_gas_company_names %>% 
  filter(Type == "Equity" & (!is.na(Rystad_Name))) %>%
  select(ISIN_Code, Rystad_Name) %>%
  rename(finance_merge_isin_code = ISIN_Code,
         rystad_name = Rystad_Name)

company_names2 <- company_names %>%
  mutate(finance_merge_isin_code = case_when(equity_isin_code_1 %in% oil_and_gas_companies$finance_merge_isin_code ~ equity_isin_code_1,
                                             equity_isin_code_2 %in% oil_and_gas_companies$finance_merge_isin_code ~ equity_isin_code_2,
                                             equity_isin_code_3 %in% oil_and_gas_companies$finance_merge_isin_code ~ equity_isin_code_3,
                                             TRUE ~ NA_character_))

oil_and_gas_companies2 <- company_names2 %>%
  left_join(oil_and_gas_companies, by = "finance_merge_isin_code") %>%
  select(company_id, company, rystad_name) %>%
  filter(!is.na(rystad_name)) %>%
  arrange(company_id)

# Save matching of NZT company names to Rystad company names (used for results mapping later)
# Note that Rystad_name is unique in the Rystad data (each company appears only once in each dataset)
save_dated(oil_and_gas_companies2, "Companies_list", folder = "Output", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Clean gas, liquid production and economics datasets ----

### Gas production data cleaning
# Company-level gas production
company_gas_data2 <- company_gas_data %>%
  rename(rystad_name = `Company/Country`) %>%
  # Have to fill gaps in rystad names (no additional identifiers but spreadsheet has consistent format)
  fill(rystad_name) %>%
  select(rystad_name, everything()) %>%
  arrange(rystad_name) %>%
  separate(`[Data Values]`, into = c("item", "scenario"), sep = " ", extra = "merge") %>%
  mutate(scenario = case_when(!grepl(" Case", scenario) ~ "Central",
                              TRUE ~ substring(scenario, 1, as.numeric(gregexpr("Case", scenario)) - 2))) %>%
  mutate(units = "Billion cm") %>%
  mutate(product = "Gas") %>%
  select(rystad_name, product, scenario, item, units, everything())

# Global gas production
global_gas_data2 <- global_gas_data %>%
  separate(`...1`, into = c("item", "scenario"), sep = " ", extra = "merge") %>%
  mutate(scenario = case_when(!grepl(" Case", scenario) ~ "Central",
                              TRUE ~ substring(scenario, 1, as.numeric(gregexpr("Case", scenario)) - 2))) %>%
  mutate(rystad_name = "Global",
         units = "Billion cm",
         product = "Gas")

# Bind together gas data
company_gas_data3 <- company_gas_data2 %>%
  bind_rows(global_gas_data2)

save_dated(company_gas_data3, "Gas_production_data", folder = "Interim", csv = TRUE)

### Liquids production data cleaning
# Company-level liquid production
company_liquid_data2 <- company_liquid_data %>%
  rename(rystad_name = `Company/Country`) %>%
  # Have to fill gaps in rystad names (no additional identifiers but spreadsheet has consistent format)
  fill(rystad_name) %>%
  select(rystad_name, everything()) %>%
  arrange(rystad_name) %>%
  separate(`[Data Values]`, into = c("item", "scenario"), sep = " ", extra = "merge") %>%
  mutate(scenario = case_when(!grepl(" Case", scenario) ~ "Central",
                              TRUE ~ substring(scenario, 1, as.numeric(gregexpr("Case", scenario)) - 2))) %>%
  mutate(units = "kbbl/d") %>%
  mutate(product = "Liquid") %>%
  select(rystad_name, product, scenario, item, units, everything())

# Global liquid production
global_liquid_data2 <- global_liquid_data %>%
  separate(X__1, into = c("item", "scenario"), sep = " ", extra = "merge") %>%
  mutate(scenario = case_when(!grepl(" Case", scenario) ~ "Central",
                              TRUE ~ substring(scenario, 1, as.numeric(gregexpr("Case", scenario)) - 2))) %>%
  mutate(rystad_name = "Global",
         units = "kbbl/d",
         product = "Liquid")

# Bind together liquid data
company_liquid_data3 <- company_liquid_data2 %>%
  bind_rows(global_liquid_data2)

save_dated(company_liquid_data3, "Liquid_production_data", folder = "Interim", csv = TRUE)

### Economics data cleaning
economics_data2 <- economics_data %>%
  rename(rystad_name = `Company/Country`) %>%
  select(rystad_name, everything()) %>%
  arrange(rystad_name) %>%
  gather(key = "item", value = "value", -(rystad_name:Year)) %>%
  spread(key = "Year", value = "value") %>%
  mutate(product = substring(item, 1, stri_locate_first_regex(item, "_")[, 1] - 1)) %>%
  mutate(scenario = case_when(!grepl(" Case", item) ~ "Central",
                              TRUE ~ substring(item, stri_locate_first_regex(item, "Economics ")[, 2] + 1,
                                               as.numeric(gregexpr("Case", item)) - 2))) %>%
  mutate(item = substring(item, stri_locate_first_regex(item, "_")[, 2] + 1, stri_locate_last_regex(item, "_")[, 1] - 1)) %>%
  mutate(units = "MUSD")

save_dated(economics_data2, "Economics_data", folder = "Interim", csv = TRUE)

#-----------------------------------------------------------------------------------------------------------------------

##### SECTION 4 - Join together the three datasets and cleanup ----

company_data <- company_gas_data3 %>%
  bind_rows(company_liquid_data3) %>%
  bind_rows(economics_data2) %>%
  select(rystad_name, product, item, units, scenario, `2016`:`2050`) %>%
  mutate(scenario = case_when(scenario == "Low Low" ~ "Very_Low",
                              TRUE ~ scenario)) %>%
  filter(!(rystad_name %in% c("Saudi Arabia","Russia","Iran","Iraq","Venezuela","Nigeria")))

company_data2 <- company_data %>%
  gather(key = "year", value = "value", (`2016`:`2050`)) %>%
  mutate(item = gsub(" ", "_", item)) %>%
  # Change units: kbbl/d -> bbl, bcm -> cm, MUSD -> USD
  mutate(value = case_when(item == "Production" & product == "Liquid" ~ value * 365 * 10^3,
                           item == "Production" & product == "Gas" ~ value * 10^9,
                           item != "Production" ~ value * 10^6)) %>%
  mutate(units = case_when(item == "Production" & product == "Liquid" ~ "bbl",
                           item == "Production" & product == "Gas" ~ "cm",
                           item != "Production" ~ "USD"))

# Reshape and save units for later
units_data <- company_data2 %>%
  select(product, item, units) %>%
  unique()

company_data3 <- company_data2 %>%
  select(-units) %>%
  mutate(item = tolower(item)) %>%
  spread(key = "item", value = "value")

# Add global production as a separate column
company_data4 <- company_data3 %>%
  group_by(product, scenario, year) %>%
  mutate(production_g = production[[which(rystad_name == "Global")]]) %>%
  filter(rystad_name != "Global") %>%
  ungroup()

# Rename variables and create unit costs and firm prices
company_data5 <- company_data4 %>%
  rename(production_f = production) %>%
  mutate(unit_cost_f = (costs + government_take) / production_f,
         price_f = (costs + free_cash_flow + government_take) / production_f) %>%
  # Remove negative / zero production observations
  mutate_at(vars(production_f, unit_cost_f, price_f), funs(ifelse(production_f <= 0, NA_real_, .)))

save_dated(company_data5, "Full_prod_and_economic_data", folder = "Interim", csv = TRUE)

#-----------------------------------------------------------------------------------------------------------------------

##### SECTION 5 - Calculate included companies share of global production ----

company_data6 <- company_data5 %>%
  group_by(product, scenario, year) %>%
  # Remove outliers which have anomalous data: Kinder Morgan (gas ONLY), Genting Berhad, Cabot
  # production and economics produce the highest oil prices (6000USD/bbl)
  filter(!(rystad_name %in% c("Genting Berhad", "Cabot Oil and Gas")) & !(rystad_name == "Kinder Morgan" & product == "Gas")) %>%
  mutate(production_all = sum(production_f, na.rm = TRUE),
         production_share_pct = production_all / production_g * 100) %>%
  # Company data covers around 75% of global production
  ungroup()

#-----------------------------------------------------------------------------------------------------------------------

##### SECTION 6 - Calculate global price based on  firm weights
company_data7 <- company_data6 %>%
  group_by(product, scenario, year) %>%
  # Replace firm prices with 0 if they are negative
  mutate(price_f = ifelse(price_f < 0, NA_real_, price_f),
         price_g = sum(price_f * (production_f / production_all), na.rm = TRUE)) %>%
  # Ad hoc changes to price / unit cost data for Cheniere Energy - gas
  mutate(price_f = ifelse(rystad_name == "Cheniere Energy" & product == "Gas", price_g, price_f),
         production_f = ifelse(rystad_name == "Cheniere Energy" & product == "Gas", (costs + free_cash_flow + government_take) / price_f, production_f),
         unit_cost_f = ifelse(rystad_name == "Cheniere Energy" & product == "Gas", costs / production_f, unit_cost_f)) %>%
  ungroup() %>%
  select(rystad_name, product, scenario, year, production_f, production_g, price_f, price_g, unit_cost_f)
  
#-----------------------------------------------------------------------------------------------------------------------

##### SECTION 7 - Calculate alpha adjustment factors (price_g -> price_f ratio based on linear regression w/o constant)
company_data8 <- company_data7 %>%
  mutate(alpha_id = paste(rystad_name, product, year))

company_product_year_list <- company_data8 %>%
  group_by(alpha_id) %>% 
  nest() %>%
  select(data) %>%
  unlist(recursive = FALSE)

alpha_factors <- function(temp_alpha_tibble = NULL) {
  
  # Alpha factor is only calculable when there are more than 2 observations to use for interpolation
  if(sum(!is.na(temp_alpha_tibble$price_f)) > 1) {
    alpha_factor <- lm(temp_alpha_tibble$price_f ~ temp_alpha_tibble$price_g - 1)
    temp_alpha_tibble %<>%
      mutate(alpha = alpha_factor[[1]])
  }
  
  return(temp_alpha_tibble)
}  

company_data9 <- map(company_product_year_list, alpha_factors) %>%
  bind_rows() %>%
  mutate(alpha_price_f = alpha * price_g,
         alpha_unit_cost_f = 1 * unit_cost_f) %>%
  ungroup() %>%
  # Drop NA value rows to match Justine's code
  filter(!is.na(alpha))

#-----------------------------------------------------------------------------------------------------------------------

##### SECTION 8 - Adjust proudction quantities to be in increasing order across scenarios
company_data10 <- company_data9 %>%
  group_by(rystad_name, product, year) %>%
  # Replace high scenario values with central when central > high, replace low values with central when low > central
  mutate(production_f_adj = case_when(scenario == "High" & production_f <= production_f[[which(scenario == "Central")]] ~ production_f[[which(scenario == "Central")]],
                                      scenario == "Low" & production_f >= production_f[[which(scenario == "Central")]] ~ production_f[[which(scenario == "Central")]],
                                      TRUE ~ production_f)) %>%
  # Replace very low scenario values with low whenever Very low >= Low
  mutate(production_f_adj = ifelse(scenario == "Very_Low" & production_f_adj >= production_f_adj[[which(scenario == "Low")]],
                                   production_f_adj[[which(scenario == "Low")]],
                                   production_f_adj)) %>%
  ungroup()

save_dated(company_data10, "Model_prod_and_economic_data", folder = "Output", csv = TRUE)