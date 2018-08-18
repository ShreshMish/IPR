##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  10/08/2018
##### Code author:        Justine Schafer
#####                     Minor edits by Shyamal for new folder structure
##### Description:        This script reads in Rystad oil and gas production and economics data and cleans it in
#####                     preparation for later modelling on fossil fuel demand destruction
##### Dependencies:       1.  Latest Thomson Reuters cleaned dataset: "1 - Financial prelim/Output/TR_cleaned_2016USD_data.xlsx"
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA

packages <- c("tidyverse", "magrittr", "readxl", "here", "stringi")
lapply(packages, require, character.only = TRUE)
source(here::here("utils.R"))
options(error=traceback)

path_to_data <- path_to_data_file("04_Fossil_fuels/Oil_and_gas/Input/Rystad oil and gas.xlsx")

# Read in company gas production data
gas_production_data <- read_excel(path_to_data,
                                  sheet = "Gas production", range = "$A$1:$J$497", col_names = TRUE)

gas_production_data %<>%
  rename(Company = `Company/Country`)

# Read in global gas production data
global_gas <- read_excel(path_to_data,
                         sheet = "Global", range = "$A$25:$I$29", col_names = TRUE) %>%
  mutate(Company="Global",Product="Gas") %>%
  rename(Case = X__1) %>%
  separate(Case, c("Item","Case"), " ", remove=TRUE, extra="merge")

# Read in company liquid production data
liquid_production_data <- read_excel(path_to_data,
                                     sheet = "Liquid production", range = "$A$1:$J$501", col_names = TRUE) %>%
  rename(Company = `Company/Country`)

# Read in global liquid production data
global_liquid <- read_excel(path_to_data,
                            sheet = "Global", range = "$A$18:$I$22", col_names = TRUE) %>%
  mutate(Company="Global",Product="Liquid") %>%
  rename(Case = X__1) %>%
  separate(Case, c("Item","Case"), " ", remove=TRUE, extra="merge")

# Read in economics data
economics_data <- read_excel(path_to_data,
                             sheet = "Economics", range = "$A$4:$Z$996", col_names = TRUE) %>%
  rename(Company = `Company/Country`) %>%
  gather(key = "Case", value = "Value", -(Company:Year)) %>%
  spread(key = "Year", value="Value") %>%      
  separate("Case", c("Product","Item","Case"),"_")

# Read in Rystad-Thomson Reuters company name matches
company_name_lookup <- read_excel(path_to_data,
                                  sheet = "Clean lookup table", range = "$A$3:$E$150", col_names = TRUE) %>%
  mutate(Rystad_Name=gsub(" ","_",Rystad_Name))

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Consolidate gas and liquid datasets, save E&P company names / ISIN codes dataset

# Save E&P company names/ISIN codes dataset for product data processing
company_name_lookup %<>% 
  filter(Type == "Equity") %>%
  filter(!is.na(Rystad_Name)) %>%
  select(ISIN_Code, VE_Name, Rystad_Name) %>%
  rename(ISIN_code = ISIN_Code,
         company = VE_Name) %>%
  arrange(ISIN_code)

save_dated(company_name_lookup, "04_Fossil_fuels/Oil_and_gas/Output/DD_analysis_companies_list", csv = TRUE)

# Gas consolidation
gas_production_data %<>% fill(Company) %>%
  separate("[Data Values]", c("Item","Case"), " ", remove=TRUE, extra="merge") %>%
  mutate("Product"="Gas") %>%
  select(Company,Product,everything()) %>%
  bind_rows(global_gas)

save_dated(gas_production_data, "04_Fossil_fuels/Oil_and_gas/Interim/DD_gas_clean_data")

# Liquids consolidation
liquid_production_data %<>% fill("Company") %>%
  separate("[Data Values]", c("Item","Case"), " ", remove=TRUE, extra="merge") %>%
  mutate("Product"="Liquid") %>%
  select(Company,Product,everything()) %>%
  bind_rows(global_liquid)

save_dated(liquid_production_data, "04_Fossil_fuels/Oil_and_gas/Interim/DD_liquid_clean_data")

#-----------------------------------------------------------------------------------------------------------------------

##### SECTION 3 - Joint data
# defining consistent labels for high, medium, low, and lowlow scenarios in production and economics datasets
# THIS REMOVES UNITS UNTIL THEY ARE CHANGED BELOW. UNITS ARE:
# ECONOMICS DATA; COSTS, FCF AND GOVERNMENT TAKE ARE IN MUSD
# PRODUCTION DATA IS IN BILLION CM (GAS) AND KBBL/D (LIQUID)

production_cases <- tibble(Gas = unique(gas_production_data$Case), Liquid = unique(liquid_production_data$Case)) %>% 
  mutate(Scenario=c("High","Central","Low","Very_Low"))

economics_cases <- tibble(Economics = unique(economics_data$Case)) %>% 
  mutate (Scenario=c("Central","High","Low","Very_Low"))

all_cases <- production_cases %>% left_join(economics_cases) %>%
  select(Scenario,everything()) %>%
  gather(key="Product_Item", value="Case" , -(Scenario)) %>%
  select(Scenario, Case)

# join together production and economics datasets and introduce consistent scenario labels as new column
# this step removes the Case variable that contains units. 

full_data <- bind_rows(gas_production_data,liquid_production_data,economics_data) %>%
  left_join(all_cases, by=NULL) %>%
  separate("Case", c("Case","Units"), "\\(") %>%
  separate ("Units", c("Units","Excess"), "\\)") %>%
  select(Company:Item,Units,Scenario,everything(),-Case,-Excess) %>%
  # DELETE COUNTRY LEVEL DATA AS NOT NEEDED FOR ANALYSIS
  filter(!(Company %in% c("Saudi Arabia","Russia","Iran","Iraq","Venezuela","Nigeria")))

#-----------------------------------------------------------------------------------------------------------------------

##### SECTION 4 - standardising Units and converting to panel
# convert Liquid Production data into kbbl instead of kbbl/d

# convert shape of data to have years in new column and a single value column
full_data_panel <- full_data %>% 
  gather(key="Year",value="Value",-("Company":"Scenario")) %>%
  # Change Item names so they can be used as variables
  mutate(Item=gsub(" ","_",Item))

# keep units data separately to remove in full_data_panel for unit conversion
units_data <- full_data_panel %>% select(Item,Scenario,Units,Product) %>% unique()

full_data_panel %<>%
  mutate(Value=ifelse(Item=="Production" & Product=="Liquid", Value*365, Value)) %>%
  mutate(Units=gsub("\\/d","",Units))

save_dated(full_data_panel, "04_Fossil_fuels/Oil_and_gas/Interim/DD_full_data_panel")

#-----------------------------------------------------------------------------------------------------------------------

##### SECTION 5 - creating dataset for demand destruction analysis
# (creating variables needed for analysis, including:
# Quantity (Firm, Global) - Units: Oil - kbbl; Gas - bcm (These are converted first thing below)
# Price (Firm, Global) - Units: Oil - USD/bbl, Gas - USD/cm
# Unit Cost (Firm) - Units: MUSD

model_data_full <- full_data_panel %>% 
  # convert kbbl > bbl, bcm > cm and MUSD>USD for ease of calculations
  mutate(Value=ifelse(Item=="Production" & Product=="Liquid", Value*10^3, Value)) %>%
  mutate(Value=ifelse(Item=="Production" & Product=="Gas", Value*10^9, Value)) %>%
  mutate(Value=ifelse(Item!="Production", Value*10^6, Value)) %>%
  # change Units column contents accordingly
  mutate(Units=gsub("MUSD","USD",Units)) %>% mutate (Units=gsub("kbbl","bbl",Units)) %>%
  mutate(Units=gsub("Billion cm","cm",Units)) 

# drop the Units column until later (saving it separately for later use)
model_units <- model_data_full %>% select(Units)
model_data_full %<>% select(everything(),-Units) %>% spread(key="Item", value="Value") 

# Develop new columns for analysis
# Global Quantity column
global_data <- model_data_full %>% filter(Company=="Global") %>% 
  select(Product:Year, Production) %>% mutate(Production_G=Production) %>%
  select(everything(),-Production)

model_data_full %<>% filter(Company!="Global") %>% left_join(global_data, by=NULL) %>%
  # Firm variables
  mutate(Production_F=Production,
         Unit_Cost_F=(Costs+Government_Take)/Production_F,
         Price_F=(Costs+Free_Cash_flow+Government_Take)/Production_F) %>%
  select(everything(), -Production) %>%
  # get rid of observations with negative production or zero production
  mutate(Production_F=ifelse(Production_F<=0,NA,Production_F),
         Unit_Cost_F=ifelse(is.na(Production_F),NA,Unit_Cost_F),
         Price_F=ifelse(is.na(Production_F),NA,Price_F),
         # remove spaces from Company names to be able to use them as variables
         Company=gsub(" ","_",Company))

save_dated(model_data_full, "04_Fossil_fuels/Oil_and_gas/Interim/DD_model_data_full")

# Find total firms share of global
firm_totals <- model_data_full %>%   
  # remove two companies from the data with anomaly data: Kinder Morgan and Genting Berhad
  # Cabot production and economics numebrs produce the highest oil prices (6000USD/bbl) which is a clear
  # outlier so remove
  mutate(temp_var=paste0(Company, "_", Product)) %>%
  filter(!Company %in% c("Genting_Berhad", "Cabot_Oil_and_Gas"),
         temp_var!="Kinder_Morgan_Gas") %>%
  group_by(Product, Scenario, Year) %>% select(everything(),-temp_var) %>%
  # ensure only those firms' production values are used for weighting that actually have a price value
  mutate(Production_temp=ifelse(!is.na(Price_F),Production_F,NA),
         Production_all=sum(Production_temp,na.rm = TRUE),
         Production_share_pct=(Production_all/Production_G)*100) %>% 
  select(everything(),-Production_temp)
# Seems data covers around 75% of global production  at the onset.
# In later years, especially in the Very_Low scenario, this drops off significantly to around 20%
# This could be due to unassigned resources discovered in the future or company share falling
# likely the former. Potentially ask Jo 

# Create global price using firm weights
# Note this is an approximation and should really be called Price_all - Price_G is non-computable!!!
# Individual firm's contributions to global price

firm_prices <- firm_totals %>% 
  # remove negative price data from Rystad to avoid affecting Price_G calculation
  mutate(Price_F=ifelse(Price_F<0,NA,Price_F), 
         Price_contribution_F=Production_F/Production_all*Price_F, 
         Market_share_F=Production_F/Production_all,
         Price_G=sum(Price_contribution_F, na.rm=TRUE),
         Price_F=case_when(Company=="Cheniere_Energy" & Product =="Gas" ~ Price_G,
                           TRUE ~ Price_F),
         Production_F=case_when(Company=="Cheniere_Energy" & Product =="Gas" ~
                                  (Costs + Free_Cash_flow + Government_Take)/Price_F,
                                TRUE ~ Production_F),
         Unit_Cost_F=case_when(Company=="Cheniere_Energy" & Product =="Gas" ~
                                 Costs/Production_F, TRUE ~ Unit_Cost_F))

# only keeping five variables: Production (Firm, Global), Price (Firm, Global), Unit Cost (Firm)
model_data_compact <- firm_prices %>%
  select(everything(), -Production_all,-Price_contribution_F,-Production_share_pct,-Market_share_F)

# DELETION OF KINDER MORGAN AND GENTING BERHAD DUE TO ANOMALIES IN COST DATA
# THE BELOW SHOWS THEIR MARKET SHARE IS BELOW 0.01% AT ALL TIMES SO SHOULD NOT PRESENT A PROBLEM.
firm_totals %<>% mutate(Production_share_F=Production_F/Production_G)

save_dated(model_data_compact, "04_Fossil_fuels/Oil_and_gas/Output/DD_model_data_compact", csv = TRUE)

#-----------------------------------------------------------------------------------------------------------------------

##### SECTION 6 - Find the alpha of firm price to global price

alpha_data <- model_data_compact

alpha_fun <- function(unique_ID = NULL) {
  company_name<-str_split(unique_ID, " ")[[1]][1]
  product_name<-str_split(unique_ID, " ")[[1]][2]
  date<-str_split(unique_ID, " ")[[1]][3]

  temp <- alpha_data %>% ungroup() %>%
    filter(Company == company_name, Product == product_name, Year == date) 
  
  results <- lm(temp$Price_F ~ temp$Price_G - 1)
  temp %<>% mutate(alpha=results[[1]])
  
}


unique_ID_set <- alpha_data %>% 
  group_by(Company, Product, Year) %>%
  mutate(Na_Test=ifelse(!is.na(Price_F),1,0),
         Na_Test_2=sum(Na_Test)) %>%
  filter(Na_Test_2>=2) %>%
  select(Company, Product, Year) %>%
  mutate(ID=paste(Company,Product,Year)) %>%
  select(ID) %>%
  unique()

unique_ID_set <- as.list(unique_ID_set$ID)

# This takes long(er) to run so don't re-do unless necessary
alpha_results <- map(unique_ID_set,alpha_fun) %>% bind_rows() 

alpha_results %<>% mutate(alpha_Price_F=alpha*Price_G) %>%
  mutate(alpha_Unit_Cost_F=alpha*Unit_Cost_F) 

save_dated(alpha_results, "04_Fossil_fuels/Oil_and_gas/Interim/DD_alpha_results")

#Test for one item in the below loop, no need to run
Q_F_adjustment <- function(unique_ID = NULL) {
  company_name<-str_split(unique_ID, " ")[[1]][1]
  product_name<-str_split(unique_ID, " ")[[1]][2]
  date<-str_split(unique_ID, " ")[[1]][3]

  temp <- alpha_results %>% filter(Company == company_name, Year == date, Product == product_name) %>%
    select(Company:Year, Production_F) %>% spread(Scenario, Production_F) %>%
    mutate(High = ifelse(High > Central, High, Central),
           Low = ifelse(Low < Central, Low, Central),
           Very_Low = ifelse(Very_Low < Low, Very_Low, Low)) %>%
    gather(Scenario, Production_F_adj, -(Company:Year)) 
}

Prod_adj <- map(unique_ID_set,Q_F_adjustment) %>% bind_rows() 

alpha_results_Prod_adj <- alpha_results %>% left_join(Prod_adj)

save_dated(alpha_results_Prod_adj, "04_Fossil_fuels/Oil_and_gas/Output/DD_alpha_results_production_adjusted", csv = TRUE)
