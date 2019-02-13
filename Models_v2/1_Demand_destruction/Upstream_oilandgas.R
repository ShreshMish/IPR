##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  14/01/2019
##### Code author:        Justine Schafer
##### Edited by:          Shyamal Patel (new file / folder structure - add definition of discount_rate to Section 1)
##### Description:        This script imports cleaned Rystad data and scenario analysis data for demand destruction analysis
##### Dependencies:       1.  Results from oil and gas data cleaning (see "0 - Data cleaning/4 - Fossil fuels/Oil and gas/Output/")
#####                     2.  Oil and gas scenario data
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
save_dated <- function(data, name, folder, dated = TRUE, csv = FALSE) {
  main_path <- paste0("1 - Demand destruction/", folder)
  dated_path <- paste0("1 - Demand destruction/", folder, "/Dated/")
  
  # Save main file
  saveRDS(data, here(main_path, paste0(name, ".rds")))
  # Save dated file (optional)
  if(dated == TRUE) {saveRDS(data, here(dated_path, paste0(date, "_", name, ".rds")))}
  # Save dated CSV file (optional)
  if(csv == TRUE) {write_csv(data, here(dated_path, paste0(date, "_", name, ".csv")))}
}

# there is also a model data full which includes some more raw data columns not needed for this analysis
model_data_compact <- readRDS("1 - Demand destruction/Input/DD_alpha_results_production_adjusted.rds")

# Read in oil and gas scenario data
# VE_scenario_data <- read_excel("1 - Demand destruction/Input/Oil and gas demand scenarios.xlsx",
#                                sheet = "W1. TIAM cleaned", range = "$A$57:$E$169", col_names = TRUE)
VE_scenario_data <- readRDS("1 - Demand destruction/Input/Fossil_fuel_production.rds")

# Rystad company names matched against model company names
company_name_lookup <- read_excel("1 - Demand destruction/Input/Rystad oil and gas.xlsx",
                                  sheet = "Clean lookup table", range = "$A$3:$E$150", col_names = TRUE) %>%
  mutate(Rystad_Name=gsub(" ","_",Rystad_Name))

# Discount rate should be 0.0575 (2% inflation rate adjustment does not apply)
discount_rate <- 0.0575

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Modelling

model_data_compact %<>% mutate(Price_F=alpha_Price_F) %>% select(everything(), -alpha_Price_F,
                                                                 -alpha) %>%
  mutate(Unit_Cost_F=alpha_Unit_Cost_F) %>% select(everything(), -alpha_Unit_Cost_F) %>%
  mutate(Production_F=Production_F_adj) %>% select(everything(), -Production_F_adj)

# Functions for graphing Rystad data --------------------------------------
# Note that when running these functions you have to run the first part of the code only because of later
# variable redefinitions. They also might display some trends removed from the data later, for example
# Production that falls from lower to higher scenario in Rystad data.
# for stranding graphs, see end of code.
# 1. Function for generating global margin graphs
global_margin <- model_data_compact %>% 
  select(everything(),-Production_F,-Price_F,-Unit_Cost_F,-Company) %>%
  unique() 

# margin_curve_G <- function(product_name = NULL) {
#   margin_curve_G_data <- global_margin %>%
#     filter(Product == !!product_name)
#   
#   graph_title <- paste("Global margin curve", product_name)
#   
#   windows()
#   ggplot(margin_curve_G_data, mapping= aes(x=Production_G, y=Price_G, color=Year)) +
#     geom_point(aes(shape=Scenario)) + geom_line() +
#     ggtitle(graph_title) +
#     theme_vivid() + scale_colour_vivid_house2()
# }
# 
# # 2. Function for identifying firm quantity (y) from global quantity (x) graphs
# quantity_mapping <- function(company_name = NULL, product_name = NULL) {
#   quantity_mapping_data <- model_data_compact %>%
#     filter(Company == !!company_name, Product == !!product_name)
#   
#   graph_title <- paste(company_name, product_name, "quantity v global quantity")
#   windows()  
#   ggplot(quantity_mapping_data, mapping= aes(x=Production_G, y=Production_F, color=Year)) +
#     geom_point(aes(shape=Scenario)) + geom_line() +
#     ggtitle(graph_title) +
#     theme_vivid() + scale_colour_vivid_house2()
# }
# 
# # 3. Function for identifying firm unit cost (y) from firm quantity (x) graphs
# # Note that some of these have unexpected shapes (e.g. outliers in the low or lowlow scenarios) 
# # but since they are based entirely on Rystad data likely still best to use these rather than own assumptions
# unit_cost_mapping_F <- function(company_name = NULL, product_name = NULL) {
#   unit_cost_mapping_data <- model_data_compact %>%
#     filter(Company == !!company_name, Product == !!product_name)
#   
#   graph_title <- paste(company_name, "Unit Cost curve", product_name)
#   windows()
#   ggplot(unit_cost_mapping_data, mapping= aes(x=Production_F, y=Unit_Cost_F, color=Year)) +
#     geom_point(aes(shape=Scenario)) + geom_line() +
#     ggtitle(graph_title) +
#     theme_vivid() + scale_colour_vivid_house2()
# }
# 
# # 4. Function for graphing firm price spreads around global price
# price_spread <- function(product_name = NULL) {
#   price_spread_data <- model_data_compact %>%
#     filter(Product == !!product_name)
#   
#   graph_title <- paste("Firm price spread", product_name)
#   windows()
#   ggplot(price_spread_data, mapping = aes(x=Year)) +
#     geom_point(aes(y=Price_F, shape=Scenario))+
#     geom_point(aes(y=Price_G, colour=Scenario)) +
#     ggtitle(graph_title) +
#     theme_vivid() + scale_colour_vivid_house2()
# }
# 
# # 5. Function for graphing firm price against firm quantity (over time)
# price_quantity_mapping_F <- function(company_name = NULL, product_name = NULL) {
#   price_quantity_data <- model_data_compact %>% 
#     filter(Company == !!company_name, Product == !!product_name)
#   
#   graph_title <- paste(company_name,"supply curve",product_name)
#   windows()
#   ggplot(price_quantity_data, mapping = aes(x=Production_F, y=Price_F, colour=Year)) +
#     geom_point(aes(shape=Scenario)) + geom_line() +
#     ggtitle(graph_title)+
#     theme_vivid() + scale_colour_vivid_house2()
# }
# 
# # 6. Function for graphing firm price against global price (over time)
# price_firm_global_mapping <- function(company_name = NULL, product_name = NULL) {
#   price_quantity_data <- model_data_compact %>% 
#     filter(Company == !!company_name, Product == !!product_name)
#   
#   graph_title <- paste(company_name,"price against global price",product_name)
#   windows()
#   ggplot(price_quantity_data, mapping = aes(x=Price_G, y=Price_F, colour=Year)) +
#     geom_point(aes(shape=Scenario)) + geom_line() +
#     ggtitle(graph_title)
# } 

# Interpolation of Vivid scenario data from Rystad ------------------------
# 1. Load scenario data and consolidate with Rystad into a single dataset 
VE_scenario_data %<>%
  rename(Scenario = scenario,
         Product = fuel,
         Year = year,
         Units = units,
         Production_G = production) %>%
  filter(Product != "coal") %>%
  mutate(Product = case_when(Product == "gas" ~ "Gas",
                             Product == "oil" ~ "Liquid",
                             TRUE ~ NA_character_)) %>%
  select(Product, everything(),-Units)

model_data_compact %<>% ungroup() %>% mutate(Year=as.numeric(Year)) %>%
  select(Company, Product, Year, Scenario, Production_G, everything()) %>%
  # this data has some companies in certain years where both Production_F and Price_F will be zero.
  # These need to be deleted before proceeding
  filter(!is.na(Production_F) & !is.na(Price_F))

company <- unique(model_data_compact$Company)
product <- unique(model_data_compact$Product)
year <- unique(VE_scenario_data$Year)
VE_scenario_data_long <- expand.grid(company, product, year) %>%
  rename(Company = Var1, Product = Var2, Year = Var3) %>%
  left_join(VE_scenario_data) 

data_for_interpolation <- model_data_compact %>% bind_rows(VE_scenario_data_long)

# Save full dataset (SP addition - needed for product exposure analysis)
save_dated(data_for_interpolation, "DD_full_raw_dataset", folder = "Interim")

data_for_interpolation %<>%
  filter(Year!=2016)

# Gas in 2050 under BAU is higher than Rystad's High Scenario. The below replaces that value with
# the Rystad High value to ensure interpolation is possible. 

replacement <- data_for_interpolation %>% select(Company, Product, Year, Scenario, Production_G) %>%
  spread(Scenario, Production_G) %>% mutate(BAU=ifelse(Product=="Gas" & Year==2050, High, BAU)) %>%
  gather(key="Scenario",value="Production_G", -Company, -Product, -Year)

data_for_interpolation %<>% select(everything(), -Production_G) %>%
  left_join(replacement)

save_dated(data_for_interpolation, "DD_data_for_interpolation", folder = "Interim")

# This data then does not need any extrapolation. 

# 2. INTERPOLATE Price G from Production G and PRODUCTION F FROM PRODUCTION G 
#    AND PRICE F FROM PRICE G (INTERPOLATED)
#    AND UNIT COST F FROM PRODUCTION F (INTERPOLATED)
interp <- function(unique_ID = NULL) {
  company_name<-str_split(unique_ID, " ")[[1]][1]
  product_name<-str_split(unique_ID, " ")[[1]][2]
  date<-str_split(unique_ID, " ")[[1]][3]
  #print(paste(company_name, product_name, date))
  
  temp <- data_for_interpolation %>% ungroup() %>%
    filter(Company== company_name, Product== product_name, Year== date) %>% 
    arrange(Production_G) %>%
    # find a way for it to ignore those instances where there are too many NAsmutate
    mutate(is_na=is.na(Production_F & Price_F),
           count_na=sum(ifelse(is_na==FALSE,1,0)),
           Price_G_interpolated=ifelse(count_na>=2, approx(Production_G, Price_G, xout = Production_G)$y, NA),
           Production_F_interpolated=ifelse(count_na>=2,
                                            approx(Production_G, Production_F, xout=Production_G)$y, NA),
           Price_F_interpolated=ifelse(count_na>=2,approx(Price_G_interpolated, Price_F, 
                                                          xout=Price_G_interpolated)$y,NA),
           # Note that for the below interpolation of unit cost based on production_F, 
           # some of the firm production data in Rystad is the same across ALL scenarios
           # the below tells the approx function to ignore these observations
           # the relevant companies are not the big hitters so this was judged ok for now
           Test_1=mean(Production_F_interpolated, na.rm=TRUE), 
           Test_2=ifelse(Production_F_interpolated==Test_1,1,0),
           Unit_Cost_F_interpolated=ifelse(Test_2!=1,
                                           approx(Production_F_interpolated, Unit_Cost_F, 
                                                  xout = Production_F_interpolated)$y,NA),
           # the below step REMOVES THE POSSIBILITY OF NEGATIVE PROFITS
           # THIS IS VERY IMPORTANT TO KEEP IN MIND AS IT EFFECTIVELY CAPS THE POSSIBLE 
           # NEGATIVE IMPACT ON PROFITS 
           Unit_Cost_F_interpolated=ifelse(Unit_Cost_F_interpolated<Price_F_interpolated, 
                                           Unit_Cost_F_interpolated, Price_F_interpolated))
  
}

unique_ID_set <- data_for_interpolation %>% select(Company, Product, Year) %>%
  mutate(ID=paste(Company,Product,Year)) %>%
  select(ID) %>%
  unique()

unique_ID_set <- as.list(unique_ID_set$ID)

# This takes long(er) to run so don't re-do unless necessary
interpolated_data <- map(unique_ID_set,interp) %>% bind_rows()

interpolated_data %<>% select(everything(),-is_na,-count_na,-Test_1,-Test_2) 

save_dated(interpolated_data, "DD_interpolated_data", folder = "Interim")

# Analysis of interpolated data for stranding -----------------------------

data_for_analysis <- interpolated_data %>% 
  # get rid of those interpolations where Production F is negative
  mutate(Production_F_interpolated=ifelse(Production_F_interpolated>=0,
                                          Production_F_interpolated,NA),
         Price_F_interpolated=ifelse(!is.na(Production_F_interpolated),
                                     Price_F_interpolated,NA),
         Unit_Cost_F_interpolated=ifelse(!is.na(Production_F_interpolated),
                                         Unit_Cost_F_interpolated,NA),
         # get rid of those where Price F is negative
         Price_F_interpolated=ifelse(Price_F_interpolated>=0,Price_F_interpolated,NA),
         Production_F_interpolated=ifelse(!is.na(Price_F_interpolated),
                                          Production_F_interpolated,NA),
         Unit_Cost_F_interpolated=ifelse(!is.na(Price_F_interpolated),Unit_Cost_F_interpolated,NA),
         # get rid of those where Unit Cost F is negative
         Unit_Cost_F_interpolated=ifelse(Unit_Cost_F_interpolated>=0, Unit_Cost_F_interpolated,NA),
         Production_F_interpolated=ifelse(!is.na(Unit_Cost_F_interpolated),
                                          Production_F_interpolated,NA),
         Price_F_interpolated=ifelse(!is.na(Unit_Cost_F_interpolated),Price_F_interpolated,NA)) 
         
data_for_analysis %<>% 
  # filter(Scenario!="Very_Low" & Scenario!="Low" & Scenario!="Central" & Scenario!="High") %>%
  select(everything(),-Production_F,-Price_G,-Price_F,-Unit_Cost_F)

stranding_data <- data_for_analysis %>% ungroup() %>%
  transmute(Company, Product, Year, Scenario, Production_G,
            Production_F=Production_F_interpolated, Price_F=Price_F_interpolated,
            Price_G=Price_G_interpolated, Unit_Cost_F=Unit_Cost_F_interpolated) %>%
  # add profit column
  mutate(Profit_F=(Price_F-Unit_Cost_F)*Production_F) %>%
  arrange(Company, Product, Year, Scenario) %>% 
  group_by(Company, Product, Year) %>% 
  # calculate stranding 
  mutate(Profit_BAU=ifelse(Scenario=="BAU",(Price_F-Unit_Cost_F)*Production_F,NA),
         Price_BAU=ifelse(Scenario=="BAU", Price_F, NA),
         Unit_Cost_BAU=ifelse(Scenario=="BAU", Unit_Cost_F,NA),
         Production_BAU=ifelse(Scenario=="BAU", Production_F, NA)) %>%
  fill(Profit_BAU,.direction="up") %>% fill(Price_BAU, .direction="up") %>%
  fill(Unit_Cost_BAU,.direction="up") %>% fill(Production_BAU, .direction="up") %>% ungroup() %>%
  mutate(Profit_BAU=ifelse(!is.na(Production_F | Price_F | Unit_Cost_F),
                                 Profit_BAU,NA),
         # Stranding_impact=((Price_F-Unit_Cost_F)*Production_F-Profit_BAU)-Margin_impact,
         Profit_impact = Profit_BAU-Profit_F,
         Profit_impact_hat = ifelse(Profit_impact!=0, Profit_BAU-(Price_F-Unit_Cost_BAU)*Production_F, NA),
         Margin_impact_hat = ifelse(Profit_impact!=0, (Price_BAU-Price_F)*Production_F, NA),
         Sense_test = Margin_impact_hat/Profit_impact_hat,
         Margin_impact = Profit_impact*(Margin_impact_hat/Profit_impact_hat),
         Stranding_impact = (1-Margin_impact_hat/Profit_impact_hat)*Profit_impact) %>%
  select(everything()#,-Profit_BAU, -Price_BAU, -Unit_Cost_BAU ,-Profit_impact_hat, -Margin_impact_hat
         )

total_company_stranding <- stranding_data %>% group_by(Company,Year,Scenario) %>%
  summarise(Profit_F=sum(Profit_F,na.rm=TRUE), Margin_impact=sum(Margin_impact,na.rm=TRUE), 
            Stranding_impact=sum(Stranding_impact,na.rm=TRUE), Profit_impact=sum(Profit_impact,na.rm=TRUE)) %>%
  mutate(Product="All") %>% ungroup()

stranding_data_full<- stranding_data %>% bind_rows(total_company_stranding)
save_dated(stranding_data_full, "DD_full_stranding_data", folder = "Interim")

# drop columns that are no longer needed

stranding_data_compact <- stranding_data_full %>% ungroup() %>%
  arrange(Company,Year,Scenario) %>%
  mutate(Total_Profit_BAU=ifelse(Scenario=="BAU" & Product=="All",Profit_F,NA)) %>%
  fill(Total_Profit_BAU,.direction="up") %>%
  mutate(Profit_BAU=ifelse(Product=="All", Total_Profit_BAU, Profit_BAU),
         Profit_impact_pct=ifelse(Profit_BAU!=0,Profit_impact/Profit_BAU,NA),
         Margin_impact_pct=ifelse(Profit_BAU!=0,Margin_impact/Profit_BAU,NA),
         Stranding_impact_pct=ifelse(Profit_BAU!=0,Stranding_impact/Profit_BAU,NA)) %>%
  select(everything(), -Total_Profit_BAU)

# augment with column of Company names used for rest of analysis

stranding_data_compact %<>% mutate(Rystad_Name=Company) %>% select(Rystad_Name,everything(),-Company) %>%
  left_join(company_name_lookup) %>% select(ISIN_Code:OG_Subsidiary,everything())

stranding_chart_data <- stranding_data_compact

stranding_data_compact %<>% filter(!(Scenario %in% c("High", "Central", "Low", "Very_Low")))

save_dated(stranding_data_compact, "DD_stranding_data_compact", folder = "Interim")

stranding_data_compact %>%
  select(everything(),-c(Production_F:Unit_Cost_F))

# test <- stranding_data_compact %>% mutate(Check1=ifelse(abs(Profit_impact_pct)<1,1,0),
#                                           Check1_1=ifelse(is.na(Profit_impact_pct),1,0),
#                                           Check2=ifelse(Stranding_impact_pct<=0 & Margin_impact<=0 | 
#                                                           Stranding_impact_pct>=0 & Margin_impact>=0,1,0),
#                                           Check3=ifelse(abs(Stranding_impact_pct)>abs(Margin_impact_pct),1,0))
# 
# test1 <- sum(test$Check1, na.rm=TRUE)
# test1_1 <- sum(test$Check1_1, na.rm=TRUE)
# # test2 <- test %<>% filter(Check2==0, Product=="Gas")
# test3 <- sum(test$Check3, na.rm=TRUE)
# test4 <- test %>% filter(Check3==1)

# stranding_data_compact %<>% filter(Rystad_Name=="Shell", Product=="All")

# Stranding charts --------------------------------------------------------

# Global chart for % stranding versus margin impacts.
# global_stranding_chart <- function(VE_scenario = NULL, select_year = NULL, product_name = NULL) { 
#   
#   chart_data <- stranding_chart_data %>% filter(Scenario == VE_scenario | Scenario == "BAU" | Scenario == "High" |
#                                    Scenario == "Central" | Scenario == "Low" | Scenario == "Very_Low",
#                                    Year == select_year, Product == product_name) %>%
#     mutate(Shading1=ifelse(Scenario == VE_scenario | Scenario == "BAU", Price_G, NA),
#            Shading2=ifelse(Scenario == VE_scenario | Scenario == "BAU", Production_G, NA),
#            Shading3 = case_when(Production_G < min(Shading2, na.rm = TRUE) ~ NA_real_,
#                                 Production_G > max(Shading2, na.rm = TRUE) ~ NA_real_,
#                                 TRUE ~ Production_G))
#   
#   chart_title <- paste("Global margin v stranding impact", select_year, VE_scenario)
#   
#   windows()
#   ggplot(chart_data, mapping=aes(x=Production_G, y=Price_G)) +
#     geom_point(aes(y=Price_G, shape=Scenario)) + geom_line(aes(y=Price_G))+
#     geom_point(aes(y=Shading1, shape=Scenario)) + 
#     geom_hline(aes(yintercept=Shading1, group=Scenario)) +
#     geom_vline(aes(xintercept=Shading2, group=Scenario)) +
#     geom_ribbon(aes(x= Shading3, ymin = Price_G, ymax = max(Shading1, na.rm = TRUE)), alpha=0.5) +
#     ggtitle(chart_title) + theme_vivid() + scale_colour_vivid_house2() +
#     scale_x_continuous(limits = c(0, 6*10^10))
# }
# 
# # Company chart function
# company_stranding_chart <- function(Rystad_company = NULL, VE_scenario = NULL, select_year = NULL, product_name = NULL) { 
#   
#   chart_data <- stranding_chart_data %>% filter(Scenario == VE_scenario | Scenario == "BAU" | Scenario == "High" |
#                                                   Scenario == "Central" | Scenario == "Low" | Scenario == "Very_Low",
#                                                 Year == select_year, Product == product_name) %>%
#     filter(Rystad_Name == Rystad_company) %>%
#     mutate(Shading1=ifelse(Scenario == VE_scenario | Scenario == "BAU", Price_F, NA),
#            Shading2=ifelse(Scenario == VE_scenario | Scenario == "BAU", Production_F, NA),
#            Shading3 = case_when(Production_F < min(Shading2, na.rm = TRUE) ~ NA_real_,
#                                 Production_F > max(Shading2, na.rm = TRUE) ~ NA_real_,
#                                 TRUE ~ Production_F))
#   
#   chart_title <- paste(Rystad_company, product_name, "margin v stranding impact", select_year, VE_scenario)
#   
#   windows()
#   ggplot(chart_data, mapping=aes(x=Production_F, y=Price_F)) +
#     geom_point(aes(y=Price_F, shape=Scenario)) + geom_line(aes(y=Price_F))+
#     geom_point(aes(y=Unit_Cost_F, shape=Scenario)) + geom_line(aes(y=Unit_Cost_F)) +
#     geom_point(aes(y=Shading1, shape=Scenario)) + 
#     geom_hline(aes(yintercept=Shading1, group=Scenario)) +
#     geom_vline(aes(xintercept=Shading2, group=Scenario)) +
#     geom_ribbon(aes(x= Shading3, ymin = Price_F, ymax = max(Shading1, na.rm = TRUE)), alpha=0.5) +
#     ggtitle(chart_title) + #theme_vivid() + scale_colour_vivid_house2() +
#     scale_x_continuous(limits = c(min(chart_data$Production_F), max(chart_data$Production_F)))
# }

# NPV impact calculation --------------------------------------------------

NPV_stranding_impacts <- stranding_data_compact %>% ungroup() %>% 
  select(ISIN_Code:Scenario, Profit_BAU, Profit_impact, 
         Margin_impact, Stranding_impact) %>%
  # Weights are assigned in order to include profits from 2018-2019
  # assume one period runs for 5 years from 2017.5-2022.5 for example
  # the first period starts in 2018-2022.5, so weight 0.9
  # last period runs from 2047.5 to 2050, so weight 0.5
  mutate(Weight = case_when(Year == 2020 ~ 0.9, Year == 2050 ~ 0.5, TRUE ~ 1),
         # Discount rate should be 0.0575 (2% inflation rate adjustment does not apply)
         Discount_factor = discount_rate, Base_year = 2018,
         NPV_Profit_impact = Profit_impact/((1+Discount_factor)^(Year-Base_year)),
         NPV_Margin_impact = Margin_impact/((1+Discount_factor)^(Year-Base_year)),
         NPV_Stranding_impact = Stranding_impact/((1+Discount_factor)^(Year-Base_year))) %>%
  group_by(Rystad_Name, VE_Name, Product, Scenario) %>%
  arrange(.by_group = TRUE) %>%
  mutate(NPV_Profit_BAU_sum = sum(Weight * Profit_BAU/((1+Discount_factor)^(Year-Base_year)),
                                  na.rm=TRUE),
         NPV_Profit_impact_sum = sum(Weight * NPV_Profit_impact, na.rm=TRUE), 
         NPV_Stranding_impact_sum = sum(Weight * NPV_Stranding_impact, na.rm=TRUE),
         NPV_Margin_impact_sum = sum(Weight * NPV_Margin_impact, na.rm=TRUE),
         NPV_Profit_impact_pct = NPV_Profit_impact_sum/NPV_Profit_BAU_sum,
         NPV_Stranding_impact_pct = NPV_Stranding_impact_sum/NPV_Profit_BAU_sum,
         NPV_Margin_impact_pct = NPV_Margin_impact_sum/NPV_Profit_BAU_sum)

save_dated(NPV_stranding_impacts, "DD_NPV_stranding_impacts", folder = "Output", csv = TRUE)