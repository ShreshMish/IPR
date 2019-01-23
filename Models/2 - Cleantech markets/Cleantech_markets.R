##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  14/01/2019
##### Code author:        Justine Schafer
##### Edited by:          Shyamal Patel (new file / folder structure)
##### Description:        This script uses cleaned Orbis IP data for Vivid categories of green revenue to project future 
#####                     market size and company market share for greentech companies
##### Dependencies:       1.  Results from Financial prelim data cleaning "0 - Data cleaning/1 - Financial prelim/Output/"
#####                     2.  Results from Cleantech markets data cleaning "0 - Data cleaning/5 - Green upside/Output/"
##### Notes:              

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA ----

packages <- c("tidyverse", "magrittr", "readr", "readxl", "here", "stringi", "stringr", "themeVE")
lapply(packages, require, character.only = TRUE)

# Define date for save file names
day <- format(Sys.time(), "%d")
month <- match(format(Sys.time(), "%b"), month.abb)
if(nchar(month) == 1) {month <- paste0("0", month)}
year <- substr(format(Sys.time(), "%Y"), 3, 4)
date <- paste0(year, month, day)

# These functions count the number of missing or zero-value observations in a tibble
na_counter <- function(x) {sum(is.na(x))}
zero_counter <- function(x) {sum(ifelse(x == 0, 1, 0), na.rm = TRUE)}

# These functions save base, and dated files in the Interim or Outputs folder for later use
# Dated files are kept for version control purposes
save_dated <- function(data, name = " ", folder, dated = "YES", csv = "NO") {
  main_path <- paste0("2 - Cleantech markets/", folder)
  dated_path <- paste0("2 - Cleantech markets/", folder, "/Dated/")
  
  if(name == " ") {name <- gsub("_", " ", deparse(substitute(data)))}
  
  # Save main file
  saveRDS(data, here(main_path, paste0(name, ".rds")))
  # Save dated file (optional)
  if(dated == "YES") {saveRDS(data, here(dated_path, paste0(date, "_", name, ".rds")))}
  # Save dated CSV file (optional)
  if(csv == "YES") {write_csv(data, here(dated_path, paste0(date, "_", name, ".csv")))}
}

# Read in cleaned GR data
green_revenue_data <- readRDS("2 - Cleantech markets/Input/Combined_Green_Revenue_data.rds")

# Read in revenue data
total_revenue_data <- readRDS("2 - Cleantech markets/Input/TR_cleaned_2016USD_data.rds")

# Read in renewables scenario data
ren_market_size_data <- readRDS("2 - Cleantech markets/Input/Renewable_capacity.rds")

# Read in EV scenario data
EV_market_size_data <- readRDS("2 - Cleantech markets/Input/EV_new_capacity.rds")

# Read in biofuels scenario data
biofuels_market_size_data <- readRDS("2 - Cleantech markets/Input/Biofuels_production.rds")

# Set discount rate
discount_rate <- 0.0575

##### SECTION 2 - Data cleaning ----

green_revenue_data %<>% select(everything(), -country_of_listing, -market_cap, -tr_company) %>%
  filter(GR_percent != 0, VE_category != "CCS")

total_revenue_data %<>% 
  select(ticker:market_cap, revenue:corporation_tax_rate) %>%
  rename(tr_company = company)

combined_revenue_data <- green_revenue_data %>% 
  left_join(total_revenue_data, by = c("VE_ISIN" = "ISIN_code")) %>%
  select(company, tr_company, VE_ISIN, revenue, profit, everything()) %>%
  # There are two companies without TR revenue - Linde and China Communications Construction 'A'. 
  # These are binned in the following step
  filter(!is.na(revenue)) %>%
  # see issues log #54 for explanation for the steps below. 
  filter(tr_company != "YAMAHA") %>% 
  select(everything(), -Is_FTSE_Subsidiary) 

# Calculate current market shares -----------------------------------------

analysis_revenue_data <- combined_revenue_data %>% 
  # aggregate green revenue data for each company by VE_category (delete individual line items) 
  group_by(VE_ISIN, VE_category) %>%
  mutate(VE_category_rev_share = sum(VE_category_rev_share, na.rm = TRUE)) %>%
  select(company:GR_percent, ISIN_code:GR_subsegments, AllIPC:corporation_tax_rate) %>%
  unique() %>%
  mutate(VE_category_rev = VE_category_rev_share * revenue) %>% 
  ungroup() %>% group_by(VE_category) %>%
  # REVISIT THE BELOW TO SEE WHETHER ANY ITEMS ARE DOUBLE COUNTED
  mutate(Total_VE_category_rev = sum(VE_category_rev, na.rm = TRUE),
         Existing_market_share = VE_category_rev/Total_VE_category_rev,
         No_IP_VE_category_m_s = ifelse(is.na(Total_AllIPC), Existing_market_share, NA),
         No_IP_VE_category_m_s = sum(No_IP_VE_category_m_s, na.rm = TRUE),
         GR = GR_percent * revenue)

# Calculate total IP per VE category and Green IP shares ------------------
# REVISIT YAMAHA (ONLY ONE WHERE FTSE OBSERVATION IS A SUBSIDIARY)
# ASSUME THOSE WITHOUT IP DATA MAINTAIN THEIR EXISTING MARKET SHARE 
# THE SUM OF THESE SHARES THEN NEEDS TO BE SUBTRACTED FROM THE TOTAL BEFORE CALCULATING IP SHARES TO ENSURE 
# THEY ADD UP TO 100% (USE CHECK AND CHECK_100 TO CONFIRM)

IP_totals <- combined_revenue_data %>% 
  select(company:VE_category, AllIPC:Total_OnlyMainIPC) %>%
  group_by(VE_category) %>% select(VE_ISIN:Total_OnlyMainIPC, -year, -revenue, -profit) %>%
  unique() %>%
  mutate(sum_VE_cat_AllIPC = sum(AllIPC, na.rm = TRUE)) %>%
  select(VE_category, sum_VE_cat_AllIPC) %>% unique()

analysis_revenue_data %<>% left_join(IP_totals) %>%
  mutate(IP_market_share = AllIPC/sum_VE_cat_AllIPC * (1 - No_IP_VE_category_m_s),
         IP_market_share = ifelse(is.na(IP_market_share) & !is.na(Total_AllIPC), 0, IP_market_share),
         IP_market_share = ifelse(is.na(IP_market_share), Existing_market_share, IP_market_share)) %>%
  select(company:VE_ISIN, VE_category, Existing_market_share, IP_market_share, everything()) %>%
  mutate(CHECK_IP = sum(IP_market_share, na.rm = TRUE),
         CHECK_existing = sum(Existing_market_share, na.rm = TRUE)) %>%
  select(CHECK_IP, CHECK_existing, everything(), -CHECK_IP, -CHECK_existing)

market_share_analysis <- analysis_revenue_data %>% 
  select(tr_company:IP_market_share, revenue, profit, VE_category_rev, Total_VE_category_rev, GR_percent, 
         estimate_date)

years <- c(2018, seq(2020, 2050, 5))

expand_years <- expand.grid(tr_company = unique(market_share_analysis$tr_company),
                              VE_category = unique(market_share_analysis$VE_category),
                              year = years)

market_share_analysis %<>% left_join(expand_years) %>%
  mutate(estimate_date = as.numeric(gsub("FY","",estimate_date)),
         year = as.numeric(year))
  

# Calculate future market size --------------------------------------------

VE_category_mapping <- green_revenue_data %>% ungroup() %>% select(VE_category) %>% 
  filter(VE_category != "Other") %>% unique() %>% arrange(VE_category) %>%
  mutate(Fuel = c(NA, NA, "Hydro", NA, "Wind + Solar", "Wind + Solar"))

# DATA ON RENEWABLES

ren_market_size_data1 <- ren_market_size_data %>%
  select(Scenario, Fuel, `2015`:`2050`) %>%
  filter(Fuel %in% c("Wind + Solar", "Hydro"), Scenario != "BAU") %>%
  mutate(Scenario = ifelse(Scenario == "WEO BAU", "BAU", Scenario)) %>%
  left_join(VE_category_mapping) %>%
  select(Scenario, VE_category, everything(), -Fuel) %>%
  mutate(`2017` = `2016` + (`2020`-`2016`)/4,
         `2025` = ifelse(is.na(`2025`), `2020` + (`2030`-`2020`)/2, `2025`),
         `2035` = `2030` + (`2040`-`2030`)/2,
         `2045` = `2040` + (`2050`-`2040`)/2) %>% 
  gather(`2015`:`2045`, key = year, value = Stock) %>%
  filter(year >= 2017)

# DATA ON EVs

EV_market_size_data1 <- EV_market_size_data %>%
  filter(!Scenario %in% c("IEA 2DS")) %>%
  mutate(Scenario = ifelse(Scenario == "IEA RTS", "BAU", Scenario)) %>%
  mutate(`2017` = `2015` + (`2020`-`2015`)*2/5,
         `2025` = `2020` + (`2030`-`2020`)/2,
         `2035` = `2030` + (`2040`-`2030`)/2,
         `2045` = `2040` + (`2050`-`2040`)/2) %>%
  gather(`2015`:`2045`, key = year, value = Stock) %>%
  mutate(VE_category = "EV_aggregate") %>%
  filter(year >= 2017)

minerals_market_size_data1 <- EV_market_size_data1 %>%
  mutate(VE_category = "Minerals_for_batteries")

# DATA ON BIOFUELS

# remove unnecessary variables - this removes units (TJ)
biofuels_market_size_data1 <- biofuels_market_size_data %>%
  select(Scenario, `2012`:`2050`) %>% 
  mutate(`2017` = `2012` + (`2020`-`2012`)*5/8,
         `2025` = `2020` + (`2030`-`2020`)/2,
         `2035` = `2030` + (`2040`-`2030`)/2,
         `2045` = `2040` + (`2050`-`2040`)/2) %>%
  gather(`2012`:`2045`, key = year, value = Stock) %>%
  mutate(VE_category = "Biofuels_production") %>%
  filter(year >= 2017)
  
# MERGE MARKET SIZE DATASETS TOGETHER  
all_VE_market_size1 <- ren_market_size_data1 %>%
  bind_rows(EV_market_size_data1, biofuels_market_size_data1, minerals_market_size_data1) %>%
  arrange(by = year) %>%
  mutate(initial_stock = ifelse(year == "2017", Stock, NA)) %>%
  group_by(VE_category) %>%
  fill(initial_stock) %>%
  ungroup() %>%
  group_by(Scenario, VE_category) %>%
  mutate(total_pct_change = Stock/initial_stock,
         yoy_absolute_change = Stock - lag(Stock, default = 0),
         yoy_absolute_change = ifelse(yoy_absolute_change == Stock, 0, yoy_absolute_change),
         yoy_pct_change = yoy_absolute_change/lag(Stock, default = 0),
         yoy_pct_change = ifelse(is.nan(yoy_pct_change), 0, yoy_pct_change),
         year = as.numeric(year))

# Determining alpha / constraint for MS projection ------------------------

market_share_projection_data <- market_share_analysis %>% 
  mutate(time_zero = 2020)

# Create a function for projecting future market share using two arguments; alpha and a constraint. 
# Conceptually, alpha = 1 / (years in which you want Projected market share = IP market share)
# i.e. if you want market share to become IP share in 100 years, alpha = 1/100
# Constraint should be a number between 0 and 1

future_market_share_EDA <- function(parameter_set = NULL) {
alpha <- as.numeric(str_split(parameter_set, " ")[[1]][1])
constraint <- as.numeric(str_split(parameter_set, " ") [[1]][2])
print(paste(alpha, constraint))
    
  temp <- market_share_projection_data %>% ungroup() %>%
    mutate(Projected_market_share = Existing_market_share + alpha * (IP_market_share - Existing_market_share) *
             (year - time_zero),
           Projected_market_share = 
             ifelse(abs(Projected_market_share - Existing_market_share)/Existing_market_share > constraint, 
                    Existing_market_share * (1 + sign(Projected_market_share - Existing_market_share) * constraint), 
                    Projected_market_share),
           Projected_market_share = case_when(Projected_market_share < 0 ~ 0,
                                              Projected_market_share > 1 ~ 1,
                                              TRUE ~ Projected_market_share),
           Constraint = constraint,
           Alpha = alpha)
}

years_to_convergence <- c(seq(5,100,5)) 
alpha_values <- 1/years_to_convergence
constraint_values <- c(10^6, seq(0.05, 0.5, 0.05))
parameter_values <- merge(alpha_values, constraint_values) %>%
  rename(alpha = x, constraint = y) %>%
  mutate(ID = paste(alpha, constraint)) %>%
  select(ID) 

parameter_ID <- as.list(parameter_values$ID) 

MS_EDA <- map(parameter_ID, future_market_share) %>% bind_rows()

# Graph different parameter values for the same company
graph_alpha <- function (product = NULL, constraint = NULL) {
temp<- filter(MS_EDA, VE_category == product, Constraint == constraint) %>%
  arrange(desc(Existing_market_share)) %>%
  slice(1:160) %>%
  mutate(Alpha = as.character(Alpha))

windows()
ggplot(temp, aes(x = year, y = Projected_market_share)) +
  geom_line(aes(colour = Alpha)) + 
  geom_hline(aes(yintercept = Existing_market_share)) +
  geom_hline(aes(yintercept = IP_market_share)) +
  theme_vivid() + scale_colour_vivid_house2()
}
graph_constraint <- function (product = NULL, alpha = NULL) {
  temp<- filter(MS_EDA, VE_category == product, Alpha == alpha) %>%
    arrange(desc(Existing_market_share)) %>%
    slice(1:88) %>%
    mutate(Constraint = as.character(Constraint))
  
  windows()
  ggplot(temp, aes(x = year, y = Projected_market_share)) +
    geom_line(aes(colour = Constraint)) + 
    geom_hline(aes(yintercept = Existing_market_share)) +
    geom_hline(aes(yintercept = IP_market_share)) +
    theme_vivid() + scale_colour_vivid_house2()  
}

# Market share projection -------------------------------------------------
# Settled on alpha = 1/20 (2040 realisation of whichever is larger, (1 +/- constraint) * Existing or IP)
# Number 1 applies a constraint as in: cannot travel beyond a 10% range around the existing share
future_market_share_1 <- function(alpha = NULL, constraint = NULL) {
  temp <- market_share_projection_data %>% ungroup() %>%
    mutate(Projected_market_share = 
             case_when(constraint * Existing_market_share <= abs(IP_market_share - Existing_market_share)/
                         Existing_market_share ~ Existing_market_share * (1 + alpha * sign(IP_market_share - Existing_market_share) * 
                                                    constraint * (year - time_zero)),
                       constraint * Existing_market_share > abs(IP_market_share - Existing_market_share)/
                         Existing_market_share ~
                         Existing_market_share + alpha * (IP_market_share - Existing_market_share) * 
                         (year - time_zero),
                       TRUE ~ NA_real_),
           Projected_market_share = case_when(abs(Projected_market_share - Existing_market_share) >
                                                abs(IP_market_share - Existing_market_share) ~
                                                IP_market_share,
                                              abs(Projected_market_share - Existing_market_share)/Existing_market_share > constraint ~ 
                                                (1 + sign(IP_market_share - Existing_market_share) * constraint) * 
                                                Existing_market_share,
                                              TRUE ~ Projected_market_share),
           Projected_market_share = case_when(Projected_market_share < 0 ~ 0,
                                              Projected_market_share > 1 ~ 1,
                                              Existing_market_share == 0 & IP_market_share == 0 ~ 0,
                                              TRUE ~ Projected_market_share),
           Constraint = constraint,
           Alpha = alpha) %>%
    group_by(VE_category, year) %>%
    mutate(rebalance = sum(Projected_market_share, na.rm = TRUE),
           Projected_market_share = Projected_market_share / rebalance) %>%
    select(tr_company:IP_market_share, Projected_market_share, everything())
}
# Number 2 applies a constraint to mean cannot travel more than 20% of the difference between Existing and IP
future_market_share_2 <- function(alpha = NULL, constraint = NULL) {
  temp <- market_share_projection_data %>% ungroup() %>%
    mutate(Projected_market_share = Existing_market_share + alpha * constraint * 
             (IP_market_share - Existing_market_share) * (year - time_zero),
           Projected_market_share = case_when(Projected_market_share < 0 ~ 0,
                                              Projected_market_share > 1 ~ 1,
                                              Existing_market_share == 0 & IP_market_share == 0 ~ 0,
                                              TRUE ~ Projected_market_share),
           Projected_market_share = case_when(abs(Projected_market_share - Existing_market_share) > 
                                                abs(constraint*(IP_market_share - Existing_market_share)) ~
                                                Existing_market_share + constraint * 
                                                (IP_market_share - Existing_market_share),
                                              TRUE ~ Projected_market_share),
           Constraint = constraint,
           Alpha = alpha) %>%
    group_by(VE_category, year) %>%
    mutate(rebalance = sum(Projected_market_share, na.rm = TRUE),
           Projected_market_share = Projected_market_share / rebalance) %>%
    select(tr_company:IP_market_share, Projected_market_share, everything())
}

market_share_projection <- future_market_share_2(1/20, .2) %>%
  group_by(VE_category, year) %>%
  mutate(CHECK = sum(Projected_market_share, na.rm = TRUE),
         CHECK2 = Projected_market_share/(IP_market_share - Existing_market_share)) #%>%
#select(everything(), -CHECK2)

# Combine market size and share projections -------------------------------

full_analysis <- market_share_projection %>% 
  left_join(all_VE_market_size1) %>% 
  mutate(Projected_revenue = Projected_market_share * Total_VE_category_rev * total_pct_change,
         Projected_profit = Projected_revenue * (profit / revenue)) %>%
  filter(year >= 2020, VE_category != "Other") %>% 
  select(tr_company, VE_category_rev, Projected_profit, Projected_revenue, Existing_market_share, IP_market_share,
         Projected_market_share, everything())

# NPV impact calculation --------------------------------------------------

NPV_revenue_impacts <- full_analysis %>%
  # Weights are assigned in order to include profits from 2018-2019
  # assume one period runs for 5 years from 2017.5-2022.5 for example
  # the first period starts in 2018-2022.5, so weight 0.9
  # last period runs from 2047.5 to 2050, so weight 0.5
  group_by(VE_category, tr_company, Scenario) %>% 
  # Discount rate should be 0.0575 (2% inflation rate adjustment does not apply)
  mutate(Discount_factor = discount_rate, Base_year = 2018,
         NPV_future_profits = sum(Projected_profit/((1+Discount_factor)^(year-Base_year)), na.rm = TRUE),
         BAU_NPV_future_profits = ifelse(Scenario == "BAU", NPV_future_profits, NA)) %>%
  ungroup() %>% group_by(VE_category, tr_company) %>% 
  arrange(.by_group = TRUE, by = Scenario) %>% fill(BAU_NPV_future_profits, .direction = "up") %>%
  mutate(Pct_change_NPV_profits = (NPV_future_profits - BAU_NPV_future_profits)/BAU_NPV_future_profits,
         Pct_change_NPV_profits = ifelse(BAU_NPV_future_profits == 0, 0, Pct_change_NPV_profits)) %>%
  select(tr_company, VE_category, Scenario, year, Pct_change_NPV_profits, BAU_NPV_future_profits, 
         NPV_future_profits, everything())

save_dated(NPV_revenue_impacts, "NPV_revenue_impacts_green_upside", folder = "Output", csv = TRUE)

# Repackage data for report and submission to HSBC ------------------------
Data_for_report_charts <- NPV_revenue_impacts %>% 
  filter(Existing_market_share > 0) %>%
  ungroup() %>% group_by(year, VE_category, Scenario) %>%
  mutate(Relative_revenue = Projected_revenue/VE_category_rev,
         increase_test = case_when(IP_market_share > Existing_market_share ~ 1,
                                   IP_market_share < Existing_market_share ~ 0,
                                   TRUE ~ NA_real_),
         yearly_average = weighted.mean(Relative_revenue, Existing_market_share, na.rm = TRUE)) %>%
  group_by(year, VE_category, increase_test, Scenario) %>%
  mutate(yearly_average_inc = ifelse(increase_test == 1, weighted.mean(Relative_revenue, Existing_market_share, na.rm = TRUE), NA),
         yearly_average_dec = ifelse(increase_test == 0, weighted.mean(Relative_revenue, Existing_market_share, na.rm = TRUE), NA)) %>%
  ungroup() %>% group_by(year, VE_category, Scenario) %>% 
  fill(yearly_average_inc, .direction = "up") %>% fill(yearly_average_dec, .direction = "up") %>%
  ungroup() %>% 
  mutate(Projected_Total_VE_cat_rev = Total_VE_category_rev * total_pct_change)

Market_share_ranking <- filter(Data_for_report_charts, year == 2050, Scenario == "2DS central") %>% 
  ungroup() %>%
  select(tr_company, VE_category, Existing_market_share, IP_market_share, Projected_market_share) %>% 
  group_by(VE_category) %>%
  arrange(.by_group = TRUE, desc(Existing_market_share)) %>%
  mutate(Position_in_category = 1:n()) %>%
  mutate(Total_in_category = n(),
         Cumulative_IP = cumsum(IP_market_share),
         Cumulative_Existing = cumsum(Existing_market_share),
         Cumulative_Projected = cumsum(Projected_market_share)) %>%
  select(tr_company, Existing_market_share:Projected_market_share, Cumulative_IP:Cumulative_Projected,everything()) %>%
  select(tr_company, Existing_market_share, everything(), -Projected_market_share, -Cumulative_IP, 
         -Cumulative_Projected, -Cumulative_Existing, -IP_market_share)

Data_for_report_charts %<>% left_join(Market_share_ranking) %>% ungroup() %>% 
  group_by(VE_category, year, Scenario) %>%
  mutate(Projected_top_5 = ifelse(Position_in_category <= 5, Projected_market_share, NA),
         Projected_top_5 = sum(Projected_top_5, na.rm = TRUE),
         Projected_top_10 = ifelse(Position_in_category <= 10, Projected_market_share, NA),
         Projected_top_10 = sum(Projected_top_10, na.rm = TRUE),
         Existing_top_5 = ifelse(Position_in_category <= 5, Existing_market_share, NA),
         Existing_top_5 = sum(Existing_top_5, na.rm = TRUE),
         Existing_top_10 = ifelse(Position_in_category <= 10, Existing_market_share, NA),
         Existing_top_10 = sum(Existing_top_10, na.rm = TRUE))

write.csv(Data_for_report_charts, "2 - Cleantech markets/Interim/Data for green upside report charts.csv")

# String plots for NPV impact dispersion ----------------------------------
graph_NPV_impacts <- function(product = NULL) {

temp <- filter(NPV_revenue_impacts, VE_category == product, Existing_market_share != 0) %>%
  group_by(tr_company, Scenario) %>% 
  mutate(NPV_profits_EMS = (sum(((profit/revenue) * Existing_market_share * Total_VE_category_rev * total_pct_change) /
                                        ((1+Discount_factor)^(year-Base_year)), na.rm = TRUE)),
         BAU_NPV_profits_EMS = ifelse(Scenario == "BAU", NPV_profits_EMS, NA)) %>%
  ungroup() %>% group_by(tr_company) %>% 
  arrange(.by_group = TRUE, by = Scenario) %>% fill(BAU_NPV_profits_EMS, .direction = "up") %>%
  mutate(NPV_pct_EMS = (NPV_profits_EMS - BAU_NPV_profits_EMS)/BAU_NPV_profits_EMS,
         NPV_pct_EMS = ifelse(BAU_NPV_profits_EMS == 0, 0, NPV_pct_EMS)) %>%
  filter(year == 2050, Existing_market_share != IP_market_share)

graph_title <- paste(product)
ggplot(temp, aes(x = Scenario, y = Pct_change_NPV_profits, colour = Scenario)) +
  geom_jitter(width = 0.1) +
  geom_point(aes(x = Scenario, y = NPV_pct_EMS, colour = "No change in market share"), size = 5) +
  ggtitle(graph_title, "Alpha = 1/20, Constraint = 0.2") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()

ggsave(paste0("2 - Cleantech markets/Interim/", "Constraint 0.2 ", product, ".png"), width = 22, height = 11.5)

}

# do not run
#graph_NPV_impacts("Hydro_power")
#graph_NPV_impacts("Solar_power")
#graph_NPV_impacts("Wind_power")
#graph_NPV_impacts("Biofuels_production")
#graph_NPV_impacts("EV_aggregate")