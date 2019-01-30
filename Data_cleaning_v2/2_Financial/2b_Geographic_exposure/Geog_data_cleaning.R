##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  28/01/2019
##### Code author:        Shyamal Patel
##### Description:        This script reads in TR financial data from Excel, and cleans the data before
#####                     geographic exposure analysis
##### Dependencies:       1.  Latest Thomson Reuters cleaned company dataset: "2_Financial/2a_Preliminary/Output/Companies_2016USD_data.rds"
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "2_Financial/2b_Geographic_exposure"
source("utils.R")

# Read in Thomson Reuters cleaned dataset
company_clean_data <- readRDS("2_Financial/2a_Preliminary/Output/Companies_2016USD_data.rds")

# 'Domestic' category remapped regions dataset (Dexter analysis in 07/18)
domestic_geog_clean_data <- read_csv(input_source("TR_domestic_geogs_renamed.csv")) %>%
  select(-X1, -region_num, -region_revenue, -revenue)

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Remove redundant variables and regions not involved in analysis

company_geog_data <- company_clean_data %>%
  select(company_id:corporation_tax_rate, contains("region"))

# Remove regions which should not be used for geographic exposure analysis (identified through list of unique geographies reported - TR geographies raw.rds)
redundant_regions <- c("Acquisition_costs", "Adjustment", "Adjustment_and_Disposals", "Adjustment_&_Eliminations",
                       "Adjustment_and_eliminations_&_Other", "Adjustment_and_Elimination", "Adjustment/Other", "Adjustments", "Adjustments/Intragroup", "Adjustments_&_Eliminations", "Adjustments_and_eliminations_&_Other",
                       "Aerospace_System_&_Climate", "All_Other_Segments", "All_Others_segments", "Appliances_and_lightening_&_Other", "Associated_Companies", "Associates_and_Joint_Ventures",
                       "Banking_Corporate_Expense/Other", "Benefits_and_Rewards_Services", "Bottling_Investments",
                       "Business_tax", "Central_Function", "Central_Funding_& _liminations", "Charge_for_fair_value_markup_of_acquired_inventory",
                       "City_Living", "Common_Function", "Common_Functions", "Connected_Fitness", "Consolidate_Adjustment", "Consolidated_Adjustment",
                       "Consolidation", "Consolidation_Adj.", "Consolidation_Adjustment", "Consolidation_Adjustments", "Consolidation_Corrections",
                       "Coporate_&_Other", "corporate", "Corporate", "Corporate_Administration", "Corporation_Administration", "Corporate/Eliminations", "Corporate/Other", "Corporate_&_Eliminations",
                       "Corporate_&_other", "Corporate_&_Other", "Corporate_/_Eliminations", "Corporate_/_ELiminations", "Corporate_Activities", "Corporate_Adjustments/Eliminations",
                       "Corporate_and_eliminations", "Corporate_and_other", "Corporate_and_Other", "Corporate_and_others", "Corporate_and_Others", "Corporate_Centre",
                       "Corporate_Center", "Corporate_Eliminations_&_Other", "Corporate_Expense", "Corporate_Expense/Other", "Corporate_Expenses/Other",
                       "Corporate_net_Financing_charges_and_tax", "Cosolidate_Adj", "Currency_Hedges", "Deferred_tax_assets", "E&P", "Elimination",
                       "Elimination_adjustments", "Eliminations", "Eliminations/Corporate", "Eliminations/Other", "Eliminations_and_Other", "Eliminations_&_Other",
                       "Elininations", "Eliminations_and_Corporate", "Eliminations_Generic", "Eliminations_Group", "Enterprise", "Foreign_exchange",
                       "Financial_Services_SFS", "General_Corporate", "Goodwill", "Head_Office", "Head_Office_Costs", "Head_Offices",
                       "Head_OfficeS", "Headoffice", "Headquarter/Consolidation", "Holdings", "Holdings_and_Other_Activites", "IFRS_2_charges",
                       "IFRS_5_reclassification", "Impairment_&_Restructuring_Charges_&_Acquisition_Costs", "Impairments_and_other_charges",
                       "Industrial_Services_and_Other", "Infrastructure_DevelopmentOther_Areas_&_Other", "Infrastructure_DevelopmentUS",
                       "Infrastructure_related_business", "Intangible_assets", "Intangible_Assets", "Intercompany_Eliminations", "Intergeographic_sales", "Internal_Eliminations", "Intersegment", "Internal_Tansacion",
                       "Internal_Sales", "Intersegment_Balances", "Intersegment_Elimination", "Intersegment_eliminations", "Intersegment_Eliminations",
                       "Intersegment_Eliminations_and_Other", "Intragroup", "INVESTMENTS_IN_ASSOCIATES", "Less_:_Equity_Accounted_Investments", "Investments_&_Other",
                       "Lifeco_Corporate", "Logistics_&_Other", "Market_Optimization", "Midstream", "No_Allocated", "Non__Beverage", "Non_allocated", "Non_Operating_Assets/Liabilities", "NonAllocated",
                       "Noncontrolling_interest_and_redeemable_noncontrolling", "Nonscheduled_Services/Incidental_Revenue", "Not_allocated", "NonDistributed",
                       "Not_Allocated", "OEM", "Oil_Sands_Mining_&_Upgrading", "Operating_Expenses_&_Indirect_Costs_of_Net_Revenues",
                       "Other/Consolidation", "Other/Elimination", "Other/Licensing", "Other/Unallocated", "Other_Eliminations", "Other_income_and_expense_net",
                       "Other_reconciling_items", "Other_unallocated", "Other_Unallocated", "Other_unallocated_assets", "Others_&_Unallocated",
                       "Others_eliminations_and_adjustments", "Pensions_and_savings", "Pet_Nutrition", "Product_Category_Operating_Segments", "Raw_Materials_&_Other",
                       "Reconciliation", "Reconciliation_Items", "Reconcilation_items", "Reconcillation", "Reconcilliations", "Restructuring_Activities", "Right_Management",
                       "Rounding_Adjustment", "Share_in_joint_venture", "Shared_assets", "Shared_services", "Specialty", "Standard_Bank_International",
                       "Standard_Life_Investments", "Stuart_Weitzman", "Transfer_Intercompany", "Unallocable_Corporate_Items", "Unallocated_and_Eliminations", "Unallotcated",
                       "Un_allocated/Eliminations", "unalloacted", "Unallocable", "unallocated", "Unallocated",
                       "Unallocated/Other", "Unallocated_/_IntraGroup", "Unallocated_assets", "Unallocated_Assets", "Unallocated_corporate_costs", "Unallocated_Corporate_Costs",
                       "Unallocated_Costs", "Unallocated_Items", "unallocated_Items", "Undetermined_Geography", "Unlocated", "Z_country")

domestic_regions <- c("Country_of_domicile", "Domestic", "Domestic_and_Canada_RCs", "Domestic_Market", "Domestic_Markets", "Domestic_Operation",
                      "Domestic_Operations", "Domestic_Sales", "Domestic_streaming_&_DVD", "Domestic_Tariff", "Gas__Domestic", "Local",
                      "Local/_Domestic", "Local__South_Africa", "Long_Canyon_&_Other", "National_Market", "North", "South", "West",
                      "Southern", "Eastern", "Western", "Central", "Northern")

company_geog_data2 <- company_geog_data %>%
  gather(key = "region_num", value = "region", (region_name_1:region_name_10)) %>% # Gather region names
  mutate(region_num = as.numeric(gsub("\\D+", "", region_num))) %>% #Number regions from 1 to 10 based on existing categories
  select(company_id:revenue, region_num, region, everything()) %>%
  gather(key = "region_revenue_num", value = "region_revenue", (region_revenue_1:region_revenue_10)) %>% # Gather region revenue
  mutate(region_revenue_num = as.numeric(gsub("\\D+", "", region_revenue_num))) %>% #Number regional revenue from 1 to 10 to match region name numbering above
  filter(region_num == region_revenue_num) %>% #Filter out redundant rows
  select(-region_revenue_num) %>% # Now matches segment_num exactly, for every row
  
  # Replace spaces and other characters in region names
  mutate(region = gsub(" ", "_", region),
         region = gsub(",", "", region),
         region = gsub("'", "", region),
         region = gsub("-", "", region),
         region = gsub("\\(", "", region),
         region = gsub("\\)", "", region)) %>%
  
  # Replace redundant regions with NA
  mutate(region_revenue = case_when(region %in% redundant_regions ~ NA_real_,
                                    TRUE ~ region_revenue))

# Companies which have missing values for all region name and region revenue fields
missing_geog_data <- company_geog_data2 %>%
  group_by(company_id, company) %>% # Identify missing value and name regions
  mutate(region_na = ifelse(is.na(region) | is.na(region_revenue), 1, 0)) %>% # Missing value indicator functions
  summarise(region_na_count = sum(region_na)) %>%
  filter(region_na_count == 10) %>%
  select(-starts_with("region"))

# Assume all revenue is global for companies which have no data
missing_geog_data2 <- missing_geog_data %>%
  left_join(company_geog_data2) %>%
  filter(region_num == 1) %>%
  mutate(region = "World",
         region_revenue = revenue)

save_dated(missing_geog_data2, "Missing_geog_exposure_companies", folder = "Interim", csv = TRUE)

# Domestic companies list for Excel analysis (already analysed, but kept for posterity)
# Note that this code does not need to be run to obtain final outputs
domestic_geog_data <- company_geog_data2 %>%
  filter(region %in% domestic_regions)

save_dated(domestic_geog_data, "Domestic_geog_exposure_companies", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Replace domestic and foreign with actual region names based on Dexter's analysis (07/18) and
#####             fill in 'World' for companies with no geographic exposure data (assume global)

company_geog_data3 <- company_geog_data2 %>%
  left_join(domestic_geog_clean_data) %>%
  mutate(region = ifelse(!is.na(region_new), region_new, region)) %>%
  select(-region_new) %>%
  filter(!(company_id %in% missing_geog_data2$company_id)) %>%
  bind_rows(missing_geog_data2)

# Sum over any duplicate (correctly entered regions) for each company
company_geog_data4 <- company_geog_data3 %>%
  group_by(company_id, company, region) %>%
  filter(!is.na(region) & !is.na(region_revenue)) %>%
  summarise(revenue = mean(revenue), #This changes nothing but stops summarise removing revenue
            region_num = mean(region_num), # This changes nothing but stops summarise removing segment_num
            region_revenue = sum(region_revenue)) %>%
  ungroup()

save_dated(company_geog_data4, "Geog_exposure_full_data", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Create list of unique geographic segment entries for analysis in 'Region ISO code matching' file

unique_geographies_data <- tibble(region = unique(company_geog_data4$region))

save_dated(unique_geographies_data, "Unique_geogs_data", folder = "Interim", csv = TRUE)
