##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  11/02/2019
##### Code author:        Justine Schafer
##### Edited by:          Shyamal Patel
##### Description:        This script reads in FTSE Russell green revenue, Thomson Reuters financial data, and the results from Orbis green IP data cleaning
#####                     and builds a complete green IP / revenue dataset for later green upside modelling
##### Dependencies:       1.  ADD
#####                     Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "3_ESG/3d_Cleantech"
source("utils.R")

# Read in cleaned green IP data
patent_data <- readRDS("3_ESG/3d_Cleantech/Output/Cleantech_patent_VE_category_results.rds")

# Read in FTSE GR full universe best estimates data file (Best Estimates all)
rev_best_est_data <- read_excel(input_source("Green_Revenues_Full_Universe_Hist_+_Best_Estimate_180618.xlsx"),
                                sheet = "BestEstimatesAll", range = "$A$2:$O$2898")

# Read in FTSE GR full universe history data file (Green Revenues history)
rev_hist_est_data <- read_excel(input_source("Green_Revenues_Full_Universe_Hist_+_Best_Estimate_180618.xlsx"),
                                sheet = "GreenRevenuesHistory", range = "$A$4:$M$25773")

# Read in FTSE GR EQ + EG breakdown data file
rev_eq_eg_data <- read_excel(input_source("Companies_EQ+EG.xlsx"),
                             sheet = "CompaniesEQ+EG")

# Read in Vivid - FTSE Russell ISIN code matching for companies which were missing from FTSE Russell database
ftse_names_missing <- read_excel(input_source("TR_MASTER_to_Green_Revenues_SP.xlsx"),
                                 sheet = "Sheet2", range = "$A$1:$E$8")

# Read in Vivid - FTSE GR category matching
ve_ftse_rev_categories <- read_excel(input_source("VE_FTSE_category_matching.xlsx"),
                                   sheet = "Sheet1", range = "$A$1:$D$9")

# Read in company unique names list
company_names <- readRDS("2_Financial/2a_Preliminary/Output/Companies_list.rds")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Rename FTSE GR dataset variables in preparation for further data cleaning and merging ----

# Green revenue full universe best estimates
rev_best_est_data2 <- rev_best_est_data %>%
  rename(ftse_id = FTSE_ID,
         ftse_merge_isin_code = ISIN,
         country_of_listing = `Country Name`,
         market_cap = `Market Cap USD(m)`,
         total_market_cap = `Total Market Cap USD(m)`,
         ftse_company = Company,
         icb_industry = `ICB Industry`,
         icb_supersector = `ICB Supersector`,
         gr_status = `GR Status`,
         gr_sectors = `GR Sectors`,
         gr_subsegments = `GR Subsegments`,
         estimate_type = `Estimate Type`,
         estimate_date = `Estimate Date`,
         gr_percent = `GR Best Estimate (%)`) %>%
  select(-country_of_listing, -market_cap, -total_market_cap) %>%
  select(ftse_merge_isin_code, ftse_company, estimate_date, everything())

# Green revenue EG and EQ estimates
rev_eq_eg_data2 <- rev_eq_eg_data %>%
  rename(ftse_company = Name,
         year = Title) %>%
  rename_at(vars(everything()), funs(tolower(.))) %>%
  rename(SEDOL = sedol) %>%
  filter(!is.na(gr_subsector)) %>%
  select(ftse_company, year, everything())

# Green revenue full universe historical estimates
rev_hist_est_data2 <- rev_hist_est_data %>%
  rename(ftse_id = FTSE_ID,
         ftse_company = Name,
         estimate_date = Title,
         gr_subsector = GRCS_SubSectors) %>%
  select(ftse_id, ftse_company, SEDOL, estimate_date, gr_subsector)
           
# Missing FTSE company names
ftse_names_missing2 <- ftse_names_missing %>%
  select(ISIN_code, FTSE_ISIN) %>%
  rename(vivid_merge_isin_code = ISIN_code,
         ftse_merge_isin_code = FTSE_ISIN)

# Vivid - FTSE green revenue category matching
ve_ftse_rev_categories2 <- ve_ftse_rev_categories %>%
  rename(gr_subsector = FTSE_code,
         ve_category = VE_category) %>%
  select(-contains("FTSE"))

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Create cleantech revenue company names dataframe for later use ----

# Add merge variable to the uniqe company names / IDs dataset and combine with green revenue full univerrse best estimates data
cleantech_rev_companies <- company_names %>%
  mutate(vivid_merge_isin_code = case_when(equity_isin_code_1 %in% ftse_names_missing2$vivid_merge_isin_code ~ equity_isin_code_1,
                                           equity_isin_code_2 %in% ftse_names_missing2$vivid_merge_isin_code ~ equity_isin_code_2,
                                           equity_isin_code_3 %in% ftse_names_missing2$vivid_merge_isin_code ~ equity_isin_code_3,
                                           TRUE ~ NA_character_)) %>%
  left_join(ftse_names_missing2, by = "vivid_merge_isin_code") %>%
  mutate(ftse_merge_isin_code = case_when(!is.na(ftse_merge_isin_code) ~ ftse_merge_isin_code,
                                          equity_isin_code_1 %in% rev_best_est_data2$ftse_merge_isin_code ~ equity_isin_code_1,
                                          equity_isin_code_2 %in% rev_best_est_data2$ftse_merge_isin_code ~ equity_isin_code_2,
                                          equity_isin_code_3 %in% rev_best_est_data2$ftse_merge_isin_code ~ equity_isin_code_3,
                                          TRUE ~ NA_character_)) %>%
  left_join(rev_best_est_data2, by = "ftse_merge_isin_code", na_matches = "never") %>%
  select(company_id, company, ftse_id, ftse_company, SEDOL, vivid_merge_isin_code, ftse_merge_isin_code) %>%
  group_by(company_id) %>%
  # Keep only one FTSE ID code when there are multiple FTSE matches for a unique NZT company ID code (note that underlying data should be the same)
  # This is just to avoid double counting green revenue for these companies
  arrange(company_id, ftse_id) %>%
  filter(row_number() == 1)

# Save matching of NZT company names to cleantech revenue company names (used for results mapping later)
cleantech_rev_companies2 <- cleantech_rev_companies %>%
  select(company_id, company, ftse_id, ftse_company)

save_dated(cleantech_rev_companies2, "FTSE_rev_companies_list", folder = "Output", csv = TRUE)

# Save NZT company names missing from cleantech revenue companies list
no_cleantech_rev_companies <- cleantech_rev_companies %>%
  select(company_id, company, ftse_id, ftse_company) %>%
  filter(is.na(ftse_id))

save_dated(no_cleantech_rev_companies, "FTSE_rev_missing_companies_list", folder = "Output", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Merge together green revenue datasets ----

cleantech_rev_data <- cleantech_rev_companies %>%
  left_join(rev_best_est_data2, by = c("ftse_merge_isin_code", "ftse_company", "ftse_id", "SEDOL"), na_matches = "never") %>%
  select(-vivid_merge_isin_code, -ftse_merge_isin_code) %>%
  # Remove no data companies
  filter(!is.na(ftse_id)) %>%
  left_join(rev_eq_eg_data2, by = c("ftse_company", "SEDOL")) %>%
  # Replace ER05 with Lithium if in description, and remove ER05 from dataset otherwise
  mutate(gr_subsector = case_when(gr_subsector == "ER05" & grepl("lithium", description) ~ "ER05_Lithium",
                                  TRUE ~ gr_subsector)) %>%
  # Remove years which do not match between datasets
  filter(estimate_date == year | is.na(gr_subsector)) %>%
  # Add temporary FTSE_ID and year column
  mutate(indicator = paste(ftse_id, estimate_date))

# Separate dataset for gr subsectors which are missing in the above
cleantech_rev_na_data <- cleantech_rev_data %>%
  filter(is.na(gr_subsector)) %>%
  select(-gr_subsector, -indicator)

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Clean IP data ----

patent_data2 <- patent_data %>%
  rename(ve_category = VE_category) %>%
  # Replace EV batteries and EVs categories with 'EV_aggregate'
  mutate(ve_category = case_when(ve_category %in% c("EVs", "EV_batteries") ~ "EV_aggregate",
                                 TRUE ~ ve_category)) %>%
  group_by(company_id, company, bvd_id, bvd_name, ve_category) %>%
  summarise(Total_AllIPC = as.integer(mean(Total_AllIPC, na.rm = TRUE)),
            Total_OnlyMainIPC = as.integer(mean(Total_OnlyMainIPC, na.rm = TRUE)),
            AllIPC = sum(AllIPC, na.rm = TRUE),
            OnlyMainIPC = sum(OnlyMainIPC, na.rm = TRUE)) %>%
  ungroup()

patent_data_VE_cats <- patent_data2 %>%
  filter(!is.na(bvd_id)) %>%
  select(company_id, company, ve_category, AllIPC, OnlyMainIPC)

patent_data_totals <- patent_data2 %>%
  filter(!is.na(bvd_id)) %>%
  select(company_id, company, Total_AllIPC, Total_OnlyMainIPC) %>%
  unique()

#--------------------------------------------------------------------------------------------------

##### SECTION 6 - Merge in 'historical' green revenue dataset ----
#####             (contains list of green sectors for companies where green revenue decomposition is not possible)

# Reshape historical green revenue data
rev_hist_est_data3 <- rev_hist_est_data2 %>%
  mutate(gr_subsector = str_split(gr_subsector, ", ")) %>%
  unnest(gr_subsector) %>%
  mutate(indicator = paste(ftse_id, estimate_date)) %>%
  filter(indicator %in% cleantech_rev_data$indicator) %>%
  select(-indicator, -estimate_date) %>%
  # Join in company names list
  left_join(cleantech_rev_companies2, by = c("ftse_id", "ftse_company")) %>%
  # Join in NA companies list and get rid of these observations
  left_join(cleantech_rev_na_data) %>%
  filter(!is.na(estimate_date))

# Merge all datasets together
cleantech_rev_data2 <- cleantech_rev_data %>%
  filter(!is.na(gr_subsector)) %>%
  # Remove no longer needed indicator function
  select(-indicator) %>%
  bind_rows(rev_hist_est_data3)

# Merge in Vivid category datasets and clean up data
cleantech_rev_data3 <- cleantech_rev_data2 %>%
  mutate(`revenue%` = ifelse(is.na(`revenue%`), 0, `revenue%`)) %>%
  left_join(ve_ftse_rev_categories2, by = "gr_subsector") %>%
  left_join(patent_data_VE_cats, by = c("company_id", "company", "ve_category")) %>%
  left_join(patent_data_totals, by = c("company_id", "company")) %>%
  select(company, year, gr_subsector, `revenue%`, ve_category, gr_percent, everything()) %>%
  mutate(relevance_check = ifelse(!is.na(ve_category), 1, 0)) %>%
  group_by(company_id) %>%
  mutate(relevance_check = sum(relevance_check, na.rm = TRUE)) %>%
  filter(relevance_check > 0) %>% 
  arrange(.by_group = TRUE, Total_AllIPC) %>%
  fill(Total_AllIPC, Total_OnlyMainIPC) %>%
  mutate(ve_category = ifelse(is.na(ve_category), "Other", ve_category))

  
# Create 'Other' category for patents that are not assigned within VE/FTSE match
# Is this really necessary? Have preserved it as I haven't unpicked the subsequent code cleaning and don't want
# to create issues - 'other' is not well defined
cleantech_rev_other <- cleantech_rev_data3 %>%
  select(company_id, company, ve_category, AllIPC:Total_OnlyMainIPC) %>%
  unique() %>%
  group_by(company_id) %>%
  mutate(AllIPC = case_when(ve_category == "Other" ~ Total_AllIPC - sum(AllIPC, na.rm = TRUE),
                            TRUE ~ AllIPC),
         OnlyMainIPC = case_when(ve_category == "Other" ~ Total_OnlyMainIPC - sum(OnlyMainIPC, na.rm = TRUE),
                                 TRUE ~ OnlyMainIPC)) %>%
  ungroup()

# Merge in using 'other' constructed dataset
cleantech_rev_data4 <- cleantech_rev_data3 %>%
  select(-relevance_check, -AllIPC, -OnlyMainIPC, -Total_AllIPC, -Total_OnlyMainIPC) %>%
  left_join(cleantech_rev_other, by = c("company_id", "company", "ve_category"))

#--------------------------------------------------------------------------------------------------

##### SECTION 7 - Define temp dataset and go through each data cleaning case (see list below) ----
##### Have not tried to clean this - can we simplify the approach at some stage ?!

temp <- cleantech_rev_data4 %>%
  group_by(company_id) %>%
  mutate(gr_percent = gr_percent / 100, 
         sum_revenue_pct = sum(unique(`revenue%`), na.rm = TRUE),
         IPC_data_check = ifelse(!is.na(Total_AllIPC), 1, 0),
         IPC_data_check = sum(IPC_data_check, na.rm = TRUE),
         non_computables = case_when(ve_category != "Other" & `revenue%` == 0 & sum_revenue_pct > 0 &
                                       is.na(Total_AllIPC) ~ 1,
                                     TRUE ~ 0),
         non_computables = sum(non_computables, na.rm = TRUE), 
         no_of_ve_cat = case_when(ve_category != "Other" ~ 1, TRUE ~ 0), 
         no_of_ve_cat = sum(no_of_ve_cat)) %>%
  select(company, year, gr_subsector, `revenue%`, ve_category, gr_percent,
         non_computables, no_of_ve_cat, everything()) %>%
  ungroup()

# List of cases a company's data can fall into
# A. No Revenue data and no IPC data ~ equally allocate GR percent between line items.
# 1. No Revenue data but some/all IPC data for VE categories ~ AllIPC/Total_AllIPC * GR_percent
# B. Some Revenue data but not for VE categories, no IPC data ~ equally allocate the remainder of GR percent to remaining line items.
# 2. Some Revenue data and some IPC data. Here we construct three estimates, in the end picking the best one. 
# Orbis_hat_1 is AllIPC/Total_AllIPC * GR_percent. 
# Orbis_hat_2 is constructed if Orbis_hat_1 is greater than the sum of Revenue% for that VE_category. If the reverse is the case, Orbis_hat_1 is the BE.
# Orbis_hat_2 constructs a remainder of GR_percent and Patent data based on which Orbis_hat_1 are kept, and allocates these again according to patent share.
# FTSE_hat is constructed if both Orbis_hat_1 and Orbis_hat_2 are not available (due to missing patent data) and 
#     equally allocates whatever Revenue% has not yet been assigned across line items.

# Case A ------------------------------------------------------------------
# A. No green revenue by category data and no IPC data ~ equally allocate overall GR percent between GR Subsectors

Case_A <- temp %>%
  filter(sum_revenue_pct == 0 & IPC_data_check == 0) %>%
  group_by(company_id) %>%
  mutate(count = sum(ifelse(!is.na(gr_subsector), 1, 0)),
         ve_category_rev_share = 1 / count * gr_percent,
         ve_category_rev_share = ifelse(ve_category_rev_share < 0, 0, ve_category_rev_share),
         CHECK = gr_percent - sum(ve_category_rev_share)) %>%
  ungroup() %>%
  mutate(Case = "A") %>%
  select(company_id, company, year, ve_category, ve_category_rev_share, gr_percent, `revenue%`,
         everything(), -CHECK, -count)

# Case B ------------------------------------------------------------------
# B. Some Revenue data but not for VE categories, no IPC data ~ equally allocate the remainder of GR percent to remaining line items.

Case_B <- temp %>% 
  filter(non_computables != 0, non_computables == no_of_ve_cat) %>%
  group_by(company_id) %>% 
  mutate(count = sum(ifelse(`revenue%` == 0, 1, 0)),
         ve_category_rev_share = case_when(`revenue%` == 0 ~ 1 / count * (gr_percent - sum(`revenue%`, na.rm = TRUE)),
                                           `revenue%` != 0 ~ `revenue%`,
                                           TRUE ~ NA_real_),
         ve_category_rev_share = ifelse(ve_category_rev_share < 0, 0, ve_category_rev_share),
         CHECK = gr_percent - sum(ve_category_rev_share),
         ve_category_rev_share = case_when(CHECK < 0 ~ (`revenue%` / sum(`revenue%`)) * gr_percent,
                                           TRUE ~ ve_category_rev_share),
         CHECK = gr_percent - sum(ve_category_rev_share)) %>%
  ungroup() %>%
  mutate(Case = "B") %>%
  select(company_id, company, year, ve_category, ve_category_rev_share, gr_percent, `revenue%`,
         everything(), -CHECK, -count)

# Case 1 ------------------------------------------------------------------
# 1. No Revenue data but some/all IPC data for VE categories ~ AllIPC/Total_AllIPC * GR_percent

Case_1 <- temp %>% 
  filter(sum_revenue_pct == 0, IPC_data_check > 0) %>%
  group_by(company_id, ve_category) %>% 
  mutate(No_of_items_in_cat = n(),
         ve_category_rev_share = (1 / No_of_items_in_cat) * (AllIPC / Total_AllIPC) * gr_percent) %>% 
  ungroup() %>%
  group_by(company_id) %>%
  mutate(No_of_SubSectors = n(),
         ve_category_rev_share = ifelse(No_of_SubSectors == 1, gr_percent, ve_category_rev_share),
         sum_ve_category_AllIPC = sum(ifelse(ve_category != "Other", AllIPC, NA), na.rm = TRUE),
         CHECK_other = sum(ifelse(ve_category == "Other", 1, 0)),
         ve_category_rev_share = ifelse(CHECK_other == 0, 
                                        (1 / No_of_items_in_cat) * (AllIPC / sum_ve_category_AllIPC) * gr_percent, 
                                        ve_category_rev_share),
         CHECK = sum(ve_category_rev_share, na.rm = TRUE),
         CHECK2 = gr_percent - CHECK,
         CHECK2 = sum(CHECK2, na.rm = TRUE),
         FILTER = sum(ifelse(is.na(ve_category_rev_share), 1, 0))) %>%
  select(company_id, company, year, ve_category, ve_category_rev_share, gr_percent, `revenue%`, CHECK,
         everything(), -CHECK2, -CHECK_other)

temp_1 <- Case_1 %>%
  filter(FILTER > 0, is.na(ve_category_rev_share)) %>%
  mutate(ve_category_rev_share = 0) 

Case_1_2 <- Case_1 %>%
  filter(!is.na(ve_category_rev_share)) %>%
  bind_rows(temp_1) %>%
  ungroup() %>% 
  mutate(Case = "1") %>%
  select(-CHECK, -No_of_SubSectors, -FILTER, -No_of_items_in_cat, -sum_ve_category_AllIPC)

# Case 2 ------------------------------------------------------------------
# 2. Some Revenue data and some IPC data. Here we construct three estimates, in the end picking the best one. 
# Orbis_hat_1 is AllIPC/Total_AllIPC * GR_percent. 
# Orbis_hat_2 is constructed if Orbis_hat_1 is greater than the sum of Revenue% for that VE_category. If the reverse is the case, Orbis_hat_1 is the best estimate.
# Orbis_hat_2 constructs a remainder of GR_percent and Patent data based on which Orbis_hat_1 are kept, and allocates these again according to patent share.
# FTSE_hat is constructed if both Orbis_hat_1 and Orbis_hat_2 are not available (due to missing patent data) and 
# equally allocates whatever Revenue% has not yet been assigned across FTSE GR subsector categories.

Case_2 <- temp %>%
  filter(non_computables != 0 & non_computables != no_of_ve_cat |
           non_computables == 0 & sum_revenue_pct>0) %>%
  group_by(company_id) %>%
  mutate(IPC_data_check = sum(AllIPC, na.rm = TRUE),
         sum_of_rev_pct = sum(`revenue%`),
         ve_category_rev_pct = ifelse(`revenue%` != 0, `revenue%`, NA)) %>%
  group_by(company_id, ve_category) %>% 
  mutate(sum_of_ve_rev_pct = sum(ve_category_rev_pct, na.rm=TRUE),
         No_of_items_in_cat = n(),
         AllIPC = ifelse(!is.na(Total_AllIPC) & is.na(AllIPC), 0, AllIPC),
         OnlyMainIPC = ifelse(!is.na(Total_OnlyMainIPC) & is.na(OnlyMainIPC), 0, OnlyMainIPC)) %>%
  group_by(company_id) %>% 
  mutate(Orbis_hat_1 = AllIPC / Total_AllIPC * gr_percent,
         Orbis_hat_2 = case_when(sum_of_ve_rev_pct >= Orbis_hat_1 ~ sum_of_ve_rev_pct,
                                 !is.na(sum_of_ve_rev_pct) & is.na(Orbis_hat_1) & !is.na(Total_AllIPC) ~ sum_of_ve_rev_pct,
                                 TRUE ~ NA_real_),
         Orbis_hat_2 = ifelse(Orbis_hat_2 > gr_percent, gr_percent, Orbis_hat_2),
         Remaining_AllIPC = ifelse(!is.na(Orbis_hat_2), Total_AllIPC - AllIPC, NA),
         Remaining_AllIPC = ifelse(sum(unique(Remaining_AllIPC), na.rm = TRUE) != 0,
                                   sum(unique(Remaining_AllIPC), na.rm = TRUE), NA)) %>%
  mutate(Remaining_gr = gr_percent - sum(unique(Orbis_hat_2), na.rm = TRUE),
         Orbis_hat_2 = case_when(is.na(Orbis_hat_2) ~ AllIPC / Remaining_AllIPC * Remaining_gr,
                                 TRUE ~ Orbis_hat_2))

CHECK_2 <- Case_2 %>%
  select(company_id, company, ve_category, Orbis_hat_1, Orbis_hat_2) %>%
  group_by(company_id) %>%
  unique() %>% 
  mutate(CHECK = case_when(!is.na(Orbis_hat_2) ~ sum(Orbis_hat_2, na.rm = TRUE),
                           is.na(Orbis_hat_2) & !is.na(Orbis_hat_1) ~ sum(Orbis_hat_1, na.rm = TRUE),
                           TRUE ~ NA_real_)) 
Case_2_2 <- Case_2 %>% 
  left_join(CHECK_2) %>%
  fill(CHECK) %>%
  select(company_id, company, year, ve_category, sum_of_ve_rev_pct, gr_percent, CHECK, Orbis_hat_1, Orbis_hat_2,
         `revenue%`, Remaining_AllIPC, Remaining_gr, everything()) 

Case_2_b <- Case_2_2 %>%
  filter(is.na(Orbis_hat_1) & is.na(Orbis_hat_2)) %>%
  mutate(FTSE_hat = case_when (!is.na(`revenue%`) ~ `revenue%`, TRUE ~ NA_real_),
         count_1 = ifelse(`revenue%` == 0, 1, 0)) %>%
  group_by(company_id, ve_category) %>%
  mutate(count_2 = sum(count_1)) %>%
  ungroup() 

CHECK_2_b <- Case_2_b %>%
  select(company_id, company, ve_category, FTSE_hat) %>%
  group_by(company_id) %>%
  unique() %>% 
  mutate(CHECK = sum(FTSE_hat, na.rm = TRUE))

Case_2_b_2 <- Case_2_b %>%
  select(-CHECK) %>%
  left_join(CHECK_2_b) %>%
  group_by(company_id) %>%
  mutate(count_1 = sum(count_1, na.rm = TRUE),
         FTSE_hat = ifelse(`revenue%` == 0, count_2 / count_1 * (gr_percent - CHECK), FTSE_hat)) 

temp_2 <- Case_2_b_2 %>%
  select(company_id, company, ve_category, FTSE_hat) %>%
  unique() %>%
  mutate(CHECK2 = sum(FTSE_hat, na.rm = TRUE))

Case_2_b_3 <- Case_2_b_2 %>%
  left_join(temp_2) %>%
  ungroup() %>%
  mutate(FTSE_hat = ifelse(abs(gr_percent - CHECK2) > 0.0000001,
                           `revenue%`/sum_of_rev_pct * gr_percent,
                           FTSE_hat)) %>%
  select(company:gr_percent, CHECK, CHECK2, FTSE_hat, everything(), -count_1, -count_2)

Case_2_3 <- Case_2_2 %>%
  filter(!is.na(Orbis_hat_1) | !is.na(Orbis_hat_2) | CHECK == gr_percent) %>%
  mutate(FTSE_hat = NA,
         CHECK2 = NA) %>%
  select(company:CHECK, CHECK2, Orbis_hat_1, Orbis_hat_2, FTSE_hat, everything()) %>%
  bind_rows(Case_2_b_3) %>%
  mutate(ve_category_rev_share = case_when(!is.na(Orbis_hat_2) ~ Orbis_hat_2,
                                           is.na(Orbis_hat_2) ~ ifelse(!is.na(Orbis_hat_1), Orbis_hat_1, FTSE_hat),
                                           TRUE ~ NA_real_)) %>%
  mutate(Case = "2") %>%
  group_by(company_id, ve_category) %>%
  mutate(No_of_items_in_cat = n(),
         ve_category_rev_share = (1 / No_of_items_in_cat) * ve_category_rev_share) %>%
  ungroup() %>%
  select(company_id, company, year, ve_category, ve_category_rev_share, gr_percent, `revenue%`,
         everything(), -Orbis_hat_1, -Orbis_hat_2, -FTSE_hat, -Remaining_AllIPC, -Remaining_gr,
         -sum_of_rev_pct, -CHECK, -CHECK2, -ve_category_rev_pct, -sum_of_rev_pct, -sum_of_ve_rev_pct,
         -No_of_items_in_cat)

# Combine case-level datasets to produce final estimates of green revenue percentage by company ---------------------------------------------------

cleantech_rev_results <- Case_2_3 %>% 
  bind_rows(Case_1_2, Case_A, Case_B) %>%
  select(-IPC_data_check) %>%
  group_by(company_id) %>%
  mutate(lithium_adj = sum(ifelse(gr_subsector == "ER05_Lithium", 1, 0))) %>%
  ungroup()

lithium_adjustment <- cleantech_rev_results %>%
  filter(lithium_adj > 0) %>%
  group_by(company_id) %>%
  mutate(count = sum(ifelse(ve_category_rev_share > 0 | ve_category == "Minerals_for_batteries", 1, 0)),
         ve_category_rev_share_adj = ifelse(ve_category == "Minerals_for_batteries", (1 / count) * gr_percent, NA),
         ve_category_rev_share_adj = ifelse(is.na(ve_category_rev_share_adj), (ve_category_rev_share / gr_percent) * 
                                              gr_percent * (count - 1) / count, ve_category_rev_share_adj)) %>%
  select(-ve_category_rev_share, -count) %>%
  rename(ve_category_rev_share = ve_category_rev_share_adj) %>%
  ungroup()

cleantech_rev_results2 <- cleantech_rev_results %>%
  filter(lithium_adj == 0) %>%
  bind_rows(lithium_adjustment) %>%
  group_by(company_id) %>%
  mutate(Total_ve_cat_rev_share = sum(ve_category_rev_share, na.rm = TRUE),
         Calc_check = case_when(Total_ve_cat_rev_share - gr_percent > 0.0001 ~ 1,
                                TRUE ~ 0)) %>%
  select(company:gr_percent, Total_ve_cat_rev_share, Calc_check, Case, everything())

companies_before <- unique(temp$company)
companies_after <- unique(cleantech_rev_results2$company)

save_dated(cleantech_rev_results2, "Cleantech_patent_and_rev_data", folder = "Output", csv = TRUE)