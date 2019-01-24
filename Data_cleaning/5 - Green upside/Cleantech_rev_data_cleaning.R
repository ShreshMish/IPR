##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  29/08/2018
##### Code author:        Justine Schafer
##### Edited by:          Shyamal Patel
##### Description:        This script reads in FTSE Russell green revenue, Thomson Reuters financial data, and the results from Orbis green IP data cleaning
#####                     and builds a complete green IP / revenue dataset for later green upside modelling
##### Dependencies:       1.  Latest Thomson Reuters cleaned dataset: "1 - Financial prelim/Output/TR_cleaned_2016USD_data.xlsx"
#####                     2.  Add
#####                     Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA ----

packages <- c("tidyverse", "magrittr", "readxl", "here", "stringr", "stringi")
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
  main_path <- paste0("5 - Green upside/", folder)
  dated_path <- paste0("5 - Green upside/", folder, "/Dated/")
  
  # Save main file
  saveRDS(data, here(main_path, paste0(name, ".rds")))
  # Save dated file (optional)
  if(dated) {saveRDS(data, here(dated_path, paste0(date, "_", name, ".rds")))}
  # Save dated CSV file (optional)
  if(csv) {write_csv(data, here(dated_path, paste0(date, "_", name, ".csv")))}
}

# Read in cleaned Thomson Reuters financial dataset
financial_data <- readRDS("1 - Financial prelim/Output/TR_cleaned_2016USD_data.rds")

# Read in cleaned green IP data
green_IP_data <- readRDS("5 - Green upside/Output/Green_IP_VE_category_results.rds")

# Read in FTSE GR full universe best estimates data file (Best Estimates all)
green_rev_best_est_data <- read_excel("5 - Green upside/Input/Green Revenues Full Universe Hist + Best Estimate_180618.xlsx",
                                            sheet = "BestEstimatesAll", range = "$A$2:$O$2898")

# Read in FTSE GR full universe history data file (Green Revenues history)
green_rev_hist_est_data <- read_excel("5 - Green upside/Input/Green Revenues Full Universe Hist + Best Estimate_180618.xlsx",
                                      sheet = "GreenRevenuesHistory", range = "$A$4:$M$25773")

# Read in FTSE GR EQ + EG breakdown data file
green_rev_eq_eg_data <- read_excel("5 - Green upside/Input/Companies EQ+EG.xlsx",
                                   sheet = "CompaniesEQ+EG")

# Read in Vivid - FTSE Russell ISIN code matching for companies which were missing from FTSE Russell database
ftse_names_missing <- read_excel("5 - Green upside/Input/180808 TR MASTER to Green Revenues_SP.xlsx",
                                 sheet = "Sheet2", range = "$A$1:$E$8")

# Read in Vivid - FTSE GR category matching
ve_gr_rev_categories <- read_excel("5 - Green upside/Input/VE FTSE category matching.xlsx",
                                   sheet = "Sheet1", range = "$A$1:$D$9")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean FTSE GR datasets and merge together companies list ----

# Green revenue full universe best estimates
green_rev_best_est_data %<>%
  rename(ISIN_code = ISIN,
         country_of_listing = `Country Name`,
         market_cap = `Market Cap USD(m)`,
         company = Company,
         ICB_industry = `ICB Industry`,
         ICB_supersector = `ICB Supersector`,
         GR_status = `GR Status`,
         GR_sectors = `GR Sectors`,
         GR_subsegments = `GR Subsegments`,
         estimate_type = `Estimate Type`,
         estimate_date = `Estimate Date`,
         GR_percent = `GR Best Estimate (%)`) %>%
  select(company, SEDOL, estimate_date, everything())

# Green revenue full universe historical estimates
green_rev_hist_est_data %<>%
  rename(company = Name,
         estimate_date = Title)

# Green revenue EG and EQ estimates
green_rev_eq_eg_data %<>%
  filter(!is.na(GR_SubSector)) %>%
  rename(company = Name,
         year = Title) %>%
  select(company, year, everything())

# Thomson Reuters financial data
financial_data %<>%
  rename(tr_company = company,
         VE_ISIN = ISIN_code) %>%
  select(tr_company, VE_ISIN)

# Merge together companies list using Mark's missing companies gap filling
tr_company_list <- financial_data %>%
  left_join(ftse_names_missing) %>%
  mutate(Company = ifelse(is.na(Company), tr_company, Company),
         Is_FTSE_Subsidiary = ifelse(is.na(Is_FTSE_Subsidiary), "No", Is_FTSE_Subsidiary),
         FTSE_ISIN = ifelse(is.na(FTSE_ISIN), VE_ISIN, FTSE_ISIN)) %>%
  select(-ISIN_code) %>%
  rename(ISIN_code = FTSE_ISIN,
         FTSE_company = Company)

# Aggregate EV category in green IP data
green_IP_data %<>%
  # Reshape data to get VE categories as columns
  gather(key = Item, value = Count, Total_AllIPC:OnlyMainIPC) %>%
  spread(key = VE_category, value = Count) %>%
  group_by(ISIN_code, Item) %>%
  mutate(EV_aggregate = ifelse(Item %in% c("AllIPC", "OnlyMainIPC"),
                               sum(EVs, EV_batteries, na.rm=TRUE), 
                               ifelse(is.na(EV_batteries),
                                      EVs,
                                      EV_batteries))) %>%
  gather(key = VE_category, value = Count, Biofuels_production:EV_aggregate) %>%
  spread(Item, value = Count) %>%
  ungroup() %>%
  filter(!is.na(Total_AllIPC)) %>%
  select(-company)

# Split total and category-based green IP datasets
green_IP_VE_cat <- green_IP_data %>%
  select(ISIN_code, bvd_id, VE_category, AllIPC, OnlyMainIPC) %>%
  filter(!is.na(bvd_id))

green_IP_totals <- green_IP_data %>%
  filter(!is.na(bvd_id)) %>%
  select(ISIN_code, Total_AllIPC, Total_OnlyMainIPC) %>%
  unique()

# Vivid - FTSE GR category matching
ve_gr_rev_categories %<>%
  mutate(GR_SubSector = FTSE_code) %>%
  select(-FTSE_code, -FTSE_sector, -FTSE_SubSector)

# Merge green revenue best estimates and EQ / EG subcategory data together
green_rev_company_data <- tr_company_list %>%
  left_join(green_rev_best_est_data) %>%
  # Remove no data companies
  filter(!is.na(FTSE_ID)) %>%
  select(-FTSE_company) %>%
  left_join(green_rev_eq_eg_data) %>%
  # Replace ER05 with Lithium if in description, and remove ER05 from dataset otherwise
  mutate(GR_SubSector = case_when(GR_SubSector == "ER05" & grepl("lithium", Description) ~ "ER05_Lithium",
                                  TRUE ~ GR_SubSector)) %>%
  # Remove years which do not match between datasets
  filter(estimate_date == year | is.na(GR_SubSector))

# Separate dataset for GR subsectors which are missing in the above
green_rev_na_company_data <- green_rev_company_data %>%
  filter(is.na(GR_SubSector)) %>%
  select(-GR_SubSector)

# Merge historical data into current data
green_rev_consolidated_data <- green_rev_best_est_data %>%
  select(company, estimate_date) %>%
  left_join(green_rev_hist_est_data) %>%
  mutate(GRCS_SubSectors_new = str_split(GRCS_SubSectors, ", ")) %>%
  unnest(GRCS_SubSectors_new) %>%
  mutate(GRCS_SubSectors = GRCS_SubSectors_new) %>%
  select(company, ISIN, SEDOL, GRCS_SubSectors) %>%
  rename(ISIN_code = ISIN,
         GR_SubSector = GRCS_SubSectors)

green_rev_add_sources_company_data <- tr_company_list %>%
  left_join(green_rev_consolidated_data) %>%
  left_join(green_rev_na_company_data) %>%
  filter(!is.na(estimate_date))

# Computing revenue share of VE categories
green_rev_company_data %<>%
  filter(!is.na(GR_SubSector)) %>%
  bind_rows(green_rev_add_sources_company_data) %>%
  mutate(`Revenue%` = ifelse(is.na(`Revenue%`),0,`Revenue%`)) %>%
  left_join(ve_gr_rev_categories) %>%
  left_join(green_IP_VE_cat, by = c("VE_ISIN" = "ISIN_code", "VE_category")) %>%
  left_join(green_IP_totals, by = c("VE_ISIN" = "ISIN_code")) %>%
  select(company, year, GR_SubSector, `Revenue%`, VE_category, GR_percent, 
         everything()) %>%
  mutate(relevance_check = ifelse(!is.na(VE_category),1,0)) %>%
  group_by(VE_ISIN) %>%
  mutate(relevance_check = sum(relevance_check, na.rm = TRUE)) %>%
  filter(relevance_check > 0) %>% 
  arrange(.by_group = TRUE, Total_AllIPC) %>%
  fill(Total_AllIPC, Total_OnlyMainIPC) 

# Construct an "other" category that catches the patents not assigned within the VE/FTSE match
construct_other <- green_rev_company_data %>%
  select(company, VE_ISIN, VE_category, AllIPC:Total_OnlyMainIPC) %>%
  unique() %>%
  group_by(VE_ISIN) %>%
  mutate(VE_category = ifelse(is.na(VE_category), "Other", VE_category),
         AllIPC = ifelse(VE_category == "Other" & is.na(AllIPC),
                         Total_AllIPC - sum(AllIPC, na.rm = TRUE),
                         AllIPC),
         OnlyMainIPC = ifelse(VE_category == "Other" & is.na(OnlyMainIPC), 
                              Total_OnlyMainIPC - sum(OnlyMainIPC, na.rm = TRUE),
                              OnlyMainIPC))

green_rev_company_data %<>%
  mutate(VE_category = ifelse(is.na(VE_category), "Other", VE_category)) %>%
  select(-relevance_check, -AllIPC, -OnlyMainIPC, -Total_AllIPC, -Total_OnlyMainIPC) %>%
  left_join(construct_other)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Define temp dataset and go through each data cleaning case (see list below) ----

temp <- green_rev_company_data %>%
  ungroup() %>%
  group_by(VE_ISIN) %>%
  mutate(GR_percent = GR_percent / 100, 
         sum_revenue_pct = sum(unique(`Revenue%`),na.rm = TRUE),
         IPC_data_check = ifelse(!is.na(Total_AllIPC), 1, 0),
         IPC_data_check = sum(IPC_data_check, na.rm = TRUE), 
         non_computables = case_when(VE_category != "Other" & `Revenue%` == 0 & sum_revenue_pct > 0 &
                                       is.na(Total_AllIPC) ~ 1,
                                     TRUE ~ 0),
         non_computables = sum(non_computables, na.rm = TRUE), 
         no_of_VE_cat = case_when(VE_category != "Other" ~ 1, TRUE ~ 0), 
         no_of_VE_cat = sum(no_of_VE_cat)) %>%
  # VE_category_rev_share = ifelse(`Revenue%` == 0, VE_category_share_AllIPC*(GR_percent), NA)) %>%
  select(company, year, GR_SubSector, `Revenue%`, VE_category, GR_percent, # VE_category_rev_share,
         non_computables, no_of_VE_cat, everything()) %>% ungroup()

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

Case_A <- filter(temp, sum_revenue_pct == 0, IPC_data_check == 0) %>%
  group_by(VE_ISIN) %>%
  mutate(count = ifelse(!is.na(GR_SubSector),1,0), count = sum(count, na.rm = TRUE),
         VE_category_rev_share = 1/count * GR_percent,
         VE_category_rev_share = ifelse(VE_category_rev_share < 0, 0, VE_category_rev_share),
         CHECK = GR_percent - sum(VE_category_rev_share)) %>%
  ungroup() %>%
  mutate(Case = "A") %>%
  select(company, year, VE_category, VE_category_rev_share, GR_percent, `Revenue%`, everything(), -CHECK, -count)

# Case B ------------------------------------------------------------------

Case_B <- filter(temp, non_computables != 0, non_computables == no_of_VE_cat) %>%
  group_by(VE_ISIN) %>% 
  mutate(count = ifelse(`Revenue%` == 0, 1, 0), count = sum(count, na.rm = TRUE),
         VE_category_rev_share = case_when(`Revenue%` == 0 ~ 1/count * (GR_percent - sum(`Revenue%`, na.rm = TRUE)),
                                           `Revenue%` != 0 ~ `Revenue%`, TRUE ~ NA_real_),
         VE_category_rev_share = ifelse(VE_category_rev_share < 0, 0, VE_category_rev_share),
         CHECK = GR_percent - sum(VE_category_rev_share),
         VE_category_rev_share = case_when(CHECK < 0 ~ (`Revenue%` / sum(`Revenue%`)) * GR_percent,
                                           TRUE ~ VE_category_rev_share),
         CHECK = GR_percent - sum(VE_category_rev_share)) %>%
  ungroup() %>%
  mutate(Case = "B") %>%
  select(company, year, VE_category, VE_category_rev_share, GR_percent, `Revenue%`, everything(), -CHECK, -count)

# Case 1 ------------------------------------------------------------------

Case_1 <- filter(temp, sum_revenue_pct == 0, IPC_data_check > 0) %>%
  group_by(VE_ISIN, VE_category) %>% 
  mutate(No_of_items_in_cat = 1, No_of_items_in_cat = sum(No_of_items_in_cat, na.rm = TRUE),
         VE_category_rev_share = 1/No_of_items_in_cat * AllIPC/Total_AllIPC * GR_percent) %>% 
  ungroup() %>%
  group_by(VE_ISIN) %>%
  mutate(No_of_SubSectors = 1, No_of_SubSectors = sum(No_of_SubSectors, na.rm = TRUE),
         VE_category_rev_share = ifelse(No_of_SubSectors==1, GR_percent, VE_category_rev_share),
         sum_VE_category_AllIPC = ifelse(VE_category != "Other", AllIPC, NA),
         sum_VE_category_AllIPC = sum(sum_VE_category_AllIPC, na.rm = TRUE),
         CHECK_other = ifelse(VE_category == "Other", 1, 0), CHECK_other = sum(CHECK_other, na.rm = TRUE),
         VE_category_rev_share = ifelse(CHECK_other == 0, 
                                        1/No_of_items_in_cat * AllIPC/sum_VE_category_AllIPC * GR_percent, 
                                        VE_category_rev_share),
         CHECK = sum(VE_category_rev_share, na.rm = TRUE),
         CHECK2 = GR_percent - CHECK, CHECK2 = sum(CHECK2, na.rm = TRUE),
         FILTER = ifelse(is.na(VE_category_rev_share), 1, 0), FILTER = sum(FILTER, na.rm = TRUE)) %>%
  select(company, year, VE_category, VE_category_rev_share, GR_percent, CHECK, `Revenue%`, everything(), -CHECK2, -CHECK_other) 

temp_1 <- Case_1 %>%
  filter(FILTER > 0, is.na(VE_category_rev_share)) %>%
  mutate(VE_category_rev_share = 0) 

Case_1 %<>%
  filter(!is.na(VE_category_rev_share)) %>%
  bind_rows(temp_1) %>% ungroup() %>% 
  mutate(Case = "1") %>%
  select(company, year, VE_category, VE_category_rev_share, GR_percent, CHECK, `Revenue%`, everything(), -CHECK,
         -No_of_SubSectors, -FILTER, -No_of_items_in_cat, -sum_VE_category_AllIPC)

# Case 2 ------------------------------------------------------------------

Case_2 <- filter(temp, non_computables != 0 & non_computables != no_of_VE_cat |
                   non_computables == 0 & sum_revenue_pct>0) %>%
  group_by(VE_ISIN) %>%
  mutate(IPC_data_check = sum(AllIPC, na.rm = TRUE),
         sum_of_Rev_pct = sum(`Revenue%`),
         VE_category_Rev_pct = ifelse(`Revenue%` != 0, `Revenue%`, NA)) %>%
  ungroup() %>% group_by(VE_ISIN, VE_category) %>% 
  mutate(sum_of_VE_Rev_pct = sum(VE_category_Rev_pct, na.rm=TRUE),
         No_of_items_in_cat = 1, No_of_items_in_cat = sum(No_of_items_in_cat, na.rm = TRUE),
         AllIPC = ifelse(!is.na(Total_AllIPC) & is.na(AllIPC), 0, AllIPC),
         OnlyMainIPC = ifelse(!is.na(Total_OnlyMainIPC) & is.na(OnlyMainIPC), 0, OnlyMainIPC)) %>%
  ungroup() %>%
  group_by(VE_ISIN) %>% # arrange(VE_category) %>% fill(Total_AllIPC) %>%
  mutate(Orbis_hat_1 = AllIPC / Total_AllIPC * GR_percent,
         Orbis_hat_2 = case_when(sum_of_VE_Rev_pct >= Orbis_hat_1 ~ sum_of_VE_Rev_pct,
                                 !is.na(sum_of_VE_Rev_pct) & is.na(Orbis_hat_1) & !is.na(Total_AllIPC) ~ sum_of_VE_Rev_pct,
                                 TRUE ~ NA_real_),
         Orbis_hat_2 = ifelse(Orbis_hat_2 > GR_percent, GR_percent, Orbis_hat_2),
         Remaining_AllIPC = ifelse(!is.na(Orbis_hat_2), Total_AllIPC - AllIPC, NA),
         Remaining_AllIPC = ifelse(sum(unique(Remaining_AllIPC), na.rm = TRUE) != 0,
                                   sum(unique(Remaining_AllIPC), na.rm = TRUE), NA)) %>%
  # ungroup() %>% group_by(VE_ISIN, VE_category) %>%
  mutate(Remaining_GR = GR_percent - sum(unique(Orbis_hat_2), na.rm = TRUE),
         Orbis_hat_2 = case_when(is.na(Orbis_hat_2) ~ AllIPC/Remaining_AllIPC * Remaining_GR,
                                 TRUE ~ Orbis_hat_2))

CHECK_2 <- Case_2 %>% select(company, VE_ISIN, VE_category, Orbis_hat_1, Orbis_hat_2) %>%
  ungroup() %>%
  group_by(VE_ISIN) %>%
  unique() %>% 
  mutate(CHECK = case_when(!is.na(Orbis_hat_2) ~ sum(Orbis_hat_2, na.rm = TRUE),
                           is.na(Orbis_hat_2) & !is.na(Orbis_hat_1) ~ sum(Orbis_hat_1, na.rm = TRUE),
                           TRUE ~ NA_real_)) 
Case_2 %<>% 
  left_join(CHECK_2) %>%
  fill(CHECK) %>%
  select(company, year, VE_category, sum_of_VE_Rev_pct, GR_percent, CHECK, Orbis_hat_1, Orbis_hat_2, `Revenue%`, 
         Remaining_AllIPC, Remaining_GR, everything()) 

Case_2_b <- Case_2 %>%
  filter(is.na(Orbis_hat_1) & is.na(Orbis_hat_2)) %>%
  mutate(FTSE_hat = case_when (!is.na(`Revenue%`) ~ `Revenue%`, TRUE ~ NA_real_),
         count_1 = ifelse(`Revenue%` == 0, 1, 0)) %>%
  ungroup() %>%
  group_by(VE_ISIN, VE_category) %>%
  mutate(count_2 = sum(count_1)) %>%
  ungroup() 

CHECK_2_b <- Case_2_b %>%
  select(company, VE_ISIN, VE_category, FTSE_hat) %>%
  group_by(VE_ISIN) %>%
  unique() %>% 
  mutate(CHECK = sum(FTSE_hat, na.rm = TRUE))

Case_2_b %<>% select(everything(), -CHECK) %>%
  left_join(CHECK_2_b) %>%
  group_by(VE_ISIN) %>%
  mutate(count_1 = sum(count_1, na.rm = TRUE),
         FTSE_hat = ifelse(`Revenue%` == 0, count_2/count_1 * (GR_percent-CHECK), FTSE_hat)) 

temp_2 <- Case_2_b %>%
  select(company, VE_category, FTSE_hat, VE_ISIN) %>%
  unique() %>%
  mutate(CHECK2=sum(FTSE_hat, na.rm = TRUE))

Case_2_b %<>%
  left_join(temp_2) %>%
  ungroup() %>%
  mutate(FTSE_hat = ifelse(abs(GR_percent - CHECK2) > 0.0000001, `Revenue%`/sum_of_Rev_pct * GR_percent, FTSE_hat)) %>%
  select(company:GR_percent, CHECK, CHECK2, FTSE_hat, everything(), -count_1, -count_2)

Case_2 %<>%
  filter(!is.na(Orbis_hat_1) | !is.na(Orbis_hat_2) | CHECK == GR_percent) %>%
  mutate(FTSE_hat = NA, CHECK2=NA) %>%
  select(company:CHECK, CHECK2, Orbis_hat_1, Orbis_hat_2, FTSE_hat, everything()) %>%
  bind_rows(Case_2_b) %>%
  mutate(VE_category_rev_share = case_when(!is.na(Orbis_hat_2) ~ Orbis_hat_2,
                                           is.na(Orbis_hat_2) ~ ifelse(!is.na(Orbis_hat_1), Orbis_hat_1, FTSE_hat),
                                           TRUE ~ NA_real_)) %>%
  ungroup() %>%
  mutate(Case = "2") %>%
  group_by(VE_ISIN, VE_category) %>%
  mutate(No_of_items_in_cat = 1, No_of_items_in_cat = sum(No_of_items_in_cat, na.rm = TRUE),
         VE_category_rev_share = 1/No_of_items_in_cat * VE_category_rev_share) %>%
  ungroup() %>%
  select(company, year, VE_category, VE_category_rev_share, GR_percent, `Revenue%`, everything(), -Orbis_hat_1,
         -Orbis_hat_2, -FTSE_hat, -Remaining_AllIPC, -Remaining_GR, -sum_of_Rev_pct, -CHECK, -CHECK2,
         -VE_category_Rev_pct, -sum_of_Rev_pct, -sum_of_VE_Rev_pct, -No_of_items_in_cat)

# Combine Case datasets ---------------------------------------------------

green_rev_company_results <- Case_2 %>% 
  bind_rows(Case_1, Case_A, Case_B) %>%
  select(everything(), -IPC_data_check) %>%
  group_by(tr_company) %>%
  mutate(lithium_adj = ifelse(GR_SubSector == "ER05_Lithium", 1, 0),
         lithium_adj = sum(lithium_adj, na.rm = TRUE))

Lithium_adjustment <- filter(green_rev_company_results, lithium_adj > 0) %>%
  group_by(tr_company) %>%
  mutate(count = ifelse(VE_category_rev_share > 0 | VE_category == "Minerals_for_batteries",
                        1, 0),
         count = sum(count, na.rm = TRUE),
         VE_category_rev_share_adj = ifelse(VE_category == "Minerals_for_batteries", 1/count*GR_percent, NA),
         VE_category_rev_share_adj = ifelse(is.na(VE_category_rev_share_adj), (VE_category_rev_share/GR_percent) * 
                                              GR_percent * (count-1)/count, VE_category_rev_share_adj)) %>%
  select(everything(), -VE_category_rev_share, -count) %>%
  rename(VE_category_rev_share = VE_category_rev_share_adj)

green_rev_company_results %<>%
  ungroup() %>%
  filter(lithium_adj == 0) %>%
  bind_rows(Lithium_adjustment) %>%
  group_by(VE_ISIN) %>%
  mutate(Total_VE_cat_rev_share = #ifelse(VE_category != "Other", 
           sum(VE_category_rev_share, na.rm = TRUE),#, NA),
         Calc_check = case_when(Total_VE_cat_rev_share - GR_percent > 0.0001 ~ 1, TRUE ~ 0)) %>%
  select(company:GR_percent, Total_VE_cat_rev_share, Calc_check, Case, everything())

companies_before <- unique(temp$company)
companies_after <- unique(green_rev_company_results$company)

save_dated(green_rev_company_results, "Combined_Green_Revenue_data", folder = "Output")
