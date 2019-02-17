##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  04/02/2019
##### Code author:        Justine Schafer
##### Edited by:          Shyamal Patel
##### Description:        This script reads in Orbis green IP, and Thomson Reuters financial data
#####                     and builds a complete green IP dataset for later green revenue data cleaning
##### Dependencies:       1.  ADD
#####                     2.  Orbis IP company data files (x4)
#####                     3.  Orbis IP company name matching files (x2)
#####                     4.  Results from filling in gaps in name matching by Vivid
#####                     5.  Green IPC codes from WIPO IPC green inventory
#####                     6.  Vivid-green IPC code mapping
#####                     Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "3_ESG/3d_Cleantech"
source("utils.R")

# Read in Orbis green IP datasets (4 tab separated files)
orbis_ip_raw_tsv_1 <- read_tsv(input_source("HSBC_GreenPatent_V1.txt"))
orbis_ip_raw_tsv_2 <- read_tsv(input_source("HSBC_GreenPatent_V2.txt"))
orbis_ip_raw_tsv_3 <- read_tsv(input_source("HSBC_GreenPatent_V3.txt"))
orbis_ip_raw_tsv_4 <- read_tsv(input_source("HSBC_GreenPatent_2.txt"))

# Read in Orbis company name - BvD ID matching workbooks
orbis_names_1 <- read_excel(input_source("HSBC_Green_patent_list_of_BvDids.xlsx"),
                            sheet = "Results", range = "$B$1:$I$2414")
orbis_names_2 <- read_excel(input_source("Export_20_07_2018_19_03.xlsx"),
                            sheet = "Results", range = "$B$1:$D$315")

# Read in Vivid - Orbis ISIN code matching for companies which were missing from BvD ISIN codes list
orbis_names_missing <- read_excel(input_source("Orbis_missing_companies_analysis.xlsx"),
                                  sheet = "R1. Orbis IP missing", range = "$A$7:$D$142")

# Read in green IPC codes (Based on IPC green inventory)
green_ipc_codes <- read_excel(input_source("ipc_green_inventory.xlsx"),
                             sheet = "Consolidated", range = "$A$1:$E$3375")

# Read in Vivid - WIPO IPC category matching
ve_ipc_green_categories <- read_excel(input_source("VE_Orbis_category_matching.xlsx"),
                                      sheet = "Sheet1", range = "$A$1:$C$18")

# Read in company unique names list
company_names <- readRDS("2_Financial/2a_Preliminary/Output/Companies_list.rds")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean and merge together Orbis green IP data ----

# IP data itself
orbis_ip_data <- orbis_ip_raw_tsv_1 %>%
  bind_rows(orbis_ip_raw_tsv_2, orbis_ip_raw_tsv_3, orbis_ip_raw_tsv_4) %>%
  rename(bvd_id = `bvd id number`)

# Company ISIN codes / names
orbis_names_list <- list(orbis_names_1, orbis_names_2)

clean_names <- function(data) {
  data %<>%
    rename(bvd_id = `BvD ID number`,
           ISIN_code = `ISIN number`,
           bvd_name = `Company name`) %>%
    select(bvd_id, ISIN_code, bvd_name)
}

orbis_names_data <- map(orbis_names_list, clean_names) %>%
  bind_rows()

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Create cleantech IP company names dataframe for later use ----

#  Clean up Vivid-Orbis missing names matching results
orbis_names_missing2 <- orbis_names_missing %>%
  rename(bvd_id = `BvD ID`,
         vivid_merge_isin_code = `Vivid ISIN Code`,
         finance_merge_isin_code = `BvD ISIN Code`)
         
# Add missing name matching results to the main companies list and create Vivid merge variable
company_names2 <- company_names %>%
  mutate(vivid_merge_isin_code = case_when(equity_isin_code_1 %in% orbis_names_missing2$vivid_merge_isin_code ~ equity_isin_code_1,
                                           equity_isin_code_2 %in% orbis_names_missing2$vivid_merge_isin_code ~ equity_isin_code_2,
                                           equity_isin_code_3 %in% orbis_names_missing2$vivid_merge_isin_code ~ equity_isin_code_3,
                                           TRUE ~ NA_character_)) %>%
  left_join(orbis_names_missing2, by = "vivid_merge_isin_code") %>%
  select(-vivid_merge_isin_code, -bvd_id, -Company)

# Create financial data merge variable within the Orbis patent dataset
cleantech_ip_companies <- orbis_names_data %>%
  rename(finance_merge_isin_code = ISIN_code) %>%
  select(finance_merge_isin_code, bvd_id, bvd_name)

# Create financial data merge variable within the company names dataset
company_names3 <- company_names2 %>% 
  mutate(finance_merge_isin_code = case_when(equity_isin_code_1 %in% cleantech_ip_companies$finance_merge_isin_code ~ equity_isin_code_1,
                                             equity_isin_code_2 %in% cleantech_ip_companies$finance_merge_isin_code ~ equity_isin_code_2,
                                             equity_isin_code_3 %in% cleantech_ip_companies$finance_merge_isin_code ~ equity_isin_code_3,
                                             TRUE ~ finance_merge_isin_code))

cleantech_ip_companies2 <- company_names3 %>%
  left_join(cleantech_ip_companies, by = "finance_merge_isin_code") %>%
  select(company_id, company, bvd_name, bvd_id) %>%
  filter(!is.na(bvd_name)) %>%
  arrange(company_id)

# Save matching of NZT company names to cleantech IP company names (used for results mapping later)
save_dated(cleantech_ip_companies2, "Orbis_IP_companies_list", folder = "Output", csv = TRUE)

# Save NZT company names missing from cleantech IP company names list
no_cleantech_ip_companies <- company_names %>%
  filter(!(company_id %in% cleantech_ip_companies2$company_id))

save_dated(no_cleantech_ip_companies, "Orbis_IP_missing_companies_list", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Merge together datasets and merge green IPC codes ----

# Green IPC codes
cleantech_ipc_codes <- green_ipc_codes %>%
  rename(ipc_class_symbol = IPC) %>%
  mutate(category = "Green")

# IP data full (build out based on Thomson Reuters company names and ISIN codes)
patent_data <- cleantech_ip_companies2 %>%
  # Join in Orbis patent data
  left_join(orbis_ip_data, by = "bvd_id") %>%
  # Join in IPC green inventory dataset
  left_join(cleantech_ipc_codes, by = "ipc_class_symbol") %>%
  select(company_id, company, bvd_name, bvd_id, ipc_class_symbol, starts_with("Tier"), everything()) %>%
  rename_at(.vars = vars(contains("Tier")), .funs = funs(paste0("Green_sector_level_", gsub("\\D+", "", .))))

save_dated(patent_data, "Full_patent_data", folder = "Interim", csv =  FALSE)

# Filter down to cleantech IP data based on green inventory classification
cleantech_patent_data <- patent_data %>%
  filter(category == "Green") %>%
  mutate(Green_sector_level_2 = case_when(is.na(Green_sector_level_2) ~ Green_sector_level_1,
                                          TRUE ~ Green_sector_level_2),
         Green_sector_level_3 = case_when(is.na(Green_sector_level_3) ~ Green_sector_level_2,
                                          TRUE ~ Green_sector_level_3),
         Green_sector_level_4 = case_when(is.na(Green_sector_level_4) ~ Green_sector_level_3,
                                          TRUE ~ Green_sector_level_4)) %>%
  select(-category)

save_dated(cleantech_patent_data, "Cleantech_patent_data", folder = "Interim", csv = FALSE)

# Separate list of companies which have no cleantech patents
no_cleantech_patent_companies <- patent_data %>%
  select(company_id, company, bvd_name, bvd_id) %>%
  filter(!(company_id %in% cleantech_patent_data$company_id)) %>%
  unique()

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Summarise patent count by green IPC sector-level category for later use ----

# Define function for summarising by level (drop direct matched variables - see email chain from Santhosh)
summarise_level <- function(data, level) {
  
  group_level <- rlang::sym(paste0("Green_sector_level_", level))
  temp <- data %>%
    group_by(company_id, company, !!group_level) %>%
    summarise_at(.vars = vars(contains("Patent")), .funs = funs(sum(., na.rm = TRUE))) %>%
    select(company_id, company, contains("Green"), AllIPC_AllPatents, OnlyMainIPC_AllPatents) %>%
    rename_at(.vars = vars(AllIPC_AllPatents, OnlyMainIPC_AllPatents),
              .funs = funs(paste0("Level_", level, "_", substr(., 1, str_locate(., "_") - 1))))
  
  return(temp)
}

# Apply function over green sector-levels 1 - 4
levels <- c(1:4)
summary_cleantech_patent_results <- map(levels, summarise_level, data = cleantech_patent_data)

# Merge together results and add back in no cleantech patent companies
cleantech_patent_results <- cleantech_patent_data %>%
  # Prepare dataset
  select(company_id:bvd_id, starts_with("Green")) %>%
  unique() %>%
  bind_rows(no_cleantech_patent_companies) %>%
  bind_rows(no_cleantech_ip_companies) %>%
  # Merge in results summaries
  left_join(summary_cleantech_patent_results[[1]]) %>%
  left_join(summary_cleantech_patent_results[[2]]) %>%
  left_join(summary_cleantech_patent_results[[3]]) %>%
  left_join(summary_cleantech_patent_results[[4]])

# Save results from cleaning green IP dataset
save_dated(cleantech_patent_results, "Cleantech_patent_full_results", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 6 - Keep Vivid categories of interest based on matching against IPC green inventory categories ----

# Find total green patents for each company (Level 4 is unique patents)
cleantech_patent_results2 <- cleantech_patent_results %>%
  group_by(company_id) %>%
  mutate(Total_AllIPC = sum(Level_4_AllIPC),
         Total_OnlyMainIPC = sum(Level_4_OnlyMainIPC)) %>%
  ungroup()

# Define level 1 and level 2 category variables
ve_cat_level_1 <- ve_ipc_green_categories %>%
  filter(Level == "Green_sector_level_1") 
ve_cat_level_2 <- ve_ipc_green_categories %>%
  filter(Level == "Green_sector_level_2") 

ve_cat_level_1_2 <- ve_cat_level_1 %>%
  select(-Level) %>% 
  rename(Green_sector_level_1 = Orbis_category)

ve_cat_level_2_2 <- ve_cat_level_2 %>%
  select(-Level) %>%
  rename(Green_sector_level_2 = Orbis_category)

ve_cat_matching <- cleantech_patent_results2 %>% 
  select(Green_sector_level_1, Green_sector_level_2) %>%
  unique() %>%
  left_join(ve_cat_level_1_2) %>%
  left_join(ve_cat_level_2_2, by="Green_sector_level_2") %>%
  mutate(VE_category = ifelse(!is.na(VE_category.x), VE_category.x, VE_category.y)) %>%
  select(-VE_category.x, -VE_category.y)

cleantech_patent_results3 <- cleantech_patent_results2 %>%
  left_join(ve_cat_matching) %>%
  select(company_id:bvd_id, VE_category, everything())

#--------------------------------------------------------------------------------------------------

##### SECTION 7 - Calculate sums over Vivid categories of interests (across IPC codes / categories) ----

cleantech_patent_results4 <- cleantech_patent_results3 %>%
  group_by(company_id) %>%
  mutate(Total_AllIPC = sum(Level_4_AllIPC),
         Total_OnlyMainIPC = sum(Level_4_OnlyMainIPC)) %>%
  group_by(company_id, VE_category, Green_sector_level_1, Green_sector_level_2) %>%
  select(company_id, company, bvd_id, bvd_name, VE_category, Green_sector_level_1, Green_sector_level_2, Level_1_AllIPC:Level_2_OnlyMainIPC,
         Total_AllIPC, Total_OnlyMainIPC) %>%
  unique() %>%
  group_by(company_id, VE_category) %>%
  select(company_id:Green_sector_level_2, Level_1_AllIPC:Level_2_OnlyMainIPC, Total_AllIPC, Total_OnlyMainIPC) %>%
  mutate(AllIPC = ifelse(VE_category %in% c("EV_batteries", "Biofuels_production"),
                         Level_1_AllIPC, sum(Level_2_AllIPC, na.rm = TRUE)),
         OnlyMainIPC = ifelse(VE_category %in% c("EV_batteries", "Biofuels_production"),
                              Level_1_OnlyMainIPC, sum(Level_2_OnlyMainIPC, na.rm = TRUE))) %>%
  ungroup() %>%
  select(company_id:VE_category, Total_AllIPC:OnlyMainIPC) %>%
  unique() %>%
  mutate(VE_category = ifelse(is.na(VE_category), "Non_VE", VE_category))

# Save results from matching against Vivid dataset
save_dated(cleantech_patent_results4, "Cleantech_patent_VE_category_results", folder = "Output", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 8 - Dataset with top 30 players for each category (TN inspection) ----

top_30_companies <- function(data, category) {
  temp <- data %>%
    filter(VE_category == category) %>%
    # Calculate market share for each company
    mutate(AllIPC_share = AllIPC / sum(AllIPC, na.rm = TRUE),
           OnlyMainIPC_share = OnlyMainIPC / sum(OnlyMainIPC, na.rm = TRUE)) %>%
    # Take top 30 only
    top_n(n = 30, wt = AllIPC) %>%
    arrange(desc(AllIPC)) %>%
    select(company_id, company, bvd_id, bvd_name, VE_category, AllIPC:OnlyMainIPC_share)
  
  save_dated(temp, paste0("Top_30_players_", category), folder = "Interim", csv = TRUE)
}

# Apply over green categories, excluding "Non_VE"
green_categories <- unique(cleantech_patent_results4$VE_category)
green_categories <- green_categories[! green_categories %in% "Non_VE"]
lapply(green_categories, top_30_companies, data = cleantech_patent_results4)