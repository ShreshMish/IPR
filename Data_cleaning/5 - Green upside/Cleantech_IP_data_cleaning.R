##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  29/08/2018
##### Code author:        Justine Schafer
##### Edited by:          Shyamal Patel
##### Description:        This script reads in Orbis green IP, and Thomson Reuters financial data
#####                     and builds a complete green IP dataset for later green revenue data cleaning
##### Dependencies:       1.  Latest Thomson Reuters cleaned dataset: "1 - Financial prelim/Output/TR_cleaned_2016USD_data.xlsx"
#####                     2.  Orbis IP company data files (x4)
#####                     3.  Orbis IP company name matching files (x2)
#####                     4.  Results from filling in gaps in name matching by Vivid
#####                     5.  Green IPC codes from WIPO IPC green inventory
#####                     6.  Vivid-green IPC code mapping
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

# Read in Orbis green IP datasets (4 tab separated files)
orbis_ip_raw_tsv_1 <- read_tsv("5 - Green upside/Input/HSBC_GreenPatent V1.txt")
orbis_ip_raw_tsv_2 <- read_tsv("5 - Green upside/Input/HSBC_GreenPatent V2.txt")
orbis_ip_raw_tsv_3 <- read_tsv("5 - Green upside/Input/HSBC_GreenPatent V3.txt")
orbis_ip_raw_tsv_4 <- read_tsv("5 - Green upside/Input/HSBC_GreenPatent_2.txt")

# Read in Orbis company name matching workbooks
orbis_names_1 <- read_excel("5 - Green upside/Input/HSBC Green patent list of BvDids.xlsx",
                            sheet = "Results", range = "$B$1:$I$2414")
orbis_names_2 <- read_excel("5 - Green upside/Input/Export 20_07_2018 19_03.xlsx",
                            sheet = "Results", range = "$B$1:$D$315")

# Read in Vivid - Orbis ISIN code matching for companies which were missing from BvD ISIN codes list
orbis_names_missing <- read_excel("5 - Green upside/Input/Orbis missing companies analysis.xlsx",
                                  sheet = "R1. Orbis IP missing", range = "$A$7:$D$142")

# Read in green IPC codes (Based on IPC green inventory)
green_ipc_codes <- read_excel("5 - Green upside/Input/ipc_green_inventory.xlsx",
                             sheet = "Consolidated", range = "$A$1:$E$3375")

# Read in Vivid - WIPO IPC category matching
ve_ipc_green_categories <- read_excel("5 - Green upside/Input/VE Orbis category matching.xlsx",
                                      sheet = "Sheet1", range = "$A$1:$C$18")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean and merge together Orbis green IP data ----

# IP data itself
orbis_ip_data <- orbis_ip_raw_tsv_1 %>%
  bind_rows(orbis_ip_raw_tsv_2) %>%
  bind_rows(orbis_ip_raw_tsv_3) %>%
  bind_rows(orbis_ip_raw_tsv_4) %>%
  rename(bvd_id = `bvd id number`)

# Company ISIN codes / names
orbis_names_list <- list(orbis_names_1, orbis_names_2)

clean_names <- function(data) {
  data %<>%
    rename(bvd_id = `BvD ID number`,
           ISIN_code = `ISIN number`,
           company = `Company name`) %>%
    select(bvd_id, ISIN_code, company)
}

orbis_names_data <- map(orbis_names_list, clean_names) %>%
  bind_rows() %>%
  select(-company)

# Vivid-Orbis missing company matched ISIN codes
orbis_missing_names_data <- orbis_names_missing %>%
  rename(ISIN_code = `Vivid ISIN Code`,
         BvD_ISIN_code = `BvD ISIN Code`) %>%
  select(ISIN_code, BvD_ISIN_code) %>%
  filter(!is.na(BvD_ISIN_code)) #Remove NA categories (Vivid ISIN codes not found)

# Green IPC codes
green_ipc_codes_data <- green_ipc_codes %>%
  rename(ipc_class_symbol = IPC) %>%
  mutate(category = "Green")

# Green IP data full (build out based on Thomson Reuters company names and ISIN codes)
green_ip_data <- financial_data %>%
  select(ISIN_code, company) %>%
  # Join in rematched ISIN codes (TR ISIN code =/= Orbis ISIN code)
  left_join(orbis_missing_names_data, by = "ISIN_code") %>%
  # Fill in BvD ISIN code variable with Vivid/TR ISIN code when former is blank
  mutate(BvD_ISIN_code = ifelse(is.na(BvD_ISIN_code), ISIN_code, BvD_ISIN_code)) %>%
  # Rename ISIN code variables to match Orbis IP dataset variable names
  rename(Vivid_ISIN_code = ISIN_code,
         ISIN_code = BvD_ISIN_code) %>%
  # Join in BvD company names dataset
  left_join(orbis_names_data, by = "ISIN_code") %>%
  # Join in Orbis patent data
  left_join(orbis_ip_data, by = "bvd_id") %>%
  # Join in IPC green inventory dataset
  left_join(green_ipc_codes_data, by = "ipc_class_symbol") %>%
  # Remove ISIN_code variable used for match, and rename Vivid ISIN_code variable
  select(-ISIN_code) %>%
  rename(ISIN_code = Vivid_ISIN_code) %>%
  select(ISIN_code, bvd_id, company, ipc_class_symbol, starts_with("Tier"), everything()) %>%
  rename_at(.vars = vars(contains("Tier")), .funs = funs(paste0("Green_sector_level_", gsub("\\D+", "", .))))

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Save interim datasets, fill in categorical variables as appropriate ----

# Dataset of companies missing from Orbis IP dataset (after manual matching of ISIN codes)
orbis_final_missing_data <- green_ip_data %>%
  filter(is.na(bvd_id)) %>%
  select(ISIN_code, bvd_id, company) %>%
  left_join(financial_data)

save_dated(orbis_final_missing_data, "IP_missing_companies_final", folder = "Interim", csv = TRUE)

# Full IP dataset (before filtering out non-green IP)
save_dated(green_ip_data, "IP_full_data", folder = "Interim", csv = FALSE)

# Record companies with IP, but no green IP
no_green_ip_data <- green_ip_data %>%
  group_by(ISIN_code, company) %>%
  arrange(category) %>%
  # Fill in category ("Green" for companies with at least one green patent)
  fill(category) %>%
  filter(is.na(category)) %>%
  select(ISIN_code:company) %>%
  unique()

# Green IP dataset (filter out non-green IP)
green_ip_data %<>%
  filter(category == "Green") %>%
  select(-category)

# Save full IP dataset (before summarising by IPC tier)
save_dated(green_ip_data, "Green_IP_full_data", folder = "Interim", csv = FALSE)

# Fill in sector-level descriptions from higher levels when NA values
green_ip_data %<>%
  mutate(Green_sector_level_2 = case_when(is.na(Green_sector_level_2) ~ Green_sector_level_1,
                                          TRUE ~ Green_sector_level_2),
         Green_sector_level_3 = case_when(is.na(Green_sector_level_3) ~ Green_sector_level_2,
                                          TRUE ~ Green_sector_level_3),
         Green_sector_level_4 = case_when(is.na(Green_sector_level_4) ~ Green_sector_level_3,
                                          TRUE ~ Green_sector_level_4))

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Summarise patent count by green IPC sector-level category for later use ----

# Define function for summarising by level (drop direct matched variables - see email chain from Santhosh)
summarise_level <- function(data, level) {
  
  group_level <- rlang::sym(paste0("Green_sector_level_", level))
  
  temp <- data %>%
    group_by(ISIN_code, company, !!group_level) %>%
    summarise_at(.vars = vars(contains("Patent")), .funs = funs(sum(., na.rm = TRUE))) %>%
    select(ISIN_code, company, contains("Green"), AllIPC_AllPatents, OnlyMainIPC_AllPatents) %>%
    rename_at(.vars = vars(AllIPC_AllPatents, OnlyMainIPC_AllPatents),
              .funs = funs(paste0("Level_", level, "_", substr(., 1, str_locate(., "_") - 1))))
  
  return(temp)
}

# Apply function over green sector-levels 1 - 4
levels <- c(1:4)
summary_green_ip_results <- map(levels, summarise_level, data = green_ip_data)

# Merge together results and add back in no green IP companies
green_ip_results <- green_ip_data %>%
  # Prepare dataset
  select(ISIN_code:company, starts_with("Green")) %>%
  unique() %>%
  bind_rows(no_green_ip_data) %>%
  # Merge in results summaries
  left_join(summary_green_ip_results[[1]]) %>%
  left_join(summary_green_ip_results[[2]]) %>%
  left_join(summary_green_ip_results[[3]]) %>%
  left_join(summary_green_ip_results[[4]])

# Save results from cleaning green IP dataset
save_dated(green_ip_results, "Green_IP_full_results", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Keep Vivid categories of interest based on matching against IPC green inventory categories ----

# Find total green patents for each company (Level 4 is unique patents)
ve_green_ip_results <- green_ip_results %>%
  group_by(ISIN_code) %>%
  mutate(Total_AllIPC = sum(Level_4_AllIPC),
         Total_OnlyMainIPC = sum(Level_4_OnlyMainIPC)) %>%
  ungroup()

# Define level 1 and level 2 category variables
ve_cat_level_1 <- ve_ipc_green_categories %>%
  filter(Level == "Green_sector_level_1") 
ve_cat_level_2 <- ve_ipc_green_categories %>%
  filter(Level == "Green_sector_level_2") 

ve_cat_level_1 %<>%
  select(-Level) %>% 
  transmute(VE_category, Green_sector_level_1=Orbis_category)

ve_cat_level_2 %<>%
  select(-Level) %>%
  transmute(VE_category, Green_sector_level_2=Orbis_category)

ve_cat_matching <- ve_green_ip_results %>% 
  select(Green_sector_level_1, Green_sector_level_2) %>%
  unique() %>%
  left_join(ve_cat_level_1) %>%
  left_join(ve_cat_level_2, by="Green_sector_level_2") %>%
  mutate(VE_category = ifelse(!is.na(VE_category.x), VE_category.x, VE_category.y)) %>%
  select(everything(), -VE_category.x, -VE_category.y)

ve_green_ip_results %<>%
  left_join(ve_cat_matching) %>%
  select(ISIN_code:company, VE_category, everything()) 

# calculate sums for Vivid categories
category_sums <- function(company_ISIN = NULL) {
  
  #print(company_ISIN)
  temp <- ve_green_ip_results %>%
    filter(ISIN_code == company_ISIN) %>%
    select(ISIN_code:Green_sector_level_2, Level_1_AllIPC:Level_2_OnlyMainIPC) %>% 
    group_by(VE_category, Green_sector_level_1, Green_sector_level_2) %>%
    unique(.by_group=TRUE) %>%
    ungroup() %>%
    group_by(VE_category) %>% 
    mutate(AllIPC = ifelse(VE_category %in% c("EV_batteries", "Biofuels_production"), 
                           Level_1_AllIPC, sum(Level_2_AllIPC, na.rm = TRUE)),
           OnlyMainIPC = ifelse(VE_category %in% c("EV_batteries", "Biofuels_production"), 
                                Level_1_OnlyMainIPC, sum(Level_2_OnlyMainIPC, na.rm = TRUE)))
}

ISIN_code_list <- unique(ve_green_ip_results$ISIN_code)
category_sum_data <- map(ISIN_code_list, category_sums) %>%
  bind_rows() 

ve_green_ip_results %<>%
  left_join(category_sum_data) %>%
  select(ISIN_code:VE_category, Total_AllIPC:OnlyMainIPC) %>%
  unique() %>%
  mutate(VE_category = ifelse(is.na(VE_category), "Non_VE", VE_category))

# Save results from matching against Vivid dataset
save_dated(ve_green_ip_results, "Green_IP_VE_category_results", folder = "Output", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 6 - Dataset with top 30 players for each category (TN inspection) ----

# Define function
top_30_companies <- function(data, category) {
  
  temp <- data %>%
    filter(VE_category == category) %>%
    # Calculate IPC market share for each company
    mutate(AllIPC_share = AllIPC / sum(AllIPC, na.rm = TRUE),
           OnlyMainIPC_share = OnlyMainIPC / sum(OnlyMainIPC, na.rm = TRUE)) %>%
    # Take top 30 companies
    top_n(n = 30, wt = AllIPC) %>%
    arrange(desc(AllIPC)) %>%
    select(ISIN_code, bvd_id, company, VE_category, AllIPC:OnlyMainIPC_share)
  
  save_dated(temp, paste0("Top_30_players_", category), folder = "Interim", csv = TRUE)
    
}

# Apply over green categories, excluding "Non_VE"
green_categories <- unique(ve_green_ip_results$VE_category)
green_categories <- green_categories[! green_categories %in% "Non_VE"]
lapply(green_categories, top_30_companies, data = ve_green_ip_results)