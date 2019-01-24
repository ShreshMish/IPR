##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  29/08/2018
##### Code author:        Shyamal Patel
##### Description:        This script reads in TR financial data from Excel, and cleans the data before
#####                     further data cleaning takes place
##### Dependencies:       1.  Latest list of TR unique geographies: "4 - Financial geog/Interim/TR_geographies_full_list
#####                     2.  Latest region-ISO code matching: "4 - Financial geog/Interim/TR/TR_mapped_geographies
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA

packages <- c("tidyverse", "magrittr", "readxl", "here", "stringi", "doParallel", "foreach")
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
save_dated <- function(data, name, folder, dated = "YES", csv = "NO") {
  main_path <- paste0("6 - Financial geog/", folder)
  dated_path <- paste0("6 - Financial geog/", folder, "/Dated/")
  
  # Save main file
  saveRDS(data, here(main_path, paste0(name, ".rds")))
  # Save dated file (optional)
  if(dated == "YES") {saveRDS(data, here(dated_path, paste0(date, "_", name, ".rds")))}
  # Save dated CSV file (optional)
  if(csv == "YES") {write_csv(data, here(dated_path, paste0(date, "_", name, ".csv")))}
}

# Read in required datasets
region_iso_code_data <- readRDS("6 - Financial geog/Interim/TR_mapped_geographies.rds") %>%
  rename(region = Region) #Rename region variable to match other files

tr_geographies_data <- readRDS("6 - Financial geog/Interim/TR_geographic_exposure_full_data.rds")

gdp_iso_code_data <- readRDS("6 - Financial geog/Interim/GDP_ISO_code_data.rds") %>%
  rename(ISO_code = `ISO code`)

lcp_project_regions <- read_excel(path = "6 - Financial geog/Input/ISO code and regional gdp.xlsx", sheet = "W5. LCP region mapping",
                                  range = "$A$7:$F$211") %>%
  select(ISO_code, Country, LCP_Region)

# Set parallelisation options

# Disable timeout
setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

# Set number of cores for scenario compiling
c1 <- makeCluster(detectCores() - 3)

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Company missing region calculation - should be NULL

company_geographies_data <- tr_geographies_data %>%
  # Filter out any redundant regions, and replace negative value regions with 0
  mutate(region_revenue = ifelse(region_revenue < 0, 0, region_revenue)) %>%
  filter((!is.na(region)) & (!is.na(region_revenue))) #Remove NA regions to avoid netting these from superregions in calculations

# Find regions which have not yet been defined in the mapped geographies file (region_iso_code_data)
# should be NULL
missing_regions <- company_geographies_data %>%
  filter(!(region %in% region_iso_code_data$region))

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Company region ISO allocation rules (order in which ISO codes are assigned to regions)

# Merge together company-region and region-ISO code mapping datasets
company_exposure <- company_geographies_data %>%
  select(ISIN_code, company, region) %>%
  left_join(region_iso_code_data) %>%
  rename(ISO_inclusion = Inclusion)

# Create region ordering rank function
# NB - Human input may be required here if there are many sources of conflict in the prioritisation
#      for instance, regional exposure sources: UK, OECD, Europe, EMEA, Rest of World could cause issues
company_region_ranks <- company_exposure %>%
  group_by(ISIN_code, company, region) %>%
  mutate(ISO_count = sum(ISO_inclusion)) %>%
  summarise(ISO_count = mean(ISO_count)) %>%
  ungroup() %>%
  arrange(company, ISO_count) %>%
  group_by(ISIN_code) %>%
  mutate(region_priority = row_number()) %>%
  ungroup()

save_dated(company_region_ranks, "TR_company_region_priorities", folder = "Interim", csv = "YES")

# Higher order lagged variables are used for duplicate region entries (ISO code mapping, not name duplicates)
company_exposure %<>%
  left_join(company_region_ranks) %>%
  arrange(company, ISO_code, region_priority) %>%
  group_by(ISIN_code, company, ISO_code) %>%
  mutate(cum_ISO_inclusion = cummax(ISO_inclusion),
         lag1_cum_ISO_inclusion = lag(cum_ISO_inclusion, n = 1),
         lag2_cum_ISO_inclusion = lag(cum_ISO_inclusion, n = 2),
         lag3_cum_ISO_inclusion = lag(cum_ISO_inclusion, n = 3),
         lag4_cum_ISO_inclusion = lag(cum_ISO_inclusion, n = 4),
         lag5_cum_ISO_inclusion = lag(cum_ISO_inclusion, n = 5),
         lag6_cum_ISO_inclusion = lag(cum_ISO_inclusion, n = 6),
         lag7_cum_ISO_inclusion = lag(cum_ISO_inclusion, n = 7),
         lag8_cum_ISO_inclusion = lag(cum_ISO_inclusion, n = 8),
         lag9_cum_ISO_inclusion = lag(cum_ISO_inclusion, n = 9))

#--------------------------------------------------------------------------------------------------

##### SEcTION 4 - Combine regions which have the same ISO_inclusion status, for a given company (duplicates)

duplicate_region_test <- function(test_company = NULL) {
  
  print(paste0(test_company))
  
  temp_company_exposure <- company_exposure %>%
    filter(company == test_company)
  
  temp_regions <- unique(temp_company_exposure$region)
  
  temp_company_exposure_wide <- temp_company_exposure %>%
    select(ISIN_code:ISO_inclusion) %>%
    spread(key = "region", value = "ISO_inclusion")
  
  identical_regions <- c(rep(NA, length(temp_regions)))
  for(i in seq(1, length(temp_regions), 1)) {
    j = i + 1
    while(j <= length(temp_regions)) {
      var1 <- pull(temp_company_exposure_wide[temp_regions[[i]]])
      var2 <- pull(temp_company_exposure_wide[temp_regions[[j]]])
      if(all.equal(var1, var2) == TRUE) { identical_regions[j] <- i }
      j = j + 1
    }
  }
  
  duplicate_regions <- tibble(region_priority = 1:length(temp_regions),
                              duplicate = identical_regions)
  
  temp_company_exposure %<>%
    left_join(duplicate_regions)
  
  return(temp_company_exposure)
  
}

company_list <- unique(company_exposure$company)

# Parallelisation - Leave one free processing core so code can run in background
registerDoParallel(c1, cores = detectCores() - 3)

company_exposure_duplicates <- foreach(i = 1:length(company_list), .packages = c("tidyverse", "magrittr"), .combine = bind_rows) %dopar% 
{ company <- company_list[i]
duplicate_region_test(company) }

stopImplicitCluster()

#company_exposure_duplicates_list <- purrr::map(company_list, duplicate_region_test)
#company_exposure_duplicates <- bind_rows(company_exposure_duplicates_list) %>%
#  select(company:region_priority, duplicate, everything())

for(i in seq(1, 9, 1)) {
  new_var <- paste0("duplicate_lag", i)
  company_exposure_duplicates %<>%
    group_by(ISIN_code, ISO_code) %>%
    mutate(!!(new_var) := lag(duplicate, i))
}

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Allocate ISO codes to each reported region

# Allocate ISO codes to each reported region (subtracting when an ISO code has already been allocated to a higher priority region)
# Duplicate exception lines is to avoid removing ISO codes from regions which are equally defined (e.g. mainland China and Taiwan)
for(i in 1:max(company_region_ranks$region_priority)) {
  if(i > 1) 
  {company_exposure_duplicates %<>%
      mutate(ISO_inclusion = case_when(region_priority != i | (region_priority == i & ISO_inclusion != 1) ~ ISO_inclusion,
                                       is.na(duplicate) ~ ISO_inclusion - lag1_cum_ISO_inclusion,
                                       is.na(duplicate_lag1) ~ ISO_inclusion - lag2_cum_ISO_inclusion,
                                       is.na(duplicate_lag2) ~ ISO_inclusion - lag3_cum_ISO_inclusion,
                                       is.na(duplicate_lag3) ~ ISO_inclusion - lag4_cum_ISO_inclusion,
                                       is.na(duplicate_lag4) ~ ISO_inclusion - lag5_cum_ISO_inclusion,
                                       is.na(duplicate_lag5) ~ ISO_inclusion - lag6_cum_ISO_inclusion,
                                       is.na(duplicate_lag6) ~ ISO_inclusion - lag7_cum_ISO_inclusion,
                                       is.na(duplicate_lag7) ~ ISO_inclusion - lag8_cum_ISO_inclusion,
                                       is.na(duplicate_lag8) ~ ISO_inclusion - lag9_cum_ISO_inclusion))
  }
}

#--------------------------------------------------------------------------------------------------

##### SECTION 6 - Find ISO-code level exposure by mapping regional revenue to ISO codes

# Find exposures
company_exposure_duplicates %<>%
  ungroup() %>%
  select(ISIN_code, company:ISO_inclusion) %>%
  left_join(gdp_iso_code_data) %>%
  mutate(GDP_ISO_inclusion = ISO_inclusion * GDP) %>%
  select(-GDP) %>%
  group_by(ISIN_code, region) %>%
  mutate(ISO_region_share = GDP_ISO_inclusion / sum(GDP_ISO_inclusion)) %>%
  ungroup()

company_exposure_results <- company_geographies_data %>%
  select(ISIN_code, company, region, region_revenue)

company_exposure_results %<>%
  left_join(company_exposure_duplicates) %>%
  mutate(ISO_revenue = ISO_region_share * region_revenue) %>%
  group_by(ISIN_code, company, ISO_code, Country) %>%
  summarise(ISO_revenue = sum(ISO_revenue, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(ISIN_code) %>%
  mutate(#ISO_revenue = as.numeric(sprintf("%.04f", ISO_revenue)),
    ISO_revenue_share = ISO_revenue / sum(ISO_revenue, na.rm = TRUE))

company_exposure_results %<>%
  left_join(lcp_project_regions) %>%
  select(company, LCP_Region, everything()) %>%
  group_by(ISIN_code, company, LCP_Region) %>%
  summarise(LCP_Region_revenue = sum(ISO_revenue),
            LCP_Region_revenue_share = sum(ISO_revenue_share)) %>%
  ungroup()

save_dated(company_exposure_results, "Company_geog_exposure_results", folder = "Output", csv = "YES")

company_exposure_results_wide <- company_exposure_results %>%
  select(ISIN_code:LCP_Region_revenue) %>%
  mutate(LCP_Region = paste0(LCP_Region, "_revenue")) %>%
  spread(key = "LCP_Region", value = "LCP_Region_revenue")

temp <- company_exposure_results %>%
  select(ISIN_code, LCP_Region, LCP_Region_revenue_share) %>%
  mutate(LCP_Region = paste0(LCP_Region, "_revenue_share")) %>%
  spread(key = "LCP_Region", value = "LCP_Region_revenue_share")

company_exposure_results_wide %<>%
  left_join(temp, by = "ISIN_code")

save_dated(company_exposure_results_wide, "Company_geog_exposure_results_wide", folder = "Output", csv = "YES")