##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  29/01/2019
##### Code author:        Shyamal Patel
##### Description:        This script reads in geographic exposure data cleaning outputs and models company-level exposure
##### Dependencies:       1.  Latest list of TR unique geographies: "2_Financial/2b_Geographic_exposure/Interim/TR_geographies_full_list
#####                     2.  Latest region-ISO code matching: "4 - Financial geog/Interim/TR/TR_mapped_geographies
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "2_Financial/2b_Geographic_exposure"
source("utils.R")

# Read in region-ISO code mapping dataset
region_iso_code_mapping_data <- readRDS("2_Financial/2b_Geographic_exposure/Interim/Mapped_geog_categories.rds") %>%
  rename(region = Region) #Rename region variable to match other files

# Read in company-level geographic exposure dataset
company_geog_data <- readRDS("2_Financial/2b_Geographic_exposure/Interim/Geog_exposure_full_data.rds")

# Read in GDP by ISO code dataset
gdp_iso_code_data <- readRDS("2_Financial/2b_Geographic_exposure/Interim/GDP_by_ISO_data.rds") %>%
  rename(ISO_code = `ISO code`)

# Readin NZT toolkit region definitions
nzt_regions_data <- read_excel(input_source("ISO_code_and_regional_gdp.xlsx"),
                          sheet = "W5. LCP region mapping", range = "$A$7:$F$211") %>%
  select(ISO_code, Country, NZT_region)

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Company missing region calculation - should be NULL

company_geog_data2 <- company_geog_data %>%
  # Filter out any redundant regions, and replace negative value regions with 0
  mutate(region_revenue = ifelse(region_revenue < 0, 0, region_revenue)) %>%
  filter((!is.na(region)) & (!is.na(region_revenue))) #Remove NA regions to avoid netting these from superregions in calculations

# Find regions which have not yet been defined in the mapped geographies file (region_iso_code_mapping_data)
# should be NULL
missing_region_test <- company_geog_data2 %>%
  filter(!(region %in% region_iso_code_mapping_data$region))

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Company region ISO allocation rules (order in which ISO codes are assigned to regions)

# Merge together company-region and region-ISO code mapping datasets
company_geog_data3 <- company_geog_data2 %>%
  select(company_id, company, region) %>%
  left_join(region_iso_code_mapping_data) %>%
  rename(ISO_inclusion = Inclusion)

# Create region ordering rank function
# NB - Human input may be required here if there are many sources of conflict in the prioritisation
#      for instance, regional exposure sources: UK, OECD, Europe, EMEA, Rest of World could cause issues
company_region_rank_data <- company_geog_data3 %>%
  group_by(company_id, company, region) %>%
  mutate(ISO_count = sum(ISO_inclusion)) %>%
  summarise(ISO_count = mean(ISO_count)) %>%
  ungroup() %>%
  arrange(company_id, ISO_count) %>%
  group_by(company_id) %>%
  mutate(region_priority = row_number()) %>%
  ungroup()

save_dated(company_region_rank_data, "Company_geog_region_priorities", folder = "Interim", csv = TRUE)

# Higher order lagged variables are used for duplicate region entries (ISO code mapping, not name duplicates)
company_geog_data4 <- company_geog_data3 %>%
  left_join(company_region_rank_data) %>%
  arrange(company_id, ISO_code, region_priority) %>%
  group_by(company_id, company, ISO_code) %>%
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

# Split the company geog data frame into a list based on company_id variable
company_geog_data_list <- company_geog_data4 %>%
  group_by(company_id) %>%
  nest() %>%
  select(data) %>% 
  unlist(recursive = FALSE)

# Create list of company unique IDs as they appear in company_geog_data_4
company_id_list <- unique(company_geog_data4$company_id)

# Set names of each dataframe
company_geog_data_list <- setNames(company_geog_data_list, company_id_list)

# Define function to test for duplicates and run over the list of dataframes
duplicate_region_test <- function(test_company_id = NULL) {
  
  temp_company_exposure <- company_geog_data_list[[test_company_id]]
  temp_regions <- unique(temp_company_exposure$region)
  
  temp_company_exposure_wide <- temp_company_exposure %>%
    select(company:ISO_inclusion) %>%
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
    left_join(duplicate_regions, by = c("region_priority")) %>%
    mutate(company_id = test_company_id) %>%
    select(company_id, everything())
  
  # Find duplicate lag variables if and only if there are non-NA duplicate variable values
  if(sum(is.na(temp_company_exposure$duplicate)) != nrow(temp_company_exposure)) {
    for(i in seq(1, 9, 1)) {
      new_var <- paste0("duplicate_lag", i)
      temp_company_exposure %<>%
        group_by(ISO_code) %>%
        mutate(!!(new_var) := lag(duplicate, i))
    }
  }

  return(temp_company_exposure)
}

# Combine results
company_geog_duplicate_data <- map(company_id_list, duplicate_region_test) %>%
  bind_rows()

# Duplicate lag variables
# for(i in seq(1, 9, 1)) {
#   new_var <- paste0("duplicate_lag", i)
#   company_geog_duplicate_data %<>%
#     group_by(company_id, ISO_code) %>%
#     mutate(!!(new_var) := lag(duplicate, i))
# }

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Allocate ISO codes to each reported region

# Allocate ISO codes to each reported region (subtracting when an ISO code has already been allocated to a higher priority region)
# Duplicate exception lines is to avoid removing ISO codes from regions which are equally defined (e.g. mainland China and Taiwan)
for(i in 1:max(company_region_rank_data$region_priority)) {
  if(i > 1) 
  # {company_exposure_duplicates %<>%
  {company_geog_duplicate_data %<>%
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
company_geog_duplicate_data2 <- company_geog_duplicate_data %>%
  ungroup() %>%
  select(company_id, company:ISO_inclusion) %>%
  left_join(gdp_iso_code_data) %>%
  mutate(GDP_ISO_inclusion = ISO_inclusion * GDP) %>%
  select(-GDP) %>%
  group_by(company_id, region) %>%
  mutate(ISO_region_share = GDP_ISO_inclusion / sum(GDP_ISO_inclusion)) %>%
  ungroup()

company_exposure_results <- company_geog_data2 %>%
  select(company_id, company, region, region_revenue)

company_exposure_results2 <- company_exposure_results %>%
  left_join(company_geog_duplicate_data2) %>%
  mutate(ISO_revenue = ISO_region_share * region_revenue) %>%
  group_by(company_id, company, ISO_code, Country) %>%
  summarise(ISO_revenue = sum(ISO_revenue, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(company_id) %>%
  mutate(#ISO_revenue = as.numeric(sprintf("%.04f", ISO_revenue)),
    ISO_revenue_share = ISO_revenue / sum(ISO_revenue, na.rm = TRUE))

company_exposure_results3 <- company_exposure_results2 %>%
  left_join(nzt_regions_data) %>%
  select(company, NZT_region, everything()) %>%
  group_by(company_id, company, NZT_region) %>%
  summarise(NZT_region_revenue = sum(ISO_revenue),
            NZT_region_revenue_share = sum(ISO_revenue_share)) %>%
  ungroup()

save_dated(company_exposure_results3, "Geog_exposure_results", folder = "Output", csv = TRUE)

company_exposure_results_wide <- company_exposure_results3 %>%
  select(company_id:NZT_region_revenue) %>%
  mutate(NZT_region = paste0(NZT_region, "_revenue")) %>%
  spread(key = "NZT_region", value = "NZT_region_revenue")

temp <- company_exposure_results3 %>%
  select(company_id, NZT_region, NZT_region_revenue_share) %>%
  mutate(NZT_region = paste0(NZT_region, "_revenue_share")) %>%
  spread(key = "NZT_region", value = "NZT_region_revenue_share")

company_exposure_results_wide2 <- company_exposure_results_wide %>%
  left_join(temp, by = "company_id")

save_dated(company_exposure_results_wide2, "Geog_exposure_results_wide", folder = "Output", csv = TRUE)