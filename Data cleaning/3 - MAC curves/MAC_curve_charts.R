##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  09/08/2018
##### Code author:        Shyamal Patel
##### Description:        This script generates MAC curve data (units already 2016US$ following Excel work)
##### Dependencies:       1.  Latest Mac curve output file: "3 - MAC curves/Output/MAC_curve_cleaned_2016USD_data.rds"
#####                         Older results files are archived in the ".../Dated" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA

packages <- c("tidyverse", "magrittr", "readxl", "here", "stringi", "themeVE")
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
  main_path <- paste0("3 - MAC curves/", folder)
  dated_path <- paste0("3 - MAC curves/", folder, "/Dated/")
  
  # Save main file
  saveRDS(data, here(main_path, paste0(name, ".rds")))
  # Save dated file (optional)
  if(dated == "YES") {saveRDS(data, here(dated_path, paste0(date, "_", name, ".rds")))}
  # Save dated CSV file (optional)
  if(csv == "YES") {write_csv(data, here(dated_path, paste0(date, "_", name, ".csv")))}
}

mac_data_cleaned <- readRDS(here("3 - MAC curves/Output/MAC_curve_cleaned_2016USD_data.rds"))

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Generate MAC curve charts

mac_curve_chart <- function(mac_region = region_code, mac_sector = sector_code, mac_year = year,
                            mac_cost_cap = "ON") {
  
  temp <- mac_data_cleaned %>%
    filter(region_code == mac_region & sector_code == mac_sector & year == mac_year) %>%
    arrange(abatement_cost) %>%
    select(-mean_abatement_cost)
  
  if(mac_cost_cap == "ON") {temp %<>% mutate(abatement_cost = case_when(abatement_cost < 0 ~ 0, TRUE ~ abatement_cost))}
  
  temp %<>%
    mutate(ymin = 0,
           lag_cum_abatement_potential = lag(cum_abatement_potential),
           lag_cum_abatement_potential = ifelse(is.na(lag_cum_abatement_potential), 0, lag_cum_abatement_potential),
           lever = fct_inorder(lever, ordered = TRUE))
  
  ggplot(temp) +
    geom_rect(aes(xmin = lag_cum_abatement_potential, xmax = cum_abatement_potential,
                  ymin = 0, ymax = abatement_cost, colour = lever, fill = lever)) +
    scale_x_continuous(name = "Abatement (% of total emissions)", expand = c(0,0), labels = scales::comma) + 
    scale_y_continuous(name = "Abatement cost (2016US$)", expand = c(0,0)) + 
    scale_colour_vivid_house2() +
    scale_fill_vivid_house2() + 
    theme_vivid() +
    ggtitle(paste0(mac_region, " ", mac_sector, " ", mac_year, " MAC curve, no negative costs = ", mac_cost_cap))
  
  save_name <- paste0("3 - MAC curves/Output/MAC curve plots/", mac_region, "_", mac_sector, "_", mac_year,
                      "_", mac_cost_cap, ".png")
  ggsave(save_name, width = 29, height = 13.5, units = c("cm"))
}

regions <- unique(mac_data_cleaned$region_code)
sectors <- unique(mac_data_cleaned$sector_code)
years <- seq(2015, 2050, by = 5)
cost_cap <- c("ON", "OFF")
parameters <- expand.grid(regions, sectors, years, cost_cap) %>%
  mutate(Unique = paste0(Var1, "_", Var2, "_", Var3, "_", Var4))

parameters_vec <- as.list(parameters$Unique)

loop_mac_curve_chart <- function(unique_param = NULL) {
  region <- substring(unique_param, 1, stri_locate_all(unique_param, regex = "_")[[1]][1,1] - 1)
  sector <- substring(unique_param, stri_locate_all(unique_param, regex = "_")[[1]][1,1] + 1,
                      stri_locate_all(unique_param, regex = "_")[[1]][2,1] - 1)
  year <- substring(unique_param, stri_locate_all(unique_param, regex = "_")[[1]][2,1] + 1,
                    stri_locate_all(unique_param, regex = "_")[[1]][3,1] - 1)
  cost_cap <- substring(unique_param, stri_locate_all(unique_param, regex = "_")[[1]][3,1] + 1,
                        nchar(unique_param))
  
  mac_curve_chart(region, sector, year, cost_cap)
}

map(parameters_vec, loop_mac_curve_chart)