##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  11/08/2018
##### Code author:        Shyamal Patel
##### Description:        This script reads in TIAM scenario data from Excel and cleans carbon price data for
#####                     use in later calculations and modelling
##### Dependencies:       Latest Thomson Reuters financial data Excel file
#####                     (current = "Interim data/180716 TR data consolidated.xlsx")

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA

packages <- c("tidyverse", "magrittr", "readxl", "here", "stringi", "themeVE")
lapply(packages, require, character.only = TRUE)
source(here::here("utils.R"))

# Read in TIAM scenario data
tiam_scenarios_data <- read_excel(path_to_data_file("00_Scenarios/Input/TIAM scenario analysis v2.xlsx"),
                                 sheet = "W0. Carbon prices for R", range = "$B$8:$I$260")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean, reshape and interpolate carbon price data (2016-50)

# Prices are already in 2016USD
carbon_price_2016USD_data <- tiam_scenarios_data %>%
  gather(key = "year", value = "carbon_price", -(Scenario:Sector)) %>%
  arrange(Scenario, Region, Sector, year) %>%
  mutate(year = as.numeric(year)) %>%
  rename(scenario = Scenario,
         region = Region,
         sector = Sector)

# Expand grid to create rows for additional years
scenarios <- unique(carbon_price_2016USD_data$scenario)
regions <- unique(carbon_price_2016USD_data$region)
sectors <- unique(carbon_price_2016USD_data$sector)
years <- seq(2016, 2050, 1)
years <- subset(years, subset = !(years %in% carbon_price_2016USD_data$year))

carbon_price_new_years <- expand.grid(scenarios, regions, sectors, years) %>%
  rename(scenario = Var1,
         region = Var2,
         sector = Var3,
         year = Var4)

carbon_price_2016USD_data %<>%
  bind_rows(carbon_price_new_years) %>%
  arrange(scenario, region, sector, year)

# Interpolate between years (point-to-point)
carbon_price_2016USD_data %<>%
  group_by(scenario, region, sector) %>%
  mutate(carbon_price = approx(x = year, y = carbon_price, xout = year)$y) %>%
  ungroup()

# Carbon prices are set to zero prior to 2020 (first year of climate policy action)
carbon_price_2016USD_data %<>%
  mutate(carbon_price = ifelse(year < 2020, 0, carbon_price))

# Create function to test plots
plot_carbon_prices <- function(plot_scenario, plot_region = NULL, plot_sector = NULL) {
  temp <- carbon_price_2016USD_data %>%
    filter(scenario == plot_scenario & region == plot_region & sector == plot_sector)
  
  windows()
  ggplot(temp) +
    geom_line(aes(x = year, y = carbon_price)) + 
    theme_vivid()
}

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Rename regions to be consistent with financial dataset

carbon_price_2016USD_data %<>%
  mutate(region = case_when(region == "Africa and ME" ~ "Africa & ME",
                            TRUE ~ region))

save_dated(carbon_price_2016USD_data, "00_Scenarios/Output/Carbon_prices_2016USD", csv = TRUE)