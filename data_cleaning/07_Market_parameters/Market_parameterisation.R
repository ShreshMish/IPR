##### Project code:       171211HSB - HSBC Low Carbon Portfolio
##### Date of last edit:  11/08/2018
##### Code author:        Shyamal Patel
##### Description:        This script reads in TR financial data from Excel, and cleans the data before
#####                     further data cleaning takes place
##### Dependencies:       1.  Latest list of markets: "6 - Financial prod/Output/Final markets list.rds"
#####                     2.  Results from TN and RR market parameterisation: "7 - Market parameters/Input/Market parameterisation_final.xlsx"
#####                     3.  Results from MAC curve data cleaning: "3 - MAC curves/Output/MAC_curve_Cleaned_2016USD_data.rds"
#####                     4.  Latest scenario carbon price data: "0 - Scenarios/Output/Carbon_prices_2016USD.rds"
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping, data read in and useful functions for saving/writing files, and QA

packages <- c("tidyverse", "magrittr", "readxl", "here", "stringi")
lapply(packages, require, character.only = TRUE)
source(here::here("utils.R"))


# Read in list of markets
market_data <- readRDS(path_to_data_file("06_Financial_prod/Output/Final_markets_list.rds"))

# Read in market parameterisation results
market_parameter_data <- read_excel(path_to_data_file("07_Market_parameters/Input/Market parameterisation_final.xlsx"),
                                 sheet = "W1. Market parameterisation fin", range = "$G$9:$L$157")

# Read in MAC curve data
mac_curve_data <- readRDS(path_to_data_file("03_MAC_curves/Output/MAC_curve_cleaned_2016USD_data.rds"))

# Read in carbon price data
carbon_price_data <- readRDS(path_to_data_file("00_Scenarios/Output/Carbon_prices_2016USD.rds"))

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean datasets in preparation for merge

# List of markets
market_data %<>%
  arrange(market)

# Market parameterisation results
market_parameter_data %<>%
  arrange(Market) %>%
  rename(market = Market,
         elasticity = Elasticity,
         # Third product differentiation column is final parameters from model (first two are RR and TN responses)
         product_differentiation = `Product differentiation__2`) %>%
  select(market, elasticity, product_differentiation)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Allocate carbon prices to markets, based on carbon price sector definitions

# Carbon price sector allocation
agri_and_fish <- c("Farm Fish Plantation", "Food and broadline retailers", "Food Products", "Forestry", "Paper")
electricity <- c("Electricity retail", "Electricity T&D", "Power generation")
industry <- c("Aerospace", "Aluminum", "Aluminum mining", "Apparel Retailers", "Asset Managers", "Auto Parts", "Automobiles",
              "Banks", "Biotechnology", "Brewers", "Broadcast & Entertain", "Bus.Train & Employmnt",
              "Business Support Svs.", "Ceramics and glass", "Clothing, footwear and accessories", "Coal", "Coal production", "Coal retail",
              "Comm. Vehicles,Trucks", "Computer Hardware", "Computer Services", "Concrete and cement", "Consumer Electronics", "Consumer Finance",
              "Containers & Package", "Copper", "Copper Mining", "Defense", "Diamonds & Gemstones", "Distillers & Vintners", "Divers. Industrials",
              "Drug Retailers", "Dur. Household Prod.", "Elec. Office Equip.", "Electrical Equipment", "Exploration and production",
              "Eq. Investment Inst", "Financial Admin.", "Fixed Line Telecom.", "Full Line Insurance", "Furnishings", "Gambling", "Gas Distribution",
              "Gas processing", "Gas retail", "Gas storage", "Gold", "Gold Mining", "Healthcare Providers", "Heavy Construction",
              "Home Construction", "Industrial Machinery", "Industrial Suppliers", "Insurance Brokers", "Internet", "Investment Services", "Iron & Steel",
              "Iron ore mining", "Life Insurance", "Lithium", "Lithium mining", "Media Agencies", "Medical Equipment", "Medical Supplies", "Metal product retail",
              "Mobile Telecom.", "Mortgage Finance", "Multiutilities", "Nickel mining", "Nondur.Household Prod", "Nonferrous Metals",
              "O&G T&D", "Oil and gas other", "Oil Equip. & Services", "Oil retail", "Other building materials", "Other Chemicals",
              "Other coal-related products", "Other metals production", "Other Mining", "Personal Products", "Petrochemicals", "Pharmaceuticals",
              "Plat.& Precious Metal", "Prop. & Casualty Ins.", "Publishing", "Recreational products and services", "Reinsurance",
              "Renewable Energy Eq.", "Semiconductors", "Soft Drinks", "Software", "Spec.Consumer Service",
              "Specialty Chemicals", "Specialty Finance", "Specialty Retailers", "Telecom", "Telecom. Equipment",
              "Tires", "Tobacco", "Toys", "Waste, Disposal Svs.", "Water")
offroad_transport <- c("Transport - Air", "Transport - Cruises", "Transport - Delivery", "Transport - Ferry", "Transport - Ports",
                       "Transport - Public", "Transport - Rail", "Transport - Shipping", "Transport - Taxi", "Transport - Trucking",
                       "Transport Services", "Travel & Tourism")
buildings <- c("Diversified REITs", "Home Improvement Ret.", "Hotels", "Ind. & Office REITs", "Mortgage REITs", "Real Estate Hold, Dev",
               "Real Estate Services", "Residential REITs", "Restaurants & Bars", "Retail REITs", "Specialty REITs")
road_transport <- c("") #This category is not currently used

# Create identifier column in market data
market_data %<>% 
  mutate(carbon_price_sector = case_when(market %in% agri_and_fish ~ "Agriculture & fishing",
                                         market %in% electricity ~ "Electricity",
                                         market %in% industry ~ "Industry",
                                         market %in% offroad_transport ~ "Off-road",
                                         market %in% buildings ~ "Residential & Commercial",
                                         market %in% road_transport ~ "Road"))

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Allocate MAC curves to markets, based on MAC curve sector definitions

# MAC curve sector allocation
agriculture <- c("Farm Fish Plantation", "Food and broadline retailers", "Food Products")
buildings <- c("Diversified REITs", "Gambling", "Home Improvement Ret.", "Hotels", "Ind. & Office REITs", "Mortgage REITs", "Real Estate Hold, Dev",
               "Real Estate Services", "Residential REITs", "Restaurants & Bars", "Retail REITs", "Specialty REITs")
cement <- c("Ceramics and glass", "Concrete and cement", "Other building materials") #This category is broad but includes cement companies (additional screening required)
chemicals <- c("Biotechnology", "Other Chemicals", "Petrochemicals", "Pharmaceuticals", "Specialty Chemicals")
forestry <- c("Forestry", "Paper")
iron_and_steel <- c("Aluminum", "Copper", "Iron & Steel", "Lithium", "Metal product retail", "Nonferrous Metals", "Other metals production",
                    "Plat.& Precious Metal")
# Includes metals mining
oil_gas_coal <- c("Aluminum mining", "Coal", "Coal production", "Coal retail", "Copper Mining", "Exploration and production", "Gas Distribution",
                  "Gas processing", "Gas retail", "Gas storage", "Gold", "Gold Mining", "Iron ore mining", "Lithium mining",
                  "Nickel mining", "O&G T&D", "Oil and gas other", "Oil Equip. & Services", "Oil retail", "Other coal-related products", "Other Mining",
                  "Tires")
# Catch all for goods markets which have no MAC curve
other_industry <- c("Aerospace", "Auto Parts", "Automobiles", "Brewers", "Clothing, footwear and accessories", "Comm. Vehicles,Trucks",
                    "Computer Hardware", "Consumer Electronics", "Containers & Package", "Defense",
                    "Diamonds & Gemstones", "Distillers & Vintners", "Divers. Industrials", "Dur. Household Prod.",
                    "Elec. Office Equip.", "Electrical Equipment", "Fixed Line Telecom.",
                    "Furnishings", "Gold Mining", "Heavy Construction", "Home Construction",
                    "Industrial Machinery", "Industrial Suppliers", "Medical Equipment",
                    "Medical Supplies", "Mobile Telecom.", "Multiutilities", "Nondur.Household Prod", "Personal Products",
                    "Publishing", "Recreational products and services", "Renewable Energy Eq.", "Semiconductors",
                    "Soft Drinks", "Transport - Rail", "Telecom. Equipment", "Telecom", "Tobacco", "Toys", "Travel & Tourism", "Transport Services", "Water")
power <- c("Electricity retail", "Electricity T&D", "Power generation")
transport_air <- c("Transport - Air")
transport_road <- c("Transport - Delivery", "Transport - Public", "Transport - Taxi", "Transport - Trucking")
transport_sea <- c("Transport - Cruises", "Transport - Ferry", "Transport - Ports", "Transport - Shipping")
waste <- c("Waste, Disposal Svs.")

# Some sectors do not have abatement options based on above MACs - the below category is used for these
# (includes retail of non-emissions intensive products (emissions-intensive are included in above to be conservative
# - may have some production which is not reflected in current product categorisation))
none <- c("Apparel Retailers", "Asset Managers", "Banks", "Broadcast & Entertain",
          "Bus.Train & Employmnt", "Business Support Svs.",  "Computer Services", "Consumer Finance", "Drug Retailers",
          "Eq. Investment Inst", "Financial Admin.", "Full Line Insurance", "Healthcare Providers",
          "Insurance Brokers", "Internet", "Investment Services", "Life Insurance", "Media Agencies",
          "Mortgage Finance", "Prop. & Casualty Ins.", "Reinsurance", "Software", "Spec.Consumer Service",
          "Specialty Retailers", "Specialty Finance")

# Create identifier column in market data
market_data %<>%
  mutate(mac_curve_sector = case_when(market %in% agriculture ~ "Agriculture",
                                      market %in% buildings ~ "Buildings",
                                      market %in% cement ~ "Cement",
                                      market %in% chemicals ~ "Chemicals",
                                      market %in% forestry ~ "Forestry",
                                      market %in% iron_and_steel ~ "Iron and steel",
                                      market %in% oil_gas_coal ~ "Oil, gas and coal",
                                      market %in% other_industry ~ "Other industry",
                                      market %in% power ~ "Power (high renewables / low CCS)",
                                      market %in% transport_air ~ "Transport air",
                                      market %in% transport_road ~ "Transport road",
                                      market %in% transport_sea ~ "Transport sea",
                                      market %in% waste ~ "Waste",
                                      market %in% none ~ "Not applicable",
                                      TRUE ~ NA_character_))

#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Merge together datasets and convert variables as required

# Combine list of markets with elasticity and product differentiation data
market_data %<>%
  left_join(market_parameter_data) %>%
  # Reparameterise product differentiation parameter based on Robert R's suggestion (High = 1/3, Medium = 2/3, Low = 1)
  mutate(product_differentiation = case_when(product_differentiation == "High" ~ (1/3),
                                            product_differentiation == "Medium" ~ (2/3),
                                            product_differentiation == "Low" ~ 1,
                                            TRUE ~ NA_real_))

# Expand grid to cover full set of regions, scenarios and years
markets <- unique(market_data$market)
regions <- unique(carbon_price_data$region)
scenarios <- unique(carbon_price_data$scenario)
years <- unique(carbon_price_data$year)
years <- years[!(years == 2016)]

# Remove redundant variables from other datasets
mac_curve_data %<>%
  select(region, sector, year, lever:mean_abatement_cost) %>%
  rename(mac_curve_sector = sector) %>%
  mutate(region = case_when(region == "Africa and ME" ~ "Africa & ME",
                            TRUE ~ region))

carbon_price_data %<>%
  rename(carbon_price_sector = sector)

# Create full dataset and merge everything in
market_full_data <- expand.grid(scenario = scenarios, region = regions, year = years, market = markets) %>%
  left_join(market_data) %>%
  left_join(carbon_price_data) %>%
  left_join(mac_curve_data) %>%
  arrange(scenario, region, year, market)

# Do not save a CSV (file is very large owing to presence of data on each and every abatement lever)
save_dated(market_full_data, "07_Market_parameters/Output/Market_parameter_data")
