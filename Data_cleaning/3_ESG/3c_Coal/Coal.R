##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  17/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script reads in coal supply curve data from Dexter Lee's analysis
#####                     in preparation for later modelling on fossil fuel demand destruction
##### Dependencies:       1.  ADD
#####                         Older files can be found in the ".../Dated/" folder

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "3_ESG/3c_Coal"
source("utils.R")

# Read in seaborne coal supply curve data (Dexter's research - Macquarie curve)
coal_supply_curve_data <- read_excel(input_source("Coal_supply_curve_and_companies.xlsx"),
                                sheet = "R2. Seaborne CC 16", range = "$A$8:$C$34")

# Read in coal mine company-level data (Dexter's research)
coal_exposure_data <- read_excel(input_source("Coal_supply_curve_and_companies.xlsx"),
                                    sheet = "R1. Company coal data", range = "$A$9:$R$100")

# Read in ISIN code matching
company_ISIN_matches <- read_excel(input_source("Coal_supply_curve_and_companies.xlsx"),
                                      sheet = "R0. ISIN code matching", range = "$A$8:$C$38")

# Read in company unique names list
company_names <- readRDS("2_Financial/2a_Preliminary/Output/Companies_list.rds")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Create coal company names dataframe for later use ----

# Save coal company names / ISIN codes dataset for product data processing
coal_companies <- company_ISIN_matches %>%
  rename(finance_merge_isin_code = `ISIN code`,
         producer_name = `R1 name`) %>%
  select(finance_merge_isin_code, producer_name)

company_names2 <- company_names %>% 
  mutate(finance_merge_isin_code = case_when(equity_isin_code_1 %in% coal_companies$finance_merge_isin_code ~ equity_isin_code_1,
                                             equity_isin_code_2 %in% coal_companies$finance_merge_isin_code ~ equity_isin_code_2,
                                             equity_isin_code_3 %in% coal_companies$finance_merge_isin_code ~ equity_isin_code_3,
                                             TRUE ~ NA_character_))

coal_companies2 <- company_names2 %>%
  left_join(coal_companies, by = "finance_merge_isin_code") %>%
  select(company_id, company, producer_name) %>%
  filter(!is.na(producer_name)) %>%
  arrange(company_id)

# Save matching of NZT company names to coal producer company names (used for results mapping later)
# Note that producer name may not be unique, but company data will be unique for each company_id in the financial dataset
save_dated(coal_companies2, "Companies_list", folder = "Output", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Clean coal supply curve dataset ----

# Seaborne supply curve data
coal_supply_curve_data2 <- coal_supply_curve_data %>%
  rename(regional_production = `Volume (Mt)`,
         unit_cost = `Price ($/t)`, # Coal cost (supply price) - units are 2016US$
         region = `Country/Region`) %>%
  select(region, everything()) %>%
  # Rename Indonesian coal categories and ROW
  mutate(region = case_when(grepl("Indo", region) ~ "Indonesia",
                            region == "Other" ~ "Rest of World",
                            TRUE ~ region)) %>%
  arrange(region, unit_cost, regional_production) %>%
  # Create IDs for each mine (in unit cost order)
  mutate(mine_ID = rank(unit_cost, ties.method = "first")) %>%
  select(mine_ID, region, unit_cost, regional_production) %>%
  arrange(mine_ID) %>%
  mutate(cumulative_production = cumsum(regional_production))

save_dated(coal_supply_curve_data2, "Seaborne_supply_curve", folder = "Output", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Clean coal company dataset ----

# Rename variables from company exposure dataset
coal_exposure_data2 <- coal_exposure_data %>%
  rename(producer_name = Company,
         country = Country,
         mine = Mine,
         observation_year = `Data year (latest available if multiple)`,
         perc_ownership = `Ownership (%)`,
         currency_units = `Units: currency`,
         production = Production,
         price = Price,
         revenue = `Revenue (millions)`,
         unit_cost = Cost,
         reserve_life = `Resource / reserve life (yrs)`) %>%
  select(producer_name, country, mine, observation_year, perc_ownership,
         currency_units, production, price, revenue, unit_cost, reserve_life)

# Replace 'N/A' with missing values and change type of numeric columns
coal_exposure_data3 <- coal_exposure_data2 %>%
  mutate_at(vars(everything()),
            funs(case_when(. == "N/A" ~ NA_character_,
                           TRUE ~ .))) %>%
  mutate_at(vars(observation_year, perc_ownership, production, price,
                 revenue, unit_cost, reserve_life),
            funs(as.numeric(.)))

save_dated(coal_exposure_data3, "Full_prod_and_economic_data", folder = "Interim", csv = TRUE)

# Remove companies that do not produce thermal coal
coal_exposure_data4 <- coal_exposure_data3 %>%
  # Rio Tinto no longer coal producer; Shaanxi Xishan is coking coal
  # Shenergy is O&G company - coal exposure unclear, Hubei Energy is a utility - coal exposure unclear
  filter(!(producer_name %in% c("Rio Tinto", "SHANXI XISHAN C&ELY.PWR. 'A'",
                                "SHENERGY 'A'", "HUBEI ENERGY GROUP 'A'")))

# Add missing companies which have exposure to coal, but no asset-level data
coal_exposure_data5 <- coal_exposure_data4 %>%
  # Four companies have exposure to miningcoal mining but no mine-level data (Astra Int - Indonesian conglomerate, CEZ - Czech conglomerate,
  # Inversiones Argos - Grupo Argos is a Colombian conglomerate, Idemitsu Kosan - Japanese energy conglomerate with coal assets in Australia)
  mutate(production = case_when(producer_name %in% c("ASTRA INTERNATIONAL", "CEZ", "INVERSIONES ARGOS", "IDEMITSU KOSAN") ~ 1,
                                TRUE ~ production),
         mine = case_when(producer_name %in% c("ASTRA INTERNATIONAL", "CEZ", "INVERSIONES ARGOS", "IDEMITSU KOSAN") ~ "Dummy",
                          TRUE ~ mine)) %>%
  # Remove blank entries in mine column (belong to Glencore)
  filter(!is.na(mine))

# Fill in missing company-level data with default values
coal_exposure_data6 <- coal_exposure_data5 %>%
  # Observation year default = 2017, ownership is 100% owned by company, reserve life is industry-average
  mutate(observation_year = ifelse(is.na(observation_year), 2017, observation_year),
         perc_ownership = ifelse(is.na(perc_ownership), 100, perc_ownership),
         reserve_life = ifelse(is.na(reserve_life), mean(reserve_life, na.rm = TRUE), reserve_life))

# Calculate mine-level production shares for each company
coal_exposure_data7 <- coal_exposure_data6 %>%
  group_by(producer_name) %>%
  # Calculate share of production for mines with existing production data
  mutate(production_share = production * perc_ownership / sum(production * perc_ownership, na.rm = TRUE)) %>%
  # Fill in missing observations with company average
  mutate(production_share = case_when(is.na(production_share) & n() == 1 ~ 1, # NA value companies with just 1 mine
                                      is.na(production_share) ~ mean(production_share, na.rm = TRUE), # Companies with >1 mine with production data, and >1 mine with no production data
                                      TRUE ~ production_share)) %>%
  # Rebase each mine's share of production based on new values
  mutate(production_share = production_share / sum(production_share)) %>%
  ungroup() %>%
  # Remove redundant variables (some variables would be useful but have data coverage which is too patchy to be used)
  # Note that each company's absolute level of production is not used in coal demand destruction modelling - regional exposure based on
  # the location of its mines determines impacts
  select(producer_name, country, mine, observation_year, production_share, reserve_life)
                                            
# Replace reserve life with 2 if current value is 1
coal_exposure_data8 <- coal_exposure_data7 %>%
  mutate(reserve_life = ifelse(reserve_life < 2, 2, reserve_life))

save_dated(coal_exposure_data8, "Model_reg_exposure_data", folder = "Output", csv = TRUE)