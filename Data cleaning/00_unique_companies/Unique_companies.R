library(tidyverse)
library(magrittr)
library(readxl)

unique_companies <- read_excel("00_unique_companies/Input/190122_Unique_companies.xlsx",
                               skip = 7, guess_max = 5000)

unique_companies2 <- unique_companies %>%
  rename(ISIN_code_2 = `ISIN code 2`,
         ISIN_code_3 = `ISIN code 3`,
         company_name_2 = `company 2`,
         company_name_3 = `company 3`) %>%
  select(-`ISIN code 1`)

panel_model_data <- readRDS("00_unique_companies/Input/Model_panel.rds") %>%
  filter(scenario == "BAU") %>%
  group_by(ISIN_code, company) %>%
  summarise(revenue_2017 = sum(revenue_2017, na.rm = TRUE),
            market_cap_2017 = sum(market_cap_2017, na.rm = TRUE),
            profit_2017 = sum(profit_2017, na.rm = TRUE)) %>%
  ungroup()

panel_model_data2 <- panel_model_data %>%
  left_join(unique_companies2) %>%
  select(ISIN_code, company, ISIN_code_2, company_name_2, ISIN_code_3, company_name_3, everything()) %>%
  filter(!is.na(ISIN_code)) %>%
  mutate(unique_id = paste(ISIN_code,
                           ifelse(is.na(ISIN_code_2), "", ISIN_code_2),
                           ifelse(is.na(ISIN_code_3), "", ISIN_code_3)),
         unique_id = as.vector(strsplit(unique_id, " ")))

panel_model_data2$unique_id <- lapply(panel_model_data2$unique_id, sort_function <- function(x) {sort(x, na.last = TRUE)})

concatenate_function <- function(x) paste(x, collapse = " ")
panel_model_data3 <- panel_model_data2 %>%
  mutate(unique_id = map_chr(unique_id, concatenate_function)) %>%
  select(unique_id, everything()) %>%
  arrange(unique_id)

write_csv(panel_model_data3, "00_unique_companies/Output/190122_Unique_companies_panel.csv")

