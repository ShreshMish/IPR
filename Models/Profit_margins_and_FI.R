library(tidyverse)
library(magrittr)
library(readxl)

panel_data <- readRDS("C:/Users/Shyamal/Vivid Economics Ltd/171211HSB - Low Carbon Portfolio - Documents/6 - analysis/Models/3 - Cost and competition/Input/Model_panel.rds")
credit_data <- read_excel("C:/Users/Shyamal/Vivid Economics Ltd/171211HSB - Low Carbon Portfolio - Documents/5 - data/Raw data from HSBC/Thomson Reuters financial/180830 TR data/Additional credit data 300818.xlsx",
                          sheet = "2017", range = "$B$1:$AR$2781")
us_inflation_data <- read_excel("C:/Users/Shyamal/Vivid Economics Ltd/171211HSB - Low Carbon Portfolio - Documents/6 - analysis/Data cleaning/1 - Financial prelim/Input/OECD CPI inflation.xlsx",
                                sheet = "R1. OECD CPI inflation", range = "$A$9:$G$4408")

parameter_data <- tibble(discount_rate = 0.0575)

### Clean up US inflation data

# Clean US inflation data
us_inflation_data %<>%
  rename(iso_code = LOCATION,
         date = TIME,
         inflation_rate = Value) %>%
  select(iso_code, date, inflation_rate) %>%
  filter(iso_code == "USA") %>%
  mutate(year = as.numeric(substr(date, 1, 4))) %>%
  group_by(iso_code, year) %>%
  #Annual average inflation rate
  summarise(inflation_rate = mean(inflation_rate),
            count = n())

# 2016 value from the above approach is close to WB data (1.26% vs. 1.25%) so approach is close enough
us_inflation_rates <- us_inflation_data$inflation_rate
names(us_inflation_rates) <- us_inflation_data$year
us_inflation_rates <- us_inflation_rates / 100 + 1

us_price_level <- tibble(year = us_inflation_data$year,
                         # 1 + inflation rate
                         inflation_rate = 1 + us_inflation_data$inflation_rate / 100)

us_price_level %<>%
  mutate(cum_inflation_rate = cumprod(inflation_rate),
         price_level = 100 * cum_inflation_rate / cum_inflation_rate[year == 2016])

us_price_level_vec <- us_price_level$price_level
names(us_price_level_vec) <- us_price_level$year

### Process panel model data to arrive at market cap implied profit margins and net income based profit margins (pre-tax)
profit_margin_data <- panel_data %>%
  select(scenario:profit_2017, corporation_tax_rate) %>%
  filter(scenario == "BAU") %>%
  mutate(corporation_tax_rate = corporation_tax_rate / 100) %>%
  rename(net_income_2017 = profit_2017) %>%
  # Defined as net income before tax (in the case where costs are positive) divided by revenue
  mutate(net_income_margin_2017 = ifelse(revenue_2017 - net_income_2017 / (1 - corporation_tax_rate) >= 0, 1 / (1 - corporation_tax_rate), 1) *
           net_income_2017 / revenue_2017 ) %>%
  mutate(mcap_margin_2017 = ifelse(revenue_2017 - net_income_2017 / (1 - corporation_tax_rate) >= 0, 1 / (1 - corporation_tax_rate), 1) *
           market_cap_2017 * (parameter_data$discount_rate / (1 + parameter_data$discount_rate)) * 1000 / revenue_2017) %>%
  group_by(scenario, ISIN_code, company) %>%
  summarise(market_cap_2017 = sum(market_cap_2017),
            revenue_2017 = sum(revenue_2017 / 10^3),
            net_income_margin_2017 = mean(net_income_margin_2017, na.rm = TRUE),
            mcap_margin_2017 = mean(mcap_margin_2017, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-scenario)

### Process credit data for total liabilities and other financials (EBIT, total assets, current assets & liabilities)
# NB: inflation adjustment needs to be made here (2017US$ -> 2016US$ (model currency))
liabilities_data <- credit_data %>%
  select(NAME, `ISIN CODE`, `XWC03101~U$`, `XWC02201~U$`, `XWC03495~U$`, `XWC18191~U$`,
         `XWC05476~U$`, `XWC03351~U$`, `XWC02999~U$`) %>%
  rename(company = NAME,
         ISIN_code = `ISIN CODE`,
         current_assets = `XWC02201~U$`,
         retained_earnings = `XWC03495~U$`,
         EBIT = `XWC18191~U$`,
         book_value = `XWC05476~U$`,
         total_liabilities = `XWC03351~U$`,
         total_assets = `XWC02999~U$`) %>%
  select(-`XWC03101~U$`) %>%
  # Change units to bn US$, and adjust to be in 2016US$
  mutate_at(.vars = vars(current_assets:total_assets),
            .funs = funs(as.numeric(.) / 10^3 * us_price_level_vec["2016"] / us_price_level_vec["2017"])) %>%
  # Retained earnings is not available in the 180830 FI dataset
  select(-retained_earnings)

profit_margin_data2 <- profit_margin_data %>%
  left_join(liabilities_data) %>%
  select(ISIN_code, company, market_cap_2017, total_liabilities, revenue_2017, net_income_margin_2017, mcap_margin_2017) %>%
  rename_at(.vars = vars(ends_with("_2017")),
            .funs = funs(str_replace(., "_2017", ""))) %>%
  mutate(debt_ratio = total_liabilities / (market_cap + total_liabilities))

write_csv(profit_margin_data2, 
          "C:/Users/Shyamal/Vivid Economics Ltd/171211HSB - Low Carbon Portfolio - Documents/6 - analysis/Models/Profit margin and FI data.csv")
