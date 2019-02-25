##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  19/02/2019
##### Code author:        Shyamal Patel
##### Description:        This script adds net income margin data to the panel and calculates the 'gamma factor'
#####                     revenue growth that is required to reconcile company market cap with assumed profit margin (either median net income
#####                     profit margin by market (model market), or company profit margin if the company is above the median)
##### Dependencies:       1. Results from Combine_datasets.R script: "Model_panel_final.rds"
#####                     2. Company median (over 2013 - 17 observation) net income profit margins [accountancy margins] 
##### Notes:

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Define master_folder and source utils file which contains useful functions
main_save_folder <- "4_Panel"
source("utils.R")

# Read in current model panel final
model_panel <- readRDS(fs::path(main_save_folder, "Output", "model_panel_final.rds"))

# Read in net income median company profit margins
net_income_margin_data <- readRDS("2_Financial/2d_TEMP_MARGINS/Output/Company_net_income_margins.rds")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Merge in net income margins and calculate model market percentiles ----

net_income_margin_data2 <- net_income_margin_data %>%
  select(company_id, company, net_income_margin)

model_panel2 <- model_panel %>%
  left_join(net_income_margin_data2, by = c("company_id", "company")) %>%
  select(company_id:corporation_tax_rate, net_income_margin, everything())

market_margins <- model_panel2 %>% 
  select(market, company_id, company, net_income_margin) %>%
  unique()

market_margins2 <- market_margins %>%
  nest(-market) %>%
  mutate(quantiles = map(data, ~quantile(.$net_income_margin, seq(0, 1, 0.1))),
         quantiles = map(quantiles, ~ bind_rows(.) %>% gather())) %>%
  unnest(quantiles) %>%
  spread(key = key, value = value)

msci_margins <- market_margins %>%
  nest() %>%
  mutate(quantiles = map(data, ~quantile(.$net_income_margin, seq(0, 1, 0.1))),
         quantiles = map(quantiles, ~ bind_rows(.) %>% gather())) %>%
  unnest(quantiles) %>%
  spread(key = key, value = value) %>%
  mutate(market = "MSCI ACWI")

market_margins3 <- market_margins2 %>%
  bind_rows(msci_margins)

save_dated(market_margins3, "Market_net_income_margins", folder = "Interim", csv = TRUE)

# Keep model median and apply adjustments:
# a) if <0, set to MSCI ACWI median
# b) if above 50%, capped to 50%

market_margins4 <- market_margins3 %>%
  gather(key = percentile, value = value, -(market)) %>%
  filter(percentile == "50%") %>%
  select(-percentile) %>%
  mutate(net_income_margin_med = case_when(value < 0 ~ value[[which(market == "MSCI ACWI")]],
                                           value >= 0.5 ~ 0.5,
                                           TRUE ~ value)) %>%
  select(-value)

save_dated(market_margins4, "Market_net_income_margins", folder = "Output", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - Add median net income margins to the panel and adjust company margins ----

model_panel3 <- model_panel2 %>%
  left_join(market_margins4, by = "market") %>%
  select(company_id:net_income_margin, net_income_margin_med, everything())

# Adjust net income margins below the median to the median
# Do not adjust net income margins above the median but below 50%
# Screen values above 50% using 50%
model_panel4 <- model_panel3 %>%
  mutate(net_income_margin_new = case_when(net_income_margin < net_income_margin_med ~ net_income_margin_med,
                                           net_income_margin > 0.5 ~ 0.5,
                                           TRUE ~ net_income_margin)) %>%
  select(company_id:net_income_margin_med, net_income_margin_new, everything())

model_panel5 <- model_panel4 %>%
  select(-net_income_margin, -net_income_margin_med) %>% 
  rename(net_income_margin = net_income_margin_new)

save_dated(model_panel5, "Company_adj_net_income_margins", folder = "Interim", csv = TRUE)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Calculate gamma parameter ----

discount_rate <- 0.0575

# Write up how gamma is defined properly - this is correct based on the algebra, how we calculate profit flows
# and what the definition of gamma is (yearly Q multiplier factor)
model_panel6 <- model_panel5 %>%
  mutate(a_factor = market_cap_2017 / (revenue_2017 * net_income_margin),
         gamma_factor = (a_factor * (1 + discount_rate)) / (a_factor + 1 + discount_rate)) %>%
  select(-a_factor) %>%
  select(company_id:net_income_margin, gamma_factor, everything())
  
save_dated(model_panel6, "Model_panel_finalv2", folder = "Output", csv = TRUE)