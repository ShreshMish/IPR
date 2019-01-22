panel_data <- readRDS("3 - Cost and competition/Input/Model_panel.rds")

market_list <- sort(unique(panel_data$market))

sector_companies <- function(model_market) {
  
  temp <- panel_data %>%
    filter(market == model_market) %>%
    select(ISIN_code, company, market, revenue_2017) %>%
    group_by(ISIN_code, company, market) %>%
    summarise(revenue = sum(revenue_2017)) %>%
    ungroup()
  
  View(temp)
  
}

#sector_companies("Tobacco")