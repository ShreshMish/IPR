### Read in required datasets
NPV_stranding_impacts <- readRDS("1 - Demand destruction/Output/DD_NPV_stranding_impacts.rds")

# Must save a custom version of the stranding data compact dataset, that includes the Price_G variable (change the code to include this longer term)
dd_stranding_data <- readRDS("1 - Demand destruction/Interim/DD_stranding_data_compact.rds")

# Process and store a cleaned version of the company-level results dataset for cross checking against LLO results
company_results <- NPV_stranding_impacts %>%
  ungroup() %>% 
  filter(Scenario == "2DS_central") %>%
  select(ISIN_Code, VE_Name, Product, Scenario,
         NPV_Profit_impact_pct, NPV_Margin_impact_pct, NPV_Stranding_impact_pct) %>%
  unique() %>%
  filter(!is.na(ISIN_Code)) %>%
  arrange(Product, desc(NPV_Profit_impact_pct))

company_results_oil <- company_results %>%
  filter(Product == "Liquid")

company_results_gas <- company_results %>%
  filter(Product == "Gas")

company_results_all <- company_results %>%
  filter(Product == "All")

write_csv(company_results_all, "1 - Demand destruction/Interim/company_all_results.csv")
write_csv(company_results_oil, "1 - Demand destruction/Interim/company_oil_results.csv")
write_csv(company_results_gas, "1 - Demand destruction/Interim/company_gas_results.csv")

# Produce charts showing prices and production under each scenario for comparison against LLO
global_results <- dd_stranding_data %>%
  select(Product, Year, Scenario, Production_G, Price_G) 

chart_fn <- function(fuel, variable) {
  
  varname <- rlang::sym(paste0(variable))
  print(varname)
  temp <- global_results %>%
    select(Product, Year, Scenario, !!varname) %>%
    filter(Product == fuel)
  
  last_plot <- ggplot(temp) + 
    geom_line(aes(x = Year, y = !!varname, colour = Scenario)) + 
    theme_vivid() + scale_colour_vivid_house()
  
  filename <- paste0("1 - Demand destruction/Interim/", fuel, "_", variable, ".png")
  print(filename)
  ggsave(filename, plot = last_plot, width = 16, height = 9)
  
}

product_variable_combs <- expand.grid(fuel = c("Liquid", "Gas"),
                                      variable = c("Production_G", "Price_G")) %>%
  mutate(combs = paste(fuel, variable),
         combs = as.list(strsplit(combs, " "))) %>%
  select(combs)

product_variable_combs <- as.list(product_variable_combs$combs)
lapply(product_variable_combs, wrapper_chart_fn <- function(x) {chart_fn(x[1], x[2])})

# Calculate aggregate revenues across the industry (all producers - global production and prices)
oil_data_2 <- dd_stranding_data %>%
  filter(VE_Name == "BP" & Year != 2017 & Product != "All") %>%
  select(Product, Year, Scenario, Production_G, Price_G)

oil_data_3 <- expand.grid(Year = seq(2018, 2050, 1),
                          Product = unique(oil_data_2$Product),
                          Scenario = unique(oil_data_2$Scenario)) %>%
  left_join(oil_data_2) %>%
  group_by(Product, Scenario) %>%
  mutate(Price_G = approx(x = Year, y = Price_G, xout = Year, rule = 2)$y,
         Production_G = approx(x = Year, y = Production_G, xout = Year, rule = 2)$y) %>%
  ungroup() %>%
  mutate(Revenue_G = Price_G * Production_G)

oil_data_4 <- oil_data_3 %>%
  mutate(Revenue_G_NPV = Revenue_G / (1 + 0.0575) ^ (Year - 2018)) %>%
  group_by(Product, Scenario) %>% 
  summarise(Revenue_G_NPV = sum(Revenue_G_NPV)) %>%
  group_by(Product) %>%
  mutate(Revenue_G_impact = Revenue_G_NPV / Revenue_G_NPV[[which(Scenario == "BAU")]] - 1)

View(oil_data_4)



