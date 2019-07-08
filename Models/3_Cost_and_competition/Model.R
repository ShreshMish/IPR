##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  08/07/2019
##### Model author:       Robert Ritz
##### Code author:        Shyamal Patel
##### Dependencies:       N/A
##### Notes:              This file defines the Robert Ritz cost & competition model applied over model scenarios, regions and sectors
#####                     The function is defined below, but no data is called in by this script
##### Called by:          Run_model.R

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Define function to run the Robert Ritz model ----

# Note that value chain element currently does nothing
run_rr_model <- function(rr_data, rr_carbon_cost_pwh_data = NULL, rr_parameter_data, rr_value_chain_element) {
  
  # Combine carbon cost dataset with RR dataset (defining new variables as carbon_cost_pwh_i) if not NULL
  if(!is.null(rr_carbon_cost_pwh_data))
    # Note that carbon costs in the PWH sector are 0 in 2017 so merging using this variable does not cause issues
  {rr_panel_model_run <- rr_data %>% left_join(rr_carbon_cost_pwh_data, by = c("scenario", "region", "carbon_cost_pwh_2017"))
  } else  {rr_panel_model_run <- rr_data
  for(j in model_years) {
    carbon_cost_pwh = rlang::sym(paste0("carbon_cost_pwh_", j))
    rr_panel_model_run %<>%
      mutate(!!carbon_cost_pwh := 0)
  }
  }
  
  # User input on abatement options: if "OFF", replace co2 cost with co2 price in all years, otherwise, change nothing
  for(i in model_years) {
    carbon_price = rlang::sym(paste0("carbon_price_", i))
    carbon_cost = rlang::sym(paste0("carbon_cost_", i))
    carbon_cost_pwh = rlang::sym(paste0("carbon_cost_pwh_", i))
    
    rr_panel_model_run %<>%
      # Adjust carbon costs for abatement opporunities switch (if OFF, carbon costs = carbon price)
      mutate(!!carbon_cost := if(rr_parameter_data$abatement_opportunities == "ON") (!!carbon_cost) else (!!carbon_price),
             !!carbon_cost_pwh := if(rr_parameter_data$abatement_opportunities == "ON") (!!carbon_cost_pwh) else (!!carbon_price))
  }
  
  # User input on product differentiation
  if(rr_parameter_data$product_differentiation != "B.0") {
    rr_panel_model_run %<>%
      mutate(product_differentiation = rr_parameter_data$product_differentiation)
  }
  
  # User input on elasticity
  if(rr_parameter_data$elasticity != "B.0") {
    rr_panel_model_run %<>%
      mutate(elasticity = rr_parameter_data$elasticity)
  }
  
  for(i in model_years) {
    
    t_new <- i
    t_old <- i - 1
    discount_yrs <- i - 2018
    
    # Create variable names for use within loop
    prefix_variables_new <- c("carbon_cost_", "carbon_cost_pwh_", "a_term_", "b_term_", "number_firms_", "c_term_", "industry_cost_pass_through_", "d_relative_co2_intensity_",
                              "rho_cost_pass_through_", "e_sales_impact_", "f_margin_impact_", "profit_pre_closure_pre_tax_", "quantity_pre_closure_",
                              "sector_quantity_pre_closure_", "quantity_reallocated_", "sector_quantity_reallocated_", "quantity_post_closure_",
                              "sector_quantity_post_closure_", "market_share_post_closure_", "profit_post_closure_pre_tax_", 
                              "unit_cost_", "delta_unit_cost_", "price_", "revenue_actual_quantity_", "sector_revenue_actual_quantity_", "profit_margin_",
                              "sector_profit_post_closure_pre_tax_", "sector_profit_margin_", "product_of_share_and_co2_intensity_scope_1_", "product_of_share_and_co2_intensity_scope_2_",
                              "product_of_share_and_co2_intensity_scope_3_", "sector_average_co2_intensity_scope_1_", "sector_average_co2_intensity_scope_2_",
                              "sector_average_co2_intensity_scope_3_", "delta_sector_average_unit_cost_", "sector_average_unit_cost_", "revenue_", "sector_revenue_")
    
    prefix_variables_old <- c("quantity_post_closure_", "carbon_cost_", "carbon_cost_pwh_", "sector_profit_margin_", "profit_margin_", "profit_pre_closure_pre_tax_", "price_",
                              "sector_quantity_post_closure_", "profit_post_closure_pre_tax_", "market_share_post_closure_", "unit_cost_", "sector_average_co2_intensity_scope_1_",
                              "sector_average_co2_intensity_scope_2_", "delta_sector_average_unit_cost_", "sector_average_unit_cost_")
    
    for (var in prefix_variables_new) {
      assign(paste0(var, "t_new"), rlang::sym(paste0(var, t_new)))
    }
    
    for (var in prefix_variables_old) {
      assign(paste0(var, "t_old"), rlang::sym(paste0(var, t_old)))
    }
    
    rr_panel_model_run %<>%
      # Remove companies with profit margins over 100%
      mutate(indicator = ifelse(net_income_2017 / revenue_2017 > 1, 1, 0)) %>%
      filter(indicator != 1) %>%
      select(-indicator) %>%
      group_by(scenario, market, region) %>%
      mutate(!!delta_unit_cost_t_new := co2_intensity_scope_1 * ((!!carbon_cost_t_new) - (!!carbon_cost_t_old)) 
                                        + co2_intensity_scope_2 * ((!!carbon_cost_pwh_t_new) - (!!carbon_cost_pwh_t_old)),
             !!delta_sector_average_unit_cost_t_new := (!!sector_average_co2_intensity_scope_1_t_old) * ((!!carbon_cost_t_new) - (!!carbon_cost_t_old))
                                                        + (!!sector_average_co2_intensity_scope_2_t_old) * ((!!carbon_cost_pwh_t_new) - (!!carbon_cost_pwh_t_old)),
             !!unit_cost_t_new := (!!unit_cost_t_old) + (!!delta_unit_cost_t_new),
             !!sector_average_unit_cost_t_new := (!!sector_average_unit_cost_t_old) + (!!delta_sector_average_unit_cost_t_new),
             ## Terms A - D in the profit equation (see annotated verison of Robert Ritz industry note)
             !!a_term_t_new := 1 / (1 + rr_parameter_data$competition - product_differentiation),
             !!b_term_t_new := 1 - product_differentiation,
             !!number_firms_t_new := if(rr_parameter_data$num_firms == "ON") (1 / sum((!!market_share_post_closure_t_old)^2))
                                      else if(rr_parameter_data$num_firms == "OFF") 1
                                      else if(rr_parameter_data$num_firms == "PC") 10^9
                                      else NA,
             !!c_term_t_new := (rr_parameter_data$competition * product_differentiation * (!!number_firms_t_new)) / (1 + rr_parameter_data$competition + product_differentiation * ((!!number_firms_t_new) - 1)),
             !!industry_cost_pass_through_t_new := if(rr_parameter_data$cost_pass_through == "OFF") 0
                                                    else (!!a_term_t_new) * ((!!b_term_t_new) + (!!c_term_t_new)),
             !!d_relative_co2_intensity_t_new := if(rr_parameter_data$num_firms == "OFF") 1
                                                  # Fix to 1 when there is no cost impact (industry average unit cost change is 0)
                                                  else { ifelse(rep(mean(!!delta_sector_average_unit_cost_t_new, na.rm = TRUE) == 0, n()), 1,
                                                          (!!delta_sector_average_unit_cost_t_new) / (!!delta_unit_cost_t_new)) },
             !!rho_cost_pass_through_t_new := if(rr_parameter_data$cost_pass_through == "ON") { (!!a_term_t_new) * ((!!b_term_t_new) + (!!c_term_t_new) * (!!d_relative_co2_intensity_t_new)) }
                                               else {0},
             ## Term E - sales impact
             !!e_sales_impact_t_new := if(rr_parameter_data$sales_impact != "ON") {0}
                                        else if(rr_parameter_data$num_firms != "OFF")
                                              {(- elasticity * (!!industry_cost_pass_through_t_new) * ((!!delta_sector_average_unit_cost_t_new) / (!!sector_average_unit_cost_t_old))
                                                 * (1 - (!!sector_profit_margin_t_old)))}
                                        else {(- elasticity * (!!industry_cost_pass_through_t_new) * ((!!delta_unit_cost_t_new) / (!!unit_cost_t_old))
                                              * (1 - (!!profit_margin_t_old)))},
             ## Term F - margin impact
             !!f_margin_impact_t_new := (- (1 - (!!rho_cost_pass_through_t_new)) * ((!!delta_unit_cost_t_new) / (!!unit_cost_t_old))
                                         * (1 - (!!profit_margin_t_old)) / (!!profit_margin_t_old)),
             
             ## Profit - before firm closure, before corporation tax adjustment
             !!profit_pre_closure_pre_tax_t_new := ifelse((!!quantity_post_closure_t_old) == 0, 0,
                                                          (!!profit_post_closure_pre_tax_t_old) * gamma_factor * (1 + (!!f_margin_impact_t_new) + (!!e_sales_impact_t_new) + (!!f_margin_impact_t_new) * (!!e_sales_impact_t_new))),
             ## Quantity reallocated due to firm closure (if profit pre-closure and pre-tax is negative)
             !!quantity_pre_closure_t_new := ifelse((!!quantity_post_closure_t_old) == 0, 0,
                                                    (!!quantity_post_closure_t_old) * (1 + (!!e_sales_impact_t_new)) * gamma_factor),
             !!sector_quantity_pre_closure_t_new := sum((!!quantity_pre_closure_t_new)),
             # TRY REPLACING BELOW WITH IF PROFIT MARGIN < 0 ...
             !!quantity_reallocated_t_new := if(rr_parameter_data$firm_closure == "ON")
                                              {ifelse((!!profit_pre_closure_pre_tax_t_new) < 0, (!!quantity_pre_closure_t_new), 0)}
                                              else {0},
             !!sector_quantity_reallocated_t_new := sum((!!quantity_reallocated_t_new), na.rm = TRUE),
             # Firms receive reallocated quantity units in proportion to their initial shares of the market
             !!quantity_post_closure_t_new := if(rr_parameter_data$firm_closure != "ON")
                                              (!!quantity_pre_closure_t_new)
                                              else ifelse((!!profit_pre_closure_pre_tax_t_new) <= 0, 0,
                                                          (!!quantity_pre_closure_t_new) + rr_parameter_data$quantity_reallocation * (!!sector_quantity_reallocated_t_new) * 
                                                            ((!!quantity_pre_closure_t_new) / ((!!sector_quantity_pre_closure_t_new) - (!!sector_quantity_reallocated_t_new)))),
             !!sector_quantity_post_closure_t_new := sum((!!quantity_post_closure_t_new), na.rm = TRUE),
             !!market_share_post_closure_t_new := ifelse((!!quantity_pre_closure_t_new) == 0, 0,
                                                         (!!quantity_post_closure_t_new) / (!!sector_quantity_post_closure_t_new)),
             
             ## Profit - after firm closure, before corporation tax adjustment
             !!profit_post_closure_pre_tax_t_new := ifelse((!!quantity_pre_closure_t_new) == 0, 0,
                                                           (!!profit_pre_closure_pre_tax_t_new) * ((!!quantity_post_closure_t_new) / (!!quantity_pre_closure_t_new))),
             
             # TRY CHANGING THESE FORMULA - IT ISN'T REALLY 'ACTUAL' COST, SO VERIFY THAT IT IS APPROPRIATE FOR THIS SITUATION - ARE THE QUANTITIES RIGHT?
             !!price_t_new := if(rr_parameter_data$cost_pass_through == "ON")
                               ((!!price_t_old) + (!!delta_sector_average_unit_cost_t_new) * (1 / (1 + rr_parameter_data$competition - product_differentiation))
                                * ((1 - product_differentiation) + (rr_parameter_data$competition * product_differentiation * (!!number_firms_t_new)) /
                               (1 + rr_parameter_data$competition + product_differentiation * ((!!number_firms_t_new) - 1))))
                              else (!!price_t_old),
             !!revenue_t_new := (!!quantity_post_closure_t_new) * (!!price_t_new),
             !!sector_revenue_t_new := sum((!!revenue_t_new), na.rm = TRUE),
             !!profit_margin_t_new := (!!profit_post_closure_pre_tax_t_new) / (!!revenue_t_new),
             !!sector_profit_post_closure_pre_tax_t_new := sum((!!profit_post_closure_pre_tax_t_new), na.rm = TRUE),
             !!sector_profit_margin_t_new := (!!sector_profit_post_closure_pre_tax_t_new) / (!!sector_revenue_t_new),
             
             # Sector average co2 intensity by scope
             !!product_of_share_and_co2_intensity_scope_1_t_new := (!!market_share_post_closure_t_new) * co2_intensity_scope_1,
             !!product_of_share_and_co2_intensity_scope_2_t_new := (!!market_share_post_closure_t_new) * co2_intensity_scope_2,
             !!product_of_share_and_co2_intensity_scope_3_t_new := (!!market_share_post_closure_t_new) * co2_intensity_scope_3,
             !!sector_average_co2_intensity_scope_1_t_new := sum((!!product_of_share_and_co2_intensity_scope_1_t_new), na.rm = TRUE),
             !!sector_average_co2_intensity_scope_2_t_new := sum((!!product_of_share_and_co2_intensity_scope_2_t_new), na.rm = TRUE),
             !!sector_average_co2_intensity_scope_3_t_new := sum((!!product_of_share_and_co2_intensity_scope_3_t_new), na.rm = TRUE)
            
      ) %>%
      ungroup()
    
  }
  
  return(rr_panel_model_run)
}

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Define wrapper function to run model over power sector first and use other switches ----

model_years <- c(2018:2050)
run_model <- function(data, parameter_data, value_chain_element, variables) {
  
  ### SECTION 2a - Data cleaning which is parameter choice contingent
  print(paste0("Cleaning data - ", format(Sys.time(), "%H:%M:%S")))
  
  # Adjust market cap for asset stranding (if selected by user)
  panel_model_data <- data %>%
    mutate(profit_impact_pct = case_when(parameter_data$stranding != "ON" & substring(market, 1, 3) != "GR_" ~ NA_real_,
                                         parameter_data$green_upside != "ON" & substring(market, 1, 3) == "GR_" ~ NA_real_,
                                         TRUE ~ profit_impact_pct),
           market_cap_model = case_when(!is.na(profit_impact_pct) ~ market_cap_2017 * (1 + profit_impact_pct),
                                                                                TRUE ~ market_cap_2017)) %>%
    select(scenario:market_cap_2017, market_cap_model, everything())
  
  # Create 2017 model variables for use as the first year in the recursive model (2018 - 50)
  panel_model_data2 <- panel_model_data %>%
    # Scale down quantities in proportion to ratio of stranding market cap to pre-stranding market cap (assumed to have no effect on margin or co2 intensity of output)
    mutate(revenue_2017 = revenue_2017 * (market_cap_model / market_cap_2017),
           net_income_2017 = net_income_2017 * (market_cap_model / market_cap_2017),
           # Save unadjusted CO2 emissions (scope 1 + 2) data for later
           co2_emissions = co2_scope_1_2017 + co2_scope_2_2017,
           co2_scope_1_2017 = co2_scope_1_2017 * (market_cap_model / market_cap_2017),
           co2_scope_2_2017 = co2_scope_2_2017 * (market_cap_model / market_cap_2017),
           co2_scope_3_2017 = co2_scope_3_2017 * (market_cap_model / market_cap_2017)) %>%
    
    # Adjust up cost and profit to be pre-tax using corporation tax rate
    mutate(total_cost_2017 = ifelse(net_income_2017 >= 0 & revenue_2017 - net_income_2017 / (1 - corporation_tax_rate) >= 0,
                                    revenue_2017 - net_income_2017 / (1 - corporation_tax_rate),
                                    revenue_2017 - net_income_2017),
           profit_pre_closure_pre_tax_2017 = ifelse(revenue_2017 - net_income_2017 / (1 - corporation_tax_rate) >= 0, 1 / (1 - corporation_tax_rate), 1) * 
             net_income_margin * revenue_2017,
           profit_post_closure_pre_tax_2017 = profit_pre_closure_pre_tax_2017,
           revenue_BAU_quantity_2017 = revenue_2017) %>%
    group_by(scenario, region, market) %>%
    mutate(sector_revenue_actual_quantity_2017 = sum(revenue_BAU_quantity_2017),
           profit_margin_2017 = profit_pre_closure_pre_tax_2017 / revenue_2017, #Exception written for companies not active in particular markets (at this stage no company should have zero revenue at the global level)
           sector_profit_post_closure_pre_tax_2017 = sum(profit_pre_closure_pre_tax_2017),
           sector_profit_margin_2017 = sector_profit_post_closure_pre_tax_2017 / sector_revenue_actual_quantity_2017)
    
  ### Adjust emissions down based on Winsorisation procedure [if applicable]
  # Store initial emissions as base values
  # Variation in CO2 intensity is set to 0.49 - 0.51 percentiles for iron & steel
  # Variation in CO2 intensity is set to 0.2 - 0.8 percentiles for concrete and cement
  if(parameter_data$winsorise_scope_1 == "ON") {
    panel_model_data3 <- panel_model_data2 %>%
      mutate(co2_base_scope_1_2017 = co2_scope_1_2017,
             co2_base_scope_2_2017 = co2_scope_2_2017,
             co2_base_scope_3_2017 = co2_scope_3_2017) %>%
      # Revenue 2017 case is for companies which are wiped out in the DD / CM analysis
      mutate(co2_intensity_scope_1 = ifelse(revenue_2017 == 0, 0, co2_scope_1_2017 / revenue_2017)) %>%
      group_by(scenario, market) %>%
      mutate(low_co2_intensity_scope_1 = case_when(market == "Iron & Steel" ~ quantile(co2_intensity_scope_1, probs = 0.49, na.rm = TRUE),
                                                   market == "Concrete and cement" ~ quantile(co2_intensity_scope_1, probs = 0.2, na.rm = TRUE),
                                                   TRUE ~ quantile(co2_intensity_scope_1, probs = parameter_data$winsorise_qlow, na.rm = TRUE)),
             high_co2_intensity_scope_1 = case_when(market == "Iron & Steel" ~ quantile(co2_intensity_scope_1, probs = 0.51, na.rm = TRUE),
                                                    market == "Concrete and cement" ~ quantile(co2_intensity_scope_1, probs = 0.8, na.rm = TRUE),
                                                    TRUE ~ quantile(co2_intensity_scope_1, probs = parameter_data$winsorise_qhigh, na.rm = TRUE))) %>%
      ungroup() %>%
      mutate(co2_intensity_scope_1 = case_when(co2_intensity_scope_1 <= low_co2_intensity_scope_1 ~ low_co2_intensity_scope_1,
                                               co2_intensity_scope_1 >= high_co2_intensity_scope_1 ~ high_co2_intensity_scope_1,
                                                     TRUE ~ co2_intensity_scope_1)) %>%
      mutate(co2_scope_1_2017 = revenue_2017 * co2_intensity_scope_1) %>%
      select(-co2_intensity_scope_1, -low_co2_intensity_scope_1, -high_co2_intensity_scope_1)
  } else {panel_model_data3 <- panel_model_data2}
      
  # Change units of CO2 emissions from tonnes to Mtonnes (all monetary values are in million US$ (2016))
  # Give zero emissions companies an arbitrary amount of emissions so the model can be run for them (1 tonne)
  panel_model_data4 <- panel_model_data3 %>%  
    mutate(co2_scope_1_2017 = case_when(co2_scope_1_2017 == 0 & revenue_2017 >= 0 ~ 1 / 10^6,
                                        TRUE ~ co2_scope_1_2017 / 10^6),
           co2_scope_2_2017 = co2_scope_2_2017 / 10^6,
           co2_scope_3_2017 = co2_scope_3_2017 / 10^6)
  
  # Preserve starting emissions variables and calculate market shares
  panel_model_data5 <- panel_model_data4 %>%
    group_by(scenario, market, region) %>%
    mutate(co2_scope_1_BAU_quantity_2017 = co2_scope_1_2017,
           co2_scope_2_BAU_quantity_2017 = co2_scope_2_2017,
           co2_scope_3_BAU_quantity_2017 = co2_scope_3_2017,
           sector_revenue_BAU_quantity_2017 = sum(revenue_BAU_quantity_2017),
           total_cost_BAU_quantity_2017 = total_cost_2017,
           sector_total_cost_BAU_quantity_2017 = sum(total_cost_BAU_quantity_2017),
           total_cost_actual_quantity_2017 = total_cost_BAU_quantity_2017,
           sector_total_cost_actual_quantity_2017 = sum(total_cost_actual_quantity_2017),
           quantity_post_closure_2017 = revenue_BAU_quantity_2017,
           sector_quantity_post_closure_2017 = sum(quantity_post_closure_2017),
           market_share_post_closure_2017 = quantity_post_closure_2017 / sector_quantity_post_closure_2017,
           # WLOG p2017 = 1
           price_2017 = 1,
           quantity_2017 = revenue_2017 / price_2017,
           # Emissions intensity by scope variables
           co2_intensity_scope_1 = co2_scope_1_2017 / quantity_2017,
           co2_intensity_scope_2 = co2_scope_2_2017 / quantity_2017,
           co2_intensity_scope_3 = co2_scope_3_2017 / quantity_2017) %>%
    ungroup()
  
  # Create unit cost and sector average variables
  panel_model_data6 <- panel_model_data5 %>%
    group_by(scenario, market, region) %>%
    mutate(unit_cost_2017 = total_cost_2017 / quantity_2017,
           sector_average_unit_cost_2017 = sector_total_cost_actual_quantity_2017 / sector_quantity_post_closure_2017,
           # Sectoral average emissions intensity by scope variables
           product_of_share_and_co2_intensity_scope_1_2017 = co2_intensity_scope_1 * market_share_post_closure_2017,
           product_of_share_and_co2_intensity_scope_2_2017 = co2_intensity_scope_2 * market_share_post_closure_2017,
           product_of_share_and_co2_intensity_scope_3_2017 = co2_intensity_scope_3 * market_share_post_closure_2017,
           sector_average_co2_intensity_scope_1_2017 = sum(product_of_share_and_co2_intensity_scope_1_2017, na.rm = TRUE),
           sector_average_co2_intensity_scope_2_2017 = sum(product_of_share_and_co2_intensity_scope_2_2017, na.rm = TRUE),
           sector_average_co2_intensity_scope_3_2017 = sum(product_of_share_and_co2_intensity_scope_3_2017, na.rm = TRUE)) %>%
    ungroup()
  
  # Set carbon price and cost to be 0 in 2017 (setting up the baseline)
  panel_model_data7 <- panel_model_data6 %>%
    mutate(carbon_price_2017 = 0,
           carbon_cost_2017 = 0,
           carbon_cost_pwh_2017 = 0)
  
  # Define the final panel run dataset before running the model
  panel_run <- panel_model_data7
  
  ### SECTION 2b - Run recursive Robert Ritz model for power sector
  print(paste0("Power sector model - ", format(Sys.time(), "%H:%M:%S")))
  
  # Separate out the power sector
  pwh_panel_run <- panel_run %>%
    filter(market == "Power generation")
  
  # Run the Robert Ritz model for the power sector
  pwh_results <- run_rr_model(rr_data = pwh_panel_run, rr_parameter_data = parameter_data, rr_value_chain_element = value_chain_element)
  
  ### SECTION 3c - Calculate power sector indirect carbon costs imposed on other sectors
  
  # Find carbon costs from the power sector model run
  pwh_carbon_cost_results <- pwh_results %>%
    select(scenario, region, starts_with("carbon_cost_"), starts_with("industry_cost_pass_through_")) %>%
    unique()
  
  for (j in model_years) {
    carbon_cost_pwh = rlang::sym(paste0("carbon_cost_pwh_", j))
    carbon_cost = rlang::sym(paste0("carbon_cost_", j))
    pwh_cost_pass_through = rlang::sym(paste0("industry_cost_pass_through_", j)) 
    
    pwh_carbon_cost_results %<>%
      mutate(!!carbon_cost_pwh := !!carbon_cost * !!pwh_cost_pass_through)
  }
  
  pwh_carbon_cost_results %<>%
    select(scenario, region, contains("_pwh"))
  
  ### SECTION 3d - Run recursive Robert Ritz model for all other sectors
  print(paste0("All other sectors model - ", format(Sys.time(), "%H:%M:%S")))
  all_other_panel_model_run <- panel_run %>%
    filter(market != "Power generation")
  
  all_other_results <- run_rr_model(rr_data = all_other_panel_model_run, rr_carbon_cost_pwh_data = pwh_carbon_cost_results,
                                    rr_parameter_data = parameter_data, rr_value_chain_element = value_chain_element)
  
  ### SECTION 3e - Combine results
  run_results <- pwh_results %>% 
    bind_rows(all_other_results)
  
  ### SECTION 3f - Calculate terminal value of profits, post tax profits and net present values
  print(paste0("NPV results and aggregation - ", format(Sys.time(), "%H:%M:%S")))
   
  # Terminal values, post-tax profits and profit summary
  run_results2 <- run_results %>%
    # Post-tax profits
    mutate_at(vars(starts_with("profit_post_closure_pre_tax")), 
              funs(post_tax = ifelse(revenue_2017 - net_income_2017 / (1 - corporation_tax_rate) > 0,
                                     . * (1 - corporation_tax_rate), .))) %>%
    rename_at(vars(ends_with("post_tax")), funs(paste0("profit_post_closure_post_tax_", stri_extract_all_regex(., "[0-9]+")))) %>%
    # Post-tax NPV profits
    mutate_at(vars(starts_with("profit_post_closure_post_tax")), 
              funs(npv = . / (1 + parameter_data$discount_rate) ^ (as.numeric(stri_extract_all_regex(deparse(substitute(.)), "[0-9]+")) - 2018))) %>%
    rename_at(vars(ends_with("npv")), funs(paste0("profit_npv_post_closure_post_tax_", stri_extract_all_regex(., "[0-9]+")))) %>%
    mutate(profit_npv_post_closure_post_tax_terminal = profit_npv_post_closure_post_tax_2050 * (gamma_factor / ( 1 + parameter_data$discount_rate - gamma_factor)))
    
  subsidiary_results <- run_results2 %>%
    select(scenario, company_id, company, market, region, market_cap_2017, market_cap_model, !!(variables)) %>%
    # Calculate model company division level 'value impairment' index
    mutate(index = (rowSums(.[grep("profit_npv_post_closure_post_tax", names(.))]) - profit_npv_post_closure_post_tax_2017) / market_cap_2017 - 1,
           index_cap = case_when(grepl("GR_", market) ~ index,
                                 index >= (1 + parameter_data$profit_cap) ~ parameter_data$profit_cap * parameter_data$profit_cap_weight + index * (1 - parameter_data$profit_cap_weight),
                                 TRUE ~ index))
    
  # Summarise by user-defined categories
  summarise_results <- function(...) {
    group_vars <- enquos(...)
    temp <- subsidiary_results %>%
      group_by(!!!group_vars) %>%
      mutate(profit_capped = (index_cap + 1) * market_cap_2017) %>%
      summarise_at(vars(market_cap_2017, market_cap_model, starts_with("profit_post_closure_pre_tax"),
                        starts_with("profit_post_closure_post_tax"), starts_with("profit_npv_post_closure_post_tax"),
                        profit_capped),
                   funs(sum(., na.rm = TRUE))) %>%
      ungroup() %>%
      mutate(index = (rowSums(.[grep("profit_npv_post_closure_post_tax", names(.))]) - profit_npv_post_closure_post_tax_2017) / market_cap_2017 - 1,
             index_cap = profit_capped / market_cap_2017 - 1) %>%
      select(-profit_capped)
    
    return(temp)
  }
  
  region_market_results <- summarise_results(scenario, region, market)
  market_results <- summarise_results(scenario, market)
  region_results <- summarise_results(scenario, region)
  
  # Set attributes so that parameter values can be recalled for run results when needed
  attr(subsidiary_results, "parameters") <- parameter_data
  attr(region_market_results, "parameters") <- parameter_data
  attr(market_results, "parameters") <- parameter_data
  attr(region_results, "parameters") <- parameter_data
  
  return(list(subsidiary_results, region_market_results, market_results, region_results))
}