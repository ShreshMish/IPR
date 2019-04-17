### 0a - Define make all component
.PHONY : all
all : 4_Panel/Output/Model_panel_final.rds 1_Scenarios/Output/Model_co2_prices_2016USD.rds \
1_Scenarios/Output/Renewable_capacity.rds 1_Scenarios/Output/ICE_new_capacity.rds 1_Scenarios/Output/EV_new_capacity.rds \
1_Scenarios/Output/Fossil_fuel_production.rds 1_Scenarios/Output/Biofuels_production.rds \
3_ESG/3c_Coal/Output/Model_reg_exposure_data.rds

### 0 - Background data

# MAC curves
0_Background/Output/MAC_curve_cleaned_2016USD_data.rds : \
	0_Background/Input/MAC_curve_data.xlsx \
	0_Background/MAC_curves.R
	Rscript 0_Background/MAC_curves.R

# Market parameterisation
0_Background/Output/Market_parameter_data.rds : \
	0_Background/Input/Final_markets_list.rds \
	0_Background/Input/Market_parameterisation_final.xlsx \
	0_Background/Input/Final_carbon_prices_list.rds \
	0_Background/Output/MAC_curve_cleaned_2016USD_data.rds \
	0_Background/Market_parameterisation.R
	Rscript 0_Background/Market_parameterisation.R

### 1 - Scenarios

# Carbon prices
1_Scenarios/Output/Model_co2_prices_2016USD.rds : \
	1_Scenarios/Input/Vivid_20190208.xlsx \
	1_Scenarios/Input/Vivid_scenario_names.xlsx \
	1_Scenarios/Input/Effective_carbon_price_weights.xlsx \
	1_Scenarios/Input/CPI_inflation.xlsx \
	1_Scenarios/Carbon_prices.R
	Rscript 1_Scenarios/Carbon_prices.R

# Renewable deployment
1_Scenarios/Output/Renewable_capacity.rds : \
	1_Scenarios/Input/Vivid_20190208.xlsx \
	1_Scenarios/Input/Vivid_scenario_names.xlsx \
	1_Scenarios/Input/Public_scenarios.xlsx \
	1_Scenarios/Renewable_deployment.R
	Rscript 1_Scenarios/Renewable_deployment.R

# Vehicle deployment
1_Scenarios/Output/ICE_new_capacity.rds 1_Scenarios/Output/EV_new_capacity.rds : \
	1_Scenarios/Input/Vivid_20190208.xlsx \
	1_Scenarios/Input/Vivid_scenario_names.xlsx \
	1_Scenarios/Input/Public_scenarios.xlsx \
	1_Scenarios/Vehicle_deployment.R
	Rscript 1_Scenarios/Vehicle_deployment.R

# Fossil fuel production
1_Scenarios/Output/Fossil_fuel_production.rds : \
	1_Scenarios/Input/Vivid_20190208.xlsx \
	1_Scenarios/Input/Public_scenarios.xlsx \
	1_Scenarios/Input/Energy_conversion_factors.xlsx \
	1_Scenarios/Input/Vivid_scenario_names.xlsx \
	1_Scenarios/Fossil_fuel_production.R
	Rscript 1_Scenarios/Fossil_fuel_production.R

# Biofuel production
1_Scenarios/Output/Biofuels_production.rds : \
	1_Scenarios/Input/Vivid_20190208.xlsx \
	1_Scenarios/Input/Vivid_scenario_names.xlsx \
	1_Scenarios/Biofuel_production.R
	Rscript 1_Scenarios/Biofuel_production.R

### 2 - Financial

## 2a - Preliminary

# Import data and clean up
2_Financial/2a_Preliminary/Interim/Equity_full_2016USD_data.rds \
2_Financial/2a_Preliminary/Interim/Credit_rating_full_data.rds : \
	2_Financial/2a_Preliminary/Input/TR_data_consolidated.xlsx \
	2_Financial/2a_Preliminary/Input/OECD_CPI_inflation.xlsx \
	2_Financial/2a_Preliminary/Import_and_cleanup.R
	Rscript 2_Financial/2a_Preliminary/Import_and_cleanup.R

# Companies panel
2_Financial/2a_Preliminary/Output/Companies_list.rds \
2_Financial/2a_Preliminary/Output/Companies_2016USD_data.rds \
2_Financial/2a_Preliminary/Output/Equity_reconciled_2016USD_data.rds \
2_Financial/2a_Preliminary/Output/FI_reconciled_2016USD_data.rds : \
	2_Financial/2a_Preliminary/Interim/Equity_full_2016USD_data.rds \
	2_Financial/2a_Preliminary/Interim/Credit_rating_full_data.rds \
	2_Financial/2a_Preliminary/Input/Multi_listed_companies.xlsx \
	2_Financial/2a_Preliminary/Companies_panel.R
	Rscript 2_Financial/2a_Preliminary/Companies_panel.R

## 2b - Geographic exposure

# Geographic data cleaning
2_Financial/2b_Geographic_exposure/Interim/Unique_geogs_data.rds \
2_Financial/2b_Geographic_exposure/Interim/Geog_exposure_full_data.rds \
2_Financial/2b_Geographic_exposure/Interim/Domestic_geog_exposure_companies.rds \
2_Financial/2b_Geographic_exposure/Interim/Missing_geog_exposure_companies.rds : \
	2_Financial/2a_Preliminary/Output/Companies_2016USD_data.rds \
	2_Financial/2b_Geographic_exposure/Input/TR_domestic_geogs_renamed.csv \
	2_Financial/2b_Geographic_exposure/Geog_data_cleaning.R
	Rscript 2_Financial/2b_Geographic_exposure/Geog_data_cleaning.R

# Geographic region - ISO code matching
2_Financial/2b_Geographic_exposure/Interim/Mapped_geog_categories_wide.rds \
2_Financial/2b_Geographic_exposure/Interim/Mapped_geog_categories.rds : \
	2_Financial/2b_Geographic_exposure/Input/ISO_code_and_regional_gdp.xlsx \
	2_Financial/2b_Geographic_exposure/Interim/Unique_geogs_data.rds \
	2_Financial/2b_Geographic_exposure/Geog_region_ISO_data_matching.R
	Rscript 2_Financial/2b_Geographic_exposure/Geog_region_ISO_data_matching.R

# Geographic exposure modelling
2_Financial/2b_Geographic_exposure/Output/Geog_exposure_results_wide.rds \
2_Financial/2b_Geographic_exposure/Output/Geog_exposure_results.rds : \
	2_Financial/2b_Geographic_exposure/Input/ISO_code_and_regional_gdp.xlsx \
	2_Financial/2b_Geographic_exposure/Interim/Mapped_geog_categories.rds \
	2_Financial/2b_Geographic_exposure/Interim/Geog_exposure_full_data.rds \
	2_Financial/2b_Geographic_exposure/Interim/GDP_by_ISO_data.rds \
	2_Financial/2b_Geographic_exposure/Geog_exposure_modelling.R
	Rscript 2_Financial/2b_Geographic_exposure/Geog_exposure_modelling.R

## 2c - Product exposure
2_Financial/2c_Product_exposure/Output/Prod_exposure_results.rds \
2_Financial/2c_Product_exposure/Output/Prod_exposure_results_2016.rds \
2_Financial/2c_Product_exposure/Output/Prod_market_list.rds : \
	2_Financial/2a_Preliminary/Output/Companies_list.rds \
	2_Financial/2a_Preliminary/Output/Companies_2016USD_data.rds \
	2_Financial/2c_Product_exposure/Input/Market_product_classification_EM.xlsx \
	2_Financial/2c_Product_exposure/Input/Company_product_classification_AT.xlsx \
	2_Financial/2c_Product_exposure/Input/Rystad_oil_and_gas.xlsx \
	2_Financial/2c_Product_exposure/Prod_exposure.R
	Rscript 2_Financial/2c_Product_exposure/Prod_exposure.R

## 2d - Net income margins
2_Financial/2d_Net_income_margins/Output/Company_net_income_margins.rds : \
	2_Financial/2a_Preliminary/Output/Companies_2016USD_data.rds \
	2_Financial/2d_Net_income_margins/Net_income_margins.R
	Rscript 2_Financial/2d_Net_income_margins/Net_income_margins.R

### 3 - ESG

## 3a - CO2 emissions
3_ESG/3a_CO2_emissions/Output/Emissions_data.rds : \
	3_ESG/3a_CO2_emissions/Input/Carbon_Emissions_Screen_v1.0.xlsx \
	2_Financial/2a_Preliminary/Output/Companies_list.rds \
	3_ESG/3a_CO2_emissions/Input/TR_missing_companies_analysis.xlsx \
	3_ESG/3a_CO2_emissions/CO2_emissions.R
	Rscript 3_ESG/3a_CO2_emissions/CO2_emissions.R

## 3b - Oil and gas
3_ESG/3b_Oil_and_gas/Output/Model_prod_and_economic_data.rds \
3_ESG_3b_Oil_and_gas/Interim/Full_prod_and_economic_data.rds : \
	3_ESG/3b_Oil_and_gas/Input/Rystad_oil_and_gas.xlsx \
	2_Financial/2a_Preliminary/Output/Companies_list.rds \
	3_ESG/3b_Oil_and_gas/Oil_and_gas.R
	Rscript 3_ESG/3b_Oil_and_gas/Oil_and_gas.R

## 3c - Coal
3_ESG/3c_Coal/Output/Model_reg_exposure_data.rds \
3_ESG/3c_Coal/Output/Seaborne_supply_curve.rds \
3_ESG/3c_Coal/Output/Companies_list.rds : \
	3_ESG/3c_Coal/Input/Coal_supply_curve_and_companies.xlsx \
	2_Financial/2a_Preliminary/Output/Companies_list.rds \
	3_ESG/3c_Coal/Coal.R
	Rscript 3_ESG/3c_Coal/Coal.R

## 3d - Cleantech

# Cleantech patents
3_ESG/3d_Cleantech/Output/Cleantech_patent_VE_category_results.rds : \
	3_ESG/3d_Cleantech/Input/HSBC_GreenPatent_V1.txt \
	3_ESG/3d_Cleantech/Input/HSBC_GreenPatent_V2.txt \
	3_ESG/3d_Cleantech/Input/HSBC_GreenPatent_V3.txt \
	3_ESG/3d_Cleantech/Input/HSBC_GreenPatent_2.txt \
	3_ESG/3d_Cleantech/Input/HSBC_Green_patent_list_of_BvDids.xlsx \
	3_ESG/3d_Cleantech/Input/Orbis_missing_companies_analysis.xlsx \
	3_ESG/3d_Cleantech/Input/ipc_green_inventory.xlsx \
	3_ESG/3d_Cleantech/Input/VE_Orbis_category_matching.xlsx \
	2_Financial/2a_Preliminary/Output/Companies_list.rds \
	3_ESG/3d_Cleantech/Cleantech_patents.R
	Rscript 3_ESG/3d_Cleantech/Cleantech_patents.R

# Cleantech revenues
3_ESG/3d_Cleantech/Output/Cleantech_patent_and_rev_data.rds : \
	3_ESG/3d_Cleantech/Input/Green_Revenues_Full_Universe_Hist_+_Best_Estimate_180618.xlsx \
	3_ESG/3d_Cleantech/Input/Companies_EQ+EG.xlsx \
	3_ESG/3d_Cleantech/Input/TR_MASTER_to_Green_Revenues_SP.xlsx \
	3_ESG/3d_Cleantech/Input/VE_FTSE_category_matching.xlsx \
	2_Financial/2a_Preliminary/Output/Companies_list.rds \
	3_ESG/3d_Cleantech/Output/Cleantech_patent_VE_category_results.rds \
	3_ESG/3d_Cleantech/Cleantech_revenues.R
	Rscript 3_ESG/3d_Cleantech/Cleantech_revenues.R

### 4 - Panel

## Revised product exposure
4_Panel/Output/Rev_prod_exposure_results.rds \
4_Panel/Output/Green_upside_reclassification.rds \
4_Panel/Interim/Prod_exposure_results_oilgascoalgreen.rds \
4_Panel/Interim/Cleantech_prod_exposure_results.rds : \
	2_Financial/2c_Product_exposure/Output/Prod_exposure_results.rds \
	2_Financial/2c_Product_exposure/Output/Prod_exposure_results_2016.rds \
	3_ESG/3b_Oil_and_gas/Output/Model_prod_and_economic_data.rds \
	3_ESG/3b_Oil_and_gas/Output/Companies_list.rds \
	3_ESG/3d_Cleantech/Output/Cleantech_patent_and_rev_data.rds \
	4_Panel/Rev_prod_exposure.R
	Rscript 4_Panel/Rev_prod_exposure.R

## Emissions intensity
4_Panel/Output/Product_emissions_results.rds \
4_Panel/Interim/Market_emissions_intensities.rds : \
	3_ESG/3a_CO2_emissions/Output/Emissions_data.rds \
	4_Panel/Output/Rev_prod_exposure_results.rds \
	4_Panel/Emissions_intensity.R
	Rscript 4_Panel/Emissions_intensity.R

## Combine datasets
4_Panel/Output/Model_panel_final.rds : \
	0_Background/Output/Market_parameter_data.rds \
	2_Financial/2a_Preliminary/Output/Companies_2016USD_data.rds \
	2_Financial/2b_Geographic_exposure/Output/Geog_exposure_results.rds \
	2_Financial/2d_Net_income_margins/Output/Company_net_income_margins.rds \
	4_Panel/Output/Rev_prod_exposure_results.rds \
	4_Panel/Output/Product_emissions_results.rds \
	4_Panel/Profit_margins_combine_datasets.R
	Rscript 4_Panel/Profit_margins_combine_datasets.R