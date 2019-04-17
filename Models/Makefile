### 0 - Define make all component
.PHONY : all
all : 1_Demand_destruction/Output/ICE_vehicle_dd_qimpacts.rds 1_Demand_destruction/Output/Oil_and_gas_dd_downstream_qimpacts.rds \
1_Demand_destruction/Output/Oil_and_gas_dd_npv_impacts.rds 1_Demand_destruction/Output/Coal_dd_npv_impacts.rds \
2_Cleantech_markets/Output/Cleantech_npv_impacts.rds 3_Cost_and_competition/Interim/Carbon_costs.rds \
3_Cost_and_competition/Interim/Cleaned_model_panel.rds 3_Cost_and_competition/Output/Subsidiary_results.rds \
4_Asset_impacts/Interim/FI_market_detailed_results.rds

### 1 - Demand destruction model

# ICE vehicles modelling
1_Demand_destruction/Output/ICE_vehicle_dd_qimpacts.rds : \
	1_Demand_destruction/Input/ICE_scenario_data.rds \
	1_Demand_destruction/ICE_vehicles.R
	Rscript 1_Demand_destruction/ICE_vehicles.R

# Downstream and services modelling
1_Demand_destruction/Output/Oil_and_gas_dd_downstream_qimpacts.rds : \
	1_Demand_destruction/Interim/Oil_and_gas_dd_full_results.rds \
	1_Demand_destruction/Downstream_and_svcs.R
	Rscript 1_Demand_destruction/Downstream_and_svcs.R

# Upstream oil & gas modelling
1_Demand_destruction/Output/Oil_and_gas_dd_npv_impacts.rds \
1_Demand_destruction/Interim/Oil_and_gas_dd_full_results.rds : \
	1_Demand_destruction/Input/Oil_and_gas_company_data.rds \
	1_Demand_destruction/Input/Oil_and_gas_scenario_data.rds \
	1_Demand_destruction/Input/Oil_and_gas_company_names.rds \
	1_Demand_destruction/Upstream_oilandgas.R
	Rscript 1_Demand_destruction/Upstream_oilandgas.R

# Upstream coal modelling
1_Demand_destruction/Output/Coal_dd_npv_impacts.rds : \
	1_Demand_destruction/Input/Coal_company_data.rds \
	1_Demand_destruction/Input/Coal_scenario_data.rds \
	1_Demand_destruction/Input/Coal_company_names.rds \
	1_Demand_destruction/Input/Coal_cost_curve_data.rds \
	1_Demand_destruction/Upstream_coal.R
	Rscript 1_Demand_destruction/Upstream_coal.R

### 2 - Cleantech markets model
2_Cleantech_markets/Output/Cleantech_npv_impacts.rds : \
	2_Cleantech_markets/Input/Cleantech_company_data.rds \
	2_Cleantech_markets/Input/Financial_company_data.rds \
	2_Cleantech_markets/Input/Renewables_scenario_data.rds \
	2_Cleantech_markets/Input/EVs_scenario_data.rds \
	2_Cleantech_markets/Input/Biofuels_scenario_data.rds \
	2_Cleantech_markets/Cleantech_markets.R
	Rscript 2_Cleantech_markets/Cleantech_markets.R

### 3 - Cost and competition model

# Cost and competition model carbon costs
3_Cost_and_competition/Interim/Carbon_costs.rds : \
	3_Cost_and_competition/Input/Carbon_cost_curves.rds \
	3_Cost_and_competition/Input/Carbon_prices_scenario_data.rds \
	3_Cost_and_competition/Carbon_costs.R
	Rscript 3_Cost_and_competition/Carbon_costs.R

# Cost and competition model combined dataset
3_Cost_and_competition/Interim/Cleaned_model_panel.rds : \
	1_Demand_destruction/Output/Oil_and_gas_dd_npv_impacts.rds \
	1_Demand_destruction/Output/Coal_dd_npv_impacts.rds \
	1_Demand_destruction/Output/Oil_and_gas_dd_downstream_qimpacts.rds \
	1_Demand_destruction/Output/Ice_vehicle_dd_qimpacts.rds \
	2_Cleantech_markets/Output/Cleantech_npv_impacts.rds \
	3_Cost_and_competition/Input/Green_upside_reclassification.rds \
	3_Cost_and_competition/Interim/Carbon_costs.rds \
	3_Cost_and_competition/Input/Model_panel.rds \
	3_Cost_and_competition/Combine_datasets.R
	Rscript 3_Cost_and_competition/Combine_datasets.R

# Cost and competition model final results
3_Cost_and_competition/Output/Subsidiary_results.rds \
3_Cost_and_competition/Output/Region_results.rds \
3_Cost_and_competition/Output/Market_results.rds \
3_Cost_and_competition/Output/Market_region_results.rds : \
	3_Cost_and_competition/Interim/Cleaned_model_panel.rds \
	3_Cost_and_competition/Run_model.R \
	3_Cost_and_competition/Model.R
	Rscript 3_Cost_and_competition/Run_model.R
	
### 4 - Asset impacts model
4_Asset_impacts/Interim/FI_market_detailed_results.rds \
4_Asset_impacts/Interim/FI_summary_credit_rating_results.rds \
4_Asset_impacts/Interim/FI_credit_rating_results.rds \
4_Asset_impacts/Interim/Equity_level_results.rds \
4_Asset_impacts/Interim/Equity_pmarket_results.rds \
4_Asset_impacts/Interim/Equity_dom_results.rds \
4_Asset_impacts/Interim/Equity_pmarket_dom_results.rds \
4_Asset_impacts/Interim/Equity_msci_results.rds : \
	4_Asset_impacts/Input/Equity_reconciled_2016USD_data.rds \
	4_Asset_impacts/Input/FI_reconciled_2016USD_data.rds \
	4_Asset_impacts/Input/Revised_product_exposure_results.rds \
	3_Cost_and_competition/Output/Subsidiary_results.rds \
	4_Asset_impacts/Input/Credit_rating_rankings.xlsx \
	4_Asset_impacts/Asset_impacts.R
	Rscript 4_Asset_impacts/Asset_impacts.R