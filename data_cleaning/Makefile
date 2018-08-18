10_Panel_setting/Output/Model_panel.rds 10_Panel_setting/Output/Carbon_cost : \
	01_Financial_prelim/Output/TR_Cleaned_2016USD_data.rds \
	05_Financial_geog/Output/Company_geog_exposure_results.rds \
	08_Fossil_fuel_prod/Output/Market_exposure_results_oilandgas.rds \
	07_Market_parameters/Output/Market_parameter_data.rds \
	09_Emissions_intensity/Output/Product_level_emissions_data.rds \
	10_Panel_setting/Panel_setting.R
	Rscript 10_Panel_setting/Panel_setting.R

00_Scenarios/Output/Carbon_prices_2016USD.rds : \
    00_Scenarios/Input/TIAM\ scenario\ analysis\ v2.xlsx \
	00_Scenarios/Carbon_prices.R
	Rscript 00_Scenarios/Carbon_prices.R

01_Financial_prelim/Output/TR_cleaned_2016USD_data.rds : \
    01_Financial_prelim/Input/TR\ data\ consolidated.xlsx \
    01_Financial_prelim/Financial_preliminary.R
	Rscript 01_Financial_prelim/Financial_preliminary.R

02_CO2_emissions/Output/Trucost_cleaned_emissions_data.rds : \
	01_Financial_prelim/Output/TR_cleaned_2016USD_data.rds \
	02_CO2_emissions/CO2_emissions.R \
	02_CO2_emissions/Input/TR\ missing\ companies\ analysis.xlsx
	Rscript 02_CO2_emissions/CO2_emissions.R

03_MAC_curves/Output/MAC_curve_cleaned_2016USD_data.rds: \
	03_MAC_curves/MAC_curves.R \
	03_MAC_curves/Input/MAC\ curve\ data.xlsx
	Rscript 03_MAC_curves/MAC_curves.R

04_Fossil_fuels/Oil_and_gas/Output/DD_analysis_companies_list.rds : \
	04_Fossil_fuels/Oil_and_gas/Oil_and_gas.R 
	Rscript 04_Fossil_fuels/Oil_and_gas/Oil_and_gas.R

05_Financial_geog/Output/Company_geog_exposure_results.rds 05_Financial_geog/Output/Company_geog_exposure_results_wide.rds : \
	05_Financial_geog/Input/ISO\ code\ and\ regional\ gdp.xlsx \
	01_Financial_prelim/Output/TR_cleaned_2016USD_data.rds \
	05_Financial_geog/Financial_geog.R
	Rscript 05_Financial_geog/Financial_geog.R


06_Financial_prod/Output/Final_markets_list.rds \
06_Financial_prod/Output/Market_exposure_results_2016.rds \
06_Financial_prod/Output/Market_exposure_results.rds : \
	01_Financial_prelim/Output/TR_cleaned_2016USD_data.rds \
	04_Fossil_fuels/Oil_and_gas/Output/DD_analysis_companies_list.rds \
	02_CO2_emissions/Output/Trucost_cleaned_emissions_data.rds \
	06_Financial_prod/Input/Market\ product\ classification_EM.xlsx \
	06_Financial_prod/Input/Company\ product\ classification_AT.xlsx \
	06_Financial_prod/Financial_products.R
	Rscript 06_Financial_prod/Financial_products.R


07_Market_parameters/Output/Market_parameter_data.rds : \
	06_Financial_prod/Output/Final_markets_list.rds \
	07_Market_parameters/Input/Market\ parameterisation_final.xlsx \
	03_MAC_curves/Output/MAC_curve_cleaned_2016USD_data.rds \
	00_Scenarios/Output/Carbon_prices_2016USD.rds \
	07_Market_parameters/Market_parameterisation.R
	Rscript 07_Market_parameters/Market_parameterisation.R


08_Fossil_fuel_prod/Output/Oil_and_gas_quantity_contraction.rds : \
	06_Financial_prod/Output/Market_exposure_results.rds \
	06_Financial_prod/Output/Market_exposure_results_2016.rds \
	08_Fossil_fuel_prod/Input/DD_NPV_stranding_impacts.rds \
	08_Fossil_fuel_prod/Input/DD_stranding_data_compact.rds \
	08_Fossil_fuel_prod/Input/DD_full_raw_dataset.rds
	RScript 08_Fossil_fuel_prod/Fossil_fuel_prod_exposure.R

09_Emissions_intensity/Output/Market_emissions_intensities.rds 09_Emissions_intensity/Output/Product_level_emissions_data.rds : \
	02_CO2_emissions/Output/Trucost_cleaned_emissions_data.rds \
	09_Emissions_intensity/Emissions_intensity.R
	Rscript 09_Emissions_intensity/Emissions_intensity.R


