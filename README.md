### Net-Zero Toolkit

#### Audit to be completed - unit tests to be added (testthat)

#### Description
The Net-Zero Toolkit model assesses the impact of low-carbon transition scenarios on financial assets - currently listed equities (MSCI ACWI) and corporate debt (MSCI ACWI issuers only).

The code structure is set out in the project organisation section below. The data cleaning and model folders (1 - Demand Destruction, 2 - Cleantech Markets, 3 - Cost and competition, and 4 - Asset impacts) include their own README files detailing the scripts, data requirements and outputs.

Data cleaning and modelling is performed in R. Make is used to track version control.

#### Prerequisites
| Category   | Description        | Comments                                                                                               |
|------------|--------------------|--------------------------------------------------------------------------------------------------------|
| Programs   | R (>3.5.0) <br> RStudio (>1.1.383)    |   <br> Optional																	   |
| R packages | `tidyverse` <br> `multidplyr` <br> `doParallel` <br> `stringi` <br> `stringr` <br> `ggrepel` <br> `here` <br> `themeVE` <br> <br> `dotenv` <br> `fs`| `ggplot` dev. version may be required <br> Used for parallel processing <br> Used for parallel processing <br> <br> <br> <br> <br> Vivid theme package (old colours) <br> <https://gitlab.com/vivideconomics/business-development/vivid-r-theme> <br> Used to configure file paths <br> Used to configure file paths                                                                   |

#### Code overview

1. Data cleaning
	2. Scenarios - Carbon prices, fossil fuel demand, renewable deployment, EV sales, ICE sales, and biofuel production [TIAM-Grantham - Imperial College London]
	3. Financial - Company-level revenue, profit, cost, regional and product market segmentation [Thomson Reuters]
	4. Emissions / ESG - CO<sub>2</sub> emissions by scope 1, 2 and 3 data [Trucost]
	5. Demand destruction - Oil, gas and coal company-level and scenario data [Rystad & public sources]
	6. Cleantech markets - Company-level cleantech markets revenue and patent data, and scenario data [FTSE Russell (revenue), Orbis IP (IP)]
	7. Market parameterisation - Market-level price elasticity and product differentiation parameter data [Public sources, RR / TN expert judgement]
	8. Marginal Abatement Cost (MAC) curves - Sector-region level abatement potential and cost data [McKinsey]

2. Models
	3. Demand destruction - Oil, gas, coal and ICE vehicle manufacturing industry modelling of demand-side impacts (ICE vehicles and downstream oil & gas sectors are market- rather than company-level)
	4. Cleantech markets - Renewables (solar, wind, hydro), EVs, Biofuel industry modelling of demand-side impacts (company-level)
	5. Cost and competition - Modelling of supply-side cashflow impacts of implicit carbon pricing in all markets (company-level)
	6. Asset impacts - Mapping of cashflow modelling results to financial assets (company-level), production of summary statistics and graphics

#### Folder tree

Note that the following folder tree is for the Net-Zero Toolkit project code only - it does not provide an overview of other files in the project folder directory, which may be on Sharepoint but not on Gitlab.

```
Net-Zero Toolkit model overview
    ├──	README.md                                           <- Top-level README whic provides a summary of the tool and software requirements
    ├──	Model_data_update.Rproj                             <- Model data update project file
    ├──	Model_data_update.R                                 <- Script for updating 'Models' folder with results from latest 'Data_cleaning' code
    ├──	Model_data_update.R                                 <- Script for updating 'Models' folder with results from latest 'Data_cleaning' code
    ├── utils.R                                             <- Utilities called by Model_data_update.R scripts (equivalent to other utils.R files)
    ├──	Data_cleaning
    |   Makefile                                            <- Data cleaning Makefile
    |   utils.R                                             <- Utilities called by Data_cleaning scripts (equivalent to other utils.R files)
    |   ├── 0_Background                                    <- MAC curves and market parameter (elasticities, prod differentiation etc.) data clean up
    |       ├── MAC_curves.R                                <- MAC curve data cleaning
    |       ├── MAC_curve_charts.R                          <- MAC curve chart generation
    |       └── Market_parameterisation.R                   <- Market parameters data cleaning
    |
    |   ├── 1_Scenarios                                     <- Scenario data clean up (only place where scenarios enter in the Data_cleaning folder)
    |       ├── Biofuel_production.R                        <- Biofuel global production data (for Cleantech markets)
    |       ├── Carbon_prices.R                             <- Carbon prices data (for Cost & competition)
    |       ├── Fossil_fuel_production.R                    <- Fossil fuel global production data (for Demand destruction)
    |       ├── Renewable_deployment.R                      <- Renewable deployment data (for Cleantech markets)
    |       ├── Vehicle_deployment.R                        <- Vehicle deployment data (for Cleantech markets and Demand destruction)
    |
    |   ├── 2_Financial                                     <- Financial data clean up
    |       ├── 2a_Preliminary                              <- Preliminary data cleaning before more detailed product / region / profit margin analysis
    |           ├── Import_and_cleanup.R                    <- Read in spreadsheets and change variable names
    |           └── Companies_panel.R                       <- Set up unique companies panel, and equity / corporate bond datasets for later Asset impacts analysis
    |       ├── 2b_Geographic_exposure                      <- Geographic exposure analysis
    |           ├── Geog_data_cleaning.R                    <- Clean up 'as reported' geographic segment data
    |           ├── Geog_region_ISO_data_matching.R         <- Find list of unique regions and allocate ISO codes / regions
    |           └── Geog_exposure_modelling.R               <- Calculate consolidated geographic segments based on NZT regions
    |       ├── 2c_Product_exposure                         <- Clean up 'as reported' product segment data
    |           └── Prod_exposure.R                         <- Calculate consolidated product segments based on NZT markets (note spreadsheet analysis by Ethan and Aaron)
    |       └── 2d_Net_income_margins                       <- Clean up net income profit margins data
    |           └── Net_income_margins.R
    |
    |   ├── 3_ESG                                           <- ESG data clean up
    |       ├── 3a_CO2_emissions                            <- CO2 emissions data clean up
    |           └── CO2_emissions.R
    |       ├── 3b_Oil_and_gas                              <- Oil and gas data clean up
    |           └── Oil_and_gas.R
    |       ├── 3c_Coal                                     <- Coal data clean up
    |           └── Coal.R
    |       └── 3d_Cleantech                                <- Cleantech markets data clean up (IP and revenue)
    |           ├── Cleantech_patents.R                     <- Cleantech patents data clean up (Orbis IP)
    |           └── Cleantech_revenues.R                    <- Cleantech revenues data clean up (FTSE GR)
    |
    |   └── 4_Panel                                         <- Build model datasets using 0 - 3 outputs
    |       ├── Rev_product_exposure.R                      <- Revise product exposure data based on 3_ESG outputs (fossil fuel and cleantech revenue companies only)
    |       ├── Emissions_intensity.R                       <- Estimate emissions for subsidiaries based on model market emissions intensity, and for companies without emissions data
    |       └── Profit_margins_combine_datasets.R           <- Combine datasets and calculate revenue growth factors based on industry profit margins
    |
    ├── Models
    ├──	Models.Rproj                                        <- Models project file
    ├── Makefile                                            <- Models Makefile
    ├── utils.R                                             <- Utilities called by Models scripts (equivalent to other utils.R files)
    |   ├── 1_Demand_destruction                            <- Demand destruction model implementation
    |       ├── Upstream_oilandgas.R                        <- Oil & gas upstream (E&P) sector
    |       ├── Upstream_coal.R                             <- Coal upstream sector
    |       ├── Downstream_and_svcs.R                       <- Oil & gas midstream and downstream sector
    |       ├── ICE_vehicles.R                              <- ICE vehicle sector
    |
    |   ├── 2_Cleantech_markets                             <- Cleantech markets model implementation
    |       ├── Cleantech_markets.R                         <- Cleantech markets
    |
    |   ├── 3_Cost_and_competition                          <- Cost & competition model implementation
    |       ├── Carbon_costs.R                              <- Carbon costs using MACCs and carbon prices
    |       ├── Combine_datasets.R                          <- Combines Panel with DD and CM model results
    |       ├── Model.R                                     <- Cost & competition model functions
    |       └── Run_model.R                                 <- Runs model based on parameter choices
    |
    |   ├── 4_Asset_impacts                                 <- Overlay of company-level impacts to financial assets
    |       ├── Asset_impacts.R                             <- Equity and FI asset-level impact analysis
    |       └── <Other scripts>.R                           <- Graphics and summary statistics for publication
    |
    |   └── 5_Unit_tests                                    <- Testing of model implementation
    |       └── TBC.R                                       <- TBC
    
All model and data cleaning folders have the following subfolder structure
    ├── Input                                               <- Raw input data (csv files in Data cleaning, rds files written by Model_data_update.R in Models)
    ├── Interim                                             <- Interim data outputs (useful for debugging)
    ├── Output                                              <- Final data outputs (used in later modelling or for results processing)
```