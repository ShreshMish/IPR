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
	6. Asset impacts - Mapping of cashflow modelling results to financial assets (company-level)

#### Folder tree

Note that the following folder tree is for the Net-Zero Toolkit project code only - it does not provide an overview of other files in the project folder directory, which are on Sharepoint but not on Gitlab.

```
Net-Zero Toolkit model overview
    ├──	Makefile							   <- Makefile with commands (TBC)
    ├──	README.md							  <- Top-level README whic provides a summary of the tool and software requirements
    ├──	Model_data_update.Rproj				<- Model data update project file
    ├──	Model_data_update.R					<- Script for updating 'Models' folder with results from latest 'Data_cleaning' code
    ├──	Issues_log.xlsx						<- Summary of outstanding NZT model and data cleaning issues
    ├──	Model_data_update.R					<- Script for updating 'Models' folder with results from latest 'Data_cleaning' code
    ├──	Data_cleaning
    │	  └──	TBC (including unit tests for data cleaning)
    |
    ├── Models
    ├──	Models.Rproj						   <- Models project file
    |	  ├──	1 - Demand destruction		  <- Demand destruction model implementation
    |			 ├── 	Upstream_oilandgas.R	<- Oil & gas upstream (E&P) sector
    |			 ├── 	Upstream_coal.R		 <- Coal upstream sector
    |			 ├── 	Downstream_and_svcs.R   <- Oil & gas midstream and downstream sector
    |			 ├── 	ICE_vehicles.R		  <- ICE vehicle sector
    |			 └── 	Diagnostics.R		   <- Debugging
    |
    |	  ├──	2 - Cleantech markets		   <- Cleantech markets model implementation
    |			 ├── 	Cleantech_markets.R	 <- Cleantech markets
    |			 └── 	Diagnostics.R		   <- Debugging
    |
    |	  ├──	3 - Cost and competition		<- Cost & competition model implementation
    |			 ├── 	Cost_and_competition.R  <- Cost & competition
    |			 └── 	Diagnostics.R		   <- Debugging
    |
    |	  ├──	4 - Asset impacts			   <- Overlay of company-level impacts to financial assets
    |			 ├── 	TBC.R			  	 <- TBC
    |			 └── 	Diagnostics.R		   <- Debugging
    |
    |	  └──	5 - Unit tests				  <- Testing of model implementation
    |			 ├── 	TBC.R			  	 <- TBC
    |			 └── 	Diagnostics.R		   <- Debugging

All model and data cleaning folders have the following subfolder structure
    ├──	Input								  <- Raw input data (csv files in Data cleaning, rds files written by Model_data_update.R in Models)
    ├──	Interim								<- Interim data outputs (useful for debugging)
    ├──	Output								 <- Final data outputs (used in later modelling or for results processing)
```