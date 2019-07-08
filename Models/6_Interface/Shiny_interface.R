##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  28/06/2019
##### Code author:        Shyamal Patel
##### Dependencies:       1. Sectoral results: ""
#####                     2. Company results: ""
##### Notes:              None
##### Called by:          N/A

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

# Equity-level results
equity_level_results <- readRDS("C:/Users/Shyamal/Vivid Economics Ltd/171211HSB - Low Carbon Portfolio - Documents/6 - analysis/Models/6_Interface/Input/Equity_level_results.rds")

# Sectoral data
sector_results <- readRDS("C:/Users/Shyamal/Vivid Economics Ltd/171211HSB - Low Carbon Portfolio - Documents/6 - analysis/Models/6_Interface/Input/Equity_reportmarket_results.rds")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Data cleaning and generation of datasets that are not script specific ----

scenarios <- unique(sector_results$scenario)
markets <- sort(c(unique(sector_results$parent_market), "Computers & Internet"))

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - User Interface ----

ui <- fluidPage (

  # App title ----
  titlePanel("Net Zero Toolkit results explorer"),
  
  # Sidebar layout with input / output definitions
  sidebarLayout (
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: scenario selection dropdown
      selectInput(inputId = "plot_scenario",
                  label = "Scenario",
                  choices = scenarios),
      
      # Input: market 1 selection dropdown
      selectInput(inputId = "plot_market_1",
                  label = "Market choice 1",
                  choices = markets,
                  selected = "Coal"),
      
      # Input: market 2 selection dropdown
      selectInput(inputId = "plot_market_2",
                  label = "Market choice 2",
                  choices = markets,
                  selected = "Exploration and production"),
      
      # Input: market 3 selection dropdown
      selectInput(inputId = "plot_market_3",
                  label = "Market choice 3",
                  choices = markets,
                  selected = "Concrete and cement"),
      
      # Input: market 4 selection dropdown
      selectInput(inputId = "plot_market_4",
                  label = "Market choice 4",
                  choices = markets,
                  selected = "Iron & Steel"
                  ),
      
      # Input: market 5 selection dropdown
      selectInput(inputId = "plot_market_5",
                  label = "Market choice 5",
                  choices = markets,
                  selected = "Power generation"),
      
      # Input: market 6 selection dropdown
      selectInput(inputId = "plot_market_6",
                  label = "Market choice 6", 
                  choices = markets,
                  selected = "Automobiles")
      
    ),
    
    mainPanel(
      
      # Output: scatterplot chart
      plotOutput(outputId = "scatterplot")
      
    )
  )
)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Server ----

server <- function(input, output) {
  
  # Concatenate together market choices
  plot_markets2 <- reactive(c(input$plot_market_1, input$plot_market_2, input$plot_market_3,
                             input$plot_market_4, input$plot_market_5, input$plot_market_6))
  plot_markets <- c("Coal", "Exploration and production")
  
  # Generate scatterplot
  output$scatterplot <- renderPlot({
    
    # Filter data for chosen scenario
    temp_equity_level <- equity_level_results %>%
      filter(scenario == input$plot_scenario)
    
    temp_sector_level <- sector_results %>%
      filter(scenario == input$plot_scenario)
    
    # Filter data for chosen sectors (preset only right now)
    temp1 <- temp_equity_level %>%
      filter(parent_market %in% plot_markets)
    
    temp2 <- temp_equity_level %>% 
      mutate(parent_market = "MSCI ACWI")
    
    # Computers and internet market results (if chosen)
    if("Computers & Internet" %in% plot_markets) {
      temp3 <- equity_level_results %>%
        filter(scenario == input$plot_scenario) %>%
        filter(parent_market %in% c("Computer Services", "Internet", "Software")) %>%
        mutate(parent_market = "Computers & Internet")
      
      temp4 <- temp1 %>%
        bind_rows(temp2, temp3)
      
    } else {
      temp4 <- temp1 %>%
        bind_rows(temp2)
    }
    
    temp5 <- temp4 %>%
      mutate(parent_market = fct_rev(ordered(parent_market, levels = c("MSCI ACWI", plot_markets))))
    
    temp6 <- temp_sector_level %>%
      filter(scenario == input$plot_scenario) %>%
      filter(parent_market %in% c("MSCI ACWI", plot_markets)) %>%
      mutate(parent_market = fct_rev(ordered(parent_market, levels = c("MSCI ACWI", plot_markets))))
    
    # Generate the plot
    ggplot() +
      geom_jitter(aes(x = temp5$index, y = temp5$parent_market), height = 0.15, size = 1, alpha = 0.2,
                  shape = 16, colour = rgb(0, 191, 214, max = 255)) + 
      geom_errorbarh(aes(xmin = temp6$index_p10, xmax = temp6$index_p90, y = temp6$parent_market), colour = "black", height = 0.2) +
      geom_text_repel(aes(x = temp6$index, y = temp6$parent_market, label = scales::percent_format(1)(round(temp6$index, digits = 2))),
                      nudge_x = 0.1, nudge_y = 0.4, segment.color	= "black", family = "Nordique Pro Semibold") +
      geom_point(aes(x = temp6$index, y = temp6$parent_market), size = 3, shape = 23,
                 fill = rgb(0, 143, 159, max = 255), colour = "black") +
      scale_x_continuous(name = paste0("Change in valuation under ", gsub("_", " ", input$plot_scenario), " scenario"), breaks = seq(-0.6, 0.6, 0.3),
                         limits = c(-0.6, 0.7), labels = scales::percent, expand = c(0, 0)) +
      scale_y_discrete(name = "Equity parent market") +
      theme_vivid(vivid_size = 1.4, vivid_font = "Nordique Pro Semibold") +
      theme(panel.grid.major.x = element_line(linetype = "dashed"))
    
  })
}
    
#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Shiny App ----

shinyApp(ui = ui, server = server)