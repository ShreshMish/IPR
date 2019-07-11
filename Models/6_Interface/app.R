##### Project code:       Net-Zero Toolkit for modelling the financial impacts of low-carbon transition scenarios
##### Date of last edit:  08/07/2019
##### Code author:        Shyamal Patel
##### Dependencies:       1. Sectoral results: ""
#####                     2. Company results: ""
##### Notes:              None
##### Called by:          N/A

#--------------------------------------------------------------------------------------------------

##### SECTION 1 - Housekeeping and data read in ----

library(shiny)
library(tidyverse)
library(magrittr)
library(readxl)
library(stringi)
library(ggrepel)
library(digest)
library(purrr)
library(extrafont)

# Equity-level results
equity_level_results <- readRDS("Input/Equity_level_results.rds")

# Sectoral data
sector_results <- readRDS("Input/Equity_reportmarket_results.rds")

# Source house colours (not using themeVE to prevent Shiny hosting issues)
source("colours.R")

#--------------------------------------------------------------------------------------------------

##### SECTION 2 - Data cleaning and generation of datasets that are not script specific ----

scenarios <- unique(sector_results$scenario)
markets <- sort(c(unique(sector_results$parent_market), "Computers & Internet"))
companies <- c("", sort(unique(equity_level_results$company)))

#--------------------------------------------------------------------------------------------------

##### SECTION 3 - User Interface ----

ui <- fluidPage (
  
  # Change font for page header
  tags$head(
    tags$style("h2{font-family: Nordique Pro Semibold;}",
               "label{font-family: Nordique Pro Semibold;}")
  ),

  # App title ----
  titlePanel("Net Zero Toolkit results explorer"),
  
  # Sidebar layout with input / output definitions
  sidebarLayout (
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: scenario selection dropdown
      selectInput(inputId = "plot_scenario",
                  label = "Scenario",
                  choices = stri_replace_all(scenarios, replacement = " ", fixed = "_")),
      
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
                  selected = "Automobiles"),
      
      # Input: company highlighter selection dropdown
      selectInput(inputId = "plot_highlight_company",
                  label = "Highlighted company",
                  choices = companies,
                  selected = "")
      
    ),
    
    mainPanel(
      
      # Output: scatterplot chart
      plotOutput(outputId = "scatterplot", width = "100%", height = "500px"),
      
      # Add download button option
      downloadButton(outputId = "scatterplot_download", label = "Download"),
      
      # Add Vivid logo
      img(src = "VE_logo.svg", align = "right", width = "20%")
    )
  )
)

#--------------------------------------------------------------------------------------------------

##### SECTION 4 - Server ----

server <- function(input, output) {
  
  # Generate scatterplot
  output$scatterplot <- renderPlot({
    
    # Concatenate together market choices
    plot_markets <- c(input$plot_market_1, input$plot_market_2, input$plot_market_3,
                      input$plot_market_4, input$plot_market_5, input$plot_market_6)
    
    # Clean up plot scenario
    plot_scenario <- stri_replace_all(input$plot_scenario, replacement = "_", fixed = " ")
    
    # Filter data for chosen scenario
    temp_equity_level <- equity_level_results %>%
      filter(scenario == plot_scenario)
    
    temp_sector_level <- sector_results %>%
      filter(scenario == plot_scenario)
    
    # Filter data for chosen sectors (preset only right now)
    temp1 <- temp_equity_level %>%
      filter(parent_market %in% plot_markets)
    
    temp2 <- temp_equity_level %>% 
      mutate(parent_market = "MSCI ACWI")
    
    # Computers and internet market results (if chosen)
    if("Computers & Internet" %in% plot_markets) {
      temp3 <- equity_level_results %>%
        filter(scenario == plot_scenario) %>%
        filter(parent_market %in% c("Computer Services", "Internet", "Software")) %>%
        mutate(parent_market = "Computers & Internet")
      
      temp4 <- temp1 %>%
        bind_rows(temp2, temp3)
      
    } else {
      temp4 <- temp1 %>%
        bind_rows(temp2)
    }
    
    temp5 <- temp4 %>%
      mutate(parent_market = fct_rev(ordered(parent_market, levels = unique(c("MSCI ACWI", plot_markets)))))
    
    temp6 <- temp_sector_level %>%
      filter(scenario == plot_scenario) %>%
      filter(parent_market %in% c("MSCI ACWI", plot_markets)) %>%
      mutate(parent_market = fct_rev(ordered(parent_market, levels = unique(c("MSCI ACWI", plot_markets)))))
    
    # Generate the plot
    set.seed(seed = 1000)
    plot <- ggplot() +
      geom_jitter(aes(x = temp5$index, y = temp5$parent_market), height = 0.15, size = 1, alpha = 0.2,
                  shape = 16, colour = rgb(0, 191, 214, max = 255)) + 
      geom_errorbarh(aes(xmin = temp6$index_p10, xmax = temp6$index_p90, y = temp6$parent_market), colour = "black", height = 0.2) +
      geom_text_repel(aes(x = temp6$index, y = temp6$parent_market, label = scales::percent_format(1)(round(temp6$index, digits = 2))),
                      nudge_x = 0.1, nudge_y = 0.3, segment.color	= "black", family = "Calibri", size = 6) +
      geom_point(aes(x = temp6$index, y = temp6$parent_market), size = 3, shape = 23,
                 fill = rgb(0, 143, 159, max = 255), colour = "black") +
      scale_x_continuous(name = paste0("Change in valuation under ", input$plot_scenario, " scenario"), breaks = seq(-1, 1, 0.5),
                         limits = c(-1, 1), labels = scales::percent, expand = c(0, 0)) +
      scale_y_discrete(name = "Equity parent market") +
      
      # Replicate theme VE elements (not compatible with Shiny server hosting)
      theme_bw(base_size = 1.6 * 16, base_family = "Calibri") +
      theme(panel.border = element_blank(),
            aspect.ratio = 11.5 / 22, # Vivid plot size proportions
            
            # Axes
            axis.ticks = element_blank(),
            axis.line = element_line(colour = vivid_house_colours["gray"]),
            axis.title = element_text(colour = "black"),
            axis.title.y.left = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20)),
            axis.title.x.bottom = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
            axis.title.x.top = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
            axis.text = element_text(colour = "black"),
            axis.text.y.left = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.text.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
            axis.text.x.bottom = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.text.x.top = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)),
            
            # Gridlines
            panel.grid = element_blank(),
            
            # Legend
            legend.title = element_blank(),
            
            # Title
            plot.title = element_text(hjust = 0.5, margin = margin(t = 0, r = 0, b = 40, l = 0), rel(1.8)),
            panel.grid.major.x = element_line(linetype = "dashed"))
  
  # Alterations to the plot for highlighted company
    if(input$plot_highlight_company != "") {

      temp7 <- temp_equity_level %>%
        filter(company == input$plot_highlight_company) %>%
        head(n = 1)

      if(!(temp7$parent_market %in% plot_markets)) {
        temp7 %<>%
          mutate(parent_market = "MSCI ACWI")
      }

      plot <- plot +
        geom_point(aes(x = temp7$index, y = temp7$parent_market), size = 3, shape = 23,
                   fill = rgb(215, 0, 109, max = 255), colour = "black") +
        geom_text_repel(aes(x = temp7$index, y = temp7$parent_market, label = paste0(input$plot_highlight_company, " = ", scales::percent_format(1)(round(temp7$index, digits = 2)))),
                        nudge_x = -0.1, nudge_y = -0.3, segment.color = "black", family = "Calibri", size = 6)
    }
    
    return(plot)
  })
  
  # Download handler for download button
  output$scatterplot_download <- downloadHandler(filename = function() {paste("scatter-", Sys.Date(), ".png", sep = "")},
                                                 content = function(file) {ggsave(filename = file, width = 16, height = 9)},
                                                 contentType = "image/png")
  
}
    
#--------------------------------------------------------------------------------------------------

##### SECTION 5 - Shiny App ----

shinyApp(ui = ui, server = server)