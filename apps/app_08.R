# Use materialize instead of shinydashboards

# Code common across app. Can also place in separate global.R -----------
# Load packages
library(shiny)
library(tidyverse)
library(shinymaterial)
library(plotly)
# Load data. Anonymized upsides data for 10 countries for BAU, FMSY, and Opt policies
upsides_data <- read_csv("../data/upsides_data.csv")

# Source shiny modules
source("modules/projection_plotly_material_mod.R")
source("modules/upsides_box_material_mod.R")

# User interface. Can also place in separate ui.R -----------
ui <- material_page(
    
    # Application title -----------
    title = "Fishery Projections Explorer",
    
    # Initiate material Sidebar layout  -----------
    material_side_nav(
      
      # Input for country filter  -----------
      material_dropdown(input_id = "country",
                  label = "Select country for figures",
                  choices = unique(upsides_data$Country) %>% sort()),
      
      # Input for policy filter  -----------
      material_dropdown(input_id = "policy",
                         label = "Select policies for figures",
                         choices = unique(upsides_data$Policy),
                         selected = "Business As Usual")
    ),
    
    # Configure main panel  -----------
    #dashboardBody(
      
      # Initialize fluid row  -----------
      material_row(
        
        # Display biomass figure  -----------
        projection_plot_mod_UI("biomass_plot"),
        
        # Display catch figure  -----------
        projection_plot_mod_UI("catch_plot"),
        
        # Display Profits figure  -----------
        projection_plot_mod_UI("profits_plot")
        
      ),
    material_row(
        
        # Display change in biomass infoBox  -----------
        upsides_box_mod_UI("biomass_box"),
        
        # Display change in catch infoBox  -----------
        upsides_box_mod_UI("catch_box"),
        
        # Display change in profits infoBox  -----------
        upsides_box_mod_UI("profits_box")
      )
    )

# Server. can also place in separate server.R -----------
server <- function(input, output, session) {
  
  # Reactive filtered dataset  -----------
  filtered_data <- reactive({
    
    # Require policy and country input - don't go further until it exists
    req(input$policy, input$country)
    
    upsides_data %>%
      filter(Country == input$country &
               Policy %in% input$policy)
    
  })
  
  # Reactive upsides dataset  -----------
  filtered_upsides_data <- reactive({
    
    # Determine upside from first to last year, for each indicator
    filtered_data() %>% 
      filter(Year %in% c(min(filtered_data()$Year),
                         max(filtered_data()$Year))) %>%
      mutate(Year = ifelse(Year == min(filtered_data()$Year), 
                           "start", 
                           "end")) %>%
      gather(indicator, value, Biomass:Profits) %>%
      spread(Year,value) %>%
      mutate(change = end - start)
    
  })
  
  # Call all modules  -----------
  
  callModule(projection_plot_mod, "biomass_plot", data = filtered_data, indicator = "Biomass")
  
  callModule(projection_plot_mod, "catch_plot", data = filtered_data, indicator = "Catch")
  
  callModule(projection_plot_mod, "profits_plot", data = filtered_data, indicator = "Profits")
  
  callModule(upsides_box_mod, "biomass_box", data_box = filtered_upsides_data, indicator_box = "Biomass")
  
  callModule(upsides_box_mod, "catch_box", data_box = filtered_upsides_data, indicator_box = "Catch")
  
  callModule(upsides_box_mod, "profits_box", data_box = filtered_upsides_data, indicator_box = "Profits")
  
}

shinyApp(ui, server)