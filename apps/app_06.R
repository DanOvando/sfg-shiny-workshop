# Take advantage of modules to streamline code

# Code common across app. Can also place in separate global.R -----------
# Load packages
library(shiny)
library(tidyverse)
library(shinydashboard)
# Load data. Upsides data from RAM stocks for BAU, FMSY, and Opt policies
upsides_ram_data <- read_csv("../data/upsides_ram_data.csv")

# Source shiny modules
source("modules/projection_plot_mod.R")
source("modules/upsides_box_mod.R")

# User interface. Can also place in separate ui.R -----------
ui <- function(request){
  dashboardPage(
    
    # Application title -----------
    dashboardHeader(title = "Upsides RAM Stocks Explorer"),
    
    # Initiate Sidebar layout  -----------
    dashboardSidebar(
      
      # Input for country filter  -----------
      selectInput(inputId = "country",
                  label = "Select country for figures",
                  choices = unique(upsides_ram_data$Country)),
      
      # Input for policy filter  -----------
      checkboxGroupInput(inputId = "policy",
                         label = "Select policies for figures",
                         choices = unique(upsides_ram_data$Policy),
                         selected = "Business As Usual"),
      
      # Bookmark button  -----------
      bookmarkButton()
    ),
    
    # Configure main panel  -----------
    dashboardBody(
      
      # Initialize fluid row  -----------
      fluidRow(
        # Now we can just call module UIs
        # Display biomass figure  -----------
        projection_plot_mod_UI("biomass_plot"),
        
        # Display catch figure  -----------
        projection_plot_mod_UI("catch_plot"),
        
        # Display Profits figure  -----------
        projection_plot_mod_UI("profits_plot")
        
      ),
      fluidRow(
        # Now we can just call module UIs
        # Display change in biomass infoBox  -----------
        upsides_box_mod_UI("biomass_box"),
        
        # Display change in catch infoBox  -----------
        upsides_box_mod_UI("catch_box"),
        
        # Display change in profits infoBox  -----------
        upsides_box_mod_UI("profits_box")
      )
    )
  )
}
# Server. can also place in separate server.R -----------
server <- function(input, output, session) {
  
  # Reactive filtered dataset  -----------
  filtered_data <- reactive({
    
    # Require policy and country input - don't go further until it exists
    req(input$policy, input$country)
    
    upsides_ram_data %>%
      filter(Country == input$country &
               Policy %in% input$policy)
    
  }) %>% 
    # Delay filtering of data until 1 second has passed since last input change
    # Will delay all downstream rendering as well
    debounce(1000)
  
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

shinyApp(ui, server, enableBookmarking = "url")