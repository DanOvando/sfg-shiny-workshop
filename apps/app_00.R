# Basic Shiny app to explore RAM stock projections in upsides database

# Code common across app. Can also place in separate global.R -----------
# Load packages
library(shiny)
library(tidyverse)
# Load data. Upsides data from RAM stocks for BAU, FMSY, and Opt policies
upsides_ram_data <- read_csv("../data/upsides_ram_data.csv")

# User interface. Can also place in separate ui.R -----------
ui <- fluidPage(
  
  # Application title -----------
  titlePanel("Upsides RAM Stocks Explorer"),
  
  # Initiate Sidebar layout  -----------
  sidebarLayout(
    
    # Configure sidebar  -----------
    sidebarPanel(
      
      # Input for country filter  -----------
      selectInput(inputId = "country",
                  label = "Select country for figures",
                  choices = unique(upsides_ram_data$Country)),
      
      # Input for policy filter  -----------
      checkboxGroupInput(inputId = "policy",
                         label = "Select policies for figures",
                         choices = unique(upsides_ram_data$Policy))
    ),
    
    # Configure main panel  -----------
    mainPanel(
      
      # Display biomass figure  -----------
      plotOutput("biomass_plot")
      
    )
  )
)

# Server. can also place in separate server.R -----------
server <- function(input, output, session) {
  
  # Reactive filtered dataset  -----------
  filtered_data <- reactive({
    
    # Require policy input - don't go further until it exists
    req(input$policy)
    
    upsides_ram_data %>%
      filter(Country == input$country &
               Policy %in% input$policy)
    
  })
  
  # Render biomass plot using filtered data set  -----------
  output$biomass_plot <- renderPlot({
    
    filtered_data() %>%
      ggplot(aes(x = Year, y = Biomass, color = Policy)) +
      geom_line()
    
  })
  
}

shinyApp(ui, server)