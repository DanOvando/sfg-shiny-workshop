# Turning the basic shiny app into shinydashboard

# Code common across app. Can also place in separate global.R -----------
# Load packages
library(shiny)
library(tidyverse)
library(shinydashboard)
# Load data. Upsides data from RAM stocks for BAU, FMSY, and Opt policies
upsides_ram_data <- read_csv("../data/upsides_ram_data.csv")

# User interface. Can also place in separate ui.R -----------
# To switch from regular shiny layout to shinydashboard, just change fluidPage to dashboardPage
ui <- dashboardPage(
  
  # Application title -----------
  # To switch from regular shiny layout to shinydashboard, just change titlePanel to dashboardHeader, and specify title
  dashboardHeader(title = "Upsides RAM Stocks Explorer"),
  
  # Initiate Sidebar layout  -----------
  # To switch from regular shiny layout to shinydashboard, just change sidebarLayout to dashboardSidebar
  dashboardSidebar(
    
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
  # To switch from regular shiny layout to shinydashboard, just change mainPanel to dashboardBody
  dashboardBody(
    
    # Display biomass figure  -----------
    plotOutput("biomass_plot"),
    
    # Display catch figure  -----------
    plotOutput("catch_plot"),
    
    # Display profits figure  -----------
    plotOutput("profits_plot")
    
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
  
  # Render catch plot using filtered data set  -----------
  output$catch_plot <- renderPlot({
    
    filtered_data() %>%
      ggplot(aes(x = Year, y = Catch, color = Policy)) +
      geom_line()
    
  })
  
  # Render profits plot using filtered data set  -----------
  output$profits_plot <- renderPlot({
    
    filtered_data() %>%
      ggplot(aes(x = Year, y = Profits, color = Policy)) +
      geom_line()
    
  })
}

shinyApp(ui, server)