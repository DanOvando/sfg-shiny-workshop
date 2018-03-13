# Basic Shiny app to explore fishery projections

# # Handy snippet for quick-starting a shiny app:
# library(shiny)
# 
# ui <- fluidPage(
#   
# )
# 
# server <- function(input, output, session) {
#   
# }
# 
# shinyApp(ui, server)

# Code common across app. Can also place in separate global.R -----------
# Load packages
library(shiny)
library(tidyverse)
# Load data. Anonymized upsides data for 10 countries for BAU, FMSY, and Opt policies
upsides_data <- read_csv("../data/upsides_data.csv")

# User interface. Can also place in separate ui.R -----------
ui <- fluidPage(
  
  # Application title -----------
  titlePanel("Fishery Projections Explorer"),
  
  # Initiate Sidebar layout  -----------
  sidebarLayout(
    
    # Configure sidebar  -----------
    sidebarPanel(
      
      # Input for country filter  -----------
      selectInput(inputId = "country",
                  label = "Select country for figures",
                  choices = unique(upsides_data$Country) %>% sort()),
      
      # Input for policy filter  -----------
      checkboxGroupInput(inputId = "policy",
                         label = "Select policies for figures",
                         choices = unique(upsides_data$Policy),
                         selected = "Business As Usual")
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
    
    # Require policy and country input - don't go further until it exists
    req(input$policy, input$country)
    
    upsides_data %>%
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