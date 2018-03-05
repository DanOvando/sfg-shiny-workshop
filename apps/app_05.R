# Take advantage of more shinydashboard features by adding infoBoxes

# Code common across app. Can also place in separate global.R -----------
# Load packages
library(shiny)
library(tidyverse)
library(shinydashboard)
# Load data. Upsides data from RAM stocks for BAU, FMSY, and Opt policies
upsides_ram_data <- read_csv("../data/upsides_ram_data.csv")

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
                         choices = unique(upsides_ram_data$Policy)),
      
      # Bookmark button  -----------
      bookmarkButton()
    ),
    
    # Configure main panel  -----------
    dashboardBody(
      
      # Initialize fluid row  -----------
      fluidRow(
        
        # Display biomass figure  -----------
        # Put inside a box  -----------
        box(title = "Biomass Projections", 
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotOutput("biomass_plot")),
        
        
        # Display catch figure  -----------
        # Put inside a box  -----------
        box(title = "Catch Projections", 
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotOutput("catch_plot")),
        
        # Display profits figure  -----------
        # Put inside a box  -----------
        box(title = "Profits Projections", 
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotOutput("profits_plot"))
        
      ),
      fluidRow(
        
        # Display change in biomass infoBox  -----------
        infoBoxOutput("biomass_box",
                      width = 4),
        
        # Display change in catch infoBox  -----------
        infoBoxOutput("catch_box",
                      width = 4),
        
        # Display change in profits infoBox  -----------
        infoBoxOutput("profits_box",
                      width = 4)
      )
    )
  )
}
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
  
  # Render infoBox for biomass changes  -----------
  output$biomass_box <- renderInfoBox({
    
    infoBox(
      "Change in biomass from start to end",
      
      filtered_upsides_data() %>%
        filter(indicator == "Biomass") %>%
        mutate(output_text = paste0(Policy," Change: ",prettyNum(round(change,0),big.mark=","))) %>%
        .$output_text %>%
        paste(br()) %>%
        HTML()
      
    )
    
  })
  
  # Render infoBox for catch changes  -----------
  output$catch_box <- renderInfoBox({
    
    infoBox(
      "Change in catch from start to end",
      
      filtered_upsides_data() %>%
        filter(indicator == "Catch") %>%
        mutate(output_text = paste0(Policy," Change: ",prettyNum(round(change,0),big.mark=","))) %>%
        .$output_text %>%
        paste(br()) %>%
        HTML()
      
    )
    
  })
  
  # Render infoBox for profits changes  -----------
  output$profits_box <- renderInfoBox({
    
    infoBox(
      "Change in profits from start to end",
      
      filtered_upsides_data() %>%
        filter(indicator == "Profits") %>%
        mutate(output_text = paste0(Policy," Change: ",prettyNum(round(change,0),big.mark=","))) %>%
        .$output_text %>%
        paste(br()) %>%
        HTML()
      
    )
    
  })
}

shinyApp(ui, server, enableBookmarking = "url")