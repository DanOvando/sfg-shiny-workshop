projection_plot_mod_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # Display figure  -----------
    # Put inside a box  -----------
    uiOutput(ns("plot_box"))
  )
}

projection_plot_mod <- function(input, output, session, data, indicator) {
  output$projection_plot <- renderPlotly({
    
    # Render projection figure, now in plotly  -----------
    p <- data() %>%
      ggplot(aes_string(x = "Year", y = indicator, color = "Policy")) +
      geom_line()
    
    ggplotly(p)
    
  })
  
  output$plot_box <- renderUI({
    box(title = paste(indicator,"Projections"), 
        status = "primary",
        solidHeader = TRUE,
        width = 4,
        plotlyOutput(session$ns("projection_plot")))
  })
}