projection_plot_mod_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # Display figure  -----------
    # Put inside a box  -----------
      uiOutput(ns("plot_box"))
    
  )
}

projection_plot_mod <- function(input, output, session, data, indicator) {
  output$projection_plot <- renderPlot({
    
    # Render projection figure  -----------
    data() %>%
      ggplot(aes_string(x = "Year", y = indicator, color = "Policy")) +
      geom_line()
    
  })
  
  output$plot_box <- renderUI({
    box(title = paste(indicator,"Projections"), 
        status = "primary",
        solidHeader = TRUE,
        width = 4,
        plotOutput(session$ns("projection_plot")))
  })
}