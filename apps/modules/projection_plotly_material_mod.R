# Projection plot module using plotly and shinymaterial layout
projection_plot_mod_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # Display figure  -----------
    # Put inside a box  -----------
    material_column(
      width = 4,
    uiOutput(ns("plot_box"))
    )
  )
}

projection_plot_mod <- function(input, output, session, data, indicator) {
  output$projection_plot <- renderPlotly({
    
    # Start up materialize spinner  -----------
    material_spinner_show(session, session$ns("plot_box"))
    
    # Render projection figure, now in plotly  -----------
    p <- data() %>%
      ggplot(aes_string(x = "Year", y = indicator, color = "Policy")) +
      geom_line()
    
    p_plotly <- ggplotly(p)
    
    # Stop materialize spinner  -----------
    material_spinner_hide(session, session$ns("plot_box"))
    
    p_plotly
    
  })
  
  output$plot_box <- renderUI({
    material_card(title = paste(indicator,"Projections"), 
        plotlyOutput(session$ns("projection_plot")))
  })
}