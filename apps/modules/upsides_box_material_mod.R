# Upsides box module using shinymaterial layout

upsides_box_mod_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # Display change in infoBox  -----------
    material_column(
      width = 4,
    uiOutput(ns("upsides_box"))
    )
  )
}

upsides_box_mod <- function(input, output, session, data_box, indicator_box) {
  
  output$upsides_text <- renderUI({ 
    data_box() %>%
      filter(indicator == indicator_box) %>%
      mutate(output_text = paste0(Policy," Change: ",prettyNum(round(change,0),big.mark=","))) %>%
      .$output_text %>%
      paste(br()) %>%
      HTML()
  })
  
  output$upsides_box <- renderUI({
    material_card(title = paste(indicator_box," changes from start to end"),
                  htmlOutput(session$ns("upsides_text")))
  })

}