upsides_box_mod_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # Display change in infoBox  -----------
    infoBoxOutput(ns("box"),
                  width = 4)
    
  )
}

upsides_box_mod <- function(input, output, session, data_box, indicator_box) {
  output$box <- renderInfoBox({

    infoBox(
      paste(indicator_box," changes from start to end"),
      
      data_box() %>%
        filter(indicator == indicator_box) %>%
        mutate(output_text = paste0(Policy," Change: ",prettyNum(round(change,0),big.mark=","))) %>%
        .$output_text %>%
        paste(br()) %>%
        HTML()
      
    )
    
  })
}