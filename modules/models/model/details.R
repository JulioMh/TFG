detailsUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("models"))
}

details <-
  function(input,
           output,
           session,
           models) {
    observeEvent(models(), {
      #print(models())
    })
    
    output$models <- renderUI({
      tagList(lapply(models(), function(model) {
        tabPanel(title = model$method,
                 verbatimTextOutput(session$ns(model$method)))
      }))
    })
    
    lapply(models(), function(model) {
      output[[model()$method]] <- renderUI({
        h5("todo")
      })
    })
    
  }
