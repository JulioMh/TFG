detailsUI <- function(id) {
  ns <- NS(id)
  fluidRow(column(6, plotOutput(ns("varImp"))),
           column(6, verbatimTextOutput(ns("description"))),)
}

details <-
  function(input,
           output,
           session,
           model) {
    output$varImp <-
      renderPlot({
        plot(varImp(model), main = "Importancia de las variables")
      })
    
    output$description <- renderPrint(model)
  }
