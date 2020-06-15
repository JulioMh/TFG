datasetUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("mode"))

}

dataset <- function(input, output, session, dataset){
  ns <- session$ns
  output$mode <- renderUI({
    if(is.null(dataset())){
      fluidRow(
        align = "center",
        helpText("Selecciona un dataset para previsualizarlo.")
      )
    }else{
      tableUI(ns("dataset"))
    }
  })
  
  observeEvent(dataset(),{
    req(dataset())
    callModule(table, "dataset", dataset)
  })
}