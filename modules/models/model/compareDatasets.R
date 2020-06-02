compareDatasetsUI <- function(id) {
  ns <- NS(id)
  tagList(
    radioGroupButtons(
      inputId = ns("select_type"),
      choices = c("Sin procesar", 
                  "Procesado"),
      justified = TRUE
    ),
    hr(),
    uiOutput(ns("type"))
  )
}

compareDatasets <- function(input, output, session, datasets) {
  callModule(table, "raw", reactive({datasets()$test}))
  callModule(table, "process", reactive({datasets()$processed}))
  
  output$type <- renderUI({
    if(input$select_type == "Sin procesar"){
      tableUI(session$ns("raw"))
    }else{
      tableUI(session$ns("process"))
    }
  })
}
