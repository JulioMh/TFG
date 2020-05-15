datasetsUI <- function (id) {
  ns <- NS(id)
  tagList(tabsetPanel(
    tabPanel("My datasets",
             uiOutput(ns("mode"))),
    tabPanel("New dataset",
             newDatasetUI(ns("new")))
  ))
}

datasets <- function (input, output, session) {
  v <- reactiveValues(doEdit = FALSE)
  callModule(newDataset, "new")
  callModule(listDataset, "list", v)
  callModule(editDataset, "edit", v)
  
  output$mode <- renderUI({
    if (v$doEdit == FALSE) {
      listDatasetUI(session$ns("list"))
    } else{
      editDatasetUI(session$ns("edit"))
    }
  })
}


