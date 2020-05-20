datasetUI <- function (id) {
  ns <- NS(id)
  tagList(tabsetPanel(
    tabPanel("My datasets",
             uiOutput(ns("mode"))),
    tabPanel("New dataset",
             insertUI(ns("new")))
  ))
}

dataset <- function (input, output, session) {
  v <- reactiveValues(doEdit = FALSE)
  data <- reactiveValues(name = "", description = "")
  callModule(insert, "new", formDatasetUI, formDataset, "Dataset", data)
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


