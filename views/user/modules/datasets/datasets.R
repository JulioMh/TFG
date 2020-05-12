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
  callModule(newDataset, "new")
  callModule(listDataset, "list")
  output$mode <- renderUI({
    if (session$userData$user$dataset == "") {
      listDatasetUI(session$ns("list"))
    } else{
      editDatasetUI(session$ns("edit"))
    }
  })
}


