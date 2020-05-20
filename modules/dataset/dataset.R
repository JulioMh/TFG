datasetUI <- function (id) {
  ns <- NS(id)
  tagList(tabsetPanel(
    tabPanel("My datasets",
             uiOutput(ns("mode"))),
    tabPanel(
      "New dataset",
      sidebarPanel(formDatasetUI(ns("new"))),
      mainPanel(tableUI(ns("dataset")))
    )
  ))
}

dataset <- function (input, output, session) {
  edit <- reactiveValues(name = "", description = "", doEdit = FALSE)
  
  form_result <- callModule(formDataset, "new", edit)
  callModule(table, "dataset", form_result)
}
