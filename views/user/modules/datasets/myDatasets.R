myDatasetsUI <- function (id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("My datasets",
                tableOutput(ns("datasets"))),
      tabPanel("New dataset",
               newDatasetUI("fromDataset"))
    )
  )
}