datasetUI <- function(id){
  ns <- NS(id)
  tableUI(ns("dataset"))
}

dataset <- function(input, output, session, dataset_id){
  callModule(table, "dataset", getDataset(dataset_id))
}