userUI <- function(id) {
  ns <- NS(id)
  tagList(
    navbarPage(
      title = "BestAppEver",
      theme = shinytheme("flatly"),
      tabPanel("MyModels", 
               modelsUI(ns("models"))),
      tabPanel("MyDatasets", 
               datasetUI(ns("datasets"))),
      tabPanel("Community", 
               h2("TODO")),
      tabPanel("Log out", 
               h4("TODO"))
    ))
}

user <- function(input, output, session){
  callModule(dataset, "datasets")
  callModule(models, "models")
}