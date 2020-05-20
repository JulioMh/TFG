userUI <- function(id) {
  ns <- NS(id)
  tagList(
    navbarPage(
      title = "BestAppEver",
      theme = shinytheme("cosmo"),
      tabPanel("MyModels", 
               h1("TODO")),
      tabPanel("MyDatasets", 
               datasetUI(ns("dataset"))),
      tabPanel("Community", 
               h2("TODO")),
      tabPanel("Perfil", 
               h3("TODO")),
      tabPanel("Log out", 
               h4("TODO"))
    ))
}

user <- function(input, output, session){
  callModule(dataset, "dataset")
}