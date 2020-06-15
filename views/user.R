userUI <- function(id) {
  ns <- NS(id)
  tagList(
    use_waiter(include_js = FALSE),
    useShinyjs(),
    useSweetAlert(),
    
    navbarPage(
      title = "BestAppEver",
      theme = shinytheme("flatly"),
      tabPanel("MyModels",
               modelsUI(ns("models"))),
      tabPanel("MyDatasets",
               datasetsUI(ns("datasets"))),
      tabPanel("Community",
               modelsUI(ns("community"))),
      tabPanel("Log out",
               h4("TODO"))
    )
  )
}

user <- function(input, output, session) {
  callModule(datasets, "datasets")
  callModule(models, "models", loadOwnModels)
  callModule(models, "community", loadNotOwnModels)
}