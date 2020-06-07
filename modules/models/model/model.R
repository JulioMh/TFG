modelUI <- function(id) {
  ns <- NS(id)
  tagList(tabsetPanel(
    tabPanel("Resumen",
             br(),
             modelSummaryUI(ns("summary"))),
    tabPanel("Dataset de prueba",
             br(),
             compareDatasetsUI(ns("compare"))),
    tabPanel("Usar",
             br(),
             useModelUI(ns("use")))
  ))
}

model <- function(input, output, session, model_id) {
  values <- reactiveValues(data = NULL, id = NULL)
  callModule(compareDatasets, "compare", reactive({
    values$data$datasets
  }))
  callModule(modelSummary, "summary", reactive({
    values$data
  }), reactive({
    values$id
  }))
  callModule(useModel, "use", reactive({
    values$data
  }))
  observeEvent(model_id(), {
    if (is.null(model_id())) {
      all_models <- loadModels(session$userData$user$id)
      id <- all_models[1,]$model_id()
    } else{
      id <- model_id()
    }
    values$data <- loadModel(id)
    values$id <- id
  })
}
