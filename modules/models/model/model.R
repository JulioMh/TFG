modelUI <- function(id) {
  ns <- NS(id)
  tagList(tabsetPanel(
    id = ns("tabs"),
    tabPanel("Resumen",
             modelSummaryUI(ns("summary"))),
    tabPanel("Conjunto de entrenamiento",
             br(),
             datasetUI(ns("train"))),
    tabPanel("Usar",
             br(),
             useModelUI(ns("use")))
  ))
}

model <- function(input, output, session, model_id) {
  ns <- session$ns
  values <- reactiveValues()
  
  #MODULES
  callModule(dataset, "train", reactive({
    values$datasets$train
  }))
  
  callModule(useModel, "use", model_id)
  tabs <- callModule(modelSummary, "summary", model_id)
  
  #LISTENERS
  observeEvent(input$tabs, {
    if (input$tabs == "Conjunto de entrenamiento") {
      if (is.null(values$datasets)) {
        values$datasets <- getModelDataset(model_id)
      }
    }
  })
  
  observeEvent(tabs$doTabs(), {
    lapply(tabs$models(), function(model) {
      tab <- tabPanel(title = model$modelInfo$label,
                      br(),
                      detailsUI(ns(model$method)))
      insertTab("tabs",
                tab,
                target = "Conjunto de entrenamiento",
                position = "before",
                select = model$method == first(tabs$models())$method)
      callModule(details, model$method, model, model_id)
    })
  })
}