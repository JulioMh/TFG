modelUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionBttn(
      inputId = ns("cancel"),
      icon = icon("arrow-circle-left"),
      style = "pill",
      color = "warning",
      size = "xs"
    ),
    hr(),
    tabsetPanel(
      id = ns("model_tabs"),
      tabPanel("Resumen",
               br(),
               modelSummaryUI(ns("summary"))),
      tabPanel("Dataset de prueba",
               br(),
               compareDatasetsUI(ns("compare"))),
      tabPanel("Usar",
               br(),
               useModelUI(ns("use")))
    )
  )
}

model <- function(input, output, session, model_id) {
  values <- reactiveValues(data = NULL, id = NULL)
  
  callModule(compareDatasets, "compare", reactive({
    values$data$datasets
  }))
  show_tabs <- callModule(modelSummary, "summary", reactive({
    values$data
  }), reactive({
    values$id
  }),
  reactive(input$cancel))
  callModule(useModel, "use", reactive({
    values$data
  }))
  
  observeEvent(show_tabs(), {
    lapply(values$data$models, function(model){
      tab <- tabPanel(title = model$modelInfo$label,
                      br(),
                      detailsUI(session$ns(model$method)))
      
      appendTab("model_tabs",
                tab,
                select = ifelse(
                  model$method == first(values$data$models)$method,
                  TRUE,
                  FALSE
                ))
      callModule(details, model$method, model)
    })
  })
  
  observeEvent(model_id(), {
    if (is.null(model_id())) {
      all_models <- loadModels(session$userData$user$id)
      id <- all_models[1, ]$model_id()
    } else{
      id <- model_id()
    }
    values$data <- loadModel(id)
    values$id <- id
    
    
  })
  
  return(reactive({
    input$cancel
  }))
}