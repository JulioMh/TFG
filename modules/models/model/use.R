useModelUI <- function(id) {
  ns <- NS(id)
  tagList(sidebarPanel(
    selectDatasetUI(ns("dataset")),
    uiOutput(ns("submit"))
  ),
  mainPanel(tableUI(ns("result"))))
}

useModel <- function(input, output, session, data) {
  values <-
    reactiveValues()
  callModule(table, "result", reactive({
    values$dataset
  }))
  
  selected <- callModule(selectDataset, "dataset", reactive({
    values$availableDatasets
  }))
  
  output$submit <- renderUI({
    validate(
      need(selected$id(), "Selecciona un dataset")
    )
    
    return(div(
      style = "text-align: center;",
      actionBttn(
        inputId = session$ns("submit"),
        label = "Entrenar",
        style = "stretch",
        color = "warning"
      )
    ))
  })
  
  observeEvent(selected$dataset, {
    if(!availableToPredict(data()$cols, selected$dataset)){
      sendSweetAlert(
        session = session,
        title = "Información",
        text = "Este modelo no puede hacer predicciones sobre este dataset",
        type = "info"
      )
    }
  })
  
  observeEvent(selected$reload(),{
    if(selected$reload()){
      values$availableDatasets <-
        getAvailableDatasetsToPredict(data()$cols,
                                      session$userData$user$id)  
    }
  })
  
  observeEvent(data(), {
    req(data())
    values$availableDatasets <-
      getAvailableDatasetsToPredict(data()$cols,
                                    session$userData$user$id)
  })
  
  observeEvent(selected$dataset(), {
    req(selected$dataset())
    values$dataset <- selected$dataset()
  })
  
  observeEvent(input$submit, {
    confirmSweetAlert(
      session = session,
      inputId = "confirm",
      type = "warning",
      title = "¿Estas seguro?"
    )
  })
  
  observeEvent(input$confirm, {
    if (isTRUE(input$confirm)) {
      processed_dataset <- prepareDataToPredict(
        dataset = values$dataset,
        impute_model = data()$preProcess$impute_model,
        dummy_model = data()$preProcess$dummy_model,
        center_model = data()$preProcess$center_model,
        target = data()$preProcess$target
      )
      for (model in isolate(data()$model)) {
        tryCatch({
          pred <- predict(model, processed_dataset)
          values$dataset[[model$method]] <- pred
        }, error = function(cond) {
          sendSweetAlert(
            session = session,
            title = "Se ha encontrado un problema...",
            text = cond,
            type = "error"
          )
        })
        
      }
    }
  })
  
}
