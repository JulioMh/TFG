useModelUI <- function(id) {
  ns <- NS(id)
  tagList(sidebarPanel(selectDatasetUI(ns("dataset")),
                       uiOutput(ns("submit"))),
          mainPanel(tableUI(ns("result"))))
}

useModel <- function(input, output, session, model_id) {
  values <-
    reactiveValues()
  
  observeEvent(model_id, {
    values$cols = getModelPreds(model_id)
  })
  
  selected <- callModule(selectDataset, "dataset", reactive({
    values$availableDatasets
  }))
  
  callModule(table, "result", reactive({
    values$dataset
  }))
  
  output$submit <- renderUI({
    validate(need(selected$id(), "Selecciona un dataset"))
    
    return(div(
      style = "text-align: center;",
      actionBttn(
        inputId = session$ns("submit"),
        label = "Predecir",
        style = "minimal",
        color = "success"
      )
    ))
  })
  
  observeEvent(values$dataset, {
    if (!availableToPredict(values$cols, values$dataset)) {
      values$dataset <- selected$dataset()
      sendSweetAlert(
        session = session,
        title = "Información",
        text = "Dataset no compatible con este modelo",
        type = "info"
      )
    }
  })
  
  observeEvent(selected$dataset(), {
    values$dataset <- selected$dataset()
  })
  
  
  observeEvent(values$cols, {
    values$availableDatasets <-
      getAvailableDatasetsToPredict(values$cols,
                                    session$userData$user$id)
  })
  
  observeEvent(selected$reload(), {
    values$availableDatasets <-
      getAvailableDatasetsToPredict(values$cols,
                                    session$userData$user$id)
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
      sendSweetAlert(
        session = session,
        title = "Preparando datos...",
        closeOnClickOutside = FALSE,
        type = "info",
        btn_labels = NA,
        showCloseButton = FALSE
      )
      tryCatch({
        values$pre = getPreModels(model_id)
        
        processed_dataset <- prepareDataToPredict(
          dataset = values$dataset,
          impute_model = values$pre$impute_model,
          dummy_model = values$pre$dummy_model,
          center_model = values$pre$center_model,
          target = values$pre$target
        )
        closeSweetAlert(session)
      },
      error = function(cond) {
        sendSweetAlert(
          session = session,
          title = "Hay un problema con los datos...",
          text = cond,
          type = "error"
        )
      })
      
      if(is.null(values$models)){
        values$models <- getModels(model_id) 
      }

      for (model in values$models) {
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
