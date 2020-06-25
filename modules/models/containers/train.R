trainUI <- function (id) {
  ns <- NS(id)
  setUpTrainUI(ns("set_up"))
}

trainServer <- function (input, output, session) {
  values <- reactiveValues()
  
  settings <- callModule(setUpTrain, "set_up")
  
  state <-
    reactiveValues(
      processing_data = FALSE,
      training_model = FALSE,
      saving_model = FALSE,
      done = FALSE
    )

  observeEvent(state$processing_data, {
    if (state$processing_data) {
      sendSweetAlert(
        session = session,
        title = "Preparando datos...",
        closeOnClickOutside = FALSE,
        type = "info",
        btn_labels = NA,
        showCloseButton = FALSE
      )
      tryCatch({
        state$processedData <-
          prepareDataForTraining(
            dataset = settings$dataset(),
            preds = settings$preds(),
            preds_to_center = settings$to_center(),
            do_pred_na = settings$pred_missing_fields(),
            target = settings$target()
          )
        state$training_model <- TRUE
      },
      error = function(cond) {
        sendSweetAlert(
          session = session,
          title = "Hay un problema con los datos...",
          text = cond,
          type = "error"
        )
      },
      finally = {
        state$processing_data  <- FALSE
      })
    }
  })
  
  observeEvent(state$training_model, {
    if (state$training_model) {
      tryCatch({
        sendSweetAlert(
          session = session,
          title = "Entrenando...",
          text = "Espere un momento, estamos trabajando en ello",
          closeOnClickOutside = FALSE,
          type = "info",
          btn_labels = NA,
          showCloseButton = FALSE
        )
        
        models <-
          doTrain(settings$methods(),
                  state$processedData$trainData,
                  settings$target())
        
        models$impute_model <- state$processedData$impute_model
        models$dummy_model <- state$processedData$dummy_model
        models$center_model <- state$processedData$center_model
        
        state$models <- models
        
        state$saving_model <- TRUE
      },
      error = function(cond) {
        sendSweetAlert(
          session = session,
          title = "No se han podido entrenar los modelos...",
          text = "Prueba utilizando otro algoritmo de entrenamiento",
          type = "error"
        )
      },
      finally = state$training_model  <- FALSE)
    }
  })
  
  observeEvent(state$saving_model, {
    if (state$saving_model) {
      tryCatch({
        sendSweetAlert(
          session = session,
          title = "Almacenando en la base de datos...",
          closeOnClickOutside = FALSE,
          type = "info",
          btn_labels = NA,
          showCloseButton = FALSE
        )
        
        trainRowNumbers <-
          paste(as.vector(state$processedData$index), collapse = ", ")
        
        values$id <- saveModel(
          state$models,
          settings$target(),
          settings$name(),
          settings$description(),
          session$userData$user$id,
          settings$dataset_id(),
          trainRowNumbers,
          settings$preds(),
          settings$isPublic()
        )
        sendSweetAlert(
          session = session,
          title = "Listo!!",
          text = "Modelo entrenado y guardado",
          type = "success"
        )
        state$done <- TRUE
      },
      error = function(cond) {
        sendSweetAlert(
          session = session,
          title = "No se han podido guardar los mdelos...",
          text = cond,
          type = "error"
        )
      },
      finally = {
        state$saving_model  <- FALSE
        state$models <- NULL
        state$datsets <- NULL
      })
    }
  })
  
  observeEvent(settings$confirm(), {
    if (isTRUE(settings$confirm())) {
      state$processing_data <- TRUE
    }
  })
  
  return(reactive(values$id))
}
