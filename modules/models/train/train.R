trainUI <- function (id) {
  ns <- NS(id)
  tagList(
    useSweetAlert(),
    sidebarPanel(
      selectDatasetUI(ns("dataset")),
      hr(),
      uiOutput(ns("picker_target")),
      actionBttn(
        inputId = ns("open_advance"),
        label = "Modo avanzado",
        icon = icon("caret-down"),
        style = "simple",
        size = "xs",
        color = "primary"
      ),
      uiOutput(ns("advance_ui")),
      hr(),
      basicFormUI(ns("basic")),
      uiOutput(ns("submit"))
    ),
    mainPanel(tableUI(ns("models")))
  )
}

trainServer <- function (input, output, session) {
  preds_names <-
    reactive({
      getPredsNames(selected_dataset$dataset(), input$target)
    })
  status <- reactiveValues(done = FALSE)
  fields <- callModule(basicForm, "basic", NULL, reactive({NULL}))
  selected_dataset <- callModule(selectDataset, "dataset", reactive({loadDatasets(session$userData$user$id)}))
  settings <- callModule(advanceMode, "advance", preds_names)
  callModule(table, "models", reactive({
    selected_dataset$dataset()
  }))
  train_status <-
    reactiveValues(
      process_data = FALSE,
      train_model = FALSE,
      save_model = FALSE,
      done = FALSE
    )
  advance_values <-
    reactiveValues(
      methods = c("rf", "nnet"),
      preds = preds_names,
      to_center = c(),
      pred_missing_fields = FALSE
    )
  
  output$advance_ui <- renderUI({
    if ((input$open_advance %% 2) != 0) {
      advanceModeUI(session$ns("advance"))
    }
  })
  
  output$picker_target <- renderUI({
    pickerInput(
      inputId = session$ns("target"),
      label = tagList(icon("bullseye"), "Selecciona la columna objetivo:"),
      choices = colnames(selected_dataset$dataset()),
      options = list(`live-search` = TRUE)
    )
  })
  
  output$submit <- renderUI({
    validate(
      need(selected_dataset$id(), "Selecciona un dataset"),
      need(
        length(advance_values$preds()) > 0,
        "Necesitas al menos un predictor"
      ),
      need(fields$name() != "", "Debes introducir un nombre")
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

  observeEvent(input$submit, {
    confirmSweetAlert(
      session = session,
      inputId = "confirm",
      type = "warning",
      title = "¿Estas seguro?",
      text = "Este proceso puede tardar varios minutos"
    )
  })
  
  observeEvent(settings$preds(), {
    advance_values$preds <- settings$preds
  })
  
  observeEvent(settings$methods(), {
    advance_values$methods <- settings$methods()
  })
  
  observeEvent(settings$to_center(), {
    advance_values$to_center <- settings$to_center()
  })
  observeEvent(settings$pred_missing_fields(), {
    advance_values$pred_missing_fields <- settings$pred_missing_fields()
  })
  
  observeEvent(train_status$process_data, {
    if (train_status$process_data) {
      sendSweetAlert(
        session = session,
        title = "Preparando datos...",
        closeOnClickOutside = FALSE,
        type = "info",
        btn_labels = NA,
        showCloseButton = FALSE
      )
      tryCatch({
        train_status$datasets <-
          prepareDataForTraining(
            dataset = selected_dataset$dataset(),
            preds = advance_values$preds(),
            preds_to_center = advance_values$to_center,
            do_pred_na = advance_values$pred_missing_fields,
            target = input$target
          )
        train_status$train_model <- TRUE
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
        train_status$process_data  <- FALSE
      })
    }
  })
  
  observeEvent(train_status$train_model, {
    if (train_status$train_model) {
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
          doTrain(advance_values$methods,
                  train_status$datasets$trainData,
                  input$target)
        
        models$impute_model <- train_status$datasets$impute_model
        models$dummy_model <- train_status$datasets$dummy_model
        models$center_model <- train_status$datasets$center_model
        
        train_status$models <- models
        
        train_status$save_model <- TRUE
      },
      error = function(cond) {
        sendSweetAlert(
          session = session,
          title = "No se han podido entrenar los modelos...",
          text = cond,
          type = "error"
        )
      },
      finally = train_status$train_model  <- FALSE)
    }
  })
  
  observeEvent(train_status$save_model, {
    if (train_status$save_model) {
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
          paste(as.vector(train_status$datasets$index), collapse = ", ")
        
        saveModel(
          train_status$models,
          input$target,
          fields$name(),
          fields$description(),
          session$userData$user$id,
          selected_dataset$id(),
          trainRowNumbers,
          advance_values$preds()
        )
        train_status$done <- TRUE
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
        train_status$save_model  <- FALSE
        train_status$models <- NULL
        train_status$datsets <- NULL
      })
    }
  })
  
  observeEvent(train_status$done, {
    if (train_status$done) {
      sendSweetAlert(
        session = session,
        title = "Listo!!",
        text = "Modelo entrenado y guardado",
        type = "success"
      )
      train_status$done <- FALSE
    }
  })
  
  observeEvent(input$confirm, {
    if (isTRUE(input$confirm)) {
      train_status$process_data <- TRUE
    }
  })
  
  return(list(do_report = reactive({
    status$done
  })))
}

getPredsNames <- function(dataset, target) {
  preds_names <- colnames(dataset)
  preds_names <- preds_names[!preds_names %in% target]
  return(preds_names)
}
