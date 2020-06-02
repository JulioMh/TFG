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
  fields <- callModule(basicForm, "basic", NULL)
  selected_dataset <- callModule(selectDataset, "dataset")
  settings <- callModule(advanceMode, "advance", preds_names)
  advance_values <-
    reactiveValues(
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
  
  observeEvent(selected_dataset$dataset(), {
    req(selected_dataset$dataset())
    callModule(table, "models", reactive({
      selected_dataset$dataset()
    }))
  })
  
  observeEvent(input$submit, {
    confirmSweetAlert(
      session = session,
      inputId = "confirm",
      type = "warning",
      title = "¿Estas seguro?",
      text = "Este proceso tardará varios minutos"
    )
  })
  
  observeEvent(settings$preds(), {
    advance_values$preds <- settings$preds
  })
  
  observeEvent(settings$to_center(), {
    advance_values$to_center <- settings$to_center()
  })
  observeEvent(settings$pred_missing_fields(), {
    advance_values$pred_missing_fields <- settings$pred_missing_fields()
  })
  
  observeEvent(input$confirm, {
    if (isTRUE(input$confirm)) {
      tryCatch({
        sendSweetAlert(
          session = session,
          title = "Preparando datos...",
          closeOnClickOutside = FALSE,
          type = "info",
          btn_labels = NA,
          showCloseButton = FALSE
        )
        datasets <-
          prepareDataForTraining(
            dataset = selected_dataset$dataset(),
            preds = advance_values$preds(),
            preds_to_center = advance_values$to_center,
            do_pred_na = advance_values$pred_missing_fields,
            target = input$target
          )
        
        sendSweetAlert(
          session = session,
          title = "Entrenando los modelos...",
          text = "Espere un momento, estamos trabajando en ello",
          closeOnClickOutside = FALSE,
          type = "info",
          btn_labels = NA,
          showCloseButton = FALSE
        )
        
        models <-
          doTrain(datasets$trainData,
                  input$target)
        models$impute_model <- datasets$impute_model
        models$dummy_model <- datasets$dummy_model
        models$center_model <- datasets$center_model
        
        sendSweetAlert(
          session = session,
          title = "Almacenando en la base de datos...",
          closeOnClickOutside = FALSE,
          type = "info",
          btn_labels = NA,
          showCloseButton = FALSE
        )
        
        trainRowNumbers <- paste(as.vector(trainRowNumbers), collapse = ", ")
        
        saveModel(
          models,
          advance_values$preds(),
          input$target,
          fields$name(),
          fields$description(),
          session$userData$user$id,
          selected_dataset$id(),
          trainRowNumbers
        )
        reset("name")
        reset("description")
        sendSweetAlert(
          session = session,
          title = "Listo!!",
          text = "Modelo entrenado y guardado",
          type = "success"
        )
        status$done <- TRUE
      },
      error = function(cond) {
        sendSweetAlert(
          session = session,
          title = "Ha habido un problema...",
          text = cond,
          type = "error"
        )
      })
    }
  })
  
  return(list(
    do_report = reactive({status$done})
  ))
}

getPredsNames <- function(dataset, target) {
  preds_names <- colnames(dataset)
  preds_names <- preds_names[!preds_names %in% target]
  return(preds_names)
}
