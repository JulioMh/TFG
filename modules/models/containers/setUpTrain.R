setUpTrainUI <- function (id) {
  ns <- NS(id)
  fluidRow(column(
    3,
    wellPanel(
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
      modelFormUI(ns("form")),
      uiOutput(ns("submit"))
    )
  ),
  column(6,
         datasetUI(ns("dataset"))),
  column(3,
         wellPanel(selectDatasetUI(
           ns("select_dataset")
         ))))
}

setUpTrain <- function (input, output, session) {
  #VARIABLES
  values <- reactiveValues(reload = FALSE)
  
  #MODULES
  attributes <-
    callModule(modelForm, "form", NULL, reactive(values$reload))
  selected <-
    callModule(selectDataset, "select_dataset", reactive(NULL))
  settings <-
    callModule(
      advanceMode,
      "advance",
      reactive(values$methods),
      reactive(input$target),
      selected$dataset,
      reactive(values$preds)
    )
  callModule(dataset, "dataset", selected$dataset)
  
  #RENDERS
  output$picker_target <- renderUI({
    pickerInput(
      inputId = session$ns("target"),
      label = tagList(icon("bullseye"), "Selecciona la columna objetivo:"),
      choices = colnames(selected$dataset()),
      options = list(`live-search` = TRUE)
    )
  })
  
  output$advance_ui <- renderUI({
    if ((input$open_advance %% 2) != 0) {
      advanceModeUI(session$ns("advance"))
    }
  })
  
  output$submit <- renderUI({
    validate(
      need(selected$id(), "Selecciona un dataset"),
      need(attributes$name() != "", "Debes introducir un nombre")
    )
    
    return(div(
      style = "text-align: center;",
      actionBttn(
        inputId = session$ns("submit"),
        label = "Entrenar",
        style = "stretch",
        color = "success"
      )
    ))
  })
  
  #LISTENER
  observeEvent(settings$methods(), {
    values$methods <- settings$methods()
  })
  
  observeEvent(settings$preds(), {
    values$preds <- settings$preds()
  })
  
  observeEvent(input$confirm, {
    if (isTRUE(input$confirm)) {
      values$reload <- !values$reload
    }
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
  
  observeEvent(input$target, {
    values$preds <- getPredsNames(selected$dataset(), input$target)
    values$methods <-
      ifelse(is.factor(selected$dataset()[[input$target]]),
             "adaboost",
             "rf")
  })
  
  return(
    list(
      dataset = reactive(selected$dataset()),
      dataset_id = selected$id,
      preds = reactive(values$preds),
      to_center = settings$to_center,
      pred_missing_fields = settings$pred_missing_fields,
      methods = reactive(values$methods),
      name = attributes$name,
      description = attributes$description,
      confirm = reactive(input$confirm),
      target = reactive(input$target),
      isPublic = attributes$isPublic
    )
  )
}
