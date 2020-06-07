advanceModeUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
      pickerInput(
        inputId = ns("methods"),
        label = "Selecciona uno o mas métodos:",
        choices = list(
          "Random Forest" = getListForPickerMethod("Random Forest"),
          "Neural Network" = getListForPickerMethod("Neural Network"),
          "Tree-Based Model" = getListForPickerMethod("Tree-Based Model")
        ),
        selected = c("rf", "nnet"),
        multiple = TRUE,
        options = list(`live-search` = TRUE,
                       `actions-box` = TRUE)
      ),
    uiOutput(ns("pick_predictors")),
    materialSwitch(inputId = ns("predict_na"),
                   label = "Predecir campos vacíos"),
    uiOutput(ns("pick_to_center"))
  )
}

advanceMode <- function(input, output, session, columns) {

  output$pick_predictors <- renderUI({
    tagList(
      pickerInput(
        inputId = session$ns("predictors"),
        label = "Selecciona los predictores:",
        choices = columns(),
        selected = columns(),
        multiple = TRUE,
        options = list(`live-search` = TRUE,
                       `actions-box` = TRUE)
      )
    )
  })
  output$pick_to_center <- renderUI({
    pickerInput(
      inputId = session$ns("preds_to_center"),
      label = "Selecciona los predicores a normalizar:",
      choices = input$predictors,
      multiple = TRUE,
      options = list(`live-search` = TRUE,
                     `actions-box` = TRUE)
    )
  })
  
  return(list(
    preds = reactive({
      input$predictors
    }),
    to_center = reactive({
      input$preds_to_center
    }),
    pred_missing_fields = reactive({
      input$predict_na
    }),
    methods = reactive({
      input$methods
    })
  ))
}

getListForPickerMethod = function(tag) {
  raw_methods <- list.filter(getModelInfo(), tag %in% tags)
  names <- names(raw_methods)
  methods <- list()
  for (name in names) {
    methods[[raw_methods[[name]]$label]] <- name
  }
  return(methods)
}
