advanceModeUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    uiOutput(ns("pick_method")),
    uiOutput(ns("pick_predictors")),
    materialSwitch(inputId = ns("predict_na"),
                   label = "Predecir campos vacíos"),
    uiOutput(ns("pick_to_center"))
  )
}

advanceMode <-
  function(input,
           output,
           session,
           method,
           target,
           dataset,
           preds) {
    values <- reactiveValues()
    
    observeEvent(target(), {
      if (is.factor(dataset()[[target()]])) {
        values$method_choices <- list(
          "Random Forest" = getListForPickerMethod("Random Forest", "Classification"),
          "Neural Network" = getListForPickerMethod("Neural Network", "Classification"),
          "Tree-Based Model" = getListForPickerMethod("Tree-Based Model", "Classification")
        )
      } else{
        values$method_choices <- list(
          "Random Forest" = getListForPickerMethod("Random Forest", "Regression"),
          "Neural Network" = getListForPickerMethod("Neural Network", "Regression"),
          "Tree-Based Model" = getListForPickerMethod("Tree-Based Model", "Regression")
        )
      }
    })
    
    output$pick_method <- renderUI({
      pickerInput(
        inputId = session$ns("methods"),
        label = "Selecciona uno o mas métodos:",
        choices = values$method_choices,
        selected = method(),
        multiple = TRUE,
        options = list(`live-search` = TRUE,
                       `actions-box` = TRUE)
      )
    })
    
    output$pick_predictors <- renderUI({
      tagList(
        pickerInput(
          inputId = session$ns("predictors"),
          label = "Selecciona los predictores:",
          choices = getPredsNames(dataset(), target()),
          selected = getPredsNames(dataset(), target()),
          multiple = TRUE,
          options = list(`live-search` = TRUE,
                         `actions-box` = TRUE)
        )
      )
    })
    output$pick_to_center <- renderUI({
      pickerInput(
        inputId = session$ns("preds_to_center"),
        label = "Selecciona los predictores a normalizar:",
        choices = input$predictors,
        multiple = TRUE,
        options = list(`live-search` = TRUE,
                       `actions-box` = TRUE)
      )
    })
    
    return(
      list(
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
      )
    )
  }
