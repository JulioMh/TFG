detailsUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("mode"))
}

details <-
  function(input,
           output,
           session,
           model,
           model_id) {
    ns <- session$ns
    output$mode <- renderUI({
      if (model$modelType != "Regression") {
        fluidRow(
          column(4, verbatimTextOutput(ns("description"))),
          column(4, plotOutput(ns("varImp"))),
          column(4, verbatimTextOutput(ns("confusion")))
        )
      }else{
        fluidRow(
          column(6, verbatimTextOutput(ns("description"))),
          column(6, plotOutput(ns("varImp")))
        )
      }
    })
    
    output$varImp <-
      renderPlot({
        plot(varImp(model), main = "Importancia de las variables")
      })
    
    output$description <- renderPrint(model)
    
    output$confusion <- renderPrint({
      if (model$modelType != "Regression") {
        datasets <- getModelDataset(model_id)
        target <- getModelTarget(model_id)
        pre <- getPreModels(model_id)
        
        processed_dataset <- prepareDataToPredict(
          dataset = datasets$test,
          impute_model = pre$impute_model,
          dummy_model = pre$dummy_model,
          center_model = pre$center_model,
          target = target
        )
        tryCatch({
          pred <- predict(model, processed_dataset)
          return(confusionMatrix(reference = datasets$test[[target]], data = pred))
        }, error = function(cond) {
          sendSweetAlert(
            session = session,
            title = "Se ha encontrado un problema...",
            text = cond,
            type = "error"
          )
        })
      }
    })
  }
