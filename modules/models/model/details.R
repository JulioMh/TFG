detailsUI <- function(id) {
  ns <- NS(id)
  tagList(verbatimTextOutput(ns("confusion_matrix")))
}

details <-
  function(input,
           output,
           session,
           test,
           predicted,
           target) {
    
    observeEvent(test(),{
      req(test())
      print(test()[[target()]])
      print(as.numeric(predicted()))
    })
    output$confusion_matrix <-
      renderPrint({
        confusionMatrix(reference = test()[[target()]], data = as.numeric(predicted()))})
  }
