modelUI <- function(id) {
  ns <- NS(id)
  tagList(tabsetPanel(
    id = ns("model_tabs"),
    type = "pills",
    tabPanel("Información",
             br(),
             modelSummaryUI(ns("summary"))),
    tabPanel("Usar",
             br(),
             useModelUI(ns("use")))
  ))
}

model <- function(input, output, session, model_id) {
  callModule(modelSummary, "summary", model_id)
  callModule(useModel, "use", model_id)
  
  
}