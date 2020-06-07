modelsUI <- function (id) {
  ns <- NS(id)
  uiOutput(ns("models"))
}

models <- function (input, output, session) {
  values <- reactiveValues(show_details = FALSE, reload = FALSE)
  train <- callModule(trainServer, "train")
  model_id <-
    callModule(listServer, "list", loadModels, reactive({
      values$reload
    }))
  cancel <- callModule(model, "details", reactive({
    values$id
  }))
  
  observeEvent(cancel(), {
    values$show_details <- FALSE
    values$reload <- TRUE
  })
  
  observeEvent(model_id(), {
    req(model_id())
    values$show_details <- TRUE
    values$reload <- FALSE
    values$id <- model_id()
  })
  
  observeEvent(train$do_report(), {
    req(train$do_report())
    if (train$do_report()) {
      values$show_details <- TRUE
      values$id <- first(loadModels(session$userData$user$id)$id)
    }
  })
  
  output$models <- renderUI({
    if (values$show_details) {
      modelUI(session$ns("details"))
    } else{
      tagList(tabsetPanel(
        tabPanel("Modelos",
                 br(),
                 listUI(session$ns("list"))),
        tabPanel("Entrena a un nuevo modelo",
                 br(),
                 trainUI(session$ns("train")))
      ))
    }
  })
}