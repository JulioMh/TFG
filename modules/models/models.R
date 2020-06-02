modelsUI <- function (id) {
  ns <- NS(id)
  uiOutput(ns("models"))
}

models <- function (input, output, session) {
  values <- reactiveValues(show_details = FALSE, reload = FALSE)
  train <- callModule(trainServer, "train")
  model_id <- callModule(listServer, "list", loadModels, reactive({values$reload}))
  callModule(model, "details", model_id)
  
  observeEvent(input$cancel, {
    values$show_details <- FALSE
    values$reload <- TRUE
  })
  
  observeEvent(model_id(), {
    req(model_id())
    values$show_details <- TRUE
    values$reload <- FALSE
  })
  
  output$models <- renderUI({
    if (values$show_details) {
      tagList(
        actionBttn(
          inputId = session$ns("cancel"),
          icon = icon("arrow-circle-left"),
          style = "pill", 
          color = "warning",
          size = "xs"
        ),
        hr(),
        modelUI(session$ns("details")))
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