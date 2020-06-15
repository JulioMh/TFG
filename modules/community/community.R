communityUI <- function (id) {
  ns <- NS(id)
  uiOutput(ns("mode"))
}

community <- function (input, output, session) {
  values <- reactiveValues(show_details = FALSE, reload = FALSE, count = 0)
  
  train_id <- callModule(trainServer, "train")
  model_id <-
    callModule(listServer, "list", loadModels, reactive({
      values$reload
    }))
  
  observeEvent(input$cancel, {
    values$show_details <- !values$show_details
    values$reload <- TRUE
  })
  
  observeEvent(model_id(), {
    req(model_id())
    values$count <- values$count + 1
    values$id <- model_id()
    values$show_details <- !values$show_details
    values$reload <- FALSE
    callModule(model, paste0("model-", values$id, "-", values$count), values$id)
  })
  
  output$mode <- renderUI({
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
        modelUI(session$ns(paste0("model-", values$id, "-", values$count))) 
      )
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