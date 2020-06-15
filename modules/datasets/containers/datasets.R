datasetsUI <- function (id) {
  ns <- NS(id)
  tagList(tabsetPanel(
    id = ns("tabsetPanel"),
    tabPanel("Mis datasets",
             br(),
             uiOutput(ns("mode"))),
    tabPanel(
      "Nuevo conjunto de datos",
      br(),
      sidebarPanel(uploadDatasetUI(ns("upload"))),
      mainPanel(datasetUI(ns("dataset")))
    )
  ))
}

datasets <- function (input, output, session) {
  values <- reactiveValues(reload = FALSE, show_edit = FALSE)
  path <- callModule(uploadDatasetServer, "upload")
  cancel <- callModule(editDatasetServer, "edit", dataset_id)
  callModule(dataset, "dataset", reactive({
    req(path())
    read.csv(path())
  }))
  
  
  dataset_id <-
    callModule(listServer, "list", loadDatasets, reactive(values$reload))
  
  observeEvent(input$tabsetPanel, {
    if (input$tabsetPanel == "Nuevo conjunto de datos") {
      values$reload <- FALSE
    } else{
      values$reload <- TRUE
    }
  })
  
  observeEvent(dataset_id(), {
    values$show_edit <- TRUE
    values$reload <- FALSE
  })
  
  observeEvent(cancel(), {
    values$show_edit <- FALSE
    values$reload <- TRUE
  })
  
  output$mode <- renderUI({
    if (values$show_edit) {
      editDatasetUI(session$ns("edit"))
    } else {
      listUI(session$ns("list"))
    }
  })
}