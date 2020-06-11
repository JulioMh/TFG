datasetUI <- function (id) {
  ns <- NS(id)
  tagList(tabsetPanel(
    id = ns("tabsetPanel"),
    tabPanel("My datasets",
             value = "mydatasets",
             br(),
             uiOutput(ns("mode"))),
    tabPanel(
      "Nuevo conjunto de datos",
      br(),
      sidebarPanel(datasetFormUI(ns("new"))),
      mainPanel(uiOutput(ns("table")))
    )
  ))
}

dataset <- function (input, output, session) {
  data <-
    reactiveValues(
      name = "",
      description = "",
      id = "",
      dataset = "",
      doEdit = FALSE
    )
  
  values <- reactiveValues(reload = NULL)
  
  edit_result <- callModule(datasetForm, "edit", reactive({
    data
  }))
  create_result <- callModule(datasetForm, "new", NULL)
  
  callModule(table, "edit", reactive({
    data$dataset
  }))
  callModule(table, "dataset", reactive({
    data$dataset
  }))
  
  dataset_id <-
    callModule(listServer, "list", loadDatasets, reactive({
      values$reload
    }))
  
  observeEvent(input$tabsetPanel, {
    if (input$tabsetPanel == "Nuevo conjunto de datos") {
      values$reload <- FALSE
    } else{
      values$reload <- TRUE
    }
  })
  
  output$mode <- renderUI({
    if (data$doEdit) {
      tagList(sidebarPanel(
        datasetFormUI(session$ns("edit")),
        actionBttn(
          inputId = session$ns("cancel"),
          icon = icon("arrow-circle-left"),
          style = "pill",
          color = "warning",
          size = "xs"
        ),
      ),
      mainPanel(tableUI(session$ns("edit"))))
    } else {
      listUI(session$ns("list"))
    }
  })
  
  observeEvent(input$cancel, {
    data$id <- ""
    data$name <- ""
    data$description <- ""
    data$doEdit <- FALSE
    values$reload <- TRUE
  })
  
  observeEvent(dataset_id(), {
    req(dataset_id())
    dataset <- loadDataset(dataset_id())
    data$id <- dataset$id
    data$name <- dataset$name
    data$description <- dataset$description
    data$dataset <- dataset$dataset
    data$doEdit <- TRUE
    values$reload <- FALSE
  })
  
  observeEvent(create_result$path_dataset(), {
    if (create_result$path_dataset() == "reset") {
      output$table <- NULL
    } else{
      data$dataset <- read.csv(create_result$path_dataset())
      output$table <- renderUI(tableUI(session$ns("dataset")))
    }
  })

}