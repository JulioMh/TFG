datasetUI <- function (id) {
  ns <- NS(id)
  tagList(tabsetPanel(id=ns("tabsetPanel"),
    tabPanel("My datasets",
             value="mydatasets",
             br(),
             uiOutput(ns("mode"))),
    tabPanel(
      "New dataset",
      br(),
      sidebarPanel(datasetFormUI(ns("new"))),
      mainPanel(uiOutput(ns("table")))
    )
  ))
}

testdataset <- function (input, output, session) {
  data <-
    reactiveValues(
      name = "",
      description = "",
      id = "",
      dataset = "",
      fetch = loadDatasets(session$userData$user$id),
      doEdit = FALSE
    )
  
  edit_result <- callModule(datasetForm, "edit", reactive({data}))
  create_result <- callModule(datasetForm, "new", NULL)
  
  
  selected <-
    callModule(table, "list", reactive({
      data$fetch[2:3]
    }))
  
  observeEvent(input$tabsetPanel,{
    data$fetch <- loadDatasets(session$userData$user$id)
  })
  
  observeEvent(input$reload,{
    data$fetch <- loadDatasets(session$userData$user$id)
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
      mainPanel(tableUI(session$ns(
        "edit"
      ))))
    } else {
      tagList(
      actionBttn(
        inputId = session$ns("reload"),
        label = NULL,
        style = "material-circle", 
        color = "default",
        icon = icon("redo"),
        size = "xs"
      ),
      br(),
      tableUI(session$ns("list")))
    }
  })
  
  
  
  observeEvent(input$cancel, {
    data$id <- ""
    data$name <- ""
    data$description <- ""
    data$doEdit <- FALSE
    data$fetch <- loadDatasets(session$userData$user$id)
  })
  
  observeEvent(selected$index(), {
    req(selected$index())
    data$id <- data$fetch$id[selected$index()]
    data$name <- data$fetch$name[selected$index()]
    data$description <- data$fetch$description[selected$index()]
    data$dataset <- read.csv(data$fetch$dataset[selected$index()])
    data$doEdit <- TRUE
    callModule(table, "edit", reactive({
      data$dataset
    }))
  })
  
  observeEvent(create_result$path_dataset(), {
    if(create_result$path_dataset() == "reset"){
      output$table <- renderUI(h1(""))
    }else{
      dataset <- read.csv(create_result$path_dataset())
      output$table <- renderUI(tableUI(session$ns("dataset")))
      callModule(table, "dataset", reactive({
        dataset
      }))  
    }
    
  })
}