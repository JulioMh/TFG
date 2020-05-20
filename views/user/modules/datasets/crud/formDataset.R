formDatasetUI <- function (id) {
  ns <- NS(id)
  tagList(
    br(),
    sidebarPanel(
      uiOutput(ns("form")),
          br(),
          textOutput(ns("err")),),
    mainPanel(DT::dataTableOutput(ns("tablePrev")))
  )
}

formDataset <- function (input, output, session, data, onClick) {
  dataset <- reactiveValues()
  output$form <- renderUI({
    tagList(
      fileInput(
        session$ns("dataset"),
        tagList(icon("file-medical-alt"), "Sube el conjunto de datos:"),
        multiple = FALSE,
        accept = c(".json",
                   ".csv")
      ),
      textInput(
        session$ns("name"),
        tagList(icon("signature"), "Nombre:"),
        value = data$name
      ),
      textAreaInput(
        session$ns("description"),
        tagList(icon("signature"), "Descripcion:"),
        value = data$description
      ),
      uiOutput(session$ns("submit"))
    )
  })
  
  output$submit <- renderUI({
    validate(need(input$dataset != "", label = "File"),
             need(input$name != "", label = "Name"))
    return(div(style = "text-align: center;",
               actionButton(session$ns("submit"), "Save")))
  })
  
  observeEvent(input$submit, {
    submitedData <- list(
      "name" = input$name,
      "description" = input$description,
      "dataset" = dataset$content,
      "user_id" = session$userData$user$id
    )
    res <- onClick(submitedData)
    output$err <- renderText(res)
  })
  
  output$tablePrev <- DT::renderDataTable({
    req(input$dataset$datapath)
    tryCatch({
      dataset$content <- read.csv(input$dataset$datapath)
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    return(DT::datatable(
      dataset$content,
      options = list(lengthMenu = list(c(5, 10,-1), c('5', '10', 'All')),
                     pageLength = 10)
    ))
  })
  
}