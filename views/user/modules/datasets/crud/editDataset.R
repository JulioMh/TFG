editDatasetUI <- function (id) {
  ns <- NS(id)
  tagList(
    br(),
    sidebarPanel(
      fileInput(
        ns("dataset"),
        tagList(icon("file-medical-alt"), "Sube el conjunto de datos:"),
        multiple = FALSE,
        accept = c(".json",
                   ".csv")
      ),
      textInput(ns("name"), tagList(icon("signature"), "Nombre:")),
      textAreaInput(ns("description"), tagList(icon("signature"), "Descripcion:")),
      uiOutput(ns("submit"))
    ),
    mainPanel(DT::dataTableOutput(ns("tablePrev")))
  )
}

editDataset <- function (input, output, session) {
  dataset <- reactiveValues(loadData(session$userData$dataset))
  
  output$submit <- renderUI({
    validate(need(input$dataset != "", label = "File"),
             need(input$name != "", label = "Name"))
    return(div(style = "text-align: center;",
               actionButton(session$ns("submit"), "Save")))
  })
  
  observeEvent(input$submit, {
    data <- list("name" = input$name,
                 "description" = input$description,
                 "file" = dataset$content,
                 "user_id" = USER$id)
    res <- performanceDatasetSave(data)
    output$err <- renderText(res)
    
    if(res == "Something went worng"){
      output$err <- renderText(res)
    }
  })
  
  output$tablePrev <- DT::renderDataTable({
    req(data$content)
    tryCatch({
      dataset$content <- read.csv(input$dataset$datapath)
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    return(DT::datatable(dataset$content, options = list(
      lengthMenu = list(c(5, 15, -1), c('5', '10', 'All')),
      pageLength = 10
    )))
  })
}

loadData <- function(name) {
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  query <-
    sprintf("select * from Database where name= '%s'",
            name)
  tryCatch({
    response <- dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  })
  if (nrow(response) == 0) {
    res <- "Wrong dataset name"
  } else{
    res <- response
  }
  dbDisconnect(db)
  return(res)
}
uploadData <- function(data){
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  res <- "Done!"
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    "dataset",
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  tryCatch({
    dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  })
  dbDisconnect(db)
  return(res)
}