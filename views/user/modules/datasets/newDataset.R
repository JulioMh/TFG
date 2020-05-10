newDatasetUI <- function (id) {
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
      uiOutput(ns("submit")),
      br(),
      textOutput(ns("err"))
    ),
    mainPanel(DT::dataTableOutput(ns("tablePrev")))
  )
}

newDataset <- function (input, output, session, USER) {
  output$submit <- renderUI({
    validate(need(input$dataset != "", label = "File"),
             need(input$name != "", label = "Name"))
    return(div(style = "text-align: center;",
               actionButton(session$ns("submit"), "Save")))
  })
  
  observeEvent(input$submit, {
    data <- list("name" = input$name,
                 "description" = input$description,
                 "file" = paste(input$dataset$datapath),
                 "user_id" = USER$id)
    res <- performanceDatasetSave(data)
    output$err <- renderText(res)

    if(res == "Something went worng"){
      output$err <- renderText(res)
    }
  })
  
  output$tablePrev <- DT::renderDataTable({
    req(input$dataset)
    tryCatch({
      ds <- read.csv(input$dataset$datapath)
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    return(DT::datatable(ds, options = list(
      lengthMenu = list(c(5, 15, -1), c('5', '10', 'All')),
      pageLength = 10
    )))
  })
}

performanceDatasetSave <- function(data){
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
  }, error = function(res) {
    res <- "Something went worng"
  })
  dbDisconnect(db)
  return(res)
}