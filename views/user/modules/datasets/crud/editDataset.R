editDatasetUI <- function (id) {
  ns <- NS(id)
  tagList(actionButton(ns("back"), "Go back"),
          formDatasetUI(ns("edit")))
}

editDataset <- function (input, output, session, v) {
  dataset <- reactiveValues(id = session$userData$user$dataset, content = "")
  
  if(dataset$id != ""){
    dataset$content <- loadData(dataset$id)
    callModule(formDataset, "edit", dataset$content, update)
  }
  
  observeEvent(input$back, {
    v$doEdit = FALSE
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
    sprintf("select * from Dataset where id= %s",
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
uploadData <- function(data) {
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