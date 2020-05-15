newDatasetUI <- function (id) {
  ns <- NS(id)
  formDatasetUI(ns("new"))
}

newDataset <- function (input, output, session) {
  data <- reactiveValues(name = "", description = "")
  callModule(formDataset, "new", data, create)
}

create <- function(data){
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
    "Dataset",
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