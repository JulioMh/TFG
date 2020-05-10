myDatasetsUI <- function (id) {
  ns <- NS(id)
  tagList(tabsetPanel(
    tabPanel("My datasets",
             DT::dataTableOutput(ns("datasets"))),
    tabPanel("New dataset",
             newDatasetUI("fromDataset"))
  ))
}

myDatasets <- function (input, output, session, USER) {
  output$datasets <-
    DT::renderDataTable(DT::datatable(
      loadDatasets(USER$id),
      options = list(lengthMenu = list(c(5, 10,-1), c('5', '10', 'All')),
                     pageLength = 10)
    ))
}

loadDatasets <- function(id) {
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
    sprintf("select name, description from dataset where user_id= '%s'",
            id)
  tryCatch({
    response <- dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  })
  dbDisconnect(db)
  return(response)
}