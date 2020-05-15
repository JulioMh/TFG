listDatasetUI <- function (id) {
  ns <- NS(id)
  tagList(br(), DT::dataTableOutput(ns("table")))
}

listDataset <- function(input, output, session, v) {
  datasets <- loadDatasets(session$userData$user$id)
  observeEvent(input$table_rows_selected, {
    index = input$table_rows_selected
    dataset_id <- datasets$id[index]
    session$userData$user$dataset <- dataset_id
    v$doEdit = TRUE
  })
  
  output$table <-
    DT::renderDataTable(
      datasets[2:3],
      options = list(lengthMenu = list(c(5, 10,-1), c('5', '10', 'All')),
                     pageLength = 10),
      selection = 'single',
    )
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
    sprintf("select * from Dataset where user_id= '%s'",
            id)
  tryCatch({
    response <- dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  })
  dbDisconnect(db)
  return(response)
}